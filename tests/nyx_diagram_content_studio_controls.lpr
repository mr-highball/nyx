{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}program nyx_diagram_content_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.codegen, nyx.controls, nyx.callbacks, nyx.canvas.workspace,
  nyx.diagram, nyx.diagram.content,
  nyx.diagram.declarations, nyx.diagram.editor, nyx.studio.projects,
  nyx.studio.sourcejobs, nyx.studio.session, nyx.test.diagram.content
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser, nyx.diagram.editor.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.studio.lcl,
    nyx.diagram.editor.lcl, nyx.test.capture.lcl{$endif};

{$ifndef PAS2JS}
type
  { Borrow a fixture-owned ordinary native button only for its installed Click.
    No widget or event receiver survives the owning Studio/window fixture. }
  TClickControl = class(TControl);
  { Fixture-owned error sink outlives deferred UI retirement and records failures
    without creating a modal exception dialog that would obscure the journey. }
  TFailure = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
{$endif}

var
  GStudio: {$ifdef PAS2JS}TNyxStudio{$else}TNyxNativeStudio{$endif};
  GWindow: INyxDiagramEditorWindow;
  GView: INyxDiagramEditorView;
  GContent: INyxDiagramContentView;
  GTarget: {$ifdef PAS2JS}INyxBrowserDiagramEditorView{$else}INyxLCLDiagramEditorView{$endif};
  GOriginal: TNyxProjectPair;
  GFirst: TNyxProjectPair;
  GSecond: TNyxProjectPair;
  GSubmitted: TNyxText;
  GStage: Integer;
  GChecks: Integer;
  GStarted: Double;
  GDone: Boolean;
  {$ifndef PAS2JS}
  GForm: TForm;
  GFailure: TFailure;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}

  if (GFailure <> nil) and (GFailure.Message <> '') then
  begin
    raise Exception.Create(GFailure.Message);
  end;
  {$endif}

  if not AValue then
  begin
    raise Exception.Create('Studio diagram content: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Message := AError.ClassName + ': ' + AError.Message;
end;

procedure ExportPair(const AName: TNyxText; const APair: TNyxProjectPair);
var
  LDirectory: TNyxText;

  procedure Save(const AName, AText: TNyxText);
  var
    LFile: TFileStream;
  begin
    LFile := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if AText <> '' then
      begin
        LFile.WriteBuffer(AText[1], Length(AText));
      end;
    finally
      LFile.Free;
    end;
  end;

begin
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + AName + PathDelim;
  ForceDirectories(LDirectory);
  Save('nyx.generated.view.pas', APair.Source);
  Save('expected.nyx', APair.Design);
end;
{$endif}

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := window.performance.now;{$else}Result := GetTickCount64;{$endif}
end;

function ID(APart: TNyxDiagramEditorPart): TNyxText;
begin
  Result := NyxDiagramEditorPart(GTarget.Renderer.Root.ID, APart).ID;
end;

procedure Click(APart: TNyxDiagramEditorPart);
begin
  {$ifdef PAS2JS}GTarget.Renderer.ElementFor(ID(APart)).click;
  {$else}TClickControl(GTarget.Renderer.ControlFor(ID(APart))).Click;{$endif}
end;

procedure ShellClick(const AID: TNyxText);
begin
  {$ifdef PAS2JS}GStudio.ShellView.ElementFor(AID).click;
  {$else}TClickControl(GStudio.ShellView.ControlFor(AID)).Click;{$endif}
end;

procedure Choose(APart: TNyxDiagramEditorPart; const AValue: TNyxText);
{$ifndef PAS2JS}
var
  LChoice: TComboBox;
{$endif}
begin
  {$ifdef PAS2JS}
  TJSHTMLSelectElement(GTarget.Renderer.InputFor(ID(APart))).value := AValue;
  GTarget.Renderer.InputFor(ID(APart)).dispatchEvent(TJSEvent.new('change'));
  {$else}
  LChoice := TComboBox(GTarget.Renderer.InputFor(ID(APart)));
  Check(LChoice.Items.IndexOf(AValue) >= 0, 'actual Studio picker includes the requested source-context choice');
  LChoice.ItemIndex := LChoice.Items.IndexOf(AValue);
  LChoice.OnChange(LChoice);
  {$endif}
end;

function SavedDiagram(const APair: TNyxProjectPair): TNyxText;
var
  LDocument: TNyxDocument;
begin
  LDocument := TNyxCodec.Decode(APair.Design);
  try
    Result := LDocument.Diagrams.Definition(NyxDiagramRef('Content board')).ToData.ToJSON;
  finally
    LDocument.Free;
  end;
end;

procedure Drive;
var
  LDocument: TNyxDocument;
  LRuntime: INyxCanvasRuntimeView;
begin

  if Clock - GStarted > 90000 then
  begin
    raise Exception.Create('Studio content deadline at stage ' + IntToStr(GStage));
  end;

  if GStudio.SourceBusy or GStudio.PresentationPending then
  begin
    Exit;
  end;
  case GStage of
    0:
      begin
        { Explicit trusted fixture seeding is local-only. This does not claim
          project-file import, compiler execution or authenticated MCP delivery. }
        LDocument := DiagramContentDocument;
        try
          { Studio authoring must not require constructing an application's
            callback implementation. This valid handler intentionally has no
            registration in the editor process; runtime dispatch is separate. }
          NyxCallbacks(LDocument.FindComponent('Details').Find('details-send')).OnClick
            .Add(NyxHandler('TPreviewMustRemainUnresolved'), NyxCallbackID('preview-only'));
          GOriginal := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
        finally
          LDocument.Free;
        end;
        {$ifdef PAS2JS}
        GStudio.SourceCommands.OpenProject(GOriginal);
        {$else}
        GStudio.LoadProject(GOriginal);
        {$endif}
      end;
    1:
      begin
        GWindow := GStudio.ShowDiagram(NyxDiagramRef('Content board'));
        GView := GWindow.View;
        Check(Supports(GView, {$ifdef PAS2JS}INyxBrowserDiagramEditorView{$else}INyxLCLDiagramEditorView{$endif},
          GTarget), 'ordinary Studio uses the public target editor for rich saved intent');
        Check(Supports(GView, INyxDiagramContentView, GContent), 'ordinary Studio exposes the same public content capability');
        Check(Supports(GView.Canvas, INyxCanvasRuntimeView, LRuntime) and
          (LRuntime.BehaviorFor(GContent.ContentItem(NyxDiagramVertexRef('Form'))) = nil),
          'ordinary Studio opens uncompiled behavioral reusable definitions in explicit design preview');
        {$ifndef PAS2JS}

        if ParamStr(2) = '--compact' then
        begin
          GetParentForm(GTarget.Renderer.ControlFor(ID(ndepSave))).SetBounds(20, 20, 390, 800);
        end;
        {$endif}
        GView.SelectVertex(NyxDiagramVertexRef('Form'));
        Click(ndepToggleContent);
        Choose(ndepContentReference, 'Summary');
        Check(GView.HasPendingProperties and
          (EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GOriginal)),
          'actual picker input remains local and changes neither accepted file');
        Click(ndepApplyContent);
        GSubmitted := GView.Diagram.ToData.ToJSON;
        Check(not GView.HasPendingProperties and GView.HasUnsavedChanges and
          (GView.Diagram.FindVertex(NyxDiagramVertexRef('Form')).Content.Component.Name = 'Summary'),
          'public Apply changes local reusable content before explicit project Save');
      end;
    2:
      begin
        {$ifndef PAS2JS}
        GetParentForm(GTarget.Renderer.ControlFor(ID(ndepSave))).BringToFront;
        SaveNyxNativeCapture(GetParentForm(GTarget.Renderer.ControlFor(ID(ndepSave))),
          IncludeTrailingPathDelimiter(ParamStr(1)) + 'content-window.png', ncmDisplay);
        {$endif}
        Click(ndepSave);
        Check(GView.SavePending and (EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GOriginal)),
          'ordinary Save queues one captured rich diagram through the owning source command');
      end;
    3:
      begin
        GFirst := GStudio.ProjectSnapshot;
        Check((GStudio.SourceCommands.State = nssApplied) and not GView.SavePending and
          not GView.HasUnsavedChanges and (SavedDiagram(GFirst) = GSubmitted),
          'source command acknowledgement retains the exact saved rich graph');
        Check((GFirst.Source <> GOriginal.Source) and
          (Pos('NyxDiagramComponent(NyxComponent(''Summary''))', GFirst.Source) > 0),
          'ordinary Studio emits crafted specialized content source beside the saved design');
        Choose(ndepContentKind, NyxDiagramContentCaption(ndckImage));
        Choose(ndepContentReference, 'Hosted tile');
        Click(ndepApplyContent);
        GSubmitted := GView.Diagram.ToData.ToJSON;
        Click(ndepSave);
        Check(GView.SavePending, 'hosted resource abstraction uses the same deliberate Save contract');
      end;
    4:
      begin
        GSecond := GStudio.ProjectSnapshot;
        Check((GStudio.SourceCommands.State = nssApplied) and not GView.SavePending and
          (SavedDiagram(GSecond) = GSubmitted),
          'hosted reference with authored fallback is durable without rewriting it as an embedded source');
        GWindow.Hide;
        ShellClick('action-undo');
      end;
    5:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GFirst),
          'one ordinary Studio Undo restores both exact files from before the hosted-content Save');
        ShellClick('action-redo');
      end;
    6:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GSecond),
          'one ordinary Studio Redo restores both exact hosted-content files');
        {$ifndef PAS2JS}
        ExportPair('original', GOriginal);
        ExportPair('reusable', GFirst);
        ExportPair('hosted', GSecond);
        {$endif}
        GDone := True;
      end;
  end;
  Inc(GStage);
end;

{$ifdef PAS2JS}
procedure Tick;
begin
  try
    Drive;

    if GDone then
    begin
      GStudio.Free;
      GStudio := nil;
      document.body.setAttribute('data-diagram-content-studio', 'passed');
      document.body.setAttribute('data-diagram-content-studio-checks', IntToStr(GChecks));
    end
    else
    begin
      window.setTimeout(@Tick, 30);
    end;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-diagram-content-studio', 'failed');
      document.body.setAttribute('data-diagram-content-studio-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}

    if (ParamCount < 1) or DirectoryExists(ParamStr(1)) then
    begin
      raise Exception.Create('Supply a fresh owned Studio content fixture directory');
    end;
    Application.Initialize;
    GFailure := TFailure.Create;
    Application.OnException := GFailure.Failed;
    GForm := TForm.CreateNew(nil);
    GForm.SetBounds(20, 20, 1280, 900);
    GForm.Show;
    GStudio := TNyxNativeStudio.Create(GForm, ParamStr(1));
    GStudio.Run;
    {$else}
    GStudio := TNyxStudio.Create;
    GStudio.Run(False);
    {$endif}
    GStarted := Clock;
    {$ifdef PAS2JS}
    window.setTimeout(@Tick, 100);
    {$else}
    try
      while not GDone do
      begin
        CheckSynchronize;
        Application.ProcessMessages;
        Drive;
        Sleep(25);
      end;
      WriteLn('PASS ', GChecks, ' ordinary Studio diagram content checks');
    finally
      GStudio.Free;
      GStudio := nil;
      GContent := nil;
      GTarget := nil;
      GView := nil;
      GWindow := nil;
      CheckSynchronize;
      Application.ProcessMessages;
      GForm.Free;
      Application.OnException := nil;
      GFailure.Free;
    end;
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-content-studio', 'failed');
      document.body.setAttribute('data-diagram-content-studio-error', E.Message);
      {$else}
      WriteLn('FAIL stage ', GStage, ' / ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
