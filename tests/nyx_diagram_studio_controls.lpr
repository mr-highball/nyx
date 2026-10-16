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
}
program nyx_diagram_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.codec,
  nyx.diagram, nyx.diagram.declarations, nyx.diagram.editing, nyx.diagram.adoption,
  nyx.diagram.editor, nyx.canvas.geometry, nyx.controls, nyx.scheduler,
  nyx.studio.projects, nyx.studio.sourcejobs, nyx.studio.edits, nyx.studio.session,
  nyx.studio.transactions
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser, nyx.diagram.editor.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Graphics, nyx.studio.lcl,
    nyx.diagram.editor.lcl, nyx.test.capture.lcl{$endif};

type
  { Explicit test host holds a copied request without claiming publication.
    It has no project or editor authority beyond that UI-owned reply. }
  THeldHost = class(TInterfacedObject, INyxDiagramSaveHost)
  public
    Request: INyxDiagramSaveRequest;
    procedure Submit(const ARequest: INyxDiagramSaveRequest);
  end;

  {$ifndef PAS2JS}
  { Borrow a mounted native control solely for its installed Click handler. }
  TControlAccess = class(TControl);
  { Fixture-owned exception sink outlives all deferred native UI retirement. }
  TFailure = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
  { Joined worker retains UI interfaces. All attempts must refuse through their
    independent schedulers before borrowing a view, window or supplied document. }
  TReplyWorker = class(TThread)
  public
    Request: INyxDiagramSaveRequest;
    Window: INyxDiagramEditorWindow;
    Refusals: Integer;
    procedure Execute; override;
  end;
  {$endif}

var
  GStudio: {$ifdef PAS2JS}TNyxStudio{$else}TNyxNativeStudio{$endif};
  GWindow: INyxDiagramEditorWindow;
  GView: INyxDiagramEditorView;
  GTarget: {$ifdef PAS2JS}INyxBrowserDiagramEditorView{$else}INyxLCLDiagramEditorView{$endif};
  GBefore: TNyxProjectPair;
  GSaved1: TNyxProjectPair;
  GSaved2: TNyxProjectPair;
  GPeer: TNyxProjectPair;
  GSubmitted: TNyxText;
  GLocal: TNyxText;
  GHeld: THeldHost;
  GHeldLease: INyxDiagramSaveHost;
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

  if not AValue then
  begin
    raise Exception.Create('Studio diagram: ' + AReason);
  end;
  {$ifndef PAS2JS}

  if (GFailure <> nil) and (GFailure.Message <> '') then
  begin
    raise Exception.Create(GFailure.Message);
  end;
  {$endif}
  Inc(GChecks);
end;

procedure THeldHost.Submit(const ARequest: INyxDiagramSaveRequest);
begin
  Request := ARequest;
end;

{$ifndef PAS2JS}
procedure TFailure.Failed(ASender: TObject; AError: Exception);
begin
  Message := AError.ClassName + ': ' + AError.Message;
end;

procedure TReplyWorker.Execute;
begin
  try
    Request.Accepted(nil);
  except
    on E: ENyxSchedule do
    begin
      Inc(Refusals);
    end;
  end;
  try
    Request.Refused('worker refusal');
  except
    on E: ENyxSchedule do
    begin
      Inc(Refusals);
    end;
  end;
  try
    Window.Hide;
  except
    on E: ENyxSchedule do
    begin
      Inc(Refusals);
    end;
  end;
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

procedure BindTarget;
begin
  GView := GWindow.View;
  Check(Supports(GView, {$ifdef PAS2JS}INyxBrowserDiagramEditorView{$else}INyxLCLDiagramEditorView{$endif},
    GTarget), 'ordinary Studio mounts the public target diagram editor');
  {$ifndef PAS2JS}

  if ParamStr(2) = '--compact' then
  begin
    GetParentForm(GTarget.Renderer.ControlFor(ID(ndepSave))).SetBounds(20, 20, 390, 780);
  end;
  {$endif}
end;

procedure ShellClick(const AID: TNyxText);
begin
  {$ifdef PAS2JS}GStudio.ShellView.ElementFor(AID).click;
  {$else}TControlAccess(GStudio.ShellView.ControlFor(AID)).Click;{$endif}
end;

procedure Click(APart: TNyxDiagramEditorPart);
begin
  {$ifdef PAS2JS}GTarget.Renderer.ElementFor(ID(APart)).click;
  {$else}TControlAccess(GTarget.Renderer.ControlFor(ID(APart))).Click;{$endif}
end;

procedure WriteCaption(const AValue: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GTarget.Renderer.InputFor(ID(ndepCaption))).value := AValue;
  GTarget.Renderer.InputFor(ID(ndepCaption)).dispatchEvent(TJSEvent.new('input'));
  {$else}
  TEdit(GTarget.Renderer.InputFor(ID(ndepCaption))).Text := AValue;
  {$endif}
  Check(GView.HasPendingProperties, 'real target input retains an unfinished caption');
end;

procedure ConfirmReload;
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(GTarget.Renderer.InputFor(ID(ndepConfirmReload))).checked := True;
  GTarget.Renderer.InputFor(ID(ndepConfirmReload)).dispatchEvent(TJSEvent.new('change'));
  {$else}
  TCheckBox(GTarget.Renderer.InputFor(ID(ndepConfirmReload))).Checked := True;
  {$endif}
end;

function ReadCaption: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLInputElement(GTarget.Renderer.InputFor(ID(ndepCaption))).value;
  {$else}
  Result := TEdit(GTarget.Renderer.InputFor(ID(ndepCaption))).Text;
  {$endif}
end;

procedure Dismiss;
begin
  {$ifdef PAS2JS}
  Click(ndepClose);
  {$else}

  if GStage = 1 then
  begin
    GetParentForm(GTarget.Renderer.ControlFor(ID(ndepSave))).Close;
  end
  else
  begin
    Click(ndepClose);
  end;
  {$endif}
end;

function Definition(const APair: TNyxProjectPair): TNyxText;
var
  LDocument: TNyxDocument;
begin
  LDocument := TNyxCodec.Decode(APair.Design);
  try
    Result := LDocument.Diagrams.Definition(NyxDiagramRef('Diagram')).ToData.ToJSON;
  finally
    LDocument.Free;
  end;
end;

{$ifndef PAS2JS}
procedure ExportPair(const AName: TNyxText; const APair: TNyxProjectPair);
var
  LDirectory: TNyxText;
  LFile: TFileStream;

  procedure Save(const AFileName, AText: TNyxText);
  begin
    LFile := TFileStream.Create(LDirectory + AFileName, fmCreate);
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

procedure Drive;
var
  LRoot: TNyxNode;
  LDocument: TNyxDocument;
  LPlan: INyxDiagram;
  LEdit: TNyxStudioDesignEdit;
  LFailed: Boolean;
  LOriginalView: INyxDiagramEditorView;
  {$ifndef PAS2JS}
  LWorker: TReplyWorker;
  LForm: TCustomForm;
  {$endif}
begin

  if Clock - GStarted > 180000 then
  begin
    raise Exception.Create('Studio diagram input deadline at stage ' + IntToStr(GStage));
  end;

  if GStudio.SourceBusy or GStudio.PresentationPending then
  begin
    Exit;
  end;
  case GStage of
    0:
      begin
        LRoot := GStudio.ShellView.Root.Find('studio-diagrams-disclosure');

        if LRoot = nil then
        begin

          if GStudio.ShellView.Root.Find('action-panel-project') <> nil then
          begin
            ShellClick('action-panel-project');
          end;
          Exit;
        end;
        ShellClick(LRoot.Part(NyxPart('header')).ID);
      end;
    1:
      begin
        GBefore := GStudio.ProjectSnapshot;
        ShellClick('studio-diagram-library-new');
        GWindow := GStudio.ShowDiagram(NyxDiagramRef('Diagram'));
        BindTarget;
        Check(GWindow.IsOpen and (GView.Diagram.VertexCount = 3) and
          (EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GBefore)),
          'actual New opens an independent English proposal without changing the project');
        Click(ndepToggleDraw);
        Click(ndepAddRectangle);
        GView.SetGeometry(GView.Selection, NyxCanvasBounds(-200, 180, 160, 100), GView.Revision);
        Click(ndepStartOrthogonal);
        GView.SelectVertex(NyxDiagramVertexRef('review'));
        Click(ndepFinishConnection);
        Check((GView.Diagram.VertexCount = 4) and (GView.Diagram.EdgeCount = 3) and
          (GView.SelectedEdge.Name = 'Connection1'),
          'ordinary Studio creates a shape and connection through public Nyx authoring controls');
        GView.SelectVertex(NyxDiagramVertexRef('start'));
        Click(ndepToggleDraw);
        GView.SetGeometry(NyxDiagramVertexRef('start'), NyxCanvasBounds(40, 20, 150, 90), GView.Revision);
        GSubmitted := GView.Diagram.ToData.ToJSON;
        Click(ndepSave);
        Check(GView.SavePending and (EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GBefore)),
          'Save returns pending before ordinary paired source publication');
        GView.SetGeometry(NyxDiagramVertexRef('start'), NyxCanvasBounds(70, 35, 160, 95), GView.Revision);
        GLocal := GView.Diagram.ToData.ToJSON;
        GView.ShowTools(True);
        WriteCaption('Typed while saving');
        Dismiss;
        Check(not GWindow.IsOpen and GView.Connected, 'window dismissal hides the same mounted proposal');
        LOriginalView := GView;
        GWindow := GStudio.ShowDiagram(NyxDiagramRef('Diagram'));
        BindTarget;
        Check((GView = LOriginalView) and (GView.Diagram.ToData.ToJSON = GLocal),
          'reopening while saving preserves the exact newer local model');
      end;
    2:
      begin
        Check((GStudio.SourceCommands.State = nssApplied) and not GView.SavePending,
          'Save acknowledges the owning source command outcome: ' + GStudio.SourceCommands.Message);
        GSaved1 := GStudio.ProjectSnapshot;
        Check((Definition(GSaved1) = GSubmitted) and (GView.Diagram.ToData.ToJSON = GLocal) and
          GView.HasUnsavedChanges and GView.HasPendingProperties and
          (ReadCaption = 'Typed while saving'),
          'late acknowledgement saves only the captured model and retains newer local edits and input');
        GView.ShowTools(True);
        WriteCaption('Unfinished review note');
        Click(ndepSave);
        Check(not GView.SavePending and GView.HasPendingProperties and
          (EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GSaved1)),
          'unfinished property input refuses saving without losing the draft or accepted pair');
        Dismiss;
        GWindow := GStudio.ShowDiagram(NyxDiagramRef('Diagram'));
        BindTarget;
        Check(ReadCaption = 'Unfinished review note', 'same actual input survives dismissal/reopening');
        Click(ndepRestore);
        Click(ndepSave);
        Check(GView.SavePending, 'restored property form can submit the applied newer model');
        GStudio.SourceCommands.Cancel;
      end;
    3:
      begin
        Check(not GView.SavePending and GView.HasUnsavedChanges and
          (EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GSaved1)),
          'ordinary cancellation preserves accepted files and the applied local proposal');
        Click(ndepSave);
      end;
    4:
      begin
        Check((GStudio.SourceCommands.State = nssApplied) and not GView.SavePending and
          not GView.HasUnsavedChanges, 'a fresh Save after cancellation publishes and advances only its baseline');
        GSaved2 := GStudio.ProjectSnapshot;
        Check(Definition(GSaved2) = GLocal, 'second exact model is durable in the document');
        Dismiss;
        ShellClick('action-undo');
      end;
    5:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GSaved1),
          'one actual Studio Undo restores both exact files from before the second save');
        ShellClick('action-redo');
      end;
    6:
      begin
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GSaved2),
          'one actual Studio Redo restores the exact saved pair');
        GWindow := GStudio.ShowDiagram(NyxDiagramRef('Diagram'));
        BindTarget;
        GView.SetGeometry(NyxDiagramVertexRef('start'), NyxCanvasBounds(95, 45, 160, 95), GView.Revision);
        GLocal := GView.Diagram.ToData.ToJSON;
        LDocument := TNyxCodec.Decode(GSaved2.Design);
        try
          LPlan := LDocument.Diagrams.Definition(NyxDiagramRef('Diagram'));
          LEdit := Default(TNyxStudioDesignEdit);
          LEdit.Action := sdaTransaction;
          LEdit.View := 'home';
          LEdit.Selection := 'home';
          LEdit.Transaction := CaptureNyxProjectTransaction(NyxProjectTransaction([
            NyxDesignStep(NyxDiagramPatch([NyxDefineDiagram(NyxDiagramRef('Diagram'),
              LPlan.WithVertex(LPlan.Vertex(0).Captioned('Reviewed elsewhere'))).Expect(LPlan)]))]));
          GStudio.SourceCommands.Edit(LEdit);
        finally
          LDocument.Free;
        end;
      end;
    7:
      begin
        Check(GStudio.SourceCommands.State = nssApplied, 'another ordinary command updates the accepted definition');
        GPeer := GStudio.ProjectSnapshot;
        Click(ndepSave);
      end;
    8:
      begin
        Check((GStudio.SourceCommands.State = nssRejected) and not GView.SavePending and
          (EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GPeer)) and
          (GView.Diagram.ToData.ToJSON = GLocal) and GView.HasUnsavedChanges,
          'stale editor baseline refuses overwrite and retains both accepted and local work');
        Click(ndepReloadSaved);
        Check(GView.HasUnsavedChanges and (GView.Diagram.ToData.ToJSON = GLocal),
          'Reload warns before discarding an applied local model');
        ConfirmReload;
        Click(ndepReloadSaved);
        Check(not GView.HasUnsavedChanges and not GView.HasPendingProperties and
          (GView.Diagram.ToData.ToJSON = Definition(GPeer)),
          'confirmed Reload replaces local intent with the exact accepted definition');
        Click(ndepSave);
        Check(not GView.SavePending and not GStudio.SourceBusy,
          'unchanged local Save does not create another history command');
        {$ifndef PAS2JS}
        { Only this owned fixture is activated. The following turn's display
          capture still verifies foreground ownership and refuses if denied. }
        LForm := GetParentForm(GTarget.Renderer.ControlFor(ID(ndepSave)));
        LForm.BringToFront;
        LForm.SetFocus;
        {$endif}
      end;
    9:
      begin
        { Capture on the following actual event-loop turn, after the reload's
          visibility/layout updates have reached the host's display surface. }
        {$ifndef PAS2JS}
        LForm := GetParentForm(GTarget.Renderer.ControlFor(ID(ndepSave)));
        SaveNyxNativeCapture(LForm, IncludeTrailingPathDelimiter(ParamStr(1)) + 'diagram-window.png', ncmDisplay);
        ExportPair('saved', GSaved2);
        ExportPair('reviewed', GPeer);
        {$endif}
      end;
    10:
      begin
        GHeld := THeldHost.Create;
        GHeldLease := GHeld;
        LDocument := TNyxCodec.Decode(GPeer.Design);
        try
          GView.SavingTo(NyxDiagramSaveTarget(LDocument, NyxDiagramRef('Diagram')), GHeldLease);
        finally
          LDocument.Free;
        end;
        GView.SetGeometry(NyxDiagramVertexRef('start'), NyxCanvasBounds(110, 50, 160, 95), GView.Revision);
        Click(ndepSave);
        Check(GView.SavePending and (GHeld.Request <> nil), 'explicit host retains a request without claiming durable publication');
        LFailed := False;
        LDocument := TNyxCodec.Decode(GPeer.Design);
        try
          try
            GHeld.Request.Accepted(LDocument);
          except
            on E: ENyxDiagram do
            begin
              LFailed := True;
            end;
          end;
        finally
          LDocument.Free;
        end;
        Check(LFailed and GView.SavePending, 'acknowledgement must match the submitted complete definition');
        {$ifndef PAS2JS}
        LWorker := TReplyWorker.Create(True);
        try
          LWorker.Request := GHeld.Request;
          LWorker.Window := GWindow;
          LWorker.Start;
          LWorker.WaitFor;
          Check(LWorker.Refusals = 3, 'actual worker replies and dismissal refuse before borrowed UI owner access');
        finally
          LWorker.Free;
        end;
        {$endif}
        GWindow.Disconnect;
        Check(not GView.Connected and not GHeld.Request.Pending and not GHeld.Request.Accepted(nil),
          'retirement revokes the receiver and late publication acknowledgement');
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = EncodeNyxProject(GPeer),
          'reply/retirement qualification mutates no accepted project');
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
      document.body.setAttribute('data-diagram-studio', 'passed');
      document.body.setAttribute('data-diagram-studio-checks', IntToStr(GChecks));
    end
    else
    begin
      window.setTimeout(@Tick, 25);
    end;
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-diagram-studio', 'failed');
      document.body.setAttribute('data-diagram-studio-error', E.Message);
      document.body.setAttribute('data-diagram-studio-stage', IntToStr(GStage));
    end;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}

    if (ParamCount < 1) or DirectoryExists(ParamStr(1)) then
    begin
      raise Exception.Create('Supply a fresh owned Studio fixture directory');
    end;
    Application.Initialize;
    GFailure := TFailure.Create;
    Application.OnException := GFailure.Failed;
    GForm := TForm.CreateNew(nil);

    if ParamStr(2) = '--compact' then
    begin
      GForm.SetBounds(20, 20, 390, 850);
    end
    else
    begin
      GForm.SetBounds(20, 20, 1280, 900);
    end;
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
      WriteLn('PASS ', GChecks, ' ordinary Studio diagram checks');
    finally
      GStudio.Free;
      GStudio := nil;
      GTarget := nil;
      GView := nil;
      GWindow := nil;
      GHeldLease := nil;
      GForm.Free;
      Application.OnException := nil;
      GFailure.Free;
    end;
    {$endif}
  except
    on E: Exception do
    begin
      {$ifndef PAS2JS}
      WriteLn('FAIL stage ', GStage, ' / ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$else}
      document.body.setAttribute('data-diagram-studio', 'failed');
      document.body.setAttribute('data-diagram-studio-error', E.Message);
      {$endif}
    end;
  end;
end.
