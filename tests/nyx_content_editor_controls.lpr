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
program nyx_content_editor_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, Spin, Graphics, IntfGraphics,
  FPWritePNG, nyx.render.lcl, nyx.studio.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.content, nyx.behavior,
  nyx.content.editor, nyx.data, nyx.codec, nyx.presentations, nyx.responsive, nyx.schema,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
  nyx.studio.view, nyx.studio.inspector, nyx.generated.view;

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  TControlAccess = class(TControl);
  {$endif}

  { This is an ordinary Properties composition/queue consumer, not a second
    editor implementation. The blueprint is compiled unchanged from bounded MCP
    export; explicit test helpers dispatch physical controls and inspect results.
    It does not connect, import into, or replace a running user's Studio pair. }
  TReview = class
  private
    FHost: THost;
    FPreviewHost: THost;
    FRenderer: TRenderer;
    FPreview: TRenderer;
    FShell: TNyxDocument;
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FStage: Integer;
    FBefore: TNyxText;
    FAfter: TNyxText;
    procedure Refresh;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Change(AField: TNyxContentEditorField; const AValue: TNyxText);
    procedure Click(const AID: TNyxText);
    procedure Preview(AWidth, AHeight: Integer; AManual: Boolean);
    function Pair: TNyxText;
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
  end;

var
  GReview: TReview;
  GChecks: Integer;
  GPolls: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

constructor TReview.Create(const ASource: TNyxText);
var
  LDocument: TNyxDocument;
begin
  inherited Create;
  LDocument := BuildNyxDocument;
  try
    FSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument), ASource));
  finally
    LDocument.Free;
  end;
  FSession.Select('workspace');
  FCommands := TNyxSourceCommands.Create(FSession, {$ifdef PAS2JS}@{$endif}Changed);
  FRenderer := TRenderer.Create;
  FPreview := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('div'));
  FHost.style.cssText := 'height:900px;overflow:auto;max-width:680px;';
  FPreviewHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(FHost);
  document.body.appendChild(FPreviewHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(10, 10, 700, 940);
  FHost.Show;
  FPreviewHost := TForm.CreateNew(nil);
  FPreviewHost.SetBounds(730, 10, 900, 600);
  FPreviewHost.Show;
  {$endif}
  Refresh;
end;

destructor TReview.Destroy;
begin
  FCommands.Free;
  FPreview.Free;
  FRenderer.Free;
  FShell.Free;
  FSession.Free;
  {$ifdef PAS2JS}
  FPreviewHost.remove;
  FHost.remove;
  {$else}
  FPreviewHost.Free;
  FHost.Free;
  {$endif}
  inherited Destroy;
end;

function TReview.Pair: TNyxText;
begin
  Result := EncodeNyxProject(FSession.ProjectSnapshot);
end;

procedure TReview.Refresh;
var
  LShell: TNyxDocument;
  LState: TNyxStudioViewState;
begin
  LState := DefaultNyxStudioViewState;
  LState.InspectorTab := nitProperties;
  LShell := BuildNyxStudioView(FSession, LState);
  try
    { Render the ordinary inspector subtree in an owned harness host. Its real
      controls and callbacks are unchanged; this is not a full Studio deployment
      or an observing HTTP qualification. }
    FRenderer.Render(LShell, LShell.Find('studio-right'), FHost);
    FreeAndNil(FShell);
    FShell := LShell;
    LShell := nil;
  finally
    LShell.Free;
  end;
end;

procedure TReview.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin

  if AState = nssApplied then
  begin
    Refresh;
  end;
end;

procedure TReview.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  FCommands.Route(ANode, AEvent.Trigger, FRenderer.Root);
end;

procedure TReview.Change(AField: TNyxContentEditorField; const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}
  LInput: TJSHTMLElement;
  {$else}
  LInput: TControl;
  LChoice: TComboBox;
  {$endif}
begin
  LID := NyxContentEditorFieldID('inspector-content', AField);
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'The ordinary recipe field is physically mounted: ' + LID);
  {$ifdef PAS2JS}
  TJSHTMLInputElement(LInput).value := AValue;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}

  if LInput is TSpinEdit then
  begin
    TSpinEdit(LInput).Value := StrToInt(AValue);
    TSpinEdit(LInput).OnChange(LInput);
  end
  else
  begin
    LChoice := TComboBox(LInput);
    LChoice.ItemIndex := LChoice.Items.IndexOf(AValue);
    LChoice.OnChange(LChoice);
  end;
  {$endif}
  Check(FRenderer.Root.Find(LID).Prop('value') = AValue, 'Physical input copies its exact choice');
end;

procedure TReview.Click(const AID: TNyxText);
begin
  {$ifdef PAS2JS}
  FRenderer.ElementFor(AID).click;
  {$else}
  TControlAccess(FRenderer.ControlFor(AID)).Click;
  {$endif}
end;

procedure TReview.Preview(AWidth, AHeight: Integer; AManual: Boolean);
begin
  {$ifdef PAS2JS}
  FPreviewHost.style.cssText := 'width:' + IntToStr(AWidth) + 'px;height:' +
    IntToStr(AHeight) + 'px;overflow:auto;';
  {$else}
  FPreviewHost.ClientWidth := AWidth;
  FPreviewHost.ClientHeight := AHeight;
  {$endif}
  FPreview.Render(FSession.Document, FSession.Document.Pages[0], FPreviewHost);

  if AManual then
  begin
    FPreview.PresentationSelection := TNyxPresentationSelection.Use(NyxPresentation('focused'));
  end;
end;

function TReview.Step: Boolean;
var
  LBefore: TNyxText;
  LEdit: TNyxStudioDesignEdit;
  LRefused: Boolean;
  LRequest: TNyxStudioDesignRequest;
  LData: TNyxDataValue;
begin
  Result := False;

  if FCommands.Busy then
  begin
    Exit;
  end;

  if (FStage in [3, 5]) and
    (FPreview.PresentationSelection.Reference.Name <> 'focused') then
  begin
    Exit;
  end;
  case FStage of
    0:
      begin
        Check(FRenderer.Root.Find('inspector-content') <> nil, 'Ordinary Properties contains the public recipe editor');
        Check(FSession.Selected.Content.Count = 0, 'The semantic seed retains its legacy default without fabricated rules');
        FBefore := Pair;
        Change(ncfScope, 'Available size');
        Change(ncfRecipe, 'compact-card');
        Change(ncfWidthMaximum, '700');
        Change(ncfHeightMaximum, '500');
        Change(ncfOrientation, 'Landscape');
        Click(NyxContentEditorFieldID('inspector-content', ncfApply));
        Check(FCommands.Busy and (Pair = FBefore), 'Apply enqueues independent work without mutating the pair inline');
      end;
    1:
      begin
        Check(FCommands.State = nssApplied, 'Ordinary controls reach isolated paired admission: ' + FCommands.Message);
        Check(FSession.Selected.Content.Count = 1, 'One size scope is admitted');
        Check((FSession.Selected.Content.Rule(0).Viewport.WidthMaximum = 700) and
          (FSession.Selected.Content.Rule(0).Viewport.HeightMaximum = 500) and
          (FSession.Selected.Content.Rule(0).Viewport.OrientationValue = nvoLandscape),
          'Width, height and orientation remain strongly typed');
        Check((Pos('.WhenViewport(', FSession.Source) > 0) and
          (Pos('.Use(NyxComponent(''compact-card''))', FSession.Source) > 0),
          'Adjacent Pascal expresses a typed reusable recipe choice');
        FAfter := Pair;
        FSession.Undo;
        Check(Pair = FBefore, 'One Undo restores both exact accepted files');
        FSession.Redo;
        Check(Pair = FAfter, 'One Redo restores both exact accepted files');
        Refresh;
        Preview(650, 400, False);
        Check(FPreview.Root.Find(NyxQualifiedID('workspace', 'compact-name')) <> nil,
          'Actual target preview chooses the compact structure');
        Preview(900, 600, False);
        Check(FPreview.Root.Find(NyxQualifiedID('workspace', 'comfortable-heading')) <> nil,
          'Actual target preview preserves the default structure outside the condition');
        Change(ncfScope, 'Named presentation');
        Change(ncfRecipe, 'reading-card');
        Click(NyxContentEditorFieldID('inspector-content', ncfApply));
      end;
    2:
      begin
        Check((FCommands.State = nssApplied) and (FSession.Selected.Content.Count = 2),
          'Named presentation is a second independent scope');
        Preview(900, 600, True);
      end;
    3:
      begin
        Check(FPreview.Root.Find(NyxQualifiedID('workspace', 'reading-notes')) <> nil,
          'Actual named presentation selects a different control family');
        Change(ncfScope, 'Named presentation');
        Change(ncfPlatform, 'Native LCL');
        Change(ncfRecipe, 'compact-card');
        Click(NyxContentEditorFieldID('inspector-content', ncfApply));
      end;
    4:
      begin
        Check((FCommands.State = nssApplied) and (FSession.Selected.Content.Count = 3),
          'Typed platform scope is admitted independently');
        Preview(900, 600, True);
      end;
    5:
      begin
        {$ifdef PAS2JS}
        Check(FPreview.Root.Find(NyxQualifiedID('workspace', 'reading-notes')) <> nil,
          'Browser keeps the common presentation recipe');
        {$else}
        Check(FPreview.Root.Find(NyxQualifiedID('workspace', 'compact-name')) <> nil,
          'Native consumes its explicit presentation recipe');
        {$endif}
        FBefore := Pair;
        Click('inspector-content-rule-2-remove');
      end;
    6:
      begin
        Check((FCommands.State = nssApplied) and (FSession.Selected.Content.Count = 2),
          'Removal deletes only the exact registered scope');
        FAfter := Pair;
        FSession.Undo;
        Check(Pair = FBefore, 'Removal has one paired Undo');
        FSession.Redo;
        Check(Pair = FAfter, 'Removal has exact paired Redo');
        Refresh;
        LBefore := Pair;
        Change(ncfScope, 'Available size');
        Change(ncfWidthMinimum, '800');
        Change(ncfWidthMaximum, '700');
        LRefused := False;
        try
          CaptureNyxContentInspector(FSession,
            FRenderer.Root.Find(NyxContentEditorFieldID('inspector-content', ncfApply)),
            FRenderer.Root, LEdit);
        except
          on LError: Exception do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused and (Pair = LBefore), 'Invalid interval refuses without changing either file');
        Change(ncfWidthMinimum, '0');
        Change(ncfWidthMaximum, '400');
        Check(CaptureNyxContentInspector(FSession,
          FRenderer.Root.Find(NyxContentEditorFieldID('inspector-content', ncfApply)),
          FRenderer.Root, LEdit), 'Typed queued intent is captured from actual controls');
        LRequest := FSession.PrepareDesignRequest(LEdit, NyxSchemaRevision);
        LData := LRequest.ToData;
        Check((LData.Field('version').AsInteger = 10) and
          LRequest.SameRequest(ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LData.ToJSON))),
          'Copied recipe command survives its independent worker envelope');
        FBefore := Pair;
        { Two clicks capture the same mounted registry. The second must refuse
          after the first publishes, rather than erase its new choice. }
        FCommands.Edit(LEdit);
        FCommands.Edit(LEdit);
      end;
    7:
      begin
        Check(FCommands.State = nssRejected, 'Queued stale registry refuses: ' + FCommands.Message);
        Check(FSession.Selected.Content.Count = 3, 'The earlier queued choice remains accepted');
        FAfter := Pair;
        FSession.Undo;
        Check(Pair = FBefore, 'Stale refusal adds no history step');
        FSession.Redo;
        Check(Pair = FAfter, 'Accepted queue result retains exact Redo');
        Result := True;
      end;
  end;
  Inc(FStage);
end;

procedure Drive;
{$ifndef PAS2JS}
var
  LFinalStream: TFileStream;
  LFinalSource: TNyxText;
{$endif}
begin
  try
    Inc(GPolls);

    if GPolls > 2000 then
    begin
      raise Exception.Create('Recipe editor queue did not finish');
    end;

    if GReview.Step then
    begin
      {$ifndef PAS2JS}
      if ParamCount > 1 then
      begin
        LFinalSource := GReview.FSession.Source;
        LFinalStream := TFileStream.Create(ParamStr(2), fmCreate);
        try
          LFinalStream.WriteBuffer(LFinalSource[1], Length(LFinalSource));
        finally
          LFinalStream.Free;
        end;
      end;
      FreeAndNil(GReview);
      {$endif}
      WriteLn('PASS ', GChecks, ' actual Studio recipe editor/queue checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'passed');
      document.body.setAttribute('data-projection-refresh-checks', IntToStr(GChecks));
      {$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(@Drive, 25);
    end{$endif};
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'failed');
      document.body.setAttribute('data-projection-refresh-error', LError.Message);
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
      FreeAndNil(GReview);
    end;
  end;
end;

{$ifndef PAS2JS}
{ The ordinary native controller must consume the same library editor and queue,
  update its retained Pascal input and preserve one paired history step. These
  controls are reached through actual widget callbacks, with no HTTP connection. }
procedure RunNativeStudio(const ASource: TNyxText);
var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LDocument: TNyxDocument;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LCode: TControl;

  procedure Ready;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Ordinary native Studio did not finish: ' + LStudio.Status);
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceCommands.Busy;
  end;

  procedure Button(const AID: TNyxText);
  begin
    TControlAccess(LStudio.ShellView.ControlFor(AID)).Click;
    Ready;
  end;

  procedure Choice(AField: TNyxContentEditorField; const AValue: TNyxText);
  var
    LChoice: TComboBox;
  begin
    LChoice := TComboBox(LStudio.ShellView.InputFor(
      NyxContentEditorFieldID('inspector-content', AField)));
    LChoice.ItemIndex := LChoice.Items.IndexOf(AValue);
    LChoice.OnChange(LChoice);
  end;

  procedure Capture(const AName: TNyxText);
  var
    LBitmap: TBitmap;
    LImage: TLazIntfImage;
    LWriter: TFPWriterPNG;
  begin

    if ParamCount < 3 then
    begin
      Exit;
    end;
    ForceDirectories(ParamStr(3));
    LBitmap := TBitmap.Create;
    LImage := nil;
    LWriter := nil;
    try
      LBitmap.SetSize(LForm.ClientWidth, LForm.ClientHeight);
      LForm.PaintTo(LBitmap.Canvas, 0, 0);
      LImage := LBitmap.CreateIntfImage;
      LWriter := TFPWriterPNG.Create;
      LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(3)) + AName + '.png', LWriter);
    finally
      LWriter.Free;
      LImage.Free;
      LBitmap.Free;
    end;
  end;

begin
  LForm := TForm.CreateNew(nil);
  LStudio := nil;
  try
    LForm.SetBounds(20, 20, 1280, 940);
    LForm.Show;
    LStudio := TNyxNativeStudio.Create(LForm, ExpandFileName(ParamStr(1)) + '-studio-projects');
    LDocument := BuildNyxDocument;
    try
      LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), ASource));
    finally
      LDocument.Free;
    end;
    LStudio.Session.Select('workspace');
    LStudio.Run;
    Ready;
    Check(LStudio.ShellView.Root.Find('inspector-content') <> nil,
      'Ordinary native Studio mounts the reusable recipe editor');
    Button('action-code');
    LCode := LStudio.CodeView.InputFor('studio-code');
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Choice(ncfScope, 'Named presentation');
    Choice(ncfRecipe, 'reading-card');
    Button(NyxContentEditorFieldID('inspector-content', ncfApply));
    Check((LStudio.SourceCommands.State = nssApplied) and
      (LStudio.Session.Selected.Content.Count = 1),
      'Ordinary native callback admits the recipe through its own queued processor');
    Check((LStudio.CodeView.InputFor('studio-code') = LCode) and
      (Pos('.WhenPresentation(', TMemo(LCode).Text) > 0),
      'Ordinary native Studio updates its retained Pascal editor');
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Button('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Actual native Undo button restores the exact paired seed');
    Button('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'Actual native Redo button restores the exact paired choice');
    Capture('studio-recipe-desktop');
    LForm.ClientWidth := 390;
    Ready;
    Button('action-panel-inspector');
    Capture('studio-recipe-compact');
    Check(LStudio.Session.SelectedID = 'workspace', 'Compact panel navigation retains the recipe owner');
  finally
    LStudio.Free;
    LForm.Free;
  end;
  WriteLn('PASS ', GChecks, ' recipe editor checks including ordinary native Studio');
end;
{$endif}

{$ifdef PAS2JS}
var
  GRequest: TJSXMLHttpRequest;
  GCompactFrame: TJSHTMLIFrameElement;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;

  if GRequest.status <> 200 then
  begin
    document.body.setAttribute('data-projection-refresh', 'failed');
    document.body.setAttribute('data-projection-refresh-error', 'Exact MCP source export is unavailable');
    Exit;
  end;
  GReview := TReview.Create(GRequest.responseText);
  Drive;
end;

function ReviewClosed(AEvent: TJSEvent): Boolean;
begin
  FreeAndNil(GReview);
  Result := True;
end;

{ Exact narrow viewport uses another Pascal consumer in a bounded iframe. The
  host only observes result markers; it never authors the child document or
  substitutes screenshot-driven editor automation for semantic composition. }
procedure ObserveCompact;
var
  LBody: TJSHTMLElement;
  LResult: TNyxText;
begin
  Inc(GPolls);

  if GCompactFrame.contentDocument <> nil then
  begin
    LBody := TJSHTMLElement(GCompactFrame.contentDocument.body);
    LResult := LBody.getAttribute('data-projection-refresh');

    if (LResult = 'passed') or (LResult = 'failed') then
    begin
      document.body.setAttribute('data-projection-refresh', LResult);
      document.body.setAttribute('data-projection-refresh-checks',
        LBody.getAttribute('data-projection-refresh-checks'));
      document.body.setAttribute('data-projection-refresh-error',
        LBody.getAttribute('data-projection-refresh-error'));
      Exit;
    end;
  end;

  if GPolls > 2000 then
  begin
    document.body.setAttribute('data-projection-refresh', 'failed');
    document.body.setAttribute('data-projection-refresh-error', 'Exact-390 recipe editor did not finish');
    Exit;
  end;
  window.setTimeout(@ObserveCompact, 100);
end;
{$else}
var
  LStream: TFileStream;
  LSource: TNyxText;
{$endif}

begin
  {$ifdef PAS2JS}

  if window.location.search = '?compact=1' then
  begin
    GCompactFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    GCompactFrame.style.cssText := 'width:390px;height:940px;border:0;display:block;';
    GCompactFrame.src := window.location.pathname;
    document.body.appendChild(GCompactFrame);
    window.setTimeout(@ObserveCompact, 100);
  end
  else
  begin
    GRequest := TJSXMLHttpRequest.new;
    GRequest.open('GET', 'seed.pas.txt', True);
    GRequest.onload := @Loaded;
    GRequest.send;
    window.addEventListener('pagehide', @ReviewClosed);
  end;
  {$else}
  Application.Initialize;
  LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LSource, LStream.Size);
    LStream.ReadBuffer(LSource[1], Length(LSource));
  finally
    LStream.Free;
  end;
  GReview := TReview.Create(LSource);
  while GReview <> nil do
  begin
    Drive;
    Application.ProcessMessages;
    CheckSynchronize(10);
  end;
  CheckSynchronize;

  if ExitCode = 0 then
  begin
    RunNativeStudio(LSource);
  end;
  {$endif}
end.
