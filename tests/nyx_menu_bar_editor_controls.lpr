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

program nyx_menu_bar_editor_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, Spin, Graphics, IntfGraphics,
  FPWritePNG, nyx.render.lcl, nyx.studio.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.menu.editor,
  nyx.menu.bar.editor, nyx.menu.bar.declarations, nyx.menu.declarations,
  nyx.menu.types, nyx.typeahead, nyx.data, nyx.codec, nyx.codegen, nyx.schema,
  nyx.behavior, nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
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
  { The compiled companion is the exact saved semantic export. The fixture owns
    its project/host; it uses ordinary Studio composition and the independent
    source worker queue, never a user's live project or a fake publisher. }
  TReview = class
  private
    FHost: THost;
    FRenderer: TRenderer;
    FShell: TNyxDocument;
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FState: TNyxStudioViewState;
    FStage: Integer;
    FBefore: TNyxText;
    procedure Refresh;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Change(const APrefix: TNyxText; AField: TNyxMenuBarEditorField;
      const AValue: TNyxText);
    procedure Click(AAction: TNyxMenuBarEditorAction;
      const APrefix: TNyxText = 'inspector-menu-bar');
    function Pair: TNyxText;
    procedure History;
    procedure Refused(AAction: TNyxMenuBarEditorAction;
      const APrefix: TNyxText = 'inspector-menu-bar');
    procedure DraftBoundary;
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
  end;

const
  CEditor = 'inspector-menu-bar';
  CUnicode: TNyxText = 'My workspace 🧭 / 👩‍💻';
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
  FSession.Activate('menu-workspace');
  FSession.Select('workspace-menu-bar');
  FState := DefaultNyxStudioViewState;
  FCommands := TNyxSourceCommands.Create(FSession, {$ifdef PAS2JS}@{$endif}Changed);
  FRenderer := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('div'));
  FHost.style.cssText := 'height:900px;overflow:auto;max-width:680px;';
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(10, 10, 700, 940);
  FHost.Show;
  {$endif}
  Refresh;
  {$ifdef PAS2JS}
  FRenderer.ElementFor(CEditor).scrollIntoView;
  document.body.setAttribute('data-capture-checkpoint', 'bar-editor-form');
  {$endif}
end;

destructor TReview.Destroy;
begin
  FCommands.Free;
  FRenderer.Free;
  FShell.Free;
  FSession.Free;
  {$ifdef PAS2JS}FHost.remove;{$else}FHost.Free;{$endif}
  inherited Destroy;
end;

function TReview.Pair: TNyxText;
begin
  Result := EncodeNyxProject(FSession.ProjectSnapshot);
end;

procedure TReview.Refresh;
var
  LShell: TNyxDocument;
begin
  { Match ordinary shell draft ownership before replacing disposable controls. }
  FState.MenuBarEditorDraft.Capture(CEditor, FRenderer.Root);
  LShell := BuildNyxStudioView(FSession, FState);
  try
    FState.MenuBarEditorDraft.Restore(LShell.Pages[0]);
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
  FCommands.Route(ANode, AEvent, FRenderer.Root);
end;

procedure TReview.Change(const APrefix: TNyxText; AField: TNyxMenuBarEditorField;
  const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LID := NyxMenuBarEditorFieldID(APrefix, AField);
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'Bar field is physically mounted: ' + LID);
  {$ifdef PAS2JS}

  if FRenderer.Root.Find(LID).ProjectionKind = NyxKindName(nkCheckbox) then
  begin
    TJSHTMLInputElement(LInput).checked := AValue = 'true';
  end
  else
  begin
    TJSHTMLInputElement(LInput).value := AValue;
  end;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}

  if LInput is TSpinEdit then
  begin
    TSpinEdit(LInput).Value := StrToInt(AValue);
    TSpinEdit(LInput).OnChange(LInput);
  end
  else if LInput is TComboBox then
  begin
    TComboBox(LInput).ItemIndex := TComboBox(LInput).Items.IndexOf(AValue);
    TComboBox(LInput).OnChange(LInput);
  end
  else if LInput is TCheckBox then
  begin
    TCheckBox(LInput).Checked := AValue = 'true';
    TCheckBox(LInput).OnChange(LInput);
  end
  else
  begin
    TEdit(LInput).Text := AValue;
    TEdit(LInput).OnChange(LInput);
  end;
  {$endif}
  Check(FRenderer.Root.Find(LID).Prop('value') = AValue,
    'Actual adapter retains the exact typed field value / ' + LID +
      ' / expected ' + AValue + ' / actual ' + FRenderer.Root.Find(LID).Prop('value'));
end;

procedure TReview.Click(AAction: TNyxMenuBarEditorAction; const APrefix: TNyxText);
var
  LID: TNyxText;
begin
  LID := NyxMenuBarEditorActionID(APrefix, AAction);
  {$ifdef PAS2JS}FRenderer.ElementFor(LID).click;{$else}
  TControlAccess(FRenderer.ControlFor(LID)).Click;{$endif}
end;

procedure TReview.History;
var
  LAfter: TNyxText;
begin
  LAfter := Pair;
  FSession.Undo;
  Check(Pair = FBefore, 'One Undo restores the exact accepted design/source pair');
  FSession.Redo;
  Check(Pair = LAfter, 'One Redo restores the exact complete bar candidate');
  Refresh;
end;

procedure TReview.Refused(AAction: TNyxMenuBarEditorAction; const APrefix: TNyxText);
var
  LEdit: TNyxStudioDesignEdit;
  LFailed: Boolean;
  LBefore: TNyxText;
begin
  LBefore := Pair;
  LFailed := False;
  try
    CaptureNyxMenuBarInspector(FSession,
      FRenderer.Root.Find(NyxMenuBarEditorActionID(APrefix, AAction)), FRenderer.Root, LEdit);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (Pair = LBefore), 'Invalid/unreviewed capture preserves both accepted files');
end;

procedure TReview.DraftBoundary;
var
  LForm: INyxCard;
  LReplacement: INyxCard;
  LDraft: TNyxMenuBarEditorDraft;
  LCopy: TNyxMenuBarEditorDraft;
  LDocument: TNyxDocument;
  LRow: TNyxNode;
  LParent: TNyxNode;
  LInstance: INyxComponent;
  LSibling: INyxComponent;
  LPlan: INyxMenuBarDefinition;
  LChange: TNyxMenuBarEditorChange;
  LIndex: Integer;
  LBefore: TNyxText;
begin
  LBefore := Pair;
  LDocument := FSession.Document.Clone;
  try
    LRow := LDocument.Find('workspace-menu-bar');
    LParent := LRow.Parent;
    for LIndex := 0 to LParent.Count - 1 do
    begin

      if LParent.Children[LIndex] = LRow then
      begin
        LDocument.AddComponent(LParent.Extract(LIndex));
        Break;
      end;
    end;
    LInstance := NewNyxComponent('first-bar');
    LInstance.Configure.Component(NyxComponent('workspace-menu-bar')).Done;
    LParent.Add(LInstance.Node);
    LSibling := NewNyxComponent('second-bar');
    LSibling.Configure.Component(NyxComponent('workspace-menu-bar')).Done;
    LParent.Add(LSibling.Node);
    ValidateNyxDocumentProperties(LDocument);
    LForm := NewNyxMenuBarEditor('draft-bar', NyxControl('first-bar'), LDocument);
    Check(LForm.Part(NyxPart('label')).Node.Kind = NyxKindName(nkInput),
      'Creators can customize the specialized policy control through its named part');
    Check(LForm.Part(NyxPart('heading-0/menu')).Node.Kind = NyxKindName(nkSelect),
      'Heading cards expose nested named parts through the public compound contract');
    Check(Pos('Inherited', LForm.Node.Find('draft-bar-status').Prop('text')) = 1,
      'Public compound visibly resolves effective inherited grouping');
    LForm.Node.Find(NyxMenuBarEditorFieldID('draft-bar', nbfLabel)).SetProp('value', 'A careful draft');
    LForm.Node.Find(NyxMenuBarEditorFieldID('draft-bar', nbfSearchWindow)).SetProp('value', '-');
    LDraft.Capture('draft-bar', LForm.Node);
    LCopy := LDraft;
    LForm := nil;
    Check(not LDraft.Restore(nil), 'Absent panel parks its owned scalar draft');
    LReplacement := NewNyxMenuBarEditor('draft-bar', NyxControl('first-bar'), LDocument);
    Check(LDraft.Restore(LReplacement.Node), 'Destroyed form restores the same exact inherited context');
    Check(LReplacement.Node.Find(NyxMenuBarEditorFieldID('draft-bar', nbfSearchWindow)).Prop('value') = '-',
      'Incomplete numeric input survives without becoming application state');
    LDraft.Clear;
    Check(LCopy.Restore(LReplacement.Node), 'Clearing a copied snapshot preserves the independent draft');
    LReplacement := NewNyxMenuBarEditor('draft-bar', NyxControl('second-bar'), LDocument);
    LDraft := LCopy;
    Check(not LDraft.Restore(LReplacement.Node), 'A draft refuses another component instance');
    LForm := NewNyxMenuBarEditor('draft-bar', NyxControl('first-bar'), LDocument);
    LForm.Node.Find(NyxMenuBarEditorFieldID('draft-bar', nbfLabel)).SetProp('value', 'First component');
    Check(CaptureNyxMenuBarEditor(LForm.Node.Find(NyxMenuBarEditorActionID('draft-bar', nmbSave)),
      LForm.Node, LChange, LPlan), 'Inherited form captures an independent typed local override');
    LInstance.Configure.MenuBar(LPlan).Done;
    LReplacement := NewNyxMenuBarEditor('draft-bar', NyxControl('first-bar'), LDocument);
    LDraft := LCopy;
    Check(not LDraft.Restore(LReplacement.Node), 'Changed local/effective baseline retires incomplete input');
    Check((LInstance.Node.MenuBar.Options.Caption = 'First component') and
      (LRow.MenuBar.Options.Caption = 'Workspace commands') and not LSibling.Node.HasMenuBar,
      'Local capture leaves the reusable definition and sibling independently owned');
  finally
    LForm := nil;
    LReplacement := nil;
    LInstance := nil;
    LSibling := nil;
    LDocument.Free;
  end;
  Check(Pair = LBefore, 'Library draft/reuse fixture never edits the actual consumer pair');
end;

function TReview.Step: Boolean;
var
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LBar: INyxMenuBarDefinition;
begin
  Result := False;

  if FCommands.Busy then
  begin
    Exit;
  end;
  {$ifndef PAS2JS}WriteLn('Bar editor stage ', FStage); Flush(Output);{$endif}
  case FStage of
    0:
      begin
        {$ifdef PAS2JS}

        if document.body.getAttribute('data-capture-observed') <> 'bar-editor-form' then
        begin
          Exit;
        end;
        {$endif}
        Check(FRenderer.Root.Find(CEditor) <> nil, 'Ordinary Studio inspector consumes the public bar compound');
        DraftBoundary;
        Change(CEditor, nbfLabel, CUnicode);
        Refresh;
        Check(FRenderer.Root.Find(NyxMenuBarEditorFieldID(CEditor, nbfLabel)).Prop('value') = CUnicode,
          'Actual disposable shell refresh retains unsaved Unicode');
        Change(CEditor, nbfWrap, 'true');
        Change(CEditor, nbfHover, 'true');
        Change(CEditor, nbfSearch, 'false');
        Change(CEditor, nbfSearchWindow, '2345');
        Change(CEditor, nbfSearchMatch, 'Unicode folded');
        Change(NyxMenuBarEditorHeadingID(CEditor, 0), nbfMenu, '1 / density');
        Change(NyxMenuBarEditorHeadingID(CEditor, 0), nbfEnabled, 'false');
        Check(CaptureNyxMenuBarInspector(FSession,
          FRenderer.Root.Find(NyxMenuBarEditorActionID(CEditor, nmbSave)), FRenderer.Root, LEdit),
          'Physical form captures complete typed bar intent');
        LRequest := FSession.PrepareDesignRequest(LEdit, NyxSchemaRevision);
        Check(LEdit.Menu.IsMenuBar and LRequest.SameRequest(
          ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LRequest.ToData.ToJSON))),
          'Typed group and baseline survive the exact independent worker protocol');
        FBefore := Pair;
        FCommands.Edit(LEdit);
        FCommands.Edit(LEdit);
      end;
    1:
      begin
        Check(FCommands.State = nssRejected, 'Second queued mounted baseline refuses after first publication');
        LBar := FSession.Selected.MenuBar;
        Check((LBar.Options.Caption = CUnicode) and LBar.Options.Wraps and LBar.Options.Hovers and
          not LBar.Options.Search.IsEnabled and (LBar.Options.Search.WindowMS = 2345) and
          (LBar.Options.Search.MatchMode = ntmFolded) and (LBar.Item(0).Menu.Name = 'density') and
          not LBar.Item(0).IsEnabled, 'Whole-form policy and heading defaults retain exact values');
        Check(Pos(CUnicode, FSession.Source) > 0, 'Crafted adjacent Pascal preserves supplementary Unicode');
        History;
        FRenderer.Root.Find(NyxMenuBarEditorFieldID(CEditor, nbfSearchMatch)).SetProp('value', 'Unknown');
        Refused(nmbSave);
        Change(CEditor, nbfSearchMatch, 'Unicode folded');
        FBefore := Pair;
        Click(nmbMoveLater, NyxMenuBarEditorHeadingID(CEditor, 0));
      end;
    2:
      begin
        Check((FCommands.State = nssApplied) and (FSession.Selected.MenuBar.Item(0).Part.Name = 'edit'),
          'Actual reorder replaces the whole saved plan / state ' +
            TNyxText(IntToStr(Ord(FCommands.State))) + ' / ' + FCommands.Message +
            ' / first ' + FSession.Selected.MenuBar.Item(0).Part.Name);
        History;
        Refused(nmbRemoveHeading, NyxMenuBarEditorHeadingID(CEditor, 0));
        Change(NyxMenuBarEditorHeadingID(CEditor, 0), nbfConfirm, 'true');
        FBefore := Pair;
        Click(nmbRemoveHeading, NyxMenuBarEditorHeadingID(CEditor, 0));
      end;
    3:
      begin
        Check((FCommands.State = nssApplied) and (FSession.Selected.MenuBar.Count = 3),
          'Reviewed physical heading removal is admitted as one candidate');
        History;
        Change(CEditor + TNyxText('-new-heading'), nbfEnabled, 'false');
        FBefore := Pair;
        Click(nmbAddHeading);
      end;
    4:
      begin
        Check((FCommands.State = nssApplied) and (FSession.Selected.MenuBar.Count = 4) and
          (FSession.Selected.MenuBar.Item(3).Part.Name = 'edit') and
          not FSession.Selected.MenuBar.Item(3).IsEnabled,
          'New heading offers the unused exact part and retains its logical default');
        History;
        Refused(nmbMask);
        Change(CEditor, nbfConfirm, 'true');
        FBefore := Pair;
        Click(nmbMask);
      end;
    5:
      begin
        Check((FCommands.State = nssApplied) and FSession.Selected.HasMenuBar and
          (FSession.Selected.MenuBar = nil) and (FSession.Document.Menus.Count = 3),
          'Reviewed local suppression retains menu definitions and explicit mask meaning');
        History;
        FBefore := Pair;
        Click(nmbInherit);
      end;
    6:
      begin
        Check((FCommands.State = nssApplied) and not FSession.Selected.HasMenuBar,
          'Physical restore removes only the local declaration');
        History;
        FSession.SetSourceDraft(FSession.Source + #10 + TNyxText('{ Pending user draft }'));
        FBefore := Pair;
        Check(CaptureNyxMenuBarInspector(FSession,
          FRenderer.Root.Find(NyxMenuBarEditorActionID(CEditor, nmbAddHeading)), FRenderer.Root, LEdit),
          'A copied form intent can be inspected without publishing over a pending draft');
        FCommands.Edit(LEdit);
      end;
    7:
      begin
        Check((FCommands.State = nssRejected) and (Pair = FBefore) and FSession.SourceDraftPending,
          'Ordinary queue refuses pending source and preserves the complete draft/pair');
        FSession.DiscardSourceDraft;
        Result := True;
      end;
  end;
  Inc(FStage);
end;

procedure Drive;
begin
  try
    Inc(GPolls);

    if GPolls > 2000 then
    begin
      raise Exception.Create('Bar editor worker did not finish stage ' + IntToStr(GReview.FStage));
    end;

    if GReview.Step then
    begin
      WriteLn('PASS ', GChecks, ' actual bar editor/paired queue checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'passed');
      document.body.setAttribute('data-checks', IntToStr(GChecks));
      {$else}FreeAndNil(GReview);{$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(@Drive, 25);
    end{$endif};
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
      {$else}DumpExceptionBackTrace(Output); ExitCode := 1;{$endif}
      FreeAndNil(GReview);
    end;
  end;
end;

{$ifdef PAS2JS}
var
  GRequest: TJSXMLHttpRequest;

function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;

  if GRequest.status <> 200 then
  begin
    document.body.setAttribute('data-result', 'failed');
    document.body.setAttribute('data-event-error', 'Exact exported source is unavailable');
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
{$else}
var
  LStream: TFileStream;
  LSource: TNyxText;

{ Exercise the ordinary native controller, including its own draft refresh and
  source queue. Only the explicitly named ignored fixture directory is written. }
procedure RunNativeStudio(const ASource: TNyxText);
var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LDocument: TNyxDocument;
  LLabel: TEdit;
  LBefore: TNyxText;
  LAfter: TNyxText;

  procedure Ready;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;

      if GetTickCount64 - LStarted > 60000 then
      begin
        raise Exception.Create('Ordinary Studio bar edit exceeded its bound: ' + LStudio.Status);
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceCommands.Busy;
  end;

  procedure Button(const AID: TNyxText);
  begin
    TControlAccess(LStudio.ShellView.ControlFor(AID)).Click;
    Ready;
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

  if ParamCount < 2 then
  begin
    Exit;
  end;
  LStudio := nil;
  LDocument := nil;
  LForm := TForm.CreateNew(nil);
  try
    LForm.SetBounds(20, 20, 1280, 940);
    LForm.Show;
    LStudio := TNyxNativeStudio.Create(LForm, ParamStr(2));
    LDocument := BuildNyxDocument;
    LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), ASource));
    LStudio.Session.Activate('menu-workspace');
    LStudio.Session.Select('workspace-menu-bar');
    LStudio.Run;
    Ready;
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    LLabel := TEdit(LStudio.ShellView.InputFor(NyxMenuBarEditorFieldID(CEditor, nbfLabel)));
    Check(LLabel <> nil, 'Ordinary native Studio mounts the public bar label');
    LLabel.Text := 'A careful bar draft';
    LLabel.OnChange(LLabel);
    Button(NyxInspectorEventsID);
    Button(NyxInspectorPropertiesID);
    LLabel := TEdit(LStudio.ShellView.InputFor(NyxMenuBarEditorFieldID(CEditor, nbfLabel)));
    Check(TNyxText(LLabel.Text) = 'A careful bar draft',
      'Ordinary native panel switches retain unsaved bar input');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Retained chrome input leaves the accepted source/design pair exact');
    LLabel.Text := 'Studio bar commands';
    LLabel.OnChange(LLabel);
    Button(NyxMenuBarEditorActionID(CEditor, nmbSave));
    Check(LStudio.Session.Selected.MenuBar.Options.Caption = 'Studio bar commands',
      'Actual ordinary Studio save applies typed complete grouping');
    Check(Pos('Studio bar commands', LStudio.Session.Source) > 0,
      'Ordinary Studio regenerates adjacent crafted Pascal');
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Button('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Ordinary native Undo restores the exact original pair');
    Button('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'Ordinary native Redo restores the exact candidate pair');
    Capture('studio-bar-desktop');
    LForm.ClientWidth := 390;
    Ready;
    Button('action-panel-inspector');
    Check(LStudio.ShellView.InputFor(NyxMenuBarEditorFieldID(CEditor, nbfLabel)) <> nil,
      'Compact ordinary native Studio retains the public form');
    Capture('studio-bar-compact');
  finally
    LStudio.Free;
    LDocument.Free;
    LForm.Free;
  end;
  WriteLn('PASS ', GChecks, ' including ordinary native Studio bar authoring');
end;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'seed.pas.txt', True);
  GRequest.onload := @Loaded;
  GRequest.send;
  window.addEventListener('pagehide', @ReviewClosed);
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
