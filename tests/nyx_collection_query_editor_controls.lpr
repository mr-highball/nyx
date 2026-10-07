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

program nyx_collection_query_editor_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.render.browser,{$else}
  Interfaces, Classes, Forms, Controls, StdCtrls, Graphics, IntfGraphics,
  FPWritePNG, nyx.render.lcl, nyx.studio.lcl,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data, nyx.state,
  nyx.collections, nyx.collections.query, nyx.collections.query.editor,
  nyx.collections.view.types, nyx.codec, nyx.studio.projects,
  nyx.studio.session, nyx.studio.sourcejobs, nyx.studio.view,
  nyx.studio.collections, nyx.studio.collectionintent, nyx.studio.collectionedits,
  nyx.menu.editor, nyx.behavior, nyx.studio.authoring,
  nyx.studio.inspector, nyx.generated.view;

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  THost = TForm;
  TControlAccess = class(TControl);
  {$endif}
  { Owns an ordinarily compiled exact semantic companion, disposable physical
    controls and the same independent paired source queue consumed by Studio.
    No protected server/session/project is mutated by this runtime fixture. }
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
    FInitial: TNyxText;
    procedure Refresh;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure Change(const APrefix: TNyxText; AField: TNyxQueryEditorField;
      const AValue: TNyxText);
    procedure Click(AAction: TNyxQueryEditorAction; const APrefix: TNyxText = '');
    function Pair: TNyxText;
    function Query: TNyxCollectionQuery;
    procedure History;
    procedure Refused(AAction: TNyxQueryEditorAction; const APrefix: TNyxText = '');
    procedure Boundaries;
  public
    constructor Create(const ASource: TNyxText);
    destructor Destroy; override;
    function Step: Boolean;
  end;

const
  CEditor = 'inspector-collection-query';
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
  FSession.Activate('grid-review');
  FSession.Select('work-table');
  FState := DefaultNyxStudioViewState;
  FState.BindingsVisible := True;
  FCommands := TNyxSourceCommands.Create(FSession, {$ifdef PAS2JS}@{$endif}Changed);
  FRenderer := TRenderer.Create;
  FRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}Event;
  {$ifdef PAS2JS}
  FHost := TJSHTMLElement(document.createElement('div'));
  FHost.style.cssText := 'height:100vh;overflow:auto;max-width:680px;';
  document.body.appendChild(FHost);
  {$else}
  FHost := TForm.CreateNew(nil);
  FHost.SetBounds(10, 10, 700, 900);
  FHost.Show;
  {$endif}
  Refresh;
  FInitial := Pair;
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

function TReview.Query: TNyxCollectionQuery;
begin
  Result := FSession.Selected.CollectionView.QueryPolicy;
end;

procedure TReview.Refresh;
var
  LShell: TNyxDocument;
begin
  FState.QueryEditorDraft.Capture(CEditor, FRenderer.Root);
  FState.PendingDesign := FCommands.PendingDesign;
  LShell := BuildNyxStudioView(FSession, FState);
  try
    FState.QueryEditorDraft.Restore(LShell.Pages[0]);
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

procedure TReview.Change(const APrefix: TNyxText; AField: TNyxQueryEditorField;
  const AValue: TNyxText);
var
  LID: TNyxText;
  {$ifdef PAS2JS}LInput: TJSHTMLElement;{$else}LInput: TControl;{$endif}
begin
  LID := NyxQueryEditorFieldID(APrefix, AField);
  LInput := FRenderer.InputFor(LID);
  Check(LInput <> nil, 'Specialized query field physically mounted / ' + LID);
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

  if LInput is TComboBox then
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
    'Adapter retains exact query field input');
end;

procedure TReview.Click(AAction: TNyxQueryEditorAction; const APrefix: TNyxText);
var
  LID: TNyxText;
  LPrefix: TNyxText;
begin
  LPrefix := APrefix;

  if LPrefix = '' then
  begin
    LPrefix := CEditor;
  end;
  LID := NyxQueryEditorActionID(LPrefix, AAction);
  Check(FRenderer.Root.Find(LID) <> nil, 'Query action is mounted');
  {$ifdef PAS2JS}FRenderer.ElementFor(LID).click;{$else}
  TControlAccess(FRenderer.ControlFor(LID)).Click;{$endif}
end;

procedure TReview.History;
var
  LAfter: TNyxText;
begin
  LAfter := Pair;
  Check(LAfter <> FBefore, 'Query action changed the accepted pair');
  FSession.Undo;
  Check(Pair = FBefore, 'One Undo restores exact document and adjacent source');
  FSession.Redo;
  Check(Pair = LAfter, 'One Redo restores exact complete query candidate');
  Refresh;
end;

procedure TReview.Refused(AAction: TNyxQueryEditorAction; const APrefix: TNyxText);
var
  LEdit: TNyxStudioDesignEdit;
  LFailed: Boolean;
  LPrefix: TNyxText;
  LBefore: TNyxText;
begin
  LPrefix := APrefix;

  if LPrefix = '' then
  begin
    LPrefix := CEditor;
  end;
  LBefore := Pair;
  LFailed := False;
  try
    CaptureNyxStudioQuery(FSession,
      FRenderer.Root.Find(NyxQueryEditorActionID(LPrefix, AAction)), FRenderer.Root,
      FCommands.PendingDesign, LEdit);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (Pair = LBefore), 'Invalid/stale query capture preserves accepted pair');
end;

procedure TReview.Boundaries;
var
  LSchema: TNyxCollectionSchema;
  LSpec: TNyxCollectionViewSpec;
  LForm: INyxCard;
  LReplacement: INyxCard;
  LDraft: TNyxQueryEditorDraft;
  LChange: TNyxQueryEditorChange;
  LIntent: TNyxStudioCollectionIntent;
  LKey: TNyxCollectionRef;
  LBefore: TNyxText;
  LPolicy: TNyxCollectionQuery;
  LDiscovery: TNyxDataValue;
  LBranches: TNyxDataValue;
  LProperties: TNyxDataValue;
  LIndex: Integer;
  LFoundBinding: Boolean;
begin
  LBefore := Pair;
  LKey := NyxCollection('families');
  LSchema := NyxCollectionSchema.Text(NyxTextField('title 🧭'), '')
    .Boolean(NyxBooleanField('complete'), False)
    .Integer(NyxIntegerField('priority'), 0).Number(NyxNumberField('estimate'), 0);
  LPolicy := NyxCollectionQuery.Where(
    NyxWhere(NyxBooleanField('complete')).EqualTo(False)
      .OrElse(NyxWhere(NyxNumberField('estimate')).AtMost(1.25)).Negated)
    .OrderBy(NyxTextField('title 🧭'), nsdDescending, nqtAsciiInsensitive);
  LSpec := NyxCollectionView(LKey).Column(NyxTextField('title 🧭'), 'Title').Query(LPolicy);
  LForm := NewNyxQueryEditor('family-query', NyxControl('families-table'), LSchema, LSpec);
  Check(CaptureNyxQueryEditor(LForm.Node.Find(
    NyxQueryEditorActionID('family-query', nqeSave)), LForm.Node, LChange) and
    (LChange.Query.ToData.ToJSON = LPolicy.ToData.ToJSON),
    'Complete nested boolean/number/Unicode text-sort form retains typed meaning');
  Check(LForm.Part(NyxPart('field')).Node.Kind = NyxKindName(nkSelect),
    'Creators can reshape specialized named query parts');
  LForm.Node.Find(NyxQueryEditorFieldID('family-query-filter-0-1', nqfValue))
    .SetProp('value', '-');
  LDraft.Capture('family-query', LForm.Node);
  LReplacement := NewNyxQueryEditor('family-query', NyxControl('families-table'), LSchema, LSpec);
  Check(LDraft.Restore(LReplacement.Node), 'Incomplete input survives independent form replacement');
  Check(LReplacement.Node.Find(NyxQueryEditorFieldID(
    'family-query-filter-0-1', nqfValue)).Prop('value') = '-',
    'Incomplete number remains exact presentation input');
  LReplacement := NewNyxQueryEditor('family-query', NyxControl('different-table'), LSchema, LSpec);
  Check(not LDraft.Restore(LReplacement.Node), 'Draft cannot cross owner identity');
  LIntent := Default(TNyxStudioCollectionIntent);
  LIntent.Action := scaQuery;
  LIntent.Key := LKey;
  LIntent.Projection := cpTable;
  LIntent.Query := LPolicy.Copy;
  LIntent.QueryBaseline := NyxQueryEditorBaseline(LSchema, LSpec);
  Check(LIntent.SameIntent(TNyxStudioCollectionIntent.FromData(LIntent.ToData)),
    'Worker descriptor retains full typed query and exact baseline');
  LDiscovery := NyxCollectionAgentSchema;
  Check(LDiscovery.Field('$defs').Field('nyxQueryPredicate').Field('oneOf').Count = 6,
    'MCP discovery describes exact scalar and recursive predicate alternatives');
  LBranches := LDiscovery.Field('oneOf');
  LBranches := LBranches.Item(LBranches.Count - 1).Field('properties')
    .Field('changes').Field('items').Field('oneOf');
  Check(LBranches.Item(LBranches.Count - 1).Field('properties').Field('action')
    .Field('const').AsText = 'query', 'MCP discovery describes the typed query intent');
  LFoundBinding := False;
  for LIndex := 0 to LBranches.Count - 1 do
  begin
    LProperties := LBranches.Item(LIndex).Field('properties');

    if (LProperties.Field('op').Key(0) = 'const') and
      (LProperties.Field('op').Field('const').AsText = 'bind') then
    begin
      Check(LProperties.Field('spec').Field('oneOf').Count = 3,
        'MCP discovers all three strict binding descriptor versions');
      Check(LProperties.Field('spec').Field('oneOf').Item(2).Field('properties')
        .Field('version').Field('const').AsInteger = 3,
        'MCP query-bearing binding version agrees with the decoder');
      LFoundBinding := True;
    end;
  end;
  Check(LFoundBinding, 'MCP binding operation remains discoverable');
  Check(Pair = LBefore, 'Public form boundary checks change no active design');
end;

function TReview.Step: Boolean;
var
  LEdit: TNyxStudioDesignEdit;
begin
  Result := False;

  if FCommands.Busy then
  begin
    Exit;
  end;
  case FStage of
    0:
      begin
        Boundaries;
        Check(not Query.Defined, 'Semantic companion starts without authored query');
        Change(CEditor, nqfField, NyxMenuEditorChoiceCaption(1, 'priority'));
        FBefore := Pair;
        Click(nqeAnd, NyxQueryEditorPredicateID(CEditor));
      end;
    1:
      begin
        Check((FCommands.State = nssApplied) and
          (Query.Filter.FieldKind = nskInteger),
          'Chosen Integer field creates typed predicate / ' + FCommands.Message);
        History;
        Change(NyxQueryEditorPredicateID(CEditor), nqfValue, '-');
        Check(Pair <> FInitial, 'Unaccepted partial input retains previously accepted query');
        Refused(nqeSave);
        Refresh;
        Check(FRenderer.Root.Find(NyxQueryEditorFieldID(
          NyxQueryEditorPredicateID(CEditor), nqfValue)).Prop('value') = '-',
          'Studio shell repaint retains incomplete integer input');
        Change(NyxQueryEditorPredicateID(CEditor), nqfValue, '2');
        Change(NyxQueryEditorPredicateID(CEditor), nqfComparison, 'At least');
        FBefore := Pair;
        Click(nqeSave);
      end;
    2:
      begin
        Check((FCommands.State = nssApplied) and
          (Query.Filter.Comparison = nqcAtLeast) and (Query.Filter.Expected.IntegerValue = 2),
          'Actual Apply admits typed comparison and numeric value');
        Check((Pos('.AtLeast(2)', FSession.Source) > 0) and
          (Pos('NyxCollectionQuery', FSession.Source) > 0),
          'Adjacent source uses crafted typed fluent query');
        History;
        FBefore := Pair;
        Click(nqeAddSort);
      end;
    3:
      begin
        Check((FCommands.State = nssApplied) and (Query.SortCount = 1) and
          (Query.SortAt(0).FieldName = 'task'), 'Recreated form defaults to first exact field');
        History;
        Change(NyxQueryEditorSortID(CEditor, 0), nqfDirection, 'Descending');
        Change(NyxQueryEditorSortID(CEditor, 0), nqfTextMatch, 'ASCII case insensitive');
        Change(CEditor, nqfField, NyxMenuEditorChoiceCaption(1, 'priority'));
        FBefore := Pair;
        Click(nqeAddSort);
      end;
    4:
      begin
        Check((FCommands.State = nssApplied) and (Query.SortCount = 2) and
          (Query.SortAt(0).Direction = nsdDescending) and
          (Query.SortAt(0).TextComparison = nqtAsciiInsensitive),
          'Adding sort captures existing draft keys and exact text policy');
        History;
        FBefore := Pair;
        Click(nqeSortEarlier, NyxQueryEditorSortID(CEditor, 1));
      end;
    5:
      begin
        Check((FCommands.State = nssApplied) and (Query.SortAt(0).FieldName = 'priority'),
          'Physical reorder retains complete ordered policy');
        History;
        Change(NyxQueryEditorSortID(CEditor, 1), nqfField,
          NyxMenuEditorChoiceCaption(1, 'priority'));
        Refused(nqeSave);
        Change(NyxQueryEditorSortID(CEditor, 1), nqfField,
          NyxMenuEditorChoiceCaption(0, 'task'));
        FBefore := Pair;
        Click(nqeNegate, NyxQueryEditorPredicateID(CEditor));
      end;
    6:
      begin
        Check((FCommands.State = nssApplied) and (Query.Filter.Kind = nqpNot),
          'Physical NOT action preserves independently captured filter');
        History;
        FBefore := Pair;
        Click(nqeClearFilter);
      end;
    7:
      begin
        Check((FCommands.State = nssApplied) and (Query.Filter = nil) and
          (Query.SortCount = 2), 'Clearing filter retains sorted result defaults');
        History;
        FSession.SetSourceDraft(FSession.Source + #10 + TNyxText('{ Pending user draft }'));
        FBefore := Pair;
        Check(CaptureNyxStudioQuery(FSession,
          FRenderer.Root.Find(NyxQueryEditorActionID(CEditor, nqeClearSort)), FRenderer.Root,
          FCommands.PendingDesign, LEdit), 'Form can be inspected without overwriting pending source');
        FCommands.Edit(LEdit);
      end;
    8:
      begin
        Check((FCommands.State = nssRejected) and (Pair = FBefore) and
          FSession.SourceDraftPending, 'Paired queue refuses pending source and preserves draft');
        FSession.DiscardSourceDraft;
        FBefore := Pair;
        Click(nqeClearSort);
      end;
    9:
      begin
        Check((FCommands.State = nssApplied) and not Query.Defined,
          'Clearing last ordering returns canonical query-free binding');
        History;
        {$ifdef PAS2JS}
        { Leave the preceding owned sorted policy visible for the capture.
          Semantic/paired assertions above already qualified clearing it. }
        FSession.Undo;
        Refresh;
        FRenderer.ElementFor(CEditor).scrollIntoView;
        {$endif}
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
      raise Exception.Create('Query editor worker did not finish');
    end;

    if GReview.Step then
    begin
      WriteLn('PASS ', GChecks, ' actual query editor/paired queue checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-query-editor', 'passed');
      document.body.setAttribute('data-query-editor-checks', IntToStr(GChecks));
      {$else}FreeAndNil(GReview);{$endif}
    end
    {$ifdef PAS2JS}else
    begin
      window.setTimeout(@Drive, 25);
    end{$endif};
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-query-editor', 'failed: ' + E.Message);
      {$else}WriteLn('FAIL ', E.Message); FreeAndNil(GReview); ExitCode := 1;{$endif}
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
    document.body.setAttribute('data-query-editor', 'failed: companion source HTTP');
    Exit;
  end;
  GReview := TReview.Create(GRequest.responseText);
  Drive;
end;

function QueryReviewClosed(AEvent: TJSEvent): Boolean;
begin
  FreeAndNil(GReview);
  Result := True;
end;
{$else}
var
  LStream: TFileStream;
  LSource: TNyxText;

procedure RunNativeStudio;
var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LDocument: TNyxDocument;
  LInput: TEdit;
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
        raise Exception.Create('Ordinary Studio query edit exceeded its bound: ' + LStudio.Status);
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
    { Ordinary native Studio has a nested sidebar scrollbox. Its containing
      renderer's nonvirtual Reveal scrolls the outer host only; capture the
      actual form through the sidebar's native scrolling contract instead. }
    TScrollBox(LStudio.ShellView.ControlFor('studio-right')).ScrollInView(
      LStudio.ShellView.ControlFor(CEditor));
    Application.ProcessMessages;
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
    LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), LSource));
    LStudio.Session.Activate('grid-review');
    LStudio.Session.Select('work-table');
    LStudio.Run;
    Ready;
    Button(NyxStudioBindingsToggleID);
    Button(NyxQueryEditorActionID(NyxQueryEditorPredicateID(CEditor), nqeAnd));
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    LInput := TEdit(LStudio.ShellView.InputFor(
      NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue)));
    Check(LInput <> nil, 'Ordinary native Studio mounts the specialized query input');
    LInput.Text := 'A careful draft';
    LInput.OnChange(LInput);
    Button(NyxInspectorEventsID);
    Button(NyxInspectorPropertiesID);
    LInput := TEdit(LStudio.ShellView.InputFor(
      NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue)));
    Check(TNyxText(LInput.Text) = 'A careful draft',
      'Ordinary native panel switches retain unsaved query input');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Retained query input leaves the accepted pair exact');
    LInput.Text := 'A thoughtful idea';
    LInput.OnChange(LInput);
    Button(NyxQueryEditorActionID(CEditor, nqeSave));
    Check(LStudio.Session.Selected.CollectionView.QueryPolicy.Filter.Expected.TextValue =
      'A thoughtful idea', 'Ordinary native Save applies the complete typed query');
    Check(Pos('A thoughtful idea', LStudio.Session.Source) > 0,
      'Ordinary Studio updates the adjacent crafted source');
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Button('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Ordinary native Undo restores the exact pair');
    Button('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'Ordinary native Redo restores the exact query candidate');
    Capture('studio-query-desktop');
    LForm.ClientWidth := 390;
    Ready;
    Button('action-panel-inspector');
    Check(LStudio.ShellView.InputFor(NyxQueryEditorFieldID(CEditor, nqfField)) <> nil,
      'Compact ordinary native Studio retains the query form');
    Capture('studio-query-compact');
  finally
    LStudio.Free;
    LDocument.Free;
    LForm.Free;
  end;
  WriteLn('PASS ', GChecks, ' including ordinary native Studio query authoring');
end;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'seed.pas.txt', True);
  GRequest.onload := @Loaded;
  GRequest.send;
  window.addEventListener('pagehide', @QueryReviewClosed);
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
    RunNativeStudio;
  end;
  {$endif}
end.
