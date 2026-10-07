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

program nyx_studio_menu_editor_observer;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, fphttpclient, nyx.text, nyx.data, nyx.types, nyx.contract,
  nyx.menu.types, nyx.menu.declarations, nyx.menu.editor, nyx.root.types,
  nyx.menu.bar.declarations, nyx.menu.bar.editor, nyx.typeahead,
  nyx.studio.edits, nyx.test.mcp.client, nyx.test.browser.pipe;

var
  GClient: TNyxMCPTestClient;
  GHost: TNyxBrowserPipe;
  GWorkspace: TNyxText;
  GDirectory: String;
  GRevision: Integer;
  GChecks: Integer;
  GSequence: Integer;
  GWidth: Integer;
  GHeight: Integer;
  GResetFixture: Boolean;
  GInteractOnly: Boolean;
  GObserveOnly: Boolean;
  GBarEditor: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ Only one explicitly enrolled isolated service is used. The fixture creates its
  own ordinary project through MCP; user projects and temporary review lifecycle
  never become editor-writing workarounds. The new project remains reviewable. }
function Call(const AName: TNyxText; const AFields: array of TNyxDataField;
  AContext: Boolean = True; ARefuse: Boolean = False): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LPacket: TNyxDataValue;
  LContext: Boolean;
begin
  LContext := AContext and (GWorkspace <> '');
  SetLength(LFields, Length(AFields) + Ord(LContext));
  for LIndex := 0 to High(AFields) do
  begin
    LFields[LIndex] := AFields[LIndex];
  end;

  if LContext then
  begin
    LFields[High(LFields)] := NyxField('workspace', NyxData(GWorkspace));
  end;
  LPacket := GClient.Tool(AName, NyxObject(LFields));
  Check(LPacket.Field('isError').AsBoolean = ARefuse,
    AName + ' unexpected admission / ' + Copy(LPacket.ToJSON, 1, 900));
  Result := LPacket.Field('structuredContent');
end;

procedure Save(const AName: String; const AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(GDirectory + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

function Operation: TNyxText;
begin
  Inc(GSequence);
  Result := 'menu-editor-observer-' + TNyxText(IntToStr(GSequence));
end;

function Source: TNyxText;
var
  LPacket: TNyxDataValue;
  LLine: Integer;
  LIndex: Integer;
begin
  Result := '';
  LLine := 1;
  repeat
    LPacket := Call('nyx_source', [NyxField('line', NyxData(LLine)),
      NyxField('count', NyxData(80))]);
    Check(LPacket.Field('revision').AsInteger = GRevision,
      'bounded source context keeps one accepted revision');

    if (LPacket.Field('lines').Count = 0) and
      (LLine <= LPacket.Field('totalLines').AsInteger) then
    begin
      raise Exception.Create('Bounded source context did not advance');
    end;
    for LIndex := 0 to LPacket.Field('lines').Count - 1 do
    begin
      Result := Result + LPacket.Field('lines').Item(LIndex).AsText + TNyxText(#10);
    end;
    Inc(LLine, LPacket.Field('lines').Count);
  until LLine > LPacket.Field('totalLines').AsInteger;
end;

procedure WaitFor(const ASelector: TNyxText; AExists: Boolean = True);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if GHost.RuntimeError <> '' then
    begin
      raise Exception.Create('Actual browser raised; inspect runtime-error.json');
    end;

    if GHost.Exists(ASelector) = AExists then
    begin
      Inc(GChecks);
      Exit;
    end;

    if GetTickCount64 - LStarted > 20000 then
    begin
      raise Exception.Create('Ordinary Studio control did not settle / ' + ASelector);
    end;
    Sleep(100);
  until False;
end;

procedure WaitField(const ASelector, AValue: TNyxText);
var
  LStarted: QWord;
  LValue: TNyxText;
begin
  LStarted := GetTickCount64;
  repeat

    if GHost.TryFieldValue(ASelector, LValue) and (LValue = AValue) then
    begin
      Inc(GChecks);
      Exit;
    end;

    if (GHost.RuntimeError <> '') or (GetTickCount64 - LStarted > 20000) then
    begin
      raise Exception.Create('Ordinary Studio value did not settle / ' + ASelector +
        ' / expected ' + Copy(AValue, 1, 100) + ' / observed ' + Copy(LValue, 1, 100));
    end;
    Sleep(100);
  until False;
end;

{ One bounded agent row proves the observing shell actually admitted a changed
  connection roster. No arbitrary delay or injected script stands in for paint. }
procedure WaitText(const ASelector, AText: TNyxText; AContains: Boolean);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat

    if (Pos(AText, GHost.ElementHTML(ASelector)) > 0) = AContains then
    begin
      Inc(GChecks);
      Exit;
    end;

    if (GHost.RuntimeError <> '') or (GetTickCount64 - LStarted > 20000) then
    begin
      raise Exception.Create('Expected observing roster paint did not arrive');
    end;
    Sleep(100);
  until False;
end;

procedure AwaitRevision(AExpected: Integer);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;

    if GRevision = AExpected then
    begin
      Inc(GChecks);
      Exit;
    end;

    if (GRevision > AExpected) or (GHost.RuntimeError <> '') or
      (GetTickCount64 - LStarted > 20000) then
    begin
      raise Exception.Create('Expected paired editor revision did not arrive');
    end;
    Sleep(100);
  until False;
end;

{ Pending draft publication is revisioned independently of accepted Undo steps.
  Wait for its actual observing receipt rather than assuming a DOM edit or reset
  has reached the shared pair. This fixture owns its explicit project handle. }
procedure AwaitDraft(APending: Boolean);
var
  LStarted: QWord;
  LSession: TNyxDataValue;
begin
  LStarted := GetTickCount64;
  repeat
    LSession := Call('nyx_session', []);
    GRevision := LSession.Field('revision').AsInteger;

    if LSession.Field('pendingDraft').AsBoolean = APending then
    begin
      Inc(GChecks);
      Exit;
    end;

    if (GHost.RuntimeError <> '') or (GetTickCount64 - LStarted > 20000) then
    begin
      raise Exception.Create('Expected observing draft receipt did not arrive');
    end;
    Sleep(100);
  until False;
end;

function FieldSelector(AField: TNyxMenuEditorField; const AFace: TNyxText): TNyxText;
begin
  Result := '[data-node=' + NyxMenuEditorFieldID('inspector-menu', AField) + '] ' + AFace;
end;

function ActionSelector(AAction: TNyxMenuEditorAction): TNyxText;
begin
  Result := '[data-node=' + NyxMenuEditorActionID('inspector-menu', AAction) + ']';
end;

{ Selectors are a target input boundary. Policy, parts, menu references and
  mutations still use the public typed contract and authenticated document API. }
function BarField(AField: TNyxMenuBarEditorField;
  const AFace: TNyxText = 'input'): TNyxText;
begin
  Result := '[data-node=' + NyxMenuBarEditorFieldID('inspector-menu-bar', AField) +
    '] ' + AFace;
end;

function BarAction(AAction: TNyxMenuBarEditorAction; AHeading: Integer = -1): TNyxText;
var
  LPrefix: TNyxText;
begin
  LPrefix := 'inspector-menu-bar';

  if AHeading >= 0 then
  begin
    LPrefix := NyxMenuBarEditorHeadingID(LPrefix, AHeading);
  end;
  Result := '[data-node=' + NyxMenuBarEditorActionID(LPrefix, AAction) + ']';
end;

function BarPlan(const ALabel: TNyxText): INyxMenuBarDefinition;
begin
  Result := NewNyxMenuBarDefinition(NyxMenuBar(ALabel).Wrap(False)
    .HoverSwitch(False).TypeAhead(NyxTypeAhead.WindowMilliseconds(1700).Match(ntmExact)))
    .Heading(NyxPart('file'), NyxMenuRef('actions'))
    .Heading(NyxPart('edit'), NyxMenuRef('actions'))
    .Heading(NyxPart('view'), NyxMenuRef('density'));
end;

{ Native select keyboard behavior is exercised rather than changing DOM values.
  Menu metadata already maps these captions to typed exact document references. }
procedure Choose(const ASelector: TNyxText; AIndex: Integer);
var
  LIndex: Integer;
begin
  GHost.Click(ASelector);
  GHost.Key(nbkHome);
  for LIndex := 1 to AIndex do
  begin
    GHost.Key(nbkDown);
  end;
  GHost.Key(nbkEnter);
  GHost.Tab;
end;

procedure Inspector(AEvents: Boolean);
begin

  if GHost.Exists('[data-node=studio-panelbar]') then
  begin
    GHost.Click('[data-node=action-panel-inspector]');
  end;

  if AEvents then
  begin
    GHost.Click('[data-node=inspector-tab-events]');
  end
  else
  begin
    GHost.Click('[data-node=inspector-tab-properties]');
  end;
end;

procedure Compose;
var
  LPrimary: TNyxDataValue;
  LCreation: TNyxDataValue;
  LOperations: array of TNyxDataValue;
  LIndex: Integer;
  LActions: INyxMenuDefinition;
  LDensity: INyxMenuDefinition;
  LBarCreation: TNyxDataValue;
  LSelected: TNyxText;
begin

  if (ParamCount >= 5) and not GBarEditor then
  begin
    { An explicitly retained fixture workspace permits recovery of a failed
      input attempt without filling the service's bounded project registry.
      The caller owns this exact handle; no project is replaced or re-composed. }
    GWorkspace := ParamStr(5);

    if GObserveOnly and (GWorkspace = 'primary') then
    begin
      GWorkspace := '';
    end;
    Save('workspace.txt', GWorkspace);
    LPrimary := Call('nyx_session', []);
    GRevision := LPrimary.Field('revision').AsInteger;

    if GObserveOnly then
    begin
      Check(LPrimary.Field('selection').AsText <> '',
        'read-only observing qualification requires an existing selected component');
      Exit;
    end;
    Check((LPrimary.Field('selection').AsText = 'open-actions') and
      (LPrimary.Field('view').AsText = 'home') and
      (GResetFixture or not LPrimary.Field('pendingDraft').AsBoolean),
      'retained fixture context is exact');
    Exit;
  end;
  LPrimary := Call('nyx_session', [], False);
  GWorkspace := Call('nyx_workspaces', [NyxField('mode', NyxData('create')),
    NyxField('base', NyxData('empty')),
    NyxField('label', NyxData('Thoughtful choices ' + TNyxText(IntToStr(GWidth)))),
    NyxField('expectedRevision', LPrimary.Field('revision')),
    NyxField('operationId', NyxData(Operation))], False).Field('workspace').AsText;
  Check(GWorkspace <> '', 'independent ordinary workspace has an exact handle');
  Save('workspace.txt', GWorkspace);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
  { Raw transport properties are confined to this semantic protocol boundary;
    menu contracts are constructed with the public typed Pascal builders. }
  LCreation := TNyxDataValue.ParseJSON(
    '[{"op":"create","kind":"page","id":"home","root":"page","properties":{"gap":12,"padding":24}},' +
    '{"op":"create","kind":"button","id":"open-actions","parent":"home","properties":{"text":"Actions"}},' +
    '{"op":"create","kind":"column","id":"actions-content","root":"component","properties":{"gap":4,"padding":8,"compound":true}},' +
    '{"op":"create","kind":"button","id":"copy-draft","parent":"actions-content","properties":{"text":"Copy draft","part":"copy"}},' +
    '{"op":"create","kind":"button","id":"show-guides","parent":"actions-content","properties":{"text":"Show guides","part":"guides"}},' +
    '{"op":"create","kind":"separator","id":"action-divider","parent":"actions-content","properties":{"part":"divider","height":1}},' +
    '{"op":"create","kind":"button","id":"choose-density","parent":"actions-content","properties":{"text":"Density","part":"density"}},' +
    '{"op":"create","kind":"column","id":"density-content","root":"component","properties":{"gap":4,"padding":8,"compound":true}},' +
    '{"op":"create","kind":"button","id":"roomy-density","parent":"density-content","properties":{"text":"Roomy","part":"roomy"}},' +
    '{"op":"create","kind":"button","id":"compact-density","parent":"density-content","properties":{"text":"Compact","part":"compact"}}]');
  LActions := NewNyxMenuDefinition(NyxReusableRoot('actions-content'), NyxMenu('Thoughtful actions'))
    .Action(NyxPart('copy'), NyxMenuCommand('copy-draft'))
    .Check(NyxPart('guides'), NyxMenuCommand('show-guides'), True)
    .Separator(NyxPart('divider'))
    .Submenu(NyxPart('density'), NyxMenuRef('density'));
  LDensity := NewNyxMenuDefinition(NyxReusableRoot('density-content'), NyxMenu('Density'))
    .Radio(NyxPart('roomy'), NyxMenuCommand('roomy'), NyxMenuGroup('spacing'), True)
    .Radio(NyxPart('compact'), NyxMenuCommand('compact'), NyxMenuGroup('spacing'), False);
  LSelected := 'open-actions';
  LBarCreation := NyxArray([]);

  if GBarEditor then
  begin
    LSelected := 'workspace-menu-bar';
    LBarCreation := TNyxDataValue.ParseJSON(
      '[{"op":"create","kind":"row","id":"workspace-menu-bar","parent":"home",' +
      '"properties":{"gap":8,"compound":true}},' +
      '{"op":"create","kind":"button","id":"bar-file","parent":"workspace-menu-bar",' +
      '"properties":{"text":"File","part":"file"}},' +
      '{"op":"create","kind":"button","id":"bar-edit","parent":"workspace-menu-bar",' +
      '"properties":{"text":"Edit","part":"edit"}},' +
      '{"op":"create","kind":"button","id":"bar-view","parent":"workspace-menu-bar",' +
      '"properties":{"text":"View","part":"view"}}]');
  end;
  SetLength(LOperations, LCreation.Count + 3 + LBarCreation.Count + Ord(GBarEditor));
  for LIndex := 0 to LCreation.Count - 1 do
  begin
    LOperations[LIndex] := LCreation.Item(LIndex);
  end;
  LOperations[LCreation.Count] := NyxDefineMenu(NyxMenuRef('actions'), LActions).ToData;
  LOperations[LCreation.Count + 1] := NyxDefineMenu(NyxMenuRef('density'), LDensity).ToData;
  LOperations[LCreation.Count + 2] := NyxAttachMenu(NyxControl('open-actions'), NyxMenuRef('actions')).ToData;
  for LIndex := 0 to LBarCreation.Count - 1 do
  begin
    LOperations[LCreation.Count + 3 + LIndex] := LBarCreation.Item(LIndex);
  end;

  if GBarEditor then
  begin
    LOperations[High(LOperations)] := NyxConfigureMenuBar(
      NyxControl('workspace-menu-bar'), BarPlan('Studio commands')).ToData;
  end;
  Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation)), NyxField('operations', NyxArray(LOperations))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
  Call('nyx_select', [NyxField('id', NyxData('home')), NyxField('activate', NyxData(True)),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
  Call('nyx_select', [NyxField('id', NyxData(LSelected)),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(Operation))]);
  GRevision := Call('nyx_session', []).Field('revision').AsInteger;
end;

{ Real compiler jobs prove the accepted companion remains consumable. Menu
  Interact below qualifies mounted menu input, not execution of TODO handlers. }
procedure Build(const ATarget: TNyxText; const AScope: TNyxText = 'application');
var
  LReply: TNyxDataValue;
  LOutput: TNyxDataValue;
  LJob: TNyxText;
  LStarted: QWord;
  LRequest: array of TNyxDataField;
  LHTTP: TFPHTTPClient;
  LBytes: TMemoryStream;
  LCompiled: TNyxText;
  LBase: TNyxText;
begin
  LOutput := Call('nyx_build', [NyxField('mode', NyxData('outputs'))]);
  SetLength(LRequest, 6 + Ord(AScope <> 'application'));
  LRequest[0] := NyxField('mode', NyxData('request'));
  LRequest[1] := NyxField('expectedRevision', NyxData(GRevision));
  LRequest[2] := NyxField('operationId', NyxData(Operation));
  LRequest[3] := NyxField('outputID', LOutput.Field('outputID'));
  LRequest[4] := NyxField('target', NyxData(ATarget));
  LRequest[5] := NyxField('scope', NyxData(AScope));

  if AScope <> 'application' then
  begin
    LRequest[6] := NyxField('view', NyxData('home'));
  end;
  LReply := Call('nyx_build', LRequest);
  LJob := LReply.Field('job').AsText;
  LStarted := GetTickCount64;
  repeat
    Sleep(100);
    LReply := Call('nyx_build', [NyxField('mode', NyxData('status')),
      NyxField('job', NyxData(LJob)), NyxField('severity', NyxData('error')),
      NyxField('limit', NyxData(8))]);

    if (LReply.Field('state').AsText <> 'queued') and (LReply.Field('state').AsText <> 'running') then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise Exception.Create('Compiler job exceeded the maintained input budget');
    end;
  until False;
  Save(String(ATarget + '-' + AScope) + '-build.json', LReply.ToJSON);
  Check((LReply.Field('state').AsText = 'succeeded') and LReply.Field('currentSource').AsBoolean,
    'accepted callback/menu source compiles / ' + ATarget + '/' + AScope);
  { Status alone does not prove compiler input. Compare actual HTTP source bytes
    with bounded semantic source at that revision; target artifacts stay owned. }
  LHTTP := TFPHTTPClient.Create(nil);
  LBytes := TMemoryStream.Create;
  try
    LBase := TNyxText(ParamStr(2));

    if Copy(LBase, Length(LBase), 1) = '/' then
    begin
      SetLength(LBase, Length(LBase) - 1);
    end;
    LHTTP.Get(LBase + TNyxText('/') + LReply.Field('compiledSource').AsText, LBytes);
    SetLength(LCompiled, LBytes.Size);

    if LBytes.Size > 0 then
    begin
      Move(LBytes.Memory^, LCompiled[1], LBytes.Size);
    end;
    Check(LCompiled = Source, 'real compiler input equals the exact accepted Pascal');
  finally
    LBytes.Free;
    LHTTP.Free;
  end;
end;

{ Source presence is insufficient: trusted input at the real caret must reach
  the advertised implementation line. This probe belongs only to this fixture's
  local source draft, then ordinary Restore retires it without admission/history. }
procedure VerifyCaret(const AAcceptedSource: TNyxText);
const
  CSignature = 'procedure TOpenActionsButtonActivate.Invoke';
  CProbe = '{ Navigation probe }';
var
  LPosition: Integer;
  LDraft: TNyxText;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
begin
  LPosition := Pos(CSignature, AAcceptedSource);
  Check(LPosition > 0, 'fixture has its exact named callback implementation');
  LDraft := Copy(AAcceptedSource, 1, LPosition - 1) + CProbe +
    Copy(AAcceptedSource, LPosition, Length(AAcceptedSource));
  LBefore := Call('nyx_session', []);
  GHost.TypeText(CProbe);
  WaitField('[data-node=studio-code]', LDraft);
  AwaitDraft(True);
  Check(Source = AAcceptedSource, 'typing at the navigated caret keeps accepted source unchanged');
  GHost.Click('[data-node=action-reset-source]');
  WaitField('[data-node=studio-code]', AAcceptedSource);
  AwaitDraft(False);
  LAfter := Call('nyx_session', []);
  Check((LAfter.Field('canUndo').AsBoolean = LBefore.Field('canUndo').AsBoolean) and
    (LAfter.Field('canRedo').AsBoolean = LBefore.Field('canRedo').AsBoolean) and
    (Source = AAcceptedSource), 'restoring the probe retains accepted history/source');
end;

{ Recovery is restricted to the explicitly retained fixture whose first menu
  save and first callback already completed. A failed caret assertion may leave
  only our own probe draft. Ordinary Restore clears it; revision-aware semantic
  Undo retires those two fixture edits without replacing any project/root. }
procedure ResetFixture;
var
  LAccepted: TNyxText;
  LIndex: Integer;
begin
  WaitFor('[data-node=action-actions]');
  LAccepted := Source;
  Check((Pos('Thoughtful choices', LAccepted) > 0) and
    (Pos('procedure TOpenActionsButtonActivate.Invoke', LAccepted) > 0) and
    (Pos('procedure TOpenActionsButtonActivate2.Invoke', LAccepted) = 0),
    'owned recovery pair is exactly the first-save/first-callback fixture');
  Inspector(True);
  WaitFor('[data-node^=event-named-] [data-node$=-callback-0-source]');
  GHost.Click('[data-node^=event-named-] [data-node$=-callback-0-source]');
  WaitFor('[data-node=action-reset-source]');
  GHost.Click('[data-node=action-reset-source]');
  WaitField('[data-node=studio-code]', LAccepted);
  AwaitDraft(False);
  for LIndex := 1 to 2 do
  begin
    Call('nyx_history', [NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation))]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
  end;
  LAccepted := Source;
  Check((Pos('Thoughtful actions', LAccepted) > 0) and
    (Pos('TOpenActionsButtonActivate', LAccepted) = 0),
    'semantic Undo restores the exact initial owned companion boundary');
end;

{ Actual menu input is separate from source admission or TODO execution. An
  independently initialized read-only MCP witness joins and retires while the
  family is open, forcing both ordinary shell rebuilds without touching a pair. }
procedure Interact(const AAcceptedSource: TNyxText);
var
  LPeer: TNyxMCPTestClient;
  LReply: TNyxDataValue;
  LRow: TNyxText;
  LWitness: TNyxText;
begin

  if GHost.Exists('[data-node=studio-panelbar]') then
  begin
    GHost.Click('[data-node=action-panel-design]');
  end;
  WaitFor('[data-node=action-preview]');
  WriteLn('Ordinary Studio / canvas Interact');
  Flush(Output);
  GHost.Click('[data-node=action-preview]');
  GHost.Click('[data-node=studio-canvas] [data-node=open-actions]');
  WaitFor('.nyx-popover:popover-open [data-node=copy-draft]');
  { Workspace handles may contain punctuation. Quote the exact CSS attribute;
    a fresh witness name also distinguishes concurrent ordinary review clients. }
  LRow := '[data-node="studio-agent-workspace-' + GWorkspace + '"]';
  LWitness := 'Scooty portal witness ' + IntToStr(GetTickCount64);
  LPeer := TNyxMCPTestClient.Create(ParamStr(1), LWitness);
  try
    LReply := LPeer.Tool('nyx_session', NyxObject([
      NyxField('workspace', NyxData(GWorkspace))]));
    Check(not LReply.Field('isError').AsBoolean, 'independent witness inspects only the owned workspace');
    WaitText(LRow, LWitness, True);
    Check(GHost.Exists('.nyx-popover:popover-open [data-node=show-guides]'),
      'open menu survives the observed agent roster insertion');
    LPeer.Close;
    WaitText(LRow, LWitness, False);
    Check(GHost.Exists('.nyx-popover:popover-open [data-node=show-guides]'),
      'open menu survives the observed agent roster retirement');
  finally
    { Retire the owned MCP witness even when physical observation refuses. Close
      is idempotent; its session must not stay in the observing Studio roster. }
    try
      LPeer.Close;
    finally
      LPeer.Free;
    end;
  end;
  GHost.Capture('interact-actions');
  GHost.Click('.nyx-popover:popover-open [data-node=show-guides]');
  WaitFor('.nyx-popover:popover-open', False);
  GHost.Click('[data-node=studio-canvas] [data-node=open-actions]');
  WaitFor('.nyx-popover:popover-open [data-node=show-guides][aria-checked=false]');
  GHost.Click('.nyx-popover:popover-open [data-node=choose-density]');
  WaitFor('.nyx-popover:popover-open [data-node=compact-density]');
  GHost.Click('.nyx-popover:popover-open [data-node=compact-density]');
  WaitFor('.nyx-popover:popover-open', False);
  GHost.Click('[data-node=studio-canvas] [data-node=open-actions]');
  GHost.Click('.nyx-popover:popover-open [data-node=choose-density]');
  WaitFor('.nyx-popover:popover-open [data-node=compact-density][aria-checked=true]');
  GHost.Capture('interact-density');
  GHost.Key(nbkEscape);
  GHost.Key(nbkEscape);
  WaitFor('.nyx-popover:popover-open', False);
  Check(Source = AAcceptedSource, 'runtime menu state preserves the accepted design and source');
  Check(Pos('refused', LowerCase(GHost.ElementHTML('[data-node=studio-status]'))) = 0,
    'ordinary canvas Interact reports no refusal');
end;

{ The installed host can be qualified while its ordinary registry is full. Use
  the exact supplied observing project, never replace it or synthesize another
  accepted pair. Unsaved menu form text is disposable local presentation only.
  Actual tab/roster rebuilds must retain it, and all semantic session fields stay
  exact. Protected complete pairs/checkpoint are independently guarded by the
  deployment controller before and after this read-only input consumer. }
procedure ObserveMenuEditor;
const
  CFields: array[0..9] of TNyxText = ('revision', 'selection', 'view',
    'pendingDraft', 'permission', 'canUndo', 'canRedo', 'pages', 'components', 'title');
var
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LPeer: TNyxMCPTestClient;
  LReply: TNyxDataValue;
  LRow: TNyxText;
  LWitness: TNyxText;
  LInitialTitle: TNyxText;
  LField: Integer;

  { Compact Inspector intentionally omits the center's session list. Observe
    actual roster paint in Design, then return to the same unsaved Properties
    form. Desktop keeps both faces mounted. No accepted project is navigated. }
  procedure ShowObservingRow;
  begin

    if GHost.Exists('[data-node=studio-panelbar]') then
    begin
      GHost.Click('[data-node=action-panel-design]');
    end;

    if not GHost.Exists('[data-node=studio-agents]') then
    begin
      GHost.Click('[data-node=action-actions]');
      WaitFor('.nyx-popover:popover-open [data-node=studio-menu-project]');
      GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-project]');
      WaitFor('.nyx-popover:popover-open [data-node=studio-menu-agents]');
      GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-agents]');
      WaitFor('.nyx-popover:popover-open', False);
    end;

    if not GHost.Exists(LRow) and GHost.Exists('[data-node=action-details-toggle]') then
    begin
      GHost.Click('[data-node=action-details-toggle]');
    end;
    WaitFor(LRow);
  end;

begin
  LBefore := Call('nyx_session', []);
  WaitFor('[data-node=action-actions]');
  WaitText('[data-node=studio-subtitle]', LBefore.Field('title').AsText, True);
  WaitFor('[data-node=studio-canvas] [data-node="' +
    LBefore.Field('selection').AsText + '"]');
  Inspector(False);
  WaitFor(FieldSelector(nmfDefinition, 'select'));
  Choose(FieldSelector(nmfDefinition, 'select'), 0);
  GHost.Click(ActionSelector(nmeChoose));
  Check(GHost.TryFieldValue(FieldSelector(nmfTitle, 'input'), LInitialTitle),
    'installed ordinary Properties exposes the public typed menu form');
  GHost.ReplaceText(FieldSelector(nmfTitle, 'input'), 'A thoughtful menu');
  WaitField(FieldSelector(nmfTitle, 'input'), 'A thoughtful menu');
  Inspector(True);
  Inspector(False);
  WaitField(FieldSelector(nmfTitle, 'input'), 'A thoughtful menu');

  if GWorkspace = '' then
  begin
    LRow := '[data-node=studio-agent-workspace-primary]';
  end
  else
  begin
    LRow := '[data-node="studio-agent-workspace-' + GWorkspace + '"]';
  end;
  LWitness := 'Scooty observing menu witness ' + IntToStr(GetTickCount64);
  ShowObservingRow;
  LPeer := TNyxMCPTestClient.Create(ParamStr(1), LWitness);
  try

    if GWorkspace = '' then
    begin
      LReply := LPeer.Tool('nyx_session', NyxObject([]));
    end
    else
    begin
      LReply := LPeer.Tool('nyx_session', NyxObject([
        NyxField('workspace', NyxData(GWorkspace))]));
    end;
    Check(not LReply.Field('isError').AsBoolean,
      'independent installed MCP witness reads only the observing project');
    WaitText(LRow, LWitness, True);
    Inspector(False);
    WaitField(FieldSelector(nmfTitle, 'input'), 'A thoughtful menu');
    ShowObservingRow;
    LPeer.Close;
    WaitText(LRow, LWitness, False);
    Inspector(False);
    WaitField(FieldSelector(nmfTitle, 'input'), 'A thoughtful menu');
  finally
    try
      LPeer.Close;
    finally
      LPeer.Free;
    end;
  end;
  GHost.Capture('observing-menu-form');
  GHost.ReplaceText(FieldSelector(nmfTitle, 'input'), LInitialTitle);
  WaitField(FieldSelector(nmfTitle, 'input'), LInitialTitle);
  LAfter := Call('nyx_session', []);
  for LField := 0 to High(CFields) do
  begin
    Check(LAfter.Field(CFields[LField]).ToJSON = LBefore.Field(CFields[LField]).ToJSON,
      'read-only observing form retains semantic ' + CFields[LField]);
  end;
end;

procedure Journey;
const
  CNamedCard = '[data-node^=event-named-]';
  CNamedAdd = CNamedCard + ' [data-node$=-add]';
  CFirstSource = CNamedCard + ' [data-node$=-callback-0-source]';
  CFirstRemove = CNamedCard + ' [data-node$=-callback-0-remove]';
var
  LBefore: TNyxText;
  LAfter: TNyxText;
  LCallbackSource: TNyxText;
  LRevision: Integer;
  LMenu: TNyxDataValue;
  LStageHeight: Double;
  LSourceBox: TNyxBrowserBox;
begin
  WriteLn('Ordinary Studio / properties');
  Flush(Output);
  WaitFor('[data-node=action-actions]');
  Inspector(False);
  { First shell readiness precedes its authenticated workspace attachment.
    Require the exact selected owner before touching its authoring controls. }
  WaitFor(FieldSelector(nmfDefinition, 'select'));
  Choose(FieldSelector(nmfDefinition, 'select'), 1);
  WaitField(FieldSelector(nmfDefinition, 'select'), '1 / actions');
  LBefore := Source;
  GHost.Click(ActionSelector(nmeChoose));
  WaitField(FieldSelector(nmfTitle, 'input'), 'Thoughtful actions');
  Check(Source = LBefore, 'opening the form changes no accepted source');
  GHost.ReplaceText(FieldSelector(nmfTitle, 'input'), 'Thoughtful choices');
  WaitField(FieldSelector(nmfTitle, 'input'), 'Thoughtful choices');
  LRevision := GRevision;
  GHost.Click(ActionSelector(nmeSave));
  AwaitRevision(LRevision + 1);
  LAfter := Source;
  Check(Pos('Thoughtful choices', LAfter) > 0, 'physical form save generates typed adjacent source');
  LMenu := Call('nyx_menus', [NyxField('name', NyxData('actions')),
    NyxField('itemLimit', NyxData(1))]);
  Check(LMenu.Field('options').Field('title').AsText = 'Thoughtful choices',
    'MCP observes the editor menu title');
  GHost.Capture('menu-properties');
  GHost.Click('[data-node=action-undo]');
  AwaitRevision(GRevision + 1);
  Check(Source = LBefore, 'physical Undo restores the exact paired pre-save source');
  GHost.Click('[data-node=action-redo]');
  AwaitRevision(GRevision + 1);
  Check(Source = LAfter, 'physical Redo restores the exact menu/source pair');

  Inspector(True);
  WriteLn('Ordinary Studio / first callback');
  Flush(Output);
  WaitFor(CNamedAdd);
  Check(Pos('OnActivate', GHost.ElementHTML(CNamedCard + ' [data-node$=-title]')) > 0,
    'Events exposes the declared menu completion callback');
  GHost.Click(CNamedAdd);
  AwaitRevision(GRevision + 1);
  WaitFor('[data-node=studio-code]');
  LCallbackSource := Source;
  Check((Pos('nseActivate', LCallbackSource) > 0) and (Pos('TODO', LCallbackSource) > 0),
    'Add callback creates fluent named registration and a Pascal TODO implementation');
  WaitField('[data-node=studio-code]', LCallbackSource);
  LSourceBox := GHost.Bounds('[data-node=studio-source-mount]');
  Check((LSourceBox.Height > 80) and (LSourceBox.Top >= 0) and
    (LSourceBox.Top + LSourceBox.Height <= 900),
    'callback source has a positive pane inside the actual host viewport');
  GHost.Capture('callback-source');
  VerifyCaret(LCallbackSource);

  if GWidth > 960 then
  begin
    LStageHeight := GHost.Bounds('[data-node=studio-stage]').Height;
    GHost.DragTouch('[data-node=studio-details-split] > .nyx-split-divider', 0, -64);
    Check(GHost.Bounds('[data-node=studio-stage]').Height > LStageHeight + 40,
      'public details touch grip enlarges the desktop design/source stage');
    Check(Source = LCallbackSource, 'details resizing changes no accepted design/source');
    GHost.Capture('resized-workspace');
  end;

  Inspector(True);
  WaitFor(CFirstSource);
  WriteLn('Ordinary Studio / callback navigation');
  Flush(Output);
  LRevision := GRevision;
  GHost.Click(CFirstSource);
  WaitFor('[data-node=studio-code]');
  WaitField('[data-node=studio-code]', LCallbackSource);
  Check(Call('nyx_session', []).Field('revision').AsInteger = LRevision,
    'source navigation adds no document history');
  VerifyCaret(LCallbackSource);
  Inspector(True);
  GHost.Click(CNamedAdd);
  AwaitRevision(GRevision + 1);
  Inspector(True);
  WaitFor(CNamedCard + ' [data-node$=-callback-1-source]');
  WriteLn('Ordinary Studio / multiple callbacks and policy');
  Flush(Output);
  Check(Pos('2 registrations', GHost.ElementHTML(CNamedCard + ' [data-node$=-count]')) > 0,
    'two ordered registrations are visible');
  Choose(CNamedCard + ' [data-node$=-policy] select', 1);
  AwaitRevision(GRevision + 1);
  Check(Pos('neAsynchronous', Source) > 0, 'per-event execution policy reaches the accepted Pascal');
  LBefore := Source;
  WriteLn('Ordinary Studio / confirmed removal');
  Flush(Output);
  GHost.Click(CFirstRemove);
  WaitFor('[data-node=event-removal-warning]');
  Check(Source = LBefore, 'removal request only reveals the warning');
  GHost.Click('[data-node=event-removal-cancel]');
  WaitFor('[data-node=event-removal-warning]', False);
  Check(Source = LBefore, 'cancel keeps both registrations and exact source');
  GHost.Click(CFirstRemove);
  WaitFor('[data-node=event-removal-confirm]');
  GHost.Capture('callback-removal-warning');
  GHost.Click('[data-node=event-removal-confirm]');
  AwaitRevision(GRevision + 1);
  WaitFor(CNamedCard + ' [data-node$=-callback-1-source]', False);
  LAfter := Source;
  Check(LAfter <> LBefore, 'confirmed removal changes the accepted registration pair');
  GHost.Click('[data-node=action-undo]');
  AwaitRevision(GRevision + 1);
  Check(Source = LBefore, 'one Undo restores exact registrations, policy and source');
  GHost.Click('[data-node=action-redo]');
  AwaitRevision(GRevision + 1);
  Check(Source = LAfter, 'Redo repeats exact confirmed registration removal');
  Save('nyx.generated.view.pas', LAfter);
  WriteLn('Ordinary Studio / both application builds');
  Flush(Output);
  Build('browser');
  Build('lcl');

  Interact(LAfter);
end;

{ Full ordinary bar journey. The companion is composed semantically in an owned
  ordinary workspace; browser input establishes only physical authoring/runtime
  behavior. Agent updates must reach the actual inspector and retain unrelated
  unfinished presentation while connection presence alone changes. }
procedure BarJourney;
var
  LBefore: TNyxText;
  LAfter: TNyxText;
  LMask: TNyxText;
  LReply: TNyxDataValue;
  LPeer: TNyxMCPTestClient;
  LRow: TNyxText;
  LWitness: TNyxText;
  LPlan: INyxMenuBarDefinition;
  LRevision: Integer;

  function Query: TNyxDataValue;
  begin
    Result := Call('nyx_menus', [NyxField('row', NyxData('workspace-menu-bar')),
      NyxField('itemLimit', NyxData(1))]);
    Check(Result.Field('revision').AsInteger = GRevision,
      'bounded bar context belongs to the exact active revision');
  end;

  procedure Edit(const AEdit: TNyxMenuEdit);
  begin
    LRevision := GRevision;
    Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation)),
      NyxField('operations', NyxArray([AEdit.ToData]))]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
    Check(GRevision = LRevision + 1, 'one semantic bar operation is one paired revision');
  end;

  procedure ShowRow;
  begin

    if GHost.Exists('[data-node=studio-panelbar]') then
    begin
      GHost.Click('[data-node=action-panel-design]');
    end;

    if not GHost.Exists('[data-node=studio-agents]') then
    begin
      GHost.Click('[data-node=action-actions]');
      WaitFor('.nyx-popover:popover-open [data-node=studio-menu-project]');
      GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-project]');
      WaitFor('.nyx-popover:popover-open [data-node=studio-menu-agents]');
      GHost.Click('.nyx-popover:popover-open [data-node=studio-menu-agents]');
      WaitFor('.nyx-popover:popover-open', False);
    end;

    if not GHost.Exists(LRow) and GHost.Exists('[data-node=action-details-toggle]') then
    begin
      GHost.Click('[data-node=action-details-toggle]');
    end;
    WaitFor(LRow);
  end;

  procedure Physical(AAction: TNyxMenuBarEditorAction; AHeading: Integer = -1);
  begin
    LRevision := GRevision;
    GHost.Click(BarAction(AAction, AHeading));
    AwaitRevision(LRevision + 1);
  end;

  procedure History(const ADirection: TNyxText);
  begin
    Call('nyx_history', [NyxField('direction', NyxData(ADirection)),
      NyxField('expectedRevision', NyxData(GRevision)),
      NyxField('operationId', NyxData(Operation))]);
    GRevision := Call('nyx_session', []).Field('revision').AsInteger;
  end;

begin
  WriteLn('Ordinary Studio / bar Properties and observing draft');
  Flush(Output);
  WaitFor('[data-node=action-actions]');
  Inspector(False);
  WaitField(BarField(nbfLabel), 'Studio commands');
  LReply := Query;
  Check((LReply.Field('total').AsInteger = 3) and LReply.Field('hasMore').AsBoolean and
    (LReply.Field('headings').Count = 1), 'authenticated bar query pages ordered heading meaning');
  LBefore := Source;
  GHost.ReplaceText(BarField(nbfLabel), 'My command workspace');
  Inspector(True);
  Inspector(False);
  WaitField(BarField(nbfLabel), 'My command workspace');
  LRow := '[data-node="studio-agent-workspace-' + GWorkspace + '"]';
  ShowRow;
  LWitness := 'Scooty bar draft witness ' + TNyxText(IntToStr(GetTickCount64));
  LPeer := TNyxMCPTestClient.Create(ParamStr(1), LWitness);
  try
    LReply := LPeer.Tool('nyx_session', NyxObject([NyxField('workspace', NyxData(GWorkspace))]));
    Check(not LReply.Field('isError').AsBoolean, 'independent bar witness reads only the owned workspace');
    WaitText(LRow, LWitness, True);
    Inspector(False);
    WaitField(BarField(nbfLabel), 'My command workspace');
    GHost.Capture('bar-retained-draft');
    ShowRow;
    LPeer.Close;
    WaitText(LRow, LWitness, False);
    Inspector(False);
    WaitField(BarField(nbfLabel), 'My command workspace');
  finally
    try
      LPeer.Close;
    finally
      LPeer.Free;
    end;
  end;
  Check(Source = LBefore, 'tab/roster rebuilds retain unfinished bar input without publishing');
  GHost.Click(BarField(nbfWrap));
  GHost.ReplaceText(BarField(nbfSearchWindow), '2300');
  GHost.Tab;
  Choose(BarField(nbfSearchMatch, 'select'), 0);
  Physical(nmbSave);
  LAfter := Source;
  LReply := Query;
  Check((LReply.Field('options').Field('label').AsText = 'My command workspace') and
    LReply.Field('options').Field('wrap').AsBoolean and
    (LReply.Field('options').Field('searchWindowMS').AsInteger = 2300) and
    (LReply.Field('options').Field('searchMatch').AsText = 'folded'),
    'physical whole-form Save publishes complete typed policy to MCP');
  GHost.Click('[data-node=action-undo]');
  AwaitRevision(GRevision + 1);
  Check(Source = LBefore, 'physical Undo restores exact pre-bar-editor Pascal');
  GHost.Click('[data-node=action-redo]');
  AwaitRevision(GRevision + 1);
  Check(Source = LAfter, 'physical Redo restores exact complete bar candidate');
  Physical(nmbMoveLater, 0);
  Check(Query.Field('headings').Item(0).Field('part').AsText = 'edit',
    'nested compound origin reaches the ordinary reorder command');
  GHost.Click('[data-node=' + NyxMenuBarEditorFieldID(
    NyxMenuBarEditorHeadingID('inspector-menu-bar', 0), nbfConfirm) + '] input');
  Physical(nmbRemoveHeading, 0);
  Check(Query.Field('total').AsInteger = 2, 'reviewed physical removal keeps the other headings');
  LBefore := Source;
  GHost.Click(BarField(nbfConfirm));
  Physical(nmbMask);
  LReply := Query;
  Check(LReply.Field('localDeclared').AsBoolean and not LReply.Field('configured').AsBoolean,
    'physical suppression is an explicit local mask, not registry removal');
  LMask := Source;
  Edit(NyxInheritMenuBar(NyxControl('workspace-menu-bar')));
  Check(not Query.Field('localDeclared').AsBoolean, 'authenticated inheritance removes only local grouping');
  History('undo');
  Check(Source = LMask, 'semantic Undo restores the exact explicit bar mask');
  History('undo');
  Check(Source = LBefore, 'semantic Undo restores the exact remaining headings');
  LPlan := BarPlan('Shared command workspace');
  Edit(NyxConfigureMenuBar(NyxControl('workspace-menu-bar'), LPlan));
  WaitField(BarField(nbfLabel), 'Shared command workspace');
  GHost.Capture('bar-semantic-observed');
  LBefore := Source;
  Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision - 1)),
    NyxField('operationId', NyxData(Operation)),
    NyxField('operations', NyxArray([NyxNoMenuBar(NyxControl('workspace-menu-bar')).ToData]))], True, True);
  Call('nyx_transaction', [NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData(Operation)), NyxField('operations', NyxArray([
      NyxConfigureMenuBar(NyxControl('workspace-menu-bar'),
        NewNyxMenuBarDefinition(NyxMenuBar('Invalid heading'))
          .Heading(NyxPart('absent'), NyxMenuRef('actions'))).ToData]))], True, True);
  Check(Source = LBefore, 'stale and invalid semantic bar candidates retain the accepted pair');
  Build('browser');
  Build('lcl');
  Build('browser', 'view');
  Build('lcl', 'view');
  Save('nyx.generated.view.pas', LBefore);

  WriteLn('Ordinary Studio / coordinated bar Interact');
  Flush(Output);
  ShowRow;
  WaitFor('[data-node=action-preview]');
  GHost.Click('[data-node=action-preview]');
  GHost.Click('[data-node=studio-canvas] [data-node=bar-file]');
  WaitFor('.nyx-popover:popover-open [data-node=copy-draft]');
  LWitness := 'Scooty bar portal witness ' + TNyxText(IntToStr(GetTickCount64));
  LPeer := TNyxMCPTestClient.Create(ParamStr(1), LWitness);
  try
    LReply := LPeer.Tool('nyx_session', NyxObject([NyxField('workspace', NyxData(GWorkspace))]));
    Check(not LReply.Field('isError').AsBoolean, 'independent witness joins the mounted bar workspace');
    WaitText(LRow, LWitness, True);
    Check(GHost.Exists('.nyx-popover:popover-open [data-node=copy-draft]'),
      'bound bar dropdown survives observing roster insertion');
    LPeer.Close;
    WaitText(LRow, LWitness, False);
    Check(GHost.Exists('.nyx-popover:popover-open [data-node=copy-draft]'),
      'bound bar dropdown survives observing roster retirement');
  finally
    try
      LPeer.Close;
    finally
      LPeer.Free;
    end;
  end;
  GHost.Key(nbkRight);
  WaitFor('[data-node=studio-canvas] [data-node=bar-edit][aria-expanded=true]');
  GHost.Click('[data-node=studio-canvas] [data-node=bar-view]');
  WaitFor('.nyx-popover:popover-open [data-node=compact-density]');
  GHost.Click('.nyx-popover:popover-open [data-node=compact-density]');
  WaitFor('.nyx-popover:popover-open', False);
  GHost.Click('[data-node=studio-canvas] [data-node=bar-view]');
  WaitFor('.nyx-popover:popover-open [data-node=compact-density][aria-checked=true]');
  GHost.Capture('bar-interact');
  GHost.Key(nbkEscape);
  WaitFor('.nyx-popover:popover-open', False);
  Check(Source = LBefore, 'mounted bar state changes preserve exact authored Pascal');
end;

{ A bounded diagnostic reuses an explicitly owned accepted fixture. It changes
  presentation only, so an unresponsive navigation path can be located without
  composing another project or replaying callback mutations. }
procedure Navigation;
const
  CAllocation: array[0..6] of TNyxText = ('studio-stage', 'studio-split',
    'studio-canvas', 'studio-source-mount', 'studio-source-pane', 'studio-code',
    'inspector-tab-events');
var
  LSource: TNyxText;
  LIndex: Integer;
  LBox: TNyxBrowserBox;
  LAllocation: array of TNyxDataField;
begin
  WaitFor('[data-node=action-actions]');
  LSource := Source;
  Inspector(True);
  WaitFor('[data-node^=event-named-] [data-node$=-callback-0-source]');
  WriteLn('Navigation / open registered callback source');
  Flush(Output);
  GHost.Click('[data-node^=event-named-] [data-node$=-callback-0-source]');
  WaitField('[data-node=studio-code]', LSource);
  { Capture host allocation before attempting the failing return path. These
    seven physical faces are bounded diagnostic evidence, not editor state or
    permission to infer visibility from a source field's mere DOM presence. }
  SetLength(LAllocation, Length(CAllocation));
  for LIndex := 0 to High(CAllocation) do
  begin

    if GHost.Exists('[data-node=' + CAllocation[LIndex] + ']') then
    begin
      LBox := GHost.Bounds('[data-node=' + CAllocation[LIndex] + ']');
      LAllocation[LIndex] := NyxField(CAllocation[LIndex], NyxObject([
        NyxField('left', NyxData(LBox.Left)), NyxField('top', NyxData(LBox.Top)),
        NyxField('width', NyxData(LBox.Width)), NyxField('height', NyxData(LBox.Height))]));

      if CAllocation[LIndex] = 'studio-stage' then
      begin
        Check((LBox.Height >= 300) and (LBox.Top >= 0) and
          (LBox.Top + LBox.Height <= 900),
          'desktop details retain a positive visible design/source allocation');
      end;
    end
    else
    begin
      LAllocation[LIndex] := NyxField(CAllocation[LIndex], NyxNull);
    end;
  end;
  Save('navigation-allocation.json', NyxObject(LAllocation).ToJSON);
  GHost.Capture('navigation-source');
  WriteLn('Navigation / return to Events');
  Flush(Output);
  Inspector(True);
  WaitFor('[data-node^=event-named-] [data-node$=-callback-0-source]');
  Check(Source = LSource, 'callback navigation preserves the exact accepted source');
end;

begin
  try

    if not (ParamCount in [4, 5, 6]) then
    begin
      raise Exception.Create('Use menu editor observer <isolated config.toml> <loopback editor URL> <fresh evidence directory> <CSS width> [retained owned workspace]');
    end;
    GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(3)));
    GWidth := StrToInt(ParamStr(4));

    GResetFixture := (ParamCount = 6) and (ParamStr(6) = 'reset-fixture');
    GInteractOnly := (ParamCount = 6) and (ParamStr(6) = 'interact');
    GObserveOnly := (ParamCount = 6) and (ParamStr(6) = 'observe');
    GBarEditor := (ParamCount = 6) and (ParamStr(6) = 'bar');
    GHeight := 900;

    if GBarEditor and (GWidth <= 960) then
    begin
      { Match a tighter mobile viewing area, including its scrolling inspector
        and popup bounds, instead of qualifying only a tall emulated phone. }
      GHeight := 640;
    end;

    if GBarEditor and (ParamStr(5) <> 'new') then
    begin
      raise Exception.Create('Bar qualification requires new, never a retained user workspace');
    end;

    if (ParamCount = 6) and (ParamStr(6) <> 'navigation') and
      not GResetFixture and not GInteractOnly and not GObserveOnly and not GBarEditor then
    begin
      raise Exception.Create('Use navigation, interact, observe, reset-fixture or new/bar');
    end;

    if DirectoryExists(GDirectory) then
    begin
      raise Exception.Create('Evidence directory must be new; retain prior input results');
    end;
    ForceDirectories(GDirectory);
    GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty ordinary menu editor');
    Compose;
    { Primary is represented by an omitted context. An explicitly empty query
      is an invalid reference, just as it is at the authenticated MCP boundary. }

    if GWorkspace = '' then
    begin
      GHost := TNyxBrowserPipe.Create(ParamStr(2), GDirectory, GWidth, GHeight);
    end
    else
    begin
      GHost := TNyxBrowserPipe.Create(ParamStr(2) + '?workspace=' + GWorkspace,
        GDirectory, GWidth, GHeight);
    end;

    if GBarEditor then
    begin
      BarJourney;
    end
    else if GObserveOnly then
    begin
      ObserveMenuEditor;
    end
    else if GResetFixture then
    begin
      ResetFixture;
    end
    else if GInteractOnly then
    begin
      WaitFor('[data-node=action-actions]');
      Inspector(True);
      WaitFor('[data-node^=event-named-] [data-node$=-callback-0-source]');
      GHost.Click('[data-node^=event-named-] [data-node$=-callback-0-source]');
      WaitFor('[data-node=studio-code]');
      Interact(Source);
    end
    else if ParamCount = 6 then
    begin
      Navigation;
    end
    else
    begin
      Journey;
    end;
    GHost.Capture('completed');
    Save('result.json', NyxObject([NyxField('checks', NyxData(GChecks)),
      NyxField('width', NyxData(GWidth)), NyxField('height', NyxData(GHeight)),
      NyxField('revision', NyxData(GRevision)),
      NyxField('workspace', NyxData(GWorkspace)), NyxField('result', NyxData('passed')),
      NyxField('navigationOnly', NyxData((ParamCount = 6) and
        not GResetFixture and not GInteractOnly and not GObserveOnly and not GBarEditor)),
      NyxField('interactOnly', NyxData(GInteractOnly)),
      NyxField('observeOnly', NyxData(GObserveOnly)),
      NyxField('barEditor', NyxData(GBarEditor)),
      NyxField('resetFixture', NyxData(GResetFixture))]).ToJSON);
    FreeAndNil(GHost);
    GClient.Close;
    FreeAndNil(GClient);

    if GObserveOnly then
    begin
      WriteLn('PASS ', GChecks, ' read-only observing Studio menu editor / CSS ', GWidth);
    end
    else if GResetFixture then
    begin
      WriteLn('PASS ', GChecks, ' owned fixture draft/history recovery / CSS ', GWidth);
    end
    else if GInteractOnly then
    begin
      WriteLn('PASS ', GChecks, ' ordinary browser Studio portal/Interact retention / CSS ', GWidth);
    end
    else if ParamCount = 6 then
    begin

      if GBarEditor then
      begin
        WriteLn('PASS ', GChecks, ' ordinary browser Studio bar workflow / CSS ', GWidth);
      end
      else
      begin
        WriteLn('PASS ', GChecks, ' ordinary browser Studio source/Events navigation / CSS ', GWidth);
      end;
    end
    else
    begin
      WriteLn('PASS ', GChecks, ' full ordinary browser Studio menu editor / CSS ', GWidth);
    end;
  except
    on LError: Exception do
    begin
      { Capture is secondary evidence. A dead debugger must not obscure the
        original failure or prevent retirement of this fixture's owned handles. }
      WriteLn('FAIL ', LError.Message);
      Flush(Output);

      if GHost <> nil then
      begin
        try
          GHost.Capture('failure');
        except
          on LCaptureError: Exception do
          begin
            WriteLn('Failure capture unavailable / ', LCaptureError.Message);
          end;
        end;
      end;
      FreeAndNil(GHost);

      if GClient <> nil then
      begin
        try
          { Retire only this fixture's own initialized MCP transport on failure.
            Keep the original assertion visible if transport cleanup also refuses. }
          GClient.Close;
        except
          on LCloseError: Exception do
          begin
            WriteLn('Transport retirement refused / ', LCloseError.ClassName);
          end;
        end;
      end;
      FreeAndNil(GClient);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
