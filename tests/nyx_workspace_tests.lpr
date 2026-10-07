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
program nyx_workspace_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.codegen, nyx.presentations,
  nyx.studio.projects, nyx.studio.agents,
  nyx.studio.workspaces, nyx.studio.presentation, nyx.studio.palette,
  nyx.studio.view, nyx.studio.inspector, nyx.studio.authoring
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;
  GPrimary: TNyxAgentSession;
  GWorkspaces: TNyxStudioWorkspaces;
  GBaseline: TNyxDataValue;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Frame(ASession: TNyxAgentSession): TNyxDataValue;
begin
  Result := ASession.Exchange(NyxObject([NyxField('op', NyxData('observe'))]));
end;

procedure EqualFrame(const ABefore, AAfter: TNyxDataValue);
var
  LIndex: Integer;
const
  CKeys: array[0..8] of TNyxText = ('revision', 'title', 'selection', 'view',
    'pages', 'components', 'pendingDraft', 'canUndo', 'canRedo');
begin
  Check(ABefore.Field('project').AsText = AAfter.Field('project').AsText,
    'Project accepted/draft/base bytes remain exact');
  for LIndex := 0 to High(CKeys) do
  begin
    Check(ABefore.Field('session').Field(CKeys[LIndex]).ToJSON =
      AAfter.Field('session').Field(CKeys[LIndex]).ToJSON,
      'Project retains its own ' + CKeys[LIndex]);
  end;
end;

procedure Preserved;
begin
  EqualFrame(GBaseline, Frame(GPrimary));
end;

function TitleArguments(ARevision: Integer; const AOperation, ATitle: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('expectedRevision', NyxData(ARevision)),
    NyxField('operationId', NyxData(AOperation)),
    NyxField('operations', NyxArray([NyxObject([
      NyxField('op', NyxData('title')), NyxField('value', NyxData(ATitle))])]))]);
end;

function Creation(const AOperation, ABase: TNyxText): TNyxDataValue;
const
  CLabel: TNyxText = 'Workshop 🌙漢字';
begin
  Result := NyxObject([NyxField('mode', NyxData('create')),
    NyxField('expectedRevision', NyxData(GPrimary.Revision)),
    NyxField('operationId', NyxData(AOperation)), NyxField('label', NyxData(CLabel)),
    NyxField('base', NyxData(ABase))]);
end;

procedure Refuse(const AOwner, ATool: TNyxText; const AArguments: TNyxDataValue);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    GWorkspaces.Call(ATool, AOwner, 'Scooty', AArguments);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Invalid or unauthorized workspace operation refuses');
  Preserved;
end;

procedure PresentationChecks;
var
  LOriginal: TNyxStudioPresentation;
  LDecoded: TNyxStudioPresentation;
  LPacket: TNyxDataValue;
  LBad: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCase: Integer;
  LKey: TNyxText;
  LValue: TNyxDataValue;
  LRefused: Boolean;

  function AllocationField(const AName: TNyxText): Boolean;
  begin
    Result := (AName = 'detailsPercent') or (AName = 'detailsExpanded') or
      (AName = 'canvasToolsVisible') or (AName = 'canvasExpanded');
  end;
begin
  Check(NyxStudioScrollPosition(-12.5) = 0, 'Negative platform overscroll preserves a valid preference');
  Check(NyxStudioScrollPosition(0) = 0, 'Origin scroll remains exact');
  Check(NyxStudioScrollPosition(12.49) = 12, 'Fractional scroll rounds below the nearest logical pixel');
  Check(NyxStudioScrollPosition(12.51) = 13, 'Fractional scroll rounds above the nearest logical pixel');
  Check(NyxStudioScrollPosition(12.5) = 13, 'Exact half pixels round consistently on both targets');
  Check(NyxStudioScrollPosition(2147483647.25) = 2147483647,
    'Large platform scroll saturates at the portable signed bound');
  LOriginal := DefaultNyxStudioPresentation;
  LOriginal.CodeVisible := True;
  LOriginal.SourceTab := nstMessages;
  LOriginal.SourceExpanded := True;
  LOriginal.CanvasPercent := 10;
  LOriginal.Phone := True;
  LOriginal.AgentsVisible := True;
  LOriginal.Panel := High(TNyxStudioPanel);
  LOriginal.InspectorTab := High(TNyxInspectorTab);
  LOriginal.NewStateName := 'draft 🌙';
  LOriginal.NewStateValue := '漢字 👩‍💻';
  LOriginal.NewStateInput := High(TNyxStudioStateInput);
  LOriginal.CanvasView := 'own page 🌙';
  LOriginal.PresentationSelection := TNyxPresentationSelection.Use(NyxPresentation('reading 🌙'));
  LOriginal.CodeCaretStart := 2147483646;
  LOriginal.CodeCaretEnd := 2147483647;
  LOriginal.CodeScrollTop := 2147483647;
  LOriginal.Palette.Search := 'button 🌙漢字';
  LOriginal.Palette.Mode := pmGrouped;
  LDecoded := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LOriginal));
  Check(EncodeNyxStudioPresentation(LDecoded) = EncodeNyxStudioPresentation(LOriginal),
    'Project presentation retains all typed fields, Unicode and boundary carets exactly');
  LOriginal.CanvasPercent := 90;
  LDecoded := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LOriginal));
  Check(LDecoded.CanvasPercent = 90, 'Presentation accepts both public splitter boundaries');
  LPacket := TNyxDataValue.ParseJSON(EncodeNyxStudioPresentation(LOriginal));
  { The previous strict packet remains readable. New per-project choices use
    their defaults, while every earlier preference and Unicode value survives. }
  SetLength(LFields, LPacket.Count - 7);
  LCase := 0;
  for LIndex := 0 to LPacket.Count - 1 do
  begin

    if (LPacket.Key(LIndex) <> 'sourceTab') and
      (LPacket.Key(LIndex) <> 'sourceExpanded') and
      (LPacket.Key(LIndex) <> 'presentation') and
      not AllocationField(LPacket.Key(LIndex)) then
    begin
      LValue := LPacket.Field(LPacket.Key(LIndex));

      if LPacket.Key(LIndex) = 'version' then
      begin
        LValue := NyxData(2);
      end;
      LFields[LCase] := NyxField(LPacket.Key(LIndex), LValue);
      Inc(LCase);
    end;
  end;
  LDecoded := DecodeNyxStudioPresentation(NyxObject(LFields).ToJSON);
  Check((LDecoded.SourceTab = nstSource) and not LDecoded.SourceExpanded and
    not LDecoded.PresentationSelection.Reference.Defined,
    'Version 2 preferences migrate to the inline source view');
  LDecoded.SourceTab := LOriginal.SourceTab;
  LDecoded.SourceExpanded := LOriginal.SourceExpanded;
  LDecoded.PresentationSelection := LOriginal.PresentationSelection;
  Check(EncodeNyxStudioPresentation(LDecoded) = LPacket.ToJSON,
    'Migration retains every earlier per-project preference exactly');
  { Existing Studio installations also wrote version 3, which already owns
    source tabs and expansion. Its absent manual choice must not discard those
    fields or any other per-project preference during this migration. }
  SetLength(LFields, LPacket.Count - 5);
  LCase := 0;
  for LIndex := 0 to LPacket.Count - 1 do
  begin

    if (LPacket.Key(LIndex) <> 'presentation') and
      not AllocationField(LPacket.Key(LIndex)) then
    begin
      LValue := LPacket.Field(LPacket.Key(LIndex));

      if LPacket.Key(LIndex) = 'version' then
      begin
        LValue := NyxData(3);
      end;
      LFields[LCase] := NyxField(LPacket.Key(LIndex), LValue);
      Inc(LCase);
    end;
  end;
  LDecoded := DecodeNyxStudioPresentation(NyxObject(LFields).ToJSON);
  Check((LDecoded.SourceTab = LOriginal.SourceTab) and
    (LDecoded.SourceExpanded = LOriginal.SourceExpanded) and
    not LDecoded.PresentationSelection.Reference.Defined,
    'Version 3 migration retains source presentation and defaults its manual choice');
  LDecoded.PresentationSelection := LOriginal.PresentationSelection;
  Check(EncodeNyxStudioPresentation(LDecoded) = LPacket.ToJSON,
    'Version 3 migration retains every earlier Unicode, caret and workspace preference exactly');
  { Version 4 is the last observing release's exact packet. Its manual preview
    and all unrelated preferences survive, while new details start collapsed. }
  SetLength(LFields, LPacket.Count - 4);
  LCase := 0;
  for LIndex := 0 to LPacket.Count - 1 do
  begin

    if not AllocationField(LPacket.Key(LIndex)) then
    begin
      LValue := LPacket.Field(LPacket.Key(LIndex));

      if LPacket.Key(LIndex) = 'version' then
      begin
        LValue := NyxData(4);
      end;
      LFields[LCase] := NyxField(LPacket.Key(LIndex), LValue);
      Inc(LCase);
    end;
  end;
  LDecoded := DecodeNyxStudioPresentation(NyxObject(LFields).ToJSON);
  Check(EncodeNyxStudioPresentation(LDecoded) = LPacket.ToJSON,
    'Version 4 retains all preferences and supplies collapsed allocation defaults');
  LOriginal.DetailsPercent := 60;
  LOriginal.DetailsExpanded := True;
  LOriginal.CanvasToolsVisible := True;
  LOriginal.CanvasExpanded := True;
  LPacket := TNyxDataValue.ParseJSON(EncodeNyxStudioPresentation(LOriginal));
  LDecoded := DecodeNyxStudioPresentation(LPacket.ToJSON);
  Check(EncodeNyxStudioPresentation(LDecoded) = LPacket.ToJSON,
    'Version 5 independently retains all workspace allocation choices');
  for LCase := 0 to 26 do
  begin
    LKey := 'version';
    LValue := NyxData(6);
    case LCase of
      1:
      begin
        LKey := 'canvasPercent';
        LValue := NyxData(9);
      end;
      2:
      begin
        LKey := 'canvasPercent';
        LValue := NyxData(91);
      end;
      3:
      begin
        LKey := 'canvasPercent';
        LValue := NyxData('65');
      end;
      4:
      begin
        LKey := 'codeVisible';
        LValue := NyxData('true');
      end;
      5:
      begin
        LKey := 'panel';
        LValue := NyxData(Ord(High(TNyxStudioPanel)) + 1);
      end;
      6:
      begin
        LKey := 'newStateInput';
        LValue := NyxData(-1);
      end;
      7:
      begin
        LKey := 'codeCaretEnd';
        LValue := NyxData(1);
      end;
      8:
      begin
        LKey := 'codeScrollTop';
        LValue := NyxData(-1);
      end;
      9:
      begin
        LKey := 'codeCaretEnd';
        LValue := NyxData(2147483648.0);
      end;
      10:
      begin
        LKey := 'outputTarget';
        LValue := NyxData('unknown');
      end;
      11:
      begin
        LKey := 'palette';
        LValue := NyxData('{}');
      end;
      12:
      begin
        LKey := 'newStateName';
        LValue := NyxData(42);
      end;
      13:
      begin
        LKey := 'panel';
        LValue := NyxData(0.5);
      end;
      14:
      begin
        LKey := 'sourceTab';
        LValue := NyxData(Ord(High(TNyxStudioSourceTab)) + 1);
      end;
      15:
      begin
        LKey := 'sourceTab';
        LValue := NyxData('source');
      end;
      16:
      begin
        LKey := 'sourceExpanded';
        LValue := NyxData('true');
      end;
      17:
      begin
        LKey := 'version';
        LValue := NyxData(2);
      end;
      18:
      begin
        LKey := 'presentation';
        LValue := NyxData(42);
      end;
      19:
      begin
        LKey := 'presentation';
        LValue := NyxData(True);
      end;
      20:
      begin
        LKey := 'presentation';
        LValue := NyxData('');
      end;
      21:
      begin
        LKey := 'presentation';
        LValue := NyxData(TNyxText('reading') + #10);
      end;
      22:
      begin
        LKey := 'version';
        LValue := NyxData(3);
      end;
      23:
        begin
          LKey := 'detailsPercent';
          LValue := NyxData(14);
        end;
      24:
        begin
          LKey := 'detailsPercent';
          LValue := NyxData(61);
        end;
      25:
        begin
          LKey := 'detailsExpanded';
          LValue := NyxData('true');
        end;
      26:
        begin
          LKey := 'detailsPercent';
          LValue := NyxData(32.5);
        end;
    end;
    SetLength(LFields, LPacket.Count);
    for LIndex := 0 to LPacket.Count - 1 do
    begin
      LFields[LIndex] := NyxField(LPacket.Key(LIndex), LPacket.Field(LPacket.Key(LIndex)));

      if LPacket.Key(LIndex) = LKey then
      begin
        LFields[LIndex] := NyxField(LKey, LValue);
      end;
    end;
    LBad := NyxObject(LFields);
    LRefused := False;
    try
      LDecoded := DecodeNyxStudioPresentation(LBad.ToJSON);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Malformed presentation refuses without publishing any field: ' + LKey);
    Check(EncodeNyxStudioPresentation(LOriginal) = LPacket.ToJSON,
      'Detached failed preference admission preserves its caller baseline');
  end;
end;

procedure LifetimeBudgets;
var
  LPrimary: TNyxAgentSession;
  LProjects: TNyxStudioWorkspaces;
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LReference: TNyxWorkspaceRef;
  LFirstReceipt: TNyxDataValue;
  LFirstRequest: TNyxDataValue;
  LRequest: TNyxDataValue;
  LBefore: TNyxText;
  LLabel: TNyxText;
  LIndex: Integer;
  LRefused: Boolean;
begin
  LDocument := TNyxDocument.Create;
  try
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LDocument.Free;
  end;
  LPrimary := TNyxAgentSession.Create(LPair);
  try
    LProjects := TNyxStudioWorkspaces.Create(LPrimary, 'budget-fixture');
    try
      LReference := LProjects.OpenProject('Presence', LPair);
      for LIndex := 1 to 64 do
      begin
        LProjects.RecordRequest('owner-' + IntToStr(LIndex), 'Scooty', LReference);
      end;
      Check(LProjects.Observe.Item(1).Field('connections').Count = 64,
        'Build/preview-only presence supports exactly sixty-four connection/project pairs');
      LBefore := LProjects.Observe.ToJSON;
      LRefused := False;
      try
        LProjects.RecordRequest('overflow', 'Scooty', LReference);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LProjects.Observe.ToJSON = LBefore),
        'Presence overflow refuses without changing any existing session or observer metadata');
      LProjects.ReleaseOwner('owner-1');
      LProjects.RecordRequest('replacement', 'Scooty', LReference);
      Check(LProjects.Observe.Item(1).Field('connections').Count = 64,
        'Explicit disconnect admits a fresh connection without retiring its project');
      LProjects.CloseProject(LReference, LProjects.Find(LReference).Revision, True);
      Check(LProjects.Observe.Count = 1, 'Confirmed close releases every connection for only that project');
      for LIndex := 1 to 64 do
      begin
        LRequest := NyxObject([NyxField('mode', NyxData('create')),
          NyxField('expectedRevision', NyxData(LPrimary.Revision)),
          NyxField('operationId', NyxData('creation-' + IntToStr(LIndex))),
          NyxField('label', NyxData('Receipt fixture')), NyxField('base', NyxData('empty'))]);

        if LIndex = 1 then
        begin
          LFirstRequest := LRequest.Copy;
        end;
        LRequest := LProjects.Manage('receipt-owner', 'Scooty', LRequest);

        if LIndex = 1 then
        begin
          LFirstReceipt := LRequest.Copy;
        end;
        LReference := NyxWorkspace(LRequest.Field('workspace').AsText);
        LProjects.CloseProject(LReference, LProjects.Find(LReference).Revision, True);
      end;
      Check(LProjects.Manage('receipt-owner', 'Scooty', LFirstRequest).ToJSON =
        LFirstReceipt.ToJSON, 'The oldest creation receipt survives sixty-four closed projects');
      Check(LProjects.Find(NyxWorkspace(LFirstReceipt.Field('workspace').AsText)) = nil,
        'Old retry does not resurrect a retired project');
      LRefused := False;
      try
        LProjects.Manage('receipt-owner', 'Scooty', NyxObject([
          NyxField('mode', NyxData('create')),
          NyxField('expectedRevision', NyxData(LPrimary.Revision)),
          NyxField('operationId', NyxData('creation-overflow')),
          NyxField('label', NyxData('Overflow')), NyxField('base', NyxData('empty'))]));
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LProjects.Observe.Count = 1),
        'Receipt overflow neither evicts retries nor publishes a new project');
      LProjects.ReleaseOwner('receipt-owner');
      LPair.Design := '{}';
      LRefused := False;
      try
        LProjects.OpenProject('Invalid pair', LPair);
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LProjects.Observe.Count = 1),
        'Invalid detached pair admission leaves the registry untouched');
      LPair := DecodeNyxProject(Frame(LPrimary).Field('project').AsText);
      LLabel := '';
      for LIndex := 1 to 256 do
      begin
        LLabel := LLabel + NyxScalarText($1f319);
      end;
      LReference := LProjects.OpenProject(LLabel, LPair);
      Check(LProjects.Observe.Item(1).Field('label').AsText = LLabel,
        'A label budget counts Unicode scalars consistently, independent of target storage units');
      LRefused := False;
      try
        LProjects.OpenProject(LLabel + NyxScalarText($1f319), LPair);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LProjects.Observe.Count = 2),
        'Supplementary Unicode label overflow leaves the admitted project intact');
      LRefused := False;
      try
        LProjects.CloseProject(NyxPrimaryWorkspace, LPrimary.Revision, True);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LProjects.Observe.Count = 2),
        'The primary editor cannot be retired through registry closure');
    finally
      LProjects.Free;
    end;
  finally
    LPrimary.Free;
  end;
end;

procedure Run;
var
  LPair: TNyxProjectPair;
  LFirst: TNyxWorkspaceRef;
  LSecond: TNyxWorkspaceRef;
  LClosed: TNyxWorkspaceRef;
  LValue: TNyxDataValue;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LBefore: TNyxDataValue;
  LRoot: TNyxDataValue;
  LTicket: TNyxDataValue;
  LRefused: Boolean;
  LIndex: Integer;
  LTotal: Integer;
  LSession: TNyxAgentSession;
const
  CDraft: TNyxText = #10 + '// Keep my project draft 🌙漢字';
begin
  PresentationChecks;
  LifetimeBudgets;
  GPrimary := TNyxAgentSession.Create;
  GWorkspaces := TNyxStudioWorkspaces.Create(GPrimary, 'qualified-service');
  GPrimary.Call('nyx_transaction', 'User', TitleArguments(GPrimary.Revision, 'user-one', 'User one'));
  GPrimary.Call('nyx_transaction', 'User', TitleArguments(GPrimary.Revision, 'user-two', 'User two'));
  GPrimary.Call('nyx_history', 'User', NyxObject([
    NyxField('expectedRevision', NyxData(GPrimary.Revision)),
    NyxField('operationId', NyxData('user-undo')), NyxField('direction', NyxData('undo'))]));
  LPair := DecodeNyxProject(Frame(GPrimary).Field('project').AsText);
  LPair.Pending := True;
  LPair.DraftBase := LPair.Source;
  LPair.Draft := LPair.Source + CDraft;
  GPrimary.Exchange(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', NyxData(GPrimary.Revision)),
    NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  GBaseline := Frame(GPrimary);
  Check(GBaseline.Field('session').Field('pendingDraft').AsBoolean and
    GBaseline.Field('session').Field('canUndo').AsBoolean and
    GBaseline.Field('session').Field('canRedo').AsBoolean,
    'User baseline includes a Unicode draft and both history stacks');

  LRequest := Creation('first-project', 'accepted');
  LReceipt := GWorkspaces.Manage('connection-a', 'Scooty', LRequest);
  LFirst := NyxWorkspace(LReceipt.Field('workspace').AsText);
  Check(GWorkspaces.Manage('connection-a', 'Scooty', LRequest).ToJSON = LReceipt.ToJSON,
    'Exact creation retry returns the immutable project receipt');
  LValue := GWorkspaces.Manage('connection-b', 'Scooty', Creation('second-project', 'empty'));
  LSecond := NyxWorkspace(LValue.Field('workspace').AsText);
  Check(LFirst.ID <> LSecond.ID, 'Project identity is independent of the same display actor');
  Check(not Frame(GWorkspaces.Find(LFirst)).Field('session').Field('pendingDraft').AsBoolean,
    'Accepted project seed excludes the primary pending draft');
  Check(Frame(GWorkspaces.Find(LSecond)).Field('session').Field('pages').AsInteger = 0,
    'Empty project seed includes no primary authored roots');
  Preserved;

  LSession := GWorkspaces.Find(LFirst);
  LRequest := NyxWithWorkspace(TitleArguments(LSession.Revision, 'shared-id', 'First project 🌙'), LFirst);
  LReceipt := GWorkspaces.Call('nyx_transaction', 'connection-a', 'Scooty', LRequest);
  Refuse('connection-b', 'nyx_transaction', LRequest);
  Check(GWorkspaces.Call('nyx_transaction', 'connection-a', 'Scooty', LRequest).ToJSON =
    LReceipt.ToJSON, 'Same display names do not share transaction retry authority');
  LRequest := NyxWithWorkspace(TitleArguments(LSession.Revision, 'shared-id', 'Other author 漢字'), LFirst);
  GWorkspaces.Call('nyx_transaction', 'connection-b', 'Scooty', LRequest);
  Check(LSession.Revision = 3, 'Another connection can use its own identical operation ID');
  LValue := GWorkspaces.Manage('connection-a', 'Scooty',
    NyxObject([NyxField('mode', NyxData('inspect')), NyxField('workspace', NyxData(LFirst.ID))]));
  Check((LValue.Field('connections').Count = 2) and
    (LValue.Field('connections').Item(0).Field('session').AsInteger <>
      LValue.Field('connections').Item(1).Field('session').AsInteger),
    'Observer distinguishes two sessions with identical friendly names');
  Check(Pos('connection-a', LValue.ToJSON) = 0, 'Observer omits private transport authority');

  LRoot := NyxArray([NyxObject([NyxField('root', NyxData('page')),
    NyxField('id', NyxData('home'))])]);
  LTicket := GWorkspaces.Call('nyx_roots', 'connection-a', 'Scooty',
    NyxWithWorkspace(NyxObject([NyxField('mode', NyxData('review')),
      NyxField('expectedRevision', NyxData(LSession.Revision)),
      NyxField('roots', LRoot)]), LFirst));
  Refuse('connection-b', 'nyx_roots', NyxWithWorkspace(NyxObject([
    NyxField('mode', NyxData('apply')),
    NyxField('expectedRevision', NyxData(LSession.Revision)),
    NyxField('operationId', NyxData('foreign-close-roots')),
    NyxField('roots', LRoot), NyxField('reviewID', LTicket.Field('reviewID'))]), LFirst));

  LPair := DecodeNyxProject(Frame(LSession).Field('project').AsText);
  LPair.Pending := True;
  LPair.DraftBase := LPair.Source;
  LPair.Draft := LPair.Source + CDraft;
  LSession.Exchange(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', NyxData(LSession.Revision)),
    NyxField('project', NyxData(EncodeNyxProject(LPair))),
    NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  LBefore := Frame(LSession);
  GWorkspaces.Call('nyx_transaction', 'connection-b', 'Scooty',
    NyxWithWorkspace(TitleArguments(GWorkspaces.Find(LSecond).Revision,
      'second-title', 'Independent second project'), LSecond));
  EqualFrame(LBefore, Frame(GWorkspaces.Find(LFirst)));
  Preserved;

  GWorkspaces.ReleaseOwner('connection-a');
  EqualFrame(LBefore, Frame(GWorkspaces.Find(LFirst)));
  Check(GWorkspaces.Manage('connection-b', 'Scooty', NyxObject([
    NyxField('mode', NyxData('inspect')), NyxField('workspace', NyxData(LFirst.ID))]))
    .Field('connections').Count = 1, 'Disconnect removes presence and retains the project');
  LRefused := False;
  try
    GWorkspaces.CloseProject(LFirst, LSession.Revision, False);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Declined close retains unsaved source/draft/history');
  EqualFrame(LBefore, Frame(GWorkspaces.Find(LFirst)));
  LRefused := False;
  try
    GWorkspaces.CloseProject(LFirst, LSession.Revision - 1, True);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Stale close consent refuses');
  GWorkspaces.CloseProject(LFirst, LSession.Revision, True);
  Check(GWorkspaces.Find(LFirst) = nil, 'Confirmed operator close retires only its project');
  Refuse('connection-b', 'nyx_session', NyxWithWorkspace(NyxObject([]), LFirst));
  Check(GWorkspaces.Find(LSecond) <> nil, 'Closing another project retains the second project');

  LRequest := Creation('close-retry', 'empty');
  LReceipt := GWorkspaces.Manage('connection-b', 'Scooty', LRequest);
  LClosed := NyxWorkspace(LReceipt.Field('workspace').AsText);
  GWorkspaces.CloseProject(LClosed, GWorkspaces.Find(LClosed).Revision, True);
  LTotal := GWorkspaces.Observe.Count;
  Check(GWorkspaces.Manage('connection-b', 'Scooty', LRequest).ToJSON = LReceipt.ToJSON,
    'Closed creation retry returns its original retired handle');
  Check((GWorkspaces.Find(LClosed) = nil) and (GWorkspaces.Observe.Count = LTotal),
    'Delayed retry never silently creates a replacement project');

  GPrimary.Exchange(NyxObject([NyxField('op', NyxData('configure')),
    NyxField('permission', NyxData('readOnly'))]));
  Check(GWorkspaces.Resolve(LSecond).Permission = apReadOnly,
    'Every project inherits the current operator permission');
  Refuse('connection-b', 'nyx_transaction',
    NyxWithWorkspace(TitleArguments(GWorkspaces.Find(LSecond).Revision,
      'readonly-edit', 'Refused'), LSecond));
  GPrimary.Exchange(NyxObject([NyxField('op', NyxData('configure')),
    NyxField('permission', NyxData('disabled'))]));
  Refuse('connection-b', 'nyx_session', NyxWithWorkspace(NyxObject([]), LSecond));
  GWorkspaces.ReleaseOwner('connection-b');
  Check(GWorkspaces.Find(LSecond) <> nil, 'Disabled disconnect still retains user project');
  GPrimary.Exchange(NyxObject([NyxField('op', NyxData('configure')),
    NyxField('permission', NyxData('edit'))]));
  Refuse('connection-c', 'nyx_session', NyxObject([NyxField('workspace', NyxData(''))]));
  Refuse('connection-c', 'nyx_session', NyxObject([
    NyxField('workspace', NyxData(LSecond.ID)), NyxField('review', NyxData('review-1'))]));
  Refuse('connection-c', 'nyx_session', NyxWithWorkspace(NyxObject([]),
    NyxWorkspace('other-service.project-1')));
  for LIndex := 1 to 7 do
  begin
    GWorkspaces.CreateProject('Budget project', nwbEmpty, GPrimary.Revision);
  end;
  Check(GWorkspaces.Observe.Count = 9, 'Primary and eight independently owned projects are bounded');
  LRefused := False;
  try
    GWorkspaces.CreateProject('Overflow', nwbEmpty, GPrimary.Revision);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (GWorkspaces.Observe.Count = 9), 'Overflow admission changes no existing project');
  Preserved;
end;

begin
  try
    try
      {$ifdef NYX_PRESENTATION_ONLY}
      { Browser UI qualification needs the changed strict preference boundary,
        independent of this fixture's large workspace lifetime-budget loop. }
      PresentationChecks;
      {$else}
      Run;
      {$endif}
    finally
      GWorkspaces.Free;
      GPrimary.Free;
    end;
    {$ifdef NYX_PRESENTATION_ONLY}
    WriteLn('PASS ', GChecks, ' editor presentation checks');
    {$else}
    WriteLn('PASS ', GChecks, ' concurrent project ownership checks');
    {$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-workspaces', 'passed');
    document.body.setAttribute('data-nyx-workspace-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-workspaces', 'failed');
      document.body.setAttribute('data-nyx-workspace-error', LException.Message);
      {$else}
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
