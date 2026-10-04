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
program nyx_agent_callback_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}Web,{$endif}
  nyx.text, nyx.types, nyx.data, nyx.model, nyx.callbacks, nyx.codec, nyx.scheduler,
  nyx.studio.session, nyx.studio.projects, nyx.studio.agents;

var
  GSession: TNyxAgentSession;
  GRevision: Integer;
  GCount: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GCount);
end;

function Arguments(const AMode, AID: TNyxText;
  const AChanges: TNyxDataValue; const AReview: TNyxText = ''): TNyxDataValue;
var
  LFields: array of TNyxDataField;
begin
  SetLength(LFields, 3);
  LFields[0] := NyxField('expectedRevision', NyxData(GRevision));
  LFields[1] := NyxField('mode', NyxData(AMode));
  LFields[2] := NyxField('changes', AChanges);

  if AID <> '' then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('operationId', NyxData(AID));
  end;

  if AReview <> '' then
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField('reviewID', NyxData(AReview));
  end;
  Result := NyxObject(LFields);
end;

function Change(const AOp, AOwner: TNyxText;
  const AEvent: TNyxDataValue; const ARegistration: TNyxText = ''): TNyxDataValue;
var
  LFields: array of TNyxDataField;
begin
  SetLength(LFields, 3);
  LFields[0] := NyxField('op', NyxData(AOp));
  LFields[1] := NyxField('id', NyxData(AOwner));
  LFields[2] := NyxField('event', AEvent);

  if ARegistration <> '' then
  begin
    SetLength(LFields, 4);
    LFields[3] := NyxField('registration', NyxData(ARegistration));
  end;
  Result := NyxObject(LFields);
end;

function Pair: TNyxText;
begin
  Result := EncodeNyxProject(GSession.PreviewPair(GRevision, 'home'));
end;

procedure Refuses(const AArgs: TNyxDataValue; const AReason: TNyxText;
  const AActor: TNyxText = 'Scooty');
var
  LBefore: TNyxText;
  LSummary: TNyxText;
  LRejected: Boolean;
begin
  LBefore := Pair;
  LSummary := GSession.Call('nyx_session', 'Scooty', NyxObject([])).ToJSON;
  LRejected := False;
  try
    GSession.Call('nyx_callbacks', AActor, AArgs);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (GSession.Revision = GRevision) and (Pair = LBefore), AReason);
  { Activity is intentionally changed by refusal; ordinary history, selection
    and active view must retain their previous values. }
  Check(GSession.Call('nyx_session', 'Scooty', NyxObject([])).Field('selection').AsText =
    TNyxDataValue.ParseJSON(LSummary).Field('selection').AsText, 'Refusal retains selection');
end;

procedure History(const ADirection, AID: TNyxText);
begin
  GSession.Call('nyx_history', 'Scooty', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData(AID)),
    NyxField('direction', NyxData(ADirection))]));
  Inc(GRevision);
end;

procedure Run;
var
  LClick: TNyxDataValue;
  LAdds: TNyxDataValue;
  LArgs: TNyxDataValue;
  LValue: TNyxDataValue;
  LReview: TNyxDataValue;
  LRemoves: TNyxDataValue;
  LFirst: TNyxText;
  LSecond: TNyxText;
  LBefore: TNyxText;
  LAdded: TNyxText;
  LMoved: TNyxText;
  LProject: TNyxProjectPair;
  LAuthor: TNyxStudioSession;
  LEvents: TNyxAuthoredEventInfos;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LOwner: TNyxText;
begin
  LClick := NyxObject([NyxField('trigger', NyxData('click'))]);
  LBefore := Pair;
  LAdds := NyxArray([Change('add', 'create-project', LClick),
    Change('add', 'create-project', LClick)]);
  LArgs := Arguments('apply', 'add-pair', LAdds);
  LValue := GSession.Call('nyx_callbacks', 'Scooty', LArgs);
  Inc(GRevision);
  Check(LValue.Field('revision').AsInteger = GRevision, 'Two handlers publish one revision');
  Check(LValue.Field('selection').AsText = 'home', 'Agent authoring retains operator selection');
  LFirst := LValue.Field('callbacks').Item(0).Field('registration').AsText;
  LSecond := LValue.Field('callbacks').Item(1).Field('registration').AsText;
  Check((LFirst <> '') and (LSecond <> LFirst), 'Collision-safe distinct crafted registrations');
  Check((LValue.Field('callbacks').Item(0).Field('line').AsInteger > 0) and
    (LValue.Field('callbacks').Item(1).Field('line').AsInteger > 0), 'Final-source TODO navigation for each add');
  LAdded := Pair;
  Check(GSession.Call('nyx_callbacks', 'Scooty', LArgs).ToJSON = LValue.ToJSON,
    'Exact add retry returns original receipt without duplicate code');
  History('undo', 'undo-add');
  Check(Pair = LBefore, 'One undo restores exact pre-batch pair');
  History('redo', 'redo-add');
  Check(Pair = LAdded, 'One redo restores both implementations and registrations');
  LValue := GSession.Call('nyx_callbacks', 'Scooty', Arguments('apply', 'reorder-policy',
    TNyxDataValue.ParseJSON('[{"op":"move","id":"create-project","event":{"trigger":"click"},"registration":"' +
      LFirst + '","index":1},{"op":"policy","id":"create-project","event":{"trigger":"click"},"policy":"ui-queue"}]')));
  Inc(GRevision);
  Check(LValue.Field('callbacks').Item(0).Field('index').AsInteger = 1, 'Forward move uses the zero-based final position');
  LProject := GSession.PreviewPair(GRevision, 'home');
  LAuthor := TNyxStudioSession.Create;
  try
    LAuthor.LoadProject(LProject);
    LEvents := NyxAuthoredEvents(LAuthor.Document.Find('create-project'));
    Check((LEvents[0].Callbacks[0].ID.Name = LSecond) and
      (LEvents[0].Callbacks[1].ID.Name = LFirst) and (LEvents[0].Policy = neUIQueue),
      'Admitted descriptors retain order, exact identities and event policy');
  finally
    LAuthor.Free;
  end;
  LMoved := Pair;
  LValue := GSession.Call('nyx_callbacks', 'Scooty', Arguments('apply', 'same-position',
    TNyxDataValue.ParseJSON('[{"op":"move","id":"create-project","event":{"trigger":"click"},"registration":"' +
      LSecond + '","index":0}]')));
  Check((LValue.Field('revision').AsInteger = GRevision) and (Pair = LMoved),
    'Moving to the current position preserves pair and revision');
  Refuses(Arguments('apply', 'invalid-group', NyxArray([Change('add', 'project-name',
    NyxObject([NyxField('trigger', NyxData('key-down'))])), Change('add', 'absent', LClick)])),
    'Later invalid owner rolls back an earlier valid code addition');
  Refuses(Arguments('apply', 'wrong-event', NyxArray([Change('add', 'create-project',
    NyxObject([NyxField('trigger', NyxData('click')), NyxField('name', NyxData('click'))]))])),
    'Ambiguous physical/semantic event identity refuses');
  Refuses(Arguments('apply', 'wrong-type', TNyxDataValue.ParseJSON(
    '[{"op":"move","id":"create-project","event":{"trigger":"click"},"registration":"' + LFirst + '","index":true}]')),
    'Position is strongly admitted as an integer');
  Refuses(Arguments('apply', 'wrong-index', TNyxDataValue.ParseJSON(
    '[{"op":"move","id":"create-project","event":{"trigger":"click"},"registration":"' + LFirst + '","index":2}]')),
    'Out-of-range order cannot modify source/history');
  Refuses(Arguments('apply', 'unsupported', NyxArray([Change('add', 'create-project',
    NyxObject([NyxField('name', NyxData('made-up-event'))]))])), 'Undeclared semantic event refuses');
  LRemoves := NyxArray([Change('remove', 'create-project', LClick, LSecond)]);
  Refuses(Arguments('apply', 'blind-removal', LRemoves), 'Removal cannot bypass review');
  LReview := GSession.Call('nyx_callbacks', 'Scooty', Arguments('review', '', LRemoves));
  Check((Pair = LMoved) and (GSession.Revision = GRevision), 'Review changes neither pair nor revision');
  Check(Pos('implementation is retained', LReview.Field('callbacks').Item(0).Field('warning').AsText) > 0,
    'Review warning explains retained Pascal implementation');
  Refuses(Arguments('apply', 'wrong-actor', LRemoves, LReview.Field('reviewID').AsText),
    'Review belongs to its actor', 'Another agent');
  Refuses(Arguments('apply', 'changed-review', NyxArray([Change('remove', 'create-project', LClick, LFirst)]),
    LReview.Field('reviewID').AsText), 'Review cannot authorize a different exact registration');
  LArgs := Arguments('apply', 'reviewed-removal', LRemoves, LReview.Field('reviewID').AsText);
  LValue := GSession.Call('nyx_callbacks', 'Scooty', LArgs);
  Inc(GRevision);
  Check(LValue.Field('callbacks').Item(0).Field('line').AsInteger > 0, 'Removed handler code retains a navigable location');
  Check(GSession.Call('nyx_callbacks', 'Scooty', LArgs).ToJSON = LValue.ToJSON,
    'Consumed review still allows exact successful retry');
  History('undo', 'undo-removal');
  Check(Pair = LMoved, 'Removal undo restores exact source and ordered registrations');
  Refuses(Arguments('apply', 'stale-review', LRemoves, LReview.Field('reviewID').AsText),
    'Old review is unusable after undo revision');

  { A reusable definition change affects inheriting instances; an instance edit
    materializes a local callback override without altering the definition. }
  LValue := GSession.Call('nyx_callbacks', 'Scooty', Arguments('apply', 'definition-add',
    NyxArray([Change('add', 'welcome-card', LClick)])));
  Inc(GRevision);
  LOwner := LValue.Field('callbacks').Item(0).Field('registration').AsText;
  LValue := GSession.Call('nyx_callbacks', 'Scooty', Arguments('apply', 'instance-add',
    NyxArray([Change('add', 'welcome-instance', LClick)])));
  Inc(GRevision);
  Check(LValue.Field('callbacks').Item(0).Field('index').AsInteger = 1, 'Instance appends after inherited registration');
  LReview := GSession.Call('nyx_callbacks', 'Scooty', Arguments('review', '',
    NyxArray([Change('remove', 'welcome-card', LClick, LOwner)])));
  Check(Pos('Inheriting instances are affected', LReview.Field('callbacks').Item(0).Field('warning').AsText) > 0,
    'Definition removal warning discloses reusable reach');

  LProject := GSession.PreviewPair(GRevision, 'home');
  LProject.Pending := True;
  LProject.Draft := LProject.Source + #10 + '// local draft';
  LProject.DraftBase := LProject.Source;
  GSession.Exchange(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('project', NyxData(EncodeNyxProject(LProject))),
    NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  Inc(GRevision);
  Refuses(Arguments('apply', 'draft-add', LAdds), 'Pending draft survives callback addition refusal');
  Refuses(Arguments('review', '', LRemoves), 'Pending draft survives removal review refusal');
  LProject.Pending := False;
  LProject.Draft := '';
  LProject.DraftBase := '';
  GSession.Exchange(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('project', NyxData(EncodeNyxProject(LProject))),
    NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  Inc(GRevision);

  GSession.Exchange(NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('readOnly'))]));
  Refuses(Arguments('apply', 'read-only', LAdds), 'Read-only operator control refuses callback changes');
  Check(GSession.Call('nyx_callbacks', 'Scooty', Arguments('review', '', LRemoves)).Field('reviewID').AsText <> '',
    'Read-only operator permits nonediting removal review');
  GSession.Exchange(NyxObject([NyxField('op', NyxData('configure')), NyxField('permission', NyxData('edit'))]));

  { Exercise the maximum supported batch and owner identity, then refuse one
    additional change. The transport response still fits its context budget. }
  LOwner := TNyxText(StringOfChar('x', 128));
  GSession.Call('nyx_transaction', 'Scooty', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)), NyxField('operationId', NyxData('long-owner')),
    NyxField('operations', NyxArray([NyxObject([NyxField('op', NyxData('create')),
      NyxField('kind', NyxData('button')), NyxField('id', NyxData(LOwner)), NyxField('parent', NyxData('home'))])]))]));
  Inc(GRevision);
  SetLength(LItems, 32);
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := NyxObject([NyxField('op', NyxData('policy')), NyxField('id', NyxData(LOwner)),
      NyxField('event', LClick), NyxField('policy', NyxData('ui-queue'))]);
  end;
  LValue := GSession.Call('nyx_callbacks', 'Scooty', Arguments('apply', 'maximum-batch', NyxArray(LItems)));
  Inc(GRevision);
  Check((LValue.Field('callbacks').Count = 32) and (Length(LValue.ToJSON) < 48 * 1024),
    'Maximum admitted callback batch returns bounded context');
  SetLength(LItems, 33);
  LItems[32] := LItems[0];
  Refuses(Arguments('apply', 'too-many-changes', NyxArray(LItems)), 'Oversized batch refuses before publishing');
end;

begin
  try
    GSession := TNyxAgentSession.Create;
    GRevision := GSession.Revision;
    Run;
    FreeAndNil(GSession);
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GCount) + ' semantic callback checks';
    document.body.setAttribute('data-nyx-agent-callbacks', 'passed');
    {$else}
    WriteLn('PASS ', GCount, ' semantic callback checks');
    {$endif}
  except
    on LException: Exception do
    begin
      FreeAndNil(GSession);
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-agent-callbacks', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      Halt(1);
      {$endif}
    end;
  end;
end.
