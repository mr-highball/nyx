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
program nyx_review_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.agents, nyx.studio.reviews,
  nyx.studio.projects
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;
  GActive: TNyxAgentSession;
  GReviews: TNyxReviewWorkspaces;
  GBaseline: TNyxDataValue;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Observe: TNyxDataValue;
begin
  Result := GActive.Exchange(NyxObject([NyxField('op', NyxData('observe'))]));
end;

{ Active activity is expected to change. Every authored/recovery/view/history
  field and its monotonic revision must remain exact through review operations. }
procedure Preserved;
var
  LCurrent: TNyxDataValue;
  LIndex: Integer;
const
  CKeys: array[0..7] of TNyxText = ('revision', 'selection', 'view', 'pages',
    'components', 'pendingDraft', 'canUndo', 'canRedo');
begin
  LCurrent := Observe;
  Check(LCurrent.Field('project').AsText = GBaseline.Field('project').AsText,
    'The entire active accepted/draft pair remains byte-identical');
  for LIndex := 0 to High(CKeys) do
  begin
    Check(LCurrent.Field('session').Field(CKeys[LIndex]).ToJSON =
      GBaseline.Field('session').Field(CKeys[LIndex]).ToJSON,
      'The active workspace retains ' + CKeys[LIndex]);
  end;
end;

function CreateArgs(const AOperation, ABase: TNyxText): TNyxDataValue;
const
  CLabel: TNyxText = 'A protected review 🌙';
begin
  Result := NyxObject([NyxField('mode', NyxData('create')),
    NyxField('expectedRevision', NyxData(GActive.Revision)),
    NyxField('operationId', NyxData(AOperation)), NyxField('label', NyxData(CLabel)),
    NyxField('base', NyxData(ABase))]);
end;

function Tool(const AName: TNyxText; const ARef: TNyxReviewRef;
  const AFields: array of TNyxDataField): TNyxDataValue;
begin
  Result := GReviews.Call(AName, 'transport-one', 'Same actor',
    NyxWithReview(NyxObject(AFields), ARef));
end;

procedure Refuse(const AName, AOwner: TNyxText; const AArguments: TNyxDataValue);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    GReviews.Call(AName, AOwner, 'Same actor', AArguments);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'The invalid or foreign request must refuse');
  Preserved;
end;

procedure Run;
var
  LBeforeDraft: TNyxDataValue;
  LPair: TNyxProjectPair;
  LValue: TNyxDataValue;
  LRef: TNyxReviewRef;
  LEmpty: TNyxReviewRef;
  LOther: TNyxReviewRef;
  LArguments: TNyxDataValue;
  LDisposed: TNyxDataValue;
  LUndo: TNyxDataValue;
  LRevision: Integer;
  LIndex: Integer;
  LSlots: array[0..5] of TNyxReviewRef;
  LFirstReceipt: TNyxDataValue;
  LFirstRequest: TNyxDataValue;
  LReviewBefore: TNyxDataValue;
const
  CDraft: TNyxText = #10 + '// Keep this unresolved draft 🌙';
  CLabel: TNyxText = 'A protected review 🌙';
begin
  GActive := TNyxAgentSession.Create;
  GReviews := TNyxReviewWorkspaces.Create(GActive);
  try
    { Establish ordinary user Undo/Redo before a pending draft. A review seed
      must retain accepted text without adopting that independent editor buffer. }
    GActive.Call('nyx_transaction', 'User', NyxObject([
      NyxField('expectedRevision', NyxData(GActive.Revision)),
      NyxField('operationId', NyxData('user-title')),
      NyxField('operations', NyxArray([NyxObject([
        NyxField('op', NyxData('title')), NyxField('value', NyxData('User work'))])]))]));
    GActive.Call('nyx_history', 'User', NyxObject([
      NyxField('expectedRevision', NyxData(GActive.Revision)),
      NyxField('operationId', NyxData('user-undo')), NyxField('direction', NyxData('undo'))]));
    LBeforeDraft := Observe;
    LPair := DecodeNyxProject(LBeforeDraft.Field('project').AsText);
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + CDraft;
    GActive.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(GActive.Revision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('welcome-title')), NyxField('view', NyxData('home'))]));
    GBaseline := Observe;
    Check(GBaseline.Field('session').Field('pendingDraft').AsBoolean,
      'The preservation fixture actually has a pending draft');
    LArguments := CreateArgs('create-accepted', 'accepted');
    LValue := GReviews.Manage('transport-one', 'Same actor', LArguments);
    LRef := NyxReview(LValue.Field('review').AsText);
    Check(LValue.Field('label').AsText = CLabel,
      'Supplementary Unicode survives the observer label');
    Check(not LValue.Field('session').Field('pendingDraft').AsBoolean and
      not LValue.Field('session').Field('canUndo').AsBoolean,
      'An accepted seed excludes the user draft and starts independent history');
    Check(GReviews.Manage('transport-one', 'Same actor', LArguments).ToJSON = LValue.ToJSON,
      'An exact create retry returns the original receipt');
    Preserved;
    LValue := Tool('nyx_node', LRef, [NyxField('id', NyxData('welcome-title')),
      NyxField('keys', NyxArray([NyxData('text')]))]);
    Check(LValue.Field('review').AsText = LRef.ID, 'Bounded queries return explicit review context');
    LValue := Tool('nyx_transaction', LRef, [
      NyxField('expectedRevision', NyxData(GReviews.Resolve('transport-one', LRef).Revision)),
      NyxField('operationId', NyxData('review-related')),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"title","value":"Independent review"},{"op":"create","kind":"label",' +
        '"id":"owned-label","parent":"home","properties":{"text":"An owned caption"}}]'))]);
    LRevision := LValue.Field('revision').AsInteger;
    Check(LValue.Field('canUndo').AsBoolean, 'Related review edits share ordinary paired Undo');
    LValue := Tool('nyx_history', LRef, [
      NyxField('expectedRevision', NyxData(LRevision)), NyxField('operationId', NyxData('review-undo')),
      NyxField('direction', NyxData('undo'))]);
    Check(LValue.Field('title').AsText = GBaseline.Field('session').Field('title').AsText,
      'One review Undo restores the seed title');
    LUndo := LValue;
    LValue := Tool('nyx_history', LRef, [
      NyxField('expectedRevision', LUndo.Field('revision')),
      NyxField('operationId', NyxData('review-redo')), NyxField('direction', NyxData('redo'))]);
    Check(LValue.Field('title').AsText = 'Independent review', 'Review Redo restores only review content');
    LReviewBefore := GReviews.Resolve('transport-one', LRef).Exchange(
      NyxObject([NyxField('op', NyxData('observe'))]));
    Refuse('nyx_transaction', 'transport-one', NyxWithReview(NyxObject([
      NyxField('expectedRevision', LValue.Field('revision')),
      NyxField('operationId', NyxData('nested-routing-refusal')),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"title","value":"Must not publish"},{"op":"update","id":"welcome-title",' +
        '"review":"home","properties":{"text":"Must not publish"}}]'))]), LRef));
    Check(GReviews.Resolve('transport-one', LRef).Exchange(
      NyxObject([NyxField('op', NyxData('observe'))])).Field('project').AsText =
      LReviewBefore.Field('project').AsText, 'Failed grouped routing retains the entire review pair');
    Check(Tool('nyx_session', LRef, []).Field('revision').AsInteger =
      LReviewBefore.Field('session').Field('revision').AsInteger,
      'Failed grouped routing retains the review revision/history');
    Refuse('nyx_reviews', 'transport-one', CreateArgs('create-accepted', 'empty'));
    Preserved;
    Refuse('nyx_session', 'transport-two', NyxObject([NyxField('review', NyxData(LRef.ID))]));
    Refuse('nyx_reviews', 'transport-two', NyxObject([
      NyxField('mode', NyxData('inspect')), NyxField('review', NyxData(LRef.ID))]));
    Refuse('nyx_session', 'transport-one', NyxObject([NyxField('review', NyxData(''))]));
    Refuse('nyx_session', 'transport-one', NyxObject([NyxField('review', NyxData(True))]));
    Refuse('nyx_transaction', 'transport-one', NyxObject([
      NyxField('review', NyxData(LRef.ID)), NyxField('expectedRevision', NyxData(1)),
      NyxField('operationId', NyxData('stale-review')), NyxField('operations', NyxArray([]))]));
    LValue := GReviews.Manage('transport-one', 'Same actor', CreateArgs('create-empty', 'empty'));
    LEmpty := NyxReview(LValue.Field('review').AsText);
    Check((LValue.Field('session').Field('pages').AsInteger = 0) and
      (LValue.Field('session').Field('components').AsInteger = 0), 'An empty review does not copy user roots');
    Check(Tool('nyx_outline', LEmpty, []).Field('total').AsInteger = 0,
      'Explicit empty context never falls back to active roots');
    LOther := GReviews.CreateReview('transport-two', 'Same actor', 'Other owner', nrbEmpty, GActive.Revision);
    Check(GReviews.Manage('transport-one', 'Same actor', NyxObject([
      NyxField('mode', NyxData('list'))])).Field('total').AsInteger = 2,
      'Same display names do not share review ownership or lists');
    Check(GReviews.Observe.Count = 3, 'The trusted observer sees all bounded review summaries');
    GActive.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('readOnly'))]));
    Check(Tool('nyx_session', LRef, []).Field('permission').AsText = 'readOnly',
      'Operator reductions apply immediately to review reads');
    Refuse('nyx_reviews', 'transport-one', CreateArgs('readonly-create', 'empty'));
    Refuse('nyx_transaction', 'transport-one', NyxObject([
      NyxField('review', NyxData(LRef.ID)),
      NyxField('expectedRevision', NyxData(GReviews.Resolve('transport-one', LRef).Revision)),
      NyxField('operationId', NyxData('readonly-edit')), NyxField('operations', NyxArray([]))]));
    GActive.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('disabled'))]));
    Refuse('nyx_session', 'transport-one', NyxObject([NyxField('review', NyxData(LRef.ID))]));
    GReviews.ReleaseOwner('transport-two');
    Check(GReviews.Find(LOther) = nil, 'Transport teardown releases only that owner even while disabled');
    GActive.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('edit'))]));
    for LIndex := 0 to High(LSlots) do
    begin
      LSlots[LIndex] := GReviews.CreateReview('transport-one', 'Same actor',
        'Bounded review', nrbEmpty, GActive.Revision);
    end;
    Check(GReviews.Observe.Count = 8, 'The actual live workspace budget is eight');
    Refuse('nyx_reviews', 'transport-one', CreateArgs('ninth', 'empty'));
    Refuse('nyx_reviews', 'transport-one', NyxObject([
      NyxField('mode', NyxData('discard')), NyxField('review', NyxData(LRef.ID)),
      NyxField('expectedRevision', NyxData(1)), NyxField('operationId', NyxData('stale-discard'))]));
    LArguments := NyxObject([NyxField('mode', NyxData('discard')),
      NyxField('review', NyxData(LRef.ID)),
      NyxField('expectedRevision', NyxData(GReviews.Resolve('transport-one', LRef).Revision)),
      NyxField('operationId', NyxData('discard-review'))]);
    LDisposed := GReviews.Manage('transport-one', 'Same actor', LArguments);
    Check(LDisposed.Field('disposed').AsBoolean and (GReviews.Find(LRef) = nil),
      'Disposal retires the exact independent session');
    Check(GReviews.Manage('transport-one', 'Same actor', LArguments).ToJSON = LDisposed.ToJSON,
      'Exact disposal retries retain their receipt after retirement');
    Refuse('nyx_session', 'transport-one', NyxObject([NyxField('review', NyxData(LRef.ID))]));
    LOther := GReviews.CreateReview('transport-one', 'Same actor', 'Later review', nrbEmpty, GActive.Revision);
    Check(LOther.ID <> LRef.ID, 'Retired identities are never reused');
    Preserved;
    GReviews.ReleaseOwner('transport-one');
    Check(GReviews.Observe.Count = 0, 'Owner teardown releases every review without an active-work replacement');
    { A long-lived transport must not forget the oldest receipt and accidentally
      create again at the unchanged user revision. Reserved slots always allow
      all owned reviews to be discarded before reconnecting. }
    for LIndex := 0 to 31 do
    begin
      LArguments := CreateArgs('bounded-create-' + IntToStr(LIndex), 'empty');
      LValue := GReviews.Manage('receipt-owner', 'Same actor', LArguments);

      if LIndex = 0 then
      begin
        LFirstRequest := LArguments;
        LFirstReceipt := LValue;
      end;
      LRef := NyxReview(LValue.Field('review').AsText);
      GReviews.Manage('receipt-owner', 'Same actor', NyxObject([
        NyxField('mode', NyxData('discard')), NyxField('review', NyxData(LRef.ID)),
        NyxField('expectedRevision', LValue.Field('session').Field('revision')),
        NyxField('operationId', NyxData('bounded-discard-' + IntToStr(LIndex)))]));
    end;
    Check(GReviews.Manage('receipt-owner', 'Same actor', LFirstRequest).ToJSON =
      LFirstReceipt.ToJSON, 'The oldest receipt survives the complete live transport budget');
    Check(GReviews.Observe.Count = 0, 'Retrying a retired creation receipt cannot resurrect a review');
    Refuse('nyx_reviews', 'receipt-owner', CreateArgs('over-receipt-budget', 'empty'));
    GReviews.ReleaseOwner('receipt-owner');
    Check(GReviews.Observe.Count = 0, 'Receipt cleanup does not acquire new document ownership');
    Preserved;
  finally
    GReviews.Free;
    GReviews := nil;
    GActive.Free;
    GActive := nil;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' review checks';
    document.body.setAttribute('data-nyx-review-tests', 'passed');
    document.body.setAttribute('data-nyx-review-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' review checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := LException.Message;
      document.body.setAttribute('data-nyx-review-tests', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
