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

unit nyx.test.agent.reusables;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.projects;

{ Run the shared semantic workflow without a listener or operator-project reset.
  Return the exact accepted companion for unchanged target compilation. }
function RunNyxReusableJourney(out APair: TNyxProjectPair): Integer;

{ Extend the ordinary semantic companion with an executed-source extraction.
  Planning does not confer execution authority: target consumers compile and
  execute these exact returned bytes against the expected design separately. }
function RunNyxReusableExtraction(out APair: TNyxProjectPair): Integer;

implementation

uses
  SysUtils, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.composition, nyx.state, nyx.callbacks, nyx.data, nyx.source,
  nyx.studio.session, nyx.studio.agents, nyx.studio.edits,
  nyx.studio.workspaces, nyx.studio.reviews;

{ Fixture source insertion preserves portable text units directly. ANSI RTL
  replacement must not transcode supplementary characters or embedded NUL. }
function ReplaceOnce(const AText, AFrom, ATo: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(AFrom, AText);

  if LPosition = 0 then
  begin
    raise Exception.Create('The exact reusable fixture source anchor is missing');
  end;
  Result := Copy(AText, 1, LPosition - 1) + ATo +
    Copy(AText, LPosition + Length(AFrom), MaxInt);
end;

function Seed: TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LCard: INyxColumn;
  LActions: INyxRow;
  LMemo: INyxMemo;
  LLabel: INyxLabel;
  LButton: INyxButton;
  LBadge: INyxBadge;
  LSource: TNyxText;
  LAuthor: TNyxStudioSession;
  LLine: Integer;
  LHandler: TNyxHandlerRef;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Reusable component workshop';
    LDocument.State.SetValue(NyxTextState('reply'), 'Ready to compose.')
      .SetValue(NyxTextState('qualification'), TNyxText('A🌙') + #0 + TNyxText('éZ'));
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LCard := NewNyxColumn('original-card');
    LCard.Configure.Surface(True).Padding(20).Gap(12).Done;
    LPage.Add(LCard);
    LLabel := NewNyxLabel('original-heading');
    LLabel.Configure.PartName(NyxPart('heading')).Text('Write something wonderful').Done;
    LCard.Add(LLabel);
    LMemo := NewNyxMemo('original-editor');
    LMemo.Configure.PartName(NyxPart('editor')).Text('Your reply').Done;
    LMemo.Binds.Value(NyxTextState('reply')).Done;
    LCard.Add(LMemo);
    LActions := NewNyxRow('original-actions');
    LActions.Configure.PartName(NyxPart('actions')).Done;
    LCard.Add(LActions);
    LButton := NewNyxButton('original-submit');
    LButton.Configure.PartName(NyxPart('submit')).Text('Post reply').Done;
    LActions.Add(LButton);
    LBadge := NewNyxBadge('original-badge');
    LBadge.Configure.PartName(NyxPart('badge')).Text('Draft').Done;
    LActions.Add(LBadge);
    LSource := TNyxCodegen.Generate(LDocument);
    LSource := ReplaceOnce(LSource, 'function BuildNyxDocument: TNyxDocument;',
      'function ReusableWorkshopInvocations: Integer;' + #10 +
      'function BuildNyxDocument: TNyxDocument;');
    LSource := ReplaceOnce(LSource, 'implementation' + #10,
      'implementation' + #10 + #10 +
      'var' + #10 + '  LReusableWorkshopInvocations: Integer;' + #10 + #10 +
      'function ReusableWorkshopInvocations: Integer;' + #10 + 'begin' + #10 +
      '  Result := LReusableWorkshopInvocations;' + #10 + 'end;' + #10 + #10 +
      '{ Application helper retained while reusing designs. }' + #10 +
      'function ReusableWorkshopNote: TNyxText;' + #10 + 'begin' + #10 +
      '  Result := ''Keep crafting.'';' + #10 + 'end;' + #10);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LDocument.Free;
  end;
  LAuthor := TNyxStudioSession.Create(Result);
  try
    LAuthor.Select('original-submit');
    LHandler := LAuthor.AddCallback(ntClick, LLine);
    LAuthor.SetSourceDraft(ReplaceOnce(LAuthor.Source,
      '// TODO: implement ' + LHandler.Name + '.', 'Inc(LReusableWorkshopInvocations);'));
    LAuthor.ApplySourceDraft;
    Result := LAuthor.ProjectSnapshot;
  finally
    LAuthor.Free;
  end;
end;

function RunNyxReusableJourney(out APair: TNyxProjectPair): Integer;
var
  LAgent: TNyxAgentSession;
  LRevision, LChecks: Integer;
  LBefore, LAccepted, LHandler: TNyxText;
  LArgs, LReceipt, LValue: TNyxDataValue;
  LDocument: TNyxDocument;
  LRuntime: TNyxNode;
  LTyped: TNyxStudioSession;
  LPending: TNyxProjectPair;
  LEvents: TNyxAuthoredEventInfos;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Semantic reusables: ' + AReason);
    end;
    Inc(LChecks);
  end;

  function Arguments(const AID, AOperations: TNyxText): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)),
      NyxField('operations', TNyxDataValue.ParseJSON(AOperations))]);
  end;

  procedure Apply(const AID, AOperations: TNyxText);
  begin
    LAgent.Call('nyx_transaction', 'Scooty', Arguments(AID, AOperations));
    Inc(LRevision);
  end;

  procedure History(const AID, ADirection: TNyxText);
  begin
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData(AID)),
      NyxField('direction', NyxData(ADirection))]));
    Inc(LRevision);
  end;

  procedure Refuses(const AID, AOperations: TNyxText);
  var
    LRejected: Boolean;
    LPair: TNyxProjectPair;
    LState, LAfter: TNyxDataValue;
  begin
    LPair := LAgent.PreviewPair(LRevision, 'home');
    LState := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
    LRejected := False;
    try
      LAgent.Call('nyx_transaction', 'Scooty', Arguments(AID, AOperations));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    LAfter := LAgent.Call('nyx_session', 'Scooty', NyxObject([]));
    Check(LRejected and (LAgent.Revision = LRevision) and
      (LAfter.Field('canUndo').AsBoolean = LState.Field('canUndo').AsBoolean) and
      (LAfter.Field('canRedo').AsBoolean = LState.Field('canRedo').AsBoolean) and
      (EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = EncodeNyxProject(LPair)),
      'refusal preserves the complete pair/revision/history: ' + AID);
  end;

  function Parts(const AID: TNyxText; AOffset, ALimit: Integer): TNyxDataValue;
  begin
    Result := LAgent.Call('nyx_node', 'Scooty', NyxObject([
      NyxField('id', NyxData(AID)), NyxField('parts', NyxData(True)),
      NyxField('partOffset', NyxData(AOffset)), NyxField('partLimit', NyxData(ALimit)),
      NyxField('events', NyxData(True)), NyxField('eventLimit', NyxData(1)),
      NyxField('limit', NyxData(1))]));
  end;

  { Accepted forks share meaning, never mutable ownership, history or retry
    authority. This portable case deliberately uses genuine literal admission;
    the production compiler consumer separately qualifies executed checkpoints. }
  procedure AcceptedSeeds;
  var
    LSeed: TNyxAgentSession;
    LPair: TNyxText;
    LSnapshot: TNyxDataValue;
    LRejected: Boolean;
  begin
    LPair := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
    LSeed := LAgent.ForkAccepted(LRevision);
    try
      LSnapshot := LSeed.Call('nyx_session', 'Scooty', NyxObject([]));
      Check((LSeed.Revision = 1) and not LSnapshot.Field('canUndo').AsBoolean and
        not LSnapshot.Field('canRedo').AsBoolean and
        not LSnapshot.Field('pendingDraft').AsBoolean,
        'accepted forks start independent clean history and navigation');
      Check(EncodeNyxProject(LSeed.PreviewPair(1, 'home')) = LPair,
        'accepted forks retain the exact complete pair');
      LSeed.Call('nyx_transaction', 'Scooty', NyxObject([
        NyxField('expectedRevision', NyxData(1)),
        NyxField('operationId', NyxData('derive-and-customize')),
        NyxField('operations', TNyxDataValue.ParseJSON(
          '[{"op":"update","id":"original-heading","properties":{"text":"An independent caption"}}]'))]));
      Check((LSeed.Revision = 2) and
        (EncodeNyxProject(LSeed.PreviewPair(2, 'home')) <> LPair),
        'the parent retry identity carries no authority into a new owner');
      Check((LAgent.Revision = LRevision) and
        (EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = LPair),
        'forked descendant edits leave the original source and tree exact');
      LSeed.Call('nyx_history', 'Scooty', NyxObject([
        NyxField('expectedRevision', NyxData(2)),
        NyxField('operationId', NyxData('fork-undo')),
        NyxField('direction', NyxData('undo'))]));
      Check(EncodeNyxProject(LSeed.PreviewPair(LSeed.Revision, 'home')) = LPair,
        'the fork owns its own paired Undo command');
    finally
      LSeed.Free;
    end;
    LRejected := False;
    try
      LSeed := LAgent.ForkAccepted(LRevision - 1);
      LSeed.Free;
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'stale accepted creation refuses before publishing an owner');
    LAgent.InheritPermission(apReadOnly);
    try
      LRejected := False;
      try
        LSeed := LAgent.ForkAccepted(LRevision);
        LSeed.Free;
      except
        on ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'read-only agents cannot create accepted owners');
    finally
      LAgent.InheritPermission(apEdit);
    end;
  end;

  procedure Contexts;
  var
    LWorkspaces: TNyxStudioWorkspaces;
    LReviews: TNyxReviewWorkspaces;
    LWorkspace: TNyxWorkspaceRef;
    LReview: TNyxReviewRef;
    LRequest, LResult: TNyxDataValue;
    LPrimary: TNyxText;
    LRejected: Boolean;
  begin
    LPrimary := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
    LWorkspaces := TNyxStudioWorkspaces.Create(LAgent, 'reusable-context-fixture');
    LReviews := TNyxReviewWorkspaces.Create(LAgent);
    try
      LWorkspace := LWorkspaces.OpenProject('Independent reusable workshop', APair);
      LRequest := NyxWithWorkspace(NyxObject([
        NyxField('expectedRevision', NyxData(LWorkspaces.Resolve(LWorkspace).Revision)),
        NyxField('operationId', NyxData('context-derive')),
        NyxField('operations', TNyxDataValue.ParseJSON(
          '[{"op":"derive","source":"original-card","id":"workspace-card","identities":{' +
          '"original-heading":"workspace-heading","original-editor":"workspace-editor",' +
          '"original-actions":"workspace-actions","original-submit":"workspace-submit","original-badge":"workspace-badge"}},' +
          '{"op":"delete","id":"original-card"},' +
          '{"op":"instance","component":"workspace-card","id":"original-card","parent":"home","index":0}]'))]), LWorkspace);
      LResult := LWorkspaces.Call('nyx_transaction', 'context-owner', 'Scooty', LRequest);
      Check(LResult.Field('workspace').AsText = LWorkspace.ID,
        'grouped reusable receipt retains explicit workspace');
      LResult := LWorkspaces.Call('nyx_node', 'context-owner', 'Scooty',
        NyxWithWorkspace(NyxObject([NyxField('id', NyxData('original-card')),
          NyxField('parts', NyxData(True)), NyxField('partLimit', NyxData(1))]), LWorkspace));
      Check((LResult.Field('parts').Field('items').Item(0).Field('source').AsText = 'workspace-card') and
        (LResult.Field('workspace').AsText = LWorkspace.ID),
        'bounded context targets subtree promotion with the retained original control identity');
      LWorkspaces.ReleaseOwner('context-owner');
      Check(LWorkspaces.Resolve(LWorkspace).Revision > 1,
        'ordinary project derivation survives agent disconnection');
      LReview := LReviews.CreateReview('review-owner', 'Scooty', 'Owned reusable review',
        nrbAccepted, LRevision);
      LRequest := NyxWithReview(NyxObject([
        NyxField('expectedRevision', NyxData(LReviews.Resolve('review-owner', LReview).Revision)),
        NyxField('operationId', NyxData('context-derive')),
        NyxField('operations', TNyxDataValue.ParseJSON(
          '[{"op":"derive","source":"reply-editor","id":"review-editor","identities":{}},' +
          '{"op":"instance","component":"review-editor","id":"review-copy","parent":"home"}]'))]), LReview);
      LResult := LReviews.Call('nyx_transaction', 'review-owner', 'Scooty', LRequest);
      Check(LResult.Field('review').AsText = LReview.ID,
        'grouped reusable receipt retains actor-owned review');
      LRequest := NyxWithReview(NyxObject([NyxField('id', NyxData('review-copy')),
        NyxField('parts', NyxData(True)), NyxField('partLimit', NyxData(1))]), LReview);
      LRejected := False;
      try
        LReviews.Call('nyx_node', 'foreign-owner', 'Scooty', LRequest);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'foreign review owner refuses bounded part access');
      LReviews.ReleaseOwner('review-owner');
      LRejected := False;
      try
        LReviews.Call('nyx_node', 'review-owner', 'Scooty', LRequest);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'retired reusable review refuses without project fallback');
      Check((LAgent.Revision = LRevision) and
        (EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = LPrimary),
        'concurrent reusable work retains the primary accepted pair/history');
    finally
      LReviews.Free;
      LWorkspaces.Free;
    end;
  end;

begin
  LChecks := 0;
  LAgent := TNyxAgentSession.Create(Seed);
  try
    LRevision := LAgent.Revision;
    LBefore := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
    LDocument := TNyxCodec.Decode(LAgent.PreviewPair(LRevision, 'home').Design);
    try
      LEvents := NyxAuthoredEvents(LDocument.Find('original-submit'));
      LHandler := LEvents[0].Callbacks[0].Handler.Name;
    finally
      LDocument.Free;
    end;
    LArgs := Arguments('derive-and-customize',
      '[{"op":"derive","source":"original-card","id":"reply-card","identities":{' +
      '"original-heading":"reply-heading","original-editor":"reply-editor",' +
      '"original-actions":"reply-actions","original-submit":"reply-submit","original-badge":"reply-badge"}},' +
      '{"op":"instance","id":"first-card","component":"reply-card","parent":"home"},' +
      '{"op":"instance","id":"second-card","component":"reply-card","parent":"home"},' +
      '{"op":"override","instance":"first-card","id":"first-title","path":"heading","mode":"replace"},' +
      '{"op":"create","kind":"label","id":"first-heading","parent":"first-title","properties":{"text":"A featured reply"}},' +
      '{"op":"override","instance":"first-card","id":"first-actions","path":"actions","mode":"append"},' +
      '{"op":"create","kind":"button","id":"first-save","parent":"first-actions","properties":{"text":"Save draft"}},' +
      '{"op":"override","instance":"first-card","id":"first-editor","path":"editor","mode":"properties"},' +
      '{"op":"update","id":"first-editor","properties":{"placeholder":"Your thoughtful reply","height":120}}]');
    LReceipt := LAgent.Call('nyx_transaction', 'Scooty', LArgs);
    Inc(LRevision);
    Check(LAgent.Revision = LRevision, 'the related derivation/instance/override group publishes once');
    Check(LAgent.Call('nyx_transaction', 'Scooty', LArgs).ToJSON = LReceipt.ToJSON,
      'an exact actor-bound delivery retry returns the original receipt');
    LAccepted := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
    History('undo-reuse', 'undo');
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = LBefore,
      'one paired Undo removes the whole reusable workflow');
    History('redo-reuse', 'redo');
    Check(EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home')) = LAccepted,
      'one paired Redo restores exact generated source and design');
    LValue := Parts('first-card', 1, 2);
    Check((LValue.Field('parts').Field('items').Count = 2) and
      (LValue.Field('parts').Field('total').AsInteger = 6) and
      (LValue.Field('parts').Field('offset').AsInteger = 1),
      'named parts page independently of property/event windows');
    Check((LValue.Field('parts').Field('items').Item(0).Field('path').AsText = 'heading') and
      (LValue.Field('parts').Field('items').Item(0).Field('source').AsText = 'first-heading') and
      (LValue.Field('parts').Field('items').Item(0).Field('overrideID').AsText = 'first-title') and
      (LValue.Field('events').Count = 1),
      'effective replacement identity/local override and events coexist');
    LValue := Parts('second-card', 4, 2);
    Check((LValue.Field('parts').Field('items').Item(0).Field('path').AsText = 'actions/submit') and
      (LValue.Field('parts').Field('items').Item(0).Field('source').AsText = 'reply-submit'),
      'nested paths retain exact source identities without runtime key confusion');
    Refuses('incomplete-map', '[{"op":"derive","source":"reply-card","id":"broken-card","identities":{}}]');
    Refuses('foreign-map', '[{"op":"derive","source":"reply-editor","id":"broken-card","identities":{"reply-heading":"unused"}}]');
    Refuses('root-map', '[{"op":"derive","source":"reply-editor","id":"broken-card","identities":{"reply-editor":"new-root"}}]');
    Refuses('occupied-definition', '[{"op":"derive","source":"reply-editor","id":"reply-card","identities":{}}]');
    Refuses('occupied-descendant', '[{"op":"derive","source":"reply-actions","id":"broken-card","identities":{"reply-submit":"home","reply-badge":"new-badge"}}]');
    Refuses('duplicate-destination', '[{"op":"derive","source":"reply-actions","id":"broken-card","identities":{"reply-submit":"same","reply-badge":"same"}}]');
    Refuses('wrong-map-primitive', '[{"op":"derive","source":"reply-editor","id":"broken-card","identities":{"other":7}}]');
    Refuses('missing-definition', '[{"op":"instance","component":"missing","id":"bad-instance","parent":"home"}]');
    Refuses('occupied-instance', '[{"op":"instance","component":"reply-card","id":"first-card","parent":"home"}]');
    Refuses('foreign-owner', '[{"op":"override","instance":"reply-editor","id":"bad-rule","path":".","mode":"properties"}]');
    Refuses('missing-path', '[{"op":"override","instance":"second-card","id":"bad-rule","path":"absent","mode":"properties"}]');
    Refuses('ambiguous-path', '[{"op":"create","kind":"badge","id":"duplicate-badge","parent":"reply-actions","properties":{"part":"badge"}},' +
      '{"op":"override","instance":"second-card","id":"bad-rule","path":"actions/badge","mode":"properties"}]');
    Refuses('retargeted-rule', '[{"op":"override","instance":"first-card","id":"different-rule","path":"heading","mode":"properties"}]');
    Refuses('missing-replacement', '[{"op":"override","instance":"second-card","id":"bad-rule","path":"heading","mode":"replace"}]');
    Refuses('empty-append', '[{"op":"override","instance":"second-card","id":"bad-rule","path":"actions","mode":"append"}]');
    Refuses('invalid-mode', '[{"op":"override","instance":"second-card","id":"bad-rule","path":"heading","mode":"guess"}]');
    Refuses('wrong-inherit-id', '[{"op":"inherit","instance":"first-card","id":"different-rule","path":"heading"}]');
    Refuses('missing-inheritance', '[{"op":"inherit","instance":"second-card","id":"bad-rule","path":"heading"}]');
    Refuses('root-removal', '[{"op":"override","instance":"second-card","id":"bad-rule","path":".","mode":"remove"}]');
    Refuses('atomic-late-refusal', '[{"op":"instance","component":"reply-card","id":"temporary-card","parent":"home"},' +
      '{"op":"override","instance":"temporary-card","id":"bad-rule","path":"absent","mode":"remove"}]');
    Refuses('derive-and-customize', '[{"op":"title","value":"different delivery retry"}]');
    Apply('prepend-and-remove',
      '[{"op":"override","instance":"second-card","id":"second-actions","path":"actions","mode":"prepend"},' +
      '{"op":"create","kind":"button","id":"second-cancel","parent":"second-actions","properties":{"text":"Cancel"}},' +
      '{"op":"override","instance":"second-card","id":"second-badge","path":"actions/badge","mode":"remove"}]');
    LValue := Parts('second-card', 4, 2);
    Check(LValue.Field('parts').Field('total').AsInteger = 5,
      'removed effective part is absent without changing inherited definition');
    Apply('restore-inheritance',
      '[{"op":"inherit","instance":"first-card","id":"first-title","path":"heading"},' +
      '{"op":"inherit","instance":"second-card","id":"second-badge","path":"actions/badge"}]');
    Check(Parts('second-card', 0, 1).Field('parts').Field('total').AsInteger = 6,
      'restoring exact inheritance returns the removed named part');
    Apply('reapply-replacement',
      '[{"op":"override","instance":"first-card","id":"featured-title","path":"heading","mode":"replace"},' +
      '{"op":"create","kind":"label","id":"featured-heading","parent":"featured-title","properties":{"text":"A featured reply"}}]');
    APair := LAgent.PreviewPair(LRevision, 'home');
    LDocument := TNyxCodec.Decode(APair.Design);
    try
      LRuntime := RealizeNyxView(LDocument, LDocument.Find('home'));
      try
        Check((LRuntime.Find('first-card/featured-heading').Prop('text') = 'A featured reply') and
          (LRuntime.Find('second-card/reply-heading').Prop('text') = 'Write something wonderful') and
          (LDocument.FindComponent('reply-card').Part(NyxPart('heading')).Prop('text') = 'Write something wonderful'),
          'replacement retains sibling and definition independence');
        Check((LRuntime.Find('first-card/reply-actions').Children[2].SourceID = 'first-save') and
          (LRuntime.Find('second-card/reply-actions').Children[0].SourceID = 'second-cancel'),
          'append/prepend retain their exact physical content order');
      finally
        LRuntime.Free;
      end;
      LEvents := NyxAuthoredEvents(LDocument.Find('reply-submit'));
      Check((LDocument.Find('original-submit').ID = 'original-submit') and
        (LEvents[0].Callbacks[0].Handler.Name = LHandler),
        'derivation copies callback contracts while retaining the shared source implementation');
      Check(LDocument.State.Value(NyxTextState('reply').Name).TextValue = 'Ready to compose.',
        'document defaults remain owned and unchanged');
      Check(LDocument.State.Value('qualification').TextValue =
        TNyxText('A🌙') + #0 + TNyxText('éZ'),
        'supplementary/NUL defaults survive source generation and paired history');
      Check(Pos('function ReusableWorkshopNote', APair.Source) > 0,
        'handwritten helpers survive every paired edit');
    finally
      LDocument.Free;
    end;
    LAgent.InheritPermission(apReadOnly);
    Refuses('readonly', '[{"op":"instance","component":"reply-card","id":"readonly-card","parent":"home"}]');
    Check(Parts('first-card', 0, 1).Field('parts').Field('items').Count = 1,
      'read-only permission still allows bounded named-part context');
    LAgent.InheritPermission(apEdit);
    { Test pending-source refusal on a separate owned session; do not replace
      either a real user project or this journey's admitted history. }
    LPending := APair;
    LPending.Pending := True;
    LPending.DraftBase := APair.Source;
    LPending.Draft := APair.Source + #10 + '{ Unaccepted handwritten work }';
    LTyped := TNyxStudioSession.Create(LPending);
    try
      LBefore := EncodeNyxProject(LTyped.ProjectSnapshot);
      try
        LTyped.ApplyPatch(NyxReusablePatch([NyxInstantiateComponent(NyxComponent('reply-card'),
          NyxControl('pending-card'), NyxControl('home'))]));
        Check(False, 'pending draft must refuse');
      except
        on ENyxModel do
        begin
          Check((EncodeNyxProject(LTyped.ProjectSnapshot) = LBefore) and
            not LTyped.CanUndo and not LTyped.CanRedo,
            'pending-source refusal retains draft/baseline/history');
        end;
      end;
    finally
      LTyped.Free;
    end;
    LTyped := TNyxStudioSession.Create(APair);
    try
      LTyped.ApplyPatch(NyxReusablePatch([NyxDeriveComponent(NyxControl('reply-editor'),
        NyxComponent('standalone-editor'), []),
        NyxInstantiateComponent(NyxComponent('standalone-editor'),
          NyxControl('third-editor'), NyxControl('home'))]));
      Check(LTyped.Document.FindComponent('standalone-editor').Kind = 'memo',
        'typed Pascal commands derive a leaf without JSON behavior strings');
      LTyped.Undo;
      Check(EncodeNyxProject(LTyped.ProjectSnapshot) = EncodeNyxProject(APair),
        'typed derivation/instantiation shares ordinary paired Undo');
      LTyped.ApplyPatch(NyxReusablePatch([NyxDeriveComponent(NyxControl('first-card'),
        NyxComponent('featured-card'), [
        NyxIdentity(NyxControl('first-actions'), NyxControl('featured-actions')),
        NyxIdentity(NyxControl('first-save'), NyxControl('featured-save')),
        NyxIdentity(NyxControl('first-editor'), NyxControl('featured-editor')),
        NyxIdentity(NyxControl('featured-title'), NyxControl('featured-rule')),
        NyxIdentity(NyxControl('featured-heading'), NyxControl('featured-caption'))]),
        NyxInstantiateComponent(NyxComponent('featured-card'),
          NyxControl('third-card'), NyxControl('home'))]));
      LRuntime := RealizeNyxView(LTyped.Document, LTyped.Document.Find('home'));
      try
        Check(LRuntime.Find('third-card/featured-card/featured-caption').Prop('text') =
          'A featured reply', 'deriving an instance retains nested reference/override meaning');
      finally
        LRuntime.Free;
      end;
      LTyped.Undo;
      Check(EncodeNyxProject(LTyped.ProjectSnapshot) = EncodeNyxProject(APair),
        'nested reusable derivation retains independent paired history');
      LTyped.Select('original-card');
      LBefore := EncodeNyxProject(LTyped.ProjectSnapshot);
      LTyped.CreateComponent;
      Check(LTyped.Document.ComponentCount = 2,
        'ordinary inspector command uses shared independent derivation');
      LTyped.Undo;
      Check(EncodeNyxProject(LTyped.ProjectSnapshot) = LBefore,
        'ordinary inspector derivation has one paired Undo step');
    finally
      LTyped.Free;
    end;
    AcceptedSeeds;
    Contexts;
    Result := LChecks;
  finally
    LAgent.Free;
  end;
end;

function RunNyxReusableExtraction(out APair: TNyxProjectPair): Integer;
var
  LBase: TNyxProjectPair;
  LAgent: TNyxAgentSession;
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LRetained: INyxControl;
  LSource: TNyxText;
  LBeforeDesign: TNyxText;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Reusable extraction: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := RunNyxReusableJourney(LBase);
  LBefore := nil;
  LAfter := nil;
  LAgent := TNyxAgentSession.Create(LBase);
  try
    LBefore := TNyxCodec.Decode(LBase.Design);
    LBeforeDesign := TNyxCodec.Encode(LBefore);
    LRetained := RetainNyxControl(LBefore.Find('original-editor'));
    { Make the retained builder genuinely computed, without manufacturing its
      projection: the exact unchanged companion is executed by both targets. }
    LSource := ReplaceOnce(LBase.Source, '.Padding(20)', '.Padding(5 * 4)');
    LAgent.Call('nyx_transaction', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('extract-original-card')),
      NyxField('operations', TNyxDataValue.ParseJSON(
        '[{"op":"derive","source":"original-card","id":"extracted-card","identities":{' +
        '"original-heading":"extracted-heading","original-editor":"extracted-editor",' +
        '"original-actions":"extracted-actions","original-submit":"extracted-submit",' +
        '"original-badge":"extracted-badge"}},' +
        '{"op":"delete","id":"original-card"},' +
        '{"op":"instance","component":"extracted-card","id":"original-card","parent":"home","index":0}]'))]));
    APair := LAgent.PreviewPair(LAgent.Revision, 'home');
    LAfter := TNyxCodec.Decode(APair.Design);
    APair.Source := CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
    Check((LAfter.Find('original-card').Kind = NyxKindName(nkComponent)) and
      (LAfter.Find('original-card').ID = LBefore.Find('original-card').ID),
      'a compound extraction retains its public identity with a specialized component');
    Check((TNyxCodec.Encode(LBefore) = LBeforeDesign) and
      (LRetained.Node = LBefore.Find('original-editor')),
      'planning preserves the old tree and its retained managed implementation');
    Check((Pos('.Padding(5 * 4)', APair.Source) > 0) and
      (Pos('function ReusableWorkshopNote', APair.Source) > 0),
      'computed Pascal and unrelated helpers survive extraction verbatim');
    Check(Pos('LOriginalCardComponent: INyxComponent;', APair.Source) > 0,
      'the continuation uses the new specialized managed interface');
    { The opposite replacement is deliberately unsafe: this old descendant
      survives outside the replaced host. Admission must refuse the complete
      source proposal before freeing or snapshotting that implementation. }
    LAfter.MoveControl(NyxControl('original-card'), NyxControl('home'), 0);
    LAfter.AddPage(NewNyxPage('retained-host'));
    LAfter.RemoveControl(NyxControl('original-card'));
    LAfter.Find('home').Add(LBefore.Find('original-card').Clone);
    LAfter.MoveControl(NyxControl('original-editor'), NyxControl('retained-host'), 0);
    LAfter.RemoveControl(NyxControl('original-card'));
    LAfter.Find('home').Add(NewNyxComponent('original-card'));
    LAfter.Find('original-card').Configure.Component(NyxComponent('extracted-card'));
    LRejected := False;
    try
      CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
    except
      on LError: ENyxModel do
      begin
        LRejected := Pos('retains a descendant', LError.Message) > 0;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LBefore) = LBeforeDesign) and
      (LRetained.Node = LBefore.Find('original-editor')),
      'unsafe replacement refuses without destroying retained descendants');
  finally
    LRetained := nil;
    LAfter.Free;
    LBefore.Free;
    LAgent.Free;
  end;
end;

end.
