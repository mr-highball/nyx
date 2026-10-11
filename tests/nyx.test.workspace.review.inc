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

{ Included by the Delphi-dialect, UTF-8 native/browser ownership consumer.
  These independent owners model real accepted/draft/history values; no DOM/LCL
  type, synthetic executed checkpoint or user registry supplies authority. }
procedure ReviewHandoff;
var
  LPrimary: TNyxAgentSession;
  LReviews: TNyxReviewWorkspaces;
  LProjects: TNyxStudioWorkspaces;
  LRecoveredPrimary: TNyxAgentSession;
  LRecoveredProjects: TNyxStudioWorkspaces;
  LReview: TNyxReviewRef;
  LReviewSession: TNyxAgentSession;
  LProject: TNyxWorkspaceRef;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LParent: TNyxDataValue;
  LBefore: TNyxDataValue;
  LAccepted: TNyxProjectPair;
  LIndex: Integer;

  procedure Pending(ASession: TNyxAgentSession);
  var
    LPair: TNyxProjectPair;
  begin
    LPair := DecodeNyxProject(Frame(ASession).Field('project').AsText);
    LPair.Pending := True;
    LPair.DraftBase := LPair.Source;
    LPair.Draft := LPair.Source + TNyxText(#10 + '// unfinished review 🌙');
    ASession.Exchange(NyxObject([NyxField('op', NyxData('commit')),
      NyxField('expectedRevision', NyxData(ASession.Revision)),
      NyxField('project', NyxData(EncodeNyxProject(LPair))),
      NyxField('selection', NyxData('home')), NyxField('view', NyxData('home'))]));
  end;

  function Request(const AOperation, ABase: TNyxText; ARevision: Integer;
    const AReview: TNyxText): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('mode', NyxData('create')),
      NyxField('operationId', NyxData(AOperation)), NyxField('base', NyxData(ABase)),
      NyxField('label', NyxData('A lasting notebook')),
      NyxField('expectedRevision', NyxData(ARevision)),
      NyxField('sourceReview', NyxData(AReview))]);
  end;

  procedure RefuseSeed(const AOwner: TNyxText; const AArguments: TNyxDataValue);
  var
    LRefused: Boolean;
    LCount: Integer;
  begin
    LCount := LProjects.Observe.Count;
    LRefused := False;
    try
      LProjects.Manage(AOwner, 'Same friendly actor', AArguments, LReviews);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LCount = LProjects.Observe.Count),
      'invalid review seed refuses without creating another project');
    EqualFrame(LParent, Frame(LPrimary));
  end;

begin
  LPrimary := nil;
  LReviews := nil;
  LProjects := nil;
  LRecoveredPrimary := nil;
  LRecoveredProjects := nil;
  try
    LPrimary := TNyxAgentSession.Create;
    LReviews := TNyxReviewWorkspaces.Create(LPrimary);
    LProjects := TNyxStudioWorkspaces.Create(LPrimary, 'review-handoff');
    LReview := LReviews.CreateReview('owner-a', 'Same friendly actor',
      'Independent review', nrbAccepted, LPrimary.Revision);
    LReviewSession := LReviews.Resolve('owner-a', LReview);
    LReviews.Call('nyx_transaction', 'owner-a', 'Same friendly actor',
      NyxWithReview(TitleArguments(1, 'review-title', 'A reviewed notebook 🌙'), LReview));
    Pending(LReviewSession);
    LBefore := Frame(LReviewSession);
    LAccepted := DecodeNyxProject(LBefore.Field('project').AsText);
    LAccepted.Pending := False;
    LAccepted.Draft := '';
    LAccepted.DraftBase := '';
    LPrimary.Call('nyx_transaction', 'User', TitleArguments(1, 'primary-one', 'User work'));
    LPrimary.Call('nyx_transaction', 'User', TitleArguments(2, 'primary-two', 'More user work'));
    Pending(LPrimary);
    LParent := Frame(LPrimary);
    Check(LPrimary.Revision <> LReviewSession.Revision,
      'handoff guards the review revision independently of the primary');
    LRequest := Request('keep-review', 'accepted', LReviewSession.Revision, LReview.ID);
    RefuseSeed('owner-b', LRequest);
    RefuseSeed('owner-a', Request('stale-review', 'accepted', 1, LReview.ID));
    RefuseSeed('owner-a', Request('wrong-base', 'empty', 2, LReview.ID));
    RefuseSeed('owner-a', Request('empty-reference', 'accepted', 2, ''));
    LReceipt := LProjects.Manage('owner-a', 'Same friendly actor', LRequest, LReviews);
    LProject := NyxWorkspace(LReceipt.Field('workspace').AsText);
    Check((LReceipt.Field('sourceReview').AsText = LReview.ID) and
      LReceipt.Field('pendingDraftExcluded').AsBoolean and
      (LReceipt.Field('sourceRevision').AsInteger = LReviewSession.Revision),
      'bounded handoff receipt discloses the retained pending review draft');
    Check(Frame(LProjects.Find(LProject)).Field('project').AsText = EncodeNyxProject(LAccepted),
      'handoff copies the exact accepted pair without the independent draft');
    Check((LProjects.Find(LProject).Revision = 1) and
      not LReceipt.Field('session').Field('canUndo').AsBoolean and
      not LReceipt.Field('session').Field('canRedo').AsBoolean,
      'ordinary project starts with fresh history and revision authority');
    EqualFrame(LBefore, Frame(LReviewSession));
    EqualFrame(LParent, Frame(LPrimary));
    LProjects.Call('nyx_transaction', 'owner-a', 'Same friendly actor',
      NyxWithWorkspace(TitleArguments(1, 'project-title', 'A lasting edited notebook'), LProject));
    EqualFrame(LBefore, Frame(LReviewSession));
    LProjects.Call('nyx_history', 'owner-a', 'Same friendly actor', NyxWithWorkspace(
      NyxObject([NyxField('expectedRevision', NyxData(2)),
      NyxField('operationId', NyxData('project-undo')), NyxField('direction', NyxData('undo'))]), LProject));
    Check(Frame(LProjects.Find(LProject)).Field('project').AsText = EncodeNyxProject(LAccepted),
      'one paired project Undo restores its exact accepted review seed');
    LRecoveredPrimary := TNyxAgentSession.CreateRecovered(LPrimary.RecoveryFrame);
    LRecoveredProjects := TNyxStudioWorkspaces.CreateRecovered(LRecoveredPrimary,
      LProjects.RecoveryFrame);
    EqualFrame(Frame(LProjects.Find(LProject)), Frame(LRecoveredProjects.Find(LProject)));
    LReviews.Discard('owner-a', LReview, LReviewSession.Revision);
    Check(LProjects.Manage('owner-a', 'Renamed actor', LRequest, LReviews).ToJSON = LReceipt.ToJSON,
      'exact creation retry survives review disposal without resolving a retired seed');
    RefuseSeed('owner-a', Request('keep-review', 'accepted', 1, LReview.ID));
    RefuseSeed('owner-a', Request('new-from-retired', 'accepted', 2, LReview.ID));
    LReviews.ReleaseOwner('owner-a');
    LProjects.ReleaseOwner('owner-a');
    Check(LProjects.Find(LProject) <> nil, 'ordinary project survives its agent disconnect');
    RefuseSeed('owner-a', LRequest);
    for LIndex := LProjects.Observe.Count to 8 do
    begin
      LProjects.CreateProject('Other notebook', nwbEmpty, LPrimary.Revision);
    end;
    LReview := LReviews.CreateReview('owner-c', 'Scooty', 'Budget review',
      nrbAccepted, LPrimary.Revision);
    RefuseSeed('owner-c', Request('overflow', 'accepted', 1, LReview.ID));
    LPrimary.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('readOnly'))]));
    LParent := Frame(LPrimary);
    RefuseSeed('owner-c', Request('readonly', 'accepted', 1, LReview.ID));
  finally
    LRecoveredProjects.Free;
    LRecoveredPrimary.Free;
    LProjects.Free;
    LReviews.Free;
    LPrimary.Free;
  end;
end;
