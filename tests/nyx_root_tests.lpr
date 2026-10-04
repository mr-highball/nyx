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
program nyx_root_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.root.types, nyx.types, nyx.model, nyx.controls,
  nyx.codec, nyx.data, nyx.source, nyx.callbacks, nyx.studio.session,
  nyx.studio.rootedits, nyx.studio.projects, nyx.studio.agents,
  nyx.studio.rootview
  {$ifdef PAS2JS}, Web{$endif};

type
  { A different admitted implementation must survive document removal when the
    caller retains its specialized interface, and retire exactly once otherwise. }
  TCountingColumn = class(TNyxColumn)
  public
    destructor Destroy; override;
  end;

var
  GChecks: Integer;
  GDisposed: Integer;

destructor TCountingColumn.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function RootWire(const AName, AKind: TNyxText): TNyxDataValue;
begin
  Result := NyxObject([NyxField('root', NyxData(AKind)), NyxField('id', NyxData(AName))]);
end;

procedure ModelOwnership;
var
  LDocument: TNyxDocument;
  LFirst: INyxColumn;
  LSecond: INyxColumn;
  LRefused: Boolean;
begin
  LDocument := TNyxDocument.Create;
  try
    LFirst := TCountingColumn.Create('kept-🌙');
    LSecond := TCountingColumn.Create('remaining');
    LDocument.AddPage(LFirst).AddPage(LSecond);
    LRefused := False;
    try
      LDocument.RemoveRoot(NyxReusableRoot('kept-🌙'));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LDocument.Count = 2), 'Wrong partition refuses before releasing any anchor');
    LDocument.RemoveRoot(NyxPageRoot('kept-🌙'));
    Check((LDocument.Count = 1) and (LDocument.Pages[0] = LSecond.Node), 'Removal retains order and sibling implementation');
    Check((GDisposed = 0) and (LFirst.Node.ID = 'kept-🌙') and
      not LDocument.Contains(LFirst.Node), 'Retained specialized implementation remains independently alive');
    LDocument.AddComponent(LFirst);
    LFirst := nil;
    Check(GDisposed = 0, 'Reattached implementation anchor owns its supplier');
    LDocument.RemoveRoot(NyxReusableRoot('kept-🌙'));
    Check(GDisposed = 1, 'The final implementation anchor retires exactly once');
    LSecond := nil;
    LDocument.RemoveRoot(NyxPageRoot('remaining'));
    Check((GDisposed = 2) and (LDocument.Count = 0), 'Removing the last page is valid and retires its supplier');
    LDocument.Validate;
    LDocument.AddPage(TNyxNode.Create(nkPage, 'raw'));
    LDocument.RemoveRoot(NyxPageRoot('raw'));
    Check(LDocument.Count = 0, 'Raw-owned root lifetime also completes');
    LRefused := False;
    try
      LDocument.FindRoot(Default(TNyxRootRef));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'An absent typed reference cannot select a default root');
  finally
    LDocument.Free;
    LFirst := nil;
    LSecond := nil;
  end;
end;

procedure PairedRemoval;
var
  LSession: TNyxStudioSession;
  LReview: INyxRootRemoval;
  LBlocked: INyxRootRemoval;
  LOriginal: TNyxProjectPair;
  LRemoved: TNyxProjectPair;
  LRefused: Boolean;
  LPrefix: TNyxText;
  LBuilder: TNyxText;
  LSuffix: TNyxText;
  LNewPrefix: TNyxText;
  LNewSuffix: TNyxText;
  LLine: Integer;
  LParts: TNyxStrings;
begin
  LSession := TNyxStudioSession.Create;
  try
    LSession.Document.Find('home').Add(TNyxNode.Create(nkComponent, 'kept-use')
      .Configure.Component(NyxComponent('welcome-card')).Done);
    LSession.Select('project-name');
    LSession.AddCallback(ntBeforeTextInput, LLine);
    LOriginal := LSession.ProjectSnapshot;
    LBlocked := ReviewNyxRootRemoval(LOriginal, [NyxReusableRoot('welcome-card')]);
    Check(not LBlocked.Inspect.Field('ready').AsBoolean and
      (LBlocked.Inspect.Field('retainedReferences').AsInteger = 2), 'Review reports the original and added retained reusable dependencies');
    LRefused := False;
    try
      LSession.RemoveRoots(LBlocked);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LOriginal)),
      'Blocked removal preserves the entire accepted pair');
    LReview := ReadNyxRootRemoval(LOriginal, NyxArray([
      RootWire('welcome-card', 'component'), RootWire('home', 'page')]));
    Check(LReview.Inspect.Field('ready').AsBoolean and
      (LReview.Inspect.Field('registrations').AsInteger = 1), 'Grouped root dependencies and callback warnings are exact');
    SplitNyxSourceFrame(LOriginal.Source, LPrefix, LBuilder, LSuffix);
    LSession.RemoveRoots(LReview);
    LRemoved := LSession.ProjectSnapshot;
    SplitNyxSourceFrame(LRemoved.Source, LNewPrefix, LBuilder, LNewSuffix);
    Check((LSession.Document.Count = 0) and (LSession.Document.ComponentCount = 0), 'A whole root group retires into a valid empty document');
    Check((LSession.ActiveViewID = '') and (LSession.SelectedID = ''), 'Empty workspace has no stale selection/view pointer');
    Check((LPrefix = LNewPrefix) and (LSuffix = LNewSuffix), 'Pascal imports, helpers and real TODO classes retain every byte');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LOriginal), 'One Undo restores both exact roots and companion');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LRemoved), 'One Redo restores the exact retired pair');
    LSession.Undo;
    LParts := TNyxStrings.Create;
    try
      LParts.Add(LSession.Source);
      LParts.Add(#10);
      LParts.Add('// Protected draft 🌙');
      LSession.SetSourceDraft(LParts.Join);
    finally
      LParts.Free;
    end;
    LRefused := False;
    try
      LSession.RemoveRoots(LReview);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (Pos('Protected draft 🌙', LSession.DraftSource) > 0) and LSession.CanRedo,
      'Pending drafts and Redo survive a refused reviewed command');
    LSession.DiscardSourceDraft;
    LSession.Document.Title := 'Changed after review';
    LRefused := False;
    try
      LSession.RemoveRoots(LReview);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Document.Title = 'Changed after review'), 'Immutable reviews cannot remove after an intervening project edit');
    LBlocked := nil;
    LReview := nil;
  finally
    LSession.Free;
  end;
end;

procedure SemanticRemoval;
var
  LAgent: TNyxAgentSession;
  LRoots: TNyxDataValue;
  LReview: TNyxDataValue;
  LResult: TNyxDataValue;
  LApply: TNyxDataValue;
  LProject: TNyxText;
  LRevision: Integer;
  LIndex: Integer;
  LArgs: TNyxDataValue;
  LTicket: TNyxText;

  function Pair: TNyxText;
  begin
    Result := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe')),
      NyxField('after', NyxData(0))])).Field('project').AsText;
  end;

  function Args(const AMode: TNyxText; const ARoots: TNyxDataValue;
    const AReviewID: TNyxText = ''): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
  begin
    SetLength(LFields, 3);
    LFields[0] := NyxField('mode', NyxData(AMode));
    LFields[1] := NyxField('expectedRevision', NyxData(LAgent.Revision));
    LFields[2] := NyxField('roots', ARoots);

    if AReviewID <> '' then
    begin
      SetLength(LFields, 5);
      LFields[3] := NyxField('operationId', NyxData('remove-reviewed-roots'));
      LFields[4] := NyxField('reviewID', NyxData(AReviewID));
    end;
    Result := NyxObject(LFields);
  end;

  procedure Refuses(const AArguments: TNyxDataValue; const AReason: TNyxText;
    const AActor: TNyxText = 'Scooty');
  var
    LBefore: TNyxText;
    LRejected: Boolean;
    LBeforeRevision: Integer;
  begin
    LBefore := Pair;
    LBeforeRevision := LAgent.Revision;
    LRejected := False;
    try
      LAgent.Call('nyx_roots', AActor, AArguments);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (Pair = LBefore) and (LAgent.Revision = LBeforeRevision), AReason);
  end;

  procedure Permission(const AValue: TNyxText);
  begin
    LAgent.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData(AValue))]));
  end;

begin
  LAgent := TNyxAgentSession.Create;
  try
    LProject := Pair;
    LRevision := LAgent.Revision;
    LRoots := NyxArray([RootWire('home', 'page'), RootWire('welcome-card', 'component')]);
    LReview := LAgent.Call('nyx_roots', 'Scooty', Args('review', LRoots));
    Check((LReview.Field('removal').Field('roots').Count = 2) and
      LReview.Field('removal').Field('ready').AsBoolean, 'Semantic review sees a complete root group');
    Check((Pair = LProject) and (LAgent.Revision = LRevision), 'Review is observational and adds no content history');
    Refuses(Args('review', NyxArray([RootWire('project-name', 'page')])), 'Descendants cannot masquerade as roots');
    Refuses(Args('review', NyxArray([RootWire('home', 'component')])), 'Wrong root partition refuses');
    Refuses(Args('review', NyxArray([RootWire('home', 'page'), RootWire('absent', 'page')])), 'Missing grouped root refuses atomically');
    Refuses(Args('review', NyxArray([RootWire('home', 'page'), RootWire('home', 'page')])), 'Duplicate roots refuse');
    Refuses(Args('review', NyxArray([NyxObject([NyxField('root', NyxData('page')),
      NyxField('id', NyxData('home')), NyxField('cascade', NyxData(True))])])), 'Unknown cascading fields refuse');
    Refuses(Args('review', NyxArray([NyxObject([NyxField('root', NyxData('page')),
      NyxField('id', NyxData(7))])])), 'Root ID scalar type is exact');
    LApply := Args('apply', LRoots, LReview.Field('reviewID').AsText);
    Refuses(LApply, 'Review actor identity is enforced', 'Another actor');
    Refuses(Args('apply', NyxArray([RootWire('home', 'page')]), LReview.Field('reviewID').AsText),
      'Review cannot silently narrow a removal group');
    Permission('readOnly');
    LAgent.Call('nyx_roots', 'Scooty', Args('review', LRoots));
    Check(Pair = LProject, 'Read-only clients can inspect a removal review');
    Refuses(LApply, 'Read-only clients cannot remove roots');
    Permission('disabled');
    Refuses(Args('review', LRoots), 'Disabled access refuses even reviews');
    Permission('edit');
    for LIndex := 1 to 8 do
    begin
      LAgent.Call('nyx_roots', 'Scooty', Args('review', LRoots));
    end;
    Refuses(LApply, 'Evicted review tickets refuse rather than select another group');
    LReview := LAgent.Call('nyx_roots', 'Scooty', Args('review', LRoots));
    LTicket := LReview.Field('reviewID').AsText;
    LApply := Args('apply', LRoots, LTicket);
    LResult := LAgent.Call('nyx_roots', 'Scooty', LApply);
    Check((LResult.Field('pages').AsInteger = 0) and
      (LResult.Field('components').AsInteger = 0) and
      LResult.Field('canUndo').AsBoolean and not LResult.Field('canRedo').AsBoolean,
      'Root removal publishes one ordinary paired command');
    Check((LResult.Field('view').AsText = '') and (LResult.Field('selection').AsText = ''),
      'Semantic receipt never points into a retired root');
    Check(LAgent.Call('nyx_roots', 'Scooty', LApply).ToJSON = LResult.ToJSON,
      'Exact retry returns the original receipt after consuming its review');
    Check(LAgent.Revision = LRevision + 1, 'Retry neither removes again nor advances revision');
    LArgs := NyxObject([NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('undo-root-group')), NyxField('direction', NyxData('undo'))]);
    LAgent.Call('nyx_history', 'Scooty', LArgs);
    Check(Pair = LProject, 'Semantic history restores the entire accepted pair in one step');
    Refuses(LApply, 'Reused operation identity cannot remove again at another revision', 'Another actor');
    LAgent.Call('nyx_history', 'Scooty', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('redo-root-group')), NyxField('direction', NyxData('redo'))]));
    Check(LAgent.Call('nyx_session', 'Scooty', NyxObject([])).Field('pages').AsInteger = 0,
      'Semantic Redo retires the same root group');
  finally
    LAgent.Free;
  end;
end;

procedure OrdinaryReview;
var
  LSession: TNyxStudioSession;
  LReview: INyxRootRemoval;
  LCard: TNyxNode;
  LPair: TNyxText;
begin
  LSession := TNyxStudioSession.Create;
  try
    LPair := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(RouteNyxRootRemoval(LSession, NyxStudioReviewRootID, ntClick, LReview) = nreReview,
      'Ordinary Studio routes the exact page review');
    LCard := BuildNyxRootRemovalCard(LReview.Inspect);
    try
      Check((LCard.Find(NyxStudioRemoveRootID) <> nil) and
        (Pos('Pascal imports', LCard.Find('root-removal-warning').Prop('text')) > 0),
        'Nyx confirmation exposes the warning and removal action');
    finally
      LCard.Free;
    end;
    RouteNyxRootRemoval(LSession, NyxStudioCancelRootID, ntClick, LReview);
    Check((LReview = nil) and (EncodeNyxProject(LSession.ProjectSnapshot) = LPair),
      'Cancel releases its command while retaining the project');
    RouteNyxRootRemoval(LSession, NyxStudioReviewRootID, ntClick, LReview);
    RouteNyxRootRemoval(LSession, NyxStudioRemoveRootID, ntClick, LReview);
    Check((LSession.Document.Count = 0) and (LSession.Document.ComponentCount = 1) and
      (LSession.ActiveViewID = 'welcome-card'), 'Last-page deletion falls back to a surviving reusable view');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LPair, 'Ordinary command restores its exact paired checkpoint');
    LSession.Activate('welcome-card');
    RouteNyxRootRemoval(LSession, NyxStudioReviewRootID, ntClick, LReview);
    LCard := BuildNyxRootRemovalCard(LReview.Inspect);
    try
      Check(LCard.Find(NyxStudioRemoveRootID).Prop('enabled') = 'false',
        'Referenced reusable removal is visibly unavailable');
    finally
      LCard.Free;
    end;
    LReview := nil;
  finally
    LSession.Free;
  end;
end;

begin
  try
    ModelOwnership;
    PairedRemoval;
    SemanticRemoval;
    OrdinaryReview;
    WriteLn('PASS ', GChecks, ' root ownership/paired removal checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-roots', 'passed');
    document.body.setAttribute('data-nyx-root-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-roots', 'failed');
      document.body.setAttribute('data-nyx-root-error', LException.Message);
      {$else}ExitCode := 1;{$endif}
    end;
  end;
end.
