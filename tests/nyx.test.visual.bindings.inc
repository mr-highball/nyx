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


{ This complete handwritten project journey uses the ordinary Studio compiler
  queue. Its live coordinator realizes a view against independent runtime state,
  exercising binding projection/editing, not merely state-store assignment.
  Native/browser controls and HTTP execution retain their separate input gates. }

procedure BindingJourney(AExecutor: TNyxBuildExecutor; const ACompiler: INyxSourceCompiler;
  const AOriginal: INyxSourceProjection);
const
  CRenamed: TNyxText = 'reply-text 🚀';
  CEdited: TNyxText = 'A live reply 🚀 𐐷 é';
var
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LDraft: TNyxText;
  LMemo: TNyxText;
  LSource: TNyxText;
  LEdit: TNyxStudioDesignEdit;
  LSpec: TNyxBindingSpec;
  LPair: TNyxProjectPair;
  LRejected: Boolean;
  LRevision: Integer;
  LRuntime: TNyxNode;
  LState: TNyxState;
  LBindings: TNyxLiveBindings;
  LBuild: INyxSourceProjectionBuild;
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LNewMemo: INyxMemo;

  function Intent(AAction: TNyxStudioDesignAction; const ASelection: TNyxText): TNyxStudioDesignEdit;
  begin
    Result := Default(TNyxStudioDesignEdit);
    Result.Action := AAction;
    Result.Selection := ASelection;
    Result.View := LSession.ActiveViewID;
  end;

  procedure Submit(const AEdit: TNyxStudioDesignEdit; const AReason: TNyxText);
  var
    LPrevious: TNyxProjectPair;
    LAccepted: TNyxProjectPair;
    LSelection: TNyxText;
    LView: TNyxText;
  begin
    LSession.Select(AEdit.Selection);
    LPrevious := NyxProjectPair(LSession.Save, LSession.Source);
    LCommands.Edit(AEdit);
    Drain(LCommands);
    Check(LCommands.State = nssApplied, AReason + ': ' + LCommands.Message);
    LAccepted := NyxProjectPair(LSession.Save, LSession.Source);
    LSelection := LSession.SelectedID;
    LView := LSession.ActiveViewID;
    Check((LSession.DraftSource = LDraft) and (LSession.SourceDraftBase = AOriginal.Source),
      AReason + ' retains the exact independent draft/base');
    LSession.Undo;
    Check((LSession.Save = LPrevious.Design) and (LSession.Source = LPrevious.Source),
      AReason + ' has one exact paired Undo');
    LSession.Redo;
    Check((LSession.Save = LAccepted.Design) and (LSession.Source = LAccepted.Source) and
      (LSession.DraftSource = LDraft), AReason + ' has one exact paired Redo');
    LSession.Activate(LView);
    LSession.Select(LSelection);
  end;

  procedure DefaultValue(AAction: TNyxStudioDesignAction; const AName, AValue: TNyxText;
    AInput: TNyxStudioStateInput; const AReason: TNyxText);
  var
    LIntent: TNyxStudioDesignEdit;
  begin
    LIntent := Intent(AAction, LSession.SelectedID);
    LIntent.Name := AName;
    LIntent.Value := AValue;
    LIntent.StateInput := AInput;
    Submit(LIntent, AReason);
  end;

  procedure Bind(const AControl: TNyxText; const ASpec: TNyxBindingSpec;
    const AReason: TNyxText);
  var
    LIntent: TNyxStudioDesignEdit;
  begin
    LIntent := Intent(sdaSetBinding, AControl);
    LIntent.Binding := ASpec.Copy;
    Submit(LIntent, AReason);
  end;

  procedure Inherit(const AControl: TNyxText; ATarget: TNyxBindingProperty);
  var
    LIntent: TNyxStudioDesignEdit;
  begin
    LIntent := Intent(sdaInheritBinding, AControl);
    LIntent.Binding := TNyxBindingSpec.Clear(ATarget);
    Submit(LIntent, 'restore inherited binding');
  end;

begin
  LSession := TNyxStudioSession.Create;
  LCommands := nil;
  LRuntime := nil;
  LState := nil;
  LBindings := nil;
  LBefore := nil;
  LAfter := nil;
  try
    LSession.AdoptProjectedProject(NyxProjectPair(AOriginal.Design, AOriginal.Source), AOriginal);
    LSession.Activate('notebook-1');
    LSession.Select('heading-1');
    LDraft := AOriginal.Source + TNyxText(#10 + '{ Another unfinished idea 🚀 𐐷 é }');
    LSession.SetSourceDraft(LDraft);
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LCommands.UseCompiler(ACompiler);

    DefaultValue(sdaCreateStateDefault, 'entry-enabled', 'true', ssiBoolean,
      'create Boolean default');
    DefaultValue(sdaSetStateDefault, 'scale', '0.123456789012345', ssiNumber,
      'update exact numeric default');
    DefaultValue(sdaSetStateDefault, 'notes', '-2147483648', ssiInteger,
      'update signed integer default');
    Bind('heading-1', TNyxBindingSpec.Bound(bpText, 'greeting', nskText, bdFromState),
      'bind existing heading text');
    LEdit := Intent(sdaAddKind, 'notebook-1');
    LEdit.Name := NyxKindName(nkMemo);
    Submit(LEdit, 'create input in handwritten view');
    LMemo := LSession.SelectedID;
    Bind(LMemo, TNyxBindingSpec.Bound(bpValue, 'greeting', nskText, bdTwoWay),
      'bind memo value in both directions');
    DefaultValue(sdaRenameStateDefault, 'greeting', CRenamed, ssiText,
      'rename bound text and all authored references');
    Check((LSession.Document.State.Key(0) = CRenamed) and
      not LSession.Document.State.Has('greeting') and
      LSession.Document.Find(LMemo).FindBinding(bpValue, LSpec) and
      (LSpec.StateName = CRenamed), 'rename retains order and exact Unicode binding reference');
    Check((Pos('.Rename(NyxTextState(''greeting''), NyxTextState(', LSession.Source) > 0) and
      (Pos('TNotebookCards.Caption', LSession.Source) > 0),
      'typed rename retains the original computed default instead of hardcoding its value');
    Bind(LMemo, TNyxBindingSpec.Bound(bpEnabled, 'entry-enabled', nskBoolean, bdFromState),
      'bind Boolean enabled state');
    Bind('heading-1', TNyxBindingSpec.Resource(bpHint,
      NyxResourceValue(NyxResourceRef('instructions')).AsText), 'bind packed text resource');
    Bind('heading-1', TNyxBindingSpec.Resource(bpText,
      NyxResourceValue(NyxResourceRef('data')).Field('value').AsText),
      'bind packed JSON selector');
    Bind('heading-1', TNyxBindingSpec.Clear(bpText), 'author an explicit binding clear');
    Check(LSession.Document.Find('heading-1').FindAuthoredBinding(bpText, LSpec) and LSpec.Cleared and
      not LSession.Document.Find('heading-1').FindBinding(bpText, LSpec),
      'explicit clear remains distinct from absence');
    Inherit('heading-1', bpText);
    Check(not LSession.Document.Find('heading-1').FindBinding(bpText, LSpec),
      'inherit removes the explicit binding descriptor');
    Bind('heading-1', TNyxBindingSpec.Bound(bpText, CRenamed, nskText, bdFromState),
      'restore text binding after inheritance');
    Check((LSession.Document.Find('heading-1').Bindings[0].Target = bpHint) and
      (LSession.Document.Find('heading-1').Bindings[1].Target = bpText),
      'clear/inherit/rebind keeps exact authored binding order');

    { The real coordinator owns no document defaults or view; the fixture owns
      and frees it before either borrowed argument. Runtime edits must project to
      all bound controls while the Studio design/defaults remain independent. }
    LRuntime := RealizeNyxView(LSession.Document, LSession.Document.Find('notebook-1'));
    LState := LSession.Document.State.Clone;
    LBindings := TNyxLiveBindings.Create(LRuntime, LState);
    LBindings.Activate;
    Check(LRuntime.Find('heading-1').Prop('text') = LState.GetValue(NyxTextState(CRenamed)),
      'live view projects the compiled default to its existing heading');
    LBindings.Edit(LRuntime.Find(LMemo), CEdited);
    Check((LState.GetValue(NyxTextState(CRenamed)) = CEdited) and
      (LRuntime.Find('heading-1').Prop('text') = CEdited),
      'live memo edit updates runtime state and the second bound control');
    LState.SetValue(NyxBooleanState('entry-enabled'), False);
    Check((LRuntime.Find(LMemo).Prop('enabled') = 'false') and
      LSession.Document.State.GetValue(NyxBooleanState('entry-enabled')) and
      (LSession.Document.State.GetValue(NyxTextState(CRenamed)) <> CEdited),
      'live Boolean projection and runtime edit leave document defaults untouched');
    { A store-only rename cannot repair references in the borrowed live view.
      Its existing validator must refuse before changing either owner. }
    LRevision := LState.Revision;
    LRejected := False;
    try
      LState.Rename(NyxTextState(CRenamed), NyxTextState('runtime-only-reply'));
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LState.Revision = LRevision) and LState.Has(CRenamed) and
      not LState.Has('runtime-only-reply') and
      (LRuntime.Find('heading-1').Prop('text') = CEdited),
      'live binding validation refuses an incomplete rename before publication');
    FreeAndNil(LBindings);
    FreeAndNil(LRuntime);
    FreeAndNil(LState);

    { Bound removal must fail as a whole before accepted files/history change. }
    LPair := NyxProjectPair(LSession.Save, LSession.Source);
    LEdit := Intent(sdaRemoveStateDefault, LSession.SelectedID);
    LEdit.Name := CRenamed;
    LEdit.StateInput := ssiText;
    LCommands.Edit(LEdit);
    Drain(LCommands);
    Check((LCommands.State = nssRejected) and (LSession.Save = LPair.Design) and
      (LSession.Source = LPair.Source) and (LSession.DraftSource = LDraft),
      'removing a used default refuses the complete source/design/draft change: ' +
      LCommands.Message);
    Inherit('heading-1', bpText);
    Inherit(LMemo, bpValue);
    DefaultValue(sdaRemoveStateDefault, CRenamed, '', ssiText,
      'remove unused text default');
    DefaultValue(sdaRenameStateDefault, 'notes', 'note-count', ssiInteger,
      'rename typed integer default');
    DefaultValue(sdaRenameStateDefault, 'scale', 'zoom-scale', ssiNumber,
      'rename typed number default');
    DefaultValue(sdaRenameStateDefault, 'entry-enabled', 'editing-enabled', ssiBoolean,
      'rename typed Boolean default and its binding');
    { Compare the portable Double family. FPC can retain a wider uncast literal
      during comparison, despite exact persisted/compiled Double reproduction. }
    Check((LSession.Document.State.Key(0) = 'note-count') and
      (LSession.Document.State.GetValue(NyxIntegerState('note-count')) = Low(Integer)) and
      (LSession.Document.State.GetValue(NyxNumberState('zoom-scale')) = Double(0.123456789012345)) and
      LSession.Document.Find(LMemo).FindBinding(bpEnabled, LSpec) and
      (LSpec.StateName = 'editing-enabled'), 'all renamed scalar families retain exact values/order');
    DefaultValue(sdaCreateStateDefault, 'scratch', '"New 🚀\u0000draft"', ssiEscapedText,
      'create text containing supplementary Unicode and NUL');
    Check(LSession.Document.State.GetValue(NyxTextState('scratch')) = TNyxText('New 🚀' + #0 + 'draft'),
      'actual compiled source retains embedded NUL exactly');
    DefaultValue(sdaRemoveStateDefault, 'scratch', '', ssiText,
      'remove newly created text default');

    { Public rename refusal is independent of Studio's document transaction.
      Collisions and wrong family must retain exact order/value/revision. }
    LState := LSession.Document.State.Clone;
    LRevision := LState.Revision;
    LSource := TNyxCodec.Encode(LSession.Document);
    LRejected := False;
    try
      LState.Rename(NyxIntegerState('note-count'), NyxIntegerState('zoom-scale'));
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LState.Revision = LRevision) and (LState.Key(0) = 'note-count'),
      'typed rename collision refuses before order/value/revision publication');
    LRejected := False;
    try
      LState.Rename(NyxBooleanState('note-count'), NyxBooleanState('wrong-family'));
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LState.Revision = LRevision) and not LState.Has('wrong-family'),
      'typed rename rejects a wrong reference family atomically');
    LState.Rename(NyxIntegerState('note-count'), NyxIntegerState('note-count'));
    Check((LState.Revision = LRevision) and (LSession.Save = LSource),
      'same-name rename is a no-op and independent state does not change Studio');
    Check(CustomizeNyxExecutedSource(LSession.Source, LSession.Document,
      LSession.Document) = LSession.Source, 'complete binding journey no-op retains exact source');
    { One complete proposal also combines rename with an explicit value update
      and a newly bound control/default. This qualifies writer/compiler meaning;
      it does not claim grouped editor publication with a divergent draft. }
    LBefore := LSession.Document.Clone;
    LAfter := LBefore.Clone;
    LAfter.State.Rename(NyxNumberState('zoom-scale'), NyxNumberState('review-scale'));
    LAfter.State.SetValue(NyxNumberState('review-scale'), 18.0);
    LAfter.State.SetValue(NyxTextState('review-prompt'), CEdited);
    LNewMemo := NewNyxMemo('review-memo');
    LNewMemo.Configure.Text('Review prompt').Done;
    LNewMemo.Binds.Value(NyxTextState('review-prompt')).Done;
    LAfter.Find('notebook-2').Add(LNewMemo);
    LSource := CustomizeNyxExecutedSource(LSession.Source, LBefore, LAfter);
    Check((Pos('.Rename(NyxNumberState(''zoom-scale''), NyxNumberState(''review-scale''))',
      LSource) > 0) and (Pos('.SetValue(NyxNumberState(''review-scale''), 18.0)', LSource) > 0),
      'one proposal keeps both typed rename and subsequent exact value update');
    LBuild := AExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = TNyxCodec.Encode(LAfter)),
      'complete mixed state/control/binding construction executes with exact meaning');
    Check((LSession.DraftSource = LDraft) and (LSession.SourceDraftBase = AOriginal.Source),
      'independent mixed proposal never consumes the editor unfinished draft');
    LBuild := AExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'), btBrowser);
    Check((LBuild.Projection.State = spsCompiled) and (LBuild.Projection.Design = ''),
      'complete state/resource-binding journey compiles for pas2js without asserting execution');
  finally
    LNewMemo := nil;
    LAfter.Free;
    LBefore.Free;
    LBindings.Free;
    LRuntime.Free;
    LState.Free;
    LCommands.Free;
    LSession.Free;
  end;
end;
