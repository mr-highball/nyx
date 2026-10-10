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


{ Exercise ordinary Studio commands with the production native compiler. Each
  accepted candidate is executed and compared as a complete document by the
  source queue. The test owns its session; no observing/user workspace is edited.
  Historical helper scopes, unfinished Unicode source and paired Undo/Redo must
  survive controls being moved, removed and later recreated with the same ID. }

procedure TreeJourney(AExecutor: TNyxBuildExecutor; const ACompiler: INyxSourceCompiler;
  const AOriginal: INyxSourceProjection);
const
  CText: TNyxText = 'An idea 🚀 𐐷 é';
var
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LRow: TNyxText;
  LMemo: TNyxText;
  LButton: TNyxText;
  LPage: TNyxText;
  LDefinition: TNyxText;
  LInstance: TNyxText;
  LDraft: TNyxText;
  LScalarSource: TNyxText;
  LBuild: INyxSourceProjectionBuild;
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LPair: TNyxProjectPair;
  LRejected: Boolean;
  LMemoHandle: INyxMemo;

  function Intent(AAction: TNyxStudioDesignAction; const ASelection: TNyxText;
    const AName: TNyxText = ''; const AValue: TNyxText = ''): TNyxStudioDesignEdit;
  begin
    Result := Default(TNyxStudioDesignEdit);
    Result.Action := AAction;
    Result.Selection := ASelection;
    Result.View := LSession.ActiveViewID;
    Result.Name := AName;
    Result.Value := AValue;
  end;

  procedure Submit(const AEdit: TNyxStudioDesignEdit; const AReason: TNyxText);
  var
    LPrevious: TNyxProjectPair;
    LAccepted: TNyxProjectPair;
    LAcceptedView: TNyxText;
    LAcceptedSelection: TNyxText;
  begin
    { Ordinary toolbar/inspector actions capture the current selection. Navigate
      to the authored target before submission, so completion can follow it. }

    if AEdit.Selection <> '' then
    begin
      LSession.Select(AEdit.Selection);
    end;
    LPrevious := NyxProjectPair(LSession.Save, LSession.Source);
    LCommands.Edit(AEdit);
    Drain(LCommands);
    Check(LCommands.State = nssApplied, AReason + ': ' + LCommands.Message);
    LAccepted := NyxProjectPair(LSession.Save, LSession.Source);
    LAcceptedView := LSession.ActiveViewID;
    LAcceptedSelection := LSession.SelectedID;
    Check((LSession.DraftSource = LDraft) and
      (LSession.SourceDraftBase = AOriginal.Source),
      AReason + ' retains the exact unfinished draft and original baseline');
    LSession.Undo;
    Check((LSession.Save = LPrevious.Design) and (LSession.Source = LPrevious.Source),
      AReason + ' is one exact paired Undo');
    LSession.Redo;
    Check((LSession.Save = LAccepted.Design) and (LSession.Source = LAccepted.Source) and
      (LSession.DraftSource = LDraft), AReason + ' is one exact paired Redo');
    { Selection/view are navigation, independent of paired application history.
      Resume the ordinary journey at its accepted selection after qualification. }
    LSession.Activate(LAcceptedView);
    LSession.Select(LAcceptedSelection);
  end;

  procedure Move(const AControl, ATarget: TNyxText; APlace: TNyxPlacement;
    const AReason: TNyxText);
  begin
    Submit(LSession.CapturePlacement(NyxPlaceControl(NyxControl(AControl),
      NyxControl(ATarget), APlace), LSession.CommandContext), AReason);
  end;

begin
  LSession := TNyxStudioSession.Create;
  LCommands := nil;
  LBefore := nil;
  LAfter := nil;
  try
    LSession.AdoptProjectedProject(NyxProjectPair(AOriginal.Design, AOriginal.Source), AOriginal);
    LSession.Activate('notebook-1');
    LSession.Select('notebook-1');
    LDraft := AOriginal.Source + TNyxText(#10 + '{ An unfinished tree idea 🚀 𐐷 é }');
    LSession.SetSourceDraft(LDraft);
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LCommands.UseCompiler(ACompiler);

    Submit(Intent(sdaAddKind, 'notebook-1', NyxKindName(nkRow)), 'construct a specialized row');
    LRow := LSession.SelectedID;
    Check(LSession.Selected.Kind = NyxKindName(nkRow), 'new row is selected by its exact authored ID');
    Submit(Intent(sdaAddKind, LRow, NyxKindName(nkMemo)), 'construct a specialized memo');
    LMemo := LSession.SelectedID;
    Submit(Intent(sdaProperty, LMemo, 'text', CText), 'edit new memo text');
    LScalarSource := LSession.Source;
    Submit(Intent(sdaProperty, LMemo, 'text', 'A more focused idea'), 'merge repeated memo text');
    Check((Pos(CText, LSession.Source) = 0) and
      (Length(LSession.Source) <= Length(LScalarSource) + 50),
      'latest scalar stage replaces its old value without accumulating a new helper');
    Check(Pos('INyxMemo;', LSession.Source) > 0,
      'constructed memo keeps its specialized reference-counted interface');

    Submit(Intent(sdaAddKind, LRow, NyxKindName(nkLabeledButton)),
      'construct a compound labeled button');
    LButton := LSession.SelectedID;
    Check((LSession.Selected.Kind = NyxKindName(nkLabeledButton)) and
      (LSession.Selected.Count > 0) and (Pos('INyxLabeledButton;', LSession.Source) > 0),
      'compound construction retains real independent parts and specialized interfaces');
    Submit(Intent(sdaAddPage, ''), 'construct another page');
    LPage := LSession.ActiveViewID;
    Check(LSession.Document.Count = 3, 'new page joins the complete handwritten application');
    Move(LRow, LPage, nplInside, 'move populated row across pages');
    Check(LSession.Document.Find(LMemo).Parent.ID = LRow,
      'cross-page placement retains descendants and their authored values');

    Submit(Intent(sdaCreateComponent, LButton), 'derive a reusable compound definition');
    LDefinition := LSession.ActiveViewID;
    Check((LSession.Document.ComponentCount = 2) and
      (LSession.Document.Find(LButton) <> nil) and
      (LSession.Document.FindComponent(LDefinition).Count > 0),
      'reuse adds an independently owned definition and retains the original compound');
    LSession.Activate(LPage);
    Submit(Intent(sdaAddInstance, LPage, LDefinition), 'instantiate the new reusable component');
    LInstance := LSession.SelectedID;
    Check(LSession.Document.Find(LInstance).Prop('component') = LDefinition,
      'new instance points at the exact retained definition');

    Move(LMemo, LPage, nplInside, 'retain memo outside its old host');
    Submit(Intent(sdaDelete, LRow), 'remove old host and its original compound');
    Check((LSession.Document.Find(LRow) = nil) and
      (LSession.Document.Find(LButton) = nil) and
      (LSession.Document.Find(LMemo) <> nil) and
      (LSession.Document.FindComponent(LDefinition) <> nil),
      'host removal retains the moved memo and independently derived definition');
    Submit(Intent(sdaDelete, LMemo), 'remove the formerly constructed memo');
    Submit(LSession.CapturePlacement(NyxPlaceNewControl(nkMemo, NyxControl(LMemo),
      NyxControl(LPage), nplInside), LSession.CommandContext),
      'recreate a control with an earlier exact identity');
    Submit(Intent(sdaProperty, LMemo, 'text', CText), 'edit recreated memo independently');
    Move(LMemo, LInstance, nplBefore, 'reorder recreated memo before its sibling');
    Check((LSession.Document.Find(LMemo).Parent.ID = LPage) and
      (LSession.Document.Find(LPage).Children[0].ID = LMemo) and
      (LSession.Document.Find(LMemo).Prop('text') = CText),
      'recreated control has exact text, parent and final sibling order');
    Check((Pos('Text(PageName(LIndex))', LSession.Source) > 0) and
      (Pos('Padding(8 + LIndex)', LSession.Source) > 0) and
      (Pos('class function TNotebookCards.Definition', LSession.Source) > 0),
      'the complete journey retains unrelated handwritten helpers and expressions');
    Check(CustomizeNyxExecutedSource(LSession.Source, LSession.Document,
      LSession.Document) = LSession.Source, 'tree continuation no-op preserves exact source bytes');

    { Public moves must not release implementation anchors, including same-parent
      ordering and invalid/cyclic attempts. The independent clone is discarded;
      holding a specialized facade across removal demonstrates its lifetime. }
    LBefore := LSession.Document.Clone;
    LMemoHandle := RequireNyxControl(LBefore, NyxControl(LMemo), nkMemo) as INyxMemo;
    LBefore.MoveControl(NyxControl(LMemo), NyxControl('notebook-2'), 0);
    LBefore.MoveRoot(NyxPageRoot(LPage), 0);
    LBefore.Validate;
    Check((LMemoHandle.Node.Parent.ID = 'notebook-2') and
      (LMemoHandle.Node.Prop('text') = CText) and (LBefore.Pages[0].ID = LPage),
      'public moves retain specialized implementation, value and exact root partition');
    LPair := NyxProjectPair(TNyxCodec.Encode(LBefore), LSession.Source);
    LRejected := False;
    try
      LBefore.MoveControl(NyxControl(LMemo), NyxControl(LMemo), 0);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LBefore) = LPair.Design),
      'cycle refusal leaves all owners and order unchanged');
    LRejected := False;
    try
      LBefore.MoveControl(NyxControl(LMemo), NyxControl(LPage), -1);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LBefore) = LPair.Design),
      'invalid destination index refuses before changing ownership');
    LBefore.RemoveControl(NyxControl(LMemo));
    Check((LBefore.Find(LMemo) = nil) and (LMemoHandle.Node.Parent = nil) and
      (LMemoHandle.Node.Prop('text') = CText),
      'removing document ownership leaves an independently retained facade alive');
    LMemoHandle := nil;
    FreeAndNil(LBefore);

    { Preparation refuses a damaged ownership boundary and mixed unsupported
      meaning. Neither refusal may mutate accepted source, draft or history. }
    LBefore := LSession.Document.Clone;
    LAfter := LBefore.Clone;
    LAfter.Find(LMemo).Configure.Text('Another idea').Done;
    LPair := NyxProjectPair(LSession.Save, LSession.Source);
    LRejected := False;
    try
      CustomizeNyxExecutedSource(StringReplace(LSession.Source,
        '{ nyx:continuation-end }', '{ Missing continuation end }', []), LBefore, LAfter);
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LPair.Design) and (LSession.Source = LPair.Source) and
      (LSession.DraftSource = LDraft), 'damaged continuation refuses without touching the editor pair');
    LBuild := AExecutor.ProjectSource(LSession.Source, NyxPascalUnit('nyx.projection.fixture'),
      btBrowser);
    Check((LBuild.Projection.State = spsCompiled) and (LBuild.Projection.Design = ''),
      'the complete tree journey compiles for pas2js without claiming browser execution');
  finally
    LMemoHandle := nil;
    LAfter.Free;
    LBefore.Free;
    LCommands.Free;
    LSession.Free;
  end;
end;
