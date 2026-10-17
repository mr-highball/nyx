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

type
  { Focused journeys reuse the actual editor/provider owner. Semantic mode
    exercises the production tools with ordinary observing controls; it never
    substitutes a new listener, browser or trusted source publication. }
  TNyxSourceFileJourney = (sfjFull, sfjFeedback, sfjSemantic, sfjProjectRoutines, sfjOutline);

{ Same maintained ordinary-input harness, with no capture/HTTP execution. Both
  local and connected modes use real compiler processes; the connected transport
  substitutes sockets only, preserving the production backend/provider/bridge.
  Every project, recovery seed and machine profile belongs to this fresh packet. }
procedure RunSourceFileControls(const ADirectories: TNyxStudioDirectories;
  const ATools: TNyxDataValue; AJourney: TNyxSourceFileJourney);
const
  CInitial: TNyxText = 'An independent first plan';
  CChanged: TNyxText = 'A file-authored plan 🚀 𐐷';
  CMainDraft: TNyxText = '{ An unfinished main idea';
  CUnitDraft: TNyxText = '{ An unfinished independent unit';
  CMainDuring: TNyxText = '{ A newer main idea typed during helper compilation';
  { Explicit portable operands prevent a supplementary-character source literal
    from promoting the complete fixture through an ANSI RTL conversion. }
  CBrowserFailure: TNyxText = '  { 🚀 𐐷 } MissingBrowserDiagnostic;';
  CNativeFailure: TNyxText = '  { 🚀 𐐷 } MissingDiagnostic;';
var
  LProfile: TNyxOutputConfiguration;
  LPair: TNyxProjectPair;
  LSource: TNyxText;
  LDocument: TNyxDocument;
  LInputs: INyxPascalProject;
  LVerifier: INyxRuntimeSourceVerifier;
  LProjectVerifier: INyxRuntimeProjectVerifier;
  LProjection: INyxSourceProjection;
  LExtraUnit: TNyxText;
  LUnitEnd: Integer;

  { Real pas2js diagnostics qualify the compiler boundary only. The generated
    application/worker never executes, and no host, socket or browser starts. }
  procedure QualifyBrowserCompilerSite;
  var
    LHelper: TNyxPascalSourceRef;
    LText: TNyxText;
    LAt: Integer;
    LCandidate: INyxPascalProject;
    LExecutor: TNyxBuildExecutor;
    LBuild: INyxSourceProjectionBuild;
    LReceived: INyxSourceProjectionBuild;
    LItem: TNyxCompilerDiagnostic;
    LIndex: Integer;
    LFound: Boolean;
  begin
    LHelper := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('project.values'));
    LText := LHelper.Text(LInputs);
    LAt := Pos('implementation', LText) + Length('implementation') - 1;
    Check(LAt >= Length('implementation'), 'browser compiler fixture owns its explicit unit body');
    LText := Copy(LText, 1, LAt) + #10 + 'procedure BrokenBrowserDiagnostic;' + #10 +
      'begin' + #10 + CBrowserFailure + #10 + 'end;' + #10 +
      Copy(LText, LAt + 1, MaxInt);
    LCandidate := LInputs.WithUnit(NyxPascalUnit('project.values'), LText);
    LExecutor := TNyxBuildExecutor.Create(ADirectories, LProfile.Encode);
    try
      LBuild := LExecutor.ProjectSource(LCandidate, btBrowser, spcChecked);
    finally
      LExecutor.Free;
    end;
    Check(LBuild.Projection.State = spsCompilationFailed,
      'actual pas2js compiler refuses the helper before any browser execution');
    LFound := False;
    for LIndex := 0 to LBuild.Projection.Report.Count - 1 do
    begin
      LItem := LBuild.Projection.Report.Item(LIndex);

      if (LItem.Severity = csError) and LItem.SourceFile.SameFile(LHelper) then
      begin
        LFound := LItem.Navigable and (LItem.SourceColumn = 11);
        Break;
      end;
    end;
    Check(LFound, 'real pas2js helper byte columns map to its typed Unicode scalar location');
    LReceived := DecodeNyxBrowserSourceBuild(LCandidate.Source,
      EncodeNyxBrowserSourceBuild(LBuild), TNyxPascalProjectFiles.Capture(LCandidate));
    Check(LReceived.Projection.Report.Encode = LBuild.Projection.Report.Encode,
      'actual browser compiler failure crosses the source-free private receipt exactly');
    WriteLn('BROWSER HELPER compiler-only diagnostic passed');
    Flush(Output);
  end;

  procedure RunCase(AWidth: Integer; AShared: Boolean);
  var
    LHome: TNyxText;
    LCase: TNyxText;
    LDirectories: TNyxStudioDirectories;
    LFactory: INyxSharedSourceCompilerFactory;
    LFrame: TNyxAgentRecoveryFrame;
    LPrimary: TNyxAgentSession;
    LWorkspaces: TNyxStudioWorkspaces;
    LStore: TNyxStudioRuntimeStore;
    LSeed: TNyxStudioSession;
    LMemo: TMemo;
    LPicker: TComboBox;
    LInclude: TNyxPascalSourceRef;
    LUnit: TNyxPascalSourceRef;
    LUtility: TNyxPascalSourceRef;
    LInitialInclude: TNyxText;
    LChangedInclude: TNyxText;
    LQueuedUnit: TNyxText;
    LQueuedInclude: TNyxText;
    LBeforeQueueInclude: TNyxText;
    LEdit: TNyxStudioDesignEdit;
    LBefore: TNyxText;
    LAfter: TNyxText;
    LStarted: QWord;
    LSite: TNyxSourceDiagnostic;
    LPreviousAction: TNyxNode;
    LRejected: Boolean;
    LUnitSource: TNyxText;
    LInsertion: Integer;

    { Compact chrome intentionally hides the direct Pascal toolbar face. The
      actual popup's English starter caption identifies its physical menu item;
      no hidden command, controller callback or accepted model is invoked. }
    procedure OpenCompactSource;
    const
      CPath: array[0..1] of TNyxText = ('View', 'Pascal source');
    var
      LFormIndex: Integer;
      LFace: TControl;
      LStep: Integer;
      LCaption: TNyxText;

      function FindFace(AParent: TWinControl): TControl;
      var
        LIndex: Integer;
        LChild: TControl;
      begin
        Result := nil;
        for LIndex := 0 to AParent.ControlCount - 1 do
        begin
          LChild := AParent.Controls[LIndex];

          if LChild.Visible and LChild.Enabled and
            ((TNyxText(TControlAccess(LChild).Caption) = LCaption) or
            (Pos(LCaption + ' ', TNyxText(TControlAccess(LChild).Caption)) = 1)) then
          begin
            Exit(LChild);
          end;

          if (LChild is TWinControl) and LChild.Visible then
          begin
            Result := FindFace(TWinControl(LChild));

            if Result <> nil then
            begin
              Exit;
            end;
          end;
        end;
      end;

    begin
      Click('action-actions');
      for LStep := Low(CPath) to High(CPath) do
      begin
        LCaption := CPath[LStep];
        LFace := nil;
        for LFormIndex := 0 to Screen.FormCount - 1 do
        begin

          if (Screen.Forms[LFormIndex] <> GForm) and Screen.Forms[LFormIndex].Visible then
          begin
            LFace := FindFace(Screen.Forms[LFormIndex]);

            if LFace <> nil then
            begin
              Break;
            end;
          end;
        end;
        Check(LFace <> nil, 'compact Actions menu exposes the visible ' + LCaption + ' item');
        TControlAccess(LFace).Click;
        Pump;
      end;
    end;

    procedure Settled;
    var
      LReply: TNyxDataValue;
    begin
      LStarted := GetTickCount64;
      repeat
        Pump;

        if not AShared then
        begin
          Exit;
        end;

        if GStudio.Agents.Conflict then
        begin
          raise Exception.Create('Source-file shared conflict: ' + GStudio.Agents.Status);
        end;

        if GStudio.Agents.Connected and (GSharedExchange.Token <> '') then
        begin
          LReply := GSharedEngine.EditorExchange(GSharedExchange.Token,
            NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));

          if not Assigned(GSharedExchange.Reply) and not GStudio.Agents.Busy and
            (LReply.Field('project').AsText = EncodeNyxProject(GStudio.Session.ProjectSnapshot)) then
          begin
            Exit;
          end;
        end;
        Sleep(1);
      until GetTickCount64 - LStarted > 180000;
      raise Exception.Create('Source-file shared acknowledgement exceeded its budget');
    end;

    procedure Pick(const AFile: TNyxPascalSourceRef);
    var
      LIndex: Integer;
    begin
      LPicker := TComboBox(GStudio.SourceView.InputFor(NyxStudioSourceFileID));
      Check(LPicker <> nil, 'ordinary source-file picker is mounted');
      LIndex := LPicker.Items.IndexOf(GStudio.Session.SourceFileTitle(AFile));
      Check(LIndex >= 0, 'picker contains the exact typed file');
      LPicker.ItemIndex := LIndex;
      LPicker.OnChange(LPicker);
      Settled;
      Check(GStudio.Session.ActiveSourceFile.SameFile(AFile), 'physical choice selects its typed file');
      Check(GStudio.CodeView.InputFor('studio-code') = LMemo, 'file switching keeps the same native memo');
      { Win32 memo presentation uses CRLF. The portable model still retains its
        exact original bytes; normalize only the widget comparison oracle. }
      Check((AdjustLineBreaks(LMemo.Text, tlbsLF) =
        AdjustLineBreaks(String(GStudio.Session.DraftFileText(AFile)), tlbsLF)) and
        (GStudio.CodeView.Root.Prop('value') = GStudio.Session.DraftFileText(AFile)),
        'mounted text and exact code model come from the selected independent buffer');
    end;

    { Ordinary Nyx outline actions navigate the same retained memo, including
      expanded mode. Only target drafts prevent physical navigation; unrelated
      buffers/history remain exact. No browser, capture or host is involved. }
    procedure OutlineControls;
    var
      LOutlineOwner: TComboBox;
      LSnapshot: TComboBox;
      LSearch: TCustomEdit;
      LIndex: INyxPascalRoutineIndex;
      LRow: Integer;
      LScan: Integer;
      LSite: TNyxPascalRoutineSite;
      LLocation: TNyxPascalRoutineLocation;
      LUnchanged: TNyxText;
      LCanUndo: Boolean;
      LCanRedo: Boolean;
      LDetached: TNyxNode;
      LForm: INyxCard;
      LOutline: INyxStudioSourceOutline;
      LEvent: TNyxEventInfo;
      LEffect: TNyxStudioSourceOutlineEffect;
      LOwnSession: TNyxStudioSession;

      procedure OwnerUnit;
      begin
        Click(NyxStudioSourceOutlineTabID);
        Settled;
        LSnapshot := TComboBox(GStudio.SourceView.InputFor(NyxStudioSourceOutlineID + '-snapshot'));
        Check(LSnapshot <> nil, 'ordinary outline exposes its explicit source snapshot');
        LSnapshot.ItemIndex := LSnapshot.Items.IndexOf('Accepted code');
        LSnapshot.OnChange(LSnapshot);
        Settled;
        LOutlineOwner := TComboBox(GStudio.SourceView.InputFor(NyxStudioSourceOutlineID + '-owner'));
        Check(LOutlineOwner <> nil, 'outline uses an ordinary typed Nyx owner selector');
        LOutlineOwner.ItemIndex := LOutlineOwner.Items.IndexOf('project.values.pas');
        Check(LOutlineOwner.ItemIndex >= 0, 'outline lists captured units independently of includes');
        LOutlineOwner.OnChange(LOutlineOwner);
        Settled;
      end;

      procedure Go(APart: TNyxPascalOutlinePart);
      begin
        Click(NyxStudioSourceOutlineID + '-go-' + IntToStr(LRow) + '-' + IntToStr(Ord(APart)));
        Settled;
      end;

    begin
      LIndex := CaptureNyxPascalRoutineIndex(GStudio.Session.AcceptedPascalProject, LUnit);
      LRow := -1;
      for LScan := 0 to LIndex.Count - 1 do
      begin

        if LIndex.Item(LScan).Routine.Name = 'ProjectPlan' then
        begin
          LRow := LScan;
          Break;
        end;
      end;
      Check(LRow >= 0, 'compiled helper routine is discoverable in its owning unit');
      LSite := LIndex.Item(LRow);
      Check(LSite.Definition.FileReference.SameFile(LInclude) and
        LSite.DeclarationLocation.FileReference.SameFile(LUnit),
        'fixture owns a physical include definition and distinct unit declaration');
      OwnerUnit;
      LSearch := TCustomEdit(GStudio.SourceView.InputFor(NyxStudioSourceOutlineID + '-query'));
      Check(LSearch <> nil, 'outline search is an ordinary Nyx input');
      LSearch.Text := 'projectplan';
      Settled;
      Check(GStudio.SourceView.InputFor(NyxStudioSourceOutlineID + '-query') = LSearch,
        'typing preserves the mounted search control');
      Click(NyxStudioSourceOutlineID + '-find');
      Settled;
      Check(GStudio.SourceView.Root.Find(NyxStudioSourceOutlineID + '-summary').Prop('text') =
        '1 routine / accepted source', 'explicit Find searches names/signatures case insensitively');
      LUnchanged := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      LCanUndo := GStudio.Session.CanUndo;
      LCanRedo := GStudio.Session.CanRedo;
      Go(popDefinition);
      Check(GStudio.Session.ActiveSourceFile.SameFile(LInclude) and
        (LMemo.CaretPos.Y + 1 = LSite.Definition.Line),
        'definition jump chooses the physical include instead of main');
      Check((GStudio.CodeView.InputFor('studio-code') = LMemo) and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LUnchanged) and
        (GStudio.Session.CanUndo = LCanUndo) and (GStudio.Session.CanRedo = LCanRedo),
        'routine navigation preserves the complete pair, history and actual memo');
      Click('action-expand-source');
      Settled;
      OwnerUnit;
      Go(popDeclaration);
      Check(GStudio.SourceModal.IsOpen and GStudio.Session.ActiveSourceFile.SameFile(LUnit) and
        (LMemo.CaretPos.Y + 1 = LSite.DeclarationLocation.Line),
        'expanded outline selects the distinct physical unit declaration');
      LMemo.Text := CUnitDraft;
      Settled;
      OwnerUnit;
      Go(popBody);
      Check(GStudio.Session.ActiveSourceFile.SameFile(LInclude) and
        (LMemo.CaretPos.Y + 1 = LSite.BodyLocation.Line) and
        (GStudio.Session.DraftFileText(LUnit) = CUnitDraft),
        'body jump retains an unrelated unfinished unit buffer');
      LMemo.Text := '{ An unfinished include idea 🚀';
      Settled;
      OwnerUnit;
      LUnchanged := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Go(popDefinition);
      Check(GStudio.SourceView.Root.Find(NyxStudioSourceOutlineID + '-message').Prop('text') =
        'This file has unfinished Pascal. Apply or restore it before navigating',
        'unfinished target refuses an obsolete accepted caret with scoped feedback');
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LUnchanged,
        'refused navigation preserves every independent buffer/base/history pair');
      Click('action-source-tab');
      Settled;
      Click('action-reset-source');
      Settled;
      OwnerUnit;
      Go(popBody);
      Check(GStudio.Session.ActiveSourceFile.SameFile(LInclude) and
        (LMemo.CaretPos.Y + 1 = LSite.BodyLocation.Line),
        'explicit target Restore re-enables accepted source navigation');
      Pick(LUnit);
      Click('action-reset-source');
      Settled;
      Click('action-expand-source');
      Settled;
      Check(not GStudio.SourceModal.IsOpen and (GStudio.CodeView.InputFor('studio-code') = LMemo),
        'closing expanded source retains the same input after outline navigation');

      { Detached public boundary checks do not count as physical widget input.
        They cover stale source/load/node snapshots before deferred repaint. }
      LOutline := NewNyxStudioSourceOutline;
      LForm := LOutline.Build(GStudio.Session);
      LEvent := Default(TNyxEventInfo);
      LEvent.Trigger := ntClick;
      LDetached := LForm.Node.Find(NyxStudioSourceOutlineID + '-find').Clone;
      try
        LEffect := LOutline.Route(GStudio.Session, LForm.Node, LDetached, LEvent, LLocation);
        Check((LEffect = soeRebuild) and not LLocation.IsDefined,
          'detached stale action cannot route through a current outline');
      finally
        LDetached.Free;
      end;
      LOwnSession := TNyxStudioSession.Create;
      try
        LForm := LOutline.Build(LOwnSession);
        LOwnSession.SetTitle('A changed source owner');
        LEffect := LOutline.Route(LOwnSession, LForm.Node,
          LForm.Node.Find(NyxStudioSourceOutlineID + '-find'), LEvent, LLocation);
        Check((LEffect = soeRebuild) and not LLocation.IsDefined,
          'accepted input changes reject the still-mounted former snapshot');
        LForm := LOutline.Build(LOwnSession);
        LOwnSession.Load(LOwnSession.Save);
        LEffect := LOutline.Route(LOwnSession, LForm.Node,
          LForm.Node.Find(NyxStudioSourceOutlineID + '-find'), LEvent, LLocation);
        Check((LEffect = soeRebuild) and not LLocation.IsDefined,
          'same-byte reload retires earlier outline session authority');
      finally
        LForm := nil;
        LOwnSession.Free;
      end;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LPair),
        'complete owned observing fixture is restored through ordinary scoped input');
      WriteLn('SOURCE OUTLINE ', LCase, ' passed');
      Flush(Output);
    end;

    { Current-buffer navigation uses the ordinary physical memo and the same
      shared presenter. Its unfinished buffers are never compiled or staged as
      accepted source; all restoration stays with ordinary scoped actions. }
    procedure DraftOutlineControls;
    const
      CMainPrefix: TNyxText = '{ A current main note 🚀 }' + #10#10;
      CUnitPrefix: TNyxText = '{ A current helper note 𐐷 }' + #10;
      CIncludePrefix: TNyxText = '{ A current include note 🚀 𐐷 }' + #10#10;
      CUnfinished: TNyxText = 'procedure WorkInProgress;' + #10 + 'begin';
      CUnrelated: TNyxText = '{ An unrelated unfinished include';
    var
      LSnapshot: TComboBox;
      LOwner: TComboBox;
      LMainText: TNyxText;
      LUnitText: TNyxText;
      LIncludeText: TNyxText;
      LBeforeDrafts: TNyxText;
      LCanUndo: Boolean;
      LCanRedo: Boolean;
      LOutline: INyxStudioSourceOutline;
      LForm: INyxCard;
      LEvent: TNyxEventInfo;
      LLocation: TNyxPascalRoutineLocation;
      LEffect: TNyxStudioSourceOutlineEffect;

      function LineOf(const AText, AName: TNyxText): Integer;
      var
        LIndex: Integer;
        LAt: Integer;
      begin
        LAt := Pos(AName, AText);
        Check(LAt > 0, 'known owned fixture marker exists independently of the routine index');
        Result := 1;
        for LIndex := 1 to LAt - 1 do
        begin

          if AText[LIndex] = #10 then
          begin
            Inc(Result);
          end;
        end;
      end;

      procedure Browse(AKind: TNyxPascalSourceSnapshotKind);
      var
        LCaption: TNyxText;
      begin
        Click(NyxStudioSourceOutlineTabID);
        Settled;
        LSnapshot := TComboBox(GStudio.SourceView.InputFor(NyxStudioSourceOutlineID + '-snapshot'));
        LCaption := 'Editing buffers';

        if AKind = pssAccepted then
        begin
          LCaption := 'Accepted code';
        end;
        LSnapshot.ItemIndex := LSnapshot.Items.IndexOf(LCaption);
        Check(LSnapshot.ItemIndex >= 0, 'source snapshot selector offers its explicit typed mode');
        LSnapshot.OnChange(LSnapshot);
        Settled;
        LOwner := TComboBox(GStudio.SourceView.InputFor(NyxStudioSourceOutlineID + '-owner'));
        LOwner.ItemIndex := LOwner.Items.IndexOf('project.values.pas');
        LOwner.OnChange(LOwner);
        Settled;
      end;

      procedure Go(APart: TNyxPascalOutlinePart);
      begin
        Click(NyxStudioSourceOutlineID + '-go-0-' + IntToStr(Ord(APart)));
        Settled;
      end;

    begin
      LCanUndo := GStudio.Session.CanUndo;
      LCanRedo := GStudio.Session.CanRedo;
      LMainText := CMainPrefix + GStudio.Session.Source;
      LUnitText := CUnitPrefix + GStudio.Session.SourceFileText(LUnit);
      LIncludeText := CIncludePrefix + GStudio.Session.SourceFileText(LInclude) + #10 + CUnfinished;
      Pick(TNyxPascalSourceRef.Main);
      LMemo.Text := LMainText;
      Settled;
      Pick(LUnit);
      LMemo.Text := LUnitText;
      Settled;
      Pick(LUtility);
      LMemo.Text := CUnrelated;
      Settled;
      Pick(LInclude);
      LMemo.Text := LIncludeText;
      Settled;
      Check((GStudio.Session.DraftSource = LMainText) and
        (GStudio.Session.DraftFileText(LUnit) = LUnitText) and
        (GStudio.Session.DraftFileText(LInclude) = LIncludeText),
        'actual memo entry retains exact shifted supplementary main/unit/include buffers');
      LBeforeDrafts := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Click('action-expand-source');
      Settled;
      Browse(pssEditing);
      Check(GStudio.SourceView.Root.Find(NyxStudioSourceOutlineID + '-summary').Prop('text') =
        '1 routine / editing buffers', 'editing mode retains its complete helper before the unfinished body');
      Check(Pos('plan-values.inc', GStudio.SourceView.Root.Find(
        NyxStudioSourceOutlineID + '-message').Prop('text')) > 0,
        'ordinary partial-source feedback names the exact unfinished physical include');
      Go(popDefinition);
      Check(GStudio.SourceModal.IsOpen and GStudio.Session.ActiveSourceFile.SameFile(LInclude) and
        (LMemo.CaretPos.Y + 1 = LineOf(LIncludeText, 'function ProjectPlan')),
        'editing definition navigates to the shifted include inside the same expanded editor');
      Check((EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeDrafts) and
        (GStudio.CodeView.InputFor('studio-code') = LMemo),
        'current-buffer navigation preserves the complete unfinished pair and native input');
      Browse(pssAccepted);
      Go(popDefinition);
      Check(GStudio.SourceView.Root.Find(NyxStudioSourceOutlineID + '-message').Prop('text') =
        'This file has unfinished Pascal. Apply or restore it before navigating',
        'explicit accepted mode still refuses a stale accepted location in a changed buffer');
      Browse(pssEditing);
      Go(popDeclaration);
      Check(GStudio.Session.ActiveSourceFile.SameFile(LUnit) and
        (LMemo.CaretPos.Y + 1 = LineOf(LUnitText, 'function ProjectPlan')),
        'editing declaration selects its independently shifted unfinished unit buffer');
      Browse(pssEditing);
      Go(popBody);
      Check(GStudio.Session.ActiveSourceFile.SameFile(LInclude) and
        (GStudio.CodeView.Root.Prop('value') = LIncludeText) and
        (GStudio.Session.DraftFileText(LUtility) = CUnrelated),
        'editing body navigation preserves unrelated invalid input and exact target text');
      Check((GStudio.Session.CanUndo = LCanUndo) and (GStudio.Session.CanRedo = LCanRedo) and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeDrafts),
        'current-buffer inspection publishes no accepted pair or Undo command');

      { The same shared route checks a changed buffer before a deferred repaint;
        this boundary assertion is separate from controlled widget navigation. }
      LOutline := NewNyxStudioSourceOutline;
      LForm := LOutline.Build(GStudio.Session);
      LMemo.Text := CIncludePrefix + LIncludeText;
      Settled;
      LEvent := Default(TNyxEventInfo);
      LEvent.Trigger := ntClick;
      LEffect := LOutline.Route(GStudio.Session, LForm.Node,
        LForm.Node.Find(NyxStudioSourceOutlineID + '-find'), LEvent, LLocation);
      Check((LEffect = soeRebuild) and not LLocation.IsDefined and
        (GStudio.Session.DraftFileText(LInclude) = CIncludePrefix + LIncludeText),
        'a still-mounted former buffer snapshot refuses after newer typing without overwriting it');
      LForm := nil;
      LOutline := nil;
      Pick(TNyxPascalSourceRef.Main);
      Click('action-reset-source');
      Settled;
      Pick(LUnit);
      Click('action-reset-source');
      Settled;
      Pick(LUtility);
      Click('action-reset-source');
      Settled;
      Pick(LInclude);
      Click('action-reset-source');
      Settled;
      Click('action-expand-source');
      Settled;
      Check(not GStudio.SourceModal.IsOpen and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LPair)) and
        (GStudio.CodeView.InputFor('studio-code') = LMemo),
        'ordinary scoped Restore returns the exact owned pair and retains the editor');
      WriteLn('DRAFT OUTLINE ', LCase, ' passed');
      Flush(Output);
    end;

    { Actual production semantic dispatch and constructor workers publish into
      this ordinary connected editor. No accepted buffer is written by the test;
      the copied initial seed remains trusted setup, not semantic import evidence. }
    procedure SemanticFiles;
    const
      COwner: TNyxText = 'semantic-file-qualification';
      CActor: TNyxText = 'Source project qualification';
      CBadValue: TNyxText = '{ 🚀 𐐷 } MissingSemanticValue';
    var
      LFiles: TNyxDataValue;
      LWindow: TNyxDataValue;
      LArguments: TNyxDataValue;
      LReceipt: TNyxDataValue;
      LStatus: TNyxDataValue;
      LItems: TNyxDataValue;
      LItem: TNyxDataValue;
      LBefore: TNyxText;
      LAfter: TNyxText;
      LMainText: TNyxText;
      LUnitText: TNyxText;
      LIncludeText: TNyxText;
      LHelperOnlyMain: TNyxText;
      LIndex: Integer;
      LMember: Integer;
      LRejected: Boolean;
      LFound: Boolean;
      LStarted: QWord;

      function Revision: Integer;
      begin
        Result := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor,
          NyxObject([NyxField('mode', NyxData('files')),
            NyxField('limit', NyxData(1))])).Field('revision').AsInteger;
      end;

      function Selector(const AFile: TNyxPascalSourceRef): TNyxDataValue;
      var
        LKind: TNyxText;
      begin

        if AFile.Role = npsMain then
        begin
          Exit(NyxObject([NyxField('kind', NyxData('main'))]));
        end;
        LKind := 'unit';

        if AFile.Role = npsInclude then
        begin
          LKind := 'include';
        end;
        Result := NyxObject([NyxField('kind', NyxData(LKind)),
          NyxField('name', NyxData(AFile.Name))]);
      end;

      function Change(const AFile: TNyxPascalSourceRef; const AText,
        AExpected, AReplacement: TNyxText; AAppend: Boolean = False): TNyxDataValue;
      var
        LAt: Integer;
        LOffset: Integer;
      begin
        LAt := Pos(AExpected, AText);

        if AAppend then
        begin
          LAt := Length(AText) + 1;
        end;
        Check(LAt > 0, 'semantic fixture owns the exact requested source span');
        LOffset := NyxTextScalarCount(Copy(AText, 1, LAt - 1));
        LWindow := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor, NyxObject([
          NyxField('mode', NyxData('unit')), NyxField('expectedRevision', NyxData(Revision)),
          NyxField('file', Selector(AFile)), NyxField('offset', NyxData(LOffset)),
          NyxField('count', NyxData(4096))]));
        Check((LWindow.Field('file').ToJSON = Selector(AFile).ToJSON) and
          (Copy(LWindow.Field('text').AsText, 1, Length(AExpected)) = AExpected),
          'bounded semantic text windows retain exact file identity and Unicode expectations');
        Result := NyxObject([NyxField('file', Selector(AFile)),
          NyxField('offset', NyxData(LOffset)), NyxField('expected', NyxData(AExpected)),
          NyxField('replacement', NyxData(AReplacement))]);
      end;

      function Arguments(const AOperation: TNyxText;
        const AChanges: array of TNyxDataValue): TNyxDataValue;
      begin
        Result := NyxObject([NyxField('mode', NyxData('edit-unit')),
          NyxField('expectedRevision', NyxData(Revision)),
          NyxField('operationId', NyxData(AOperation)), NyxField('changes', NyxArray(AChanges))]);
      end;

      function Apply(const AArguments: TNyxDataValue): TNyxDataValue;
      begin
        Result := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor, AArguments);
      end;

      function Await(const AJob: TNyxText): TNyxDataValue;
      begin
        LStarted := GetTickCount64;
        repeat
          Result := GSharedEngine.InvokeBuild(COwner, CActor, NyxObject([
            NyxField('mode', NyxData('status')), NyxField('job', NyxData(AJob))]));

          if (Result.Field('publication').Field('state').AsText = 'committed') or
            (Result.Field('publication').Field('state').AsText = 'refused') then
          begin
            Settled;
            Exit;
          end;
          Pump;
          Sleep(1);
        until GetTickCount64 - LStarted > 180000;
        raise Exception.Create('Semantic source-file compiler/publication exceeded its budget');
      end;

      procedure RefuseRead(const AArguments: TNyxDataValue; const AReason: TNyxText);
      var
        LRejected: Boolean;
        LBeforeRead: TNyxText;
      begin
        LBeforeRead := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        LRejected := False;
        try
          GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor, AArguments);
        except
          on Exception do
          begin
            LRejected := True;
          end;
        end;
        Check(LRejected and (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeRead), AReason);
      end;

      { Ordinary typing publishes acknowledged buffers into the existing semantic
        owner. Reads are bounded, revision-pinned and separate from compilation,
        accepted mutation, history and the retained physical source editor. }
      procedure BufferContext;
      const
        CPrefix: TNyxText = '{ An agent-readable buffer 🚀 𐐷 }' + #10#10;
        CUnitPrefix: TNyxText = '{ A helper buffer 𐐷 }' + #10;
        CUnfinished: TNyxText = 'procedure InProgress;' + #10 + 'begin' + #10 +
          '  WriteLn(''An unfinished literal 🚀';
        CUnrelated: TNyxText = '{ An unrelated unfinished include';
        CEmoji: TNyxText = '🚀';
      var
        LMainBuffer: TNyxText;
        LUnitBuffer: TNyxText;
        LIncludeBuffer: TNyxText;
        LBeforeRead: TNyxText;
        LBeforeAccepted: TNyxText;
        LReply: TNyxDataValue;
        LCatalog: TNyxDataValue;
        LBody: TNyxDataValue;
        LDeclaration: TNyxDataValue;
        LReadRevision: Integer;
        LEmojiOffset: Integer;
        LCanUndo: Boolean;
        LCanRedo: Boolean;

        function Read(const AMode: TNyxText; const AFile: TNyxPascalSourceRef;
          const AExtra: array of TNyxDataField): TNyxDataValue;
        var
          LFields: array of TNyxDataField;
          LField: Integer;
        begin
          SetLength(LFields, 4 + Length(AExtra));
          LFields[0] := NyxField('mode', NyxData(AMode));
          LFields[1] := NyxField('snapshot', NyxData('editing'));
          LFields[2] := NyxField('expectedRevision', NyxData(LReadRevision));
          LFields[3] := NyxField('file', Selector(AFile));
          for LField := 0 to High(AExtra) do
          begin
            LFields[4 + LField] := AExtra[LField];
          end;
          Result := Apply(NyxObject(LFields));
        end;

      begin
        LBeforeAccepted := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        LCanUndo := GStudio.Session.CanUndo;
        LCanRedo := GStudio.Session.CanRedo;
        LMainBuffer := CPrefix + GStudio.Session.Source;
        LUnitBuffer := CUnitPrefix + GStudio.Session.SourceFileText(LUnit);
        LIncludeBuffer := CPrefix + GStudio.Session.SourceFileText(LInclude) + #10 + CUnfinished;
        Pick(TNyxPascalSourceRef.Main);
        LMemo.Text := LMainBuffer;
        Settled;
        Pick(LUnit);
        LMemo.Text := LUnitBuffer;
        Settled;
        Pick(LUtility);
        LMemo.Text := CUnrelated;
        Settled;
        Pick(LInclude);
        LMemo.Text := LIncludeBuffer;
        Settled;
        Check((GStudio.Session.DraftSource = LMainBuffer) and
          (GStudio.Session.DraftFileText(LUnit) = LUnitBuffer) and
          (GStudio.Session.DraftFileText(LInclude) = LIncludeBuffer),
          'ordinary source input publishes exact supplementary buffers before semantic reads');
        LBeforeRead := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        LReadRevision := Revision;
        LReply := Apply(NyxObject([NyxField('mode', NyxData('files')),
          NyxField('snapshot', NyxData('editing')),
          NyxField('expectedRevision', NyxData(LReadRevision)), NyxField('limit', NyxData(2))]));
        Check((LReply.Field('snapshot').AsText = 'editing') and
          LReply.Field('inspectionOnly').AsBoolean and (LReply.Field('files').Count = 2) and
          (LReply.Field('total').AsInteger = GStudio.Session.SourceFileCount),
          'buffer inventory stays bounded and labels acknowledged inspection origin');
        LReply := Read('unit', TNyxPascalSourceRef.Main, [NyxField('count', NyxData(80))]);
        Check((Pos(CPrefix, LReply.Field('text').AsText) = 1) and
          (LReply.Field('total').AsInteger = NyxTextScalarCount(LMainBuffer)) and
          LReply.Field('pendingDraft').AsBoolean and LReply.Field('inspectionOnly').AsBoolean,
          'main windows read current text without parsing its application or exporting the whole unit');
        LReply := Read('unit', LUnit, [NyxField('count', NyxData(80))]);
        Check((Pos(CUnitPrefix, LReply.Field('text').AsText) = 1) and
          (LReply.Field('file').ToJSON = Selector(LUnit).ToJSON),
          'current helper windows preserve captured file identity and shifted Unicode text');
        LReply := Read('unit', LUtility, [NyxField('count', NyxData(4096))]);
        Check(LReply.Field('text').AsText = CUnrelated,
          'incomplete unrelated buffers remain readable independently of lexical discovery');
        LEmojiOffset := NyxTextScalarCount(Copy(CPrefix, 1, Pos(CEmoji, CPrefix) - 1));
        LReply := Read('unit', LInclude, [NyxField('offset', NyxData(LEmojiOffset)),
          NyxField('count', NyxData(1))]);
        Check((LReply.Field('text').AsText = CEmoji) and
          (LReply.Field('nextOffset').AsInteger = LEmojiOffset + 1),
          'one-scalar buffer windows never split a supplementary character');
        LReply := Read('unit', LInclude, [NyxField('line', NyxData(3)),
          NyxField('count', NyxData(48))]);
        Check((Pos('function ProjectPlan', LReply.Field('text').AsText) = 1) and
          (LReply.Field('line').AsInteger = 3), 'buffer line windows use current physical coordinates');
        LReply := Apply(NyxObject([NyxField('mode', NyxData('unit')),
          NyxField('expectedRevision', NyxData(LReadRevision)),
          NyxField('file', Selector(LInclude)), NyxField('count', NyxData(48))]));
        Check((Pos('function ProjectPlan', LReply.Field('text').AsText) = 1) and
          (Pos('"snapshot"', LReply.ToJSON) = 0),
          'omitted snapshot retains accepted source and the legacy successful wire shape');
        LCatalog := Read('routines', LInclude, [NyxField('owner', Selector(LUnit)),
          NyxField('limit', NyxData(1))]);
        Check((LCatalog.Field('total').AsInteger = 1) and (LCatalog.Field('routines').Count = 1) and
          not LCatalog.Field('routines').Item(0).Field('editable').AsBoolean and
          (LCatalog.Field('routines').Item(0).Field('definition').Field('line').AsInteger = 3),
          'editing routine discovery retains only the complete prefix and grants no mutation capability');
        Check(LCatalog.Field('problem').Field('defined').AsBoolean and
          (LCatalog.Field('problem').Field('location').Field('file').ToJSON = Selector(LInclude).ToJSON),
          'semantic partial-source problems preserve exact physical include ownership');
        LBody := Read('routine', LInclude, [NyxField('owner', Selector(LUnit)),
          NyxField('routine', NyxData('ProjectPlan')), NyxField('count', NyxData(4096))]);
        Check((Pos(TNyxCodegen.TextLiteral(CInitial), LBody.Field('text').AsText) > 0) and
          not LBody.Field('editable').AsBoolean and LBody.Field('inspectionOnly').AsBoolean and
          (LBody.Field('definition').Field('line').AsInteger = 3),
          'body windows come from the captured current source rather than an accepted-position rebase');
        LDeclaration := Read('declaration', LInclude, [NyxField('owner', Selector(LUnit)),
          NyxField('routine', NyxData('ProjectPlan')), NyxField('count', NyxData(4096))]);
        Check((LDeclaration.Field('textSource').Field('file').ToJSON = Selector(LUnit).ToJSON) and
          LDeclaration.Field('pendingDraft').AsBoolean and
          (Pos('function ProjectPlan', LDeclaration.Field('text').AsText) = 1),
          'current declaration windows preserve the independent unfinished helper counterpart');
        Check((EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeRead) and
          (Revision = LReadRevision) and GStudio.Session.ActiveSourceFile.SameFile(LInclude) and
          (GStudio.CodeView.InputFor('studio-code') = LMemo) and
          (GStudio.Session.CanUndo = LCanUndo) and (GStudio.Session.CanRedo = LCanRedo),
          'semantic inspection changes no pair, revision, selection, history or physical source editor');
        RefuseRead(NyxObject([NyxField('mode', NyxData('unit')),
          NyxField('snapshot', NyxData('editing')), NyxField('file', Selector(LInclude))]),
          'editing text requires an explicit exact revision');
        RefuseRead(NyxObject([NyxField('mode', NyxData('routines')),
          NyxField('snapshot', NyxData('guess')), NyxField('expectedRevision', NyxData(LReadRevision))]),
          'unknown inspection origins cannot fall back to accepted code');
        RefuseRead(NyxObject([NyxField('mode', NyxData('unit')),
          NyxField('snapshot', NyxData('editing')), NyxField('expectedRevision', NyxData(LReadRevision)),
          NyxField('file', Selector(LInclude)), NyxField('line', NyxData(1)),
          NyxField('offset', NyxData(0))]), 'current text preserves the exclusive scalar/line selector');
        LMemo.Text := CPrefix + LIncludeBuffer;
        Settled;
        Check(Revision <> LReadRevision, 'acknowledged newer typing advances the semantic revision');
        RefuseRead(NyxObject([NyxField('mode', NyxData('routine')),
          NyxField('snapshot', NyxData('editing')), NyxField('expectedRevision', NyxData(LReadRevision)),
          NyxField('file', Selector(LInclude)), NyxField('owner', Selector(LUnit)),
          NyxField('routine', NyxData('ProjectPlan'))]), 'stale buffer pages cannot read a newer editing state');
        LReadRevision := Revision;
        LReply := Read('routines', LInclude, [NyxField('owner', Selector(LUnit))]);
        Check(LReply.Field('routines').Item(0).Field('definition').Field('line').AsInteger = 5,
          'revision-owned routine caches retire after current-buffer publication');
        Pick(TNyxPascalSourceRef.Main);
        Click('action-reset-source');
        Settled;
        Pick(LUnit);
        Click('action-reset-source');
        Settled;
        Pick(LUtility);
        Click('action-reset-source');
        Settled;
        Pick(LInclude);
        Click('action-reset-source');
        Settled;
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeAccepted,
          'ordinary scoped Restore returns the exact owned accepted pair after buffer reads');
        RefuseRead(NyxObject([NyxField('mode', NyxData('edit-unit')),
          NyxField('snapshot', NyxData('editing')), NyxField('expectedRevision', NyxData(Revision)),
          NyxField('operationId', NyxData('snapshot-is-read-only')),
          NyxField('changes', NyxArray([NyxObject([NyxField('offset', NyxData(0)),
            NyxField('expected', NyxData('')), NyxField('replacement', NyxData('{ A proposed comment }' + #10))])]))]),
          'read-only snapshot selectors cannot enter compiler-aware source mutations');
        WriteLn('SEMANTIC BUFFERS ', LCase, ' passed');
        Flush(Output);
      end;

      { Inventory changes use production wire operations and the same observing
        editor. New helpers actually supply the constructed plan, so captured
        metadata alone cannot satisfy this journey. All fixtures are owned. }
      procedure MembershipFiles;
      const
        CPlan: TNyxText = 'A semantic helper plan 🚀 𐐷';
        CUnitText: TNyxText = 'unit semantic.extra;' + #10 +
          '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
          'interface' + #10 + 'uses nyx.text;' + #10 +
          'function ExtraPlan: TNyxText;' + #10 + 'implementation' + #10 +
          'function ExtraPlan: TNyxText;' + #10 + 'begin' + #10 +
          '  Result := TNyxText(''A semantic helper plan 🚀 𐐷'');' + #10 + 'end;' + #10 + 'end.' + #10;
        CIncludeText: TNyxText = 'function LocalExtraPlan: TNyxText;' + #10 +
          'begin' + #10 + '  Result := ExtraPlan;' + #10 + 'end;' + #10;
        CImplementation: TNyxText = 'implementation' + #10 +
          'uses semantic.extra;' + #10 + '{$I semantic-extra.inc}';
      var
        LNewUnit: TNyxPascalSourceRef;
        LNewInclude: TNyxPascalSourceRef;
        LBeforeMembership: TNyxText;
        LAfterMembership: TNyxText;
        LCreate: TNyxDataValue;
        LRemove: TNyxDataValue;
        LRelated: TNyxDataValue;
        LReview: TNyxDataValue;
        LApply: TNyxDataValue;
        LReply: TNyxDataValue;
        LJob: TNyxDataValue;
        LTicket: TNyxText;
        LRejected: Boolean;
        LFileCount: Integer;

        function Member(const AOperation: TNyxText;
          const AFile: TNyxPascalSourceRef; const AText: TNyxText = ''): TNyxDataValue;
        begin

          if AOperation = 'create' then
          begin
            Exit(NyxObject([NyxField('op', NyxData(AOperation)),
              NyxField('file', Selector(AFile)), NyxField('text', NyxData(AText))]));
          end;
          Result := NyxObject([NyxField('op', NyxData(AOperation)),
            NyxField('file', Selector(AFile))]);
        end;

        function Group(const AMode, AOperation: TNyxText;
          const AChanges, AEdits: TNyxDataValue; const ATicket: TNyxText = ''): TNyxDataValue;
        var
          LFields: array of TNyxDataField;
        begin
          SetLength(LFields, 4);
          LFields[0] := NyxField('mode', NyxData(AMode));
          LFields[1] := NyxField('expectedRevision', NyxData(Revision));
          LFields[2] := NyxField('changes', AChanges);
          LFields[3] := NyxField('edits', AEdits);

          if AMode = 'edit-files' then
          begin
            SetLength(LFields, 5);
            LFields[4] := NyxField('operationId', NyxData(AOperation));
          end;

          if ATicket <> '' then
          begin
            SetLength(LFields, Length(LFields) + 1);
            LFields[High(LFields)] := NyxField('reviewID', NyxData(ATicket));
          end;
          Result := NyxObject(LFields);
        end;

      begin
        LNewUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('semantic.extra'));
        LNewInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('semantic-extra.inc'));
        LBeforeMembership := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        LFileCount := GStudio.Session.SourceFileCount;
        LCreate := NyxArray([Member('create', LNewUnit, CUnitText),
          Member('create', LNewInclude, CIncludeText)]);
        LRelated := NyxArray([
          Change(LUnit, GStudio.Session.SourceFileText(LUnit), 'implementation', CImplementation),
          Change(LInclude, GStudio.Session.SourceFileText(LInclude),
            TNyxCodegen.TextLiteral(CInitial), 'LocalExtraPlan')]);
        RefuseRead(Group('edit-files', 'membership-case-alias', NyxArray([
          Member('create', TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('PLAN-values.inc')), '')]),
          LRelated), 'membership refuses case aliases without publishing any paired input');
        LReply := Apply(Group('edit-files', 'membership-compiler-refusal', NyxArray([
          Member('create', LNewUnit, CUnitText), Member('create', LNewInclude,
            'function LocalExtraPlan: TNyxText;' + #10 + 'begin' + #10 +
            '  Result := MissingNewHelper;' + #10 + 'end;' + #10)]), LRelated));
        LJob := Await(LReply.Field('job').AsText);
        Check((LJob.Field('publication').Field('state').AsText = 'refused') and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeMembership) and
          (GStudio.Session.SourceFileCount = LFileCount),
          'new helper compiler failure retains every old input and admits no partial membership');
        LApply := Group('edit-files', 'membership-create', LCreate, LRelated);
        LReply := Apply(LApply);
        Check((EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeMembership) and
          (GStudio.Session.SourceFileCount = LFileCount) and (Apply(LApply).ToJSON = LReply.ToJSON),
          'grouped semantic creation retains visible inputs and returns one exact retry receipt');
        LJob := Await(LReply.Field('job').AsText);
        Check((LJob.Field('publication').Field('state').AsText = 'committed') and
          (GStudio.Session.SourceFileCount = LFileCount + 2) and
          (GStudio.Session.SourceFileText(LNewUnit) = CUnitText) and
          (GStudio.Session.SourceFileText(LNewInclude) = CIncludeText) and
          (Pos(CPlan, GStudio.Session.Save) > 0),
          'new unit/include and related ranges construct their actual Unicode document meaning / ' +
          LJob.Field('error').AsText);
        Check(GStudio.Session.ActiveSourceFile.SameFile(LInclude) and
          (GStudio.CodeView.InputFor('studio-code') = LMemo) and
          (AdjustLineBreaks(LMemo.Text, tlbsLF) = AdjustLineBreaks(
            String(GStudio.Session.SourceFileText(LInclude)), tlbsLF)),
          'ordinary observer keeps its selected helper and real memo through file creation');
        LAfterMembership := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        Click('action-undo');
        Settled;
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeMembership,
          'one ordinary Undo removes both new helpers and restores dependency source/design exactly');
        Click('action-redo');
        Settled;
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfterMembership,
          'one ordinary Redo restores the entire constructed inventory');
        LRemove := NyxArray([Member('remove', LNewUnit), Member('remove', LNewInclude)]);
        LRelated := NyxArray([
          Change(LUnit, GStudio.Session.SourceFileText(LUnit), CImplementation, 'implementation'),
          Change(LInclude, GStudio.Session.SourceFileText(LInclude), 'LocalExtraPlan',
            TNyxCodegen.TextLiteral(CInitial))]);
        RefuseRead(Group('edit-files', 'membership-no-review', LRemove, LRelated),
          'semantic helper removal requires an explicit current warning ticket');
        LReview := Apply(Group('review-files', '', LRemove, LRelated));
        LTicket := LReview.Field('reviewID').AsText;
        Check((LReview.Field('removals').Count = 2) and
          (Pos('removed', LReview.Field('warning').AsText) > 0) and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfterMembership),
          'bounded review names exact removals without compiling, mutation or history');
        LApply := Group('edit-files', 'membership-remove', LRemove, LRelated, LTicket);
        LRejected := False;
        try
          GSharedEngine.InvokeTool('nyx_pascal', 'a-different-owner', CActor, LApply);
        except
          on Exception do
          begin
            LRejected := True;
          end;
        end;
        Check(LRejected and (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfterMembership),
          'same display name cannot borrow another transport removal review');
        RefuseRead(Group('edit-files', 'membership-changed-group',
          NyxArray([Member('remove', LNewUnit)]), LRelated, LTicket),
          'review authority requires the exact unchanged membership/range group');
        LMemo.Text := CUnitDraft;
        Settled;
        RefuseRead(LApply, 'unfinished ordinary typing refuses membership without losing any draft text');
        Click('action-reset-source');
        Settled;
        { A fresh review follows any revision changed by ordinary draft restore. }
        LReview := Apply(Group('review-files', '', LRemove, LRelated));
        LTicket := LReview.Field('reviewID').AsText;
        LApply := Group('edit-files', 'membership-remove', LRemove, LRelated, LTicket);
        LReply := Apply(LApply);
        LJob := Await(LReply.Field('job').AsText);
        Check((LJob.Field('publication').Field('state').AsText = 'committed') and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeMembership),
          'reviewed removal and dependency repair publish the exact original pair in one constructor / ' +
          LJob.Field('error').AsText);
        Click('action-undo');
        Settled;
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfterMembership,
          'one ordinary Undo restores removed files and all authored source exactly');
        Click('action-redo');
        Settled;
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeMembership,
          'one ordinary Redo reapplies the complete reviewed removal');
        RefuseRead(Group('edit-files', 'membership-expired-review', LRemove, LRelated, LTicket),
          'completed history/revision changes revoke earlier removal reviews');
        LRelated := NyxArray([
          Change(LUnit, GStudio.Session.SourceFileText(LUnit), 'implementation', CImplementation),
          Change(LInclude, GStudio.Session.SourceFileText(LInclude),
            TNyxCodegen.TextLiteral(CInitial), 'LocalExtraPlan')]);
        LReply := Apply(Group('edit-files', 'membership-late-draft', LCreate, LRelated));
        LMemo.Text := CUnitDraft;
        Settled;
        LAfterMembership := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        LJob := Await(LReply.Field('job').AsText);
        Check((LJob.Field('publication').Field('state').AsText = 'refused') and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfterMembership) and
          (GStudio.Session.SourceFileCount = LFileCount) and (LMemo.Text = CUnitDraft),
          'late ordinary typing revokes complete membership publication and preserves its exact buffer');
        Click('action-reset-source');
        Settled;
        WriteLn('SEMANTIC MEMBERSHIP ', LCase, ' passed');
        Flush(Output);
      end;

      { Existing production transport/pool and ordinary mounted controls qualify
        file-aware routine context. Trusted initial seeding remains distinct from
        semantic edits; every application/helper proposal below uses public tools. }
      procedure ProjectRoutines;
      var
        LCatalog: TNyxDataValue;
        LHelper: TNyxDataValue;
        LMain: TNyxDataValue;
        LConvention: TNyxDataValue;
        LDefault: TNyxDataValue;
        LDefaultFound: Boolean;
        LDefaultScan: Integer;
        LDeclaration: TNyxDataValue;
        LArgs: TNyxDataValue;
        LReply: TNyxDataValue;
        LJob: TNyxDataValue;
        LBeforeRoutines: TNyxText;
        LAfterRoutines: TNyxText;
        LHelperBody: TNyxText;
        LMainBody: TNyxText;
        LConventionBody: TNyxText;
        LNewBody: TNyxText;
        LStableMain: TNyxText;

        function ReadBody(const AFile, AOwner: TNyxPascalSourceRef;
          const AName: TNyxText): TNyxDataValue;
        begin
          Result := Apply(NyxObject([NyxField('mode', NyxData('routine')),
            NyxField('expectedRevision', NyxData(Revision)), NyxField('file', Selector(AFile)),
            NyxField('owner', Selector(AOwner)), NyxField('routine', NyxData(AName)),
            NyxField('count', NyxData(4096))]));
          Check(Result.Field('nextOffset').AsInteger = Result.Field('total').AsInteger,
            'owned routine fixture fits its bounded exact source window');
        end;

        function BodyEdit(const AFile, AOwner: TNyxPascalSourceRef;
          const AName, AExpected, ACode: TNyxText): TNyxDataValue;
        begin
          Result := NyxObject([NyxField('file', Selector(AFile)), NyxField('owner', Selector(AOwner)),
            NyxField('routine', NyxData(AName)), NyxField('expected', NyxData(AExpected)),
            NyxField('implementation', NyxData(ACode))]);
        end;

        function BodyArguments(const AOperation: TNyxText;
          const AEdits: array of TNyxDataValue): TNyxDataValue;
        begin
          Result := NyxObject([NyxField('mode', NyxData('edit-routines')),
            NyxField('expectedRevision', NyxData(Revision)),
            NyxField('operationId', NyxData(AOperation)), NyxField('changes', NyxArray(AEdits))]);
        end;

      begin
        WriteLn('PROJECT ROUTINES ', LCase, ' starting');
        Flush(Output);
        Check((NyxStudioMCPTools.Field('tools').Count = 24) and
          (Pos('physical Unicode locations', NyxStudioMCPTools.ToJSON) > 0),
          'source schema extends existing tools with explicit routine owning context');
        Pick(LInclude);
        LBeforeRoutines := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        LStableMain := GStudio.Session.Source;
        LDefault := Apply(NyxObject([NyxField('mode', NyxData('routines')),
          NyxField('expectedRevision', NyxData(Revision)), NyxField('limit', NyxData(50))]));
        LDefaultFound := False;
        for LDefaultScan := 0 to LDefault.Field('routines').Count - 1 do
        begin

          if LDefault.Field('routines').Item(LDefaultScan).Field('routine').AsText = 'ReadProjectPlan' then
          begin
            LDefaultFound := (LDefault.Field('routines').Item(LDefaultScan).Field('line').AsInteger = 0) and
              (LDefault.Field('routines').Item(LDefaultScan).Field('definition').Field('file').ToJSON =
                Selector(LUtility).ToJSON);
          end;
        end;
        Check(LDefaultFound and (LDefault.Field('owner').ToJSON = Selector(TNyxPascalSourceRef.Main).ToJSON),
          'default main discovery uses captured includes and never labels their lines as main-unit navigation');
        LDefault := Apply(NyxObject([NyxField('mode', NyxData('routine')),
          NyxField('expectedRevision', NyxData(Revision)), NyxField('routine', NyxData('ReadProjectPlan'))]));
        Check((LDefault.Field('line').AsInteger = 0) and
          (LDefault.Field('textSource').Field('file').ToJSON = Selector(LUtility).ToJSON) and
          (Pos('GProjectConstructions', LDefault.Field('text').AsText) > 0),
          'default routine windows retain the main owning context and physical include body');
        LDefault := Apply(NyxObject([NyxField('mode', NyxData('declaration')),
          NyxField('expectedRevision', NyxData(Revision)), NyxField('routine', NyxData('ReadProjectPlan')),
          NyxField('part', NyxData('implementation'))]));
        Check((LDefault.Field('line').AsInteger = 0) and
          (LDefault.Field('textSource').Field('file').ToJSON = Selector(LUtility).ToJSON) and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeRoutines),
          'default counterpart queries are file-aware without changing accepted source, buffers or selection');
        LCatalog := Apply(NyxObject([NyxField('mode', NyxData('routines')),
          NyxField('expectedRevision', NyxData(Revision)), NyxField('file', Selector(LUnit)),
          NyxField('limit', NyxData(1))]));
        Check((LCatalog.Field('total').AsInteger = 4) and
          (LCatalog.Field('routines').Count = 1) and
          (LCatalog.Field('routines').Item(0).Field('routine').AsText = 'ProjectPlan') and
          (LCatalog.Field('routines').Item(0).Field('definition').Field('file').ToJSON = Selector(LInclude).ToJSON),
          'bounded helper catalog locates the actual include-owned definition');
        LCatalog := Apply(NyxObject([NyxField('mode', NyxData('routines')),
          NyxField('expectedRevision', NyxData(Revision)), NyxField('file', Selector(LUnit)),
          NyxField('offset', NyxData(1)), NyxField('limit', NyxData(2))]));
        Check((LCatalog.Field('routines').Count = 2) and
          not LCatalog.Field('routines').Item(0).Field('editable').AsBoolean and
          not LCatalog.Field('routines').Item(1).Field('editable').AsBoolean,
          'overloads remain distinct discovery rows with focused-edit refusal');
        LDeclaration := Apply(NyxObject([NyxField('mode', NyxData('routine')),
          NyxField('expectedRevision', NyxData(Revision)), NyxField('file', Selector(LUnit)),
          NyxField('routine', NyxData('OverloadedPlan')),
          NyxField('ordinal', LCatalog.Field('routines').Item(0).Field('ordinal'))]));
        Check((Pos('Result := AValue', LDeclaration.Field('text').AsText) > 0) and
          not LDeclaration.Field('editable').AsBoolean,
          'revision-pinned ordinal reads an exact overload implementation without granting mutation');
        RefuseRead(BodyArguments('project-routines-overload', [
          BodyEdit(LUnit, LUnit, 'OverloadedPlan', LDeclaration.Field('text').AsText,
            #10 + 'begin' + #10 + '  Result := 3;' + #10 + 'end;')]),
          'overloaded identities cannot become implicit focused mutation targets');
        LHelper := ReadBody(LInclude, LUnit, 'ProjectPlan');
        LHelperBody := LHelper.Field('text').AsText;
        Check(LHelper.Field('textSource').Field('defined').AsBoolean and
          (LHelper.Field('textSource').Field('file').ToJSON = Selector(LInclude).ToJSON) and
          LHelper.Field('editable').AsBoolean and (Pos(CInitial, LHelperBody) > 0),
          'body inspection returns exact accepted physical helper text and editability');
        LDeclaration := Apply(NyxObject([NyxField('mode', NyxData('declaration')),
          NyxField('expectedRevision', NyxData(Revision)), NyxField('file', Selector(LInclude)),
          NyxField('owner', Selector(LUnit)), NyxField('routine', NyxData('ProjectPlan'))]));
        Check((LDeclaration.Field('visibility').AsText = 'interface') and
          (LDeclaration.Field('textSource').Field('file').ToJSON = Selector(LUnit).ToJSON) and
          (LDeclaration.Field('text').AsText = 'function ProjectPlan: TNyxText;'),
          'counterpart window names its own helper-unit location rather than the body include');
        LMain := ReadBody(LUtility, TNyxPascalSourceRef.Main, 'ReadProjectPlan');
        LMainBody := LMain.Field('text').AsText;
        LConvention := ReadBody(LUnit, LUnit, 'ConventionPlan');
        LConventionBody := LConvention.Field('text').AsText;
        Check((Pos('cdecl;', LConvention.Field('signature').AsText) > 0) and
          (Pos('cdecl', LConventionBody) = 0) and LConvention.Field('editable').AsBoolean,
          'calling convention is immutable declaration context outside the focused body');
        RefuseRead(BodyArguments('project-routines-convention-escape', [
          BodyEdit(LUnit, LUnit, 'ConventionPlan', LConventionBody,
            #10 + 'stdcall;' + #10 + 'begin Result := 9; end;')]),
          'a semantic body replacement cannot alter its calling convention');
        Check((LMain.Field('textSource').Field('file').ToJSON = Selector(LUtility).ToJSON) and
          (Pos('GProjectConstructions', LMainBody) > 0),
          'main-unit context also discovers its physical included helper without losing managed guards');
        RefuseRead(NyxObject([NyxField('mode', NyxData('routines')),
          NyxField('file', Selector(LInclude))]), 'include browsing never guesses an owning translation unit');
        RefuseRead(NyxObject([NyxField('mode', NyxData('routine')),
          NyxField('file', Selector(LInclude)), NyxField('owner', Selector(TNyxPascalSourceRef.Main)),
          NyxField('routine', NyxData('ProjectPlan'))]),
          'a shared fragment selector cannot borrow an unrelated owning routine');
        RefuseRead(NyxObject([NyxField('mode', NyxData('routines')),
          NyxField('expectedRevision', NyxData(Revision - 1)), NyxField('file', Selector(LUnit))]),
          'stale routine catalogs refuse without altering the complete project');
        RefuseRead(NyxObject([NyxField('mode', NyxData('routine')),
          NyxField('file', Selector(LUnit)), NyxField('routine', NyxData('ProjectPlan')),
          NyxField('ordinal', NyxData(0))]), 'routine ordinals require their exact revision pin');
        LNewBody := #10 + 'begin' + #10#10 + '  if ConventionPlan = 9 then' + #10 +
          '  begin' + #10 + '    Result := TNyxText(' + TNyxCodegen.TextLiteral(CChanged) + ');' + #10 +
          '  end' + #10 + '  else' + #10 + '  begin' + #10 +
          '    Result := ''A stale convention helper'';' + #10 + '  end;' + #10 + 'end;';
        LArgs := BodyArguments('project-routines-group', [
          BodyEdit(LUtility, TNyxPascalSourceRef.Main, 'ReadProjectPlan', LMainBody,
            #10 + '{ An authored main helper note. }' + LMainBody),
          BodyEdit(LUnit, LUnit, 'ConventionPlan', LConventionBody,
            #10 + 'begin' + #10 + '  Result := 9;' + #10 + 'end;'),
          BodyEdit(LInclude, LUnit, 'ProjectPlan', LHelperBody, LNewBody)]);
        LReply := Apply(LArgs);
        Check((LReply.Field('publication').Field('state').AsText = 'pending') and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeRoutines) and
          (Apply(LArgs).ToJSON = LReply.ToJSON),
          'one named routine group stages no visible text and retains its exact retry receipt');
        LJob := Await(LReply.Field('job').AsText);
        Check((LJob.Field('publication').Field('state').AsText = 'committed') and
          (GStudio.Session.Source = LStableMain) and (Pos(CChanged, GStudio.Session.Save) > 0) and
          (Pos('authored main helper note', GStudio.Session.SourceFileText(LUtility)) > 0),
          'main/helper include bodies construct actual Unicode meaning together with unchanged main text / ' +
          LJob.Field('error').AsText);
        Check(GStudio.Session.ActiveSourceFile.SameFile(LInclude) and
          (GStudio.CodeView.InputFor('studio-code') = LMemo) and
          (AdjustLineBreaks(LMemo.Text, tlbsLF) = AdjustLineBreaks(
            String(GStudio.Session.SourceFileText(LInclude)), tlbsLF)),
          'ordinary observer retains selection and its real memo while both routine files update');
        LAfterRoutines := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        LDeclaration := ReadBody(LUnit, LUnit, 'ConventionPlan');
        Check((LDeclaration.Field('signature').AsText = LConvention.Field('signature').AsText) and
          (Pos('Result := 9', LDeclaration.Field('text').AsText) > 0),
          'compiled body replacement retains every calling-convention signature byte');
        LHelper := ReadBody(LInclude, LUnit, 'ProjectPlan');
        Check(LHelper.Field('text').AsText = LNewBody, 'accepted revision invalidates prior routine index windows');
        Click('action-undo');
        Settled;
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBeforeRoutines,
          'one ordinary Undo restores both routine bodies and exact design');
        LHelper := ReadBody(LInclude, LUnit, 'ProjectPlan');
        Check(LHelper.Field('text').AsText = LHelperBody, 'Undo invalidates cached accepted body coordinates/text');
        Click('action-redo');
        Settled;
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfterRoutines,
          'one ordinary Redo restores the entire compiler-authored routine pair');
        RefuseRead(BodyArguments('project-routines-mismatch', [
          BodyEdit(LInclude, LUnit, 'ProjectPlan', LHelperBody, LNewBody)]),
          'old exact helper expectations refuse without a compiler/publication side effect');
        LReply := Apply(BodyArguments('project-routines-compiler-failure', [
          BodyEdit(LInclude, LUnit, 'ProjectPlan', LNewBody,
            #10 + 'begin' + #10 + '  Result := MissingRoutineValue;' + #10 + 'end;')]));
        LJob := Await(LReply.Field('job').AsText);
        Check((LJob.Field('publication').Field('state').AsText = 'refused') and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfterRoutines) and
          (Pos('submittedSource', LJob.Field('diagnostics').ToJSON) > 0),
          'real helper compiler failure retains the pair and physical proposed-file diagnostics');
        LReply := Apply(BodyArguments('project-routines-late-draft', [
          BodyEdit(LInclude, LUnit, 'ProjectPlan', LNewBody,
            #10 + 'begin' + #10 + '  Result := TNyxText(' +
              TNyxCodegen.TextLiteral(CInitial) + ');' + #10 + 'end;')]));
        LMemo.Text := CUnitDraft;
        Settled;
        LAfterRoutines := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        LJob := Await(LReply.Field('job').AsText);
        Check((LJob.Field('publication').Field('state').AsText = 'refused') and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfterRoutines) and
          (LMemo.Text = CUnitDraft), 'newer helper typing revokes focused routine publication exactly');
        RefuseRead(BodyArguments('project-routines-pending', [
          BodyEdit(LInclude, LUnit, 'ProjectPlan', LNewBody, LHelperBody)]),
          'pending ordinary drafts refuse focused routine mutation at its current revision');
        LHelper := ReadBody(LInclude, LUnit, 'ProjectPlan');
        Check(LHelper.Field('pendingDraft').AsBoolean and (LHelper.Field('text').AsText = LNewBody),
          'accepted body inspection reports pending text without exporting that unfinished buffer');
        Click('action-reset-source');
        Settled;
        WriteLn('PROJECT ROUTINES ', LCase, ' passed');
        Flush(Output);
      end;

    begin

      if AJourney = sfjProjectRoutines then
      begin
        BufferContext;
        ProjectRoutines;
        Exit;
      end;
      WriteLn('SEMANTIC FILES ', LCase, ' starting');
      Flush(Output);
      LFiles := NyxStudioMCPTools.Field('tools');
      Check(LFiles.Count = 24, 'source-file semantics extend the existing 24-tool inventory');
      LFound := False;
      for LIndex := 0 to LFiles.Count - 1 do
      begin

        if LFiles.Item(LIndex).Field('name').AsText = 'nyx_pascal' then
        begin
          LItems := LFiles.Item(LIndex).Field('inputSchema').Field('oneOf');
          LFound := Pos('submittedSource', LFiles.Item(LIndex).Field('description').AsText) > 0;
          Break;
        end;
      end;
      Check(LFound and (LItems.Count > 0), 'ordinary tool discovery parses the extended bounded Pascal schema');
      Pick(LInclude);
      LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      LMainText := GStudio.Session.Source;
      LUnitText := GStudio.Session.SourceFileText(LUnit);
      LIncludeText := GStudio.Session.SourceFileText(LInclude);
      LFiles := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor, NyxObject([
        NyxField('mode', NyxData('files')), NyxField('expectedRevision', NyxData(Revision)),
        NyxField('limit', NyxData(1))]));
      Check((LFiles.Field('files').Count = 1) and
        (LFiles.Field('files').Item(0).Field('file').Field('kind').AsText = 'main') and
        (LFiles.Field('total').AsInteger = GStudio.Session.SourceFileCount) and
        (Pos(LIncludeText, LFiles.ToJSON) = 0),
        'semantic inventory is bounded metadata in authored order, not project/source export');
      LFiles := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor, NyxObject([
        NyxField('mode', NyxData('files')), NyxField('expectedRevision', NyxData(Revision)),
        NyxField('offset', NyxData(1)), NyxField('limit', NyxData(50))]));
      Check((LFiles.Field('nextOffset').AsInteger = GStudio.Session.SourceFileCount) and
        (LFiles.Field('files').Count = GStudio.Session.SourceFileCount - 1),
        'file discovery pages the remaining exact helper identities');
      RefuseRead(NyxObject([NyxField('mode', NyxData('files')),
        NyxField('expectedRevision', NyxData(Revision - 1))]),
        'stale inventory revisions refuse without moving selection or changing source');
      RefuseRead(NyxObject([NyxField('mode', NyxData('unit')),
        NyxField('file', NyxObject([NyxField('kind', NyxData('include')),
          NyxField('name', NyxData('../outside.inc'))]))]),
        'public source selectors refuse host paths');
      RefuseRead(NyxObject([NyxField('mode', NyxData('unit')),
        NyxField('file', NyxObject([NyxField('kind', NyxData('unit')),
          NyxField('name', NyxData('PROJECT.values.pas'))]))]),
        'public source selectors require the exact accepted case identity');
      RefuseRead(NyxObject([NyxField('mode', NyxData('unit')),
        NyxField('file', Selector(LInclude)), NyxField('line', NyxData(1000000))]),
        'helper line navigation refuses a location outside its accepted file');
      LWindow := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor, NyxObject([
        NyxField('mode', NyxData('unit')), NyxField('expectedRevision', NyxData(Revision)),
        NyxField('file', Selector(LInclude)), NyxField('line', NyxData(3)),
        NyxField('count', NyxData(80))]));
      Check((LWindow.Field('line').AsInteger = 3) and
        (Pos('Result :=', LWindow.Field('text').AsText) > 0),
        'one revision-pinned helper line query maps directly to its bounded scalar window');
      LArguments := Arguments('semantic-files-group', [
        Change(LUnit, LUnitText, '', #10 + '{ A retained semantic helper note. }', True),
        Change(LInclude, LIncludeText, TNyxCodegen.TextLiteral(CInitial),
          TNyxCodegen.TextLiteral(CChanged)),
        Change(TNyxPascalSourceRef.Main, LMainText, '',
          #10 + '{ A grouped main note. }', True),
        Change(LInclude, LIncludeText, '', #10 + '{ A final include note. }', True)]);
      LReceipt := Apply(LArguments);
      Check((LReceipt.Field('publication').Field('state').AsText = 'pending') and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore),
        'one queued semantic group keeps all proposed text off the observing buffers');
      Check(Apply(LArguments).ToJSON = LReceipt.ToJSON,
        'exact compiler retries retain the original queue receipt and one job');
      LStatus := Await(LReceipt.Field('job').AsText);
      Check(LStatus.Field('publication').Field('state').AsText = 'committed',
        'actual complete-project construction publishes the group / ' + LStatus.Field('error').AsText);
      Check((GStudio.Session.Source <> LMainText) and
        (GStudio.Session.SourceFileText(LUnit) <> LUnitText) and
        (Pos(CChanged, GStudio.Session.SourceFileText(LInclude)) > 0) and
        (GStudio.Session.Save <> LPair.Design) and
        GStudio.Session.ActiveSourceFile.SameFile(LInclude) and
        (AdjustLineBreaks(LMemo.Text, tlbsLF) = AdjustLineBreaks(
          String(GStudio.Session.SourceFileText(LInclude)), tlbsLF)) and
        (GStudio.CodeView.Root.Prop('value') = GStudio.Session.SourceFileText(LInclude)) and
        (GStudio.CodeView.InputFor('studio-code') = LMemo),
        'ordinary observing Studio retains selection/editor and constructed meaning across all changed files');
      LAfter := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Click('action-undo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
        'one ordinary Undo restores the complete original main/unit/include pair');
      Click('action-redo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter,
        'one ordinary Redo restores the exact constructed multi-file source');
      LRejected := False;
      try
        Apply(Arguments('semantic-files-old-expectation', [
          NyxObject([NyxField('file', Selector(LInclude)),
            NyxField('offset', NyxData(NyxTextScalarCount(Copy(LIncludeText, 1,
              Pos(TNyxCodegen.TextLiteral(CInitial), LIncludeText) - 1)))),
            NyxField('expected', NyxData(TNyxCodegen.TextLiteral(CInitial))),
            NyxField('replacement', NyxData('''Refused'''))])]));
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter),
        'mismatched accepted helper expectations refuse the complete semantic group');
      LIncludeText := GStudio.Session.SourceFileText(LInclude);
      LReceipt := Apply(Arguments('semantic-files-compiler-refusal', [
        Change(LInclude, LIncludeText, TNyxCodegen.TextLiteral(CChanged), CBadValue)]));
      LStatus := Await(LReceipt.Field('job').AsText);
      Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter),
        'real helper compiler failure retains all accepted inputs and paired history');
      LItems := LStatus.Field('diagnostics').Field('items');
      LFound := False;
      for LIndex := 0 to LItems.Count - 1 do
      begin
        LItem := LItems.Item(LIndex);
        LWindow := NyxNull;
        for LMember := 0 to LItem.Count - 1 do
        begin

          if LItem.Key(LMember) = 'submittedSource' then
          begin
            LWindow := LItem.Field('submittedSource');
          end;
        end;

        if (LWindow.Kind = ndObject) and
          (LWindow.Field('file').Field('kind').AsText = 'include') and
          (LWindow.Field('file').Field('name').AsText = LInclude.Name) then
        begin
          LFound := (LWindow.Field('line').AsInteger > 0) and
            (LWindow.Field('column').AsInteger > 0) and
            not LItem.Field('navigable').AsBoolean and (LItem.Field('line').AsInteger = 0);
          Break;
        end;
      end;
      Check(LFound, 'bounded helper diagnostics name proposed Unicode coordinates without main navigation / ' +
        LStatus.Field('diagnostics').ToJSON);
      LMemo.Text := CMainDraft;
      Settled;
      LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      LRejected := False;
      try
        Apply(Arguments('semantic-files-pending-refusal', [
          Change(LUnit, GStudio.Session.SourceFileText(LUnit), '', #10 + '{ Refused. }', True)]));
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore),
        'an independent unfinished helper buffer refuses grouped semantic source changes');
      LFiles := GSharedEngine.InvokeTool('nyx_pascal', COwner, CActor, NyxObject([
        NyxField('mode', NyxData('files')), NyxField('expectedRevision', NyxData(Revision))]));
      Check(LFiles.Field('pendingDrafts').AsBoolean,
        'bounded file discovery visibly reports independent pending source work');
      Click('action-reset-source');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter,
        'ordinary scoped Restore consumes only the owned qualification draft');
      LHelperOnlyMain := GStudio.Session.Source;
      LReceipt := Apply(Arguments('semantic-files-helper-only', [
        Change(LInclude, LIncludeText, TNyxCodegen.TextLiteral(CChanged),
          TNyxCodegen.TextLiteral(CInitial))]));
      LStatus := Await(LReceipt.Field('job').AsText);
      Check((LStatus.Field('publication').Field('state').AsText = 'committed') and
        (GStudio.Session.Source = LHelperOnlyMain) and
        (AdjustLineBreaks(LMemo.Text, tlbsLF) = AdjustLineBreaks(
          String(GStudio.Session.SourceFileText(LInclude)), tlbsLF)) and
        (Pos(CInitial, GStudio.Session.SourceFileText(LInclude)) > 0),
        'a helper-only group constructs and publishes while main source remains unchanged');
      LIncludeText := GStudio.Session.SourceFileText(LInclude);
      LReceipt := Apply(Arguments('semantic-files-late-draft', [
        Change(LInclude, LIncludeText, TNyxCodegen.TextLiteral(CInitial),
          TNyxCodegen.TextLiteral(CChanged))]));
      LMemo.Text := CUnitDraft;
      Settled;
      LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      LStatus := Await(LReceipt.Field('job').AsText);
      Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore) and
        (LMemo.Text = CUnitDraft),
        'late ordinary helper typing revokes publication and retains its exact draft/base');
      Click('action-reset-source');
      Settled;
      MembershipFiles;
      WriteLn('SEMANTIC FILES ', LCase, ' passed');
      Flush(Output);
    end;

    { Ordinary public Files controls own the user journey. Source templates are
      project data until the existing compiler/publisher admits the candidate;
      unrelated drafts and exact paired history remain observable throughout. }
    procedure ManageFiles;
    var
      LNewUnit: TNyxPascalSourceRef;
      LNewInclude: TNyxPascalSourceRef;
      LNameInput: TEdit;
      LKindInput: TComboBox;
      LBefore: TNyxText;
      LAfter: TNyxText;
      LCount: Integer;
      LMessage: TNyxNode;
      LFeedback: TNyxText;

      procedure NameFile(const AName: TNyxText; AInclude: Boolean);
      begin
        Click('action-source-files-tab');
        Settled;
        LKindInput := TComboBox(GStudio.SourceView.InputFor('studio-source-files-kind'));
        Check(LKindInput <> nil, 'Files exposes its ordinary typed unit/include selector');
        LKindInput.ItemIndex := Ord(AInclude);
        LKindInput.OnChange(LKindInput);
        Settled;
        LNameInput := TEdit(GStudio.SourceView.InputFor('studio-source-files-name'));
        Check(LNameInput <> nil, 'Files exposes the ordinary new-name input');
        LNameInput.Text := AName;
        Settled;
      end;

      procedure ReviewRemoval(const AFile: TNyxPascalSourceRef);
      begin
        Pick(AFile);
        Click('action-source-files-tab');
        Click('studio-source-files-remove');
        Settled;
        LMessage := GStudio.SourceView.Root.Find('studio-source-files-warning');
        Check((LMessage <> nil) and (Pos(AFile.Name, LMessage.Prop('text')) > 0),
          'visible removal warning names the exact captured source file');
      end;

    begin
      WriteLn('SOURCE MANAGEMENT ', LCase, ' starting');
      Flush(Output);
      LNewUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('project.toolbox'));
      LNewInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('toolbox-notes.inc'));
      LCount := GStudio.Session.SourceFileCount;
      LMemo.Text := CMainDraft;
      Settled;
      Pick(LUtility);
      LMemo.Text := '{ An independent file-management idea 🚀';
      Settled;
      LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      NameFile('../invalid', False);
      Click('studio-source-files-create');
      Settled;
      LMessage := GStudio.SourceView.Root.Find('studio-source-files-message');
      Check((LMessage <> nil) and (LMessage.Prop('text') <> '') and
        (GStudio.SourceView.Root.Find('studio-source-files-name').Prop('value') = '../invalid') and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore),
        'invalid creation retains its actual field and complete accepted/draft pair');

      if AJourney = sfjFeedback then
      begin
        { The complete management journey separately qualifies compilation and
          history. This focused real-input continuation checks only ownership of
          successive local refusals/reviews, without repeating unrelated builds. }
        LFeedback := LMessage.Prop('text');
        ReviewRemoval(LUtility);
        Check((GStudio.SourceView.Root.Find('studio-source-files-message').Prop('text') <> LFeedback) and
          (GStudio.SourceView.Root.Find('studio-source-files-name').Prop('value') = '../invalid') and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore),
          'a new removal review clears earlier local feedback while retaining exact fields and pair');
        Click('action-source-tab');
        LMemo.Text := GStudio.Session.DraftFileText(LUtility) + #10 + '{ A later review idea. }';
        Settled;
        LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        Click('action-source-files-tab');
        Click('studio-source-files-remove-confirm');
        Settled;
        LFeedback := GStudio.SourceView.Root.Find('studio-source-files-message').Prop('text');
        Check((Pos('changed', LFeedback) > 0) and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore),
          'changed reviewed input receives its own scoped refusal without publication');
        Click('studio-source-files-remove-cancel');
        Settled;
        Check((GStudio.SourceView.Root.Find('studio-source-files-warning') = nil) and
          (GStudio.SourceView.Root.Find('studio-source-files-message').Prop('text') <> LFeedback) and
          (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore),
          'Keep file retires only that refusal and review, preserving every accepted and draft byte');
        WriteLn('SOURCE FEEDBACK ', LCase, ' passed');
        Flush(Output);
        Exit;
      end;
      NameFile('project.toolbox', False);
      Click('studio-source-files-create');
      Settled;
      Check((GStudio.SourceCommands.State = nssApplied) and
        (GStudio.Session.SourceFileCount = LCount + 1) and
        GStudio.Session.ActiveSourceFile.SameFile(LNewUnit) and
        (Pos('unit project.toolbox;', LMemo.Text) > 0),
        'ordinary creation publishes and opens its typed unit template / ' + GStudio.SourceCommands.Message);
      LAfter := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Click('action-undo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
        'unit creation Undo restores all earlier files and independent buffers');
      Click('action-redo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter,
        'unit creation Redo restores exact captured source and buffers');
      Pick(LNewUnit);
      LMemo.Text := '{ An unfinished new unit 🚀 𐐷';
      Settled;
      LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      ReviewRemoval(LNewUnit);
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
        'review alone never removes a file, draft or history value');
      Click('studio-source-files-remove-cancel');
      Settled;
      Check((GStudio.SourceView.Root.Find('studio-source-files-warning') = nil) and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore),
        'Keep file cancels the warning without changing project data');
      ReviewRemoval(LNewUnit);
      Click('action-source-tab');
      LMemo.Text := GStudio.Session.ActiveSourceText + #10 + '{ A newer draft. }';
      Settled;
      LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Click('action-source-files-tab');
      Click('studio-source-files-remove-confirm');
      Settled;
      Check((EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore) and
        (GStudio.SourceView.Root.Find('studio-source-files-message').Prop('text') <> ''),
        'changed reviewed draft refuses confirmation and preserves every byte');
      Click('studio-source-files-remove-cancel');
      ReviewRemoval(LNewUnit);
      Click('studio-source-files-remove-confirm');
      Settled;
      Check((GStudio.SourceCommands.State = nssApplied) and
        (GStudio.Session.SourceFileCount = LCount) and
        not GStudio.Session.FileDrafts.Contains(LNewUnit),
        'confirmed removal retires only its named file and draft / ' + GStudio.SourceCommands.Message);
      LAfter := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Click('action-undo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
        'removal Undo restores exact unit text and unfinished draft/base together');
      Click('action-redo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter,
        'removal Redo retains the exact independent buffers');
      NameFile('toolbox-notes.inc', True);
      Click('studio-source-files-create');
      Settled;
      Check((GStudio.SourceCommands.State = nssApplied) and
        GStudio.Session.ActiveSourceFile.SameFile(LNewInclude),
        'ordinary include creation opens its independently owned fragment');
      ReviewRemoval(LNewInclude);
      Click('studio-source-files-remove-confirm');
      Settled;
      Check((GStudio.SourceCommands.State = nssApplied) and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter),
        'include creation/removal returns exactly to the retained earlier pair');
      ReviewRemoval(LInclude);
      Click('studio-source-files-remove-confirm');
      Settled;
      Check((EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter) and
        (GStudio.SourceView.Root.Find('studio-source-files-message').Prop('text') <> ''),
        'a referenced include refuses removal before any paired publication');
      Click('studio-source-files-remove-cancel');
      ReviewRemoval(LUnit);
      Click('studio-source-files-remove-confirm');
      Settled;
      Check((GStudio.SourceCommands.State in [nssRejected, nssFailed]) and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter),
        'actual compiler rejects deleting the used unit and keeps the complete pair');
      Pick(LUtility);
      Click('action-reset-source');
      Settled;
      Check(not GStudio.Session.FileDrafts.Contains(LUtility) and
        (GStudio.Session.DraftSource = CMainDraft) and
        (GStudio.CodeView.InputFor('studio-code') = LMemo),
        'file management retains the same memo and only explicit Restore consumes its fixture draft');
      Pick(GStudio.Session.SourceFile(0));
      WriteLn('SOURCE MANAGEMENT ', LCase, ' passed');
      Flush(Output);
    end;

  begin
    LCase := 'local-';

    if AShared then
    begin
      LCase := 'shared-';
    end;
    LCase := LCase + IntToStr(AWidth);
    WriteLn('SOURCE FILES ', LCase, ' starting');
    Flush(Output);
    LHome := IncludeTrailingPathDelimiter(ParamStr(4)) + LCase;
    LDirectories := ADirectories.RunningIn(LHome).EnrollingProject(LHome);
    GForm.ClientWidth := AWidth;
    GForm.ClientHeight := 900;
    LFactory := nil;
    try

      if AShared then
      begin
        { Trusted copied qualification seed, not semantic import evidence. Its
          exact complete input is independently verified during backend recovery. }
        LSeed := TNyxStudioSession.Create;
        try
          LSeed.AdoptProjectedProject(LPair, LProjection);
          LFrame := Default(TNyxAgentRecoveryFrame);
          LFrame.Revision := 7;
          LFrame.Claimed := True;
          LFrame.Permission := apEdit;
          LFrame.Session.Pair := LPair;
          LFrame.Session.AcceptedCheckpoint := LSeed.AcceptedSourceCheckpoint;
          LFrame.Session.Selection := 'compose';
          LFrame.Session.View := 'compose';
          LPrimary := TNyxAgentSession.CreateRecovered(LFrame);
          try
            LWorkspaces := TNyxStudioWorkspaces.Create(LPrimary, 'source-file-controls');
            try
              LStore := TNyxStudioRuntimeStore.Create(LDirectories);
              try
                LStore.Save(LPrimary, LWorkspaces);
              finally
                LStore.Free;
              end;
            finally
              LWorkspaces.Free;
            end;
          finally
            LPrimary.Free;
          end;
        finally
          LSeed.Free;
        end;
        GSharedEngine := TNyxStudioMCP.Create(LDirectories, 8762, 8763,
          LProfile.Encode, NewNyxNativeRuntimeSourceVerifier(LDirectories,
            LProfile.Encode, TNyxCompilerLimits.Default), rrmImmediate);
        LFactory := NewNyxSharedNativeSourceCompilerFactory(TConnectedSourceTransport.Create,
          TNyxNativeSourceServiceOptions.Defaults.Polling(10));
        GStudio := TConnectedStudio.Create(GForm, LDirectories.Projects, nil,
          LFactory, NewNyxNativeSourceCompiler(LDirectories, LProfile.Encode,
            TNyxCompilerLimits.Default));
      end
      else
      begin
        GStudio := TNyxNativeStudio.Create(GForm, LDirectories.Projects,
          NewNyxNativeSourceCompiler(LDirectories, LProfile.Encode, TNyxCompilerLimits.Default));
      end;
      GStudio.Run;

      if AShared then
      begin
        GStudio.ConnectService('http://127.0.0.1:8762', NyxPrimaryWorkspace);
      end
      else
      begin
        GStudio.LoadProject(LPair);
      end;
      Settled;
      Check(GStudio.Session.ProjectSnapshot.Files.Encoded = LPair.Files.Encoded,
        'ordinary project entry retains exact supporting files');
      Check(GForm.Enabled, 'ordinary host is enabled before source navigation');

      if GStudio.ShellView.Root.Find('action-panel-design') <> nil then
      begin
        Click('action-panel-design');
      end;

      if GStudio.CodeView.Root = nil then
      begin

        if AWidth < 640 then
        begin
          OpenCompactSource;
        end
        else
        begin
          Click('action-code');
        end;
      end;
      Settled;
      Check(GStudio.CodeView.Root <> nil, 'source editor mounted after ordinary open / ' + GStudio.Status);
      LMemo := TMemo(GStudio.CodeView.InputFor('studio-code'));
      Check(LMemo <> nil, 'ordinary Nyx source editor is mounted');
      LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('plan-values.inc'));
      LUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('project.values'));
      LUtility := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('ui-plan.inc'));
      LInitialInclude := GStudio.Session.SourceFileText(LInclude);
      LChangedInclude := 'function ProjectPlan: TNyxText;' + #10 + 'begin' + #10 +
        '  Result := ' + TNyxCodegen.TextLiteral(CChanged) + ';' + #10 + 'end;' + #10;

      if AJourney = sfjOutline then
      begin
        OutlineControls;
        DraftOutlineControls;
        Exit;
      end;

      if AJourney in [sfjSemantic, sfjProjectRoutines] then
      begin
        SemanticFiles;
        Exit;
      end;
      ManageFiles;

      if AJourney = sfjFeedback then
      begin
        Exit;
      end;
      LMemo.Text := CMainDraft;
      Settled;
      Click('action-expand-source');
      Check(GStudio.SourceModal.IsOpen and
        (GStudio.CodeView.InputFor('studio-code') = LMemo),
        'expanded source retains the same native editor and independent buffers');
      Pick(LUtility);
      LMemo.Text := '{ An unfinished utility buffer';
      Settled;
      Pick(LUnit);
      LMemo.Text := CUnitDraft;
      Settled;
      Pick(LInclude);
      Click('action-expand-source');
      Check(not GStudio.SourceModal.IsOpen and
        GForm.Enabled and (GStudio.CodeView.InputFor('studio-code') = LMemo),
        'closing expanded source preserves selected file and mounted editor');
      LMemo.Text := LChangedInclude;
      Settled;
      { Actual native input owns its platform newline spelling. The captured
        portable buffer, not the pre-widget assignment, is the compiler oracle. }
      LChangedInclude := GStudio.Session.DraftFileText(LInclude);
      Check((GStudio.Session.DraftSource = CMainDraft) and
        (GStudio.Session.DraftFileText(LUnit) = CUnitDraft),
        'physical switching retains independent unfinished main and unit buffers');
      { The async command's Undo baseline is the exact dispatch frame on both
        peers. Newer typing stays current and is retained in the opposite Redo
        entry when Undo deliberately restores that earlier frame. }
      LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      { Activate the ordinary Apply button without the harness's settling pump.
        Switch the actual picker/memo on the following real UI turn, while the
        independent compiler still owns its input. No draft is written by fiat. }
      TControlAccess(GStudio.SourceView.ControlFor('action-apply-source')).Click;
      Check(GStudio.SourceCommands.Busy, 'real helper Apply has an ongoing compiler lifetime');
      LPicker := TComboBox(GStudio.SourceView.InputFor(NyxStudioSourceFileID));
      LPicker.ItemIndex := LPicker.Items.IndexOf(GStudio.Session.SourceFileTitle(LUtility));
      LPicker.OnChange(LPicker);
      Application.ProcessMessages;
      TControlAccess(GStudio.SourceView.ControlFor('action-reset-source')).Click;
      Check(not GStudio.Session.FileDrafts.Contains(LUtility) and GStudio.SourceCommands.Busy,
        'Restore of another file preserves the active helper compiler intent');
      LPicker := TComboBox(GStudio.SourceView.InputFor(NyxStudioSourceFileID));
      LPicker.ItemIndex := LPicker.Items.IndexOf(GStudio.Session.SourceFileTitle(GStudio.Session.SourceFile(0)));
      LPicker.OnChange(LPicker);
      Application.ProcessMessages;
      Check(GStudio.Session.ActiveSourceFile.SameFile(GStudio.Session.SourceFile(0)),
        'main buffer can be selected while helper compilation continues');
      Check(GStudio.SourceCommands.Busy, 'unrelated typing occurs before compiler completion');
      LMemo.Text := CMainDuring;
      Settled;
      Check(GStudio.SourceCommands.State = nssApplied,
        'selected include applies through actual compiler/publication');
      Check((GStudio.Session.SourceFileText(LInclude) = LChangedInclude) and
        (GStudio.Session.Source = LPair.Source), 'helper Apply changes only selected accepted input');
      Check((GStudio.Session.DraftSource = CMainDuring) and
        (GStudio.Session.DraftFileText(LUnit) = CUnitDraft) and
        not GStudio.Session.FileDrafts.Contains(LInclude),
        'helper publication consumes only its selected draft');
      Check(GStudio.Session.Save <> LPair.Design, 'actual helper reconstruction changes the accepted UI meaning');
      LAfter := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Click('action-undo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
        'one physical Undo restores exact accepted pair and every pending file/base');
      Click('action-redo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter,
        'one physical Redo restores exact compiled pair and independent drafts');
      Check(GStudio.Session.DraftSource = CMainDuring,
        'opposite history retains newer unrelated typing from compiler time');
      { Typed queue API coverage complements the physical controls above. A
        second file must not supersede the active first file; two waiting Apply
        calls for that second file coalesce to its latest exact buffer. }
      LQueuedUnit := GStudio.Session.SourceFileText(LUnit) + #10 + '{ A retained unit note. }' + #10;
      LBeforeQueueInclude := GStudio.Session.SourceFileText(LInclude);
      LQueuedInclude := LBeforeQueueInclude + #10 + '{ The latest queued include note. }' + #10;
      Pick(LUnit);
      LMemo.Text := LQueuedUnit;
      Settled;
      LQueuedUnit := GStudio.Session.DraftFileText(LUnit);
      GStudio.SourceCommands.ApplyFile(LUnit);
      Check(GStudio.SourceCommands.Busy, 'first typed file Apply owns its compiler lifetime');
      LPicker := TComboBox(GStudio.SourceView.InputFor(NyxStudioSourceFileID));
      LPicker.ItemIndex := LPicker.Items.IndexOf(GStudio.Session.SourceFileTitle(LInclude));
      LPicker.OnChange(LPicker);
      Application.ProcessMessages;
      Check(GStudio.SourceCommands.Busy, 'second file is queued before the first compiler completes');
      LMemo.Text := LBeforeQueueInclude + #10 + '{ An obsolete queued include note. }' + #10;
      GStudio.SourceCommands.ApplyFile(LInclude);
      LMemo.Text := LQueuedInclude;
      LQueuedInclude := GStudio.Session.DraftFileText(LInclude);
      GStudio.SourceCommands.ApplyFile(LInclude);
      Settled;
      Check((GStudio.SourceCommands.State = nssApplied) and
        (GStudio.Session.SourceFileText(LUnit) = LQueuedUnit) and
        (GStudio.Session.SourceFileText(LInclude) = LQueuedInclude),
        'distinct files apply FIFO and waiting repeats retain only the latest buffer');
      Check((GStudio.Session.DraftSource = CMainDuring) and
        not GStudio.Session.FileDrafts.Contains(LUnit) and
        not GStudio.Session.FileDrafts.Contains(LInclude),
        'queued file applications consume their own buffers and retain unrelated main typing');
      LAfter := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Click('action-undo');
      Settled;
      Check((GStudio.Session.SourceFileText(LUnit) = LQueuedUnit) and
        (GStudio.Session.SourceFileText(LInclude) = LBeforeQueueInclude) and
        (GStudio.Session.DraftFileText(LInclude) = LQueuedInclude),
        'one Undo restores exactly the second queued file at its refreshed dispatch frame');
      Click('action-redo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter,
        'queued file Redo restores the exact complete pair and buffers');
      { Visual continuation must use accepted helpers while retaining an older
        unfinished unit and a newly typed utility buffer. Its paired history
        uses the same dispatch snapshot on the ordinary client and backend. }
      Pick(LUnit);
      LMemo.Text := CUnitDraft;
      Settled;
      LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      LEdit := Default(TNyxStudioDesignEdit);
      LEdit.Action := sdaTitle;
      LEdit.Selection := GStudio.Session.SelectedID;
      LEdit.View := GStudio.Session.ActiveViewID;
      LEdit.Value := 'File-aware visual editing';
      GStudio.SourceCommands.Edit(LEdit);
      Check(GStudio.SourceCommands.Busy, 'visual continuation has an ongoing compiler lifetime');
      LPicker := TComboBox(GStudio.SourceView.InputFor(NyxStudioSourceFileID));
      LPicker.ItemIndex := LPicker.Items.IndexOf(GStudio.Session.SourceFileTitle(LUtility));
      LPicker.OnChange(LPicker);
      Application.ProcessMessages;
      LMemo.Text := '{ A utility edited during visual compilation';
      Settled;
      Check((GStudio.SourceCommands.State = nssApplied) and
        (GStudio.Session.Document.Title = LEdit.Value) and
        (GStudio.Session.DraftFileText(LUnit) = CUnitDraft) and
        GStudio.Session.FileDrafts.Contains(LUtility) and
        (GStudio.Session.DraftSource = CMainDuring),
        'visual compiler continuation preserves old and newer independent file buffers / ' +
        IntToStr(Ord(GStudio.SourceCommands.State)) + ' / ' + GStudio.SourceCommands.Message);
      LAfter := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Click('action-undo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
        'visual Undo restores the exact earlier source/file/draft dispatch snapshot');
      Click('action-redo');
      Settled;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LAfter,
        'visual Redo preserves newer helper typing in the opposite history entry');
      Pick(LUnit);
      Click('action-reset-source');
      Settled;
      Check(not GStudio.Session.FileDrafts.Contains(LUnit) and
        (GStudio.Session.DraftSource = CMainDuring), 'Restore affects only selected unit');
      Pick(LInclude);
      LMemo.Text := 'function ProjectPlan: TNyxText;' + #10 + 'begin' + #10 +
        '  { 🚀 𐐷 } Result := ;' + #10 + 'end;' + #10;
      Settled;
      LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
      Click('action-apply-source');
      Settled;
      Check(GStudio.SourceCommands.State in [nssRejected, nssFailed],
        'actual compiler refuses unfinished selected include');
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
        'failed helper compilation retains accepted pair and all draft text/bases');
      Check(GStudio.Session.SourceDiagnostic.Defined and
        (GStudio.Session.SourceDiagnostic.Message <> '') and
        (GStudio.Session.SourceDiagnostic.Line = 3) and
        (GStudio.Session.SourceDiagnostic.Column = 21) and
        GStudio.Session.SourceDiagnosticFile.SameFile(LInclude),
        'failed include owns its exact compiler line and supplementary scalar column / ' +
          IntToStr(GStudio.Session.SourceDiagnostic.Line) + ':' +
          IntToStr(GStudio.Session.SourceDiagnostic.Column));
      LPreviousAction := GStudio.SourceView.Root.Find(NyxStudioDiagnosticGoID).Clone;
      try
        Pick(GStudio.Session.SourceFile(0));
        Check(GStudio.Session.SourceDiagnostic.Defined,
          'diagnostic target survives choosing another independent source buffer');
        { File selection presents Source. Follow the ordinary visible Messages
          tab before clicking its action; hidden native buttons refuse input. }
        Click('action-messages-tab');
        Settled;
        Click(NyxStudioDiagnosticGoID);
        Settled;
        Check(GStudio.Session.ActiveSourceFile.SameFile(LInclude) and
          (LMemo.CaretPos.Y = 2) and (LMemo.CaretPos.X = 22),
          'ordinary source action selects the include and places the real Win32 UTF-16 caret / ' +
          GStudio.Session.ActiveSourceFile.Name + ' / ' + IntToStr(LMemo.CaretPos.X) + ':' +
          IntToStr(LMemo.CaretPos.Y));
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
          'file navigation changes no accepted source, pending buffer/base or history pair');
        { A new real unit error must replace only diagnostic presentation. The
          earlier include remains unfinished, but never becomes compiler input. }
        Pick(LUnit);
        LUnitSource := GStudio.Session.SourceFileText(LUnit);
        LInsertion := Pos('implementation', LUnitSource) + Length('implementation') - 1;
        Check(LInsertion >= Length('implementation'), 'fixture contains its explicit unit implementation');
        LMemo.Text := Copy(LUnitSource, 1, LInsertion) + #10 +
          'procedure BrokenDiagnostic;' + #10 + 'begin' + #10 +
          CNativeFailure + #10 + 'end;' + #10 +
          Copy(LUnitSource, LInsertion + 1, MaxInt);
        Settled;
        LBefore := EncodeNyxProject(GStudio.Session.ProjectSnapshot);
        Click('action-apply-source');
        Settled;
        LSite := GStudio.Session.SourceDiagnostic;
        Check(LSite.Defined and (LSite.Line > 0) and (LSite.Column = 11) and
          GStudio.Session.SourceDiagnosticFile.SameFile(LUnit),
          'a real unit failure retains its own exact Unicode scalar location');
        Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = LBefore,
          'unit failure preserves the complete pair and the earlier invalid include buffer');
        LRejected := False;
        try
          RouteNyxSourceDiagnostic(GStudio.Session, LPreviousAction, ntClick, LSite);
        except
          on ENyxModel do
          begin
            LRejected := True;
          end;
        end;
        Check(LRejected and GStudio.Session.ActiveSourceFile.SameFile(LUnit),
          'earlier rendered include action cannot redirect the newer unit diagnostic');
        Pick(LInclude);
        Click('action-messages-tab');
        Settled;
        Click(NyxStudioDiagnosticGoID);
        Settled;
        Check(GStudio.Session.ActiveSourceFile.SameFile(LUnit) and
          (LMemo.CaretPos.Y + 1 = GStudio.Session.SourceDiagnostic.Line) and
          (LMemo.CaretPos.X = 12),
          'ordinary unit error navigation selects the correct buffer and physical caret / ' +
          GStudio.Session.ActiveSourceFile.Name + ' / ' + IntToStr(LMemo.CaretPos.X) + ':' +
          IntToStr(LMemo.CaretPos.Y));
        LMemo.Text := GStudio.Session.DraftFileText(LUnit) + #10 + '{ New independent typing. }';
        Settled;
        Check(not GStudio.Session.SourceDiagnostic.Defined,
          'changing the exact target retires its earlier compiler location');
      finally
        LPreviousAction.Free;
      end;
      Check(GStudio.CodeView.InputFor('studio-code') = LMemo,
        'compiler success/history/refusal keep the same native source widget');
      Check(LInitialInclude <> LChangedInclude, 'fixture changes the real helper implementation');
      WriteLn('SOURCE FILES ', LCase, ' passed');
      Flush(Output);
    finally
      FreeAndNil(GStudio);
      GSharedExchange := nil;
      LFactory := nil;
      FreeAndNil(GSharedEngine);
      CheckSynchronize(1);
    end;
  end;

begin
  LProfile := TNyxOutputConfiguration.Create;
  try
    LProfile.SetField('fpc', ATools.Field('FPC').AsText);
    LProfile.SetField('pas2js', ATools.Field('PAS2JS').AsText);
    LProfile.SetField('runtime', ATools.Field('PAS2JS_RUNTIME').AsText);
    LDocument := CompiledRuntimeDocument(CInitial, LSource);
    try
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    finally
      LDocument.Free;
    end;
    LInputs := CompiledRuntimeProject(LSource, CInitial);

    if AJourney = sfjProjectRoutines then
    begin
      { The routine journey alone owns these real compiled overloads. Other
        maintained file fixtures and their original expected inventories stay
        unchanged; this adds no metadata-only stand-in for compiler evidence. }
      LExtraUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('project.values')).Text(LInputs);
        LUnitEnd := Pos('implementation', LExtraUnit);
        Check(LUnitEnd > 0, 'owned helper fixture has its explicit implementation boundary');
        LExtraUnit := Copy(LExtraUnit, 1, LUnitEnd - 1) +
          'function ConventionPlan: Integer; cdecl;' + #10 + Copy(LExtraUnit, LUnitEnd, MaxInt);
      LUnitEnd := Pos('end.', LExtraUnit);
      Check(LUnitEnd > 0, 'owned helper fixture has its explicit final unit boundary');
      LExtraUnit := Copy(LExtraUnit, 1, LUnitEnd - 1) +
        'function OverloadedPlan(AValue: Integer): Integer; overload;' + #10 +
        'begin' + #10 + '  Result := AValue;' + #10 + 'end;' + #10 +
        'function OverloadedPlan(const AValue: TNyxText): TNyxText; overload;' + #10 +
        'begin' + #10 + '  Result := AValue;' + #10 + 'end;' + #10 +
        'function ConventionPlan: Integer; cdecl;' + #10 +
        'begin' + #10 + '  Result := 7;' + #10 + 'end;' + #10 +
        Copy(LExtraUnit, LUnitEnd, MaxInt);
      LInputs := LInputs.WithUnit(NyxPascalUnit('project.values'), LExtraUnit);
    end;
    LPair.Source := LInputs.Source;
    LPair.Files := TNyxPascalProjectFiles.Capture(LInputs);
    LVerifier := NewNyxNativeRuntimeSourceVerifier(ADirectories,
      LProfile.Encode, TNyxCompilerLimits.Default);
    Check(Supports(LVerifier, INyxRuntimeProjectVerifier, LProjectVerifier),
      'qualification uses the real complete-project native producer');
    LProjection := LProjectVerifier.VerifyProject(LInputs);
    Check((LProjection.State = spsExecuted) and (LProjection.Design = LPair.Design),
      'initial fixture construction independently matches its expected document');

    if AJourney = sfjFull then
    begin
      QualifyBrowserCompilerSite;
    end;
    RunCase(1280, True);
    RunCase(390, True);

    if not (AJourney in [sfjSemantic, sfjProjectRoutines]) then
    begin
      RunCase(1280, False);
      RunCase(390, False);
    end;
  finally
    LProjection := nil;
    LProjectVerifier := nil;
    LVerifier := nil;
    LProfile.Free;
  end;
end;
