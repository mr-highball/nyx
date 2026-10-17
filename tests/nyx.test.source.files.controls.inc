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

{ Same maintained ordinary-input harness, with no capture/HTTP execution. Both
  local and connected modes use real compiler processes; the connected transport
  substitutes sockets only, preserving the production backend/provider/bridge.
  Every project, recovery seed and machine profile belongs to this fresh packet. }
procedure RunSourceFileControls(const ADirectories: TNyxStudioDirectories;
  const ATools: TNyxDataValue);
const
  CInitial: TNyxText = 'An independent first plan';
  CChanged: TNyxText = 'A file-authored plan 🚀 𐐷';
  CMainDraft: TNyxText = '{ An unfinished main idea';
  CUnitDraft: TNyxText = '{ An unfinished independent unit';
  CMainDuring: TNyxText = '{ A newer main idea typed during helper compilation';
var
  LProfile: TNyxOutputConfiguration;
  LPair: TNyxProjectPair;
  LSource: TNyxText;
  LDocument: TNyxDocument;
  LInputs: INyxPascalProject;
  LVerifier: INyxRuntimeSourceVerifier;
  LProjectVerifier: INyxRuntimeProjectVerifier;
  LProjection: INyxSourceProjection;

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
      'begin' + #10 + '  { 🚀 𐐷 } MissingBrowserDiagnostic;' + #10 + 'end;' + #10 +
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
          '  { 🚀 𐐷 } MissingDiagnostic;' + #10 + 'end;' + #10 +
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
    LPair.Source := LInputs.Source;
    LPair.Files := TNyxPascalProjectFiles.Capture(LInputs);
    LVerifier := NewNyxNativeRuntimeSourceVerifier(ADirectories,
      LProfile.Encode, TNyxCompilerLimits.Default);
    Check(Supports(LVerifier, INyxRuntimeProjectVerifier, LProjectVerifier),
      'qualification uses the real complete-project native producer');
    LProjection := LProjectVerifier.VerifyProject(LInputs);
    Check((LProjection.State = spsExecuted) and (LProjection.Design = LPair.Design),
      'initial fixture construction independently matches its expected document');
    QualifyBrowserCompilerSite;
    RunCase(1280, True);
    RunCase(390, True);
    RunCase(1280, False);
    RunCase(390, False);
  finally
    LProjection := nil;
    LProjectVerifier := nil;
    LVerifier := nil;
    LProfile.Free;
  end;
end;
