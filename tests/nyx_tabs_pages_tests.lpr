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

program nyx_tabs_pages_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.tabs,
  nyx.tabs.pages.editor, nyx.menu.editor, nyx.codec, nyx.codegen, nyx.source, nyx.state,
  nyx.studio.edits, nyx.studio.projects, nyx.studio.session
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LForm: TNyxDocument;
  LTabs: INyxTabs;
  LEditor: INyxCard;
  LChange: TNyxTabPageChange;
  LSession: TNyxStudioSession;
  LWorkspace: TNyxSourceWorkspace;
  LDraft: TNyxTabPagesDraft;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LPair: TNyxProjectPair;
  LRefused: Boolean;
  LChecks: Integer;
  LReference: INyxComponent;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Tab page authoring: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure RebuildForm;
begin
  LEditor := nil;
  FreeAndNil(LForm);
  LForm := TNyxDocument.Create;
  LEditor := NewNyxTabPagesEditor('pages-editor', NyxControl('project-tabs'), LDocument);
  LForm.AddPage(LEditor);
end;

procedure Capture(AOperation: TNyxTabPageOperation; const APage: TNyxText = 'plan');
begin
  LChange := Default(TNyxTabPageChange);
  LChange.Owner := NyxControl('project-tabs');
  LChange.Page := NyxControl(APage);
  LChange.Operation := AOperation;
  LChange.Caption := 'New ideas';
  LChange.Baseline := NyxTabPagesBaseline(LDocument, LChange.Owner);
  LChange.Confirmed := AOperation = ntpRemove;
end;

begin
  try
    LDocument := TNyxDocument.Create;
    LTabs := NewNyxTabs('project-tabs');
    LTabs.Add(NewNyxTab('plan').Configure.Text('Plan').Done);
    LTabs.Node.Children[0].Add(NewNyxMemo('notes').Configure.Value('Exact notes / 🌙').Done);
    LTabs.Add(NewNyxTab('details').Configure.Text('Details').Done);
    LTabs.Add(NewNyxTab('review').Configure.Text('Review').Done);
    LDocument.AddPage(LTabs);
    LTabs := nil;
    LBefore := TNyxCodec.Encode(LDocument);
    RebuildForm;
    Check(Supports(LEditor.Part(NyxPart('page')), INyxSelect) and
      Supports(LEditor.Part(NyxPart('confirm')), INyxCheckbox), 'public form uses specialized Nyx parts');
    Check(Pos(TNyxText('Exact notes / 🌙'), NyxTabPagesBaseline(LDocument, NyxControl('project-tabs'))) > 0,
      'exact subtree baseline retains supplementary Unicode');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'form construction retains exact source');
    LEditor.Part(NyxPart('new-caption')).Configure.Value('A fresh idea').Done;
    Check(CaptureNyxTabPagesEditor(LEditor.Part(NyxPart('actions/add')).Node, LForm.Pages[0], LChange),
      'public form captures a typed add intent');
    Check(LChange.ToData.ToJSON = TNyxTabPageChange.FromData(LChange.ToData).ToData.ToJSON,
      'strict intent round-trip retains exact values');
    LCandidate := EditNyxTabPages(LDocument, LChange);
    Check((LCandidate.Pages[0].Count = 4) and
      (LCandidate.Pages[0].Children[3].ID = 'project-tabs-page-1') and
      (LCandidate.Pages[0].Children[3].Prop('text') = 'A fresh idea'),
      'creation chooses an unused purpose/page identity and the authored caption');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'candidate creation preserves its input');
    LWorkspace := TNyxSourceWorkspace.Create;
    LAfter := TNyxCodegen.Generate(LCandidate);
    Check(Pos('INyxTab', LAfter) > 0, 'new page generates its specialized interface');
    FreeAndNil(LCandidate);
    LCandidate := LWorkspace.Candidate(LDocument, LAfter);
    Check(LCandidate.Pages[0].Count = 4, 'ordinary Pascal reconstructs the new page');
    FreeAndNil(LCandidate);
    LSession := TNyxStudioSession.Create(NyxProjectPair(LBefore, TNyxCodegen.Generate(LDocument)));
    LPair := LSession.ProjectSnapshot;
    LSession.ApplyPatch(NyxTabPagePatch(LChange));
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'one paired Undo restores the exact pre-create pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter,
      'one paired Redo restores exact created source and design');
    Capture(ntpRename);
    LChange.Caption := 'Planning';
    LCandidate := EditNyxTabPages(LDocument, LChange);
    Check((LCandidate.Find('plan').Prop('text') = 'Planning') and
      (LCandidate.Find('notes').Prop('value') = TNyxText('Exact notes / 🌙')),
      'renaming preserves identity, descendants and exact values');
    FreeAndNil(LCandidate);
    Capture(ntpLater);
    LCandidate := EditNyxTabPages(LDocument, LChange);
    Check((LCandidate.Pages[0].Children[1].ID = 'plan') and (LCandidate.Find('notes') <> nil),
      'reorder retains complete page ownership and contents');
    FreeAndNil(LCandidate);
    Capture(ntpEarlier);
    LRefused := False;
    try
      LCandidate := EditNyxTabPages(LDocument, LChange);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore), 'edge movement refuses without changing source');
    Capture(ntpRemove, 'review');
    LChange.Confirmed := False;
    LRefused := False;
    try
      LCandidate := EditNyxTabPages(LDocument, LChange);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused, 'removal requires an explicit warning acknowledgement');
    LChange.Confirmed := True;
    LCandidate := EditNyxTabPages(LDocument, LChange);
    Check((LCandidate.Find('review') = nil) and (LCandidate.Find('notes') <> nil),
      'confirmed removal affects only the exact page');
    FreeAndNil(LCandidate);
    Capture(ntpRemove);
    LDocument.Find('notes').Configure.Value('Later work').Done;
    LRefused := False;
    try
      LCandidate := EditNyxTabPages(LDocument, LChange);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused and (LDocument.Find('notes').Prop('value') = 'Later work'),
      'a descendant edit invalidates captured removal consent');
    SetNyxTabsOptions(LDocument.Pages[0].ComponentReference as INyxControl,
      TNyxTabsOptions.Defaults.Initial(NyxControl('review')));
    Capture(ntpRemove, 'review');
    LRefused := False;
    try
      LCandidate := EditNyxTabPages(LDocument, LChange);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused and (LDocument.Find('review') <> nil), 'an initial selection dependency refuses removal');
    SetNyxTabsOptions(LDocument.Pages[0].ComponentReference as INyxControl, TNyxTabsOptions.Defaults);
    LDocument.State.SetValue(NyxTextState('selected-page'), 'review');
    LDocument.Pages[0].Binds.Value(NyxTextState('selected-page')).Done;
    Capture(ntpRemove, 'review');
    LRefused := False;
    try
      LCandidate := EditNyxTabPages(LDocument, LChange);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused and (LDocument.State.GetValue(NyxTextState('selected-page')) = 'review'),
      'a bound state default refuses removal without being rewritten');
    RebuildForm;
    LEditor.Part(NyxPart('caption')).Configure.Value('Unfinished caption').Done;
    LDraft.Capture('pages-editor', LForm.Pages[0]);
    LEditor.Part(NyxPart('caption')).Configure.Value('Other value').Done;
    Check(LDraft.Restore(LForm.Pages[0]) and
      (LEditor.Part(NyxPart('caption')).Node.Prop('value') = 'Unfinished caption'),
      'ephemeral form draft restores at the same complete baseline');
    LDocument.Find('notes').Configure.Value('Another revision').Done;
    RebuildForm;
    Check(not LDraft.Restore(LForm.Pages[0]), 'changed descendant content retires the old form draft');
    LDocument.State.SetValue(NyxTextState('caption'), 'Bound title');
    LDocument.Find('details').Binds.Text(NyxTextState('caption')).Done;
    Capture(ntpRename, 'details');
    LRefused := False;
    try
      LCandidate := EditNyxTabPages(LDocument, LChange);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused and (LDocument.State.GetValue(NyxTextState('caption')) = 'Bound title'),
      'bound captions require an explicit binding/default edit instead of a silent override');
    LEditor := nil;
    FreeAndNil(LForm);
    FreeAndNil(LDocument);
    LDocument := TNyxDocument.Create;
    LTabs := NewNyxTabs('project-tabs');
    LTabs.Add(NewNyxTab('plan').Configure.Text('Section').Done);
    LTabs.Add(NewNyxTab('details').Configure.Text('Section').Done);
    LDocument.AddComponent(LTabs);
    LTabs := nil;
    LDocument.AddPage(NewNyxPage('workspace'));
    LReference := NewNyxComponent('first-instance');
    LReference.Configure.Component(NyxComponent('project-tabs')).Done;
    LDocument.Pages[0].Add(LReference);
    LReference := nil;
    LForm := TNyxDocument.Create;
    LEditor := NewNyxTabPagesEditor('pages-editor', NyxControl('first-instance'), LDocument);
    LForm.AddPage(LEditor);
    Check((NyxTabPagesDefinition(LEditor.Node).ID = 'project-tabs') and
      Supports(LEditor.Part(NyxPart('definition')), INyxButton),
      'an instance exposes exact reusable-definition navigation through public parts');
    Capture(ntpAdd);
    LChange.Owner := NyxControl('first-instance');
    LChange.Baseline := NyxTabPagesBaseline(LDocument, LChange.Owner);
    LRefused := False;
    try
      LCandidate := EditNyxTabPages(LDocument, LChange);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused and (LDocument.Components[0].Count = 2),
      'instance structural intent cannot silently edit the shared definition');
    RebuildForm;
    LEditor.Part(NyxPart('page')).Configure.Value(NyxMenuEditorChoiceCaption(1, 'Section')).Done;
    Check(RouteNyxTabPageChoice(LEditor.Part(NyxPart('page')).Node, LForm.Pages[0]),
      'duplicate captions still resolve an exact ordinal choice');
    LEditor.Part(NyxPart('caption')).Configure.Value('Second section').Done;
    Check(CaptureNyxTabPagesEditor(LEditor.Part(NyxPart('actions/rename')).Node, LForm.Pages[0], LChange) and
      (LChange.Page.ID = 'details'), 'captured choice retains identity independently of its caption');
    LCandidate := EditNyxTabPages(LDocument, LChange);
    Check((LCandidate.Find('details').Prop('text') = 'Second section') and
      (LCandidate.Find('plan').Prop('text') = 'Section') and
      (LDocument.Find('details').Prop('text') = 'Section'),
      'definition edit owns independent data and changes only the chosen page');
    FreeAndNil(LCandidate);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-tab-pages', 'passed');
    document.body.setAttribute('data-nyx-tab-pages-checks', IntToStr(LChecks));
    {$endif}
    WriteLn('PASS ', LChecks, ' shared tab page authoring checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-tab-pages', 'failed');
      {$else}
      DumpExceptionBackTrace(Output);
      {$endif}
      ExitCode := 1;
    end;
  end;
  LWorkspace.Free;
  LSession.Free;
  LEditor := nil;
  LTabs := nil;
  LReference := nil;
  LForm.Free;
  LDocument.Free;
  LCandidate.Free;
end.
