{ Nyx completion navigation qualification. Distributed under the repository's
  MIT license. These pure editor tests create no host, socket or compiler job. }
program nyx_completion_navigation_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.controls, nyx.model,
  nyx.editing, nyx.editing.history, nyx.pascal.names, nyx.pascal.project,
  nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion,
  nyx.pascal.completion.view {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
end;

procedure Run;
const
  CBuffer: TNyxText = '// 🚀' + #13#10 + 'LBadge.Wi';
  CDeclaration: TNyxText = '// 𐐷' + #13#10 + 'procedure &WithText; // unfinished';
  CInclude: TNyxText = 'procedure WithTheme;';
var
  LSources: INyxPascalSourceSnapshot;
  LProject: INyxPascalProject;
  LHistory: INyxTextEditHistory;
  LMenu: INyxPascalCompletionMenu;
  LNavigation: INyxPascalCompletionNavigation;
  LQuery: INyxPascalCompletionRequest;
  LReply: INyxPascalCompletionResult;
  LTicket: TNyxPascalCompletionTicket;
  LMain, LUnit, LInclude: TNyxPascalSourceRef;
  LLocation: TNyxPascalCompletionLocation;
  LCard, LOld: INyxCard;
  LFrame: TNyxTextEditorFrame;
  LGeneration: Integer;
  LNameStart: Integer;

  procedure Present;
  begin
    LQuery := LMenu.BeginQuery(LSources, LMain, LMain, LTicket);
    LReply := NewNyxPascalCompletionResult(LQuery, pcsReady, '',
      LQuery.Caret - 2, 'Wi', [
        NyxPascalCompletion('WithText', 'WithText', 'Project method', pckMethod),
        NyxPascalCompletion('WithTheme', 'WithTheme', 'Include declaration', pckMethod),
        NyxPascalCompletion('WithExternal', 'WithExternal', 'SDK-only method', pckMethod)], 3);
    LReply := NewNyxPascalCompletionDefinitions(LReply, [
      NyxPascalCompletionLocation(LUnit, LNameStart, 9, 'WithText'),
      NyxPascalCompletionLocation(LInclude, 10, 9, 'WithTheme'),
      Default(TNyxPascalCompletionLocation)]);
    Check(LMenu.Present(LTicket, LReply, LSources), 'Current captured page presents');
    LCard := LMenu.Build('completion', LSources);
  end;
begin
  LMain := TNyxPascalSourceRef.Main;
  LUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('menu.helper'));
  LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('menu.inc'));
  LProject := NewNyxPascalProject(NyxPascalUnit('menu.source'),
    'unit menu.source; interface implementation end.')
    .WithUnit(NyxPascalUnit('menu.helper'), 'unit menu.helper; interface implementation end.')
    .WithInclude(NyxPascalInclude('menu.inc'), CInclude);
  LSources := CaptureNyxPascalSources(LProject).WithBuffer(LMain, CBuffer)
    .WithBuffer(LUnit, CDeclaration);
  LNameStart := NyxTextScalarCount('// 𐐷' + #13#10 + 'procedure ');
  LFrame := NyxTextEditorFrame(CBuffer,
    NyxTextSelection(CBuffer, NyxTextScalarCount(CBuffer), NyxTextScalarCount(CBuffer)));
  LHistory := NewNyxTextEditHistory(LFrame);
  LMenu := NewNyxPascalCompletionMenu(LHistory);
  Check(Supports(LMenu, INyxPascalCompletionNavigation, LNavigation), 'Menu exposes optional guarded navigation');
  Present;
  LGeneration := LHistory.Generation;
  LLocation := LNavigation.SelectedDefinition(LSources);
  Check(LLocation.Available and LLocation.FileReference.SameFile(LUnit) and
    (LLocation.Start = LNameStart) and (LLocation.Length = 9) and (LLocation.Name = 'WithText'),
    'Menu retains exact escaped Unicode/scalar project declaration');
  Check((LCard.Node.Find('completion-row-0-definition') <> nil) and
    (LCard.Node.Find('completion-row-1-definition') <> nil) and
    (LCard.Node.Find('completion-row-2-definition') = nil),
    'Only rows with a project-local target show Definition');
  Check(LMenu.Key(NyxKeyStroke(nkF12Key), LSources) = pcaDefinition,
    'F12 navigates the selected suggestion');
  Check((LHistory.Generation = LGeneration) and LHistory.Current.SameValue(LFrame) and
    not LHistory.CanUndo, 'Reading/routing navigation creates no text command');
  LOld := LCard;
  LCard := LMenu.Build('completion', LSources);
  Check(LMenu.Route('completion', LCard.Node, LOld.Node.Find('completion-row-1-definition'),
    ntClick, LSources) = pcaNone, 'A retired physical button cannot navigate another mount');
  Check((LMenu.Route('completion', LCard.Node, LCard.Node.Find('completion-row-1-definition'),
    ntClick, LSources) = pcaDefinition) and
    LNavigation.SelectedDefinition(LSources).FileReference.SameFile(LInclude),
    'Current row action chooses its exact include declaration');
  LCard.Node.Find('completion-row-0-definition').Configure.Enabled(False).Done;
  Check(LMenu.Route('completion', LCard.Node, LCard.Node.Find('completion-row-0-definition'),
    ntClick, LSources) = pcaNone, 'Disabled definition action has no command authority');
  LMenu.Key(NyxKeyStroke(nkDownKey), LSources);
  Check((LMenu.Selected = 2) and
    (LMenu.Key(NyxKeyStroke(nkF12Key), LSources) = pcaNone),
    'SDK-only row does not invent a local F12 target');
  Check(not LNavigation.SelectedDefinition(LSources.WithBuffer(LUnit, CDeclaration + ' ')).Available,
    'Any changed supporting draft retires navigation');
  Present;
  LHistory.Observe(NyxTextEditorFrame(CBuffer,
    NyxTextSelection(CBuffer, 0, 0)));
  LHistory.Observe(LFrame);
  Check(not LNavigation.SelectedDefinition(LSources).Available,
    'Caret movement back to equal coordinates cannot revive a captured page');
  Present;
  LOld := LCard;
  Present;
  Check(LMenu.Route('completion', LOld.Node, LOld.Node.Find('completion-row-0-definition'),
    ntClick, LSources) = pcaNone, 'An older request ticket stays retired even on its own mount');
  LMenu.Accept(LSources);
  Check((LHistory.UndoCount = 1) and LHistory.Undo and LHistory.Current.SameValue(LFrame),
    'Definition metadata preserves ordinary completion insertion and exact Undo');
  Present;
  LHistory.Observe(LFrame, True);
  Check((LMenu.Key(NyxKeyStroke(nkF12Key), LSources) = pcaNone) and
    not LNavigation.SelectedDefinition(LSources).Available,
    'Composition retains its physical keys and revokes navigation');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' completion navigation checks';
    {$else}
    WriteLn('PASS ', GChecks, ' completion navigation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, 'FAIL ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
