{ Nyx completion navigation qualification. Distributed under the repository's
  MIT license. These pure editor tests create no host, socket or compiler job. }
program nyx_completion_navigation_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.controls, nyx.model,
  nyx.editing, nyx.editing.history, nyx.pascal.names, nyx.pascal.project,
  nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion,
  nyx.pascal.completion.view, nyx.pascal.definition {$ifdef PAS2JS}, Web{$endif};

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
  LDefinitionMenu: INyxPascalDefinitionMenu;
  LDefinitionQuery: INyxPascalDefinitionRequest;
  LDefinitionResult: INyxPascalDefinitionResult;
  LPreparation: INyxPascalCompletionPreparation;
  LRejected: Boolean;

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
  LHistory := NewNyxTextEditHistory(LFrame);
  LMenu := NewNyxPascalCompletionMenu(LHistory);
  Supports(LMenu, INyxPascalDefinitionMenu, LDefinitionMenu);
  Check(LDefinitionMenu <> nil, 'Existing Nyx menu exposes specialized definition inspection');
  LDefinitionQuery := LDefinitionMenu.BeginDefinitionQuery(LSources, LMain, LMain, LTicket);
  LDefinitionResult := NewNyxPascalDefinitionResult(LDefinitionQuery, pdsReady, '', [
    NyxPascalDefinitionTarget('WithText', 'Project method', pckMethod,
      NyxPascalCompletionLocation(LUnit, LNameStart, 9, 'WithText')),
    NyxPascalDefinitionTarget('External', 'SDK declaration', pckMethod,
      Default(TNyxPascalCompletionLocation))], 2);
  Check(LDefinitionMenu.PresentDefinition(LTicket, LDefinitionResult, LSources),
    'Specialized definition result presents under its exact query and ticket');
  LCard := LMenu.Build('definition', LSources);
  Check((LCard.Node.Find('definition-title').Prop('text') = 'Go to definition') and
    (LCard.Node.Find('definition-row-1-external') <> nil),
    'Definition mode identifies its purpose and makes external-source limits visible');
  Check((LMenu.Key(NyxKeyStroke(nkEnterKey), LSources) = pcaDefinition) and
    (LMenu.Route('definition', LCard.Node, LCard.Node.Find('definition-row-0-choose'),
      ntClick, LSources) = pcaDefinition), 'Keyboard and visible row both request navigation');
  Check(LMenu.Route('definition', LCard.Node, LCard.Node.Find('definition-row-1-choose'),
    ntClick, LSources) = pcaNone, 'External declaration cannot invent a local navigation action');
  LRejected := False;
  try
    LMenu.Accept(LSources);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and not LHistory.CanUndo and LHistory.Current.SameValue(LFrame),
    'Definition mode refuses text acceptance without changing the buffer/history');
  Supports(LMenu, INyxPascalCompletionPreparation, LPreparation);
  LRejected := False;
  try
    LPreparation.Preview(LSources);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'Alternate completion-preparation access cannot prepare a definition edit');
  Check(not LMenu.Present(LTicket, NyxPascalDefinitionPage(LDefinitionResult), LSources),
    'Ordinary prefix presentation cannot replace a definition page');
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
