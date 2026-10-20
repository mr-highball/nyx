{ nyx - MIT License, Copyright (c) 2020 mr-highball }
program nyx_caret_definition_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.editing, nyx.scheduler, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.completion,
  nyx.pascal.completion.owned, nyx.pascal.definition
  {$ifdef PAS2JS}, Web{$endif};

const
  CBase: TNyxText = 'unit Consumer; {$mode delphi} interface implementation end.';
  CHeader: TNyxText = 'unit Consumer; {$mode delphi} interface ' +
    'type TBadge=class public Caption:TBadge; &type:TBadge; ' +
    'function WithText:TBadge; ' +
    'function Click(A:TBadge):TBadge; overload; ' +
    'function Click(A:TBadge; B:TBadge):TBadge; overload; end; ' +
    'function Build:TBadge; var Badge:TBadge; Global:TBadge; implementation ';
  CBody: TNyxText = 'procedure Run; var Badge:TBadge; begin ' +
    'Badge.WithText; Badge.Click(Badge); Badge.&type; Build().WithText; ' +
    'Global:=badge; Badge.With; Missing; end; end.';

type
  { Alternate provider deliberately exposes an unrelated query. The boundary
    must refuse it before touching any row, even when the row getter has effects. }
  TForeignResult = class(TInterfacedObject, INyxPascalDefinitionResult)
  public
    Query: INyxPascalDefinitionRequest;
    Reads: Integer;
    function Request: INyxPascalDefinitionRequest;
    function Status: TNyxPascalDefinitionStatus;
    function MessageText: TNyxText;
    function Count: Integer;
    function Total: Integer;
    function Target(AIndex: Integer): TNyxPascalDefinitionTarget;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

function TForeignResult.Request: INyxPascalDefinitionRequest;
begin
  Result := Query;
end;

function TForeignResult.Status: TNyxPascalDefinitionStatus;
begin
  Result := pdsReady;
end;

function TForeignResult.MessageText: TNyxText;
begin
  Result := '';
end;

function TForeignResult.Count: Integer;
begin
  Result := 1;
end;

function TForeignResult.Total: Integer;
begin
  Result := 1;
end;

function TForeignResult.Target(AIndex: Integer): TNyxPascalDefinitionTarget;
begin
  Inc(Reads);
  Result := Default(TNyxPascalDefinitionTarget);
  raise Exception.Create('Foreign result row must not be read');
end;

function Provider(const AOptions: TNyxOwnedPascalCompletionOptions): INyxPascalCompletionProvider;
var
  LContext: INyxPascalCompilerContext;
  LFiles: INyxPascalFileCatalog;
begin
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
    .WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, 30301)
    .CompleteDefines(True);
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('sdk/system.pas'),
      'unit System; {$mode delphi} interface type TObject=class end; ' +
      'IInterface=interface end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/objpas.pas'),
      'unit ObjPas; {$mode objfpc} interface implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/External.pas'),
      'unit External; {$mode delphi} interface type TExternal=class end; implementation end.');
  Result := NewNyxOwnedPascalCompletionProvider(LContext, LFiles,
    [NyxPascalPhysicalFile('sdk/system.pas'), NyxPascalPhysicalFile('sdk/objpas.pas'),
     NyxPascalPhysicalFile('sdk/External.pas')], AOptions);
end;

function Sources(const AText: TNyxText): INyxPascalSourceSnapshot;
begin
  Result := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase))
    .WithBuffer(TNyxPascalSourceRef.Main, AText);
end;

function Query(const ASources: INyxPascalSourceSnapshot; const AAnchor: TNyxText;
  ABack: Integer = 0): INyxPascalDefinitionRequest;
var
  LText: TNyxText;
  LAt: Integer;
begin
  LText := ASources.Text(TNyxPascalSourceRef.Main);
  LAt := Pos(AAnchor, LText);
  Check(LAt > 0, 'Fixture anchor exists in the exact current source');
  Result := NewNyxPascalDefinitionRequest(ASources).At(TNyxPascalSourceRef.Main,
    NyxTextScalarCount(Copy(LText, 1, LAt - 1) + AAnchor) - ABack);
end;

procedure Target(const AResult: INyxPascalDefinitionResult; const AName: TNyxText;
  const AFile: TNyxPascalSourceRef; AStart: Integer);
var
  LTarget: TNyxPascalDefinitionTarget;
begin
  Check((AResult.Status = pdsReady) and (AResult.Count = 1),
    'One exact declared target is ready: ' + AResult.MessageText);
  LTarget := AResult.Target(0);
  Check((LTarget.Name = AName) and LTarget.Location.Available and
    LTarget.Location.FileReference.SameFile(AFile) and (LTarget.Location.Start = AStart),
    'Target keeps its authored name, physical project membership and scalar start');
end;

procedure Run;
var
  LProvider: INyxPascalCompletionProvider;
  LDefinitions: INyxPascalDefinitionProvider;
  LSources, LChanged: INyxPascalSourceSnapshot;
  LQuery: INyxPascalDefinitionRequest;
  LResult, LCopy, LForeignLease: INyxPascalDefinitionResult;
  LForeign: TForeignResult;
  LMain, LUnit, LInclude: TNyxPascalSourceRef;
  LProject: INyxPascalProject;
  LTarget: TNyxPascalDefinitionTarget;
  LAbsent: TNyxPascalCompletionLocation;
  LCancel: INyxCancellationScope;
  LText, LPrefix, LDetails: TNyxText;
  LAt, LFirst, LSecond: Integer;
  LRejected: Boolean;
  LTargets: array of TNyxPascalDefinitionTarget;
begin
  LMain := TNyxPascalSourceRef.Main;
  LUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('Models'));
  LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('members.inc'));
  LAbsent := Default(TNyxPascalCompletionLocation);
  LProvider := Provider(TNyxOwnedPascalCompletionOptions.Defaults);
  Check(Supports(LProvider, INyxPascalDefinitionProvider, LDefinitions),
    'Existing owned providers expose the optional specialized definition contract');
  LSources := Sources(CHeader + CBody);
  LQuery := Query(LSources, 'Badge.WithText', 4);
  LResult := LDefinitions.ResolveDefinition(LQuery);
  Target(LResult, 'WithText', LMain,
    NyxTextScalarCount(Copy(CHeader, 1, Pos('WithText', CHeader) - 1)));
  Check(LResult.Request.Caret = LQuery.Caret, 'Interior caret remains the exact original query');
  LCopy := CaptureNyxPascalDefinitionResult(LResult, LQuery);
  Check((LCopy.Target(0).Location.Start = LResult.Target(0).Location.Start) and
    LCopy.Request.Matches(LSources), 'Detached result retains the exact complete source query');

  { A bare lexical identifier resolves its local declaration, including case
    differences. It must not navigate to the earlier same-named global. }
  LResult := LDefinitions.ResolveDefinition(Query(LSources, 'Global:=badge'));
  LPrefix := CHeader + 'procedure Run; var ';
  Target(LResult, 'Badge', LMain, NyxTextScalarCount(LPrefix));
  LResult := LDefinitions.ResolveDefinition(Query(LSources, 'var Badge', 3));
  Target(LResult, 'Badge', LMain, NyxTextScalarCount(Copy(CHeader, 1, Pos('var Badge', CHeader) + 3)));
  LResult := LDefinitions.ResolveDefinition(Query(LSources, 'Build().WithText'));
  Target(LResult, 'WithText', LMain,
    NyxTextScalarCount(Copy(CHeader, 1, Pos('WithText', CHeader) - 1)));
  LResult := LDefinitions.ResolveDefinition(Query(LSources, 'Badge.&type'));
  Target(LResult, 'type', LMain, NyxTextScalarCount(Copy(CHeader, 1, Pos('&type', CHeader) - 1)));
  Check(LResult.Target(0).Location.Length = 5, 'Escaped declaration span retains the physical ampersand');

  LQuery := Query(LSources, 'Badge.Click');
  LResult := LDefinitions.ResolveDefinition(LQuery);
  Check((LResult.Status = pdsReady) and (LResult.Count = 2) and (LResult.Total = 2),
    'Overload declarations stay alternatives without invented call selection');
  LFirst := LResult.Target(0).Location.Start;
  LSecond := LResult.Target(1).Location.Start;
  Check(LFirst <> LSecond, 'Distinct overloads retain distinct original declaration names');
  LCopy := LDefinitions.ResolveDefinition(LQuery.Take(1));
  Check((LCopy.Count = 1) and (LCopy.Total = 2) and
    (LCopy.Target(0).Location.Start = LFirst), 'First bounded page retains the full alternative count');
  LCopy := LDefinitions.ResolveDefinition(LQuery.Skip(1).Take(1));
  Check((LCopy.Count = 1) and (LCopy.Total = 2) and
    (LCopy.Target(0).Location.Start = LSecond), 'Second page preserves declaration identity');
  LCopy := LDefinitions.ResolveDefinition(LQuery.Skip(2));
  Check((LCopy.Status = pdsReady) and (LCopy.Count = 0) and (LCopy.Total = 2),
    'Past-page query returns an empty page without losing the exact total');
  Check((LQuery.Offset = 0) and (LQuery.Limit = 20), 'Fluent paging never mutates a prior request');

  LResult := LDefinitions.ResolveDefinition(Query(LSources, 'Badge.With;', 1));
  Check((LResult.Status = pdsUnavailable) and (LResult.Count = 0),
    'An incomplete name cannot acquire the longer WithText definition');
  LResult := LDefinitions.ResolveDefinition(Query(LSources, 'Missing'));
  Check((LResult.Status = pdsUnavailable) and (LResult.Count = 0), 'Missing lexical name stays explicit');
  LText := CHeader + 'procedure Run; begin { Badge.WithText } end; end.';
  LResult := LDefinitions.ResolveDefinition(Query(Sources(LText), 'Badge.WithText', 2));
  Check((LResult.Status = pdsUnavailable) and (LResult.Count = 0), 'Comment text is never treated as a reference');
  LText := CHeader + 'procedure Run; begin ''Badge.WithText''; end; end.';
  LResult := LDefinitions.ResolveDefinition(Query(Sources(LText), 'Badge.WithText', 2));
  Check((LResult.Status = pdsUnavailable) and (LResult.Count = 0), 'Literal text is never treated as a reference');
  LText := CHeader + 'procedure Run; begin {$IFDEF NEVER} Badge.WithText; {$ENDIF} end; end.';
  LResult := LDefinitions.ResolveDefinition(Query(Sources(LText), 'Badge.WithText'));
  Check((LResult.Status = pdsUnavailable) and (LResult.Count = 0), 'Inactive source grants no definition authority');

  LPrefix := 'unit Models; {$mode delphi} interface type ';
  LProject := NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase)
    .WithUnit(NyxPascalUnit('Models'), LPrefix + 'TOld=class end; implementation end.');
  LSources := CaptureNyxPascalSources(LProject)
    .WithBuffer(LUnit, LPrefix + 'TBadge=class public {$I members.inc} end; implementation end.');
  { Add the include through project membership before capturing its current data. }
  LText := 'function WithText:TBadge;';
  LProject := LProject.WithInclude(NyxPascalInclude('members.inc'), LText);
  LSources := CaptureNyxPascalSources(LProject)
    .WithBuffer(LUnit, LPrefix + 'TBadge=class public {$I members.inc} end; implementation end.')
    .WithBuffer(LMain, 'unit Consumer; {$mode delphi} interface uses Models; implementation ' +
      'procedure Run; var Badge:TBadge; begin Badge.WithText; Models.TBadge; end; end.');
  LResult := LDefinitions.ResolveDefinition(Query(LSources, 'Models.TBadge'));
  Target(LResult, 'TBadge', LUnit, NyxTextScalarCount(LPrefix));
  LResult := LDefinitions.ResolveDefinition(Query(LSources, 'Badge.WithText'));
  Target(LResult, 'WithText', LInclude, 9);
  LQuery := NewNyxPascalDefinitionRequest(LSources).From(LUnit).At(LInclude, 15);
  LResult := LDefinitions.ResolveDefinition(LQuery);
  Target(LResult, 'WithText', LInclude, 9);
  Check(Pos('TOld', LResult.Request.Sources.AcceptedText(LUnit)) > 0,
    'Definition inspection never replaces accepted helper text with a draft');
  LChanged := LSources.WithBuffer(LUnit, LSources.Text(LUnit) + #13#10);
  Check(not LResult.Request.Matches(LChanged), 'Editing any helper invalidates the original complete query');

  LSources := Sources('unit Consumer; {$mode delphi} interface uses External; implementation ' +
    'procedure Run; var Value:TExternal; begin Value:=Value; end; end.');
  LResult := LDefinitions.ResolveDefinition(Query(LSources, 'Value:TExternal'));
  Check((LResult.Status = pdsReady) and (LResult.Count = 1) and
    (LResult.Target(0).Name = TNyxText('TExternal')) and not LResult.Target(0).Location.Available,
    'SDK declaration is resolved without fabricating project membership');

  LSources := Sources(CHeader + CBody);
  LQuery := Query(LSources, 'Badge.WithText');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LResult := LDefinitions.ResolveDefinition(LQuery, LCancel);
  Check((LResult.Status = pdsCancelled) and (LResult.Count = 0) and (LResult.Total = 0),
    'Cancellation publishes no partial targets');
  LProvider := Provider(TNyxOwnedPascalCompletionOptions.Defaults.UpToReceiverTokens(1));
  Supports(LProvider, INyxPascalDefinitionProvider, LDefinitions);
  LResult := LDefinitions.ResolveDefinition(LQuery);
  Check((LResult.Status = pdsUnavailable) and (LResult.Count = 0),
    'Definition scanning respects the unchanged receiver-work allowance');

  LRejected := False;
  try
    LQuery.Take(51);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'Oversized pages refuse');
  LRejected := False;
  try
    LQuery.Skip(-1);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'Negative offsets refuse');
  LTarget := NyxPascalDefinitionTarget('Forged', '', pckType,
    NyxPascalCompletionLocation(LMain, 0, 6, 'Forged'));
  LRejected := False;
  try
    LResult := NewNyxPascalDefinitionResult(LQuery, pdsReady, '', [LTarget], 1);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LResult = nil), 'Forged source/name target refuses without a provisional result');
  LTarget := NyxPascalDefinitionTarget('WithText', '', pckMethod, LAbsent);
  LRejected := False;
  try
    LResult := NewNyxPascalDefinitionResult(LQuery, pdsCancelled, '', [LTarget], 1);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LResult = nil), 'Non-ready results cannot expose targets');
  LForeign := TForeignResult.Create;
  LForeignLease := LForeign;
  LForeign.Query := LQuery.At(LMain, LQuery.Caret - 1);
  LRejected := False;
  try
    LResult := CaptureNyxPascalDefinitionResult(LForeignLease, LQuery);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LForeign.Reads = 0) and (LResult = nil),
    'Different provider caret refuses before reading alternate target getters');

  LDetails := '';
  for LAt := 1 to 2048 do LDetails := LDetails + TNyxText('🚀');
  SetLength(LTargets, 50);
  for LAt := 0 to High(LTargets) do
  begin
    LTargets[LAt] := NyxPascalDefinitionTarget('WithText', LDetails, pckMethod, LAbsent);
  end;
  LRejected := False;
  try
    LResult := NewNyxPascalDefinitionResult(LQuery.Take(50), pdsReady, '', LTargets, 50);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LResult = nil), 'UTF-8 aggregate budget refuses a large Unicode result atomically');
  LProvider := nil;
  LDefinitions := nil;
  Check(LCopy.Request.Matches(Sources(CHeader + CBody)),
    'Retained detached overload page survives provider retirement');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' caret definition checks';
    {$else}
    WriteLn('PASS ', GChecks, ' caret definition checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
