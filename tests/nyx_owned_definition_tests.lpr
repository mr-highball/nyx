{ nyx - MIT License, Copyright (c) 2020 mr-highball }
program nyx_owned_definition_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.scheduler, nyx.editing, nyx.data, nyx.pascal.names,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.context, nyx.pascal.files, nyx.pascal.lexing,
  nyx.pascal.completion, nyx.pascal.completion.owned, nyx.pascal.completion.wire
  {$ifdef PAS2JS}, Web{$endif};

const
  CBase: TNyxText = 'unit Consumer; {$mode delphi} interface implementation end.';
  CMemberPrefix = '// 🚀 𐐷' + #0 + #13#10 + 'function ';
  CMembers: TNyxText = CMemberPrefix + 'WithText:TBadge;';

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

function Provider: INyxPascalCompletionProvider;
var
  LContext: INyxPascalCompilerContext;
  LFiles: INyxPascalFileCatalog;
  LRoots: array of TNyxPascalPhysicalFile;
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
    .WithFile(NyxPascalPhysicalFile('sdk/Templates.pas'),
      'unit Templates; {$mode delphi} interface type TBadge=class public ' +
      '{$I members.inc} end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/members.inc'), CMembers);
  SetLength(LRoots, 3);
  LRoots[0] := NyxPascalPhysicalFile('sdk/system.pas');
  LRoots[1] := NyxPascalPhysicalFile('sdk/objpas.pas');
  LRoots[2] := NyxPascalPhysicalFile('sdk/Templates.pas');
  Result := NewNyxOwnedPascalCompletionProvider(LContext, LFiles, LRoots,
    TNyxOwnedPascalCompletionOptions.Defaults);
end;

function Query(const ASources: INyxPascalSourceSnapshot;
  const AExpression: TNyxText): INyxPascalCompletionRequest;
var
  LText: TNyxText;
  LAt: Integer;
begin
  LText := ASources.Text(TNyxPascalSourceRef.Main);
  LAt := Pos(AExpression, LText);
  Check(LAt > 0, 'The fixture retains its original member expression');
  Result := NewNyxPascalCompletionRequest(ASources).At(TNyxPascalSourceRef.Main,
    NyxTextScalarCount(Copy(LText, 1, LAt - 1) + AExpression));
end;

function ReadyLocation(const AResult: INyxPascalCompletionResult): TNyxPascalCompletionLocation;
var
  LDefinitions: INyxPascalCompletionDefinitions;
begin
  Check((AResult.Status = pcsReady) and (AResult.Count = 1),
    'Expected one ready project definition: ' + AResult.MessageText);
  Check(Supports(AResult, INyxPascalCompletionDefinitions, LDefinitions),
    'Owned result publishes the optional portable definition capability');
  Result := LDefinitions.DefinitionLocation(0);
end;

procedure CheckProjectTarget(const AResult: INyxPascalCompletionResult;
  const AFile: TNyxPascalSourceRef; AStart, ALength: Integer;
  const AName: TNyxText);
var
  LLocation: TNyxPascalCompletionLocation;
  LReceived: INyxPascalCompletionResult;
begin
  LLocation := ReadyLocation(AResult);
  Check(LLocation.Available and LLocation.FileReference.SameFile(AFile) and
    (LLocation.Start = AStart) and (LLocation.Length = ALength) and
    (LLocation.Name = AName), 'Project definition retains exact physical source/name/scalar span');
  LReceived := DecodeNyxCompletionReply(EncodeNyxCompletionReply(AResult, 'project'),
    'project', AResult.Request);
  LLocation := ReadyLocation(LReceived);
  Check(LLocation.Available and LLocation.FileReference.SameFile(AFile) and
    (LLocation.Start = AStart) and (LLocation.Length = ALength) and
    (LLocation.Name = AName), 'Owned project target survives private transport and detached recapture');
end;

procedure Run;
var
  LProvider: INyxPascalCompletionProvider;
  LProject: INyxPascalProject;
  LSources, LChanged: INyxPascalSourceSnapshot;
  LRequest: INyxPascalCompletionRequest;
  LResult, LReceived: INyxPascalCompletionResult;
  LWithoutLocations: INyxPascalCompletionResult;
  LMain, LUnit, LInclude: TNyxPascalSourceRef;
  LLocation: TNyxPascalCompletionLocation;
  LOwned: INyxOwnedPascalCompletionResult;
  LCancelPort: INyxCancellablePascalCompletionProvider;
  LCancel: INyxCancellationScope;
  LDefinitions: INyxPascalCompletionDefinitions;
  LText, LPrefix, LWire: TNyxText;
  LAt, LCaret: Integer;
  LRejected: Boolean;
begin
  LMain := TNyxPascalSourceRef.Main;
  LUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('Models'));
  LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('members.inc'));
  LProvider := Provider;
  LPrefix := 'unit Consumer; {$mode delphi} interface type TBadge=class public ' +
    '// 🚀 𐐷' + #0 + #13#10 + 'function ';
  LText := LPrefix + 'WithText:TBadge; end; implementation ' +
    'procedure Run; var Badge:TBadge; begin Badge.Wi; end; end.';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase))
    .WithBuffer(LMain, LText);
  LRequest := Query(LSources, 'Badge.Wi');
  LResult := LProvider.Complete(LRequest);
  CheckProjectTarget(LResult, LMain, NyxTextScalarCount(LPrefix), 8, 'WithText');
  Supports(LResult, INyxOwnedPascalCompletionResult, LOwned);
  Check((LOwned <> nil) and
    (LOwned.Definition(0).FileReference.Name = TNyxText('Consumer.pas')) and
    (LOwned.DefinitionSource(0).Source = LText),
    'Portable navigation preserves the existing detailed physical source capability');

  LProject := NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase)
    .WithUnit(NyxPascalUnit('Models'),
      'unit Models; {$mode delphi} interface type TBadge=class public ' +
      'function OldName:TBadge; end; implementation end.');
  LPrefix := 'unit Models; {$mode delphi} interface type TBadge=class public function ';
  LSources := CaptureNyxPascalSources(LProject)
    .WithBuffer(LUnit, LPrefix + 'WithText:TBadge; end; implementation end.')
    .WithBuffer(LMain, 'unit Consumer; {$mode delphi} interface uses Models; implementation ' +
      'procedure Run; var Badge:TBadge; begin Badge.Wi; end; end.');
  LRequest := Query(LSources, 'Badge.Wi');
  LResult := LProvider.Complete(LRequest);
  CheckProjectTarget(LResult, LUnit, NyxTextScalarCount(LPrefix), 8, 'WithText');
  Check(Pos('OldName', LResult.Request.Sources.AcceptedText(LUnit)) > 0,
    'Current declaration navigation does not overwrite the accepted helper source');
  LChanged := LSources.WithBuffer(LUnit, LSources.Text(LUnit) + #13#10);
  Check(not LResult.Request.Matches(LChanged), 'Any helper edit retires the original navigation query');
  LRejected := False;
  try
    LResult.Preview(0, LChanged, LCaret);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LCaret = 0), 'Definition metadata cannot authorize a stale insertion');

  LProject := NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase)
    .WithUnit(NyxPascalUnit('Models'),
      'unit Models; {$mode delphi} interface type TBadge=class public ' +
      '{$I members.inc} end; TSecond=class public {$I members.inc} end; implementation end.')
    .WithInclude(NyxPascalInclude('members.inc'), CMembers);
  LSources := CaptureNyxPascalSources(LProject)
    .WithBuffer(LMain, 'unit Consumer; {$mode delphi} interface uses Models; implementation ' +
      'procedure Run; var Badge:TSecond; begin Badge.Wi; end; end.');
  LResult := LProvider.Complete(Query(LSources, 'Badge.Wi'));
  CheckProjectTarget(LResult, LInclude, NyxTextScalarCount(CMemberPrefix), 8, 'WithText');
  Supports(LResult, INyxOwnedPascalCompletionResult, LOwned);
  Check(LOwned.Definition(0).Occurrence > 1,
    'Repeated includes retain their actual separate in-process occurrence provenance');
  LProvider := nil;
  LLocation := ReadyLocation(CaptureNyxPascalCompletionResult(LResult));
  Check(LLocation.Available and (LResult.Request.Sources.Text(LInclude) = CMembers),
    'Project definitions remain valid after provider retirement without reopening files');

  LProvider := Provider;
  LPrefix := 'unit Consumer; {$mode delphi} interface type TBadge=class public ';
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase))
    .WithBuffer(LMain, LPrefix + '&type:TBadge; end; implementation ' +
      'procedure Run; var Badge:TBadge; begin Badge.ty; end; end.');
  LResult := LProvider.Complete(Query(LSources, 'Badge.ty'));
  CheckProjectTarget(LResult, LMain, NyxTextScalarCount(LPrefix), 5, 'type');

  { A project include with matching leaf/text is still a different physical file
    from sdk/members.inc, so SDK source cannot gain project-local authority. }
  LProject := NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase)
    .WithInclude(NyxPascalInclude('members.inc'), CMembers);
  LSources := CaptureNyxPascalSources(LProject)
    .WithBuffer(LMain, 'unit Consumer; {$mode delphi} interface uses Templates; implementation ' +
      'procedure Run; var Badge:TBadge; begin Badge.Wi; end; end.');
  LRequest := Query(LSources, 'Badge.Wi');
  LResult := LProvider.Complete(LRequest);
  LLocation := ReadyLocation(LResult);
  Check(not LLocation.Available,
    'An SDK include with the same project leaf/text has no fabricated project selector');
  Supports(LResult, INyxOwnedPascalCompletionResult, LOwned);
  Check(LOwned.Definition(0).FileReference.Name = TNyxText('sdk/members.inc'),
    'External SDK detailed provenance remains available to explicit in-process consumers');
  LWire := EncodeNyxCompletionReply(LResult, 'sdk');
  LWithoutLocations := NewNyxPascalCompletionResult(LRequest, LResult.Status,
    LResult.MessageText, LResult.ReplaceStart, LResult.ExpectedText,
    [LResult.Item(0)], LResult.Total);
  Check((TNyxDataValue.ParseJSON(LWire).Field('version').AsInteger = 1) and
    (LWire = EncodeNyxCompletionReply(LWithoutLocations, 'sdk')),
    'SDK-only definitions retain exact legacy reply bytes without transporting SDK source');
  LReceived := DecodeNyxCompletionReply(LWire, 'sdk', LRequest);
  Check((LReceived.Count = 1) and (LReceived.Item(0).Name = TNyxText('WithText')),
    'Omitting external targets preserves useful ordinary completion suggestions');
  Supports(LProvider, INyxCancellablePascalCompletionProvider, LCancelPort);
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LResult := LCancelPort.CompleteWithCancellation(LRequest, LCancel);
  Check((LResult.Status = pcsCancelled) and (LResult.Count = 0) and (LResult.Total = 0),
    'Cancelled owned analysis publishes no partial insertion or definition rows');
  LRejected := False;
  try
    Check(Supports(LResult, INyxPascalCompletionDefinitions, LDefinitions),
      'Cancelled owned results preserve their optional capability without rows');
    LAt := LDefinitions.DefinitionLocation(0).Start;
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'A cancelled result cannot be consumed as a ready definition');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' owned definition checks';
    {$else}
    WriteLn('PASS ', GChecks, ' owned definition checks');
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
