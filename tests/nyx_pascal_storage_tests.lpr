program nyx_pascal_storage_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.scheduler, nyx.pascal.storage,
  nyx.pascal.lexing, nyx.pascal.work
  {$ifndef PAS2JS}, Classes, nyx.text, Process, LazFileUtils{$endif};

type
  { Mutable alternate fixture. Capture must copy it, detect self-cycles and
    reject invalid child counts without borrowing this object after return. }
  TProvider = class(TInterfacedObject, INyxPascalStorageShape)
  public
    Values: array of INyxPascalStorageShape;
    Reads: Integer;
    function Kind: TNyxPascalStorageKind;
    function Primitive: TNyxPascalStoragePrimitive;
    function Form: TNyxPascalStorageForm;
    function Alignment: TNyxPascalStorageAlignment;
    function ArrayLength: Integer;
    function Count: Integer;
    function Child(AIndex: Integer): INyxPascalStorageShape;
  end;
  { Deterministic cancellation at each observed planning boundary. The fixture
    holds no plan/provider; published getters must never call it afterward. }
  TCancelOnCheck = class(TInterfacedObject, INyxCancellationScope)
  public
    Calls, AtCall: Integer;
    Stopped: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
end;

function TProvider.Kind: TNyxPascalStorageKind;
begin Inc(Reads); Result := pskRecord; end;
function TProvider.Primitive: TNyxPascalStoragePrimitive;
begin Result := pspByte; raise Exception.Create('Unused primitive getter'); end;
function TProvider.Form: TNyxPascalStorageForm;
begin Inc(Reads); Result := psfOrdinary; end;
function TProvider.Alignment: TNyxPascalStorageAlignment;
begin Inc(Reads); Result := psaDefault; end;
function TProvider.ArrayLength: Integer;
begin Result := 0; raise Exception.Create('Unused array getter'); end;
function TProvider.Count: Integer;
begin Inc(Reads); Result := Length(Values); end;
function TProvider.Child(AIndex: Integer): INyxPascalStorageShape;
begin Inc(Reads); Result := Values[AIndex]; end;

procedure TCancelOnCheck.Cancel;
begin Stopped := True; end;
function TCancelOnCheck.GetCancelled: Boolean;
begin
  Inc(Calls);
  Result := Stopped or ((AtCall > 0) and (Calls >= AtCall));
end;

function Pair: INyxPascalStorageRecord;
begin
  Result := NewNyxPascalStorageRecord.Field(NewNyxPascalStoragePrimitive(pspByte),
    TNyxPascalStorageOptions.Defaults).Field(NewNyxPascalStoragePrimitive(pspLongint),
    TNyxPascalStorageOptions.Defaults);
end;

procedure Shapes;
var
  LEmpty, LFirst, LPair: INyxPascalStorageRecord;
  LByte, LArray: INyxPascalStorageShape;
  LRecord: INyxPascalStorageRecord;
  LPlan, LOther: INyxPascalStoragePlan;
  LFailed: Boolean;
  LAt: Integer;
begin
  LEmpty := NewNyxPascalStorageRecord;
  LByte := NewNyxPascalStoragePrimitive(pspByte);
  LFirst := LEmpty.Field(LByte, TNyxPascalStorageOptions.Defaults);
  LPair := LFirst.Field(NewNyxPascalStoragePrimitive(pspLongint), TNyxPascalStorageOptions.Defaults);
  Check((LEmpty.Count = 0) and (LFirst.Count = 1) and (LPair.Count = 2),
    'Fluent field additions preserve every earlier record');
  Check(not Supports(LByte, INyxPascalStorageRecord, LRecord),
    'A scalar exposes no misleading record capability');
  LArray := NewNyxPascalStorageArray(LPair, 7, psfOrdinary, TNyxPascalStorageOptions.Defaults);
  Check((LArray.ArrayLength = 7) and (LArray.Count = 1) and
    not Supports(LArray, INyxPascalStorageRecord, LRecord), 'An array owns one element and no record capability');
  LPlan := PlanNyxPascalStorage(LPair, TNyxPascalStorageOptions.Defaults);
  LOther := PlanNyxPascalStorage(Pair, TNyxPascalStorageOptions.Defaults);
  Check((LPlan.Key = LOther.Key) and (LPlan.Declarations = LOther.Declarations) and
    (LPlan.NodeCount = 3), 'Independent equal shapes retain exact structural keys and sterile declarations');
  LOther := PlanNyxPascalStorage(NewNyxPascalStorageRecord(psfPacked)
    .Field(LByte, TNyxPascalStorageOptions.Defaults)
    .Field(NewNyxPascalStoragePrimitive(pspLongint), TNyxPascalStorageOptions.Defaults),
    TNyxPascalStorageOptions.Defaults);
  Check(LPlan.Key <> LOther.Key, 'Authored packing is part of shape identity');
  for LAt := 0 to 4 do
  begin
    LFailed := False;
    LOther := nil;
    try
      case LAt of
        0: LOther := PlanNyxPascalStorage(LPair, TNyxPascalStorageOptions.Defaults.UpToNodes(2));
        1: LOther := PlanNyxPascalStorage(LArray, TNyxPascalStorageOptions.Defaults.UpToDepth(1));
        2: LOther := PlanNyxPascalStorage(LPair,
          TNyxPascalStorageOptions.Defaults.UpToBytes(Length(LPlan.Declarations) - 1));
        3: LArray := NewNyxPascalStorageArray(LByte, 0, psfOrdinary, TNyxPascalStorageOptions.Defaults);
        4: LPair.Child(-1);
      else
        raise Exception.Create('Unknown fixture case');
      end;
    except
      on ENyxPascalStorage do LFailed := True;
    end;
    Check(LFailed and (LOther = nil) and (LPlan.Shape.Count = 2),
      'Node/depth/source/shape refusal publishes no partial plan or earlier mutation');
  end;
  LPair := nil;
  LEmpty := nil;
  LFirst := nil;
  Check((LPlan.Shape.Count = 2) and (LPlan.TypeName <> ''), 'Published plans retain their detached value tree');
end;

procedure Providers;
var
  LProvider: TProvider;
  LLease, LShape: INyxPascalStorageShape;
  LRecord: INyxPascalStorageRecord;
  LPlan: INyxPascalStoragePlan;
  LScope: INyxCancellationScope;
  LFailed: Boolean;
  LReads: Integer;
begin
  LProvider := TProvider.Create;
  LLease := LProvider;
  SetLength(LProvider.Values, 1);
  LProvider.Values[0] := NewNyxPascalStoragePrimitive(pspLongint);
  LShape := CaptureNyxPascalStorageShape(LLease, TNyxPascalStorageOptions.Defaults);
  LReads := LProvider.Reads;
  LProvider.Values[0] := NewNyxPascalStoragePrimitive(pspByte);
  LPlan := PlanNyxPascalStorage(LShape, TNyxPascalStorageOptions.Defaults);
  Check((LProvider.Reads = LReads) and (LShape.Child(0).Primitive = pspLongint) and
    Supports(LShape, INyxPascalStorageRecord, LRecord),
    'Alternate capture detaches values and reconstructs the actual specialized capability');
  LProvider.Values[0] := LLease;
  try
    LFailed := False;
    try
      CaptureNyxPascalStorageShape(LLease, TNyxPascalStorageOptions.Defaults);
    except
      on ENyxPascalStorage do LFailed := True;
    end;
    Check(LFailed, 'Alternate provider cycles refuse before publication');
  finally
    LProvider.Values[0] := nil;
  end;
  LScope := NewNyxCancellationScope;
  LScope.Cancel;
  LReads := LProvider.Reads;
  LFailed := False;
  try
    CaptureNyxPascalStorageShape(LLease, TNyxPascalStorageOptions.Defaults, LScope);
  except
    on ENyxPascalAnalysisCancelled do LFailed := True;
  end;
  Check(LFailed and (LProvider.Reads = LReads), 'Entry cancellation calls no provider getter');
  LLease := nil;
  Check(LPlan.Shape.Child(0).Primitive = pspLongint, 'Provider retirement cannot change a plan');
end;

procedure Cancellation;
var
  LShape: INyxPascalStorageShape;
  LPlan, LRejected: INyxPascalStoragePlan;
  LScope: TCancelOnCheck;
  LLease: INyxCancellationScope;
  LCalls, LAt: Integer;
  LFailed: Boolean;
begin
  LShape := NewNyxPascalStorageArray(Pair, 9, psfPacked, TNyxPascalStorageOptions.Defaults);
  LScope := TCancelOnCheck.Create;
  LLease := LScope;
  LPlan := PlanNyxPascalStorage(LShape, TNyxPascalStorageOptions.Defaults, LLease);
  LCalls := LScope.Calls;
  LScope.Cancel;
  Check((LPlan.Declarations <> '') and (LPlan.Key <> '') and
    (LPlan.Shape.ArrayLength = 9) and (LScope.Calls = LCalls),
    'Published getters prepare no work and retain no cancellation callback');
  for LAt := 1 to LCalls do
  begin
    LScope := TCancelOnCheck.Create;
    LLease := LScope;
    LScope.AtCall := LAt;
    LRejected := nil;
    LFailed := False;
    try
      LRejected := PlanNyxPascalStorage(LShape, TNyxPascalStorageOptions.Defaults, LLease);
    except
      on ENyxPascalAnalysisCancelled do LFailed := True;
    end;
    Check(LFailed and (LRejected = nil) and (LPlan.Shape.ArrayLength = 9),
      'Cancellation at every actual boundary leaves the earlier shape/plan intact');
  end;
  WriteLn('storage plan cancellation boundaries / ', LCalls);
end;

procedure Admission;
var
  LShape: INyxPascalStorageShape;
  LPlan, LRejected: INyxPascalStoragePlan;
  LBudget: INyxPascalAnalysisBudget;
  LTotal, LAt: Integer;
  LFailed: Boolean;
begin
  LShape := Pair;
  LBudget := NewNyxPascalAnalysisBudget(1000);
  try
    LPlan := PlanNyxPascalStorage(LShape, TNyxPascalStorageOptions.Defaults, LBudget);
    LTotal := LBudget.Receipt.Total;
    Check((LTotal > 0) and (LBudget.Receipt.Used(papTypeProof) > 0) and
      (LBudget.Receipt.Used(papTypeToken) >= LPlan.NodeCount), 'Real proof/emission reads retain explicit admission');
  finally
    LBudget.Retire;
  end;
  for LAt := 1 to LTotal do
  begin
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    LRejected := nil;
    LFailed := False;
    try
      try
        LRejected := PlanNyxPascalStorage(LShape, TNyxPascalStorageOptions.Defaults, LBudget);
      except
        on ENyxPascalAnalysisWorkLimit do LFailed := True;
      end;
      Check(((LAt < LTotal) and LFailed and (LRejected = nil)) or
        ((LAt = LTotal) and not LFailed and (LRejected.Key = LPlan.Key)),
        'Every smaller planning allowance refuses atomically; the exact fit retains its immutable input');
    finally
      LBudget.Retire;
    end;
  end;
  WriteLn('storage plan admitted units / ', LTotal, ' / smaller refusals / ', LTotal - 1);
end;

{$ifndef PAS2JS}
procedure CompilerOracle;
var
  LPath, LCompiler, LRoot: String;
  LSource, LControls: TNyxText;
  LPlan: INyxPascalStoragePlan;
  LShape: INyxPascalStorageShape;
  LInner: INyxPascalStorageRecord;
  LAt: Integer;
  LFile: TStringList;
  LProcess: TProcess;
  LCompilerLog: TMemoryStream;
  LBuffer: array[0..8191] of Byte;
  LRead: Integer;
  LStarted: QWord;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  Check(ParamCount = 2, 'Supply compiler and a fresh private oracle directory');
  LCompiler := ParamStr(1);
  LRoot := ExpandFileName(ParamStr(2));
  Check(FileExistsUTF8(LCompiler) and not DirectoryExistsUTF8(LRoot) and CreateDirUTF8(LRoot),
    'The oracle owns fresh output and an explicitly selected compiler');
  LControls := 'type'#10 +
    '  TPlain = record Lead:Byte; Value:Longint; end;'#10 +
    '  TPacked = packed record Lead:Byte; Value:Longint; end;'#10 +
    '  TBits = bitpacked record Left:Boolean; Right:Boolean; end;'#10 +
    '  TEmpty = record end;'#10 +
    '  TArray = array[-4..2] of TPlain;'#10 +
    '{$push}{$packrecords c}'#10 +
    '  TC = record Lead:Byte; Value:Longint; end;'#10'{$pop}'#10 +
    '{$push}{$packrecords 2}'#10 +
    '  TNested = record Inner:TPacked; Tail:Longint; end;'#10'{$pop}'#10;
  LSource := 'program StorageOracle;'#10'{$mode delphi}{$codepage utf8}'#10 + LControls;
  for LAt := 0 to 6 do
  begin
    case LAt of
      0: begin LShape := Pair; LPath := 'TPlain'; end;
      1:
        begin
          LShape := NewNyxPascalStorageRecord(psfPacked)
            .Field(NewNyxPascalStoragePrimitive(pspByte), TNyxPascalStorageOptions.Defaults)
            .Field(NewNyxPascalStoragePrimitive(pspLongint), TNyxPascalStorageOptions.Defaults);
          LPath := 'TPacked';
        end;
      2:
        begin
          LShape := NewNyxPascalStorageRecord(psfBitPacked)
            .Field(NewNyxPascalStoragePrimitive(pspBoolean), TNyxPascalStorageOptions.Defaults)
            .Field(NewNyxPascalStoragePrimitive(pspBoolean), TNyxPascalStorageOptions.Defaults);
          LPath := 'TBits';
        end;
      3: begin LShape := NewNyxPascalStorageRecord; LPath := 'TEmpty'; end;
      4:
        begin
          LShape := NewNyxPascalStorageArray(Pair, 7, psfOrdinary, TNyxPascalStorageOptions.Defaults);
          LPath := 'TArray';
        end;
      5:
        begin
          LShape := NewNyxPascalStorageRecord(psfOrdinary, psaC)
            .Field(NewNyxPascalStoragePrimitive(pspByte), TNyxPascalStorageOptions.Defaults)
            .Field(NewNyxPascalStoragePrimitive(pspLongint), TNyxPascalStorageOptions.Defaults);
          LPath := 'TC';
        end;
      6:
        begin
          LInner := NewNyxPascalStorageRecord(psfPacked)
            .Field(NewNyxPascalStoragePrimitive(pspByte), TNyxPascalStorageOptions.Defaults)
            .Field(NewNyxPascalStoragePrimitive(pspLongint), TNyxPascalStorageOptions.Defaults);
          LShape := NewNyxPascalStorageRecord(psfOrdinary, psa2)
            .Field(LInner, TNyxPascalStorageOptions.Defaults)
            .Field(NewNyxPascalStoragePrimitive(pspLongint), TNyxPascalStorageOptions.Defaults);
          LPath := 'TNested';
        end;
    else
      raise Exception.Create('Unknown oracle case');
    end;
    LPlan := PlanNyxPascalStorage(LShape, TNyxPascalStorageOptions.Defaults);
    LSource := LSource + 'procedure Case' + TNyxText(IntToStr(LAt)) + ';'#10 +
      'type'#10 + LPlan.Declarations + '{$if sizeof(' + LPlan.TypeName + ') <> sizeof(' +
      TNyxText(LPath) + ')}{$fatal storage comparison failed}{$endif}'#10'begin end;'#10;
  end;
  LSource := LSource + 'begin end.'#10;
  LFile := TStringList.Create;
  LProcess := TProcess.Create(nil);
  LCompilerLog := TMemoryStream.Create;
  try
    LFile.Text := String(LSource);
    LPath := IncludeTrailingPathDelimiter(LRoot) + 'storage_oracle.lpr';
    LFile.SaveToFile(LPath);
    LProcess.Executable := LCompiler;
    LProcess.Parameters.Add('-Mdelphi');
    LProcess.Parameters.Add('-FE' + LRoot);
    LProcess.Parameters.Add('-FU' + LRoot);
    LProcess.Parameters.Add(LPath);
    LProcess.Options := [poUsePipes, poStderrToOutPut];
    LProcess.Execute;
    LStarted := GetTickCount64;
    repeat

      if LProcess.Output.NumBytesAvailable > 0 then
      begin
        LRead := LProcess.Output.Read(LBuffer, SizeOf(LBuffer));

        if LRead > 1048576 - LCompilerLog.Size then
        begin
          raise Exception.Create('Storage oracle compiler output exceeded its bound');
        end;
        LCompilerLog.WriteBuffer(LBuffer, LRead);
      end
      else if LProcess.Running then
      begin
        Sleep(10);
      end;

      if LProcess.Running and (GetTickCount64 - LStarted > 30000) then
      begin
        raise Exception.Create('Storage oracle compiler exceeded its deadline');
      end;
    until not LProcess.Running and (LProcess.Output.NumBytesAvailable = 0);
    LProcess.WaitOnExit;
    LCompilerLog.SaveToFile(IncludeTrailingPathDelimiter(LRoot) + 'compiler.log');
    Check(LProcess.ExitStatus = 0,
      'Independent source records equal generated shape sizes in the configured compiler');
    WriteLn('storage compiler oracle / seven independent shapes / compiled only');
  finally

    if LProcess.Running then
    begin
      LProcess.Terminate(1);
      LProcess.WaitOnExit;
    end;
    LCompilerLog.Free;
    LProcess.Free;
    LFile.Free;
  end;
end;
{$endif}

begin
  try
    Shapes;
    Providers;
    Cancellation;
    Admission;
    {$ifndef PAS2JS}CompilerOracle;{$endif}
    WriteLn('PASS ', GChecks, ' source-free storage planning checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      ExitCode := 1;
    end;
  end;
end.
