{ Exact arithmetic vectors, budget/cancellation and concurrent immutable readers
  qualify the shared mathematical owner. An independent whole-expression native
  oracle observes actual compiler types and bytes under five source policies.
  Browser builds compile the portable math contract; execution is separate. }
program nyx_pascal_real_operation_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.reals,
  nyx.pascal.values, nyx.pascal.work, nyx.pascal.lexing
  {$ifndef PAS2JS}, real.operations.fixture{$endif};

type
  { Detached work input, shared only after construction. No query, mutable
    arithmetic workspace or interface field is stored in this value record. }
  TOperationInput = record
    Left, Right: TNyxPascalBinaryReal;
  end;
{$ifndef PAS2JS}
  { Each worker owns one budget and all work integers. It reads the copied
    values and publishes only an error string after the main thread joins it. }
  TOperationReader = class(TThread)
  public
    Input: TOperationInput;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

var GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Round(const AText: TNyxText;
  APrecision: TNyxPascalBinaryPrecision = pbpX87Extended80): TNyxPascalBinaryReal;
var LSource: TNyxPascalRealValue;
begin

  if (AText <> '') and (AText[1] = '-') then
    LSource := NyxPascalRealToken(Copy(AText,2,Length(AText)-1)).Negated
  else LSource := NyxPascalRealToken(AText);
  Result := NyxPascalRoundRealToEven(LSource,APrecision);
end;

function Operation(AKind: TNyxPascalRealOperation; const ALeft, ARight: TNyxText;
  APrecision: TNyxPascalBinaryPrecision): TNyxPascalBinaryReal;
begin
  Result := NyxPascalCalculateRealToEven(AKind,Round(ALeft),Round(ARight),APrecision);
end;

procedure Vectors;
var LFormat: TNyxPascalBinaryPrecision; LValue, LLeft, LRight: TNyxPascalBinaryReal;
  LOperation: TNyxPascalRealOperation; LRejected: Boolean; LInvalid: Integer;
begin
  for LFormat := pbpBinary32 to pbpBinary128 do
  begin
    LValue := Operation(proAdd,'1.25','2.5',LFormat);
    Check(LValue.SameValue(Round('3.75',LFormat)) and not LValue.Inexact,'Exact addition');
    LValue := Operation(proSubtract,'1.25','2.5',LFormat);
    Check(LValue.SameValue(Round('-1.25',LFormat)) and not LValue.Inexact,'Signed exact subtraction');
    LValue := Operation(proMultiply,'1.25','2.5',LFormat);
    Check(LValue.SameValue(Round('3.125',LFormat)) and not LValue.Inexact,'Exact multiplication');
    LValue := Operation(proDivide,'1.0','10.0',LFormat);
    Check(LValue.SameValue(Round('0.1',LFormat)) and LValue.Inexact,'Recurring rational quotient');
    Check(Operation(proAdd,'9.5','-1.25',LFormat).SameValue(Round('8.25')),
      'Opposite signs retain the larger left magnitude');
    Check(Operation(proAdd,'-1.25','9.5',LFormat).SameValue(Round('8.25')),
      'Opposite signs retain the larger right magnitude');
    Check(Operation(proAdd,'-9.5','1.25',LFormat).SameValue(Round('-8.25')),
      'Negative larger magnitude retains its sign');
    Check(Operation(proAdd,'-1.25','-2.5',LFormat).SameValue(Round('-3.75')),
      'Nonzero negative magnitudes add without losing sign');
    LValue := Operation(proSubtract,'1.25','1.25',LFormat);
    Check(LValue.IsZero and not LValue.Negative and not LValue.Inexact,'Exact cancellation is positive zero');
    LValue := Operation(proAdd,'-0.0','-0.0',LFormat);
    Check(LValue.IsZero and LValue.Negative,'Equal negative zeros add to negative zero');
    LValue := Operation(proSubtract,'-0.0','-0.0',LFormat);
    Check(LValue.IsZero and not LValue.Negative,'Opposite effective zero signs cancel');
    LValue := Operation(proMultiply,'-0.0','2.0',LFormat);
    Check(LValue.IsZero and LValue.Negative,'Multiplication retains signed zero');
    LValue := Operation(proDivide,'-0.0','-2.0',LFormat);
    Check(LValue.IsZero and not LValue.Negative,'Division combines signs for zero');
  end;
  LValue := Operation(proAdd,'16777216.0','1.0',pbpBinary32);
  Check(LValue.SameValue(Round('16777216.0')) and LValue.Inexact,'Even single tie rounds down');
  LValue := Operation(proAdd,'16777218.0','1.0',pbpBinary32);
  Check(LValue.SameValue(Round('16777220.0')) and LValue.Inexact,'Odd single tie carries up');
  LLeft := Round('10384593717069655257060992658440192.0',pbpBinary128);
  LValue := NyxPascalCalculateRealToEven(proAdd,LLeft,Round('1.0'),pbpBinary128);
  Check((LValue.Significand = '1') and (LValue.BinaryExponent = 113) and LValue.Inexact,
    'Binary128 arithmetic retains an exact even significand beyond native integer/float precision');
  LValue := Operation(proMultiply,'340282346638528859811704183484516925440.0','2.0',pbpBinary32);
  Check((LValue.Status = rrsOverflow) and LValue.Inexact,'Arithmetic overflow is not a finite infinity');
  LLeft := Round('1.0e-45',pbpBinary32); LRight := Round('2.0');
  LValue := NyxPascalCalculateRealToEven(proDivide,LLeft,LRight,pbpBinary32);
  Check(LValue.IsZero and not LValue.Negative and LValue.Inexact,'Half the smallest subnormal is even zero');
  LValue := NyxPascalCalculateRealToEven(proDivide,LLeft.Negated,LRight,pbpBinary32);
  Check(LValue.IsZero and LValue.Negative and LValue.Inexact,'Subnormal underflow retains its sign');
  LValue := NyxPascalCalculateRealToEven(proMultiply,LLeft,Round('3.0'),pbpBinary32);
  LValue := NyxPascalCalculateRealToEven(proDivide,LValue,LRight,pbpBinary32);
  Check((LValue.Significand = '1') and (LValue.BinaryExponent = -148) and LValue.Inexact,
    'A subnormal odd tie rounds to its even neighbor');
  LValue := Operation(proDivide,'-1.0','0.0',pbpBinary64);
  Check((LValue.Status = rrsDivisionByZero) and LValue.Negative and
    (LValue.Significand = ''),'Nonzero division by zero carries no constructed value');
  LValue := Operation(proDivide,'0.0','-0.0',pbpBinary64);
  Check(LValue.Status = rrsInvalidOperation,'Zero divided by zero is a distinct mathematical failure');
  for LOperation := Low(TNyxPascalRealOperation) to High(TNyxPascalRealOperation) do
    Check(NyxPascalCalculateRealToEven(LOperation,TNyxPascalBinaryReal.Unknown,Round('1.0'),
      pbpBinary32).Status = rrsUnknown,'Unknown operands grant no operation proof');
  Check(Operation(proAdd,'1.0','2.0',pbpUnknown).Status = rrsUnknown,'Unknown result format stays unknown');
  LLeft := Round('0.1'); LValue := LLeft.Negated;
  Check(LLeft.Inexact and not LValue.Inexact and LValue.Negative and
    LValue.Negated.SameValue(LLeft),'Unary negation is exact without mutating the earlier value');
  LValue := TNyxPascalBinaryReal.Unknown; LRejected := False; LInvalid := 99;
  try LValue := NyxPascalCalculateRealToEven(TNyxPascalRealOperation(LInvalid),LLeft,LRight,pbpBinary32);
  except on ENyxPascalValue do LRejected := True; end;
  Check(LRejected and (LValue.Status = rrsUnknown),'Out-of-domain operations refuse atomically');
end;

procedure Comparisons;
const CValues: array[0..8] of TNyxText = ('-1.0e300','-2.5','-0.1','-0.0','0.0',
  '0.1','1.25','2.5','1.0e300');
var LLeft, LRight: Integer; LOrder, LExpected: Integer;
  LFormat: TNyxPascalBinaryPrecision; LValues: array of TNyxPascalBinaryReal;
begin
  for LFormat := pbpBinary64 to pbpBinary128 do
  begin
    SetLength(LValues,Length(CValues));
    for LLeft := 0 to High(CValues) do LValues[LLeft] := Round(CValues[LLeft],LFormat);
    for LLeft := 0 to High(CValues) do
      for LRight := 0 to High(CValues) do
      begin
        LExpected := 0;

        if LLeft < LRight then LExpected := -1
        else if LLeft > LRight then LExpected := 1;

        if (LLeft in [3,4]) and (LRight in [3,4]) then LExpected := 0;
        Check(LValues[LLeft].TryCompare(LValues[LRight],LOrder) and (LOrder = LExpected),
          'Finite comparisons retain sign, order, equality and signed zero');
      end;
  end;
  Check(Round('1.25',pbpBinary32).TryCompare(Round('1.25',pbpBinary128),LOrder) and
    (LOrder = 0),'Represented values compare independently of format');
  Check(Round('0.1',pbpBinary32).TryCompare(Round('0.1',pbpBinary64),LOrder) and
    (LOrder > 0),'Different rounded values are not equal just because their source spelling matches');
  LOrder := 99;
  Check(not TNyxPascalBinaryReal.Unknown.TryCompare(Round('1.0'),LOrder) and
    (LOrder = 0),'An unknown comparison publishes no relation');
end;

procedure WorkContract;
var LInput: TOperationInput; LValue, LRetained: TNyxPascalBinaryReal;
  LBudget: INyxPascalAnalysisBudget; LCancel: INyxCancellationScope;
  LAt, LFit, LOrder: Integer; LRejected: Boolean;
begin
  LInput.Left := Round('1.0'); LInput.Right := Round('3.0'); LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LValue := TNyxPascalBinaryReal.Unknown; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LValue := NyxPascalCalculateRealToEven(proDivide,LInput.Left,LInput.Right,pbpX87Extended80,LBudget);
        Check(LValue.SameValue(Operation(proDivide,'1.0','3.0',pbpX87Extended80)),
          'The first sufficient allowance publishes the whole result');
        LFit := LAt; LRetained := LValue;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LValue.Status = rrsUnknown) and (LBudget.Receipt.Total <= LAt),
            'Every smaller allowance refuses without a partial arithmetic result');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (LRetained.RoundToEven(pbpBinary64).Status = rrsFinite),
    'Detached arithmetic values retain no retired work lease');
  WriteLn('real operation work / first fit=',LFit,' every smaller allowance refused');
  LInput.Left := Round('0.1',pbpBinary32); LInput.Right := Round('0.1',pbpBinary64); LFit := -1;
  for LAt := 1 to 100 do
  begin
    LOrder := 99; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        Check(LInput.Left.TryCompare(LInput.Right,LOrder,LBudget) and (LOrder > 0),
          'The first sufficient comparison allowance publishes the complete relation');
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do Check((LOrder = 0) and (LBudget.Receipt.Total <= LAt),
          'Every smaller comparison allowance refuses without a partial relation');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check(LFit > 1,'A finite significand comparison charges its actual digit work');
  WriteLn('real comparison work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope; LCancel.Cancel;
  LValue := TNyxPascalBinaryReal.Unknown; LRejected := False;
  try LValue := NyxPascalCalculateRealToEven(proAdd,LInput.Left,LInput.Right,pbpBinary32,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LValue.Status = rrsUnknown),'Ordinary cancellation publishes no arithmetic value');
  LOrder := 99; LRejected := False;
  try LInput.Left.TryCompare(LInput.Right,LOrder,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LOrder = 0),'Ordinary cancellation publishes no comparison relation');
end;

{$ifndef PAS2JS}
function Describe(const AValue: TNyxPascalBinaryReal): TNyxText;
begin
  Result := '0';

  if AValue.Negative then Result := '1';
  Result := Result+'|'+AValue.Significand+'|'+TNyxText(IntToStr(AValue.BinaryExponent));
end;

function TypeFormat(const AValue: TNyxPascalBinaryReal;
  AMinimum: TNyxPascalBinaryPrecision): TNyxPascalBinaryPrecision;
begin
  Result := pbpX87Extended80;

  if AValue.RoundToEven(AMinimum).SameValue(AValue) then Result := AMinimum;
end;

procedure NativeOracle;
var LRows, LParts: TStringList; LRow, LOp, LOrder: Integer; LExpected, LType: TNyxText;
  LLeft, LRight, LValue: TNyxPascalBinaryReal;
  LMinimum, LFormat, LOther: TNyxPascalBinaryPrecision; LRelation: Boolean;
begin
  LRows := TStringList.Create; LParts := TStringList.Create;
  try
    LRows.Text := String(NativeRealOperationObservations);
    Check((LRows.Count = 115) and (NativeRealOperationCalls = 115),
      'All 23 whole expressions run under all five authored policy sites');
    LParts.StrictDelimiter := True; LParts.Delimiter := '|';
    for LRow := 0 to LRows.Count-1 do
    begin
      LParts.DelimitedText := LRows[LRow];
      Check(LParts.Count >= 7,'The independent expression observation is complete');
      LOp := StrToInt(LParts[2]); LLeft := Round(TNyxText(LParts[3])); LRight := Round(TNyxText(LParts[4]));
      LMinimum := pbpBinary32;

      if Copy(LParts[0],1,2) = '64' then LMinimum := pbpBinary64;
      LFormat := TypeFormat(LLeft,LMinimum); LOther := TypeFormat(LRight,LMinimum);

      if Ord(LOther) > Ord(LFormat) then LFormat := LOther;

      if Pos('-on-',LParts[0]) > 0 then LFormat := pbpX87Extended80;

      if LOp < 4 then
      begin
        Check(LParts.Count = 9,'A real expression retains the full native byte observation');
        LValue := NyxPascalCalculateRealToEven(TNyxPascalRealOperation(LOp),LLeft,LRight,pbpX87Extended80);
        LValue := LValue.RoundToEven(LFormat); LExpected := Describe(LValue);
        LType := 'Extended';

        if LFormat = pbpBinary32 then LType := 'Single'
        else if LFormat = pbpBinary64 then LType := 'Double';
        WriteLn('native real operation / ',LRows[LRow]);
        Check((TNyxText(LParts[5]) = LType) and
          (LExpected = TNyxText(LParts[6]+'|'+LParts[7]+'|'+LParts[8])),
          'Compiler expression type/bytes differ: '+LParts[0]+'/'+LParts[1]);
      end
      else
      begin
        Check(LLeft.TryCompare(LRight,LOrder),'Finite oracle inputs supply a comparison');
        LRelation := False;
        case LOp of
          4: LRelation := LOrder = 0;
          5: LRelation := LOrder <> 0;
          6: LRelation := LOrder < 0;
          7: LRelation := LOrder <= 0;
          8: LRelation := LOrder > 0;
          9: LRelation := LOrder >= 0;
        else raise Exception.Create('Unknown native relation observation'); end;
        Check((LParts.Count = 7) and (LParts[5] = 'Boolean') and
          (StrToInt(LParts[6]) = Ord(LRelation)),'Actual compiler comparison agrees with represented finite values');
      end;
    end;
  finally LParts.Free; LRows.Free; end;
end;

procedure TOperationReader.Execute;
var LBudget: INyxPascalAnalysisBudget; LValue: TNyxPascalBinaryReal;
begin
  LBudget := NewNyxPascalAnalysisBudget(100000);
  try
    try
      LValue := NyxPascalCalculateRealToEven(proDivide,Input.Left,Input.Right,pbpX87Extended80,LBudget);

      if not LValue.SameValue(Operation(proDivide,'1.0','3.0',pbpX87Extended80)) then
        raise Exception.Create('Concurrent real arithmetic result differs');
    except on E: Exception do ErrorText := E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentReaders;
var LReaders: array of TOperationReader; LAt: Integer; LInput: TOperationInput;
begin
  LInput.Left := Round('1.0'); LInput.Right := Round('3.0'); SetLength(LReaders,64);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TOperationReader.Create(True); LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].Input := LInput; LReaders[LAt].Start;
    end;
    for LAt := 0 to High(LReaders) do
    begin LReaders[LAt].WaitFor; Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText); end;
  finally
    for LAt := 0 to High(LReaders) do
    begin

      if LReaders[LAt] <> nil then begin LReaders[LAt].WaitFor; LReaders[LAt].Free; end;
    end;
  end;
  WriteLn('real operation readers / 64 joined');
end;
{$endif}

begin
  Vectors; Comparisons; WorkContract;
  {$ifndef PAS2JS}NativeOracle; ConcurrentReaders;{$endif}
  WriteLn('PASS real operation ',GChecks,' checks');
end.
