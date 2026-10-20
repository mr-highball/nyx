{ Mathematical nearest-even rounding has its own exact vectors. The independent
  native witness additionally records compiler-parsed literals and runtime casts;
  agreement is measured rather than assumed from a target define or host Double. }
program nyx_pascal_binary_real_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, Classes, nyx.text, nyx.scheduler, nyx.pascal.reals,
  nyx.pascal.work, nyx.pascal.lexing
  {$ifndef PAS2JS}, binary.real.fixture{$endif};

{$ifndef PAS2JS}
type
  { Each worker has its own mutable arithmetic workspace and budget over one
    copied immutable value. Main joins all workers before observing or freeing. }
  TBinaryRealReader = class(TThread)
  public
    Value: TNyxPascalRealValue;
    ErrorText: String;
    procedure Execute; override;
  end;
{$endif}

const
  CHalfSingle = '1.000000059604644775390625';
  CAboveHalf = '1.00000005960464477539062500000000000000000000000001';
  CHalfDouble = '1.00000000000000011102230246251565404236316680908203125';
  CSingleTiny = '1.40129846432481707092372958328991613128026194187651577175706828388979108268586060148663818836212158203125e-45';
  CHalfTiny = '7.00649232162408535461864791644958065640130970938257885878534141944895541342930300743319094181060791015625e-46';
var GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function Exact(const AText: TNyxText): TNyxPascalRealValue;
begin

  if (AText <> '') and (AText[1] = '-') then
    Result := NyxPascalRealToken(Copy(AText,2,Length(AText)-1)).Negated
  else Result := NyxPascalRealToken(AText);
end;

function Round(const AText: TNyxText; APrecision: TNyxPascalBinaryPrecision): TNyxPascalBinaryReal;
begin
  Result := NyxPascalRoundRealToEven(Exact(AText),APrecision);
end;

procedure Expect(const AText: TNyxText; APrecision: TNyxPascalBinaryPrecision;
  const ABits: TNyxText; AExponent: Integer; AInexact, ASubnormal: Boolean);
var LValue: TNyxPascalBinaryReal;
begin
  LValue := Round(AText,APrecision);
  WriteLn('binary vector / ',String(AText),' format=',Ord(APrecision),
    ' status=',Ord(LValue.Status),' bits=',String(LValue.Significand),' exponent=',LValue.BinaryExponent);
  Check((LValue.Status = rrsFinite) and (LValue.Precision = APrecision) and
    (LValue.Significand = ABits) and (LValue.BinaryExponent = AExponent) and
    (LValue.Inexact = AInexact) and (LValue.IsSubnormal = ASubnormal),
    'Nearest-even vector differs: '+String(AText));
end;

function Repeated(const AText: TNyxText; ACount: Integer): TNyxText;
var LAt: Integer;
begin
  Result := '';
  for LAt := 1 to ACount do Result := Result+AText;
end;

procedure Vectors;
var LValue, LParsed, LDirect, LCast: TNyxPascalBinaryReal;
  LSource: TNyxPascalRealValue; LFormat: TNyxPascalBinaryPrecision;
begin
  for LFormat := pbpBinary32 to pbpBinary128 do Expect('1.25',LFormat,'101',-2,False,False);
  Expect('0.1',pbpBinary32,'110011001100110011001101',-27,True,False);
  Expect('0.1',pbpBinary64,Repeated('1100',12)+'1101',-55,True,False);
  Expect('0.1',pbpX87Extended80,Repeated('1100',15)+'1101',-67,True,False);
  Expect('0.1',pbpBinary128,Repeated('1100',27)+'1101',-115,True,False);
  Expect(CHalfSingle,pbpBinary32,'1',0,True,False);
  Expect(CAboveHalf,pbpBinary32,'1'+TNyxText(StringOfChar('0',22))+'1',-23,True,False);
  Expect(CHalfDouble,pbpBinary64,'1',0,True,False);
  Expect(CSingleTiny,pbpBinary32,'1',-149,False,True);
  Expect(CHalfTiny,pbpBinary32,'0',0,True,False);
  Expect('1.0e-45',pbpBinary32,'1',-149,True,True);
  Expect('340282346638528859811704183484516925440.0',pbpBinary32,
    TNyxText(StringOfChar('1',24)),104,False,False);
  LValue := Round('340282356779733661637539395458142568448.0',pbpBinary32);
  Check((LValue.Status = rrsOverflow) and LValue.Inexact,'A carry beyond the maximum exponent reports finite-format overflow');
  LValue := Round('-'+CHalfTiny,pbpBinary32);
  Check(LValue.IsZero and LValue.Negative and LValue.Inexact,'A negative underflow tie retains negative zero');
  LValue := Round('0.0',pbpBinary32);
  Check(LValue.IsZero and not LValue.Inexact and LValue.SameValue(Round('-0.0',pbpBinary64)),
    'Zero equality is mathematical while sign and explicit format remain available');
  Check(not TNyxPascalBinaryReal.Unknown.SameValue(TNyxPascalBinaryReal.Unknown),
    'Unknown supplies no binary equality proof');
  Check(NyxPascalRoundRealToEven(TNyxPascalRealValue.Unknown,pbpBinary32).Status = rrsUnknown,
    'Unknown decimal evidence supplies no rounded result');
  Check(Round('1.0',pbpUnknown).Status = rrsUnknown,'An unknown format cannot borrow a host precision');
  LValue := Round('1.0e999999999999999999999999999999999',pbpX87Extended80);
  Check(LValue.Status = rrsOverflow,'Huge positive orders are classified without expanding a power');
  LValue := Round('-1.0e-999999999999999999999999999999999',pbpX87Extended80);
  Check(LValue.IsZero and LValue.Negative and LValue.Inexact,'Huge negative orders retain signed inexact zero');
  LSource := Exact(CAboveHalf);
  LParsed := NyxPascalRoundRealToEven(LSource,pbpX87Extended80);
  LDirect := NyxPascalRoundRealToEven(LSource,pbpBinary32);
  LCast := LParsed.RoundToEven(pbpBinary32);
  Check(not LDirect.SameValue(LCast) and LCast.SameValue(Round('1.0',pbpBinary32)),
    'Rounding an intermediate value is distinct from direct decimal rounding');
  LParsed := Round('1.00000000000000000000001',pbpX87Extended80);
  Check(LParsed.Inexact and LParsed.SameValue(Round('1.0',pbpBinary32)) and
    not LParsed.RoundToEven(pbpBinary32).Inexact,
    'A rounded parser value may fit Single even when the mathematical source does not');
  Check((LSource.Spelling = CAboveHalf) and LSource.SameValue(Exact(CAboveHalf)),
    'Rounding never mutates the original exact source evidence');
end;

procedure WorkContract;
var LSource: TNyxPascalRealValue; LValue, LRetained: TNyxPascalBinaryReal;
  LBudget: INyxPascalAnalysisBudget; LCancel: INyxCancellationScope;
  LAt, LFit: Integer; LRejected: Boolean;
begin
  LSource := Exact('0.1'); LFit := -1;
  for LAt := 1 to 20000 do
  begin
    LValue := TNyxPascalBinaryReal.Unknown; LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LValue := NyxPascalRoundRealToEven(LSource,pbpBinary32,LBudget);
        Check((LValue.Status = rrsFinite) and (LValue.Significand = '110011001100110011001101'),
          'First sufficient allowance publishes the complete rounded result');
        LFit := LAt; LRetained := LValue;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LValue.Status = rrsUnknown) and (LBudget.Receipt.Total <= LAt),
            'Every smaller allowance refuses atomically and releases all work integers');
      end;
    finally LBudget.Retire; end;

    if LFit > 0 then Break;
  end;
  Check((LFit > 1) and (LRetained.RoundToEven(pbpBinary64).Status = rrsFinite),
    'Published rounded values retain no retired work scope');
  WriteLn('binary real work / first fit=',LFit,' every smaller allowance refused');
  LCancel := NewNyxCancellationScope; LCancel.Cancel;
  LValue := TNyxPascalBinaryReal.Unknown; LRejected := False;
  try LValue := NyxPascalRoundRealToEven(LSource,pbpBinary32,LCancel);
  except on ENyxPascalAnalysisCancelled do LRejected := True; end;
  Check(LRejected and (LValue.Status = rrsUnknown),'Ordinary cancellation publishes no partial rounded value');
  LBudget := NewNyxPascalAnalysisBudget(4000000);
  try
    LValue := NyxPascalRoundRealToEven(Exact('1.0e4000'),pbpX87Extended80,LBudget);
    Check((LValue.Status = rrsFinite) and (LBudget.Receipt.Total < 4000000),
      'Large finite orders use bounded squaring and limited-significand division');
    WriteLn('binary real large / work=',LBudget.Receipt.Total);
  finally LBudget.Retire; end;
end;

{$ifndef PAS2JS}
function Describe(const AValue: TNyxPascalBinaryReal): TNyxText;
begin
  Result := '0';

  if AValue.Negative then Result := '1';

  if AValue.Status = rrsOverflow then Exit(Result+'|overflow|0');
  Result := Result+'|'+AValue.Significand+'|'+TNyxText(IntToStr(AValue.BinaryExponent));
end;

procedure NativeWitness;
var LRows, LFields: TStringList; LAt: Integer; LText: TNyxText;
  LValue, LSingle, LDouble: TNyxPascalBinaryReal;
begin
  LText := TNyxText(NativeBinaryRealObservations); Write(LText);
  Check(NativeBinaryRealCalls = 23,'Every original native literal observation executes once');
  LRows := TStringList.Create; LFields := TStringList.Create;
  try
    LRows.Text := String(LText); LFields.Delimiter := '|'; LFields.StrictDelimiter := True;
    Check(LRows.Count = 23,'The complete compiler oracle retains every original row');
    for LAt := 0 to LRows.Count-1 do
    begin
      LFields.DelimitedText := LRows[LAt];
      Check(LFields.Count = 12,'The native row retains source, generic type and all three byte observations');
      LValue := NyxPascalRoundRealToEven(Exact(TNyxText(LFields[1])),pbpX87Extended80);
      LSingle := LValue.RoundToEven(pbpBinary32); LDouble := LValue.RoundToEven(pbpBinary64);
      WriteLn('native binary comparison / ',LFields[0],' model=',String(Describe(LValue)));
      Check(Describe(LValue) = TNyxText(LFields[3]+'|'+LFields[4]+'|'+LFields[5]),
        'Compiler-parsed Extended differs from mathematical nearest-even: '+LFields[0]);
      Check(Describe(LSingle) = TNyxText(LFields[6]+'|'+LFields[7]+'|'+LFields[8]),
        'Native Single cast differs: '+LFields[0]);
      Check(Describe(LDouble) = TNyxText(LFields[9]+'|'+LFields[10]+'|'+LFields[11]),
        'Native Double cast differs: '+LFields[0]);
    end;
  finally LFields.Free; LRows.Free; end;
end;

procedure TBinaryRealReader.Execute;
var LBudget: INyxPascalAnalysisBudget; LValue: TNyxPascalBinaryReal;
begin
  LBudget := NewNyxPascalAnalysisBudget(20000);
  try
    try
      LValue := NyxPascalRoundRealToEven(Value,pbpX87Extended80,LBudget).RoundToEven(pbpBinary32,LBudget);

      if (LValue.Status <> rrsFinite) or (LValue.Significand <> '110011001100110011001101') then
        ErrorText := 'Concurrent binary rounding differs';
    except on E: Exception do ErrorText := E.ClassName+': '+E.Message; end;
  finally LBudget.Retire; end;
end;

procedure ConcurrentContract;
var LReaders: array[0..63] of TBinaryRealReader; LAt: Integer;
begin
  for LAt := 0 to High(LReaders) do LReaders[LAt] := nil;
  try
    for LAt := 0 to High(LReaders) do
    begin LReaders[LAt] := TBinaryRealReader.Create(True); LReaders[LAt].Value := Exact('0.1'); end;
    for LAt := 0 to High(LReaders) do LReaders[LAt].Start;
    for LAt := 0 to High(LReaders) do LReaders[LAt].WaitFor;
    for LAt := 0 to High(LReaders) do Check(LReaders[LAt].ErrorText = '',LReaders[LAt].ErrorText);
  finally
    for LAt := 0 to High(LReaders) do LReaders[LAt].Free;
  end;
  Check(NativeBinaryRealCalls = 23,'Mathematical and concurrent queries invoke no compiler witness body');
  WriteLn('native concurrent binary real readers / 64 joined');
end;
{$endif}

begin
  try
    Vectors;
    WorkContract;
    {$ifndef PAS2JS}NativeWitness; ConcurrentContract;{$endif}
    WriteLn('PASS ',GChecks,' exact binary real rounding checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ',E.ClassName,': ',E.Message);
      ExitCode := 1;
    end;
  end;
end.
