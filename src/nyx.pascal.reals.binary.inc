{ Private rational arithmetic for nyx.pascal.reals. Base 2^15 limbs keep every
  product plus carry below 2^30 on both FPC and pas2js. Long division needs only
  the requested significand bits; exponentiation uses squaring. No host float
  conversion, shared workspace, compiler default or rounded source fact occurs. }

type
  { A sole-owned mutable work integer, never published. All returned copies
    have independent limb vectors. Its borrowed execution scope is released by
    Destroy; every public result instead owns only immutable scalar/string data. }
  TRealNatural = class
  private
    FWords: array of Integer;
    FCount: Integer;
    FCancel: INyxCancellationScope;
    procedure Ensure(ACount: Integer);
    procedure Trim;
    function WordAt(AIndex: Integer): Integer;
    function ShiftedWord(AIndex, AShift: Integer): Integer;
  public
    constructor Create(const ACancel: INyxCancellationScope);
    destructor Destroy; override;
    procedure AddSmall(AValue: Integer);
    procedure MultiplySmall(AValue: Integer);
    procedure ShiftLeft(ABits: Integer);
    function Clone: TRealNatural;
    function Multiply(const AOther: TRealNatural): TRealNatural;
    function BitLength: Integer;
    function IsZero: Boolean;
    function CompareShifted(const AOther: TRealNatural; AShift: Integer): Integer;
    procedure SubtractShifted(const AOther: TRealNatural; AShift: Integer);
  end;

const
  CRealLimbBits = 15;
  CRealLimbMask = 32767;
  CRealPrecision: array[TNyxPascalBinaryPrecision] of Integer = (0,24,53,64,113);
  CRealMinimum: array[TNyxPascalBinaryPrecision] of Integer = (0,-126,-1022,-16382,-16382);
  CRealMaximum: array[TNyxPascalBinaryPrecision] of Integer = (0,127,1023,16383,16383);

procedure ValidateRealPrecision(AValue: TNyxPascalBinaryPrecision);
begin

  if (Ord(AValue) < Ord(Low(TNyxPascalBinaryPrecision))) or
    (Ord(AValue) > Ord(High(TNyxPascalBinaryPrecision))) then
    raise ENyxPascalValue.Create('A binary real precision is outside its closed domain');
end;

{ Capacity refusal is separate from a mathematical underflow/overflow result.
  Never let a work-vector shift wrap the host's signed integer arithmetic. }
function RealExponentDifference(ALeft, ARight: Integer): Integer;
begin

  if ((ARight > 0) and (ALeft < Low(Integer)+ARight)) or
    ((ARight < 0) and (ALeft > High(Integer)+ARight)) then
    raise ENyxPascalValue.Create('A real work exponent exceeds addressable capacity');
  Result := ALeft-ARight;
end;

constructor TRealNatural.Create(const ACancel: INyxCancellationScope);
begin
  inherited Create;
  FCancel := ACancel;
  Admit(FCancel,papMetadataNode);
end;

destructor TRealNatural.Destroy;
begin
  FCancel := nil;
  SetLength(FWords,0);
  inherited Destroy;
end;

procedure TRealNatural.Ensure(ACount: Integer);
begin

  if (ACount < 0) or (ACount > High(Integer) div CRealLimbBits) then
    raise ENyxPascalValue.Create('A real work integer exceeds addressable bit capacity');

  if ACount <= Length(FWords) then Exit;
  Admit(FCancel,papMetadataValue,ACount-Length(FWords));
  SetLength(FWords,ACount);
end;

procedure TRealNatural.Trim;
begin
  while FCount > 0 do
  begin
    Admit(FCancel,papConstantStep);

    if FWords[FCount-1] <> 0 then Break;
    Dec(FCount);
  end;
end;

function TRealNatural.WordAt(AIndex: Integer): Integer;
begin
  Admit(FCancel,papConstantStep);
  Result := 0;

  if (AIndex >= 0) and (AIndex < FCount) then Result := FWords[AIndex];
end;

function TRealNatural.ShiftedWord(AIndex, AShift: Integer): Integer;
var LAt, LRest: Integer;
begin
  LAt := AIndex-AShift div CRealLimbBits;
  LRest := AShift mod CRealLimbBits;
  Result := (WordAt(LAt) shl LRest) and CRealLimbMask;

  if LRest > 0 then Result := Result or (WordAt(LAt-1) shr (CRealLimbBits-LRest));
end;

procedure TRealNatural.AddSmall(AValue: Integer);
var LAt, LCarry, LSum: Integer;
begin
  LAt := 0; LCarry := AValue;
  while LCarry > 0 do
  begin
    Admit(FCancel,papConstantStep);

    if LAt = FCount then
    begin
      Ensure(FCount+1); FWords[FCount] := 0; Inc(FCount);
    end;
    LSum := FWords[LAt]+LCarry;
    FWords[LAt] := LSum and CRealLimbMask;
    LCarry := LSum shr CRealLimbBits; Inc(LAt);
  end;
end;

procedure TRealNatural.MultiplySmall(AValue: Integer);
var LAt, LCarry, LProduct: Integer;
begin
  LCarry := 0;
  for LAt := 0 to FCount-1 do
  begin
    Admit(FCancel,papConstantStep);
    LProduct := FWords[LAt]*AValue+LCarry;
    FWords[LAt] := LProduct and CRealLimbMask;
    LCarry := LProduct shr CRealLimbBits;
  end;

  if LCarry > 0 then
  begin
    Ensure(FCount+1); FWords[FCount] := LCarry; Inc(FCount);
  end;
end;

procedure TRealNatural.ShiftLeft(ABits: Integer);
var LShifted: array of Integer; LWhole, LRest, LAt, LCarry, LValue, LLength: Integer;
begin

  if (ABits = 0) or (FCount = 0) then Exit;
  LWhole := ABits div CRealLimbBits; LRest := ABits mod CRealLimbBits;

  if (ABits < 0) or (LWhole > High(Integer) div CRealLimbBits-FCount-1) then
    raise ENyxPascalValue.Create('A real work shift exceeds addressable bit capacity');
  LLength := FCount+LWhole+1;
  Admit(FCancel,papMetadataValue,LLength);
  SetLength(LShifted,LLength); LCarry := 0;
  for LAt := 0 to FCount-1 do
  begin
    Admit(FCancel,papConstantStep);
    LValue := FWords[LAt] shl LRest;
    LShifted[LAt+LWhole] := (LValue and CRealLimbMask) or LCarry;
    LCarry := LValue shr CRealLimbBits;
  end;
  LShifted[FCount+LWhole] := LCarry;
  FWords := LShifted; FCount := LLength; Trim;
end;

function TRealNatural.Clone: TRealNatural;
var LAt: Integer;
begin
  Result := TRealNatural.Create(FCancel);
  try
    Result.Ensure(FCount);
    for LAt := 0 to FCount-1 do
    begin
      Admit(FCancel,papConstantStep); Result.FWords[LAt] := FWords[LAt];
    end;
    Result.FCount := FCount;
  except Result.Free; raise; end;
end;

function TRealNatural.Multiply(const AOther: TRealNatural): TRealNatural;
var LAt, LOtherAt, LCarry, LProduct, LIndex: Integer;
begin
  Result := TRealNatural.Create(FCancel);
  try

    if IsZero or AOther.IsZero then Exit;
    Result.Ensure(FCount+AOther.FCount);
    for LAt := 0 to FCount-1 do
    begin
      LCarry := 0;
      for LOtherAt := 0 to AOther.FCount-1 do
      begin
        Admit(FCancel,papConstantStep);
        LIndex := LAt+LOtherAt;
        LProduct := FWords[LAt]*AOther.FWords[LOtherAt]+Result.FWords[LIndex]+LCarry;
        Result.FWords[LIndex] := LProduct and CRealLimbMask;
        LCarry := LProduct shr CRealLimbBits;
      end;
      Result.FWords[LAt+AOther.FCount] := LCarry;
    end;
    Result.FCount := FCount+AOther.FCount; Result.Trim;
  except Result.Free; raise; end;
end;

function TRealNatural.BitLength: Integer;
var LTop: Integer;
begin
  Result := 0;

  if FCount = 0 then Exit;
  Result := (FCount-1)*CRealLimbBits;
  LTop := WordAt(FCount-1);
  while LTop > 0 do
  begin
    Admit(FCancel,papConstantStep); Inc(Result); LTop := LTop shr 1;
  end;
end;

function TRealNatural.IsZero: Boolean;
begin
  Result := FCount = 0;
end;

function TRealNatural.CompareShifted(const AOther: TRealNatural; AShift: Integer): Integer;
var LBits, LOtherBits, LAt, LWord, LOtherWord: Integer;
begin
  LBits := BitLength; LOtherBits := AOther.BitLength;

  if LOtherBits > 0 then
  begin

    if AShift > High(Integer)-LOtherBits then Exit(-1);
    Inc(LOtherBits,AShift);
  end;

  if LBits < LOtherBits then Exit(-1);

  if LBits > LOtherBits then Exit(1);
  Result := 0;
  for LAt := FCount-1 downto 0 do
  begin
    LWord := WordAt(LAt); LOtherWord := AOther.ShiftedWord(LAt,AShift);

    if LWord < LOtherWord then Exit(-1);

    if LWord > LOtherWord then Exit(1);
  end;
end;

procedure TRealNatural.SubtractShifted(const AOther: TRealNatural; AShift: Integer);
var LAt, LValue, LBorrow: Integer;
begin
  LBorrow := 0;
  for LAt := 0 to FCount-1 do
  begin
    LValue := WordAt(LAt)-AOther.ShiftedWord(LAt,AShift)-LBorrow;
    LBorrow := 0;

    if LValue < 0 then begin Inc(LValue,CRealLimbMask+1); LBorrow := 1; end;
    FWords[LAt] := LValue;
  end;

  if LBorrow <> 0 then raise ENyxPascalValue.Create('A real work subtraction became negative');
  Trim;
end;

function RealPowerFive(AExponent: Integer; const ACancel: INyxCancellationScope): TRealNatural;
var LBase, LNext: TRealNatural;
begin
  Result := nil; LBase := nil; LNext := nil;
  try
    try
      Result := TRealNatural.Create(ACancel); Result.AddSmall(1);
      LBase := TRealNatural.Create(ACancel); LBase.AddSmall(5);
      while AExponent > 0 do
      begin
        Admit(ACancel,papConstantStep);

        if AExponent mod 2 <> 0 then
        begin
          LNext := Result.Multiply(LBase); Result.Free; Result := LNext; LNext := nil;
        end;
        AExponent := AExponent div 2;

        if AExponent > 0 then
        begin
          LNext := LBase.Multiply(LBase); LBase.Free; LBase := LNext; LNext := nil;
        end;
      end;
    except Result.Free; Result := nil; raise; end;
  finally LNext.Free; LBase.Free; end;
end;

function RealNatural(const AText: TNyxText; ABase: Integer;
  const ACancel: INyxCancellationScope): TRealNatural;
var LAt: Integer;
begin
  Result := TRealNatural.Create(ACancel);
  try
    Admit(ACancel,papSourceProof,Length(AText));
    for LAt := 1 to Length(AText) do
    begin
      Admit(ACancel,papConstantStep);
      Result.MultiplySmall(ABase); Result.AddSmall(Ord(AText[LAt])-Ord('0'));
    end;
  except Result.Free; raise; end;
end;

function RealRoundedZero(APrecision: TNyxPascalBinaryPrecision; ANegative,
  AInexact: Boolean): TNyxPascalBinaryReal;
begin
  Result := TNyxPascalBinaryReal.Unknown;
  Result.FStatus := rrsFinite; Result.FPrecision := APrecision;
  Result.FNegative := ANegative; Result.FInexact := AInexact; Result.FSignificand := '0';
end;

function RealRoundedOverflow(APrecision: TNyxPascalBinaryPrecision;
  ANegative: Boolean): TNyxPascalBinaryReal;
begin
  Result := TNyxPascalBinaryReal.Unknown;
  Result.FStatus := rrsOverflow; Result.FPrecision := APrecision;
  Result.FNegative := ANegative; Result.FInexact := True;
end;

{ N/D * 2^E is never converted to an approximate intermediate. The quotient
  contains at most precision+1 bits. One exact remainder comparison decides the
  nearest-even result, including a carry into the next binade or normal range. }
function RoundRealRational(const ANumerator, ADenominator: TRealNatural;
  AExponent: Integer; APrecision: TNyxPascalBinaryPrecision; ANegative: Boolean;
  const ACancel: INyxCancellationScope): TNyxPascalBinaryReal;
var LPower, LScale, LMinimum, LShift, LAt, LLength, LCompare, LCarry: Integer;
  LBits: TNyxText; LRemainder, LDivisor: TRealNatural; LInexact: Boolean;
begin
  Result := TNyxPascalBinaryReal.Unknown;

  if ANumerator.IsZero then Exit(RealRoundedZero(APrecision,ANegative,False));
  LPower := ANumerator.BitLength-ADenominator.BitLength;

  if LPower >= 0 then LCompare := ANumerator.CompareShifted(ADenominator,LPower)
  else LCompare := -ADenominator.CompareShifted(ANumerator,-LPower);

  if LCompare < 0 then Dec(LPower);

  if ((AExponent > 0) and (LPower > High(Integer)-AExponent)) or
    ((AExponent < 0) and (LPower < Low(Integer)-AExponent)) then
    raise ENyxPascalValue.Create('A real rational exponent exceeds addressable capacity');
  Inc(LPower,AExponent);
  LMinimum := CRealMinimum[APrecision]-CRealPrecision[APrecision]+1;

  if LPower > CRealMaximum[APrecision] then Exit(RealRoundedOverflow(APrecision,ANegative));

  if LPower < LMinimum-1 then Exit(RealRoundedZero(APrecision,ANegative,True));
  LScale := LPower-CRealPrecision[APrecision]+1;

  if LScale < LMinimum then LScale := LMinimum;
  LRemainder := nil; LDivisor := nil;
  try
    LRemainder := ANumerator.Clone; LDivisor := ADenominator.Clone;
    LShift := RealExponentDifference(AExponent,LScale);

    if LShift >= 0 then LRemainder.ShiftLeft(LShift)
    else LDivisor.ShiftLeft(-LShift);
    LLength := LRemainder.BitLength-LDivisor.BitLength+1;

    if LLength > CRealPrecision[APrecision]+1 then
      raise ENyxPascalValue.Create('A real quotient exceeded its proved significand bound');

    if LLength <= 0 then LBits := '0'
    else
    begin
      Admit(ACancel,papMetadataValue,LLength); SetLength(LBits,LLength);
      for LAt := LLength-1 downto 0 do
      begin
        Admit(ACancel,papConstantStep); LBits[LLength-LAt] := '0';

        if LRemainder.CompareShifted(LDivisor,LAt) >= 0 then
        begin
          LRemainder.SubtractShifted(LDivisor,LAt); LBits[LLength-LAt] := '1';
        end;
      end;
    end;
    LInexact := not LRemainder.IsZero;
    LCompare := -LDivisor.CompareShifted(LRemainder,1);

    if (LCompare > 0) or ((LCompare = 0) and (LBits[Length(LBits)] = '1')) then
    begin
      LAt := Length(LBits); LCarry := 1;
      while (LAt > 0) and (LCarry > 0) do
      begin
        Admit(ACancel,papConstantStep);

        if LBits[LAt] = '0' then begin LBits[LAt] := '1'; LCarry := 0; end
        else LBits[LAt] := '0';
        Dec(LAt);
      end;

      if LCarry > 0 then LBits := '1'+LBits;
    end;
    LAt := 1;
    while (LAt < Length(LBits)) and (LBits[LAt] = '0') do
    begin Admit(ACancel,papConstantStep); Inc(LAt); end;
    LBits := Copy(LBits,LAt,Length(LBits)-LAt+1);

    if LBits = '0' then Exit(RealRoundedZero(APrecision,ANegative,LInexact));
    LLength := Length(LBits);
    while (LLength > 1) and (LBits[LLength] = '0') do
    begin Admit(ACancel,papConstantStep); Dec(LLength); Inc(LScale); end;
    SetLength(LBits,LLength);

    if LScale+LLength-1 > CRealMaximum[APrecision] then
      Exit(RealRoundedOverflow(APrecision,ANegative));
    Result.FStatus := rrsFinite; Result.FPrecision := APrecision; Result.FNegative := ANegative;
    Result.FInexact := LInexact; Result.FSignificand := LBits; Result.FBinaryExponent := LScale;
  finally LDivisor.Free; LRemainder.Free; end;
end;

class function TNyxPascalBinaryReal.Unknown: TNyxPascalBinaryReal;
begin
  Result := Default(TNyxPascalBinaryReal);
end;

function TNyxPascalBinaryReal.IsZero: Boolean;
begin
  Result := (FStatus = rrsFinite) and (FSignificand = '0');
end;

function TNyxPascalBinaryReal.IsSubnormal: Boolean;
begin
  Result := (FStatus = rrsFinite) and not IsZero and
    (FBinaryExponent+Length(FSignificand)-1 < CRealMinimum[FPrecision]);
end;

function TNyxPascalBinaryReal.SameValue(const AOther: TNyxPascalBinaryReal;
  const ACancellation: INyxCancellationScope): Boolean;
begin
  Admit(ACancellation,papConstantStep);
  Result := (FStatus = rrsFinite) and (AOther.FStatus = rrsFinite);

  if not Result then Exit;
  Admit(ACancellation,papSourceProof,Length(FSignificand));
  Result := (FSignificand = AOther.FSignificand) and (FBinaryExponent = AOther.FBinaryExponent) and
    ((FNegative = AOther.FNegative) or IsZero);
end;

function TNyxPascalBinaryReal.RoundToEven(APrecision: TNyxPascalBinaryPrecision;
  const ACancellation: INyxCancellationScope): TNyxPascalBinaryReal;
var LNumerator, LDenominator: TRealNatural;
begin
  Admit(ACancellation,papConstantStep); ValidateRealPrecision(APrecision);
  Result := TNyxPascalBinaryReal.Unknown;

  if (FStatus <> rrsFinite) or (APrecision = pbpUnknown) then Exit;
  LNumerator := nil; LDenominator := nil;
  try
    LNumerator := RealNatural(FSignificand,2,ACancellation);
    LDenominator := TRealNatural.Create(ACancellation); LDenominator.AddSmall(1);
    Result := RoundRealRational(LNumerator,LDenominator,FBinaryExponent,APrecision,FNegative,ACancellation);
  finally LDenominator.Free; LNumerator.Free; end;
end;

function NyxPascalRoundRealToEven(const AValue: TNyxPascalRealValue;
  APrecision: TNyxPascalBinaryPrecision;
  const ACancellation: INyxCancellationScope): TNyxPascalBinaryReal;
var LOrder, LExponent: Integer; LOrderText: TNyxText;
  LNumerator, LDenominator, LPower, LNext: TRealNatural;
begin
  Admit(ACancellation,papConstantStep); ValidateRealPrecision(APrecision);
  Result := TNyxPascalBinaryReal.Unknown;

  if not AValue.Known or (APrecision = pbpUnknown) then Exit;

  if AValue.IsZero then Exit(RealRoundedZero(APrecision,AValue.Negative,False));
  { Decimal order includes the full coefficient length. Comparing an isolated
    huge negative exponent would wrongly discard a compensating long coefficient.
    The order is read as a checked decimal integer, never a partial real value. }
  LOrderText := AddExponent(AValue.Exponent,Length(AValue.Coefficient)-1,ACancellation);

  if not ReadRealExponent(LOrderText,LOrder,ACancellation) then
  begin

    if LOrderText[1] = '-' then Exit(RealRoundedZero(APrecision,AValue.Negative,True));
    Exit(RealRoundedOverflow(APrecision,AValue.Negative));
  end;
  { Deliberately loose integer bounds avoid logarithms or host approximation.
    Values inside them receive the complete rational conversion below. }

  if LOrder > CRealMaximum[APrecision]+1 then Exit(RealRoundedOverflow(APrecision,AValue.Negative));

  if LOrder < CRealMinimum[APrecision]-CRealPrecision[APrecision]-2 then
    Exit(RealRoundedZero(APrecision,AValue.Negative,True));

  if not AValue.TryExponent(LExponent,ACancellation) or (LExponent = Low(Integer)) then
    raise ENyxPascalValue.Create('A finite real exponent exceeds addressable work capacity');
  LNumerator := nil; LDenominator := nil; LPower := nil; LNext := nil;
  try
    LNumerator := RealNatural(AValue.Coefficient,10,ACancellation);

    if LExponent >= 0 then
    begin
      LPower := RealPowerFive(LExponent,ACancellation);
      LNext := LNumerator.Multiply(LPower); LNumerator.Free; LNumerator := LNext; LNext := nil;
      LDenominator := TRealNatural.Create(ACancellation); LDenominator.AddSmall(1);
    end
    else LDenominator := RealPowerFive(-LExponent,ACancellation);
    Result := RoundRealRational(LNumerator,LDenominator,LExponent,APrecision,AValue.Negative,ACancellation);
  finally LNext.Free; LPower.Free; LDenominator.Free; LNumerator.Free; end;
end;
