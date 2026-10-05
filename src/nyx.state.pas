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

unit nyx.state;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  Math,
  nyx.text;

const
  NyxMaximumStateEntries = 1024;
  NyxMaximumStateBytes = 1024 * 1024;
  NyxMaximumStateSubscriptions = 1024;

type
  { Store failures preserve admitted values/revision. Notification failures are
    different: a validated update was committed, and all remaining observers
    were still notified. Both retain UTF-8 diagnostic text on older Windows FPC. }
  ENyxState = class(Exception)
  public
    constructor Create(const AMessage: TNyxText); reintroduce;
  end;
  ENyxStateNotification = class(ENyxState);

  TNyxState = class;

  TNyxStateKind = (nskText, nskBoolean, nskInteger, nskNumber);

  { Immutable scalar value. Getters refuse a different kind instead of coercing
    text into behavior. Constructors initialize every field; CopyValue copies
    fields explicitly so pas2js records/arrays never expose shared mutable data.
    Integers use signed 32-bit semantics; numbers are finite IEEE Double values.
    Signed zero is canonicalized. Unicode text is strict UTF-8/native or UTF-16/JS. }
  TNyxStateValue = record
  private
    FKind: TNyxStateKind;
    FText: TNyxText;
    FBoolean: Boolean;
    FInteger: Integer;
    FNumber: Double;
    function GetText: TNyxText;
    function GetBoolean: Boolean;
    function GetInteger: Integer;
    function GetNumber: Double;
    function GetNumberText: TNyxText;
  public
    class function FromText(const AValue: TNyxText): TNyxStateValue; static;
    class function FromBoolean(const AValue: Boolean): TNyxStateValue; static;
    class function FromInteger(const AValue: Integer): TNyxStateValue; static;
    class function FromNumber(const AValue: Double): TNyxStateValue; static;
    function SameValue(const AOther: TNyxStateValue): Boolean;
    { Copy an already admitted scalar without sharing a mutable pas2js record
      or repeating text/domain validation at a readonly snapshot boundary. }
    function Copy: TNyxStateValue;
    procedure Validate;
    property Kind: TNyxStateKind read FKind;
    property TextValue: TNyxText read GetText;
    property BooleanValue: Boolean read GetBoolean;
    property IntegerValue: Integer read GetInteger;
    property NumberValue: Double read GetNumber;
    { Compact round-tripping decimal, cached for codec/codegen use. }
    property NumberText: TNyxText read GetNumberText;
  end;

  { Typed open names. A Boolean reference cannot be supplied to a text setter.
    References own their key text and do not borrow a store/document/control. }
  TNyxTextStateRef = record
  private
    FName: TNyxText;
    function GetName: TNyxText;
  public
    property Name: TNyxText read GetName;
  end;
  TNyxBooleanStateRef = record
  private
    FName: TNyxText;
    function GetName: TNyxText;
  public
    property Name: TNyxText read GetName;
  end;
  TNyxIntegerStateRef = record
  private
    FName: TNyxText;
    function GetName: TNyxText;
  public
    property Name: TNyxText read GetName;
  end;
  TNyxNumberStateRef = record
  private
    FName: TNyxText;
    function GetName: TNyxText;
  public
    property Name: TNyxText read GetName;
  end;

  { Input data only. Apply copies every field before validation/publication;
    callers may reuse or change their own arrays/records afterward. Exists=False
    removes a key. Empty text remains a present value and differs from removal. }
  TNyxStateAssignment = record
    Key: TNyxText;
    Value: TNyxStateValue;
    Exists: Boolean;
  end;

  { Borrowed immutable change data, valid only during a validator/observer call.
    Immutable copied values avoid exposing mutable record/array aliases under pas2js.
    Read HadValue/HasValue before fetching an optional old/new value.
    Keys follow request order; no-op writes are omitted. }
  TNyxStateChanges = class
  private
    FKeys: array of TNyxText;
    FBefore: array of TNyxStateValue;
    FAfter: array of TNyxStateValue;
    FHadValue: array of Boolean;
    FHasValue: array of Boolean;
    FOrderChanged: Boolean;
    procedure Add(const AKey: TNyxText; const ABefore, AAfter: TNyxStateValue;
      AHadValue, AHasValue: Boolean);
    procedure CheckIndex(AIndex: Integer);
    function GetCount: Integer;
  public
    function Key(AIndex: Integer): TNyxText;
    function BeforeValue(AIndex: Integer): TNyxStateValue;
    function AfterValue(AIndex: Integer): TNyxStateValue;
    function HadValue(AIndex: Integer): Boolean;
    function HasValue(AIndex: Integer): Boolean;
    property Count: Integer read GetCount;
    property OrderChanged: Boolean read FOrderChanged;
  end;

  { Validators inspect a read-only complete proposed snapshot before publication.
    Observers inspect the committed store after the entire batch is published.
    Callbacks may disconnect subscriptions; state writes/subscription additions
    during either phase are refused. Put dependent writes in one domain-command
    Apply batch so views never observe half of a multi-key edit. }
  TNyxStateValidator = procedure(ACandidate: TNyxState;
    AChanges: TNyxStateChanges) of object;
  TNyxStateObserver = procedure(AState: TNyxState;
    AChanges: TNyxStateChanges) of object;

  { Caller owns the subscription. Free/Disconnect removes it immediately. The
    store borrows this token and callback receivers; a receiver must disconnect
    before it is freed. Freeing the store detaches outstanding tokens so they
    can subsequently be freed safely. Never free the store inside its callbacks. }
  TNyxStateSubscription = class
  private
    FState: TNyxState;
    FSerial: Integer;
    FValidator: TNyxStateValidator;
    FObserver: TNyxStateObserver;
    function GetConnected: Boolean;
  public
    destructor Destroy; override;
    procedure Disconnect;
    property Connected: Boolean read GetConnected;
  end;

  { Ordered portable observable values. A document owns authored defaults;
    applications clone them into runtime state. The store owns scalar data, never UI
    nodes/controls. Its data and subscription receivers are confined to one UI
    event thread; adapters marshal background work before changing state.

    Apply builds a detached candidate, validates budgets and every subscriber,
    then publishes one revision. Apply/Assign can require a baseline revision.
    Rejected edits preserve keys, ordering and revision. No-op edits do not
    notify or advance revision. Types cannot change
    through Apply; Assign explicitly replaces a dataset and is validator-admitted.
    Order-only replacements publish once with OrderChanged. Updates preserve
    positions; new keys append in request order. Assign replaces data atomically
    and Clone copies data without listeners.
    Count<=1024; key/text content uses UTF-8 bytes, Boolean/integer/number
    payloads use 1/4/8 bytes, within one shared 1 MiB content budget. }
  TNyxState = class
  private
    FKeys: array of TNyxText;
    FValues: array of TNyxStateValue;
    FSubscriptions: array of TNyxStateSubscription;
    FRevision: Integer;
    FSerial: Integer;
    FBusy: Boolean;
    FReadOnly: Boolean;
    function IndexOf(const AKey: TNyxText): Integer;
    function GetCount: Integer;
    procedure Put(const AKey: TNyxText; const AValue: TNyxStateValue; AExists: Boolean);
    procedure CheckWritable(AExpectedRevision: Integer = -1);
    procedure CheckReference(const AKey: TNyxText; AKind: TNyxStateKind);
    procedure Disconnect(ASubscription: TNyxStateSubscription);
    function Subscription(ASerial: Integer): TNyxStateSubscription;
    procedure Publish(ACandidate: TNyxState; AChanges: TNyxStateChanges);
  public
    destructor Destroy; override;
    function Has(const AKey: TNyxText): Boolean;
    function Value(const AKey: TNyxText): TNyxStateValue;
    function GetValue(const ARef: TNyxTextStateRef): TNyxText; overload;
    function GetValue(const ARef: TNyxBooleanStateRef): Boolean; overload;
    function GetValue(const ARef: TNyxIntegerStateRef): Integer; overload;
    function GetValue(const ARef: TNyxNumberStateRef): Double; overload;
    function Key(AIndex: Integer): TNyxText;
    function SetValue(const ARef: TNyxTextStateRef; const AValue: TNyxText): TNyxState; overload;
    function SetValue(const ARef: TNyxBooleanStateRef; const AValue: Boolean): TNyxState; overload;
    function SetValue(const ARef: TNyxIntegerStateRef; const AValue: Integer): TNyxState; overload;
    function SetValue(const ARef: TNyxNumberStateRef; const AValue: Double): TNyxState; overload;
    function Remove(const AKey: TNyxText): TNyxState; overload;
    function Remove(const ARef: TNyxTextStateRef): TNyxState; overload;
    function Remove(const ARef: TNyxBooleanStateRef): TNyxState; overload;
    function Remove(const ARef: TNyxIntegerStateRef): TNyxState; overload;
    function Remove(const ARef: TNyxNumberStateRef): TNyxState; overload;
    procedure Apply(const AValues: array of TNyxStateAssignment;
      AExpectedRevision: Integer = -1);
    procedure Assign(ASource: TNyxState; AExpectedRevision: Integer = -1);
    function Clone: TNyxState;
    procedure Validate;
    function Subscribe(AObserver: TNyxStateObserver;
      AValidator: TNyxStateValidator = nil): TNyxStateSubscription;
    property Count: Integer read GetCount;
    property Revision: Integer read FRevision;
    property ReadOnly: Boolean read FReadOnly;
  end;

{ References and typed batch helpers. NyxStateAssign is the explicit codec/bulk
  boundary with a tagged value. Removal carries a zero text placeholder whose
  kind is irrelevant; change accessors refuse absent before/after values. }
function NyxTextState(const AName: TNyxText): TNyxTextStateRef;
function NyxBooleanState(const AName: TNyxText): TNyxBooleanStateRef;
function NyxIntegerState(const AName: TNyxText): TNyxIntegerStateRef;
function NyxNumberState(const AName: TNyxText): TNyxNumberStateRef;
function NyxStateValue(const ARef: TNyxTextStateRef; const AValue: TNyxText): TNyxStateAssignment; overload;
function NyxStateValue(const ARef: TNyxBooleanStateRef; const AValue: Boolean): TNyxStateAssignment; overload;
function NyxStateValue(const ARef: TNyxIntegerStateRef; const AValue: Integer): TNyxStateAssignment; overload;
function NyxStateValue(const ARef: TNyxNumberStateRef; const AValue: Double): TNyxStateAssignment; overload;
function NyxStateAssign(const AKey: TNyxText;
  const AValue: TNyxStateValue): TNyxStateAssignment;
function NyxStateRemove(const AKey: TNyxText): TNyxStateAssignment;
{ Numeric wire/code generation finds a round-tripping decimal with at most 17
  significant digits. Formatting is locale-independent; parsing uses a local
  invariant settings record. Neither changes the application's global settings. }
function NyxStateNumberText(AValue: Double): TNyxText;
function TryNyxStateNumber(const AText: TNyxText; out AValue: Double): Boolean;
{ Complete signed 32-bit control decimal. Optional sign/leading zeroes remain
  accepted wire forms; whitespace, fractional suffixes and overflow fail. }
function TryNyxStateInteger(const AText: TNyxText; out AValue: Integer): Boolean;
function NyxStateKindName(AKind: TNyxStateKind): TNyxText;

implementation

function TryNyxStateInteger(const AText: TNyxText; out AValue: Integer): Boolean;
var
  LIndex: Integer;
  LStart: Integer;
begin
  Result := False;
  AValue := 0;

  if (AText = '') or (Length(AText) > 11) then
  begin
    Exit;
  end;
  LStart := 1;

  if AText[1] in ['+', '-'] then
  begin
    LStart := 2;
  end;

  if LStart > Length(AText) then
  begin
    Exit;
  end;
  for LIndex := LStart to Length(AText) do
  begin

    if not (AText[LIndex] in ['0'..'9']) then
    begin
      Exit;
    end;
  end;
  Result := TryStrToInt(AText, AValue);
end;

constructor ENyxState.Create(const AMessage: TNyxText);
begin
  inherited Create('');
  {$IFDEF PAS2JS}
  Message := AMessage;
  {$ELSE}
  { Preserve this diagnostic's UTF-8 bytes without changing global codepages. }
  Message := RawByteString(AMessage);
  {$ENDIF}
end;

function TextBytes(const AText: TNyxText; AKey: Boolean): Integer;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
  LContent: Boolean;
begin
  Result := 0;
  LIndex := 1;
  LCount := 0;
  LContent := False;
  while LIndex <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxState.Create('State text contains malformed Unicode');
    end;
    Inc(LCount);
    LContent := LContent or not NyxScalarWhitespace(LScalar);

    if AKey and ((LCount > 128) or (LScalar < 32) or
      ((LScalar >= $7f) and (LScalar <= $9f)) or
      (LScalar = $2028) or (LScalar = $2029)) then
    begin
      raise ENyxState.Create('State key requires 1..128 Unicode scalars without controls');
    end;

    if LScalar <= $7f then
    begin
      Inc(Result);
    end
    else if LScalar <= $7ff then
    begin
      Inc(Result, 2);
    end
    else if LScalar <= $ffff then
    begin
      Inc(Result, 3);
    end
    else
    begin
      Inc(Result, 4);
    end;

    if Result > NyxMaximumStateBytes then
    begin
      raise ENyxState.Create('State exceeds 1 MiB UTF-8 storage budget');
    end;
  end;

  if AKey and not LContent then
  begin
    raise ENyxState.Create('State key requires non-whitespace content');
  end;
end;

function CopyValue(const AValue: TNyxStateValue): TNyxStateValue;
begin
  Result.FKind := AValue.FKind;
  Result.FText := AValue.FText;
  Result.FBoolean := AValue.FBoolean;
  Result.FInteger := AValue.FInteger;
  Result.FNumber := AValue.FNumber;
end;

function TNyxStateValue.Copy: TNyxStateValue;
begin
  Result := CopyValue(Self);
end;

function NyxStateKindName(AKind: TNyxStateKind): TNyxText;
const
  CNames: array[TNyxStateKind] of TNyxText = ('text', 'boolean', 'integer', 'number');
begin
  Result := CNames[AKind];
end;

class function TNyxStateValue.FromText(const AValue: TNyxText): TNyxStateValue;
begin
  Result.FKind := nskText;
  Result.FText := '';
  Result.FBoolean := False;
  Result.FInteger := 0;
  Result.FNumber := 0;
  Result.FText := AValue;
  Result.Validate;
end;

function TNyxStateValue.GetText: TNyxText;
begin

  if FKind <> nskText then
  begin
    raise ENyxState.Create('State value is ' + NyxStateKindName(FKind) + ', expected text');
  end;
  Result := FText;
end;

class function TNyxStateValue.FromBoolean(const AValue: Boolean): TNyxStateValue;
begin
  Result.FKind := nskBoolean;
  Result.FText := '';
  Result.FBoolean := False;
  Result.FInteger := 0;
  Result.FNumber := 0;
  Result.FBoolean := AValue;
  Result.Validate;
end;

function TNyxStateValue.GetBoolean: Boolean;
begin

  if FKind <> nskBoolean then
  begin
    raise ENyxState.Create('State value is ' + NyxStateKindName(FKind) + ', expected boolean');
  end;
  Result := FBoolean;
end;

class function TNyxStateValue.FromInteger(const AValue: Integer): TNyxStateValue;
begin
  Result.FKind := nskInteger;
  Result.FText := '';
  Result.FBoolean := False;
  Result.FInteger := 0;
  Result.FNumber := 0;
  Result.FInteger := AValue;
  Result.Validate;
end;

function TNyxStateValue.GetInteger: Integer;
begin

  if FKind <> nskInteger then
  begin
    raise ENyxState.Create('State value is ' + NyxStateKindName(FKind) + ', expected integer');
  end;
  Result := FInteger;
end;

class function TNyxStateValue.FromNumber(const AValue: Double): TNyxStateValue;
begin
  Result.FKind := nskNumber;
  Result.FText := '';
  Result.FBoolean := False;
  Result.FInteger := 0;
  Result.FNumber := 0;
  Result.FNumber := AValue;

  if AValue = 0 then
  begin
    Result.FNumber := 0;
  end;
  Result.Validate;
  Result.FText := NyxStateNumberText(Result.FNumber);
end;

function TNyxStateValue.GetNumber: Double;
begin

  if FKind <> nskNumber then
  begin
    raise ENyxState.Create('State value is ' + NyxStateKindName(FKind) + ', expected number');
  end;
  Result := FNumber;
end;

function TNyxStateValue.GetNumberText: TNyxText;
begin
  GetNumber;
  Result := FText;
end;

procedure TNyxStateValue.Validate;
begin
  { Validate cast/bridge ordinals explicitly; a defensive rejection must remain
    reachable even when all declared enum members are handled below. }
  case Ord(FKind) of
    Ord(nskText): TextBytes(FText, False);
    Ord(nskBoolean): ;
    Ord(nskInteger):
      begin
        { These explicit checks also protect a pas2js value received across a
          JavaScript bridge; native Integer already has signed 32-bit storage. }

        if (FInteger < -2147483648.0) or (FInteger > 2147483647.0) or
          IsNaN(FInteger) or IsInfinite(FInteger) then
        begin
          raise ENyxState.Create('State integer must fit signed 32-bit storage');
        end;

        if FInteger <> Trunc(FInteger) then
        begin
          raise ENyxState.Create('State integer must be integral');
        end;
      end;
    Ord(nskNumber):
      begin

        if IsNaN(FNumber) or IsInfinite(FNumber) then
        begin
          raise ENyxState.Create('State number must be finite');
        end;
      end;
    else
      begin
        raise ENyxState.Create('Unknown state value kind');
      end;
  end;
end;

function TNyxStateValue.SameValue(const AOther: TNyxStateValue): Boolean;
begin
  Result := False;

  if FKind <> AOther.FKind then
  begin
    Exit;
  end;
  case FKind of
    nskText: Result := FText = AOther.FText;
    nskBoolean: Result := FBoolean = AOther.FBoolean;
    nskInteger: Result := FInteger = AOther.FInteger;
    nskNumber: Result := FNumber = AOther.FNumber;
  end;
end;

function NyxStateNumberText(AValue: Double): TNyxText;
var
  LPrecision: Integer;
  LReadback: Double;
  LExponentIndex: Integer;
  LExponent: Integer;
  LMantissa: TNyxText;
  LDigits: TNyxText;
  LSign: TNyxText;
  LPoint: Integer;
begin

  if IsNaN(AValue) or IsInfinite(AValue) then
  begin
    raise ENyxState.Create('State number must be finite');
  end;

  if AValue = 0 then
  begin
    Exit('0');
  end;
  { Choose the first significant-digit count that reads back to the same Double.
    Pascal Str emits a dot independently of locale. In the installed browser RTL,
    FloatToStrF(ffExponent) reads global settings even when given local settings;
    native Digits=0 can also remove E+0. Str keeps both boundaries explicit.
    The first round-tripping candidate produces readable ordinary decimals
    while preserving values requiring all 17 digits. Number records cache it. }
  for LPrecision := 1 to 17 do
  begin
    Str(AValue:LPrecision + 7, Result);
    Result := Trim(Result);

    if TryNyxStateNumber(Result, LReadback) and (LReadback = AValue) then
    begin
      Break;
    end;
  end;

  if not TryNyxStateNumber(Result, LReadback) or (LReadback <> AValue) then
  begin
    raise ENyxState.Create('State number could not be represented without loss');
  end;
  LExponentIndex := Pos('E', UpperCase(Result));
  LMantissa := Copy(Result, 1, LExponentIndex - 1);
  LExponent := StrToInt(Copy(Result, LExponentIndex + 1, MaxInt));
  LSign := '';

  if LMantissa[1] = '-' then
  begin
    LSign := '-';
    Delete(LMantissa, 1, 1);
  end;
  LDigits := StringReplace(LMantissa, '.', '', [rfReplaceAll]);
  while (Length(LDigits) > 1) and (LDigits[Length(LDigits)] = '0') do
  begin
    Delete(LDigits, Length(LDigits), 1);
  end;

  if (LExponent < -6) or (LExponent > 16) then
  begin
    Result := LSign + Copy(LDigits, 1, 1);

    if Length(LDigits) > 1 then
    begin
      Result := Result + '.' + Copy(LDigits, 2, MaxInt);
    end;
    Result := Result + 'e' + IntToStr(LExponent);
    Exit;
  end;
  LPoint := LExponent + 1;

  if LPoint <= 0 then
  begin
    Result := LSign + '0.' + StringOfChar('0', -LPoint) + LDigits;
  end
  else if LPoint >= Length(LDigits) then
  begin
    Result := LSign + LDigits + StringOfChar('0', LPoint - Length(LDigits));
  end
  else
  begin
    Result := LSign + Copy(LDigits, 1, LPoint) + '.' + Copy(LDigits, LPoint + 1, MaxInt);
  end;
end;

function TryNyxStateNumber(const AText: TNyxText; out AValue: Double): Boolean;
var
  LIndex: Integer;
  LStart: Integer;
  LSettings: TFormatSettings;
  LMantissaNonzero: Boolean;
begin
  { Require the entire JSON decimal grammar before RTL parsing. Some browser
    number parsers otherwise accept prefixes such as "1 trailing text". }
  Result := False;
  AValue := 0;

  if (AText = '') or (Length(AText) > 128) then
  begin
    Exit;
  end;
  LIndex := 1;
  LMantissaNonzero := False;

  if AText[LIndex] = '-' then
  begin
    Inc(LIndex);
  end;

  if LIndex > Length(AText) then
  begin
    Exit;
  end;

  if AText[LIndex] = '0' then
  begin
    Inc(LIndex);
  end
  else
  begin
    LStart := LIndex;
    while (LIndex <= Length(AText)) and (AText[LIndex] in ['0'..'9']) do
    begin
      LMantissaNonzero := LMantissaNonzero or (AText[LIndex] <> '0');
      Inc(LIndex);
    end;

    if LStart = LIndex then
    begin
      Exit;
    end;
  end;

  if (LIndex <= Length(AText)) and (AText[LIndex] = '.') then
  begin
    Inc(LIndex);
    LStart := LIndex;
    while (LIndex <= Length(AText)) and (AText[LIndex] in ['0'..'9']) do
    begin
      LMantissaNonzero := LMantissaNonzero or (AText[LIndex] <> '0');
      Inc(LIndex);
    end;

    if LStart = LIndex then
    begin
      Exit;
    end;
  end;

  if (LIndex <= Length(AText)) and (AText[LIndex] in ['e', 'E']) then
  begin
    Inc(LIndex);

    if (LIndex <= Length(AText)) and (AText[LIndex] in ['+', '-']) then
    begin
      Inc(LIndex);
    end;
    LStart := LIndex;
    while (LIndex <= Length(AText)) and (AText[LIndex] in ['0'..'9']) do
    begin
      Inc(LIndex);
    end;

    if LStart = LIndex then
    begin
      Exit;
    end;
  end;

  if LIndex <= Length(AText) then
  begin
    Exit;
  end;
  { Exact zero needs no platform exponent conversion. A nonzero mantissa must
    remain nonzero in Double; rejecting underflow avoids different RTL policies
    silently changing an imported value into zero. }

  if not LMantissaNonzero then
  begin
    Exit(True);
  end;
  LSettings := FormatSettings;
  LSettings.DecimalSeparator := '.';
  LSettings.ThousandSeparator := #0;
  try
    Result := TryStrToFloat(AText, AValue, LSettings);
    Result := Result and not IsNaN(AValue) and not IsInfinite(AValue) and (AValue <> 0);
  except
    { Overflow is an invalid imported value, never an admitted nonfinite number. }
    Result := False;
  end;

  if Result and (AValue = 0) then
  begin
    AValue := 0;
  end;
end;

function TNyxTextStateRef.GetName: TNyxText;
begin
  Result := FName;
end;

function NyxTextState(const AName: TNyxText): TNyxTextStateRef;
begin
  TextBytes(AName, True);
  Result.FName := AName;
end;

function TNyxBooleanStateRef.GetName: TNyxText;
begin
  Result := FName;
end;

function NyxBooleanState(const AName: TNyxText): TNyxBooleanStateRef;
begin
  TextBytes(AName, True);
  Result.FName := AName;
end;

function TNyxIntegerStateRef.GetName: TNyxText;
begin
  Result := FName;
end;

function NyxIntegerState(const AName: TNyxText): TNyxIntegerStateRef;
begin
  TextBytes(AName, True);
  Result.FName := AName;
end;

function TNyxNumberStateRef.GetName: TNyxText;
begin
  Result := FName;
end;

function NyxNumberState(const AName: TNyxText): TNyxNumberStateRef;
begin
  TextBytes(AName, True);
  Result.FName := AName;
end;

function NyxStateValue(const ARef: TNyxTextStateRef; const AValue: TNyxText): TNyxStateAssignment;
begin
  Result := NyxStateAssign(ARef.Name, TNyxStateValue.FromText(AValue));
end;

function NyxStateValue(const ARef: TNyxBooleanStateRef; const AValue: Boolean): TNyxStateAssignment;
begin
  Result := NyxStateAssign(ARef.Name, TNyxStateValue.FromBoolean(AValue));
end;

function NyxStateValue(const ARef: TNyxIntegerStateRef; const AValue: Integer): TNyxStateAssignment;
begin
  Result := NyxStateAssign(ARef.Name, TNyxStateValue.FromInteger(AValue));
end;

function NyxStateValue(const ARef: TNyxNumberStateRef; const AValue: Double): TNyxStateAssignment;
begin
  Result := NyxStateAssign(ARef.Name, TNyxStateValue.FromNumber(AValue));
end;

function NyxStateAssign(const AKey: TNyxText; const AValue: TNyxStateValue): TNyxStateAssignment;
begin
  Result.Key := AKey;
  Result.Value := CopyValue(AValue);
  Result.Exists := True;
end;

function NyxStateRemove(const AKey: TNyxText): TNyxStateAssignment;
begin
  Result.Key := AKey;
  Result.Value := TNyxStateValue.FromText('');
  Result.Exists := False;
end;

procedure TNyxStateChanges.Add(const AKey: TNyxText;
  const ABefore, AAfter: TNyxStateValue;
  AHadValue, AHasValue: Boolean);
var
  LIndex: Integer;
begin
  LIndex := Count;
  SetLength(FKeys, LIndex + 1);
  SetLength(FBefore, LIndex + 1);
  SetLength(FAfter, LIndex + 1);
  SetLength(FHadValue, LIndex + 1);
  SetLength(FHasValue, LIndex + 1);
  FKeys[LIndex] := AKey;
  FBefore[LIndex] := CopyValue(ABefore);
  FAfter[LIndex] := CopyValue(AAfter);
  FHadValue[LIndex] := AHadValue;
  FHasValue[LIndex] := AHasValue;
end;

procedure TNyxStateChanges.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxState.Create('State change index is outside its range');
  end;
end;

function TNyxStateChanges.GetCount: Integer;
begin
  Result := Length(FKeys);
end;

function TNyxStateChanges.Key(AIndex: Integer): TNyxText;
begin
  CheckIndex(AIndex);
  Result := FKeys[AIndex];
end;

function TNyxStateChanges.BeforeValue(AIndex: Integer): TNyxStateValue;
begin
  CheckIndex(AIndex);

  if not FHadValue[AIndex] then
  begin
    raise ENyxState.Create('State change has no previous value');
  end;
  Result := CopyValue(FBefore[AIndex]);
end;

function TNyxStateChanges.AfterValue(AIndex: Integer): TNyxStateValue;
begin
  CheckIndex(AIndex);

  if not FHasValue[AIndex] then
  begin
    raise ENyxState.Create('State change has no new value');
  end;
  Result := CopyValue(FAfter[AIndex]);
end;

function TNyxStateChanges.HadValue(AIndex: Integer): Boolean;
begin
  CheckIndex(AIndex);
  Result := FHadValue[AIndex];
end;

function TNyxStateChanges.HasValue(AIndex: Integer): Boolean;
begin
  CheckIndex(AIndex);
  Result := FHasValue[AIndex];
end;

destructor TNyxStateSubscription.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TNyxStateSubscription.Disconnect;
begin

  if FState <> nil then
  begin
    FState.Disconnect(Self);
  end;
end;

function TNyxStateSubscription.GetConnected: Boolean;
begin
  Result := FState <> nil;
end;

destructor TNyxState.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FSubscriptions) - 1 do
  begin
    FSubscriptions[LIndex].FState := nil;
  end;
  inherited Destroy;
end;

function TNyxState.IndexOf(const AKey: TNyxText): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Count - 1 do
  begin

    if FKeys[LIndex] = AKey then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TNyxState.GetCount: Integer;
begin
  Result := Length(FKeys);
end;

function TNyxState.Has(const AKey: TNyxText): Boolean;
begin
  Result := IndexOf(AKey) >= 0;
end;

function TNyxState.Value(const AKey: TNyxText): TNyxStateValue;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AKey);

  if LIndex < 0 then
  begin
    raise ENyxState.Create('State value is missing: ' + AKey);
  end;
  Result := CopyValue(FValues[LIndex]);
end;

function TNyxState.Key(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxState.Create('State key index is outside its range');
  end;
  Result := FKeys[AIndex];
end;

procedure TNyxState.Put(const AKey: TNyxText; const AValue: TNyxStateValue;
  AExists: Boolean);
var
  LIndex: Integer;
  LNext: Integer;
begin
  LIndex := IndexOf(AKey);

  if not AExists then
  begin

    if LIndex >= 0 then
    begin
      for LNext := LIndex to Count - 2 do
      begin
        FKeys[LNext] := FKeys[LNext + 1];
        FValues[LNext] := CopyValue(FValues[LNext + 1]);
      end;
      SetLength(FKeys, Count - 1);
      SetLength(FValues, Length(FValues) - 1);
    end;
    Exit;
  end;

  if LIndex < 0 then
  begin
    LIndex := Count;
    SetLength(FKeys, LIndex + 1);
    SetLength(FValues, LIndex + 1);
    FKeys[LIndex] := AKey;
  end;
  FValues[LIndex] := CopyValue(AValue);
end;

procedure TNyxState.CheckWritable(AExpectedRevision: Integer);
begin

  if (AExpectedRevision < -1) or
    ((AExpectedRevision >= 0) and (AExpectedRevision <> FRevision)) then
  begin
    raise ENyxState.Create('State update uses a stale or invalid revision');
  end;

  if FReadOnly then
  begin
    raise ENyxState.Create('Proposed state snapshot is read-only');
  end;

  if FBusy then
  begin
    raise ENyxState.Create('State callbacks cannot mutate state; use one Apply batch');
  end;
end;

procedure TNyxState.Validate;
var
  LIndex: Integer;
  LBytes: Integer;
begin

  if Count > NyxMaximumStateEntries then
  begin
    raise ENyxState.Create('State exceeds its 1024-key budget');
  end;
  LBytes := 0;
  for LIndex := 0 to Count - 1 do
  begin
    Inc(LBytes, TextBytes(FKeys[LIndex], True));
    FValues[LIndex].Validate;
    case FValues[LIndex].Kind of
      nskText: Inc(LBytes, TextBytes(FValues[LIndex].TextValue, False));
      nskBoolean: Inc(LBytes, 1);
      nskInteger: Inc(LBytes, 4);
      nskNumber: Inc(LBytes, 8);
    end;

    if LBytes > NyxMaximumStateBytes then
    begin
      raise ENyxState.Create('State exceeds 1 MiB UTF-8 storage budget');
    end;
  end;
end;

function TNyxState.Clone: TNyxState;
var
  LIndex: Integer;
begin
  Result := TNyxState.Create;
  try
    Result.FKeys := Copy(FKeys, 0, Count);
    SetLength(Result.FValues, Count);
    for LIndex := 0 to Count - 1 do
    begin
      Result.FValues[LIndex] := CopyValue(FValues[LIndex]);
    end;
    Result.FRevision := FRevision;
  except
    Result.Free;
    raise;
  end;
end;

procedure TNyxState.Disconnect(ASubscription: TNyxStateSubscription);
var
  LIndex: Integer;
  LNext: Integer;
begin
  for LIndex := 0 to Length(FSubscriptions) - 1 do
  begin

    if FSubscriptions[LIndex] = ASubscription then
    begin
      ASubscription.FState := nil;
      for LNext := LIndex to Length(FSubscriptions) - 2 do
      begin
        FSubscriptions[LNext] := FSubscriptions[LNext + 1];
      end;
      SetLength(FSubscriptions, Length(FSubscriptions) - 1);
      Exit;
    end;
  end;
end;

function TNyxState.Subscription(ASerial: Integer): TNyxStateSubscription;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to Length(FSubscriptions) - 1 do
  begin

    if FSubscriptions[LIndex].FSerial = ASerial then
    begin
      Exit(FSubscriptions[LIndex]);
    end;
  end;
end;

function TNyxState.Subscribe(AObserver: TNyxStateObserver;
  AValidator: TNyxStateValidator): TNyxStateSubscription;
begin
  CheckWritable;

  if not Assigned(AObserver) and not Assigned(AValidator) then
  begin
    raise ENyxState.Create('State subscription needs an observer or validator');
  end;

  if (Length(FSubscriptions) >= NyxMaximumStateSubscriptions) or
    (FSerial = High(Integer)) then
  begin
    raise ENyxState.Create('State exceeds its subscription/serial budget');
  end;
  Inc(FSerial);
  Result := TNyxStateSubscription.Create;
  try
    Result.FSerial := FSerial;
    Result.FObserver := AObserver;
    Result.FValidator := AValidator;
    SetLength(FSubscriptions, Length(FSubscriptions) + 1);
    FSubscriptions[Length(FSubscriptions) - 1] := Result;
    Result.FState := Self;
  except
    Result.Free;
    raise;
  end;
end;

procedure TNyxState.Publish(ACandidate: TNyxState; AChanges: TNyxStateChanges);
var
  LIDs: array of Integer;
  LIndex: Integer;
  LConnection: TNyxStateSubscription;
  LError: TNyxText;
  LFailed: Boolean;
begin

  if (AChanges.Count = 0) and not AChanges.OrderChanged then
  begin
    Exit;
  end;

  if FRevision = High(Integer) then
  begin
    raise ENyxState.Create('State revision budget is exhausted');
  end;
  ACandidate.Validate;
  ACandidate.FReadOnly := True;
  ACandidate.FRevision := FRevision + 1;
  SetLength(LIDs, Length(FSubscriptions));
  for LIndex := 0 to Length(FSubscriptions) - 1 do
  begin
    LIDs[LIndex] := FSubscriptions[LIndex].FSerial;
  end;
  FBusy := True;
  try
    { Snapshot serials, not object pointers: a callback may disconnect/free its
      token or another token. Re-resolving the serial safely skips removed ones. }
    for LIndex := 0 to Length(LIDs) - 1 do
    begin
      LConnection := Subscription(LIDs[LIndex]);

      if (LConnection <> nil) and Assigned(LConnection.FValidator) then
      begin
        LConnection.FValidator(ACandidate, AChanges);
      end;
    end;
    { Transfer detached arrays only after every validator succeeds. Clear the
      candidate references without resizing shared pas2js arrays. }
    FKeys := ACandidate.FKeys;
    FValues := ACandidate.FValues;
    ACandidate.FKeys := nil;
    ACandidate.FValues := nil;
    Inc(FRevision);
    LError := '';
    LFailed := False;
    for LIndex := 0 to Length(LIDs) - 1 do
    begin
      LConnection := Subscription(LIDs[LIndex]);

      if (LConnection <> nil) and Assigned(LConnection.FObserver) then
      begin
        try
          LConnection.FObserver(Self, AChanges);
        except
          on LException: Exception do
          begin

            if not LFailed then
            begin
              LError := TNyxText(LException.Message);
              LFailed := True;
            end;
          end;
        end;
      end;
    end;

    if LFailed then
    begin
      raise ENyxStateNotification.Create('State committed; observer failed: ' + LError);
    end;
  finally
    FBusy := False;
  end;
end;

procedure TNyxState.CheckReference(const AKey: TNyxText; AKind: TNyxStateKind);
begin

  if Has(AKey) and (Value(AKey).Kind <> AKind) then
  begin
    raise ENyxState.Create('State reference has a different kind: ' + AKey);
  end;
end;

function TNyxState.GetValue(const ARef: TNyxTextStateRef): TNyxText;
begin
  Result := Value(ARef.Name).TextValue;
end;

function TNyxState.SetValue(const ARef: TNyxTextStateRef; const AValue: TNyxText): TNyxState;
begin
  Apply([NyxStateValue(ARef, AValue)]);
  Result := Self;
end;

function TNyxState.Remove(const ARef: TNyxTextStateRef): TNyxState;
begin
  CheckReference(ARef.Name, nskText);
  Result := Remove(ARef.Name);
end;

function TNyxState.GetValue(const ARef: TNyxBooleanStateRef): Boolean;
begin
  Result := Value(ARef.Name).BooleanValue;
end;

function TNyxState.SetValue(const ARef: TNyxBooleanStateRef; const AValue: Boolean): TNyxState;
begin
  Apply([NyxStateValue(ARef, AValue)]);
  Result := Self;
end;

function TNyxState.Remove(const ARef: TNyxBooleanStateRef): TNyxState;
begin
  CheckReference(ARef.Name, nskBoolean);
  Result := Remove(ARef.Name);
end;

function TNyxState.GetValue(const ARef: TNyxIntegerStateRef): Integer;
begin
  Result := Value(ARef.Name).IntegerValue;
end;

function TNyxState.SetValue(const ARef: TNyxIntegerStateRef; const AValue: Integer): TNyxState;
begin
  Apply([NyxStateValue(ARef, AValue)]);
  Result := Self;
end;

function TNyxState.Remove(const ARef: TNyxIntegerStateRef): TNyxState;
begin
  CheckReference(ARef.Name, nskInteger);
  Result := Remove(ARef.Name);
end;

function TNyxState.GetValue(const ARef: TNyxNumberStateRef): Double;
begin
  Result := Value(ARef.Name).NumberValue;
end;

function TNyxState.SetValue(const ARef: TNyxNumberStateRef; const AValue: Double): TNyxState;
begin
  Apply([NyxStateValue(ARef, AValue)]);
  Result := Self;
end;

function TNyxState.Remove(const ARef: TNyxNumberStateRef): TNyxState;
begin
  CheckReference(ARef.Name, nskNumber);
  Result := Remove(ARef.Name);
end;

procedure TNyxState.Apply(const AValues: array of TNyxStateAssignment;
  AExpectedRevision: Integer);
var
  LCandidate: TNyxState;
  LChanges: TNyxStateChanges;
  LIndex: Integer;
  LSeen: TNyxStrings;
  LBefore: TNyxStateValue;
  LHadValue: Boolean;
  LChanged: Boolean;
begin
  CheckWritable(AExpectedRevision);
  LCandidate := Clone;
  LChanges := nil;
  LSeen := nil;
  try
    LChanges := TNyxStateChanges.Create;
    LSeen := TNyxStrings.Create;
    for LIndex := 0 to High(AValues) do
    begin
      TextBytes(AValues[LIndex].Key, True);

      if LSeen.IndexOf(AValues[LIndex].Key) >= 0 then
      begin
        raise ENyxState.Create('Duplicate state key in one update: ' + AValues[LIndex].Key);
      end;
      LSeen.Add(AValues[LIndex].Key);
      LHadValue := Has(AValues[LIndex].Key);
      LBefore := TNyxStateValue.FromText('');

      if LHadValue then
      begin
        LBefore := Value(AValues[LIndex].Key);
      end;

      if AValues[LIndex].Exists then
      begin
        AValues[LIndex].Value.Validate;

        if LHadValue and (LBefore.Kind <> AValues[LIndex].Value.Kind) then
        begin
          raise ENyxState.Create('State update changes an existing value kind: ' + AValues[LIndex].Key);
        end;
      end;
      LChanged := LHadValue <> AValues[LIndex].Exists;

      if LHadValue and AValues[LIndex].Exists then
      begin
        LChanged := not LBefore.SameValue(AValues[LIndex].Value);
      end;

      if LChanged then
      begin
        LChanges.Add(AValues[LIndex].Key, LBefore, AValues[LIndex].Value,
          LHadValue, AValues[LIndex].Exists);
        LCandidate.Put(AValues[LIndex].Key, AValues[LIndex].Value, AValues[LIndex].Exists);
      end;
    end;
    Publish(LCandidate, LChanges);
  finally
    LSeen.Free;
    LChanges.Free;
    LCandidate.Free;
  end;
end;

function TNyxState.Remove(const AKey: TNyxText): TNyxState;
begin
  Apply([NyxStateRemove(AKey)]);
  Result := Self;
end;

procedure TNyxState.Assign(ASource: TNyxState; AExpectedRevision: Integer);
var
  LCandidate: TNyxState;
  LChanges: TNyxStateChanges;
  LIndex: Integer;
  LBefore: TNyxStateValue;
begin
  CheckWritable(AExpectedRevision);

  if ASource = nil then
  begin
    raise ENyxState.Create('Source state is required');
  end;
  ASource.Validate;
  LCandidate := ASource.Clone;
  LChanges := nil;
  try
    LChanges := TNyxStateChanges.Create;
    LChanges.FOrderChanged := Count <> ASource.Count;

    if not LChanges.FOrderChanged then
    begin
      for LIndex := 0 to Count - 1 do
      begin

        if Key(LIndex) <> ASource.Key(LIndex) then
        begin
          LChanges.FOrderChanged := True;
          Break;
        end;
      end;
    end;
    for LIndex := 0 to Count - 1 do
    begin

      if not ASource.Has(FKeys[LIndex]) then
      begin
        LChanges.Add(FKeys[LIndex], FValues[LIndex], TNyxStateValue.FromText(''), True, False);
      end;
    end;
    for LIndex := 0 to ASource.Count - 1 do
    begin
      LBefore := TNyxStateValue.FromText('');

      if Has(ASource.Key(LIndex)) then
      begin
        LBefore := Value(ASource.Key(LIndex));
      end;

      if not Has(ASource.Key(LIndex)) or
        not LBefore.SameValue(ASource.Value(ASource.Key(LIndex))) then
      begin
        LChanges.Add(ASource.Key(LIndex), LBefore,
          ASource.Value(ASource.Key(LIndex)), Has(ASource.Key(LIndex)), True);
      end;
    end;
    Publish(LCandidate, LChanges);
  finally
    LChanges.Free;
    LCandidate.Free;
  end;
end;

end.
