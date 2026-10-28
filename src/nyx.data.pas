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

unit nyx.data;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text;

type
  {$ifdef NYX_SOURCE_PROFILE}
  { Isolated diagnostic counters only. InputUnits counts native UTF-8 bytes or
    browser UTF-16 storage units, not a portable wire-byte claim. Measurements
    include failed reads and are cumulative on one diagnostic execution thread. }
  TNyxDataProfileSample = record
    Calls: Integer;
    InputUnits: Double;
    Milliseconds: Double;
  end;
  {$endif}

  { Structured extension data has a closed value vocabulary, independent of
    target controls. Number retains its admitted decimal spelling; converting
    it to Double is an explicit read, never the persistence representation. }
  TNyxDataKind = (ndNull, ndText, ndBoolean, ndNumber, ndObject, ndArray);

  { A decimal reference preserves precision beyond a target's Double mantissa.
    NyxDecimal admits strict, finite JSON numbers using the shared decoder.
    Text is immutable; the default record is invalid until constructed. }
  TNyxDecimal = record
  private
    FText: TNyxText;
  public
    property Text: TNyxText read FText;
  end;

  { Immutable, owned-by-value snapshot. There are no borrowed JSON containers,
    child objects or caller arrays to mutate after admission. Copy explicitly
    copies the record while sharing only private immutable text/index storage.
    Default records are invalid. Re-admission creates a fresh index before any
    writes; published index arrays must never be modified in place.
    ParseJSON/ToJSON are explicit interchange boundaries; authoring uses typed
    factories below. Object member order and array order remain significant.
    Immediate members are indexed once beside canonical text; reads never parse
    unrelated siblings. No mutable JSON tree or descendant payload is retained.
    Text reads reuse the exact admitted scalar. Immediate text members retain
    only their decoded scalar, not nested payloads or JSON trees. Scalar child
    extraction reuses admission when its formatter policy is unchanged. Nested
    containers rebuild only their own immediate index from that admitted text;
    changed policies retain ordinary re-admission. A child owns its snapshot
    and never retains a parent or its index. }
  TNyxDataValue = record
  private
    FKind: TNyxDataKind;
    FJSON: TNyxText;
    FText: TNyxText;
    FReadBudget: Boolean;
    FStrictEscaping: Boolean;
    FCompressedJSON: Boolean;
    FQuotedMembers: Boolean;
    FMemberNames: array of TNyxText;
    FMemberStarts: array of Integer;
    FMemberLengths: array of Integer;
    FMemberTexts: array of TNyxText;
    function GetDefined: Boolean;
    procedure RequireKind(AKind: TNyxDataKind);
    procedure RequireReadBudget;
    function Member(AIndex: Integer): TNyxDataValue;
  public
    class function ParseJSON(const ASource: TNyxText): TNyxDataValue; static;
    procedure Validate;
    function Copy: TNyxDataValue;
    function ToJSON: TNyxText;
    function AsText: TNyxText;
    function AsBoolean: Boolean;
    { Integer reads require a signed 32-bit integer spelling. Decimal/exponent
      forms use AsDecimal/AsNumber, without implicit rounding into an integer. }
    function AsInteger: Integer;
    { Explicit approximate IEEE Double projection; ToJSON/AsDecimal remain exact. }
    function AsNumber: Double;
    function AsDecimal: TNyxDecimal;
    { Count applies to arrays/objects. Field requires an object member and Item
      an array index. Returned values are independent immutable snapshots;
      missing keys, wrong kinds and invalid indices raise a contract error. }
    function Count: Integer;
    function Key(AIndex: Integer): TNyxText;
    function Field(const AName: TNyxText): TNyxDataValue;
    function Item(AIndex: Integer): TNyxDataValue;
    property Kind: TNyxDataKind read FKind;
    { Presence check for optional value records. Default is absent; NyxNull is
      an explicitly constructed value. Reads/copies still validate the payload. }
    property Defined: Boolean read GetDefined;
  end;

  { Fields and references carry open application names as data. Keys are exact
    Unicode, case-sensitive, and may include NUL or be empty, as JSON permits.
    Use a namespace such as 'my-company.assets' to avoid future collisions. }
  TNyxDataField = record
    Name: TNyxText;
    Value: TNyxDataValue;
  end;

  { Admitted open extension identity. Construction validates exact Unicode and
    the quoted JSON byte budget without creating a temporary data tree. Copies
    retain only immutable name/initialization strings, never a store or owner. }
  TNyxExtensionRef = record
  private
    FName: TNyxText;
    FInitialized: TNyxText;
  public
    procedure Validate;
    function Copy: TNyxExtensionRef;
    property Name: TNyxText read FName;
  end;

  { Scope protects the standard wire fields on each kind of owner. Opaque data
    cannot replace document version/pages/state or a node's kind/props/bindings. }
  TNyxExtensionScope = (nesDocument, nesNode);

  TNyxExtensionEntry = record
    Key: TNyxExtensionRef;
    Value: TNyxDataValue;
  end;

  { Document/node-owned ordered data. Readers receive immutable snapshots.
    SetValue, Assign and Overlay admit their complete candidate before mutation;
    rejection retains values and ordering. Overlay replaces a whole matching
    value, without implicitly merging nested objects. Stores never retain
    subscriptions, renderer handles or pointers to their owning tree.
    Clone is caller-owned; other fluent methods return this borrowed store. }
  TNyxExtensions = class
  private
    FScope: TNyxExtensionScope;
    FEntries: array of TNyxExtensionEntry;
    function GetCount: Integer;
    function IndexOf(const AKey: TNyxExtensionRef): Integer;
    procedure CheckKey(const AKey: TNyxExtensionRef);
    procedure CheckIndex(AIndex: Integer);
  public
    constructor Create(AScope: TNyxExtensionScope);
    function Key(AIndex: Integer): TNyxExtensionRef;
    function Has(const AKey: TNyxExtensionRef): Boolean;
    function Value(const AKey: TNyxExtensionRef): TNyxDataValue;
    function SetValue(const AKey: TNyxExtensionRef;
      const AValue: TNyxDataValue): TNyxExtensions;
    function Remove(const AKey: TNyxExtensionRef): TNyxExtensions;
    procedure Assign(ASource: TNyxExtensions);
    procedure Overlay(ASource: TNyxExtensions);
    function Clone: TNyxExtensions;
    procedure Validate;
    { Serializes only the ordered extension object, without standard fields. }
    function ToJSON: TNyxText;
    { Explicit interchange boundary. Requires an object of extension fields;
      recognized owner fields and invalid candidates preserve this store. }
    procedure LoadJSON(const ASource: TNyxText);
    property Scope: TNyxExtensionScope read FScope;
    property Count: Integer read GetCount;
  end;

{ Typed factories never infer Boolean/numeric behavior from ordinary text.
  NyxObject/NyxArray copy all members before returning, reject duplicate decoded
  keys and observe the shared byte/depth/member budgets. Null is explicit. }
function NyxNull: TNyxDataValue;
function NyxData(const AValue: TNyxText): TNyxDataValue; overload;
function NyxData(AValue: Boolean): TNyxDataValue; overload;
function NyxData(AValue: Integer): TNyxDataValue; overload;
function NyxData(AValue: Double): TNyxDataValue; overload;
function NyxData(const AValue: TNyxDecimal): TNyxDataValue; overload;
function NyxDecimal(const AText: TNyxText): TNyxDecimal;
function NyxField(const AName: TNyxText; const AValue: TNyxDataValue): TNyxDataField;
function NyxObject(const AFields: array of TNyxDataField): TNyxDataValue;
function NyxArray(const AItems: array of TNyxDataValue): TNyxDataValue;
function NyxExtension(const AName: TNyxText): TNyxExtensionRef;
{ Exact standard field mapping shared by model admission and codec boundaries. }
function NyxReservedField(AScope: TNyxExtensionScope; const AName: TNyxText): Boolean;

{$ifdef NYX_SOURCE_PROFILE}
{ Diagnostic consumers reset/read copied counters; no data or caller is retained. }
procedure ResetNyxDataProfile;
function ReadNyxDataProfile: TNyxDataProfileSample;
{$endif}

implementation

uses
  fpjson,
  nyx.json,
  nyx.state
  {$ifdef PAS2JS}, JS{$endif}
  {$if defined(PAS2JS) and defined(NYX_SOURCE_PROFILE)}, WebOrWorker{$endif};

{$ifdef NYX_SOURCE_PROFILE}
var
  GDataProfile: TNyxDataProfileSample;

function DataProfileClock: Double;
begin
  {$ifdef PAS2JS}
  Result := performance.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

procedure ResetNyxDataProfile;
begin
  GDataProfile := Default(TNyxDataProfileSample);
end;

function ReadNyxDataProfile: TNyxDataProfileSample;
begin
  Result := GDataProfile;
end;
{$endif}

const
  CExtensionReferenceMarker = 'nyx.extension';

function QuoteJSON(const AText: TNyxText): TNyxText;
begin
  Result := EncodeNyxJSONString(AText, TJSONString.StrictEscaping);
end;

{ Index one owned, already admitted canonical container. AData is the borrowed
  decoder tree during first admission, or nil when extracting a child from an
  immutable parent under exactly the same formatter policy. No tree or parent
  survives this call. Only immediate names/text and value spans are retained;
  nested payloads are scanned for boundaries, never decoded into another tree.
  This is private post-admission work, never an arbitrary JSON input boundary. }
procedure IndexNyxCanonicalContainer(var AValue: TNyxDataValue; AData: TJSONData);
var
  LPosition: Integer;
  LMember: Integer;
  LCapacity: Integer;
  LClosing: Char;
  LName: TNyxText;

  procedure SkipSpace;
  begin
    while LPosition <= Length(AValue.FJSON) do
    begin
      case AValue.FJSON[LPosition] of
        #9, #10, #13, ' ':
          begin
            Inc(LPosition);
          end;
        else
          begin
            Break;
          end;
      end;
    end;
  end;

  procedure Take(AChar: Char);
  begin
    SkipSpace;

    if (LPosition > Length(AValue.FJSON)) or (AValue.FJSON[LPosition] <> AChar) then
    begin
      raise ENyxJSON.Create('Snapshot formatter emitted an unexpected boundary');
    end;
    Inc(LPosition);
  end;

  procedure SkipString;
  begin
    Take('"');
    while LPosition <= Length(AValue.FJSON) do
    begin

      if AValue.FJSON[LPosition] = '"' then
      begin
        Inc(LPosition);
        Exit;
      end;

      if AValue.FJSON[LPosition] = '\' then
      begin
        Inc(LPosition);
      end;
      Inc(LPosition);
    end;
    raise ENyxJSON.Create('Snapshot formatter omitted a string terminator');
  end;

  function ReadText: TNyxText;
  var
    LStart: Integer;
    LEscaped: Boolean;
    LText: TJSONData;
  begin
    SkipSpace;
    LStart := LPosition;
    LEscaped := False;
    Take('"');
    while LPosition <= Length(AValue.FJSON) do
    begin

      if AValue.FJSON[LPosition] = '"' then
      begin
        Inc(LPosition);

        if not LEscaped then
        begin
          Exit(System.Copy(AValue.FJSON, LStart + 1, LPosition - LStart - 2));
        end;
        { Escapes retain the one existing Unicode/NUL decoder. This decodes
          only the selected immediate scalar, never its containing subtree. }
        LText := DecodeNyxJSON(System.Copy(AValue.FJSON, LStart, LPosition - LStart));
        try
          Exit(LText.AsString);
        finally
          LText.Free;
        end;
      end;

      if AValue.FJSON[LPosition] = '\' then
      begin
        LEscaped := True;
        Inc(LPosition);
      end;
      Inc(LPosition);
    end;
    raise ENyxJSON.Create('Snapshot formatter omitted a string terminator');
  end;

  procedure SkipValue;
  var
    LDepth: Integer;
  begin
    SkipSpace;

    if LPosition > Length(AValue.FJSON) then
    begin
      raise ENyxJSON.Create('Snapshot formatter omitted a value');
    end;

    if AValue.FJSON[LPosition] = '"' then
    begin
      SkipString;
    end
    else if (AValue.FJSON[LPosition] = '{') or (AValue.FJSON[LPosition] = '[') then
    begin
      LDepth := 0;
      repeat

        if LPosition > Length(AValue.FJSON) then
        begin
          raise ENyxJSON.Create('Snapshot formatter omitted a container terminator');
        end;

        { Case avoids constructing transient Pascal sets per source character
          in pas2js. Strings consume their own escaped punctuation atomically. }
        case AValue.FJSON[LPosition] of
          '"':
            begin
              SkipString;
            end;
          '{', '[':
            begin
              Inc(LDepth);
              Inc(LPosition);
            end;
          '}', ']':
            begin
              Dec(LDepth);
              Inc(LPosition);
            end;
          else
            begin
              Inc(LPosition);
            end;
        end;
      until LDepth = 0;
    end
    else
    begin
      while LPosition <= Length(AValue.FJSON) do
      begin
        case AValue.FJSON[LPosition] of
          #9, #10, #13, ' ', ',', '}', ']':
            begin
              Exit;
            end;
          else
            begin
              Inc(LPosition);
            end;
        end;
      end;
    end;
  end;

  procedure Reserve(ACapacity: Integer);
  begin
    SetLength(AValue.FMemberStarts, ACapacity);
    SetLength(AValue.FMemberLengths, ACapacity);
    SetLength(AValue.FMemberTexts, ACapacity);

    if AValue.FKind = ndObject then
    begin
      SetLength(AValue.FMemberNames, ACapacity);
    end;
  end;
begin
  LPosition := 1;
  LMember := 0;
  LCapacity := 0;

  if AData <> nil then
  begin
    LCapacity := AData.Count;
    Reserve(LCapacity);
  end;

  if AValue.FKind = ndObject then
  begin
    Take('{');
    LClosing := '}';
  end
  else
  begin
    Take('[');
    LClosing := ']';
  end;
  SkipSpace;
  while (LPosition <= Length(AValue.FJSON)) and (AValue.FJSON[LPosition] <> LClosing) do
  begin

    if LMember > 0 then
    begin
      Take(',');
    end;

    if LMember = LCapacity then
    begin
      { Unknown child counts grow geometrically and are trimmed before publish.
        The admitted four-MiB text bounds the count and this arithmetic. }
      LCapacity := LCapacity * 2 + 8;
      Reserve(LCapacity);
    end;

    if AValue.FKind = ndObject then
    begin

      if AData <> nil then
      begin
        LName := TJSONObject(AData).Names[LMember];
        SkipString;
      end
      else
      begin
        LName := ReadText;
      end;
      AValue.FMemberNames[LMember] := LName;
      Take(':');
    end;
    SkipSpace;
    AValue.FMemberStarts[LMember] := LPosition;

    if (AData = nil) and (LPosition <= Length(AValue.FJSON)) and
      (AValue.FJSON[LPosition] = '"') then
    begin
      AValue.FMemberTexts[LMember] := ReadText;
    end
    else
    begin
      SkipValue;

      if (AData <> nil) and (AData.Items[LMember].JSONType = jtString) then
      begin
        AValue.FMemberTexts[LMember] := AData.Items[LMember].AsString;
      end;
    end;
    AValue.FMemberLengths[LMember] := LPosition - AValue.FMemberStarts[LMember];
    Inc(LMember);
    SkipSpace;
  end;
  Take(LClosing);
  SkipSpace;

  if LPosition <= Length(AValue.FJSON) then
  begin
    raise ENyxJSON.Create('Snapshot formatter emitted trailing data');
  end;
  Reserve(LMember);
end;

class function TNyxDataValue.ParseJSON(const ASource: TNyxText): TNyxDataValue;
const
  CKinds: array[TJSONType] of TNyxDataKind =
    (ndNull, ndNumber, ndText, ndBoolean, ndNull, ndArray, ndObject);
var
  LData: TJSONData;
  LType: TJSONType;
  {$ifdef NYX_SOURCE_PROFILE}
  LProfileStarted: Double;
  {$endif}
  {$ifdef PAS2JS}
  LIndex: Integer;
  LScalar: Integer;
  LBytes: Integer;
  {$endif}

begin
  {$ifdef NYX_SOURCE_PROFILE}
  LProfileStarted := DataProfileClock;
  try
  {$endif}
  LData := DecodeNyxJSON(ASource);
  try
    { Evaluate the virtual class function before indexing. Older pas2js can
      otherwise emit its function object as the array subscript. }
    LType := LData.JSONType;
    Result.FKind := CKinds[LType];
    Result.FStrictEscaping := TJSONString.StrictEscaping;
    Result.FCompressedJSON := TJSONData.CompressedJSON;
    Result.FQuotedMembers := not TJSONObject.UnquotedMemberNames;
    Result.FJSON := LData.AsJSON;
    Result.FText := '';
    Result.FMemberNames := nil;
    Result.FMemberStarts := nil;
    Result.FMemberLengths := nil;
    Result.FMemberTexts := nil;
    { Formatting can expand an admitted raw string/container beyond the reader's
      byte budget. Preserve the former read-time refusal instead of making a
      cached read bypass it. Native TNyxText units are already UTF-8 bytes. }
    Result.FReadBudget := Length(Result.FJSON) <= NyxMaximumJSONBytes;
    {$ifdef PAS2JS}
    LIndex := 1;
    LBytes := 0;
    while Result.FReadBudget and (LIndex <= Length(Result.FJSON)) do
    begin
      { Repeated large resource envelopes are mostly ASCII. Count those units
        directly instead of allocating scalar var/out bridges; high units retain
        strict Unicode checks and the same formatted UTF-8 read-budget refusal. }

      if Ord(Result.FJSON[LIndex]) <= $7f then
      begin
        Inc(LIndex);
        Inc(LBytes);
      end
      else
      begin

        if not NyxNextScalar(Result.FJSON, LIndex, LScalar) then
        begin
          raise ENyxJSON.Create('Snapshot formatter emitted malformed Unicode');
        end;

        if LScalar <= $7ff then
        begin
          Inc(LBytes, 2);
        end
        else if LScalar <= $ffff then
        begin
          Inc(LBytes, 3);
        end
        else
        begin
          Inc(LBytes, 4);
        end;
      end;
      Result.FReadBudget := LBytes <= NyxMaximumJSONBytes;
    end;
    {$endif}

    if Result.FKind = ndText then
    begin
      Result.FText := LData.AsString;
    end;

    if Result.FReadBudget and (Result.FKind in [ndObject, ndArray]) then
    begin
      IndexNyxCanonicalContainer(Result, LData);
    end;
  finally
    LData.Free;
  end;
  {$ifdef NYX_SOURCE_PROFILE}
  finally
    Inc(GDataProfile.Calls);
    GDataProfile.InputUnits := GDataProfile.InputUnits + Length(ASource);
    GDataProfile.Milliseconds := GDataProfile.Milliseconds + DataProfileClock - LProfileStarted;
  end;
  {$endif}
end;

procedure TNyxDataValue.Validate;
begin

  if FJSON = '' then
  begin
    raise ENyxJSON.Create('Construct a typed data value before use');
  end;
end;

procedure TNyxDataValue.RequireKind(AKind: TNyxDataKind);
begin
  Validate;

  if FKind <> AKind then
  begin
    raise ENyxJSON.Create('Extension value has a different data kind');
  end;
end;

procedure TNyxDataValue.RequireReadBudget;
begin

  if not FReadBudget then
  begin
    raise ENyxJSON.Create('JSON exceeds 4 MiB formatted UTF-8 read budget');
  end;
end;

function TNyxDataValue.Member(AIndex: Integer): TNyxDataValue;
var
  LJSON: TNyxText;
  LValue: TNyxDataValue;
begin
  RequireReadBudget;

  if (AIndex < 0) or (AIndex >= Length(FMemberStarts)) then
  begin
    raise ENyxJSON.Create('Data member index out of range');
  end;
  LJSON := System.Copy(FJSON, FMemberStarts[AIndex], FMemberLengths[AIndex]);
  { The complete parent already admitted syntax, finite exact numbers, Unicode
    and budgets. A child span cannot exceed that parent's formatted budget or
    nesting/member limits. Its new immediate index owns only the selected text;
    retaining a parent/index or eagerly caching descendant snapshots would
    multiply memory for deep documents.

    The formatter preference is mutable. Its change must still re-admit and
    reformat the selected span, including the established case where formatting
    expands a child beyond its read budget. Container whitespace also depends
    on CompressedJSON. Unquoted-member output is not strict JSON, so neither
    an old nor a current unquoted policy grants trusted container extraction. }

  if (FStrictEscaping = TJSONString.StrictEscaping) and
    (not (LJSON[1] in ['{', '[']) or
    ((FCompressedJSON = TJSONData.CompressedJSON) and FQuotedMembers and
      not TJSONObject.UnquotedMemberNames)) then
  begin
    { Build before publishing: a compiler may reuse receiver storage when the
      caller replaces a container with its own child. All parent reads finish
      while that parent's private arrays are still alive. }
    LValue := Default(TNyxDataValue);
    LValue.FJSON := LJSON;
    LValue.FReadBudget := True;
    LValue.FStrictEscaping := FStrictEscaping;
    LValue.FCompressedJSON := FCompressedJSON;
    LValue.FQuotedMembers := FQuotedMembers;
    case LJSON[1] of
      '"':
        begin
          LValue.FKind := ndText;
          LValue.FText := FMemberTexts[AIndex];
        end;
      'n':
        begin
          LValue.FKind := ndNull;
        end;
      't', 'f':
        begin
          LValue.FKind := ndBoolean;
        end;
      '{':
        begin
          LValue.FKind := ndObject;
        end;
      '[':
        begin
          LValue.FKind := ndArray;
        end;
      else
        begin
          LValue.FKind := ndNumber;
        end;
    end;

    if LValue.FKind in [ndObject, ndArray] then
    begin
      IndexNyxCanonicalContainer(LValue, nil);
    end;
    Result := LValue;
  end
  else
  begin
    Result := ParseJSON(LJSON);
  end;
end;

function TNyxDataValue.Copy: TNyxDataValue;
var
  LValue: TNyxDataValue;
begin
  Validate;
  { Admission/extraction builds each index in a fresh candidate before
    publication. Its elements are private immutable text/integer values,
    so a snapshot needs retained storage, not three fresh arrays on every read.
    There is still no shared mutable tree, parent snapshot or owner reference.
    Build the whole local first: a caller may replace the receiver with its own
    Copy result, including compiler paths that reuse hidden result storage. }
  LValue.FKind := FKind;
  LValue.FJSON := FJSON;
  LValue.FText := FText;
  LValue.FReadBudget := FReadBudget;
  LValue.FStrictEscaping := FStrictEscaping;
  LValue.FCompressedJSON := FCompressedJSON;
  LValue.FQuotedMembers := FQuotedMembers;
  LValue.FMemberNames := FMemberNames;
  LValue.FMemberStarts := FMemberStarts;
  LValue.FMemberLengths := FMemberLengths;
  LValue.FMemberTexts := FMemberTexts;
  Result := LValue;
end;

function TNyxDataValue.GetDefined: Boolean;
begin
  Result := FJSON <> '';
end;

function TNyxDataValue.ToJSON: TNyxText;
begin
  Validate;
  Result := FJSON;
end;

function TNyxDataValue.AsText: TNyxText;
begin
  RequireKind(ndText);
  RequireReadBudget;
  Result := FText;
end;

function TNyxDataValue.AsBoolean: Boolean;
begin
  RequireKind(ndBoolean);
  Result := FJSON = 'true';
end;

function TNyxDataValue.AsInteger: Integer;
begin
  RequireKind(ndNumber);

  { The snapshot already passed strict JSON syntax. Refuse decimal/exponent
    spellings before RTL conversion so rounded Double reads never admit them. }

  if (Length(FJSON) > 11) or (Pos('.', FJSON) > 0) or (Pos('e', FJSON) > 0) or
    (Pos('E', FJSON) > 0) or not TryStrToInt(FJSON, Result) then
  begin
    raise ENyxJSON.Create('Data integer requires a signed 32-bit integer spelling');
  end;
end;

function TNyxDataValue.AsNumber: Double;
begin
  RequireKind(ndNumber);

  if not TryNyxStateNumber(FJSON, Result) then
  begin
    raise ENyxJSON.Create('Data number requires a finite decimal');
  end;
end;

function TNyxDataValue.AsDecimal: TNyxDecimal;
begin
  RequireKind(ndNumber);
  Result.FText := FJSON;
end;

function TNyxDataValue.Count: Integer;
begin
  Validate;

  if not (FKind in [ndArray, ndObject]) then
  begin
    raise ENyxJSON.Create('Count requires an array or object');
  end;
  RequireReadBudget;
  Result := Length(FMemberStarts);
end;

function TNyxDataValue.Key(AIndex: Integer): TNyxText;
begin
  RequireKind(ndObject);
  RequireReadBudget;

  if (AIndex < 0) or (AIndex >= Length(FMemberNames)) then
  begin
    raise ENyxJSON.Create('Data member index out of range');
  end;
  Result := FMemberNames[AIndex];
end;

function TNyxDataValue.Field(const AName: TNyxText): TNyxDataValue;
var
  LIndex: Integer;
begin
  RequireKind(ndObject);
  RequireReadBudget;
  for LIndex := 0 to High(FMemberNames) do
  begin

    if FMemberNames[LIndex] = AName then
    begin
      Exit(Member(LIndex));
    end;
  end;
  raise ENyxJSON.Create('Data member is missing: ' + AName);
end;

function TNyxDataValue.Item(AIndex: Integer): TNyxDataValue;
begin
  RequireKind(ndArray);
  RequireReadBudget;

  if (AIndex < 0) or (AIndex >= Length(FMemberStarts)) then
  begin
    raise ENyxJSON.Create('Data item index out of range');
  end;
  Result := Member(AIndex);
end;

function NyxNull: TNyxDataValue;
begin
  { Closed Pascal scalar domains need no JSON grammar/tree to establish their
    meaning. These short canonical literals are within every read budget. }
  Result := Default(TNyxDataValue);
  Result.FKind := ndNull;
  Result.FJSON := 'null';
  Result.FReadBudget := True;
end;

function NyxData(const AValue: TNyxText): TNyxDataValue;
var
  LStrict: Boolean;
begin
  { Typed text has no container grammar, duplicate names or numeric spelling to
    resolve. Admit its complete Unicode and quoted UTF-8 budget directly, then
    retain the same formatter spelling. Avoid decoding/formatting a second copy
    of large source/resource strings merely to recover this exact input scalar.
    Snapshot the formatter preference once so admission and storage agree even
    if another owner changes that external preference. Arbitrary JSON continues
    through ParseJSON and its full admission boundary. }
  LStrict := TJSONString.StrictEscaping;
  NyxJSONStringByteLength(AValue, LStrict);
  Result := Default(TNyxDataValue);
  Result.FKind := ndText;
  Result.FJSON := EncodeNyxJSONString(AValue, LStrict);
  Result.FText := AValue;
  Result.FReadBudget := True;
end;

function NyxData(AValue: Boolean): TNyxDataValue;
begin
  Result := Default(TNyxDataValue);
  Result.FKind := ndBoolean;
  Result.FReadBudget := True;

  if AValue then
  begin
    Result.FJSON := 'true';
  end
  else
  begin
    Result.FJSON := 'false';
  end;
end;

function NyxData(AValue: Integer): TNyxDataValue;
var
  LJSON: TNyxText;
  {$ifdef PAS2JS}
  LNumber: Double;
  {$endif}
begin
  LJSON := IntToStr(AValue);
  {$ifdef PAS2JS}
  { JavaScript bridges can violate an erased Integer parameter. Preserve the
    former full-parser behavior for fractions, nonfinite and out-of-range
    values rather than granting those values a trusted ordinal shortcut. }
  LNumber := AValue;

  if not isNumber(AValue) or (LNumber < -2147483648.0) or (LNumber > 2147483647.0) or
    (LNumber <> LNumber) or (Trunc(LNumber) <> LNumber) then
  begin
    Exit(TNyxDataValue.ParseJSON(LJSON));
  end;
  {$endif}
  Result := Default(TNyxDataValue);
  Result.FKind := ndNumber;
  Result.FJSON := LJSON;
  Result.FReadBudget := True;
end;

function NyxData(AValue: Double): TNyxDataValue;
begin
  Result := TNyxDataValue.ParseJSON(TNyxStateValue.FromNumber(AValue).NumberText);
end;

function NyxData(const AValue: TNyxDecimal): TNyxDataValue;
begin
  { Only NyxDecimal can populate this private spelling, after strict numeric
    admission. A default reference still takes the ordinary refusal path. }

  if AValue.Text = '' then
  begin
    Exit(TNyxDataValue.ParseJSON(AValue.Text));
  end;
  Result := Default(TNyxDataValue);
  Result.FKind := ndNumber;
  Result.FJSON := AValue.Text;
  Result.FReadBudget := True;
end;

function NyxDecimal(const AText: TNyxText): TNyxDecimal;
var
  LValue: TNyxDataValue;
begin
  LValue := TNyxDataValue.ParseJSON(AText);
  LValue.RequireKind(ndNumber);
  Result.FText := LValue.ToJSON;
end;

function NyxField(const AName: TNyxText; const AValue: TNyxDataValue): TNyxDataField;
begin
  Result.Name := AName;
  Result.Value := AValue.Copy;
end;

function NyxObject(const AFields: array of TNyxDataField): TNyxDataValue;
var
  LParts: TNyxStrings;
  LIndex: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    for LIndex := 0 to High(AFields) do
    begin
      LParts.Add(QuoteJSON(AFields[LIndex].Name) + ':' + AFields[LIndex].Value.ToJSON);
    end;
    Result := TNyxDataValue.ParseJSON('{' + LParts.Join(',') + '}');
  finally
    LParts.Free;
  end;
end;

function NyxArray(const AItems: array of TNyxDataValue): TNyxDataValue;
var
  LParts: TNyxStrings;
  LIndex: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    for LIndex := 0 to High(AItems) do
    begin
      LParts.Add(AItems[LIndex].ToJSON);
    end;
    Result := TNyxDataValue.ParseJSON('[' + LParts.Join(',') + ']');
  finally
    LParts.Free;
  end;
end;

function NyxExtension(const AName: TNyxText): TNyxExtensionRef;
begin
  { Use the same quoted UTF-8 budget/escape policy as NyxData(text), without
    formatting, parsing and retiring a JSON tree for every typed name lookup.
    Keep exact source bytes/units, including '=', NUL and empty names. }
  NyxJSONStringByteLength(AName, TJSONString.StrictEscaping);
  Result.FName := AName;
  Result.FInitialized := CExtensionReferenceMarker;
end;

procedure TNyxExtensionRef.Validate;
begin

  { A managed marker is initialized even in ordinary native local records.
    A Boolean flag would contain uninitialized stack data on older FPC. }

  if FInitialized <> CExtensionReferenceMarker then
  begin
    raise ENyxJSON.Create('Construct a typed extension reference before use');
  end;
end;

function TNyxExtensionRef.Copy: TNyxExtensionRef;
begin
  Validate;
  Result.FName := FName;
  Result.FInitialized := FInitialized;
end;

function NyxReservedField(AScope: TNyxExtensionScope; const AName: TNyxText): Boolean;
begin
  { Preserve refusal of invalid cast/bridge ordinals, including on compilers
    that otherwise discard an exhaustive enum case's defensive fallback. }
  case Ord(AScope) of
    Ord(nesDocument):
      begin
        Result := (AName = 'version') or (AName = 'title') or (AName = 'state') or
          (AName = 'pages') or (AName = 'components');
      end;
    Ord(nesNode):
      begin
        Result := (AName = 'kind') or (AName = 'id') or (AName = 'props') or
          (AName = 'children') or (AName = 'bindings');
      end;
    else
      begin
        raise ENyxJSON.Create('Unknown extension owner scope');
      end;
  end;
end;

constructor TNyxExtensions.Create(AScope: TNyxExtensionScope);
begin
  inherited Create;
  { Check even explicitly cast invalid enum values before retaining the scope. }
  NyxReservedField(AScope, '');
  FScope := AScope;
end;

function TNyxExtensions.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

procedure TNyxExtensions.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxJSON.Create('Extension index out of range');
  end;
end;

procedure TNyxExtensions.CheckKey(const AKey: TNyxExtensionRef);
begin
  AKey.Validate;

  if NyxReservedField(FScope, AKey.Name) then
  begin
    raise ENyxJSON.Create('Extension cannot replace a standard field: ' + AKey.Name);
  end;
end;

function TNyxExtensions.IndexOf(const AKey: TNyxExtensionRef): Integer;
var
  LIndex: Integer;
begin
  AKey.Validate;
  for LIndex := 0 to Count - 1 do
  begin

    if FEntries[LIndex].Key.Name = AKey.Name then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TNyxExtensions.Key(AIndex: Integer): TNyxExtensionRef;
begin
  CheckIndex(AIndex);
  Result := FEntries[AIndex].Key.Copy;
end;

function TNyxExtensions.Has(const AKey: TNyxExtensionRef): Boolean;
begin
  CheckKey(AKey);
  Result := IndexOf(AKey) >= 0;
end;

function TNyxExtensions.Value(const AKey: TNyxExtensionRef): TNyxDataValue;
var
  LIndex: Integer;
begin
  CheckKey(AKey);
  LIndex := IndexOf(AKey);
  CheckIndex(LIndex);
  Result := FEntries[LIndex].Value.Copy;
end;

function TNyxExtensions.ToJSON: TNyxText;
var
  LParts: TNyxStrings;
  LIndex: Integer;
begin
  LParts := TNyxStrings.Create;
  try
    for LIndex := 0 to Count - 1 do
    begin
      LParts.Add(QuoteJSON(FEntries[LIndex].Key.Name) + ':' + FEntries[LIndex].Value.ToJSON);
    end;
    Result := '{' + LParts.Join(',') + '}';
  finally
    LParts.Free;
  end;
end;

procedure TNyxExtensions.Validate;
var
  LData: TJSONData;
begin

  if Count = 0 then
  begin
    Exit;
  end;
  { Reserve the five standard fields even if some are currently optional. }

  if Count > NyxMaximumJSONMembers - 5 then
  begin
    raise ENyxJSON.Create('Extension owner exceeds member budget');
  end;
  LData := DecodeNyxJSON(ToJSON);
  LData.Free;
end;

procedure TNyxExtensions.LoadJSON(const ASource: TNyxText);
var
  LData: TJSONData;
  LObject: TJSONObject;
  LCandidate: TNyxExtensions;
  LIndex: Integer;
begin
  LData := DecodeNyxJSON(ASource);
  LCandidate := nil;
  try

    if LData.JSONType <> jtObject then
    begin
      raise ENyxJSON.Create('Extensions require an object');
    end;
    LObject := TJSONObject(LData);
    LCandidate := TNyxExtensions.Create(FScope);
    SetLength(LCandidate.FEntries, LObject.Count);
    for LIndex := 0 to LObject.Count - 1 do
    begin
      LCandidate.FEntries[LIndex].Key := NyxExtension(LObject.Names[LIndex]);
      CheckKey(LCandidate.FEntries[LIndex].Key);
      LCandidate.FEntries[LIndex].Value := TNyxDataValue.ParseJSON(LObject.Items[LIndex].AsJSON);
    end;
    Assign(LCandidate);
  finally
    LCandidate.Free;
    LData.Free;
  end;
end;

function TNyxExtensions.SetValue(const AKey: TNyxExtensionRef;
  const AValue: TNyxDataValue): TNyxExtensions;
var
  LCandidate: TNyxExtensions;
  LIndex: Integer;
begin
  CheckKey(AKey);
  AValue.Validate;
  LCandidate := Clone;
  try
    LIndex := LCandidate.IndexOf(AKey);

    if LIndex < 0 then
    begin
      LIndex := LCandidate.Count;
      SetLength(LCandidate.FEntries, LIndex + 1);
      LCandidate.FEntries[LIndex].Key := AKey.Copy;
    end;
    LCandidate.FEntries[LIndex].Value := AValue.Copy;
    Assign(LCandidate);
  finally
    LCandidate.Free;
  end;
  Result := Self;
end;

function TNyxExtensions.Remove(const AKey: TNyxExtensionRef): TNyxExtensions;
var
  LIndex: Integer;
  LNext: Integer;
begin
  CheckKey(AKey);
  LIndex := IndexOf(AKey);

  if LIndex >= 0 then
  begin
    for LNext := LIndex to Count - 2 do
    begin
      FEntries[LNext].Key := FEntries[LNext + 1].Key.Copy;
      FEntries[LNext].Value := FEntries[LNext + 1].Value.Copy;
    end;
    SetLength(FEntries, Count - 1);
  end;
  Result := Self;
end;

procedure TNyxExtensions.Assign(ASource: TNyxExtensions);
var
  LIndex: Integer;
  LEntries: array of TNyxExtensionEntry;
begin

  if (ASource = nil) or (ASource.Scope <> FScope) then
  begin
    raise ENyxJSON.Create('Assign requires an extension store of the same scope');
  end;
  ASource.Validate;
  SetLength(LEntries, ASource.Count);
  for LIndex := 0 to ASource.Count - 1 do
  begin
    LEntries[LIndex].Key := ASource.FEntries[LIndex].Key.Copy;
    LEntries[LIndex].Value := ASource.FEntries[LIndex].Value.Copy;
  end;
  FEntries := LEntries;
end;

function TNyxExtensions.Clone: TNyxExtensions;
begin
  Result := TNyxExtensions.Create(FScope);
  try
    Result.Assign(Self);
  except
    Result.Free;
    raise;
  end;
end;

procedure TNyxExtensions.Overlay(ASource: TNyxExtensions);
var
  LCandidate: TNyxExtensions;
  LIndex: Integer;
begin

  if (ASource = nil) or (ASource.Scope <> FScope) then
  begin
    raise ENyxJSON.Create('Overlay requires an extension store of the same scope');
  end;

  if ASource.Count = 0 then
  begin
    Exit;
  end;
  LCandidate := Clone;
  try
    for LIndex := 0 to ASource.Count - 1 do
    begin
      LCandidate.SetValue(ASource.Key(LIndex), ASource.Value(ASource.Key(LIndex)));
    end;
    Assign(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

end.
