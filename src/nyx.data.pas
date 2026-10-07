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
    copies fields for pas2js record semantics. Default records are invalid.
    ParseJSON/ToJSON are explicit interchange boundaries; authoring uses typed
    factories below. Object member order and array order remain significant.
    Immediate members are indexed once beside canonical text; reads never parse
    unrelated siblings. No mutable JSON tree or descendant payload is retained.
    Text reads reuse the exact admitted scalar. Child extraction admits its own
    snapshot, so a small child does not retain a large parent or its index. }
  TNyxDataValue = record
  private
    FKind: TNyxDataKind;
    FJSON: TNyxText;
    FText: TNyxText;
    FReadBudget: Boolean;
    FMemberNames: array of TNyxText;
    FMemberStarts: array of Integer;
    FMemberLengths: array of Integer;
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

implementation

uses
  fpjson,
  nyx.json,
  nyx.state;

const
  CExtensionReferenceMarker = 'nyx.extension';

function QuoteJSON(const AText: TNyxText): TNyxText;
var
  LString: TJSONString;
begin
  LString := TJSONString.Create(AText);
  try
    Result := LString.AsJSON;
  finally
    LString.Free;
  end;
end;

class function TNyxDataValue.ParseJSON(const ASource: TNyxText): TNyxDataValue;
const
  CKinds: array[TJSONType] of TNyxDataKind =
    (ndNull, ndNumber, ndText, ndBoolean, ndNull, ndArray, ndObject);
var
  LData: TJSONData;
  LType: TJSONType;
  LPosition: Integer;
  LMember: Integer;
  {$ifdef PAS2JS}
  LIndex: Integer;
  LScalar: Integer;
  LBytes: Integer;
  {$endif}

  { Only trusted formatter output reaches this cursor. Admission still belongs
    to DecodeNyxJSON, including Unicode, exact numbers, duplicates and budgets.
    The cursor locates immediate spans without reparsing/formatting each child;
    it accepts formatter whitespace on either target and checks every boundary. }
  procedure SkipSpace;
  begin
    while (LPosition <= Length(Result.FJSON)) and
      (Result.FJSON[LPosition] in [#9, #10, #13, ' ']) do
    begin
      Inc(LPosition);
    end;
  end;

  procedure Take(AChar: Char);
  begin
    SkipSpace;

    if (LPosition > Length(Result.FJSON)) or (Result.FJSON[LPosition] <> AChar) then
    begin
      raise ENyxJSON.Create('Snapshot formatter emitted an unexpected boundary');
    end;
    Inc(LPosition);
  end;

  procedure SkipString;
  begin
    Take('"');
    while LPosition <= Length(Result.FJSON) do
    begin

      if Result.FJSON[LPosition] = '"' then
      begin
        Inc(LPosition);
        Exit;
      end;

      if Result.FJSON[LPosition] = '\' then
      begin
        { Escaped quotes/brackets are string data; strict admission has already
          qualified escape/surrogate syntax, including the following character. }
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

    if LPosition > Length(Result.FJSON) then
    begin
      raise ENyxJSON.Create('Snapshot formatter omitted a value');
    end;

    if Result.FJSON[LPosition] = '"' then
    begin
      SkipString;
    end
    else if Result.FJSON[LPosition] in ['{', '['] then
    begin
      LDepth := 0;
      repeat

        if LPosition > Length(Result.FJSON) then
        begin
          raise ENyxJSON.Create('Snapshot formatter omitted a container terminator');
        end;

        if Result.FJSON[LPosition] = '"' then
        begin
          SkipString;
        end
        else
        begin

          if Result.FJSON[LPosition] in ['{', '['] then
          begin
            Inc(LDepth);
          end
          else if Result.FJSON[LPosition] in ['}', ']'] then
          begin
            Dec(LDepth);
          end;
          Inc(LPosition);
        end;
      until LDepth = 0;
    end
    else
    begin
      while (LPosition <= Length(Result.FJSON)) and
        not (Result.FJSON[LPosition] in [#9, #10, #13, ' ', ',', '}', ']']) do
      begin
        Inc(LPosition);
      end;
    end;
  end;
begin
  LData := DecodeNyxJSON(ASource);
  try
    { Evaluate the virtual class function before indexing. Older pas2js can
      otherwise emit its function object as the array subscript. }
    LType := LData.JSONType;
    Result.FKind := CKinds[LType];
    Result.FJSON := LData.AsJSON;
    Result.FText := '';
    Result.FMemberNames := nil;
    Result.FMemberStarts := nil;
    Result.FMemberLengths := nil;
    { Formatting can expand an admitted raw string/container beyond the reader's
      byte budget. Preserve the former read-time refusal instead of making a
      cached read bypass it. Native TNyxText units are already UTF-8 bytes. }
    Result.FReadBudget := Length(Result.FJSON) <= NyxMaximumJSONBytes;
    {$ifdef PAS2JS}
    LIndex := 1;
    LBytes := 0;
    while Result.FReadBudget and (LIndex <= Length(Result.FJSON)) do
    begin

      if not NyxNextScalar(Result.FJSON, LIndex, LScalar) then
      begin
        raise ENyxJSON.Create('Snapshot formatter emitted malformed Unicode');
      end;

      if LScalar <= $7f then
      begin
        Inc(LBytes);
      end
      else if LScalar <= $7ff then
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
      Result.FReadBudget := LBytes <= NyxMaximumJSONBytes;
    end;
    {$endif}

    if Result.FKind = ndText then
    begin
      Result.FText := LData.AsString;
    end;

    if Result.FReadBudget and (Result.FKind in [ndObject, ndArray]) then
    begin
      SetLength(Result.FMemberStarts, LData.Count);
      SetLength(Result.FMemberLengths, LData.Count);

      if Result.FKind = ndObject then
      begin
        SetLength(Result.FMemberNames, LData.Count);
      end;
      LPosition := 1;

      if Result.FKind = ndObject then
      begin
        Take('{');
      end
      else
      begin
        Take('[');
      end;
      for LMember := 0 to LData.Count - 1 do
      begin

        if LMember > 0 then
        begin
          Take(',');
        end;

        if Result.FKind = ndObject then
        begin
          Result.FMemberNames[LMember] := TJSONObject(LData).Names[LMember];
          SkipString;
          Take(':');
        end;
        SkipSpace;
        Result.FMemberStarts[LMember] := LPosition;
        SkipValue;
        Result.FMemberLengths[LMember] := LPosition - Result.FMemberStarts[LMember];
      end;

      if Result.FKind = ndObject then
      begin
        Take('}');
      end
      else
      begin
        Take(']');
      end;
      SkipSpace;

      if LPosition <= Length(Result.FJSON) then
      begin
        raise ENyxJSON.Create('Snapshot formatter emitted trailing data');
      end;
    end;
  finally
    LData.Free;
  end;
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
begin
  RequireReadBudget;

  if (AIndex < 0) or (AIndex >= Length(FMemberStarts)) then
  begin
    raise ENyxJSON.Create('Data member index out of range');
  end;
  Result := ParseJSON(System.Copy(FJSON, FMemberStarts[AIndex], FMemberLengths[AIndex]));
end;

function TNyxDataValue.Copy: TNyxDataValue;
var
  LIndex: Integer;
begin
  Validate;
  Result.FKind := FKind;
  Result.FJSON := FJSON;
  Result.FText := FText;
  Result.FReadBudget := FReadBudget;
  Result.FMemberNames := nil;
  Result.FMemberStarts := nil;
  Result.FMemberLengths := nil;
  { The index contains only immutable text/integer values. Explicit arrays keep
    both compilers' record-copy semantics independent; no JSON owner is shared. }
  SetLength(Result.FMemberNames, Length(FMemberNames));
  for LIndex := 0 to High(FMemberNames) do
  begin
    Result.FMemberNames[LIndex] := FMemberNames[LIndex];
  end;
  SetLength(Result.FMemberStarts, Length(FMemberStarts));
  SetLength(Result.FMemberLengths, Length(FMemberLengths));
  for LIndex := 0 to High(FMemberStarts) do
  begin
    Result.FMemberStarts[LIndex] := FMemberStarts[LIndex];
    Result.FMemberLengths[LIndex] := FMemberLengths[LIndex];
  end;
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
  Result := TNyxDataValue.ParseJSON('null');
end;

function NyxData(const AValue: TNyxText): TNyxDataValue;
begin
  Result := TNyxDataValue.ParseJSON(QuoteJSON(AValue));
end;

function NyxData(AValue: Boolean): TNyxDataValue;
begin

  if AValue then
  begin
    Result := TNyxDataValue.ParseJSON('true');
  end
  else
  begin
    Result := TNyxDataValue.ParseJSON('false');
  end;
end;

function NyxData(AValue: Integer): TNyxDataValue;
begin
  Result := TNyxDataValue.ParseJSON(IntToStr(AValue));
end;

function NyxData(AValue: Double): TNyxDataValue;
begin
  Result := TNyxDataValue.ParseJSON(TNyxStateValue.FromNumber(AValue).NumberText);
end;

function NyxData(const AValue: TNyxDecimal): TNyxDataValue;
begin
  Result := TNyxDataValue.ParseJSON(AValue.Text);
  Result.RequireKind(ndNumber);
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
var
  LChecked: TNyxDataValue;
begin
  { Validate keys with the same Unicode/byte contract as a JSON string. No
    lossy name=value storage is involved, so '=', NUL and empty keys survive. }
  LChecked := NyxData(AName);
  Result.FName := LChecked.AsText;
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
