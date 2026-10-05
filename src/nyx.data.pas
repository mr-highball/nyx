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
    factories below. Object member order and array order remain significant. }
  TNyxDataValue = record
  private
    FKind: TNyxDataKind;
    FJSON: TNyxText;
    function GetDefined: Boolean;
    procedure RequireKind(AKind: TNyxDataKind);
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
begin
  LData := DecodeNyxJSON(ASource);
  try
    { Evaluate the virtual class function before indexing. Older pas2js can
      otherwise emit its function object as the array subscript. }
    LType := LData.JSONType;
    Result.FKind := CKinds[LType];
    Result.FJSON := LData.AsJSON;
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

function TNyxDataValue.Copy: TNyxDataValue;
begin
  Validate;
  Result.FKind := FKind;
  Result.FJSON := FJSON;
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
var
  LData: TJSONData;
begin
  RequireKind(ndText);
  LData := DecodeNyxJSON(FJSON);
  try
    Result := LData.AsString;
  finally
    LData.Free;
  end;
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
var
  LData: TJSONData;
begin
  Validate;

  if not (FKind in [ndArray, ndObject]) then
  begin
    raise ENyxJSON.Create('Count requires an array or object');
  end;
  LData := DecodeNyxJSON(FJSON);
  try
    Result := LData.Count;
  finally
    LData.Free;
  end;
end;

function TNyxDataValue.Key(AIndex: Integer): TNyxText;
var
  LData: TJSONData;
begin
  RequireKind(ndObject);
  LData := DecodeNyxJSON(FJSON);
  try

    if (AIndex < 0) or (AIndex >= LData.Count) then
    begin
      raise ENyxJSON.Create('Data member index out of range');
    end;
    Result := TJSONObject(LData).Names[AIndex];
  finally
    LData.Free;
  end;
end;

function TNyxDataValue.Field(const AName: TNyxText): TNyxDataValue;
var
  LData: TJSONData;
  LField: TJSONData;
begin
  RequireKind(ndObject);
  LData := DecodeNyxJSON(FJSON);
  try
    LField := TJSONObject(LData).Find(AName);

    if LField = nil then
    begin
      raise ENyxJSON.Create('Data member is missing: ' + AName);
    end;
    Result := ParseJSON(LField.AsJSON);
  finally
    LData.Free;
  end;
end;

function TNyxDataValue.Item(AIndex: Integer): TNyxDataValue;
var
  LData: TJSONData;
begin
  RequireKind(ndArray);
  LData := DecodeNyxJSON(FJSON);
  try

    if (AIndex < 0) or (AIndex >= LData.Count) then
    begin
      raise ENyxJSON.Create('Data item index out of range');
    end;
    Result := ParseJSON(LData.Items[AIndex].AsJSON);
  finally
    LData.Free;
  end;
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
