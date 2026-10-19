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
unit nyx.pascal.storage;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler;

type
  { A malformed shape, missing child or unqualified input publishes no plan.
    This is a storage-description boundary, not Pascal source admission. }
  ENyxPascalStorage = class(EArgumentException);
  { Independent finite storage descriptions refuse rather than truncate nodes,
    recursion or generated compiler input. No partial candidate is published. }
  ENyxPascalStorageLimit = class(ENyxPascalStorage);
  { A source-free data shape. It grants no nominal type, source-scope binding,
    compiler identity, size, alignment or application execution authority. }
  TNyxPascalStorageKind = (pskPrimitive, pskArray, pskRecord);
  { Closed compiler primitive spellings, selected only after actual binding.
    Width/alignment remain compiler-owned, including mode-dependent Integer,
    NativeInt, Char and Extended. No identifier/source text is accepted here. }
  TNyxPascalStoragePrimitive = (pspByte, pspShortInt, pspWord, pspSmallInt,
    pspLongword, pspLongint, pspQWord, pspInt64, pspInteger, pspCardinal,
    pspNativeInt, pspNativeUInt, pspSizeInt, pspSizeUInt, pspPointer,
    pspBoolean, pspByteBool, pspWordBool, pspLongBool, pspChar, pspWideChar,
    pspSingle, pspDouble, pspExtended, pspCurrency, pspComp, pspAnsiString,
    pspUnicodeString, pspWideString);
  { Authored container form; packed and bitpacked remain distinct compiler
    requests. The planner never computes either representation itself. }
  TNyxPascalStorageForm = (psfOrdinary, psfPacked, psfBitPacked);
  { PACKRECORDS intent attached to the exact record declaration. Default and C
    select the configured compiler's rules, never the analyzer host's ABI. }
  TNyxPascalStorageAlignment = (psaDefault, psaC, psa1, psa2, psa4, psa8,
    psa16, psa32);
  { Positive, copied construction caps. Zeroed options refuse; no interface or
    source/provider is retained. Generated declarations contain ASCII only. }
  TNyxPascalStorageOptions = record
  private
    FDefined: Boolean;
    FNodes, FDepth, FBytes: Integer;
  public
    class function Defaults: TNyxPascalStorageOptions; static;
    function UpToNodes(AValue: Integer): TNyxPascalStorageOptions;
    function UpToDepth(AValue: Integer): TNyxPascalStorageOptions;
    function UpToBytes(AValue: Integer): TNyxPascalStorageOptions;
    procedure Validate;
    property MaximumNodes: Integer read FNodes;
    property MaximumDepth: Integer read FDepth;
    property MaximumBytes: Integer read FBytes;
  end;
  { Immutable data-only tree. Primitive has no children, array has one element
    child and a positive length, record has ordered field children. Unused
    kind-specific getters refuse. Alternate providers must be caller-serialized
    while captured; no provider survives that operation. }
  INyxPascalStorageShape = interface(IInterface)
    ['{671C2493-7758-48B3-83DF-16ACBB7B1CC8}']
    function Kind: TNyxPascalStorageKind;
    function Primitive: TNyxPascalStoragePrimitive;
    function Form: TNyxPascalStorageForm;
    function Alignment: TNyxPascalStorageAlignment;
    function ArrayLength: Integer;
    function Count: Integer;
    function Child(AIndex: Integer): INyxPascalStorageShape;
  end;
  { Specialized fluent record builder. Each field creates a new owned shape;
    earlier records/children stay immutable and no parent back-link is retained. }
  INyxPascalStorageRecord = interface(INyxPascalStorageShape)
    ['{689216EC-C0A4-4CE9-9293-2D80F47F4B77}']
    function Field(const AType: INyxPascalStorageShape;
      const AOptions: TNyxPascalStorageOptions;
      const ACancellation: INyxCancellationScope = nil): INyxPascalStorageRecord;
  end;
  { A fully constructed sterile type fragment and exact structural key. The key
    describes storage only, never nominal equivalence or a measured ABI. Getters
    perform no deferred traversal/preparation and retain no source or execution. }
  INyxPascalStoragePlan = interface(IInterface)
    ['{1445A86E-193E-4C63-A727-2E490A445152}']
    function Shape: INyxPascalStorageShape;
    function Key: TNyxText;
    function TypeName: TNyxText;
    function Declarations: TNyxText;
    function NodeCount: Integer;
  end;

function NyxPascalStoragePrimitiveName(AValue: TNyxPascalStoragePrimitive): TNyxText;
function NewNyxPascalStoragePrimitive(AValue: TNyxPascalStoragePrimitive;
  const ACancellation: INyxCancellationScope = nil): INyxPascalStorageShape;
function NewNyxPascalStorageRecord(AForm: TNyxPascalStorageForm = psfOrdinary;
  AAlignment: TNyxPascalStorageAlignment = psaDefault;
  const ACancellation: INyxCancellationScope = nil): INyxPascalStorageRecord;
function NewNyxPascalStorageArray(const AElement: INyxPascalStorageShape;
  ALength: Integer; AForm: TNyxPascalStorageForm;
  const AOptions: TNyxPascalStorageOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalStorageShape;
function CaptureNyxPascalStorageShape(const ASource: INyxPascalStorageShape;
  const AOptions: TNyxPascalStorageOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalStorageShape;
function PlanNyxPascalStorage(const AShape: INyxPascalStorageShape;
  const AOptions: TNyxPascalStorageOptions;
  const ACancellation: INyxCancellationScope = nil): INyxPascalStoragePlan;

implementation

uses nyx.pascal.work, nyx.pascal.lexing;

type
  { Forward immutable implementation owner, borrowed only under IShapeOwner. }
  TStorageShape = class;
  { Sealed construction statistics. Public provider counts cannot assert node/
    depth/key bounds or skip capture. The raw owner is borrowed under this lease. }
  IShapeOwner = interface(IInterface)
    ['{81783BDB-B319-45BD-9078-A84C748A23A5}']
    function StorageShape: TStorageShape;
  end;
  { Class-owned dynamic child vector. Managed references never occur in a
    record/static array. Only constructors write it; descendants own no parent. }
  TStorageShape = class(TInterfacedObject, INyxPascalStorageShape, IShapeOwner)
  public
    ShapeKind: TNyxPascalStorageKind;
    Scalar: TNyxPascalStoragePrimitive;
    ContainerForm: TNyxPascalStorageForm;
    RecordAlignment: TNyxPascalStorageAlignment;
    LengthValue: Integer;
    Children: array of INyxPascalStorageShape;
    Nodes, Depth: Integer;
    Signature: TNyxText;
    function StorageShape: TStorageShape;
    function Kind: TNyxPascalStorageKind;
    function Primitive: TNyxPascalStoragePrimitive;
    function Form: TNyxPascalStorageForm;
    function Alignment: TNyxPascalStorageAlignment;
    function ArrayLength: Integer;
    function Count: Integer;
    function Child(AIndex: Integer): INyxPascalStorageShape;
  end;
  { Only record shapes expose the specialized fluent field interface. Scalars
    and arrays cannot advertise a record capability which would later refuse. }
  TStorageRecord = class(TStorageShape, INyxPascalStorageRecord)
  public
    function Field(const AType: INyxPascalStorageShape;
      const AOptions: TNyxPascalStorageOptions;
      const ACancellation: INyxCancellationScope): INyxPascalStorageRecord;
  end;
  { One bounded capture owner. Active interface leases detect alternate-provider
    cycles and retire on every exit. No mutable provider enters a frozen shape. }
  TShapeCapture = class
  public
    Options: TNyxPascalStorageOptions;
    Cancellation: INyxCancellationScope;
    Active: array of INyxPascalStorageShape;
    Nodes: Integer;
    function Freeze(const ASource: INyxPascalStorageShape): INyxPascalStorageShape;
  end;
  { Construction-only emitter. Generated names, forms and scalar spellings are
    closed; no caller text can inject a directive or executable declaration.
    Publication retires its cancellation/parts references. }
  TStoragePlan = class(TInterfacedObject, INyxPascalStoragePlan)
  private
    FShape: INyxPascalStorageShape;
    FKey, FName, FSource: TNyxText;
    FNodes, FBytes: Integer;
    FOptions: TNyxPascalStorageOptions;
    FParts: TNyxStrings;
    FCancellation: INyxCancellationScope;
    procedure Add(const AText: TNyxText);
    function Emit(const AShape: INyxPascalStorageShape): TNyxText;
  public
    constructor Create(const AShape: INyxPascalStorageShape;
      const AOptions: TNyxPascalStorageOptions; const ACancellation: INyxCancellationScope);
    function Shape: INyxPascalStorageShape;
    function Key: TNyxText;
    function TypeName: TNyxText;
    function Declarations: TNyxText;
    function NodeCount: Integer;
  end;

procedure Check(const ACancellation: INyxCancellationScope);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    raise ENyxPascalAnalysisCancelled.Create('Pascal storage planning cancelled');
  end;
end;

class function TNyxPascalStorageOptions.Defaults: TNyxPascalStorageOptions;
begin
  Result.FDefined := True;
  Result.FNodes := 4096;
  Result.FDepth := 64;
  Result.FBytes := 1048576;
end;

procedure TNyxPascalStorageOptions.Validate;
begin

  if not FDefined or (FNodes < 1) or (FNodes > 65536) or
    (FDepth < 1) or (FDepth > 128) or (FBytes < 1) or (FBytes > 4194304) then
  begin
    raise ENyxPascalStorage.Create('Use positive bounded storage options');
  end;
end;

function TNyxPascalStorageOptions.UpToNodes(AValue: Integer): TNyxPascalStorageOptions;
begin
  Result := Self;
  Result.FNodes := AValue;
  Result.Validate;
end;

function TNyxPascalStorageOptions.UpToDepth(AValue: Integer): TNyxPascalStorageOptions;
begin
  Result := Self;
  Result.FDepth := AValue;
  Result.Validate;
end;

function TNyxPascalStorageOptions.UpToBytes(AValue: Integer): TNyxPascalStorageOptions;
begin
  Result := Self;
  Result.FBytes := AValue;
  Result.Validate;
end;

function NyxPascalStoragePrimitiveName(AValue: TNyxPascalStoragePrimitive): TNyxText;
const
  CNames: array[TNyxPascalStoragePrimitive] of TNyxText = (
    'Byte', 'ShortInt', 'Word', 'SmallInt', 'Longword', 'Longint', 'QWord', 'Int64',
    'Integer', 'Cardinal', 'NativeInt', 'NativeUInt', 'SizeInt', 'SizeUInt', 'Pointer',
    'Boolean', 'ByteBool', 'WordBool', 'LongBool', 'Char', 'WideChar', 'Single',
    'Double', 'Extended', 'Currency', 'Comp', 'AnsiString', 'UnicodeString', 'WideString');
begin

  if (Ord(AValue) < Ord(Low(TNyxPascalStoragePrimitive))) or
    (Ord(AValue) > Ord(High(TNyxPascalStoragePrimitive))) then
  begin
    raise ENyxPascalStorage.Create('Unknown storage primitive');
  end;
  Result := CNames[AValue];
end;

procedure CheckForm(AForm: TNyxPascalStorageForm);
begin

  if (Ord(AForm) < Ord(Low(TNyxPascalStorageForm))) or
    (Ord(AForm) > Ord(High(TNyxPascalStorageForm))) then
  begin
    raise ENyxPascalStorage.Create('Unknown storage container form');
  end;
end;

function AlignmentName(AAlignment: TNyxPascalStorageAlignment): TNyxText;
const
  CNames: array[TNyxPascalStorageAlignment] of TNyxText =
    ('default', 'c', '1', '2', '4', '8', '16', '32');
begin

  if (Ord(AAlignment) < Ord(Low(TNyxPascalStorageAlignment))) or
    (Ord(AAlignment) > Ord(High(TNyxPascalStorageAlignment))) then
  begin
    raise ENyxPascalStorage.Create('Unknown storage record alignment');
  end;
  Result := CNames[AAlignment];
end;

{ Validate at the integer boundary before assigning an alternate provider's
  returned enum. This also keeps future, not-yet-supported kinds explicit. }
function CheckedKind(AOrdinal: Integer): TNyxPascalStorageKind;
begin
  case AOrdinal of
    Ord(pskPrimitive): Result := pskPrimitive;
    Ord(pskArray): Result := pskArray;
    Ord(pskRecord): Result := pskRecord;
  else
    raise ENyxPascalStorage.Create('Unknown alternate storage kind');
  end;
end;

function Owned(const ASource: INyxPascalStorageShape): TStorageShape;
var
  LOwner: IShapeOwner;
begin

  if not Supports(ASource, IShapeOwner, LOwner) then
  begin
    raise ENyxPascalStorage.Create('Storage operation needs its frozen shape');
  end;
  Result := LOwner.StorageShape;
end;

procedure Finish(AShape: TStorageShape; const AOptions: TNyxPascalStorageOptions;
  const ACancellation: INyxCancellationScope);
var
  LAt: Integer;
  LChild: TStorageShape;
  LParts: TNyxStrings;
begin
  AOptions.Validate;
  Check(ACancellation);
  AShape.Nodes := 1;
  AShape.Depth := 1;
  LParts := TNyxStrings.Create;
  try
    LParts.Add(IntToStr(Ord(AShape.ShapeKind)) + ':' +
      IntToStr(Ord(AShape.Scalar)) + ':' + IntToStr(Ord(AShape.ContainerForm)) + ':' +
      IntToStr(Ord(AShape.RecordAlignment)) + ':' + IntToStr(AShape.LengthValue) + '[');
    for LAt := 0 to High(AShape.Children) do
    begin
      Check(ACancellation);
      NyxPascalAdmitAnalysis(ACancellation, papTypeProof);
      LChild := Owned(AShape.Children[LAt]);

      if LChild.Nodes > AOptions.MaximumNodes - AShape.Nodes then
      begin
        raise ENyxPascalStorageLimit.Create('Storage shape exceeds its node budget');
      end;
      Inc(AShape.Nodes, LChild.Nodes);

      if LChild.Depth >= AShape.Depth then
      begin
        AShape.Depth := LChild.Depth + 1;
      end;
      LParts.Add(LChild.Signature);
    end;
    LParts.Add(']');
    AShape.Signature := LParts.Join;

    if (AShape.Depth > AOptions.MaximumDepth) or
      (Length(AShape.Signature) > AOptions.MaximumBytes) then
    begin
      raise ENyxPascalStorageLimit.Create('Storage shape exceeds its depth/key budget');
    end;
  finally
    LParts.Free;
  end;
end;

function TStorageShape.StorageShape: TStorageShape;
begin Result := Self; end;
function TStorageShape.Kind: TNyxPascalStorageKind;
begin Result := ShapeKind; end;
function TStorageShape.Count: Integer;
begin Result := Length(Children); end;

function TStorageShape.Primitive: TNyxPascalStoragePrimitive;
begin

  if ShapeKind <> pskPrimitive then
  begin
    raise ENyxPascalStorage.Create('Only a scalar shape has a primitive');
  end;
  Result := Scalar;
end;

function TStorageShape.Form: TNyxPascalStorageForm;
begin

  if ShapeKind = pskPrimitive then
  begin
    raise ENyxPascalStorage.Create('A scalar has no container form');
  end;
  Result := ContainerForm;
end;

function TStorageShape.Alignment: TNyxPascalStorageAlignment;
begin

  if ShapeKind <> pskRecord then
  begin
    raise ENyxPascalStorage.Create('Only a record has PACKRECORDS intent');
  end;
  Result := RecordAlignment;
end;

function TStorageShape.ArrayLength: Integer;
begin

  if ShapeKind <> pskArray then
  begin
    raise ENyxPascalStorage.Create('Only an array has an element count');
  end;
  Result := LengthValue;
end;

function TStorageShape.Child(AIndex: Integer): INyxPascalStorageShape;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalStorage.Create('Storage child is outside its immutable shape');
  end;
  Result := Children[AIndex];
end;

function NewNyxPascalStoragePrimitive(AValue: TNyxPascalStoragePrimitive;
  const ACancellation: INyxCancellationScope): INyxPascalStorageShape;
var
  LShape: TStorageShape;
begin
  NyxPascalStoragePrimitiveName(AValue);
  Check(ACancellation);
  NyxPascalAdmitAnalysis(ACancellation, papTypeNode);
  LShape := TStorageShape.Create;
  Result := LShape;
  LShape.ShapeKind := pskPrimitive;
  LShape.Scalar := AValue;
  Finish(LShape, TNyxPascalStorageOptions.Defaults, ACancellation);
end;

function NewNyxPascalStorageRecord(AForm: TNyxPascalStorageForm;
  AAlignment: TNyxPascalStorageAlignment;
  const ACancellation: INyxCancellationScope): INyxPascalStorageRecord;
var
  LShape: TStorageRecord;
begin
  CheckForm(AForm);
  AlignmentName(AAlignment);
  Check(ACancellation);
  NyxPascalAdmitAnalysis(ACancellation, papTypeNode);
  LShape := TStorageRecord.Create;
  Result := LShape;
  LShape.ShapeKind := pskRecord;
  LShape.ContainerForm := AForm;
  LShape.RecordAlignment := AAlignment;
  Finish(LShape, TNyxPascalStorageOptions.Defaults, ACancellation);
end;

function TStorageRecord.Field(const AType: INyxPascalStorageShape;
  const AOptions: TNyxPascalStorageOptions;
  const ACancellation: INyxCancellationScope): INyxPascalStorageRecord;
var
  LShape: TStorageRecord;
  LChild: INyxPascalStorageShape;
  LAt: Integer;
begin

  if ShapeKind <> pskRecord then
  begin
    raise ENyxPascalStorage.Create('Only a record accepts another field');
  end;
  LChild := CaptureNyxPascalStorageShape(AType, AOptions, ACancellation);

  if Count >= AOptions.MaximumNodes - 1 then
  begin
    raise ENyxPascalStorageLimit.Create('Record fields exceed the storage node budget');
  end;
  NyxPascalAdmitAnalysis(ACancellation, papTypeNode);
  LShape := TStorageRecord.Create;
  Result := LShape;
  LShape.ShapeKind := pskRecord;
  LShape.ContainerForm := ContainerForm;
  LShape.RecordAlignment := RecordAlignment;
  SetLength(LShape.Children, Count + 1);
  for LAt := 0 to Count - 1 do
  begin
    Check(ACancellation);
    NyxPascalAdmitAnalysis(ACancellation, papTypeNode);
    LShape.Children[LAt] := Children[LAt];
  end;
  NyxPascalAdmitAnalysis(ACancellation, papTypeNode);
  LShape.Children[Count] := LChild;
  Finish(LShape, AOptions, ACancellation);
end;

function NewNyxPascalStorageArray(const AElement: INyxPascalStorageShape;
  ALength: Integer; AForm: TNyxPascalStorageForm;
  const AOptions: TNyxPascalStorageOptions;
  const ACancellation: INyxCancellationScope): INyxPascalStorageShape;
var
  LShape: TStorageShape;
  LChild: INyxPascalStorageShape;
begin
  CheckForm(AForm);

  if ALength < 1 then
  begin
    raise ENyxPascalStorage.Create('A storage array needs a positive length');
  end;
  LChild := CaptureNyxPascalStorageShape(AElement, AOptions, ACancellation);
  NyxPascalAdmitAnalysis(ACancellation, papTypeNode);
  LShape := TStorageShape.Create;
  Result := LShape;
  LShape.ShapeKind := pskArray;
  LShape.ContainerForm := AForm;
  LShape.LengthValue := ALength;
  SetLength(LShape.Children, 1);
  NyxPascalAdmitAnalysis(ACancellation, papTypeNode);
  LShape.Children[0] := LChild;
  Finish(LShape, AOptions, ACancellation);
end;

function TShapeCapture.Freeze(const ASource: INyxPascalStorageShape): INyxPascalStorageShape;
var
  LOwner: IShapeOwner;
  LShape: TStorageShape;
  LChild: INyxPascalStorageShape;
  LKind: TNyxPascalStorageKind;
  LAt, LCount, LDepth: Integer;
begin
  Check(Cancellation);

  if ASource = nil then
  begin
    raise ENyxPascalStorage.Create('A storage shape is required');
  end;
  NyxPascalAdmitAnalysis(Cancellation, papTypeProof);

  if Supports(ASource, IShapeOwner, LOwner) then
  begin
    LShape := LOwner.StorageShape;

    if (LShape.Nodes > Options.MaximumNodes - Nodes) or
      (LShape.Depth > Options.MaximumDepth - Length(Active)) or
      (Length(LShape.Signature) > Options.MaximumBytes) then
    begin
      raise ENyxPascalStorageLimit.Create('Owned storage shape exceeds capture budgets');
    end;
    Inc(Nodes, LShape.Nodes);
    Exit(ASource);
  end;

  if (Nodes >= Options.MaximumNodes) or (Length(Active) >= Options.MaximumDepth) then
  begin
    raise ENyxPascalStorageLimit.Create('Alternate storage shape exceeds capture budgets');
  end;
  for LAt := 0 to High(Active) do
  begin

    if Active[LAt] = ASource then
    begin
      raise ENyxPascalStorage.Create('A storage provider contains a cycle');
    end;
  end;
  Inc(Nodes);
  LDepth := Length(Active);
  SetLength(Active, LDepth + 1);
  Active[LDepth] := ASource;
  try
    NyxPascalAdmitAnalysis(Cancellation, papTypeNode);
    LKind := CheckedKind(Ord(ASource.Kind));

    if LKind = pskRecord then
    begin
      LShape := TStorageRecord.Create;
    end
    else
    begin
      LShape := TStorageShape.Create;
    end;
    Result := LShape;
    LShape.ShapeKind := LKind;
    LCount := ASource.Count;

    if (LCount < 0) or (LCount > Options.MaximumNodes - Nodes) then
    begin
      raise ENyxPascalStorageLimit.Create('Alternate storage child count exceeds capture budgets');
    end;
    case LShape.ShapeKind of
      pskPrimitive:
        begin

          if LCount <> 0 then
          begin
            raise ENyxPascalStorage.Create('A primitive cannot contain children');
          end;
          LShape.Scalar := ASource.Primitive;
          NyxPascalStoragePrimitiveName(LShape.Scalar);
        end;
      pskArray, pskRecord:
        begin
          LShape.ContainerForm := ASource.Form;
          CheckForm(LShape.ContainerForm);

          if LShape.ShapeKind = pskArray then
          begin
            LShape.LengthValue := ASource.ArrayLength;

            if (LCount <> 1) or (LShape.LengthValue < 1) then
            begin
              raise ENyxPascalStorage.Create('An array needs one child and a positive length');
            end;
          end
          else
          begin
            LShape.RecordAlignment := ASource.Alignment;
            AlignmentName(LShape.RecordAlignment);
          end;
        end;
    end;
    SetLength(LShape.Children, LCount);
    for LAt := 0 to LCount - 1 do
    begin
      LChild := Freeze(ASource.Child(LAt));
      NyxPascalAdmitAnalysis(Cancellation, papTypeNode);
      LShape.Children[LAt] := LChild;
    end;
    Finish(LShape, Options, Cancellation);
  finally
    SetLength(Active, LDepth);
  end;
end;

function CaptureNyxPascalStorageShape(const ASource: INyxPascalStorageShape;
  const AOptions: TNyxPascalStorageOptions;
  const ACancellation: INyxCancellationScope): INyxPascalStorageShape;
var
  LCapture: TShapeCapture;
begin
  AOptions.Validate;
  LCapture := TShapeCapture.Create;
  try
    LCapture.Options := AOptions;
    LCapture.Cancellation := ACancellation;
    Result := LCapture.Freeze(ASource);
  finally
    LCapture.Free;
  end;
end;

procedure TStoragePlan.Add(const AText: TNyxText);
begin
  Check(FCancellation);

  if Length(AText) > FOptions.MaximumBytes - FBytes then
  begin
    raise ENyxPascalStorageLimit.Create('Storage declarations exceed their byte budget');
  end;
  Inc(FBytes, Length(AText));
  FParts.Add(AText);
end;

function TStoragePlan.Emit(const AShape: INyxPascalStorageShape): TNyxText;
const
  CForms: array[TNyxPascalStorageForm] of TNyxText = ('', 'packed ', 'bitpacked ');
var
  LAt, LOrdinal: Integer;
  LChildName: TNyxText;
  LChildren: TNyxStrings;
begin
  Check(FCancellation);
  NyxPascalAdmitAnalysis(FCancellation, papTypeToken);
  Inc(FNodes);
  LOrdinal := FNodes;
  Result := 'TNyxStorage' + TNyxText(IntToStr(LOrdinal));
  LChildren := TNyxStrings.Create;
  try
    for LAt := 0 to AShape.Count - 1 do
    begin
      LChildName := Emit(AShape.Child(LAt));
      LChildren.Add(LChildName);
    end;
    case AShape.Kind of
      pskPrimitive:
        Add('  ' + Result + ' = ' + NyxPascalStoragePrimitiveName(AShape.Primitive) + ';'#10);
      pskArray:
        Add('  ' + Result + ' = ' + CForms[AShape.Form] + 'array[0..' +
          TNyxText(IntToStr(AShape.ArrayLength - 1)) + '] of ' + LChildren[0] + ';'#10);
      pskRecord:
        begin
          Add('{$push}{$packrecords ' + AlignmentName(AShape.Alignment) + '}'#10);
          Add('  ' + Result + ' = ' + CForms[AShape.Form] + 'record'#10);
          for LAt := 0 to AShape.Count - 1 do
          begin
            NyxPascalAdmitAnalysis(FCancellation, papTypeToken);
            Add('    NyxField' + TNyxText(IntToStr(LAt + 1)) + ': ' + LChildren[LAt] + ';'#10);
          end;
          Add('  end;'#10'{$pop}'#10);
        end;
    end;
  finally
    LChildren.Free;
  end;
end;

constructor TStoragePlan.Create(const AShape: INyxPascalStorageShape;
  const AOptions: TNyxPascalStorageOptions; const ACancellation: INyxCancellationScope);
begin
  inherited Create;
  FOptions := AOptions;
  FShape := CaptureNyxPascalStorageShape(AShape, AOptions, ACancellation);
  FKey := Owned(FShape).Signature;
  FCancellation := ACancellation;
  FParts := TNyxStrings.Create;
  try
    FName := Emit(FShape);
    FSource := FParts.Join;
  finally
    FParts.Free;
    FParts := nil;
    FCancellation := nil;
  end;
end;

function TStoragePlan.Shape: INyxPascalStorageShape;
begin Result := FShape; end;
function TStoragePlan.Key: TNyxText;
begin Result := FKey; end;
function TStoragePlan.TypeName: TNyxText;
begin Result := FName; end;
function TStoragePlan.Declarations: TNyxText;
begin Result := FSource; end;
function TStoragePlan.NodeCount: Integer;
begin Result := FNodes; end;

function PlanNyxPascalStorage(const AShape: INyxPascalStorageShape;
  const AOptions: TNyxPascalStorageOptions;
  const ACancellation: INyxCancellationScope): INyxPascalStoragePlan;
begin
  Result := TStoragePlan.Create(AShape, AOptions, ACancellation);
end;

end.
