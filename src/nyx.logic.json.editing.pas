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
unit nyx.logic.json.editing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.logic, nyx.flow.types, nyx.scheduler;

type
  { Object Upsert adds/replaces, Replace requires an existing member and Insert
    requires an absent member. For arrays, Insert shifts at 0..Count; Upsert
    replaces an existing item or appends at Count; Replace requires 0..Count-1.
    No mode creates missing parents or sparse arrays. An empty path addresses
    the existing root: Upsert/Replace replace it, while Insert refuses. }
  TNyxJSONSetMode = (njsUpsert, njsReplace, njsInsert);

  { Missing object members/array items (including missing ancestors) either
    refuse or leave the input unchanged. A wrong container family always refuses.
    Removing the root always refuses: a missing value is not JSON null. }
  TNyxJSONMissing = (njmRequire, njmIgnore);

  { Recursive object overlay preserves explicit null values. MergePatch follows
    RFC 7396: object null members delete; a non-object patch replaces the target.
    Both replace arrays as complete values. Existing key order is retained;
    newly added keys follow right-hand order. Exact decimals are never rounded. }
  TNyxJSONMergePolicy = (njmpOverlay, njmpMergePatch);

  { Managed immutable path update. Direct and graph calls build an independent
    complete result or raise without changing either input. Graph inputs are
    nlpJSON (source) and nlpRight (replacement); nlpValue is the result.
    The operation retains only copied path/mode, never a document or live store. }
  INyxJSONSet = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000071}']
    function SetValue(const ASource, AValue: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetMode: TNyxJSONSetMode;
    property Path: TNyxDataPath read GetPath;
    property Mode: TNyxJSONSetMode read GetMode;
  end;

  { Managed structural removal with detached path/missing policy. Array removal
    closes the gap. Graph input nlpJSON produces the complete nlpValue result;
    cancellation or any failed path/budget check publishes no partial value. }
  INyxJSONRemove = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000072}']
    function Remove(const ASource: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetMissing: TNyxJSONMissing;
    property Path: TNyxDataPath read GetPath;
    property Missing: TNyxJSONMissing read GetMissing;
  end;

  { Managed recursive merge of two owned JSON snapshots. Graph inputs nlpLeft
    and nlpRight produce nlpValue. Policy is explicit, immutable and data-only;
    no network request or external authority is implied by MergePatch. }
  INyxJSONMerge = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000073}']
    function Merge(const ALeft, ARight: TNyxDataValue): TNyxDataValue;
    function GetPolicy: TNyxJSONMergePolicy;
    property Policy: TNyxJSONMergePolicy read GetPolicy;
  end;

{ Paths use the shared at-most-32-step Field/Item contract. Construction checks
  configuration; invocation validates all inputs and the complete result against
  Nyx's JSON byte/depth/member budgets. All direct calls have the same semantics
  as graph execution; graph loops additionally cooperate with cancellation. }
function NewNyxJSONSet(const APath: TNyxDataPath;
  AMode: TNyxJSONSetMode = njsUpsert): INyxJSONSet;
function NewNyxJSONRemove(const APath: TNyxDataPath;
  AMissing: TNyxJSONMissing = njmRequire): INyxJSONRemove;
function NewNyxJSONMerge(APolicy: TNyxJSONMergePolicy = njmpOverlay): INyxJSONMerge;
{ Closed persistence factory: unknown kinds return nil; malformed known kinds
  refuse. Import reconstructs pure typed behavior without acquiring authority. }
function NyxJSONEditingFromData(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;

implementation

uses nyx.json;

type
  { Immutable set implementation. Private recursion owns rebuilt ancestors;
    no caller container, array, scheduler or cancellation scope is retained. }
  TJSONSet = class(TNyxLogicOperation, INyxJSONSet)
  private
    FPath: TNyxDataPath;
    FMode: TNyxJSONSetMode;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const APath: TNyxDataPath; AMode: TNyxJSONSetMode);
    function SetValue(const ASource, AValue: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetMode: TNyxJSONSetMode;
  end;

  { Immutable removal implementation. Missing policy does not suppress shape,
    invalid-input, cancellation or budget failures. Results own their snapshots. }
  TJSONRemove = class(TNyxLogicOperation, INyxJSONRemove)
  private
    FPath: TNyxDataPath;
    FMissing: TNyxJSONMissing;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const APath: TNyxDataPath; AMissing: TNyxJSONMissing);
    function Remove(const ASource: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    function GetMissing: TNyxJSONMissing;
  end;

  { Immutable merge implementation. Recursive candidates are invocation-local;
    unchanged children are copied by value, with no mutable JSON tree sharing. }
  TJSONMerge = class(TNyxLogicOperation, INyxJSONMerge)
  private
    FPolicy: TNyxJSONMergePolicy;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(APolicy: TNyxJSONMergePolicy);
    function Merge(const ALeft, ARight: TNyxDataValue): TNyxDataValue;
    function GetPolicy: TNyxJSONMergePolicy;
  end;

procedure CheckCancellation(const AScope: INyxCancellationScope);
begin

  if (AScope <> nil) and AScope.Cancelled then
  begin
    raise ENyxFlow.Create('JSON editing was cancelled');
  end;
end;

{ Exact decoded keys include empty/NUL/supplementary names. Never use an ANSI
  RTL name/value list or dotted expression to resolve a structural field. }
function FieldIndex(const AObject: TNyxDataValue; const AName: TNyxText;
  const AScope: INyxCancellationScope): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to AObject.Count - 1 do
  begin
    CheckCancellation(AScope);

    if AObject.Key(LIndex) = AName then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

{ One path walk handles set/removal without inventing containers. It rebuilds
  only ancestors and admits each complete container. Array capacity is bounded
  by the already admitted input plus one; output budgets still apply at root. }
function Edit(const ASource, ASteps, AValue: TNyxDataValue; ADepth: Integer;
  ARemove: Boolean; AMode: TNyxJSONSetMode; AMissing: TNyxJSONMissing;
  const AScope: INyxCancellationScope): TNyxDataValue;
var
  LStep: TNyxDataValue;
  LChild: TNyxDataValue;
  LFields: array of TNyxDataField;
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LTarget: Integer;
  LCount: Integer;
  LOut: Integer;
  LLeaf: Boolean;
  LInsert: Boolean;
  LDelete: Boolean;
begin
  CheckCancellation(AScope);

  if ADepth = ASteps.Count then
  begin

    if ARemove or (AMode = njsInsert) then
    begin
      raise ENyxFlow.Create('JSON root exists; replace it explicitly instead of inserting/removing it');
    end;
    Exit(AValue.Copy);
  end;
  LStep := ASteps.Item(ADepth);
  LLeaf := ADepth = ASteps.Count - 1;

  if ((LStep.Kind = ndText) and (ASource.Kind <> ndObject)) or
    ((LStep.Kind = ndNumber) and (ASource.Kind <> ndArray)) then
  begin
    raise ENyxFlow.Create('JSON edit path has the wrong container family');
  end;
  LCount := ASource.Count;
  LTarget := -1;

  if LStep.Kind = ndText then
  begin
    LTarget := FieldIndex(ASource, LStep.AsText, AScope);
  end
  else if LStep.AsInteger < LCount then
  begin
    LTarget := LStep.AsInteger;
  end;

  if LTarget < 0 then
  begin

    if ARemove and (AMissing = njmIgnore) then
    begin
      Exit(ASource.Copy);
    end;

    if not LLeaf or ARemove or (AMode = njsReplace) or
      ((LStep.Kind = ndNumber) and (LStep.AsInteger <> LCount)) then
    begin
      raise ENyxFlow.Create('JSON edit path requires an existing parent/item');
    end;
    LTarget := LCount;
  end
  else if LLeaf and not ARemove and (AMode = njsInsert) and
    (LStep.Kind = ndText) then
  begin
    raise ENyxFlow.Create('JSON insert requires an absent object member');
  end;
  LInsert := LLeaf and not ARemove and ((LTarget = LCount) or
    ((LStep.Kind = ndNumber) and (AMode = njsInsert)));
  LDelete := LLeaf and ARemove;

  if not LLeaf then
  begin

    if LStep.Kind = ndText then
    begin
      LChild := ASource.Field(LStep.AsText);
    end
    else
    begin
      LChild := ASource.Item(LTarget);
    end;
    LChild := Edit(LChild, ASteps, AValue, ADepth + 1, ARemove, AMode, AMissing, AScope);
  end
  else if not ARemove then
  begin
    LChild := AValue.Copy;
  end;
  LOut := LCount + Ord(LInsert) - Ord(LDelete);

  if ASource.Kind = ndObject then
  begin
    SetLength(LFields, LOut);
    LOut := 0;
    for LIndex := 0 to LCount - 1 do
    begin
      CheckCancellation(AScope);

      if (LIndex = LTarget) and LDelete then
      begin
        Continue;
      end;

      if LIndex = LTarget then
      begin
        LFields[LOut] := NyxField(ASource.Key(LIndex), LChild);
      end
      else
      begin
        LFields[LOut] := NyxField(ASource.Key(LIndex), ASource.Field(ASource.Key(LIndex)));
      end;
      Inc(LOut);
    end;

    if LInsert then
    begin
      LFields[LOut] := NyxField(LStep.AsText, LChild);
    end;
    Result := NyxObject(LFields);
  end
  else
  begin
    SetLength(LItems, LOut);
    LOut := 0;
    for LIndex := 0 to LCount - 1 do
    begin
      CheckCancellation(AScope);

      if (LIndex = LTarget) and LDelete then
      begin
        Continue;
      end;

      if LIndex = LTarget then
      begin
        LItems[LOut] := LChild.Copy;
        Inc(LOut);

        if not LInsert then
        begin
          Continue;
        end;
      end;
      LItems[LOut] := ASource.Item(LIndex);
      Inc(LOut);
    end;

    if LTarget = LCount then
    begin
      LItems[LOut] := LChild.Copy;
    end;
    Result := NyxArray(LItems);
  end;
  CheckCancellation(AScope);
end;

{ Build one candidate in original-left order then append unseen right keys.
  MergePatch's absent/scalar object target starts empty. Explicit recursive
  depth checking precedes further descent; ordinary admission checks the final
  combined byte/depth/member budget, not merely each input in isolation. }
function MergeValue(const ALeft, ARight: TNyxDataValue; APolicy: TNyxJSONMergePolicy;
  ADepth: Integer; const AScope: INyxCancellationScope): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LBase: TNyxDataValue;
  LRight: TNyxDataValue;
  LValue: TNyxDataValue;
  LName: TNyxText;
  LIndex: Integer;
  LOut: Integer;
  LFound: Integer;
begin
  CheckCancellation(AScope);

  if ADepth > NyxMaximumJSONDepth then
  begin
    raise ENyxFlow.Create('JSON merge exceeds the shared depth budget');
  end;

  if ARight.Kind <> ndObject then
  begin
    Exit(ARight.Copy);
  end;
  LBase := ALeft;

  if LBase.Kind <> ndObject then
  begin
    LBase := NyxObject([]);
  end;
  SetLength(LFields, LBase.Count + ARight.Count);
  LOut := 0;
  for LIndex := 0 to LBase.Count - 1 do
  begin
    CheckCancellation(AScope);
    LName := LBase.Key(LIndex);
    LFound := FieldIndex(ARight, LName, AScope);
    LValue := LBase.Field(LName);

    if LFound >= 0 then
    begin
      LRight := ARight.Field(LName);

      if (APolicy = njmpMergePatch) and (LRight.Kind = ndNull) then
      begin
        Continue;
      end;
      LValue := MergeValue(LValue, LRight, APolicy, ADepth + 1, AScope);
    end;
    LFields[LOut] := NyxField(LName, LValue);
    Inc(LOut);
  end;
  for LIndex := 0 to ARight.Count - 1 do
  begin
    CheckCancellation(AScope);
    LName := ARight.Key(LIndex);

    if FieldIndex(LBase, LName, AScope) >= 0 then
    begin
      Continue;
    end;
    LRight := ARight.Field(LName);

    if (APolicy = njmpMergePatch) and (LRight.Kind = ndNull) then
    begin
      Continue;
    end;
    LFields[LOut] := NyxField(LName, MergeValue(NyxNull, LRight, APolicy, ADepth + 1, AScope));
    Inc(LOut);
  end;
  SetLength(LFields, LOut);
  Result := NyxObject(LFields);
  CheckCancellation(AScope);
end;

constructor TJSONSet.Create(const APath: TNyxDataPath; AMode: TNyxJSONSetMode);
begin
  inherited Create;

  if (Ord(AMode) < Ord(Low(TNyxJSONSetMode))) or
    (Ord(AMode) > Ord(High(TNyxJSONSetMode))) then
  begin
    raise ENyxFlow.Create('Unknown JSON set mode');
  end;
  FPath := TNyxDataPath.FromData(APath.ToData);
  FMode := AMode;
  FConfiguration := NyxObject([NyxField('path', FPath.ToData), NyxField('mode', NyxData(Ord(FMode)))]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('json-set'), 'Set JSON value',
    'Insert, replace or upsert a typed Field/Item path without creating missing parents.', nlgJSON)
    .Input(NyxLogicPort(nlpJSON), nftJSON, 'Original JSON')
    .Input(NyxLogicPort(nlpRight), nftJSON, 'Replacement JSON value')
    .Output(NyxLogicPort(nlpValue), nftJSON, 'Complete updated JSON')
    .Labelled('update').Labelled('insert').Labelled('replace').Labelled('construct');
end;

function TJSONSet.GetPath: TNyxDataPath;
begin
  Result := FPath.Copy;
end;

function TJSONSet.GetMode: TNyxJSONSetMode;
begin
  Result := FMode;
end;

function TJSONSet.SetValue(const ASource, AValue: TNyxDataValue): TNyxDataValue;
begin
  ASource.Validate;
  AValue.Validate;
  Result := Edit(ASource, FPath.ToData, AValue, 0, False, FMode, njmRequire, nil);
end;

function TJSONSet.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpValue), NyxFlowJSON(Edit(
    AInputs.Value(NyxLogicPort(nlpJSON)).AsJSON, FPath.ToData,
    AInputs.Value(NyxLogicPort(nlpRight)).AsJSON, 0, False, FMode, njmRequire, ACancellation)));
end;

constructor TJSONRemove.Create(const APath: TNyxDataPath; AMissing: TNyxJSONMissing);
begin
  inherited Create;

  if (Ord(AMissing) < Ord(Low(TNyxJSONMissing))) or
    (Ord(AMissing) > Ord(High(TNyxJSONMissing))) then
  begin
    raise ENyxFlow.Create('Unknown JSON missing-value policy');
  end;
  FPath := TNyxDataPath.FromData(APath.ToData);
  FMissing := AMissing;
  FConfiguration := NyxObject([NyxField('path', FPath.ToData), NyxField('missing', NyxData(Ord(FMissing)))]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('json-remove'), 'Remove JSON value',
    'Remove a typed object member or array item; choose whether a missing path refuses.', nlgJSON)
    .Input(NyxLogicPort(nlpJSON), nftJSON, 'Original JSON')
    .Output(NyxLogicPort(nlpValue), nftJSON, 'Complete JSON after removal')
    .Labelled('delete').Labelled('remove').Labelled('transform');
end;

function TJSONRemove.GetPath: TNyxDataPath;
begin
  Result := FPath.Copy;
end;

function TJSONRemove.GetMissing: TNyxJSONMissing;
begin
  Result := FMissing;
end;

function TJSONRemove.Remove(const ASource: TNyxDataValue): TNyxDataValue;
begin
  ASource.Validate;
  Result := Edit(ASource, FPath.ToData, NyxNull, 0, True, njsUpsert, FMissing, nil);
end;

function TJSONRemove.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpValue), NyxFlowJSON(Edit(
    AInputs.Value(NyxLogicPort(nlpJSON)).AsJSON, FPath.ToData, NyxNull,
    0, True, njsUpsert, FMissing, ACancellation)));
end;

constructor TJSONMerge.Create(APolicy: TNyxJSONMergePolicy);
begin
  inherited Create;

  if (Ord(APolicy) < Ord(Low(TNyxJSONMergePolicy))) or
    (Ord(APolicy) > Ord(High(TNyxJSONMergePolicy))) then
  begin
    raise ENyxFlow.Create('Unknown JSON merge policy');
  end;
  FPolicy := APolicy;
  FConfiguration := NyxObject([NyxField('policy', NyxData(Ord(FPolicy)))]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('json-merge'), 'Merge JSON values',
    'Recursively overlay objects or apply JSON Merge Patch; arrays replace as complete values.', nlgJSON)
    .Input(NyxLogicPort(nlpLeft), nftJSON, 'Original JSON')
    .Input(NyxLogicPort(nlpRight), nftJSON, 'Overlay or merge patch')
    .Output(NyxLogicPort(nlpValue), nftJSON, 'Complete merged JSON')
    .Labelled('combine').Labelled('patch').Labelled('overlay').Labelled('construct');
end;

function TJSONMerge.GetPolicy: TNyxJSONMergePolicy;
begin
  Result := FPolicy;
end;

function TJSONMerge.Merge(const ALeft, ARight: TNyxDataValue): TNyxDataValue;
begin
  ALeft.Validate;
  ARight.Validate;
  Result := MergeValue(ALeft, ARight, FPolicy, 0, nil);
end;

function TJSONMerge.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpValue), NyxFlowJSON(MergeValue(
    AInputs.Value(NyxLogicPort(nlpLeft)).AsJSON, AInputs.Value(NyxLogicPort(nlpRight)).AsJSON,
    FPolicy, 0, ACancellation)));
end;

function NewNyxJSONSet(const APath: TNyxDataPath; AMode: TNyxJSONSetMode): INyxJSONSet;
begin
  Result := TJSONSet.Create(APath, AMode);
end;

function NewNyxJSONRemove(const APath: TNyxDataPath; AMissing: TNyxJSONMissing): INyxJSONRemove;
begin
  Result := TJSONRemove.Create(APath, AMissing);
end;

function NewNyxJSONMerge(APolicy: TNyxJSONMergePolicy): INyxJSONMerge;
begin
  Result := TJSONMerge.Create(APolicy);
end;

function NyxJSONEditingFromData(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
var
  LChoice: Integer;
begin
  Result := nil;

  if AKind.Name = 'json-set' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['path', 'mode']);
    LChoice := AConfiguration.Field('mode').AsInteger;

    if (LChoice < Ord(Low(TNyxJSONSetMode))) or (LChoice > Ord(High(TNyxJSONSetMode))) then
    begin
      raise ENyxFlow.Create('Unknown JSON set mode');
    end;
    Result := NewNyxJSONSet(TNyxDataPath.FromData(AConfiguration.Field('path')), TNyxJSONSetMode(LChoice));
  end
  else if AKind.Name = 'json-remove' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['path', 'missing']);
    LChoice := AConfiguration.Field('missing').AsInteger;

    if (LChoice < Ord(Low(TNyxJSONMissing))) or (LChoice > Ord(High(TNyxJSONMissing))) then
    begin
      raise ENyxFlow.Create('Unknown JSON missing-value policy');
    end;
    Result := NewNyxJSONRemove(TNyxDataPath.FromData(AConfiguration.Field('path')), TNyxJSONMissing(LChoice));
  end
  else if AKind.Name = 'json-merge' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['policy']);
    LChoice := AConfiguration.Field('policy').AsInteger;

    if (LChoice < Ord(Low(TNyxJSONMergePolicy))) or (LChoice > Ord(High(TNyxJSONMergePolicy))) then
    begin
      raise ENyxFlow.Create('Unknown JSON merge policy');
    end;
    Result := NewNyxJSONMerge(TNyxJSONMergePolicy(LChoice));
  end;
end;

end.
