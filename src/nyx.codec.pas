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

unit nyx.codec;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.data,
  SysUtils,
  fpjson,
  nyx.state,
  nyx.collections.registry,
  nyx.binding.types,
  nyx.collections.view.types,
  nyx.model;

type
  { Version 1/2/3 design persistence. The format deliberately stores the portable
    model, not target widget handles or generated source. All custom kinds and
    string-valued extension properties survive an encode/decode round trip.
    Unknown root/node fields retain typed nested extension data, exact strings
    and admitted numeric spelling; recognized fields cannot be overridden.
    Optional typed state defaults extend version 1 without changing empty-state
    designs. Numbers use an explicitly tagged decimal string to preserve every
    finite Double through older JSON formatters; authoring still uses Double.
    Typed collection defaults select version 2 and a separately versioned
    collection descriptor. Version-1 opaque collections extensions keep their
    original meaning. A conflicting extension/default pair refuses export.
    Typed node view bindings select version 3. Older opaque collectionView fields
    retain extension meaning; conflicting promotion is refused, never guessed.
    Decode returns ownership to its caller and frees partial trees on failure. }
  TNyxCodec = class
  public
    class function Encode(ADocument: TNyxDocument): TNyxText; static;
    class function Decode(const ASource: TNyxText): TNyxDocument; static;
  end;

implementation

uses
  nyx.json,
  nyx.collections.codec;

procedure WriteExtensions(AExtensions: TNyxExtensions; AObject: TJSONObject);
var
  LData: TJSONData;
  LFields: TJSONObject;
  LIndex: Integer;
begin

  if AExtensions.Count = 0 then
  begin
    Exit;
  end;
  LData := DecodeNyxJSON(AExtensions.ToJSON);
  try
    LFields := TJSONObject(LData);
    for LIndex := 0 to LFields.Count - 1 do
    begin
      AObject.Add(LFields.Names[LIndex], LFields.Items[LIndex].Clone);
    end;
  finally
    LData.Free;
  end;
end;

procedure ReadExtensions(AObject: TJSONObject; AExtensions: TNyxExtensions;
  ACollections: Boolean = False; ACollectionViews: Boolean = False);
var
  LFields: TJSONObject;
  LIndex: Integer;
begin
  LFields := TJSONObject.Create;
  try
    for LIndex := 0 to AObject.Count - 1 do
    begin

      if not NyxReservedField(AExtensions.Scope, AObject.Names[LIndex]) and
        (not ACollections or (AObject.Names[LIndex] <> NyxCollectionsWireField)) and
        (not ACollectionViews or (AObject.Names[LIndex] <> NyxCollectionViewWireField)) then
      begin
        LFields.Add(AObject.Names[LIndex], AObject.Items[LIndex].Clone);
      end;
    end;

    if LFields.Count > 0 then
    begin
      AExtensions.LoadJSON(LFields.AsJSON);
    end;
  finally
    LFields.Free;
  end;
end;

function StateJSON(AState: TNyxState): TJSONObject;
var
  LIndex: Integer;
  LEntry: TJSONObject;
  LValue: TNyxStateValue;
begin
  Result := TJSONObject.Create;
  try
    for LIndex := 0 to AState.Count - 1 do
    begin
      LEntry := TJSONObject.Create;
      Result.Add(AState.Key(LIndex), LEntry);
      LValue := AState.Value(AState.Key(LIndex));
      LEntry.Add('type', NyxStateKindName(LValue.Kind));
      case LValue.Kind of
        nskText:
          begin
            LEntry.Add('value', LValue.TextValue);
          end;
        nskBoolean:
          begin
            LEntry.Add('value', LValue.BooleanValue);
          end;
        nskInteger:
          begin
            LEntry.Add('value', LValue.IntegerValue);
          end;
        nskNumber:
          begin
            LEntry.Add('value', LValue.NumberText);
          end;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function NodeJSON(ANode: TNyxNode): TJSONObject;
var
  LProps: TJSONObject;
  LChildren: TJSONArray;
  LBindings: TJSONArray;
  LBinding: TJSONObject;
  LSpec: TNyxBindingSpec;
  LIndex: Integer;
begin
  { Attach JSON containers immediately so the single Result.Free failure path
    releases every completed property and descendant built so far. }
  Result := TJSONObject.Create;
  try
    Result.Add('kind', ANode.Kind);
    Result.Add('id', ANode.ID);
    LProps := TJSONObject.Create;
    Result.Add('props', LProps);
    for LIndex := 0 to ANode.Props.Count - 1 do
    begin
      LProps.Add(ANode.Props.Names[LIndex], ANode.Prop(ANode.Props.Names[LIndex]));
    end;

    if ANode.BindingCount > 0 then
    begin
      LBindings := TJSONArray.Create;
      Result.Add('bindings', LBindings);
      for LIndex := 0 to ANode.BindingCount - 1 do
      begin
        LSpec := ANode.Bindings[LIndex];
        LBinding := TJSONObject.Create;
        LBindings.Add(LBinding);
        LBinding.Add('property', NyxBindingPropertyName(LSpec.Target));

        if LSpec.Cleared then
        begin
          LBinding.Add('clear', True);
        end
        else
        begin
          LBinding.Add('state', LSpec.StateName);
          LBinding.Add('type', NyxStateKindName(LSpec.ValueKind));
          LBinding.Add('direction', NyxBindingDirectionName(LSpec.Direction));
        end;
      end;
    end;
    LChildren := TJSONArray.Create;
    { Version 3 distinguishes a missing inherited binding from an explicit null
      (clear). Legacy opaque data is kept in Extensions by older decoders. }

    if ANode.HasCollectionView then
    begin
      Result.Add(NyxCollectionViewWireField,
        DecodeNyxJSON(ANode.CollectionView.ToData.ToJSON));
    end;
    Result.Add('children', LChildren);
    for LIndex := 0 to ANode.Count - 1 do
    begin
      LChildren.Add(NodeJSON(ANode.Children[LIndex]));
    end;
    WriteExtensions(ANode.Extensions, Result);
  except
    Result.Free;
    raise;
  end;
end;

class function TNyxCodec.Encode(ADocument: TNyxDocument): TNyxText;
var
  LRoot: TJSONObject;
  LPages: TJSONArray;
  LComponents: TJSONArray;
  LIndex: Integer;
  LAdmitted: TJSONData;
begin

  if ADocument = nil then
    raise ENyxModel.Create('Document is required');
  ADocument.Validate;
  LRoot := TJSONObject.Create;
  try

    if ADocument.HasCollectionViews then
    begin
      LRoot.Add('version', 3);
      LRoot.Add(NyxCollectionsWireField,
        DecodeNyxJSON(EncodeNyxCollectionDefaults(ADocument.Collections)));
    end
    else if ADocument.Collections.Count > 0 then
    begin
      LRoot.Add('version', 2);
      LRoot.Add(NyxCollectionsWireField,
        DecodeNyxJSON(EncodeNyxCollectionDefaults(ADocument.Collections)));
    end
    else
    begin
      LRoot.Add('version', 1);
    end;
    LRoot.Add('title', ADocument.Title);

    if ADocument.State.Count > 0 then
    begin
      LRoot.Add('state', StateJSON(ADocument.State));
    end;
    LPages := TJSONArray.Create;
    LRoot.Add('pages', LPages);
    LComponents := TJSONArray.Create;
    LRoot.Add('components', LComponents);
    for LIndex := 0 to ADocument.Count - 1 do
    begin
      LPages.Add(NodeJSON(ADocument.Pages[LIndex]));
    end;
    for LIndex := 0 to ADocument.ComponentCount - 1 do
    begin
      LComponents.Add(NodeJSON(ADocument.Components[LIndex]));
    end;
    WriteExtensions(ADocument.Extensions, LRoot);
    Result := LRoot.AsJSON;
    { The complete exported design must satisfy the same budgets as an import.
      Individually admitted payloads may exceed bytes/depth when combined with
      pages or other fields. This runs before Studio history/publication too. }
    LAdmitted := DecodeNyxJSON(Result);
    LAdmitted.Free;
  finally
    LRoot.Free;
  end;
end;

function RequireField(AObject: TJSONObject; const AName: TNyxText;
  AType: TJSONType): TJSONData;
begin
  Result := AObject.Find(AName);

  if (Result = nil) or (Result.JSONType <> AType) then
    raise ENyxModel.Create('Missing or invalid field: ' + AName);
end;

procedure ReadState(AData: TJSONData; AState: TNyxState);
var
  LObject: TJSONObject;
  LEntry: TJSONObject;
  LIndex: Integer;
  LType: TNyxText;
  LNumber: Double;
  LAssignments: array of TNyxStateAssignment;
  LValue: TNyxStateValue;
begin

  if AData = nil then
  begin
    Exit;
  end;

  if AData.JSONType <> jtObject then
  begin
    raise ENyxModel.Create('State defaults must be an object');
  end;
  LObject := TJSONObject(AData);

  if LObject.Count > NyxMaximumStateEntries then
  begin
    raise ENyxModel.Create('State exceeds entry budget');
  end;
  SetLength(LAssignments, LObject.Count);
  for LIndex := 0 to LObject.Count - 1 do
  begin

    if LObject.Items[LIndex].JSONType <> jtObject then
    begin
      raise ENyxModel.Create('State entry must be a typed object');
    end;
    LEntry := TJSONObject(LObject.Items[LIndex]);

    if LEntry.Count <> 2 then
    begin
      raise ENyxModel.Create('State entry requires exactly type and value');
    end;
    LType := RequireField(LEntry, 'type', jtString).AsString;

    if LType = NyxStateKindName(nskText) then
    begin
      LValue := TNyxStateValue.FromText(RequireField(LEntry, 'value', jtString).AsString);
    end
    else if LType = NyxStateKindName(nskBoolean) then
    begin
      LValue := TNyxStateValue.FromBoolean(RequireField(LEntry, 'value', jtBoolean).AsBoolean);
    end
    else if LType = NyxStateKindName(nskInteger) then
    begin
      { The integer wire tag requires an integer spelling. Reading via Double
        can round 1.0000000000000000001 into 1 and admit a fractional default. }
      try
        LValue := TNyxStateValue.FromInteger(TNyxDataValue.ParseJSON(
          RequireField(LEntry, 'value', jtNumber).AsJSON).AsInteger);
      except
        on LException: ENyxJSON do
        begin
          raise ENyxModel.Create('State integer requires signed 32-bit integer spelling');
        end;
      end;
    end
    else if LType = NyxStateKindName(nskNumber) then
    begin

      if not TryNyxStateNumber(RequireField(LEntry, 'value', jtString).AsString, LNumber) then
      begin
        raise ENyxModel.Create('State number requires a complete finite decimal');
      end;
      LValue := TNyxStateValue.FromNumber(LNumber);
    end
    else
    begin
      raise ENyxModel.Create('Unknown state value type: ' + LType);
    end;
    LAssignments[LIndex] := NyxStateAssign(LObject.Names[LIndex], LValue);
  end;
  { Publish defaults once, after every entry passes admission. Decode itself owns
    a detached document, so any subsequent tree failure also discards this store. }
  AState.Apply(LAssignments);
end;

procedure ReadBindings(AData: TJSONData; ANode: TNyxNode);
var
  LArray: TJSONArray;
  LEntry: TJSONObject;
  LIndex: Integer;
  LKind: TNyxStateKind;
  LFoundKind: Boolean;
  LKindName: TNyxText;
  LProperty: TNyxBindingProperty;
  LDirection: TNyxBindingDirection;
  LSeen: set of TNyxBindingProperty;
begin

  if AData = nil then
  begin
    Exit;
  end;

  if AData.JSONType <> jtArray then
  begin
    raise ENyxModel.Create('Node bindings must be an array');
  end;
  LArray := TJSONArray(AData);

  if LArray.Count > Ord(High(TNyxBindingProperty)) + 1 then
  begin
    raise ENyxModel.Create('Node exceeds binding target budget');
  end;
  LSeen := [];
  for LIndex := 0 to LArray.Count - 1 do
  begin

    if LArray.Items[LIndex].JSONType <> jtObject then
    begin
      raise ENyxModel.Create('Binding must be an object');
    end;
    LEntry := TJSONObject(LArray.Items[LIndex]);

    if not TryNyxBindingProperty(RequireField(LEntry, 'property', jtString).AsString,
      LProperty) then
    begin
      raise ENyxModel.Create('Unknown binding target');
    end;

    if LProperty in LSeen then
    begin
      raise ENyxModel.Create('Duplicate binding target');
    end;
    Include(LSeen, LProperty);

    if LEntry.Find('clear') <> nil then
    begin

      if (LEntry.Count <> 2) or not RequireField(LEntry, 'clear', jtBoolean).AsBoolean then
      begin
        raise ENyxModel.Create('Cleared binding requires only property and clear=true');
      end;
      ANode.SetBinding(TNyxBindingSpec.Clear(LProperty));
      Continue;
    end;

    if LEntry.Count <> 4 then
    begin
      raise ENyxModel.Create('Binding requires property, state, type and direction');
    end;
    LKindName := RequireField(LEntry, 'type', jtString).AsString;
    LFoundKind := False;
    for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
    begin

      if NyxStateKindName(LKind) = LKindName then
      begin
        LFoundKind := True;
        Break;
      end;
    end;

    if not LFoundKind or not TryNyxBindingDirection(
      RequireField(LEntry, 'direction', jtString).AsString, LDirection) then
    begin
      raise ENyxModel.Create('Unknown binding value kind or direction');
    end;
    ANode.SetBinding(TNyxBindingSpec.Bound(LProperty,
      RequireField(LEntry, 'state', jtString).AsString, LKind, LDirection));
  end;
end;

function ReadNode(AData: TJSONData; ADepth: Integer; var ACount: Integer;
  ACollectionViews: Boolean): TNyxNode;
var
  LObject: TJSONObject;
  LProps: TJSONObject;
  LChildren: TJSONArray;
  LIndex: Integer;
  LKind: TNyxText;
  LID: TNyxText;
begin
  { Count spans every page and definition, rather than restarting per root.
    Reject wrong types instead of silently coercing damaged design data. }
  Inc(ACount);

  if (ADepth > 128) or (ACount > 10000) then
    raise ENyxModel.Create('Document exceeds depth or node budget');

  if AData.JSONType <> jtObject then
    raise ENyxModel.Create('Node must be an object');
  LObject := TJSONObject(AData);
  LKind := RequireField(LObject, 'kind', jtString).AsString;
  LID := RequireField(LObject, 'id', jtString).AsString;

  if LID = '' then
    raise ENyxModel.Create('Serialized node ID is required');
  LProps := TJSONObject(RequireField(LObject, 'props', jtObject));
  LChildren := TJSONArray(RequireField(LObject, 'children', jtArray));

  if LProps.Count > 256 then
    raise ENyxModel.Create('Node exceeds property budget');
  Result := TNyxNode.Create(LKind, LID);
  try
    for LIndex := 0 to LProps.Count - 1 do
    begin

      if LProps.Items[LIndex].JSONType <> jtString then
        raise ENyxModel.Create('Property values must be strings');
      Result.SetProp(LProps.Names[LIndex], LProps.Items[LIndex].AsString);
    end;
    ReadBindings(LObject.Find('bindings'), Result);

    if ACollectionViews and (LObject.Find(NyxCollectionViewWireField) <> nil) then
    begin
      Result.SetCollectionView(TNyxCollectionViewSpec.FromData(
        TNyxDataValue.ParseJSON(LObject.Find(NyxCollectionViewWireField).AsJSON)));
    end;
    ReadExtensions(LObject, Result.Extensions, False, ACollectionViews);
    for LIndex := 0 to LChildren.Count - 1 do
    begin
      Result.Add(ReadNode(LChildren.Items[LIndex], ADepth + 1, ACount, ACollectionViews));
    end;
  except
    Result.Free;
    raise;
  end;
end;


class function TNyxCodec.Decode(const ASource: TNyxText): TNyxDocument;
var
  LData: TJSONData;
  LRoot: TJSONObject;
  LPages: TJSONArray;
  LComponents: TJSONArray;
  LIndex: Integer;
  LCount: Integer;
  LVersion: TNyxText;
  LCollections: INyxCollectionDefaults;
begin
  LData := DecodeNyxJSON(ASource);
  try

    if (LData = nil) or (LData.JSONType <> jtObject) then
      raise ENyxModel.Create('Design root must be an object');
    LRoot := TJSONObject(LData);

    LVersion := RequireField(LRoot, 'version', jtNumber).AsJSON;

    if (LVersion <> '1') and (LVersion <> '2') and (LVersion <> '3') then
    begin
      raise ENyxModel.Create('Unsupported design version');
    end;
    LPages := TJSONArray(RequireField(LRoot, 'pages', jtArray));
    LComponents := TJSONArray(RequireField(LRoot, 'components', jtArray));
    Result := TNyxDocument.Create;
    try
      Result.Title := RequireField(LRoot, 'title', jtString).AsString;
      ReadState(LRoot.Find('state'), Result.State);

      if (LVersion = '2') or (LVersion = '3') then
      begin
        LCollections := DecodeNyxCollectionDefaults(
          RequireField(LRoot, NyxCollectionsWireField, jtObject).AsJSON);
        for LIndex := 0 to LCollections.Count - 1 do
        begin
          Result.Collections.Define(LCollections.Snapshot(LCollections.Key(LIndex)));
        end;
      end;
      ReadExtensions(LRoot, Result.Extensions, LVersion <> '1');
      LCount := 0;
      for LIndex := 0 to LPages.Count - 1 do
      begin
        Result.AddPage(ReadNode(LPages.Items[LIndex], 0, LCount, LVersion = '3'));
      end;
      for LIndex := 0 to LComponents.Count - 1 do
      begin
        Result.AddComponent(ReadNode(LComponents.Items[LIndex], 0, LCount, LVersion = '3'));
      end;
      Result.Validate;
    except
      Result.Free;
      raise;
    end;
  finally
    LData.Free;
  end;
end;

end.
