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
unit nyx.projection.refresh;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model;

{ Portable adapter boundary for an independently realized candidate. These
  routines own no document, node or native/DOM handle. A retained view may reuse
  only ordinary scalar presentation: identities, structure, contracts, binding
  descriptors, platform metadata and collection semantics must remain exact.
  Caller keeps both independent roots alive throughout checking/copying. }
function CanRefreshNyxProjection(AExisting, ACandidate: TNyxNode): Boolean;

{ Apply only a completely compatible tree. False changes nothing. Property
  collections are copied, never shared; callbacks/bindings/ownership remain on
  the existing realized nodes. Target adapters then call their normal Sync and
  retain an independent previous clone for rollback on a widget failure.
  Supplying ABaseline applies only changed authored fields, preserving independent
  runtime input for unchanged defaults. Nil copies all properties for rollback.
  The optional baseline must have the same completely compatible structure. }
function RefreshNyxProjectionProperties(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode = nil): Boolean;

{ Exact immutable document context for a mounted view. Fresh encoding validates
  current public data/budgets; it is not an admission cache. All document fields
  except title/pages/components remain significant, including defaults, tokens,
  collection registries and creator extension data. Requested-root realization
  separately checks structure and every node. Unrelated authored roots may
  change only when the requested realized view retains identical semantics. }
function NyxProjectionContext(ADocument: TNyxDocument): TNyxText;

implementation

uses
  nyx.codec, nyx.data;

function RefreshableKey(const AKey: TNyxText): Boolean;
var
  LAttribute: TNyxAttribute;
begin
  Result := TryNyxAttribute(AKey, LAttribute) and
    (LAttribute in [atText, atValue, atHint, atAccessibleName, atEnabled,
      atVisible, atReadOnly]);
end;

function CanRefreshNyxProjection(AExisting, ACandidate: TNyxNode): Boolean;
var
  LIndex: Integer;
  LKey: TNyxText;

  function CompatibleProperties(AFrom, ATo: TNyxNode): Boolean;
  var
    LProperty: Integer;
  begin
    Result := False;
    for LProperty := 0 to AFrom.Props.Count - 1 do
    begin
      LKey := AFrom.Props.Names[LProperty];

      if ((ATo.Props.IndexOfName(LKey) < 0) or
        (AFrom.Prop(LKey) <> ATo.Prop(LKey))) and not RefreshableKey(LKey) then
      begin
        Exit;
      end;
    end;
    Result := True;
  end;

begin
  Result := False;

  if (AExisting = nil) or (ACandidate = nil) then
  begin
    Exit;
  end;

  if (AExisting.Kind <> ACandidate.Kind) or
    (AExisting.ID <> ACandidate.ID) or
    (AExisting.SourceID <> ACandidate.SourceID) or
    (AExisting.DesignID <> ACandidate.DesignID) or
    (AExisting.InstanceScopeID <> ACandidate.InstanceScopeID) or
    (AExisting.ProjectionKind <> ACandidate.ProjectionKind) or
    (AExisting.IsRealized <> ACandidate.IsRealized) or
    (AExisting.Count <> ACandidate.Count) or
    (AExisting.BindingCount <> 0) or (ACandidate.BindingCount <> 0) or
    (AExisting.HasCollectionView <> ACandidate.HasCollectionView) or
    (AExisting.Extensions.ToJSON <> ACandidate.Extensions.ToJSON) then
  begin
    Exit;
  end;

  if AExisting.HasCollectionView and
    (AExisting.CollectionView.ToData.ToJSON <> ACandidate.CollectionView.ToData.ToJSON) then
  begin
    Exit;
  end;

  if not CompatibleProperties(AExisting, ACandidate) or
    not CompatibleProperties(ACandidate, AExisting) then
  begin
    Exit;
  end;
  for LIndex := 0 to AExisting.Count - 1 do
  begin

    if not CanRefreshNyxProjection(AExisting.Children[LIndex], ACandidate.Children[LIndex]) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function RefreshNyxProjectionProperties(AExisting, ACandidate: TNyxNode;
  ABaseline: TNyxNode): Boolean;

  procedure CopyProperties(AFrom, ATo, ABefore: TNyxNode);
  var
    LIndex: Integer;
    LKey: TNyxText;
    LProperty: Integer;
  begin

    if ABefore = nil then
    begin
      ATo.Props.Assign(AFrom.Props);
    end
    else
    begin
      { A mounted runtime may own edited text/values independent of its authored
        defaults. Apply only actual authored deltas; an unrelated caption must
        never reset that input to an unchanged document default. }
      for LIndex := 0 to AFrom.Props.Count - 1 do
      begin
        LKey := AFrom.Props.Names[LIndex];

        if (ABefore.Props.IndexOfName(LKey) < 0) or
          (ABefore.Prop(LKey) <> AFrom.Prop(LKey)) then
        begin
          ATo.SetProp(LKey, AFrom.Prop(LKey));
        end;
      end;
      for LIndex := 0 to ABefore.Props.Count - 1 do
      begin
        LKey := ABefore.Props.Names[LIndex];

        if AFrom.Props.IndexOfName(LKey) < 0 then
        begin
          LProperty := ATo.Props.IndexOfName(LKey);

          if LProperty >= 0 then
          begin
            ATo.Props.Delete(LProperty);
          end;
        end;
      end;
    end;
    for LIndex := 0 to AFrom.Count - 1 do
    begin

      if ABefore = nil then
      begin
        CopyProperties(AFrom.Children[LIndex], ATo.Children[LIndex], nil);
      end
      else
      begin
        CopyProperties(AFrom.Children[LIndex], ATo.Children[LIndex], ABefore.Children[LIndex]);
      end;
    end;
  end;

begin
  Result := CanRefreshNyxProjection(AExisting, ACandidate);

  if Result and (ABaseline <> nil) then
  begin
    Result := CanRefreshNyxProjection(ABaseline, ACandidate);
  end;

  if Result then
  begin
    CopyProperties(ACandidate, AExisting, ABaseline);
  end;
end;

function NyxProjectionContext(ADocument: TNyxDocument): TNyxText;
var
  LData: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
  LKey: TNyxText;
begin
  LData := TNyxDataValue.ParseJSON(TNyxCodec.Encode(ADocument));
  SetLength(LFields, LData.Count);
  LCount := 0;
  for LIndex := 0 to LData.Count - 1 do
  begin
    LKey := LData.Key(LIndex);

    if (LKey = 'title') or (LKey = 'pages') or (LKey = 'components') then
    begin
      Continue;
    end;
    LFields[LCount] := NyxField(LKey, LData.Field(LKey));
    Inc(LCount);
  end;
  SetLength(LFields, LCount);
  Result := NyxObject(LFields).ToJSON;
end;

end.
