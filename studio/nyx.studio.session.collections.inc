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


{ Ordinary collection replay. This include belongs to nyx.studio.session:
  requests contain only immutable values; candidate document/source ownership
  and paired publication remain with the existing session command boundary. }

function TNyxStudioPendingDesign.CollectionLocked(const AKey: TNyxCollectionRef): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(Collections) do
  begin

    if not NyxStudioCollectionViewAction(Collections[LIndex].Intent.Action) and
      NyxStudioCollectionStructuralAction(Collections[LIndex].Intent.Action) and
      (Collections[LIndex].Intent.Key.Name = AKey.Name) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioPendingDesign.CollectionViewLocked(const AOwner: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(Collections) do
  begin

    if (Collections[LIndex].Owner = AOwner) and
      NyxStudioCollectionViewAction(Collections[LIndex].Intent.Action) and
      NyxStudioCollectionStructuralAction(Collections[LIndex].Intent.Action) then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioPendingDesign.CollectionCreationPending: Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(Collections) do
  begin

    if Collections[LIndex].Intent.Action = scaCreate then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioPendingDesign.CollectionValue(const AKey: TNyxCollectionRef;
  const AItem: TNyxItemRef; const AField: TNyxStudioCollectionFieldRef;
  AAction: TNyxStudioCollectionAction; const AOwner: TNyxText;
  out AIntent: TNyxStudioCollectionIntent): Boolean;
var
  LIndex: Integer;
  LItemMatches: Boolean;
begin
  AIntent := Default(TNyxStudioCollectionIntent);
  for LIndex := High(Collections) downto 0 do
  begin

    if (Collections[LIndex].Owner <> AOwner) or
      (Collections[LIndex].Intent.Action <> AAction) or
      (Collections[LIndex].Intent.Key.Name <> AKey.Name) or
      not Collections[LIndex].Intent.Field.SameReference(AField) or
      (Collections[LIndex].Intent.Item.Defined <> AItem.Defined) then
    begin
      Continue;
    end;
    LItemMatches := True;

    if AItem.Defined then
    begin
      LItemMatches := (Collections[LIndex].Intent.Item.Collection.Name = AItem.Collection.Name) and
        (Collections[LIndex].Intent.Item.ID = AItem.ID);
    end;

    if LItemMatches then
    begin
      AIntent := Collections[LIndex].Intent;
      Exit(True);
    end;
  end;
  Result := False;
end;

function RequireCollectionIntentField(const ASchema: TNyxCollectionSchema;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionField;
var
  LIndex: Integer;
begin
  for LIndex := 0 to ASchema.Count - 1 do
  begin
    Result := ASchema.FieldAt(LIndex);

    if Result.Name = AField.Name then
    begin

      if Result.Kind <> AField.Kind then
      begin
        raise ENyxCollection.Create('Collection field changed its scalar family');
      end;
      Exit;
    end;
  end;
  raise ENyxCollection.Create('Collection field no longer exists');
end;

function PutCollectionIntentValue(const AItem: TNyxCollectionItem;
  const AField: TNyxStudioCollectionFieldRef; const AValue: TNyxStateValue): TNyxCollectionItem;
begin

  if AField.Kind <> AValue.Kind then
  begin
    raise ENyxCollection.Create('Collection cell value has the wrong scalar family');
  end;
  case Ord(AField.Kind) of
    Ord(nskText):
      begin
        Result := AItem.WithValue(NyxTextField(AField.Name), AValue.TextValue);
      end;
    Ord(nskBoolean):
      begin
        Result := AItem.WithValue(NyxBooleanField(AField.Name), AValue.BooleanValue);
      end;
    Ord(nskInteger):
      begin
        Result := AItem.WithValue(NyxIntegerField(AField.Name), AValue.IntegerValue);
      end;
    Ord(nskNumber):
      begin
        Result := AItem.WithValue(NyxNumberField(AField.Name), AValue.NumberValue);
      end;
  else
    begin
      raise ENyxCollection.Create('Collection cell has an unknown scalar family');
    end;
  end;
end;

{ A cleared local descriptor intentionally hides the effective collection key.
  Inherit must qualify the captured key against this same owner's inherited
  contract, without removing the live mask first. The temporary document and
  realized root are independent; the returned specification owns its values. }
function RestoredCollectionView(ADocument: TNyxDocument; const AOwner: TNyxText):
  TNyxCollectionViewSpec;
var
  LDocument: TNyxDocument;
  LNode: TNyxNode;
  LRoot: TNyxNode;
  LProjection: TNyxNode;
begin
  Result := Default(TNyxCollectionViewSpec);
  LDocument := ADocument.Clone;
  try
    LNode := LDocument.Find(AOwner);

    if (LNode = nil) or not LNode.HasCollectionView or LNode.CollectionView.Defined then
    begin
      raise ENyxCollection.Create('Inheritance requires this exact cleared local descriptor');
    end;
    LNode.RemoveCollectionView;
    LRoot := RealizeNyxContext(LDocument, LNode, LProjection);
    try

      if (LProjection <> nil) and LProjection.HasCollectionView then
      begin
        Result := LProjection.CollectionView;
      end;
    finally
      LRoot.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

function TNyxStudioSession.ClearedCollectionInheritance(const AOwner: TNyxText):
  TNyxCollectionViewSpec;
begin
  Result := RestoredCollectionView(Document, AOwner);
end;

procedure TNyxStudioSession.ApplyCollectionIntent(const AIntent: TNyxStudioCollectionIntent);
var
  LData: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LNextSchema: TNyxCollectionSchema;
  LItems: array of TNyxCollectionItem;
  LField: TNyxCollectionField;
  LValue: TNyxStateValue;
  LIndex: Integer;
  LRow: Integer;
  LName: TNyxText;
  LFound: Boolean;
  LProjection: TNyxNode;
  LExpectedKind: TNyxText;
  LSpec: TNyxCollectionViewSpec;
begin
  AIntent.Validate;

  if (AIntent.Action = scaQuery) and SourceDraftPending then
  begin
    { A complete authored query replacement also regenerates managed Pascal.
      Refuse before any mutation while a handwritten draft is pending, matching
      the existing menu/form candidate contract and external paired admission. }
    raise ENyxCollection.Create('Resolve the pending Pascal draft before applying a query');
  end;

  if AIntent.Action = scaCreate then
  begin

    if Document.Collections.Has(AIntent.Key) then
    begin
      raise ENyxCollection.Create('Collection creation name is already owned');
    end;
    DefineCollection(AIntent.Key, NyxCollectionSchema.Text(NyxTextField('caption'), ''), []);
    Exit;
  end;

  if AIntent.Action = scaRemove then
  begin
    RemoveCollection(AIntent.Key);
    Exit;
  end;
  LData := Document.Collections.Snapshot(AIntent.Key);
  LSchema := LData.Schema;

  if NyxStudioCollectionViewAction(AIntent.Action) then
  begin
    LProjection := SelectedProjection;
    try

      if LProjection = nil then
      begin
        raise ENyxCollection.Create('Collection view authoring requires its selected control');
      end;
      case Ord(AIntent.Projection) of
        Ord(cpList):
          begin
            LExpectedKind := NyxKindName(nkList);
          end;
        Ord(cpTable):
          begin
            LExpectedKind := NyxKindName(nkTable);
          end;
        Ord(cpTree):
          begin
            LExpectedKind := NyxKindName(nkTree);
          end;
      else
        begin
          raise ENyxCollection.Create('Collection view has an unknown projection');
        end;
      end;

      if LProjection.ProjectionKind <> LExpectedKind then
      begin
        raise ENyxCollection.Create('Collection view changed its projection kind');
      end;
      LSpec := LProjection.CollectionView;

      if AIntent.Action = scaInherit then
      begin

        if not LSpec.Defined then
        begin
          LSpec := ClearedCollectionInheritance(FSelectedID);
        end;

        if not LSpec.Defined or (LSpec.Key.Name <> AIntent.Key.Name) then
        begin
          raise ENyxCollection.Create('Collection binding changed before inheritance');
        end;
        InheritCollectionView;
      end
      else
      begin
        SetCollectionView(ApplyNyxStudioCollectionViewIntent(AIntent, LSpec, LSchema));
      end;
    finally
      LProjection.Free;
    end;
    Exit;
  end;
  SetLength(LItems, LData.Count);
  for LIndex := 0 to LData.Count - 1 do
  begin
    LItems[LIndex] := LData.ItemAt(LIndex);
  end;
  case AIntent.Action of
    scaAddField:
      begin
        LIndex := 1;
        repeat
          LName := NyxStateKindName(AIntent.Kind) + IntToStr(LIndex);
          Inc(LIndex);
          LFound := False;
          for LRow := 0 to LSchema.Count - 1 do
          begin
            LFound := LFound or (LSchema.FieldAt(LRow).Name = LName);
          end;
        until not LFound;
        case AIntent.Kind of
          nskText:
            begin
              LSchema := LSchema.Text(NyxTextField(LName), '');
            end;
          nskBoolean:
            begin
              LSchema := LSchema.Boolean(NyxBooleanField(LName), False);
            end;
          nskInteger:
            begin
              LSchema := LSchema.Integer(NyxIntegerField(LName), 0);
            end;
          nskNumber:
            begin
              LSchema := LSchema.Number(NyxNumberField(LName), 0);
            end;
        end;
      end;
    scaDefault:
      begin
        RequireCollectionIntentField(LSchema, AIntent.Field);
        LValue := ParseNyxStudioStateInput(AIntent.Input, AIntent.Value);
        LNextSchema := NyxCollectionSchema;
        for LIndex := 0 to LSchema.Count - 1 do
        begin
          LField := LSchema.FieldAt(LIndex);

          if LField.Name = AIntent.Field.Name then
          begin
            LNextSchema := LNextSchema.Field(LField.Name, LValue, LField.Domain);
          end
          else
          begin
            LNextSchema := LNextSchema.Field(LField.Name, LField.DefaultValue, LField.Domain);
          end;
        end;
        LSchema := LNextSchema;
      end;
    scaAddRow:
      begin

        if LData.IndexOf(AIntent.Item) >= 0 then
        begin
          raise ENyxCollection.Create('Collection row creation identity is already owned');
        end;
        SetLength(LItems, LData.Count + 1);
        LItems[LData.Count] := NyxCollectionItem(AIntent.Item);
      end;
    scaRemoveRow, scaCell:
      begin
        LRow := LData.IndexOf(AIntent.Item);

        if LRow < 0 then
        begin
          raise ENyxCollection.Create('Collection row no longer exists');
        end;

        if AIntent.Action = scaRemoveRow then
        begin
          for LIndex := LRow to High(LItems) - 1 do
          begin
            LItems[LIndex] := LItems[LIndex + 1];
          end;
          SetLength(LItems, Length(LItems) - 1);
        end
        else
        begin
          RequireCollectionIntentField(LSchema, AIntent.Field);
          LValue := ParseNyxStudioStateInput(AIntent.Input, AIntent.Value);
          LItems[LRow] := PutCollectionIntentValue(LItems[LRow], AIntent.Field, LValue);
        end;
      end;
  else
    begin
      raise ENyxCollection.Create('Unsupported ordinary collection data operation');
    end;
  end;
  { The existing candidate boundary validates domains, rows, binding references
    and source meaning before either accepted owner or history can change. }
  DefineCollection(AIntent.Key, LSchema, LItems);
end;
