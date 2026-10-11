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

{ Included after the shared typed emitters. A customization changes only explicit
  edited meaning: unrelated computed properties, defaults and resource definitions
  continue to be supplied by the original handwritten builder on every execution. }

function CustomizationVariable(ADocument: TNyxDocument; ANode: TNyxNode): TNyxText;
var
  LOrdinal: Integer;
  LMatches: Integer;
  LStem: TNyxText;

  procedure Visit(ACurrent: TNyxNode);
  var
    LIndex: Integer;
  begin

    if ControlStem(ACurrent.ID) + ControlStem(ACurrent.Kind) = LStem then
    begin
      Inc(LMatches);

      if ACurrent = ANode then
      begin
        LOrdinal := LMatches;
      end;
    end;
    for LIndex := 0 to ACurrent.Count - 1 do
    begin
      Visit(ACurrent.Children[LIndex]);
    end;
  end;

var
  LIndex: Integer;
begin
  LStem := ControlStem(ANode.ID) + ControlStem(ANode.Kind);
  LOrdinal := 0;
  LMatches := 0;
  for LIndex := 0 to ADocument.Count - 1 do
  begin
    Visit(ADocument.Pages[LIndex]);
  end;
  for LIndex := 0 to ADocument.ComponentCount - 1 do
  begin
    Visit(ADocument.Components[LIndex]);
  end;
  Result := 'L' + LStem;

  if LMatches > 1 then
  begin
    Result := Result + TNyxText(IntToStr(LOrdinal));
  end;
end;

function ConstructionOwner(ADocument: TNyxDocument; ANode: TNyxNode): TNyxText;
begin

  if ANode.Parent <> nil then
  begin
    Exit('ADocument.FindControl(NyxControl(' + PascalString(ANode.Parent.ID) + ')).Add');
  end;

  if ADocument.FindComponent(ANode.ID) = ANode then
  begin
    Exit('ADocument.AddComponent');
  end;
  Result := 'ADocument.AddPage';
end;


function CustomizationProperty(ADocument: TNyxDocument; ANode: TNyxNode;
  const AKey: TNyxText; AReset: Boolean = False): TNyxText;
var
  LKey: TNyxText;
  LPlatform: TNyxPlatform;
  LAttribute: TNyxAttribute;
  LViewport: TNyxViewportCondition;
  LPresentation: TNyxPresentationRef;
  LPresentationPlatform: TNyxPlatform;
  LPresentationAttribute: TNyxAttribute;
  LKind: TNyxKind;
begin

  if not TryNyxKind(ANode.Kind, LKind) or (LKind = nkSlotOverride) then
  begin
    raise ENyxModel.Create('Visual customization requires a specialized built-in control');
  end;
  LKey := AKey;
  LPlatform := npfAny;
  LViewport := TNyxViewportCondition.Any;
  LPresentation := Default(TNyxPresentationRef);
  Result := '  ' + CustomizationVariable(ADocument, ANode) + '.Configure' + #10;

  if TryNyxPlatformKey(AKey, LPlatform, LAttribute) then
  begin
    LKey := NyxAttributeName(LAttribute);
  end
  else if TryNyxViewportKey(AKey, LViewport, LPlatform, LAttribute) then
  begin
    LKey := NyxAttributeName(LAttribute);
  end;

  if TryNyxPresentationKey(AKey, LPresentation,
    LPresentationPlatform, LPresentationAttribute) then
  begin
    LPlatform := LPresentationPlatform;
    LKey := NyxAttributeName(LPresentationAttribute);
    Result := Result + '    .WhenPresentation(NyxPresentation(' +
      PascalString(LPresentation.Name) + '))' + #10;
  end
  else if not LViewport.IsAny then
  begin
    Result := Result + '    .WhenViewport(' + LViewport.Pascal + ')' + #10;
  end;

  if LPlatform <> npfAny then
  begin
    Result := Result + '    .ForPlatform(' + NyxPlatformSymbol(LPlatform) + ')' + #10;
  end;
  Result := Result + '    .' + ConfigurationCall(ANode, LKey, ANode.Prop(AKey),
    False, False, False, AReset) + #10 +
    '    .Done;' + #10;
end;

class function TNyxCodegen.CustomizationChanges(ABefore, AAfter: TNyxDocument):
  TNyxCustomizationBlocks;
var
  LCopy: TNyxDocument;
  LCount: Integer;
  LIndex: Integer;
  LTopologyChanged: Boolean;
  LTopology: TNyxText;

  function StateReference(AKind: TNyxStateKind; const AName: TNyxText): TNyxText;
  begin
    Result := CStateFactories[AKind] + '(' + PascalString(AName) + ')';
  end;

  procedure Add(const AID, AKey, APascal: TNyxText);
  begin
    SetLength(Result, LCount + 1);
    Result[LCount].ControlID := AID;
    Result[LCount].PropertyName := AKey;
    Result[LCount].Pascal := APascal;
    Inc(LCount);
  end;

  procedure Construct(ANode: TNyxNode);
  var
    LNew: TNyxNode;
    LKind: TNyxKind;
    LChild: Integer;
  begin

    if ABefore.Find(ANode.ID) = nil then
    begin

      if not TryNyxKind(ANode.Kind, LKind) then
      begin
        raise ENyxModel.Create('New visual construction requires an admitted specialized kind');
      end;
      { Clone all owned metadata, but construct each descendant independently.
        Existing descendants may move into this new host; never duplicate them. }
      LNew := ANode.Clone;
      try
        while LNew.Count > 0 do
        begin
          LNew.Remove(LNew.Children[0]);
        end;

        if ANode.Parent <> nil then
        begin
          LCopy.Find(ANode.Parent.ID).Add(LNew);
        end
        else if AAfter.FindComponent(ANode.ID) = ANode then
        begin
          LCopy.AddComponent(LNew);
        end
        else
        begin
          LCopy.AddPage(LNew);
        end;
        LNew := nil;
      finally
        LNew.Free;
      end;
      Add(ANode.ID, NyxCustomizationConstructKey, ConstructionStatements(AAfter, ANode));
      LTopologyChanged := True;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Construct(ANode.Children[LChild]);
    end;
  end;

  procedure Resources;
  var
    LPosition: Integer;
    LReference: TNyxResourceRef;
    LLocale: TNyxLocaleRef;
    LDefinition: INyxResourceDefinition;
    LStatement: TNyxText;
    LLocalePascal: TNyxText;
  begin
    { Remove missing exact variants first, releasing their catalog budget before
      adding replacements. Never clear/recreate the entire catalog: that would
      freeze unchanged helper-computed definitions into their observed values.
      Definitions are immutable; this independent candidate owns membership only.
      Stage-local ordinal identities avoid ambiguities in open reference names. }
    for LPosition := 0 to ABefore.Resources.Count - 1 do
    begin
      LReference := ABefore.Resources.Reference(LPosition);
      LLocale := ABefore.Resources.Locale(LPosition);

      if AAfter.Resources.Contains(LReference, LLocale) then
      begin
        Continue;
      end;
      LLocalePascal := 'NyxDefaultLocale';

      if LLocale.Defined then
      begin
        LLocalePascal := 'NyxLocale(' + PascalString(LLocale.Name) + ')';
      end;
      Add('', NyxCustomizationResourcePrefix + 'remove:' + TNyxText(IntToStr(LPosition)),
        '  ADocument.Resources.Remove(NyxResourceRef(' + PascalString(LReference.Name) +
        '), ' + LLocalePascal + ');' + #10);
      LCopy.Resources.Remove(LReference, LLocale);
    end;
    for LPosition := 0 to AAfter.Resources.Count - 1 do
    begin
      LReference := AAfter.Resources.Reference(LPosition);
      LLocale := AAfter.Resources.Locale(LPosition);
      LDefinition := AAfter.Resources.Definition(LReference, LLocale);

      if LCopy.Resources.Contains(LReference, LLocale) then
      begin

        if LCopy.Resources.Definition(LReference, LLocale).ToData.ToJSON =
          LDefinition.ToData.ToJSON then
        begin
          Continue;
        end;
      end;
      LStatement := '  ADocument.Resources.Define(NyxResourceRef(' +
        PascalString(LReference.Name) + '),' + #10;

      if LLocale.Defined then
      begin
        LStatement := LStatement + '    NyxLocale(' + PascalString(LLocale.Name) + '),' + #10;
      end;
      LStatement := LStatement + '    ' + PascalResource(LDefinition, '    ') + ');' + #10;
      Add('', NyxCustomizationResourcePrefix + 'define:' + TNyxText(IntToStr(LPosition)),
        LStatement);
      LCopy.Resources.Define(LReference, LLocale, LDefinition);
    end;
    { Define preserves existing positions; new variants append. The whole-document
      equality guard below refuses unexpressed reorderings and unrelated meaning.
      Actual constructor execution remains required before source publication. }
  end;

  { Registry comparison includes source relationships as well as empty seeds.
    Equal definitions never override helper-computed construction. Snapshots,
    rows and schema/domain values are immutable independent public contracts. }
  function SameCollection(const ALeft, ARight: INyxCollectionDefaults;
    const AKey: TNyxCollectionRef): Boolean;
  var
    LBefore: INyxCollectionSnapshot;
    LAfter: INyxCollectionSnapshot;
    LBeforeSource: TNyxResourceRows;
    LAfterSource: TNyxResourceRows;
    LHasBeforeSource: Boolean;
    LHasAfterSource: Boolean;
    LPosition: Integer;
  begin
    Result := False;

    if not ALeft.Has(AKey) or not ARight.Has(AKey) then
    begin
      Exit;
    end;
    LHasBeforeSource := NyxCollectionResourceSource(ALeft, AKey, LBeforeSource);
    LHasAfterSource := NyxCollectionResourceSource(ARight, AKey, LAfterSource);

    if LHasBeforeSource <> LHasAfterSource then
    begin
      Exit;
    end;

    if LHasBeforeSource and (LBeforeSource.ToData.ToJSON <> LAfterSource.ToData.ToJSON) then
    begin
      Exit;
    end;
    LBefore := ALeft.Snapshot(AKey);
    LAfter := ARight.Snapshot(AKey);

    if not LBefore.Schema.SameSchema(LAfter.Schema) or (LBefore.Count <> LAfter.Count) then
    begin
      Exit;
    end;
    for LPosition := 0 to LBefore.Count - 1 do
    begin

      if not LBefore.ItemAt(LPosition).SameItem(LAfter.ItemAt(LPosition)) then
      begin
        Exit;
      end;
    end;
    Result := True;
  end;

  { Same-schema static row editing starts from the original builder's current
    snapshot. Partial Update retains every unmentioned computed cell, including
    cells in the edited row; independent stores own no document/subscription link.
    Membership/order operations use stable typed item references. This chain is
    admitted as a complete replacement only after its own immutable snapshot.
    Schema changes and static/resource transitions use an explicit definition. }
  procedure CollectionRows(const ABeforeRows, AAfterRows: INyxCollectionSnapshot;
    ALines: TNyxStrings);
  const
    CFields: array[TNyxStateKind] of TNyxText =
      ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
  var
    LKey: TNyxText;
    LRow: TNyxCollectionItem;
    LBeforeRow: TNyxCollectionItem;
    LSchema: TNyxCollectionSchema;
    LField: TNyxCollectionField;
    LPosition: Integer;
    LFieldIndex: Integer;
    LLine: TNyxText;
    LChanged: Boolean;
    LOrderChanged: Boolean;
  begin
    LKey := 'NyxCollection(' + PascalString(AAfterRows.Key.Name) + ')';
    LSchema := AAfterRows.Schema;
    LOrderChanged := ABeforeRows.Count <> AAfterRows.Count;

    if not LOrderChanged then
    begin
      for LPosition := 0 to AAfterRows.Count - 1 do
      begin

        if AAfterRows.ItemAt(LPosition).Ref.ID <> ABeforeRows.ItemAt(LPosition).Ref.ID then
        begin
          LOrderChanged := True;
          Break;
        end;
      end;
    end;
    ALines.Add('  ADocument.Collections.Define(');
    ALines.Add('    NewNyxCollection(ADocument.Collections.Snapshot(' + LKey + '))');
    for LPosition := 0 to ABeforeRows.Count - 1 do
    begin
      LRow := ABeforeRows.ItemAt(LPosition);

      if not AAfterRows.Has(LRow.Ref) then
      begin
        ALines.Add('      .Remove(NyxItem(' + LKey + ', ' + PascalString(LRow.Ref.ID) + '))');
      end;
    end;
    for LPosition := 0 to AAfterRows.Count - 1 do
    begin
      LRow := AAfterRows.ItemAt(LPosition);
      LChanged := False;

      if ABeforeRows.Has(LRow.Ref) then
      begin
        LBeforeRow := ABeforeRows.Item(LRow.Ref);

        if LOrderChanged then
        begin
          ALines.Add('      .Move(NyxItem(' + LKey + ', ' + PascalString(LRow.Ref.ID) +
            '), ' + PascalInteger(LPosition) + ')');
        end;
        for LFieldIndex := 0 to LSchema.Count - 1 do
        begin

          if not LRow.FieldValue(LFieldIndex).SameValue(LBeforeRow.FieldValue(LFieldIndex)) then
          begin
            LChanged := True;
            Break;
          end;
        end;

        if not LChanged then
        begin
          Continue;
        end;
        ALines.Add('      .Update(');
      end
      else
      begin
        ALines.Add('      .Insert(' + PascalInteger(LPosition) + ',');
      end;
      LLine := '        NyxCollectionItem(NyxItem(' + LKey + ', ' +
        PascalString(LRow.Ref.ID) + '))';
      for LFieldIndex := 0 to LSchema.Count - 1 do
      begin
        LField := LSchema.FieldAt(LFieldIndex);

        if LChanged then
        begin

          if LRow.FieldValue(LFieldIndex).SameValue(LBeforeRow.FieldValue(LFieldIndex)) then
          begin
            Continue;
          end;
        end
        else if LRow.FieldValue(LFieldIndex).SameValue(LField.DefaultValue) then
        begin
          Continue;
        end;
        ALines.Add(LLine);
        LLine := '          .WithValue(' + CFields[LField.Kind] + '(' +
          PascalString(LField.Name) + '), ' + PascalCollectionValue(LRow.FieldValue(LFieldIndex)) + ')';
      end;
      ALines.Add(LLine + ')');
    end;
    ALines.Add('      .Snapshot);');
  end;

  { A schema edit reconstructs only the explicitly changed metadata. Retained
    fields reuse their typed definition, and retained row cells read the original
    builder's snapshot by stable item/field references. This prevents a default
    or constraint edit from freezing unrelated helper-computed values. The
    independent final definition owns no reference to the source snapshot. }
  procedure CollectionSchema(const ABeforeRows, AAfterRows: INyxCollectionSnapshot;
    ALines: TNyxStrings);
  const
    CFields: array[TNyxStateKind] of TNyxText =
      ('NyxTextField', 'NyxBooleanField', 'NyxIntegerField', 'NyxNumberField');
    CValues: array[TNyxStateKind] of TNyxText =
      ('FromText', 'FromBoolean', 'FromInteger', 'FromNumber');
  var
    LKey: TNyxText;
    LCurrent: TNyxText;
    LPrevious: TNyxText;
    LDefault: TNyxText;
    LDomain: TNyxText;
    LLine: TNyxText;
    LSchema: TNyxCollectionSchema;
    LBeforeSchema: TNyxCollectionSchema;
    LField: TNyxCollectionField;
    LBeforeField: TNyxCollectionField;
    LRow: TNyxCollectionItem;
    LBeforeRow: TNyxCollectionItem;
    LFieldIndex: Integer;
    LBeforeIndex: Integer;
    LRowIndex: Integer;
    LHasBefore: Boolean;
    LKeep: Boolean;

    function PreviousField(const AField: TNyxCollectionField): Integer;
    var
      LIndex: Integer;
    begin
      for LIndex := 0 to LBeforeSchema.Count - 1 do
      begin
        LBeforeField := LBeforeSchema.FieldAt(LIndex);

        if (LBeforeField.Name = AField.Name) and (LBeforeField.Kind = AField.Kind) then
        begin
          Exit(LIndex);
        end;
      end;
      Result := -1;
    end;

  begin
    LKey := 'NyxCollection(' + PascalString(AAfterRows.Key.Name) + ')';
    LCurrent := 'ADocument.Collections.Snapshot(' + LKey + ')';
    LSchema := AAfterRows.Schema;
    LBeforeSchema := ABeforeRows.Schema;
    ALines.Add('  ADocument.Collections.Define(');
    ALines.Add('    ' + LKey + ',');
    LLine := '    NyxCollectionSchema';

    if LSchema.Count = 0 then
    begin
      LLine := LLine + ',';
    end;
    ALines.Add(LLine);
    for LFieldIndex := 0 to LSchema.Count - 1 do
    begin
      LField := LSchema.FieldAt(LFieldIndex);
      LBeforeIndex := PreviousField(LField);
      LDefault := 'TNyxStateValue.' + CValues[LField.Kind] +
        '(' + PascalCollectionValue(LField.DefaultValue) + ')';
      LDomain := 'NyxNoDomain';

      if LField.Domain.Defined then
      begin
        LDomain := DomainExpression(LField.Domain) + '.Definition';
      end;
      LKeep := False;

      if LBeforeIndex >= 0 then
      begin
        LPrevious := LCurrent + '.Schema.Definition(' + CFields[LField.Kind] +
          '(' + PascalString(LField.Name) + '))';
        LKeep := LBeforeField.DefaultValue.SameValue(LField.DefaultValue) and
          (LBeforeField.Domain.ToData.ToJSON = LField.Domain.ToData.ToJSON);

        if LKeep then
        begin
          LLine := '      .Field(' + LPrevious + ')';
        end
        else
        begin

          if LBeforeField.DefaultValue.SameValue(LField.DefaultValue) then
          begin
            LDefault := LPrevious + '.DefaultValue';
          end;

          if LBeforeField.Domain.ToData.ToJSON = LField.Domain.ToData.ToJSON then
          begin
            LDomain := LPrevious + '.Domain';
          end;
        end;
      end;

      if not LKeep then
      begin
        LLine := '      .Field(' + PascalString(LField.Name) + ', ' + LDefault + ', ' + LDomain + ')';
      end;

      if LFieldIndex = LSchema.Count - 1 then
      begin
        LLine := LLine + ',';
      end;
      ALines.Add(LLine);
    end;
    ALines.Add('    [');
    for LRowIndex := 0 to AAfterRows.Count - 1 do
    begin
      LRow := AAfterRows.ItemAt(LRowIndex);
      LHasBefore := ABeforeRows.Has(LRow.Ref);

      if LHasBefore then
      begin
        LBeforeRow := ABeforeRows.Item(LRow.Ref);
      end;
      LLine := '      NyxCollectionItem(NyxItem(' + LKey + ', ' + PascalString(LRow.Ref.ID) + '))';
      for LFieldIndex := 0 to LSchema.Count - 1 do
      begin
        LField := LSchema.FieldAt(LFieldIndex);
        LBeforeIndex := PreviousField(LField);
        LKeep := LHasBefore and (LBeforeIndex >= 0);

        if LKeep then
        begin
          LKeep := LRow.FieldValue(LFieldIndex).SameValue(LBeforeRow.FieldValue(LBeforeIndex));
        end;

        if LKeep then
        begin
          LDefault := LCurrent + '.Item(NyxItem(' + LKey + ', ' +
            PascalString(LRow.Ref.ID) + ')).GetValue(' + CFields[LField.Kind] +
            '(' + PascalString(LField.Name) + '))';
        end
        else
        begin

          if LRow.FieldValue(LFieldIndex).SameValue(LField.DefaultValue) then
          begin
            Continue;
          end;
          LDefault := PascalCollectionValue(LRow.FieldValue(LFieldIndex));
        end;
        ALines.Add(LLine);
        LLine := '        .WithValue(' + CFields[LField.Kind] + '(' +
          PascalString(LField.Name) + '), ' + LDefault + ')';
      end;

      if LRowIndex < AAfterRows.Count - 1 then
      begin
        LLine := LLine + ',';
      end;
      ALines.Add(LLine);
    end;
    ALines.Add('    ]);');
  end;


  procedure Collections;
  var
    LPosition: Integer;
    LKey: TNyxCollectionRef;
    LBeforeRows: INyxCollectionSnapshot;
    LAfterRows: INyxCollectionSnapshot;
    LBeforeSource: TNyxResourceRows;
    LAfterSource: TNyxResourceRows;
    LHasBeforeSource: Boolean;
    LHasAfterSource: Boolean;
    LLines: TNyxStrings;
  begin
    for LPosition := 0 to ABefore.Collections.Count - 1 do
    begin
      LKey := ABefore.Collections.Key(LPosition);

      if not AAfter.Collections.Has(LKey) then
      begin
        Add('', NyxCustomizationCollectionPrefix + 'remove:' + TNyxText(IntToStr(LPosition)),
          '  ADocument.Collections.Remove(NyxCollection(' + PascalString(LKey.Name) + '));' + #10);
        LCopy.Collections.Remove(LKey);
      end;
    end;
    LLines := TNyxStrings.Create;
    try
      for LPosition := 0 to AAfter.Collections.Count - 1 do
      begin
        LKey := AAfter.Collections.Key(LPosition);

        if SameCollection(LCopy.Collections, AAfter.Collections, LKey) then
        begin
          Continue;
        end;
        LLines.Clear;
        LAfterRows := AAfter.Collections.Snapshot(LKey);
        LHasAfterSource := NyxCollectionResourceSource(AAfter.Collections, LKey, LAfterSource);
        LBeforeRows := nil;
        LHasBeforeSource := False;

        if LCopy.Collections.Has(LKey) then
        begin
          LBeforeRows := LCopy.Collections.Snapshot(LKey);
          LHasBeforeSource := NyxCollectionResourceSource(LCopy.Collections, LKey, LBeforeSource);
        end;

        if (LBeforeRows <> nil) and not LHasBeforeSource and not LHasAfterSource and
          LBeforeRows.Schema.SameSchema(LAfterRows.Schema) then
        begin
          CollectionRows(LBeforeRows, LAfterRows, LLines);
        end
        else if (LBeforeRows <> nil) and not LHasBeforeSource and not LHasAfterSource then
        begin
          CollectionSchema(LBeforeRows, LAfterRows, LLines);
        end
        else
        begin
          EmitCollectionDefinition(AAfter.Collections, LKey, LLines, 'ADocument', '  ');
        end;
        Add('', NyxCustomizationCollectionPrefix + 'define:' + TNyxText(IntToStr(LPosition)),
          LLines.Text);

        if LHasAfterSource then
        begin
          LCopy.ResourceCollections.Define(LKey, LAfterSource);
        end
        else
        begin
          LCopy.Collections.Define(LAfterRows);
        end;
      end;
    finally
      LLines.Free;
    end;
    { Retained definitions keep their position and new keys append. Complete
      codec equality still refuses an unexpressed registry reordering or any
      unrelated unsupported metadata, without publishing a partial proposal. }
  end;


  procedure Defaults;
  var
    LPosition: Integer;
    LKey: TNyxText;
    LOldKey: TNyxText;
    LValue: TNyxStateValue;
  begin
    { A same-family replacement at the retained position can use typed rename.
      This preserves an unchanged computed value; an explicit changed value is
      applied afterwards. Separate identities keep rename plus value update in
      one group without overwriting either statement. Complete equality below
      still refuses retyping or order/meaning these operations cannot express. }
    for LPosition := 0 to AAfter.State.Count - 1 do
    begin
      LKey := AAfter.State.Key(LPosition);

      if LCopy.State.Has(LKey) or (LPosition >= ABefore.State.Count) then
      begin
        Continue;
      end;
      LOldKey := ABefore.State.Key(LPosition);
      LValue := AAfter.State.Value(LKey);

      if AAfter.State.Has(LOldKey) or not LCopy.State.Has(LOldKey) or
        (LCopy.State.Value(LOldKey).Kind <> LValue.Kind) then
      begin
        Continue;
      end;
      case LValue.Kind of
        nskText:
          begin
            LCopy.State.Rename(NyxTextState(LOldKey), NyxTextState(LKey));
          end;
        nskBoolean:
          begin
            LCopy.State.Rename(NyxBooleanState(LOldKey), NyxBooleanState(LKey));
          end;
        nskInteger:
          begin
            LCopy.State.Rename(NyxIntegerState(LOldKey), NyxIntegerState(LKey));
          end;
        nskNumber:
          begin
            LCopy.State.Rename(NyxNumberState(LOldKey), NyxNumberState(LKey));
          end;
      end;
      Add('', NyxCustomizationStatePrefix + 'rename:' + LOldKey,
        '  ADocument.State.Rename(' + StateReference(LValue.Kind, LOldKey) + ', ' +
        StateReference(LValue.Kind, LKey) + ');' + #10);
    end;
    for LPosition := 0 to ABefore.State.Count - 1 do
    begin
      LKey := ABefore.State.Key(LPosition);

      if AAfter.State.Has(LKey) or not LCopy.State.Has(LKey) then
      begin
        Continue;
      end;
      LValue := LCopy.State.Value(LKey);
      LCopy.State.Remove(LKey);
      Add('', NyxCustomizationStatePrefix + 'remove:' + LKey,
        '  ADocument.State.Remove(' + StateReference(LValue.Kind, LKey) + ');' + #10);
    end;
    for LPosition := 0 to AAfter.State.Count - 1 do
    begin
      LKey := AAfter.State.Key(LPosition);
      LValue := AAfter.State.Value(LKey);

      if LCopy.State.Has(LKey) and LCopy.State.Value(LKey).SameValue(LValue) then
      begin
        Continue;
      end;
      LCopy.State.Apply([NyxStateAssign(LKey, LValue)]);
      Add('', NyxCustomizationStatePrefix + 'value:' + LKey,
        '  ADocument.State.SetValue(' + StateReference(LValue.Kind, LKey) + ', ' +
        PascalStateValue(LValue) + ');' + #10);
    end;
  end;

  procedure Place(ANode: TNyxNode; APosition: Integer; AReusable: Boolean);
  var
    LBefore: TNyxNode;
    LChild: Integer;
    LCall: TNyxText;
    LRoot: TNyxRootRef;
    LBeforeCount: Integer;
    LBeforeAt: TNyxNode;
  begin
    LBefore := ABefore.Find(ANode.ID);
    LBeforeAt := nil;

    if ANode.Parent = nil then
    begin

      if AReusable then
      begin
        LRoot := NyxReusableRoot(ANode.ID);
        LCall := '  ADocument.MoveRoot(NyxReusableRoot(' + PascalString(ANode.ID) +
          '), ' + PascalInteger(APosition) + ');' + #10;
        LBeforeCount := ABefore.ComponentCount;

        if APosition < LBeforeCount then
        begin
          LBeforeAt := ABefore.Components[APosition];
        end;
      end
      else
      begin
        LRoot := NyxPageRoot(ANode.ID);
        LCall := '  ADocument.MoveRoot(NyxPageRoot(' + PascalString(ANode.ID) +
          '), ' + PascalInteger(APosition) + ');' + #10;
        LBeforeCount := ABefore.Count;

        if APosition < LBeforeCount then
        begin
          LBeforeAt := ABefore.Pages[APosition];
        end;
      end;

      if (LBefore <> nil) and (ABefore.FindRoot(LRoot) <> LBefore) then
      begin
        raise ENyxModel.Create('Changing an existing root partition requires explicit reconciliation');
      end;
      LCopy.MoveRoot(LRoot, APosition);
    end
    else
    begin

      if (LBefore <> nil) and (LBefore.Parent = nil) then
      begin
        raise ENyxModel.Create('Moving a root into a control requires explicit reconciliation');
      end;

      if (LBefore <> nil) and (LBefore.Parent.ID = ANode.Parent.ID) and
        (APosition < LBefore.Parent.Count) then
      begin
        LBeforeAt := LBefore.Parent.Children[APosition];
      end;
      LCall := '  ADocument.MoveControl(NyxControl(' + PascalString(ANode.ID) +
        '), NyxControl(' + PascalString(ANode.Parent.ID) + '), ' +
        PascalInteger(APosition) + ');' + #10;
      LCopy.MoveControl(NyxControl(ANode.ID), NyxControl(ANode.Parent.ID), APosition);
    end;

    if (LBeforeAt = nil) or (LBeforeAt.ID <> ANode.ID) then
    begin
      LTopologyChanged := True;
    end;
    LTopology := LTopology + LCall;
    { Final preorder first moves a host out of any old descendant that will
      contain it. Every inserted prefix then exists before its next index. }
    for LChild := 0 to ANode.Count - 1 do
    begin
      Place(ANode.Children[LChild], LChild, AReusable);
    end;
  end;

  procedure Remove(ANode: TNyxNode);
  var
    LChild: Integer;
  begin

    if AAfter.Find(ANode.ID) = nil then
    begin
      { Keep removals independently addressable. A retained descendant may have
        moved out, and a previously constructed host may disappear from source.
        The typed conditional makes already-removed descendants harmless. }
      Add(ANode.ID, NyxCustomizationRemoveKey,
        #10 + '  if ADocument.FindControl(NyxControl(' + PascalString(ANode.ID) + ')) <> nil then' + #10 +
        '  begin' + #10 +
        '    ADocument.RemoveControl(NyxControl(' + PascalString(ANode.ID) + '));' + #10 +
        '  end;' + #10);

      if LCopy.Find(ANode.ID) <> nil then
      begin
        LCopy.RemoveControl(NyxControl(ANode.ID));
      end;
      LTopologyChanged := True;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Remove(ANode.Children[LChild]);
    end;
  end;

  procedure Visit(ANode: TNyxNode);
  var
    LBefore: TNyxNode;
    LCopyNode: TNyxNode;
    LProp: Integer;
    LChild: Integer;
    LKey: TNyxText;
    LSpec: TNyxBindingSpec;
    LPreviousSpec: TNyxBindingSpec;
    LCall: TNyxText;
    LCollectionLines: TNyxStrings;

    procedure BindingStatement(ATarget: TNyxBindingProperty; const ACall: TNyxText);
    begin
      Add(ANode.ID, NyxCustomizationBindingPrefix + CBindingMethods[ATarget],
        '  ' + CustomizationVariable(AAfter, ANode) + '.Binds' + #10 +
        '    .' + ACall + #10 + '    .Done;' + #10);
    end;
  begin
    LBefore := ABefore.Find(ANode.ID);
    LCopyNode := LCopy.Find(ANode.ID);

    if (LBefore <> nil) and (LBefore.Kind <> ANode.Kind) then
    begin
      raise ENyxModel.Create('Changed control kind requires explicit reconciliation');
    end;

    if LBefore <> nil then
    begin
      for LProp := 0 to LBefore.Props.Count - 1 do
      begin

        if ANode.Props.IndexOfName(LBefore.Props.Names[LProp]) < 0 then
        begin
          LKey := LBefore.Props.Names[LProp];
          Add(ANode.ID, LKey, CustomizationProperty(AAfter, ANode, LKey, True));
        end;
      end;
      for LProp := 0 to ANode.Props.Count - 1 do
      begin
        LKey := ANode.Props.Names[LProp];

        if (LBefore.Props.IndexOfName(LKey) < 0) or (LBefore.Prop(LKey) <> ANode.Prop(LKey)) then
        begin
          Add(ANode.ID, LKey, CustomizationProperty(AAfter, ANode, LKey));
        end;
      end;
      LCopyNode.Props.Assign(ANode.Props);
      for LProp := 0 to LBefore.BindingCount - 1 do
      begin
        LSpec := LBefore.Bindings[LProp];

        if not ANode.FindAuthoredBinding(LSpec.Target, LPreviousSpec) then
        begin
          LCopyNode.Binds.Inherit(LSpec.Target).Done;
          BindingStatement(LSpec.Target, 'Inherit(bp' + CBindingMethods[LSpec.Target] + ')');
        end;
      end;
      for LProp := 0 to ANode.BindingCount - 1 do
      begin
        LSpec := ANode.Bindings[LProp];

        if LBefore.FindAuthoredBinding(LSpec.Target, LPreviousSpec) and LPreviousSpec.Same(LSpec) then
        begin
          Continue;
        end;
        LCall := '';

        if not LSpec.Cleared and (LSpec.Source = bsState) then
        begin
          LCall := StateReference(LSpec.ValueKind, LSpec.StateName);
        end;
        LCopyNode.SetBinding(LSpec);
        BindingStatement(LSpec.Target, PascalBindingCall(LSpec, LCall));
      end;
      { Authored absence, explicit clear and a defined view remain distinct.
        Queries/search/scope/selection are copied through the ordinary fluent
        emitter, never reinterpreted as runtime selection or resource content. }

      if (LBefore.HasCollectionView <> ANode.HasCollectionView) or
        (ANode.HasCollectionView and
          (LBefore.CollectionView.ToData.ToJSON <> ANode.CollectionView.ToData.ToJSON)) then
      begin
        LCollectionLines := TNyxStrings.Create;
        try
          LCollectionLines.Add('  ' + CustomizationVariable(AAfter, ANode) + '.Binds');

          if ANode.HasCollectionView then
          begin
            LCopyNode.SetCollectionView(ANode.CollectionView);
            EmitCollectionBinding(ANode, LCollectionLines, '    ');
          end
          else
          begin
            LCopyNode.RemoveCollectionView;
            LCollectionLines.Add('    .InheritCollection');
          end;
          LCollectionLines.Add('    .Done;');
          Add(ANode.ID, NyxCustomizationCollectionViewKey, LCollectionLines.Text);
        finally
          LCollectionLines.Free;
        end;
      end;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LChild]);
    end;
  end;

begin

  if (ABefore = nil) or (AAfter = nil) then
  begin
    raise ENyxModel.Create('Customization requires independent before/after documents');
  end;
  ValidateNyxDocumentProperties(ABefore);
  ValidateNyxDocumentProperties(AAfter);
  Result := nil;
  LCount := 0;
  LTopologyChanged := False;
  LTopology := '';
  LCopy := ABefore.Clone;
  try

    if ABefore.Title <> AAfter.Title then
    begin
      Add('', 'title', '  ADocument.Title := ' + PascalString(AAfter.Title) + ';' + #10);
    end;
    LCopy.Title := AAfter.Title;
    Defaults;
    Resources;
    Collections;
    for LIndex := 0 to AAfter.Count - 1 do
    begin
      Construct(AAfter.Pages[LIndex]);
    end;
    for LIndex := 0 to AAfter.ComponentCount - 1 do
    begin
      Construct(AAfter.Components[LIndex]);
    end;
    for LIndex := 0 to AAfter.Count - 1 do
    begin
      Place(AAfter.Pages[LIndex], LIndex, False);
    end;
    for LIndex := 0 to AAfter.ComponentCount - 1 do
    begin
      Place(AAfter.Components[LIndex], LIndex, True);
    end;
    for LIndex := 0 to ABefore.Count - 1 do
    begin
      Remove(ABefore.Pages[LIndex]);
    end;
    for LIndex := 0 to ABefore.ComponentCount - 1 do
    begin
      Remove(ABefore.Components[LIndex]);
    end;

    if LTopologyChanged then
    begin
      Add('', NyxCustomizationTopologyKey, LTopology);
    end;
    for LIndex := 0 to AAfter.Count - 1 do
    begin
      Visit(AAfter.Pages[LIndex]);
    end;
    for LIndex := 0 to AAfter.ComponentCount - 1 do
    begin
      Visit(AAfter.Components[LIndex]);
    end;
    { New nodes retain all metadata; existing unhandled contracts/
      extensions still refuse as a whole. A proposal is not execution evidence. }

    if TNyxCodec.Encode(LCopy) <> TNyxCodec.Encode(AAfter) then
    begin
      raise ENyxModel.Create('This visual command needs broader meaning reconciliation');
    end;
  finally
    LCopy.Free;
  end;
end;

type
  TNyxCustomizationPhase = (ncpDefaults, ncpResources, ncpCollections, ncpConstruct, ncpProperties,
    ncpPlace, ncpRemove);

class function TNyxCodegen.CustomizationRoutine(ADocument: TNyxDocument;
  const ABlocks: TNyxCustomizationBlocks): TNyxText;
var
  LLines: TNyxStrings;
  LDeclared: TNyxStrings;
  LConstructed: TNyxStrings;
  LIndex: Integer;
  LNode: TNyxNode;
  LKind: TNyxKind;
  LVariable: TNyxText;
  LIdentity: TNyxText;
  LCharacter: Integer;
  LPhase: TNyxCustomizationPhase;
  LBlockPhase: TNyxCustomizationPhase;
begin
  LLines := TNyxStrings.Create;
  LDeclared := TNyxStrings.Create;
  LConstructed := TNyxStrings.Create;
  try
    LLines.Add('procedure CustomizeNyxDocument(ADocument: TNyxDocument);');
    for LIndex := 0 to High(ABlocks) do
    begin

      if ABlocks[LIndex].PropertyName = NyxCustomizationConstructKey then
      begin
        LConstructed.Add(ABlocks[LIndex].ControlID);
      end;
    end;
    for LIndex := 0 to High(ABlocks) do
    begin

      if (ABlocks[LIndex].ControlID = '') or
        (ABlocks[LIndex].PropertyName = NyxCustomizationRemoveKey) or
        (LDeclared.IndexOf(ABlocks[LIndex].ControlID) >= 0) then
      begin
        Continue;
      end;
      LNode := ADocument.Find(ABlocks[LIndex].ControlID);

      if (LNode = nil) or not TryNyxKind(LNode.Kind, LKind) then
      begin
        raise ENyxModel.Create('Managed customization control no longer exists');
      end;

      if LDeclared.Count = 0 then
      begin
        LLines.Add('var');
      end;
      LDeclared.Add(LNode.ID);
      LLines.Add('  ' + CustomizationVariable(ADocument, LNode) + ': INyx' +
        ControlStem(LNode.Kind) + ';');
    end;
    LLines.Add('begin');
    for LIndex := 0 to LDeclared.Count - 1 do
    begin

      if LConstructed.IndexOf(LDeclared[LIndex]) >= 0 then
      begin
        Continue;
      end;
      LNode := ADocument.Find(LDeclared[LIndex]);
      LVariable := CustomizationVariable(ADocument, LNode);
      LLines.Add('  ' + LVariable + ' := RequireNyxControl(ADocument, NyxControl(' +
        PascalString(LNode.ID) + '), nk' + ControlStem(LNode.Kind) + ') as INyx' +
        ControlStem(LNode.Kind) + ';');
    end;
    { A single compiled group prepares defaults/resources, constructs parents
      before children, applies values, places retained controls in final preorder,
      then removes remaining dead subtrees. Continuation preserves earlier groups
      in separate Pascal scopes. }
    for LPhase := Low(TNyxCustomizationPhase) to High(TNyxCustomizationPhase) do
    begin
      for LIndex := 0 to High(ABlocks) do
      begin
        LBlockPhase := ncpProperties;

        if Copy(ABlocks[LIndex].PropertyName, 1, Length(NyxCustomizationStatePrefix)) =
          NyxCustomizationStatePrefix then
        begin
          LBlockPhase := ncpDefaults;
        end
        else if Copy(ABlocks[LIndex].PropertyName, 1, Length(NyxCustomizationResourcePrefix)) =
          NyxCustomizationResourcePrefix then
        begin
          LBlockPhase := ncpResources;
        end
        else if Copy(ABlocks[LIndex].PropertyName, 1, Length(NyxCustomizationCollectionPrefix)) =
          NyxCustomizationCollectionPrefix then
        begin
          LBlockPhase := ncpCollections;
        end
        else if ABlocks[LIndex].PropertyName = NyxCustomizationConstructKey then
        begin
          LBlockPhase := ncpConstruct;
        end
        else if ABlocks[LIndex].PropertyName = NyxCustomizationTopologyKey then
        begin
          LBlockPhase := ncpPlace;
        end
        else if ABlocks[LIndex].PropertyName = NyxCustomizationRemoveKey then
        begin
          LBlockPhase := ncpRemove;
        end;

        if LBlockPhase <> LPhase then
        begin
          Continue;
        end;
        LIdentity := ABlocks[LIndex].ControlID + ABlocks[LIndex].PropertyName;
        for LCharacter := 1 to Length(LIdentity) do
        begin

          if Ord(LIdentity[LCharacter]) < 32 then
          begin
            raise ENyxModel.Create('Customization identities cannot contain control characters');
          end;
        end;
        { These ordinary comments are exact editor ownership boundaries, not
          behavior switches. Quoted identities protect punctuation and Unicode. }
        LLines.Add('');
        LLines.Add('  // nyx:customize ' + PascalString(ABlocks[LIndex].ControlID) + ' ' +
          PascalString(ABlocks[LIndex].PropertyName));
        LLines.Add(TrimRight(ABlocks[LIndex].Pascal));
      end;
    end;
    LLines.Add('  // nyx:customizations-statements-end');
    LLines.Add('end;');
    Result := LLines.Text;
  finally
    LConstructed.Free;
    LDeclared.Free;
    LLines.Free;
  end;
end;
