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



{ Value-only display forms live inside the existing layout card, whose full
  schema/binding baseline and generic draft owner already survive remounts. }
const
  CFormatAction = 'studio-format-policy-action';
  CFormatColumn = 'studio-format-policy-column';
  CFormatFields: array[TNyxStudioFormatPolicyField] of TNyxText =
    ('style', 'name', 'revision', 'locale', 'decimal', 'group', 'scale',
     'rounding', 'date-order', 'date-separator', 'time-precision', 'first', 'second');
  CFormatCaptions: array[TNyxStudioFormatPolicyField] of TNyxText =
    ('Display style', 'Formatter identity', 'Formatter revision', 'Explicit locale name (optional)',
     'Decimal separator', 'Grouping separator (empty disables grouping)', 'Decimal places (0..28)',
     'Decimal rounding', 'Calendar order (date domain only)', 'Calendar separator',
     'Clock precision (time domain only)', 'True label / text prefix', 'False label / text suffix');
  CFormatStyles: array[TNyxValueFormatStyle] of TNyxText =
    ('Canonical accepted text', 'Fixed decimal', 'Calendar date', 'Clock time', 'Boolean labels', 'Text affixes');
  CFormatRounding: array[TNyxDecimalRounding] of TNyxText =
    ('Exact', 'To even', 'Away from zero', 'Toward zero', 'Floor', 'Ceiling');
  CFormatOrders: array[TNyxFormatDateOrder] of TNyxText =
    ('Year month day', 'Day month year', 'Month day year');
  CFormatPrecisions: array[TNyxTimePrecision] of TNyxText =
    ('Minute', 'Second', 'Tenth', 'Hundredth', 'Millisecond');

function NyxStudioFormatPolicyFormID(AColumn: Integer): TNyxText;
begin

  if (AColumn < 0) or (AColumn >= NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Display form requires a bounded column');
  end;
  Result := NyxStudioLayoutEditorID + '-display-' + TNyxText(IntToStr(AColumn));
end;

function NyxStudioFormatPolicyFieldID(AColumn: Integer;
  AField: TNyxStudioFormatPolicyField): TNyxText;
begin

  if (Ord(AField) < Ord(Low(TNyxStudioFormatPolicyField))) or
    (Ord(AField) > Ord(High(TNyxStudioFormatPolicyField))) then
  begin
    raise ENyxCollection.Create('Unknown display form role');
  end;
  Result := NyxStudioFormatPolicyFormID(AColumn) + '-' + CFormatFields[AField];
end;

function NyxStudioFormatPolicyApplyID(AColumn: Integer): TNyxText;
begin
  Result := NyxStudioFormatPolicyFormID(AColumn) + '-apply';
end;

function NyxStudioFormatPolicyDefaultID(AColumn: Integer): TNyxText;
begin
  Result := NyxStudioFormatPolicyFormID(AColumn) + '-default';
end;

procedure AddFormatPolicyForms(AEditor: TNyxNode; const AOwner: TNyxText;
  const ASchema: TNyxCollectionSchema; const ASpec: TNyxCollectionViewSpec; ALocked: Boolean);
var
  LIndex: Integer;
  LForm: TNyxNode;
  LColumn: TNyxCollectionColumn;
  LPolicy: TNyxValueFormat;
  LDisclosure: INyxDisclosure;
  LStatus: TNyxText;
  LStyles: TNyxText;
  LStyle: TNyxValueFormatStyle;
  LRounding: TNyxDecimalRounding;
  LOrder: TNyxFormatDateOrder;
  LPrecision: TNyxTimePrecision;
  LChoices: TNyxText;
  LDomain: TNyxValueDomain;

  procedure Field(AField: TNyxStudioFormatPolicyField; const AValue: TNyxText;
    const AChoices: TNyxText = '');
  var
    LControl: TNyxNode;
  begin
    LControl := TNyxNode.Create(nkInput, NyxStudioFormatPolicyFieldID(LIndex, AField));

    if AChoices <> '' then
    begin
      LControl.Free;
      LControl := TNyxNode.Create(nkSelect, NyxStudioFormatPolicyFieldID(LIndex, AField));
      LControl.Configure.Items(AChoices).Done;
    end;
    LControl.Configure.Text(CFormatCaptions[AField]).Value(AValue).Enabled(not ALocked).Done;
    LForm.Add(LControl);
  end;

begin
  for LIndex := 0 to ASpec.Count - 1 do
  begin
    LColumn := ASpec.ColumnAt(LIndex);
    LDomain := NyxNoDomain;
    case LColumn.Kind of
      nskText: LDomain := ASchema.Definition(NyxTextField(LColumn.FieldName)).Domain;
      nskBoolean: LDomain := ASchema.Definition(NyxBooleanField(LColumn.FieldName)).Domain;
      nskInteger: LDomain := ASchema.Definition(NyxIntegerField(LColumn.FieldName)).Domain;
      nskNumber: LDomain := ASchema.Definition(NyxNumberField(LColumn.FieldName)).Domain;
    end;
    LPolicy := LColumn.FormatPolicy;
    LForm := TNyxNode.Create(nkCard, NyxStudioFormatPolicyFormID(LIndex));
    LForm.Configure.Padding(8).Gap(6).Surface(True).Done;
    LForm.SetProp(CFormatColumn, TNyxText(IntToStr(LIndex))).SetProp(COwner, AOwner)
      .SetProp(CKey, ASpec.Key.Name).SetProp(CField, LColumn.FieldName)
      .SetProp(CKind, NyxStateKindName(LColumn.Kind))
      .SetProp(CPolicyBaseline, AEditor.Prop(NyxMenuFormBaselineKey));
    LForm.Add(TNyxNode.Create(nkHeading, LForm.ID + '-title')
      .Configure.Text(LColumn.Title + ' display').Done);
    LStatus := 'Canonical accepted text; no display policy declared.';

    if LColumn.HasFormatPolicy then
    begin
      LStatus := 'Explicit display policy. Editors, queries, comparison and RAW copy/export remain accepted data.';
    end;
    LForm.Add(TNyxNode.Create(nkLabel, LForm.ID + '-status').Configure.Text(LStatus +
      ' Date/time require the field domain. Fixed decimals use the exact decimal domain; unsupported values refuse.').Done);
    LStyles := '';
    for LStyle := Low(TNyxValueFormatStyle) to High(TNyxValueFormatStyle) do
    begin

      if (LStyle = nvfsCanonical) or ((LColumn.Kind in [nskInteger, nskNumber]) and
        (LStyle = nvfsDecimal)) or ((LColumn.Kind = nskBoolean) and (LStyle = nvfsBooleanLabels)) or
        ((LColumn.Kind = nskText) and ((LStyle = nvfsTextAffixes) or
          ((LStyle = nvfsCalendarDate) and LDomain.CalendarDate) or
          ((LStyle = nvfsClockTime) and LDomain.ClockTime))) then
      begin

        if LStyles <> '' then
        begin
          LStyles := LStyles + #10;
        end;
        LStyles := LStyles + CFormatStyles[LStyle];
      end;
    end;
    Field(sfpfStyle, CFormatStyles[LPolicy.Style], LStyles);
    Field(sfpfName, LPolicy.Identity.Name);
    Field(sfpfRevision, TNyxText(IntToStr(LPolicy.Revision)));
    Field(sfpfLocale, LPolicy.Locale.Locale.Name);
    Field(sfpfDecimal, LPolicy.Locale.DecimalSeparator);
    Field(sfpfGroup, LPolicy.Locale.GroupSeparator);
    Field(sfpfScale, TNyxText(IntToStr(LPolicy.Scale)));
    LChoices := '';
    for LRounding := Low(TNyxDecimalRounding) to High(TNyxDecimalRounding) do
    begin

      if LChoices <> '' then
      begin
        LChoices := LChoices + #10;
      end;
      LChoices := LChoices + CFormatRounding[LRounding];
    end;
    Field(sfpfRounding, CFormatRounding[LPolicy.Rounding], LChoices);
    LChoices := '';
    for LOrder := Low(TNyxFormatDateOrder) to High(TNyxFormatDateOrder) do
    begin

      if LChoices <> '' then
      begin
        LChoices := LChoices + #10;
      end;
      LChoices := LChoices + CFormatOrders[LOrder];
    end;
    Field(sfpfDateOrder, CFormatOrders[LPolicy.DateOrder], LChoices);
    Field(sfpfDateSeparator, LPolicy.DateSeparator);
    LChoices := '';
    for LPrecision := Low(TNyxTimePrecision) to High(TNyxTimePrecision) do
    begin

      if LChoices <> '' then
      begin
        LChoices := LChoices + #10;
      end;
      LChoices := LChoices + CFormatPrecisions[LPrecision];
    end;
    Field(sfpfTimePrecision, CFormatPrecisions[LPolicy.TimePrecision], LChoices);
    Field(sfpfFirst, LPolicy.FirstText);
    Field(sfpfSecond, LPolicy.SecondText);
    LForm.Add(TNyxNode.Create(nkButton, NyxStudioFormatPolicyApplyID(LIndex))
      .Configure.Text('Apply display defaults').Enabled(not ALocked)
      .Extension(CFormatAction, CPolicyApply).Done);
    LForm.Add(TNyxNode.Create(nkButton, NyxStudioFormatPolicyDefaultID(LIndex))
      .Configure.Text('Use canonical default').Enabled(not ALocked)
      .Extension(CFormatAction, CPolicyDefault).Done);
    LDisclosure := NewNyxDisclosure(LForm.ID + '-section').WithText(LColumn.Title + ' display');
    LDisclosure.Configure.Value(False).Surface(False).Done;
    LDisclosure.BodyColumn.Node.Add(LForm);
    AEditor.Add(LDisclosure);
  end;
end;

function CaptureNyxStudioFormat(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LForm: TNyxNode;
  LEditor: TNyxNode;
  LProjection: TNyxNode;
  LSpec: TNyxCollectionViewSpec;
  LSchema: TNyxCollectionSchema;
  LColumn: TNyxCollectionColumn;
  LField: TNyxStudioFormatPolicyField;
  LColumnIndex: Integer;
  LFieldIndex: Integer;
  LNumber: Integer;
  LRevision: Integer;
  LStyle: TNyxValueFormatStyle;
  LRounding: TNyxDecimalRounding;
  LOrder: TNyxFormatDateOrder;
  LPrecision: TNyxTimePrecision;
  LFound: Boolean;
  LOwner: TNyxText;
  LBaseline: TNyxText;
  LValue: TNyxText;
  LPolicy: TNyxValueFormat;
  LLocale: TNyxFormatLocale;

  function Role(const AID: TNyxText; AKind: TNyxKind): TNyxNode;
  begin
    Result := LForm.Find(AID);

    if (Result = nil) or (Result.Parent <> LForm) or
      (Result.Kind <> NyxKindName(AKind)) or (Result.ProjectionKind <> NyxKindName(AKind)) then
    begin
      raise ENyxCollection.Create('Use the current complete typed display form');
    end;
  end;

  function Value(AField: TNyxStudioFormatPolicyField): TNyxText;
  var
    LKind: TNyxKind;
  begin
    LKind := nkInput;

    if AField in [sfpfStyle, sfpfRounding, sfpfDateOrder, sfpfTimePrecision] then
    begin
      LKind := nkSelect;
    end;
    Result := Role(NyxStudioFormatPolicyFieldID(LColumnIndex, AField), LKind)
      .Prop(NyxAttributeName(atValue));
  end;

begin
  AEdit := Default(TNyxStudioDesignEdit);
  Result := (AButton <> nil) and (AButton.Prop(CFormatAction) <> '');

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (AShellRoot = nil) or (AShellRoot.Find(AButton.ID) <> AButton) or
    (AButton.Parent = nil) then
  begin
    raise ENyxCollection.Create('Use the current mounted display action');
  end;
  LForm := AButton.Parent;
  LEditor := AShellRoot.Find(NyxStudioLayoutEditorID);

  if (LEditor = nil) or (LEditor.Find(LForm.ID) <> LForm) or
    (LForm.Kind <> NyxKindName(nkCard)) or (LForm.ProjectionKind <> NyxKindName(nkCard)) or
    not TryNyxStateInteger(LForm.Prop(CFormatColumn), LColumnIndex) or
    (LColumnIndex < 0) or (LColumnIndex >= NyxMaximumCollectionFields) or
    (LForm.ID <> NyxStudioFormatPolicyFormID(LColumnIndex)) or
    (LForm.Prop(CFormatColumn) <> TNyxText(IntToStr(LColumnIndex))) then
  begin
    raise ENyxCollection.Create('Display action has no exact current column form');
  end;

  if ((AButton.Prop(CFormatAction) <> CPolicyApply) and
      (AButton.Prop(CFormatAction) <> CPolicyDefault)) or
    ((AButton.Prop(CFormatAction) = CPolicyApply) and
      (AButton.ID <> NyxStudioFormatPolicyApplyID(LColumnIndex))) or
    ((AButton.Prop(CFormatAction) = CPolicyDefault) and
      (AButton.ID <> NyxStudioFormatPolicyDefaultID(LColumnIndex))) then
  begin
    raise ENyxCollection.Create('Unknown display action');
  end;
  Role(NyxStudioFormatPolicyApplyID(LColumnIndex), nkButton);
  Role(NyxStudioFormatPolicyDefaultID(LColumnIndex), nkButton);
  Role(LForm.ID + '-title', nkHeading);
  Role(LForm.ID + '-status', nkLabel);
  for LField := Low(TNyxStudioFormatPolicyField) to High(TNyxStudioFormatPolicyField) do
  begin
    Value(LField);
  end;

  if LForm.Count <> 17 then
  begin
    raise ENyxCollection.Create('Display policy requires its complete direct form');
  end;
  LOwner := LForm.Prop(COwner);
  LBaseline := LForm.Prop(CPolicyBaseline);

  if (LOwner = '') or (ASession.SelectedID <> LOwner) or (LBaseline = '') or
    (LEditor.Prop(NyxMenuFormOwnerKey) <> LOwner) or
    (LEditor.Prop(NyxMenuFormBaselineKey) <> LBaseline) or
    APending.CollectionViewLocked(LOwner) or ASession.SourceDraftPending then
  begin
    raise ENyxCollection.Create('Select this table again after pending changes');
  end;
  LProjection := ASession.SelectedProjection;
  try

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkTable)) or
      not LProjection.HasCollectionView or not LProjection.CollectionView.Defined then
    begin
      raise ENyxCollection.Create('Display defaults require the selected bound table');
    end;
    LSpec := LProjection.CollectionView;
    LSchema := ASession.Document.Collections.Snapshot(LSpec.Key).Schema;

    if (LColumnIndex >= LSpec.Count) or (LForm.Prop(CKey) <> LSpec.Key.Name) or
      APending.CollectionLocked(LSpec.Key) or
      (NyxQueryEditorBaseline(LSchema, LSpec) <> LBaseline) then
    begin
      raise ENyxCollection.Create('Display binding/schema changed; reopen its form');
    end;
    LColumn := LSpec.ColumnAt(LColumnIndex);

    if (LColumn.FieldName <> LForm.Prop(CField)) or
      (NyxStateKindName(LColumn.Kind) <> LForm.Prop(CKind)) then
    begin
      raise ENyxCollection.Create('Display column changed its full typed identity');
    end;
    AEdit.Action := sdaCollection;
    AEdit.Selection := LOwner;
    AEdit.View := ASession.ActiveViewID;
    AEdit.Collection.Key := LSpec.Key;
    AEdit.Collection.Projection := cpTable;
    AEdit.Collection.LayoutBaseline := LBaseline;
    AEdit.Collection.Action := scaDefaultColumnFormat;
    for LFieldIndex := 0 to LSchema.Count - 1 do
    begin

      if (LSchema.FieldAt(LFieldIndex).Name = LColumn.FieldName) and
        (LSchema.FieldAt(LFieldIndex).Kind = LColumn.Kind) then
      begin
        AEdit.Collection.Field := TNyxStudioCollectionFieldRef.FromMetadata(LSchema.FieldAt(LFieldIndex));
        Break;
      end;
    end;

    if not AEdit.Collection.Field.Defined then
    begin
      raise ENyxCollection.Create('Display column no longer has its exact schema field');
    end;

    if AButton.Prop(CFormatAction) = CPolicyApply then
    begin
      LValue := Value(sfpfStyle);
      LFound := False;
      for LStyle := Low(TNyxValueFormatStyle) to High(TNyxValueFormatStyle) do
      begin

        if LValue = CFormatStyles[LStyle] then
        begin
          LFound := True;
          Break;
        end;
      end;

      if not LFound or not TryNyxStateInteger(Value(sfpfRevision), LRevision) then
      begin
        raise ENyxCollection.Create('Display requires a known style and exact revision');
      end;
      LPolicy := NyxValueFormat;
      case LStyle of
        nvfsCanonical: begin end;
        nvfsDecimal:
          begin
            LValue := Value(sfpfRounding);
            LFound := False;
            for LRounding := Low(TNyxDecimalRounding) to High(TNyxDecimalRounding) do
            begin

              if LValue = CFormatRounding[LRounding] then
              begin
                LFound := True;
                Break;
              end;
            end;

            if not LFound or not TryNyxStateInteger(Value(sfpfScale), LNumber) then
            begin
              raise ENyxCollection.Create('Decimal display needs exact places and known rounding');
            end;
            LPolicy := LPolicy.Decimal(LNumber, LRounding);
          end;
        nvfsCalendarDate:
          begin
            LValue := Value(sfpfDateOrder);
            LFound := False;
            for LOrder := Low(TNyxFormatDateOrder) to High(TNyxFormatDateOrder) do
            begin

              if LValue = CFormatOrders[LOrder] then
              begin
                LFound := True;
                Break;
              end;
            end;

            if not LFound then
            begin
              raise ENyxCollection.Create('Choose an exact calendar order');
            end;
            LPolicy := LPolicy.CalendarDate(LOrder, Value(sfpfDateSeparator));
          end;
        nvfsClockTime:
          begin
            LValue := Value(sfpfTimePrecision);
            LFound := False;
            for LPrecision := Low(TNyxTimePrecision) to High(TNyxTimePrecision) do
            begin

              if LValue = CFormatPrecisions[LPrecision] then
              begin
                LFound := True;
                Break;
              end;
            end;

            if not LFound then
            begin
              raise ENyxCollection.Create('Choose an exact clock precision');
            end;
            LPolicy := LPolicy.ClockTime(LPrecision);
          end;
        nvfsBooleanLabels: LPolicy := LPolicy.BooleanLabels(Value(sfpfFirst), Value(sfpfSecond));
        nvfsTextAffixes: LPolicy := LPolicy.TextAffixes(Value(sfpfFirst), Value(sfpfSecond));
      end;
      LLocale := NyxFormatLocale;

      if Value(sfpfLocale) <> '' then
      begin
        LLocale := LLocale.Named(NyxLocale(Value(sfpfLocale)));
      end;
      AEdit.Collection.Action := scaColumnFormat;
      AEdit.Collection.ColumnFormat := LPolicy
        .Identified(NyxFormatter(Value(sfpfName)), LRevision)
        .Localized(LLocale.NumberSeparators(Value(sfpfDecimal), Value(sfpfGroup)));
    end;
    ApplyNyxStudioCollectionViewIntent(AEdit.Collection, LSpec, LSchema);
  finally
    LProjection.Free;
  end;
end;
