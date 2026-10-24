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
program nyx_collection_presentation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.collections, nyx.collections.arrangement,
  nyx.collections.presentation, nyx.collections.view.types, nyx.collections.view,
  nyx.collections.workbench.policy, nyx.publication
  {$ifdef PAS2JS}, Web{$endif};

type
  { Borrowed observer distinguishes presentation notifications from real store
    edits. Its owner outlives both tokens and never retains the observed view. }
  TObserver = class
  public
    Views: Integer;
    Stores: Integer;
    procedure ViewChanged(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
    procedure StoreChanged(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
  end;

const
  CCommand: TNyxText = 'Command 🌙';
  CComposed: TNyxText = 'é';
  CDecomposed: TNyxText = 'é';
  CExactText: TNyxText = '🌙' + #0 + 'line' + #13#10 + 'tail';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure TObserver.ViewChanged(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
begin
  Inc(Views);
end;

procedure TObserver.StoreChanged(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
begin
  Inc(Stores);
end;

procedure ConfigurePresentation(const AView: INyxCollectionView;
  const APreferences: TNyxTablePresentationPreferences);
var
  LPresentation: INyxCollectionTablePresentation;
begin
  { The fluent self-return belongs only to this bounded procedure. In pas2js an
    entered routine may retain its interface-result temporaries until it exits. }
  Supports(AView, INyxCollectionTablePresentation, LPresentation);
  LPresentation.ConfigurePresentation(APreferences);
end;

procedure ConfigureColumns(const AView: INyxCollectionView;
  const APreferences: TNyxColumnPreferences);
var
  LColumns: INyxCollectionColumns;
begin
  Supports(AView, INyxCollectionColumns, LColumns);
  LColumns.ConfigureColumns(APreferences);
end;

function Replacing(const AData: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  I: Integer;
begin
  SetLength(LFields, AData.Count);
  for I := 0 to AData.Count - 1 do
  begin
    LFields[I] := NyxField(AData.Key(I), AData.Field(AData.Key(I)));

    if LFields[I].Name = AName then
    begin
      LFields[I].Value := AValue;
    end;
  end;
  Result := NyxObject(LFields);
end;

procedure RefuseTable(const AJSON: TNyxText);
var
  LPolicy: TNyxTablePresentationPolicy;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LPolicy := NyxTablePresentation.Density(ntrdComfortable);
  LBefore := LPolicy.ToData.ToJSON;
  LRefused := False;
  try
    LPolicy := TNyxTablePresentationPolicy.FromData(TNyxDataValue.ParseJSON(AJSON));
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPolicy.ToData.ToJSON = LBefore),
    'table codec refuses atomically: ' + AJSON);
end;

procedure RefuseColumn(const AJSON: TNyxText);
var
  LPolicy: TNyxColumnTextPolicy;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LPolicy := NyxColumnText.Wrap(5);
  LBefore := LPolicy.ToData.ToJSON;
  LRefused := False;
  try
    LPolicy := TNyxColumnTextPolicy.FromData(TNyxDataValue.ParseJSON(AJSON));
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPolicy.ToData.ToJSON = LBefore),
    'column codec refuses atomically: ' + AJSON);
end;

procedure RefusePreferences(const AData: TNyxDataValue);
var
  LPreferences: TNyxTablePresentationPreferences;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LPreferences := NyxTablePresentationPreferences.Density(ntrdCompact)
    .WithColumn(NyxColumnTextPreference(NyxColumnIdentity(NyxTextField(CCommand)), NyxColumnText.Ellipsis));
  LBefore := LPreferences.ToData.ToJSON;
  LRefused := False;
  try
    LPreferences := TNyxTablePresentationPreferences.FromData(AData);
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPreferences.ToData.ToJSON = LBefore),
    'preference admission retains the exact previous vector on refusal');
end;

procedure AuthoredValues;
var
  LTable: TNyxTablePresentationPolicy;
  LColumn: TNyxColumnTextPolicy;
  LCopy: TNyxColumnTextPolicy;
  LDensity: TNyxTableRowDensity;
  LRefused: Boolean;
  LBefore: TNyxText;
begin
  LTable := Default(TNyxTablePresentationPolicy);
  LColumn := Default(TNyxColumnTextPolicy);
  Check(not LTable.Copy.Defined and (LTable.ToData.Kind = ndNull) and
    not LColumn.Copy.Defined and (LColumn.ToData.Kind = ndNull),
    'absent body/text choices remain absent through copy and wire');
  Check(not TNyxTablePresentationPolicy.FromData(NyxNull).Defined and
    not TNyxColumnTextPolicy.FromData(NyxNull).Defined,
    'null is the exact authored absence/reset representation');
  LRefused := False;
  try
    LTable := LTable.Density(ntrdNormal);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LTable.Defined,
    'an absent table cannot accidentally manufacture a fluent authored choice');
  LRefused := False;
  try
    LColumn := LColumn.Wrap;
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and not LColumn.Defined,
    'an absent column cannot accidentally manufacture a fluent authored choice');
  LTable := NyxTablePresentation;
  LColumn := NyxColumnText;
  Check(LTable.Defined and (LTable.RowDensity = ntrdNormal) and
    LColumn.Defined and (LColumn.Overflow = nctoClip) and (LColumn.MaximumLines = 1),
    'explicit Normal/Clip retain authored presence independently of absence');
  for LDensity := Low(TNyxTableRowDensity) to High(TNyxTableRowDensity) do
  begin
    LTable := LTable.Density(LDensity);
    Check(TNyxTablePresentationPolicy.FromData(LTable.ToData).RowDensity = LDensity,
      'every closed density survives exact typed wire admission');
  end;
  Check((NyxTableRowPadding(ntrdCompact) = 2) and
    (NyxTableRowPadding(ntrdNormal) = 6) and (NyxTableRowPadding(ntrdComfortable) = 10),
    'body padding has explicit logical metrics rather than platform pixel assumptions');
  LColumn := LColumn.Wrap(7);
  LCopy := LColumn.Ellipsis;
  Check((LColumn.Overflow = nctoWrap) and (LColumn.MaximumLines = 7) and
    (LCopy.Overflow = nctoEllipsis) and (LCopy.MaximumLines = 1),
    'single-line ellipsis clears its inactive wrap limit without mutating the original');
  LCopy := LColumn.Clip;
  Check((LCopy.Overflow = nctoClip) and (LCopy.MaximumLines = 1) and
    (LColumn.MaximumLines = 7), 'single-line clipping is a detached complete choice');
  LCopy := NyxColumnText.Wrap(1);
  Check((LCopy.Overflow = nctoWrap) and (LCopy.MaximumLines = 1),
    'one wrapped line remains distinct from unwrapped clipping');
  LCopy := NyxColumnText.Wrap(High(Integer));
  Check(TNyxColumnTextPolicy.FromData(LCopy.ToData).MaximumLines = High(Integer),
    'positive caller-chosen line count is not narrowed by an adapter work default');
  LColumn := TNyxColumnTextPolicy.FromData(LColumn.ToData);
  Check((LColumn.Overflow = nctoWrap) and (LColumn.MaximumLines = 7),
    'same-destination successful read keeps the complete column choice');
  LBefore := LColumn.ToData.ToJSON;
  LRefused := False;
  try
    LColumn := LColumn.Wrap(0);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LColumn.ToData.ToJSON = LBefore),
    'zero-line builder refusal retains the caller accepted policy');
  LRefused := False;
  try
    LColumn := LColumn.Wrap(-1);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LColumn.ToData.ToJSON = LBefore),
    'negative visible-line builder refusal is atomic');
end;

procedure WireRefusals;
begin
  RefuseTable('false');
  RefuseTable('{"version":1,"density":"normal","extra":0}');
  RefuseTable('{"version":1}');
  RefuseTable('{"version":2,"density":"normal"}');
  RefuseTable('{"version":"1","density":"normal"}');
  RefuseTable('{"version":1,"density":false}');
  RefuseTable('{"version":1,"density":"NORMAL"}');
  RefuseColumn('[]');
  RefuseColumn('{"version":1,"overflow":"wrap"}');
  RefuseColumn('{"version":2,"overflow":"wrap","maximumLines":2}');
  RefuseColumn('{"version":1,"overflow":"wrap","maximumLines":2,"extra":false}');
  RefuseColumn('{"version":1,"overflow":"scroll","maximumLines":1}');
  RefuseColumn('{"version":1,"overflow":"wrap","maximumLines":0}');
  RefuseColumn('{"version":1,"overflow":"wrap","maximumLines":-1}');
  RefuseColumn('{"version":1,"overflow":"wrap","maximumLines":1.5}');
  RefuseColumn('{"version":1,"overflow":"wrap","maximumLines":"2"}');
  RefuseColumn('{"version":1,"overflow":"ellipsis","maximumLines":2}');
  RefuseColumn('{"version":1,"overflow":"clip","maximumLines":2}');
end;

procedure RuntimeWork;
var
  LLimits: TNyxTableTextWorkLimits;
  LPolicy: TNyxColumnTextPolicy;
  LText: TNyxText;
  LRefused: Boolean;
begin
  LLimits := TNyxTableTextWorkLimits.Defaults;
  LPolicy := NyxColumnText.Wrap(1000);
  LText := CExactText;
  Check((LLimits.Admit(LPolicy, LText) = nttaLineLimit) and
    (LPolicy.MaximumLines = 1000),
    'runtime line refusal leaves a larger valid authored maximum intact');
  LLimits := LLimits.Lines(1000);
  Check(LLimits.Admit(LPolicy, LText) = nttaReady,
    'caller can raise runtime line work independently of authored policy');
  LLimits := LLimits.UTF8Bytes(3);
  LPolicy := NyxColumnText;
  Check(LLimits.Admit(LPolicy, '🌙') = nttaTextLimit,
    'byte budget measures a supplementary scalar as four UTF-8 bytes on both targets');
  LLimits := LLimits.UTF8Bytes(4);
  Check(LLimits.Admit(LPolicy, '🌙') = nttaReady,
    'exact supplementary scalar byte boundary is admitted');
  LLimits := LLimits.UTF8Bytes(65536);
  Check((LLimits.Admit(LPolicy, LText) = nttaReady) and
    (LText = CExactText),
    'admission preserves embedded NUL and original line endings without returning replacement text');
  LRefused := False;
  try
    LLimits := LLimits.Lines(0);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LLimits.MaximumLines = 1000),
    'invalid work-line update retains the previous operational allowance');
  LRefused := False;
  try
    LLimits := LLimits.UTF8Bytes(NyxMaximumCollectionBytes + 1);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LLimits.MaximumUTF8Bytes = 65536),
    'operational bytes cannot exceed the established collection packet ceiling');
end;

procedure RuntimePreferences;
var
  LPrefs: TNyxTablePresentationPreferences;
  LPeer: TNyxTablePresentationPreferences;
  LRestored: TNyxTablePresentationPreferences;
  LTable: TNyxTablePresentationPolicy;
  LColumn: TNyxColumnTextPolicy;
  LCommand: TNyxColumnIdentity;
  LScore: TNyxColumnIdentity;
  LWrong: TNyxColumnIdentity;
  LBefore: TNyxText;
  LFields: array of TNyxColumnIdentity;
  LValues: array of TNyxDataValue;
  LRefused: Boolean;
  I: Integer;
begin
  LCommand := NyxColumnIdentity(NyxTextField(CCommand));
  LScore := NyxColumnIdentity(NyxIntegerField('Score'));
  LWrong := NyxColumnIdentity(NyxIntegerField(CCommand));
  LPrefs := Default(TNyxTablePresentationPreferences);
  Check(not LPrefs.Copy.Defined and (LPrefs.ToData.Kind = ndNull) and
    not LPrefs.ResolveTable(Default(TNyxTablePresentationPolicy)).Defined and
    not LPrefs.ResolveColumn(LCommand, Default(TNyxColumnTextPolicy)).Defined,
    'absent runtime preferences leave both authored absence and legacy presentation intact');
  LPrefs := NyxTablePresentationPreferences;
  Check(LPrefs.Defined and not LPrefs.HasDensity and (LPrefs.Count = 0),
    'explicit empty runtime preferences inherit every authored choice');
  LTable := NyxTablePresentation.Density(ntrdComfortable);
  LColumn := NyxColumnText.Wrap(4);
  Check((LPrefs.ResolveTable(LTable).RowDensity = ntrdComfortable) and
    (LPrefs.ResolveColumn(LCommand, LColumn).MaximumLines = 4),
    'empty preferences resolve to complete authored values');
  LPrefs := LPrefs.Density(ntrdCompact)
    .WithColumn(NyxColumnTextPreference(LCommand, NyxColumnText.Ellipsis))
    .WithColumn(NyxColumnTextPreference(LScore, NyxColumnText.Wrap(2)));
  LPeer := LPrefs.Copy;
  LBefore := LPrefs.ToData.ToJSON;
  LPrefs := LPrefs.WithColumn(NyxColumnTextPreference(LCommand, NyxColumnText.Wrap(8)));
  Check((LPeer.ColumnAt(0).TextPolicy.Overflow = nctoEllipsis) and
    (LPrefs.ColumnAt(0).TextPolicy.MaximumLines = 8) and (LPeer.ToData.ToJSON = LBefore),
    'replacing one copied override preserves the independent peer vector');
  LPeer := LPeer.WithoutColumn(LCommand).UseDefaultDensity;
  Check(not LPeer.HasDensity and (LPeer.Count = 1) and LPrefs.HasDensity and
    (LPrefs.Count = 2) and (LPrefs.RowDensity = ntrdCompact),
    'density reset and field removal do not mutate another runtime snapshot');
  Check((LPeer.ResolveTable(LTable).RowDensity = ntrdComfortable) and
    (LPeer.ResolveColumn(LCommand, LColumn).MaximumLines = 4) and
    (LPeer.ResolveColumn(LScore, NyxColumnText).MaximumLines = 2),
    'independent resets restore the exact authored choices and leave other fields overridden');
  LPrefs := LPrefs.WithoutColumn(LScore).WithColumn(NyxColumnTextPreference(LCommand, NyxColumnText));
  Check((LPrefs.ResolveColumn(LCommand, LColumn).Overflow = nctoClip) and
    (LColumn.MaximumLines = 4) and (LTable.RowDensity = ntrdComfortable),
    'explicit Clip differs from default inheritance without mutating either authored policy');
  LRestored := TNyxTablePresentationPreferences.FromData(LPrefs.ToData);
  Check((LRestored.ToData.ToJSON = LPrefs.ToData.ToJSON) and
    LRestored.ColumnAt(0).Field.Same(LCommand),
    'preference wire preserves exact supplementary Unicode name and scalar family');
  LRestored := TNyxTablePresentationPreferences.FromData(LRestored.ToData);
  Check(LRestored.ToData.ToJSON = LPrefs.ToData.ToJSON,
    'same-destination preference read is safe for a sole owning vector');
  LRestored.ValidateFields([LScore, LCommand]);
  Check(LRestored.IndexOf(LCommand) = 0,
    'current schema order never becomes the preference field identity');
  LBefore := LRestored.ToData.ToJSON;
  LRefused := False;
  try
    LRestored.ValidateFields([LScore]);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LRestored.ToData.ToJSON = LBefore),
    'missing authored field refuses schema adoption without dropping unrelated preferences');
  LRefused := False;
  try
    LRestored.ValidateFields([LWrong, LScore]);
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LRestored.ToData.ToJSON = LBefore),
    'same field name with a new scalar family cannot reinterpret the saved override');
  LRefused := False;
  try
    LRestored := LRestored.WithColumn(NyxColumnTextPreference(LWrong, NyxColumnText));
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LRestored.ToData.ToJSON = LBefore),
    'a same-name conflicting fluent replacement keeps the original typed vector');
  LPrefs := NyxTablePresentationPreferences
    .WithColumn(NyxColumnTextPreference(NyxColumnIdentity(NyxTextField(CComposed)), NyxColumnText.Wrap(2)))
    .WithColumn(NyxColumnTextPreference(NyxColumnIdentity(NyxTextField(CDecomposed)), NyxColumnText.Ellipsis));
  Check((LPrefs.Count = 2) and (LPrefs.ColumnAt(0).Field.Name = CComposed) and
    (LPrefs.ColumnAt(1).Field.Name = CDecomposed),
    'visually similar Unicode field names remain exact independent identities');
  RefusePreferences(NyxObject([NyxField('version', NyxData(1)),
    NyxField('density', NyxData('normal')), NyxField('columns', NyxArray([
      NyxObject([NyxField('field', LCommand.ToData), NyxField('text', NyxColumnText.ToData)]),
      NyxObject([NyxField('field', LCommand.ToData), NyxField('text', NyxColumnText.Wrap(2).ToData)])]))]));
  RefusePreferences(NyxObject([NyxField('version', NyxData(1)),
    NyxField('density', NyxNull), NyxField('columns', NyxArray([
      NyxObject([NyxField('field', LCommand.ToData), NyxField('text', NyxNull)])]))]));
  RefusePreferences(TNyxDataValue.ParseJSON('{"version":2,"density":null,"columns":[]}'));
  RefusePreferences(TNyxDataValue.ParseJSON('{"version":1,"density":false,"columns":[]}'));
  RefusePreferences(TNyxDataValue.ParseJSON('{"version":1,"density":null,"columns":[],"extra":0}'));
  LPrefs := NyxTablePresentationPreferences;
  SetLength(LFields, NyxMaximumColumnLayoutFields);
  SetLength(LValues, NyxMaximumColumnLayoutFields + 1);
  for I := 0 to High(LFields) do
  begin
    LFields[I] := NyxColumnIdentity(NyxTextField('column' + IntToStr(I)));
    LPrefs := LPrefs.WithColumn(NyxColumnTextPreference(LFields[I], NyxColumnText.Wrap(2)));
    LValues[I] := NyxObject([NyxField('field', LFields[I].ToData),
      NyxField('text', NyxColumnText.ToData)]);
  end;
  LPrefs.ValidateFields(LFields);
  Check(LPrefs.Count = NyxMaximumColumnLayoutFields,
    'the exact current authored-field bound is supported without coordinates');
  LBefore := LPrefs.ToData.ToJSON;
  LRefused := False;
  try
    LPrefs := LPrefs.WithColumn(NyxColumnTextPreference(
      NyxColumnIdentity(NyxTextField('one-too-many')), NyxColumnText));
  except
    on E: EArgumentException do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LPrefs.ToData.ToJSON = LBefore),
    'oversized fluent preference refuses without shortening the accepted vector');
  LValues[High(LValues)] := NyxObject([NyxField('field',
    NyxColumnIdentity(NyxTextField('one-too-many')).ToData), NyxField('text', NyxColumnText.ToData)]);
  RefusePreferences(NyxObject([NyxField('version', NyxData(1)),
    NyxField('density', NyxNull), NyxField('columns', NyxArray(LValues))]));
end;

procedure DescriptorCases;
var
  LKey: TNyxCollectionRef;
  LBase: TNyxCollectionViewSpec;
  LSpec: TNyxCollectionViewSpec;
  LPeer: TNyxCollectionViewSpec;
  LDecoded: TNyxCollectionViewSpec;
  LData: TNyxDataValue;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LKey := NyxCollection('text policies 🌙');
  LBase := NyxCollectionView(LKey).Column(NyxTextField(CCommand), CExactText)
    .Column(NyxIntegerField('Score'), 'Score').TableWorkbench(NyxTableWorkbench);
  LBefore := LBase.ToData.ToJSON;
  Check((LBase.ToData.Field('version').AsInteger = 10) and not LBase.HasPresentationPolicy,
    'version ten remains unchanged without an authored presentation choice');
  LSpec := LBase.ColumnText(NyxColumnText.Ellipsis).TablePresentation(NyxTablePresentation);
  LPeer := LSpec.Copy;
  LSpec := LSpec.ColumnText(NyxColumnText.Wrap(6));
  Check((LPeer.ColumnAt(1).TextPolicy.Overflow = nctoEllipsis) and
    (LSpec.ColumnAt(1).TextPolicy.MaximumLines = 6) and not LBase.HasColumnText,
    'preceding-column authoring detaches its complete vector and leaves the prior binding exact');
  LData := LSpec.ToData;
  Check((LData.Field('version').AsInteger = 11) and (LData.Count = 13) and
    (LData.Field('columns').Item(0).Field('textPresentation').Kind = ndNull) and
    (LData.Field('columns').Item(1).Count = 9) and
    LSpec.HasTablePresentationPolicy and LSpec.HasColumnText,
    'version eleven owns a closed nullable shape while independent column absence stays exact');
  LDecoded := TNyxCollectionViewSpec.FromData(LData);
  Check((LDecoded.ToData.ToJSON = LData.ToJSON) and
    (LDecoded.ColumnAt(0).Title = CExactText) and
    (LDecoded.TablePresentationPolicy.RowDensity = ntrdNormal) and
    (LDecoded.ColumnAt(1).TextPolicy.MaximumLines = 6),
    'complete descriptor replay preserves typed layout and exact Unicode/NUL caption');
  LDecoded := LDecoded.UseDefaultTablePresentation;
  Check(not LDecoded.HasTablePresentationPolicy and LDecoded.HasColumnText and
    (LDecoded.ToData.Field('version').AsInteger = 11),
    'table reset preserves the independent per-column text choice');
  LDecoded := LDecoded.UseDefaultColumnText;
  Check(not LDecoded.HasPresentationPolicy and (LDecoded.ToData.ToJSON = LBefore),
    'removing all presentation presence restores the exact previous version-ten bytes');
  LBefore := LSpec.ToData.ToJSON;
  LRefused := False;
  try
    LSpec := TNyxCollectionViewSpec.FromData(Replacing(LData, 'tablePresentation', NyxData(False)));
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSpec.ToData.ToJSON = LBefore),
    'malformed nested presentation retains the complete old binding on same-result assignment');
  LSpec := LBase.TablePresentation(NyxTablePresentation);
  LData := Replacing(LSpec.ToData, 'tablePresentation', NyxNull);
  LRefused := False;
  try
    LSpec := TNyxCollectionViewSpec.FromData(LData);
  except
    on E: Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and LSpec.HasTablePresentationPolicy,
    'all-null presentation cannot manufacture a version-eleven descriptor');
  LRefused := False;
  try
    ValidateNyxCollectionViewProjection(LSpec, cpList);
  except
    on E: ENyxCollection do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'known list projections refuse table presentation before any host is created');
end;

procedure PublicationCases;
var
  LKey: TNyxCollectionRef;
  LItem: TNyxItemRef;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LPeer: INyxCollectionView;
  LNext: INyxCollectionView;
  LSource: INyxCollectionSnapshot;
  LSpec: TNyxCollectionViewSpec;
  LPrefs: TNyxTablePresentationPreferences;
  LColumns: INyxCollectionColumns;
  LPresentation: INyxCollectionTablePresentation;
  LPeerPresentation: INyxCollectionTablePresentation;
  LCapture: INyxCollectionPresentationCapture;
  LAfter: INyxCollectionPresentationCapture;
  LPrepared: INyxPreparedPublication;
  LRejected: INyxPreparedPublication;
  LStoreToken: INyxCollectionSubscription;
  LViewToken: INyxCollectionViewSubscription;
  LObserver: TObserver;
  LSerial: Integer;
  LCount: Integer;
  LRefused: Boolean;
begin
  LKey := NyxCollection('presentation publication');
  LItem := NyxItem(LKey, 'process 🌙');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField(CCommand), '')
    .Integer(NyxIntegerField('Score'), 0), [NyxCollectionItem(LItem)
      .WithValue(NyxTextField(CCommand), CExactText).WithValue(NyxIntegerField('Score'), 4)]);
  LSpec := NyxCollectionView(LKey).Column(NyxTextField(CCommand), 'Command')
    .ColumnText(NyxColumnText.Wrap(3)).Column(NyxIntegerField('Score'), 'Score')
    .TableLayout(NyxTableLayout).TablePresentation(NyxTablePresentation);
  LView := NewNyxCollectionView(LStore, LSpec, cpTable);
  LPeer := NewNyxCollectionView(LStore, LSpec, cpTable);
  LSource := LStore.Snapshot;
  LObserver := TObserver.Create;
  try
    LViewToken := LView.Subscribe({$ifdef PAS2JS}@{$endif}LObserver.ViewChanged);
    LStoreToken := LStore.Subscribe({$ifdef PAS2JS}@{$endif}LObserver.StoreChanged);
    Supports(LView, INyxCollectionTablePresentation, LPresentation);
    Supports(LPeer, INyxCollectionTablePresentation, LPeerPresentation);
    Supports(LView, INyxCollectionColumns, LColumns);
    ConfigureColumns(LView, NyxColumnPreferences.WithColumn(
      NyxColumnPreference(NyxColumnIdentity(NyxTextField(CCommand))).Width(160)));
    LCount := LObserver.Views;
    LCapture := LPresentation.CapturePresentation;
    LPrefs := NyxTablePresentationPreferences.Density(ntrdCompact).WithColumn(
      NyxColumnTextPreference(NyxColumnIdentity(NyxTextField(CCommand)), NyxColumnText.Wrap(7)));
    LPrepared := LPresentation.PreparePresentation(LCapture, LPrefs);
    LRefused := False;
    try
      LRejected := LColumns.PrepareColumns(LColumns.CaptureColumns, NyxColumnPreferences);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LObserver.Views = LCount),
      'text plan reserves the existing column slot before a competing width plan can enter');
    PublishNyxGroup([LPrepared]);
    LAfter := LPresentation.CapturePresentation;
    Check((LObserver.Views = LCount + 1) and (LObserver.Stores = 0) and
      (LStore.Snapshot = LSource) and (LView.CellText(LItem, 0) = CExactText) and
      (LAfter.Spec.ToData.ToJSON = LSpec.ToData.ToJSON),
      'one presentation publication notifies once without changing source, RAW or authored descriptor');
    Check((LColumns.CaptureColumns.Preferences.ColumnAt(0).WidthValue = 160) and
      (LAfter.Preferences.RowDensity = ntrdCompact) and
      (LAfter.Preferences.ColumnAt(0).TextPolicy.MaximumLines = 7) and
      not LPeerPresentation.CapturePresentation.Preferences.HasDensity,
      'density/text overrides preserve width ownership and an independent peer instance');
    LSerial := LAfter.Serial;
    LCount := LObserver.Views;
    ConfigurePresentation(LView, LPrefs);
    Check((LObserver.Views = LCount) and (LPresentation.CapturePresentation.Serial = LSerial),
      'equal preference bytes are a complete no-op without a new serial');
    LRefused := False;
    try
      LRejected := LPeerPresentation.PreparePresentation(LAfter, LPrefs);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'another table cannot adopt a capture with the same descriptor and visible data');
    ConfigurePresentation(LView, NyxTablePresentationPreferences);
    ConfigurePresentation(LView, LPrefs);
    LRefused := False;
    try
      LRejected := LPresentation.PreparePresentation(LAfter, LPrefs);
    except
      on E: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'preference ABA cannot revive an older column/presentation capture');
    LNext := NewNyxCollectionView(LStore, NyxCollectionView(LKey)
      .Column(NyxIntegerField('Score'), 'Score').Column(NyxTextField(CCommand), 'New command')
      .ColumnText(NyxColumnText.Ellipsis).TableLayout(NyxTableLayout)
      .TablePresentation(NyxTablePresentation.Density(ntrdComfortable)), cpTable);
    LPrepared := PrepareNyxCollectionColumnTransfer(LView, LNext);
    PublishNyxGroup([LPrepared]);
    { The matched browser Supports out bridge writes a new facade directly;
      release the prior peer lease explicitly before replacing that destination. }
    LPeerPresentation := nil;
    Supports(LNext, INyxCollectionTablePresentation, LPeerPresentation);
    LAfter := LPeerPresentation.CapturePresentation;
    Check((LAfter.Preferences.RowDensity = ntrdCompact) and
      (LAfter.Preferences.ColumnAt(0).Field.Name = CCommand) and
      (LAfter.Preferences.ColumnAt(0).TextPolicy.MaximumLines = 7) and
      (LAfter.Spec.ColumnAt(0).FieldName = 'Score') and
      (LAfter.Spec.TablePresentationPolicy.RowDensity = ntrdComfortable),
      'same-store replacement carries independent runtime choices by typed field across a new authored order');
    LNext := nil;
    LPeerPresentation := nil;
    LColumns := nil;
    LPresentation := nil;
    LPeer := nil;
    LPrepared := nil;
    LRejected := nil;
    LView := nil;
    LStore := nil;
    Check(not LViewToken.Connected and not LStoreToken.Connected,
      'retained presentation captures/prefs keep no real view or store alive: view=' +
      TNyxText(BoolToStr(LViewToken.Connected, True)) + ', store=' +
      TNyxText(BoolToStr(LStoreToken.Connected, True)));
    Check((LCapture.Spec.ColumnAt(0).FieldName = CCommand) and
      (LAfter.Preferences.ColumnAt(0).TextPolicy.MaximumLines = 7),
      'detached authored/preference observations stay readable after actual owner retirement');
  finally
    LRejected := nil;
    LPrepared := nil;
    LPeerPresentation := nil;
    LPresentation := nil;
    LColumns := nil;
    LNext := nil;
    LPeer := nil;
    LView := nil;
    LStore := nil;
    LViewToken := nil;
    LStoreToken := nil;
    LObserver.Free;
  end;
end;

{ Explicit runtime choices do not require persisting a dummy authored default.
  The same view publication/transfer owners retain typed fields and reject ABA. }
procedure RuntimeOnlyPublication;
var
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LPeer: INyxCollectionView;
  LNext: INyxCollectionView;
  LPolicy: INyxCollectionTablePresentation;
  LCapture: INyxCollectionPresentationCapture;
  LPrepared: INyxPreparedPublication;
  LSource: INyxCollectionSnapshot;
  LSpec: TNyxCollectionViewSpec;
  LPrefs: TNyxTablePresentationPreferences;
  LKey: TNyxCollectionRef;
  LRefused: Boolean;
begin
  LKey := NyxCollection('runtime-only-presentation');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField(CCommand), '')
    .Integer(NyxIntegerField('Score'), 0), [NyxCollectionItem(NyxItem(LKey, 'one'))
      .WithValue(NyxTextField(CCommand), CExactText).WithValue(NyxIntegerField('Score'), 4)]);
  LSpec := NyxCollectionView(LKey).Column(NyxTextField(CCommand), 'Command')
    .Column(NyxIntegerField('Score'), 'Score');
  LView := NewNyxCollectionView(LStore, LSpec, cpTable);
  LPeer := NewNyxCollectionView(LStore, LSpec, cpTable);
  try
    LSource := LStore.Snapshot;
    LPolicy := NyxCollectionTablePresentation(LView);
    LCapture := LPolicy.CapturePresentation;
    LPrefs := NyxTablePresentationPreferences.Density(ntrdComfortable).WithColumn(
      NyxColumnTextPreference(NyxColumnIdentity(NyxTextField(CCommand)), NyxColumnText.Wrap(3)));
    LPrepared := LPolicy.PreparePresentation(LCapture, LPrefs);
    LPrepared.Validate;
    LPrepared.Retire;
    LPrepared := nil;
    Check(LPolicy.CapturePresentation.Serial = LCapture.Serial,
      'retiring an admitted runtime-only plan publishes no preference or serial');
    ConfigurePresentation(LView, LPrefs);
    Check(not LView.Spec.HasPresentationPolicy and
      (LView.Spec.ToData.ToJSON = LSpec.ToData.ToJSON) and (LStore.Snapshot = LSource) and
      LPolicy.CapturePresentation.Preferences.HasDensity,
      'runtime-only admission preserves exact absent descriptor and accepted RAW source');
    Check(not NyxCollectionTablePresentation(LPeer).CapturePresentation.Preferences.HasDensity,
      'a same-store peer keeps independent runtime-only density and text choices');
    LRefused := False;
    try
      LPrepared := LPolicy.PreparePresentation(LCapture, NyxTablePresentationPreferences);
    except
      on ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'runtime-only publication retires the exact original capture');
    LNext := NewNyxCollectionView(LStore, NyxCollectionView(LKey)
      .Column(NyxIntegerField('Score'), 'Score').Column(NyxTextField(CCommand), 'Command'), cpTable);
    LPrepared := PrepareNyxCollectionColumnTransfer(LView, LNext);
    Check(LPrepared <> nil, 'same-store replacement prepares runtime-only preference transfer without a dummy saved policy');
    PublishNyxGroup([LPrepared]);
    Check(not LNext.Spec.HasPresentationPolicy and
      (NyxCollectionTablePresentation(LNext).CapturePresentation.Preferences.ColumnAt(0).Field.Name = CCommand) and
      (LNext.Spec.ColumnAt(0).FieldName = 'Score'),
      'runtime-only transfer follows full typed identity across reordered absent target columns');
    ConfigurePresentation(LNext, NyxTablePresentationPreferences);
    Check(not NyxCollectionTablePresentation(LNext).CapturePresentation.Preferences.HasDensity and
      (NyxCollectionTablePresentation(LNext).CapturePresentation.Preferences.Count = 0) and
      (LStore.Snapshot = LSource), 'runtime-only reset clears overrides without changing source or saved absence');
  finally
    LPrepared := nil;
    LCapture := nil;
    LPolicy := nil;
    LNext := nil;
    LPeer := nil;
    LView := nil;
    LStore := nil;
    LSource := nil;
  end;
end;

begin
  try
    AuthoredValues;
    WireRefusals;
    RuntimeWork;
    RuntimePreferences;
    DescriptorCases;
    PublicationCases;
    RuntimeOnlyPublication;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-table-presentation', 'passed');
    document.body.setAttribute('data-table-presentation-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed table presentation policy checks; no target paint claim');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-table-presentation', 'failed');
      document.body.setAttribute('data-table-presentation-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
