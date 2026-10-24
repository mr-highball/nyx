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
program nyx_collection_representation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, {$ifndef PAS2JS}Classes,{$endif}
  {$ifdef NYX_COMPILED_DISPLAY}nyx.generated.view,{$endif}
  nyx.text, nyx.bytes, nyx.state, nyx.contract, nyx.resources,
  nyx.data, nyx.times, nyx.numbers.decimal, nyx.formatting,
  nyx.collections, nyx.collections.arrangement, nyx.collections.clipboard,
  nyx.collections.representation
  , nyx.collections.view, nyx.collections.view.types,
  nyx.collections.query.editor, nyx.model, nyx.types, nyx.codec, nyx.codegen, nyx.source,
  nyx.studio.collectionintent, nyx.studio.collectionedits, nyx.studio.projects,
  nyx.studio.stateedits,
  nyx.studio.authoring
  , nyx.studio.session, nyx.studio.collections, nyx.controls, nyx.composition,
  nyx.programs.declarations, nyx.programs.contract
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;
  GGeneratedSource: TNyxText;
  GGeneratedDesign: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText); forward;

{$ifndef PAS2JS}
{ Optional caller-selected artifact directory keeps the exact accepted source
  available for independent compiler consumers; all bytes are explicit UTF-8. }
procedure SaveGeneratedSource;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin

  if ParamCount <> 1 then
  begin
    Exit;
  end;

  if not DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Generated display output needs an existing caller directory');
  end;
  LBytes := NyxEncodeUTF8(GGeneratedSource);
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

{$ifdef NYX_COMPILED_DISPLAY}
{ The ordinary compiler owns this builder. Comparing its actual result against
  the original accepted design is independent of the configuration reader. }
procedure CompiledDisplay;
var
  LDocument: TNyxDocument;
begin
  LDocument := BuildNyxDocument;
  try
    LDocument.Validate;
    Check(TNyxCodec.Encode(LDocument) = GGeneratedDesign,
      'independently compiled all-style generated source reconstructs the exact accepted design');
  finally
    LDocument.Free;
  end;
end;
{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function CountText(const AText, ANeedle: TNyxText): Integer;
var
  LIndex: Integer;
begin
  Result := 0;
  LIndex := 1;
  while LIndex <= Length(AText) - Length(ANeedle) + 1 do
  begin

    if System.Copy(AText, LIndex, Length(ANeedle)) = ANeedle then
    begin
      Inc(Result);
      Inc(LIndex, Length(ANeedle));
    end
    else
    begin
      Inc(LIndex);
    end;
  end;
end;

function HasImport(const ASource, AName: TNyxText): Boolean;
var
  LImports: TNyxImportClause;
  LIndex: Integer;
begin
  LImports := ReadNyxImports(ASource, nisInterface);
  for LIndex := 0 to LImports.Count - 1 do
  begin

    if LImports.UnitAt(LIndex).Name = AName then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure FormatPrimitives;
var
  LFormat: TNyxValueFormat;
  LLocale: TNyxFormatLocale;
  LBefore: TNyxText;
  LRefused: Boolean;
  LSettings: TFormatSettings;
begin
  LLocale := TNyxFormatLocale.Invariant.Named(NyxLocale('fr-FR'))
    .NumberSeparators(',', TNyxText(' '));
  LFormat := TNyxValueFormat.Canonical.Decimal(2).Localized(LLocale)
    .Identified(NyxFormatter('price 🌙'), 3);
  Check(LFormat.FormatValue(TNyxStateValue.FromNumber(1234.5), NyxNoDomain) =
    TNyxText('1 234,50'), 'explicit decimal/grouping symbols are exact and portable');
  Check(LFormat.FormatValue(TNyxStateValue.FromInteger(Low(Integer)), NyxNoDomain) =
    TNyxText('-2 147 483 648,00'), 'signed Integer minimum formats without overflow or binary conversion');
  Check(LFormat.Decimal(1, ndrToEven).FormatValue(TNyxStateValue.FromNumber(1.25),
    NyxNoDomain) = '1,2', 'exact decimal ties use the chosen even policy');
  Check(LFormat.Decimal(1, ndrAwayFromZero).FormatValue(TNyxStateValue.FromNumber(1.25),
    NyxNoDomain) = '1,3', 'explicit alternative rounding changes display only');
  Check(LFormat.Decimal(0, ndrFloor).FormatValue(TNyxStateValue.FromNumber(-1.25),
    NyxNoDomain) = '-2', 'directed rounding honors the original numeric sign');
  LBefore := LFormat.FormatValue(TNyxStateValue.FromNumber(1234.5), NyxNoDomain);
  LSettings := FormatSettings;
  try
    FormatSettings.DecimalSeparator := ';';
    FormatSettings.ThousandSeparator := ':';
    Check(LFormat.FormatValue(TNyxStateValue.FromNumber(1234.5), NyxNoDomain) = LBefore,
      'machine format settings cannot retarget an explicitly captured locale');
  finally
    FormatSettings := LSettings;
  end;
  LRefused := False;
  try
    LFormat.Decimal(2, ndrExact).FormatValue(TNyxStateValue.FromNumber(1.125), NyxNoDomain);
  except
    on E: ENyxDecimalPrecision do LRefused := True;
  end;
  Check(LRefused, 'exact display rounding refuses nonzero discarded digits');
  LRefused := False;
  try
    LFormat.FormatValue(TNyxStateValue.FromNumber(1E100), NyxNoDomain);
  except
    on E: ENyxDecimalNumber do LRefused := True;
  end;
  Check(LRefused, 'explicit decimal presentation refuses outside the exact existing decimal domain');
  Check(TNyxValueFormat.Canonical.FormatValue(TNyxStateValue.FromNumber(1E100), NyxNoDomain) =
    TNyxStateValue.FromNumber(1E100).NumberText,
    'raw canonical numbers keep their full finite domain independently of decimal display');
  LRefused := False;
  try
    LFormat.FormatValue(TNyxStateValue.FromText('1234.50'), NyxNoDomain);
  except
    on E: ENyxFormatting do LRefused := True;
  end;
  Check(LRefused, 'numeric-looking text never acquires numeric meaning by formatting');
  LFormat := TNyxValueFormat.Canonical.CalendarDate(nfdoDayMonthYear, '/');
  Check(LFormat.FormatValue(TNyxStateValue.FromText('2024-02-29'),
    NyxDateDomain.Definition) = '29/02/2024', 'calendar display reuses actual Gregorian date admission');
  Check(LFormat.FormatValue(TNyxStateValue.FromText(''), NyxDateDomain.Definition) = '',
    'optional empty date stays empty rather than becoming an epoch or current date');
  LRefused := False;
  try
    LFormat.FormatValue(TNyxStateValue.FromText('2024-02-29'), NyxNoDomain);
  except
    on E: ENyxFormatting do LRefused := True;
  end;
  Check(LRefused, 'date display requires the real typed date domain instead of guessing text');
  LRefused := False;
  try
    LFormat.FormatValue(TNyxStateValue.FromText('2023-02-29'), NyxDateDomain.Definition);
  except
    on E: Exception do LRefused := True;
  end;
  Check(LRefused, 'invalid calendar data cannot be prettified into apparent validity');
  LFormat := TNyxValueFormat.Canonical.ClockTime(ntpMillisecond).Localized(LLocale);
  Check(LFormat.FormatValue(TNyxStateValue.FromText('09:10:11.125'),
    NyxTimeDomain.Definition) = '09:10:11,125', 'clock display retains declared millisecond value and explicit locale fraction');
  LRefused := False;
  try
    LFormat.ClockTime(ntpMinute).FormatValue(TNyxStateValue.FromText('09:10:11.125'),
      NyxTimeDomain.Definition);
  except
    on E: Exception do LRefused := True;
  end;
  Check(LRefused, 'clock display cannot silently discard nonzero time parts');
  LFormat := TNyxValueFormat.Canonical.BooleanLabels('Oui 🌙', 'Non');
  Check((LFormat.FormatValue(TNyxStateValue.FromBoolean(True), NyxNoDomain) = TNyxText('Oui 🌙')) and
    (LFormat.FormatValue(TNyxStateValue.FromBoolean(False), NyxNoDomain) = 'Non'),
    'Boolean display labels remain explicit exact user text');
  LRefused := False;
  try
    LLocale.NumberSeparators(',', ',');
  except
    on E: ENyxFormatting do LRefused := True;
  end;
  Check(LRefused and (LLocale.DecimalSeparator = ',') and (LLocale.GroupSeparator = TNyxText(' ')),
    'invalid separator changes refuse atomically without changing the original locale');
end;

{ Paint/Fit expose expected display limits without broad exception swallowing.
  Explicit formatting remains strict and accepted RAW is copied independently. }
procedure PresentationRefusals;
var
  LPaint: TNyxValuePresentation;
  LValue: TNyxStateValue;
  LRefused: Boolean;
begin
  LPaint := NyxCanonicalPresentation(TNyxStateValue.FromText(TNyxText('raw 🌙') + #0 + #13#10));
  Check((LPaint.Outcome = nvpoCanonical) and (LPaint.Text = TNyxText('raw 🌙') + #0 + #13#10) and
    (LPaint.RawText = LPaint.Text) and (LPaint.Diagnostic = ''),
    'admitted canonical paint preserves exact Unicode/NUL/newlines without introducing a formatter');
  LValue := TNyxStateValue.FromNumber(1E100);
  LPaint := NyxValueFormat.Decimal(2).PresentValue(LValue, NyxNoDomain);
  Check((LPaint.Outcome = nvpoUnavailable) and (LPaint.Diagnostic <> '') and
    (LPaint.RawText = LValue.NumberText) and (Pos('[Display unavailable]', LPaint.Text) = 1),
    'finite unrestricted number outside decimal display has explicit status and exact RAW');
  LPaint := NyxValueFormat.Decimal(2, ndrExact).PresentValue(
    TNyxStateValue.FromNumber(1.125), NyxNoDomain);
  Check((LPaint.Outcome = nvpoUnavailable) and (LPaint.RawText = '1.125') and
    (NyxUTF8ByteCount(LPaint.Diagnostic) <= 1024),
    'exact rounding refusal is a bounded separate paint diagnostic without numeric admission changes');
  LPaint := NyxValueFormat.ClockTime(ntpMinute).PresentValue(
    TNyxStateValue.FromText('09:10:11'), NyxTimeDomain.Definition);
  Check((LPaint.Outcome = nvpoUnavailable) and (LPaint.RawText = '09:10:11') and
    (LPaint.Diagnostic <> ''), 'nonzero clock seconds remain accepted even when minute display refuses');
  LPaint := NyxValueFormat.TextAffixes('[', ']').PresentValue(
    TNyxStateValue.FromText(TNyxText('retained supplementary 🌙') + #0), NyxNoDomain,
    NyxUTF8ByteCount('[Display unavailable]'));
  Check((LPaint.Outcome = nvpoUnavailable) and (LPaint.Text = '[Display unavailable]') and
    (LPaint.RawText = TNyxText('retained supplementary 🌙') + #0),
    'bounded marker-only paint keeps full supplementary Unicode/NUL RAW without truncation');
  LPaint := NyxValueFormat.Decimal(2).PresentValue(TNyxStateValue.FromNumber(1.25), NyxNoDomain);
  Check((LPaint.Outcome = nvpoFormatted) and (LPaint.Text = '1.25') and
    (LPaint.Diagnostic = ''), 'successful format does not acquire an unavailable status');
  LPaint := NyxValueFormat.PresentValue(TNyxStateValue.FromText('raw'), NyxNoDomain);
  Check((LPaint.Outcome = nvpoCanonical) and (LPaint.Text = 'raw') and
    (LPaint.RawText = 'raw'), 'canonical paint remains explicitly distinguishable from formatted display');
  LRefused := False;
  try
    NyxValueFormat.CalendarDate(nfdoYearMonthDay).PresentValue(
      TNyxStateValue.FromText('2023-02-29'), NyxDateDomain.Definition);
  except
    on E: Exception do LRefused := True;
  end;
  Check(LRefused, 'invalid accepted domain data still fails outside the expected presentation catch');
end;

{ Real view sampling shares display facts with targets, while accepted CellText
  and query/edit/RAW producers retain their exact typed values. }
procedure ColumnDisplayPolicies;
var
  LKey: TNyxCollectionRef;
  LItem: TNyxItemRef;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LColumns: INyxCollectionColumns;
  LSampling: INyxCollectionColumnSampling;
  LSample: INyxCollectionColumnSample;
  LSchema: TNyxCollectionSchema;
  LSpec: TNyxCollectionViewSpec;
  LFormatted: TNyxCollectionViewSpec;
  LFormat: TNyxValueFormat;
  LData: TNyxDataValue;
  LBefore: TNyxText;
  LRefused: Boolean;
  LStyle: TNyxValueFormatStyle;
begin
  LKey := NyxCollection('display-policy');
  LItem := NyxItem(LKey, 'row 🌙');
  LSchema := NyxCollectionSchema.Number(NyxNumberField('Price'), 0)
    .Text(NyxTextField('Name'), '').Text(NyxTextField('Date'), '', NyxDateDomain);
  LStore := NewNyxCollection(LKey, LSchema, [NyxCollectionItem(LItem)
    .WithValue(NyxNumberField('Price'), 1234.5).WithValue(NyxTextField('Name'), 'raw 🌙')
    .WithValue(NyxTextField('Date'), '2024-02-29')]);
  LSpec := NyxCollectionView(LKey).Column(NyxNumberField('Price'), 'Price', cmEditable);
  LBefore := LSpec.ToData.ToJSON;
  LFormatted := LSpec.ColumnFormat(NyxValueFormat);
  Check(not LSpec.HasColumnFormat and LFormatted.HasColumnFormat and
    LFormatted.ColumnAt(0).HasFormatPolicy and
    (LFormatted.ToData.Field('version').AsInteger = 9),
    'explicit canonical display remains distinct from absent legacy policy');
  Check(LFormatted.UseDefaultColumnFormat.ToData.ToJSON = LBefore,
    'display default reset restores exact earlier descriptor bytes');
  LFormat := NyxValueFormat.Decimal(2).Localized(NyxFormatLocale
    .Named(NyxLocale('de-DE')).NumberSeparators(',', '.'))
    .Identified(NyxFormatter('Price display 🌙'), 2);
  LFormatted := LSpec.ColumnFormat(LFormat).Column(NyxTextField('Name'), 'Name', cmEditable)
    .ColumnFormat(NyxValueFormat.TextAffixes('[', ']')).TableLayout(NyxTableLayout);
  LData := LFormatted.ToData;
  Check(TNyxCollectionViewSpec.FromData(LData).ToData.ToJSON = LData.ToJSON,
    'v9 preserves nullable independent query/layout/sizing and exact display choices');
  Check((LFormatted.ColumnAt(0).Display(LStore.Snapshot.Item(LItem), LSchema) = '1.234,50') and
    (LFormatted.ColumnAt(1).Display(LStore.Snapshot.Item(LItem), LSchema) = TNyxText('[raw 🌙]')),
    'pure paint projection uses exact typed column and explicit locale facts');
  LView := NewNyxCollectionView(LStore, LFormatted, cpTable);
  Check((LView.CellText(LItem, 0) = '1234.5') and
    (LView.CellText(LItem, 1) = TNyxText('raw 🌙')),
    'display never replaces canonical editor/RAW scalar text');
  LColumns := NyxCollectionColumns(LView);
  Check(Supports(LView, INyxCollectionColumnSampling, LSampling),
    'actual admitted view exposes the optional detached Fit sample path');
  LSample := LSampling.CaptureColumnSample(LColumns.CaptureColumns,
    NyxColumnIdentity(NyxNumberField('Price')));
  Check((LSample.TextAt(0) = '1.234,50') and
    (LSample.Column.FormatPolicy.ToData.ToJSON = LFormat.ToData.ToJSON),
    'actual Fit sampling copies the same display text and original formatter policy');
  LView.Edit(LItem, 0, TNyxStateValue.FromNumber(2000.25));
  Check((LView.CellText(LItem, 0) = '2000.25') and (LSample.TextAt(0) = '1.234,50'),
    'accepted edit remains typed raw while retained sample cannot follow a newer scalar');
  LRefused := False;
  try
    LSpec.ColumnFormat(NyxValueFormat.BooleanLabels('Yes', 'No'));
  except
    on E: ENyxFormatting do LRefused := True;
  end;
  Check(LRefused and (LSpec.ToData.ToJSON = LBefore),
    'cross-family display refuses atomically before a column copy is changed');
  LRefused := False;
  try
    LView := NewNyxCollectionView(LStore, NyxCollectionView(LKey)
      .Column(NyxTextField('Name'), 'Name').ColumnFormat(NyxValueFormat.CalendarDate(nfdoYearMonthDay)), cpTable);
  except
    on E: ENyxFormatting do LRefused := True;
  end;
  Check(LRefused, 'date-looking ordinary text cannot acquire a date formatter without its domain');
  for LStyle := Low(TNyxValueFormatStyle) to High(TNyxValueFormatStyle) do
  begin
    LFormat := NyxValueFormat;
    case LStyle of
      nvfsCanonical: begin end;
      nvfsDecimal: LFormat := LFormat.Decimal(3, ndrCeiling);
      nvfsCalendarDate: LFormat := LFormat.CalendarDate(nfdoMonthDayYear, '/');
      nvfsClockTime: LFormat := LFormat.ClockTime(ntpMillisecond);
      nvfsBooleanLabels: LFormat := LFormat.BooleanLabels('Oui 🌙', 'Non');
      nvfsTextAffixes: LFormat := LFormat.TextAffixes('🌙' + #0, #10 + 'end');
    end;
    LFormat := LFormat.Identified(NyxFormatter('format 🌙'), 7)
      .Localized(NyxFormatLocale.Named(NyxLocale('fr-FR')).NumberSeparators(',', TNyxText(' ')));
    Check(TNyxValueFormat.FromData(LFormat.ToData).ToData.ToJSON = LFormat.ToData.ToJSON,
      'each closed display style persists exact identity, locale and active rules');
  end;
  LFormat := NyxValueFormat.TextAffixes('old', 'rules').Decimal(2);
  Check(LFormat.ToData.ToJSON = NyxValueFormat.Decimal(2).ToData.ToJSON,
    'style replacement has no hidden inactive parameters that source replay could lose');
end;

{ Actual paired Studio admission exercises generated-source import promotion,
  immutable intent baselines and one Undo/Redo for a complete display policy. }
procedure DisplayAuthoring;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LTable: TNyxNode;
  LPage: TNyxNode;
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LSession: TNyxStudioSession;
  LKey: TNyxCollectionRef;
  LSpec: TNyxCollectionViewSpec;
  LPair: TNyxProjectPair;
  LVariants: TNyxCollectionViewSpec;
  LSource: TNyxText;
  LBaseline: TNyxText;
  LAccepted: TNyxText;
  LFormat: TNyxValueFormat;
  LIntent: TNyxStudioCollectionIntent;
  LEdit: TNyxStudioDesignEdit;
  LRefused: Boolean;
  LShape: TNyxDataValue;
begin
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  LSession := nil;
  LRoot := nil;
  LProjection := nil;
  try
    LKey := NyxCollection('authored-display');
    LDocument.Programs.Define(NyxProgramRef('Uninvoked display companion'),
      NewNyxProgramDefinition('begin end.', NyxProgramContract));
    LDocument.Collections.Define(LKey, NyxCollectionSchema.Number(NyxNumberField('Price'), 0)
      .Text(NyxTextField('Date'), '', NyxDateDomain)
      .Text(NyxTextField('Clock'), '', NyxTimeDomain)
      .Text(NyxTextField('Notes'), '').Boolean(NyxBooleanField('Active'), False)
      .Integer(NyxIntegerField('Count'), 0),
      [NyxCollectionItem(NyxItem(LKey, 'one')).WithValue(NyxNumberField('Price'), 1234.5)]);
    LPage := TNyxNode.Create(nkPage, 'display-page');
    LDocument.AddPage(LPage);
    LTable := TNyxNode.Create(nkTable, 'display-table');
    LPage.Add(LTable);
    LSpec := NyxCollectionView(LKey).Column(NyxNumberField('Price'), 'Price', cmEditable);
    LTable.SetCollectionView(LSpec);
    LSource := TNyxText('{ retained author prefix 🌙 }') + #10 + LWorkspace.Render(LDocument) +
      #10 + TNyxText('{ retained author suffix 漢字 }');
    LWorkspace.Accept(LDocument, LSource);
    LFormat := NyxValueFormat.Decimal(2, ndrToEven).Identified(NyxFormatter('price 🌙'), 5)
      .Localized(NyxFormatLocale.Named(NyxLocale('de-DE')).NumberSeparators(',', '.'));
    LTable.SetCollectionView(LSpec.ColumnFormat(LFormat));
    LSource := LWorkspace.Render(LDocument);
    Check(HasImport(LSource, 'nyx.formatting') and
      (Pos('.ColumnFormat(NyxValueFormat.Decimal(2, ndrToEven)', LSource) > 0) and
      (Pos('{ retained author prefix 🌙 }', LSource) > 0) and
      (Pos('{ retained author suffix 漢字 }', LSource) > 0),
      'new display authoring promotes the actual required import without losing handwritten Unicode frames');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
      'generated typed formatting reconstructs exact policy/locale/presence through the public source reader');
    FreeAndNil(LCandidate);
    Check(LWorkspace.Render(LDocument) = LSource, 'display source synchronization is idempotent');
    LVariants := LTable.CollectionView
      .Column(NyxTextField('Date'), 'Date').ColumnFormat(NyxValueFormat.CalendarDate(nfdoMonthDayYear, '/'))
      .Column(NyxTextField('Clock'), 'Clock').ColumnFormat(NyxValueFormat.ClockTime(ntpMillisecond))
      .Column(NyxTextField('Notes'), 'Notes').ColumnFormat(NyxValueFormat.TextAffixes(TNyxText('🌙') + #0, #10 + 'end'))
      .Column(NyxBooleanField('Active'), 'Active').ColumnFormat(NyxValueFormat.BooleanLabels('Oui 🌙', 'Non'))
      .Column(NyxIntegerField('Count'), 'Count').ColumnFormat(NyxValueFormat);
    LTable.SetCollectionView(LVariants);
    Check(CountText(TNyxCodegen.Generate(LDocument), '  nyx.resources,') = 1,
      'formatting and independent program registries share one actual generated resource import');
    LSource := LWorkspace.Render(LDocument);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
      'all six typed display styles replay with Unicode/NUL/newlines and exact declared time/date domains');
    FreeAndNil(LCandidate);
    GGeneratedSource := LSource;
    GGeneratedDesign := TNyxCodec.Encode(LDocument);
    LTable.SetCollectionView(LSpec.ColumnFormat(LFormat));
    LSource := LWorkspace.Render(LDocument);
    LPair.Design := TNyxCodec.Encode(LDocument);
    LPair.Source := LSource;
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('display-table');
    LAccepted := LSession.Source;
    LBaseline := TNyxCodec.Encode(LSession.Document);
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaColumnFormat;
    LIntent.Key := LKey;
    LIntent.Projection := cpTable;
    LIntent.Field := TNyxStudioCollectionFieldRef.Number(NyxNumberField('Price'));
    LIntent.LayoutBaseline := NyxQueryEditorBaseline(LSession.Document.Collections.Snapshot(LKey).Schema,
      LSession.Document.Find('display-table').CollectionView);
    LIntent.ColumnFormat := LFormat.Decimal(3, ndrFloor);
    LShape := LIntent.ToData;
    Check(TNyxStudioCollectionIntent.FromData(LShape).SameIntent(LIntent),
      'append-only ordinary display intent retains exact complete policy and binding baseline');
    LSession.ApplyCollectionIntent(LIntent);
    Check((LSession.Document.Find('display-table').CollectionView.ColumnAt(0).FormatPolicy.Scale = 3) and
      (Pos('.Decimal(3, ndrFloor)', LSession.Source) > 0),
      'one complete display intent publishes both authored design and typed Pascal');
    LSession.Undo;
    Check((TNyxCodec.Encode(LSession.Document) = LBaseline) and (LSession.Source = LAccepted),
      'one Undo restores the exact accepted design/source pair');
    LSession.Redo;
    Check(LSession.Document.Find('display-table').CollectionView.ColumnAt(0).FormatPolicy.Scale = 3,
      'one Redo restores the complete display choice');
    LRoot := TNyxNode.Create(nkPage, 'display-inspector');
    LProjection := LSession.SelectedProjection;
    AddNyxCollectionBindingPanel(LRoot, LSession, LProjection);
    FreeAndNil(LProjection);
    Check((LRoot.Find(NyxStudioFormatPolicyApplyID(0)) <> nil) and
      (LRoot.Find(NyxStudioFormatPolicyFieldID(0, sfpfScale)).Prop('value') = '3'),
      'ordinary Nyx inspector exposes actual typed display form and accepted scalar choices');
    LRoot.Find(NyxStudioFormatPolicyFieldID(0, sfpfScale)).Configure.Value('-').Done;
    LRefused := False;
    LAccepted := LSession.Source;
    try
      CaptureNyxStudioFormat(LSession, LRoot.Find(NyxStudioFormatPolicyApplyID(0)), LRoot,
        Default(TNyxStudioPendingDesign), LEdit);
    except
      on E: ENyxCollection do LRefused := True;
    end;
    Check(LRefused and (LSession.Source = LAccepted) and
      (LRoot.Find(NyxStudioFormatPolicyFieldID(0, sfpfScale)).Prop('value') = '-'),
      'incomplete decimal notation remains an exact inert form draft without partial history/source admission');
    Check(CaptureNyxStudioFormat(LSession, LRoot.Find(NyxStudioFormatPolicyDefaultID(0)), LRoot,
      Default(TNyxStudioPendingDesign), LEdit), 'default action captures without parsing an unrelated unfinished parameter');
    LSession.ApplyCollectionIntent(LEdit.Collection);
    Check(not LSession.Document.Find('display-table').CollectionView.HasColumnFormat and
      (LSession.Document.Find('display-table').CollectionView.ToData.Field('version').AsInteger = 1),
      'inspector reset clears only explicit display presence and restores legacy descriptor');
    LSession.Undo;
    Check(LSession.Document.Find('display-table').CollectionView.ColumnAt(0).FormatPolicy.Scale = 3,
      'display reset is one independently undoable paired operation');
    LRefused := False;
    try
      LSession.ApplyCollectionIntent(LIntent);
    except
      on E: ENyxCollection do LRefused := True;
    end;
    Check(LRefused, 'an earlier complete display form baseline cannot overwrite a newer format');
  finally
    LProjection.Free;
    LRoot.Free;
    LSession.Free;
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

{ Reusable property overrides preserve raw declaration ownership. Clearing a
  policy is not a whole-binding inherit, and a binding mask cannot be rebound
  accidentally by applying a display default. }
procedure DisplayInheritance;
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LRecipe: INyxColumn;
  LPage: INyxPage;
  LInstance: INyxComponent;
  LTable: TNyxNode;
  LOverride: TNyxNode;
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LKey: TNyxCollectionRef;
  LSpec: TNyxCollectionViewSpec;
  LPair: TNyxProjectPair;
  LIntent: TNyxStudioCollectionIntent;
  LDefinition: TNyxText;
  LRefused: Boolean;
begin
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LSession := nil;
  LRoot := nil;
  LProjection := nil;
  try
    LKey := NyxCollection('reusable-display');
    LDocument.Collections.Define(LKey, NyxCollectionSchema.Number(NyxNumberField('Price'), 0), []);
    LSpec := NyxCollectionView(LKey).Column(NyxNumberField('Price'), 'Price', cmEditable)
      .ColumnFormat(NyxValueFormat.Decimal(2));
    LRecipe := NewNyxColumn('display-recipe');
    LTable := TNyxNode.Create(nkTable, 'definition-table');
    LTable.Configure.PartName(NyxPart('records')).Done;
    LTable.SetCollectionView(LSpec);
    LRecipe.Node.Add(LTable);
    LDocument.AddComponent(LRecipe);
    LPage := NewNyxPage('reusable-display-page');
    LInstance := NewNyxComponent('first-display');
    LInstance.Configure.Component(NyxComponent('display-recipe')).Done;
    LOverride := LInstance.Node.OverridePart(NyxPart('records'), noProperties);
    LOverride.Named('display-override');
    LOverride.SetCollectionView(LSpec.ColumnFormat(NyxValueFormat));
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second-display');
    LInstance.Configure.Component(NyxComponent('display-recipe')).Done;
    LPage.Add(LInstance);
    LDocument.AddPage(LPage);
    LDocument.Validate;
    LDefinition := LDocument.Find('definition-table').CollectionView.ToData.ToJSON;
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    Check((LOverride.Kind = NyxKindName(nkSlotOverride)) and
      (LRoot.Children[0].Part(NyxPart('records')).CollectionView.ColumnAt(0).FormatPolicy.Style = nvfsCanonical) and
      (LRoot.Children[1].Part(NyxPart('records')).CollectionView.ColumnAt(0).FormatPolicy.Style = nvfsDecimal),
      'actual reusable composition distinguishes local explicit canonical display from inherited decimal policy');
    FreeAndNil(LRoot);
    LPair.Design := TNyxCodec.Encode(LDocument);
    LPair.Source := LWorkspace.Render(LDocument);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('display-override');
    LProjection := LSession.SelectedProjection;
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaDefaultColumnFormat;
    LIntent.Key := LKey;
    LIntent.Field := TNyxStudioCollectionFieldRef.Number(NyxNumberField('Price'));
    LIntent.Projection := cpTable;
    LIntent.LayoutBaseline := NyxQueryEditorBaseline(LSession.Document.Collections.Snapshot(LKey).Schema,
      LProjection.CollectionView);
    FreeAndNil(LProjection);
    LSession.ApplyCollectionIntent(LIntent);
    Check(LSession.Document.Find('display-override').HasCollectionView and
      not LSession.Document.Find('display-override').CollectionView.HasColumnFormat and
      (LSession.Document.Find('definition-table').CollectionView.ToData.ToJSON = LDefinition),
      'display reset keeps a local complete binding and leaves the reusable default unchanged');
    LSession.SetCollectionView(Default(TNyxCollectionViewSpec));
    LProjection := LSession.SelectedProjection;
    Check(LSession.Document.Find('display-override').HasCollectionView and
      not LSession.Document.Find('display-override').CollectionView.Defined and
      not LProjection.CollectionView.Defined, 'whole-binding clear is an explicit reusable mask');
    FreeAndNil(LProjection);
    LRefused := False;
    try
      NyxCollectionPatch([NyxUseDefaultCollectionColumnFormat(NyxBindingOwner('display-override'),
        LKey, cpTable, LIntent.Field)]).Candidate(LSession.ProjectSnapshot);
    except
      on E: ENyxCollection do LRefused := True;
    end;
    Check(LRefused, 'display default cannot silently rebind a masked reusable control');
    LSession.InheritCollectionView;
    LProjection := LSession.SelectedProjection;
    Check(not LSession.Document.Find('display-override').HasCollectionView and
      (LProjection.CollectionView.ColumnAt(0).FormatPolicy.Scale = 2) and
      (LSession.Document.Find('definition-table').CollectionView.ToData.ToJSON = LDefinition),
      'explicit inheritance restores the actual definition format while preserving its owner');
    FreeAndNil(LProjection);
    LSession.Undo;
    Check(LSession.Document.Find('display-override').HasCollectionView and
      not LSession.Document.Find('display-override').CollectionView.Defined,
      'one Undo of inheritance restores the exact mask');
  finally
    LProjection.Free;
    LRoot.Free;
    LSession.Free;
    LWorkspace.Free;
    LInstance := nil;
    LPage := nil;
    LRecipe := nil;
    LDocument.Free;
  end;
end;

procedure CellRepresentations;
var
  LStore: INyxCollection;
  LSource: INyxCollectionSnapshot;
  LKey: TNyxCollectionRef;
  LItem: TNyxItemRef;
  LRaw: INyxCollectionClipboardText;
  LRawText: INyxCollectionCellText;
  LDisplay: INyxCollectionCellText;
  LFormat: TNyxValueFormat;
  LCopy: TNyxValueFormat;
  LValue: TNyxStateValue;
  LRefused: Boolean;
  LText: TNyxText;
begin
  LKey := NyxCollection('representations');
  LItem := NyxItem(LKey, 'row 🌙');
  LText := 'full 🌙' + #0 + #13#10 + 'raw';
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField('Name'), '')
    .Number(NyxNumberField('Price'), 0), [NyxCollectionItem(LItem)
      .WithValue(NyxTextField('Name'), LText).WithValue(NyxNumberField('Price'), 1234.5)]);
  LSource := LStore.Snapshot;
  LRaw := NewNyxCollectionClipboardCell(LSource, LItem, NyxColumnIdentity(NyxTextField('Name')),
    TNyxCollectionClipboardLimits.Defaults);
  LRawText := NewNyxCollectionCellText(LRaw);
  LRawText := CopyNyxCollectionCellText(LRawText);
  Check((LRawText.Representation = nccrAcceptedRaw) and not LRawText.HasFormatter and
    (LRawText.Text = LText) and (LRawText.RawText = LText) and
    (LRawText.ByteCount = NyxUTF8ByteCount(LText)),
    'default representation is exact accepted RAW including supplementary Unicode/NUL/newlines');
  LFormat := TNyxValueFormat.Canonical.TextAffixes('[', ']')
    .Identified(NyxFormatter('inspect text'), 2);
  LDisplay := NewNyxCollectionCellDisplay(LRaw, LFormat, NyxNoDomain);
  LDisplay := CopyNyxCollectionCellText(LDisplay);
  Check((LDisplay.Representation = nccrFormattedDisplay) and
    (LDisplay.Text = '[' + LText + ']') and (LDisplay.RawText = LText) and
    (LDisplay.Value.TextValue = LText), 'display output is tagged and never overwrites accepted RAW provenance');
  Check((LDisplay.Item.Collection.Name = LKey.Name) and (LDisplay.Item.ID = LItem.ID) and
    LDisplay.Field.Same(NyxColumnIdentity(NyxTextField('Name'))) and
    (LDisplay.Revision = 0), 'display retains exact scoped row/full typed field/source revision');
  LRefused := False;
  try
    CopyNyxCollectionCellText(LDisplay, NyxUTF8ByteCount(LText));
  except
    on E: ENyxCollectionRepresentation do LRefused := True;
  end;
  Check(LRefused and (LDisplay.Text = '[' + LText + ']') and (LRawText.Text = LText),
    'foreign-value normalization admits actual representation bytes before any output authority');
  LRefused := False;
  try
    NewNyxCollectionCellDisplay(LRaw, LFormat, NyxNoDomain, NyxUTF8ByteCount(LText));
  except
    on E: ENyxFormatting do LRefused := True;
  end;
  Check(LRefused and (LRaw.Text = LText) and (LRawText.Text = LText),
    'expanded output budget refusal leaves every prior RAW/display snapshot unchanged');
  LRaw := NewNyxCollectionClipboardCell(LSource, LItem, NyxColumnIdentity(NyxNumberField('Price')),
    TNyxCollectionClipboardLimits.Defaults);
  LFormat := TNyxValueFormat.Canonical.Decimal(2)
    .Localized(TNyxFormatLocale.Invariant.Named(NyxLocale('de-DE')).NumberSeparators(',', '.'))
    .Identified(NyxFormatter('price'), 7);
  LDisplay := NewNyxCollectionCellDisplay(LRaw, LFormat, NyxNoDomain);
  LFormat := LFormat.Decimal(0).Localized(TNyxFormatLocale.Invariant)
    .Identified(NyxFormatter('changed later'), 8);
  Check((LDisplay.Text = '1.234,50') and (LDisplay.RawText = '1234.5') and
    (LDisplay.Value.NumberValue = 1234.5), 'later formatter changes cannot retarget already captured output');
  LCopy := LDisplay.Formatter;
  Check((LCopy.Identity.Name = 'price') and (LCopy.Revision = 7) and
    (LCopy.Locale.Locale.Name = 'de-DE') and (LCopy.Locale.DecimalSeparator = ',') and
    (LCopy.Style = nvfsDecimal) and (LCopy.Scale = 2),
    'complete original formatter and locale identity/facts accompany display text');
  LCopy := LCopy.Identified(NyxFormatter('caller copy'), 1).Decimal(3);
  LValue := LDisplay.Value;
  LValue := TNyxStateValue.FromNumber(0);
  Check((LDisplay.Formatter.Revision = 7) and (LDisplay.Formatter.Scale = 2) and
    (LDisplay.Value.NumberValue = 1234.5) and (LCopy.Scale = 3) and
    (LValue.NumberValue = 0), 'returned formatter/scalar copies cannot mutate immutable display facts');
  LRefused := False;
  try
    LCopy := LRawText.Formatter;
  except
    on E: ENyxCollectionRepresentation do LRefused := True;
  end;
  Check(LRefused and (LRawText.Representation = nccrAcceptedRaw),
    'RAW has no invented display formatter or active-draft tag');
  LRaw := nil;
  LSource := nil;
  LStore := nil;
  Check((LDisplay.Text = '1.234,50') and (LDisplay.Item.ID = LItem.ID) and
    (LRawText.Text = LText), 'copied representation facts remain usable after actual source owner release');
end;

begin
  try
    FormatPrimitives;
    PresentationRefusals;
    CellRepresentations;
    ColumnDisplayPolicies;
    DisplayAuthoring;
    DisplayInheritance;
    {$ifdef NYX_COMPILED_DISPLAY}CompiledDisplay;{$endif}
    {$ifndef PAS2JS}SaveGeneratedSource;{$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-cell-representation-tests', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' explicit cell representation checks';
    {$else}
    WriteLn('PASS ', GChecks, ' explicit cell representation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-cell-representation-error', E.Message);
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
