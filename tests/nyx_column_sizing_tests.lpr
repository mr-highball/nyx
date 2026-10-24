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
program nyx_column_sizing_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.codec,
  nyx.codegen, nyx.source, nyx.collections, nyx.collections.view.types,
  nyx.collections.selection, nyx.collections.query, nyx.typeahead,
  nyx.collections.columns, nyx.layout.flow, nyx.composition, nyx.controls,
  nyx.responsive,
  { BEGIN COLUMN ARRANGEMENT IMPORTS }
  nyx.collections.arrangement, nyx.collections.query.policy,
  nyx.collections.arrangement.controls, nyx.collections.view,
  nyx.collections.paging, nyx.layout.constraints, nyx.state,
  { END COLUMN ARRANGEMENT IMPORTS }
  nyx.studio.collectionintent, nyx.studio.collectionedits,
  nyx.studio.session, nyx.studio.collections, nyx.studio.authoring,
  nyx.studio.stateedits, nyx.studio.projects
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ BEGIN COLUMN ARRANGEMENT CASES }
{ Pure contract and authored-descriptor checks belong in the existing sizing
  consumer. These tests own their inputs; no live view, widget or store is used
  to manufacture runtime preferences or claim physical pinning behavior. }
function ArrangementMember(const AData: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  I: Integer;
  LFound: Boolean;
begin
  SetLength(LFields, AData.Count + 1);
  LFound := False;
  for I := 0 to AData.Count - 1 do
  begin
    LFields[I] := NyxField(AData.Key(I), AData.Field(AData.Key(I)));

    if AData.Key(I) = AName then
    begin
      LFields[I] := NyxField(AName, AValue);
      LFound := True;
    end;
  end;

  if LFound then
  begin
    SetLength(LFields, AData.Count);
  end
  else
  begin
    LFields[AData.Count()] := NyxField(AName, AValue);
  end;
  Result := NyxObject(LFields);
end;

procedure ArrangementRefuses(AKind: Integer; const AData: TNyxDataValue;
  const AReason: TNyxText);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    case AKind of
      0: TNyxColumnIdentity.FromData(AData);
      1: TNyxColumnLayoutPolicy.FromData(AData);
      2: TNyxTableLayoutPolicy.FromData(AData);
      3: TNyxColumnPreference.FromData(AData);
      4: TNyxColumnPreferences.FromData(AData);
      5: TNyxCollectionViewSpec.FromData(AData);
    else
      raise Exception.Create('Invalid arrangement test decoder');
    end;
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason);
end;

procedure ArrangementSourceRefuses(AWorkspace: TNyxSourceWorkspace;
  ADocument: TNyxDocument; const ASource, AReason: TNyxText);
var
  LCandidate: TNyxDocument;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LCandidate := nil;
  LBefore := TNyxCodec.Encode(ADocument);
  LRefused := False;
  try
    try
      LCandidate := AWorkspace.Candidate(ADocument, ASource);
    except
      on Exception do LRefused := True;
    end;
    Check(LRefused and (TNyxCodec.Encode(ADocument) = LBefore), AReason);
  finally
    LCandidate.Free;
  end;
end;

{ Promote the same layout-free document's accepted handwritten frame. Fresh
  generation already owns its imports; only this path exercises dependencies
  introduced after a user has retained application code outside the builder. }
procedure ArrangementAuthoredSourceCases(AWorkspace: TNyxSourceWorkspace;
  ADocument: TNyxDocument; ATable: TNyxNode;
  const ASpec: TNyxCollectionViewSpec);
const
  CImportNote: TNyxText = 'SysUtils { Keep authored import order / 🌙 }';
  CPrefixNote: TNyxText = '{ Authored layout application / 🌙 / 漢字 }';
  CDeclaration: TNyxText = 'function LayoutApplicationNote: TNyxText;';
  CHelper: TNyxText = '// Keep this application helper outside the builder.' + #10 +
    'function LayoutApplicationNote: TNyxText;' + #10 +
    'begin' + #10 +
    '  Result := ''Column notes / 🌙 / 漢字'';' + #10 +
    'end;';
var
  LSource: TNyxText;
  LBaselineSource: TNyxText;
  LBaselineWire: TNyxText;
  LChanged: TNyxText;
  LPrefix: TNyxText;
  LBody: TNyxText;
  LSuffix: TNyxText;
  LCheckpoint: TNyxSourceCheckpoint;
  LCandidate: TNyxDocument;

  function ReplaceOnce(const ASource, ABefore, AAfter: TNyxText): TNyxText;
  var
    LPosition: Integer;
  begin
    LPosition := Pos(ABefore, ASource);

    if LPosition = 0 then
    begin
      raise Exception.Create('Authored layout fixture requires its exact source boundary');
    end;
    Result := Copy(ASource, 1, LPosition - 1) + AAfter +
      Copy(ASource, LPosition + Length(ABefore), MaxInt);
  end;

  function ArrangementImports(const ASource: TNyxText): Integer;
  var
    LImports: TNyxImportClause;
    LIndex: Integer;
  begin
    Result := 0;
    LImports := ReadNyxImports(ASource, nisInterface);
    for LIndex := 0 to LImports.Count - 1 do
    begin

      if SameText(LImports.UnitAt(LIndex).Name, 'nyx.collections.arrangement') then
      begin
        Inc(Result);
      end;
    end;
  end;

  procedure RetainsAuthoredFrame(const ASource, AReason: TNyxText);
  var
    LWithoutImport: TNyxText;
    LActualPrefix: TNyxText;
    LActualBody: TNyxText;
    LActualSuffix: TNyxText;
  begin
    { Remove only the newly required dependency through the public lexical edit
      boundary, then compare every other prefix/suffix byte, including comments
      and the complete import order. The managed body may legitimately change. }
    LWithoutImport := EditNyxImport(ASource, nisInterface, niaRemove,
      NyxPascalUnit('nyx.collections.arrangement'));
    SplitNyxSourceFrame(LWithoutImport, LActualPrefix, LActualBody, LActualSuffix);
    Check((LActualPrefix = LPrefix) and (LActualSuffix = LSuffix) and
      (LActualBody <> ''), AReason);
  end;

begin
  LCandidate := nil;
  LSource := AWorkspace.Render(ADocument);
  Check(not ATable.CollectionView.HasLayoutControls,
    'authored-frame promotion starts before the first saved layout policy');
  Check(ArrangementImports(LSource) = 0,
    'the older accepted companion has no arrangement dependency');
  LSource := EditNyxImport(LSource, nisInterface, niaAdd, NyxPascalUnit('SysUtils'));
  LSource := ReplaceOnce(LSource, 'SysUtils', CImportNote);
  LSource := CPrefixNote + #10 + ReplaceOnce(LSource, 'implementation' + #10,
    CDeclaration + #10 + #10 + 'implementation' + #10);
  LSource := ReplaceOnce(LSource, NyxViewsEnd + #10,
    NyxViewsEnd + #10 + #10 + CHelper + #10);
  try
    LCandidate := AWorkspace.Candidate(ADocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(ADocument),
      'the layout-free handwritten helper admits without changing design meaning');
    FreeAndNil(LCandidate);
    AWorkspace.Accept(ADocument, LSource);
    LBaselineSource := LSource;
    LBaselineWire := AWorkspace.Snapshot;
    LCheckpoint := AWorkspace.Capture(ADocument);
    SplitNyxSourceFrame(LSource, LPrefix, LBody, LSuffix);
    Check((Pos(CPrefixNote, LPrefix) > 0) and (Pos(CImportNote, LPrefix) > 0) and
      (Pos(CDeclaration, LPrefix) > 0) and (Pos(CHelper, LSuffix) > 0) and (LBody <> ''),
      'the accepted custom frame contains actual declared Unicode application code');

    ATable.SetCollectionView(ASpec.UseDefaultColumnLayout);
    LSource := AWorkspace.Render(ADocument);
    Check((ArrangementImports(LSource) = 1) and
      (Pos('.TableLayout(NyxTableLayout', LSource) > 0) and
      (Pos('.ColumnLayout(', LSource) = 0),
      'first table-only layout promotion adds exactly one owning import');
    RetainsAuthoredFrame(LSource,
      'table layout promotion retains exact handwritten imports comments and helper frames');
    ATable.SetCollectionView(ASpec.UseDefaultColumnLayout
      .TableLayout(ASpec.TableLayoutPolicy.AutoSizeSample(18)));
    LSource := AWorkspace.Render(ADocument);
    Check((ArrangementImports(LSource) = 1) and
      (Pos('.AutoSizeSample(18)', LSource) > 0) and
      (AWorkspace.Render(ADocument) = LSource),
      'later layout edits and no-op rendering never duplicate the required import');
    RetainsAuthoredFrame(LSource,
      'later table edits still retain every unrelated frame byte');
    ATable.SetCollectionView(ASpec.UseDefaultColumnLayout.UseDefaultTableLayout);
    LSource := AWorkspace.Render(ADocument);
    Check((ArrangementImports(LSource) = 1) and
      not ATable.CollectionView.HasLayoutControls,
      'reset keeps the promoted dependency in a handwritten frame for application helpers');
    RetainsAuthoredFrame(LSource, 'reset preserves the complete handwritten application frame');
    AWorkspace.Restore(LCheckpoint);
    Check((AWorkspace.Render(ADocument) = LBaselineSource) and
      (AWorkspace.Snapshot = LBaselineWire),
      'restoring the old paired checkpoint restores its exact layout-free imports and source');

    ATable.SetCollectionView(ASpec.UseDefaultTableLayout);
    LSource := AWorkspace.Render(ADocument);
    Check((ArrangementImports(LSource) = 1) and
      (Pos('.ColumnLayout(NyxColumnLayout', LSource) > 0) and
      (Pos('.TableLayout(', LSource) = 0),
      'first column-only layout promotion independently adds the owning import');
    RetainsAuthoredFrame(LSource,
      'column-only promotion preserves the same original handwritten frame');
    LSource := ReplaceOnce(LSource, 'nyx.collections.arrangement',
      'NyX.CoLlEcTiOnS.ArRaNgEmEnT');
    AWorkspace.Accept(ADocument, LSource);
    ATable.SetCollectionView(ASpec.UseDefaultTableLayout.TableLayout(NyxTableLayout));
    LSource := AWorkspace.Render(ADocument);
    Check((ArrangementImports(LSource) = 1) and
      (Pos('NyX.CoLlEcTiOnS.ArRaNgEmEnT', LSource) > 0),
      'an existing case-insensitive authored import is retained rather than appended again');
    RetainsAuthoredFrame(LSource,
      'case-insensitive import reuse retains the exact unrelated prefix and suffix');
    LChanged := AWorkspace.Snapshot;
    AWorkspace.Reset;
    AWorkspace.Restore(LChanged);
    Check(AWorkspace.Render(ADocument) = LSource,
      'wire recovery retains the promoted typed companion exactly');
    LCandidate := AWorkspace.Candidate(ADocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(ADocument),
      'the promoted companion reconstructs the exact complete saved policy after recovery');
  finally
    LCandidate.Free;
  end;
end;

procedure ArrangementSourceCases(const ASpec: TNyxCollectionViewSpec);
const
  CTableExpression = '.TableLayout(NyxTableLayout.Enabled(False).AllowOnly([nclfResize])' +
    '.MinimumCenter(144).AutoSizeSample(17))';
var
  LDocument, LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSourceSpec: TNyxCollectionViewSpec;
  LPage, LTable: TNyxNode;
  LSource, LBefore, LChanged: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    { The combined descriptor case above deliberately retains typeahead.
      Its table projection does not admit that list/tree behavior; source
      roundtrip uses an explicit valid table choice without changing ASpec. }
    LSourceSpec := ASpec.UseDefaultTypeAhead;
    LDocument.Collections.Define(ASpec.Key,
      NyxCollectionSchema.Text(NyxTextField('name'), '').Integer(NyxIntegerField('pid'), 0), []);
    LPage := TNyxNode.Create(nkPage, 'layout-page');
    LDocument.AddPage(LPage);
    LTable := TNyxNode.Create(nkTable, 'layout-table');
    LPage.Add(LTable);
    LTable.SetCollectionView(LSourceSpec);
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := LWorkspace.Render(LDocument);
    Check((Pos('nyx.collections.arrangement,', LSource) > 0) and
      (Pos('.ColumnLayout(NyxColumnLayout.Visible(False).PinStart()', LSource) > 0) and
      (Pos('.AllowOnly([nclfVisibility, nclfPinning])', LSource) > 0) and
      (Pos(CTableExpression, LSource) > 0),
      'generated source spells typed logical pins features bounds and owning import');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LBefore,
      'public Pascal reader reconstructs complete combined v8 authored meaning');
    FreeAndNil(LCandidate);
    LChanged := StringReplace(LSource, '.PinStart()', '.Pinned(ncpStart)', []);
    Check(LChanged <> LSource, 'typed pin alias test changes the emitted expression');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check(TNyxCodec.Encode(LCandidate) = LBefore,
      'typed Pinned enum alias has the same persisted meaning as PinStart');
    FreeAndNil(LCandidate);
    LChanged := StringReplace(LSource, CTableExpression,
      CTableExpression + '.UseDefaultColumnLayout().UseDefaultTableLayout()', []);
    Check(LChanged <> LSource, 'source reset test changes the actual table expression');
    LCandidate := LWorkspace.Candidate(LDocument, LChanged);
    Check(LCandidate.Find('layout-table').CollectionView.ToData.ToJSON =
      LSourceSpec.UseDefaultColumnLayout.UseDefaultTableLayout.ToData.ToJSON,
      'source default methods clear both explicit layout choices without losing other policies');
    FreeAndNil(LCandidate);
    ArrangementSourceRefuses(LWorkspace, LDocument,
      StringReplace(LSource, '.PinStart()', '.Pinned(''start'')', []),
      'source reader refuses string pin instead of a typed enum');
    ArrangementSourceRefuses(LWorkspace, LDocument,
      StringReplace(LSource, '[nclfVisibility, nclfPinning]',
        '[nclfVisibility, nclfVisibility]', []),
      'source reader refuses duplicate typed feature symbols');
    ArrangementSourceRefuses(LWorkspace, LDocument,
      StringReplace(LSource, '.AutoSizeSample(17)', '.AutoSizeSample(1025)', []),
      'source reader preserves public bounded sample admission');
    LTable.SetCollectionView(LSourceSpec.UseDefaultColumnLayout.UseDefaultTableLayout);
    Check(Pos('nyx.collections.arrangement,', LWorkspace.Render(LDocument)) = 0,
      'removing final layout choice restores conditional legacy import behavior');
    ArrangementAuthoredSourceCases(LWorkspace, LDocument, LTable, LSourceSpec);
  finally
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

procedure ArrangementGeometryCases;
var
  LKey: TNyxCollectionRef;
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LColumns: INyxCollectionColumns;
  LSpec: TNyxCollectionViewSpec;
  LPreferences: TNyxColumnPreferences;
  LObservation, LCopy: TNyxColumnLayoutObservation;
  LWide, LNarrow, LEmpty: TNyxColumnPresentation;
  LNatural: TNyxFlowSizes;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  { Portable geometry owns canonical values only. Logical pins are identical
    inputs for LTR/RTL adapters; this is not a physical RTL placement test. }
  LKey := NyxCollection('layout-geometry');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(NyxTextField('name'), '')
    .Integer(NyxIntegerField('pid'), 0).Boolean(NyxBooleanField('running'), False)
    .Number(NyxNumberField('cpu'), 0));
  LSpec := NyxCollectionView(LKey)
    .Column(NyxTextField('name'), 'Name').ColumnSizing(NyxColumnSizing.Fixed(120))
      .ColumnLayout(NyxColumnLayout.PinStart)
    .Column(NyxIntegerField('pid'), 'PID').ColumnSizing(NyxColumnSizing.Fixed(100))
      .ColumnLayout(NyxColumnLayout.Visible(False))
    .Column(NyxBooleanField('running'), 'Running').ColumnSizing(NyxColumnSizing.Fixed(90))
    .Column(NyxNumberField('cpu'), 'CPU').ColumnSizing(NyxColumnSizing.Fixed(110))
      .ColumnLayout(NyxColumnLayout.PinEnd)
    .TableLayout(NyxTableLayout);
  LView := NewNyxCollectionView(LStore, LSpec, cpTable);
  LColumns := NyxCollectionColumns(LView);
  LPreferences := NyxColumnPreferences
    .WithColumn(NyxColumnPreference(NyxColumnIdentity(NyxTextField('name'))).Width(150))
    .Order([NyxColumnIdentity(NyxNumberField('cpu')),
      NyxColumnIdentity(NyxIntegerField('pid')),
      NyxColumnIdentity(NyxBooleanField('running')),
      NyxColumnIdentity(NyxTextField('name'))]);
  LColumns.ConfigureColumns(LPreferences);
  LBefore := LPreferences.ToData.ToJSON;
  LObservation := TNyxColumnLayoutObservation.Read(LColumns.CaptureColumns);
  LCopy := LObservation.Copy;
  Check((LObservation.Count = 4) and (LObservation.VisibleCount = 3) and
    (LObservation.OrderedAt(0) = 3) and (LObservation.OrderedAt(1) = 1) and
    (LObservation.OrderedAt(2) = 2) and (LObservation.OrderedAt(3) = 0),
    'geometry retains complete canonical order including hidden field identity');
  Check((LObservation.VisibleAt(0) = 0) and (LObservation.VisibleAt(1) = 2) and
    (LObservation.VisibleAt(2) = 3) and not LObservation.ColumnAt(1).IsVisible,
    'visible mapping groups logical start center end without inventing data fields');
  SetLength(LNatural, 4);
  LNatural[0] := 120;
  LNatural[1] := 0;
  LNatural[2] := 90;
  LNatural[3] := 110;
  LWide := NyxColumnPresentation(LObservation, 640, LNatural);
  Check((LWide.Width(0) = 150) and (LWide.Width(1) = 0) and
    (LWide.Width(2) = 90) and (LWide.Width(3) = 110) and
    (LWide.StartWidth = 150) and (LWide.EndWidth = 110) and not LWide.PinsSuspended,
    'wide geometry resolves overrides and hidden zero width by canonical field');
  Check((LWide.EffectivePin(0) = ncpStart) and
    (LWide.EffectivePin(2) = ncpNone) and (LWide.EffectivePin(3) = ncpEnd),
    'portable presentation retains logical edges independent of host reading direction');
  LNarrow := NyxColumnPresentation(LObservation, 300, LNatural);
  Check(LNarrow.PinsSuspended and (LNarrow.StartWidth = 0) and (LNarrow.EndWidth = 0) and
    (LNarrow.EffectivePin(0) = ncpNone) and (LNarrow.EffectivePin(3) = ncpNone) and
    (LNarrow.Width(0) = 150) and (LNarrow.Width(3) = 110),
    'narrow pin fallback preserves useful scrollable widths without consuming the center budget');
  Check((LNarrow.Observation.ColumnAt(0).Pin = ncpStart) and
    (LNarrow.Observation.ColumnAt(3).Pin = ncpEnd) and
    (LNarrow.Observation.Preferences.ToData.ToJSON = LBefore),
    'pin suspension never rewrites requested pins or immutable runtime preferences');
  LRefused := False;
  LNatural[2] := 0;
  try
    NyxColumnPresentation(LObservation, 640, LNatural);
  except
    on Exception do LRefused := True;
  end;
  Check(LRefused, 'visible field requires a positive current host measurement');
  LNatural[2] := 90;
  LRefused := False;
  try
    NyxColumnPresentation(LObservation, -1, LNatural);
  except
    on Exception do LRefused := True;
  end;
  Check(LRefused, 'negative clip geometry refuses');
  LPreferences := LPreferences
    .WithColumn(NyxColumnPreference(NyxColumnIdentity(NyxTextField('name'))).Visible(False))
    .WithColumn(NyxColumnPreference(NyxColumnIdentity(NyxBooleanField('running'))).Visible(False))
    .WithColumn(NyxColumnPreference(NyxColumnIdentity(NyxNumberField('cpu'))).Visible(False));
  LColumns.ConfigureColumns(LPreferences);
  LObservation := TNyxColumnLayoutObservation.Read(LColumns.CaptureColumns);
  LNatural[0] := 0;
  LNatural[2] := 0;
  LNatural[3] := 0;
  LEmpty := NyxColumnPresentation(LObservation, 0, LNatural);
  Check((LObservation.Count = 4) and (LObservation.VisibleCount = 0) and
    (LEmpty.Width(0) = 0) and (LEmpty.Width(1) = 0) and
    (LEmpty.Width(2) = 0) and (LEmpty.Width(3) = 0),
    'all-hidden presentation keeps canonical recovery metadata and no painted data columns');
  Check((LCopy.VisibleCount = 3) and (LCopy.ColumnAt(0).Width = 150) and
    (LCopy.Preferences.ToData.ToJSON = LBefore),
    'later accepted layout cannot mutate an earlier copied observation');
  LColumns.ConfigureColumns(NyxColumnPreferences);
  Check(TNyxColumnLayoutObservation.Read(LColumns.CaptureColumns).VisibleCount = 3,
    'removing runtime overrides restores current authored visibility');
  LColumns := nil;
  LView := nil;
  LSpec := NyxCollectionView(LKey).Column(NyxTextField('name'), 'Name')
    .ColumnSizing(NyxColumnSizing.Maximum(80)).TableLayout(NyxTableLayout);
  LView := NewNyxCollectionView(LStore, LSpec, cpTable);
  LObservation := TNyxColumnLayoutObservation.Read(NyxCollectionColumns(LView).CaptureColumns);
  SetLength(LNatural, 1);
  LNatural[0] := 500;
  Check(NyxColumnPresentation(LObservation, 640, LNatural).Width(0) = 80,
    'host content measurement respects authored portable width maximum');
end;

procedure ArrangementCases;
var
  LName, LAge, LEnabled, LWeight, LForeign: TNyxColumnIdentity;
  LAbsentIdentity: TNyxColumnIdentity;
  LColumn, LColumnCopy, LAbsentColumn: TNyxColumnLayoutPolicy;
  LTable, LTableCopy, LAbsentTable: TNyxTableLayoutPolicy;
  LPreference, LEmpty, LAbsentPreference: TNyxColumnPreference;
  LPreferences, LCopy, LAbsentPreferences: TNyxColumnPreferences;
  LFields: array of TNyxColumnIdentity;
  LItems: array of TNyxDataValue;
  LFeature: TNyxColumnLayoutFeature;
  LSpec, LSpecCopy, LRound: TNyxCollectionViewSpec;
  LData, LBefore, LColumnData, LTableData, LPreferenceData: TNyxDataValue;
  I: Integer;
  LRejected: Boolean;
begin
  LAbsentIdentity := Default(TNyxColumnIdentity);
  LAbsentColumn := Default(TNyxColumnLayoutPolicy);
  LAbsentTable := Default(TNyxTableLayoutPolicy);
  LAbsentPreference := Default(TNyxColumnPreference);
  LAbsentPreferences := Default(TNyxColumnPreferences);
  Check((LAbsentIdentity.ToData.Kind = ndNull) and
    (LAbsentColumn.Copy.ToData.Kind = ndNull) and
    (LAbsentTable.Copy.ToData.Kind = ndNull) and
    (LAbsentPreference.Copy.ToData.Kind = ndNull) and
    (LAbsentPreferences.Copy.ToData.Kind = ndNull),
    'absent arrangement records and copies retain explicit null absence');
  Check(not TNyxColumnIdentity.FromData(NyxNull).Defined and
    not TNyxColumnLayoutPolicy.FromData(NyxNull).Defined and
    not TNyxTableLayoutPolicy.FromData(NyxNull).Defined and
    not TNyxColumnPreference.FromData(NyxNull).Defined and
    not TNyxColumnPreferences.FromData(NyxNull).Defined,
    'null decodes as absence for every arrangement value');
  LRejected := False;
  try
    LAbsentIdentity.Validate;
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'undefined field identity refuses checked admission');
  for I := 0 to 1 do
  begin
    LRejected := False;
    try
      case I of
        0: LData := NyxData(LAbsentIdentity.Name);
        1: LData := NyxData(Ord(LAbsentIdentity.Kind));
      end;
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected, 'undefined identity getters refuse fabricated name or family');
  end;
  LRejected := False;
  try
    LAbsentColumn.Visible(True);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'undefined column policy cannot silently opt in');
  LRejected := False;
  try
    LAbsentTable.Enabled(True);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'undefined table policy cannot silently opt in');

  LName := NyxColumnIdentity(NyxTextField('service/~name'));
  LAge := NyxColumnIdentity(NyxIntegerField('pid'));
  LEnabled := NyxColumnIdentity(NyxBooleanField('running'));
  LWeight := NyxColumnIdentity(NyxNumberField('cpu'));
  Check(LName.Same(TNyxColumnIdentity.FromData(LName.ToData)) and
    LAge.Same(TNyxColumnIdentity.FromData(LAge.ToData)) and
    LEnabled.Same(TNyxColumnIdentity.FromData(LEnabled.ToData)) and
    LWeight.Same(TNyxColumnIdentity.FromData(LWeight.ToData)),
    'four scalar identities preserve exact names and families through wire v1');
  LForeign := NyxColumnIdentity(NyxTextField('服务/名~🙂'));
  Check(LForeign.Same(TNyxColumnIdentity.FromData(LForeign.ToData)),
    'Unicode and delimiter characters remain exact field-name data');
  LForeign := NyxColumnIdentity(NyxNumberField('pid'));
  Check(not LAge.Same(LForeign) and not LAge.Same(LName),
    'same name with another family and distinct names are different identities');

  LColumn := NyxColumnLayout;
  LTable := NyxTableLayout;
  Check(LColumn.Defined and LColumn.IsVisible and (LColumn.Pin = ncpNone) and
    LTable.Defined and LTable.IsEnabled and
    (LTable.MinimumScrollableWidth = NyxDefaultColumnScrollableWidth) and
    (LTable.AutoSizeRows = NyxDefaultColumnAutoSizeRows),
    'explicit layout factories establish documented visible unpinned defaults');
  for LFeature := Low(TNyxColumnLayoutFeature) to High(TNyxColumnLayoutFeature) do
  begin
    Check(LColumn.Allows(LFeature) and LTable.Allows(LFeature),
      'all five portable layout capabilities are enabled by explicit defaults');
  end;
  LColumnCopy := LColumn.Visible(False).PinStart.AllowOnly([nclfVisibility, nclfPinning]);
  LTableCopy := LTable.Enabled(False).MinimumCenter(144).AutoSizeSample(17)
    .AllowOnly([nclfResize]);
  Check(LColumn.IsVisible and (LColumn.Pin = ncpNone) and
    not LColumnCopy.IsVisible and (LColumnCopy.Pin = ncpStart) and
    not LColumnCopy.Allows(nclfResize) and LColumnCopy.Allows(nclfPinning),
    'column policy builders preserve the earlier independent value');
  Check(LTable.IsEnabled and (LTable.MinimumScrollableWidth = 96) and
    not LTableCopy.IsEnabled and not LTableCopy.Allows(nclfResize) and
    (LTableCopy.MinimumScrollableWidth = 144) and (LTableCopy.AutoSizeRows = 17),
    'disabled table retains static layout bounds while user commands refuse');
  Check((LColumn.PinEnd.Pin = ncpEnd) and (LColumn.PinStart.Unpinned.Pin = ncpNone),
    'logical pin convenience builders preserve all three meanings');
  LColumnData := LColumnCopy.ToData;
  LTableData := LTableCopy.ToData;
  Check((TNyxColumnLayoutPolicy.FromData(LColumnData).ToData.ToJSON = LColumnData.ToJSON) and
    (TNyxTableLayoutPolicy.FromData(LTableData).ToData.ToJSON = LTableData.ToJSON),
    'complete column and table policy wire shapes round trip exactly');
  Check((NyxColumnLayout.AllowOnly([]).ToData.Field('features').Count = 0) and
    (NyxTableLayout.MinimumCenter(MaximumNyxLayoutBound)
      .AutoSizeSample(NyxMaximumColumnAutoSizeRows).AutoSizeRows = 1024),
    'empty feature sets and inclusive maximum layout work bounds remain admitted');

  LEmpty := NyxColumnPreference(LName);
  Check(LEmpty.Defined and LEmpty.IsEmpty and not LEmpty.HasWidth and
    not LEmpty.HasVisibility and not LEmpty.HasPin and
    (LEmpty.ToData.Field('width').Kind = ndNull) and
    (LEmpty.ToData.Field('visible').Kind = ndNull) and
    (LEmpty.ToData.Field('pin').Kind = ndNull),
    'defined empty preference differs from absence with three independent null masks');
  for I := 0 to 2 do
  begin
    LRejected := False;
    try
      case I of
        0: LData := NyxData(LEmpty.WidthValue);
        1: LData := NyxData(LEmpty.IsVisible);
        2: LData := NyxData(Ord(LEmpty.Pin));
      end;
    except
      on Exception do LRejected := True;
    end;
    Check(LRejected, 'masked preference reader refuses an absent value');
  end;
  LPreference := LEmpty.Width(184).Visible(False).Pinned(ncpNone);
  LPreferenceData := LPreference.ToData;
  Check(LPreference.HasWidth and (LPreference.WidthValue = 184) and
    LPreference.HasVisibility and not LPreference.IsVisible and
    LPreference.HasPin and (LPreference.Pin = ncpNone) and LEmpty.IsEmpty,
    'explicit False and none retain masks without mutating the empty preference');
  Check(TNyxColumnPreference.FromData(LPreferenceData).ToData.ToJSON = LPreferenceData.ToJSON,
    'masked preference wire round trip preserves explicit default-valued overrides');
  Check(LPreference.UseDefaultWidth.UseDefaultVisibility.UseDefaultPin.IsEmpty,
    'individual default resets clear only their own masks');

  SetLength(LFields, 2);
  LFields[0] := LAge;
  LFields[1] := LName;
  LPreferences := NyxColumnPreferences.WithColumn(LPreference).Order(LFields);
  LBefore := LPreferences.ToData;
  LFields[0] := LEnabled;
  LCopy := LPreferences.Copy.WithColumn(NyxColumnPreference(LAge).Width(73));
  Check((LPreferences.Count = 1) and (LCopy.Count = 2) and
    LPreferences.OrderAt(0).Same(LAge) and
    (LPreferences.ToData.ToJSON = LBefore.ToJSON),
    'caller order array and copied preference builders cannot alter prior snapshots');
  LCopy := LCopy.WithColumn(NyxColumnPreference(LName).Width(210));
  Check((LCopy.Count = 2) and (LCopy.ColumnAt(0).WidthValue = 210) and
    (LPreferences.ColumnAt(0).WidthValue = 184),
    'same typed identity replaces one detached override in place');
  LCopy := LCopy.WithColumn(LEmpty);
  Check((LCopy.Count = 1) and (LCopy.IndexOf(LName) = -1) and
    (LCopy.OrderCount = 2) and LCopy.OrderAt(1).Same(LName),
    'empty preference removes only an override and preserves independent order');
  LCopy := LCopy.WithoutColumn(LAge).UseDefaultOrder;
  Check((LCopy.Count = 0) and (LCopy.OrderCount = 0) and LCopy.Defined and
    (NyxColumnPreferences.ToData.Kind = ndObject),
    'full runtime reset is a defined empty value rather than authored absence');
  Check(TNyxColumnPreferences.FromData(LBefore).ToData.ToJSON = LBefore.ToJSON,
    'preference snapshot preserves order and masked values through strict wire v1');
  LCopy := LPreferences.UseDefaultOrder;
  Check((LCopy.Count = 1) and (LCopy.OrderCount = 0) and
    (LPreferences.OrderCount = 2), 'default order reset preserves independent field overrides');
  LRejected := False;
  try
    LPreferences.WithColumn(NyxColumnPreference(NyxColumnIdentity(
      NyxNumberField('service/~name'))).Width(20));
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LPreferences.ToData.ToJSON = LBefore.ToJSON),
    'same-name cross-family override refuses without changing accepted preferences');
  LRejected := False;
  try
    LPreferences.Order([LName, LName]);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'duplicate order identity refuses instead of silently deduplicating');
  LRejected := False;
  try
    LPreferences.Order([NyxColumnIdentity(NyxNumberField('service/~name'))]);
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected, 'order cannot reinterpret the family of an existing override');
  SetLength(LFields, NyxMaximumColumnLayoutFields);
  for I := 0 to High(LFields) do
  begin
    LFields[I] := NyxColumnIdentity(NyxTextField('field-' + IntToStr(I)));
  end;
  LCopy := NyxColumnPreferences.Order(LFields);
  Check(LCopy.OrderCount = 64, 'the full admitted field count remains usable');
  LRejected := False;
  try
    LCopy.WithColumn(NyxColumnPreference(LName).Width(1));
  except
    on Exception do LRejected := True;
  end;
  Check(LRejected and (LCopy.OrderCount = 64),
    'union limit includes override-only fields beyond a full order prefix');

  ArrangementRefuses(0, ArrangementMember(LName.ToData, 'kind', NyxData('date')),
    'non-scalar identity family refuses');
  ArrangementRefuses(0, ArrangementMember(LName.ToData, 'version', NyxData(2)),
    'identity future version refuses');
  ArrangementRefuses(1, ArrangementMember(LColumnData, 'visible', NyxData('false')),
    'wire visibility strings cannot impersonate Boolean choices');
  ArrangementRefuses(1, ArrangementMember(LColumnData, 'pin', NyxData('left')),
    'physical-edge wire pin refuses in favor of logical edges');
  ArrangementRefuses(1, ArrangementMember(LColumnData, 'features',
    NyxArray([NyxData('resize'), NyxData('resize')])),
    'duplicate feature wire entries refuse');
  ArrangementRefuses(1, ArrangementMember(LColumnData, 'features',
    NyxArray([NyxData('unknown')])), 'unknown feature wire entries refuse');
  ArrangementRefuses(2, ArrangementMember(LTableData, 'minimumCenter', NyxData(0)),
    'zero center budget refuses');
  ArrangementRefuses(2, ArrangementMember(LTableData, 'minimumCenter', NyxData(MaximumNyxLayoutBound + 1)),
    'center budget above portable layout bound refuses');
  ArrangementRefuses(2, ArrangementMember(LTableData, 'autoSizeRows', NyxData(1025)),
    'auto-size sample above bounded work limit refuses');
  ArrangementRefuses(2, ArrangementMember(LTableData, 'enabled', NyxNull),
    'table enabled requires a real Boolean');
  ArrangementRefuses(3, ArrangementMember(LPreferenceData, 'width', NyxData('184')),
    'preference width requires an Integer primitive');
  ArrangementRefuses(3, ArrangementMember(LPreferenceData, 'width', NyxData(0)),
    'zero preference width refuses');
  ArrangementRefuses(3, ArrangementMember(LPreferenceData, 'width',
    NyxData(MaximumNyxLayoutBound + 1)), 'preference width above portable layout bounds refuses');
  ArrangementRefuses(3, ArrangementMember(LPreferenceData, 'field', NyxNull),
    'defined preference requires a real typed field');
  ArrangementRefuses(4, NyxObject([NyxField('version', NyxData(1)),
    NyxField('columns', NyxArray([LPreferenceData, LPreferenceData])),
    NyxField('order', NyxArray([]))]), 'duplicate snapshot overrides refuse');
  ArrangementRefuses(4, NyxObject([NyxField('version', NyxData(1)),
    NyxField('columns', NyxArray([])), NyxField('order',
    NyxArray([LName.ToData, LName.ToData]))]), 'duplicate saved order refuses');
  SetLength(LItems, NyxMaximumColumnLayoutFields + 1);
  for I := 0 to High(LItems) do
  begin
    LItems[I] := NyxColumnIdentity(NyxTextField('field-' + IntToStr(I))).ToData;
  end;
  ArrangementRefuses(4, NyxObject([NyxField('version', NyxData(1)),
    NyxField('columns', NyxArray([])), NyxField('order', NyxArray(LItems))]),
    'oversized saved order refuses before accepting a vector');
  for I := 0 to 4 do
  begin
    case I of
      0: LData := LName.ToData;
      1: LData := LColumnData;
      2: LData := LTableData;
      3: LData := LPreferenceData;
      4: LData := LBefore;
    end;
    ArrangementRefuses(I, ArrangementMember(LData, 'unexpected', NyxNull),
      'every arrangement wire shape refuses unknown members');
    ArrangementRefuses(I, NyxObject([NyxField('version', NyxData(1))]),
      'every arrangement wire shape refuses incomplete objects');
    ArrangementRefuses(I, ArrangementMember(LData, 'version', NyxData(2)),
      'every arrangement wire shape refuses unknown future versions');
  end;

  LSpec := NyxCollectionView(NyxCollection('layout-contract'))
    .Column(NyxTextField('name'), 'Name').ColumnSizing(NyxColumnSizing.Fixed(180))
    .ColumnQuery(NyxColumnQuery.Filtering(False))
    .Column(NyxIntegerField('pid'), 'PID', cmEditable)
    .TableQuery(NyxTableQuery.Sorting(False)).TypeAhead(NyxTypeAhead)
    .Paged(NyxCollectionPaging.PageSize(7));
  LBefore := LSpec.ToData;
  Check((LBefore.Field('version').AsInteger = 7) and not LSpec.HasLayoutControls and
    LSpec.TableLayoutPolicy.IsEnabled and not LSpec.HasTableLayoutPolicy and
    LSpec.ColumnAt(0).LayoutPolicy.IsVisible and not LSpec.ColumnAt(0).HasLayoutPolicy,
    'absent layout resolves defaults while preserving exact legacy v7 descriptor');
  LSpecCopy := LSpec.Copy.ColumnLayout(LColumnCopy).TableLayout(LTableCopy);
  Check((LSpec.ToData.ToJSON = LBefore.ToJSON) and LSpecCopy.HasLayoutControls and
    LSpecCopy.HasColumnLayout and LSpecCopy.HasTableLayoutPolicy and
    not LSpecCopy.ColumnAt(1).LayoutPolicy.IsVisible and
    not LSpecCopy.TableLayoutPolicy.IsEnabled,
    'authored opt-in changes only a detached binding and preceding column');
  LData := LSpecCopy.ToData;
  Check((LData.Field('version').AsInteger = 8) and (LData.Count = 11) and
    (LData.Field('columns').Item(1).Count = 7),
    'layout choice selects complete binding v8 and seven-member column shape');
  LRound := TNyxCollectionViewSpec.FromData(LData);
  Check(LRound.ToData.ToJSON = LData.ToJSON,
    'strict binding v8 round trip preserves every existing and new descriptor');
  Check((LRound.ColumnAt(0).Sizing.Value = 180) and
    not LRound.ColumnAt(0).QueryPolicy.FilterEnabled and
    not LRound.TableQueryPolicy.SortEnabled and LRound.HasTypeAhead and
    (LRound.PagingPolicy.ItemsPerPage = 7),
    'layout copy and v8 persistence retain sizing query typeahead and paging choices');
  LRound := LRound.UseDefaultColumnLayout.UseDefaultTableLayout;
  Check(not LRound.HasLayoutControls and (LRound.ToData.ToJSON = LBefore.ToJSON),
    'clearing final authored layout choices restores exact v7 bytes');
  LRound := LSpec.TableLayout(NyxTableLayout);
  Check(LRound.HasTableLayoutPolicy and not LSpec.HasTableLayoutPolicy and
    (LRound.ToData.Field('version').AsInteger = 8),
    'explicit factory-default table policy differs from absent authoring');
  LRound := LSpec.ColumnLayout(NyxColumnLayout);
  Check(LRound.HasColumnLayout and not LRound.HasTableLayoutPolicy and
    LRound.ColumnAt(1).HasLayoutPolicy and
    (LRound.ToData.Field('version').AsInteger = 8),
    'explicit factory-default column policy independently opts in');
  ArrangementRefuses(5, ArrangementMember(LData, 'unexpected', NyxNull),
    'v8 binding rejects unknown top-level members');
  ArrangementRefuses(5, ArrangementMember(LData, 'layoutControls',
    ArrangementMember(LTableData, 'unexpected', NyxNull)),
    'v8 binding does not bypass strict nested layout policy admission');
  SetLength(LItems, 2);
  LItems[0] := LData.Field('columns').Item(0);
  LItems[1] := ArrangementMember(LData.Field('columns').Item(1), 'unexpected', NyxNull);
  ArrangementRefuses(5, ArrangementMember(LData, 'columns', NyxArray(LItems)),
    'v8 binding rejects unknown column members');
  ArrangementSourceCases(LSpecCopy);
  ArrangementGeometryCases;
end;
{ END COLUMN ARRANGEMENT CASES }

type
  { Lifetime counters outlive the sample and every detached metric copy. The
    sample retains only this test-owned receiver and an immutable descriptor;
    it has no live view, store, widget or private publication proof. }
  TFitSampleLifetime = class
  public
    Created: Integer;
    Destroyed: Integer;
    Reads: Integer;
  end;

  TCountedFitSample = class(TInterfacedObject, INyxCollectionColumnSample)
  private
    FLifetime: TFitSampleLifetime;
    FColumn: TNyxCollectionColumn;
  public
    constructor Create(ALifetime: TFitSampleLifetime);
    destructor Destroy; override;
    function GetColumn: TNyxCollectionColumn;
    function GetCount: Integer;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function TextAt(AIndex: Integer): TNyxText;
  end;

constructor TCountedFitSample.Create(ALifetime: TFitSampleLifetime);
var
  LSpec: TNyxCollectionViewSpec;
begin
  inherited Create;
  FLifetime := ALifetime;
  Inc(FLifetime.Created);
  LSpec := NyxCollectionView(NyxCollection('fit-lifetime'))
    .Column(NyxTextField('accepted'), 'Accepted sample')
    .ColumnSizing(NyxColumnSizing.Minimum(60).Maximum(140));
  FColumn := LSpec.ColumnAt(0);
end;

destructor TCountedFitSample.Destroy;
begin
  Inc(FLifetime.Destroyed);
  inherited Destroy;
end;

function TCountedFitSample.GetColumn: TNyxCollectionColumn;
begin
  Inc(FLifetime.Reads);
  Result := FColumn.Copy;
end;

function TCountedFitSample.GetCount: Integer;
begin
  Inc(FLifetime.Reads);
  Result := 1;
end;

function TCountedFitSample.ItemAt(AIndex: Integer): TNyxItemRef;
begin

  if AIndex <> 0 then
  begin
    raise ENyxCollection.Create('Counted fit sample item is outside its immutable page');
  end;
  Inc(FLifetime.Reads);
  Result := NyxItem(NyxCollection('fit-lifetime'), 'first');
end;

function TCountedFitSample.TextAt(AIndex: Integer): TNyxText;
begin

  if AIndex <> 0 then
  begin
    raise ENyxCollection.Create('Counted fit sample text is outside its immutable page');
  end;
  Inc(FLifetime.Reads);
  Result := 'Accepted sample text';
end;

{ Keep interface-return temporaries in this complete helper scope. After it
  returns the outer case can observe final release without compiler-managed
  expression temporaries becoming an accidental extra sample owner. }
procedure BuildFitMeasurementCopies(ALifetime: TFitSampleLifetime;
  var ALast: INyxColumnFitMeasurement);
var
  LSample, LRead: INyxCollectionColumnSample;
  LInitial, LCopy, LWide, LAssigned: INyxColumnFitMeasurement;
  LReads: Integer;
begin
  LSample := TCountedFitSample.Create(ALifetime);
  LInitial := NyxColumnFitMeasurement(LSample, 80.25);
  LReads := ALifetime.Reads;
  LCopy := LInitial.Copy;
  LWide := LCopy.WithNaturalWidth(200.5);
  LRead := LWide.Sample;
  Check((LRead = LSample) and
    LWide.Field.Same(NyxColumnIdentity(NyxTextField('accepted'))) and
    (LWide.SampleRows = 1),
    'fit metric Copy and WithNaturalWidth retain the exact original typed sample');
  Check((LWide <> LInitial) and (LInitial.Width = 81) and not LInitial.Clamped and
    (LCopy.NaturalWidth = 80.25) and (LWide.Width = 140) and LWide.Clamped,
    'fit width replacement leaves original scalar facts independent and respects copied bounds');
  Check(ALifetime.Reads = LReads,
    'fit metric copies and width replacement never re-read the retained sample');
  LInitial := nil;
  LSample := nil;
  Check((ALifetime.Created = 1) and (ALifetime.Destroyed = 0) and
    (LInitial = nil) and (LCopy <> nil) and (LWide <> nil),
    'nil replacement and caller sample release preserve surviving metric interfaces');
  LRead := nil;
  LCopy := nil;
  LAssigned := LWide;
  LWide := nil;
  Check((ALifetime.Destroyed = 0) and (LAssigned <> nil) and
    (LAssigned.Width = 140),
    'ordinary interface assignment owns the immutable metric after other copies are cleared');
  ALast := LAssigned.Copy;
  LAssigned := nil;
end;

procedure ReplaceFitMeasurementWidth(var AValue: INyxColumnFitMeasurement);
begin
  { Exercise self replacement while the old interface owns the only sample lease. }
  AValue := AValue.WithNaturalWidth(93.25);
end;

procedure FitMeasurementLifetimeCases;
var
  LLifetime: TFitSampleLifetime;
  LLast: INyxColumnFitMeasurement;
  LReads: Integer;
begin
  LLifetime := TFitSampleLifetime.Create;
  try
    BuildFitMeasurementCopies(LLifetime, LLast);
    Check((LLifetime.Created = 1) and (LLifetime.Destroyed = 0) and (LLast <> nil),
      'last detached metric retains its sample after all builder locals finalize');
    LReads := LLifetime.Reads;
    ReplaceFitMeasurementWidth(LLast);
    Check((LLifetime.Destroyed = 0) and (LLifetime.Reads = LReads) and
      (LLast.Width = 94) and (LLast.NaturalWidth = 93.25) and not LLast.Clamped,
      'self width replacement preserves the sole sample lease without outward getters');
    LLast := nil;
    Check((LLast = nil) and (LLifetime.Destroyed = 1),
      'clearing the final metric releases the counted sample exactly once');
    LLast := nil;
    Check(LLifetime.Destroyed = 1,
      'repeated nil replacement cannot release an already finalized sample again');
  finally
    { Release any surviving lease before its borrowed counter receiver. }
    LLast := nil;
    LLifetime.Free;
  end;
end;

{ Raw reusable descriptors carry complete typed data without pretending to be
  List controls. These cases share one authored document and its cloned proposed
  changes; composition must validate the actual named target, including a recipe
  selected only by a supplied viewport. No renderer, store or widget is mounted. }
procedure ArrangementDescriptorAdmissionCases;
const
  CInstance = 'descriptor-instance';
  COverride = 'descriptor-records-override';
  CTableRecipe = 'descriptor-table-recipe';
  CListRecipe = 'descriptor-list-recipe';
  CLabelRecipe = 'descriptor-label-recipe';
  CTreeRecipe = 'descriptor-tree-recipe';
var
  LDocument: TNyxDocument;
  LCase: TNyxDocument;
  LView: TNyxNode;
  LTarget: TNyxNode;
  LPart: TNyxNode;
  LDefinition: TNyxNode;
  LInstance: INyxComponent;
  LPage: INyxPage;
  LKey: TNyxCollectionRef;
  LPlain: TNyxCollectionViewSpec;
  LDefinitionSpec: TNyxCollectionViewSpec;
  LOverrideSpec: TNyxCollectionViewSpec;
  LBefore: TNyxText;
  LRejected: Boolean;

  procedure AddRecipe(const AID, APartID: TNyxText; AKind: TNyxKind;
    AWithBinding: Boolean);
  var
    LRecipe: INyxColumn;
    LControl: TNyxNode;
  begin
    LRecipe := NewNyxColumn(AID);
    LControl := TNyxNode.Create(AKind, APartID);
    LRecipe.Node.Add(LControl);
    LControl.Configure.PartName(NyxPart('records')).Done;

    if AWithBinding then
    begin
      LControl.SetCollectionView(LDefinitionSpec);
    end;
    LDocument.AddComponent(LRecipe);
  end;

  procedure RefusesDocument(const AReason: TNyxText);
  begin
    LRejected := False;
    try
      LCase.Validate;
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LBefore), AReason);
  end;

  procedure ComposeCase;
  begin
    FreeAndNil(LView);
    LView := RealizeNyxView(LCase, LCase.Pages[0]);
    Check((LView.Count = 1) and (LView.Children[0].DesignID = CInstance),
      'realized records retain the original sole authored reusable owner');
    LTarget := LView.Children[0].Part(NyxPart('records'));
    Check(LTarget <> nil, 'admitted reusable context retains its exact named records part');
  end;

begin
  LDocument := TNyxDocument.Create;
  LCase := nil;
  LView := nil;
  LInstance := nil;
  LPage := nil;
  try
    LKey := NyxCollection('descriptor-rows');
    LDocument.Collections.Define(LKey,
      NyxCollectionSchema.Text(NyxTextField('title'), '')
        .Integer(NyxIntegerField('priority'), 0), [
      NyxCollectionItem(NyxItem(LKey, 'one')).WithValue(NyxTextField('title'),
        TNyxText('Kept / 🌙')).WithValue(NyxIntegerField('priority'), 7)]);
    LPlain := NyxCollectionView(LKey).Column(NyxTextField('title'), 'Title', cmEditable)
      .Column(NyxIntegerField('priority'), 'Priority', cmEditable);
    LDefinitionSpec := NyxCollectionView(LKey)
      .Column(NyxTextField('title'), 'Title', cmEditable)
        .ColumnLayout(NyxColumnLayout.PinStart)
      .Column(NyxIntegerField('priority'), 'Priority', cmEditable)
        .ColumnLayout(NyxColumnLayout.PinEnd)
      .TableLayout(NyxTableLayout);
    LOverrideSpec := LDefinitionSpec.TableLayout(NyxTableLayout.Enabled(False))
      .ColumnLayout(NyxColumnLayout.Visible(False).PinEnd.AllowOnly([]));
    AddRecipe(CTableRecipe, 'descriptor-table', nkTable, True);
    AddRecipe(CListRecipe, 'descriptor-list', nkList, False);
    AddRecipe(CLabelRecipe, 'descriptor-label', nkLabel, False);
    AddRecipe(CTreeRecipe, 'descriptor-tree', nkTree, False);
    LPage := NewNyxPage('descriptor-page');
    LDocument.AddPage(LPage);
    LInstance := NewNyxComponent(CInstance);
    LInstance.Configure.Component(NyxComponent(CTableRecipe)).Done;
    LPart := LInstance.Node.OverridePart(NyxPart('records'), noProperties);
    LPart.Named(COverride);
    LPart.SetCollectionView(LOverrideSpec);
    LPage.Add(LInstance);
    LDocument.Validate;
    Check((LPart.Kind = NyxKindName(nkSlotOverride)) and LPart.HasCollectionView and
      (LPart.CollectionView.ToData.ToJSON = LOverrideSpec.ToData.ToJSON),
      'raw named Table property override admits layout without rewriting its descriptor kind');
    LBefore := TNyxCodec.Encode(LDocument);
    LCase := LDocument.Clone;
    ComposeCase;
    Check((LTarget.ProjectionKind = NyxKindName(nkTable)) and
      (LTarget.CollectionView.ToData.ToJSON = LOverrideSpec.ToData.ToJSON) and
      (LTarget.CollectionView.ToData.Field('version').AsInteger = 8) and
      (LTarget.CollectionView.ColumnAt(0).LayoutPolicy.Pin = ncpStart) and
      not LTarget.CollectionView.ColumnAt(1).LayoutPolicy.IsVisible and
      (LTarget.CollectionView.ColumnAt(1).LayoutPolicy.Pin = ncpEnd) and
      (LTarget.CollectionView.ColumnAt(1).LayoutPolicy.Features = []) and
      not LTarget.CollectionView.TableLayoutPolicy.IsEnabled,
      'composition retains exact logical pins, hidden state, empty features and explicit disabled policy');
    FreeAndNil(LView);
    FreeAndNil(LCase);

    LCase := LDocument.Clone;
    LCase.Find(CInstance).Configure.Component(NyxComponent(CListRecipe)).Done;
    RefusesDocument('layout on the same named part resolving to List refuses document admission');
    FreeAndNil(LCase);
    LCase := LDocument.Clone;
    LCase.Find(CInstance).Configure.Component(NyxComponent(CLabelRecipe)).Done;
    RefusesDocument('layout cannot manufacture a collection projection on a noncollection named part');
    FreeAndNil(LCase);

    LCase := LDocument.Clone;
    LCase.Find(COverride).SetCollectionView(NyxCollectionView(LKey)
      .Column(NyxIntegerField('title'), 'Wrong family').TableLayout(NyxTableLayout));
    RefusesDocument('neutral descriptor validation still rejects an exact schema family mismatch');
    FreeAndNil(LCase);
    LCase := LDocument.Clone;
    LCase.Find(COverride).SetCollectionView(NyxCollectionView(LKey)
      .Column(NyxTextField('missing'), 'Missing').TableLayout(NyxTableLayout));
    RefusesDocument('neutral descriptor validation still rejects an unknown schema field');
    FreeAndNil(LCase);
    LCase := LDocument.Clone;
    LCase.Find(COverride).SetCollectionView(LPlain.Query(NyxCollectionQuery.Where(
      NyxWhere(NyxIntegerField('title')).EqualTo(7))));
    RefusesDocument('neutral descriptor validation retains typed query-family admission');
    FreeAndNil(LCase);

    LCase := LDocument.Clone;
    LCase.Find(COverride).SetCollectionView(LPlain.TypeAhead(NyxTypeAhead));
    RefusesDocument('authored typeahead refuses its actual effective Table target');
    FreeAndNil(LCase);
    LCase := LDocument.Clone;
    LCase.Find(CInstance).Configure.Component(NyxComponent(CTreeRecipe)).Done;
    LCase.Find(COverride).SetCollectionView(LPlain.Paged(NyxCollectionPaging.PageSize(1)));
    RefusesDocument('flat paging refuses its actual effective Tree even without a parent field');
    FreeAndNil(LCase);
    LCase := LDocument.Clone;
    LCase.Find(CInstance).Configure.Component(NyxComponent(CTreeRecipe)).Done;
    LCase.Find(COverride).SetCollectionView(LPlain);
    LCase.Validate;
    ComposeCase;
    Check(LTarget.ProjectionKind = NyxKindName(nkTree),
      'a valid parent-free reusable Tree retains its actual projection');
    FreeAndNil(LView);
    FreeAndNil(LCase);

    LCase := LDocument.Clone;
    LCase.Find(COverride).Binds.ClearCollection;
    LCase.Validate;
    ComposeCase;
    Check(LCase.Find(COverride).HasCollectionView and LTarget.HasCollectionView and
      not LTarget.CollectionView.Defined,
      'an explicit local clear stays an unbound mask over valid inherited layout');
    FreeAndNil(LView);
    LCase.Find(COverride).RemoveCollectionView;
    LCase.Validate;
    ComposeCase;
    LDefinition := LCase.Find('descriptor-table');
    Check(not LCase.Find(COverride).HasCollectionView and
      (LTarget.CollectionView.ToData.ToJSON = LDefinitionSpec.ToData.ToJSON) and
      (LDefinition.CollectionView.ToData.ToJSON = LDefinitionSpec.ToData.ToJSON),
      'withdrawing the mask restores exact authored inheritance without rewriting its definition');
    FreeAndNil(LView);
    FreeAndNil(LCase);

    LCase := LDocument.Clone;
    LCase.Find(CInstance).Content.Use(NyxComponent(CTableRecipe))
      .WhenViewport(TNyxViewportWidth.Below(640)).Use(NyxComponent(CListRecipe));
    LCase.Validate;
    LView := RealizeNyxView(LCase, LCase.Pages[0], TNyxViewFrame.At(1280, 800, npfBrowser));
    Check((LView.Count = 1) and (LView.Children[0].DesignID = CInstance),
      'wide content selection keeps its exact original authored instance owner');
    LTarget := LView.Children[0].Part(NyxPart('records'));
    Check(LTarget.ProjectionKind = NyxKindName(nkTable),
      'default and wide content recipes admit the same real Table layout');
    FreeAndNil(LView);
    LRejected := False;
    try
      LView := RealizeNyxView(LCase, LCase.Pages[0], TNyxViewFrame.At(390, 800, npfBrowser));
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LView = nil) and (TNyxCodec.Encode(LDocument) = LBefore),
      'actual compact content-selected List cannot inherit a Table-only layout after default admission');
  finally
    LView.Free;
    LCase.Free;
    { Specialized interfaces borrow no raw nodes after the document retires;
      release these local owners first so finalization cannot prolong the tree. }
    LInstance := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

procedure Run;
var
  LPolicies: TNyxColumnSizings;
  LNatural: TNyxFlowSizes;
  LWidths: TNyxFlowSizes;
  LSpec: TNyxCollectionViewSpec;
  LCopy: TNyxCollectionViewSpec;
  LKey: TNyxCollectionRef;
  LVersion: Integer;
  LBefore: TNyxText;
  LSource: TNyxText;
  LRejected: Boolean;
  LData: TNyxDataValue;
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LPage: TNyxNode;
  LTable: TNyxNode;
  LWorkspace: TNyxSourceWorkspace;
  LIntent: TNyxStudioCollectionIntent;
  LSession: TNyxStudioSession;
  LPanel: TNyxNode;
  LPatch: INyxCollectionPatch;
  LPair: TNyxProjectPair;
  LNext: TNyxProjectPair;
begin
  SetLength(LPolicies, 3);
  SetLength(LNatural, 3);
  LNatural[0] := 100;
  LNatural[1] := 50;
  LNatural[2] := 50;
  LWidths := NyxColumnWidths(600, LPolicies, LNatural);
  Check((LWidths[0] = 300) and (LWidths[1] = 150) and (LWidths[2] = 150),
    'default content fills available width proportionally');
  LPolicies[0] := NyxColumnSizing.Flex(3).Minimum(100);
  LPolicies[1] := NyxColumnSizing.Fixed(80);
  LPolicies[2] := NyxColumnSizing.Flex(1).Minimum(50);
  LWidths := NyxColumnWidths(600, LPolicies, LNatural);
  Check((LWidths[0] = 390) and (LWidths[1] = 80) and (LWidths[2] = 130),
    'mixed fixed and flexible columns share exact available width');
  LWidths := NyxColumnWidths(200, LPolicies, LNatural);
  Check((LWidths[0] = 100) and (LWidths[1] = 80) and (LWidths[2] = 50),
    'admitted minima deliberately overflow a narrow host');
  LBefore := LPolicies[0].ToData.ToJSON;
  LPolicies[0] := LPolicies[0].Maximum(120);
  LWidths := NyxColumnWidths(600, LPolicies, LNatural);
  Check((LWidths[0] = 120) and (LWidths[1] = 80) and (LWidths[2] = 400),
    'maximum-frozen space redistributes to a flexible sibling');
  Check(TNyxColumnSizing.FromData(TNyxDataValue.ParseJSON(LBefore)).Bounds.MaximumValue = 100000,
    'retained policy bytes are independent from a later fluent change');
  LRejected := False;
  try
    NyxColumnSizing.Fixed(0);
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'zero fixed pixels refuse');
  LRejected := False;
  try
    NyxColumnSizing.Flex(-1);
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'negative weights refuse');
  LRejected := False;
  try
    LPolicies[0].Maximum(99);
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LPolicies[0].Bounds.MaximumValue = 120),
    'reversed bounds refuse without mutating the retained value');
  LNatural[1] := 0;
  LRejected := False;
  try
    NyxColumnWidths(600, LPolicies, LNatural);
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'invalid host measurement refuses before allocation');
  LNatural[1] := 50;
  LRejected := False;
  try
    TNyxColumnSizing.FromData(TNyxDataValue.ParseJSON(
      '{"version":1,"mode":"other","value":1,"minimum":1,"maximum":100}'));
  except
    on EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'unknown persistence mode refuses');
  LKey := NyxCollection('ideas');
  for LVersion := 1 to 4 do
  begin
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('title'), 'Idea', cmEditable)
      .Column(NyxIntegerField('priority'), 'Priority');

    if LVersion > 1 then
    begin
      LSpec := LSpec.Selection(nsmMultiple);
    end;

    if LVersion = 3 then
    begin
      LSpec := LSpec.Query(NyxCollectionQuery.OrderBy(NyxTextField('title')));
    end;

    if LVersion = 4 then
    begin
      LSpec := LSpec.TypeAhead(NyxTypeAhead);
    end;
    LBefore := LSpec.ToData.ToJSON;
    LCopy := TNyxCollectionViewSpec.FromData(TNyxDataValue.ParseJSON(LBefore));
    Check((LCopy.ToData.ToJSON = LBefore) and not LCopy.HasColumnSizing,
      'every legacy descriptor remains byte-compatible');
    LCopy := LSpec.ColumnSizing(NyxColumnSizing.Fixed(96));
    LData := LCopy.ToData;
    Check((LData.Field('version').AsInteger = 5) and
      (TNyxCollectionViewSpec.FromData(LData).ToData.ToJSON = LData.ToJSON),
      'sizing round-trips independently with legacy query/selection/typeahead');
    Check(LSpec.ToData.ToJSON = LBefore, 'copied fluent columns do not mutate the original binding');
    Check(LCopy.UseDefaultColumnSizing.ToData.ToJSON = LBefore,
      'restoring default sizing returns exact legacy bytes');
  end;
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  try
    LDocument.Collections.Define(LKey, NyxCollectionSchema.Text(NyxTextField('title'), '')
      .Integer(NyxIntegerField('priority'), 1), [
      NyxCollectionItem(NyxItem(LKey, 'first')).WithValue(NyxTextField('title'), 'Explore an idea')]);
    LPage := TNyxNode.Create(nkPage, 'page');
    LDocument.AddPage(LPage);
    LTable := TNyxNode.Create(nkTable, 'ideas-table');
    LPage.Add(LTable);
    LSpec := NyxCollectionView(LKey).Column(NyxTextField('title'), 'Idea', cmEditable)
      .ColumnSizing(NyxColumnSizing.Flex(3).Minimum(120))
      .Column(NyxIntegerField('priority'), 'Priority')
      .ColumnSizing(NyxColumnSizing.Fixed(96));
    LTable.SetCollectionView(LSpec);
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := LWorkspace.Render(LDocument);
    Check((Pos('.ColumnSizing(NyxColumnSizing.Flex(3).Minimum(120))', LSource) > 0) and
      (Pos('nyx.collections.columns,', LSource) > 0), 'crafted source uses typed policy and its owning import');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LBefore, 'public source reader reconstructs complete column meaning');
    FreeAndNil(LCandidate);
    LIntent := Default(TNyxStudioCollectionIntent);
    LIntent.Action := scaTitle;
    LIntent.Key := LKey;
    LIntent.Field := TNyxStudioCollectionFieldRef.Text(NyxTextField('title'));
    LIntent.Projection := cpTable;
    LIntent.Value := 'A clearer idea';
    LCopy := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec,
      LDocument.Collections.Snapshot(LKey).Schema);
    Check((LCopy.ColumnAt(0).Sizing.ToData.ToJSON = LSpec.ColumnAt(0).Sizing.ToData.ToJSON) and
      (LCopy.ColumnAt(1).Sizing.ToData.ToJSON = LSpec.ColumnAt(1).Sizing.ToData.ToJSON),
      'ordinary title edits retain every copied width policy');
    LIntent.Action := scaColumnFixed;
    LIntent.Value := '144';
    LCopy := ApplyNyxStudioCollectionViewIntent(LIntent, LSpec,
      LDocument.Collections.Snapshot(LKey).Schema);
    Check((LCopy.ColumnAt(0).Sizing.Mode = ncsFixed) and
      (LCopy.ColumnAt(0).Sizing.Value = 144) and (LSpec.ColumnAt(0).Sizing.Mode = ncsFlex),
      'typed ordinary sizing changes one independent column');
    LPair := Default(TNyxProjectPair);
    LPair.Design := LBefore;
    LPair.Source := LSource;
    LPatch := NyxCollectionPatch([
      NyxCollectionIntentChange(NyxBindingOwner('ideas-table'), LIntent)]);
    LData := LPatch.ToData;
    Check(LData.Item(0).Field('value').AsInteger = 144,
      'semantic dimensions are JSON Integers at the explicit wire boundary');
    Check(ReadNyxCollectionPatch(LData).ToData.ToJSON = LData.ToJSON,
      'narrow width intent retains exact typed wire meaning');
    LNext := ReadNyxCollectionPatch(LData).Candidate(LPair);
    LCandidate := TNyxCodec.Decode(LNext.Design);
    Check((LCandidate.Find('ideas-table').CollectionView.ColumnAt(0).Sizing.Value = 144) and
      (Pos('.Fixed(144)', LNext.Source) > 0),
      'semantic width replay admits both design and fluent Pascal');
    FreeAndNil(LCandidate);
    LRejected := False;
    try
      ReadNyxCollectionPatch(TNyxDataValue.ParseJSON(
        '[{"op":"intent","action":"column-fixed","key":"ideas",' +
        '"owner":"ideas-table","projection":"table","field":"title",' +
        '"kind":"text","value":"144"}]'));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'numeric wire strings cannot impersonate typed dimensions');
    LIntent.Value := '0';
    LRejected := False;
    try
      NyxCollectionPatch([NyxCollectionIntentChange(NyxBindingOwner('ideas-table'), LIntent)])
        .Candidate(LPair);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LPair.Design = LBefore) and (LPair.Source = LSource),
      'invalid width admission preserves both accepted halves');
    LIntent.Value := '144';
    LIntent.Projection := cpList;
    LRejected := False;
    try
      LIntent.Validate;
    except
      on ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'table width actions refuse another projection family');
    LIntent.Projection := cpTable;
    LIntent.Action := scaColumnDefault;
    LIntent.Value := '';
    LCopy := ApplyNyxStudioCollectionViewIntent(LIntent, LCopy,
      LDocument.Collections.Snapshot(LKey).Schema);
    Check(not LCopy.ColumnAt(0).Sizing.Defined and LCopy.ColumnAt(1).Sizing.Defined,
      'default reset preserves other column choices');
    LSession := TNyxStudioSession.Create;
    LPanel := TNyxNode.Create(nkColumn, 'panel');
    try
      LSession.Load(LBefore);
      LSession.Select('ideas-table');
      AddNyxCollectionBindingPanel(LPanel, LSession, LSession.Document.Find('ideas-table'));
      Check((LPanel.Find('collection-column-0-flex') <> nil) and
        (LPanel.Find('collection-column-1-fixed').Prop('value') = '96'),
        'Nyx-built Bindings panel exposes actual typed sizing values');
    finally
      LPanel.Free;
      LSession.Free;
    end;
    LRejected := False;
    try
      LCandidate := LWorkspace.Candidate(LDocument,
        StringReplace(LSource, '.Fixed(96)', '.Fixed(''96'')', []));
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LBefore),
      'raw string dimensions refuse while accepted document meaning stays exact');
    LTable.SetCollectionView(NyxCollectionView(LKey).Column(NyxTextField('title'), 'Idea'));
    Check(not LDocument.HasColumnSizing and
      (Pos('nyx.collections.columns,', TNyxCodegen.Generate(LDocument)) = 0),
      'legacy source receives no unnecessary sizing import');
  finally
    LCandidate.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    { BEGIN COLUMN ARRANGEMENT CALL }
    ArrangementCases;
    FitMeasurementLifetimeCases;
    ArrangementDescriptorAdmissionCases;
    { END COLUMN ARRANGEMENT CALL }
    {$ifdef PAS2JS}
    document.body.setAttribute('data-column-sizing-tests', 'passed');
    document.body.setAttribute('data-column-sizing-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' column sizing contract/source checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-column-sizing-tests', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
