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

program nyx_date_range_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.dates, nyx.contract, nyx.date.range, nyx.date.range.editor,
  nyx.codec, nyx.composition, nyx.schema, nyx.binding, nyx.binding.types,
  nyx.state, nyx.publication
  {$ifdef PAS2JS}, Web{$endif};

type
  { Ordinary authored ranges exercise concrete, whole reusable and named-part
    ownership without constructing renderer controls or a shadow inventory. }
  TRangeFixture = class
    Document: TNyxDocument;
    Local: INyxDateRange;
    Definition: INyxDateRange;
    First: INyxComponent;
    Second: INyxComponent;
    Nested: INyxComponent;
    Rule: TNyxNode;
    constructor Create;
    destructor Destroy; override;
  end;

  { An actual state observer fails after installation to retain the existing
    committed-notification contract independently of ordering refusal. }
  TThrowingObserver = class
    Calls: Integer;
    procedure Changed(AState: TNyxState; AChanges: TNyxStateChanges);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(String(AMessage));
  end;
end;

constructor TRangeFixture.Create;
var
  LPage: INyxPage;
  LContainer: INyxColumn;
  LNestedRange: INyxDateRange;
begin
  inherited Create;
  Document := TNyxDocument.Create;
  LPage := NewNyxPage('home');
  Document.AddPage(LPage);
  Local := NewNyxDateRange('local-range');
  LPage.Add(Local);
  Definition := NewNyxDateRange('range-definition');
  Definition.StartDate.WithDate(NyxDate(2026, 10, 10));
  Definition.FinishDate.WithDate(NyxDate(2026, 10, 20));
  SetNyxDateRangeOptions(Definition, TNyxDateRangeOptions.Ordered);
  Document.AddComponent(Definition);
  First := NewNyxComponent('first-range');
  First.Reference := NyxComponent('range-definition');
  LPage.Add(First);
  Second := NewNyxComponent('second-range');
  Second.Reference := NyxComponent('range-definition');
  LPage.Add(Second);
  LContainer := NewNyxColumn('container-definition');
  LNestedRange := NewNyxDateRange('nested-range');
  LNestedRange.Configure.PartName(NyxPart('window')).Done;
  SetNyxDateRangeOptions(LNestedRange, TNyxDateRangeOptions.Ordered);
  LContainer.Add(LNestedRange);
  Document.AddComponent(LContainer);
  Nested := NewNyxComponent('nested-instance');
  Nested.Reference := NyxComponent('container-definition');
  Rule := Nested.Node.OverridePart(NyxPart('window')).Named('window-rule');
  LPage.Add(Nested);
end;

destructor TRangeFixture.Destroy;
begin
  Document.Free;
  inherited Destroy;
end;

procedure TThrowingObserver.Changed(AState: TNyxState; AChanges: TNyxStateChanges);
begin
  Inc(Calls);
  raise ENyxState.Create('Deliberate observer failure after date publication');
end;

function ValuesRefused(ADocument: TNyxDocument; AOwner: TNyxNode): Boolean;
var
  LContext: TNyxNode;
  LProjection: TNyxNode;
begin
  LContext := nil;
  Result := False;
  try
    LContext := RealizeNyxContext(ADocument, AOwner, LProjection);
    try
      ValidateNyxDateRangeValues(LProjection);
    except
      on ENyxDateRange do
      begin
        Result := True;
      end;
    end;
  finally
    LContext.Free;
  end;
end;

function OptionsRefused(const AText: TNyxText): Boolean;
var
  LOptions: TNyxDateRangeOptions;
begin
  Result := False;
  try
    LOptions := TNyxDateRangeOptions.FromData(TNyxDataValue.ParseJSON(AText));
    LOptions.Validate;
  except
    on Exception do
    begin
      Result := True;
    end;
  end;
end;

procedure PolicyCases;
const
  CInvalid: array[0..7] of TNyxText = (
    'null', '{}', '{"version":2,"order":"ascending"}',
    '{"version":1,"order":true}', '{"version":1,"order":"Ascending"}',
    '{"version":1,"order":"ascending","extra":0}',
    '{"version":1.0,"order":"ascending"}', '{"version":1,"mode":"ascending"}');
var
  LFixture: TRangeFixture;
  LOptions: TNyxDateRangeOptions;
  LCopy: TNyxDateRangeOptions;
  LBefore: TNyxText;
  LDefinition: TNyxText;
  LSecond: TNyxText;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LWrong: INyxColumn;
  LIndex: Integer;
  LRefused: Boolean;
begin
  LFixture := TRangeFixture.Create;
  try
    LBefore := TNyxCodec.Encode(LFixture.Document);
    Check(not HasNyxDateRangeOptions(LFixture.Local.Node), 'legacy local policy is absent');
    Check(GetNyxDateRangeOptions(LFixture.Local.Node).Order = ndroIndependent,
      'absence reads as independent');
    Check(TNyxCodec.Encode(LFixture.Document) = LBefore, 'reading absence authors no bytes');
    LOptions := TNyxDateRangeOptions.Ordered;
    LCopy := LOptions.Copy;
    LOptions := TNyxDateRangeOptions.Independent;
    Check(LCopy.Order = ndroAscending, 'copied options retain their independent value');
    Check(TNyxDateRangeOptions.FromData(LCopy.ToData).ToData.ToJSON = LCopy.ToData.ToJSON,
      'ordered options have an exact closed wire round trip');
    Check(TNyxDateRangeOptions.FromData(LOptions.ToData).Order = ndroIndependent,
      'explicit independent options round trip separately from absence');
    for LIndex := Low(CInvalid) to High(CInvalid) do
    begin
      Check(OptionsRefused(CInvalid[LIndex]), 'malformed policy refuses: ' + CInvalid[LIndex]);
    end;
    LDefinition := TNyxCodec.NodeSnapshot(LFixture.Definition.Node).ToJSON;
    LSecond := TNyxCodec.NodeSnapshot(LFixture.Second.Node).ToJSON;
    SetNyxDateRangeOptions(LFixture.First, LOptions);
    Check(HasNyxDateRangeOptions(LFixture.First.Node), 'independent is an explicit local mask');
    LContext := RealizeNyxContext(LFixture.Document, LFixture.First.Node, LProjection);
    try
      Check(GetNyxDateRangeOptions(LProjection).Order = ndroIndependent,
        'local independent masks inherited ordering');
    finally
      LContext.Free;
    end;
    InheritNyxDateRangeOptions(LFixture.First);
    Check(not HasNyxDateRangeOptions(LFixture.First.Node), 'inherit removes only the local key');
    LContext := RealizeNyxContext(LFixture.Document, LFixture.First.Node, LProjection);
    try
      Check(GetNyxDateRangeOptions(LProjection).Order = ndroAscending,
        'inherit reveals the ordered definition');
    finally
      LContext.Free;
    end;
    Check((TNyxCodec.NodeSnapshot(LFixture.Definition.Node).ToJSON = LDefinition) and
      (TNyxCodec.NodeSnapshot(LFixture.Second.Node).ToJSON = LSecond),
      'policy masks do not mutate the definition or sibling');
    SetNyxDateRangeOptions(RetainNyxControl(LFixture.Rule), LOptions);
    ValidateNyxDateRangeOptions(LFixture.Rule);
    LContext := RealizeNyxContext(LFixture.Document, LFixture.Rule, LProjection);
    try
      Check(GetNyxDateRangeOptions(LProjection).Order = ndroIndependent,
        'named descriptor overlays the exact range policy');
    finally
      LContext.Free;
    end;
    LWrong := NewNyxColumn('wrong-target');
    SetNyxDateRangeOptions(LWrong, LCopy);
    LRefused := False;
    try
      ValidateNyxDateRangeOptions(LWrong.Node);
    except
      on ENyxDateRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'known policy on an ordinary row/column refuses');
    Check(TNyxCodec.NodeSnapshot(LFixture.Definition.Node).ToJSON = LDefinition,
      'a refused alternate target does not change prior policy values');
  finally
    LFixture.Free;
  end;
end;

procedure ValueCases;
var
  LFixture: TRangeFixture;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LRole: TNyxNode;
  LRefused: Boolean;
begin
  LFixture := TRangeFixture.Create;
  try
    LFixture.Local.StartDate.WithDate(NyxDate(2026, 10, 20));
    LFixture.Local.FinishDate.WithDate(NyxDate(2026, 10, 10));
    Check(not ValuesRefused(LFixture.Document, LFixture.Local.Node),
      'legacy independent dates retain reversed values');
    SetNyxDateRangeOptions(LFixture.Local, TNyxDateRangeOptions.Ordered);
    Check(ValuesRefused(LFixture.Document, LFixture.Local.Node),
      'ordered reversed endpoints refuse');
    LFixture.Local.FinishDate.WithDate(NyxDate(2026, 10, 20));
    Check(not ValuesRefused(LFixture.Document, LFixture.Local.Node), 'equal dates are ordered');
    LFixture.Local.FinishDate.WithDate(NyxDate(2026, 10, 21));
    Check(not ValuesRefused(LFixture.Document, LFixture.Local.Node),
      'ascending dates are admitted');
    LFixture.Local.StartDate.WithDate(NyxNoDate);
    Check(not ValuesRefused(LFixture.Document, LFixture.Local.Node), 'empty start is not guessed');
    LFixture.Local.StartDate.WithDate(NyxDate(2026, 10, 20));
    LFixture.Local.FinishDate.WithDate(NyxNoDate);
    Check(not ValuesRefused(LFixture.Document, LFixture.Local.Node), 'empty finish is not guessed');
    LFixture.Local.StartDate.WithDate(NyxNoDate);
    Check(not ValuesRefused(LFixture.Document, LFixture.Local.Node), 'both dates may remain empty');
    LRefused := False;
    try
      ValidateNyxDateRangeValues(LFixture.Local.Node);
    except
      on ENyxDateRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'raw defaults cannot stand in for projected value admission');
    LContext := RealizeNyxContext(LFixture.Document, LFixture.Local.Node, LProjection);
    try
      LRole := LProjection.Part(NyxPart('finish'));
      LRole.Configure.ProjectAs(nkInput).Done;
      LRefused := False;
      try
        ValidateNyxDateRangeValues(LProjection);
      except
        on ENyxDateRange do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'retyped endpoint refuses instead of admitting a text lookalike');
    finally
      LContext.Free;
    end;
    LContext := RealizeNyxContext(LFixture.Document, LFixture.Local.Node, LProjection);
    try
      LRole := LProjection.Part(NyxPart('finish'));
      LProjection.Remove(LRole);
      LRefused := False;
      try
        ValidateNyxDateRangeValues(LProjection);
      except
        on ENyxDateRange do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'missing direct endpoint refuses');
    finally
      LContext.Free;
    end;
  finally
    LFixture.Free;
  end;
end;

function FormRoot(ADocument: TNyxDocument; const AOwner: TNyxControlRef): TNyxNode;
var
  LShell: TNyxDocument;
  LPage: INyxPage;
begin
  LShell := TNyxDocument.Create;
  try
    LPage := NewNyxPage('shell');
    LShell.AddPage(LPage);
    LPage.Add(NewNyxDateRangeEditor('range-editor', ADocument, AOwner));
    Result := RealizeNyxView(LShell, LPage.Node);
  finally
    LShell.Free;
  end;
end;

function CaptureRefused(ARoot: TNyxNode; ADocument: TNyxDocument;
  AAction: TNyxDateRangeEditorAction): Boolean;
var
  LChange: TNyxDateRangeEditorChange;
begin
  Result := False;
  try
    CaptureNyxDateRangeEditor(ARoot.Find(NyxDateRangeEditorActionID('range-editor', AAction)),
      ARoot, ADocument, LChange);
  except
    on Exception do
    begin
      Result := True;
    end;
  end;
end;

procedure EditorCases;
var
  LFixture: TRangeFixture;
  LRoot: TNyxNode;
  LOther: TNyxNode;
  LAction: TNyxNode;
  LChange: TNyxDateRangeEditorChange;
  LCopy: TNyxDateRangeEditorChange;
  LRead: TNyxDateRangeEditorChange;
  LOptions: TNyxDateRangeOptions;
  LBefore: TNyxText;
  LRefused: Boolean;
  LIndex: Integer;
  LValue: TNyxDataValue;
begin
  LFixture := TRangeFixture.Create;
  LRoot := nil;
  LOther := nil;
  try
    LBefore := TNyxCodec.Encode(LFixture.Document);
    LRoot := FormRoot(LFixture.Document, NyxControl('local-range'));
    LAction := LRoot.Find(NyxDateRangeEditorActionID('range-editor', ndraOrdered));
    Check(CaptureNyxDateRangeEditor(LAction, LRoot, LFixture.Document, LChange),
      'public ordered action captures its copied proposal');
    Check((LChange.Owner.ID = 'local-range') and not LChange.Inherit and
      (LChange.Options.Order = ndroAscending), 'capture exposes typed owner and ordered options');
    LCopy := LChange.Copy;
    LRead := TNyxDateRangeEditorChange.FromData(LChange.ToData);
    Check(LRead.ToData.ToJSON = LChange.ToData.ToJSON,
      'editor wire has an exact closed round trip');
    ValidateNyxDateRangeEditorChange(LFixture.Document, LRead);
    Check(TNyxCodec.Encode(LFixture.Document) = LBefore,
      'form creation/capture/admission mutate no authored document');
    Check(CaptureRefused(LRoot, LFixture.Document, ndraInherit),
      'inherit is disabled when no local policy exists');
    Check(not CaptureNyxDateRangeEditor(LRoot.Find('range-editor-title'), LRoot,
      LFixture.Document, LRead), 'unrelated form content is inert');
    LOther := FormRoot(LFixture.Document, NyxControl('local-range'));
    LRefused := False;
    try
      CaptureNyxDateRangeEditor(LAction, LOther, LFixture.Document, LRead);
    except
      on ENyxDateRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'same-ID foreign mounted form refuses');
    FreeAndNil(LOther);
    LFixture.Local.SeparatorLabel.WithText('through');
    LRefused := False;
    try
      ValidateNyxDateRangeEditorChange(LFixture.Document, LCopy);
    except
      on ENyxDateRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and CaptureRefused(LRoot, LFixture.Document, ndraOrdered),
      'exact raw/effective baseline rejects a changed range');
    Check(LCopy.ToData.ToJSON = LChange.ToData.ToJSON,
      'prior copied proposal remains readable after stale refusal');
    FreeAndNil(LRoot);
    LRoot := FormRoot(LFixture.Document, NyxControl('first-range'));
    Check(CaptureNyxDateRangeEditor(LRoot.Find(NyxDateRangeEditorActionID(
      'range-editor', ndraIndependent)), LRoot, LFixture.Document, LRead) and
      (LRead.Options.Order = ndroIndependent), 'inherited ordered range offers an explicit mask');
    Check(CaptureRefused(LRoot, LFixture.Document, ndraInherit),
      'inherited policy alone is not a removable local value');
    SetNyxDateRangeOptions(LFixture.First, TNyxDateRangeOptions.Independent);
    FreeAndNil(LRoot);
    LRoot := FormRoot(LFixture.Document, NyxControl('first-range'));
    Check(CaptureNyxDateRangeEditor(LRoot.Find(NyxDateRangeEditorActionID(
      'range-editor', ndraInherit)), LRoot, LFixture.Document, LRead) and LRead.Inherit,
      'an explicit mask offers typed inheritance');
    Check(TNyxDateRangeEditorChange.FromData(LRead.ToData).Inherit,
      'inherit serializes as its explicit action without undefined options');
    LRefused := False;
    try
      LOptions := LRead.Options;
      LOptions.Validate;
    except
      on ENyxDateRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'inherit cannot be mistaken for local independent options');
    FreeAndNil(LRoot);
    LRoot := FormRoot(LFixture.Document, NyxControl('window-rule'));
    Check(CaptureNyxDateRangeEditor(LRoot.Find(NyxDateRangeEditorActionID(
      'range-editor', ndraOrdered)), LRoot, LFixture.Document, LRead) and
      (LRead.Owner.ID = 'window-rule'), 'named-part form retains exact descriptor ownership');
    for LIndex := 0 to 4 do
    begin
      case LIndex of
        0:
          begin
            LValue := NyxObject([NyxField('version', NyxData(2)),
              NyxField('owner', NyxData('local-range')),
              NyxField('action', NyxData('ordered')),
              NyxField('baseline', NyxData(LChange.Baseline))]);
          end;
        1:
          begin
            LValue := NyxObject([NyxField('version', NyxData(1)),
              NyxField('owner', NyxData('')),
              NyxField('action', NyxData('ordered')),
              NyxField('baseline', NyxData(LChange.Baseline))]);
          end;
        2:
          begin
            LValue := NyxObject([NyxField('version', NyxData(1)),
              NyxField('owner', NyxData('local-range')),
              NyxField('action', NyxData('Ordered')),
              NyxField('baseline', NyxData(LChange.Baseline))]);
          end;
        3:
          begin
            LValue := NyxObject([NyxField('version', NyxData(1)),
              NyxField('owner', NyxData('local-range')),
              NyxField('action', NyxData('ordered')),
              NyxField('baseline', NyxData(''))]);
          end;
        4:
          begin
            LValue := NyxObject([NyxField('version', NyxData(1)),
              NyxField('owner', NyxData('local-range')),
              NyxField('action', NyxData('ordered')),
              NyxField('baseline', NyxData(LChange.Baseline)),
              NyxField('extra', NyxNull)]);
          end;
      end;
      LRefused := False;
      try
        LRead := TNyxDateRangeEditorChange.FromData(LValue);
      except
        on Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'malformed copied change refuses case ' + TNyxText(IntToStr(LIndex)));
    end;
    LAction := LRoot.Find(NyxDateRangeEditorActionID('range-editor', ndraIndependent));
    LAction.Configure.ProjectAs(nkLabel).Done;
    Check(CaptureRefused(LRoot, LFixture.Document, ndraOrdered),
      'malformed sibling action refuses the incomplete form');
  finally
    LOther.Free;
    LRoot.Free;
    LFixture.Free;
  end;
  Check((LCopy.Owner.ID = 'local-range') and
    (LCopy.Options.Order = ndroAscending) and (LCopy.Baseline <> ''),
    'copied proposal remains useful after its original document and forms retire');
end;

procedure UnboundCases;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LRange: INyxDateRange;
  LRoot: TNyxNode;
  LRuntime: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LBefore: TNyxText;
  LRevision: Integer;
  LRefused: Boolean;
begin
  LDocument := TNyxDocument.Create;
  LRoot := nil;
  LStore := nil;
  LLive := nil;
  try
    LPage := NewNyxPage('unbound');
    LDocument.AddPage(LPage);
    LRange := NewNyxDateRange('window');
    LPage.Add(LRange);
    LRange.StartDate.WithDate(NyxDate(2026, 10, 10));
    LRange.StartDate.Contract.Value(NyxDateDomain.Range(
      NyxDate(2026, 10, 1), NyxDate(2026, 10, 31)));
    LRange.FinishDate.WithDate(NyxDate(2026, 10, 20));
    SetNyxDateRangeOptions(LRange, TNyxDateRangeOptions.Ordered);
    ValidateNyxDocumentProperties(LDocument);
    LRoot := RealizeNyxView(LDocument, LPage.Node);
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LLive.Activate;
    LRuntime := LRoot.Find('window');
    LBefore := TNyxCodec.NodeSnapshot(LRoot).ToJSON;
    LRevision := LStore.Revision;
    LRefused := False;
    try
      LLive.Edit(LRuntime.Part(NyxPart('start')), '2026-10-21');
    except
      on ENyxDateRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.NodeSnapshot(LRoot).ToJSON = LBefore) and
      (LStore.Revision = LRevision), 'unbound reversed edit refuses before projection publication');
    LLive.Edit(LRuntime.Part(NyxPart('start')), '2026-10-11');
    Check((LRuntime.Part(NyxPart('start')).Prop('value') = '2026-10-11') and
      (LStore.Revision = LRevision), 'unbound valid edit publishes without manufacturing state');
    LLive.Edit(LRuntime.Part(NyxPart('finish')), '');
    Check(LRuntime.Part(NyxPart('finish')).Prop('value') = '',
      'unbound one-sided range keeps an empty finish');
    LBefore := TNyxCodec.NodeSnapshot(LRoot).ToJSON;
    LRefused := False;
    try
      LLive.Edit(LRuntime.Part(NyxPart('start')), '2026-11-01');
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.NodeSnapshot(LRoot).ToJSON = LBefore),
      'ordering does not weaken a field domain when the other endpoint is empty');
    Check(LRange.StartDate.DateValue.Compare(NyxDate(2026, 10, 10)) = 0,
      'runtime edits leave the original authored date untouched');
  finally
    LLive.Free;
    LRoot.Free;
    LStore.Free;
    LDocument.Free;
  end;
end;

procedure BoundCases(ASharedKey: Boolean);
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LRange: INyxDateRange;
  LRoot: TNyxNode;
  LRuntime: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LPrepared: INyxPreparedPublication;
  LObserver: TThrowingObserver;
  LSubscription: TNyxStateSubscription;
  LFinishKey: TNyxTextStateRef;
  LRevision: Integer;
  LBefore: TNyxText;
  LRefused: Boolean;
  LNotified: Boolean;
begin
  LDocument := TNyxDocument.Create;
  LRoot := nil;
  LStore := nil;
  LLive := nil;
  LPrepared := nil;
  LSubscription := nil;
  LObserver := nil;
  try
    LPage := NewNyxPage('bound');
    LDocument.AddPage(LPage);
    LRange := NewNyxDateRange('window');
    LPage.Add(LRange);
    SetNyxDateRangeOptions(LRange, TNyxDateRangeOptions.Ordered);
    { These defaults are deliberately reversed: complete state projection,
      rather than the raw intermediate authoring tree, owns relationship values. }
    LRange.StartDate.WithDate(NyxDate(2026, 10, 30));
    LRange.FinishDate.WithDate(NyxDate(2026, 10, 1));
    LDocument.State.SetValue(NyxTextState('start'), '2026-10-10');
    LFinishKey := NyxTextState('finish');

    if ASharedKey then
    begin
      LFinishKey := NyxTextState('start');
    end
    else
    begin
      LDocument.State.SetValue(LFinishKey, '2026-10-20');
    end;
    LRange.StartDate.Binds.Value(NyxTextState('start'), bdTwoWay).Done;
    LRange.FinishDate.Binds.Value(LFinishKey, bdTwoWay).Done;
    ValidateNyxDocumentProperties(LDocument);
    LRoot := RealizeNyxView(LDocument, LPage.Node);
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LLive.Activate;
    LRuntime := LRoot.Find('window');
    Check((LRuntime.Part(NyxPart('start')).Prop('value') = '2026-10-10') and
      (LRuntime.Part(NyxPart('finish')).Prop('value') =
        LStore.GetValue(LFinishKey)), 'complete bound values supersede reversed raw defaults');
    LRevision := LStore.Revision;

    if ASharedKey then
    begin
      LLive.Edit(LRuntime.Part(NyxPart('start')), '2026-10-21');
      Check((LStore.Revision = LRevision + 1) and
        (LRuntime.Part(NyxPart('start')).Prop('value') = '2026-10-21') and
        (LRuntime.Part(NyxPart('finish')).Prop('value') = '2026-10-21'),
        'shared-key edit compares the complete equal projected pair');
      LPrepared := LStore.PrepareApply([
        NyxStateValue(NyxTextState('start'), '2026-10-22')], LStore.Revision);
      PublishNyxGroup([LPrepared]);
      LPrepared := nil;
      Check((LRuntime.Part(NyxPart('start')).Prop('value') = '2026-10-22') and
        (LRuntime.Part(NyxPart('finish')).Prop('value') = '2026-10-22'),
        'prepared shared-key publication retains equal endpoint meaning');
    end
    else
    begin
      LBefore := TNyxCodec.NodeSnapshot(LRoot).ToJSON;
      LRefused := False;
      try
        LLive.Edit(LRuntime.Part(NyxPart('start')), '2026-10-21');
      except
        on ENyxDateRange do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LStore.Revision = LRevision) and
        (TNyxCodec.NodeSnapshot(LRoot).ToJSON = LBefore),
        'bound reversed edit leaves state revision and projection exact');
      LStore.Apply([NyxStateValue(NyxTextState('start'), '2026-10-30'),
        NyxStateValue(LFinishKey, '2026-10-31')], LRevision);
      Check((LStore.Revision = LRevision + 1) and
        (LRuntime.Part(NyxPart('start')).Prop('value') = '2026-10-30') and
        (LRuntime.Part(NyxPart('finish')).Prop('value') = '2026-10-31'),
        'multi-key Apply admits the final pair instead of intermediate endpoints');
      LRevision := LStore.Revision;
      LBefore := TNyxCodec.NodeSnapshot(LRoot).ToJSON;
      LPrepared := LStore.PrepareApply([
        NyxStateValue(NyxTextState('start'), '2026-10-12'),
        NyxStateValue(LFinishKey, '2026-10-11')], LRevision);
      LRefused := False;
      try
        PublishNyxGroup([LPrepared]);
      except
        on ENyxDateRange do
        begin
          LRefused := True;
        end;
      end;
      LPrepared := nil;
      Check(LRefused and not LStore.Busy and (LStore.Revision = LRevision) and
        (TNyxCodec.NodeSnapshot(LRoot).ToJSON = LBefore),
        'reversed prepared pair refuses atomically and retires its reservation');
      LPrepared := LStore.PrepareApply([
        NyxStateValue(NyxTextState('start'), '2026-10-05'),
        NyxStateValue(LFinishKey, '2026-10-06')], LRevision);
      PublishNyxGroup([LPrepared]);
      LPrepared := nil;
      Check((LStore.Revision = LRevision + 1) and
        (LRuntime.Part(NyxPart('start')).Prop('value') = '2026-10-05') and
        (LRuntime.Part(NyxPart('finish')).Prop('value') = '2026-10-06'),
        'valid prepared pair installs together before observers');
      LObserver := TThrowingObserver.Create;
      LSubscription := LStore.Subscribe(LObserver.Changed);
      LRevision := LStore.Revision;
      LNotified := False;
      try
        LStore.Apply([NyxStateValue(NyxTextState('start'), '2026-10-07'),
          NyxStateValue(LFinishKey, '2026-10-08')], LRevision);
      except
        on ENyxStateNotification do
        begin
          LNotified := True;
        end;
      end;
      Check(LNotified and (LObserver.Calls = 1) and
        (LStore.Revision = LRevision + 1) and
        (LRuntime.Part(NyxPart('start')).Prop('value') = '2026-10-07') and
        (LRuntime.Part(NyxPart('finish')).Prop('value') = '2026-10-08'),
        'observer failure reports a committed pair without pretending rejection');
    end;
    Check((LRange.StartDate.DateValue.Compare(NyxDate(2026, 10, 30)) = 0) and
      (LRange.FinishDate.DateValue.Compare(NyxDate(2026, 10, 1)) = 0),
      'accepted runtime projection never writes authored fallback values');
  finally
    LSubscription.Free;
    LObserver.Free;
    LPrepared := nil;
    LLive.Free;
    LRoot.Free;
    LStore.Free;
    LDocument.Free;
  end;
end;

begin
  try
    PolicyCases;
    ValueCases;
    EditorCases;
    UnboundCases;
    BoundCases(False);
    BoundCases(True);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-date-range', 'passed');
    document.body.setAttribute('data-date-range-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed date-range policy/editor checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-date-range', 'failed');
      document.body.setAttribute('data-date-range-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
