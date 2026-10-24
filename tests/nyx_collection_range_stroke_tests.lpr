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
program nyx_collection_range_stroke_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.state, nyx.collections, nyx.collections.view,
  nyx.collections.view.types, nyx.collections.query, nyx.collections.arrangement,
  nyx.collections.clipboard, nyx.collections.range, nyx.collections.range.actions,
  nyx.collections.range.pointer, nyx.collections.range.stroke, nyx.scheduler
  {$ifdef PAS2JS}, Web{$endif};

type
  { One real independent store/view supplies immutable filtered page data.
    Detached previews must permit actual subscription retirement at the end. }
  TFixture = class
  public
    Store: INyxCollection;
    View: INyxCollectionView;
    StoreToken: INyxCollectionSubscription;
    ViewToken: INyxCollectionViewSubscription;
    Source: INyxCollectionSnapshot;
    Page: INyxCollectionSnapshot;
    Fields: TNyxCollectionClipboardFields;
    State: INyxCollectionCellRangeSelection;
    Scope: INyxCancellationScope;
    Ready: INyxCancellationReadiness;
    Pointer: TNyxCollectionRangeStrokePointer;
    Input: TNyxCollectionRangeStrokeInput;
    constructor Create;
    destructor Destroy; override;
    procedure StoreChanged(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
    procedure ViewChanged(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
    function Accept: INyxCollectionCellRangeIntent;
    function Stroke(const APage: INyxCollectionSnapshot;
      const ALimits: TNyxCollectionClipboardLimits): INyxCollectionRangeStroke;
    procedure ReleaseOwners;
  end;

  { One genuine getter may enter a newer move, cancel or withdraw caller-owned
    ports. It is borrowed only until this synchronous read returns. }
  TReadCallback = procedure of object;

  { An immutable real page delegates every row and exposes one Count callout.
    Its retained reference owns no real view, store, mount or widget. }
  TReadPage = class(TInterfacedObject, INyxCollectionSnapshot)
  public
    Inner: INyxCollectionSnapshot;
    OnRead: TReadCallback;
    Calls: Integer;
    function GetKey: TNyxCollectionRef;
    function GetSchema: TNyxCollectionSchema;
    function GetCount: Integer;
    function GetRevision: Integer;
    function GetDataBytes: Integer;
    function ItemAt(AIndex: Integer): TNyxCollectionItem;
    function Item(const ARef: TNyxItemRef): TNyxCollectionItem;
    function IndexOf(const ARef: TNyxItemRef): Integer;
    function Has(const ARef: TNyxItemRef): Boolean;
  end;

  { Public-only reentry actor. A stroke owns no actor callback; withdrawal
    removes the caller's LAST selection/stroke ports during a real getter. }
  TProbe = class
  public
    Stroke: INyxCollectionRangeStroke;
    State: INyxCollectionCellRangeSelection;
    Source: INyxCollectionSnapshot;
    Page: INyxCollectionSnapshot;
    Pointer: TNyxCollectionRangeStrokePointer;
    Input: TNyxCollectionRangeStrokeInput;
    Scope: INyxCancellationScope;
    Newer: Boolean;
    procedure Move;
    procedure Cancel;
    procedure Withdraw;
    procedure InvalidateInput;
    procedure PoisonTuple;
  end;

const
  CFull: TNyxText = 'whole 🌙' + #9 + '"quoted"' + #13#10 + 'tail' + #0 + 'end';

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

function Dataset: TNyxCollectionRef;
begin
  Result := NyxCollection('stroke source 🌙');
end;

function Cell(const AID: TNyxText; AScore: Boolean = False): TNyxCollectionCellIdentity;
var
  LField: TNyxColumnIdentity;
begin
  LField := NyxColumnIdentity(NyxTextField('Name'));

  if AScore then
  begin
    LField := NyxColumnIdentity(NyxIntegerField('Score'));
  end;
  Result := NyxCollectionCellIdentity(NyxItem(Dataset, AID), LField);
end;

function Facts(const AFocus: TNyxCollectionCellIdentity; AEpoch: Integer = 1;
  AMutation: Integer = 0; AColumns: Integer = 7; AReady: Boolean = True): TNyxCollectionRangeStrokeInput;
begin
  Result := NyxCollectionRangeStrokeInput(AReady, AEpoch, AMutation, AColumns, AFocus);
end;

constructor TFixture.Create;
var
  LReady: INyxCancellationReadiness;
begin
  inherited Create;
  Store := NewNyxCollection(Dataset, NyxCollectionSchema.Text(NyxTextField('Name'), '')
    .Integer(NyxIntegerField('Score'), 0), [
      NyxCollectionItem(NyxItem(Dataset, 'a')).WithValue(NyxTextField('Name'), CFull)
        .WithValue(NyxIntegerField('Score'), 2),
      NyxCollectionItem(NyxItem(Dataset, 'b')).WithValue(NyxTextField('Name'), 'B')
        .WithValue(NyxIntegerField('Score'), 3),
      NyxCollectionItem(NyxItem(Dataset, 'c🌙')).WithValue(NyxTextField('Name'), 'C')
        .WithValue(NyxIntegerField('Score'), 4),
      NyxCollectionItem(NyxItem(Dataset, 'off page')).WithValue(NyxIntegerField('Score'), 1)]);
  View := NewNyxCollectionView(Store, NyxCollectionView(Dataset)
    .Column(NyxTextField('Name'), 'Name').Column(NyxIntegerField('Score'), 'Score'), cpTable);
  View.ConfigureQuery(NyxCollectionQuery.Where(NyxWhere(NyxIntegerField('Score')).AtLeast(2))
    .OrderBy(NyxIntegerField('Score'), nsdDescending));
  Source := Store.Snapshot;
  Page := View.Snapshot;
  StoreToken := Store.Subscribe({$ifdef PAS2JS}@{$endif}StoreChanged);
  ViewToken := View.Subscribe({$ifdef PAS2JS}@{$endif}ViewChanged);
  Fields := [NyxColumnIdentity(NyxTextField('Name')), NyxColumnIdentity(NyxIntegerField('Score'))];
  State := NewNyxCollectionCellRangeSelection;
  Scope := NewNyxCancellationScope;
  Check(Supports(Scope, INyxCancellationReadiness, LReady), 'canonical cancellation supplies flags-only readiness');
  Ready := LReady;
  Pointer := NyxCollectionRangeStrokePointer(ncrsdMouse, 0);
  Input := Facts(Cell('a'));
end;

destructor TFixture.Destroy;
begin
  State := nil;
  StoreToken.Disconnect;
  ViewToken.Disconnect;
  ReleaseOwners;
  inherited Destroy;
end;

procedure TFixture.StoreChanged(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
begin
  Check(False, 'stroke must never mutate accepted source data');
end;

procedure TFixture.ViewChanged(const AView: INyxCollectionView; const AChanges: INyxCollectionChanges);
begin
  Check(False, 'stroke must never publish query or ordinary row selection');
end;

procedure TFixture.ReleaseOwners;
begin
  View := nil;
  Store := nil;
end;

function TFixture.Accept: INyxCollectionCellRangeIntent;
begin
  Result := State.Select(Source, Page, Fields, Cell('a'), Cell('a'), 7,
    False, nccleLF, TNyxCollectionClipboardLimits.Defaults);
end;

function TFixture.Stroke(const APage: INyxCollectionSnapshot;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionRangeStroke;
begin
  Result := NewNyxCollectionRangeStroke(State, Ready, Pointer, Source, APage,
    Fields, Cell('a'), Cell('b', True), ncrpReplace, Input, False, nccleCRLF, ALimits);
end;

function TReadPage.GetKey: TNyxCollectionRef;
begin
  Result := Inner.Key;
end;

function TReadPage.GetSchema: TNyxCollectionSchema;
begin
  Result := Inner.Schema;
end;

function TReadPage.GetCount: Integer;
var
  LReply: TReadCallback;
begin
  Inc(Calls);
  LReply := OnRead;
  OnRead := nil;

  if Assigned(LReply) then
  begin
    LReply;
  end;
  Result := Inner.Count;
end;

function TReadPage.GetRevision: Integer;
begin
  Result := Inner.Revision;
end;

function TReadPage.GetDataBytes: Integer;
begin
  Result := Inner.DataBytes;
end;

function TReadPage.ItemAt(AIndex: Integer): TNyxCollectionItem;
begin
  Result := Inner.ItemAt(AIndex);
end;

function TReadPage.Item(const ARef: TNyxItemRef): TNyxCollectionItem;
begin
  Result := Inner.Item(ARef);
end;

function TReadPage.IndexOf(const ARef: TNyxItemRef): Integer;
begin
  Result := Inner.IndexOf(ARef);
end;

function TReadPage.Has(const ARef: TNyxItemRef): Boolean;
begin
  Result := Inner.Has(ARef);
end;

procedure TProbe.Move;
begin
  Newer := Stroke.Stage(Pointer, Source, Page, Input, Cell('c🌙', True));
end;

procedure TProbe.Cancel;
begin
  Stroke.Cancel;
end;

procedure TProbe.Withdraw;
begin
  Stroke := nil;
  State := nil;
end;

procedure TProbe.InvalidateInput;
begin
  Scope.Cancel;
end;

procedure TProbe.PoisonTuple;
begin
  Source := nil;
  Page := nil;
  Pointer := NyxCollectionRangeStrokePointer(ncrsdTouch, 91);
  Input := Facts(Cell('c🌙'), 2);
end;

{ Bounded construction releases compiler interface temporaries before the
  last-owner withdrawal journey. No pure getter is gratuitously wrapped. }
procedure CreateProbe(const AFixture: TFixture; AProbe: TProbe);
begin
  AProbe.State := NewNyxCollectionCellRangeSelection;
  AProbe.State.Select(AProbe.Source, AProbe.Page, AFixture.Fields, Cell('a'), Cell('a'),
    7, False, nccleLF, TNyxCollectionClipboardLimits.Defaults);
  AProbe.Stroke := NewNyxCollectionRangeStroke(AProbe.State, AFixture.Ready,
    AProbe.Pointer, AProbe.Source, AProbe.Page, AFixture.Fields, Cell('a'),
    Cell('b', True), ncrpReplace, AProbe.Input, False, nccleCRLF,
    TNyxCollectionClipboardLimits.Defaults);
end;

function EnterWithWithdrawal(AProbe: TProbe): Boolean;
begin
  Result := AProbe.Stroke.Stage(AProbe.Pointer, AProbe.Source, AProbe.Page,
    AProbe.Input, Cell('b', True));
end;

procedure Run;
var
  LFixture: TFixture;
  LStroke: INyxCollectionRangeStroke;
  LRelease: INyxCollectionRangeStrokeRelease;
  LOld: INyxCollectionCellRangeIntent;
  LPreview: INyxCollectionCellRangeCapture;
  LOther: TNyxCollectionRangeStrokePointer;
  LWrong: TNyxCollectionRangeStrokeInput;
  LPage: INyxCollectionSnapshot;
  LSourceProxy: INyxCollectionSnapshot;
  LProxy: TReadPage;
  LProbe: TProbe;
  LLimits: TNyxCollectionClipboardLimits;
  LCase: Integer;
  LOrdinal: Integer;
  LRefused: Boolean;
begin
  LFixture := TFixture.Create;
  LProbe := TProbe.Create;
  try
    LOld := LFixture.Accept;
    LStroke := LFixture.Stroke(LFixture.Page, TNyxCollectionClipboardLimits.Defaults);
    Check(LStroke.Current and (LStroke.Status = ncrssPending) and LOld.Current and
      (LFixture.State.Selection = LOld), 'initial pending preview changes no accepted range');
    LPreview := LStroke.Preview;
    Check((LPreview.Payload.RowCount = 2) and (LPreview.Payload.FieldCount = 2) and
      (LPreview.Payload.ItemAt(0).ID = 'b') and (LPreview.Payload.CellValue(1, 0).TextValue = CFull),
      'pending preview is exact filtered-page RAW Unicode NUL TSV from the same range producer');
    LOther := NyxCollectionRangeStrokePointer(ncrsdTouch, 0);
    Check(not LStroke.Stage(LOther, LFixture.Source, LFixture.Page, LFixture.Input, Cell('c🌙', True)) and
      (LStroke.PrepareRelease(LOther, LFixture.Source, LFixture.Page, LFixture.Input) = nil),
      'different device with the same numeric pointer ID cannot move or release this stroke');
    LStroke.LostCapture(LOther);
    Check(LStroke.Current and LOld.Current, 'foreign capture loss leaves the original pending stroke intact');
    LFixture.Fields[0] := NyxColumnIdentity(NyxIntegerField('Score'));
    Check(LStroke.Stage(LFixture.Pointer, LFixture.Source, LFixture.Page,
      LFixture.Input, Cell('c🌙', True)) and (LStroke.Preview.Payload.RowCount = 3) and
      LStroke.Preview.Payload.FieldAt(0).Same(Cell('a').Field) and
      (LPreview.Payload.RowCount = 2) and LOld.Current,
      'move replaces only the pending full candidate and keeps prior detached preview and accepted range exact');
    LFixture.Fields[0] := NyxColumnIdentity(NyxTextField('Name'));
    LRelease := LStroke.PrepareRelease(LFixture.Pointer, LFixture.Source, LFixture.Page, LFixture.Input);
    Check((LRelease <> nil) and not LStroke.Stage(LFixture.Pointer, LFixture.Source,
      LFixture.Page, LFixture.Input, Cell('b', True)), 'matching release reserves its exact candidate against late moves');
    Check(LRelease.Complete(True, LFixture.Source, LFixture.Page, Facts(Cell('c🌙', True))) and
      not LRelease.Complete(True, LFixture.Source, LFixture.Page, Facts(Cell('c🌙', True))) and
      (LStroke.Status = ncrssCommitted) and not LStroke.Current and not LOld.Current and
      LFixture.State.Selection.Capture.Focus.Same(Cell('c🌙', True)),
      'matching unconsumed release commits once only after exact target focus');
    LRelease.Retire;
    LStroke.Cancel;
    Check(LFixture.State.Selection.Current and (LStroke.Status = ncrssCommitted),
      'postcommit cancellation/retirement never rolls back the accepted generation');
    for LCase := 0 to 9 do
    begin
      LRelease := nil;
      LStroke := nil;
      LOld := LFixture.Accept;
      LStroke := LFixture.Stroke(LFixture.Page, TNyxCollectionClipboardLimits.Defaults);
      case LCase of
        0:
          begin
            LStroke.Cancel;
          end;
        1:
          begin
            LStroke.LostCapture(LFixture.Pointer);
          end;
        2:
          begin
            LRelease := LStroke.PrepareRelease(LFixture.Pointer, LFixture.Source, LFixture.Page, LFixture.Input);
            Check(not LRelease.Complete(False, LFixture.Source, LFixture.Page, LFixture.Input) and
              not LRelease.Complete(True, LFixture.Source, LFixture.Page, Facts(Cell('b', True))),
              'consumed-first release cannot later borrow allowed handling/focus');
          end;
        3:
          begin
            LRelease := LStroke.PrepareRelease(LFixture.Pointer, LFixture.Source, LFixture.Page, LFixture.Input);
            Check(not LRelease.Complete(True, LFixture.Source, LFixture.Page, LFixture.Input),
              'release without the exact endpoint focus refuses');
          end;
        4, 5, 6, 7:
          begin
            LWrong := Facts(Cell('a'), 2);

            if LCase = 5 then
            begin
              LWrong := Facts(Cell('a'), 1, 1);
            end;

            if LCase = 6 then
            begin
              LWrong := Facts(Cell('a'), 1, 0, 8);
            end;

            if LCase = 7 then
            begin
              LWrong := Facts(Cell('a'), 1, 0, 7, False);
            end;
            Check(not LStroke.Stage(LFixture.Pointer, LFixture.Source, LFixture.Page,
              LWrong, Cell('c🌙', True)) and (LStroke.Status = ncrssStale),
              'epoch/mutation/columns/readiness changes retire original pending input');
          end;
        8:
          begin
            LRelease := LStroke.PrepareRelease(LFixture.Pointer, LFixture.Source, LFixture.Page, LFixture.Input);
            LRelease.Retire;
          end;
        9:
          begin
            LRelease := LStroke.PrepareRelease(LFixture.Pointer, LFixture.Source, LFixture.Page, LFixture.Input);
            Check(not LRelease.Complete(True, LFixture.Source, LFixture.Page, Facts(Cell('b', True), 2)) and
              (LStroke.Status = ncrssStale),
              'epoch changes during canonical release/focus defeat even matching endpoint facts');
          end;
      end;
      Check(not LStroke.Current and LOld.Current and (LFixture.State.Selection = LOld),
        'cancel/lost/consumed/stale/abandoned lifecycle preserves exact accepted old range');
      LStroke.Retire;
    end;
    LOld := LFixture.Accept;
    LStroke := LFixture.Stroke(LFixture.Page, TNyxCollectionClipboardLimits.Defaults);
    LProxy := TReadPage.Create;
    LProxy.Inner := LFixture.Source;
    LSourceProxy := LProxy;
    Check(not LStroke.Stage(LFixture.Pointer, LSourceProxy, LFixture.Page, LFixture.Input, Cell('b')) and
      (LProxy.Calls = 0) and LOld.Current,
      'byte-identical replacement source cannot lend its identity to original pending input');
    LStroke := nil;
    LOld := LFixture.Accept;
    LStroke := LFixture.Stroke(LFixture.Page, TNyxCollectionClipboardLimits.Defaults);
    LProxy := TReadPage.Create;
    LProxy.Inner := LFixture.Page;
    LPage := LProxy;
    Check(not LStroke.Stage(LFixture.Pointer, LFixture.Source, LPage, LFixture.Input, Cell('b')) and
      (LProxy.Calls = 0) and LOld.Current,
      'byte-identical replacement page refuses before any foreign page getter');
    LStroke := nil;
    LOld := LFixture.Accept;
    LStroke := LFixture.Stroke(LPage, TNyxCollectionClipboardLimits.Defaults);
    LProbe.Source := LFixture.Source;
    LProbe.Page := LPage;
    LProbe.Pointer := LFixture.Pointer;
    LProbe.Input := LFixture.Input;
    LProbe.Stroke := LStroke;
    LProxy.OnRead := LProbe.PoisonTuple;
    Check(EnterWithWithdrawal(LProbe) and LStroke.Current and
      LStroke.Preview.Focus.Same(Cell('b', True)) and
      (LProbe.Source = nil) and (LProbe.Page = nil) and LOld.Current,
      'move copies the complete entered tuple before a real getter poisons caller source/page/input/device fields');
    LProbe.Source := LFixture.Source;
    LProbe.Page := LPage;
    LProbe.Pointer := LFixture.Pointer;
    LProbe.Input := LFixture.Input;
    LProxy.OnRead := LProbe.Move;
    Check(not LStroke.Stage(LFixture.Pointer, LFixture.Source, LPage, LFixture.Input, Cell('b')) and
      LProbe.Newer and LStroke.Current and LStroke.Preview.Focus.Same(Cell('c🌙', True)) and LOld.Current,
      'nested newer move from a real page getter survives the older candidate return');
    LProxy.OnRead := LProbe.Cancel;
    Check(not LStroke.Stage(LFixture.Pointer, LFixture.Source, LPage, LFixture.Input, Cell('b')) and
      (LStroke.Status = ncrssCancelled) and LOld.Current,
      'real getter cancellation cannot publish or revive the older pending candidate');
    LProbe.Stroke := nil;
    LStroke := nil;
    CreateProbe(LFixture, LProbe);
    LOld := LProbe.State.Selection;
    LProbe.Stroke.Cancel;
    LProbe.State := nil;
    Check(not LOld.Current and not LProbe.Stroke.Current,
      'retained cancelled stroke releases both pure owner ports without waiting for its own destruction');
    LProbe.Stroke := nil;
    CreateProbe(LFixture, LProbe);
    LOld := LProbe.State.Selection;
    LProxy.OnRead := LProbe.Withdraw;
    Check(EnterWithWithdrawal(LProbe) and (LProbe.Stroke = nil) and (LProbe.State = nil) and not LOld.Current,
      'entered move survives caller withdrawal then retires the actual sole selection owner without an async mount cycle');
    LLimits := TNyxCollectionClipboardLimits.Defaults.UTF8Bytes(1);
    LOld := LFixture.Accept;
    LStroke := LFixture.Stroke(LFixture.Page, LLimits);
    Check(not LStroke.Current and (LStroke.Status = ncrssRefused) and
      (LStroke.ErrorText <> '') and LOld.Current,
      'bounded complete-output refusal is explicit and preserves accepted selection');
    LRefused := False;
    LOrdinal := 99;
    try
      NyxCollectionRangeStrokePointer(TNyxCollectionRangeStrokeDevice(LOrdinal), 0);
    except
      on E: ENyxCollectionRange do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'invalid closed device refuses before acquiring a stroke');
    LStroke := nil;
    LOld := LFixture.Accept;
    LStroke := LFixture.Stroke(LFixture.Page, TNyxCollectionClipboardLimits.Defaults);
    LRelease := LStroke.PrepareRelease(LFixture.Pointer, LFixture.Source, LFixture.Page, LFixture.Input);
    LStroke := nil;
    Check(LRelease.Complete(True, LFixture.Source, LFixture.Page, Facts(Cell('b', True))) and
      not LOld.Current and LFixture.State.Selection.Current,
      'pure release remains safe after caller drops its last stroke interface');
    LRelease.Retire;
    LRelease := nil;
    LOld := LFixture.Accept;
    LStroke := LFixture.Stroke(LPage, TNyxCollectionClipboardLimits.Defaults);
    LProbe.Scope := LFixture.Scope;
    LProxy.OnRead := LProbe.InvalidateInput;
    Check(not LStroke.Stage(LFixture.Pointer, LFixture.Source, LPage, LFixture.Input, Cell('c🌙', True)) and
      (LStroke.Status = ncrssStale),
      'actual source getter cancellation is fenced before adopting the returning candidate');
    Check(not LStroke.Current and
      (LStroke.PrepareRelease(LFixture.Pointer, LFixture.Source, LPage, LFixture.Input) = nil) and
      LOld.Current, 'actual owner-free cancellation flags defeat original release even with unchanged copied facts');
    LStroke.Retire;
    LStroke := nil;
    LRelease := nil;
    LProbe.Stroke := nil;
    LFixture.State := nil;
    LFixture.ReleaseOwners;
    Check(not LFixture.ViewToken.Connected and not LFixture.StoreToken.Connected and
      (LPreview.Payload.CellValue(1, 0).TextValue = CFull),
      'retained preview/readiness/proxy keep no actual view/store owner alive');
  finally
    LRelease := nil;
    LStroke := nil;
    LProbe.Free;
    LFixture.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-range-stroke-tests', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' pending stroke lifecycle checks; no target/hardware claim');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-range-stroke-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
