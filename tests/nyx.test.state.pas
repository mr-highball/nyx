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

unit nyx.test.state;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.model;

function RunNyxStateTests: Integer;
{ Mixed portable defaults also travel through generated compilation and HTTP
  application/view fixtures. Callers retain their document's ownership. }
procedure AddNyxStateFixture(ADocument: TNyxDocument);

implementation

uses
  SysUtils,
  Math,
  nyx.text,
  nyx.types,
  nyx.state,
  nyx.codec,
  nyx.codegen,
  nyx.composition,
  nyx.studio.session;

procedure AddNyxStateFixture(ADocument: TNyxDocument);
begin
  ADocument.State.Apply([
    NyxStateValue(NyxTextState('🌙/reply'), 'Café / 漢字' + #10 + #0 + '''crafted'''),
    NyxStateValue(NyxTextState('empty'), ''),
    NyxStateValue(NyxBooleanState('enabled'), True),
    NyxStateValue(NyxBooleanState('hidden'), False),
    NyxStateValue(NyxIntegerState('count'), High(Integer)),
    NyxStateValue(NyxIntegerState('minimum'), Low(Integer)),
    NyxStateValue(NyxNumberState('ratio'), 0.1),
    NyxStateValue(NyxNumberState('precision'), 1.2345678901234567),
    NyxStateValue(NyxNumberState('large'), 1e300),
    NyxStateValue(NyxNumberState('tiny'), 5e-324),
    NyxStateValue(NyxNumberState('whole'), 1e16),
    NyxStateValue(NyxNumberState('zero'), -0.0)]);
end;

type
  TProbeMode = (pmObserve, pmAtomic, pmReject, pmReadOnly, pmReentrant,
    pmDisconnect, pmThrow, pmThrowEmpty);

  { Callback receiver remains alive for every token. The fixture owns tokens,
    disconnects them before destroying receivers, and borrows its store. }
  TStateProbe = class
  public
    Store: TNyxState;
    Mode: TProbeMode;
    Calls: Integer;
    Checks: Integer;
    LastRevision: Integer;
    LastCount: Integer;
    LastOrderChanged: Boolean;
    FreeOnObserve: TNyxStateSubscription;
    procedure Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    procedure Observe(AState: TNyxState; AChanges: TNyxStateChanges);
  end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxState.Create('FAIL state: ' + AMessage);
  end;
  Inc(ACount);
end;

procedure TStateProbe.Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
var
  LRejected: Boolean;
begin

  if Mode = pmReject then
  begin

    if ACandidate.GetValue(NyxIntegerState('count')) < 0 then
    begin
      raise ENyxState.Create('Count cannot be negative / 🌙');
    end;
  end;

  if Mode = pmAtomic then
  begin
    Check((ACandidate.GetValue(NyxTextState('name')) = TNyxText('After / 🌙')) and
      (ACandidate.GetValue(NyxIntegerState('count')) = 42) and
      (Store.GetValue(NyxTextState('name')) = 'Before') and
      (Store.GetValue(NyxIntegerState('count')) = 1) and
      (ACandidate.Revision = Store.Revision + 1) and ACandidate.ReadOnly,
      'validator sees complete proposal and unchanged baseline', Checks);
  end;

  if Mode = pmReadOnly then
  begin
    LRejected := False;
    try
      ACandidate.SetValue(NyxTextState('name'), 'Forbidden');
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and ACandidate.ReadOnly, 'validator cannot mutate proposal', Checks);
    LRejected := False;
    try
      Store.SetValue(NyxIntegerState('count'), 90);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'validator cannot mutate accepted store', Checks);
  end;
end;

procedure TStateProbe.Observe(AState: TNyxState; AChanges: TNyxStateChanges);
var
  LRejected: Boolean;
  LIndex: Integer;
begin
  Inc(Calls);
  LastRevision := AState.Revision;
  LastCount := AChanges.Count;
  LastOrderChanged := AChanges.OrderChanged;

  if Mode = pmAtomic then
  begin
    Check((AState.GetValue(NyxTextState('name')) = TNyxText('After / 🌙')) and
      (AState.GetValue(NyxIntegerState('count')) = 42) and not AState.ReadOnly,
      'observer sees whole committed batch', Checks);
    Check((AChanges.Key(0) = 'name') and
      (AChanges.BeforeValue(0).TextValue = 'Before') and
      (AChanges.AfterValue(0).TextValue = TNyxText('After / 🌙')) and
      (AChanges.Key(1) = 'count') and (AChanges.BeforeValue(1).IntegerValue = 1) and
      (AChanges.AfterValue(1).IntegerValue = 42),
      'change data retains typed before/after values and request order', Checks);
  end;

  if Mode = pmReentrant then
  begin
    LRejected := False;
    try
      AState.SetValue(NyxTextState('name'), 'Nested observer update');
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'observer writes cannot recurse into publication', Checks);
  end;

  if Mode = pmDisconnect then
  begin
    FreeAndNil(FreeOnObserve);
  end;

  if Mode = pmThrow then
  begin
    raise ENyxState.Create('Observer failed / 🌙');
  end;

  if Mode = pmThrowEmpty then
  begin
    raise Exception.Create('');
  end;

  { Absence is not an empty string, False or zero. Before/after access must be
    guarded by its flag; invalid access produces an explicit diagnostic. }
  for LIndex := 0 to AChanges.Count - 1 do
  begin

    if not AChanges.HadValue(LIndex) then
    begin
      LRejected := False;
      try
        AChanges.BeforeValue(LIndex);
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'absent previous value cannot be coerced', Checks);
    end;

    if not AChanges.HasValue(LIndex) then
    begin
      LRejected := False;
      try
        AChanges.AfterValue(LIndex);
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'removed value cannot be coerced', Checks);
    end;
  end;
end;

procedure CheckNumbers(var ACount: Integer);
const
  CNumbers: array[0..9] of Double = (0, 0.1, -0.125, 1.2345678901234567,
    0.0000001, 10000000000000000.0, 1e300, -1e-300, 5e-324,
    1.7976931348623157e308);
var
  LIndex: Integer;
  LValue: TNyxStateValue;
  LReadback: Double;
  {$IFDEF PAS2JS}
  LSeparator: String;
  {$ELSE}
  LSeparator: Char;
  {$ENDIF}
  LRejected: Boolean;
begin
  LSeparator := FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator := ',';
    for LIndex := 0 to High(CNumbers) do
    begin
      LValue := TNyxStateValue.FromNumber(CNumbers[LIndex]);
      Check(TryNyxStateNumber(LValue.NumberText, LReadback) and
        (LReadback = CNumbers[LIndex]), 'number preserves exact Double meaning', ACount);
      Check(FormatSettings.DecimalSeparator = ',',
        'number conversion never mutates locale settings', ACount);
    end;
    Check(TNyxStateValue.FromNumber(0.1).NumberText = '0.1',
      'ordinary authored decimal remains readable', ACount);
    Check(not TryNyxStateNumber('1 trailing text', LReadback) and
      not TryNyxStateNumber('1,5', LReadback) and
      not TryNyxStateNumber('01', LReadback) and
      not TryNyxStateNumber('+1', LReadback) and
      not TryNyxStateNumber('1e9999', LReadback) and
      not TryNyxStateNumber('NaN', LReadback),
      'numeric import requires complete finite decimal grammar', ACount);
    Check(not TryNyxStateNumber('1e-9999', LReadback) and
      not TryNyxStateNumber('1e-324', LReadback),
      'nonzero numeric underflow is refused on both targets', ACount);
    Check(TryNyxStateNumber('-0.000e9999', LReadback) and (LReadback = 0),
      'exact signed zero is independent of exponent conversion', ACount);
  finally
    FormatSettings.DecimalSeparator := LSeparator;
  end;
  LRejected := False;
  try
    TNyxStateValue.FromNumber(Infinity);
  except
    on LException: ENyxState do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'nonfinite numeric value is refused', ACount);
end;

procedure CheckPersistence(var ACount: Integer);
const
  CInvalidEntries: array[0..14] of TNyxText = (
    'null',
    '"untyped"',
    '{"type":"unknown","value":"x"}',
    '{"type":"text","value":false}',
    '{"type":"boolean","value":"true"}',
    '{"type":"integer","value":1.5}',
    '{"type":"integer","value":2147483648}',
    '{"type":"integer","value":-2147483649}',
    '{"type":"integer","value":"1"}',
    '{"type":"number","value":0.1}',
    '{"type":"number","value":"1 trailing text"}',
    '{"type":"number","value":"1e9999"}',
    '{"type":"number","value":"NaN"}',
    '{"type":"text","value":"\ud800"}',
    '{"type":"text","value":"x","extra":"unversioned"}');
  CInvalidObjects: array[0..6] of TNyxText = (
    '{"version":1,"version":1,"title":"","pages":[],"components":[]}',
    '{"version":1,"title":"","pages":[],"components":[],"state":null}',
    '{"version":1,"title":"","pages":[],"components":[],"state":[]}',
    '{"version":1,"title":"","pages":[],"components":[],"state":{"x":{"type":"text","value":"a"},"\u0078":{"type":"text","value":"b"}}}',
    '{"version":1,"title":"","pages":[],"components":[],"state":{"x":{"type":"text","type":"text","value":"a"}}}',
    '{"version":1,"title":"","pages":[],"components":[],"state":{" ":{"type":"text","value":"x"}}}',
    '{"version":1,"title":"","pages":[{"id":"p","kind":"column","children":[],"props":{"text":"a","te\u0078t":"b"}}],"components":[]}');
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LCopy: TNyxDocument;
  LView: TNyxDocument;
  LSession: TNyxStudioSession;
  LJSON: TNyxText;
  LSource: TNyxText;
  LAccepted: TNyxText;
  LBefore: TNyxText;
  LIndex: Integer;
  LRejected: Boolean;

  procedure RejectImport(const ASource: TNyxText);
  var
    LImportRejected: Boolean;
  begin
    LImportRejected := False;
    try
      LSession.Load(ASource);
    except
      on LException: Exception do
      begin
        LImportRejected := True;
      end;
    end;
    Check(LImportRejected and (LSession.Save = LAccepted),
      'invalid state/duplicate member import preserves accepted design', ACount);
  end;

begin
  LDocument := TNyxDocument.Create;
  LSession := TNyxStudioSession.Create;
  try
    LDocument.Title := 'Portable state / 🌙';
    LDocument.AddPage(TNyxNode.Create(nkColumn, 'state-page'));
    AddNyxStateFixture(LDocument);
    LJSON := TNyxCodec.Encode(LDocument);
    LDecoded := TNyxCodec.Decode(LJSON);
    try
      Check(TNyxCodec.Encode(LDecoded) = LJSON,
        'typed defaults have deterministic version 1 persistence', ACount);
      for LIndex := 0 to LDocument.State.Count - 1 do
      begin
        Check(LDocument.State.Value(LDocument.State.Key(LIndex)).SameValue(
          LDecoded.State.Value(LDocument.State.Key(LIndex))),
          'all persisted types retain exact scalar meaning', ACount);
      end;
    finally
      LDecoded.Free;
    end;
    LCopy := LDocument.Clone;
    try
      Check(LCopy.State.Revision = LDocument.State.Revision,
        'document clone retains default revision', ACount);
      LCopy.State.SetValue(NyxTextState('🌙/reply'), 'Independent');
      Check(TNyxCodec.Encode(LDocument) = LJSON,
        'document defaults never alias a clone', ACount);
    finally
      LCopy.Free;
    end;
    LView := CloneNyxViewDocument(LDocument, LDocument.Pages[0]);
    try
      Check(LView.State.Count = LDocument.State.Count,
        'standalone view receives every application default', ACount);
      LView.State.SetValue(NyxBooleanState('enabled'), False);
      Check(LDocument.State.GetValue(NyxBooleanState('enabled')),
        'standalone defaults are independently owned', ACount);
    finally
      LView.Free;
    end;
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('NyxTextState(''🌙/reply'')', LSource) > 0) and
      (Pos('.SetValue(LEnabledBooleanState, True)', LSource) > 0) and
      (Pos('.SetValue(LCountIntegerState, 2147483647)', LSource) > 0) and
      (Pos('.SetValue(LRatioNumberState, 0.1)', LSource) > 0) and
      (Pos('NyxScalarText(0)', LSource) > 0),
      'source uses crafted typed defaults and exact text escaping', ACount);
    LSession.Load(LJSON);
    LBefore := LSession.Save;
    LSession.SetStateValues([
      NyxStateValue(NyxTextState('🌙/reply'), 'Edited / 🌙'),
      NyxStateValue(NyxBooleanState('enabled'), False)]);
    LAccepted := LSession.Save;
    Check((LAccepted <> LBefore) and
      (LSession.Document.State.GetValue(NyxTextState('🌙/reply')) = TNyxText('Edited / 🌙')) and
      not LSession.Document.State.GetValue(NyxBooleanState('enabled')),
      'Studio publishes one typed default command', ACount);
    LSession.Undo;
    Check(LSession.Save = LBefore, 'undo restores every typed default', ACount);
    LAccepted := LSession.Save;
    LRejected := False;
    try
      LSession.SetStateValues([
        NyxStateValue(NyxTextState('🌙/reply'), 'Partial'),
        NyxStateValue(NyxTextState('enabled'), 'wrong kind')]);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LAccepted),
      'invalid Studio state command preserves the complete baseline', ACount);
    LSession.SetStateValues([NyxStateValue(NyxBooleanState('enabled'), True)]);
    for LIndex := 0 to High(CInvalidEntries) do
    begin
      RejectImport('{"version":1,"title":"","pages":[],"components":[],"state":{"x":' +
        CInvalidEntries[LIndex] + '}}');
    end;
    for LIndex := 0 to High(CInvalidObjects) do
    begin
      RejectImport(CInvalidObjects[LIndex]);
    end;
    LSession.Redo;
    Check(LSession.Document.State.GetValue(NyxTextState('🌙/reply')) = TNyxText('Edited / 🌙'),
      'no-op/invalid commands/imports preserve redo history', ACount);
    LDecoded := TNyxCodec.Decode('{"version":1,"title":"","pages":[],"components":[]}');
    try
      Check((LDecoded.State.Count = 0) and
        (Pos('"state"', TNyxCodec.Encode(LDecoded)) = 0),
        'legacy empty-state designs retain their original wire shape', ACount);
    finally
      LDecoded.Free;
    end;
  finally
    LSession.Free;
    LDocument.Free;
  end;
end;

function RunNyxStateTests: Integer;
const
  { Match the public IEEE Double contract, rather than comparing a stored Double
    with FPC's wider Extended decimal literal during a native expression. }
  CExpectedRatio: Double = 0.1;
var
  LState: TNyxState;
  LCopy: TNyxState;
  LReordered: TNyxState;
  LFirst: TStateProbe;
  LSecond: TStateProbe;
  LFirstToken: TNyxStateSubscription;
  LSecondToken: TNyxStateSubscription;
  LRevision: Integer;
  LCalls: Integer;
  LRejected: Boolean;
  LAssignments: array of TNyxStateAssignment;
  LLarge: TNyxText;
  LInvalid: TNyxText;
  LIndex: Integer;
begin
  Result := 0;
  CheckNumbers(Result);
  CheckPersistence(Result);
  LState := TNyxState.Create;
  LFirst := TStateProbe.Create;
  LSecond := TStateProbe.Create;
  LFirstToken := nil;
  LSecondToken := nil;
  LCopy := nil;
  LReordered := nil;
  try
    LFirst.Store := LState;
    LSecond.Store := LState;
    LState.Apply([
      NyxStateValue(NyxTextState('name'), 'Before'),
      NyxStateValue(NyxIntegerState('count'), 1),
      NyxStateValue(NyxBooleanState('enabled'), True),
      NyxStateValue(NyxNumberState('ratio'), 0.1)]);
    Check((LState.Count = 4) and (LState.Revision = 1) and
      LState.GetValue(NyxBooleanState('enabled')) and
      (LState.GetValue(NyxNumberState('ratio')) = CExpectedRatio),
      'typed batch publishes all defaults in one revision', Result);
    LFirstToken := LState.Subscribe(LFirst.Observe, LFirst.Validate);
    LSecondToken := LState.Subscribe(LSecond.Observe);
    LFirst.Mode := pmAtomic;
    LState.Apply([
      NyxStateValue(NyxTextState('name'), 'After / 🌙'),
      NyxStateValue(NyxIntegerState('count'), 42)], LState.Revision);
    Check((LFirst.Calls = 1) and (LSecond.Calls = 1) and
      (LFirst.LastRevision = 2) and (LFirst.LastCount = 2),
      'each observer receives exactly one publication per batch', Result);
    LFirst.Mode := pmObserve;
    LRevision := LState.Revision;
    LCalls := LFirst.Calls;
    LState.Apply([]);
    LState.SetValue(NyxIntegerState('count'), 42);
    LState.Remove('missing');
    Check((LState.Revision = LRevision) and (LFirst.Calls = LCalls),
      'no-op sets/removals/batches neither notify nor change revision', Result);

    LCopy := LState.Clone;
    LCopy.SetValue(NyxTextState('name'), 'Clone / 🌙');
    Check((LState.GetValue(NyxTextState('name')) = TNyxText('After / 🌙')) and
      (LFirst.Calls = LCalls), 'clone owns independent data and no listeners', Result);
    SetLength(LAssignments, 1);
    LAssignments[0] := NyxStateValue(NyxTextState('name'), 'Owned / 🌙');
    LState.Apply(LAssignments);
    LAssignments[0].Key := 'changed input';
    LAssignments[0].Value := TNyxStateValue.FromText('changed input');
    Check(LState.GetValue(NyxTextState('name')) = TNyxText('Owned / 🌙'),
      'caller record/array mutation cannot alter accepted values', Result);
    LRevision := LState.Revision;
    LRejected := False;
    try
      LState.Apply([
        NyxStateValue(NyxTextState('name'), 'Partial'),
        NyxStateValue(NyxIntegerState('enabled'), 9)]);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LState.Revision = LRevision) and
      (LState.GetValue(NyxTextState('name')) = TNyxText('Owned / 🌙')) and
      LState.GetValue(NyxBooleanState('enabled')),
      'wrong-kind batch preserves all accepted values and revision', Result);
    LRejected := False;
    try
      LState.GetValue(NyxBooleanState('name'));
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'wrong typed getter refuses coercion', Result);
    LRejected := False;
    try
      LState.Apply([
        NyxStateValue(NyxTextState('name'), 'One'),
        NyxStateValue(NyxTextState('name'), 'Two')]);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LState.Revision = LRevision),
      'ambiguous duplicate update is rejected atomically', Result);
    LRejected := False;
    try
      LState.Apply([NyxStateValue(NyxTextState('name'), 'Stale')], LRevision - 1);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LState.Revision = LRevision),
      'stale baseline cannot publish an update', Result);
    LFirst.Mode := pmReject;
    LRejected := False;
    try
      LState.Apply([
        NyxStateValue(NyxTextState('name'), 'Rejected proposal'),
        NyxStateValue(NyxIntegerState('count'), -1)]);
    except
      on LException: ENyxState do
      begin
        LRejected := Pos('🌙', TNyxText(LException.Message)) > 0;
      end;
    end;
    Check(LRejected and (LState.Revision = LRevision) and
      (LState.GetValue(NyxTextState('name')) = TNyxText('Owned / 🌙')) and
      (LState.GetValue(NyxIntegerState('count')) = 42),
      'domain validator preserves baseline with Unicode diagnostic', Result);
    LFirst.Mode := pmReadOnly;
    LState.SetValue(NyxTextState('name'), 'Accepted read-only checks');
    LFirst.Mode := pmReentrant;
    LState.SetValue(NyxTextState('name'), 'Accepted observer checks');
    LFirst.Mode := pmObserve;

    LState.SetValue(NyxTextState('empty'), '');
    Check(LState.Has('empty') and (LState.GetValue(NyxTextState('empty')) = ''),
      'empty text is a present typed value', Result);
    LState.Remove(NyxTextState('empty'));
    Check(not LState.Has('empty'), 'removal is distinct from empty text', Result);
    LState.SetValue(NyxIntegerState('count'), Low(Integer));
    Check(LState.GetValue(NyxIntegerState('count')) = Low(Integer),
      'signed 32-bit minimum survives', Result);
    LState.SetValue(NyxIntegerState('count'), High(Integer));
    Check(LState.GetValue(NyxIntegerState('count')) = High(Integer),
      'signed 32-bit maximum survives', Result);

    LReordered := TNyxState.Create;
    LReordered.SetValue(NyxNumberState('ratio'), 0.1)
      .SetValue(NyxBooleanState('enabled'), True)
      .SetValue(NyxIntegerState('count'), High(Integer))
      .SetValue(NyxTextState('name'), LState.GetValue(NyxTextState('name')));
    LRevision := LState.Revision;
    LState.Assign(LReordered, LRevision);
    Check((LState.Revision = LRevision + 1) and (LState.Key(0) = 'ratio') and
      (LFirst.LastCount = 0) and LFirst.LastOrderChanged,
      'order-only replacement publishes once without fabricated value changes', Result);
    LRevision := LState.Revision;
    LState.Assign(LReordered);
    Check(LState.Revision = LRevision, 'identical replacement is a no-op', Result);

    LFirst.Mode := pmThrow;
    LCalls := LSecond.Calls;
    LRejected := False;
    try
      LState.SetValue(NyxIntegerState('count'), 10);
    except
      on LException: ENyxStateNotification do
      begin
        LRejected := Pos('🌙', TNyxText(LException.Message)) > 0;
      end;
    end;
    Check(LRejected and (LSecond.Calls = LCalls + 1) and
      (LState.GetValue(NyxIntegerState('count')) = 10) and
      (LState.Revision = LRevision + 1),
      'observer failure reports committed state and still notifies later observers', Result);
    LFirst.Mode := pmThrowEmpty;
    LRejected := False;
    try
      LState.SetValue(NyxIntegerState('count'), 11);
    except
      on LException: ENyxStateNotification do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LState.GetValue(NyxIntegerState('count')) = 11),
      'empty observer diagnostic still reports notification failure', Result);
    LFirst.Mode := pmDisconnect;
    LFirst.FreeOnObserve := LSecondToken;
    LCalls := LSecond.Calls;
    LState.SetValue(NyxIntegerState('count'), 12);
    LSecondToken := nil; // The first callback owns/releases this test token.
    Check((LFirst.FreeOnObserve = nil) and (LSecond.Calls = LCalls),
      'freeing a later token during callback skips it safely', Result);
    LFirst.Mode := pmObserve;
    LFirstToken.Disconnect;
    LCalls := LFirst.Calls;
    LState.SetValue(NyxIntegerState('count'), 13);
    Check(not LFirstToken.Connected and (LFirst.Calls = LCalls),
      'disconnect removes a listener immediately', Result);
    FreeAndNil(LFirstToken);
    LFirstToken := LState.Subscribe(LFirst.Observe);
    FreeAndNil(LState);
    Check(not LFirstToken.Connected, 'freeing store detaches outstanding caller tokens', Result);
    Inc(Result, LFirst.Checks + LSecond.Checks);
  finally
    LSecondToken.Free;
    LFirstToken.Free;
    LReordered.Free;
    LCopy.Free;
    LSecond.Free;
    LFirst.Free;
    LState.Free;
  end;

  LState := TNyxState.Create;
  try
    LState.SetValue(NyxTextState('🌙/設定'), 'Café / 🌙 / 漢字' + #10 + #0);
    Check(LState.GetValue(NyxTextState('🌙/設定')) = TNyxText('Café / 🌙 / 漢字') + #10 + #0,
      'Unicode keys and text/control payload remain exact', Result);
    LRevision := LState.Revision;
    {$IFDEF PAS2JS}
    LInvalid := #$d800;
    {$ELSE}
    SetCodePage(RawByteString(LInvalid), CP_UTF8, False);
    RawByteString(LInvalid) := #$ed#$a0#$80;
    {$ENDIF}
    LRejected := False;
    try
      LState.SetValue(NyxTextState('invalid'), LInvalid);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LState.Revision = LRevision),
      'malformed Unicode never enters accepted state', Result);
    { Build the same scalar payload on both targets. Length counts UTF-8 bytes
      natively and UTF-16 units in the browser. Doubling avoids quadratic work. }
    LLarge := '界';
    for LIndex := 1 to 19 do
    begin
      LLarge := LLarge + LLarge;
    end;
    LRejected := False;
    try
      LState.SetValue(NyxTextState('large'), LLarge);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LState.Has('large'),
      'UTF-8 payload budget refuses oversized data on both targets', Result);
  finally
    LState.Free;
  end;
end;

end.
