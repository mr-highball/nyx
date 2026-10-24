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

{ nyx — shared table arrangement values, geometry and management compounds.
  Target adapters consume these same canonical maps. They keep their existing
  row windows/editors; this unit owns no active widget, source, query or selection. }
unit nyx.collections.arrangement.controls;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.collections, nyx.collections.arrangement,
  nyx.collections.columns, nyx.collections.view, nyx.collections.view.types,
  nyx.layout.flow, nyx.model, nyx.controls;

type
  { Effective immutable presentation of ONE canonical field. Authored defaults
    and sparse runtime overrides resolve here without becoming new defaults.
    Width remains absent until a host measures/allocates it; a hidden column
    keeps its identity and preferences for later recovery. }
  TNyxColumnLayoutEntry = record
  private
    FColumn: TNyxCollectionColumn;
    FVisible, FHasWidth: Boolean;
    FWidth: Integer;
    FPin: TNyxColumnPin;
    FFeatures: TNyxColumnLayoutFeatures;
    function GetWidth: Integer;
  public
    property Column: TNyxCollectionColumn read FColumn;
    property IsVisible: Boolean read FVisible;
    property HasWidth: Boolean read FHasWidth;
    property Width: Integer read GetWidth;
    property Pin: TNyxColumnPin read FPin;
    property Features: TNyxColumnLayoutFeatures read FFeatures;
  end;

  { All fields are indexed canonically. The complete order also retains hidden
    entries; VisibleAt groups logical start/center/end while preserving order
    inside each group. No physical RTL coordinate is inferred in portable code. }
  TNyxColumnLayoutObservation = record
  private
    FSpec: TNyxCollectionViewSpec;
    FPreferences: TNyxColumnPreferences;
    FEntries: array of TNyxColumnLayoutEntry;
    FOrder, FVisible: TNyxCollectionColumnOrder;
    FSerial: Integer;
    function GetCount: Integer;
    function GetVisibleCount: Integer;
    function GetSpec: TNyxCollectionViewSpec;
    function GetPreferences: TNyxColumnPreferences;
  public
    class function Read(const ACapture: INyxCollectionColumnsCapture): TNyxColumnLayoutObservation; static;
    function Copy: TNyxColumnLayoutObservation;
    function ColumnAt(ACanonical: Integer): TNyxColumnLayoutEntry;
    function OrderedAt(AIndex: Integer): Integer;
    function VisibleAt(AIndex: Integer): Integer;
    property Count: Integer read GetCount;
    property VisibleCount: Integer read GetVisibleCount;
    property Spec: TNyxCollectionViewSpec read GetSpec;
    property Preferences: TNyxColumnPreferences read GetPreferences;
    property Serial: Integer read FSerial;
  end;

  { One bounded host allocation in logical pixels. Widths are canonical even
    when hidden/reordered; hidden fields have zero painted width and never enter
    the flow allocator. Pin preferences survive narrow overflow. Effective pins
    suspend as a group when they would consume the required scrollable center;
    hosts surface PinsSuspended and retain normal horizontal scrolling. }
  TNyxColumnPresentation = record
  private
    FObservation: TNyxColumnLayoutObservation;
    FWidths: TNyxFlowSizes;
    FStartWidth, FEndWidth: Integer;
    FPinsSuspended: Boolean;
    function GetObservation: TNyxColumnLayoutObservation;
  public
    function Width(ACanonical: Integer): Integer;
    function EffectivePin(ACanonical: Integer): TNyxColumnPin;
    property Observation: TNyxColumnLayoutObservation read GetObservation;
    property StartWidth: Integer read FStartWidth;
    property EndWidth: Integer read FEndWidth;
    property PinsSuspended: Boolean read FPinsSuspended;
  end;

  { Closed user intentions for the ordinary Nyx management compound. Width
    application and host-measured fitting remain separate permissions. Resets
    withdraw preferences; they do not manufacture new authored defaults. }
  TNyxColumnArrangementAction = (ncaaShow, ncaaHide, ncaaMoveEarlier,
    ncaaMoveLater, ncaaApplyWidth, ncaaDefaultWidth, ncaaFitWidth,
    ncaaApplyPin, ncaaResetColumn, ncaaResetOrder, ncaaResetAll);
  { Closed editable draft slots. Display captions and temporary input strings
    are interpreted only at this compound's explicit capture boundary. }
  TNyxColumnArrangementField = (ncafWidth, ncafPin);

  { Detached form result. The host keeps its ORIGINAL exact columns capture,
    checks Baseline and publishes Preferences through that view's existing
    preparation. A fit request carries only its typed field: measure the bounded
    current page on the host, then use PrepareAutoSize, not ordinary resizing.
    No root, view, source, selection, callback or mutable draft is retained. }
  TNyxColumnArrangementChange = record
  private
    FDefined: Boolean;
    FAction: TNyxColumnArrangementAction;
    FField: TNyxColumnIdentity;
    FPreferences: TNyxColumnPreferences;
    FBaseline: TNyxText;
    procedure RequireDefined;
    function GetAction: TNyxColumnArrangementAction;
    function GetField: TNyxColumnIdentity;
    function GetPreferences: TNyxColumnPreferences;
    function GetRequiresMeasurement: Boolean;
  public
    function Copy: TNyxColumnArrangementChange;
    property Defined: Boolean read FDefined;
    property Action: TNyxColumnArrangementAction read GetAction;
    { Global resets have no field; querying it refuses instead of inventing one. }
    property Field: TNyxColumnIdentity read GetField;
    property Preferences: TNyxColumnPreferences read GetPreferences;
    property Baseline: TNyxText read FBaseline;
    property RequiresMeasurement: Boolean read GetRequiresMeasurement;
  end;

  { Detached, unparsed input text from ONE exact mounted editor and its launch
    observation. Only values differing from their launch display are drafts.
    Full typed identities and copied specification own their meaning; tokens,
    captions and realized IDs never become field identities. No widget, model
    interface, callback or authority to publish is retained.

    Rebase only AFTER a known successful publication, including a committed
    notification failure. Exact installed preferences/specification/serial must
    match the intended result. Refusal leaves this value unchanged. A measured
    fit uses a distinct method so its real installed width cannot be invented
    from the action alone. Final input/event/form guards remain the host's job. }
  TNyxColumnArrangementDraft = record
  private
    FDefined: Boolean;
    FObservation: TNyxColumnLayoutObservation;
    FBaseline: TNyxText;
    FWidths, FPins: array of TNyxText;
    FWidthDirty, FPinDirty: array of Boolean;
    procedure RequireDefined;
    function FieldIndex(const AField: TNyxColumnIdentity;
      AInput: TNyxColumnArrangementField): Integer;
    function Rebase(const AChange: TNyxColumnArrangementChange;
      const AAccepted: TNyxColumnLayoutObservation;
      const AExpected: TNyxColumnPreferences): TNyxColumnArrangementDraft;
  public
    function Copy: TNyxColumnArrangementDraft;
    function HasValue(const AField: TNyxColumnIdentity;
      AInput: TNyxColumnArrangementField): Boolean;
    function Value(const AField: TNyxColumnIdentity;
      AInput: TNyxColumnArrangementField): TNyxText;
    function AfterAccepted(const AChange: TNyxColumnArrangementChange;
      const AAccepted: TNyxColumnLayoutObservation): TNyxColumnArrangementDraft;
    function AfterMeasuredFit(const AChange: TNyxColumnArrangementChange;
      AInstalledWidth: Integer;
      const AAccepted: TNyxColumnLayoutObservation): TNyxColumnArrangementDraft;
    property Defined: Boolean read FDefined;
    property Baseline: TNyxText read FBaseline;
  end;

  { Immutable, reference-counted host metric in logical pixels. It retains the
    exact bounded model sample, never a widget/font/mount. NaturalWidth describes
    the sample plus chrome before authored bounds; Width is ceiled/clamped.
    SampleRows is not a claim about every row on the page. A nil interface means
    no measurement. Copy retains this immutable value; WithNaturalWidth creates
    a separate value from copied facts without calling sample getters again.
    This interface grants no UI, form-open or event lifetime authority. }
  INyxColumnFitMeasurement = interface(IInterface)
    ['{CE74BC91-91FC-487A-ABEB-E25CA16E386B}']
    function GetSample: INyxCollectionColumnSample;
    function GetField: TNyxColumnIdentity;
    function GetNaturalWidth: Double;
    function GetWidth: Integer;
    function GetSampleRows: Integer;
    function GetClamped: Boolean;
    function Copy: INyxColumnFitMeasurement;
    function WithNaturalWidth(ANaturalWidth: Double): INyxColumnFitMeasurement;
    property Sample: INyxCollectionColumnSample read GetSample;
    property Field: TNyxColumnIdentity read GetField;
    property NaturalWidth: Double read GetNaturalWidth;
    property Width: Integer read GetWidth;
    property SampleRows: Integer read GetSampleRows;
    property Clamped: Boolean read GetClamped;
  end;

{ Admit a finite nonnegative logical natural width from an actual host metric.
  No viewport clamp is applied; ordinary horizontal overflow/pin suspension
  stays with presentation. This does not itself measure text or publish width. }
function NyxColumnFitMeasurement(const ASample: INyxCollectionColumnSample;
  ANaturalWidth: Double): INyxColumnFitMeasurement;

{ The baseline describes the launch specification, preferences and serial only.
  Rows/query membership are not copied into a form. It is a stale-form guard,
  never a substitute for the view's private exact capture identity proof. }
function NyxColumnArrangementBaseline(
  const AObservation: TNyxColumnLayoutObservation): TNyxText;
{ IDs use bounded opaque tokens from the immutable launch specification, not
  visible indexes. Capture verifies the token AGAINST its full typed field and
  baseline; it never parses an ID to discover a target. Long/Unicode field names
  remain complete metadata rather than overflowing Nyx's 128-scalar node IDs.
  Editor IDs must be valid Nyx IDs of at most 96 scalars, leaving suffix space.
  Global resets use the fieldless form. }
function NyxColumnArrangementActionID(const AEditorID: TNyxText;
  AAction: TNyxColumnArrangementAction): TNyxText; overload;
function NyxColumnArrangementActionID(const AEditorID: TNyxText;
  const AObservation: TNyxColumnLayoutObservation;
  const AField: TNyxColumnIdentity;
  AAction: TNyxColumnArrangementAction): TNyxText; overload;
function NyxColumnArrangementFieldID(const AEditorID: TNyxText;
  const AObservation: TNyxColumnLayoutObservation;
  const AField: TNyxColumnIdentity;
  AInput: TNyxColumnArrangementField): TNyxText;
{ A public compound of specialized Nyx cards, labels, inputs, selects and
  buttons. EVERY field remains listed, including hidden fields. Resets stay
  available when commands are disabled; authored policy governs new overrides.
  The list shows complete preference order, including hidden reorder barriers;
  actual pin groups retain that order within each group. No data is changed. }
function NewNyxColumnArrangementEditor(const AID: TNyxText;
  const AObservation: TNyxColumnLayoutObservation): INyxCard; overload;
{ Reconstruct against the draft's EXACT current baseline. After accepting an
  action use AfterAccepted/AfterMeasuredFit first. Unrelated incomplete text is
  preserved without parsing; an absent draft creates the ordinary default form. }
function NewNyxColumnArrangementEditor(const AID: TNyxText;
  const AObservation: TNyxColumnLayoutObservation;
  const ADraft: TNyxColumnArrangementDraft): INyxCard; overload;
{ Supply the exact realized editor and its containing mounted root AFTER live
  value synchronization. Capture verifies ancestry, baseline and unique input
  source IDs, including hidden fields; malformed numbers remain raw drafts. }
function CaptureNyxColumnArrangementDraft(AEditor, AMountedRoot: TNyxNode;
  const AObservation: TNyxColumnLayoutObservation): TNyxColumnArrangementDraft;
{ Read the actual mounted descriptor tree AFTER its presenter has synchronized
  live values. An unrelated button returns False. Recognized stale, disabled,
  malformed or wrong-family actions refuse before returning any result. The
  observation must be the form's retained launch observation, never a freshly
  recaptured replacement. A host still owns input/event/form lifetime fencing. }
function CaptureNyxColumnArrangementEditor(AButton, AMountedRoot: TNyxNode;
  const AObservation: TNyxColumnLayoutObservation;
  out AChange: TNyxColumnArrangementChange): Boolean;

{ Natural widths are a bounded canonical vector, measured by the host from the
  CURRENT page's declared sample. Hidden entries may be zero; visible entries
  must be positive. The actual horizontal clip/client width is required here,
  not the overflowing table's own measured scroll width. }
function NyxColumnPresentation(const AObservation: TNyxColumnLayoutObservation;
  AAvailable: Integer; const ANatural: TNyxFlowSizes): TNyxColumnPresentation;

implementation

uses
  Math, nyx.data, nyx.types, nyx.contract, nyx.state, nyx.responsive;

type
  { Sole owner of one immutable measurement and its managed sample lease.
    Class/interface storage is supported by both FPC and pas2js, including normal
    assignment, self replacement and final release. Refinement copies these
    already-admitted facts before any host font/layout observation. }
  TColumnFitMeasurement = class(TInterfacedObject, INyxColumnFitMeasurement)
  private
    FSample: INyxCollectionColumnSample;
    FField: TNyxColumnIdentity;
    FNaturalWidth: Double;
    FWidth: Integer;
    FSampleRows: Integer;
    FMinimum: Integer;
    FMaximum: Integer;
    FClamped: Boolean;
  public
    constructor Create(const ASample: INyxCollectionColumnSample;
      const AField: TNyxColumnIdentity; ASampleRows, AMinimum, AMaximum: Integer;
      ANaturalWidth: Double);
    function GetSample: INyxCollectionColumnSample;
    function GetField: TNyxColumnIdentity;
    function GetNaturalWidth: Double;
    function GetWidth: Integer;
    function GetSampleRows: Integer;
    function GetClamped: Boolean;
    function Copy: INyxColumnFitMeasurement;
    function WithNaturalWidth(ANaturalWidth: Double): INyxColumnFitMeasurement;
  end;

const
  CArrangementEditor = 'nyx.column-arrangement.editor';
  CArrangementAction = 'nyx.column-arrangement.action';
  CArrangementField = 'nyx.column-arrangement.field';
  CArrangementBaseline = 'nyx.column-arrangement.baseline';
  CEnabled = 'enabled';
  CValue = 'value';
  CTrue = 'true';
  { Every suffix below is at most 32 ASCII scalars, including its separator
    and a two-digit opaque token. Reserve that space before constructing nodes. }
  CMaximumArrangementEditorIDScalars = NyxMaximumIDScalars - 32;
  CArrangementActions: array[TNyxColumnArrangementAction] of TNyxText =
    ('show', 'hide', 'move-earlier', 'move-later', 'apply-width',
     'default-width', 'fit-width', 'apply-pin', 'reset-column', 'reset-order', 'reset-all');
  CArrangementFields: array[TNyxColumnArrangementField] of TNyxText =
    ('width', 'pin');
  CPinCaptions: array[TNyxColumnPin] of TNyxText =
    ('Unpinned', 'Start edge', 'End edge');

constructor TColumnFitMeasurement.Create(const ASample: INyxCollectionColumnSample;
  const AField: TNyxColumnIdentity; ASampleRows, AMinimum, AMaximum: Integer;
  ANaturalWidth: Double);
begin
  inherited Create;

  if IsNan(ANaturalWidth) or IsInfinite(ANaturalWidth) or (ANaturalWidth < 0) then
  begin
    raise ENyxCollection.Create('Column fit requires a finite nonnegative metric');
  end;
  FSample := ASample;
  FField := AField;
  FSampleRows := ASampleRows;
  FMinimum := AMinimum;
  FMaximum := AMaximum;
  FNaturalWidth := ANaturalWidth;
  FWidth := Ceil(Max(FMinimum, Min(FMaximum, ANaturalWidth)));
  FClamped := (ANaturalWidth < FMinimum) or (ANaturalWidth > FMaximum);
end;

function TColumnFitMeasurement.GetSample: INyxCollectionColumnSample;
begin
  Result := FSample;
end;

function TColumnFitMeasurement.GetField: TNyxColumnIdentity;
begin
  Result := FField;
end;

function TColumnFitMeasurement.GetNaturalWidth: Double;
begin
  Result := FNaturalWidth;
end;

function TColumnFitMeasurement.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TColumnFitMeasurement.GetSampleRows: Integer;
begin
  Result := FSampleRows;
end;

function TColumnFitMeasurement.GetClamped: Boolean;
begin
  Result := FClamped;
end;

function TColumnFitMeasurement.Copy: INyxColumnFitMeasurement;
begin
  Result := Self;
end;

function TColumnFitMeasurement.WithNaturalWidth(
  ANaturalWidth: Double): INyxColumnFitMeasurement;
begin
  { Read private copied facts only: a foreign sample may call out from getters,
    but a host has already begun its typography/geometry observation here. }
  Result := TColumnFitMeasurement.Create(FSample, FField, FSampleRows,
    FMinimum, FMaximum, ANaturalWidth);
end;

function NyxColumnFitMeasurement(const ASample: INyxCollectionColumnSample;
  ANaturalWidth: Double): INyxColumnFitMeasurement;
var
  LSample: INyxCollectionColumnSample;
  LColumn: TNyxCollectionColumn;
  LSizing: TNyxColumnSizing;
  LCount: Integer;
begin
  LSample := ASample;

  if (LSample = nil) or IsNan(ANaturalWidth) or IsInfinite(ANaturalWidth) or
    (ANaturalWidth < 0) then
  begin
    raise ENyxCollection.Create('Column fit requires a sample and finite nonnegative metric');
  end;
  LColumn := LSample.Column;
  LCount := LSample.Count;

  if (LCount < 0) or (LCount > NyxMaximumColumnAutoSizeRows) then
  begin
    raise ENyxCollection.Create('Column fit sample exceeds the bounded row contract');
  end;
  LSizing := LColumn.Sizing;

  if not LSizing.Defined then
  begin
    LSizing := NyxColumnSizing;
  end;
  LSizing.Validate;
  Result := TColumnFitMeasurement.Create(LSample, LColumn.Identity, LCount,
    LSizing.Bounds.MinimumValue, LSizing.Bounds.MaximumValue, ANaturalWidth);
end;

procedure RequireArrangementAction(AAction: TNyxColumnArrangementAction);
begin

  if (Ord(AAction) < Ord(Low(TNyxColumnArrangementAction))) or
    (Ord(AAction) > Ord(High(TNyxColumnArrangementAction))) then
  begin
    raise ENyxCollection.Create('Unknown column arrangement action');
  end;
end;

procedure RequireArrangementEditorID(const AEditorID: TNyxText);
var
  LIndex, LScalar, LCount: Integer;
begin
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AEditorID) do
  begin

    if not NyxNextScalar(AEditorID, LIndex, LScalar) then
    begin
      raise ENyxCollection.Create('Column editor ID contains malformed Unicode');
    end;
    Inc(LCount);

    if LCount > CMaximumArrangementEditorIDScalars then
    begin
      raise ENyxCollection.Create('Column editor ID must leave 32 scalars for control suffixes');
    end;
  end;

  if LCount = 0 then
  begin
    raise ENyxCollection.Create('Column editor requires a nonempty explicit ID');
  end;
end;

function ArrangementFieldPrefix(const AEditorID: TNyxText;
  const AObservation: TNyxColumnLayoutObservation;
  const AField: TNyxColumnIdentity): TNyxText;
var
  LCanonical: Integer;
begin
  RequireArrangementEditorID(AEditorID);
  AField.Validate;
  LCanonical := AObservation.Spec.ColumnIndex(AField);

  if (LCanonical < 0) or (LCanonical >= AObservation.Count) then
  begin
    raise ENyxCollection.Create('Column token requires a field in its exact launch observation');
  end;
  { The captured descriptor order is stable across hiding, pin grouping and
    window changes. This number is only an opaque form token. The full typed
    identity remains on each action and is checked before this expected ID. }
  Result := AEditorID + TNyxText('-field-') + TNyxText(IntToStr(LCanonical));
end;

function NyxColumnArrangementActionID(const AEditorID: TNyxText;
  AAction: TNyxColumnArrangementAction): TNyxText;
begin
  RequireArrangementEditorID(AEditorID);
  RequireArrangementAction(AAction);

  if not (AAction in [ncaaResetOrder, ncaaResetAll]) then
  begin
    raise ENyxCollection.Create('This column action requires a typed field');
  end;
  Result := AEditorID + '-' + CArrangementActions[AAction];
end;

function NyxColumnArrangementActionID(const AEditorID: TNyxText;
  const AObservation: TNyxColumnLayoutObservation;
  const AField: TNyxColumnIdentity;
  AAction: TNyxColumnArrangementAction): TNyxText;
begin
  RequireArrangementAction(AAction);

  if AAction in [ncaaResetOrder, ncaaResetAll] then
  begin
    raise ENyxCollection.Create('A global reset does not address one field');
  end;
  Result := ArrangementFieldPrefix(AEditorID, AObservation, AField) + '-' + CArrangementActions[AAction];
end;

function NyxColumnArrangementFieldID(const AEditorID: TNyxText;
  const AObservation: TNyxColumnLayoutObservation;
  const AField: TNyxColumnIdentity;
  AInput: TNyxColumnArrangementField): TNyxText;
begin

  if (Ord(AInput) < Ord(Low(TNyxColumnArrangementField))) or
    (Ord(AInput) > Ord(High(TNyxColumnArrangementField))) then
  begin
    raise ENyxCollection.Create('Unknown column arrangement input');
  end;
  Result := ArrangementFieldPrefix(AEditorID, AObservation, AField) + '-' + CArrangementFields[AInput];
end;

function NyxColumnArrangementBaseline(
  const AObservation: TNyxColumnLayoutObservation): TNyxText;
begin

  if AObservation.Count = 0 then
  begin
    raise ENyxCollection.Create('Column arrangement editor requires an observation');
  end;
  Result := NyxObject([
    NyxField('spec', AObservation.Spec.ToData),
    NyxField('preferences', AObservation.Preferences.ToData),
    NyxField('serial', NyxData(AObservation.Serial))]).ToJSON;
end;

procedure TNyxColumnArrangementChange.RequireDefined;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Column arrangement result is absent');
  end;
end;

function TNyxColumnArrangementChange.GetAction: TNyxColumnArrangementAction;
begin
  RequireDefined;
  Result := FAction;
end;

function TNyxColumnArrangementChange.GetField: TNyxColumnIdentity;
begin
  RequireDefined;

  if FAction in [ncaaResetOrder, ncaaResetAll] then
  begin
    raise ENyxCollection.Create('A global reset has no individual column field');
  end;
  Result := FField;
end;

function TNyxColumnArrangementChange.GetPreferences: TNyxColumnPreferences;
begin
  RequireDefined;

  if FAction = ncaaFitWidth then
  begin
    raise ENyxCollection.Create('Fit width requires bounded host measurement');
  end;
  Result := FPreferences.Copy;
end;

function TNyxColumnArrangementChange.GetRequiresMeasurement: Boolean;
begin
  RequireDefined;
  Result := FAction = ncaaFitWidth;
end;

function TNyxColumnArrangementChange.Copy: TNyxColumnArrangementChange;
begin
  Result := Self;

  if FDefined then
  begin
    Result.FPreferences := FPreferences.Copy;
  end;
end;

function ArrangementWidthText(const AEntry: TNyxColumnLayoutEntry): TNyxText;
begin
  Result := '';

  if AEntry.HasWidth then
  begin
    Result := TNyxText(IntToStr(AEntry.Width));
  end;
end;

procedure TNyxColumnArrangementDraft.RequireDefined;
begin

  if not FDefined then
  begin
    raise ENyxCollection.Create('Column arrangement draft is absent');
  end;
end;

function TNyxColumnArrangementDraft.FieldIndex(const AField: TNyxColumnIdentity;
  AInput: TNyxColumnArrangementField): Integer;
begin
  RequireDefined;

  if (Ord(AInput) < Ord(Low(TNyxColumnArrangementField))) or
    (Ord(AInput) > Ord(High(TNyxColumnArrangementField))) then
  begin
    raise ENyxCollection.Create('Unknown column arrangement draft input');
  end;
  AField.Validate;
  Result := FObservation.Spec.ColumnIndex(AField);

  if Result < 0 then
  begin
    raise ENyxCollection.Create('Draft field is missing or has changed scalar family');
  end;
end;

function TNyxColumnArrangementDraft.HasValue(const AField: TNyxColumnIdentity;
  AInput: TNyxColumnArrangementField): Boolean;
var
  LIndex: Integer;
begin
  LIndex := FieldIndex(AField, AInput);

  if AInput = ncafWidth then
  begin
    Result := FWidthDirty[LIndex];
  end
  else
  begin
    Result := FPinDirty[LIndex];
  end;
end;

function TNyxColumnArrangementDraft.Value(const AField: TNyxColumnIdentity;
  AInput: TNyxColumnArrangementField): TNyxText;
var
  LIndex: Integer;
begin
  LIndex := FieldIndex(AField, AInput);

  if not HasValue(AField, AInput) then
  begin
    raise ENyxCollection.Create('This column input has no unfinished draft');
  end;

  if AInput = ncafWidth then
  begin
    Result := FWidths[LIndex];
  end
  else
  begin
    Result := FPins[LIndex];
  end;
end;

function TNyxColumnArrangementDraft.Copy: TNyxColumnArrangementDraft;
begin
  Result := Default(TNyxColumnArrangementDraft);

  if not FDefined then
  begin
    Exit;
  end;
  Result.FDefined := True;
  Result.FObservation := FObservation.Copy;
  Result.FBaseline := FBaseline;
  Result.FWidths := System.Copy(FWidths, 0, Length(FWidths));
  Result.FPins := System.Copy(FPins, 0, Length(FPins));
  Result.FWidthDirty := System.Copy(FWidthDirty, 0, Length(FWidthDirty));
  Result.FPinDirty := System.Copy(FPinDirty, 0, Length(FPinDirty));
end;

function TNyxColumnArrangementDraft.Rebase(
  const AChange: TNyxColumnArrangementChange;
  const AAccepted: TNyxColumnLayoutObservation;
  const AExpected: TNyxColumnPreferences): TNyxColumnArrangementDraft;
var
  LSerial, LIndex: Integer;
  LEntry: TNyxColumnLayoutEntry;
  LClearWidth, LClearPin, LTarget: Boolean;
begin
  RequireDefined;
  AChange.RequireDefined;
  AExpected.Validate;

  if (AChange.Baseline <> FBaseline) or
    (AAccepted.Spec.ToData.ToJSON <> FObservation.Spec.ToData.ToJSON) or
    (AAccepted.Preferences.ToData.ToJSON <> AExpected.ToData.ToJSON) then
  begin
    raise ENyxCollection.Create('Accepted column state does not match this draft action');
  end;
  { The existing single columns publication advances once only when preferences
    differ. Detect a reentrant layout change even if it later restores equal
    values; never silently adopt that newer state as this action's commit. }
  LSerial := FObservation.Serial;

  if AExpected.ToData.ToJSON <> FObservation.Preferences.ToData.ToJSON then
  begin

    if LSerial = High(Integer) then
    begin
      raise ENyxCollection.Create('Column draft cannot advance an exhausted serial');
    end;
    Inc(LSerial);
  end;

  if AAccepted.Serial <> LSerial then
  begin
    raise ENyxCollection.Create('Accepted columns were superseded before draft rebasing');
  end;
  Result := Copy;
  Result.FObservation := AAccepted.Copy;
  Result.FBaseline := NyxColumnArrangementBaseline(AAccepted);
  for LIndex := 0 to FObservation.Count - 1 do
  begin
    LEntry := AAccepted.ColumnAt(LIndex);
    LTarget := False;

    if not (AChange.Action in [ncaaResetOrder, ncaaResetAll]) then
    begin
      LTarget := LEntry.Column.Identity.Same(AChange.Field);
    end;
    LClearWidth := (AChange.Action = ncaaResetAll) or
      (LTarget and (AChange.Action in [ncaaApplyWidth, ncaaDefaultWidth,
        ncaaFitWidth, ncaaResetColumn]));
    LClearPin := (AChange.Action = ncaaResetAll) or
      (LTarget and (AChange.Action in [ncaaApplyPin, ncaaResetColumn]));
    Result.FWidthDirty[LIndex] := Result.FWidthDirty[LIndex] and
      not LClearWidth and (Result.FWidths[LIndex] <> ArrangementWidthText(LEntry));
    Result.FPinDirty[LIndex] := Result.FPinDirty[LIndex] and
      not LClearPin and (Result.FPins[LIndex] <> CPinCaptions[LEntry.Pin]);

    if not Result.FWidthDirty[LIndex] then
    begin
      Result.FWidths[LIndex] := '';
    end;

    if not Result.FPinDirty[LIndex] then
    begin
      Result.FPins[LIndex] := '';
    end;
  end;
end;

function TNyxColumnArrangementDraft.AfterAccepted(
  const AChange: TNyxColumnArrangementChange;
  const AAccepted: TNyxColumnLayoutObservation): TNyxColumnArrangementDraft;
begin
  { Preferences refuses a fit action, whose measured result is not in Change. }
  Result := Rebase(AChange, AAccepted, AChange.Preferences);
end;

function TNyxColumnArrangementDraft.AfterMeasuredFit(
  const AChange: TNyxColumnArrangementChange; AInstalledWidth: Integer;
  const AAccepted: TNyxColumnLayoutObservation): TNyxColumnArrangementDraft;
var
  LPreferences: TNyxColumnPreferences;
  LPreference: TNyxColumnPreference;
  LEntry: TNyxColumnLayoutEntry;
  LSizing: TNyxColumnSizing;
  LIndex: Integer;
begin
  RequireDefined;
  AChange.RequireDefined;

  if (AChange.Action <> ncaaFitWidth) or (AChange.Baseline <> FBaseline) then
  begin
    raise ENyxCollection.Create('Measured draft rebasing requires this launch''s fit action');
  end;
  LIndex := FObservation.Spec.ColumnIndex(AChange.Field);

  if LIndex < 0 then
  begin
    raise ENyxCollection.Create('Measured fit field is no longer in the launch specification');
  end;
  LEntry := FObservation.ColumnAt(LIndex);
  LSizing := LEntry.Column.Sizing;

  if not LSizing.Defined then
  begin
    LSizing := NyxColumnSizing;
  end;

  if (AInstalledWidth < LSizing.Bounds.MinimumValue) or
    (AInstalledWidth > LSizing.Bounds.MaximumValue) then
  begin
    raise ENyxCollection.Create('Measured fit width is outside the authored bounds');
  end;
  LPreferences := FObservation.Preferences;
  LPreference := NyxColumnPreference(AChange.Field);
  LIndex := LPreferences.IndexOf(AChange.Field);

  if LIndex >= 0 then
  begin
    LPreference := LPreferences.ColumnAt(LIndex);
  end;
  LPreferences := LPreferences.WithColumn(LPreference.Width(AInstalledWidth));
  Result := Rebase(AChange, AAccepted, LPreferences);
end;

function ArrangementMoveAllowed(const AObservation: TNyxColumnLayoutObservation;
  APosition, AStep: Integer): Boolean;
var
  LPeer: Integer;
begin
  LPeer := APosition + AStep;
  Result := (APosition >= 0) and (APosition < AObservation.Count) and
    (LPeer >= 0) and (LPeer < AObservation.Count);

  if Result then
  begin
    Result := (nclfReorder in AObservation.ColumnAt(
      AObservation.OrderedAt(APosition)).Features) and
      (nclfReorder in AObservation.ColumnAt(
      AObservation.OrderedAt(LPeer)).Features);
  end;
end;

function NewNyxColumnArrangementEditor(const AID: TNyxText;
  const AObservation: TNyxColumnLayoutObservation): INyxCard;
begin
  Result := NewNyxColumnArrangementEditor(AID, AObservation,
    Default(TNyxColumnArrangementDraft));
end;

function NewNyxColumnArrangementEditor(const AID: TNyxText;
  const AObservation: TNyxColumnLayoutObservation;
  const ADraft: TNyxColumnArrangementDraft): INyxCard;
var
  LEditor, LFieldCard: INyxCard;
  LRow: INyxRow;
  LEntry: TNyxColumnLayoutEntry;
  LField: TNyxColumnIdentity;
  LPreferences: TNyxColumnPreferences;
  LPosition, LCanonical: Integer;
  LPrefix, LCaption, LWidth, LPinText, LBaseline: TNyxText;
  LCanShow: Boolean;

  procedure Button(const AParent: INyxControl;
    AAction: TNyxColumnArrangementAction; const ACaption: TNyxText;
    AEnabled: Boolean);
  var
    LButton: INyxButton;
  begin

    if AAction in [ncaaResetOrder, ncaaResetAll] then
    begin
      LButton := NewNyxButton(NyxColumnArrangementActionID(AID, AAction));
    end
    else
    begin
      LButton := NewNyxButton(NyxColumnArrangementActionID(AID, AObservation, LField, AAction));
      LButton.Configure.Extension(CArrangementField, LField.ToData.ToJSON).Done;
    end;
    LButton.Configure.Text(ACaption).Enabled(AEnabled)
      .PartName(NyxPart(CArrangementActions[AAction]))
      .Extension(CArrangementEditor, AID)
      .Extension(CArrangementAction, CArrangementActions[AAction]).Done;
    AParent.Add(LButton);
  end;

  function ActionRow(const AParent: INyxControl; const ASuffix: TNyxText): INyxRow;
  begin
    Result := NewNyxRow(LPrefix + '-' + ASuffix);
    { The contextual window itself is normally 520 px wide, even on desktop.
      Keep that usable row layout; stack only at a genuinely narrow form size. }
    Result.Configure.Gap(6).WhenViewport(TNyxViewportWidth.Below(420))
      .Layout(nlColumn).Done;
    AParent.Add(Result);
  end;

begin
  RequireArrangementEditorID(AID);
  LBaseline := NyxColumnArrangementBaseline(AObservation);

  if ADraft.Defined and (ADraft.Baseline <> LBaseline) then
  begin
    raise ENyxCollection.Create('Column editor draft belongs to a different launch baseline');
  end;

  if not AObservation.Spec.HasLayoutControls then
  begin
    raise ENyxCollection.Create('Column editor requires authored arrangement controls');
  end;
  LPreferences := AObservation.Preferences;
  LEditor := NewNyxCard(AID);
  LEditor.Configure.Layout(nlColumn).Compound(True).Gap(12).Padding(12)
    .PartName(NyxPart('column-arrangement'))
    .Extension(CArrangementBaseline, LBaseline)
    .WhenViewport(TNyxViewportWidth.Below(640)).Padding(6).Done;
  LEditor.Add(NewNyxHeading(AID + '-title').Configure.Text('Arrange columns').Done);
  LEditor.Add(NewNyxLabel(AID + '-help').Configure.Text(
    'Choose visible columns, widths and pinned edges. This list includes hidden columns; ' +
    'its order applies within each pin group. Widths use logical pixels.').Done);

  if AObservation.VisibleCount = 0 then
  begin
    LCanShow := False;
    for LCanonical := 0 to AObservation.Count - 1 do
    begin
      LCanShow := LCanShow or
        (nclfVisibility in AObservation.ColumnAt(LCanonical).Features);
    end;
    LEditor.Add(NewNyxLabel(AID + '-empty').Configure.Text(
      'All columns are hidden. Show a permitted column below, or restore your defaults.').Done);

    if not LCanShow then
    begin
      LEditor.Add(NewNyxLabel(AID + '-visibility-locked').Configure.Text(
        'Visibility changes are locked by authored policy. Restoring defaults can also ' +
        'leave every column hidden; the application author controls that policy.').Done);
    end;
  end;
  for LPosition := 0 to AObservation.Count - 1 do
  begin
    LCanonical := AObservation.OrderedAt(LPosition);
    LEntry := AObservation.ColumnAt(LCanonical);
    LField := LEntry.Column.Identity;
    LPrefix := ArrangementFieldPrefix(AID, AObservation, LField);
    LCaption := LEntry.Column.Title;

    if LCaption = '' then
    begin
      LCaption := LField.Name;
    end;
    LFieldCard := NewNyxCard(LPrefix);
    LFieldCard.Configure.Layout(nlColumn).Gap(6).Padding(8).Done;
    LFieldCard.Add(NewNyxLabel(LPrefix + '-title').Configure.Text(LCaption).Done);
    LFieldCard.Add(NewNyxLabel(LPrefix + '-field').Configure.Text(
      LField.Name + TNyxText(' / ') + NyxStateKindName(LField.Kind)).Done);
    LRow := ActionRow(LFieldCard, 'order');

    if LEntry.IsVisible then
    begin
      Button(LRow, ncaaHide, 'Hide column', nclfVisibility in LEntry.Features);
    end
    else
    begin
      Button(LRow, ncaaShow, 'Show column', nclfVisibility in LEntry.Features);
    end;
    Button(LRow, ncaaMoveEarlier, 'Move earlier',
      ArrangementMoveAllowed(AObservation, LPosition, -1));
    Button(LRow, ncaaMoveLater, 'Move later',
      ArrangementMoveAllowed(AObservation, LPosition, 1));
    LRow := ActionRow(LFieldCard, 'pinning');
    LPinText := CPinCaptions[LEntry.Pin];

    if ADraft.Defined and ADraft.HasValue(LField, ncafPin) then
    begin
      LPinText := ADraft.Value(LField, ncafPin);
    end;
    LRow.Add(NewNyxSelect(NyxColumnArrangementFieldID(AID, AObservation, LField, ncafPin))
      .Configure.PartName(NyxPart('pin')).Text('Pinned edge')
      .Items(CPinCaptions[ncpNone] + #10 + CPinCaptions[ncpStart] + #10 + CPinCaptions[ncpEnd])
      .Value(LPinText).Enabled(nclfPinning in LEntry.Features).Done);
    Button(LRow, ncaaApplyPin, 'Set pinned edge', nclfPinning in LEntry.Features);
    LRow := ActionRow(LFieldCard, 'sizing');
    LWidth := ArrangementWidthText(LEntry);

    if ADraft.Defined and ADraft.HasValue(LField, ncafWidth) then
    begin
      LWidth := ADraft.Value(LField, ncafWidth);
    end;
    LRow.Add(NewNyxInput(NyxColumnArrangementFieldID(AID, AObservation, LField, ncafWidth))
      .Configure.PartName(NyxPart('width')).Text('Width in logical pixels')
      .Placeholder('Use authored sizing').Value(LWidth).Enabled(nclfResize in LEntry.Features).Done);
    Button(LRow, ncaaApplyWidth, 'Set width', nclfResize in LEntry.Features);
    Button(LRow, ncaaDefaultWidth, 'Restore default width', LEntry.HasWidth);
    Button(LRow, ncaaFitWidth, 'Fit current page', nclfAutoSize in LEntry.Features);
    Button(LFieldCard, ncaaResetColumn, 'Reset width, visibility and pinning',
      LPreferences.IndexOf(LField) >= 0);
    LEditor.Add(LFieldCard);
  end;
  { Recovery remains available after the field controls, rather than taking
    initial keyboard focus ahead of the operations users came here to perform. }
  Button(LEditor, ncaaResetOrder, 'Restore default column order', True);
  Button(LEditor, ncaaResetAll, 'Restore all column defaults', True);
  Result := LEditor;
end;

function ArrangementSourcePart(ARoot: TNyxNode;
  const ASourceID: TNyxText): TNyxNode;
var
  LFound: TNyxNode;
  LVisited: Integer;

  procedure Visit(ANode: TNyxNode; ADepth: Integer);
  var
    LChild: Integer;
  begin
    Inc(LVisited);

    if (ADepth > NyxMaximumTreeDepth) or (LVisited > NyxMaximumNodes) then
    begin
      raise ENyxCollection.Create('Column editor exceeds the mounted tree bounds');
    end;

    if ANode.SourceID = ASourceID then
    begin

      if LFound <> nil then
      begin
        raise ENyxCollection.Create('Column editor input source identity is ambiguous');
      end;
      LFound := ANode;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LChild], ADepth + 1);
    end;
  end;

begin
  { Search ONLY the exact editor instance already established by ancestry.
    Realization escapes source separators and qualifies reusable instances, so
    a raw helper ID must not be compared with a runtime-qualified ID. Refuse a
    duplicate source part rather than accidentally reading a nested peer. }
  LFound := nil;
  LVisited := 0;
  Visit(ARoot, 0);
  Result := LFound;
end;

function CaptureNyxColumnArrangementDraft(AEditor, AMountedRoot: TNyxNode;
  const AObservation: TNyxColumnLayoutObservation): TNyxColumnArrangementDraft;
var
  LAncestor, LInput: TNyxNode;
  LEditorID, LBaseline: TNyxText;
  LEntry: TNyxColumnLayoutEntry;
  LField: TNyxColumnIdentity;
  LIndex, LDepth: Integer;
  LFound: Boolean;

  function Input(AField: TNyxColumnArrangementField; AKind: TNyxKind): TNyxNode;
  begin
    Result := ArrangementSourcePart(AEditor,
      NyxColumnArrangementFieldID(LEditorID, AObservation, LField, AField));

    if (Result = nil) or (Result.ProjectionKind <> NyxKindName(AKind)) then
    begin
      raise ENyxCollection.Create('Column draft input is missing or has changed kind');
    end;
  end;

begin
  Result := Default(TNyxColumnArrangementDraft);

  if (AEditor = nil) or (AMountedRoot = nil) or
    (AEditor.ProjectionKind <> NyxKindName(nkCard)) or
    not AObservation.Spec.HasLayoutControls then
  begin
    raise ENyxCollection.Create('Column draft requires its exact mounted editor');
  end;
  LEditorID := AEditor.SourceID;
  RequireArrangementEditorID(LEditorID);
  LBaseline := NyxColumnArrangementBaseline(AObservation);

  if AEditor.Prop(CArrangementBaseline) <> LBaseline then
  begin
    raise ENyxCollection.Create('Column draft does not match its launch observation');
  end;
  LAncestor := AEditor;
  LDepth := 0;
  LFound := False;
  while LAncestor <> nil do
  begin

    if LAncestor = AMountedRoot then
    begin
      LFound := True;
      Break;
    end;
    Inc(LDepth);

    if LDepth > NyxMaximumTreeDepth then
    begin
      raise ENyxCollection.Create('Column draft ancestry exceeds the mounted tree bounds');
    end;
    LAncestor := LAncestor.Parent;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Column draft editor is outside the exact mounted root');
  end;
  Result.FObservation := AObservation.Copy;
  Result.FBaseline := LBaseline;
  SetLength(Result.FWidths, AObservation.Count);
  SetLength(Result.FPins, AObservation.Count);
  SetLength(Result.FWidthDirty, AObservation.Count);
  SetLength(Result.FPinDirty, AObservation.Count);
  for LIndex := 0 to AObservation.Count - 1 do
  begin
    LEntry := AObservation.ColumnAt(LIndex);
    LField := LEntry.Column.Identity;
    LInput := Input(ncafWidth, nkInput);
    Result.FWidths[LIndex] := LInput.Prop(CValue);
    Result.FWidthDirty[LIndex] :=
      Result.FWidths[LIndex] <> ArrangementWidthText(LEntry);
    LInput := Input(ncafPin, nkSelect);
    Result.FPins[LIndex] := LInput.Prop(CValue);
    Result.FPinDirty[LIndex] := Result.FPins[LIndex] <> CPinCaptions[LEntry.Pin];
  end;
  { Publish this detached VALUE only after every required input was captured.
    Disabled/hidden inputs may retain text; retaining it grants no action. }
  Result.FDefined := True;
end;

function CaptureNyxColumnArrangementEditor(AButton, AMountedRoot: TNyxNode;
  const AObservation: TNyxColumnLayoutObservation;
  out AChange: TNyxColumnArrangementChange): Boolean;
var
  LEditor, LInput, LAncestor: TNyxNode;
  LEditorID, LActionName, LExpectedID, LWire: TNyxText;
  LAction: TNyxColumnArrangementAction;
  LPin: TNyxColumnPin;
  LFound: Boolean;
  LNext: TNyxColumnArrangementChange;
  LEntry: TNyxColumnLayoutEntry;
  LSpec: TNyxCollectionViewSpec;
  LPreference: TNyxColumnPreference;
  LOrder: array of TNyxColumnIdentity;
  LCanonical, LIndex, LPosition, LStep, LWidth: Integer;
  LDepth: Integer;
  LSwap: TNyxColumnIdentity;
  LSizing: TNyxColumnSizing;

  procedure RequireFeature(AFeature: TNyxColumnLayoutFeature);
  begin

    if not (AFeature in LEntry.Features) then
    begin
      raise ENyxCollection.Create('Column action is disabled by its authored policy');
    end;
  end;

  function Input(AField: TNyxColumnArrangementField;
    AKind: TNyxKind): TNyxNode;
  begin
    Result := ArrangementSourcePart(LEditor,
      NyxColumnArrangementFieldID(LEditorID, AObservation, LNext.FField, AField));

    if (Result = nil) or (Result.ProjectionKind <> NyxKindName(AKind)) or
      (Result.Prop(CEnabled, CTrue) <> CTrue) then
    begin
      raise ENyxCollection.Create('Column editor input is missing, disabled or has changed kind');
    end;
  end;

begin
  AChange := Default(TNyxColumnArrangementChange);
  Result := False;

  if (AButton = nil) or (AMountedRoot = nil) or
    (AButton.Prop(CArrangementEditor) = '') then
  begin
    Exit;
  end;
  LEditorID := AButton.Prop(CArrangementEditor);
  LEditor := nil;
  LAncestor := AButton;
  LDepth := 0;
  LFound := False;
  while LAncestor <> nil do
  begin

    if (LEditor = nil) and (LAncestor.SourceID = LEditorID) then
    begin
      LEditor := LAncestor;
    end;

    if LAncestor = AMountedRoot then
    begin
      LFound := True;
      Break;
    end;
    Inc(LDepth);

    if LDepth > NyxMaximumTreeDepth then
    begin
      raise ENyxCollection.Create('Column action ancestry exceeds the mounted tree bounds');
    end;
    LAncestor := LAncestor.Parent;
  end;

  if not LFound or (LEditor = nil) or
    (LEditor.ProjectionKind <> NyxKindName(nkCard)) or
    (AButton.ProjectionKind <> NyxKindName(nkButton)) or
    (AButton.Prop(CEnabled, CTrue) <> CTrue) then
  begin
    raise ENyxCollection.Create('Column editor action is no longer mounted or enabled');
  end;
  LNext := Default(TNyxColumnArrangementChange);
  LNext.FBaseline := NyxColumnArrangementBaseline(AObservation);

  if LEditor.Prop(CArrangementBaseline) <> LNext.FBaseline then
  begin
    raise ENyxCollection.Create('Column editor no longer matches its launch observation');
  end;
  LActionName := AButton.Prop(CArrangementAction);
  LFound := False;
  for LAction := Low(TNyxColumnArrangementAction) to High(TNyxColumnArrangementAction) do
  begin

    if CArrangementActions[LAction] = LActionName then
    begin
      LNext.FAction := LAction;
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Column editor action is unknown');
  end;
  LNext.FPreferences := AObservation.Preferences;
  LSpec := AObservation.Spec;

  if not LSpec.HasLayoutControls then
  begin
    raise ENyxCollection.Create('Column editor launch has no arrangement controls');
  end;

  if LNext.FAction in [ncaaResetOrder, ncaaResetAll] then
  begin
    LExpectedID := NyxColumnArrangementActionID(LEditorID, LNext.FAction);

    if AButton.Prop(CArrangementField) <> '' then
    begin
      raise ENyxCollection.Create('A global reset cannot contain a field target');
    end;

    if LNext.FAction = ncaaResetOrder then
    begin
      LNext.FPreferences := LNext.FPreferences.UseDefaultOrder;
    end
    else
    begin
      LNext.FPreferences := NyxColumnPreferences;
    end;
  end
  else
  begin
    LNext.FField := TNyxColumnIdentity.FromData(
      TNyxDataValue.ParseJSON(AButton.Prop(CArrangementField)));
    LCanonical := LSpec.ColumnIndex(LNext.FField);

    if LCanonical < 0 then
    begin
      raise ENyxCollection.Create('Column editor field is missing or has changed scalar family');
    end;
    LExpectedID := NyxColumnArrangementActionID(LEditorID, AObservation, LNext.FField, LNext.FAction);
    LEntry := AObservation.ColumnAt(LCanonical);
    LPreference := NyxColumnPreference(LNext.FField);
    LIndex := LNext.FPreferences.IndexOf(LNext.FField);

    if LIndex >= 0 then
    begin
      LPreference := LNext.FPreferences.ColumnAt(LIndex);
    end;
    case LNext.FAction of
      ncaaShow, ncaaHide:
        begin
          RequireFeature(nclfVisibility);
          LNext.FPreferences := LNext.FPreferences.WithColumn(
            LPreference.Visible(LNext.FAction = ncaaShow));
        end;
      ncaaApplyWidth:
        begin
          RequireFeature(nclfResize);
          LInput := Input(ncafWidth, nkInput);
          LWire := LInput.Prop(CValue);

          if not TryStrToInt(LWire, LWidth) or (LWidth < 1) or
            (TNyxText(IntToStr(LWidth)) <> LWire) then
          begin
            raise ENyxCollection.Create('Width requires a positive whole number in logical pixels');
          end;
          LSizing := LEntry.Column.Sizing;

          if not LSizing.Defined then
          begin
            LSizing := NyxColumnSizing;
          end;

          if (LWidth < LSizing.Bounds.MinimumValue) or
            (LWidth > LSizing.Bounds.MaximumValue) then
          begin
            raise ENyxCollection.Create('Width is outside this field''s authored bounds');
          end;
          LNext.FPreferences := LNext.FPreferences.WithColumn(LPreference.Width(LWidth));
        end;
      ncaaDefaultWidth:
        begin
          LNext.FPreferences := LNext.FPreferences.WithColumn(LPreference.UseDefaultWidth);
        end;
      ncaaFitWidth:
        begin
          RequireFeature(nclfAutoSize);
        end;
      ncaaApplyPin:
        begin
          RequireFeature(nclfPinning);
          LInput := Input(ncafPin, nkSelect);
          LFound := False;
          for LPin := Low(TNyxColumnPin) to High(TNyxColumnPin) do
          begin

            if LInput.Prop(CValue) = CPinCaptions[LPin] then
            begin
              LNext.FPreferences := LNext.FPreferences.WithColumn(LPreference.Pinned(LPin));
              LFound := True;
              Break;
            end;
          end;

          if not LFound then
          begin
            raise ENyxCollection.Create('Pinned edge requires one of the displayed choices');
          end;
        end;
      ncaaMoveEarlier, ncaaMoveLater:
        begin
          LPosition := -1;
          SetLength(LOrder, AObservation.Count);
          for LIndex := 0 to AObservation.Count - 1 do
          begin
            LOrder[LIndex] := AObservation.ColumnAt(AObservation.OrderedAt(LIndex)).Column.Identity;

            if LOrder[LIndex].Same(LNext.FField) then
            begin
              LPosition := LIndex;
            end;
          end;
          LStep := 1;

          if LNext.FAction = ncaaMoveEarlier then
          begin
            LStep := -1;
          end;

          if not ArrangementMoveAllowed(AObservation, LPosition, LStep) then
          begin
            raise ENyxCollection.Create('Column cannot move across this order boundary');
          end;
          LSwap := LOrder[LPosition];
          LOrder[LPosition] := LOrder[LPosition + LStep];
          LOrder[LPosition + LStep] := LSwap;
          LNext.FPreferences := LNext.FPreferences.Order(LOrder);
        end;
      ncaaResetColumn:
        begin
          LNext.FPreferences := LNext.FPreferences.WithoutColumn(LNext.FField);
        end;
    else
      begin
        raise ENyxCollection.Create('Column editor has an invalid field action');
      end;
    end;
  end;

  if AButton.SourceID <> LExpectedID then
  begin
    raise ENyxCollection.Create('Column action identity does not match its typed target');
  end;
  LNext.FPreferences.Validate;
  LNext.FDefined := True;
  AChange := LNext.Copy;
  Result := True;
end;

function TNyxColumnLayoutEntry.GetWidth: Integer;
begin

  if not FHasWidth then
  begin
    raise ENyxCollection.Create('This column uses host-allocated width');
  end;
  Result := FWidth;
end;

class function TNyxColumnLayoutObservation.Read(
  const ACapture: INyxCollectionColumnsCapture): TNyxColumnLayoutObservation;
var
  LCapture: INyxCollectionColumnsCapture;
  LColumn: TNyxCollectionColumn;
  LPreference: TNyxColumnPreference;
  LTable: TNyxTableLayoutPolicy;
  LIndex, LCanonical, LPreferenceIndex, LCount: Integer;
  LPin: TNyxColumnPin;
  LFeature: TNyxColumnLayoutFeature;
begin
  LCapture := ACapture;

  if LCapture = nil then
  begin
    raise ENyxCollection.Create('Column presentation requires a table capture');
  end;
  Result := Default(TNyxColumnLayoutObservation);
  Result.FSpec := LCapture.Spec.Copy;
  Result.FPreferences := LCapture.Preferences.Copy;
  Result.FSerial := LCapture.Serial;
  Result.FSpec.Validate;
  Result.FPreferences.Validate;

  if not Result.FSpec.Defined or (Result.FSpec.Count = 0) then
  begin
    raise ENyxCollection.Create('Column observation requires defined authored fields');
  end;
  Result.FOrder := Result.FSpec.ResolveColumnOrder(Result.FPreferences);
  LTable := Result.FSpec.TableLayoutPolicy;
  SetLength(Result.FEntries, Result.FSpec.Count);
  for LIndex := 0 to Result.FPreferences.Count - 1 do
  begin

    if Result.FSpec.ColumnIndex(Result.FPreferences.ColumnAt(LIndex).Field) < 0 then
    begin
      raise ENyxCollection.Create('Column observation contains an incompatible field');
    end;
  end;
  for LIndex := 0 to Result.Count - 1 do
  begin
    LColumn := Result.FSpec.ColumnAt(LIndex);
    Result.FEntries[LIndex].FColumn := LColumn.Copy;
    Result.FEntries[LIndex].FVisible := LColumn.LayoutPolicy.IsVisible;
    Result.FEntries[LIndex].FPin := LColumn.LayoutPolicy.Pin;
    Result.FEntries[LIndex].FFeatures := [];
    for LFeature := Low(TNyxColumnLayoutFeature) to High(TNyxColumnLayoutFeature) do
    begin

      if Result.FSpec.HasLayoutControls and LTable.Allows(LFeature) and
        LColumn.LayoutPolicy.Allows(LFeature) then
      begin
        Include(Result.FEntries[LIndex].FFeatures, LFeature);
      end;
    end;
    LPreferenceIndex := Result.FPreferences.IndexOf(LColumn.Identity);

    if LPreferenceIndex >= 0 then
    begin
      LPreference := Result.FPreferences.ColumnAt(LPreferenceIndex);
      Result.FEntries[LIndex].FHasWidth := LPreference.HasWidth;

      if LPreference.HasWidth then
      begin
        Result.FEntries[LIndex].FWidth := LPreference.WidthValue;
      end;

      if LPreference.HasVisibility then
      begin
        Result.FEntries[LIndex].FVisible := LPreference.IsVisible;
      end;

      if LPreference.HasPin then
      begin
        Result.FEntries[LIndex].FPin := LPreference.Pin;
      end;
    end;
  end;
  SetLength(Result.FVisible, Result.Count);
  LCount := 0;
  { Explicit sequence avoids making presentation depend on enum ordinals. }
  for LIndex := 0 to 2 do
  begin
    case LIndex of
      0: LPin := ncpStart;
      1: LPin := ncpNone;
      else LPin := ncpEnd;
    end;
    for LCanonical in Result.FOrder do
    begin

      if Result.FEntries[LCanonical].FVisible and
        (Result.FEntries[LCanonical].FPin = LPin) then
      begin
        Result.FVisible[LCount] := LCanonical;
        Inc(LCount);
      end;
    end;
  end;
  SetLength(Result.FVisible, LCount);
end;

function TNyxColumnLayoutObservation.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TNyxColumnLayoutObservation.GetVisibleCount: Integer;
begin
  Result := Length(FVisible);
end;

function TNyxColumnLayoutObservation.GetSpec: TNyxCollectionViewSpec;
begin
  Result := FSpec.Copy;
end;

function TNyxColumnLayoutObservation.GetPreferences: TNyxColumnPreferences;
begin
  Result := FPreferences.Copy;
end;

function TNyxColumnLayoutObservation.Copy: TNyxColumnLayoutObservation;
var
  LIndex: Integer;
begin
  Result := Default(TNyxColumnLayoutObservation);
  Result.FSpec := FSpec.Copy;
  Result.FPreferences := FPreferences.Copy;
  Result.FSerial := FSerial;
  SetLength(Result.FEntries, Count);
  for LIndex := 0 to Count - 1 do
  begin
    Result.FEntries[LIndex] := FEntries[LIndex];
    Result.FEntries[LIndex].FColumn := FEntries[LIndex].FColumn.Copy;
  end;
  Result.FOrder := System.Copy(FOrder, 0, Length(FOrder));
  Result.FVisible := System.Copy(FVisible, 0, Length(FVisible));
end;

function TNyxColumnLayoutObservation.ColumnAt(ACanonical: Integer): TNyxColumnLayoutEntry;
begin

  if (ACanonical < 0) or (ACanonical >= Count) then
  begin
    raise ENyxCollection.Create('Canonical column is outside its observation');
  end;
  Result := FEntries[ACanonical];
  Result.FColumn := FEntries[ACanonical].FColumn.Copy;
end;

function TNyxColumnLayoutObservation.OrderedAt(AIndex: Integer): Integer;
begin

  if (AIndex < 0) or (AIndex >= Length(FOrder)) then
  begin
    raise ENyxCollection.Create('Column order is outside its observation');
  end;
  Result := FOrder[AIndex];
end;

function TNyxColumnLayoutObservation.VisibleAt(AIndex: Integer): Integer;
begin

  if (AIndex < 0) or (AIndex >= VisibleCount) then
  begin
    raise ENyxCollection.Create('Visible column is outside its observation');
  end;
  Result := FVisible[AIndex];
end;

function NyxColumnPresentation(const AObservation: TNyxColumnLayoutObservation;
  AAvailable: Integer; const ANatural: TNyxFlowSizes): TNyxColumnPresentation;
var
  LPolicies: TNyxColumnSizings;
  LNatural, LWidths: TNyxFlowSizes;
  LEntry: TNyxColumnLayoutEntry;
  LIndex, LCanonical: Integer;
begin

  if (AAvailable < 0) or (Length(ANatural) <> AObservation.Count) then
  begin
    raise ENyxCollection.Create('Column geometry requires a nonnegative clip and canonical measurements');
  end;
  Result := Default(TNyxColumnPresentation);
  Result.FObservation := AObservation.Copy;
  SetLength(Result.FWidths, AObservation.Count);
  SetLength(LPolicies, AObservation.VisibleCount);
  SetLength(LNatural, AObservation.VisibleCount);
  for LIndex := 0 to AObservation.VisibleCount - 1 do
  begin
    LCanonical := AObservation.VisibleAt(LIndex);
    LEntry := AObservation.ColumnAt(LCanonical);
    LPolicies[LIndex] := LEntry.Column.Sizing;

    if LEntry.HasWidth then
    begin

      if not LPolicies[LIndex].Defined then
      begin
        LPolicies[LIndex] := NyxColumnSizing;
      end;
      LPolicies[LIndex] := LPolicies[LIndex].Fixed(LEntry.Width);
    end;
    LNatural[LIndex] := ANatural[LCanonical];
  end;

  if AObservation.VisibleCount = 0 then
  begin
    Exit;
  end;
  LWidths := NyxColumnWidths(AAvailable, LPolicies, LNatural);
  for LIndex := 0 to AObservation.VisibleCount - 1 do
  begin
    LCanonical := AObservation.VisibleAt(LIndex);
    Result.FWidths[LCanonical] := LWidths[LIndex];
    case AObservation.ColumnAt(LCanonical).Pin of
      ncpNone: begin end;
      ncpStart: Inc(Result.FStartWidth, LWidths[LIndex]);
      ncpEnd: Inc(Result.FEndWidth, LWidths[LIndex]);
    end;
  end;
  Result.FPinsSuspended := (Result.FStartWidth + Result.FEndWidth > 0) and
    (Result.FStartWidth + Result.FEndWidth +
      AObservation.Spec.TableLayoutPolicy.MinimumScrollableWidth > AAvailable);

  if Result.FPinsSuspended then
  begin
    Result.FStartWidth := 0;
    Result.FEndWidth := 0;
  end;
end;

function TNyxColumnPresentation.GetObservation: TNyxColumnLayoutObservation;
begin
  Result := FObservation.Copy;
end;

function TNyxColumnPresentation.Width(ACanonical: Integer): Integer;
begin
  FObservation.ColumnAt(ACanonical);
  Result := FWidths[ACanonical];
end;

function TNyxColumnPresentation.EffectivePin(ACanonical: Integer): TNyxColumnPin;
var
  LEntry: TNyxColumnLayoutEntry;
begin
  LEntry := FObservation.ColumnAt(ACanonical);
  Result := ncpNone;

  if LEntry.IsVisible and not FPinsSuspended then
  begin
    Result := LEntry.Pin;
  end;
end;

end.
