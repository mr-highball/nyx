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

unit nyx.binding;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.resource.context,
  nyx.types,
  nyx.data,
  nyx.contract,
  nyx.sliders,
  nyx.state,
  nyx.expressions,
  nyx.publication,
  nyx.resources,
  nyx.binding.types,
  nyx.binding.inputs,
  nyx.model,
  nyx.behavior;

type
  TNyxBindingSync = procedure of object;
  { Optional target synchronization for an admitted, unbound value-only edit.
    The realized node is borrowed for this synchronous call. True means the
    target has synchronized that exact field; False requests ordinary OnSync.
    Full candidate/store validation and publication still precede this hook.
    Raising leaves that admitted projection committed, matching OnSync failure
    semantics. The callback must not retain its borrowed node or retire its view. }
  TNyxBindingValueSync = function(ANode: TNyxNode): Boolean of object;

  { Trusted synchronous adapter observation of exact physical input roles.
    The returned weak proof owns copied facts, never widgets or a renderer.
    CheckReady must be a pure UI-thread observation: no callbacks, allocation
    of controls, scheduling or model mutation. It refuses composition, capture,
    changed drafts and retired/replaced physical generations. Nil refuses an
    installed hook's capture; an absent hook retains model-only operation. }
  TNyxBindingInputCapture = function(const AControls: array of TNyxControlRef):
    INyxPublicationReadiness of object;

  { Synchronize one already committed reset through ordinary target Sync.
    Only the supplied roles may force same-accepted-value draft restoration;
    unrelated drafts retain their normal baselines. The borrowed receiver may
    fail after publication, following the existing committed-notification rule. }
  TNyxBindingInputSync = procedure(const AControls: array of TNyxControlRef) of object;

  { A live view borrows its realized root and runtime store. Neither may be
    freed before this coordinator. Activate owns one subscription token; Retire
    disconnects its authority before the renderer frees controls/model/state.
    Retirement is terminal, not ownership: an entered call still requires this
    coordinator and its borrowed root/store to stay alive until it returns.

    Construction projects and admits the initial snapshot before target controls
    are built. Activate is separate so a failed factory can discard a candidate
    without subscribing an incomplete view. Commands stage a complete independent
    tree; state validators admit it before any accepted properties change.

    Inputs exposes a revocable typed batch port over this same owner, not a
    second state engine. Prepared input/state publication reserves this view;
    its detached projection installs before notification and preserves nodes.
    OnSync updates existing target controls. A notification exception means state
    is already committed, matching TNyxState's contract. Semantic callbacks belong
    to the adapter and run only after an accepted command returns. }
  TNyxLiveBindings = class
  private
    FRoot: TNyxNode;
    FState: TNyxState;
    FSubscription: TNyxStateSubscription;
    FOnSync: TNyxBindingSync;
    FOnValueSync: TNyxBindingValueSync;
    FOnInputCapture: TNyxBindingInputCapture;
    FOnInputSync: TNyxBindingInputSync;
    FCommandRoot: TNyxNode;
    FCommandProjected: Boolean;
    FResourcePort: IInterface; { managed revocable weak receiver, never owns this view }
    FResourcePreparation: TObject; { weak; one resource or scalar projection stage }
    FInputSerial: Integer; { finite view-local accepted-command observation }
    function GetInputs: INyxInputCommands;
    function GetRetired: Boolean;
    procedure SetOnSync(AValue: TNyxBindingSync);
    procedure SetOnValueSync(AValue: TNyxBindingValueSync);
    procedure SetOnInputCapture(AValue: TNyxBindingInputCapture);
    procedure SetOnInputSync(AValue: TNyxBindingInputSync);
    function GetRefreshReady: Boolean;
    function GetCommandActive: Boolean;
    function GetResourcePublicationBusy: Boolean;
    function GetHasBindings: Boolean;
    procedure ValidateCandidate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    procedure StateChanged(AState: TNyxState; AChanges: TNyxStateChanges);
    procedure Synchronize;
    function PrepareResourceReload(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef): TNyxNode;
    procedure CaptureEventValue(AOrigin: TNyxNode; const ADispatch: TNyxDispatch;
      var AInfo: TNyxEventInfo);
    function Command(ANode: TNyxNode; ATrigger: TNyxTrigger; const AValue: TNyxText;
      AEdit: Boolean): TNyxDispatch;
  public
    constructor Create(ARoot: TNyxNode; AState: TNyxState);
    destructor Destroy; override;
    procedure Activate;
    { Terminal, idempotent UI-thread authority retirement. Revoke prepared
      resource receivers and both target callbacks before removing the exact
      state subscription. An already-entered command may still finish or report
      committed notification failure; this method neither rolls it back nor
      owns/defer-frees its coordinator, root or store. Activate and reassignment
      of either target callback cannot revive it; clearing callbacks stays legal. }
    procedure Retire;
    { UI-thread resource/locale reload. All selectors and concrete properties
      validate on an independent candidate before existing controls synchronize.
      Invalid data retains accepted values/context. A sync exception follows
      publication. Input events never write a packed file. }
    procedure ReloadResources(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
    { Contribute scalar/context adoption to a coordinated application commit.
      Holds the state snapshot, owns a detached clone and revokes its borrowed
      root/receiver when this coordinator is destroyed. Install only exchanges
      already allocated storage; target synchronization occurs during Notify. }
    function PrepareResourcePublication(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef): INyxPreparedPublication;
    { Explicit DOM/LCL wire boundary. Scalar decoding and payload capture happen
      in the detached candidate before publication; handwritten application
      state writes use typed references/arguments on TNyxState instead. }
    function Edit(ANode: TNyxNode; const AValue: TNyxText): TNyxDispatch;
    { Application commands accept runtime trigger enums; designer triggers fail
      admission. Returned Source/Target borrow this view; Info.Copy owns data. }
    function Dispatch(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxDispatch; reintroduce;
    { Read-only focus notification captures the current accepted domain/value
      without cloning a view, changing state or executing compound actions. }
    function Focus(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxDispatch;
    { Keyboard signals capture accepted value/domain without running actions or
      writing state. Text changes continue through Edit, including IME input. }
    function Keyboard(ANode: TNyxNode; ATrigger: TNyxTrigger;
      const AStroke: TNyxKeyStroke): TNyxDispatch;
    { Read-only interaction snapshot. Pointer/text/extension adapters use this
      route so notifications cannot accidentally execute a click action. }
    function Signal(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxDispatch;
    { Adapter phase supplier: captures that phase's own declared value source
      and scalar domain. The returned record owns its data, with no node handles. }
    function SignalSnapshot(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxEventInfo;
    { Proposed complete text replacement. Captures the accepted baseline before
      admission; a sequential adapter input response may reject this proposal. }
    function ProposeText(ANode: TNyxNode; const AValue: TNyxText): TNyxDispatch;
    { Prepare independent refresh roots against this view's current store. The
      caller owns both returned roots on success; failure returns nil roots and
      changes neither the mounted tree nor the store/subscription. Exact node
      identities/scopes and ordered binding descriptors must remain unchanged.
      A command, candidate commit or notification in progress refuses reentry.
      Invalid projection/allocation raises after releasing detached candidates.
      This is a UI-thread admission boundary, not a cross-thread store lock. }
    function TryPrepareRefresh(ACandidate, ABaseline: TNyxNode;
      out AProjectedCandidate, AProjectedBaseline: TNyxNode): Boolean;
    { Fresh UI-thread observations, never cached admission. Unbound views avoid
      extra projection clones but must still refuse command/notification reentry. }
    property RefreshReady: Boolean read GetRefreshReady;
    { UI-thread observation for adapter notifications which must wait until an
      accepted control command returns. State-store notification alone does not
      imply a control command, and this flag grants no reentry authority. }
    property CommandActive: Boolean read GetCommandActive;
    property ResourcePublicationBusy: Boolean read GetResourcePublicationBusy;
    { Detached admission for application-wide validators. No resources,
      properties, subscribers or target handles are published by this check. }
    procedure ValidateResourceReload(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
    property HasBindings: Boolean read GetHasBindings;
    { Managed weak commands; retirement revokes retained ports/captures. }
    property Inputs: INyxInputCommands read GetInputs;
    property OnSync: TNyxBindingSync read FOnSync write SetOnSync;
    { Opt in to exact field synchronization without replacing the legacy full
      callback contract. Other property effects, state bindings or unsupported
      target controls always use OnSync. Neither callback owns its receiver. }
    property OnValueSync: TNyxBindingValueSync read FOnValueSync write SetOnValueSync;
    { Adapter-only physical admission and selective reset notification. Both
      borrow their receiver and revoke during Retire. Install these together
      before exposing Inputs; ordinary model-only consumers may leave them nil. }
    property OnInputCapture: TNyxBindingInputCapture read FOnInputCapture write SetOnInputCapture;
    property OnInputSync: TNyxBindingInputSync read FOnInputSync write SetOnInputSync;
  end;

{ Shared projection/admission for renderers, application validators and tests.
  Only runtime trees are mutated; caller owns both arguments. All bound keys must
  exist with their exact declared kinds. Concrete input types, ranges and layout
  constraints are checked before admission. }
procedure ApplyNyxBindings(ARoot: TNyxNode; AState: TNyxState);

{ Concrete control admission also supplies authoring choices. The node is
  borrowed, normally an independently realized selection/part. Empty means
  unsupported; returned sets/arrays own no mutable model or target references. }
{ Image resource projections are not scalar state targets. Discovery and
  authoring use this separate capability; real binding admission checks it too. }
function NyxSupportsResourceImage(ANode: TNyxNode): Boolean;
function NyxBindingKinds(ANode: TNyxNode;
  ATarget: TNyxBindingProperty): TNyxStateKinds;
function NyxBindingTargets(ANode: TNyxNode): TNyxBindingTargetInfos;

implementation

uses
  nyx.schema,
  nyx.date.range,
  nyx.images,
  nyx.interaction,
  nyx.text.index,
  nyx.projection.refresh;

type
  { Shared revocable weak receiver for resource and scalar preparations. The
    port never owns its live binding, borrowed root, state or platform host. }
  IBindingResourcePort = interface
    ['{BE01BE99-9604-42D3-B844-F5380C273AC2}']
    function Owner: TNyxLiveBindings;
    procedure Revoke;
  end;
  { One weak implementation supplies both optional command facets. Its owner
    revokes it before subscription/control teardown; retained ports stay inert. }
  TBindingResourcePort = class(TInterfacedObject, IBindingResourcePort, INyxInputCommands)
  private
    FOwner: TNyxLiveBindings;
  public
    constructor Create(AOwner: TNyxLiveBindings);
    function Owner: TNyxLiveBindings;
    procedure Revoke;
    function GetConnected: Boolean;
    function Capture(const AControls: array of TNyxControlRef): INyxInputCapture;
    function Prepare(const ACapture: INyxInputCapture;
      const AValues: array of TNyxInputValue): INyxPreparedPublication;
  end;
  { Owns one detached resource projection and the old values displaced during
    allocation-free installation. The held store and weak view port end at Retire. }
  TBindingResourcePreparation = class(TInterfacedObject, INyxPreparedPublication)
  private
    FPort: IBindingResourcePort;
    FHold: INyxStateSnapshotHold;
    FCandidate: TNyxNode;
    FValidated: Boolean;
    FInstalled: Boolean;
    FRetired: Boolean;
  public
    constructor Create(AOwner: TNyxLiveBindings; const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
    destructor Destroy; override;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

  { Private proof unwraps only this exact canonical public snapshot. Returning
    another object's proof cannot impersonate its public capture identity. }
  IBindingInputProof = interface(IInterface)
    ['{AF7054F1-20CA-4ABA-A6D1-294694189975}']
    function SnapshotObject: TObject;
  end;
  { Copied descriptors have no executable/runtime owners, including expression
    declarations. Each role has its own independent binding membership. }
  TInputBindingSpecs = array of TNyxBindingSpec;
  { A small observation retains only copied role facts and an inert weak port.
    No original node/tree/store is kept alive by a caller's old capture. }
  TBindingInputCapture = class(TInterfacedObject, INyxInputCapture, IBindingInputProof)
  private
    FPort: IBindingResourcePort;
    FSerial: Integer;
    FStateRevision: Integer;
    FControls: array of TNyxControlRef;
    FPresent: array of Boolean;
    FValues: array of TNyxStateValue;
    FKinds: array of TNyxStateKind;
    FProps: array of TNyxStrings;
    FContracts: array of TNyxText;
    FBindings: array of TInputBindingSpecs;
    FPolicies: array of TNyxInteractionPolicy;
    FPhysical: INyxPublicationReadiness;
    procedure CheckIndex(AIndex: Integer);
    procedure RequireCurrent(AOwner: TNyxLiveBindings);
  public
    constructor Create(AOwner: TNyxLiveBindings;
      const AControls: array of TNyxControlRef);
    destructor Destroy; override;
    function SnapshotObject: TObject;
    function GetCount: Integer;
    function ControlAt(AIndex: Integer): TNyxControlRef;
    function HasValueAt(AIndex: Integer): Boolean;
    function ValueAt(AIndex: Integer): TNyxStateValue;
  end;
  { One scalar projection participant joins the existing prepared state owner.
    Only independent clones are retained. The live owner is borrowed through
    its revocable port, never owned by this preparation or its public group.
    Final readiness compares owned facts without cloning, executing expressions
    or invoking validators. Installation exchanges admitted property storage;
    the original nodes and all controls keep their identities. }
  TBindingInputPreparation = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FPort: IBindingResourcePort;
    FCapture: INyxInputCapture;
    { Borrowed canonical proof; FCapture retains this exact immutable object. }
    FSnapshot: TBindingInputCapture;
    FBefore: TNyxNode;
    FCandidate: TNyxNode;
    FStateStage: INyxPreparedPublication;
    FResetControls: array of TNyxControlRef;
    FValidated: Boolean;
    FInstalled: Boolean;
    FNotified: Boolean;
    FRetired: Boolean;
    procedure RequireOwner;
  public
    constructor Create(AOwner: TNyxLiveBindings;
      const ACapture: INyxInputCapture; const AValues: array of TNyxInputValue);
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

constructor TBindingResourcePort.Create(AOwner: TNyxLiveBindings);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TBindingResourcePort.Owner: TNyxLiveBindings;
begin
  Result := FOwner;
end;

procedure TBindingResourcePort.Revoke;
begin
  FOwner := nil;
end;

constructor TBindingResourcePreparation.Create(AOwner: TNyxLiveBindings;
  const AResources: INyxResources; const ALocale, AFallback: TNyxLocaleRef);
begin
  inherited Create;

  if (AOwner.FSubscription = nil) or not AOwner.FSubscription.Connected or
    (AOwner.FCommandRoot <> nil) or (AOwner.FResourcePreparation <> nil) then
  begin
    raise ENyxState.Create('Resource publication requires active idle bindings');
  end;
  FHold := AOwner.FState.HoldSnapshot;
  FPort := AOwner.FResourcePort as IBindingResourcePort;
  AOwner.FResourcePreparation := Self;
  FCandidate := AOwner.FRoot.Clone;
  FCandidate.BindResources(AResources, ALocale, AFallback);
  ApplyNyxBindings(FCandidate, AOwner.FState);
end;

destructor TBindingResourcePreparation.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TBindingResourcePreparation.Validate;
begin

  if FRetired or FValidated or (FPort.Owner = nil) then
  begin
    raise ENyxState.Create('Resource scalar preparation is retired or already admitted');
  end;

  if FPort.Owner.FInputSerial = High(Integer) then
  begin
    raise ENyxState.Create('Live input observation serial is exhausted');
  end;
  FValidated := True;
end;

procedure TBindingResourcePreparation.Install;
begin

  if FPort.Owner <> nil then
  begin
    FPort.Owner.FRoot.ExchangeResourceProjection(FCandidate);
    Inc(FPort.Owner.FInputSerial);
  end;
  FInstalled := True;
end;

procedure TBindingResourcePreparation.Notify;
var
  LOwner: TNyxLiveBindings;
begin

  if not FInstalled or FRetired then
  begin
    raise ENyxState.Create('Resource scalar notification requires installed data');
  end;
  LOwner := FPort.Owner;

  if LOwner <> nil then
  begin
    LOwner.Synchronize;
  end;
end;

procedure TBindingResourcePreparation.Retire;
var
  LOwner: TNyxLiveBindings;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if FPort <> nil then
  begin
    LOwner := FPort.Owner;

    if (LOwner <> nil) and (LOwner.FResourcePreparation = Self) then
    begin
      LOwner.FResourcePreparation := nil;
    end;
  end;
  FreeAndNil(FCandidate);
  FHold := nil;
  FPort := nil;
end;

function ScalarText(const AValue: TNyxStateValue): TNyxText;
begin
  case AValue.Kind of
    nskText:
      begin
        Result := AValue.TextValue;
      end;
    nskBoolean:
      begin
        Result := 'false';

        if AValue.BooleanValue then
        begin
          Result := 'true';
        end;
      end;
    nskInteger:
      begin
        Result := IntToStr(AValue.IntegerValue);
      end;
    nskNumber:
      begin
        Result := AValue.NumberText;
      end;
  end;
end;

function ReadScalar(const AText: TNyxText; AKind: TNyxStateKind): TNyxStateValue;
var
  LInteger: Integer;
  LNumber: Double;
begin
  case AKind of
    nskText:
      begin
        Result := TNyxStateValue.FromText(AText);
      end;
    nskBoolean:
      begin

        if (AText <> 'true') and (AText <> 'false') then
        begin
          raise ENyxState.Create('Boolean control value requires true or false');
        end;
        Result := TNyxStateValue.FromBoolean(AText = 'true');
      end;
    nskInteger:
      begin

        if not TryNyxInteger(AText, LInteger) then
        begin
          raise ENyxState.Create('Integer control value requires a complete signed integer');
        end;
        Result := TNyxStateValue.FromInteger(LInteger);
      end;
    nskNumber:
      begin

        if not TryNyxStateNumber(AText, LNumber) then
        begin
          raise ENyxState.Create('Number control value requires a complete finite decimal');
        end;
        Result := TNyxStateValue.FromNumber(LNumber);
      end;
  end;
end;

function NyxSupportsResourceImage(ANode: TNyxNode): Boolean;
begin
  Result := (ANode <> nil) and (ANode.ProjectionKind = 'image');
end;

{ Scalar input admission is shared by bound projections and explicit live
  batches. It never invents a binding/state key merely to validate an unbound
  control. Stored state remains exact; this result is the portable input wire. }
function InputProjectionText(ANode: TNyxNode;
  const AValue: TNyxStateValue): TNyxText;
begin
  Result := ScalarText(AValue);

  if Pos(#0, Result) > 0 then
  begin
    { State/persistence preserve NUL, but native text widgets use terminated
      strings. Refuse a lossy portable control projection before publication. }
    raise ENyxState.Create('Portable control text cannot contain NUL: ' + ANode.ID);
  end;

  if AValue.Kind <> nskText then
  begin
    Exit;
  end;

  if (ANode.ProjectionKind = 'memo') or (ANode.ProjectionKind = 'code-editor') then
  begin
    { Both textarea and LCL memo editing expose LF. Keep exact stored defaults
      until an actual edit; normalized projection prevents a synthetic change
      from rewriting CRLF data merely because a control was mounted. }
    Result := StringReplace(Result, #13#10, #10, [rfReplaceAll]);
    Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
  end
  else if (ANode.ProjectionKind = 'input') or (ANode.ProjectionKind = 'select') or
    (ANode.ProjectionKind = 'date') or (ANode.ProjectionKind = 'time') or
    (ANode.ProjectionKind = 'color') then
  begin

    if (Pos(#10, Result) > 0) or (Pos(#13, Result) > 0) then
    begin
      raise ENyxState.Create('Single-line control binding cannot contain line breaks: ' + ANode.ID);
    end;
  end;
end;

function ProjectionText(ANode: TNyxNode; const ASpec: TNyxBindingSpec;
  const AValue: TNyxStateValue): TNyxText;
begin

  if ASpec.Target = bpValue then
  begin
    Exit(InputProjectionText(ANode, AValue));
  end;
  Result := ScalarText(AValue);

  if (ASpec.Target in [bpText, bpPlaceholder, bpHint, bpAccessibleName]) and
    (Pos(#0, Result) > 0) then
  begin
    raise ENyxState.Create('Portable control text cannot contain NUL: ' + ANode.ID);
  end;
end;

function NyxBindingKinds(ANode: TNyxNode;
  ATarget: TNyxBindingProperty): TNyxStateKinds;
var
  LProperties: TNyxPropertyInfos;
  LIndex: Integer;
  LFound: Boolean;
  LDomain: TNyxValueDomain;
begin
  Result := [];

  if ANode = nil then
  begin
    raise ENyxState.Create('Binding choices require a control');
  end;

  if ATarget = bpImage then
  begin
    { An image never advertises scalar text-state binding merely because its
      persisted source wire is textual. The public selector retains its type. }
    Exit;
  end;
  { A compound exposes a declared self value or named fields; container shape
    never grants four arbitrary scalar choices. Numbers permit the integral
    subset too, while the binding descriptor retains the selected exact kind. }

  if ATarget = bpValue then
  begin
    LDomain := NyxNodeValueDomain(ANode);

    if LDomain.Defined then
    begin
      Result := [LDomain.Kind];

      if LDomain.Kind = nskNumber then
      begin
        Include(Result, nskInteger);
      end;
    end;
    Exit;
  end;
  LFound := False;
  LProperties := NyxProperties(ANode);
  for LIndex := 0 to Length(LProperties) - 1 do
  begin

    if LProperties[LIndex].Key = NyxBindingPropertyName(ATarget) then
    begin
      { A retained unsupported attribute is persistence metadata, not a new
        standard binding capability. Creator schemas may explicitly support
        their own projection; both missing grades cannot authorize this mode. }

      if (ATarget = bpIndeterminate) and
        (LProperties[LIndex].Support.Browser = ncMissing) and
        (LProperties[LIndex].Support.Native = ncMissing) then
      begin
        Exit;
      end;
      LFound := True;
      case LProperties[LIndex].ValueType of
        npBoolean:
          begin
            Result := [nskBoolean];
          end;
        npInteger:
          begin
            Result := [nskInteger];
          end;
        npText, npLines:
          begin
            Result := [nskText];
          end;
        else
          begin
            { Choice/reference metadata does not infer a scalar binding family.
              Keep the explicit domain or existing target default. }
          end;
      end;
      Break;
    end;
  end;

  if (ATarget = bpValue) and (ANode.ProjectionKind = 'input') and
    (ANode.Prop('input-type') = 'number') then
  begin
    Result := [nskInteger, nskNumber];
  end;
  { Recipes may expose a semantic value on a container, independent of the
    physical projection. The descriptor supplies that value's exact scalar kind.
    Pressed is the public Boolean button state used by selection recipes. }


  if (ATarget = bpPressed) and
    ((ANode.ProjectionKind = 'button') or (ANode.ProjectionKind = 'link')) then
  begin
    LFound := True;
    Result := [nskBoolean];
  end;

  if not LFound then
  begin
    Result := [];
  end
  else if ATarget = bpText then
  begin
    Result := [nskText, nskBoolean, nskInteger, nskNumber];
  end;
end;

function NyxBindingTargets(ANode: TNyxNode): TNyxBindingTargetInfos;
var
  LTarget: TNyxBindingProperty;
  LKinds: TNyxStateKinds;
  LIndex: Integer;
begin
  Result := nil;
  for LTarget := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
  begin
    LKinds := NyxBindingKinds(ANode, LTarget);

    if LKinds <> [] then
    begin
      LIndex := Length(Result);
      SetLength(Result, LIndex + 1);
      Result[LIndex].Target := LTarget;
      Result[LIndex].Title := NyxBindingPropertyTitle(LTarget);
      Result[LIndex].ValueKinds := LKinds;
    end;
  end;
end;

procedure CheckTarget(ANode: TNyxNode; const ASpec: TNyxBindingSpec);
begin

  if (ASpec.Source = bsResourceImage) and NyxSupportsResourceImage(ANode) then
  begin
    Exit;
  end;

  if not (ASpec.ValueKind in NyxBindingKinds(ANode, ASpec.Target)) then
  begin
    raise ENyxState.Create('Binding target/kind is unsupported on ' + ANode.ID);
  end;
end;

{ Numeric bounds belong to the input, independently of whether Value has a
  state binding. Explicit commands must obey the same physical/domain scale. }
procedure CheckScalarRange(ANode: TNyxNode; const AValue: TNyxStateValue);
var
  LValue: Double;
  LScale: TNyxSliderScale;
  LMinimum: Integer;
  LMaximum: Integer;
  LDefaultMinimum: TNyxText;
  LDefaultMaximum: TNyxText;
begin

  if not (AValue.Kind in [nskInteger, nskNumber]) then
  begin
    Exit;
  end;

  if ANode.ProjectionKind = 'slider' then
  begin
    { Declared numeric bounds/choices govern the private physical scale.
      Legacy min/max only supply fallback bounds for an unbounded domain. }
    LScale := TNyxSliderScale.Create(NyxNodeValueDomain(ANode),
      StrToIntDef(ANode.Prop('min'), 0), StrToIntDef(ANode.Prop('max'), 100),
      StrToIntDef(ANode.Prop('slider-intervals'), 1000));
    LScale.PositionOf(InputProjectionText(ANode, AValue));
    Exit;
  end;
  LValue := 0;

  if AValue.Kind = nskInteger then
  begin
    LValue := AValue.IntegerValue;
  end
  else
  begin
    LValue := AValue.NumberValue;
  end;
  LDefaultMinimum := '';
  LDefaultMaximum := '';

  if (ANode.ProjectionKind = 'spin') or (ANode.ProjectionKind = 'slider') or
    (ANode.ProjectionKind = 'progress') then
  begin
    LDefaultMinimum := '0';
    LDefaultMaximum := '100';
  end;

  if TryNyxInteger(ANode.Prop('min', LDefaultMinimum), LMinimum) and
    (LValue < LMinimum) then
  begin
    raise ENyxState.Create('Bound value is below its minimum on ' + ANode.ID);
  end;

  if TryNyxInteger(ANode.Prop('max', LDefaultMaximum), LMaximum) and
    (LValue > LMaximum) then
  begin
    raise ENyxState.Create('Bound value exceeds its maximum on ' + ANode.ID);
  end;
end;

procedure CheckValueRange(ANode: TNyxNode; const ASpec: TNyxBindingSpec;
  const AValue: TNyxStateValue);
begin

  if ASpec.Target = bpValue then
  begin
    CheckScalarRange(ANode, AValue);
  end;
end;

{ Borrow the accepted and completely projected prospective trees. Changed
  scalar values and bounds share admission even when Value is unbound. State
  validation already owns this detached projection; no extra tree is allocated.
  An unchanged legacy face is not reinterpreted merely for an unrelated edit. }
procedure CheckInputChanges(ABefore, AAfter: TNyxNode);
var
  LIndex: Integer;
  LDomain: TNyxValueDomain;
  LValue: TNyxStateValue;
begin

  if (ABefore.ID <> AAfter.ID) or (ABefore.Count <> AAfter.Count) then
  begin
    raise ENyxState.Create('Scalar commands cannot change their mounted tree shape');
  end;
  ValidateNyxDateRangeValues(AAfter);

  if (ABefore.Prop('value') <> AAfter.Prop('value')) or
    (ABefore.Prop('min') <> AAfter.Prop('min')) or
    (ABefore.Prop('max') <> AAfter.Prop('max')) then
  begin
    LDomain := NyxNodeValueDomain(AAfter);

    if LDomain.Defined then
    begin
      LValue := ReadScalar(AAfter.Prop('value'), LDomain.Kind);
      InputProjectionText(AAfter, LValue);
      CheckScalarRange(AAfter, LValue);
    end;
  end;
  for LIndex := 0 to AAfter.Count - 1 do
  begin
    CheckInputChanges(ABefore.Children[LIndex], AAfter.Children[LIndex]);
  end;
end;

procedure ApplyNyxBindings(ARoot: TNyxNode; AState: TNyxState);

type
  { One invocation owns these copied admitted values. No cache survives a state
    candidate, resource projection, command or view. Nodes are borrowed from
    this exact root only; contracts are compared before reusing an entry. }
  TComputedProjectionValue = record
    Node: TNyxNode;
    Spec: TNyxBindingSpec;
    Value: TNyxStateValue;
  end;
var
  LExpressionInputs: INyxExpressionInputs;
  LComputedValues: array of TComputedProjectionValue;
  LComputedIndex: TNyxTextIndex;

  function ComputedKey(ANode: TNyxNode; const ASpec: TNyxBindingSpec): TNyxText;
  begin
    { Length-prefixed identity plus closed target avoids delimiter ambiguity
      for arbitrary admitted Unicode IDs. One projection has unique node IDs. }
    Result := TNyxText(IntToStr(Length(ANode.ID))) + ':' + ANode.ID + ':' +
      TNyxText(IntToStr(Ord(ASpec.Target)));
  end;

  function ComputedIndex(ANode: TNyxNode; const ASpec: TNyxBindingSpec): Integer;
  var
    LIndex: Integer;
  begin
    Result := -1;

    if LComputedIndex = nil then
    begin
      Exit;
    end;
    LIndex := LComputedIndex.IndexOf(ComputedKey(ANode, ASpec));

    if (LIndex >= 0) and (LComputedValues[LIndex].Node = ANode) and
      LComputedValues[LIndex].Spec.Same(ASpec) then
    begin
      Result := LIndex;
    end;
  end;

  function Value(ANode: TNyxNode; const ASpec: TNyxBindingSpec): TNyxStateValue;
  var
    LIndex: Integer;
  begin

    if ASpec.Source = bsResource then
    begin
      Exit(ANode.ReadResource(ASpec.ResourceValue));
    end;

    if ASpec.Source = bsExpression then
    begin
      LIndex := ComputedIndex(ANode, ASpec);

      if LIndex >= 0 then
      begin
        Exit(LComputedValues[LIndex].Value.Copy);
      end;

      if LExpressionInputs = nil then
      begin
        { Capture once for this complete staged projection. All expressions
          evaluate the same independently owned frame before publication. }
        LExpressionInputs := CaptureNyxExpressionInputs(AState);
      end;
      Result := ANode.BindingExpression(ASpec.Target).Value(LExpressionInputs);
      LIndex := Length(LComputedValues);
      SetLength(LComputedValues, LIndex + 1);
      LComputedValues[LIndex].Node := ANode;
      LComputedValues[LIndex].Spec := ASpec.Copy;
      LComputedValues[LIndex].Value := Result.Copy;

      if LComputedIndex = nil then
      begin
        LComputedIndex := TNyxTextIndex.Create;
      end;
      LComputedIndex.AddFirst(ComputedKey(ANode, ASpec), LIndex);
      Exit;
    end;
    Result := AState.Value(ASpec.StateName);
  end;

  procedure Project(ANode: TNyxNode);
  var
    LIndex: Integer;
    LSpec: TNyxBindingSpec;
    LValue: TNyxStateValue;
    LText: TNyxText;
    LImage: TNyxImageSource;
  begin
    for LIndex := 0 to ANode.BindingCount - 1 do
    begin
      LSpec := ANode.Bindings[LIndex];

      if (LSpec.Source <> bsExpression) or (ComputedIndex(ANode, LSpec) < 0) then
      begin
        { The second property pass sees the same exact admitted descriptor.
          New/changed contracts still run full validation before projection. }
        LSpec.Validate;
      end;

      if LSpec.Cleared then
      begin
        Continue;
      end;
      CheckTarget(ANode, LSpec);
      { Prepared resource reload resolves a typed source before changing the
        candidate. Ordinary adapter synchronization then owns decoding/events;
        no second network loader or scalar coercion is introduced here. }

      if LSpec.Source = bsResourceImage then
      begin
        { A valid pending hosted selector projects no image until the existing
          application loader publishes bytes. No fallback or successful load is
          fabricated; diagnostics remain owned by the resource broker. }

        if not ANode.TryReadResourceImage(LSpec.ResourceImage, LImage) then
        begin
          LImage := NyxNoImage;
        end;
        ANode.Configure.Source(LImage).Done;
        Continue;
      end;
      LValue := Value(ANode, LSpec);

      if LValue.Kind <> LSpec.ValueKind then
      begin
        raise ENyxState.Create('Bound state kind changed: ' + LSpec.StateName);
      end;
      ANode.SetProp(NyxBindingPropertyName(LSpec.Target), ProjectionText(ANode, LSpec, LValue));
    end;
    { Unbound multiline fields use the same physical editing convention as
      bound fields. Normalize only the runtime projection, leaving authored
      source/defaults exact and avoiding a synthetic mount-time change. }

    if (ANode.ProjectionKind = 'memo') or (ANode.ProjectionKind = 'code-editor') then
    begin
      LText := ANode.Prop('value');

      if Pos(#13, LText) > 0 then
      begin
        LText := StringReplace(LText, #13#10, #10, [rfReplaceAll]);
        ANode.SetProp('value', StringReplace(LText, #13, #10, [rfReplaceAll]));
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Project(ANode.Children[LIndex]);
    end;
  end;

  procedure CheckRanges(ANode: TNyxNode);
  var
    LIndex: Integer;
    LSpec: TNyxBindingSpec;
  begin
    { Relational values are authoritative only after both full projection passes.
      Shared endpoint keys and atomic multi-key updates must be compared together. }
    ValidateNyxDateRangeValues(ANode);
    for LIndex := 0 to ANode.BindingCount - 1 do
    begin
      LSpec := ANode.Bindings[LIndex];

      if not LSpec.Cleared then
      begin
        { Image sources have already been projected through typed admission.
          They cannot be read as a scalar merely to inspect numeric ranges. }

        if LSpec.Source <> bsResourceImage then
        begin
          CheckValueRange(ANode, LSpec, Value(ANode, LSpec));
        end;
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      CheckRanges(ANode.Children[LIndex]);
    end;
  end;

begin
  LComputedIndex := nil;
  try

    if (ARoot = nil) or (AState = nil) or not ARoot.IsRealized then
    begin
      raise ENyxState.Create('Bindings require a realized view and runtime state');
    end;
    Project(ARoot);
    PrepareNyxBehavior(ARoot);
    { Explicit bound button state takes precedence over derived selection state. }
    Project(ARoot);
    ValidateNyxPropertyTree(ARoot);
    CheckRanges(ARoot);
  finally
    LComputedIndex.Free;
  end;
end;

procedure CopyProjection(ATarget, ASource: TNyxNode);
var
  LIndex: Integer;
begin
  { Commands retain the tree's shape/identity. Copy only properties, preserving
    all renderer bindings and caller-borrowed node/control pointers. }
  ATarget.Props.Assign(ASource.Props);
  for LIndex := 0 to ATarget.Count - 1 do
  begin
    CopyProjection(ATarget.Children[LIndex], ASource.Children[LIndex]);
  end;
end;

{ Compare the complete staged projection before publication. Only the exact
  edited leaf's value may differ. This is a synchronization plan, never a
  replacement for full command validation or a shortcut through atomic admission. }
function OnlyEditedValue(ABefore, AAfter: TNyxNode;
  const AEditedID: TNyxText): Boolean;
var
  LIndex: Integer;
  LKey: TNyxText;
begin
  Result := False;

  if (ABefore.ID <> AAfter.ID) or (ABefore.Count <> AAfter.Count) then
  begin
    Exit;
  end;
  for LIndex := 0 to ABefore.Props.Count - 1 do
  begin
    LKey := ABefore.Props.Names[LIndex];

    if (ABefore.ID = AEditedID) and (ABefore.Count = 0) and
      (LKey = NyxAttributeName(atValue)) then
    begin
      Continue;
    end;

    if (AAfter.Props.IndexOfName(LKey) < 0) or
      (ABefore.StoredProp(LKey) <> AAfter.StoredProp(LKey)) then
    begin
      Exit;
    end;
  end;
  for LIndex := 0 to AAfter.Props.Count - 1 do
  begin
    LKey := AAfter.Props.Names[LIndex];

    if (AAfter.ID = AEditedID) and (AAfter.Count = 0) and
      (LKey = NyxAttributeName(atValue)) then
    begin
      Continue;
    end;

    if ABefore.Props.IndexOfName(LKey) < 0 then
    begin
      Exit;
    end;
  end;
  for LIndex := 0 to ABefore.Count - 1 do
  begin

    if not OnlyEditedValue(ABefore.Children[LIndex], AAfter.Children[LIndex], AEditedID) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

constructor TNyxLiveBindings.Create(ARoot: TNyxNode; AState: TNyxState);
var
  LCandidate: TNyxNode;
begin
  inherited Create;
  FRoot := ARoot;
  FState := AState;
  FResourcePort := TBindingResourcePort.Create(Self);

  if (FRoot = nil) or (FState = nil) then
  begin
    raise ENyxState.Create('Live bindings require a realized root and state');
  end;
  LCandidate := FRoot.Clone;
  try
    ApplyNyxBindings(LCandidate, FState);
    CopyProjection(FRoot, LCandidate);
  finally
    LCandidate.Free;
  end;
end;

destructor TNyxLiveBindings.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TNyxLiveBindings.Retire;
var
  LSubscription: TNyxStateSubscription;
begin
  { The private port is also the terminal marker for an unactivated candidate.
    Detach the field before freeing the token so no cleanup can revive authority. }

  if FResourcePort <> nil then
  begin
    (FResourcePort as IBindingResourcePort).Revoke;
  end;
  FOnSync := nil;
  FOnValueSync := nil;
  FOnInputCapture := nil;
  FOnInputSync := nil;
  LSubscription := FSubscription;
  FSubscription := nil;
  LSubscription.Free;
end;

function TNyxLiveBindings.PrepareResourceReload(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef): TNyxNode;
begin

  if not RefreshReady then
  begin
    raise ENyxState.Create('Resource reload refuses a busy view');
  end;
  Result := FRoot.Clone;
  try
    Result.BindResources(AResources, ALocale, AFallback);
    ApplyNyxBindings(Result, FState);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TNyxLiveBindings.ValidateResourceReload(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef);
var
  LCandidate: TNyxNode;
begin
  LCandidate := PrepareResourceReload(AResources, ALocale, AFallback);
  LCandidate.Free;
end;

procedure TNyxLiveBindings.ReloadResources(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef);
var
  LPrepared: INyxPreparedPublication;
begin
  LPrepared := PrepareResourcePublication(AResources, ALocale, AFallback);
  PublishNyxGroup([LPrepared]);
end;

function TNyxLiveBindings.PrepareResourcePublication(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef): INyxPreparedPublication;
begin
  Result := TBindingResourcePreparation.Create(Self, AResources, ALocale, AFallback);
end;

procedure TNyxLiveBindings.Activate;
begin

  if GetRetired then
  begin
    raise ENyxState.Create('Retired live bindings cannot be activated');
  end;

  if FSubscription <> nil then
  begin
    raise ENyxState.Create('Live bindings are already activated');
  end;
  FSubscription := FState.Subscribe(StateChanged, ValidateCandidate);
end;

function TNyxLiveBindings.GetRetired: Boolean;
begin
  Result := (FResourcePort = nil) or
    ((FResourcePort as IBindingResourcePort).Owner <> Self);
end;

function TNyxLiveBindings.GetInputs: INyxInputCommands;
begin
  Result := FResourcePort as INyxInputCommands;
end;

procedure TNyxLiveBindings.SetOnSync(AValue: TNyxBindingSync);
begin

  if Assigned(AValue) and GetRetired then
  begin
    raise ENyxState.Create('Retired live bindings cannot acquire a target callback');
  end;
  FOnSync := AValue;
end;

procedure TNyxLiveBindings.SetOnValueSync(AValue: TNyxBindingValueSync);
begin

  if Assigned(AValue) and GetRetired then
  begin
    raise ENyxState.Create('Retired live bindings cannot acquire a value callback');
  end;
  FOnValueSync := AValue;
end;

procedure TNyxLiveBindings.SetOnInputCapture(AValue: TNyxBindingInputCapture);
begin

  if Assigned(AValue) and GetRetired then
  begin
    raise ENyxState.Create('Retired live bindings cannot acquire physical input admission');
  end;
  FOnInputCapture := AValue;
end;

procedure TNyxLiveBindings.SetOnInputSync(AValue: TNyxBindingInputSync);
begin

  if Assigned(AValue) and GetRetired then
  begin
    raise ENyxState.Create('Retired live bindings cannot acquire physical input synchronization');
  end;
  FOnInputSync := AValue;
end;

function TNyxLiveBindings.GetRefreshReady: Boolean;
begin
  Result := (FSubscription <> nil) and FSubscription.Connected and
    (FCommandRoot = nil) and (FResourcePreparation = nil) and not FState.Busy;
end;

function TNyxLiveBindings.GetCommandActive: Boolean;
begin
  Result := FCommandRoot <> nil;
end;

function TNyxLiveBindings.GetResourcePublicationBusy: Boolean;
begin
  Result := FResourcePreparation <> nil;
end;

function TNyxLiveBindings.GetHasBindings: Boolean;

  function HasBindings(ANode: TNyxNode): Boolean;
  var
    LChild: Integer;
  begin
    Result := ANode.BindingCount <> 0;

    if Result then
    begin
      Exit;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin

      if HasBindings(ANode.Children[LChild]) then
      begin
        Exit(True);
      end;
    end;
  end;

begin
  Result := HasBindings(FRoot);
end;

function TNyxLiveBindings.TryPrepareRefresh(ACandidate, ABaseline: TNyxNode;
  out AProjectedCandidate, AProjectedBaseline: TNyxNode): Boolean;
var
  LRevision: Integer;
  LResources: INyxResourceContext;
begin
  Result := False;
  AProjectedCandidate := nil;
  AProjectedBaseline := nil;

  if not RefreshReady then
  begin
    Exit;
  end;

  if not SameNyxProjectionBindingContracts(FRoot, ACandidate) or
    not SameNyxProjectionBindingContracts(FRoot, ABaseline) then
  begin
    Exit;
  end;
  LRevision := FState.Revision;
  try
    AProjectedCandidate := ACandidate.Clone;
    AProjectedBaseline := ABaseline.Clone;
    { Responsive/layout candidates originate from authored defaults. Their
      binding contracts already match, so retain the accepted runtime resource
      catalog/locale before comparing current projections. }
    LResources := FRoot.ResourceContext;
    AProjectedCandidate.BindResources(LResources.Snapshot, LResources.Locale, LResources.Fallback);
    AProjectedBaseline.BindResources(LResources.Snapshot, LResources.Locale, LResources.Fallback);
    { Project BOTH copies. Comparing a live value with the original document
      default could otherwise make a layout refresh overwrite accepted state.
      No subscriber is replaced: its validator continues borrowing FRoot, whose
      realized nodes are retained by the subsequent arrangement publication. }
    ApplyNyxBindings(AProjectedCandidate, FState);
    ApplyNyxBindings(AProjectedBaseline, FState);
    Result := not FState.Busy and (FCommandRoot = nil) and
      (FState.Revision = LRevision);
  finally

    if not Result then
    begin
      FreeAndNil(AProjectedBaseline);
      FreeAndNil(AProjectedCandidate);
    end;
  end;
end;

procedure TNyxLiveBindings.ValidateCandidate(ACandidate: TNyxState;
  AChanges: TNyxStateChanges);
var
  LView: TNyxNode;
begin

  if (FInputSerial = High(Integer)) and
    ((AChanges.Count > 0) or AChanges.OrderChanged) then
  begin
    raise ENyxState.Create('Live input observation serial is exhausted');
  end;
  LView := FRoot;

  if FCommandRoot <> nil then
  begin
    LView := FCommandRoot;
  end;
  LView := LView.Clone;
  try
    ApplyNyxBindings(LView, ACandidate);
    CheckInputChanges(FRoot, LView);
  finally
    LView.Free;
  end;
end;

procedure TNyxLiveBindings.Synchronize;
begin

  if not GetRetired and Assigned(FOnSync) then
  begin
    FOnSync;
  end;
end;

procedure TNyxLiveBindings.StateChanged(AState: TNyxState;
  AChanges: TNyxStateChanges);
begin
  { The grouped input participant has already installed the complete admitted
    projection. Its own Notify synchronizes once, even if another state observer
    fails. Reapplying here would overwrite unbound edits or execute projections
    a second time before the independent group notification gets its turn. }

  if (FResourcePreparation is TBindingInputPreparation) and
    TBindingInputPreparation(FResourcePreparation).FInstalled then
  begin
    Exit;
  end;
  Inc(FInputSerial);

  if FCommandRoot <> nil then
  begin
    CopyProjection(FRoot, FCommandRoot);
    FCommandProjected := True;
  end;
  ApplyNyxBindings(FRoot, AState);
  Synchronize;
end;

procedure TNyxLiveBindings.CaptureEventValue(AOrigin: TNyxNode;
  const ADispatch: TNyxDispatch; var AInfo: TNyxEventInfo);
var
  LSpec: TNyxBindingSpec;
  LDomain: TNyxValueDomain;
  LHasDeclared: Boolean;
  LTarget: TNyxNode;
begin
  LHasDeclared := ResolveNyxEventValueContract(AOrigin, ADispatch.Source,
    ADispatch.Target, AInfo.Trigger, LTarget, LDomain);
  AInfo.ValueID := '';
  AInfo.HasValue := False;
  AInfo.ValueKind := nskText;
  AInfo.Value := NyxNull;

  if LDomain.Defined then
  begin
    AInfo.ValueKind := LDomain.Kind;
  end;

  if LTarget = nil then
  begin
    Exit;
  end;
  AInfo.ValueID := LTarget.ID;
  AInfo.HasValue := LTarget.Props.IndexOfName(NyxAttributeName(atValue)) >= 0;

  if not AInfo.HasValue then
  begin
    Exit;
  end;

  if not LDomain.Defined then
  begin
    raise ENyxContract.Create('Event value requires an explicit scalar domain on ' + LTarget.ID);
  end;
  AInfo.Value := LDomain.ReadWire(LTarget.Prop(NyxAttributeName(atValue)));

  if not LHasDeclared and LTarget.FindBinding(bpValue, LSpec) and not LSpec.Cleared then
  begin
    { The number domain admits integral bindings; the descriptor preserves their
      narrower state kind, which must also admit this accepted wire value. }
    ReadScalar(LTarget.Prop(NyxAttributeName(atValue)), LSpec.ValueKind);
    AInfo.ValueKind := LSpec.ValueKind;
  end;
end;

function TNyxLiveBindings.Command(ANode: TNyxNode; ATrigger: TNyxTrigger;
  const AValue: TNyxText;
  AEdit: Boolean): TNyxDispatch;
var
  LNode: TNyxNode;
  LDispatch: TNyxDispatch;
  LAssignments: array of TNyxStateAssignment;
  LRevision: Integer;
  LValueOnly: Boolean;

  procedure Collect(ACandidate, ABaseline: TNyxNode);
  var
    LIndex: Integer;
    LAssignmentIndex: Integer;
    LSpec: TNyxBindingSpec;
    LValue: TNyxStateValue;
    LKey: TNyxText;
    LFound: Boolean;
  begin
    { Admission protects every value, not only fields with two-way bindings.
      Compound actions may target unbound descendants under a read-only scope. }

    if (ACandidate.Prop('value') <> ABaseline.Prop('value')) and
      NyxInteractionPolicy(ABaseline).ReadOnly then
    begin
      raise ENyxState.Create('Read-only control value cannot be edited');
    end;
    for LIndex := 0 to ACandidate.BindingCount - 1 do
    begin
      LSpec := ACandidate.Bindings[LIndex];

      if LSpec.Cleared then
      begin
        Continue;
      end;
      LKey := NyxBindingPropertyName(LSpec.Target);

      if ACandidate.Prop(LKey) = ABaseline.Prop(LKey) then
      begin
        Continue;
      end;

      if (LSpec.Direction <> bdTwoWay) or (LSpec.Target <> bpValue) then
      begin
        raise ENyxState.Create('Control action cannot write a state-only projection');
      end;

      LValue := ReadScalar(ACandidate.Prop(LKey), LSpec.ValueKind);

      if LValue.SameValue(FState.Value(LSpec.StateName)) then
      begin
        Continue;
      end;
      LFound := False;
      for LAssignmentIndex := 0 to Length(LAssignments) - 1 do
      begin

        if LAssignments[LAssignmentIndex].Key = LSpec.StateName then
        begin

          if not LAssignments[LAssignmentIndex].Value.SameValue(LValue) then
          begin
            raise ENyxState.Create('Control action proposes conflicting values for one key');
          end;
          LFound := True;
          Break;
        end;
      end;

      if not LFound then
      begin
        LAssignmentIndex := Length(LAssignments);
        SetLength(LAssignments, LAssignmentIndex + 1);
        LAssignments[LAssignmentIndex] := NyxStateAssign(LSpec.StateName, LValue);
      end;
    end;
    for LIndex := 0 to ACandidate.Count - 1 do
    begin
      Collect(ACandidate.Children[LIndex], ABaseline.Children[LIndex]);
    end;
  end;

begin

  if (FSubscription = nil) or not FSubscription.Connected or
    (FCommandRoot <> nil) or (FResourcePreparation <> nil) or FState.Busy then
  begin
    raise ENyxState.Create('Control command requires active, idle bindings');
  end;

  if FInputSerial = High(Integer) then
  begin
    raise ENyxState.Create('Live input observation serial is exhausted');
  end;

  if (ANode = nil) or (FRoot.Find(ANode.ID) <> ANode) then
  begin
    raise ENyxState.Create('Control command must originate in its mounted view');
  end;

  if not NyxInteractionPolicy(ANode).CanIssueCommand then
  begin
    raise ENyxState.Create('Disabled or hidden controls cannot issue commands');
  end;

  if AEdit and NyxInteractionPolicy(ANode).ReadOnly then
  begin
    raise ENyxState.Create('Read-only control cannot be edited');
  end;
  LRevision := FState.Revision;
  { Commands execute against the same applied presentation as their origin.
    A stored-only clone would silently suppress a responsive visible control
    whose default scope is hidden, or lose a responsive read-only restriction. }
  FCommandRoot := FRoot.CloneRuntimeProjection;
  FCommandProjected := False;
  try
    LNode := FCommandRoot.Find(ANode.ID);

    if AEdit then
    begin
      LNode.SetProp('value', AValue);
    end;
    LDispatch := DispatchNyxBehavior(LNode, ATrigger);
    Collect(FCommandRoot, FRoot);
    { Validate unbound values as well as projected state. A rejected malformed
      native edit must not mutate the accepted model before store admission. }
    ValidateNyxPropertyTree(FCommandRoot);
    { Capture typed data before publication too. Invalid scalar conversion is
      a rejected command, never a failure discovered after state has committed. }
    CaptureEventValue(LNode, LDispatch, LDispatch.Info);

    if Length(LAssignments) = 0 then
    begin
      { Zero-assignment commands do not call state validators. Admit the same
        complete scalar/date-range rules before copying their projection. }
      CheckInputChanges(FRoot, FCommandRoot);
    end;
    LValueOnly := AEdit and Assigned(FOnValueSync) and not HasBindings and
      OnlyEditedValue(FRoot, FCommandRoot, ANode.ID);
    FState.Apply(LAssignments, LRevision);

    if not FCommandProjected then
    begin
      Inc(FInputSerial);
      CopyProjection(FRoot, FCommandRoot);
      ApplyNyxBindings(FRoot, FState);
      { Keep the existing synchronous-before-events contract. An eligible target
        consumes the complete admitted field immediately; all other updates
        retain full synchronization and its notification failure semantics. }

      if not LValueOnly or GetRetired or not Assigned(FOnValueSync) or
        not FOnValueSync(FRoot.Find(ANode.ID)) then
      begin
        Synchronize;
      end;
    end;
    Result.Source := FRoot.Find(LDispatch.Source.ID);
    Result.Target := FRoot.Find(LDispatch.Target.ID);
    Result.Info := LDispatch.Info.Copy;
    Result.Changed := LDispatch.Changed or AEdit;
    Result.Info.Changed := Result.Changed;
  finally
    ReleaseNyxNode(FCommandRoot);
  end;
end;

function TNyxLiveBindings.Edit(ANode: TNyxNode; const AValue: TNyxText): TNyxDispatch;
var
  LBefore: TNyxText;
  LText: Boolean;
begin
  { Diagnose a detached/nil caller before reading its accepted text baseline. }

  if ANode = nil then
  begin
    raise ENyxState.Create('Edits require an accepted mounted control');
  end;
  LText := NyxSupportsTextInput(ANode);
  LBefore := ANode.Prop(NyxAttributeName(atValue));
  Result := Command(ANode, ntChange, AValue, True);

  if LText then
  begin
    Result.Info.HasTextEdit := True;
    Result.Info.TextEdit.Before := LBefore;
    Result.Info.TextEdit.After := AValue;
  end;
end;

function TNyxLiveBindings.Dispatch(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxDispatch;
begin
  Result := Command(ANode, ATrigger, '', False);
end;

function TNyxLiveBindings.Focus(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxDispatch;
begin

  if not (ATrigger in [ntAfterEnter, ntAfterExit]) or
    (FSubscription = nil) or not FSubscription.Connected or
    (FCommandRoot <> nil) or (ANode = nil) or (FRoot.Find(ANode.ID) <> ANode) then
  begin
    raise ENyxState.Create('Focus requires an accepted mounted control and focus trigger');
  end;
  Result := DispatchNyxBehavior(ANode, ATrigger);
  CaptureEventValue(ANode, Result, Result.Info);
end;

function TNyxLiveBindings.Keyboard(ANode: TNyxNode; ATrigger: TNyxTrigger;
  const AStroke: TNyxKeyStroke): TNyxDispatch;
begin

  if not NyxIsKeyboardTrigger(ATrigger) then
  begin
    raise ENyxState.Create('Keyboard signals require a keyboard trigger');
  end;
  Result := Signal(ANode, ATrigger);
  Result.Info.HasKeyboard := True;
  Result.Info.Keyboard := AStroke;
end;

function TNyxLiveBindings.Signal(ANode: TNyxNode;
  ATrigger: TNyxTrigger): TNyxDispatch;
begin
  { A prepared input reset borrows FCommandRoot for prospective validation.
    Read-only physical phase signals must still observe the accepted FRoot:
    composition can start before the final fence and must be able to revoke
    that admission without becoming a native message exception. Signal cannot
    execute click/change actions. All writes keep Command's existing busy guard;
    an ordinary entered control command still refuses signal reentry. }

  if not NyxIsRuntimeTrigger(ATrigger) or (ATrigger in [ntClick, ntChange]) or
    (FSubscription = nil) or not FSubscription.Connected or
    ((FCommandRoot <> nil) and not (FResourcePreparation is TBindingInputPreparation)) or
    (ANode = nil) or (FRoot.Find(ANode.ID) <> ANode) then
  begin
    raise ENyxState.Create('Signals require an accepted mounted control and notification trigger');
  end;
  Result := DispatchNyxBehavior(ANode, ATrigger);
  CaptureEventValue(ANode, Result, Result.Info);
end;

function TNyxLiveBindings.ProposeText(ANode: TNyxNode;
  const AValue: TNyxText): TNyxDispatch;
begin

  if not NyxSupportsTextInput(ANode) then
  begin
    raise ENyxState.Create('Text proposals require a text-editing control');
  end;
  Result := Signal(ANode, ntBeforeTextInput);

  if not NyxInteractionPolicy(ANode).CanEditValue then
  begin
    raise ENyxState.Create('Text proposals require an editable enabled visible control');
  end;
  Result.Info.HasTextEdit := True;
  Result.Info.TextEdit.Before := ANode.Prop(NyxAttributeName(atValue));
  Result.Info.TextEdit.After := AValue;
end;

function TNyxLiveBindings.SignalSnapshot(ANode: TNyxNode;
  ATrigger: TNyxTrigger): TNyxEventInfo;
var
  LDispatch: TNyxDispatch;
begin
  LDispatch := Signal(ANode, ATrigger);
  Result := LDispatch.Info.Copy;
end;

{$I nyx.binding.inputs.implementation.inc}

end.
