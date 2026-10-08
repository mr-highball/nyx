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
  nyx.types,
  nyx.data,
  nyx.contract,
  nyx.sliders,
  nyx.state,
  nyx.resources,
  nyx.binding.types,
  nyx.model,
  nyx.behavior;

type
  TNyxBindingSync = procedure of object;

  { A live view borrows its realized root and runtime store. Neither may be
    freed before this coordinator. Activate owns one subscription token; destroy
    disconnects it before the renderer frees controls/model/state.

    Construction projects and admits the initial snapshot before target controls
    are built. Activate is separate so a failed factory can discard a candidate
    without subscribing an incomplete view. Commands stage a complete independent
    tree; state validators admit it before any accepted properties change.

    OnSync updates existing target controls. A notification exception means state
    is already committed, matching TNyxState's contract. Semantic callbacks belong
    to the adapter and run only after an accepted command returns. }
  TNyxLiveBindings = class
  private
    FRoot: TNyxNode;
    FState: TNyxState;
    FSubscription: TNyxStateSubscription;
    FOnSync: TNyxBindingSync;
    FCommandRoot: TNyxNode;
    FCommandProjected: Boolean;
    function GetRefreshReady: Boolean;
    function GetHasBindings: Boolean;
    procedure ValidateCandidate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
    procedure StateChanged(AState: TNyxState; AChanges: TNyxStateChanges);
    procedure Synchronize;
    procedure CaptureEventValue(AOrigin: TNyxNode; const ADispatch: TNyxDispatch;
      var AInfo: TNyxEventInfo);
    function Command(ANode: TNyxNode; ATrigger: TNyxTrigger; const AValue: TNyxText;
      AEdit: Boolean): TNyxDispatch;
  public
    constructor Create(ARoot: TNyxNode; AState: TNyxState);
    destructor Destroy; override;
    procedure Activate;
    { UI-thread resource/locale reload. All selectors and concrete properties
      validate on an independent candidate before existing controls synchronize.
      Invalid data retains accepted values/context. A sync exception follows
      publication. Input events never write a packed file. }
    procedure ReloadResources(const AResources: INyxResources;
      const ALocale, AFallback: TNyxLocaleRef);
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
    property HasBindings: Boolean read GetHasBindings;
    property OnSync: TNyxBindingSync read FOnSync write FOnSync;
  end;

{ Shared projection/admission for renderers, application validators and tests.
  Only runtime trees are mutated; caller owns both arguments. All bound keys must
  exist with their exact declared kinds. Concrete input types, ranges and layout
  constraints are checked before admission. }
procedure ApplyNyxBindings(ARoot: TNyxNode; AState: TNyxState);

{ Concrete control admission also supplies authoring choices. The node is
  borrowed, normally an independently realized selection/part. Empty means
  unsupported; returned sets/arrays own no mutable model or target references. }
function NyxBindingKinds(ANode: TNyxNode;
  ATarget: TNyxBindingProperty): TNyxStateKinds;
function NyxBindingTargets(ANode: TNyxNode): TNyxBindingTargetInfos;

implementation

uses
  nyx.schema,
  nyx.interaction,
  nyx.projection.refresh;

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

function ProjectionText(ANode: TNyxNode; const ASpec: TNyxBindingSpec;
  const AValue: TNyxStateValue): TNyxText;
begin
  Result := ScalarText(AValue);

  if ASpec.Target in [bpText, bpValue, bpPlaceholder, bpHint, bpAccessibleName] then
  begin
    { State/persistence preserve NUL, but native text widgets use terminated
      strings. Refuse a lossy portable control projection before publication. }

    if Pos(#0, Result) > 0 then
    begin
      raise ENyxState.Create('Portable control text cannot contain NUL: ' + ANode.ID);
    end;
  end;

  if (ASpec.Target <> bpValue) or (AValue.Kind <> nskText) then
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

  if not (ASpec.ValueKind in NyxBindingKinds(ANode, ASpec.Target)) then
  begin
    raise ENyxState.Create('Binding target/kind is unsupported on ' + ANode.ID);
  end;
end;

procedure CheckValueRange(ANode: TNyxNode; const ASpec: TNyxBindingSpec;
  const AValue: TNyxStateValue);
var
  LValue: Double;
  LScale: TNyxSliderScale;
  LMinimum: Integer;
  LMaximum: Integer;
  LDefaultMinimum: TNyxText;
  LDefaultMaximum: TNyxText;
begin

  if (ASpec.Target <> bpValue) or
    not (AValue.Kind in [nskInteger, nskNumber]) then
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
    LScale.PositionOf(ProjectionText(ANode, ASpec, AValue));
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

procedure ApplyNyxBindings(ARoot: TNyxNode; AState: TNyxState);

  function Value(ANode: TNyxNode; const ASpec: TNyxBindingSpec): TNyxStateValue;
  begin

    if ASpec.Source = bsResource then
    begin
      Exit(ANode.ReadResource(ASpec.ResourceValue));
    end;
    Result := AState.Value(ASpec.StateName);
  end;

  procedure Project(ANode: TNyxNode);
  var
    LIndex: Integer;
    LSpec: TNyxBindingSpec;
    LValue: TNyxStateValue;
    LText: TNyxText;
  begin
    for LIndex := 0 to ANode.BindingCount - 1 do
    begin
      LSpec := ANode.Bindings[LIndex];
      LSpec.Validate;

      if LSpec.Cleared then
      begin
        Continue;
      end;
      CheckTarget(ANode, LSpec);
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
    for LIndex := 0 to ANode.BindingCount - 1 do
    begin
      LSpec := ANode.Bindings[LIndex];

      if not LSpec.Cleared then
      begin
        CheckValueRange(ANode, LSpec, Value(ANode, LSpec));
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      CheckRanges(ANode.Children[LIndex]);
    end;
  end;

begin

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

constructor TNyxLiveBindings.Create(ARoot: TNyxNode; AState: TNyxState);
var
  LCandidate: TNyxNode;
begin
  inherited Create;
  FRoot := ARoot;
  FState := AState;

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
  FSubscription.Free;
  inherited Destroy;
end;

procedure TNyxLiveBindings.ReloadResources(const AResources: INyxResources;
  const ALocale, AFallback: TNyxLocaleRef);
var
  LCandidate: TNyxNode;
begin

  if not RefreshReady then
  begin
    raise ENyxState.Create('Resource reload refuses a busy view');
  end;
  LCandidate := FRoot.Clone;
  try
    LCandidate.BindResources(AResources, ALocale, AFallback);
    ApplyNyxBindings(LCandidate, FState);
    FRoot.CopyResourceContext(LCandidate);
    CopyProjection(FRoot, LCandidate);
  finally
    LCandidate.Free;
  end;
  Synchronize;
end;

procedure TNyxLiveBindings.Activate;
begin

  if FSubscription <> nil then
  begin
    raise ENyxState.Create('Live bindings are already activated');
  end;
  FSubscription := FState.Subscribe(StateChanged, ValidateCandidate);
end;

function TNyxLiveBindings.GetRefreshReady: Boolean;
begin
  Result := (FSubscription <> nil) and FSubscription.Connected and
    (FCommandRoot = nil) and not FState.Busy;
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
  LView := FRoot;

  if FCommandRoot <> nil then
  begin
    LView := FCommandRoot;
  end;
  LView := LView.Clone;
  try
    ApplyNyxBindings(LView, ACandidate);
  finally
    LView.Free;
  end;
end;

procedure TNyxLiveBindings.Synchronize;
begin

  if Assigned(FOnSync) then
  begin
    FOnSync;
  end;
end;

procedure TNyxLiveBindings.StateChanged(AState: TNyxState;
  AChanges: TNyxStateChanges);
begin

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
    (FCommandRoot <> nil) then
  begin
    raise ENyxState.Create('Control command requires active, idle bindings');
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
  FCommandRoot := FRoot.Clone;
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
    FState.Apply(LAssignments, LRevision);

    if not FCommandProjected then
    begin
      CopyProjection(FRoot, FCommandRoot);
      ApplyNyxBindings(FRoot, FState);
      Synchronize;
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

  if not NyxIsRuntimeTrigger(ATrigger) or (ATrigger in [ntClick, ntChange]) or
    (FSubscription = nil) or not FSubscription.Connected or
    (FCommandRoot <> nil) or (ANode = nil) or (FRoot.Find(ANode.ID) <> ANode) then
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

end.
