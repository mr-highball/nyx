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

unit nyx.callbacks;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.model,
  nyx.events,
  nyx.scheduler;

const
  NyxCallbacksKey = 'nyx.callbacks';
  NyxMaximumCallbacks = 128;

type
  { Pascal class names are distinct from registration identity. Only the handler
    reference is used in generated Pascal; the ID remains exact portable data. }
  TNyxHandlerRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  TNyxCallbackRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  TNyxCallbackInfo = record
    ID: TNyxCallbackRef;
    Handler: TNyxHandlerRef;
  end;
  TNyxAuthoredEventInfo = record
    Trigger: TNyxTrigger;
    { Exact semantic identity for ntNamed; empty for closed physical families. }
    Name: TNyxEventRef;
    Policy: TNyxExecutionPolicy;
    Callbacks: array of TNyxCallbackInfo;
  end;
  TNyxAuthoredEventInfos = array of TNyxAuthoredEventInfo;

  { Authored callbacks are portable descriptors, not live subscriptions. The
    facade retains its component/descriptor; streams retain the facade without
    a reverse cache. No widget, scheduler or callback instance enters a design.
    Every edit admits a complete independent metadata value before publication. }
  INyxAuthoredEvent = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001003000001}']
    function Policy(AValue: TNyxExecutionPolicy): INyxAuthoredEvent;
    function Add(const AHandler: TNyxHandlerRef;
      const AID: TNyxCallbackRef): INyxAuthoredEvent;
    function Remove(const AID: TNyxCallbackRef): INyxAuthoredEvent;
  end;
  INyxAuthoredEvents = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001003000002}']
    function On(ATrigger: TNyxTrigger): INyxAuthoredEvent;
    function OnNamed(const AName: TNyxEventRef): INyxAuthoredEvent;
    function OnClick: INyxAuthoredEvent;
    function OnChange: INyxAuthoredEvent;
    function OnAfterEnter: INyxAuthoredEvent;
    function OnAfterExit: INyxAuthoredEvent;
    function OnKeyDown: INyxAuthoredEvent;
   function OnKeyUp: INyxAuthoredEvent;
    { Authored physical families retain separate ordered registrations/policies.
      Key phases bracket Nyx dispatch; text phases bracket model admission.
      Pointer data and full text replacements are owned callback snapshots. }
    function OnBeforeKeyDown: INyxAuthoredEvent;
    function OnAfterKeyDown: INyxAuthoredEvent;
    function OnKeyPress: INyxAuthoredEvent;
    function OnBeforeKeyPress: INyxAuthoredEvent;
    function OnAfterKeyPress: INyxAuthoredEvent;
    function OnBeforeKeyUp: INyxAuthoredEvent;
    function OnAfterKeyUp: INyxAuthoredEvent;
    function OnBeforeTextInput: INyxAuthoredEvent;
    function OnTextInput: INyxAuthoredEvent;
    function OnAfterTextInput: INyxAuthoredEvent;
    function OnBeforeEdit: INyxAuthoredEvent;
    function OnCompositionStart: INyxAuthoredEvent;
    function OnCompositionUpdate: INyxAuthoredEvent;
    function OnCompositionEnd: INyxAuthoredEvent;
    function OnTextSelectionChange: INyxAuthoredEvent;
    function OnPointerCancel: INyxAuthoredEvent;
    function OnPointerCapture: INyxAuthoredEvent;
    function OnPointerCaptureLost: INyxAuthoredEvent;
    function OnDragStart: INyxAuthoredEvent;
    function OnDrag: INyxAuthoredEvent;
    function OnDragEnter: INyxAuthoredEvent;
    function OnDragOver: INyxAuthoredEvent;
    function OnDragExit: INyxAuthoredEvent;
    function OnDrop: INyxAuthoredEvent;
    function OnDragEnd: INyxAuthoredEvent;
    function OnDoubleClick: INyxAuthoredEvent;
    function OnPointerDown: INyxAuthoredEvent;
    function OnPointerUp: INyxAuthoredEvent;
    function OnPointerMove: INyxAuthoredEvent;
    function OnPointerEnter: INyxAuthoredEvent;
    function OnPointerExit: INyxAuthoredEvent;
    function OnContextMenu: INyxAuthoredEvent;
    { Stored wheel phases and viewport notifications retain independent policy
      and ordered handler identities through history and crafted generation. }
    function OnBeforeWheel: INyxAuthoredEvent;
    function OnWheel: INyxAuthoredEvent;
    function OnAfterWheel: INyxAuthoredEvent;
    function OnScroll: INyxAuthoredEvent;
    function OnScrollEnd: INyxAuthoredEvent;
    function OnSelectionChange: INyxAuthoredEvent;
    function Snapshot: TNyxDataValue;
    procedure Metadata(const AValue: TNyxDataValue);
    { Clear explicitly suppresses inherited registrations. Inherit removes this
      local descriptor, restoring the reusable definition at the next mount. }
    procedure Clear;
    procedure Inherit;
  end;

  { Alternative factories may inject application dependencies or return callbacks
    on other reference-counted bases. Resolve must return an owned interface.
    The default class registry constructs a fresh callback for each registration. }
  INyxCallbackFactory = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001003000003}']
    function Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
  end;
  TNyxCallbackClass = class of TNyxEventCallback;

function NyxHandler(const AClassName: TNyxText): TNyxHandlerRef;
function NyxCallbackID(const AName: TNyxText): TNyxCallbackRef;
function NyxCallbacks(const AControl: INyxNode): INyxAuthoredEvents; overload;
function NyxCallbacks(ANode: TNyxNode): INyxAuthoredEvents; overload;
{ Each call decodes an independent value snapshot. Invalid known metadata is
  rejected, including duplicate event/registration identities and closed choices. }
function NyxAuthoredEvents(ANode: TNyxNode): TNyxAuthoredEventInfos;
function EncodeNyxAuthoredEvents(const AEvents: TNyxAuthoredEventInfos): TNyxDataValue;
procedure ValidateNyxCallbacks(ANode: TNyxNode);
function NyxPolicyName(APolicy: TNyxExecutionPolicy): TNyxText;
function TryNyxPolicy(const AName: TNyxText; out APolicy: TNyxExecutionPolicy): Boolean;
{ Registry authoring is a startup/UI-thread operation. Duplicate names fail
  before replacement. Classes carry no retained instance or component cycle. }
procedure RegisterNyxCallback(const AHandler: TNyxHandlerRef; AClass: TNyxCallbackClass);
{ Bind once to a fresh application router before mounting. Realized page/part
  identities preserve reusable inheritance and instance overrides. All factories
  and policies are admitted before any registration is published. On failure
  newly installed subscriptions are cancelled; no authored document is changed. }
procedure BindNyxCallbacks(ADocument: TNyxDocument; const AEvents: INyxEvents;
  const AFactory: INyxCallbackFactory = nil);

implementation

uses
  nyx.composition;

type
  TNyxAuthoredEvents = class(TInterfacedObject, INyxAuthoredEvents)
  private
    FNode: TNyxNode;
    FControl: INyxNode;
  public
    constructor Create(ANode: TNyxNode; const AControl: INyxNode);
    destructor Destroy; override;
    function On(ATrigger: TNyxTrigger): INyxAuthoredEvent;
    function OnNamed(const AName: TNyxEventRef): INyxAuthoredEvent;
    function OnClick: INyxAuthoredEvent;
    function OnChange: INyxAuthoredEvent;
    function OnAfterEnter: INyxAuthoredEvent;
    function OnAfterExit: INyxAuthoredEvent;
    function OnKeyDown: INyxAuthoredEvent;
   function OnKeyUp: INyxAuthoredEvent;
    function OnBeforeKeyDown: INyxAuthoredEvent;
    function OnAfterKeyDown: INyxAuthoredEvent;
    function OnKeyPress: INyxAuthoredEvent;
    function OnBeforeKeyPress: INyxAuthoredEvent;
    function OnAfterKeyPress: INyxAuthoredEvent;
    function OnBeforeKeyUp: INyxAuthoredEvent;
    function OnAfterKeyUp: INyxAuthoredEvent;
    function OnBeforeTextInput: INyxAuthoredEvent;
    function OnTextInput: INyxAuthoredEvent;
    function OnAfterTextInput: INyxAuthoredEvent;
    function OnBeforeEdit: INyxAuthoredEvent;
    function OnCompositionStart: INyxAuthoredEvent;
    function OnCompositionUpdate: INyxAuthoredEvent;
    function OnCompositionEnd: INyxAuthoredEvent;
    function OnTextSelectionChange: INyxAuthoredEvent;
    function OnPointerCancel: INyxAuthoredEvent;
    function OnPointerCapture: INyxAuthoredEvent;
    function OnPointerCaptureLost: INyxAuthoredEvent;
    function OnDragStart: INyxAuthoredEvent;
    function OnDrag: INyxAuthoredEvent;
    function OnDragEnter: INyxAuthoredEvent;
    function OnDragOver: INyxAuthoredEvent;
    function OnDragExit: INyxAuthoredEvent;
    function OnDrop: INyxAuthoredEvent;
    function OnDragEnd: INyxAuthoredEvent;
    function OnDoubleClick: INyxAuthoredEvent;
    function OnPointerDown: INyxAuthoredEvent;
    function OnPointerUp: INyxAuthoredEvent;
    function OnPointerMove: INyxAuthoredEvent;
    function OnPointerEnter: INyxAuthoredEvent;
    function OnPointerExit: INyxAuthoredEvent;
    function OnContextMenu: INyxAuthoredEvent;
    function OnBeforeWheel: INyxAuthoredEvent;
    function OnWheel: INyxAuthoredEvent;
    function OnAfterWheel: INyxAuthoredEvent;
    function OnScroll: INyxAuthoredEvent;
    function OnScrollEnd: INyxAuthoredEvent;
    function OnSelectionChange: INyxAuthoredEvent;
    function Snapshot: TNyxDataValue;
    procedure Metadata(const AValue: TNyxDataValue);
    procedure Clear;
    procedure Inherit;
  end;
  TNyxAuthoredEvent = class(TInterfacedObject, INyxAuthoredEvent)
  private
    FOwner: INyxAuthoredEvents;
    FTrigger: TNyxTrigger;
    FName: TNyxEventRef;
    function ReadEvents(out AIndex: Integer): TNyxAuthoredEventInfos;
  public
    constructor Create(const AOwner: INyxAuthoredEvents; ATrigger: TNyxTrigger;
      const AName: TNyxEventRef);
    function Policy(AValue: TNyxExecutionPolicy): INyxAuthoredEvent;
    function Add(const AHandler: TNyxHandlerRef;
      const AID: TNyxCallbackRef): INyxAuthoredEvent;
    function Remove(const AID: TNyxCallbackRef): INyxAuthoredEvent;
  end;
  TNyxCallbackType = record
    Name: TNyxText;
    CallbackClass: TNyxCallbackClass;
  end;
  { COM interfaces stay in owned classes rather than records for pas2js. }
  TNyxCallbackPlan = class
    Target: TNyxEventTarget;
    Trigger: TNyxTrigger;
    Name: TNyxEventRef;
    Policy: TNyxExecutionPolicy;
    Callback: INyxEventCallback;
    Subscription: INyxEventSubscription;
  end;

var
  GCallbackTypes: array of TNyxCallbackType;

const
  CPolicyNames: array[TNyxExecutionPolicy] of TNyxText =
    ('sequential', 'asynchronous', 'ui-queue', 'threaded');

function NyxPolicyName(APolicy: TNyxExecutionPolicy): TNyxText;
begin
  Result := CPolicyNames[APolicy];
end;

function TryNyxPolicy(const AName: TNyxText; out APolicy: TNyxExecutionPolicy): Boolean;
var
  LPolicy: TNyxExecutionPolicy;
begin
  APolicy := neSequential;
  for LPolicy := Low(TNyxExecutionPolicy) to High(TNyxExecutionPolicy) do
  begin

    if AName = CPolicyNames[LPolicy] then
    begin
      APolicy := LPolicy;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxHandler(const AClassName: TNyxText): TNyxHandlerRef;
var
  LIndex: Integer;
begin

  if (Length(AClassName) < 2) or (Length(AClassName) > 120) or
    not (AClassName[1] in ['A'..'Z', 'a'..'z', '_']) then
  begin
    raise ENyxModel.Create('A callback handler requires a Pascal class identifier');
  end;
  for LIndex := 2 to Length(AClassName) do
  begin

    if not (AClassName[LIndex] in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
    begin
      raise ENyxModel.Create('A callback handler requires a Pascal class identifier');
    end;
  end;
  Result.FName := AClassName;
end;

function NyxCallbackID(const AName: TNyxText): TNyxCallbackRef;
var
  LNode: TNyxNode;
begin
  { Reuse authored identity admission for exact Unicode/scalar budgets. This
    temporary descriptor never enters a document or the default component set. }
  LNode := TNyxNode.Create(nkLabel, AName);
  try

    if AName = '' then
    begin
      raise ENyxModel.Create('A callback requires an explicit registration identity');
    end;
    Result.FName := AName;
  finally
    LNode.Free;
  end;
end;

procedure RequireRuntimeTrigger(ATrigger: TNyxTrigger);
begin

  if not NyxIsRuntimeTrigger(ATrigger) then
  begin
    raise ENyxModel.Create('Authored callbacks require a runtime event');
  end;
end;

function DecodeEvents(const AValue: TNyxDataValue): TNyxAuthoredEventInfos;
var
  LEvents: TNyxDataValue;
  LEvent: TNyxDataValue;
  LCallbacks: TNyxDataValue;
  LCallback: TNyxDataValue;
  LIndex: Integer;
  LCallbackIndex: Integer;
  LTrigger: TNyxTrigger;
  LFound: Boolean;
  LSeenTriggers: set of TNyxTrigger;
  LIDs: TNyxStrings;
  LNames: TNyxStrings;
begin
  Result := nil;

  if (AValue.Kind <> ndObject) or (AValue.Count <> 2) or
    (AValue.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Unsupported callback descriptor');
  end;
  LEvents := AValue.Field('events');

  { Open semantic streams have an independent bounded count; their identity is
    the exact name, whereas physical streams retain the closed trigger key. }

  if (LEvents.Kind <> ndArray) or (LEvents.Count > NyxMaximumCallbacks) then
  begin
    raise ENyxModel.Create('Callback metadata requires distinct runtime events');
  end;
  LSeenTriggers := [];
  LIDs := TNyxStrings.Create;
  LNames := TNyxStrings.Create;
  try
    SetLength(Result, LEvents.Count);
    for LIndex := 0 to LEvents.Count - 1 do
    begin
      LEvent := LEvents.Item(LIndex);

      if (LEvent.Kind <> ndObject) or not (LEvent.Count in [3, 4]) then
      begin
        raise ENyxModel.Create('An event contains trigger, policy and callbacks');
      end;
      LFound := False;
      for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
      begin

        if NyxTriggerName(LTrigger) = LEvent.Field('trigger').AsText then
        begin

          if LTrigger = ntNamed then
          begin

            if (LEvent.Count <> 4) or (Trim(LEvent.Field('name').AsText) = '') then
            begin
              raise ENyxModel.Create('A named callback requires its exact event name');
            end;
            Result[LIndex].Name := NyxNamedEvent(LEvent.Field('name').AsText);

            if LNames.IndexOf(Result[LIndex].Name.Name) >= 0 then
            begin
              raise ENyxModel.Create('Duplicate named callback event');
            end;
            LNames.Add(Result[LIndex].Name.Name);
          end
          else
          begin
            RequireRuntimeTrigger(LTrigger);

            if (LEvent.Count <> 3) or (LTrigger in LSeenTriggers) then
            begin
              raise ENyxModel.Create('Duplicate or malformed physical callback event');
            end;
            Include(LSeenTriggers, LTrigger);
          end;
          Result[LIndex].Trigger := LTrigger;
          LFound := True;
          Break;
        end;
      end;

      if not LFound or
        not TryNyxPolicy(LEvent.Field('policy').AsText, Result[LIndex].Policy) then
      begin
        raise ENyxModel.Create('Unknown callback event or execution policy');
      end;
      LCallbacks := LEvent.Field('callbacks');

      if (LCallbacks.Kind <> ndArray) or
        (LIDs.Count + LCallbacks.Count > NyxMaximumCallbacks) then
      begin
        raise ENyxModel.Create('Callback registration budget exceeded');
      end;
      SetLength(Result[LIndex].Callbacks, LCallbacks.Count);
      for LCallbackIndex := 0 to LCallbacks.Count - 1 do
      begin
        LCallback := LCallbacks.Item(LCallbackIndex);

        if (LCallback.Kind <> ndObject) or (LCallback.Count <> 2) then
        begin
          raise ENyxModel.Create('A callback contains registration identity and handler');
        end;
        Result[LIndex].Callbacks[LCallbackIndex].ID :=
          NyxCallbackID(LCallback.Field('id').AsText);
        Result[LIndex].Callbacks[LCallbackIndex].Handler :=
          NyxHandler(LCallback.Field('handler').AsText);

        if LIDs.IndexOf(Result[LIndex].Callbacks[LCallbackIndex].ID.Name) >= 0 then
        begin
          raise ENyxModel.Create('Duplicate callback registration identity');
        end;
        LIDs.Add(Result[LIndex].Callbacks[LCallbackIndex].ID.Name);
      end;
    end;
  finally
    LNames.Free;
    LIDs.Free;
  end;
end;

function EncodeNyxAuthoredEvents(const AEvents: TNyxAuthoredEventInfos): TNyxDataValue;
var
  LEvents: array of TNyxDataValue;
  LCallbacks: array of TNyxDataValue;
  LIndex: Integer;
  LCallbackIndex: Integer;
  LAdmitted: TNyxAuthoredEventInfos;
begin
  SetLength(LEvents, Length(AEvents));
  for LIndex := 0 to High(AEvents) do
  begin

    if (AEvents[LIndex].Trigger <> ntNamed) and (AEvents[LIndex].Name.Name <> '') then
    begin
      raise ENyxModel.Create('A physical callback cannot carry a named stream key');
    end;
    SetLength(LCallbacks, Length(AEvents[LIndex].Callbacks));
    for LCallbackIndex := 0 to High(LCallbacks) do
    begin
      LCallbacks[LCallbackIndex] := NyxObject([
        NyxField('id', NyxData(AEvents[LIndex].Callbacks[LCallbackIndex].ID.Name)),
        NyxField('handler', NyxData(AEvents[LIndex].Callbacks[LCallbackIndex].Handler.Name))
      ]);
    end;

    if AEvents[LIndex].Trigger = ntNamed then
    begin
      LEvents[LIndex] := NyxObject([
        NyxField('trigger', NyxData(NyxTriggerName(ntNamed))),
        NyxField('name', NyxData(AEvents[LIndex].Name.Name)),
        NyxField('policy', NyxData(NyxPolicyName(AEvents[LIndex].Policy))),
        NyxField('callbacks', NyxArray(LCallbacks))
      ]);
    end
    else
    begin
      LEvents[LIndex] := NyxObject([
        NyxField('trigger', NyxData(NyxTriggerName(AEvents[LIndex].Trigger))),
        NyxField('policy', NyxData(NyxPolicyName(AEvents[LIndex].Policy))),
        NyxField('callbacks', NyxArray(LCallbacks))
      ]);
    end;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('events', NyxArray(LEvents))]);
  LAdmitted := DecodeEvents(Result);
end;

function NyxAuthoredEvents(ANode: TNyxNode): TNyxAuthoredEventInfos;
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Callback metadata requires a component');
  end;
  Result := nil;

  if ANode.Extensions.Has(NyxExtension(NyxCallbacksKey)) then
  begin
    Result := DecodeEvents(ANode.Extensions.Value(NyxExtension(NyxCallbacksKey)));
  end;
end;

procedure ValidateNyxCallbacks(ANode: TNyxNode);
var
  LEvents: TNyxAuthoredEventInfos;
begin
  LEvents := NyxAuthoredEvents(ANode);
end;

constructor TNyxAuthoredEvents.Create(ANode: TNyxNode; const AControl: INyxNode);
begin
  inherited Create;

  if ANode = nil then
  begin
    raise ENyxModel.Create('Authored events require a retained component');
  end;
  FNode := ANode;
  FNode.AcquireReference;
  FControl := AControl;
end;

destructor TNyxAuthoredEvents.Destroy;
begin
  FNode.ReleaseReference;
  inherited Destroy;
end;

function NyxCallbacks(const AControl: INyxNode): INyxAuthoredEvents;
begin

  if AControl = nil then
  begin
    raise ENyxModel.Create('Authored events require a component interface');
  end;
  Result := TNyxAuthoredEvents.Create(AControl.Node, AControl);
end;

function NyxCallbacks(ANode: TNyxNode): INyxAuthoredEvents;
begin
  Result := TNyxAuthoredEvents.Create(ANode, nil);
end;

function TNyxAuthoredEvents.Snapshot: TNyxDataValue;
begin

  if FNode.Extensions.Has(NyxExtension(NyxCallbacksKey)) then
  begin
    Exit(FNode.Extensions.Value(NyxExtension(NyxCallbacksKey)));
  end;
  Result := EncodeNyxAuthoredEvents(nil);
end;

procedure TNyxAuthoredEvents.Metadata(const AValue: TNyxDataValue);
var
  LEvents: TNyxAuthoredEventInfos;
begin
  LEvents := DecodeEvents(AValue);
  FNode.Extensions.SetValue(NyxExtension(NyxCallbacksKey), AValue);
end;

procedure TNyxAuthoredEvents.Clear;
begin
  Metadata(EncodeNyxAuthoredEvents(nil));
end;

procedure TNyxAuthoredEvents.Inherit;
begin
  FNode.Extensions.Remove(NyxExtension(NyxCallbacksKey));
end;

function TNyxAuthoredEvents.On(ATrigger: TNyxTrigger): INyxAuthoredEvent;
begin
  RequireRuntimeTrigger(ATrigger);
  Result := TNyxAuthoredEvent.Create(Self as INyxAuthoredEvents, ATrigger, Default(TNyxEventRef));
end;

function TNyxAuthoredEvents.OnNamed(const AName: TNyxEventRef): INyxAuthoredEvent;
begin

  if Trim(AName.Name) = '' then
  begin
    raise ENyxModel.Create('A named callback requires its exact event name');
  end;
  Result := TNyxAuthoredEvent.Create(Self as INyxAuthoredEvents, ntNamed,
    NyxNamedEvent(AName.Name));
end;

function TNyxAuthoredEvents.OnClick: INyxAuthoredEvent;
begin
  Result := On(ntClick);
end;

function TNyxAuthoredEvents.OnChange: INyxAuthoredEvent;
begin
  Result := On(ntChange);
end;

function TNyxAuthoredEvents.OnAfterEnter: INyxAuthoredEvent;
begin
  Result := On(ntAfterEnter);
end;

function TNyxAuthoredEvents.OnAfterExit: INyxAuthoredEvent;
begin
  Result := On(ntAfterExit);
end;

function TNyxAuthoredEvents.OnKeyDown: INyxAuthoredEvent;
begin
  Result := On(ntKeyDown);
end;

function TNyxAuthoredEvents.OnBeforeKeyDown: INyxAuthoredEvent;
begin
  Result := On(ntBeforeKeyDown);
end;

function TNyxAuthoredEvents.OnAfterKeyDown: INyxAuthoredEvent;
begin
  Result := On(ntAfterKeyDown);
end;

function TNyxAuthoredEvents.OnKeyPress: INyxAuthoredEvent;
begin
  Result := On(ntKeyPress);
end;

function TNyxAuthoredEvents.OnBeforeKeyPress: INyxAuthoredEvent;
begin
  Result := On(ntBeforeKeyPress);
end;

function TNyxAuthoredEvents.OnAfterKeyPress: INyxAuthoredEvent;
begin
  Result := On(ntAfterKeyPress);
end;

function TNyxAuthoredEvents.OnBeforeKeyUp: INyxAuthoredEvent;
begin
  Result := On(ntBeforeKeyUp);
end;

function TNyxAuthoredEvents.OnAfterKeyUp: INyxAuthoredEvent;
begin
  Result := On(ntAfterKeyUp);
end;

function TNyxAuthoredEvents.OnBeforeTextInput: INyxAuthoredEvent;
begin
  Result := On(ntBeforeTextInput);
end;

function TNyxAuthoredEvents.OnTextInput: INyxAuthoredEvent;
begin
  Result := On(ntTextInput);
end;

function TNyxAuthoredEvents.OnBeforeEdit: INyxAuthoredEvent;
begin
  Result := On(ntBeforeEdit);
end;

function TNyxAuthoredEvents.OnCompositionStart: INyxAuthoredEvent;
begin
  Result := On(ntCompositionStart);
end;

function TNyxAuthoredEvents.OnCompositionUpdate: INyxAuthoredEvent;
begin
  Result := On(ntCompositionUpdate);
end;

function TNyxAuthoredEvents.OnCompositionEnd: INyxAuthoredEvent;
begin
  Result := On(ntCompositionEnd);
end;

function TNyxAuthoredEvents.OnTextSelectionChange: INyxAuthoredEvent;
begin
  Result := On(ntTextSelectionChange);
end;

function TNyxAuthoredEvents.OnAfterTextInput: INyxAuthoredEvent;
begin
  Result := On(ntAfterTextInput);
end;

function TNyxAuthoredEvents.OnDoubleClick: INyxAuthoredEvent;
begin
  Result := On(ntDoubleClick);
end;

function TNyxAuthoredEvents.OnPointerDown: INyxAuthoredEvent;
begin
  Result := On(ntPointerDown);
end;

function TNyxAuthoredEvents.OnPointerUp: INyxAuthoredEvent;
begin
  Result := On(ntPointerUp);
end;

function TNyxAuthoredEvents.OnPointerMove: INyxAuthoredEvent;
begin
  Result := On(ntPointerMove);
end;

function TNyxAuthoredEvents.OnPointerEnter: INyxAuthoredEvent;
begin
  Result := On(ntPointerEnter);
end;

function TNyxAuthoredEvents.OnPointerExit: INyxAuthoredEvent;
begin
  Result := On(ntPointerExit);
end;

function TNyxAuthoredEvents.OnContextMenu: INyxAuthoredEvent;
begin
  Result := On(ntContextMenu);
end;

function TNyxAuthoredEvents.OnPointerCancel: INyxAuthoredEvent;
begin
  Result := On(ntPointerCancel);
end;

function TNyxAuthoredEvents.OnPointerCapture: INyxAuthoredEvent;
begin
  Result := On(ntPointerCapture);
end;

function TNyxAuthoredEvents.OnPointerCaptureLost: INyxAuthoredEvent;
begin
  Result := On(ntPointerCaptureLost);
end;

function TNyxAuthoredEvents.OnDragStart: INyxAuthoredEvent;
begin
  Result := On(ntDragStart);
end;

function TNyxAuthoredEvents.OnDrag: INyxAuthoredEvent;
begin
  Result := On(ntDrag);
end;

function TNyxAuthoredEvents.OnDragEnter: INyxAuthoredEvent;
begin
  Result := On(ntDragEnter);
end;

function TNyxAuthoredEvents.OnDragOver: INyxAuthoredEvent;
begin
  Result := On(ntDragOver);
end;

function TNyxAuthoredEvents.OnDragExit: INyxAuthoredEvent;
begin
  Result := On(ntDragExit);
end;

function TNyxAuthoredEvents.OnDrop: INyxAuthoredEvent;
begin
  Result := On(ntDrop);
end;

function TNyxAuthoredEvents.OnDragEnd: INyxAuthoredEvent;
begin
  Result := On(ntDragEnd);
end;

function TNyxAuthoredEvents.OnBeforeWheel: INyxAuthoredEvent;
begin
  Result := On(ntBeforeWheel);
end;

function TNyxAuthoredEvents.OnWheel: INyxAuthoredEvent;
begin
  Result := On(ntWheel);
end;

function TNyxAuthoredEvents.OnAfterWheel: INyxAuthoredEvent;
begin
  Result := On(ntAfterWheel);
end;

function TNyxAuthoredEvents.OnScroll: INyxAuthoredEvent;
begin
  Result := On(ntScroll);
end;

function TNyxAuthoredEvents.OnScrollEnd: INyxAuthoredEvent;
begin
  Result := On(ntScrollEnd);
end;

function TNyxAuthoredEvents.OnSelectionChange: INyxAuthoredEvent;
begin
  Result := On(ntSelectionChange);
end;

function TNyxAuthoredEvents.OnKeyUp: INyxAuthoredEvent;
begin
  Result := On(ntKeyUp);
end;

constructor TNyxAuthoredEvent.Create(const AOwner: INyxAuthoredEvents;
  ATrigger: TNyxTrigger; const AName: TNyxEventRef);
begin
  inherited Create;
  FOwner := AOwner;
  FTrigger := ATrigger;
  FName := AName;
end;

function TNyxAuthoredEvent.ReadEvents(out AIndex: Integer): TNyxAuthoredEventInfos;
var
  LIndex: Integer;
begin
  Result := DecodeEvents(FOwner.Snapshot);
  for LIndex := 0 to High(Result) do
  begin

    if (Result[LIndex].Trigger = FTrigger) and (Result[LIndex].Name.Name = FName.Name) then
    begin
      AIndex := LIndex;
      Exit;
    end;
  end;
  AIndex := Length(Result);
  SetLength(Result, AIndex + 1);
  Result[AIndex].Trigger := FTrigger;
  Result[AIndex].Name := FName;
  Result[AIndex].Policy := neSequential;
  Result[AIndex].Callbacks := nil;
end;

function TNyxAuthoredEvent.Policy(AValue: TNyxExecutionPolicy): INyxAuthoredEvent;
var
  LEvents: TNyxAuthoredEventInfos;
  LIndex: Integer;
begin
  LEvents := ReadEvents(LIndex);
  LEvents[LIndex].Policy := AValue;
  FOwner.Metadata(EncodeNyxAuthoredEvents(LEvents));
  Result := Self as INyxAuthoredEvent;
end;

function TNyxAuthoredEvent.Add(const AHandler: TNyxHandlerRef;
  const AID: TNyxCallbackRef): INyxAuthoredEvent;
var
  LEvents: TNyxAuthoredEventInfos;
  LIndex: Integer;
  LCount: Integer;
begin
  LEvents := ReadEvents(LIndex);
  LCount := Length(LEvents[LIndex].Callbacks);
  SetLength(LEvents[LIndex].Callbacks, LCount + 1);
  LEvents[LIndex].Callbacks[LCount].ID := AID;
  LEvents[LIndex].Callbacks[LCount].Handler := AHandler;
  FOwner.Metadata(EncodeNyxAuthoredEvents(LEvents));
  Result := Self as INyxAuthoredEvent;
end;

function TNyxAuthoredEvent.Remove(const AID: TNyxCallbackRef): INyxAuthoredEvent;
var
  LEvents: TNyxAuthoredEventInfos;
  LIndex: Integer;
  LCallbackIndex: Integer;
  LNext: Integer;
begin
  LEvents := ReadEvents(LIndex);
  for LCallbackIndex := 0 to High(LEvents[LIndex].Callbacks) do
  begin

    if LEvents[LIndex].Callbacks[LCallbackIndex].ID.Name = AID.Name then
    begin
      for LNext := LCallbackIndex to High(LEvents[LIndex].Callbacks) - 1 do
      begin
        LEvents[LIndex].Callbacks[LNext] := LEvents[LIndex].Callbacks[LNext + 1];
      end;
      SetLength(LEvents[LIndex].Callbacks, Length(LEvents[LIndex].Callbacks) - 1);
      FOwner.Metadata(EncodeNyxAuthoredEvents(LEvents));
      Exit(Self as INyxAuthoredEvent);
    end;
  end;
  raise ENyxModel.Create('Callback registration is missing');
end;

procedure RegisterNyxCallback(const AHandler: TNyxHandlerRef; AClass: TNyxCallbackClass);
var
  LHandler: TNyxHandlerRef;
  LIndex: Integer;
begin
  LHandler := NyxHandler(AHandler.Name);

  if AClass = nil then
  begin
    raise ENyxModel.Create('A callback registration requires its implementation class');
  end;
  for LIndex := 0 to High(GCallbackTypes) do
  begin

    if SameText(GCallbackTypes[LIndex].Name, LHandler.Name) then
    begin
      raise ENyxModel.Create('Duplicate callback implementation: ' + LHandler.Name);
    end;
  end;
  LIndex := Length(GCallbackTypes);
  SetLength(GCallbackTypes, LIndex + 1);
  GCallbackTypes[LIndex].Name := LHandler.Name;
  GCallbackTypes[LIndex].CallbackClass := AClass;
end;

function ResolveCallback(const AHandler: TNyxHandlerRef): INyxEventCallback;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(GCallbackTypes) do
  begin

    if SameText(GCallbackTypes[LIndex].Name, AHandler.Name) then
    begin
      Exit(GCallbackTypes[LIndex].CallbackClass.Create);
    end;
  end;
  raise ENyxModel.Create('Callback implementation is missing: ' + AHandler.Name);
end;

procedure BindNyxCallbacks(ADocument: TNyxDocument; const AEvents: INyxEvents;
  const AFactory: INyxCallbackFactory);
var
  LPlans: array of TNyxCallbackPlan;
  LRoot: TNyxNode;
  LIndex: Integer;
  LInstalled: Boolean;

  procedure Collect(ANode: TNyxNode);
  var
    LInfos: TNyxAuthoredEventInfos;
    LInfoIndex: Integer;
    LCallbackIndex: Integer;
    LChildIndex: Integer;
    LPlan: TNyxCallbackPlan;
  begin
    LInfos := NyxAuthoredEvents(ANode);
    for LInfoIndex := 0 to High(LInfos) do
    begin
      AEvents.Scheduler.Admit(LInfos[LInfoIndex].Policy);
      for LCallbackIndex := 0 to High(LInfos[LInfoIndex].Callbacks) do
      begin
        LPlan := TNyxCallbackPlan.Create;
        SetLength(LPlans, Length(LPlans) + 1);
        LPlans[High(LPlans)] := LPlan;
        LPlan.Target := NyxControlEvents(ANode.ID, niRuntime);

        if ANode.Prop('compound') = 'true' then
        begin
          LPlan.Target := NyxCompoundEvents(ANode.ID, niRuntime);
        end;
        LPlan.Trigger := LInfos[LInfoIndex].Trigger;
        LPlan.Name := LInfos[LInfoIndex].Name;
        LPlan.Policy := LInfos[LInfoIndex].Policy;

        if AFactory = nil then
        begin
          LPlan.Callback := ResolveCallback(LInfos[LInfoIndex].Callbacks[LCallbackIndex].Handler);
        end
        else
        begin
          LPlan.Callback := AFactory.Resolve(LInfos[LInfoIndex].Callbacks[LCallbackIndex].Handler);
        end;

        if LPlan.Callback = nil then
        begin
          raise ENyxModel.Create('Callback factory returned an empty implementation');
        end;
      end;
    end;
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      Collect(ANode.Children[LChildIndex]);
    end;
  end;

begin

  if (ADocument = nil) or (AEvents = nil) then
  begin
    raise ENyxModel.Create('Callback binding requires a document and runtime router');
  end;
  ADocument.Validate;
  LInstalled := False;
  try
    for LIndex := 0 to ADocument.Count - 1 do
    begin
      LRoot := RealizeNyxView(ADocument, ADocument.Pages[LIndex]);
      try
        Collect(LRoot);
      finally
        ReleaseNyxNode(LRoot);
      end;
    end;

    if Length(LPlans) = 0 then
    begin
      Exit;
    end;
    for LIndex := Ord(Low(TNyxTrigger)) to Ord(High(TNyxTrigger)) do
    begin

      if AEvents.HasSubscribers(TNyxTrigger(LIndex)) then
      begin
        raise ENyxModel.Create('Bind authored callbacks once to a fresh application router');
      end;
    end;
    for LIndex := 0 to High(LPlans) do
    begin

      if LPlans[LIndex].Trigger = ntNamed then
      begin
        LPlans[LIndex].Subscription := AEvents.OnNamed(LPlans[LIndex].Target,
          LPlans[LIndex].Name).Policy(LPlans[LIndex].Policy).Subscribe(LPlans[LIndex].Callback);
      end
      else
      begin
        LPlans[LIndex].Subscription := AEvents.On(LPlans[LIndex].Target,
          LPlans[LIndex].Trigger).Policy(LPlans[LIndex].Policy).Subscribe(LPlans[LIndex].Callback);
      end;
    end;
    LInstalled := True;
  finally
    for LIndex := 0 to High(LPlans) do
    begin

      if not LInstalled and (LPlans[LIndex].Subscription <> nil) then
      begin
        LPlans[LIndex].Subscription.Cancel;
      end;
      LPlans[LIndex].Free;
    end;
  end;
end;

end.
