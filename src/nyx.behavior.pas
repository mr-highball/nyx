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

unit nyx.behavior;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.viewport,
  nyx.contract,
  nyx.state,
  SysUtils,
  nyx.model,
  nyx.collections.selection,
  nyx.editing,
  nyx.gestures;

type
  { Target-independent accepted event. Application names stay distinct references;
    triggers are closed enums. SourceID/OriginID/TargetID and Value are owned snapshots
    that may be retained after the callback/view ends. ANode in the handler is
    borrowed for the callback only. HasValue distinguishes absent from empty.
    ValueKind retains the scalar contract, including integer versus number. }
  TNyxEventInfo = record
    Trigger: TNyxTrigger;
    Name: TNyxEventRef;
    SourceID: TNyxText;
    OriginID: TNyxText;
    TargetID: TNyxText;
    { Payload may deliberately read a different field from the action target. }
    ValueID: TNyxText;
    Value: TNyxDataValue;
    ValueKind: TNyxStateKind;
    HasValue: Boolean;
    { Named extension producers may carry structured data. Scalar producers
      retain Value/ValueKind; structured producers set HasDetails instead.
      Details is an immutable owned snapshot, never a platform event handle. }
    HasDetails: Boolean;
    Details: TNyxDataValue;
    Changed: Boolean;
    { Keyboard phases carry shortcut data, not characters inserted into an editor.
      Other event families explicitly have HasKeyboard=False. Copy owns this scalar
      snapshot, even when the originating control has been disposed. }
    HasKeyboard: Boolean;
    Keyboard: TNyxKeyStroke;
    { Additional families own their data and never retain a DOM event or LCL
      control. BeforeTextInput describes a proposal; TextInput is accepted.
      After hooks report whether a synchronous before/main hook consumed it. }
    HasTextEdit: Boolean;
    TextEdit: TNyxTextEdit;
    { Physical edit/session data is separate from accepted model admission.
      Composition and selection signals may carry it without a text command. }
    HasEditing: Boolean;
    Editing: TNyxEditingSnapshot;
    HasPointer: Boolean;
    Pointer: TNyxPointerSnapshot;
    { Protected hover formats and readable drop data are immutable snapshots;
      an event never exposes a native drag object or browser transfer handle. }
    HasDrag: Boolean;
    Drag: TNyxDragSnapshot;
    { Wheel is a request with explicit units and platform cancellation support.
      Viewport observes actual scrolling, including keyboard, touch and code.
      Neither snapshot retains the platform event/control. Scroll notifications
      cannot cancel movement that has already happened. }
    HasWheel: Boolean;
    Wheel: TNyxWheelSnapshot;
    HasViewport: Boolean;
    Viewport: TNyxViewportSnapshot;
    { Runtime item identities, independent of scalar values or row text.
      Both snapshots are immutable managed leases; no dataset is retained. }
    HasCollectionSelection: Boolean;
    SelectionBefore: TNyxCollectionSelectionSnapshot;
    Selection: TNyxCollectionSelectionSnapshot;
    DefaultPrevented: Boolean;
    function IsNamed(const AName: TNyxEventRef): Boolean;
    function Copy: TNyxEventInfo;
  end;

  TNyxEventHandler = procedure(ANode: TNyxNode; const AEvent: TNyxEventInfo) of object;

  { One event contract for native controls, browser controls and compound parts.
    Source is the nearest compound, or the originating leaf for a primitive.
    Changed means adapters should synchronize existing controls from model state.
    Handlers receive semantic names such as search, submit, rate or dismiss. }
  TNyxDispatch = record
    Source: TNyxNode;
    Target: TNyxNode;
    Info: TNyxEventInfo;
    Changed: Boolean;
    { Compatibility inspection of an open name; dispatch accepts typed triggers. }
    function GetEventName: TNyxText;
    property EventName: TNyxText read GetEventName;
  end;

{ Initialize recipe selection variants from the root's declared value.
  This mutates only a realized view, never a shared component definition. }
procedure PrepareNyxBehavior(ARoot: TNyxNode);

{ Interpret an action in a detached realized candidate. Info describes routing;
  its value is captured by TNyxLiveBindings before the candidate is published.
  Use that coordinator for runtime commands and accepted callback payloads.
  Applications may implement their own domain actions in the emitted callback.
  Targets are named part paths, so recipes can move without rewriting event code. }
function DispatchNyxBehavior(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxDispatch;
{ Declared custom producer admission. Both adapters use this pure snapshot
  boundary: support and payload are checked before callbacks; disabled/hidden
  scopes suppress delivery, read-only still permits notifications. No action,
  state write or control mutation occurs. Structured details remain independent
  of the existing scalar Value family. }
function DispatchNyxNamedEvent(ANode: TNyxNode; const AName: TNyxEventRef;
  const APayload: TNyxDataValue; AHasPayload: Boolean;
  APlatform: TNyxPlatform): TNyxDispatch;

{ Designer callback data carries the exact draft as text; model admission remains
  the undoable command's responsibility. No application action runs here. }
function NyxDesignEvent(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxEventInfo;
{ The older whole-view method bridge receives default command/link clicks and
  explicitly named clicks. General managed callbacks are independently opt-in;
  extending them to labels/fields must not manufacture extra legacy events when
  LCL invokes a checkbox's click slot from a value setter. }
function NyxHasLegacyClick(ANode: TNyxNode): Boolean;

implementation

uses
  nyx.schema,
  nyx.event.payload,
  nyx.interaction;

function DispatchNyxNamedEvent(ANode: TNyxNode; const AName: TNyxEventRef;
  const APayload: TNyxDataValue; AHasPayload: Boolean;
  APlatform: TNyxPlatform): TNyxDispatch;
var
  LSchema: TNyxEventSchema;
  LDeclared: Boolean;
begin

  if not (APlatform in [npfBrowser, npfNativeLCL]) then
  begin
    raise ENyxModel.Create('Named producers require an actual output target');
  end;
  Result := DispatchNyxBehavior(ANode, ntNamed);
  LDeclared := FindNyxNamedEvent(ANode, AName, LSchema);

  if not LDeclared and (Result.Source <> ANode) then
  begin
    LDeclared := FindNyxNamedEvent(Result.Source, AName, LSchema);
  end;

  if not LDeclared then
  begin
    raise ENyxModel.Create('Named event has no admitted producer declaration: ' + AName.Name);
  end;

  if ((APlatform = npfBrowser) and (LSchema.Browser = ncMissing)) or
    ((APlatform = npfNativeLCL) and (LSchema.Native = ncMissing)) then
  begin
    raise ENyxModel.Create('Named event is unavailable on this output target: ' + AName.Name);
  end;
  if AHasPayload or not LSchema.PayloadOptional then
  begin
    LSchema.Payload.Admit(APayload, AHasPayload);
  end;

  if Result.Info.Name.Name = '' then
  begin
    Exit;
  end;
  Result.Info.Name := NyxNamedEvent(AName.Name);
  case LSchema.Payload.Kind of
    nepSignal:
      begin
        { The initialized signal has no value/details. }
      end;
    nepScalar:
      begin
        Result.Info.HasValue := AHasPayload;
        Result.Info.ValueKind := LSchema.Payload.Domain.Kind;

        if AHasPayload then
        begin
          Result.Info.Value := APayload.Copy;
          Result.Info.ValueID := ANode.ID;
        end;
      end;
    nepData:
      begin
        Result.Info.HasDetails := AHasPayload;

        if AHasPayload then
        begin
          Result.Info.Details := APayload.Copy;
        end;
      end;
  end;
end;

function NyxHasLegacyClick(ANode: TNyxNode): Boolean;
begin
  Result := (ANode.ProjectionKind = 'button') or
    (ANode.ProjectionKind = 'link') or
    (ANode.Props.IndexOfName(NyxAttributeName(atEmit)) >= 0);
end;

function TNyxEventInfo.IsNamed(const AName: TNyxEventRef): Boolean;
begin
  Result := Name.Name = AName.Name;
end;

function TNyxEventInfo.Copy: TNyxEventInfo;
begin
  Result.Trigger := Trigger;
  Result.Name := NyxEvent(Name.Name);
  Result.SourceID := SourceID;
  Result.OriginID := OriginID;
  Result.TargetID := TargetID;
  Result.ValueID := ValueID;
  Result.Value := Value.Copy;
  Result.ValueKind := ValueKind;
  Result.HasValue := HasValue;
  { Older record initializers do not populate Details. Its managed text is
    initialized by both compilers; an empty representation is no payload. }
  Result.HasDetails := Details.Defined and HasDetails;
  Result.Details := NyxNull;

  if Result.HasDetails then
  begin
    Result.Details := Details.Copy;
  end;
  Result.Changed := Changed;
  Result.HasKeyboard := HasKeyboard;
  Result.Keyboard := Keyboard;
  Result.HasTextEdit := HasTextEdit;
  Result.TextEdit := TextEdit;
  Result.HasEditing := HasEditing and Editing.Defined;
  Result.Editing := Editing;
  Result.HasPointer := HasPointer;
  Result.Pointer := Pointer;
  Result.HasDrag := HasDrag and Drag.Defined;
  Result.Drag := Drag;
  Result.HasWheel := Wheel.Defined and HasWheel;
  Result.Wheel := Wheel;
  Result.HasViewport := Viewport.Defined and HasViewport;
  Result.Viewport := Viewport;
  Result.HasCollectionSelection := HasCollectionSelection and
    SelectionBefore.Defined and Selection.Defined;
  Result.SelectionBefore := SelectionBefore;
  Result.Selection := Selection;
  Result.DefaultPrevented := DefaultPrevented;
end;

function TNyxDispatch.GetEventName: TNyxText;
begin
  Result := Info.Name.Name;
end;

function NyxDesignEvent(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxEventInfo;
begin

  if (ANode = nil) or not (ATrigger in [ntDesignSelect, ntDesignValue]) then
  begin
    raise ENyxModel.Create('Design events require a node and design trigger');
  end;
  Result.Trigger := ATrigger;
  Result.Name := NyxEvent(NyxTriggerName(ATrigger));
  Result.SourceID := ANode.ID;
  Result.OriginID := ANode.ID;
  Result.TargetID := ANode.ID;
  Result.ValueID := ANode.ID;
  Result.ValueKind := nskText;
  Result.HasValue := ANode.Props.IndexOfName(NyxAttributeName(atValue)) >= 0;
  Result.HasDetails := False;
  Result.Details := NyxNull;
  Result.Value := NyxNull;

  if Result.HasValue then
  begin
    Result.Value := NyxData(ANode.Prop(NyxAttributeName(atValue)));
  end;
  Result.Changed := ATrigger = ntDesignValue;
  Result.HasKeyboard := False;
  Result.Keyboard := NyxKeyStroke(nkUnknownKey);
  Result.HasTextEdit := False;
  Result.HasEditing := False;
  Result.Editing := Default(TNyxEditingSnapshot);
  Result.TextEdit.Before := '';
  Result.TextEdit.After := '';
  Result.HasPointer := False;
  Result.HasDrag := False;
  Result.Drag := Default(TNyxDragSnapshot);
  Result.Pointer.Kind := npiUnknown;
  Result.Pointer.Button := npbNone;
  Result.Pointer.Buttons := [];
  Result.Pointer.Modifiers := [];
  Result.Pointer.X := 0;
  Result.Pointer.Y := 0;
  Result.Pointer.HasPosition := False;
  Result.Pointer.ID := 0;
  Result.Pointer.Primary := True;
  Result.Pointer.Pressure := 0;
  Result.DefaultPrevented := False;
  Result.HasWheel := False;
  Result.Wheel := Default(TNyxWheelSnapshot);
  Result.HasViewport := False;
  Result.Viewport := Default(TNyxViewportSnapshot);
  Result.HasCollectionSelection := False;
  Result.SelectionBefore := Default(TNyxCollectionSelectionSnapshot);
  Result.Selection := Default(TNyxCollectionSelectionSnapshot);
end;

procedure UpdateSelection(ASource: TNyxNode);
var
  LIndex: Integer;
  LChild: TNyxNode;
  LAction: TNyxAction;
  LDomain: TNyxValueDomain;
  LValue: TNyxDataValue;
  LOption: TNyxDataValue;
  LPrepared: Boolean;
  LHasValue: Boolean;
  LMatches: Boolean;
begin
  LPrepared := False;
  LHasValue := False;
  LDomain := NyxNoDomain;
  LValue := NyxNull;
  for LIndex := 0 to ASource.Count - 1 do
  begin
    LChild := ASource.Children[LIndex];

    if TryNyxAction(LChild.Prop(NyxAttributeName(atAction)), LAction) and
      (LAction = naSelect) then
    begin

      if not LPrepared then
      begin
        LDomain := NyxNodeValueDomain(ASource);

        if not LDomain.Defined then
        begin
          raise ENyxContract.Create('Selection presentation requires a declared value domain');
        end;
        LHasValue := (ASource.Props.IndexOfName(NyxAttributeName(atValue)) >= 0) and
          ((ASource.Prop(NyxAttributeName(atValue)) <> '') or (LDomain.Kind = nskText));

        if LHasValue then
        begin
          LValue := LDomain.ReadWire(ASource.Prop(NyxAttributeName(atValue)));
        end;
        LPrepared := True;
      end;
      LOption := LDomain.ReadWire(LChild.Prop(NyxAttributeName(atOption)));
      LMatches := False;

      if LHasValue then
      begin
        { Selection compares admitted scalar meaning. Imported numeric spellings
          such as 05 or 1.00 must not lose their selected presentation merely
          because the runtime setter writes a canonical decimal. }
        case LDomain.Kind of
          nskText: LMatches := LOption.AsText = LValue.AsText;
          nskBoolean: LMatches := LOption.AsBoolean = LValue.AsBoolean;
          nskInteger: LMatches := LOption.AsInteger = LValue.AsInteger;
          nskNumber: LMatches := LOption.AsNumber = LValue.AsNumber;
        end;
      end;
      LChild.Configure.Variant(nvDefault).Done;

      if LMatches then
      begin
        LChild.Configure.Variant(nvPrimary).Done;
      end;
      LChild.Configure.Pressed(LMatches).Done;
    end;
  end;
end;

procedure PrepareNyxBehavior(ARoot: TNyxNode);
var
  LIndex: Integer;
begin
  UpdateSelection(ARoot);
  for LIndex := 0 to ARoot.Count - 1 do
  begin
    PrepareNyxBehavior(ARoot.Children[LIndex]);
  end;
end;

function DispatchNyxBehavior(ANode: TNyxNode; ATrigger: TNyxTrigger): TNyxDispatch;
var
  LAncestor: TNyxNode;
  LTarget: TNyxNode;
  LAction: TNyxAction;
  LValue: Integer;
  LMinimum: Integer;
  LMaximum: Integer;
  LFoundCompound: Boolean;
  LDomain: TNyxValueDomain;
  LSelected: TNyxDataValue;
begin

  if (ANode = nil) or (ATrigger in [ntDesignSelect, ntDesignValue]) then
  begin
    raise ENyxModel.Create('Application dispatch requires a node and runtime trigger');
  end;
  Result.Source := ANode;
  Result.Target := ANode;
  Result.Changed := False;
  Result.Info.Trigger := ATrigger;
  Result.Info.Name := NyxEvent(NyxTriggerName(ATrigger));
  Result.Info.SourceID := ANode.ID;
  Result.Info.OriginID := ANode.ID;
  Result.Info.TargetID := ANode.ID;
  Result.Info.ValueID := '';
  Result.Info.Value := NyxNull;
  Result.Info.ValueKind := nskText;
  Result.Info.HasValue := False;
  Result.Info.HasDetails := False;
  Result.Info.Details := NyxNull;
  Result.Info.Changed := False;
  Result.Info.HasKeyboard := False;
  Result.Info.Keyboard := NyxKeyStroke(nkUnknownKey);
  Result.Info.HasTextEdit := False;
  Result.Info.HasEditing := False;
  Result.Info.HasDrag := False;
  Result.Info.Drag := Default(TNyxDragSnapshot);
  Result.Info.Editing := Default(TNyxEditingSnapshot);
  Result.Info.TextEdit.Before := '';
  Result.Info.TextEdit.After := '';
  Result.Info.HasPointer := False;
  Result.Info.Pointer.Kind := npiUnknown;
  Result.Info.Pointer.Button := npbNone;
  Result.Info.Pointer.Buttons := [];
  Result.Info.Pointer.Modifiers := [];
  Result.Info.Pointer.X := 0;
  Result.Info.Pointer.Y := 0;
  Result.Info.Pointer.HasPosition := False;
  Result.Info.Pointer.ID := 0;
  Result.Info.Pointer.Primary := True;
  Result.Info.Pointer.Pressure := 0;
  Result.Info.DefaultPrevented := False;
  Result.Info.HasWheel := False;
  Result.Info.Wheel := Default(TNyxWheelSnapshot);
  Result.Info.HasViewport := False;
  Result.Info.Viewport := Default(TNyxViewportSnapshot);
  Result.Info.HasCollectionSelection := False;
  Result.Info.SelectionBefore := Default(TNyxCollectionSelectionSnapshot);
  Result.Info.Selection := Default(TNyxCollectionSelectionSnapshot);
  LFoundCompound := False;
  LAncestor := ANode;
  while LAncestor <> nil do
  begin

    if (ATrigger <> ntAfterExit) and
      ((LAncestor.Prop(NyxAttributeName(atEnabled), 'true') = 'false') or
      (LAncestor.Prop(NyxAttributeName(atVisible), 'true') = 'false')) then
    begin
      Result.Info.Name := NyxEvent('');
      Exit;
    end;

    if not LFoundCompound and
      (LAncestor.Prop(NyxAttributeName(atCompound)) = 'true') then
    begin
      Result.Source := LAncestor;
      Result.Info.SourceID := LAncestor.ID;
      LFoundCompound := True;
    end;
    LAncestor := LAncestor.Parent;
  end;

  if ATrigger <> ntClick then
  begin
    { Focus notifications are signals. They cannot accidentally execute a
      button action or reuse its click/change application name. }

    if ATrigger = ntChange then
    begin
      Result.Info.Name := NyxEvent(ANode.Prop(NyxAttributeName(atEmitChange),
        NyxTriggerName(ATrigger)));
    end;
    Exit;
  end;
  Result.Info.Name := NyxEvent(ANode.Prop(NyxAttributeName(atEmit), NyxTriggerName(ATrigger)));

  if not TryNyxAction(ANode.Prop(NyxAttributeName(atAction)), LAction) then
  begin
    raise ENyxModel.Create('Unknown portable action: ' + ANode.Prop(NyxAttributeName(atAction)));
  end;

  if LAction = naNone then
  begin
    Exit;
  end;
  LTarget := Result.Source;

  if (LAction <> naSelect) and (ANode.Prop(NyxAttributeName(atTarget)) <> '') then
  begin
    LTarget := Result.Source.Part(ANode.Prop(NyxAttributeName(atTarget)));
  end;

  if (LAction in [naClear, naToggle, naSelect, naIncrement, naDecrement]) and
    NyxInteractionPolicy(LTarget).ReadOnly then
  begin
    raise ENyxModel.Create('Read-only action target cannot be edited');
  end;
  case LAction of
    naNone:
      begin
        Exit;
      end;
    naClear:
      begin
        LTarget.Configure.Value('').Done;
      end;
    naDismiss:
      begin
        LTarget.Configure.Visible(False).Done;
      end;
    naToggle:
      begin
        LDomain := NyxNodeValueDomain(LTarget);

        if not LDomain.Defined or (LDomain.Kind <> nskBoolean) then
        begin
          raise ENyxContract.Create('Toggle requires a Boolean value domain');
        end;
        LSelected := LDomain.ReadWire(LTarget.Prop(NyxAttributeName(atValue), 'false'));
        LTarget.Configure.Value(not LSelected.AsBoolean).Done;
      end;
    naSelect:
      begin
        LDomain := NyxNodeValueDomain(LTarget);

        if not LDomain.Defined then
        begin
          raise ENyxContract.Create('Selection requires a declared value domain');
        end;
        LSelected := LDomain.ReadWire(ANode.Prop(NyxAttributeName(atOption)));
        case LDomain.Kind of
          nskText:
            begin
              LTarget.Configure.Value(LSelected.AsText).Done;
            end;
          nskBoolean:
            begin
              LTarget.Configure.Value(LSelected.AsBoolean).Done;
            end;
          nskInteger:
            begin
              LTarget.Configure.Value(LSelected.AsInteger).Done;
            end;
          nskNumber:
            begin
              LTarget.Configure.Value(LSelected.AsNumber).Done;
            end;
        end;
        UpdateSelection(Result.Source);
      end;
    naIncrement, naDecrement:
      begin
        { Decode strict whole values before mutation; malformed drafts never
          silently become zero. Bounded arithmetic cannot wrap on either target. }
        LValue := 0;

        if (LTarget.Prop(NyxAttributeName(atValue)) <> '') and
          not TryNyxInteger(LTarget.Prop(NyxAttributeName(atValue)), LValue) then
        begin
          raise ENyxModel.Create('Stepper value requires a signed integer');
        end;
        LMinimum := -1000000;
        LMaximum := 1000000;

        if ((LTarget.Prop(NyxAttributeName(atMinimum)) <> '') and
          not TryNyxInteger(LTarget.Prop(NyxAttributeName(atMinimum)), LMinimum)) or
          ((LTarget.Prop(NyxAttributeName(atMaximum)) <> '') and
          not TryNyxInteger(LTarget.Prop(NyxAttributeName(atMaximum)), LMaximum)) or
          (LMinimum > LMaximum) or (LMinimum < -1000000) or (LMaximum > 1000000) then
        begin
          raise ENyxModel.Create('Stepper range must be ordered within +/-1000000');
        end;

        if LValue < LMinimum then
        begin
          LValue := LMinimum;
        end;

        if LValue > LMaximum then
        begin
          LValue := LMaximum;
        end;

        if (LAction = naIncrement) and (LValue < LMaximum) then
        begin
          Inc(LValue);
        end;

        if (LAction = naDecrement) and (LValue > LMinimum) then
        begin
          Dec(LValue);
        end;
        LTarget.Configure.Value(LValue).Done;
      end;
  end;
  Result.Target := LTarget;
  Result.Info.TargetID := LTarget.ID;
  Result.Changed := True;
  Result.Info.Changed := True;
end;

end.
