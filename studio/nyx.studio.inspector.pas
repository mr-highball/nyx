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

unit nyx.studio.inspector;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.callbacks,
  nyx.scheduler,
  nyx.studio.session;

type
  TNyxInspectorTab = (nitProperties, nitEvents);
  TNyxInspectorEffect = (nieNone, nieSource, nieRequestRemoval, nieCancelRemoval, nieRemoved);
  { Confirmation is presentation state, not a saved design. Exact owner and
    registration identities prevent a stale warning from removing another item. }
  TNyxCallbackRemoval = record
    Pending: Boolean;
    OwnerID: TNyxText;
    Trigger: TNyxTrigger;
    Name: TNyxEventRef;
    ID: TNyxCallbackRef;
    Handler: TNyxHandlerRef;
  end;

const
  NyxInspectorPropertiesID = 'inspector-tab-properties';
  NyxInspectorEventsID = 'inspector-tab-events';
  NyxStudioEventCommandKey = 'studio.event-command';
  NyxStudioEventOwnerKey = 'studio.event-owner';
  NyxStudioEventTriggerKey = 'studio.event-trigger';
  NyxStudioEventNameKey = 'studio.event-name';
  NyxStudioEventHandlerKey = 'studio.event-handler';
  NyxStudioEventIDKey = 'studio.event-registration';

{ Public Nyx composition only: event cards, selects, buttons and the warning
  work on both target adapters. Projections/session remain borrowed and unmutated. }
procedure AddNyxEventsInspector(AParent: TNyxNode; ASession: TNyxStudioSession;
  AProjection: TNyxNode; const ARemoval: TNyxCallbackRemoval);
{ Portable controller boundary. Only the explicit Confirm command mutates removal;
  Request exposes an owned warning snapshot. Add returns its TODO line; Navigate
  returns the existing implementation line. Rejection retains design and history. }
function RouteNyxStudioEvents(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; const APending: TNyxCallbackRemoval;
  out AEffect: TNyxInspectorEffect; out ALine: Integer;
  out ARemoval: TNyxCallbackRemoval): Boolean;

implementation

uses
  nyx.schema;

type
  TInspectorCommand = (icAdd, icPolicy, icNavigate, icRequest, icConfirm, icCancel);

const
  CCommands: array[TInspectorCommand] of TNyxText =
    ('add', 'policy', 'navigate', 'request-removal', 'confirm-removal', 'cancel-removal');

function EventCommand(AKind: TNyxKind; const AID, AText: TNyxText;
  ACommand: TInspectorCommand; const AOwner: TNyxText;
  ATrigger: TNyxTrigger; const AName: TNyxEventRef): TNyxNode;
begin
  Result := TNyxNode.Create(AKind, AID);
  Result.Configure.Text(AText).Done;
  Result.SetProp(NyxStudioEventCommandKey, CCommands[ACommand])
    .SetProp(NyxStudioEventOwnerKey, AOwner)
    .SetProp(NyxStudioEventTriggerKey, NyxTriggerName(ATrigger))
    .SetProp(NyxStudioEventNameKey, AName.Name);
end;

procedure AddNyxEventsInspector(AParent: TNyxNode; ASession: TNyxStudioSession;
  AProjection: TNyxNode; const ARemoval: TNyxCallbackRemoval);
var
  LMetadata: TNyxEventSchemas;
  LEvents: TNyxAuthoredEventInfos;
  LIndex: Integer;
  LInfoIndex: Integer;
  LCallbackIndex: Integer;
  LInfo: TNyxAuthoredEventInfo;
  LCard: TNyxNode;
  LRow: TNyxNode;
  LButton: TNyxNode;
  LPolicy: TNyxNode;
  LKey: TNyxText;
  LChoices: TNyxStrings;
  LExecutionPolicy: TNyxExecutionPolicy;
  LRouteIndex: Integer;
  LRoute: TNyxEventRoute;
  LRouteText: TNyxText;
  LOrder: array of Integer;
  LDisplayIndex: Integer;
  LOrderCount: Integer;
begin

  if (AParent = nil) or (ASession = nil) or (AProjection = nil) then
  begin
    raise ENyxModel.Create('Events inspector requires its selected projection');
  end;
  LMetadata := NyxEventsMetadata(ASession.Selected, ASession.Document);
  LEvents := NyxAuthoredEvents(AProjection);
  { Semantic actions are the compound's main application contract. Present
    them first while retaining original metadata indices in stable command IDs. }
  SetLength(LOrder, Length(LMetadata));
  LOrderCount := 0;
  for LIndex := 0 to High(LMetadata) do
  begin

    if LMetadata[LIndex].Trigger = ntNamed then
    begin
      LOrder[LOrderCount] := LIndex;
      Inc(LOrderCount);
    end;
  end;
  for LIndex := 0 to High(LMetadata) do
  begin

    if LMetadata[LIndex].Trigger <> ntNamed then
    begin
      LOrder[LOrderCount] := LIndex;
      Inc(LOrderCount);
    end;
  end;
  AParent.Add(TNyxNode.Create(nkLabel, 'events-help')
    .Configure.Text('Sequential callbacks run in registration order. Each event has its own execution policy.').Done);

  if Length(LMetadata) = 0 then
  begin
    AParent.Add(TNyxNode.Create(nkLabel, 'events-empty')
      .Configure.Text('This component publishes no callbacks. Choose one of its interactive parts.').Done);
  end;
  LChoices := TNyxStrings.Create;
  try
    for LExecutionPolicy := Low(TNyxExecutionPolicy) to High(TNyxExecutionPolicy) do
    begin
      LChoices.Add(NyxPolicyName(LExecutionPolicy));
    end;
    for LDisplayIndex := 0 to High(LOrder) do
    begin
      LIndex := LOrder[LDisplayIndex];
      LKey := 'event-' + NyxTriggerName(LMetadata[LIndex].Trigger);

      if LMetadata[LIndex].Trigger = ntNamed then
      begin
        LKey := LKey + '-' + IntToStr(LIndex);
      end;
      LInfo.Trigger := LMetadata[LIndex].Trigger;
      LInfo.Name := LMetadata[LIndex].Name;
      LInfo.Policy := neSequential;
      LInfo.Callbacks := nil;
      for LInfoIndex := 0 to High(LEvents) do
      begin

        if (LEvents[LInfoIndex].Trigger = LInfo.Trigger) and
          (LEvents[LInfoIndex].Name.Name = LInfo.Name.Name) then
        begin
          LInfo := LEvents[LInfoIndex];
          Break;
        end;
      end;
      LCard := TNyxNode.Create(nkCard, LKey);
      LCard.Configure.Padding(12).Gap(8).Surface(True).Done;
      AParent.Add(LCard);
      LCard.Add(TNyxNode.Create(nkHeading, LKey + '-title')
        .Configure.Text(LMetadata[LIndex].Title).Done);
      LCard.Add(TNyxNode.Create(nkLabel, LKey + '-count')
        .Configure.Text(IntToStr(Length(LInfo.Callbacks)) + ' registrations').Done);
      LCard.Add(TNyxNode.Create(nkLabel, LKey + '-description')
        .Configure.Text(LMetadata[LIndex].Description).Done);

      if LMetadata[LIndex].Payload.Defined then
      begin
        LRouteText := LMetadata[LIndex].Payload.Description;

        if LMetadata[LIndex].PayloadOptional then
        begin
          LRouteText := LRouteText + ' (value may be absent before input)';
        end;
        LCard.Add(TNyxNode.Create(nkLabel, LKey + '-payload')
          .Configure.Text(LRouteText).Done);
      end;

      if Length(LMetadata[LIndex].Routes) > 0 then
      begin
        LCard.Add(TNyxNode.Create(nkLabel, LKey + '-routes-count')
          .Configure.Text(IntToStr(Length(LMetadata[LIndex].Routes)) +
            ' source controls').Done);
        { A compact route summary uses public Nyx labels. Keep large custom
          compounds bounded; selecting a part gives its own focused event card. }
        for LRouteIndex := 0 to High(LMetadata[LIndex].Routes) do
        begin

          if LRouteIndex >= 8 then
          begin
            LCard.Add(TNyxNode.Create(nkLabel, LKey + '-routes-more')
              .Configure.Text('Select a component part to inspect its remaining routes.').Done);
            Break;
          end;
          LRoute := LMetadata[LIndex].Routes[LRouteIndex];
          LRouteText := LRoute.OriginID + ' / ' + NyxTriggerName(LRoute.Trigger);

          if LRoute.Payload.Defined then
          begin
            LRouteText := LRouteText + ' / ' + LRoute.Payload.Description;
          end;

          if LRoute.ValueID <> '' then
          begin
            LRouteText := LRouteText + ' from ' + LRoute.ValueID;
          end;
          LCard.Add(TNyxNode.Create(nkLabel, LKey + '-route-' + IntToStr(LRouteIndex))
            .Configure.Text(LRouteText).Done);
        end;
      end;
      LPolicy := EventCommand(nkSelect, LKey + '-policy', 'Execution policy',
        icPolicy, ASession.SelectedID, LInfo.Trigger, LInfo.Name);
      LPolicy.Configure.Items(LChoices.Text).Value(NyxPolicyName(LInfo.Policy)).Done;
      LCard.Add(LPolicy);
      for LCallbackIndex := 0 to High(LInfo.Callbacks) do
      begin
        { Keep navigation names and removal readable in narrow inspectors. Each
          callback owns a vertical action group on both public target adapters. }
        LRow := TNyxNode.Create(nkColumn, LKey + '-callback-' + IntToStr(LCallbackIndex));
        LRow.Configure.Gap(6).Done;
        LCard.Add(LRow);
        LButton := EventCommand(nkButton, LRow.ID + '-source',
          IntToStr(LCallbackIndex + 1) + '. ' + LInfo.Callbacks[LCallbackIndex].Handler.Name,
          icNavigate, ASession.SelectedID, LInfo.Trigger, LInfo.Name);
        LButton.Configure.Hint('Go to this Pascal implementation').Done;
        LButton.SetProp(NyxStudioEventHandlerKey, LInfo.Callbacks[LCallbackIndex].Handler.Name);
        LRow.Add(LButton);
        LButton := EventCommand(nkButton, LRow.ID + '-remove', 'Remove',
          icRequest, ASession.SelectedID, LInfo.Trigger, LInfo.Name);
        LButton.Configure.Width(112)
          .AccessibleName('Remove ' + LInfo.Callbacks[LCallbackIndex].Handler.Name).Done;
        LButton.SetProp(NyxStudioEventIDKey, LInfo.Callbacks[LCallbackIndex].ID.Name)
          .SetProp(NyxStudioEventHandlerKey, LInfo.Callbacks[LCallbackIndex].Handler.Name);
        LRow.Add(LButton);
      end;
      LCard.Add(EventCommand(nkButton, LKey + '-add', '+ Add callback',
        icAdd, ASession.SelectedID, LInfo.Trigger, LInfo.Name));
    end;
  finally
    LChoices.Free;
  end;
  AParent.Add(TNyxNode.Create(nkLabel, 'events-policy-help').Configure.Text(
    'Asynchronous uses native workers or the browser event loop. Threaded requires native workers. ' +
    'UI queue defers to the UI thread.').Done);

  if ARemoval.Pending and (ARemoval.OwnerID = ASession.SelectedID) then
  begin
    LCard := TNyxNode.Create(nkCard, 'event-removal-warning');
    LCard.Configure.Padding(12).Surface(True).Done;
    AParent.Add(LCard);
    LCard.Add(TNyxNode.Create(nkHeading, 'event-removal-title')
      .Configure.Text('Remove this registration?').Done);
    LCard.Add(TNyxNode.Create(nkLabel, 'event-removal-text').Configure.Text(
      ARemoval.Handler.Name + ' will stop receiving this event. ' +
      'Its Pascal implementation is retained. Inherited changes apply only to this instance.').Done);
    LButton := EventCommand(nkButton, 'event-removal-confirm', 'Remove registration',
      icConfirm, ARemoval.OwnerID, ARemoval.Trigger, ARemoval.Name);
    LButton.Configure.Variant(nvDanger).Done;
    LButton.SetProp(NyxStudioEventIDKey, ARemoval.ID.Name);
    LCard.Add(LButton);
    LCard.Add(EventCommand(nkButton, 'event-removal-cancel', 'Keep registration',
      icCancel, ARemoval.OwnerID, ARemoval.Trigger, ARemoval.Name));
  end;
end;

function RouteNyxStudioEvents(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; const APending: TNyxCallbackRemoval;
  out AEffect: TNyxInspectorEffect; out ALine: Integer;
  out ARemoval: TNyxCallbackRemoval): Boolean;
var
  LCommand: TInspectorCommand;
  LEvent: TNyxTrigger;
  LName: TNyxEventRef;
  LCommandFound: Boolean;
  LEventFound: Boolean;
  LPolicy: TNyxExecutionPolicy;
  LHandler: TNyxHandlerRef;
  LProjection: TNyxNode;
  LInfos: TNyxAuthoredEventInfos;
  LIndex: Integer;
  LCallbackIndex: Integer;
  LFound: Boolean;
begin
  Result := False;
  AEffect := nieNone;
  ALine := 0;
  ARemoval.Pending := False;

  if (ANode = nil) or (ANode.Prop(NyxStudioEventCommandKey) = '') then
  begin
    Exit;
  end;
  LCommandFound := False;
  for LCommand := Low(TInspectorCommand) to High(TInspectorCommand) do
  begin

    if CCommands[LCommand] = ANode.Prop(NyxStudioEventCommandKey) then
    begin
      LCommandFound := True;
      Break;
    end;
  end;
  LEventFound := False;
  LName := Default(TNyxEventRef);
  for LEvent := Low(TNyxTrigger) to High(TNyxTrigger) do
  begin

    if NyxTriggerName(LEvent) = ANode.Prop(NyxStudioEventTriggerKey) then
    begin
      LEventFound := NyxIsRuntimeTrigger(LEvent);

      if (LEvent = ntNamed) and (ANode.Prop(NyxStudioEventNameKey) <> '') then
      begin
        LName := NyxEvent(ANode.Prop(NyxStudioEventNameKey));
        LEventFound := True;
      end;
      Break;
    end;
  end;

  if not LCommandFound or not LEventFound or (ASession = nil) or
    (ANode.Prop(NyxStudioEventOwnerKey) <> ASession.SelectedID) then
  begin
    raise ENyxModel.Create('Event selection changed; use its current inspector');
  end;

  if ((LCommand = icPolicy) and (ATrigger <> ntChange)) or
    ((LCommand <> icPolicy) and (ATrigger <> ntClick)) then
  begin
    Exit;
  end;
  case LCommand of
    icAdd:
      begin

        if LEvent = ntNamed then
        begin
          LHandler := ASession.AddCallback(LName, ALine);
        end
        else
        begin
          LHandler := ASession.AddCallback(LEvent, ALine);
        end;
        AEffect := nieSource;
      end;
    icPolicy:
      begin

        if not TryNyxPolicy(ANode.Prop('value'), LPolicy) then
        begin
          raise ENyxModel.Create('Choose a supported execution policy');
        end;

        if LEvent = ntNamed then
        begin
          ASession.SetCallbackPolicy(LName, LPolicy);
        end
        else
        begin
          ASession.SetCallbackPolicy(LEvent, LPolicy);
        end;
      end;
    icNavigate:
      begin
        ALine := ASession.CallbackLine(NyxHandler(ANode.Prop(NyxStudioEventHandlerKey)));
        AEffect := nieSource;
      end;
    icRequest:
      begin
        LProjection := ASession.SelectedProjection;
        try
          LInfos := NyxAuthoredEvents(LProjection);
          LFound := False;
          for LIndex := 0 to High(LInfos) do
          begin

            if (LInfos[LIndex].Trigger <> LEvent) or
              (LInfos[LIndex].Name.Name <> LName.Name) then
            begin
              Continue;
            end;
            for LCallbackIndex := 0 to High(LInfos[LIndex].Callbacks) do
            begin

              if LInfos[LIndex].Callbacks[LCallbackIndex].ID.Name =
                ANode.Prop(NyxStudioEventIDKey) then
              begin
                ARemoval.Pending := True;
                ARemoval.OwnerID := ASession.SelectedID;
                ARemoval.Trigger := LEvent;
                ARemoval.Name := LName;
                ARemoval.ID := LInfos[LIndex].Callbacks[LCallbackIndex].ID;
                ARemoval.Handler := LInfos[LIndex].Callbacks[LCallbackIndex].Handler;
                LFound := True;
              end;
            end;
          end;

          if not LFound then
          begin
            raise ENyxModel.Create('This callback registration is no longer present');
          end;
        finally
          LProjection.Free;
        end;
        AEffect := nieRequestRemoval;
      end;
    icConfirm:
      begin

        if not APending.Pending or (APending.OwnerID <> ASession.SelectedID) or
          (APending.Trigger <> LEvent) or
          (APending.Name.Name <> LName.Name) or
          (APending.ID.Name <> ANode.Prop(NyxStudioEventIDKey)) then
        begin
          raise ENyxModel.Create('Review the current removal warning before confirming');
        end;
        LProjection := ASession.SelectedProjection;
        try
          LInfos := NyxAuthoredEvents(LProjection);
          LFound := False;
          for LIndex := 0 to High(LInfos) do
          begin

            if (LInfos[LIndex].Trigger = LEvent) and
              (LInfos[LIndex].Name.Name = LName.Name) then
            begin
              for LCallbackIndex := 0 to High(LInfos[LIndex].Callbacks) do
              begin
                LFound := LFound or
                  ((LInfos[LIndex].Callbacks[LCallbackIndex].ID.Name = APending.ID.Name) and
                  (LInfos[LIndex].Callbacks[LCallbackIndex].Handler.Name = APending.Handler.Name));
              end;
            end;
          end;
        finally
          LProjection.Free;
        end;

        if not LFound then
        begin
          raise ENyxModel.Create('This registration changed; review its new removal warning');
        end;

        if LEvent = ntNamed then
        begin
          ASession.RemoveCallback(LName, APending.ID);
        end
        else
        begin
          ASession.RemoveCallback(LEvent, APending.ID);
        end;
        AEffect := nieRemoved;
      end;
    icCancel: AEffect := nieCancelRemoval;
  end;
  Result := True;
end;

end.
