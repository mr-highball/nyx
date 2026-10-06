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
  nyx.responsive,
  nyx.presentations,
  nyx.containers,
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
    { Review belongs to this session/load, even if another project uses the same
      owner and registration IDs. Reloading identical files invalidates it. }
    Context: TNyxStudioCommandContext;
  end;

const
  NyxInspectorPropertiesID = 'inspector-tab-properties';
  NyxInspectorEventsID = 'inspector-tab-events';
  NyxStudioViewportMinimumID = 'inspector-viewport-minimum';
  NyxStudioViewportMaximumID = 'inspector-viewport-maximum';
  NyxStudioViewportHeightMinimumID = 'inspector-viewport-height-minimum';
  NyxStudioViewportHeightMaximumID = 'inspector-viewport-height-maximum';
  NyxStudioViewportOrientationID = 'inspector-viewport-orientation';
  NyxStudioViewportLayoutID = 'inspector-viewport-layout';
  NyxStudioViewportApplyID = 'inspector-viewport-apply';
  NyxStudioPresentationNameID = 'inspector-presentation-name';
  NyxStudioPresentationActivationID = 'inspector-presentation-activation';
  NyxStudioPresentationContainerID = 'inspector-presentation-container';
  NyxStudioPresentationChoiceID = 'inspector-presentation-choice';
  NyxStudioPresentationAttributeID = 'inspector-presentation-attribute';
  NyxStudioPresentationPlatformID = 'inspector-presentation-platform';
  NyxStudioPresentationDefineID = 'inspector-presentation-define';
  NyxStudioPresentationUseID = 'inspector-presentation-use';
  NyxStudioPresentationResetID = 'inspector-presentation-reset';
  NyxStudioPresentationPreviewID = 'studio-presentation-preview';
  { Closed size-bound reset intent at the chrome metadata boundary. The captured
    exact authored owner prevents a delayed button acting on a later selection. }
  NyxStudioPropertyClearKey = 'studio.property-clear';
  NyxStudioPropertyOwnerKey = 'studio.property-owner';
  NyxStudioEventCommandKey = 'studio.event-command';
  NyxStudioEventOwnerKey = 'studio.event-owner';
  NyxStudioEventTriggerKey = 'studio.event-trigger';
  NyxStudioEventNameKey = 'studio.event-name';
  NyxStudioEventHandlerKey = 'studio.event-handler';
  NyxStudioEventIDKey = 'studio.event-registration';

{ Public Nyx composition only: event cards, selects, buttons and the warning
  work on both target adapters. Projections/session remain borrowed and unmutated. }
procedure AddNyxEventsInspector(AParent: TNyxNode; ASession: TNyxStudioSession;
  AProjection: TNyxNode; const ARemoval: TNyxCallbackRemoval); overload;
{ Queue presentation overlays exact policies and disables an event whose removal
  is pending. It contains no candidate document or mutable callback collection. }
procedure AddNyxEventsInspector(AParent: TNyxNode; ASession: TNyxStudioSession;
  AProjection: TNyxNode; const ARemoval: TNyxCallbackRemoval;
  const APending: TNyxStudioPendingDesign); overload;
{ Capture immutable typed mutation or a presentation-only warning/navigation.
  This function never generates source or publishes a pair on the UI thread.
  The out edit is sdaEvent only for add, policy or an exact confirmed removal. }
function CaptureNyxStudioEvents(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; const ARemovalReview: TNyxCallbackRemoval;
  const APending: TNyxStudioPendingDesign; out AEdit: TNyxStudioDesignEdit;
  out AEffect: TNyxInspectorEffect; out ALine: Integer;
  out ARemoval: TNyxCallbackRemoval): Boolean;
{ Portable controller boundary. Only the explicit Confirm command mutates removal;
  Request exposes an owned warning snapshot. Add returns its TODO line; Navigate
  returns the existing implementation line. Rejection retains design and history. }
function RouteNyxStudioEvents(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; const APending: TNyxCallbackRemoval;
  out AEffect: TNyxInspectorEffect; out ALine: Integer;
  out ARemoval: TNyxCallbackRemoval): Boolean;

{ Shared Nyx controls compose a bounded viewport condition and a typed layout.
  Existing rule properties continue through the ordinary typed inspector. }
procedure AddNyxViewportInspector(AParent: TNyxNode; const AOwner: TNyxText;
  ADocument: TNyxDocument = nil);
{ Capture one property intent for the independent paired processor. The source
  button owns an exact selection; stale owners and incomplete intervals refuse.
  No accepted document, source, history or control is changed here. }
function CaptureNyxViewportInspector(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; out AEdit: TNyxStudioDesignEdit): Boolean;
{ Closed editor selection boundary. Captions prefix manual names so an
  application name cannot collide with the default choice. Exact names retain
  Unicode and order; unknown/automatic selections refuse. No state is changed. }
function NyxStudioPresentationItems(const ADefinitions: INyxPresentationSnapshot): TNyxText;
function NyxStudioPresentationChoice(const ASelection: TNyxPresentationSelection): TNyxText;
function ReadNyxStudioPresentationChoice(const AChoice: TNyxText;
  const ADefinitions: INyxPresentationSnapshot): TNyxPresentationSelection;

implementation

uses
  nyx.schema, nyx.controls, nyx.studio.callbackedits, nyx.studio.edits;

const
  CAutomaticPresentation = 'Automatic / defaults';
  CManualPresentationPrefix = 'Manual / ';

function NyxStudioPresentationItems(const ADefinitions: INyxPresentationSnapshot): TNyxText;
var
  LIndex: Integer;
  LReference: TNyxPresentationRef;
begin
  Result := CAutomaticPresentation;

  if ADefinitions = nil then
  begin
    Exit;
  end;
  for LIndex := 0 to ADefinitions.Count - 1 do
  begin
    LReference := ADefinitions.Reference(LIndex);

    if ADefinitions.Definition(LReference).Activation = npaManual then
    begin
      Result := Result + TNyxText(#10) + TNyxText(CManualPresentationPrefix) + LReference.Name;
    end;
  end;
end;

function NyxStudioPresentationChoice(const ASelection: TNyxPresentationSelection): TNyxText;
begin
  Result := CAutomaticPresentation;

  if ASelection.Reference.Defined then
  begin
    Result := TNyxText(CManualPresentationPrefix) + ASelection.Reference.Name;
  end;
end;

function ReadNyxStudioPresentationChoice(const AChoice: TNyxText;
  const ADefinitions: INyxPresentationSnapshot): TNyxPresentationSelection;
begin
  Result := TNyxPresentationSelection.None;

  if AChoice = CAutomaticPresentation then
  begin
    Exit;
  end;

  if Copy(AChoice, 1, Length(CManualPresentationPrefix)) <> CManualPresentationPrefix then
  begin
    raise ENyxPresentation.Create('Choose an available manual presentation or automatic defaults');
  end;
  Result := TNyxPresentationSelection.Use(NyxPresentation(
    Copy(AChoice, Length(CManualPresentationPrefix) + 1, MaxInt)));
  Result.Validate(ADefinitions);
end;

procedure AddNyxViewportInspector(AParent: TNyxNode; const AOwner: TNyxText;
  ADocument: TNyxDocument);
var
  LCard: INyxCard;
  LNames: TNyxText;
  LAttributes: TNyxText;
  LFirstName: TNyxText;
  LFirstAttribute: TNyxText;
  LInfos: TNyxPropertyInfos;
  LIndex: Integer;
  LAttribute: TNyxAttribute;
  LCanLayout: Boolean;
begin
  LCard := NewNyxCard('inspector-viewport-rule');
  AParent.Add(LCard);
  LCard.Configure.Layout(nlColumn).Gap(8).Padding(12);
  LCard.Add(NewNyxHeading('inspector-viewport-title').Configure.Text('Responsive layout').Done);
  LCard.Add(NewNyxLabel('inspector-viewport-help').Configure.Text(
    'Combine available width, height and orientation. Rules keep the same controls.').Done);
  LCard.Add(NewNyxSpin(NyxStudioViewportMinimumID).Configure.Text('Minimum width (inclusive)')
    .Minimum(0).Maximum(1000000).Value(0).Done);
  LCard.Add(NewNyxSpin(NyxStudioViewportMaximumID).Configure.Text('Below width (0 = no limit)')
    .Minimum(0).Maximum(1000000).Value(640).Done);
  LCard.Add(NewNyxSpin(NyxStudioViewportHeightMinimumID).Configure.Text('Minimum height (inclusive)')
    .Minimum(0).Maximum(1000000).Value(0).Done);
  LCard.Add(NewNyxSpin(NyxStudioViewportHeightMaximumID).Configure.Text('Below height (0 = no limit)')
    .Minimum(0).Maximum(1000000).Value(0).Done);
  LCard.Add(NewNyxSelect(NyxStudioViewportOrientationID).Configure.Text('Available orientation')
    .Items(NyxViewportOrientationName(nvoAny) + #10 + NyxViewportOrientationName(nvoPortrait) +
      #10 + NyxViewportOrientationName(nvoLandscape) + #10 + NyxViewportOrientationName(nvoSquare))
    .Value(NyxViewportOrientationName(nvoAny)).Done);
  LCard.Add(NewNyxSelect(NyxStudioViewportLayoutID).Configure.Text('Layout in this range')
    .Items('column' + #10 + 'row' + #10 + 'grid' + #10 + 'absolute').Value('column').Done);
  LCard.Add(NewNyxButton(NyxStudioViewportApplyID).Configure.Text('Set layout rule').Done);
  LCard.Node.Find(NyxStudioViewportApplyID).SetProp(NyxStudioPropertyOwnerKey, AOwner);

  if ADocument = nil then
  begin
    Exit;
  end;
  LNames := '';
  LFirstName := '';
  for LIndex := 0 to ADocument.Presentations.Count - 1 do
  begin

    if LIndex = 0 then
    begin
      LFirstName := ADocument.Presentations.Reference(LIndex).Name;
    end
    else
    begin
      LNames := LNames + TNyxText(#10);
    end;
    LNames := LNames + ADocument.Presentations.Reference(LIndex).Name;
  end;
  LInfos := NyxProperties(ADocument.Find(AOwner), ADocument);
  LAttributes := '';
  LFirstAttribute := '';
  LCanLayout := False;
  for LIndex := 0 to High(LInfos) do
  begin

    if TryNyxAttribute(LInfos[LIndex].Key, LAttribute) and NyxPlatformAttribute(LAttribute) then
    begin

      if LAttributes = '' then
      begin
        LFirstAttribute := LInfos[LIndex].Key;
      end
      else
      begin
        LAttributes := LAttributes + TNyxText(#10);
      end;
      LAttributes := LAttributes + LInfos[LIndex].Key;
      LCanLayout := LCanLayout or (LAttribute = atLayout);
    end;
  end;
  LCard.Node.Find(NyxStudioViewportLayoutID).Configure.Visible(LCanLayout).Done;
  LCard.Node.Find(NyxStudioViewportApplyID).Configure.Visible(LCanLayout).Done;
  LCard.Add(NewNyxHeading('inspector-presentation-title').Configure.Text('Shared presentations').Done);
  LCard.Add(NewNyxLabel('inspector-presentation-help').Configure.Text(
    'Choose automatic bounds or manual selection. Updating a name changes all of its overrides.').Done);
  LCard.Add(NewNyxSelect(NyxStudioPresentationActivationID).Configure.Text('Activation')
    .Items('automatic' + #10 + 'manual').Value('automatic').Done);
  LCard.Add(NewNyxInput(NyxStudioPresentationNameID).Configure.Text('Presentation name')
    .Placeholder('compact').Done);
  LCard.Add(NewNyxInput(NyxStudioPresentationContainerID).Configure.Text('Query container (optional)')
    .Placeholder('Leave empty to measure the whole view').Done);
  LCard.Add(NewNyxButton(NyxStudioPresentationDefineID).Configure.Text('Define or update presentation').Done);
  LCard.Add(NewNyxSelect(NyxStudioPresentationChoiceID).Configure.Text('Shared presentation')
    .Items(LNames).Value(LFirstName).Enabled(LNames <> '').Done);
  LCard.Add(NewNyxSelect(NyxStudioPresentationAttributeID).Configure.Text('Property to override')
    .Items(LAttributes).Value(LFirstAttribute).Done);
  LCard.Add(NewNyxSelect(NyxStudioPresentationPlatformID).Configure.Text('Target scope')
    .Items('any' + #10 + 'browser' + #10 + 'native-lcl').Value('any').Done);
  LCard.Add(NewNyxButton(NyxStudioPresentationUseID).Configure.Text('Add override')
    .Enabled(LNames <> '').Done);
  LCard.Add(NewNyxButton(NyxStudioPresentationResetID).Configure.Text('Reset override')
    .Enabled(LNames <> '').Done);
  LCard.Node.Find(NyxStudioPresentationDefineID).SetProp(NyxStudioPropertyOwnerKey, AOwner);
  LCard.Node.Find(NyxStudioPresentationUseID).SetProp(NyxStudioPropertyOwnerKey, AOwner);
  LCard.Node.Find(NyxStudioPresentationResetID).SetProp(NyxStudioPropertyOwnerKey, AOwner);
end;

function CaptureNyxViewportInspector(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; out AEdit: TNyxStudioDesignEdit): Boolean;
var
  LMinimum: Integer;
  LMaximum: Integer;
  LHeightMinimum: Integer;
  LHeightMaximum: Integer;
  LViewport: TNyxViewportCondition;
  LOrientation: TNyxViewportOrientation;
  LLayout: TNyxLayoutMode;
  LChoice: TNyxText;
  LFound: Boolean;
  LReference: TNyxPresentationRef;
  LAttribute: TNyxAttribute;
  LPlatform: TNyxPlatform;
begin
  AEdit := Default(TNyxStudioDesignEdit);
  Result := (AButton <> nil) and ((AButton.ID = NyxStudioViewportApplyID) or
    (AButton.ID = NyxStudioPresentationDefineID) or (AButton.ID = NyxStudioPresentationUseID) or
    (AButton.ID = NyxStudioPresentationResetID));

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or (AShellRoot = nil) or
    (AButton.Prop(NyxStudioPropertyOwnerKey) <> ASession.SelectedID) then
  begin
    raise ENyxModel.Create('Select this component again before setting its viewport rule');
  end;
  AEdit.Selection := ASession.SelectedID;
  AEdit.View := ASession.ActiveViewID;

  if (AButton.ID = NyxStudioPresentationUseID) or (AButton.ID = NyxStudioPresentationResetID) then
  begin
    LReference := NyxPresentation(AShellRoot.Find(NyxStudioPresentationChoiceID).Prop('value'));
    ASession.Document.Presentations.Definition(LReference);

    if not TryNyxAttribute(AShellRoot.Find(NyxStudioPresentationAttributeID).Prop('value'), LAttribute) then
    begin
      raise ENyxModel.Create('Choose a published presentation property');
    end;
    LFound := False;
    for LPlatform := npfAny to npfNativeLCL do
    begin

      if NyxPlatformName(LPlatform) = AShellRoot.Find(NyxStudioPresentationPlatformID).Prop('value') then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxModel.Create('Choose a supported target scope');
    end;
    AEdit.Action := sdaPresentation;
    AEdit.Presentation := NyxUsePresentation(NyxControl(ASession.SelectedID), LReference,
      LAttribute, LPlatform);

    if AButton.ID = NyxStudioPresentationResetID then
    begin
      AEdit.Presentation := NyxResetPresentation(NyxControl(ASession.SelectedID), LReference,
        LAttribute, LPlatform);
    end;
    Exit;
  end;

  if AButton.ID = NyxStudioPresentationDefineID then
  begin
    LChoice := AShellRoot.Find(NyxStudioPresentationActivationID).Prop('value');

    if LChoice = 'manual' then
    begin
      { Bounds are deliberately unused for manual activation. No hidden
        predicate survives when a shared definition switches activation. }
      AEdit.Action := sdaPresentation;
      AEdit.Presentation := NyxDefinePresentation(NyxPresentation(
        AShellRoot.Find(NyxStudioPresentationNameID).Prop('value')), TNyxPresentationCondition.Manual);
      Exit;
    end;

    if LChoice <> 'automatic' then
    begin
      raise ENyxModel.Create('Choose automatic or manual presentation activation');
    end;
  end;

  if not TryStrToInt(AShellRoot.Find(NyxStudioViewportMinimumID).Prop('value'), LMinimum) or
    not TryStrToInt(AShellRoot.Find(NyxStudioViewportMaximumID).Prop('value'), LMaximum) or
    not TryStrToInt(AShellRoot.Find(NyxStudioViewportHeightMinimumID).Prop('value'), LHeightMinimum) or
    not TryStrToInt(AShellRoot.Find(NyxStudioViewportHeightMaximumID).Prop('value'), LHeightMaximum) then
  begin
    raise ENyxModel.Create('Enter complete Integer viewport bounds');
  end;

  LViewport := TNyxViewportCondition.Any;

  if LMaximum = 0 then
  begin
    LViewport := LViewport.WidthAtLeast(LMinimum);
  end
  else
  begin
    LViewport := LViewport.WidthBetween(LMinimum, LMaximum);
  end;

  if LHeightMaximum = 0 then
  begin
    LViewport := LViewport.HeightAtLeast(LHeightMinimum);
  end
  else
  begin
    LViewport := LViewport.HeightBetween(LHeightMinimum, LHeightMaximum);
  end;
  LChoice := AShellRoot.Find(NyxStudioViewportOrientationID).Prop('value');
  LFound := False;
  for LOrientation := Low(TNyxViewportOrientation) to High(TNyxViewportOrientation) do
  begin

    if NyxViewportOrientationName(LOrientation) = LChoice then
    begin
      LViewport := LViewport.Orientation(LOrientation);
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxModel.Create('Choose a supported viewport orientation');
  end;

  if LViewport.IsAny then
  begin
    raise ENyxModel.Create('Use the ordinary Layout property for every viewport');
  end;

  if AButton.ID = NyxStudioPresentationDefineID then
  begin
    AEdit.Action := sdaPresentation;
    AEdit.Presentation := NyxDefinePresentation(NyxPresentation(
      AShellRoot.Find(NyxStudioPresentationNameID).Prop('value')), LViewport);

    if AShellRoot.Find(NyxStudioPresentationContainerID).Prop('value') <> '' then
    begin
      AEdit.Presentation := NyxDefinePresentation(NyxPresentation(
        AShellRoot.Find(NyxStudioPresentationNameID).Prop('value')),
        TNyxPresentationCondition.Within(NyxContainer(
          AShellRoot.Find(NyxStudioPresentationContainerID).Prop('value')), LViewport));
    end;
    Exit;
  end;
  LChoice := AShellRoot.Find(NyxStudioViewportLayoutID).Prop('value');
  LFound := False;
  for LLayout := Low(TNyxLayoutMode) to High(TNyxLayoutMode) do
  begin

    if NyxLayoutName(LLayout) = LChoice then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxModel.Create('Choose a supported layout');
  end;
  AEdit.Action := sdaProperty;
  AEdit.Selection := ASession.SelectedID;
  AEdit.View := ASession.ActiveViewID;
  AEdit.Name := NyxViewportKey(LViewport, npfAny, atLayout);
  AEdit.Value := NyxLayoutName(LLayout);
end;

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
begin
  AddNyxEventsInspector(AParent, ASession, AProjection, ARemoval,
    Default(TNyxStudioPendingDesign));
end;

procedure AddNyxEventsInspector(AParent: TNyxNode; ASession: TNyxStudioSession;
  AProjection: TNyxNode; const ARemoval: TNyxCallbackRemoval;
  const APending: TNyxStudioPendingDesign);
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
  LLocked: Boolean;
  LPendingPolicy: TNyxExecutionPolicy;
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
      LLocked := APending.EventLocked(ASession.SelectedID, LInfo.Trigger, LInfo.Name);

      if APending.EventPolicy(ASession.SelectedID, LInfo.Trigger, LInfo.Name, LPendingPolicy) then
      begin
        LInfo.Policy := LPendingPolicy;
      end;
      LPolicy.Configure.Items(LChoices.Text).Value(NyxPolicyName(LInfo.Policy))
        .Enabled(not LLocked).Done;
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
        LButton.Configure.Width(112).Enabled(not LLocked)
          .AccessibleName('Remove ' + LInfo.Callbacks[LCallbackIndex].Handler.Name).Done;
        LButton.SetProp(NyxStudioEventIDKey, LInfo.Callbacks[LCallbackIndex].ID.Name)
          .SetProp(NyxStudioEventHandlerKey, LInfo.Callbacks[LCallbackIndex].Handler.Name);
        LRow.Add(LButton);
      end;
      LCard.Add(EventCommand(nkButton, LKey + '-add', '+ Add callback',
        icAdd, ASession.SelectedID, LInfo.Trigger, LInfo.Name)
        .Configure.Enabled(not LLocked).Done);
    end;
  finally
    LChoices.Free;
  end;
  AParent.Add(TNyxNode.Create(nkLabel, 'events-policy-help').Configure.Text(
    'Asynchronous uses native workers or the browser event loop. Threaded requires native workers. ' +
    'UI queue defers to the UI thread.').Done);

  if ARemoval.Pending and (ARemoval.OwnerID = ASession.SelectedID) and
    ASession.MatchesCommandContext(ARemoval.Context) then
  begin
    LCard := TNyxNode.Create(nkCard, 'event-removal-warning');
    LCard.Configure.Padding(12).Surface(True).Done;
    AParent.Add(LCard);
    LCard.Add(TNyxNode.Create(nkHeading, 'event-removal-title')
      .Configure.Text('Remove this registration?').Done);
    LCard.Add(TNyxNode.Create(nkLabel, 'event-removal-text').Configure.Text(
      NyxCallbackRemovalWarning(ASession.Document, ARemoval.OwnerID, ARemoval.Handler)).Done);
    LButton := EventCommand(nkButton, 'event-removal-confirm', 'Remove registration',
      icConfirm, ARemoval.OwnerID, ARemoval.Trigger, ARemoval.Name);
    LButton.Configure.Variant(nvDanger).Enabled(not APending.EventLocked(
      ARemoval.OwnerID, ARemoval.Trigger, ARemoval.Name)).Done;
    LButton.SetProp(NyxStudioEventIDKey, ARemoval.ID.Name);
    LCard.Add(LButton);
    LCard.Add(EventCommand(nkButton, 'event-removal-cancel', 'Keep registration',
      icCancel, ARemoval.OwnerID, ARemoval.Trigger, ARemoval.Name));
  end;
end;

function CaptureNyxStudioEvents(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; const ARemovalReview: TNyxCallbackRemoval;
  const APending: TNyxStudioPendingDesign; out AEdit: TNyxStudioDesignEdit;
  out AEffect: TNyxInspectorEffect; out ALine: Integer;
  out ARemoval: TNyxCallbackRemoval): Boolean;
var
  LCommand: TInspectorCommand;
  LEvent: TNyxTrigger;
  LName: TNyxEventRef;
  LCommandFound: Boolean;
  LEventFound: Boolean;
  LPolicy: TNyxExecutionPolicy;
  LProjection: TNyxNode;
  LInfos: TNyxAuthoredEventInfos;
  LIndex: Integer;
  LCallbackIndex: Integer;
  LFound: Boolean;
begin
  Result := False;
  AEffect := nieNone;
  ALine := 0;
  ARemoval := Default(TNyxCallbackRemoval);
  AEdit := Default(TNyxStudioDesignEdit);

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
  AEdit.Selection := ASession.SelectedID;
  AEdit.View := ASession.ActiveViewID;
  AEdit.Event.Trigger := LEvent;
  AEdit.Event.Name := LName;

  if (LCommand in [icAdd, icPolicy, icConfirm]) and
    APending.EventLocked(AEdit.Selection, LEvent, LName) then
  begin
    raise ENyxModel.Create('Wait for this callback removal before editing its event');
  end;
  case LCommand of
    icAdd:
      begin

        AEdit.Action := sdaEvent;
        AEdit.Event.Action := seaAdd;
      end;
    icPolicy:
      begin

        if not TryNyxPolicy(ANode.Prop('value'), LPolicy) then
        begin
          raise ENyxModel.Create('Choose a supported execution policy');
        end;

        AEdit.Action := sdaEvent;
        AEdit.Event.Action := seaPolicy;
        AEdit.Event.Policy := LPolicy;
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
                ARemoval.Context := ASession.CommandContext;
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

        if not ARemovalReview.Pending or
          not ASession.MatchesCommandContext(ARemovalReview.Context) or
          (ARemovalReview.OwnerID <> ASession.SelectedID) or
          (ARemovalReview.Trigger <> LEvent) or
          (ARemovalReview.Name.Name <> LName.Name) or
          (ARemovalReview.ID.Name <> ANode.Prop(NyxStudioEventIDKey)) then
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
                  ((LInfos[LIndex].Callbacks[LCallbackIndex].ID.Name = ARemovalReview.ID.Name) and
                  (LInfos[LIndex].Callbacks[LCallbackIndex].Handler.Name = ARemovalReview.Handler.Name));
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

        AEdit.Action := sdaEvent;
        AEdit.Event.Action := seaRemove;
        AEdit.Event.ID := ARemovalReview.ID;
        AEdit.Event.Handler := ARemovalReview.Handler;
      end;
    icCancel: AEffect := nieCancelRemoval;
  end;
  Result := True;
end;

function RouteNyxStudioEvents(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; const APending: TNyxCallbackRemoval;
  out AEffect: TNyxInspectorEffect; out ALine: Integer;
  out ARemoval: TNyxCallbackRemoval): Boolean;
var
  LEdit: TNyxStudioDesignEdit;
  LHandler: TNyxHandlerRef;
begin
  Result := CaptureNyxStudioEvents(ASession, ANode, ATrigger, APending,
    Default(TNyxStudioPendingDesign), LEdit, AEffect, ALine, ARemoval);

  if Result and (LEdit.Action = sdaEvent) then
  begin
    ASession.ApplyEventIntent(LEdit.Event, LHandler, ALine);
    case LEdit.Event.Action of
      seaAdd:
        begin
          AEffect := nieSource;
        end;
      seaRemove:
        begin
          AEffect := nieRemoved;
        end;
      seaPolicy:
        begin
          AEffect := nieNone;
        end;
    end;
  end;
end;

end.
