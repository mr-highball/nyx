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
program nyx_semantic_events_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.catalog,
  nyx.codec, nyx.codegen, nyx.source, nyx.composition, nyx.contract, nyx.schema,
  nyx.behavior, nyx.events, nyx.callbacks, nyx.scheduler, nyx.event.payload, nyx.state,
  nyx.master.detail, nyx.collections, nyx.collections.view.types,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.flow.routes,
  nyx.flow.route.model, nyx.logic, nyx.resources, nyx.event.discovery,
  nyx.studio.session, nyx.studio.inspector, nyx.studio.projects, nyx.studio.agents,
  {$ifdef NYX_COMPILED_SEMANTICS}nyx.semantic.fixture,{$endif}
  {$ifdef PAS2JS}Web, JS, nyx.render.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl;{$endif}

type
  {$ifdef PAS2JS}TRenderer = TNyxBrowserRenderer;
  {$else}TRenderer = TNyxLCLRenderer;
  TButtonAccess = class(TButton);
  TEditAccess = class(TEdit);
  {$endif}
  { A factory probe owns its snapshots, never a widget or document. The compiled
    variant instead resolves an ordinary Pascal class from the exported unit. }
  TProbe = class(TNyxEventCallback, INyxCallbackFactory)
  public
    Calls: Integer;
    Last: TNyxEventInfo;
    function Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  { Synchronous declared-producer checks run inside an existing immutable schema
    snapshot. No global kind/producer registration or runtime emitter is added. }
  TDetailsSchemaCheck = class(TInterfacedObject, INyxSchemaAction)
  public
    Document: TNyxDocument;
    LegacyKind: TNyxEventPayloadKind;
    UniformDetails: Boolean;
    OptionalDetails: Boolean;
    procedure Execute;
  end;

var
  GChecks: Integer;
  GQueuedProbe: TProbe;
  GQueuedOwner: INyxEventCallback;
  GQueuedToken: INyxEventSubscription;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Semantic events: ' + AReason);
  end;
  Inc(GChecks);
end;

function TProbe.Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
begin

  if AHandler.Name <> 'TRecipeAction' then
  begin
    raise ENyxModel.Create('Unexpected semantic fixture handler');
  end;
  Result := Self;
end;

procedure TProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  Inc(Calls);
  Last := AEvent.Copy;
end;

function EventIndex(const AEvents: TNyxEventSchemas; const AName: TNyxEventRef): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to High(AEvents) do
  begin

    if (AEvents[LIndex].Trigger = ntNamed) and (AEvents[LIndex].Name.Name = AName.Name) then
    begin
      Exit(LIndex);
    end;
  end;
end;

function MetadataBytes(const AEvents: TNyxEventSchemas): TNyxText;
var
  LEvents: array of TNyxDataValue;
  LRoutes: array of TNyxDataValue;
  LEvent: Integer;
  LRoute: Integer;
  LPayload: TNyxDataValue;
begin
  SetLength(LEvents, Length(AEvents));
  for LEvent := 0 to High(AEvents) do
  begin
    SetLength(LRoutes, Length(AEvents[LEvent].Routes));
    for LRoute := 0 to High(LRoutes) do
    begin
      LRoutes[LRoute] := AEvents[LEvent].Routes[LRoute].ToData;
    end;
    LPayload := NyxNull;

    if AEvents[LEvent].Payload.Defined then
    begin
      LPayload := AEvents[LEvent].Payload.ToData;
    end;
    LEvents[LEvent] := NyxObject([
      NyxField('trigger', NyxData(Ord(AEvents[LEvent].Trigger))),
      NyxField('name', NyxData(AEvents[LEvent].Name.Name)), NyxField('payload', LPayload),
      NyxField('title', NyxData(AEvents[LEvent].Title)),
      NyxField('description', NyxData(AEvents[LEvent].Description)),
      NyxField('browser', NyxData(Ord(AEvents[LEvent].Browser))),
      NyxField('native', NyxData(Ord(AEvents[LEvent].Native))),
      NyxField('optional', NyxData(AEvents[LEvent].PayloadOptional)),
      NyxField('producer', NyxData(AEvents[LEvent].DeclaredProducer)),
      NyxField('contexts', NyxEventContextsData(AEvents[LEvent].Contexts)),
      NyxField('routes', NyxArray(LRoutes))]);
  end;
  Result := NyxArray(LEvents).ToJSON;
end;

function DetailsDocument: TNyxDocument;
var
  LPage: INyxPage;
  LDetail: INyxMasterDetail;
  LInstance: INyxComponent;
begin
  Result := TNyxDocument.Create;
  try
    Result.Collections.Define(NyxCollection('people'), NyxCollectionSchema.
      Text(NyxTextField('list-name'), '').Text(NyxTextField('full-name'), '').
      Text(NyxTextField('summary'), ''), []);
    LDetail := NewNyxMasterDetail('detail-definition');
    LDetail.MasterList.Binds.Collection(NyxCollectionView(NyxCollection('people')).
      Column(NyxTextField('list-name'), 'People').Scoped(csInstance)).Done;
    SetNyxMasterDetailIntent(LDetail, TNyxMasterDetailIntent.Configured(
      NyxMasterDetailOptions(NyxTextField('full-name'), NyxTextField('summary'))));
    Result.AddComponent(LDetail);
    LPage := NewNyxPage('details-home');
    Result.AddPage(LPage);
    LInstance := NewNyxComponent('first-detail');
    LInstance.Reference := NyxComponent('detail-definition');
    LInstance.Node.OverridePart(NyxPart('detail/edit')).Named('first-edit');
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second-detail');
    LInstance.Reference := NyxComponent('detail-definition');
    LPage.Add(LInstance);
  except
    Result.Free;
    raise;
  end;
end;

procedure DetailChannelChecks;
var
  LDocument: TNyxDocument;
  LMetadata: TNyxEventSchemas;
  LDetails: TNyxEventSchemas;
  LCopy: TNyxEventSchemas;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LDetail: INyxMasterDetail;
  LFirst: INyxControl;
  LEvent: Integer;
  LIndex: Integer;
  LBefore: TNyxText;
  LLegacy: TNyxText;
  LRegistry: TNyxText;
  LDispatch: TNyxDispatch;
  LRejected: Boolean;
  LName: TNyxEventRef;
begin
  LDocument := DetailsDocument;
  LContext := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LRegistry := CaptureNyxSchemas.ToData.ToJSON;
    LLegacy := MetadataBytes(NyxEventsMetadata(LDocument.Find('first-detail'), LDocument));
    LDetails := NyxEventDetailsMetadata(LDocument.Find('first-detail'), LDocument);
    LEvent := EventIndex(LDetails, NyxSemantic(nseEdit));
    Check((LEvent >= 0) and (LDetails[LEvent].Payload.Kind = nepData) and
      (LDetails[LEvent].Payload.ToData.Field('dataKind').AsText = 'object') and
      not LDetails[LEvent].DeclaredProducer and not LDetails[LEvent].PayloadOptional,
      'configured semantic Edit independently declares a required object Details channel');
    LContext := RealizeNyxContext(LDocument, LDocument.Find('first-edit'), LProjection);
    LDispatch := DispatchNyxBehavior(LProjection, ntClick);
    Check((LDetails[LEvent].Routes[0].OriginID = LDispatch.Info.OriginID) and
      (LDetails[LEvent].Routes[0].SourceID = LDispatch.Info.SourceID),
      'discovery retains the exact nearest compound source and physical Edit origin');
    FreeAndNil(LContext);
    LCopy := NyxEventDetailsMetadata(LDocument.Find('first-edit'), LDocument);
    Check((LCopy[EventIndex(LCopy, NyxSemantic(nseEdit))].Payload.Kind = nepData) and
      (MetadataBytes(NyxEventsMetadata(LDocument.Find('first-detail'), LDocument)) = LLegacy),
      'selected named descriptor keeps its configured context without changing legacy metadata');
    LCopy[EventIndex(LCopy, NyxSemantic(nseEdit))].Routes[0].SourceID := 'foreign-copy';
    LCopy[EventIndex(LCopy, NyxSemantic(nseEdit))].Payload := NyxSignalPayload;
    Check(LDetails[LEvent].Routes[0].SourceID = LDispatch.Info.SourceID,
      'editing a detached Details result cannot rewrite another copied snapshot');
    for LIndex := 0 to High(LDetails) do
    begin

      if LDetails[LIndex].Trigger = ntClick then
      begin
        Check(not LDetails[LIndex].Payload.Defined,
          'aggregate physical root clicks make no structured Details guarantee');
      end;
    end;
    Check((TNyxCodec.Encode(LDocument) = LBefore) and
      (CaptureNyxSchemas.ToData.ToJSON = LRegistry),
      'both metadata queries mutate neither authored source nor creator registry');
    LDetail := RetainNyxControl(LDocument.Find('detail-definition')) as INyxMasterDetail;
    LDetail.DetailEditButton.Configure.Value('explicit scalar').Done;
    LDetail.DetailEditButton.Contract.On(ntClick, NyxOriginValue, NyxTextDomain);
    LMetadata := NyxEventsMetadata(LDocument.Find('first-detail'), LDocument);
    LDetails := NyxEventDetailsMetadata(LDocument.Find('first-detail'), LDocument);
    Check((LMetadata[EventIndex(LMetadata, NyxSemantic(nseEdit))].Payload.Kind = nepScalar) and
      (LMetadata[EventIndex(LMetadata, NyxSemantic(nseEdit))].Payload.Domain.Kind = nskText) and
      (LDetails[EventIndex(LDetails, NyxSemantic(nseEdit))].Payload.Kind = nepData),
      'explicit scalar Value and object Details remain independently discoverable');
    LDocument.Find('first-edit').Configure.OnClick(NyxNamedEvent('Review contact')).Done;
    LName := NyxNamedEvent('Review contact');
    LDetails := NyxEventDetailsMetadata(LDocument.Find('first-edit'), LDocument);
    Check((EventIndex(LDetails, LName) >= 0) and
      (LDetails[EventIndex(LDetails, LName)].Payload.Kind = nepData),
      'renaming the exact inherited Edit role preserves its structured contract');
    LCopy := NyxEventDetailsMetadata(LDocument.Find('second-detail'), LDocument);
    Check((EventIndex(LCopy, LName) < 0) and
      (LCopy[EventIndex(LCopy, NyxSemantic(nseEdit))].Payload.Kind = nepData),
      'named descriptor changes neither sibling identity nor its Details contract');
    LFirst := RetainNyxControl(LDocument.Find('first-detail'));
    SetNyxMasterDetailIntent(LFirst, TNyxMasterDetailIntent.Disabled);
    LDetails := NyxEventDetailsMetadata(LDocument.Find('first-detail'), LDocument);
    Check(LDetails[EventIndex(LDetails, LName)].Payload.Kind = nepSignal,
      'a local Disabled mask contributes no configured record producer');
    InheritNyxMasterDetailIntent(LFirst);
    LDocument.Find('first-edit').Configure.Compound(True).Done;
    LDetails := NyxEventDetailsMetadata(LDocument.Find('first-edit'), LDocument);
    Check(LDetails[EventIndex(LDetails, LName)].Payload.Kind = nepSignal,
      'a nearer compound changes source and cannot borrow the outer record producer');
    LDocument.Find('first-edit').Configure.Compound(False).Done;
    LRejected := False;
    LContext := RealizeNyxContext(LDocument, LDocument.Find('first-detail'), LProjection);
    try
      LDispatch := DispatchNyxNamedEvent(LProjection, LName, NyxObject([]), True, npfBrowser);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'structured discovery never grants a custom Emit producer');
  finally
    LContext.Free;
    LDetail := nil;
    LFirst := nil;
    LDocument.Free;
  end;
end;

procedure DetailMixedChecks;
var
  LDocument: TNyxDocument;
  LDetail: INyxMasterDetail;
  LExtra: INyxButton;
  LDetails: TNyxEventSchemas;
  LSelected: TNyxEventSchemas;
  LEvent: Integer;
  LRoute: Integer;
  LDataCount: Integer;
  LSignalCount: Integer;
begin
  LDocument := DetailsDocument;
  try
    LDetail := RetainNyxControl(LDocument.Find('detail-definition')) as INyxMasterDetail;
    LExtra := NewNyxButton('ordinary-edit-name');
    LExtra.Configure.OnClick(NyxSemantic(nseEdit)).Done;
    LDetail.DetailColumn.Add(LExtra);
    LDetails := NyxEventDetailsMetadata(LDocument.Find('first-detail'), LDocument);
    LEvent := EventIndex(LDetails, NyxSemantic(nseEdit));
    LDataCount := 0;
    LSignalCount := 0;
    for LRoute := 0 to High(LDetails[LEvent].Routes) do
    begin

      if LDetails[LEvent].Routes[LRoute].Payload.Kind = nepData then
      begin
        Inc(LDataCount);
      end
      else if LDetails[LEvent].Routes[LRoute].Payload.Kind = nepSignal then
      begin
        Inc(LSignalCount);
      end
      else
      begin
        raise ENyxModel.Create('Details discovery cannot flatten a scalar into its channel');
      end;
    end;
    Check(not LDetails[LEvent].Payload.Defined and (LDataCount = 1) and (LSignalCount = 1),
      'same-name mixed producers keep per-route facts without a fabricated aggregate object');
    LSelected := NyxEventDetailsMetadata(LDocument.Find('first-edit'), LDocument);
    Check(LSelected[EventIndex(LSelected, NyxSemantic(nseEdit))].Payload.Kind = nepData,
      'the exact named Edit descriptor can still describe its own unmixed route');
    LSelected := NyxEventDetailsMetadata(LExtra.Node, LDocument);
    Check(LSelected[EventIndex(LSelected, NyxSemantic(nseEdit))].Payload.Kind = nepSignal,
      'an unrelated button called edit is not a selected-record producer');
    InheritNyxMasterDetailIntent(LDetail);
    LDetails := NyxEventDetailsMetadata(LDocument.Find('first-detail'), LDocument);
    Check(LDetails[LEvent].Payload.Kind = nepSignal,
      'static absence does not inherit a live producer merely from the specialized kind');
  finally
    LDetail := nil;
    LExtra := nil;
    LDocument.Free;
  end;
end;

function DeclaredDetailsSnapshot(const APayload: TNyxEventPayloadSpec;
  AOptional: Boolean): INyxSchemaSnapshot;
var
  LEvent: TNyxDataValue;
  LSchema: TNyxDataValue;
begin
  LEvent := NyxObject([
    NyxField('trigger', NyxData(Ord(ntNamed))),
    NyxField('name', NyxData(NyxSemantic(nseEdit).Name)),
    NyxField('payload', APayload.ToData),
    NyxField('title', NyxData('Declared edit')),
    NyxField('description', NyxData('An independently declared producer.')),
    NyxField('browser', NyxData(Ord(ncCustom))),
    NyxField('native', NyxData(Ord(ncCustom))),
    NyxField('optional', NyxData(AOptional)),
    NyxField('producer', NyxData(True)),
    NyxField('contexts', NyxData(0)),
    NyxField('routes', NyxArray([]))]);
  LSchema := NyxObject([
    NyxField('kind', NyxData(NyxKindName(nkMasterDetail))),
    NyxField('properties', NyxArray([])),
    NyxField('events', NyxArray([LEvent]))]);
  Result := ReadNyxSchemaSnapshot(NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('revision', NyxData(17)),
    NyxField('schemas', NyxArray([LSchema]))]));
end;

procedure TDetailsSchemaCheck.Execute;
var
  LMetadata: TNyxEventSchemas;
  LDetails: TNyxEventSchemas;
  LPublished: TNyxEventSchema;
  LEvent: Integer;
begin
  LMetadata := NyxEventsMetadata(Document.Find('detail-definition'), Document);
  LDetails := NyxEventDetailsMetadata(Document.Find('detail-definition'), Document);
  LEvent := EventIndex(LMetadata, NyxSemantic(nseEdit));
  Check((LMetadata[LEvent].Payload.Kind = LegacyKind) and LMetadata[LEvent].DeclaredProducer and
    (LMetadata[LEvent].Title = 'Declared edit') and (Length(LMetadata[LEvent].Routes) = 1),
    'legacy creator precedence retains its own aggregate and copied physical route');
  Check(FindNyxNamedEvent(Document.Find('detail-definition'), NyxSemantic(nseEdit),
    LPublished) and (LPublished.Payload.Kind = LegacyKind) and LPublished.DeclaredProducer,
    'typed Details queries never change the actual registered Emit admission');
  LEvent := EventIndex(LDetails, NyxSemantic(nseEdit));
  Check((Length(LDetails[LEvent].Routes) = 1) and
    (LDetails[LEvent].Routes[0].Payload.Kind = nepData),
    'published payload precedence never erases the checked physical Details fact');

  if UniformDetails then
  begin
    Check(LDetails[LEvent].Payload.Defined and (LDetails[LEvent].Payload.Kind = nepData) and
      (LDetails[LEvent].PayloadOptional = OptionalDetails),
      'agreeing declared object and physical object producers retain explicit optionality');
  end
  else
  begin
    Check(not LDetails[LEvent].Payload.Defined,
      'a differing declared producer prevents a false uniform Details aggregate');
  end;
end;

procedure DetailDeclaredChecks;
var
  LDocument: TNyxDocument;
  LSnapshot: INyxSchemaSnapshot;
  LAction: TDetailsSchemaCheck;
  LLease: INyxSchemaAction;
  LPayload: TNyxEventPayloadSpec;
  LBefore: TNyxText;
  LIndex: Integer;
begin
  LDocument := DetailsDocument;
  LAction := TDetailsSchemaCheck.Create;
  LLease := LAction;
  try
    LBefore := CaptureNyxSchemas.ToData.ToJSON;
    LAction.Document := LDocument;
    for LIndex := 0 to 4 do
    begin
      case LIndex of
        0:
          begin
            LPayload := NyxScalarPayload(NyxIntegerDomain.Range(0, 10));
          end;
        1:
          begin
            LPayload := NyxSignalPayload;
          end;
        2:
          begin
            LPayload := NyxDataPayload(ndArray);
          end;
      else
        begin
          LPayload := NyxDataPayload(ndObject);
        end;
      end;
      LAction.LegacyKind := LPayload.Kind;
      LAction.UniformDetails := LIndex >= 3;
      LAction.OptionalDetails := LIndex = 4;
      LSnapshot := DeclaredDetailsSnapshot(LPayload, LAction.OptionalDetails);
      LSnapshot.Execute(LLease);
    end;
    Check(CaptureNyxSchemas.ToData.ToJSON = LBefore,
      'scoped producer checks restore the unchanged global schema environment');
  finally
    LSnapshot := nil;
    LAction.Document := nil;
    LLease := nil;
    LDocument.Free;
  end;
end;

{ Reuse the ordinary semantic Inspector, its public filter and the existing
  reusable fixture. Presentation may change only the independent shell tree. }
procedure DetailInspectorChecks;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LDetail: INyxMasterDetail;
  LExtra: INyxButton;
  LShell: TNyxNode;
  LProjection: TNyxNode;
  LLabel: TNyxNode;
  LMetadata: TNyxEventSchemas;
  LDetails: TNyxEventSchemas;
  LRemoval: TNyxCallbackRemoval;
  LFilter: TNyxEventFilterResult;
  LHandler: TNyxHandlerRef;
  LPhase: Integer;
  LEvent: Integer;
  LDetailsEvent: Integer;
  LRoute: Integer;
  LDetailsRoute: Integer;
  LMatched: Integer;
  LLine: Integer;
  LKey: TNyxText;
  LBefore: TNyxText;
  LSource: TNyxText;
  LLegacy: TNyxText;
  LRegistry: TNyxText;
  LExpected: TNyxText;

  procedure Build(const AOwner: TNyxText);
  begin
    FreeAndNil(LShell);
    LSession.Select(AOwner);
    LMetadata := NyxEventsMetadata(LSession.Selected, LSession.Document);
    LDetails := NyxEventDetailsMetadata(LSession.Selected, LSession.Document);
    LEvent := EventIndex(LMetadata, NyxSemantic(nseEdit));
    LDetailsEvent := EventIndex(LDetails, NyxSemantic(nseEdit));
    Check((LEvent >= 0) and (LDetailsEvent >= 0),
      'ordinary Inspector selects the exact Edit identity in both metadata channels');
    LKey := 'event-named-' + IntToStr(LEvent);
    LShell := TNyxNode.Create(nkColumn, 'detail-events');
    LProjection := LSession.SelectedProjection;
    try
      LRemoval := Default(TNyxCallbackRemoval);
      AddNyxEventsInspector(LShell, LSession, LProjection, LRemoval);
    finally
      FreeAndNil(LProjection);
    end;
  end;

begin
  LDocument := DetailsDocument;
  LSession := nil;
  LShell := nil;
  LProjection := nil;
  try
    LDetail := RetainNyxControl(LDocument.Find('detail-definition')) as INyxMasterDetail;
    LDetail.DetailEditButton.Configure.Value('independent scalar').Done;
    LDetail.DetailEditButton.Contract.On(ntClick, NyxOriginValue, NyxTextDomain);
    LSession := TNyxStudioSession.Create;
    for LPhase := 0 to 1 do
    begin

      if LPhase = 1 then
      begin
        LExtra := NewNyxButton('mixed-inspector-edit');
        LExtra.Configure.OnClick(NyxSemantic(nseEdit)).Done;
        LDetail.DetailColumn.Add(LExtra);
      end;
      LSession.Load(TNyxCodec.Encode(LDocument));
      LSession.Select('first-detail');
      LHandler := LSession.AddCallback(NyxSemantic(nseEdit), LLine);
      LBefore := LSession.Save;
      LSource := LSession.Source;
      LRegistry := CaptureNyxSchemas.ToData.ToJSON;
      LLegacy := MetadataBytes(NyxEventsMetadata(LSession.Selected, LSession.Document));
      Build('first-detail');
      LLabel := LShell.Find(LKey + '-details');

      if LPhase = 0 then
      begin
        LExpected := 'Details: ' + NyxDataPayload(ndObject).Description;
        Check((LLabel <> nil) and
          (LLabel.Prop(NyxAttributeName(atText)) = LExpected),
          'Inspector exposes the required object Details label independently');
        LLabel := LShell.Find(LKey + '-payload');
        Check((LLabel <> nil) and
          (Pos(NyxScalarPayload(NyxTextDomain).Description,
            LLabel.Prop(NyxAttributeName(atText))) = 1),
          'Inspector keeps the simultaneous scalar label and existing payload identity');
        LFilter := ApplyNyxEventFilter(LShell, NyxEventFilter.Query('Details OBJECT'));
        Check((LFilter.Matching = 1) and
          (LShell.Find(LKey).Prop(NyxAttributeName(atVisible)) = 'true'),
          'ordinary event search discovers the independent object Details description');
      end
      else
      begin
        Check((LLabel <> nil) and (LLabel.Prop(NyxAttributeName(atText)) =
          'Details: varies by source; inspect each route.'),
          'mixed producers explicitly report varying Details without a uniform object');
        Check(not LMetadata[LEvent].Payload.Defined and
          (LShell.Find(LKey + '-payload') = nil),
          'mixed legacy scalar/signal contracts are not replaced by a new payload label');
        LFilter := ApplyNyxEventFilter(LShell, NyxEventFilter.Query('Details varies'));
        Check((LFilter.Matching = 1) and
          (LShell.Find(LKey).Prop(NyxAttributeName(atVisible)) = 'true'),
          'mixed Details remain discoverable through their truthful presentation');
      end;
      for LRoute := 0 to High(LMetadata[LEvent].Routes) do
      begin
        LMatched := -1;
        for LDetailsRoute := 0 to High(LDetails[LDetailsEvent].Routes) do
        begin

          if (LMetadata[LEvent].Routes[LRoute].Trigger =
              LDetails[LDetailsEvent].Routes[LDetailsRoute].Trigger) and
            (LMetadata[LEvent].Routes[LRoute].OriginID =
              LDetails[LDetailsEvent].Routes[LDetailsRoute].OriginID) and
            (LMetadata[LEvent].Routes[LRoute].SourceID =
              LDetails[LDetailsEvent].Routes[LDetailsRoute].SourceID) then
          begin
            LMatched := LDetailsRoute;
            Break;
          end;
        end;
        Check(LMatched >= 0, 'route presentation has an exact trigger/origin/source match');
        LExpected := ' / Details: none';

        if LDetails[LDetailsEvent].Routes[LMatched].Payload.Kind = nepData then
        begin
          LExpected := ' / Details: ' +
            LDetails[LDetailsEvent].Routes[LMatched].Payload.Description;
        end;
        LLabel := LShell.Find(LKey + '-route-' + IntToStr(LRoute));
        Check((LLabel <> nil) and
          (Pos(LMetadata[LEvent].Routes[LRoute].OriginID,
            LLabel.Prop(NyxAttributeName(atText))) = 1) and
          (Pos(LExpected, LLabel.Prop(NyxAttributeName(atText))) > 0),
          'each physical route retains its identity and its own Details fact');
      end;
      LFilter := ApplyNyxEventFilter(LShell, NyxEventFilter.Query('unmatched-record-channel'));
      Check((LFilter.Matching = 0) and
        (LShell.Find(LKey).Prop(NyxAttributeName(atVisible)) = 'false'),
        'nonmatching search hides the card instead of bypassing existing filtering');
      LFilter := ApplyNyxEventFilter(LShell, NyxEventFilter);
      Check((LFilter.Matching = LFilter.Total) and
        (LShell.Find(LKey).Prop(NyxAttributeName(atVisible)) = 'true'),
        'clearing the filter restores all ordinary event cards');
      Check((MetadataBytes(NyxEventsMetadata(LSession.Selected,
        LSession.Document)) = LLegacy) and (LSession.Save = LBefore) and
        (LSession.Source = LSource) and (CaptureNyxSchemas.ToData.ToJSON = LRegistry) and
        (Pos(LHandler.Name, LSession.Source) > 0),
        'Inspector construction/filtering preserves schemas, callbacks and exact source/design');
      Build('first-edit');
      LLabel := LShell.Find(LKey + '-details');
      Check((LLabel <> nil) and (LLabel.Prop(NyxAttributeName(atText)) =
        'Details: ' + NyxDataPayload(ndObject).Description),
        'selected named Edit descriptor keeps its unmixed object presentation');
      Check((LSession.Save = LBefore) and (LSession.Source = LSource),
        'selected descriptor presentation grants no authored data or callback mutation');
    end;
  finally
    LProjection.Free;
    LShell.Free;
    LSession.Free;
    LDetail := nil;
    LExtra := nil;
    LDocument.Free;
  end;
end;

procedure DetailSchemaAdmissionChecks;
var
  LDocument: TNyxDocument;
  LDetail: INyxMasterDetail;
  LDescriptor: TNyxLogicDescriptor;
  LRoute: TNyxFlowRoute;
  LJSON: TNyxFlowEndpoint;
  LText: TNyxFlowEndpoint;
  LExtra: INyxButton;
  LRejected: Boolean;
begin
  LDocument := DetailsDocument;
  try
    LDetail := RetainNyxControl(LDocument.Find('detail-definition')) as INyxMasterDetail;
    LDetail.DetailEditButton.Configure.Value('explicit scalar').Done;
    LDetail.DetailEditButton.Contract.On(ntClick, NyxOriginValue, NyxTextDomain);
    LDescriptor := NyxLogicDescriptor(NyxLogicKind('inert-dual-input'), 'Dual input',
      'Schema admission only; no operation executes.', nlgData).
      Input(NyxFlowPort('Object'), nftJSON, 'Captured record').
      Input(NyxFlowPort('Text'), nftText, 'Independent scalar');
    LDocument.Flows.Define(NyxFlowRef('Both channels'), NewNyxFlowGraph.WithStep(
      NyxFlowStep('record'), NewNyxLogicDeclaration(LDescriptor, NyxObject([]))).Build);
    LJSON := NyxFlowEndpoint(NyxFlowStep('record'), NyxFlowPort('Object'));
    LText := NyxFlowEndpoint(NyxFlowStep('record'), NyxFlowPort('Text'));
    LRoute := NyxNamedFlowRoute(NyxFlowRouteID('dual'), NyxFlowRef('Both channels'),
      NyxSemantic(nseEdit), nfcmAfterDispatch).InputEventDetails(LJSON, NyxResourcePath).
      InputEventValue(LText, nftText);
    SetNyxFlowRoutes(LDetail, NewNyxFlowRoutes.WithRoute(LRoute));
    ValidateNyxControlFlowRoutes(LDetail.Node, LDocument);
    Check(NyxControlFlowRoutes(LDetail.Node).Route(0).InputCount = 2,
      'schema independently admits scalar Value and structured Details on the same Edit');
    LRoute := NyxNamedFlowRoute(NyxFlowRouteID('details'), NyxFlowRef('Both channels'),
      NyxSemantic(nseEdit), nfcmAfterDispatch).InputEventDetails(LJSON, NyxResourcePath).
      InputConstant(LText, NyxFlowText('ordinary constant'));
    SetNyxFlowRoutes(LDetail, NewNyxFlowRoutes.WithRoute(LRoute));
    SetNyxMasterDetailIntent(LDetail, TNyxMasterDetailIntent.Disabled);
    LRejected := False;
    try
      ValidateNyxControlFlowRoutes(LDetail.Node, LDocument);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'disabled intent cannot admit a structured flow input');
    SetNyxMasterDetailIntent(LDetail, TNyxMasterDetailIntent.Configured(
      NyxMasterDetailOptions(NyxTextField('full-name'), NyxTextField('summary'))));
    LExtra := NewNyxButton('mixed-schema-edit');
    LExtra.Configure.OnClick(NyxSemantic(nseEdit)).Done;
    LDetail.DetailColumn.Add(LExtra);
    LRejected := False;
    try
      ValidateNyxControlFlowRoutes(LDetail.Node, LDocument);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'same-name signal producer prevents aggregate Details flow admission');
    LRoute := NyxFlowRoute(NyxFlowRouteID('physical'), NyxFlowRef('Both channels'),
      ntClick, nfcmAfterDispatch).InputEventDetails(LJSON, NyxResourcePath).
      InputConstant(LText, NyxFlowText('ordinary constant'));
    SetNyxFlowRoutes(LDetail, NewNyxFlowRoutes.WithRoute(LRoute));
    LRejected := False;
    try
      ValidateNyxControlFlowRoutes(LDetail.Node, LDocument);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'aggregate physical root OnClick cannot borrow the semantic Edit guarantee');
  finally
    LDetail := nil;
    LExtra := nil;
    LDocument.Free;
  end;
end;

function CatalogDocument: TNyxDocument;
var
  LCatalog: TNyxCatalog;
  LIndex: Integer;
  LEventIndex: Integer;
  LPage: TNyxNode;
  LRecipe: TNyxNode;
  LMetadata: TNyxEventSchemas;
begin
  Result := TNyxDocument.Create;
  LCatalog := TNyxCatalog.Create;
  try
    try
      for LIndex := NyxPrimitiveCount to LCatalog.Count - 1 do
      begin
        LPage := TNyxNode.Create(nkColumn, 'view-' + LCatalog[LIndex].Kind);
        Result.AddPage(LPage);
        LRecipe := LCatalog.NewNode(LCatalog[LIndex].Kind, LCatalog[LIndex].Kind);
        LPage.Add(LRecipe);
        LMetadata := NyxEventsMetadata(LRecipe, Result);
        for LEventIndex := 0 to High(LMetadata) do
        begin

          if LMetadata[LEventIndex].Trigger = ntNamed then
          begin
            NyxCallbacks(LRecipe).OnNamed(LMetadata[LEventIndex].Name)
              .Add(NyxHandler('TRecipeAction'), NyxCallbackID(
                LRecipe.Kind + '.' + LMetadata[LEventIndex].Name.Name));
          end;
        end;
      end;
      ValidateNyxDocumentProperties(Result);
    except
      Result.Free;
      raise;
    end;
  finally
    LCatalog.Free;
  end;
end;

procedure InventoryChecks;
var
  LDocument: TNyxDocument;
  LMetadata: TNyxEventSchemas;
  LSecond: TNyxEventSchemas;
  LSemantic: TNyxSemanticEvent;
  LDecoded: TNyxSemanticEvent;
  LPageIndex: Integer;
  LEventIndex: Integer;
  LRouteIndex: Integer;
  LRouteCount: Integer;
  LOrigins: TNyxStrings;
  LBefore: TNyxText;
  LLegacy: TNyxText;
  LNode: TNyxNode;
  LProducer: TNyxEventSchema;
  LRejected: Boolean;
begin
  for LSemantic := Low(TNyxSemanticEvent) to High(TNyxSemanticEvent) do
  begin
    Check(TryNyxSemantic(NyxSemantic(LSemantic).Name, LDecoded) and
      (LDecoded = LSemantic), 'closed semantic references retain their exact wire identity');
  end;
  Check(not TryNyxSemantic('Search', LDecoded), 'semantic wire lookup remains case sensitive');
  LDocument := CatalogDocument;
  LOrigins := TNyxStrings.Create;
  try
    { The original 35 recipes remain, followed by Disclosure and Tool Canvas.
      Those two recipes add no named Action buttons; the route total stays 55. }
    Check((LDocument.Count = 37) and (LDocument.Find('disclosure') <> nil) and
      (LDocument.Find('tool-canvas') <> nil), 'all current compound recipes are inventoried');
    LBefore := TNyxCodec.Encode(LDocument);
    LRouteCount := 0;
    for LPageIndex := 0 to LDocument.Count - 1 do
    begin
      LNode := LDocument.Pages[LPageIndex].Children[0];
      LMetadata := NyxEventsMetadata(LNode, LDocument);
      LLegacy := MetadataBytes(LMetadata);
      LSecond := NyxEventDetailsMetadata(LNode, LDocument);
      Check((Length(LSecond) = Length(LMetadata)) and
        (MetadataBytes(NyxEventsMetadata(LNode, LDocument)) = LLegacy),
        LNode.Kind + ' keeps every legacy event/route/context after independent discovery');
      LOrigins.Clear;
      for LEventIndex := 0 to High(LMetadata) do
      begin

        if LMetadata[LEventIndex].Trigger <> ntNamed then
        begin
          Continue;
        end;
        Check(TryNyxSemantic(LMetadata[LEventIndex].Name.Name, LSemantic),
          LNode.Kind + ' publishes a typed standard semantic action');
        Check(not LMetadata[LEventIndex].DeclaredProducer,
          'physical discovery never grants an undeclared custom Emit API');
        for LRouteIndex := 0 to High(LMetadata[LEventIndex].Routes) do
        begin
          Check(LOrigins.IndexOf(LMetadata[LEventIndex].Routes[LRouteIndex].OriginID) < 0,
            'an actual recipe button contributes exactly one route');
          LOrigins.Add(LMetadata[LEventIndex].Routes[LRouteIndex].OriginID);
          Check((LMetadata[LEventIndex].Routes[LRouteIndex].Trigger = ntClick) and
            (LMetadata[LEventIndex].Routes[LRouteIndex].SourceID = LNode.ID),
            'route retains its physical trigger and nearest compound identity');
          Inc(LRouteCount);
        end;
      end;
    end;
    Check(LRouteCount = 55, 'all expanded recipe action buttons are discoverable');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'catalog discovery is read only');
    LNode := LDocument.Find('search-field');
    LMetadata := NyxEventsMetadata(LNode, LDocument);
    LEventIndex := EventIndex(LMetadata, NyxSemantic(nseSearch));
    Check((LMetadata[LEventIndex].Payload.Domain.Kind = nskText) and
      LMetadata[LEventIndex].PayloadOptional, 'search exposes an optional exact text payload');
    Check(LMetadata[LEventIndex].Routes[0].ValueID = LNode.Part(NyxPart('query')).ID,
      'search identifies the sibling query control');
    LSecond := NyxEventsMetadata(LNode.Part(NyxPart('search')), LDocument);
    Check(LSecond[EventIndex(LSecond, NyxSemantic(nseSearch))].Routes[0].ValueID =
      LNode.Part(NyxPart('query')).ID, 'selected part discovery retains its surrounding contract');
    LMetadata[LEventIndex].Routes[0].OriginID := 'changed owned result';
    LSecond := NyxEventsMetadata(LNode, LDocument);
    Check(LSecond[LEventIndex].Routes[0].OriginID <> 'changed owned result',
      'returned route arrays are detached');
    Check(not FindNyxNamedEvent(LNode, NyxSemantic(nseSearch), LProducer),
      'inferred aliases cannot admit custom producers');
    LRejected := False;
    try
      DispatchNyxNamedEvent(LNode, NyxSemantic(nseSearch), NyxData('invented'), True, npfNativeLCL);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'custom Emit cannot forge a discovered physical search route');
  finally
    LOrigins.Free;
    LDocument.Free;
  end;
end;

procedure ContextChecks;
var
  LDocument: TNyxDocument;
  LPage: INyxColumn;
  LDefinition: INyxSearchField;
  LFirst: INyxComponent;
  LSecond: INyxComponent;
  LRule: TNyxNode;
  LMetadata: TNyxEventSchemas;
  LOther: TNyxEventSchemas;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LNode: INyxButton;
  LContainer: INyxColumn;
  LValue: INyxInput;
  LIndex: Integer;
  LBefore: TNyxText;
  LOptionalSchema: TNyxEventSchema;
  LDispatch: TNyxDispatch;
  LAgent: TNyxAgentSession;
  LReply: TNyxDataValue;
begin
  LOptionalSchema := NyxNamedEventSchema(NyxSemantic(nseOpen), 'OnOpen',
    'A creator explicitly permits an absent integer.', ncCustom, ncCustom,
    NyxScalarPayload(NyxIntegerDomain.Range(0, 10)));
  LOptionalSchema.PayloadOptional := True;
  RegisterNyxSchema(NyxCustomKind('optional-semantic-producer'), [], [LOptionalSchema]);
  LRule := TNyxNode.Create(NyxCustomKind('optional-semantic-producer'), 'optional');
  try
    LDispatch := DispatchNyxNamedEvent(LRule, NyxSemantic(nseOpen), NyxNull, False, npfBrowser);
    Check(not LDispatch.Info.HasValue and not LDispatch.Info.HasDetails,
      'optional creator payload does not manufacture a value');
    LDispatch := DispatchNyxNamedEvent(LRule, NyxSemantic(nseOpen), NyxData(3), True, npfNativeLCL);
    Check(LDispatch.Info.HasValue and (LDispatch.Info.Value.AsInteger = 3),
      'present optional payload retains ordinary typed admission');
    Check(FindNyxNamedEvent(LRule, NyxSemantic(nseOpen), LOptionalSchema) and
      LOptionalSchema.DeclaredProducer, 'explicit creator schema grants its own producer');
  finally
    LRule.Free;
  end;
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxColumn('home');
    LDocument.AddPage(LPage);
    LDefinition := NewNyxSearchField('search-template');
    NyxCallbacks(LDefinition).OnNamed(NyxSemantic(nseSearch))
      .Add(NyxHandler('TRecipeAction'), NyxCallbackID('inherited.search'));
    LDocument.AddComponent(LDefinition);
    LFirst := NewNyxComponent('first');
    LFirst.Configure.Component(NyxComponent('search-template')).Done;
    LSecond := NewNyxComponent('second');
    LSecond.Configure.Component(NyxComponent('search-template')).Done;
    LPage.Add(LFirst).Add(LSecond);
    LRule := LFirst.Node.OverridePart(NyxPart('search')).Named('first-search');
    LRule.Configure.OnClick(NyxSemantic(nseOpen)).Done;
    LFirst.Node.OverridePart(NyxPart('clear'), noRemove).Named('first-clear');
    LBefore := TNyxCodec.Encode(LDocument);
    LMetadata := NyxEventsMetadata(LFirst.Node, LDocument);
    LOther := NyxEventsMetadata(LSecond.Node, LDocument);
    Check((EventIndex(LMetadata, NyxSemantic(nseOpen)) >= 0) and
      (EventIndex(LMetadata, NyxSemantic(nseSearch)) >= 0),
      'override has its renamed producer and inherited unmatched callback');
    Check(Length(LMetadata[EventIndex(LMetadata, NyxSemantic(nseSearch))].Routes) = 0,
      'an inherited callback never fabricates a removed physical route');
    Check(EventIndex(LOther, NyxSemantic(nseOpen)) < 0,
      'instance override does not alter the independent instance');
    Check(EventIndex(LMetadata, NyxSemantic(nseClear)) < 0,
      'removed parts publish no obsolete routes');
    LMetadata := NyxEventsMetadata(LRule, LDocument);
    Check(LMetadata[EventIndex(LMetadata, NyxSemantic(nseOpen))].Payload.Domain.Kind = nskText,
      'part override metadata keeps the inherited sibling value domain');
    LContext := RealizeNyxContext(LDocument, LRule, LProjection);
    try
      Check(LMetadata[EventIndex(LMetadata, NyxSemantic(nseOpen))].Routes[0].ValueID =
        LProjection.Parent.Part(NyxPart('query')).ID, 'override resolves the exact local sibling identity');
    finally
      LContext.Free;
    end;
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'reuse discovery preserves authored data');
    LAgent := TNyxAgentSession.Create;
    try
      LReply := LAgent.Exchange(NyxObject([NyxField('op', NyxData('claim')),
        NyxField('project', NyxData(EncodeNyxProject(NyxProjectPair(LBefore,
          TNyxCodegen.Generate(LDocument))))), NyxField('selection', NyxData('second')),
        NyxField('view', NyxData('home'))]));
      LReply := LAgent.Call('nyx_node', 'inherited semantic journey', NyxObject([
        NyxField('id', NyxData('second')), NyxField('events', NyxData(True)),
        NyxField('eventOffset', NyxData(EventIndex(LOther, NyxSemantic(nseSearch)))),
        NyxField('eventLimit', NyxData(1))]));
      Check((LReply.Field('totalRegistrations').AsInteger = 1) and
        (LReply.Field('totalRoutes').AsInteger = 1),
        'agent event context retains inherited registrations and their instance route');
    finally
      LAgent.Free;
    end;
    LContainer := NewNyxColumn('mixed-contracts');
    LContainer.Configure.Compound(True).Done;
    LPage.Add(LContainer);
    LValue := NewNyxInput('query');
    LValue.Configure.PartName(NyxPart('query')).Value('🌙 漢字').Done;
    LContainer.Add(LValue);
    LNode := NewNyxButton('signal');
    LNode.Configure.OnClick(NyxSemantic(nseOpen)).Done;
    LContainer.Add(LNode);
    LNode := NewNyxButton('value-action');
    LNode.Configure.OnClick(NyxSemantic(nseOpen)).Done;
    LNode.Contract.On(ntClick, NyxPartValue(NyxPart('query')), NyxTextDomain);
    LContainer.Add(LNode);
    LMetadata := NyxEventsMetadata(LContainer.Node, LDocument);
    LIndex := EventIndex(LMetadata, NyxSemantic(nseOpen));
    Check((Length(LMetadata[LIndex].Routes) = 2) and not LMetadata[LIndex].Payload.Defined,
      'one name with different producers keeps route-specific payload contracts');
    Check((LMetadata[LIndex].Routes[0].Payload.Kind = nepSignal) and
      (LMetadata[LIndex].Routes[1].Payload.Domain.Kind = nskText),
      'signal and text contracts are retained independently');
    LContainer.Add(NewNyxCommandBar('nested'));
    LMetadata := NyxEventsMetadata(LContainer.Node, LDocument);
    Check(Length(LMetadata[EventIndex(LMetadata, NyxSemantic(nseOpen))].Routes) = 2,
      'nested compound routes belong to their own event source');
  finally
    LFirst := nil;
    LSecond := nil;
    LDefinition := nil;
    LNode := nil;
    LValue := nil;
    LContainer := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

procedure AuthoringChecks;
var
  LSession: TNyxStudioSession;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LCandidate: TNyxDocument;
  LAgent: TNyxAgentSession;
  LSource: TNyxText;
  LBefore: TNyxText;
  LLegacy: TNyxText;
  LHandler: TNyxHandlerRef;
  LLine: Integer;
  LIndex: Integer;
  LOffset: Integer;
  LMetadata: TNyxEventSchemas;
  LReply: TNyxDataValue;
  LEvent: TNyxDataValue;
  LShell: TNyxNode;
  LProjection: TNyxNode;
  LRemoval: TNyxCallbackRemoval;
  LEffect: TNyxInspectorEffect;
  LNextRemoval: TNyxCallbackRemoval;
  LRejected: Boolean;
  {$ifndef PAS2JS}
  LInspectorDocument: TNyxDocument;
  LInspectorRenderer: TRenderer;
  LInspectorHost: TForm;
  {$endif}
begin
  LSession := TNyxStudioSession.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LAgent := TNyxAgentSession.Create;
  LDocument := TNyxDocument.Create;
  LShell := nil;
  try
    LDocument.AddPage(TNyxNode.Create(nkColumn, 'home'));
    LDocument.Pages[0].Add(NewNyxSearchField('search')).Add(NewNyxKanbanBoard('board'));
    LSession.Load(TNyxCodec.Encode(LDocument));
    LSession.Select('search');
    LHandler := LSession.AddCallback(NyxSemantic(nseSearch), LLine);
    Check((LLine > 0) and (Pos('// TODO: implement ' + LHandler.Name + '.', LSession.Source) > 0),
      'Studio creates the ordinary Pascal TODO and source location');
    Check(Pos('.OnNamed(NyxSemantic(nseSearch))', LSession.Source) > 0,
      'crafted semantic callbacks use the closed typed vocabulary');
    LSession.SetCallbackPolicy(NyxSemantic(nseSearch), neUIQueue);
    LSource := LSession.Source;
    LBefore := LSession.Save;
    LCandidate := LWorkspace.Candidate(LSession.Document, LSource);
    try
      Check(TNyxCodec.Encode(LCandidate) = LBefore, 'source reconstructs exact typed semantic callbacks');
    finally
      LCandidate.Free;
    end;
    LLegacy := StringReplace(LSource, 'NyxSemantic(nseSearch)', 'NyxEvent(''search'')', [rfReplaceAll]);
    LSession.SetSourceDraft(LLegacy);
    LSession.ApplySourceDraft;
    Check(LSession.Source = LLegacy, 'legacy typed open references remain authored and unaltered');
    LSession.Undo;
    Check((LSession.Source = LSource) and (LSession.Save = LBefore), 'paired history restores semantic syntax and policy');
    LSession.SetSourceDraft(StringReplace(LSource, 'NyxSemantic(nseSearch)', 'NyxSemantic(ntClick)', []));
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBefore), 'wrong enum family rejects atomically');
    LSession.DiscardSourceDraft;
    LShell := TNyxNode.Create(nkColumn, 'events');
    LProjection := LSession.SelectedProjection;
    LRemoval := Default(TNyxCallbackRemoval);
    try
      AddNyxEventsInspector(LShell, LSession, LProjection, LRemoval);
    finally
      LProjection.Free;
    end;
    LMetadata := NyxEventsMetadata(LSession.Selected, LSession.Document);
    LIndex := EventIndex(LMetadata, NyxSemantic(nseSearch));
    Check(LShell.Find('event-named-' + IntToStr(LIndex) + '-route-0') <> nil,
      'Nyx-built event card displays the semantic source control');
    {$ifndef PAS2JS}
    LInspectorDocument := TNyxDocument.Create;
    LInspectorRenderer := TRenderer.Create;
    LInspectorHost := TForm.Create(nil);
    try
      LInspectorHost.SetBounds(0, 0, 280, 900);
      LInspectorDocument.AddPage(LShell.Clone);
      LInspectorRenderer.Render(LInspectorDocument, LInspectorDocument.Pages[0], LInspectorHost);
      Check(LInspectorRenderer.ControlFor('event-named-' + IntToStr(LIndex) +
        '-callback-0-source').Width > 100, 'native callback navigation retains readable width');
      Check(LInspectorRenderer.ControlFor('event-named-' + IntToStr(LIndex) +
        '-callback-0-remove').Width = 112, 'native removal retains its compact explicit width');
    finally
      LInspectorRenderer.Free;
      LInspectorHost.Free;
      LInspectorDocument.Free;
    end;
    {$endif}
    RouteNyxStudioEvents(LSession,
      LShell.Find('event-named-' + IntToStr(LIndex) + '-callback-0-source'), ntClick,
      LRemoval, LEffect, LLine, LNextRemoval);
    Check((LEffect = nieSource) and (LLine = NyxHandlerSourceLine(LSource, LHandler)),
      'semantic source button navigates to the authored implementation');
    LReply := LAgent.Exchange(NyxObject([
      NyxField('op', NyxData('claim')), NyxField('project', NyxData(
        EncodeNyxProject(NyxProjectPair(LSession.Save, LSession.Source)))),
      NyxField('selection', NyxData('board')), NyxField('view', NyxData('home'))]));
    LMetadata := NyxEventsMetadata(LSession.Document.Find('board'), LSession.Document);
    LIndex := EventIndex(LMetadata, NyxSemantic(nseAdd));
    for LOffset := 0 to 2 do
    begin
      LReply := LAgent.Call('nyx_node', 'semantic journey', NyxObject([
        NyxField('id', NyxData('board')), NyxField('events', NyxData(True)),
        NyxField('eventOffset', NyxData(LIndex)), NyxField('eventLimit', NyxData(1)),
        NyxField('routeOffset', NyxData(LOffset)), NyxField('routeLimit', NyxData(1))]));
      LEvent := LReply.Field('events').Item(0);
      Check((LReply.Field('totalRoutes').AsInteger = 3) and
        LReply.Field('routesPartial').AsBoolean and (LEvent.Field('routes').Count = 1),
        'agent route page retains total and explicitly marks partial data');
      Check(LEvent.Field('routes').Item(0).Field('origin').AsText =
        LMetadata[LIndex].Routes[LOffset].OriginID, 'agent preserves exact route order and identity');
      Check(not LEvent.Field('declaredProducer').AsBoolean,
        'agent distinguishes a physical alias from custom Emit admission');
    end;
    LReply := LAgent.Call('nyx_node', 'semantic journey', NyxObject([
      NyxField('id', NyxData('search')), NyxField('events', NyxData(True)),
      NyxField('eventOffset', NyxData(EventIndex(NyxEventsMetadata(
        LSession.Document.Find('search'), LSession.Document), NyxSemantic(nseSearch)))),
      NyxField('eventLimit', NyxData(1))]));
    Check((LReply.Field('totalRegistrations').AsInteger = 1) and
      LReply.Field('events').Item(0).Field('payloadOptional').AsBoolean,
      'agent reports exact callback and optional text contract');
  finally
    LShell.Free;
    LDocument.Free;
    LAgent.Free;
    LWorkspace.Free;
    LSession.Free;
  end;
end;

procedure ControlChecks;
var
  LDocument: TNyxDocument;
  LExpected: TNyxDocument;
  LRenderer: TRenderer;
  LProbe: TProbe;
  LFactory: INyxCallbackFactory;
  LPhysical: TProbe;
  LPhysicalOwner: INyxEventCallback;
  LPhysicalToken: INyxEventSubscription;
  LPageIndex: Integer;
  LEventIndex: Integer;
  LRouteIndex: Integer;
  LCalls: Integer;
  LPhysicalCalls: Integer;
  LRouteCount: Integer;
  LMetadata: TNyxEventSchemas;
  LRoute: TNyxEventRoute;
  LLast: TNyxEventInfo;
  LRoot: TNyxNode;
  LBefore: TNyxText;
  {$ifdef PAS2JS}LHost: TJSHTMLElement;
  {$else}LHost: TForm;{$endif}

  procedure Click(const AID: TNyxText);
  begin
    {$ifdef PAS2JS}LRenderer.ElementFor(AID).click;
    {$else}TButtonAccess(LRenderer.ControlFor(AID)).Click;{$endif}
  end;

  function Calls: Integer;
  begin
    {$ifdef NYX_COMPILED_SEMANTICS}Result := SemanticCalls;
    {$else}Result := LProbe.Calls;{$endif}
  end;

begin
  {$ifdef NYX_COMPILED_SEMANTICS}
  LDocument := BuildNyxDocument;
  LExpected := CatalogDocument;
  try
    Check(TNyxCodec.Encode(LDocument) = TNyxCodec.Encode(LExpected),
      'compiled crafted source reconstructs all default recipes and callbacks');
  finally
    LExpected.Free;
  end;
  {$else}
  LDocument := CatalogDocument;
  {$endif}
  LBefore := TNyxCodec.Encode(LDocument);
  LRenderer := TRenderer.Create;
  LProbe := TProbe.Create;
  LFactory := LProbe;
  LPhysical := TProbe.Create;
  LPhysicalOwner := LPhysical;
  LPhysicalToken := nil;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  {$else}
  LHost := TForm.Create(nil);
  LHost.SetBounds(0, 0, 900, 700);
  {$endif}
  try
    {$ifdef NYX_COMPILED_SEMANTICS}
    BindNyxCallbacks(LDocument, LRenderer.Events);
    {$else}
    BindNyxCallbacks(LDocument, LRenderer.Events, LFactory);
    {$endif}
    LRouteCount := 0;
    for LPageIndex := 0 to LDocument.Count - 1 do
    begin
      LRoot := LDocument.Pages[LPageIndex].Children[0];
      LMetadata := NyxEventsMetadata(LRoot, LDocument);
      LRenderer.Render(LDocument, LDocument.Pages[LPageIndex], LHost);
      LPhysicalToken := LRenderer.Events.On(NyxCompoundEvents(LRoot.ID), ntClick)
        .Subscribe(LPhysicalOwner);
      for LEventIndex := 0 to High(LMetadata) do
      begin
        for LRouteIndex := 0 to High(LMetadata[LEventIndex].Routes) do
        begin
          LRoute := LMetadata[LEventIndex].Routes[LRouteIndex];
          LCalls := Calls;
          LPhysicalCalls := LPhysical.Calls;
          Click(LRoute.OriginID);
          {$ifdef NYX_COMPILED_SEMANTICS}LLast := SemanticLast;
          {$else}LLast := LProbe.Last.Copy;{$endif}
          Check(Calls = LCalls + 1, LRoot.Kind + ' invokes its semantic callback exactly once');
          Check(LPhysical.Calls = LPhysicalCalls + 1,
            'physical and named registrations independently observe one control action');
          Check((LLast.Trigger = LRoute.Trigger) and
            (LLast.Name.Name = LMetadata[LEventIndex].Name.Name) and
            (LLast.OriginID = LRoute.OriginID) and (LLast.SourceID = LRoute.SourceID),
            'actual adapter snapshot matches the discovered route');
          Check(not LLast.HasDetails, 'physical recipe scalar/signal never invents structured details');
          case LRoute.Payload.Kind of
            nepSignal:
              begin
                Check(not LLast.HasValue, 'signal action has no invented scalar value');
              end;
            nepScalar:
              begin
                Check(LLast.ValueKind = LRoute.Payload.Domain.Kind, 'actual payload retains its discovered scalar family');

                if LLast.HasValue then
                begin
                  LRoute.Payload.Admit(LLast.Value, True);
                  Check(LLast.ValueID = LRoute.ValueID, 'actual payload identifies its declared value control');
                end
                else
                begin
                  Check(LRoute.PayloadOptional, 'absent scalar is explicitly optional');
                end;
              end;
            nepData:
              begin
                raise ENyxModel.Create('A default recipe cannot invent structured Details');
              end;
          end;
          Inc(LRouteCount);
        end;
      end;
      LPhysicalToken.Cancel;
    end;
    Check(LRouteCount = 55, 'every expanded default action is exercised through its actual control');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'runtime recipe actions preserve the authored document');
    LRoot := LDocument.Find('search-field');
    LRenderer.Render(LDocument, LRoot.Parent, LHost);
    { Edit the actual query widget; the semantic button reads its sibling's
      accepted Unicode value through the same contract discovery described. }
    {$ifdef PAS2JS}
    TJSHTMLInputElement(LRenderer.InputFor(LRoot.Part(NyxPart('query')).ID)).value := '🌙 漢字';
    LRenderer.InputFor(LRoot.Part(NyxPart('query')).ID).dispatchEvent(TJSEvent.new('change'));
    {$else}
    TEditAccess(LRenderer.InputFor(LRoot.Part(NyxPart('query')).ID)).HandleNeeded;
    TEdit(LRenderer.InputFor(LRoot.Part(NyxPart('query')).ID)).Text := '🌙 漢字';
    {$endif}
    LCalls := Calls;
    Click(LRoot.Part(NyxPart('search')).ID);
    {$ifdef NYX_COMPILED_SEMANTICS}LLast := SemanticLast;
    {$else}LLast := LProbe.Last.Copy;{$endif}
    Check((Calls = LCalls + 1) and LLast.HasValue and
      (LLast.Value.AsText = TNyxText('🌙 漢字')), 'actual sibling edit reaches the semantic text callback / ' +
        IntToStr(Calls - LCalls) + ' / ' + BoolToStr(LLast.HasValue, True) + ' / ' + LLast.Value.ToJSON);
    GQueuedProbe := TProbe.Create;
    GQueuedOwner := GQueuedProbe;
    GQueuedToken := LRenderer.Events.OnNamed(NyxCompoundEvents(LRoot.ID),
      NyxSemantic(nseSearch)).Policy(neUIQueue).Subscribe(GQueuedOwner);
    Click(LRoot.Part(NyxPart('search')).ID);
    Check(GQueuedProbe.Calls = 0, 'semantic UI policy defers its callback');
    LRenderer.Unmount;
  finally
    LRenderer.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
    LPhysicalToken := nil;
    LPhysicalOwner := nil;
    LFactory := nil;
    LDocument.Free;
  end;
end;

procedure ReusableControlChecks;
var
  LDocument: TNyxDocument;
  LDefinition: INyxSearchField;
  LPage: INyxColumn;
  LInstance: INyxComponent;
  LRenderer: TRenderer;
  LProbe: TProbe;
  LFactory: INyxCallbackFactory;
  LMetadata: TNyxEventSchemas;
  LRoute: TNyxEventRoute;
  LLast: TNyxEventInfo;
  LBefore: TNyxText;
  LIndex: Integer;
  LCalls: Integer;
  LName: TNyxEventRef;
  {$ifdef PAS2JS}LHost: TJSHTMLElement;
  {$else}LHost: TForm;{$endif}
begin
  LDocument := TNyxDocument.Create;
  LRenderer := TRenderer.Create;
  LProbe := TProbe.Create;
  LFactory := LProbe;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  {$else}
  LHost := TForm.Create(nil);
  LHost.SetBounds(0, 0, 700, 500);
  {$endif}
  try
    LPage := NewNyxColumn('reused-page');
    LDocument.AddPage(LPage);
    LDefinition := NewNyxSearchField('reused-search');
    NyxCallbacks(LDefinition).OnNamed(NyxSemantic(nseSearch))
      .Add(NyxHandler('TRecipeAction'), NyxCallbackID('reused.search'));
    LDocument.AddComponent(LDefinition);
    for LIndex := 0 to 1 do
    begin
      LInstance := NewNyxComponent('instance-' + IntToStr(LIndex));
      LInstance.Configure.Component(NyxComponent('reused-search')).Done;
      LPage.Add(LInstance);
      LInstance.Node.OverridePart(NyxPart('query')).Configure
        .Value(TNyxText('Independent 🌙 漢字 ') + IntToStr(LIndex)).Done;

      if LIndex = 0 then
      begin
        LInstance.Node.OverridePart(NyxPart('search')).Configure.OnClick(NyxSemantic(nseOpen)).Done;
        NyxCallbacks(LInstance).OnNamed(NyxSemantic(nseOpen))
          .Add(NyxHandler('TRecipeAction'), NyxCallbackID('instance.open'));
      end;
    end;
    LBefore := TNyxCodec.Encode(LDocument);
    {$ifdef NYX_COMPILED_SEMANTICS}
    BindNyxCallbacks(LDocument, LRenderer.Events);
    {$else}
    BindNyxCallbacks(LDocument, LRenderer.Events, LFactory);
    {$endif}
    LRenderer.Render(LDocument, LPage.Node, LHost);
    for LIndex := 0 to 1 do
    begin
      LName := NyxSemantic(nseSearch);

      if LIndex = 0 then
      begin
        LName := NyxSemantic(nseOpen);
      end;
      LMetadata := NyxEventsMetadata(LDocument.Find('instance-' + IntToStr(LIndex)), LDocument);
      LRoute := LMetadata[EventIndex(LMetadata, LName)].Routes[0];
      {$ifdef NYX_COMPILED_SEMANTICS}LCalls := SemanticCalls;
      {$else}LCalls := LProbe.Calls;{$endif}
      {$ifdef PAS2JS}LRenderer.ElementFor(LRoute.OriginID, niRuntime).click;
      {$else}TButtonAccess(LRenderer.ControlFor(LRoute.OriginID, niRuntime)).Click;{$endif}
      {$ifdef NYX_COMPILED_SEMANTICS}
      LLast := SemanticLast;
      Check(SemanticCalls = LCalls + 1, 'compiled callbacks execute once for an independent reusable route');
      {$else}
      LLast := LProbe.Last.Copy;
      Check(LProbe.Calls = LCalls + 1, 'authored callbacks execute once for an independent reusable route');
      {$endif}
      Check(LLast.IsNamed(LName) and (LLast.SourceID = LRoute.SourceID) and
        (LLast.ValueID = LRoute.ValueID) and LLast.HasValue and
        (LLast.Value.AsText = TNyxText('Independent 🌙 漢字 ') + IntToStr(LIndex)),
        'actual reused/renamed action retains its instance payload and exact discovered identities');
    end;
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'actual reusable notifications preserve the authored pair');
  finally
    LRenderer.Free;
    {$ifdef PAS2JS}LHost.remove;{$else}LHost.Free;{$endif}
    LFactory := nil;
    LDefinition := nil;
    LPage := nil;
    LInstance := nil;
    LDocument.Free;
  end;
end;

{$ifndef PAS2JS}
procedure ExportFixture;
var
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LStream: TFileStream;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LDocument := CatalogDocument;
  try
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.semantic.fixture');
    LSource := StringReplace(LSource, 'implementation' + #10,
      'function SemanticCalls: Integer;' + #10 +
      'function SemanticLast: TNyxEventInfo;' + #10 + #10 + 'implementation' + #10 +
      #10 + 'type' + #10 + '  TRecipeAction = class(TNyxEventCallback)' + #10 +
      '    procedure Invoke(const AEvent: TNyxEventInfo;' + #10 +
      '      const AExecution: INyxExecution); override;' + #10 + '  end;' + #10 +
      #10 + 'var' + #10 + '  GCalls: Integer;' + #10 +
      '  GLast: TNyxEventInfo;' + #10 + #10 +
      'procedure TRecipeAction.Invoke(const AEvent: TNyxEventInfo;' + #10 +
      '  const AExecution: INyxExecution);' + #10 + 'begin' + #10 +
      '  Inc(GCalls);' + #10 + '  GLast := AEvent.Copy;' + #10 + 'end;' + #10 +
      #10 + 'function SemanticCalls: Integer;' + #10 + 'begin' + #10 +
      '  Result := GCalls;' + #10 + 'end;' + #10 + #10 +
      'function SemanticLast: TNyxEventInfo;' + #10 + 'begin' + #10 +
      '  Result := GLast.Copy;' + #10 + 'end;' + #10, []);
    LSource := StringReplace(LSource, #10 + 'end.' + #10, #10 + 'initialization' + #10 +
      '  RegisterNyxCallback(NyxHandler(''TRecipeAction''), TRecipeAction);' + #10 +
      #10 + 'end.' + #10, []);
    LStream := TFileStream.Create(ParamStr(1), fmCreate);
    try
      LStream.WriteBuffer(LSource[1], Length(LSource));
    finally
      LStream.Free;
    end;
  finally
    LDocument.Free;
  end;
end;
{$endif}

procedure Finish;
begin
  try
    Check(GQueuedProbe.Calls = 0, 'unmount cancels queued semantic callbacks');
    GQueuedToken := nil;
    GQueuedOwner := nil;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' semantic discovery/control checks';
    document.body.setAttribute('data-semantic-events', 'passed');
    {$else}
    ExportFixture;
    WriteLn('PASS ', GChecks, ' semantic discovery/control checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-semantic-events', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    InventoryChecks;
    ContextChecks;
    DetailChannelChecks;
    DetailMixedChecks;
    DetailDeclaredChecks;
    DetailSchemaAdmissionChecks;
    DetailInspectorChecks;
    AuthoringChecks;
    ControlChecks;
    ReusableControlChecks;
    {$ifdef PAS2JS}
    window.setTimeout(@Finish, 80);
    {$else}
    CheckSynchronize(50);
    Finish;
    {$endif}
  except
    on LException: Exception do
    begin
      GQueuedToken := nil;
      GQueuedOwner := nil;
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-semantic-events', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end
    {$ifdef PAS2JS}
    else
    begin
      document.body.textContent := 'FAIL browser host: ' + String(TJSObject(JSExceptValue)['stack']);
      document.body.setAttribute('data-semantic-events', 'failed');
    end
    {$endif};
  end;
end.
