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
    Check(LDocument.Count = 35, 'all default compound recipes are inventoried');
    LBefore := TNyxCodec.Encode(LDocument);
    LRouteCount := 0;
    for LPageIndex := 0 to LDocument.Count - 1 do
    begin
      LNode := LDocument.Pages[LPageIndex].Children[0];
      LMetadata := NyxEventsMetadata(LNode, LDocument);
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
        .Value('Independent 🌙 漢字 ' + IntToStr(LIndex)).Done;

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
