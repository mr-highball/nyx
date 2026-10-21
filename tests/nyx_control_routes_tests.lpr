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

program nyx_control_routes_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.flow.controls.editor, nyx.flow.routes, nyx.flow.route.model,
  nyx.flow.types, nyx.flow.declarations, nyx.state, nyx.codec, nyx.codegen, nyx.scheduler,
  nyx.source, nyx.studio.session, nyx.studio.projects, nyx.studio.edits,
  nyx.test.programs.logic, nyx.logic, nyx.logic.editor, nyx.logic.catalog,
  nyx.flow, nyx.resources, nyx.bytes, nyx.test.named, nyx.test.logic.table,
  nyx.collections, nyx.collections.view.types, nyx.master.detail, nyx.contract,
  nyx.schema, nyx.studio.inspector
  {$ifdef PAS2JS}, JS, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Control routes: ' + AReason);
  end;
  Inc(GChecks);
end;

function Node(const AForm: INyxCard; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Node.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise Exception.Create('Missing maintained form field: ' + AName);
  end;
end;

{ Exact metadata label lookup; tests do not parse ordinals into model identity.
  The product independently validates selected values against its owned choices. }
procedure Choose(const AForm: INyxCard; const AField, ACaption: TNyxText);
var
  LItems: TNyxStrings;
  LIndex: Integer;
  LLabel: TNyxText;
begin
  LItems := TNyxStrings.Create;
  try
    LItems.Text := Node(AForm, AField).Prop(NyxAttributeName(atItems));
    for LIndex := 0 to LItems.Count - 1 do
    begin
      LLabel := LItems[LIndex];

      if (LLabel = ACaption) or
        (Copy(LLabel, Length(LLabel) - Length(ACaption) + 1, Length(ACaption)) = ACaption) then
      begin
        Node(AForm, AField).Configure.Value(LLabel).Done;
        Exit;
      end;
    end;
  finally
    LItems.Free;
  end;
  raise Exception.Create('Unavailable maintained choice: ' + AField + ' / ' + ACaption);
end;

procedure Local(const AForm: INyxCard; ADocument: TNyxDocument; const AAction: TNyxText;
  ARevision: Integer = 42);
begin
  Check(HandleNyxControlRouteEditor(Node(AForm, AAction), AForm.Node, ADocument, ARevision),
    'Local route action belongs to its exact form: ' + AAction);
end;

{ Admission works from inert creator declarations, not executable operations.
  Exercise every exact value family through the public form's persisted route
  boundary, including bytes and JSON numbers that cannot pass through UI text
  conversion implicitly. No declaration can execute application/host behavior. }
procedure QualifyFamilies;
const
  CValues: array[TNyxFlowType] of TNyxText = (
    'A tiny greeting', '', '-2147483648', '12345678901234567890.125',
    '{"exact":12345678901234567890,"text":"Hello"}', 'AP+A', '');
var
  LDocument: TNyxDocument;
  LForm: INyxCard;
  LType: TNyxFlowType;
  LDescriptor: TNyxLogicDescriptor;
  LChange: TNyxControlRouteEditorChange;
  LExpected: TNyxFlowValue;
  LRoute: TNyxFlowRoute;
begin
  LDocument := ProgramLogicDocument;
  try
    for LType := Low(TNyxFlowType) to High(TNyxFlowType) do
    begin
      LDescriptor := NyxLogicDescriptor(NyxLogicKind('creator-' + NyxFlowTypeName(LType)),
        'Exact value declaration', 'Inert metadata; no executable creator authority.', nlgData)
        .Input(NyxFlowPort('Value'), LType, 'Exact required input')
        .Output(NyxFlowPort('Result'), LType, 'Exact typed result');
      LDocument.Flows.Define(NyxFlowRef('Exact value'), NewNyxFlowGraph
        .WithStep(NyxFlowStep('value'), NewNyxLogicDeclaration(LDescriptor, NyxObject([]))).Build);
      LForm := NewNyxControlRouteEditor('family', LDocument, NyxControl('greet-button'), 42);
      Node(LForm, 'name').Configure.Value('Exact value').Done;
      Choose(LForm, 'graph', 'Exact value');
      Local(LForm, LDocument, 'graph');
      Choose(LForm, 'event', 'OnClick');
      Choose(LForm, 'source', 'Constant');
      Choose(LForm, 'input', 'value / Value / ' + NyxFlowTypeName(LType));
      Node(LForm, 'constant').Configure.Value(CValues[LType]).Done;
      Node(LForm, 'constant-boolean').Configure.Value(True).Done;
      Local(LForm, LDocument, 'map-input');
      Check(CaptureNyxControlRouteEditor(Node(LForm, 'apply'), LForm.Node, LDocument, 42, LChange),
        'Creator ' + NyxFlowTypeName(LType) + ' input admits without executing the declaration');
      LRoute := NyxFlowRoutesFromData(LChange.RoutesData).Route(1);
      case LType of
        nftText: LExpected := NyxFlowText(CValues[LType]);
        nftBoolean: LExpected := NyxFlowBoolean(True);
        nftInteger: LExpected := NyxFlowInteger(Low(Integer));
        nftNumber: LExpected := NyxFlowNumber(NyxDecimal(CValues[LType]));
        nftJSON: LExpected := NyxFlowJSON(TNyxDataValue.ParseJSON(CValues[LType]));
        nftBytes: LExpected := NyxFlowBytes(NyxDecodeBase64(CValues[LType]));
        nftSignal: LExpected := NyxFlowSignal;
      end;
      Check(LRoute.Input(0).ConstantData.ToJSON = LExpected.ToData.ToJSON,
        'Exact ' + NyxFlowTypeName(LType) + ' constant survives the complete proposal');

      if LType = nftSignal then
      begin
        Choose(LForm, 'source', 'Invocation signal');
        Local(LForm, LDocument, 'map-input');
        CaptureNyxControlRouteEditor(Node(LForm, 'apply'), LForm.Node, LDocument, 42, LChange);
        Check(NyxFlowRoutesFromData(LChange.RoutesData).Route(1).Input(0).Source = nfisSignal,
          'Invocation signals retain event intent rather than an invented scalar value');
      end;
    end;
  finally
    LForm := nil;
    LDocument.Free;
  end;
end;

{ A nested form must not rewrite legacy parent-draft bytes until it contains
  unfinished work. Its owned disclosures are presentation only, but survive
  parent serialization alongside incomplete route names and execution text. }
procedure QualifyParentDraft;
var
  LDocument: TNyxDocument;
  LForm: INyxCard;
  LFresh: INyxCard;
  LDraft: TNyxLogicEditorDraft;
  LRestored: TNyxLogicEditorDraft;
  LLegacy: TNyxText;
begin
  LDocument := ProgramLogicDocument;
  try
    LForm := NewNyxLogicEditor('logic', LDocument, NyxFlowRef('Greeting flow'));
    LDraft.Capture('logic', LForm.Node);
    LLegacy := LDraft.ToData.ToJSON;
    LForm.Add(NewNyxControlRouteEditor('logic-control-routes', LDocument, NyxControl('name-input'), 42));
    LDraft.Capture('logic', LForm.Node);
    Check(LDraft.ToData.ToJSON = LLegacy, 'An untouched nested form retains exact legacy Logic draft bytes');
    LForm.Node.Find('logic-control-routes-name').Configure.Value('Unfinished name').Done;
    LForm.Node.Find('logic-control-routes-limit').Configure.Value('unfinished count').Done;
    LForm.Node.Find('logic-control-routes-execution').Configure.Value(True).Done;
    LDraft.Capture('logic', LForm.Node);
    Check(LDraft.ToData.Field('version').AsInteger = 13,
      'Only edited route forms opt the owning Logic draft into its new wrapper');
    LRestored := TNyxLogicEditorDraft.FromData(LDraft.ToData);
    LFresh := NewNyxLogicEditor('logic', LDocument, NyxFlowRef('Greeting flow'));
    LFresh.Add(NewNyxControlRouteEditor('logic-control-routes', LDocument, NyxControl('name-input'), 42));
    Check(LRestored.Restore('logic', LFresh.Node) and
      (LFresh.Node.Find('logic-control-routes-name').Prop('value') = 'Unfinished name') and
      (LFresh.Node.Find('logic-control-routes-limit').Prop('value') = 'unfinished count') and
      (LFresh.Node.Find('logic-control-routes-execution').Prop('value') = 'true'),
      'Parent round-trip retains unfinished fields and exact public disclosure state');
    LDraft.Capture('logic', LFresh.Node);
    Check(LDraft.ToData.ToJSON = LRestored.ToData.ToJSON,
      'A restored parent captures identical route and legacy draft bytes');
  finally
    LForm := nil;
    LFresh := nil;
    LDocument.Free;
  end;
end;

{ Named creator events retain their own identity and declared payload schema.
  Structured paths use the shared typed Field/Item contract; admission grants no
  emitter, renderer, file service or creator implementation authority. }
procedure QualifyMasterDetailChannels;
const
  CNamedConstructor: TNyxText = 'NyxNamedEvent(''edit'')';
var
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LDetail: INyxMasterDetail;
  LForm: INyxCard;
  LDescriptor: TNyxLogicDescriptor;
  LChange: TNyxControlRouteEditorChange;
  LRoutes: INyxFlowRoutes;
  LPair: TNyxProjectPair;
  LResolved: TNyxProjectPair;
  LFailed: Boolean;
  LBefore: TNyxText;
  LSource: TNyxText;
  LLegacy: TNyxText;

  { Malformed source must reach the checked named-event constructor, not merely
    fail later because its event is absent from discovery. Failed admission
    publishes no independent owners and cannot change the live source document. }
  procedure RefuseNamedSource(const AExpression, AMessage: TNyxText);
  var
    LInvalid: TNyxProjectPair;
    LFailure: TNyxText;
  begin
    LInvalid := LPair;
    LInvalid.Source := StringReplace(LSource, CNamedConstructor,
      'NyxNamedEvent(' + AExpression + ')', []);
    LFailure := '';
    try
      AdmitNyxProject(LInvalid, nprRequireMatch, LParsed, LWorkspace, LResolved);
    except
      on E: EArgumentException do
      begin
        LFailure := E.Message;
      end;
    end;
    Check((LFailure = AMessage) and (LParsed = nil) and (LWorkspace = nil) and
      (TNyxCodec.Encode(LDocument) = LPair.Design),
      'Strict source rejects checked named-event input: ' + AMessage);
  end;

begin
  LDocument := TNyxDocument.Create;
  LParsed := nil;
  LWorkspace := nil;
  try
    LDocument.Collections.Define(NyxCollection('people'),
      NyxCollectionSchema.Text(NyxTextField('name'), '').Text(NyxTextField('summary'), ''), []);
    LDetail := NewNyxMasterDetail('directory');
    LDetail.MasterList.Binds.Collection(NyxCollectionView(NyxCollection('people')).
      Column(NyxTextField('name'), 'People')).Done;
    LDetail.DetailEditButton.Node.Contract.On(ntClick, NyxOriginValue, NyxTextDomain);
    SetNyxMasterDetailIntent(LDetail, TNyxMasterDetailIntent.Configured(
      NyxMasterDetailOptions(NyxTextField('name'), NyxTextField('summary'))));
    LDocument.AddPage(NewNyxPage('home').Add(LDetail));
    { The inert graph requires both channels simultaneously. Discovery alone
      cannot establish that the public authoring/admission path preserves them. }
    LDescriptor := NyxLogicDescriptor(NyxLogicKind('dual-edit'), 'Two Edit channels',
      'An inert exact scalar and structured record consumer.', nlgData).
      Input(NyxFlowPort('Record'), nftJSON, 'Copied selected record').
      Input(NyxFlowPort('Caption'), nftText, 'Explicit scalar value');
    LDocument.Flows.Define(NyxFlowRef('Dual Edit'), NewNyxFlowGraph.WithStep(
      NyxFlowStep('review'), NewNyxLogicDeclaration(LDescriptor, NyxObject([]))).Build);
    LForm := NewNyxControlRouteEditor('dual-edit', LDocument, NyxControl('directory'), 42);
    Choose(LForm, 'event', 'edit');
    Choose(LForm, 'input', 'review / Record / JSON');
    Choose(LForm, 'source', 'Event value');
    LFailed := False;
    try
      Local(LForm, LDocument, 'map-input');
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'scalar Text cannot silently become structured JSON');
    Choose(LForm, 'source', 'Event details');
    Local(LForm, LDocument, 'path-clear');
    Local(LForm, LDocument, 'map-input');
    Choose(LForm, 'input', 'review / Caption / Text');
    Choose(LForm, 'source', 'Event value');
    Local(LForm, LDocument, 'map-input');
    Check(CaptureNyxControlRouteEditor(Node(LForm, 'apply'), LForm.Node,
      LDocument, 42, LChange), 'public form captures simultaneous Value and Details mappings');
    LRoutes := NyxFlowRoutesFromData(LChange.RoutesData);
    Check((LRoutes.Count = 1) and (LRoutes.Route(0).InputCount = 2) and
      (LRoutes.Route(0).Input(0).Source = nfisEventDetails) and
      (LRoutes.Route(0).Input(1).Source = nfisEventValue),
      'two exact typed inputs retain independent channels on the same Edit event');
    SetNyxFlowRoutes(LDetail.Node, LRoutes);
    ValidateNyxDocumentProperties(LDocument);
    Check(True, 'whole-document admission accepts both exact channel contracts together');
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LSource := LPair.Source;
    Check(Pos(CNamedConstructor, LSource) > 0,
      'Generated named Edit routes use the public checked event constructor');
    AdmitNyxProject(LPair, nprRequireMatch, LParsed, LWorkspace, LResolved);
    Check((TNyxCodec.Encode(LParsed) = LPair.Design) and
      (EncodeNyxProject(LResolved) = EncodeNyxProject(LPair)) and
      (NyxControlFlowRoutes(LParsed.Find('directory')).ToData.ToJSON = LRoutes.ToData.ToJSON),
      'Strict saved-pair admission preserves the named Edit route and both payload channels');
    Check(LWorkspace.Render(LParsed) = LSource,
      'No-op named-route source regeneration preserves the exact generated companion');
    FreeAndNil(LWorkspace);
    FreeAndNil(LParsed);
    { Legacy NyxEvent remains a distinct supported spelling. Only the new
      checked constructor acquires its additional open-name validation. }
    LLegacy := StringReplace(LSource, CNamedConstructor, 'NyxEvent(''edit'')', []);
    AdmitNyxProject(NyxProjectPair(LPair.Design, LLegacy), nprRequireMatch,
      LParsed, LWorkspace, LResolved);
    Check((TNyxCodec.Encode(LParsed) = LPair.Design) and (LResolved.Source = LLegacy),
      'Legacy event construction retains exact named-route source admission');
    FreeAndNil(LWorkspace);
    FreeAndNil(LParsed);
    RefuseNamedSource('''''', 'Named event requires content');
    RefuseNamedSource('''   ''', 'Named event requires content');
    RefuseNamedSource('''Bad'' + #10 + ''event''',
      'Named event contains a control or line separator');
    RefuseNamedSource('''' + StringOfChar('n', 129) + '''',
      'Named event exceeds 128 Unicode scalars');
    LForm := NewNyxControlRouteEditor('stale-edit', LDocument, NyxControl('directory'), 42);
    SetNyxMasterDetailIntent(LDetail, TNyxMasterDetailIntent.Disabled);
    LBefore := TNyxCodec.Encode(LDocument);
    LFailed := False;
    try
      CaptureNyxControlRouteEditor(Node(LForm, 'apply'), LForm.Node, LDocument, 42, LChange);
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'a form captured before Disabled refuses without publishing stale Details intent');
  finally
    LWorkspace.Free;
    LParsed.Free;
    LForm := nil;
    LDetail := nil;
    LDocument.Free;
  end;
end;

{ The full form may wait behind another source job. Its whole accepted design
  must still match when the existing queued transaction is prepared. }
procedure QualifyQueuedFormBaseline;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LForm: INyxCard;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LSchemas: INyxSchemaSnapshot;
  LPrepared: INyxPreparedDesign;
  LBefore: TNyxText;
begin
  LDocument := ProgramLogicDocument;
  LSession := nil;
  try
    LSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LDocument),
      TNyxCodegen.Generate(LDocument)));
    LSession.Select('greet-button');
    LForm := NewNyxControlRouteEditor('queued-route', LSession.Document,
      NyxControl('greet-button'), 0);
    Choose(LForm, 'route', 'Greet');
    Local(LForm, LSession.Document, 'route', 0);
    Check(CaptureNyxLogicInspector(LSession, Node(LForm, 'apply'), LForm.Node, LEdit) and
      (LEdit.TransactionDesignBaseline = TNyxCodec.Encode(LSession.Document)),
      'Studio route capture retains the whole accepted design for its queued source job');
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'unchanged captured route admits through the actual paired processor');
    LPrepared := nil;
    LSession.SetTitle('A newer accepted design');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(LPrepared.Diagnostic.Defined and
      (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'intervening design changes refuse a queued route without publishing or changing the current pair');
  finally
    LPrepared := nil;
    LSchemas := nil;
    LForm := nil;
    LSession.Free;
    LDocument.Free;
  end;
end;

{ Preserve existing creator events and exact typed Field/Item projections. }
procedure QualifyNamedAndRows;
var
  LDocument: TNyxDocument;
  LForm: INyxCard;
  LDescriptor: TNyxLogicDescriptor;
  LChange: TNyxControlRouteEditorChange;
  LRoute: TNyxFlowRoute;
  LBeforeRows: TNyxText;
  LFailed: Boolean;
begin
  LDocument := NamedDesign;
  try
    LDescriptor := NyxLogicDescriptor(NyxLogicKind('creator-integer'), 'Integer event',
      'An inert creator event consumer.', nlgData)
      .Input(NyxFlowPort('Value'), nftInteger, 'Exact integer');
    LDocument.Flows.Define(NyxFlowRef('Declared event'), NewNyxFlowGraph.WithStep(
      NyxFlowStep('event'), NewNyxLogicDeclaration(LDescriptor, NyxObject([]))).Build);
    LForm := NewNyxControlRouteEditor('named', LDocument, NyxControl('item-button'), 42);
    Choose(LForm, 'event', NamedOpened);
    Local(LForm, LDocument, 'map-input');
    CaptureNyxControlRouteEditor(Node(LForm, 'apply'), LForm.Node, LDocument, 42, LChange);
    LRoute := NyxFlowRoutesFromData(LChange.RoutesData).Route(0);
    Check((LRoute.Trigger = ntNamed) and (LRoute.EventName.Name = NamedOpened) and
      (LRoute.Input(0).Source = nfisEventValue) and (LRoute.Input(0).ValueType = nftInteger),
      'A named scalar event maps its exact payload and open event identity');
    LDescriptor := NyxLogicDescriptor(NyxLogicKind('creator-json'), 'Structured event',
      'An inert creator JSON consumer.', nlgData)
      .Input(NyxFlowPort('Value'), nftJSON, 'Owned structured details');
    LDocument.Flows.Define(NyxFlowRef('Declared event'), NewNyxFlowGraph.WithStep(
      NyxFlowStep('event'), NewNyxLogicDeclaration(LDescriptor, NyxObject([]))).Build);
    LForm := NewNyxControlRouteEditor('named', LDocument, NyxControl('item-button'), 42);
    Choose(LForm, 'event', NamedDetails);
    Choose(LForm, 'source', 'Event details');
    Node(LForm, 'path-part').Configure.Value('payload').Done;
    Local(LForm, LDocument, 'path-add');
    Node(LForm, 'path-kind').Configure.Value('Item').Done;
    Node(LForm, 'path-part').Configure.Value('0').Done;
    Local(LForm, LDocument, 'path-add');
    Local(LForm, LDocument, 'map-input');
    CaptureNyxControlRouteEditor(Node(LForm, 'apply'), LForm.Node, LDocument, 42, LChange);
    LRoute := NyxFlowRoutesFromData(LChange.RoutesData).Route(0);
    Check((LRoute.EventName.Name = NamedDetails) and
      (LRoute.Input(0).Source = nfisEventDetails) and
      (LRoute.Input(0).DetailsPath.ToData.ToJSON = NyxResourcePath.Field('payload').Item(0).ToData.ToJSON),
      'Supplementary named identity and structured Field/Item payload path remain exact');
  finally
    LForm := nil;
    LDocument.Free;
  end;
  LDocument := LogicTableRoutedDocument;
  try
    LBeforeRows := NyxControlFlowRoutes(LDocument.Find('load')).Route(0).ToData.Field('rowOutputs').ToJSON;
    LForm := NewNyxControlRouteEditor('rows', LDocument, NyxControl('load'), 42);
    Choose(LForm, 'route', 'Load team');
    Local(LForm, LDocument, 'route');
    Node(LForm, 'name').Configure.Value('Edited team route').Done;
    CaptureNyxControlRouteEditor(Node(LForm, 'apply'), LForm.Node, LDocument, 42, LChange);
    Check((NyxFlowRoutesFromData(LChange.RoutesData).Count = 1) and
      (NyxFlowRoutesFromData(LChange.RoutesData).Route(0).ID.Name = 'Edited team route') and
      (NyxFlowRoutesFromData(LChange.RoutesData).Route(0).ToData.Field('rowOutputs').ToJSON = LBeforeRows),
      'Renaming edits the loaded slot once and retains its complete Table row mappings');
    LFailed := False;
    try
      Local(LForm, LDocument, 'reset-mappings');
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Clearing scalar and Table mappings requires explicit acknowledgement');
    Node(LForm, 'confirm').Configure.Value(True).Done;
    Local(LForm, LDocument, 'reset-mappings');
    Check(Pos('Table outputs: 0', Node(LForm, 'summary').Prop('text')) > 0,
      'An acknowledged reset explicitly clears the staged Table mappings');
  finally
    LForm := nil;
    LDocument.Free;
  end;
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LParsed: TNyxDocument;
  LForm: INyxCard;
  LTyped: INyxControlRouteEditor;
  LFresh: INyxCard;
  LChange: TNyxControlRouteEditorChange;
  LDraft: TNyxControlRouteEditorDraft;
  LRestored: TNyxControlRouteEditorDraft;
  LSession: TNyxStudioSession;
  LWorkspace: TNyxSourceWorkspace;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LPending: TNyxText;
  LSource: TNyxText;
  LData: TNyxDataValue;
  LFlags: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFailed: Boolean;
  LRoutes: INyxFlowRoutes;
begin
  LDocument := ProgramLogicDocument;
  LSession := nil;
  LWorkspace := nil;
  LParsed := nil;
  try
    LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LTyped := NewNyxControlRouteEditor('event-route', LDocument, NyxControl('name-input'), 42);
    LForm := LTyped;
    Check((LTyped.EventSelect.Node = Node(LForm, 'event')) and
      (LTyped.GraphSelect.Node = Node(LForm, 'graph')) and
      (LTyped.NameInput.Node = Node(LForm, 'name')) and
      (LTyped.ApplyButton.Node = Node(LForm, 'apply')),
      'Specialized managed form exposes real typed reusable parts');
    Node(LForm, 'name').Configure.Value('Live greeting').Done;
    Choose(LForm, 'event', 'OnChange');
    Choose(LForm, 'graph', 'Greeting flow');
    Choose(LForm, 'input', 'greeting / Name / Text');
    Choose(LForm, 'output', 'greeting / Caption / Text');
    Choose(LForm, 'destination', 'caption-label / Text');
    LFailed := False;
    try
      CaptureNyxControlRouteEditor(Node(LForm, 'apply'), LForm.Node, LDocument, 42, LChange);
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore.Design),
      'Missing required graph input refuses before mutating authored data');

    Local(LForm, LDocument, 'map-input');
    Local(LForm, LDocument, 'map-output');
    Check(CaptureNyxControlRouteEditor(Node(LForm, 'apply'), LForm.Node, LDocument, 42, LChange),
      'Complete control-event/script/control proposal captures');
    LRoutes := NyxFlowRoutesFromData(LChange.RoutesData);
    Check((LRoutes.Count = 1) and (LRoutes.Route(0).Trigger = ntChange) and
      (LRoutes.Route(0).Policy = neUIQueue) and
      (LRoutes.Route(0).CallbackMode = nfcmAfterDispatch) and
      (LRoutes.Route(0).Input(0).Source = nfisEventValue) and
      (LRoutes.Route(0).Output(0).StateName = 'caption'),
      'Captured route retains typed event, UI queue, callback order and actual label binding');

    LSession := TNyxStudioSession.Create(LBefore);
    LSession.ApplyPatch(NyxFlowRoutesPatch(LChange.Owner, LRoutes));
    LAfter := LSession.ProjectSnapshot;
    Check((LAfter.Design <> LBefore.Design) and
      (Pos('.InputEventValue(', LAfter.Source) > 0) and
      (Pos('NyxTextState(''caption'')', LAfter.Source) > 0),
      'One route command writes crafted typed Pascal beside the design');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'One Undo restores every byte of the accepted design/Pascal pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'One Redo restores the exact route pair');
    LSession.SetSourceDraft(LSession.Source + #10 + '// unfinished route work');
    LPending := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailed := False;
    try
      LSession.ApplyPatch(NyxFlowRoutesPatch(LChange.Owner, NewNyxFlowRoutes));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LPending),
      'Pending Pascal refuses route mutation with its whole pair/buffer/base retained');
    FreeAndNil(LSession);

    LSource := LAfter.Source;
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LAfter.Design,
      'Portable typed source reconstructs the complete saved control route');
    Check(LWorkspace.Render(LParsed) = LSource,
      'No-op route regeneration retains exact source');
    FreeAndNil(LWorkspace);
    FreeAndNil(LParsed);

    Node(LForm, 'limit').Configure.Value('unfinished number').Done;
    LDraft.Capture(LForm.ID, LForm.Node);
    LRestored := TNyxControlRouteEditorDraft.FromData(LDraft.ToData);
    LFresh := NewNyxControlRouteEditor(LForm.ID, LDocument, NyxControl('name-input'), 42);
    LData := LDraft.ToData;
    LFlags := LData.Field('values');
    SetLength(LFields, LFlags.Count);
    for LIndex := 0 to High(LFields) do
    begin
      LFields[LIndex] := NyxField(LFlags.Key(LIndex), LFlags.Field(LFlags.Key(LIndex)));

      if LFlags.Key(LIndex) = 'graph' then
      begin
        LFields[LIndex] := NyxField('graph', NyxData('Unavailable saved graph'));
      end;
    end;
    LFlags := NyxObject(LFields);
    SetLength(LFields, LData.Count);
    for LIndex := 0 to High(LFields) do
    begin
      LFields[LIndex] := NyxField(LData.Key(LIndex), LData.Field(LData.Key(LIndex)));

      if LData.Key(LIndex) = 'values' then
      begin
        LFields[LIndex] := NyxField('values', LFlags);
      end;
    end;
    LRestored := TNyxControlRouteEditorDraft.FromData(NyxObject(LFields));
    Check(not LRestored.CanRestore(LFresh.ID, LFresh.Node) and
      not LRestored.Restore(LFresh.ID, LFresh.Node),
      'Unavailable stored graph refuses parent/child restoration before field publication');
    Check((Node(LFresh, 'graph').Prop('value') = Node(LForm, 'graph').Prop('value')) and
      (Node(LFresh, 'limit').Prop('value') = '8') and (Node(LFresh, 'name').Prop('value') = 'New route'),
      'Failed draft admission preserves all checked current form fields');
    LRestored := TNyxControlRouteEditorDraft.FromData(LDraft.ToData);
    Check(LRestored.Restore(LFresh.ID, LFresh.Node) and
      (Node(LFresh, 'limit').Prop('value') = 'unfinished number'),
      'Unfinished route fields survive independent draft persistence/restoration');
    LFailed := False;
    try
      CaptureNyxControlRouteEditor(Node(LFresh, 'apply'), LFresh.Node, LDocument, 42, LChange);
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore.Design),
      'Malformed unfinished numbers remain editable without publication');
    Node(LFresh, 'limit').Configure.Value('8').Done;
    LDocument.State.SetValue(NyxBooleanState('flag'), False);
    LFailed := False;
    try
      CaptureNyxControlRouteEditor(Node(LFresh, 'apply'), LFresh.Node, LDocument, 42, LChange);
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Changed schema refuses a stale route form at the same supplied revision');
    LFresh := NewNyxControlRouteEditor(LForm.ID, LDocument, NyxControl('name-input'), 42);
    LSource := TNyxCodec.Encode(LDocument);
    Check(not LRestored.Restore(LFresh.ID, LFresh.Node),
      'Stale draft restoration refuses before touching current controls');
    Check(Node(LFresh, 'limit').Prop('value') = '8',
      'Refused draft restoration retains every current field');

    Choose(LFresh, 'event', 'OnChange');
    Choose(LFresh, 'input', 'greeting / Name / Text');
    Choose(LFresh, 'source', 'Existing state/control binding');
    Choose(LFresh, 'state', 'State / flag');
    LFailed := False;
    try
      Local(LFresh, LDocument, 'map-input');
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LSource),
      'Boolean state cannot silently become a text script argument');

    Choose(LFresh, 'source', 'Constant');
    Node(LFresh, 'constant').Configure.Value('A tiny ' + NyxScalarText($1F680) + #0 + ' greeting').Done;
    Local(LFresh, LDocument, 'map-input');
    Choose(LFresh, 'output', 'greeting / Caption / Text');
    Choose(LFresh, 'destination', 'caption-label / Text');
    Local(LFresh, LDocument, 'map-output');
    Check(CaptureNyxControlRouteEditor(Node(LFresh, 'apply'), LFresh.Node, LDocument, 42, LChange),
      'Supplementary/NUL constant maps through typed text intent');
    LRoutes := NyxFlowRoutesFromData(LChange.RoutesData);
    Check(TNyxFlowValue.FromData(LRoutes.Route(0).Input(0).ConstantData).AsText =
      'A tiny ' + NyxScalarText($1F680) + #0 + ' greeting',
      'The private form boundary retains exact Unicode and embedded NUL');
    LFailed := False;
    try
      Local(LFresh, LDocument, 'unmap-output');
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Mapping removal requires explicit acknowledgement');
    Node(LFresh, 'confirm').Configure.Value(True).Done;
    Local(LFresh, LDocument, 'unmap-output');
    Check(CaptureNyxControlRouteEditor(Node(LFresh, 'apply'), LFresh.Node, LDocument, 42, LChange) and
      (NyxFlowRoutesFromData(LChange.RoutesData).Route(0).OutputCount = 0),
      'Confirmed removal changes only the exact output mapping');
    LDocument.Free;
    LDocument := nil;
    Check(LTyped.NameInput.Node.Prop('value') = 'Live greeting',
      'Retained typed form owns inert metadata and survives its source document');
  finally
    LTyped := nil;
    LForm := nil;
    LFresh := nil;
    LRoutes := nil;
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    QualifyFamilies;
    QualifyParentDraft;
    QualifyNamedAndRows;
    QualifyMasterDetailChannels;
    QualifyQueuedFormBaseline;
    WriteLn('PASS ', GChecks, ' authored control-route checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-control-route-contract', 'passed');
    document.body.setAttribute('data-control-route-contract-checks', IntToStr(GChecks));
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-control-route-contract', 'failed');
      document.body.setAttribute('data-control-route-contract-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
