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

unit nyx.studio.agents;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.schema,
  nyx.studio.session, nyx.studio.projects, nyx.studio.compiler, nyx.studio.builds,
  nyx.studio.rootedits;

type
  { Operator permissions are closed, session-local and never part of a design.
    Only the editor exchange can change them. MCP cannot grant itself access. }
  TNyxAgentPermission = (apDisabled, apReadOnly, apEdit);

  { One active, authoritative authoring session. Owns the ordinary Studio model
    and source/history; no shadow design format or browser automation exists.
    Native transports serialize all calls under an external lock. Portable tests
    and native controllers use the same command/query boundary directly. }
  TNyxAgentSession = class
  private
    FSession: TNyxStudioSession;
    FRevision: Integer;
    FPermission: TNyxAgentPermission;
    FClaimed: Boolean;
    FActivity: array of TNyxDataValue;
    FActivitySerial: Integer;
    FReport: INyxCompilerReport;
    FCompilerSequence: Integer;
    FReceiptKeys: array of TNyxText;
    FReceiptRequests: array of TNyxText;
    FReceiptResults: array of TNyxDataValue;
    { Review tickets are bounded session state, never document content. Exact
      actor/revision/change bytes bind consent; accepted retries use receipts. }
    FCallbackReviews: array of TNyxDataValue;
    FCallbackReviewSerial: Integer;
    { Eight exact paired-text snapshots bound root-review memory. Metadata and
      immutable commands are retained together; no node/session is borrowed. }
    FRootReviews: array of TNyxDataValue;
    FRootRemovals: array of INyxRootRemoval;
    FRootReviewSerial: Integer;
    procedure Changed;
    procedure RequireRevision(const AArguments: TNyxDataValue);
    procedure Log(const AActor, AOperation, AOutcome: TNyxText);
    function Summary: TNyxDataValue;
    function EditorState(AAfter: Integer): TNyxDataValue;
    function Outline(const AArguments: TNyxDataValue): TNyxDataValue;
    function NodeDetails(const AArguments: TNyxDataValue): TNyxDataValue;
    function Components(const AArguments: TNyxDataValue): TNyxDataValue;
    function Diagnostics(const AArguments: TNyxDataValue): TNyxDataValue;
    function SourceLines(const AArguments: TNyxDataValue): TNyxDataValue;
    function CompilerSnapshot: TNyxDataValue;
    function HandlerSource(const AArguments: TNyxDataValue;
      AApply: Boolean): TNyxDataValue;
    function EditCallbacks(const AArguments: TNyxDataValue;
      const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
    function RemoveRoots(const AArguments: TNyxDataValue;
      const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
  public
    constructor Create;
    destructor Destroy; override;
    { Arguments are admitted JSON data at this explicit semantic boundary.
      Results are bounded immutable copies. Rejected operations retain accepted
      pair, editable draft and history. Activity records both success and refusal. }
    function Call(const ATool, AActor: TNyxText;
      const AArguments: TNyxDataValue): TNyxDataValue;
    { Private editor transport, never advertised as an MCP tool. Observations
      send paired files only after a changed revision. Commit is compare-and-swap;
      first attachment may claim recovered local files, later views observe.
      Editor undo/redo uses this authoritative ordinary Studio history. }
    function Exchange(const ARequest: TNyxDataValue): TNyxDataValue;
    { Preview/render workers receive an independent accepted pair under the
      transport lock, then release it before invoking optional external renderers. }
    function PreviewPair(AExpected: Integer; const AView: TNyxText;
      const AActor: TNyxText = 'MCP client'): TNyxProjectPair;
    { Transport-owned work, such as rendering outside the model lock, reports
      completion/refusal through the same bounded operator-visible activity. }
    procedure RecordActivity(const AActor, AOperation, AOutcome: TNyxText);
    { Native job admission captures immutable text, never the mutable session.
      Edit permission, exact revision and absence of a pending draft are required.
      Scope/root agreement is checked before any compiler can be launched. }
    function BuildPair(AExpected: Integer; AScope: TNyxBuildScope;
      const AView: TNyxText): TNyxProjectPair;
    { Exact accepted pair comparison, independent of revision/selection changes.
      Pending drafts make diagnostics stale even if the accepted source matches. }
    function CurrentPair(const APair: TNyxProjectPair): Boolean;
    procedure PublishCompilerReport(const AReport: INyxCompilerReport);
    property Revision: Integer read FRevision;
    property Permission: TNyxAgentPermission read FPermission;
  end;

{ Shared strict field helpers at JSON admission boundaries. Unknown arguments
  are refused so a misspelled field never silently changes command meaning. }
function NyxAgentHas(const AValue: TNyxDataValue; const AKey: TNyxText): Boolean;
procedure NyxAgentFields(const AValue: TNyxDataValue; const AAllowed: TNyxText);
function NyxAgentPermissionName(AValue: TNyxAgentPermission): TNyxText;

implementation

uses
  nyx.catalog, nyx.catalog.labels, nyx.callbacks, nyx.codec, nyx.composition,
  Math, nyx.source, nyx.design.tokens, nyx.studio.edits, nyx.studio.callbackedits,
  nyx.studio.handleredits;

function NyxAgentHas(const AValue: TNyxDataValue; const AKey: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if AValue.Kind <> ndObject then
  begin
    raise ENyxModel.Create('Arguments must be an object');
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    if AValue.Key(LIndex) = AKey then
    begin
      Exit(True);
    end;
  end;
end;

procedure NyxAgentFields(const AValue: TNyxDataValue; const AAllowed: TNyxText);
var
  LIndex: Integer;
begin

  if AValue.Kind <> ndObject then
  begin
    raise ENyxModel.Create('Arguments must be an object');
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    { Pipe delimits this internal closed-key list; it is never part of a key.
      Refuse joined names instead of accepting a substring across two entries. }
    if (Pos('|', AValue.Key(LIndex)) > 0) or
      (Pos('|' + AValue.Key(LIndex) + '|', AAllowed) = 0) then
    begin
      raise ENyxModel.Create('Unknown argument: ' + AValue.Key(LIndex));
    end;
  end;
end;

function NyxAgentPermissionName(AValue: TNyxAgentPermission): TNyxText;
const
  CNames: array[TNyxAgentPermission] of TNyxText = ('disabled', 'readOnly', 'edit');
begin
  Result := CNames[AValue];
end;

function IntegerArgument(const AArgs: TNyxDataValue; const AKey: TNyxText;
  ADefault, AMinimum, AMaximum: Integer): Integer;
begin
  Result := ADefault;

  if NyxAgentHas(AArgs, AKey) then
  begin
    Result := AArgs.Field(AKey).AsInteger;
  end;

  if (Result < AMinimum) or (Result > AMaximum) then
  begin
    raise ENyxModel.Create('Argument outside its published bounds: ' + AKey);
  end;
end;

function TextArgument(const AArgs: TNyxDataValue; const AKey: TNyxText;
  const ADefault: TNyxText = ''): TNyxText;
begin
  Result := ADefault;

  if NyxAgentHas(AArgs, AKey) then
  begin
    Result := AArgs.Field(AKey).AsText;
  end;
end;

function TextSpan(const AText: TNyxText; AOffset, ALimit: Integer;
  out ATotal: Integer): TNyxText;
var
  LIndex: Integer;
  LBefore: Integer;
  LStart: Integer;
  LEnd: Integer;
  LScalar: Integer;
begin
  { Offsets/counts use Unicode scalars on both targets. Return an exact slice,
    never split UTF-8 or UTF-16 and never append decoration to application data. }
  ATotal := 0;
  LIndex := 1;
  LStart := Length(AText) + 1;
  LEnd := LStart;
  while LIndex <= Length(AText) do
  begin
    LBefore := LIndex;

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Malformed Unicode in agent context');
    end;

    if ATotal = AOffset then
    begin
      LStart := LBefore;
    end;

    if ATotal < AOffset + ALimit then
    begin
      LEnd := LIndex;
    end;
    Inc(ATotal);
  end;
  Result := Copy(AText, LStart, LEnd - LStart);
end;

function CaptionText(const AText: TNyxText; ALimit: Integer = 256): TNyxText;
var
  LTotal: Integer;
begin
  Result := TextSpan(AText, 0, ALimit, LTotal);

  if LTotal > ALimit then
  begin
    Result := Result + '…';
  end;
end;

procedure BoundContext(const AValue: TNyxDataValue);
var
  LText: TNyxText;
  LIndex: Integer;
  LScalar: Integer;
  LBytes: Integer;
begin
  LText := AValue.ToJSON;
  LIndex := 1;
  LBytes := 0;
  while LIndex <= Length(LText) do
  begin

    if not NyxNextScalar(LText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Malformed context encoding');
    end;

    if LScalar <= $7f then
    begin
      Inc(LBytes);
    end
    else if LScalar <= $7ff then
    begin
      Inc(LBytes, 2);
    end
    else if LScalar <= $ffff then
    begin
      Inc(LBytes, 3);
    end
    else
    begin
      Inc(LBytes, 4);
    end;

    if LBytes > 48 * 1024 then
    begin
      raise ENyxModel.Create('Context exceeds 48 KiB; request fewer items, property keys or text/source scalars');
    end;
  end;
end;

function Brief(ANode: TNyxNode): TNyxDataValue;
var
  LParent: TNyxText;
begin
  LParent := '';

  if ANode.Parent <> nil then
  begin
    LParent := ANode.Parent.ID;
  end;
  Result := NyxObject([NyxField('id', NyxData(ANode.ID)),
    NyxField('kind', NyxData(ANode.Kind)), NyxField('parent', NyxData(LParent)),
    NyxField('childCount', NyxData(ANode.Count))]);
end;

constructor TNyxAgentSession.Create;
begin
  inherited Create;
  FSession := TNyxStudioSession.Create;
  FRevision := 1;
  { The product owner explicitly requested enabled editing by default. Operator
    controls can reduce or disable access without changing project data. }
  FPermission := apEdit;
end;

destructor TNyxAgentSession.Destroy;
begin
  FReport := nil;
  FSession.Free;
  inherited Destroy;
end;

procedure TNyxAgentSession.Changed;
begin

  if FRevision = High(Integer) then
  begin
    raise ENyxModel.Create('Session revision budget exhausted; reconnect a new session');
  end;
  Inc(FRevision);
  FClaimed := True;
end;

procedure TNyxAgentSession.RequireRevision(const AArguments: TNyxDataValue);
begin

  if AArguments.Field('expectedRevision').AsInteger <> FRevision then
  begin
    raise ENyxModel.Create('Revision conflict; inspect current context before retrying');
  end;

  if FRevision = High(Integer) then
  begin
    raise ENyxModel.Create('Session revision budget exhausted');
  end;
end;

procedure TNyxAgentSession.Log(const AActor, AOperation, AOutcome: TNyxText);
var
  LIndex: Integer;
begin
  Inc(FActivitySerial);

  if Length(FActivity) = 24 then
  begin
    for LIndex := 1 to High(FActivity) do
    begin
      FActivity[LIndex - 1] := FActivity[LIndex];
    end;
    SetLength(FActivity, 23);
  end;
  SetLength(FActivity, Length(FActivity) + 1);
  FActivity[High(FActivity)] := NyxObject([
    NyxField('sequence', NyxData(FActivitySerial)),
    NyxField('actor', NyxData(CaptionText(AActor, 100))), NyxField('operation', NyxData(CaptionText(AOperation, 100))),
    NyxField('outcome', NyxData(CaptionText(AOutcome, 320))), NyxField('revision', NyxData(FRevision))]);
end;

function TNyxAgentSession.Summary: TNyxDataValue;
begin
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('permission', NyxData(NyxAgentPermissionName(FPermission))),
    NyxField('title', NyxData(CaptionText(FSession.Document.Title, 160))),
    NyxField('selection', NyxData(FSession.SelectedID)),
    NyxField('view', NyxData(FSession.ActiveViewID)),
    NyxField('pages', NyxData(FSession.Document.Count)),
    NyxField('components', NyxData(FSession.Document.ComponentCount)),
    NyxField('pendingDraft', NyxData(FSession.DraftSource <> FSession.Source)),
    NyxField('canUndo', NyxData(FSession.CanUndo)),
    NyxField('canRedo', NyxData(FSession.CanRedo)),
    NyxField('activitySequence', NyxData(FActivitySerial))]);
end;

function TNyxAgentSession.EditorState(AAfter: Integer): TNyxDataValue;
var
  LFields: array of TNyxDataField;
begin
  SetLength(LFields, 3);
  LFields[0] := NyxField('session', Summary);
  LFields[1] := NyxField('activity', NyxArray(FActivity));
  LFields[2] := NyxField('compiler', CompilerSnapshot);

  if AAfter <> FRevision then
  begin
    SetLength(LFields, 4);
    LFields[3] := NyxField('project', NyxData(EncodeNyxProject(FSession.ProjectSnapshot)));
  end;
  Result := NyxObject(LFields);
end;

function TNyxAgentSession.Outline(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LParent: TNyxNode;
  LScope: TNyxText;
  LOffset: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LIndex: Integer;
  LCount: Integer;
begin
  NyxAgentFields(AArguments, '|parent|scope|offset|limit|');
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 25, 1, 50);
  LScope := TextArgument(AArguments, 'scope', 'pages');
  LParent := nil;

  if NyxAgentHas(AArguments, 'parent') then
  begin
    LParent := FSession.Document.Find(AArguments.Field('parent').AsText);

    if LParent = nil then
    begin
      raise ENyxModel.Create('Outline parent is missing');
    end;
    LTotal := LParent.Count;
  end
  else if LScope = 'pages' then
  begin
    LTotal := FSession.Document.Count;
  end
  else if LScope = 'components' then
  begin
    LTotal := FSession.Document.ComponentCount;
  end
  else
  begin
    raise ENyxModel.Create('Outline scope must be pages or components');
  end;
  LCount := 0;
  SetLength(LItems, LLimit);
  for LIndex := LOffset to LTotal - 1 do
  begin

    if LCount = LLimit then
    begin
      Break;
    end;

    if LParent <> nil then
    begin
      LItems[LCount] := Brief(LParent.Children[LIndex]);
    end
    else if LScope = 'pages' then
    begin
      LItems[LCount] := Brief(FSession.Document.Pages[LIndex]);
    end
    else
    begin
      LItems[LCount] := Brief(FSession.Document.Components[LIndex]);
    end;
    Inc(LCount);
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
    NyxField('items', NyxArray(LItems))]);
end;

function PropertyValue(const AInfo: TNyxPropertyInfo; const AText: TNyxText): TNyxDataValue;
var
  LInteger: Integer;
begin

  if AText = '' then
  begin
    Exit(NyxNull);
  end;
  case AInfo.ValueType of
    npBoolean: Result := NyxData(AText = 'true');
    npInteger:
      begin

        if not TryNyxInteger(AText, LInteger) then
        begin
          raise ENyxModel.Create('Invalid admitted integer property');
        end;
        Result := NyxData(LInteger);
      end;
    npNumber: Result := NyxData(NyxDecimal(AText));
    else
    begin
      Result := NyxData(AText);
    end;
  end;
end;

function TNyxAgentSession.NodeDetails(const AArguments: TNyxDataValue): TNyxDataValue;
const
  CTypes: array[TNyxPropertyType] of TNyxText = ('string', 'lines', 'boolean',
    'integer', 'number', 'enum', 'reference');
var
  LNode: TNyxNode;
  LInfos: TNyxPropertyInfos;
  LEventContext: TNyxNode;
  LEventProjection: TNyxNode;
  LItems: array of TNyxDataValue;
  LEvents: TNyxEventSchemas;
  LEventItems: array of TNyxDataValue;
  LEventOffset: Integer;
  LEventLimit: Integer;
  LEventCount: Integer;
  LPayload: TNyxDataValue;
  LRouteItems: array of TNyxDataValue;
  LRouteIndex: Integer;
  LRouteOffset: Integer;
  LRouteLimit: Integer;
  LRouteCount: Integer;
  LRouteEventCount: Integer;
  LRouteTotal: Integer;
  LAuthored: TNyxAuthoredEventInfos;
  LRegistrations: TNyxAuthoredEventInfos;
  LAuthoredIndex: Integer;
  LCallbackIndex: Integer;
  LRegistrationIndex: Integer;
  LRegistrationOffset: Integer;
  LRegistrationLimit: Integer;
  LRegistrationCount: Integer;
  LRegistrationTotal: Integer;
  LIndex: Integer;
  LCount: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LFields: array of TNyxDataField;
  LKeys: TNyxDataValue;
  LTextOffset: Integer;
  LTextLimit: Integer;
  LKeyIndex: Integer;
  LMatch: Integer;
  LTotalText: Integer;
  LMatches: Boolean;
  LText: TNyxText;
  LValue: TNyxDataValue;
begin
  NyxAgentFields(AArguments, '|id|offset|limit|events|eventOffset|eventLimit|registrationOffset|registrationLimit|routeOffset|routeLimit|keys|textOffset|textLimit|');
  LNode := FSession.Document.Find(TextArgument(AArguments, 'id', FSession.SelectedID));

  if LNode = nil then
  begin
    raise ENyxModel.Create('Component is missing');
  end;
  LInfos := NyxProperties(LNode, FSession.Document);
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
  LTextOffset := IntegerArgument(AArguments, 'textOffset', 0, 0, 1000000);
  LTextLimit := IntegerArgument(AArguments, 'textLimit', 512, 1, 2048);
  LKeys := NyxArray([]);

  if NyxAgentHas(AArguments, 'keys') then
  begin
    LKeys := AArguments.Field('keys');

    if (LKeys.Kind <> ndArray) or (LKeys.Count > 20) then
    begin
      raise ENyxModel.Create('Property keys require an array of at most 20 exact published names');
    end;
    for LKeyIndex := 0 to LKeys.Count - 1 do
    begin
      LMatches := False;
      for LIndex := 0 to High(LInfos) do
      begin

        if LInfos[LIndex].Key = LKeys.Item(LKeyIndex).AsText then
        begin
          LMatches := True;
        end;
      end;

      if not LMatches then
      begin
        raise ENyxModel.Create('Requested property is not published');
      end;
    end;
  end;
  SetLength(LItems, LLimit);
  LCount := 0;
  LMatch := 0;
  for LIndex := 0 to High(LInfos) do
  begin
    LMatches := LKeys.Count = 0;
    for LKeyIndex := 0 to LKeys.Count - 1 do
    begin

      if LKeys.Item(LKeyIndex).AsText = LInfos[LIndex].Key then
      begin
        LMatches := True;
      end;
    end;

    if not LMatches then
    begin
      Continue;
    end;
    Inc(LMatch);

    if (LMatch <= LOffset) or (LCount = LLimit) then
    begin
      Continue;
    end;
    LText := LNode.Prop(LInfos[LIndex].Key);
    LValue := PropertyValue(LInfos[LIndex], LText);
    LTotalText := 0;

    if (LValue.Kind = ndText) and (LInfos[LIndex].ValueType <> npReference) then
    begin
      LValue := NyxData(TextSpan(LText, LTextOffset, LTextLimit, LTotalText));
    end;

    LItems[LCount] := NyxObject([
      NyxField('key', NyxData(LInfos[LIndex].Key)),
      NyxField('title', NyxData(LInfos[LIndex].Title)),
      NyxField('type', NyxData(CTypes[LInfos[LIndex].ValueType])),
      NyxField('value', LValue),
      NyxField('textOffset', NyxData(LTextOffset)), NyxField('totalScalars', NyxData(LTotalText)),
      NyxField('truncated', NyxData((LValue.Kind = ndText) and (LInfos[LIndex].ValueType <> npReference) and
        ((LTextOffset > 0) or (LTotalText > LTextOffset + LTextLimit)))),
      NyxField('default', NyxData(CaptionText(LInfos[LIndex].DefaultValue, 160))),
      NyxField('choices', NyxData(CaptionText(LInfos[LIndex].Choices, 512))),
      NyxField('meaning', NyxData(NyxPropertyMeaningText(LInfos[LIndex].Support.Meaning))),
      NyxField('browser', NyxData(NyxCapabilityText(LInfos[LIndex].Support.Browser))),
      NyxField('native', NyxData(NyxCapabilityText(LInfos[LIndex].Support.Native))),
      NyxField('help', NyxData(CaptionText(LInfos[LIndex].Support.Description, 256))),
      NyxField('minimum', NyxData(LInfos[LIndex].Minimum)),
      NyxField('maximum', NyxData(LInfos[LIndex].Maximum))]);
    Inc(LCount);
  end;
  SetLength(LItems, LCount);
  SetLength(LFields, 5);
  LFields[0] := NyxField('revision', NyxData(FRevision));
  LFields[1] := NyxField('node', Brief(LNode));
  LFields[2] := NyxField('totalProperties', NyxData(LMatch));
  LFields[3] := NyxField('offset', NyxData(LOffset));
  LFields[4] := NyxField('properties', NyxArray(LItems));

  if NyxAgentHas(AArguments, 'events') and AArguments.Field('events').AsBoolean then
  begin
    LEvents := NyxEventsMetadata(LNode, FSession.Document);
    LEventOffset := IntegerArgument(AArguments, 'eventOffset', 0, 0, 100000);
    LEventLimit := IntegerArgument(AArguments, 'eventLimit', 32, 1, 50);
    LEventCount := 0;
    { Registrations must describe the same realized instance as discovery,
      including inherited callbacks and part overrides. Decode returns values;
      release the temporary owner before constructing the bounded response. }
    LEventContext := RealizeNyxContext(FSession.Document, LNode, LEventProjection);
    try
      LAuthored := nil;

      if LEventProjection <> nil then
      begin
        LAuthored := NyxAuthoredEvents(LEventProjection);
      end;
    finally
      LEventContext.Free;
    end;
    LRegistrations := nil;
    LRegistrationOffset := IntegerArgument(AArguments, 'registrationOffset', 0, 0, 100000);
    LRegistrationLimit := IntegerArgument(AArguments, 'registrationLimit', 16, 1, 50);
    LRegistrationCount := 0;
    LRegistrationTotal := 0;
    LRouteOffset := IntegerArgument(AArguments, 'routeOffset', 0, 0, 100000);
    LRouteLimit := IntegerArgument(AArguments, 'routeLimit', 16, 1, 50);
    LRouteCount := 0;
    LRouteTotal := 0;
    SetLength(LEventItems, LEventLimit);
    for LIndex := LEventOffset to High(LEvents) do
    begin

      if LEventCount >= LEventLimit then
      begin
        Break;
      end;
      LPayload := NyxNull;

      if LEvents[LIndex].Payload.Defined then
      begin
        LPayload := LEvents[LIndex].Payload.ToData;
      end;
      { Page routes across this exact event window, rather than multiplying the
        response limit by every event. Empty pages retain stream identity/count. }
      SetLength(LRouteItems, LRouteLimit);
      LRouteEventCount := 0;
      for LRouteIndex := 0 to High(LEvents[LIndex].Routes) do
      begin

        if (LRouteTotal >= LRouteOffset) and (LRouteCount < LRouteLimit) then
        begin
          LRouteItems[LRouteEventCount] := LEvents[LIndex].Routes[LRouteIndex].ToData;
          Inc(LRouteEventCount);
          Inc(LRouteCount);
        end;
        Inc(LRouteTotal);
      end;
      SetLength(LRouteItems, LRouteEventCount);
      LEventItems[LEventCount] := NyxObject([
        NyxField('trigger', NyxData(NyxTriggerName(LEvents[LIndex].Trigger))),
        NyxField('name', NyxData(LEvents[LIndex].Name.Name)),
        NyxField('title', NyxData(CaptionText(LEvents[LIndex].Title, 160))),
        NyxField('description', NyxData(CaptionText(LEvents[LIndex].Description, 512))),
        NyxField('payload', LPayload),
        NyxField('payloadOptional', NyxData(LEvents[LIndex].PayloadOptional)),
        NyxField('declaredProducer', NyxData(LEvents[LIndex].DeclaredProducer)),
        NyxField('contexts', NyxEventContextsData(LEvents[LIndex].Contexts)),
        NyxField('totalRoutes', NyxData(Length(LEvents[LIndex].Routes))),
        NyxField('routes', NyxArray(LRouteItems)),
        NyxField('browser', NyxData(NyxCapabilityText(LEvents[LIndex].Browser))),
        NyxField('native', NyxData(NyxCapabilityText(LEvents[LIndex].Native)))]);
      Inc(LEventCount);
      { Callback context belongs only to these exact event identities. Page the
        flattened registrations in metadata-event order, retaining callback order
        and empty authored streams' policies. The envelope explicitly marks its
        partial window; it is never presented as a complete saved descriptor. }
      for LAuthoredIndex := 0 to High(LAuthored) do
      begin

        if (LAuthored[LAuthoredIndex].Trigger <> LEvents[LIndex].Trigger) or
          (LAuthored[LAuthoredIndex].Name.Name <> LEvents[LIndex].Name.Name) then
        begin
          Continue;
        end;
        LRegistrationIndex := Length(LRegistrations);
        SetLength(LRegistrations, LRegistrationIndex + 1);
        LRegistrations[LRegistrationIndex].Trigger := LAuthored[LAuthoredIndex].Trigger;
        LRegistrations[LRegistrationIndex].Name := LAuthored[LAuthoredIndex].Name;
        LRegistrations[LRegistrationIndex].Policy := LAuthored[LAuthoredIndex].Policy;
        for LCallbackIndex := 0 to High(LAuthored[LAuthoredIndex].Callbacks) do
        begin

          if (LRegistrationTotal >= LRegistrationOffset) and
            (LRegistrationCount < LRegistrationLimit) then
          begin
            LCount := Length(LRegistrations[LRegistrationIndex].Callbacks);
            SetLength(LRegistrations[LRegistrationIndex].Callbacks, LCount + 1);
            LRegistrations[LRegistrationIndex].Callbacks[LCount] :=
              LAuthored[LAuthoredIndex].Callbacks[LCallbackIndex];
            Inc(LRegistrationCount);
          end;
          Inc(LRegistrationTotal);
        end;
      end;
    end;
    SetLength(LEventItems, LEventCount);
    SetLength(LFields, 15);
    LFields[5] := NyxField('events', NyxArray(LEventItems));
    LFields[6] := NyxField('registrations', EncodeNyxAuthoredEvents(LRegistrations));
    LFields[7] := NyxField('totalEvents', NyxData(Length(LEvents)));
    LFields[8] := NyxField('eventOffset', NyxData(LEventOffset));
    LFields[9] := NyxField('totalRegistrations', NyxData(LRegistrationTotal));
    LFields[10] := NyxField('registrationOffset', NyxData(LRegistrationOffset));
    LFields[11] := NyxField('registrationsPartial', NyxData(
      (Length(LRegistrations) < Length(LAuthored)) or
      (LRegistrationOffset > 0) or (LRegistrationCount < LRegistrationTotal)));
    LFields[12] := NyxField('totalRoutes', NyxData(LRouteTotal));
    LFields[13] := NyxField('routeOffset', NyxData(LRouteOffset));
    LFields[14] := NyxField('routesPartial', NyxData(
      (LRouteOffset > 0) or (LRouteCount < LRouteTotal)));
  end;
  Result := NyxObject(LFields);
end;

function TNyxAgentSession.Components(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LIndex: Integer;
  LMatch: Integer;
  LCount: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LQuery: TNyxText;
  LGroup: TNyxPaletteGroup;
  LInfo: TNyxComponentInfo;
  LItems: array of TNyxDataValue;
begin
  NyxAgentFields(AArguments, '|query|group|offset|limit|');
  LQuery := TextArgument(AArguments, 'query');
  LGroup := pgAll;

  if not TryNyxPaletteGroup(TextArgument(AArguments, 'group', 'all'), LGroup) then
  begin
    raise ENyxModel.Create('Unknown component intent group');
  end;
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 12, 1, 50);
  SetLength(LItems, LLimit);
  LCount := 0;
  LMatch := 0;
  for LIndex := 0 to FSession.Catalog.Count - 1 do
  begin

    if FSession.Catalog.Matches(LIndex, LQuery, LGroup) then
    begin

      if (LMatch >= LOffset) and (LCount < LLimit) then
      begin
        LInfo := FSession.Catalog[LIndex];
        LItems[LCount] := NyxObject([NyxField('kind', NyxData(LInfo.Kind)),
          NyxField('title', NyxData(LInfo.Title)),
          NyxField('group', NyxData(NyxPaletteGroupKey(LInfo.Discovery.Group))),
          NyxField('labels', NyxData(NyxComponentLabelNames(LInfo.Discovery.Labels))),
          NyxField('description', NyxData(CaptionText(LInfo.Discovery.Description, 512))),
          NyxField('container', NyxData(LInfo.Container))]);
        Inc(LCount);
      end;
      Inc(LMatch);
    end;
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('total', NyxData(LMatch)), NyxField('offset', NyxData(LOffset)),
    NyxField('items', NyxArray(LItems))]);
end;

function TNyxAgentSession.Diagnostics(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LOffset: Integer;
  LLimit: Integer;
  LCount: Integer;
  LTotal: Integer;
  LItem: TNyxCompilerDiagnostic;
  LCurrent: Boolean;
  LOrder: TNyxCompilerDiagnosticIndices;
begin
  NyxAgentFields(AArguments, '|offset|limit|');
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 100000);
  LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
  LTotal := 0;
  LCurrent := False;

  if FReport <> nil then
  begin
    LTotal := FReport.Count;
    LCurrent := (FReport.Source = FSession.Source) and (FSession.DraftSource = FSession.Source);
  end;
  SetLength(LItems, LLimit);
  LOrder := NyxCompilerDiagnosticOrder(FReport);
  LCount := 0;
  for LIndex := LOffset to LTotal - 1 do
  begin

    if LCount = LLimit then
    begin
      Break;
    end;
    LItem := FReport.Item(LOrder[LIndex]);
    LItems[LCount] := NyxObject([
      NyxField('file', NyxData(LItem.FileName)),
      NyxField('severity', NyxData(Ord(LItem.Severity))),
      NyxField('message', NyxData(CaptionText(LItem.Message, 1024))),
      NyxField('line', NyxData(LItem.SourceLine)),
      NyxField('column', NyxData(LItem.SourceColumn)),
      NyxField('navigable', NyxData(LCurrent and LItem.Navigable))]);
    Inc(LCount);
  end;
  SetLength(LItems, LCount);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('total', NyxData(LTotal)), NyxField('offset', NyxData(LOffset)),
    NyxField('currentSource', NyxData(LCurrent)), NyxField('order', NyxData('severity')),
    NyxField('items', NyxArray(LItems))]);
end;

function TNyxAgentSession.SourceLines(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LLines: TNyxStrings;
  LItems: array of TNyxDataValue;
  LStart: Integer;
  LCount: Integer;
  LIndex: Integer;
begin
  NyxAgentFields(AArguments, '|line|count|');
  LStart := IntegerArgument(AArguments, 'line', 1, 1, 100000);
  LCount := IntegerArgument(AArguments, 'count', 20, 1, 80);
  LLines := TNyxStrings.Create;
  try
    LLines.Text := FSession.Source;

    if LStart > LLines.Count then
    begin
      raise ENyxModel.Create('Source line does not exist');
    end;

    if LCount > LLines.Count - LStart + 1 then
    begin
      LCount := LLines.Count - LStart + 1;
    end;
    SetLength(LItems, LCount);
    for LIndex := 0 to LCount - 1 do
    begin
      LItems[LIndex] := NyxData(LLines[LStart - 1 + LIndex]);
    end;
    Result := NyxObject([NyxField('revision', NyxData(FRevision)),
      NyxField('line', NyxData(LStart)), NyxField('totalLines', NyxData(LLines.Count)),
      NyxField('lines', NyxArray(LItems))]);
  finally
    LLines.Free;
  end;
end;

function WithResults(const ASummary, AResults: TNyxDataValue;
  const AMember: TNyxText; ABudgetCheck: Boolean = False): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, ASummary.Count + 1);
  for LIndex := 0 to ASummary.Count - 1 do
  begin
    LFields[LIndex] := NyxField(ASummary.Key(LIndex), ASummary.Field(ASummary.Key(LIndex)));

    if ABudgetCheck then
    begin
      { Reserve the largest serialized post-publication revision/history values
        before committing. A successful edit cannot outgrow this preflight. }

      if LFields[LIndex].Name = 'revision' then
      begin
        LFields[LIndex].Value := NyxData(High(Integer));
      end
      else if (LFields[LIndex].Name = 'canUndo') or (LFields[LIndex].Name = 'canRedo') then
      begin
        LFields[LIndex].Value := NyxData(False);
      end;
    end;
  end;
  { High avoids a pas2js record-method-as-array-index ambiguity. }
  LFields[High(LFields)] := NyxField(AMember, AResults);
  Result := NyxObject(LFields);
end;

function TNyxAgentSession.EditCallbacks(const AArguments: TNyxDataValue;
  const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
var
  LPatch: INyxCallbackPatch;
  LPair: TNyxProjectPair;
  LResults: TNyxCallbackEditResults;
  LCallbacks: TNyxDataValue;
  LChanges: TNyxDataValue;
  LReview: TNyxDataValue;
  LReviewID: TNyxText;
  LIndex: Integer;
  LReviewIndex: Integer;
  LRemoves: Boolean;
begin
  NyxAgentFields(AArguments, '|expectedRevision|operationId|mode|changes|reviewID|');
  RequireRevision(AArguments);
  LChanges := AArguments.Field('changes');
  LPatch := ReadNyxCallbackPatch(LChanges);
  LRemoves := False;
  for LIndex := 0 to LChanges.Count - 1 do
  begin
    LRemoves := LRemoves or (LChanges.Item(LIndex).Field('op').AsText = 'remove');
  end;
  LReviewIndex := -1;

  if AApply and LRemoves then
  begin
    LReviewID := TextArgument(AArguments, 'reviewID');
    for LIndex := 0 to High(FCallbackReviews) do
    begin
      LReview := FCallbackReviews[LIndex];

      if (LReview.Field('id').AsText = LReviewID) and
        (LReview.Field('actor').AsText = AActor) and
        (LReview.Field('revision').AsInteger = FRevision) and
        (LReview.Field('changes').ToJSON = LChanges.ToJSON) then
      begin
        LReviewIndex := LIndex;
        Break;
      end;
    end;

    if LReviewIndex < 0 then
    begin
      raise ENyxModel.Create('Removal requires a current reviewID for this actor and exact changes; call nyx_callbacks in review mode first');
    end;
  end
  else if NyxAgentHas(AArguments, 'reviewID') then
  begin
    raise ENyxModel.Create('reviewID applies only to a removal batch in apply mode');
  end;

  if not AApply then
  begin

    if not LRemoves or NyxAgentHas(AArguments, 'operationId') then
    begin
      raise ENyxModel.Create('Review requires a removal batch and has no mutation operationId');
    end;

    if FCallbackReviewSerial = High(Integer) then
    begin
      raise ENyxModel.Create('Callback review budget exhausted');
    end;
  end;

  { All commands run on a detached session, including code generation and
    exact event admission. Failure preserves live pair, draft, selection and
    history. Size refusal also precedes the sole live AdoptProject operation. }
  LPair := LPatch.Candidate(FSession, LResults);
  LCallbacks := EncodeNyxCallbackResults(LResults);
  BoundContext(WithResults(Summary, LCallbacks, 'callbacks', True));

  if AApply then
  begin
    FSession.AdoptProject(LPair);

    if LReviewIndex >= 0 then
    begin
      for LIndex := LReviewIndex + 1 to High(FCallbackReviews) do
      begin
        FCallbackReviews[LIndex - 1] := FCallbackReviews[LIndex];
      end;
      SetLength(FCallbackReviews, Length(FCallbackReviews) - 1);
    end;
    Exit(LCallbacks);
  end;

  LReviewID := 'callback-review-' + IntToStr(FCallbackReviewSerial + 1);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('reviewID', NyxData(LReviewID)), NyxField('callbacks', LCallbacks)]);
  BoundContext(Result);
  Inc(FCallbackReviewSerial);

  if Length(FCallbackReviews) = 16 then
  begin
    for LIndex := 1 to High(FCallbackReviews) do
    begin
      FCallbackReviews[LIndex - 1] := FCallbackReviews[LIndex];
    end;
    SetLength(FCallbackReviews, 15);
  end;
  SetLength(FCallbackReviews, Length(FCallbackReviews) + 1);
  FCallbackReviews[High(FCallbackReviews)] := NyxObject([
    NyxField('id', NyxData(LReviewID)), NyxField('actor', NyxData(AActor)),
    NyxField('revision', NyxData(FRevision)), NyxField('changes', LChanges)]);
end;

function TNyxAgentSession.RemoveRoots(const AArguments: TNyxDataValue;
  const AActor: TNyxText; AApply: Boolean): TNyxDataValue;
var
  LRoots: TNyxDataValue;
  LReview: TNyxDataValue;
  LRemoval: INyxRootRemoval;
  LPair: TNyxProjectPair;
  LReviewID: TNyxText;
  LIndex: Integer;
  LFound: Integer;
  LDocument: TNyxDocument;
  LSummary: TNyxDataValue;
  LFields: array of TNyxDataField;
  LView: TNyxText;
  LSelection: TNyxText;
begin
  NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|roots|reviewID|');
  RequireRevision(AArguments);
  LRoots := AArguments.Field('roots');

  if AApply then
  begin
    LReviewID := TextArgument(AArguments, 'reviewID');
    LFound := -1;
    for LIndex := 0 to High(FRootReviews) do
    begin
      LReview := FRootReviews[LIndex];

      if (LReview.Field('id').AsText = LReviewID) and
        (LReview.Field('actor').AsText = AActor) and
        (LReview.Field('revision').AsInteger = FRevision) and
        (LReview.Field('roots').ToJSON = LRoots.ToJSON) then
      begin
        LFound := LIndex;
        Break;
      end;
    end;

    if LFound < 0 then
    begin
      raise ENyxModel.Create('Root removal requires a current reviewID for this actor and exact roots');
    end;
    LRemoval := FRootRemovals[LFound];
    LPair := LRemoval.Candidate(FSession.ProjectSnapshot);
    Result := LRemoval.Inspect;
    { Response size, dependencies, draft and exact paired text are admitted
      before the only publication. Consume the review only after success. }
    { The surviving view/selection can have longer names than the removed ones.
      Preflight their actual fallback, rather than assume the old summary is an
      upper bound. Match ordinary AdoptProject's page/component/empty order. }
    LDocument := TNyxCodec.Decode(LPair.Design);
    try
      LView := FSession.ActiveViewID;
      LSelection := FSession.SelectedID;

      if LDocument.Find(LView) = nil then
      begin
        LView := '';

        if LDocument.Count > 0 then
        begin
          LView := LDocument.Pages[0].ID;
        end
        else if LDocument.ComponentCount > 0 then
        begin
          LView := LDocument.Components[0].ID;
        end;
      end;

      if LDocument.Find(LSelection) = nil then
      begin
        LSelection := LView;
      end;
      LSummary := Summary;
      SetLength(LFields, LSummary.Count);
      for LIndex := 0 to LSummary.Count - 1 do
      begin
        LFields[LIndex] := NyxField(LSummary.Key(LIndex), LSummary.Field(LSummary.Key(LIndex)));

        if LFields[LIndex].Name = 'view' then
        begin
          LFields[LIndex].Value := NyxData(LView);
        end
        else if LFields[LIndex].Name = 'selection' then
        begin
          LFields[LIndex].Value := NyxData(LSelection);
        end;
      end;
      BoundContext(WithResults(NyxObject(LFields), Result, 'removedRoots', True));
    finally
      LDocument.Free;
    end;
    FSession.AdoptProject(LPair);
    for LIndex := LFound + 1 to High(FRootReviews) do
    begin
      FRootReviews[LIndex - 1] := FRootReviews[LIndex];
      FRootRemovals[LIndex - 1] := FRootRemovals[LIndex];
    end;
    SetLength(FRootReviews, Length(FRootReviews) - 1);
    SetLength(FRootRemovals, Length(FRootRemovals) - 1);
    Exit;
  end;

  if NyxAgentHas(AArguments, 'operationId') or NyxAgentHas(AArguments, 'reviewID') then
  begin
    raise ENyxModel.Create('Root review has no mutation operationId or prior reviewID');
  end;

  if FRootReviewSerial = High(Integer) then
  begin
    raise ENyxModel.Create('Root review budget exhausted');
  end;
  LRemoval := ReadNyxRootRemoval(FSession.ProjectSnapshot, LRoots);
  LReviewID := 'root-review-' + IntToStr(FRootReviewSerial + 1);
  Result := NyxObject([NyxField('revision', NyxData(FRevision)),
    NyxField('reviewID', NyxData(LReviewID)), NyxField('removal', LRemoval.Inspect)]);
  BoundContext(Result);
  Inc(FRootReviewSerial);

  if Length(FRootReviews) = 8 then
  begin
    for LIndex := 1 to High(FRootReviews) do
    begin
      FRootReviews[LIndex - 1] := FRootReviews[LIndex];
      FRootRemovals[LIndex - 1] := FRootRemovals[LIndex];
    end;
    SetLength(FRootReviews, 7);
    SetLength(FRootRemovals, 7);
  end;
  LIndex := Length(FRootReviews);
  SetLength(FRootReviews, LIndex + 1);
  SetLength(FRootRemovals, LIndex + 1);
  FRootReviews[LIndex] := NyxObject([NyxField('id', NyxData(LReviewID)),
    NyxField('actor', NyxData(AActor)), NyxField('revision', NyxData(FRevision)),
    NyxField('roots', LRoots)]);
  FRootRemovals[LIndex] := LRemoval;
end;

function TNyxAgentSession.HandlerSource(const AArguments: TNyxDataValue;
  AApply: Boolean): TNyxDataValue;
var
  LPatch: INyxHandlerPatch;
  LPair: TNyxProjectPair;
  LResults: TNyxHandlerEditResults;
  LHandler: TNyxHandlerSource;
  LOffset: Integer;
  LCount: Integer;
  LTotal: Integer;
  LSignatureTotal: Integer;
  LSignature: TNyxText;
  LText: TNyxText;
begin

  if AApply then
  begin
    NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
    RequireRevision(AArguments);
    LPatch := ReadNyxHandlerPatch(AArguments.Field('changes'));
    LPair := LPatch.Candidate(FSession, LResults);
    Result := EncodeNyxHandlerResults(LResults);
    { Admit the final response before the only active-session publication. The
      detached patch preserves source/design/selection/history on every refusal. }
    BoundContext(WithResults(Summary, Result, 'handlers', True));
    FSession.AdoptProject(LPair);
    Exit;
  end;
  NyxAgentFields(AArguments, '|mode|handler|offset|count|');
  LHandler := ReadNyxHandlerSource(FSession.Source, NyxHandler(TextArgument(AArguments, 'handler')));
  LOffset := IntegerArgument(AArguments, 'offset', 0, 0, 4 * 1024 * 1024);
  LCount := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
  LText := TextSpan(LHandler.Code, LOffset, LCount, LTotal);

  if LOffset > LTotal then
  begin
    raise ENyxModel.Create('Callback text offset is beyond the accepted implementation');
  end;
  LSignature := TextSpan(LHandler.Signature, 0, 1024, LSignatureTotal);
  Result := NyxObject([
    NyxField('revision', NyxData(FRevision)), NyxField('handler', NyxData(LHandler.Handler.Name)),
    NyxField('line', NyxData(LHandler.Line)), NyxField('signature', NyxData(LSignature)),
    NyxField('signatureCharacters', NyxData(LSignatureTotal)),
    NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
    NyxField('nextOffset', NyxData(Min(LOffset + LCount, LTotal))),
    NyxField('text', NyxData(LText)),
    NyxField('pendingDraft', NyxData(FSession.DraftSource <> FSession.Source))]);
end;

function TNyxAgentSession.Call(const ATool, AActor: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LBefore: TNyxText;
  LOperationID: TNyxText;
  LRequest: TNyxText;
  LKey: TNyxText;
  LIndex: Integer;
  LNode: TNyxNode;
  LMutation: Boolean;
  LCallbackApply: Boolean;
  LCallbackResults: TNyxDataValue;
  LHandlerApply: Boolean;
  LHandlerResults: TNyxDataValue;
  LRootApply: Boolean;
  LRootResults: TNyxDataValue;
begin
  LCallbackApply := False;
  LCallbackResults := NyxNull;
  LHandlerApply := False;
  LHandlerResults := NyxNull;
  LRootApply := False;
  LRootResults := NyxNull;

  try

    if FPermission = apDisabled then
    begin
      raise ENyxModel.Create('Agent access is disabled in Studio');
    end;

    if ATool = 'nyx_callbacks' then
    begin
      LCallbackApply := TextArgument(AArguments, 'mode') = 'apply';
    end;

    if ATool = 'nyx_pascal' then
    begin
      LHandlerApply := TextArgument(AArguments, 'mode') = 'apply';
    end;
    if ATool = 'nyx_roots' then
    begin
      LRootApply := TextArgument(AArguments, 'mode') = 'apply';
    end;
    LMutation := (ATool = 'nyx_transaction') or (ATool = 'nyx_select') or
      (ATool = 'nyx_history') or LCallbackApply or LHandlerApply or LRootApply;

    if LMutation and (FPermission <> apEdit) then
    begin
      raise ENyxModel.Create('Agent edits require Allow edits in Studio');
    end;

    if LMutation then
    begin
      LOperationID := AArguments.Field('operationId').AsText;

      if (LOperationID = '') or (Length(LOperationID) > 120) then
      begin
        raise ENyxModel.Create('Mutation operationId must contain 1..120 characters');
      end;
      LKey := NyxObject([NyxField('actor', NyxData(AActor)),
        NyxField('id', NyxData(LOperationID))]).ToJSON;
      LRequest := NyxObject([NyxField('tool', NyxData(ATool)),
        NyxField('arguments', AArguments)]).ToJSON;
      for LIndex := 0 to High(FReceiptKeys) do
      begin

        if FReceiptKeys[LIndex] = LKey then
        begin

          if FReceiptRequests[LIndex] <> LRequest then
          begin
            raise ENyxModel.Create('operationId was already used with different arguments');
          end;
          Log(AActor, ATool, 'retry returned original receipt');
          Exit(FReceiptResults[LIndex]);
        end;
      end;
      RequireRevision(AArguments);
      LBefore := EncodeNyxProject(FSession.ProjectSnapshot);
    end;

    if ATool = 'nyx_session' then
    begin
      NyxAgentFields(AArguments, '|');
      Result := Summary;
    end
    else if ATool = 'nyx_outline' then
    begin
      Result := Outline(AArguments);
    end
    else if ATool = 'nyx_node' then
    begin
      Result := NodeDetails(AArguments);
    end
    else if ATool = 'nyx_components' then
    begin
      Result := Components(AArguments);
    end
    else if ATool = 'nyx_diagnostics' then
    begin
      Result := Diagnostics(AArguments);
    end
    else if ATool = 'nyx_source' then
    begin
      Result := SourceLines(AArguments);
    end
    else if ATool = 'nyx_tokens' then
    begin
      NyxAgentFields(AArguments, '|');
      Result := NyxObject([NyxField('revision', NyxData(FRevision)),
        NyxField('tokens', NyxDesignTokens(FSession.Document))]);
    end
    else if ATool = 'nyx_transaction' then
    begin
      NyxAgentFields(AArguments, '|expectedRevision|operationId|operations|');
      FSession.ApplyPatch(ReadNyxDesignPatch(AArguments.Field('operations')));
    end
    else if ATool = 'nyx_callbacks' then
    begin

      if not LCallbackApply and (TextArgument(AArguments, 'mode') <> 'review') then
      begin
        raise ENyxModel.Create('Callback mode must be review or apply');
      end;
      Result := EditCallbacks(AArguments, AActor, LCallbackApply);
      LCallbackResults := Result;
    end
    else if ATool = 'nyx_pascal' then
    begin

      if not LHandlerApply and (TextArgument(AArguments, 'mode') <> 'inspect') then
      begin
        raise ENyxModel.Create('Pascal mode must be inspect or apply');
      end;
      Result := HandlerSource(AArguments, LHandlerApply);
      LHandlerResults := Result;
    end
    else if ATool = 'nyx_roots' then
    begin

      if not LRootApply and (TextArgument(AArguments, 'mode') <> 'review') then
      begin
        raise ENyxModel.Create('Root mode must be review or apply');
      end;
      Result := RemoveRoots(AArguments, AActor, LRootApply);
      LRootResults := Result;
    end
    else if ATool = 'nyx_select' then
    begin
      NyxAgentFields(AArguments, '|expectedRevision|operationId|id|activate|');
      LNode := FSession.Document.Find(AArguments.Field('id').AsText);

      if LNode = nil then
      begin
        raise ENyxModel.Create('Selection is missing');
      end;

      if NyxAgentHas(AArguments, 'activate') and AArguments.Field('activate').AsBoolean then
      begin

        if LNode.Parent <> nil then
        begin
          raise ENyxModel.Create('Activate requires a page or reusable root');
        end;
        FSession.Activate(LNode.ID);
      end
      else
      begin
        FSession.Select(LNode.ID);
      end;
      Changed;
    end
    else if ATool = 'nyx_history' then
    begin
      NyxAgentFields(AArguments, '|expectedRevision|operationId|direction|');

      if FSession.DraftSource <> FSession.Source then
      begin
        raise ENyxModel.Create('Resolve the pending draft before changing agent history');
      end;

      if AArguments.Field('direction').AsText = 'undo' then
      begin
        FSession.Undo;
      end
      else if AArguments.Field('direction').AsText = 'redo' then
      begin
        FSession.Redo;
      end
      else
      begin
        raise ENyxModel.Create('History direction must be undo or redo');
      end;
    end
    else
    begin
      raise ENyxModel.Create('Unknown Nyx tool: ' + ATool);
    end;

    if LMutation then
    begin

      if (ATool <> 'nyx_select') and
        (LBefore <> EncodeNyxProject(FSession.ProjectSnapshot)) then
      begin
        Changed;
      end;
      Result := Summary;

      if LCallbackApply then
      begin
        Result := WithResults(Result, LCallbackResults, 'callbacks');
      end;

      if LHandlerApply then
      begin
        Result := WithResults(Result, LHandlerResults, 'handlers');
      end;

      if LRootApply then
      begin
        Result := WithResults(Result, LRootResults, 'removedRoots');
      end;

      if Length(FReceiptKeys) = 64 then
      begin
        for LIndex := 1 to High(FReceiptKeys) do
        begin
          FReceiptKeys[LIndex - 1] := FReceiptKeys[LIndex];
          FReceiptRequests[LIndex - 1] := FReceiptRequests[LIndex];
          FReceiptResults[LIndex - 1] := FReceiptResults[LIndex];
        end;
        SetLength(FReceiptKeys, 63);
        SetLength(FReceiptRequests, 63);
        SetLength(FReceiptResults, 63);
      end;
      LIndex := Length(FReceiptKeys);
      SetLength(FReceiptKeys, LIndex + 1);
      SetLength(FReceiptRequests, LIndex + 1);
      SetLength(FReceiptResults, LIndex + 1);
      FReceiptKeys[LIndex] := LKey;
      FReceiptRequests[LIndex] := LRequest;
      FReceiptResults[LIndex] := Result;
    end;
    { Read responses are bounded before returning. Callback results were
      preflighted before publication; ordinary receipts have a small summary.
      No whole-document data is sent through agent queries. }

    if not LMutation then
    begin
      BoundContext(Result);
    end;
    Log(AActor, ATool, 'completed');
  except
    on LException: Exception do
    begin
      Log(AActor, ATool, 'refused: ' + LException.Message);
      raise;
    end;
  end;
end;

function TNyxAgentSession.Exchange(const ARequest: TNyxDataValue): TNyxDataValue;
var
  LOperation: TNyxText;
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
  LSelected: TNyxText;
  LView: TNyxText;
  LBefore: TNyxText;
  LPermission: TNyxText;
  LAfter: Integer;
begin
  LOperation := ARequest.Field('op').AsText;
  LAfter := IntegerArgument(ARequest, 'after', 0, 0, High(Integer));

  if LOperation = 'observe' then
  begin
    NyxAgentFields(ARequest, '|op|after|');
    Exit(EditorState(LAfter));
  end;

  if LOperation = 'configure' then
  begin
    NyxAgentFields(ARequest, '|op|after|permission|');
    LPermission := ARequest.Field('permission').AsText;

    if LPermission = 'disabled' then
    begin
      FPermission := apDisabled;
    end
    else if LPermission = 'readOnly' then
    begin
      FPermission := apReadOnly;
    end
    else if LPermission = 'edit' then
    begin
      FPermission := apEdit;
    end
    else
    begin
      raise ENyxModel.Create('Unknown agent permission');
    end;
    Log('Studio operator', 'agent permissions', LPermission);
    Exit(EditorState(LAfter));
  end;

  if LOperation = 'report' then
  begin
    NyxAgentFields(ARequest, '|op|after|report|');
    PublishCompilerReport(DecodeNyxCompilerReport(ARequest.Field('report').AsText));
    Exit(EditorState(LAfter));
  end;

  if (LOperation = 'claim') or (LOperation = 'commit') then
  begin
    NyxAgentFields(ARequest, '|op|after|expectedRevision|project|selection|view|');

    if (LOperation = 'claim') and FClaimed then
    begin
      Exit(EditorState(0));
    end;

    if LOperation = 'commit' then
    begin
      RequireRevision(ARequest);
    end;
    LPair := DecodeNyxProject(ARequest.Field('project').AsText);
    LSelected := ARequest.Field('selection').AsText;
    LView := ARequest.Field('view').AsText;
    LDocument := TNyxCodec.Decode(LPair.Design);
    try

      if ((LSelected <> '') and (LDocument.Find(LSelected) = nil)) or
        ((LView <> '') and ((LDocument.Find(LView) = nil) or
          (LDocument.Find(LView).Parent <> nil))) then
      begin
        raise ENyxModel.Create('Editor selection/view is outside its candidate document');
      end;
    finally
      LDocument.Free;
    end;
    LBefore := EncodeNyxProject(FSession.ProjectSnapshot);

    if LOperation = 'claim' then
    begin
      FSession.LoadProject(LPair);
    end
    else
    begin
      FSession.AdoptProject(LPair);
    end;

    if LView <> '' then
    begin
      FSession.Activate(LView);
    end;

    if LSelected <> '' then
    begin
      FSession.Select(LSelected);
    end;

    if (LBefore <> EncodeNyxProject(FSession.ProjectSnapshot)) or
      (LOperation = 'commit') then
    begin
      Changed;
    end;
    FClaimed := True;
    Log('Studio', LOperation, 'completed');
  end
  else if LOperation = 'history' then
  begin
    NyxAgentFields(ARequest, '|op|after|expectedRevision|direction|');
    RequireRevision(ARequest);

    if FSession.DraftSource <> FSession.Source then
    begin
      raise ENyxModel.Create('Resolve the pending draft before changing shared history');
    end;
    LBefore := EncodeNyxProject(FSession.ProjectSnapshot);

    if ARequest.Field('direction').AsText = 'undo' then
    begin
      FSession.Undo;
    end
    else if ARequest.Field('direction').AsText = 'redo' then
    begin
      FSession.Redo;
    end
    else
    begin
      raise ENyxModel.Create('History direction must be undo or redo');
    end;

    if LBefore <> EncodeNyxProject(FSession.ProjectSnapshot) then
    begin
      Changed;
    end;
    Log('Studio', 'history', 'completed');
  end
  else
  begin
    raise ENyxModel.Create('Unknown editor exchange');
  end;
  Result := EditorState(0);
end;

procedure TNyxAgentSession.RecordActivity(const AActor, AOperation,
  AOutcome: TNyxText);
begin
  Log(AActor, AOperation, AOutcome);
end;

function TNyxAgentSession.BuildPair(AExpected: Integer; AScope: TNyxBuildScope;
  const AView: TNyxText): TNyxProjectPair;
var
  LNode: TNyxNode;
  LIndex: Integer;
  LFound: Boolean;
begin

  if FPermission <> apEdit then
  begin
    raise ENyxModel.Create('Agent builds require Allow edits in Studio');
  end;

  if AExpected <> FRevision then
  begin
    raise ENyxModel.Create('Build revision conflict');
  end;

  if FSession.DraftSource <> FSession.Source then
  begin
    raise ENyxModel.Create('Resolve the pending draft before building');
  end;

  if AScope = bsApplication then
  begin

    if AView <> '' then
    begin
      raise ENyxModel.Create('Application builds omit view');
    end;
  end
  else
  begin
    LNode := FSession.Document.Find(AView);
    LFound := False;

    if AScope = bsView then
    begin
      for LIndex := 0 to FSession.Document.Count - 1 do
      begin
        LFound := LFound or (FSession.Document.Pages[LIndex] = LNode);
      end;
    end
    else
    begin
      for LIndex := 0 to FSession.Document.ComponentCount - 1 do
      begin
        LFound := LFound or (FSession.Document.Components[LIndex] = LNode);
      end;
    end;

    if (LNode = nil) or not LFound then
    begin
      raise ENyxModel.Create('Build view must match the exact requested page/reusable scope');
    end;
  end;
  Result := FSession.ProjectSnapshot;
end;

function TNyxAgentSession.CurrentPair(const APair: TNyxProjectPair): Boolean;
var
  LCurrent: TNyxProjectPair;
begin
  LCurrent := FSession.ProjectSnapshot;
  Result := not LCurrent.Pending and (LCurrent.Design = APair.Design) and
    (LCurrent.Source = APair.Source);
end;

procedure TNyxAgentSession.PublishCompilerReport(const AReport: INyxCompilerReport);
begin
  FReport := AReport;
  Inc(FCompilerSequence);
end;

function TNyxAgentSession.CompilerSnapshot: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LItem: TNyxCompilerDiagnostic;
  LIndex: Integer;
  LTotal: Integer;
  LCount: Integer;
  LAccepted: Boolean;
  LOrder: TNyxCompilerDiagnosticIndices;
begin
  LTotal := 0;
  LAccepted := False;

  if FReport <> nil then
  begin
    LTotal := FReport.Count;
    LAccepted := FReport.Source = FSession.Source;
  end;
  LCount := LTotal;

  if LCount > 20 then
  begin
    LCount := 20;
  end;
  SetLength(LItems, LCount);
  LOrder := NyxCompilerDiagnosticOrder(FReport);
  for LIndex := 0 to High(LItems) do
  begin
    LItem := FReport.Item(LOrder[LIndex]);
    LItems[LIndex] := NyxObject([
      NyxField('file', NyxData(CaptionText(LItem.FileName, 512))),
      NyxField('severity', NyxData(Ord(LItem.Severity))),
      NyxField('message', NyxData(CaptionText(LItem.Message, 1024))),
      NyxField('line', NyxData(LItem.Line)), NyxField('column', NyxData(LItem.Column)),
      NyxField('sourceLine', NyxData(LItem.SourceLine)),
      NyxField('sourceColumn', NyxData(LItem.SourceColumn))]);
  end;
  { Observers already own accepted source. Send bounded diagnostics and exact
    source-match admission instead of duplicating a potentially large unit. }
  Result := NyxObject([NyxField('sequence', NyxData(FCompilerSequence)),
    NyxField('total', NyxData(LTotal)), NyxField('acceptedSource', NyxData(LAccepted)),
    NyxField('items', NyxArray(LItems))]);
end;

function TNyxAgentSession.PreviewPair(AExpected: Integer;
  const AView, AActor: TNyxText): TNyxProjectPair;
var
  LNode: TNyxNode;
begin

  if FPermission = apDisabled then
  begin
    raise ENyxModel.Create('Agent access is disabled');
  end;

  if AExpected <> FRevision then
  begin
    raise ENyxModel.Create('Preview revision conflict');
  end;
  LNode := FSession.Document.Find(AView);

  if (LNode = nil) or (LNode.Parent <> nil) then
  begin
    raise ENyxModel.Create('Preview requires a page or reusable root');
  end;
  Result := FSession.ProjectSnapshot;
  Log(AActor, 'nyx_preview', 'render requested');
end;

end.
