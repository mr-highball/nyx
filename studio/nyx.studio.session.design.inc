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

{ Private design-command preparation/publication for nyx.studio.session.
  Only immutable tickets cross processors. Existing session methods remain the
  single authoring implementation; a worker invokes them on independent owners. }

type
  TPreparedDesign = class(TInterfacedObject, INyxPreparedDesign, INyxSchemaAction)
  private
    FRequest: TNyxStudioDesignRequest;
    FDocument: TNyxDocument;
    FWorkspace: TNyxSourceWorkspace;
    FSource: TNyxText;
    FDesign: TNyxText;
    FSnapshot: TNyxText;
    FDiagnostic: TNyxSourceDiagnostic;
    FSelection: TNyxText;
    FView: TNyxText;
    FNextID: Integer;
  public
    destructor Destroy; override;
    procedure Execute;
    function Matches(const ARequest: TNyxStudioDesignRequest): Boolean;
    function GetSource: TNyxText;
    function GetDesign: TNyxText;
    function GetDiagnostic: TNyxSourceDiagnostic;
    function GetSchemaRevision: Integer;
    function GetSelection: TNyxText;
    function GetView: TNyxText;
    function GetNextID: Integer;
    function ToData: TNyxDataValue;
    procedure Take(var ADocument: TNyxDocument; var AWorkspace: TNyxSourceWorkspace);
  end;

function TNyxStudioPendingDesign.PropertyValue(const ASelection, AKey: TNyxText;
  out AValue: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  AValue := '';
  for LIndex := High(Fields) downto 0 do
  begin

    if (Fields[LIndex].Selection = ASelection) and (Fields[LIndex].Key = AKey) then
    begin
      AValue := Fields[LIndex].Value;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxStudioPendingDesign.ApplyCanvasValues(ARoot: TNyxNode;
  const AView: TNyxText): Boolean;
var
  LIndex: Integer;
  LField: TNyxNode;
begin
  Result := False;

  if ARoot = nil then
  begin
    Exit;
  end;

  if not ARoot.IsRealized then
  begin
    raise ENyxModel.Create('Pending canvas values require an independent realized presentation');
  end;
  for LIndex := 0 to High(CanvasValues) do
  begin

    if CanvasValues[LIndex].View <> AView then
    begin
      Continue;
    end;
    LField := ARoot.Find(CanvasValues[LIndex].RuntimeID);

    if (LField <> nil) and (LField.DesignID = CanvasValues[LIndex].Owner) and
      (LField.Prop(NyxAttributeName(atValue)) <> CanvasValues[LIndex].Value) then
    begin
      { This is unadmitted adapter text, not a typed authored default. Numeric
        partial input must remain visible until its queued admission completes. }
      LField.SetProp(NyxAttributeName(atValue), CanvasValues[LIndex].Value);
      Result := True;
    end;
  end;
end;

function TNyxStudioDesignRequest.ToData: TNyxDataValue;
begin
  Result := NyxObject([
    NyxField('version', NyxData(2)), NyxField('owner', NyxData(FOwner)),
    NyxField('generation', NyxData(FGeneration)), NyxField('nextId', NyxData(FNextID)),
    NyxField('schemaRevision', NyxData(FSchemaRevision)),
    NyxField('pair', NyxData(EncodeNyxProject(FPair))),
    NyxField('edit', NyxObject([
      NyxField('action', NyxData(Ord(FEdit.Action))),
      NyxField('selection', NyxData(FEdit.Selection)), NyxField('view', NyxData(FEdit.View)),
      NyxField('name', NyxData(FEdit.Name)), NyxField('value', NyxData(FEdit.Value)),
      NyxField('direction', NyxData(Ord(FEdit.Direction))),
      NyxField('platform', NyxData(Ord(FEdit.Platform)))]))]);
end;

function TNyxStudioDesignRequest.SameRequest(const AOther: TNyxStudioDesignRequest): Boolean;
begin
  { Compare values directly natively; JSON encoding is only the browser worker
    boundary, never an extra full-project encoding during ordinary UI delivery. }
  Result := (FOwner = AOther.FOwner) and (FGeneration = AOther.FGeneration) and
    (FNextID = AOther.FNextID) and (FSchemaRevision = AOther.FSchemaRevision) and
    (FPair.Design = AOther.FPair.Design) and (FPair.Source = AOther.FPair.Source) and
    (FPair.Pending = AOther.FPair.Pending) and (FPair.Draft = AOther.FPair.Draft) and
    (FPair.DraftBase = AOther.FPair.DraftBase) and (FEdit.Action = AOther.FEdit.Action) and
    (FEdit.Selection = AOther.FEdit.Selection) and (FEdit.View = AOther.FEdit.View) and
    (FEdit.Name = AOther.FEdit.Name) and (FEdit.Value = AOther.FEdit.Value) and
    (FEdit.Direction = AOther.FEdit.Direction) and (FEdit.Platform = AOther.FEdit.Platform);
end;

function ReadNyxStudioDesignRequest(const AData: TNyxDataValue): TNyxStudioDesignRequest;
var
  LEdit: TNyxDataValue;
  LAction: Integer;
  LDirection: Integer;
  LPlatform: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 7) or
    (AData.Field('version').AsInteger <> 2) then
  begin
    raise ENyxModel.Create('Design processor requires its exact versioned ticket');
  end;
  Result := Default(TNyxStudioDesignRequest);
  Result.FOwner := AData.Field('owner').AsText;
  Result.FGeneration := AData.Field('generation').AsInteger;
  Result.FNextID := AData.Field('nextId').AsInteger;
  Result.FSchemaRevision := AData.Field('schemaRevision').AsInteger;
  Result.FPair := DecodeNyxProject(AData.Field('pair').AsText);
  LEdit := AData.Field('edit');

  if (Result.FOwner = '') or (Result.FGeneration < 0) or (Result.FNextID < 0) or
    (Result.FSchemaRevision < 0) or (LEdit.Kind <> ndObject) or (LEdit.Count <> 7) then
  begin
    raise ENyxModel.Create('Design processor ticket has invalid identity or intent');
  end;
  LAction := LEdit.Field('action').AsInteger;

  if (LAction < Ord(Low(TNyxStudioDesignAction))) or
    (LAction > Ord(High(TNyxStudioDesignAction))) then
  begin
    raise ENyxModel.Create('Design processor action is outside its closed contract');
  end;
  Result.FEdit.Action := TNyxStudioDesignAction(LAction);
  Result.FEdit.Selection := LEdit.Field('selection').AsText;
  Result.FEdit.View := LEdit.Field('view').AsText;
  Result.FEdit.Name := LEdit.Field('name').AsText;
  Result.FEdit.Value := LEdit.Field('value').AsText;
  LDirection := LEdit.Field('direction').AsInteger;

  if (LDirection < Ord(Low(TNyxStudioMoveDirection))) or
    (LDirection > Ord(High(TNyxStudioMoveDirection))) then
  begin
    raise ENyxModel.Create('Design processor move is outside its closed contract');
  end;
  Result.FEdit.Direction := TNyxStudioMoveDirection(LDirection);
  LPlatform := LEdit.Field('platform').AsInteger;

  if (LPlatform < Ord(Low(TNyxPlatform))) or (LPlatform > Ord(High(TNyxPlatform))) then
  begin
    raise ENyxModel.Create('Design processor platform is outside its closed contract');
  end;
  Result.FEdit.Platform := TNyxPlatform(LPlatform);
  Result.FEdit.FCanvasContext.FOwner := Result.FOwner;
  Result.FEdit.FCanvasContext.FGeneration := Result.FGeneration;

  if ((Result.FEdit.Action = sdaCanvasValue) and (Result.FEdit.Platform = npfAny)) or
    ((Result.FEdit.Action <> sdaCanvasValue) and (Result.FEdit.Platform <> npfAny)) then
  begin
    raise ENyxModel.Create('Only canvas intent selects a concrete replay platform');
  end;
end;

destructor TPreparedDesign.Destroy;
begin
  FWorkspace.Free;
  FDocument.Free;
  inherited Destroy;
end;

procedure TPreparedDesign.Execute;
var
  LSession: TNyxStudioSession;
begin
  { Strict paired admission also verifies handwritten frame ownership. It runs
    entirely on the isolated processor, under its captured creator environment.
    The cost is deliberately kept off the UI rather than trusting a mutable
    accepted session or weakening file/source admission. }
  LSession := TNyxStudioSession.Create(FRequest.FPair);
  try
    LSession.FNextID := FRequest.FNextID;

    if FRequest.FEdit.View <> '' then
    begin
      LSession.Activate(FRequest.FEdit.View);
    end;

    if FRequest.FEdit.Selection <> '' then
    begin
      LSession.Select(FRequest.FEdit.Selection);
    end;
    case FRequest.FEdit.Action of
      sdaProperty: LSession.SetProperty(FRequest.FEdit.Name, FRequest.FEdit.Value);
      sdaAddKind: LSession.AddKind(FRequest.FEdit.Name);
      sdaDelete: LSession.DeleteSelected;
      sdaDuplicate: LSession.DuplicateSelected;
      sdaMove:
        begin

          if FRequest.FEdit.Direction = nmdPrevious then
          begin
            LSession.MoveSelected(-1);
          end
          else
          begin
            LSession.MoveSelected(1);
          end;
        end;
      sdaTitle: LSession.SetTitle(FRequest.FEdit.Value);
      sdaAddPage: LSession.AddPage;
      sdaCreateComponent: LSession.CreateComponent;
      sdaAddInstance: LSession.AddComponentInstance(FRequest.FEdit.Name);
      sdaCustomizePart: LSession.CustomizePart(FRequest.FEdit.Name);
      sdaCanvasValue: LSession.SetCapturedCanvasValue(FRequest.FEdit);
    end;
    FSource := LSession.Source;
    FDesign := LSession.FSourceWorkspace.Capture.Design;
    FSnapshot := LSession.FSourceWorkspace.Snapshot;
    FSelection := LSession.SelectedID;
    FView := LSession.ActiveViewID;
    FNextID := LSession.FNextID;
    FDocument := LSession.FDocument;
    LSession.FDocument := nil;
    FWorkspace := LSession.FSourceWorkspace;
    LSession.FSourceWorkspace := nil;
  finally
    LSession.Free;
  end;
end;

function TPreparedDesign.Matches(const ARequest: TNyxStudioDesignRequest): Boolean;
begin
  Result := FRequest.SameRequest(ARequest);
end;

function TPreparedDesign.GetSource: TNyxText;
begin
  Result := FSource;
end;

function TPreparedDesign.GetDesign: TNyxText;
begin
  Result := FDesign;
end;

function TPreparedDesign.GetDiagnostic: TNyxSourceDiagnostic;
begin
  Result := FDiagnostic;
end;

function TPreparedDesign.GetSchemaRevision: Integer;
begin
  Result := FRequest.SchemaRevision;
end;

function TPreparedDesign.GetSelection: TNyxText;
begin
  Result := FSelection;
end;

function TPreparedDesign.GetView: TNyxText;
begin
  Result := FView;
end;

function TPreparedDesign.GetNextID: Integer;
begin
  Result := FNextID;
end;

function TPreparedDesign.ToData: TNyxDataValue;
begin
  Result := NyxObject([
    NyxField('version', NyxData(1)), NyxField('request', FRequest.ToData),
    NyxField('selection', NyxData(FSelection)), NyxField('view', NyxData(FView)),
    NyxField('nextId', NyxData(FNextID)),
    NyxField('pair', NyxObject([
      NyxField('version', NyxData(1)), NyxField('source', NyxData(FSource)),
      NyxField('design', NyxData(FDesign)), NyxField('workspace', NyxData(FSnapshot)),
      NyxField('schemaRevision', NyxData(GetSchemaRevision)),
      NyxField('diagnostic', NyxObject([
        NyxField('defined', NyxData(FDiagnostic.Defined)),
        NyxField('message', NyxData(FDiagnostic.Message)),
        NyxField('line', NyxData(FDiagnostic.Line)),
        NyxField('column', NyxData(FDiagnostic.Column))]))]))]);
end;

procedure TPreparedDesign.Take(var ADocument: TNyxDocument;
  var AWorkspace: TNyxSourceWorkspace);
begin

  if (ADocument <> nil) or (AWorkspace <> nil) or FDiagnostic.Defined or
    (FDocument = nil) or (FWorkspace = nil) then
  begin
    raise ENyxModel.Create('Design transfer requires empty destinations and one admitted pair');
  end;
  ADocument := FDocument;
  AWorkspace := FWorkspace;
  FDocument := nil;
  FWorkspace := nil;
end;

function PrepareNyxStudioDesign(const ARequest: TNyxStudioDesignRequest;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedDesign;
var
  LOwner: TPreparedDesign;
  LAction: INyxSchemaAction;
begin

  if (ASchemas = nil) or (ASchemas.Revision <> ARequest.SchemaRevision) then
  begin
    raise ENyxModel.Create('Design preparation requires its captured creator environment');
  end;
  LOwner := TPreparedDesign.Create;
  Result := LOwner;
  LOwner.FRequest := ARequest;
  LAction := LOwner;
  try
    ASchemas.Execute(LAction);
  except
    on LException: Exception do
    begin
      LOwner.FDiagnostic.Defined := True;
      LOwner.FDiagnostic.Message := LException.Message;

      if LException is ENyxSource then
      begin
        LOwner.FDiagnostic.Message := ENyxSource(LException).DiagnosticText;
        LOwner.FDiagnostic.Line := ENyxSource(LException).Line;
        LOwner.FDiagnostic.Column := ENyxSource(LException).Column;
      end;
      FreeAndNil(LOwner.FDocument);
      FreeAndNil(LOwner.FWorkspace);
      LOwner.FSource := '';
      LOwner.FDesign := '';
      LOwner.FSnapshot := '';
    end;
  end;
end;

function ReceiveNyxPreparedDesign(const AData: TNyxDataValue;
  const ARequest: TNyxStudioDesignRequest;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedDesign;
var
  LOwner: TPreparedDesign;
  LPair: TNyxDataValue;
  LPrepared: INyxPreparedSource;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 6) or
    (AData.Field('version').AsInteger <> 1) or
    not ReadNyxStudioDesignRequest(AData.Field('request')).SameRequest(ARequest) then
  begin
    raise ENyxModel.Create('Design worker reply differs from its dispatched ticket');
  end;
  LPair := AData.Field('pair');
  LPrepared := ReceiveNyxPreparedSource(LPair, LPair.Field('source').AsText, ASchemas);
  LOwner := TPreparedDesign.Create;
  Result := LOwner;
  LOwner.FRequest := ARequest;
  LOwner.FSource := LPrepared.Source;
  LOwner.FDesign := LPrepared.Design;
  LOwner.FDiagnostic := LPrepared.Diagnostic;
  LOwner.FSnapshot := LPair.Field('workspace').AsText;
  LOwner.FSelection := AData.Field('selection').AsText;
  LOwner.FView := AData.Field('view').AsText;
  LOwner.FNextID := AData.Field('nextId').AsInteger;

  if not LPrepared.Diagnostic.Defined then
  begin
    LPrepared.Take(LOwner.FDocument, LOwner.FWorkspace);

    if (LOwner.FNextID < ARequest.FNextID) or
      ((LOwner.FSelection <> '') and (LOwner.FDocument.Find(LOwner.FSelection) = nil)) or
      ((LOwner.FView <> '') and (LOwner.FDocument.Find(LOwner.FView) = nil)) then
    begin
      raise ENyxModel.Create('Design worker reply has invalid presentation identities');
    end;
  end;
end;

function TNyxStudioSession.CommandContext: TNyxStudioCommandContext;
begin
  Result := Default(TNyxStudioCommandContext);
  Result.FOwner := FSourceIdentity;
  Result.FGeneration := FSourceGeneration;
end;

function TNyxStudioSession.MatchesCommandContext(
  const AContext: TNyxStudioCommandContext): Boolean;
begin
  Result := (AContext.FOwner = FSourceIdentity) and
    (AContext.FGeneration = FSourceGeneration);
end;

function TNyxStudioSession.PrepareDesignRequest(const AEdit: TNyxStudioDesignEdit;
  ASchemaRevision: Integer): TNyxStudioDesignRequest;
begin

  if (AEdit.Action = sdaCanvasValue) and not MatchesCommandContext(AEdit.CanvasContext) then
  begin
    raise ENyxModel.Create('Captured canvas intent belongs to an earlier session or project load');
  end;
  Result := Default(TNyxStudioDesignRequest);
  Result.FOwner := FSourceIdentity;
  Result.FGeneration := FSourceGeneration;
  Result.FNextID := FNextID;
  Result.FSchemaRevision := ASchemaRevision;
  Result.FPair := ProjectSnapshot;
  Result.FEdit := AEdit;
end;

function TNyxStudioSession.CompleteDesignRequest(const ARequest: TNyxStudioDesignRequest;
  const APrepared: INyxPreparedDesign): TNyxSourceCompletion;
var
  LCurrent: TNyxProjectPair;
  LCheckpoint: TNyxSourceCheckpoint;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPublication: TSourcePairPublication;
  LAction: INyxSchemaAction;
  LFollowSelection: Boolean;
  LFollowView: Boolean;
  {$ifdef NYX_STUDIO_PROFILE}
  LPhaseStarted: QWord;
  {$endif}
begin
  Result := nscStale;

  if (ARequest.FOwner <> FSourceIdentity) or
    (ARequest.FGeneration <> FSourceGeneration) or
    (ARequest.SchemaRevision <> NyxSchemaRevision) then
  begin
    Exit;
  end;
  {$ifdef NYX_STUDIO_PROFILE}LPhaseStarted := GetTickCount64;{$endif}
  LCurrent := ProjectSnapshot;
  {$ifdef NYX_STUDIO_PROFILE}
  WriteLn('studio-ui,command-current-snapshot,', GetTickCount64 - LPhaseStarted);
  LPhaseStarted := GetTickCount64;
  {$endif}
  LCheckpoint := FSourceWorkspace.Capture;
  {$ifdef NYX_STUDIO_PROFILE}
  WriteLn('studio-ui,command-capture,', GetTickCount64 - LPhaseStarted);
  {$endif}

  if (LCurrent.Design <> ARequest.FPair.Design) or
    (LCurrent.Source <> ARequest.FPair.Source) or
    (LCurrent.Pending <> ARequest.FPair.Pending) or
    (LCurrent.Draft <> ARequest.FPair.Draft) or
    (LCurrent.DraftBase <> ARequest.FPair.DraftBase) then
  begin
    Exit;
  end;

  if (APrepared = nil) or not APrepared.Matches(ARequest) or
    (APrepared.SchemaRevision <> ARequest.SchemaRevision) then
  begin
    raise ENyxModel.Create('Design completion does not match its captured command');
  end;

  if APrepared.Diagnostic.Defined then
  begin
    Exit(nscRejected);
  end;

  if (APrepared.Design = LCurrent.Design) and (APrepared.Source = LCurrent.Source) then
  begin
    Exit(nscUnchanged);
  end;
  LCandidate := nil;
  LWorkspace := nil;
  LFollowSelection := FSelectedID = ARequest.FEdit.Selection;
  LFollowView := FActiveViewID = ARequest.FEdit.View;
  try
    APrepared.Take(LCandidate, LWorkspace);
    LPublication := TSourcePairPublication.Create;
    LAction := LPublication;
    LPublication.Session := Self;
    LPublication.Checkpoint := LCheckpoint;
    LPublication.Document := LCandidate;
    LPublication.Workspace := LWorkspace;
    LCandidate := nil;
    LWorkspace := nil;

    {$ifdef NYX_STUDIO_PROFILE}LPhaseStarted := GetTickCount64;{$endif}
    if not CommitNyxSchemaRevision(ARequest.SchemaRevision, LAction) then
    begin
      Exit;
    end;
    {$ifdef NYX_STUDIO_PROFILE}
    WriteLn('studio-ui,command-publish,', GetTickCount64 - LPhaseStarted);
    {$endif}

    if APrepared.NextID > FNextID then
    begin
      FNextID := APrepared.NextID;
    end;
    { Preserve a pending handwritten draft with its ORIGINAL base. The new
      design/source is admitted together; a divergent draft stays visibly stale
      instead of being discarded or silently rebased onto generated text. }

    if LFollowView then
    begin
      FActiveViewID := APrepared.View;
    end;

    if LFollowSelection then
    begin
      FSelectedID := APrepared.Selection;
    end;

    if FDocument.Find(FSelectedID) = nil then
    begin
      FSelectedID := FActiveViewID;
    end;
    Result := nscApplied;
  finally
    LCandidate.Free;
    LWorkspace.Free;
  end;
end;
