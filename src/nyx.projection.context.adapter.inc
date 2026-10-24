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
{ Included by each renderer after its private generation helpers. Target types
  remain private; one shared phase machine owns retained context rollback. }
type
  {$ifdef PAS2JS}
  { Exact browser renderer whose generation leases its detached retirement data. }
  TFormContextRenderer = TNyxBrowserRenderer;
  {$else}
  { Exact native renderer; its canonical generation protects borrowed bindings. }
  TFormContextRenderer = TNyxLCLRenderer;
  {$endif}

  { A reversible retained-form preview, never an owner of its renderer/host.
    Its canonical generation lease makes Current safe after Clear/destruction.
    Private old nodes/blueprint and copied physical input survive until commit;
    abandonment restores them, while Install only assigns prepared bookkeeping.
    The context publication owns this port without a reverse ownership cycle. }
  TFormContextTarget = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FOwner: TFormContextRenderer;
    {$ifdef PAS2JS}
    FGenerationLease: IInterface;
    FGeneration: TBrowserViewGeneration;
    {$else}
    FGeneration: INyxLCLBindingGeneration;
    {$endif}
    FEventBefore: INyxCancellationScope;
    FNextRevision: Integer;
    FRoot: TNyxNode;
    FBaseline: TNyxNode;
    FSchema: Integer;
    FForms: TNyxProjectionFormContexts;
    FInputs: TNyxContentFaceStates;
    FBound: Boolean;
    FValuesBefore: TNyxNode;
    FBaselineBefore: TNyxNode;
    FBlueprintBefore: TNyxContentBlueprint;
    FSelectionBefore: TNyxPresentationSelection;
    FRequestedBefore: TNyxPresentationSelection;
    FFrameKnownBefore: Boolean;
    FAdopted: Boolean;
    FFrozen: Boolean;
    FInstalled: Boolean;
    FNotified: Boolean;
    FRetired: Boolean;
    function Current: Boolean;
    procedure DoRetire;
  public
    constructor Create(AOwner: TFormContextRenderer;
      const AForms: TNyxProjectionFormContexts;
      const AInputs: TNyxContentFaceStates; ABound: Boolean);
    destructor Destroy; override;
    procedure AdoptPrevious(var AValues, ABaseline: TNyxNode;
      var ABlueprint: TNyxContentBlueprint);
    procedure Freeze;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

constructor TFormContextTarget.Create(AOwner: TFormContextRenderer;
  const AForms: TNyxProjectionFormContexts; const AInputs: TNyxContentFaceStates;
  ABound: Boolean);
var
  LIndex: Integer;
  LRevision: Integer;
begin
  inherited Create;
  FOwner := AOwner;
  {$ifdef PAS2JS}
  FGenerationLease := AOwner.FGeneration;
  FGeneration := TBrowserViewGeneration(AOwner.FGenerationObject);
  {$else}
  FGeneration := AOwner.FGeneration;
  {$endif}
  FRoot := AOwner.FRoot;
  FBaseline := AOwner.FProjectionBaseline;
  FSchema := NyxSchemaRevision;
  FBound := ABound;
  FSelectionBefore := AOwner.FPresentationSelection;
  FRequestedBefore := AOwner.FContentRequestedSelection;
  FFrameKnownBefore := AOwner.FContentFrameKnown;
  SetLength(FForms, Length(AForms));
  for LIndex := 0 to High(AForms) do
  begin
    FForms[LIndex] := AForms[LIndex];
  end;
  { These records own only copied immutable values, never controls. The caller
    captured them before entering a native physical dispatch frame. }
  FInputs := Copy(AInputs, 0, Length(AInputs));
  FEventBefore := NyxEventGeneration(AOwner.FEvents);
  LRevision := AOwner.FEvents.ViewRevision;
  FNextRevision := 0;

  if LRevision < High(Integer) then
  begin
    FNextRevision := LRevision + 1;
  end;
end;

destructor TFormContextTarget.Destroy;
begin
  try
    DoRetire;
  except
    { An explicit publication retirement reports rollback failure. Destruction
      still releases every independently owned previous snapshot below. }
  end;
  FValuesBefore.Free;
  FBaselineBefore.Free;
  FBlueprintBefore.Free;
  inherited Destroy;
end;

function TFormContextTarget.Current: Boolean;
begin
  {$ifdef PAS2JS}
  Result := (FGeneration <> nil) and FGeneration.Current;
  {$else}
  Result := (FGeneration <> nil) and NativeGenerationCurrent(FGeneration);
  {$endif}
end;

procedure TFormContextTarget.AdoptPrevious(var AValues, ABaseline: TNyxNode;
  var ABlueprint: TNyxContentBlueprint);
begin
  FValuesBefore := AValues;
  AValues := nil;
  FBaselineBefore := ABaseline;
  ABaseline := nil;
  FBlueprintBefore := ABlueprint;
  ABlueprint := nil;
  FBaseline := FOwner.FProjectionBaseline;
  FAdopted := True;
end;

procedure TFormContextTarget.Freeze;
begin
  FBaseline := FOwner.FProjectionBaseline;
  FFrozen := True;
  FOwner.FUpdating := True;
end;

procedure TFormContextTarget.Validate;
begin
  CheckReady;
end;

procedure TFormContextTarget.CheckReady;
begin

  if FRetired or FInstalled or not Current then
  begin
    raise ENyxModel.Create('Retained form generation is no longer current');
  end;

  if (FEventBefore = nil) or FEventBefore.Cancelled or
    (FOwner.FRoot <> FRoot) or (FOwner.FProjectionBaseline <> FBaseline) or
    (FOwner.FUpdating <> FFrozen) or (NyxSchemaRevision <> FSchema) or
    (FOwner.FProjectionSchemaRevision <> FSchema) then
  begin
    raise ENyxModel.Create('Retained form context changed before publication');
  end;
end;

procedure TFormContextTarget.Install;
{$ifdef PAS2JS}
var
  LIndex: Integer;
{$endif}
begin
  { The final fence precedes every install; this phase cannot call out, allocate
    or release the previous model. Existing browser bridges adopt the prepared
    revision so new physical input remains eligible after scope retirement. }
  FInstalled := True;
  {$ifdef PAS2JS}
  for LIndex := 0 to High(FOwner.FBindings) do
  begin
    FOwner.FBindings[LIndex].FViewRevision := FNextRevision;
  end;
  {$endif}
  FOwner.FUpdating := False;
end;

procedure TFormContextTarget.Notify;
var
  LLease: INyxPreparedPublication;
begin
  LLease := Self;

  if FNotified or not FInstalled then
  begin
    Exit;
  end;
  FNotified := True;

  if Current then
  begin
    { Content evaluation belongs after all section/event owners are installed.
      Scheduling may retire this renderer; no borrowed field is read afterward. }
    FOwner.QueueContent;
  end;
end;

procedure TFormContextTarget.DoRetire;
var
  LRestored: Boolean;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  try

    if FAdopted and not FInstalled and Current then
    begin
      FOwner.FUpdating := False;

      if FBound then
      begin
        LRestored := RefreshNyxBoundFormProjectionProperties(FOwner.FRoot,
          FValuesBefore, nil, FForms);
      end
      else
      begin
        LRestored := RefreshNyxFormProjectionProperties(FOwner.FRoot,
          FValuesBefore, nil, FForms);
      end;

      if not LRestored then
      begin
        raise ENyxModel.Create('Retained form rollback lost its admitted shape');
      end;
      ReleaseNyxNode(FOwner.FProjectionBaseline);
      FOwner.FProjectionBaseline := FBaselineBefore;
      FBaselineBefore := nil;
      FreeAndNil(FOwner.FContentBlueprint);
      FOwner.FContentBlueprint := FBlueprintBefore;
      FBlueprintBefore := nil;
      FOwner.FPresentationSelection := FSelectionBefore;
      FOwner.FContentRequestedSelection := FRequestedBefore;
      FOwner.FContentFrameKnown := FFrameKnownBefore;
      FOwner.Sync;

      if Current then
      begin
        { This port already owns the original input witness and event fence.
          Native preparation may still hold an entered physical frame, so the
          public idle-only RestoreInteraction admission is deliberately bypassed.
          The private restore checks its canonical generation after callouts. }
        FOwner.RestoreContentFaces(FInputs, True);
      end;
    end
    else if FFrozen and not FInstalled and Current then
    begin
      FOwner.FUpdating := False;
    end;
  finally
    FOwner := nil;
  end;
end;

procedure TFormContextTarget.Retire;
var
  LLease: INyxPreparedPublication;
begin
  LLease := Self;
  DoRetire;
end;

{$ifdef PAS2JS}
function TNyxBrowserRenderer.PrepareFormRefreshCore(ADocument: TNyxDocument;
{$else}
function TNyxLCLRenderer.PrepareFormRefreshCore(ADocument: TNyxDocument;
{$endif}
  ARoot: TNyxNode; const AForms: TNyxProjectionFormContexts;
  ADesignMode: Boolean): INyxPreparedPublication;
var
  {$ifdef PAS2JS}
  LGeneration: TBrowserViewGeneration;
  LGenerationLease: IInterface;
  {$else}
  LGeneration: INyxLCLBindingGeneration;
  LPhysicalFrame: INyxNativeGestureFrame;
  {$endif}
  LForms: TNyxProjectionFormContexts;
  LInputs: TNyxContentFaceStates;
  LRestores: TNyxProjectionValueRestores;
  LCandidate: TNyxNode;
  LProjectedCandidate: TNyxNode;
  LProjectedBaseline: TNyxNode;
  LAccepted: TNyxNode;
  LBaseline: TNyxNode;
  LPrevious: TNyxNode;
  LPreviousBaseline: TNyxNode;
  LPreviousBlueprint: TNyxContentBlueprint;
  LBlueprint: TNyxContentBlueprint;
  LTheme: TNyxTheme;
  LFrame: TNyxViewFrame;
  LMeasurements: INyxContainerSnapshot;
  LSelection: TNyxPresentationSelection;
  LTarget: TFormContextTarget;
  LTargetLease: INyxPreparedPublication;
  LPublication: INyxPreparedPublication;
  LBound: Boolean;
  LContextChanged: Boolean;
  LTransferred: Boolean;
  LFailed: Boolean;
  LIndex: Integer;
  LField: Integer;

  procedure RequireCurrent;
  begin
    {$ifdef PAS2JS}

    if not LGeneration.Current then
    begin
      raise ENyxBrowserViewRetired.Create('Form preview browser generation retired');
    end;
    {$else}
    RequireNativeGeneration(LGeneration);
    {$endif}
  end;

  function Compatible(AFrom, ATo: TNyxNode): Boolean;
  begin

    if LBound then
    begin
      Result := CanRefreshNyxBoundFormProjection(AFrom, ATo, LForms);
    end
    else
    begin
      Result := CanRefreshNyxFormProjection(AFrom, ATo, LForms);
    end;
  end;

begin
  Result := nil;
  {$ifdef PAS2JS}
  LGeneration := TBrowserViewGeneration(FGenerationObject);
  LGenerationLease := FGeneration;

  if (LGeneration = nil) or not LGeneration.Current then
  begin
    Exit;
  end;
  {$else}
  LGeneration := FGeneration;

  if (LGeneration = nil) or not NativeGenerationCurrent(LGeneration) then
  begin
    Exit;
  end;
  {$endif}
  { Copy policy before any extension getter. This operation never appends to a
    caller-owned vector, nor keeps a borrowed document after returning. }
  SetLength(LForms, Length(AForms));
  for LIndex := 0 to High(AForms) do
  begin
    LForms[LIndex] := AForms[LIndex];
  end;

  if (Length(LForms) = 0) or not SectionPublicationReady then
  begin
    Exit;
  end;
  RequireCurrent;

  if (FDesignMode <> ADesignMode) or (FProjectionBaseline = nil) or
    (Length(FCoordinatorMounts) <> 0) or (FCoordinatorRegistry <> nil) or
    (FInheritedCoordinatorSnapshot <> nil) or (FCallerCoordinatorSnapshot <> nil) or
    (FProjectionSchemaRevision <> NyxSchemaRevision) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FBindings) do
  begin

    if FBindings[LIndex].FCustom or (FactoryIndex(FBindings[LIndex].FNode) >= 0) then
    begin
      Exit;
    end;
    RequireCurrent;
  end;
  { Capture physical drafts before entering native protection. The public
    continuity API correctly refuses capture from an entered physical dispatch. }
  LInputs := CaptureContentFaces;
  RequireCurrent;
  LCandidate := nil;
  LProjectedCandidate := nil;
  LProjectedBaseline := nil;
  LPrevious := nil;
  LPreviousBaseline := nil;
  LPreviousBlueprint := nil;
  LBlueprint := nil;
  LTheme := nil;
  LTransferred := False;
  LFailed := False;
  {$ifndef PAS2JS}
  LPhysicalFrame := FPhysicalFrame;
  LPhysicalFrame.Enter;
  try
  {$endif}
  try
    try
      ValidateNyxDocumentProperties(ADocument);
      RequireCurrent;

      if NyxProjectionContext(ADocument) <> FProjectionContext then
      begin
        Exit;
      end;
      RequireCurrent;

      if FBorrowedTheme <> nil then
      begin
        LTheme := NewNyxDocumentTheme(ADocument, FBorrowedTheme);
      end
      else
      begin
        LTheme := NewNyxDocumentTheme(ADocument, FBaseTheme);
      end;
      RequireCurrent;

      if LTheme.CSS <> FTheme.CSS then
      begin
        Exit;
      end;
      {$ifdef PAS2JS}
      LFrame := TNyxViewFrame.At(FHost.clientWidth, FHost.clientHeight, npfBrowser);
      {$else}
      LFrame := TNyxViewFrame.At(Max(0, FPanel.Parent.ClientWidth),
        Max(0, FPanel.Parent.ClientHeight), npfNativeLCL);
      {$endif}
      LSelection := FPresentationSelection.Reconciled(ADocument.Presentations.Snapshot);

      if LSelection.Reference.Defined then
      begin
        LFrame := LFrame.Selecting(LSelection.Reference);
      end;
      LMeasurements := MeasureContainers;
      RequireCurrent;
      LCandidate := RealizeNyxView(ADocument, ARoot, LFrame, LMeasurements);
      RequireCurrent;

      if FResourceContext <> nil then
      begin
        LCandidate.BindResources(FResourceContext.Snapshot,
          FResourceContext.Locale, FResourceContext.Fallback);
        RequireCurrent;
      end;
      LBlueprint := NewNyxContentBlueprint(ADocument, ARoot);
      ApplyNyxPlatform(LCandidate, {$ifdef PAS2JS}npfBrowser{$else}npfNativeLCL{$endif});
      LBound := (FLiveBindings <> nil) and FLiveBindings.HasBindings;
      LAccepted := LCandidate;
      LBaseline := FProjectionBaseline;

      if LBound then
      begin

        if not FLiveBindings.TryPrepareRefresh(LCandidate, FProjectionBaseline,
          LProjectedCandidate, LProjectedBaseline) then
        begin
          RequireCurrent;
          Exit;
        end;
        RequireCurrent;
        LAccepted := LProjectedCandidate;
        LBaseline := LProjectedBaseline;
      end;

      if not Compatible(FRoot, LAccepted) or not Compatible(LBaseline, LAccepted) or
        not PrepareNyxProjectionFormRestores(LBaseline, LAccepted, LForms,
          LRestores, LContextChanged) or not LContextChanged then
      begin
        Exit;
      end;
      LPrevious := FRoot.Clone;
      LTarget := TFormContextTarget.Create(Self, LForms, LInputs, LBound);
      LTargetLease := LTarget;
      LPublication := TryPrepareNyxProjectionContextPublication(FEvents, LTargetLease);
      RequireCurrent;

      if LPublication = nil then
      begin
        Exit;
      end;
      { Move old owners without releasing them. QueueContent sees no blueprint
        during Sync, so provisional form source cannot schedule a presentation.
        The target restores these exact owners on every uninstalled exit. }
      LPreviousBaseline := FProjectionBaseline;
      FProjectionBaseline := LCandidate;
      LCandidate := nil;
      LPreviousBlueprint := FContentBlueprint;
      FContentBlueprint := nil;
      LTarget.AdoptPrevious(LPrevious, LPreviousBaseline, LPreviousBlueprint);

      if LBound then
      begin
        LContextChanged := RefreshNyxBoundFormProjectionProperties(FRoot,
          LAccepted, LBaseline, LForms);
      end
      else
      begin
        LContextChanged := RefreshNyxFormProjectionProperties(FRoot,
          LAccepted, LBaseline, LForms);
      end;

      if not LContextChanged then
      begin
        raise ENyxModel.Create('Admitted form projection changed before copying');
      end;
      for LField := 0 to High(LRestores) do
      begin
        for LIndex := 0 to High(FBindings) do
        begin

          if FBindings[LIndex].FNode.ID = LRestores[LField].RuntimeID then
          begin
            FBindings[LIndex].FHasValueBaseline := False;
            Break;
          end;
        end;
      end;
      FPresentationSelection := FPresentationSelection.Reconciled(FRoot.PresentationSnapshot);
      Sync;
      RequireCurrent;
      FContentBlueprint := LBlueprint;
      LBlueprint := nil;
      FContentFrameKnown := False;
      FContentRequestedSelection := FContentRequestedSelection.Reconciled(FRoot.PresentationSnapshot);
      LTarget.Freeze;
      LTarget.CheckReady;
      Result := LPublication;
      LTransferred := True;
    except
      LFailed := True;
      raise;
    end;
  finally
    try

      if not LTransferred and (LPublication <> nil) then
      begin
        try
          LPublication.Retire;
        except
          { Cleanup must still finish while the old event fence is held, but
            neither a Pascal exception nor raw platform value may replace the
            original realization/widget failure already being propagated. }

          if not LFailed then
          begin
            raise;
          end;
        end;
      end;
    finally
      LPublication := nil;
      LTargetLease := nil;
      LTheme.Free;
      LBlueprint.Free;
      LPreviousBlueprint.Free;
      ReleaseNyxNode(LPreviousBaseline);
      ReleaseNyxNode(LPrevious);
      ReleaseNyxNode(LProjectedBaseline);
      ReleaseNyxNode(LProjectedCandidate);
      ReleaseNyxNode(LCandidate);
    end;
  end;
  {$ifndef PAS2JS}
  finally
    LPhysicalFrame.Leave;
  end;
  {$endif}
end;

{$ifdef PAS2JS}
function TNyxBrowserRenderer.TryPrepareFormRefresh(ADocument: TNyxDocument;
{$else}
function TNyxLCLRenderer.TryPrepareFormRefresh(ADocument: TNyxDocument;
{$endif}
  ARoot: TNyxNode; const AForms: TNyxProjectionFormContexts;
  ADesignMode: Boolean): INyxPreparedPublication;
var
  LPrepared: INyxPreparedPublication;
begin
  { The core's managed result is this independent local. Only after all entry
    flags/admission/cleanup are decided may assignment retire the caller's old
    preview. No borrowed receiver field is accessed after that final release. }
  LPrepared := PrepareFormRefreshCore(ADocument, ARoot, AForms, ADesignMode);
  Result := LPrepared;
end;
