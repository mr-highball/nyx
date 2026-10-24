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

{ Private implementation of the optional table-column facet. This include
  shares TView's existing reservation/notification owner, not its own data or
  subscription engine. All entry values are copied before outward interfaces
  are queried; final readiness and Install inspect canonical fields only. }

constructor TColumnCapture.Create(const AIdentity: IInterface; ASerial: Integer;
  const ASpec: TNyxCollectionViewSpec; const APreferences: TNyxColumnPreferences);
begin
  inherited Create;
  FIdentity := AIdentity;
  FSerial := ASerial;
  FSpec := ASpec.Copy;
  FPreferences := APreferences.Copy;
end;

function TColumnCapture.GetSpec: TNyxCollectionViewSpec;
begin
  Result := FSpec.Copy;
end;

function TColumnCapture.GetPreferences: TNyxColumnPreferences;
begin
  Result := FPreferences.Copy;
end;

function TColumnCapture.GetSerial: Integer;
begin
  Result := FSerial;
end;

function TColumnCapture.ColumnIdentity: IInterface;
begin
  Result := FIdentity;
end;

function TColumnCapture.ColumnSerial: Integer;
begin
  Result := FSerial;
end;

function NyxCollectionColumns(const AView: INyxCollectionView): INyxCollectionColumns;
var
  LColumns: INyxCollectionColumns;
begin
  LColumns := nil;

  if (AView = nil) or not Supports(AView, INyxCollectionColumns, LColumns) then
  begin
    raise ENyxCollection.Create('Collection view does not support column arrangement');
  end;

  if AView.Projection <> cpTable then
  begin
    raise ENyxCollection.Create('Column arrangement requires a table projection');
  end;
  Result := LColumns;
end;

function TView.CaptureColumns: INyxCollectionColumnsCapture;
begin

  if FProjection <> cpTable then
  begin
    raise ENyxCollection.Create('Column arrangement requires a table projection');
  end;
  Result := TColumnCapture.Create(FColumnIdentity, FColumnSerial,
    FSpec, FColumnPreferences);
end;

function ColumnItemText(const AItem: TNyxCollectionItem;
  const AColumn: TNyxCollectionColumn): TNyxText;
var
  LKind: TNyxStateKind;
begin
  { This is the same canonical scalar display used by CellText. Sampling reads
    retained page items, never a newer live source after a measurement callback. }
  LKind := AColumn.Kind;

  if (Ord(LKind) < Ord(nskText)) or (Ord(LKind) > Ord(nskNumber)) then
  begin
    raise ENyxCollection.Create('Column sample has an unknown scalar family');
  end;
  case LKind of
    nskText: Result := AItem.GetValue(NyxTextField(AColumn.FieldName));
    nskBoolean:
      begin
        Result := 'false';

        if AItem.GetValue(NyxBooleanField(AColumn.FieldName)) then
        begin
          Result := 'true';
        end;
      end;
    nskInteger: Result := IntToStr(AItem.GetValue(NyxIntegerField(AColumn.FieldName)));
    nskNumber: Result := TNyxStateValue.FromNumber(
      AItem.GetValue(NyxNumberField(AColumn.FieldName))).NumberText;
  end;
end;

function TColumnSample.GetColumn: TNyxCollectionColumn;
begin
  Result := FColumn.Copy;
end;

function TColumnSample.GetCount: Integer;
begin
  Result := Length(FTexts);
end;

function TColumnSample.ItemAt(AIndex: Integer): TNyxItemRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FItems)) then
  begin
    raise ENyxCollection.Create('Column sample item index is outside its bounded page');
  end;
  Result := FItems[AIndex];
end;

function TColumnSample.TextAt(AIndex: Integer): TNyxText;
begin

  if (AIndex < 0) or (AIndex >= Length(FTexts)) then
  begin
    raise ENyxCollection.Create('Column sample text index is outside its bounded page');
  end;
  Result := FTexts[AIndex];
end;

function TColumnSample.SampleData: TColumnSample;
begin
  Result := Self;
end;

procedure TColumnSample.RequireCurrent(AOwner: TView);
begin
  { Pure final facts. No snapshot/query/interface getter, formatter, UI or
    arbitrary application code can run between these tests and installation. }

  if (AOwner = nil) or (AOwner.FProjection <> cpTable) or AOwner.FNotifying or
    (AOwner.FPrepared <> nil) or (AOwner.FQueryPreparation <> nil) or
    (AOwner.FColumnIdentity <> FIdentity) or
    (AOwner.FColumnSerial <> FColumnSerial) or
    (AOwner.FColumnSampleSerial = High(Integer)) or
    (AOwner.FColumnSampleSerial <> FSampleSerial) or
    (AOwner.FQueryIntent <> FQueryIntent) or
    (AOwner.FSource <> FSource) or (AOwner.FSnapshot <> FSnapshot) or
    (AOwner.FUnpaged <> FUnpaged) then
  begin
    raise ENyxCollection.Create('Measured column sample belongs to a changed table page');
  end;
end;

function TView.CaptureColumnSample(const ACapture: INyxCollectionColumnsCapture;
  const AField: TNyxColumnIdentity): INyxCollectionColumnSample;
var
  LKeepAlive: INyxCollectionView;
  LOriginal: INyxCollectionColumnsCapture;
  LProof: IColumnCaptureProof;
  LProofIdentity: IInterface;
  LProofSerial: Integer;
  LSample: TColumnSample;
  LSampleLease: INyxCollectionColumnSample;
  LItem: TNyxCollectionItem;
  LField: TNyxColumnIdentity;
  LCanonical, LCount, LIndex: Integer;

  procedure RequireCaptureCurrent;
  begin
    LSample.RequireCurrent(Self);

    if FColumnPreparation <> nil then
    begin
      raise ENyxCollection.Create('Column sampling cannot overlap a column preparation');
    end;
  end;

begin
  { Own const caller slots and immutable entry facts BEFORE Writable/foreign QI.
    A callback cannot retarget the original launch or bless a newer page. }
  LKeepAlive := Self as INyxCollectionView;
  LOriginal := ACapture;
  LField := AField;
  LField.Validate;
  LCanonical := FSpec.ColumnIndex(LField);

  if (FProjection <> cpTable) or (LCanonical < 0) or
    not FSpec.HasLayoutControls or
    not FSpec.TableLayoutPolicy.Allows(nclfAutoSize) or
    not FSpec.ColumnAt(LCanonical).LayoutPolicy.Allows(nclfAutoSize) then
  begin
    raise ENyxCollection.Create('Column sampling requires an enabled typed AutoSize field');
  end;
  LSample := TColumnSample.Create;
  LSampleLease := LSample;
  LSample.FIdentity := FColumnIdentity;
  LSample.FColumnSerial := FColumnSerial;
  LSample.FSampleSerial := FColumnSampleSerial;
  LSample.FQueryIntent := FQueryIntent;
  LSample.FSource := FSource;
  LSample.FSnapshot := FSnapshot;
  LSample.FUnpaged := FUnpaged;
  LSample.FPreferences := FColumnPreferences.Copy;
  LSample.FColumn := FSpec.ColumnAt(LCanonical);
  Writable;
  RequireCaptureCurrent;
  LProof := nil;

  if (LOriginal = nil) or not Supports(LOriginal, IColumnCaptureProof, LProof) then
  begin
    raise ENyxCollection.Create('Column sampling requires its exact launch capture');
  end;
  RequireCaptureCurrent;
  LProofIdentity := LProof.ColumnIdentity;
  RequireCaptureCurrent;
  LProofSerial := LProof.ColumnSerial;
  RequireCaptureCurrent;

  if (LProofIdentity <> LSample.FIdentity) or
    (LProofSerial <> LSample.FColumnSerial) then
  begin
    raise ENyxCollection.Create('Column sample launch is foreign or its layout has changed');
  end;
  { Retain a canonical copy of the ADMITTED launch proof, not a foreign facade
    whose own fields could retain UI or call out during later preparation.
    Admission above still refuses an older/foreign original; this does not
    recapture authority after a stale launch or expose a public proof factory. }
  LSample.FOriginal := TColumnCapture.Create(LSample.FIdentity,
    LSample.FColumnSerial, FSpec, LSample.FPreferences);
  LCount := LSample.FSnapshot.Count;
  RequireCaptureCurrent;

  if (LCount < 0) or (LCount > NyxMaximumCollectionItems) then
  begin
    raise ENyxCollection.Create('Column sample page count is invalid');
  end;
  LCount := Min(LCount, FSpec.TableLayoutPolicy.AutoSizeRows);
  SetLength(LSample.FTexts, LCount);
  SetLength(LSample.FItems, LCount);
  for LIndex := 0 to LCount - 1 do
  begin
    LItem := LSample.FSnapshot.ItemAt(LIndex);
    RequireCaptureCurrent;
    LSample.FItems[LIndex] := LItem.Ref;
    LSample.FTexts[LIndex] := ColumnItemText(LItem, LSample.FColumn);
  end;
  RequireCaptureCurrent;
  Result := LSampleLease;
end;

function TView.PrepareSampleAutoSize(const ASample: INyxCollectionColumnSample;
  AWidth: Integer): INyxPreparedPublication;
var
  LKeepAlive: INyxCollectionView;
  LSample: INyxCollectionColumnSample;
  LProof: IColumnSampleProof;
  LData: TColumnSample;
  LPreparation: TColumnPreparation;
begin
  LKeepAlive := Self as INyxCollectionView;
  LSample := ASample;
  LProof := nil;

  if (LSample = nil) or not Supports(LSample, IColumnSampleProof, LProof) then
  begin
    raise ENyxCollection.Create('Measured AutoSize requires a canonical page sample');
  end;
  LData := LProof.SampleData;

  if LData = nil then
  begin
    raise ENyxCollection.Create('Measured AutoSize sample has no canonical payload');
  end;
  LData.RequireCurrent(Self);
  LPreparation := TColumnPreparation.Create;
  Result := LPreparation;
  LPreparation.InitializeSample(Self, LData, AWidth);
end;

function TView.PrepareColumns(const ACapture: INyxCollectionColumnsCapture;
  const APreferences: TNyxColumnPreferences): INyxPreparedPublication;
var
  LPreparation: TColumnPreparation;
begin
  LPreparation := TColumnPreparation.Create;
  Result := LPreparation;
  LPreparation.Initialize(Self, ACapture, APreferences,
    Default(TNyxColumnIdentity));
end;

function TView.PrepareAutoSize(const ACapture: INyxCollectionColumnsCapture;
  const AField: TNyxColumnIdentity; AWidth: Integer): INyxPreparedPublication;
var
  LPreparation: TColumnPreparation;
  LPreferences: TNyxColumnPreferences;
  LPreference: TNyxColumnPreference;
  LIndex: Integer;
begin
  { Do not read the public capture getter: a foreign implementation may call
    application code. Initialize checks its private proof after reserving us.
    Build from current canonical preferences; that proof still rejects stale
    measurements, including another table's capture with an equal serial. }
  AField.Validate;
  LPreferences := FColumnPreferences.Copy;
  LIndex := LPreferences.IndexOf(AField);
  LPreference := NyxColumnPreference(AField);

  if LIndex >= 0 then
  begin
    LPreference := LPreferences.ColumnAt(LIndex);
  end;
  LPreferences := LPreferences.WithColumn(LPreference.Width(AWidth));
  LPreparation := TColumnPreparation.Create;
  Result := LPreparation;
  LPreparation.Initialize(Self, ACapture, LPreferences, AField);
end;

function TView.ConfigureColumns(
  const APreferences: TNyxColumnPreferences): INyxCollectionColumns;
var
  LPrepared: INyxPreparedPublication;
begin
  Result := Self as INyxCollectionColumns;
  LPrepared := PrepareColumns(CaptureColumns, APreferences);
  PublishNyxGroup([LPrepared]);
end;

function TView.PrepareColumnImport(const ACapture: INyxCollectionColumnsCapture;
  const APreferences: TNyxColumnPreferences): INyxPreparedPublication;
var
  LPreparation: TColumnPreparation;
begin
  LPreparation := TColumnPreparation.Create;
  Result := LPreparation;
  LPreparation.Initialize(Self, ACapture, APreferences,
    Default(TNyxColumnIdentity), True);
end;

procedure TColumnPreparation.Initialize(AOwner: TView;
  const ACapture: INyxCollectionColumnsCapture;
  const APreferences: TNyxColumnPreferences;
  const AAutoSize: TNyxColumnIdentity; AImported: Boolean);
var
  LCapture: INyxCollectionColumnsCapture;
  LProof: IColumnCaptureProof;
begin
  FLease := AOwner as INyxCollectionView;
  FOwner := AOwner;
  FNext := APreferences.Copy;
  FNext.Validate;
  FAutoSize := AAutoSize;
  FImported := AImported;
  LCapture := ACapture;
  FOwner.Writable;

  if FOwner.FProjection <> cpTable then
  begin
    raise ENyxCollection.Create('Column arrangement requires a table projection');
  end;
  FBefore := FOwner.FColumnPreferences.Copy;
  FSource := FOwner.FSource;
  FSnapshot := FOwner.FSnapshot;
  FSelection := FOwner.FSelection;
  FSerial := FOwner.FColumnSerial;
  FOwner.FColumnPreparation := Self;
  LProof := nil;

  if (LCapture = nil) or not Supports(LCapture, IColumnCaptureProof, LProof) then
  begin
    raise ENyxCollection.Create('Prepared columns require their exact table capture');
  end;

  if (LProof.ColumnIdentity <> FOwner.FColumnIdentity) or
    (LProof.ColumnSerial <> FSerial) then
  begin
    raise ENyxCollection.Create('Column capture belongs to another or changed table');
  end;
  LProof := nil;
  LCapture := nil;
  RequireCurrent;
  FChanged := FBefore.ToData.ToJSON <> FNext.ToData.ToJSON;

  if FChanged and (FSerial = High(Integer)) then
  begin
    raise ENyxCollection.Create('Column arrangement serial is exhausted');
  end;
end;

procedure TColumnPreparation.InitializeSample(AOwner: TView;
  ASample: TColumnSample; AWidth: Integer);
var
  LPreferences: TNyxColumnPreferences;
  LPreference: TNyxColumnPreference;
  LIndex: Integer;
begin
  { Install exact immutable measurement facts BEFORE Initialize can call an
    alternate store Busy or original capture getter. Its ordinary RequireCurrent
    then checks this proof at every existing validation/final readiness fence. }
  FSampleLease := ASample;
  FSample := ASample;
  FSample.RequireCurrent(AOwner);
  LPreferences := FSample.FPreferences.Copy;
  LPreference := NyxColumnPreference(FSample.FColumn.Identity);
  LIndex := LPreferences.IndexOf(LPreference.Field);

  if LIndex >= 0 then
  begin
    LPreference := LPreferences.ColumnAt(LIndex);
  end;
  LPreferences := LPreferences.WithColumn(LPreference.Width(AWidth));
  Initialize(AOwner, FSample.FOriginal, LPreferences, FSample.FColumn.Identity);
end;

destructor TColumnPreparation.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TColumnPreparation.RequireCurrent;
begin

  if FRetired or (FOwner = nil) then
  begin
    raise ENyxCollection.Create('Prepared columns have retired');
  end;

  if (FOwner.FColumnPreparation <> Self) or FOwner.FNotifying or
    (FOwner.FQueryPreparation <> nil) or (FOwner.FPrepared <> nil) or
    (FOwner.FColumnSerial <> FSerial) or (FOwner.FSource <> FSource) or
    (FOwner.FSnapshot <> FSnapshot) or (FOwner.FSelection <> FSelection) then
  begin
    raise ENyxCollection.Create('Prepared column target or publication has changed');
  end;

  if FSample <> nil then
  begin
    FSample.RequireCurrent(FOwner);
  end;
end;

procedure TColumnPreparation.Validate;
var
  LSpec: TNyxCollectionViewSpec;
  LColumn: TNyxCollectionColumn;
  LBefore, LNext: TNyxColumnPreference;
  LTable: TNyxTableLayoutPolicy;
  LPreviousOrder, LNextOrder: TNyxCollectionColumnOrder;
  LPreviousPosition, LNextPosition: array of Integer;
  LIndex, LField, LPrevious, LBarrier, LPeer: Integer;
  LFeature: TNyxColumnLayoutFeature;

  procedure RequireFeature(AFeature: TNyxColumnLayoutFeature);
  begin

    if not FImported and (not LSpec.HasLayoutControls or
      not LTable.Allows(AFeature) or not LColumn.LayoutPolicy.Allows(AFeature)) then
    begin
      raise ENyxCollection.Create('Column layout command is disabled for ' + LColumn.FieldName);
    end;
  end;

begin
  RequireCurrent;

  if FValidated then
  begin
    Exit;
  end;

  if FValidationStarted then
  begin
    raise ENyxCollection.Create('A refused column preparation cannot be validated again');
  end;
  FValidationStarted := True;
  LSpec := FOwner.FSpec.Copy;
  LTable := LSpec.TableLayoutPolicy;
  FNext.Validate;

  if not LSpec.HasLayoutControls and
    ((FNext.Count > 0) or (FNext.OrderCount > 0)) then
  begin
    raise ENyxCollection.Create('Runtime column choices require authored layout opt-in');
  end;
  for LIndex := 0 to FNext.Count - 1 do
  begin
    LNext := FNext.ColumnAt(LIndex);
    LField := LSpec.ColumnIndex(LNext.Field);

    if LField < 0 then
    begin
      raise ENyxCollection.Create('Column preference contains a missing or changed-family field');
    end;
    LColumn := LSpec.ColumnAt(LField);
    LPrevious := FBefore.IndexOf(LNext.Field);
    LBefore := NyxColumnPreference(LNext.Field);

    if LPrevious >= 0 then
    begin
      LBefore := FBefore.ColumnAt(LPrevious);
    end;

    if LNext.HasWidth then
    begin

      if LColumn.Sizing.Defined and
        ((LNext.WidthValue < LColumn.Sizing.Bounds.MinimumValue) or
          (LNext.WidthValue > LColumn.Sizing.Bounds.MaximumValue)) then
      begin
        raise ENyxCollection.Create('Column width is outside the current authored bounds');
      end;

      if not LBefore.HasWidth or (LBefore.WidthValue <> LNext.WidthValue) then
      begin
        LFeature := nclfResize;

        if FAutoSize.Defined and FAutoSize.Same(LNext.Field) then
        begin
          LFeature := nclfAutoSize;
        end;
        RequireFeature(LFeature);
      end;
    end;

    if LNext.HasVisibility and
      (not LBefore.HasVisibility or (LBefore.IsVisible <> LNext.IsVisible)) then
    begin
      RequireFeature(nclfVisibility);
    end;

    if LNext.HasPin and (not LBefore.HasPin or (LBefore.Pin <> LNext.Pin)) then
    begin
      RequireFeature(nclfPinning);
    end;
  end;
  { Reset/removal needs no capability. For a nonempty requested order, fixed
    columns are barriers in the COMPLETE sequence, including hidden columns.
    Keeping a barrier's ordinal alone would wrongly allow peers to cross it. }
  LPreviousOrder := LSpec.ResolveColumnOrder(FBefore);
  LNextOrder := LSpec.ResolveColumnOrder(FNext);
  SetLength(LPreviousPosition, LSpec.Count);
  SetLength(LNextPosition, LSpec.Count);
  for LIndex := 0 to LSpec.Count - 1 do
  begin
    LPreviousPosition[LPreviousOrder[LIndex]] := LIndex;
    LNextPosition[LNextOrder[LIndex]] := LIndex;
  end;

  if not FImported and (FNext.OrderCount > 0) then
  begin
    for LBarrier := 0 to LSpec.Count - 1 do
    begin
      LColumn := LSpec.ColumnAt(LBarrier);

      if not LSpec.HasLayoutControls or not LTable.Allows(nclfReorder) or
        not LColumn.LayoutPolicy.Allows(nclfReorder) then
      begin
        for LPeer := 0 to LSpec.Count - 1 do
        begin

          if (LNextPosition[LBarrier] <> LPreviousPosition[LBarrier]) or
            ((LNextPosition[LPeer] < LNextPosition[LBarrier]) <>
              (LPreviousPosition[LPeer] < LPreviousPosition[LBarrier])) then
          begin
            raise ENyxCollection.Create('Column order crosses a field with reordering disabled');
          end;
        end;
      end;
    end;
  end;
  RequireCurrent;
  FValidated := True;
end;

procedure TColumnPreparation.CheckReady;
begin
  RequireCurrent;

  if not FValidated or FInstalled then
  begin
    raise ENyxCollection.Create('Prepared columns require a validated candidate');
  end;
end;

procedure TColumnPreparation.Install;
begin
  { Both vectors are privately retained on this plan. Assigning their managed
    arrays cannot release a final external receiver during pure installation. }

  if FValidated and not FInstalled and not FRetired and (FOwner <> nil) then
  begin

    if FChanged then
    begin
      FOwner.FColumnPreferences := FNext;
      FOwner.FColumnSerial := FSerial + 1;
    end;
    FInstalled := True;
  end;
end;

procedure TColumnPreparation.Notify;
begin

  if not FInstalled or FRetired or FNotified then
  begin
    Exit;
  end;
  FNotified := True;

  if FChanged then
  begin
    FOwner.Notify(nil);
  end;
end;

procedure TColumnPreparation.Retire;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;

  if (FOwner <> nil) and (FOwner.FColumnPreparation = Self) then
  begin
    FOwner.FColumnPreparation := nil;
  end;
  FOwner := nil;
  FSample := nil;
  FSampleLease := nil;
  FSource := nil;
  FSnapshot := nil;
  FSelection := nil;
  FBefore := Default(TNyxColumnPreferences);
  FNext := Default(TNyxColumnPreferences);
  FLease := nil;
end;

function PrepareNyxCollectionColumnTransfer(const AFormer,
  ANext: INyxCollectionView): INyxPreparedPublication;
var
  LFormer, LNext: INyxCollectionView;
  LFormerColumns, LNextColumns: INyxCollectionColumns;
  LSource, LTarget: INyxCollectionColumnsCapture;
  LSpec: TNyxCollectionViewSpec;
  LPreferences, LAccepted: TNyxColumnPreferences;
  LPreference: TNyxColumnPreference;
  LOrder: array of TNyxColumnIdentity;
  LColumn: TNyxCollectionColumn;
  LIndex, LField, LCount: Integer;
  LImport: IColumnTransferTarget;
begin
  Result := nil;
  LFormer := AFormer;
  LNext := ANext;

  if (LFormer = nil) or (LNext = nil) or (LFormer.Store <> LNext.Store) or
    (LFormer.Projection <> cpTable) or (LNext.Projection <> cpTable) or
    not Supports(LFormer, INyxCollectionColumns, LFormerColumns) or
    not Supports(LNext, INyxCollectionColumns, LNextColumns) then
  begin
    Exit;
  end;
  LSource := LFormerColumns.CaptureColumns;
  LTarget := LNextColumns.CaptureColumns;
  LSpec := LTarget.Spec;

  if not LSpec.HasLayoutControls then
  begin
    Exit;
  end;
  LPreferences := LSource.Preferences;
  LAccepted := NyxColumnPreferences;
  for LIndex := 0 to LPreferences.Count - 1 do
  begin
    LPreference := LPreferences.ColumnAt(LIndex);
    LField := LSpec.ColumnIndex(LPreference.Field);

    if LField < 0 then
    begin
      Continue;
    end;
    LColumn := LSpec.ColumnAt(LField);

    if LPreference.HasWidth and LColumn.Sizing.Defined and
      ((LPreference.WidthValue < LColumn.Sizing.Bounds.MinimumValue) or
        (LPreference.WidthValue > LColumn.Sizing.Bounds.MaximumValue)) then
    begin
      LPreference := LPreference.UseDefaultWidth;
    end;
    LAccepted := LAccepted.WithColumn(LPreference);
  end;
  SetLength(LOrder, LPreferences.OrderCount);
  LCount := 0;
  for LIndex := 0 to LPreferences.OrderCount - 1 do
  begin

    if LSpec.ColumnIndex(LPreferences.OrderAt(LIndex)) >= 0 then
    begin
      LOrder[LCount] := LPreferences.OrderAt(LIndex);
      Inc(LCount);
    end;
  end;
  SetLength(LOrder, LCount);
  LAccepted := LAccepted.Order(LOrder);
  { Rebinding preserves existing choices even if the new authored policy locks
    further user changes. It still prunes incompatible names/families/bounds.
    No interface-to-object cast or raw owner address crosses this boundary. }
  LImport := nil;

  if Supports(LNext, IColumnTransferTarget, LImport) then
  begin
    Result := LImport.PrepareColumnImport(LTarget, LAccepted);
  end
  else
  begin
    { Alternatives retain their own public admission contract; this function
      cannot grant a private bypass to an unrelated implementation. }
    Result := LNextColumns.PrepareColumns(LTarget, LAccepted);
  end;
end;
