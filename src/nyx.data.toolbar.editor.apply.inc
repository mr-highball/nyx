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

{ Included by nyx.data.toolbar.editor. All writes are to the caller's detached
  candidate. The enclosing Studio transaction owns publication and history. }

function Inside(ANode, AOwner: TNyxNode): Boolean;
begin
  Result := False;
  while ANode <> nil do
  begin

    if ANode = AOwner then
    begin
      Exit(True);
    end;
    ANode := ANode.Parent;
  end;
end;

function FreeIdentity(ADocument: TNyxDocument; const ABase: TNyxText): TNyxText;
var
  LSerial: Integer;
begin
  Result := ABase;
  LSerial := 0;
  while ADocument.Find(Result) <> nil do
  begin
    Inc(LSerial);

    if LSerial > NyxMaximumNodes then
    begin
      raise ENyxDataToolbar.Create('Toolbar identity budget exhausted');
    end;
    Result := ABase + '-' + TNyxText(IntToStr(LSerial));
  end;
end;

function WritableRule(ADocument: TNyxDocument; AReference: TNyxNode;
  const APath: TNyxText): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to AReference.Count - 1 do
  begin

    if (AReference.Children[LIndex].Kind = NyxKindName(nkSlotOverride)) and
      (AReference.Children[LIndex].StoredProp(CPath) = APath) then
    begin

      if Result <> nil then
      begin
        raise ENyxDataToolbar.Create('Toolbar role override is ambiguous');
      end;
      Result := AReference.Children[LIndex];
    end;
  end;

  if Result = nil then
  begin
    Result := AReference.OverridePart(NyxPart(APath)).Named(
      FreeIdentity(ADocument, AReference.ID + '-toolbar-role'));
  end;

  if Result.StoredProp('mode') = NyxOverrideName(noRemove) then
  begin
    raise ENyxDataToolbar.Create('Restore the required toolbar role before configuring it');
  end;
end;

function WritableRole(ADocument: TNyxDocument; AOwner, AProjection: TNyxNode;
  const APart: TNyxText): TNyxNode;
var
  LReference: TNyxNode;
  LPath: TNyxText;
begin

  if AOwner.Kind = NyxKindName(nkDataToolbar) then
  begin
    Exit(AOwner.Part(NyxPart(APart)));
  end;
  LReference := AOwner;
  LPath := APart;

  if AOwner.Kind = NyxKindName(nkSlotOverride) then
  begin
    LReference := AOwner.Parent;
    LPath := AOwner.StoredProp(CPath);

    if LPath = '.' then
    begin
      LPath := APart;
    end
    else
    begin
      LPath := LPath + '/' + APart;
    end;
  end;
  Result := ADocument.Find(AProjection.SourceID);

  if (Result <> nil) and Inside(Result, LReference) and
    (Result.Kind <> NyxKindName(nkSlotOverride)) then
  begin
    Exit;
  end;
  Result := WritableRule(ADocument, LReference, LPath);
end;

function AppendHost(ADocument: TNyxDocument; AOwner: TNyxNode): TNyxNode;
var
  LDepth: Integer;
begin
  Result := AOwner;

  if AOwner.Kind = NyxKindName(nkComponent) then
  begin
    Result := WritableRule(ADocument, AOwner, '.');
  end;
  for LDepth := 0 to NyxMaximumTreeDepth do
  begin

    if Result.Kind <> NyxKindName(nkSlotOverride) then
    begin
      Exit;
    end;

    if Result.StoredProp('mode') = NyxOverrideName(noReplace) then
    begin

      if Result.Count <> 1 then
      begin
        raise ENyxDataToolbar.Create('Toolbar replacement requires its exact local payload');
      end;
      Result := Result.Children[0];
      Continue;
    end;

    if Result.StoredProp('mode') = NyxOverrideName(noProperties) then
    begin
      Result.SetProp('mode', NyxOverrideName(noAppend));
    end;

    if not ((Result.StoredProp('mode') = NyxOverrideName(noAppend)) or
      (Result.StoredProp('mode') = NyxOverrideName(noPrepend))) then
    begin
      raise ENyxDataToolbar.Create('This toolbar descriptor cannot accept its required Clear control');
    end;
    Exit;
  end;
  raise ENyxDataToolbar.Create('Toolbar replacement exceeds the existing depth budget');
end;

function OptionalPart(AToolbar: TNyxNode; const APart: TNyxText): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to AToolbar.Count - 1 do
  begin

    if AToolbar.Children[LIndex].StoredProp(NyxAttributeName(atPart)) = APart then
    begin

      if Result <> nil then
      begin
        raise ENyxDataToolbar.Create('Toolbar role is ambiguous: ' + APart);
      end;
      Result := AToolbar.Children[LIndex];
    end;
  end;
end;

procedure CheckRoles(AProjection: TNyxNode; ARequireClear: Boolean);
var
  LSearch: TNyxNode;
  LSort: TNyxNode;
  LClear: TNyxNode;
  LDomain: TNyxValueDomain;
begin

  if AProjection.Prop(NyxAttributeName(atCompound), 'false') <> 'true' then
  begin
    raise ENyxDataToolbar.Create('Toolbar behavior requires its compound control contract');
  end;
  LSearch := AProjection.Part(NyxPart('search'));
  LSort := AProjection.Part(NyxPart('sort'));

  if (LSearch = nil) or (LSort = nil) or
    (LSearch.ProjectionKind <> NyxKindName(nkInput)) or
    (LSort.ProjectionKind <> NyxKindName(nkSelect)) then
  begin
    raise ENyxDataToolbar.Create('Toolbar Search and Sort must retain their specialized controls');
  end;
  LDomain := NyxNodeValueDomain(LSearch);

  if not LDomain.Defined or (LDomain.Kind <> nskText) then
  begin
    raise ENyxDataToolbar.Create('Toolbar Search needs an ordinary text input domain');
  end;
  LDomain.Admit(NyxData(''));
  LClear := OptionalPart(AProjection, 'clear');

  if (LClear = nil) and not ARequireClear then
  begin
    Exit;
  end;

  if (LClear = nil) or (LClear.ProjectionKind <> NyxKindName(nkButton)) or
    (LClear.Prop(NyxAttributeName(atAction), '') <> '') then
  begin
    raise ENyxDataToolbar.Create('Toolbar Clear needs an independent ordinary button');
  end;
end;

{ Admit the same reserved leaf roles as the runtime. A custom themed subtree is
  preserved, but neither a competing action nor a bound label can be claimed. }
procedure CheckQueryRoles(AProjection: TNyxNode;
  const AOptions: TNyxDataToolbarQueryOptions);
var
  LContext: TNyxNode;
  LStatus: TNyxNode;
  LDetails: TNyxNode;
  LBinding: TNyxBindingSpec;
begin

  if not AOptions.Defined or (AOptions.DisplayMode = ntqdHidden) then
  begin
    Exit;
  end;
  LContext := OptionalPart(AProjection, NyxDataToolbarQueryContextPart);

  if LContext = nil then
  begin
    raise ENyxDataToolbar.Create('Query presentation requires its named context');
  end;
  LStatus := LContext.Part(NyxPart('status'));

  if (LStatus = nil) or (LStatus.Kind <> NyxKindName(nkLabel)) or
    (LStatus.ProjectionKind <> NyxKindName(nkLabel)) or
    (LStatus.Prop(NyxAttributeName(atCompound), 'false') = 'true') or
    (LStatus.Prop(NyxAttributeName(atAction), NyxActionName(naNone)) <> NyxActionName(naNone)) or
    LStatus.FindBinding(bpText, LBinding) then
  begin
    raise ENyxDataToolbar.Create('Query status requires an independent unbound Label');
  end;

  if AOptions.DisplayMode = ntqdDetails then
  begin
    LDetails := LContext.Part(NyxPart('details'));

    if (LDetails = nil) or (LDetails.Kind <> NyxKindName(nkButton)) or
      (LDetails.ProjectionKind <> NyxKindName(nkButton)) or
      (LDetails.Prop(NyxAttributeName(atCompound), 'false') = 'true') or
      (LDetails.Prop(NyxAttributeName(atAction), NyxActionName(naNone)) <> NyxActionName(naNone)) then
    begin
      raise ENyxDataToolbar.Create('Query details require an independent ordinary Button');
    end;
  end;
end;

{ Add an optional owned subtree or edit exact local/named presentation values.
  Existing styles, extra children, callbacks and unrelated slots survive. }
procedure ApplyQueryPresentation(ADocument: TNyxDocument; AOwner, AProjection: TNyxNode;
  const AOptions: TNyxDataToolbarQueryOptions);
var
  LContext: TNyxNode;
  LWritable: TNyxNode;
  LContent: INyxColumn;
  LDetails: TNyxNode;
  LID: TNyxText;
  LIndex: Integer;
begin

  if not AOptions.Defined then
  begin
    Exit;
  end;
  LContext := OptionalPart(AProjection, NyxDataToolbarQueryContextPart);

  if LContext = nil then
  begin
    for LIndex := 0 to NyxMaximumNodes do
    begin
      LID := AOwner.ID + '-query-context-' + TNyxText(IntToStr(LIndex));

      if (ADocument.Find(LID) = nil) and (ADocument.Find(LID + '-status') = nil) and
        (ADocument.Find(LID + '-details') = nil) then
      begin
        LContent := NewNyxDataToolbarQueryContent(LID, AOptions);
        AppendHost(ADocument, AOwner).Add(LContent.Node);
        Exit;
      end;
    end;
    raise ENyxDataToolbar.Create('Query presentation identity budget exhausted');
  end;
  { Validate before any detached writes, then keep the caller's actual parts. }
  CheckQueryRoles(AProjection, AOptions);
  LWritable := WritableRole(ADocument, AOwner, LContext, NyxDataToolbarQueryContextPart);
  RetainNyxControl(LWritable).Configure.Visible(AOptions.DisplayMode <> ntqdHidden).Done;

  if AOptions.DisplayMode = ntqdHidden then
  begin
    Exit;
  end;
  LWritable := WritableRole(ADocument, AOwner, LContext.Part(NyxPart('status')),
    NyxDataToolbarQueryStatusPart);
  RetainNyxControl(LWritable).Configure.Text(AOptions.StatusCaption).Done;
  LDetails := OptionalPart(LContext, 'details');

  if LDetails <> nil then
  begin
    LWritable := WritableRole(ADocument, AOwner, LDetails, NyxDataToolbarQueryDetailsPart);
    RetainNyxControl(LWritable).Configure.Text(AOptions.ButtonCaption).
      Visible(AOptions.DisplayMode = ntqdDetails).Done;
  end;
end;

procedure ApplyNyxDataToolbarEditorChange(ADocument: TNyxDocument;
  const AChange: TNyxDataToolbarEditorChange);
var
  LRaw: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LSort: TNyxNode;
  LWritable: TNyxNode;
  LClear: INyxButton;
  LIntent: TNyxDataToolbarIntent;
  LPrevious: TNyxDataToolbarIntent;
  LOptions: TNyxDataToolbarOptions;
  LDomain: TNyxValueDomain;
  LItems: TNyxText;
  LValue: TNyxText;
  LIndex: Integer;
  LCanReset: Boolean;
begin
  ValidateNyxDataToolbarEditorChange(ADocument, AChange);
  LRaw := ADocument.FindControl(AChange.Owner);

  if AChange.Action = nteaDisable then
  begin
    SetNyxDataToolbarIntent(RetainNyxControl(LRaw), AChange.Intent);
    Exit;
  end;
  LContext := nil;
  try
    ResolveOwner(ADocument, AChange.Owner, LRaw, LContext, LProjection);
    LPrevious := GetNyxDataToolbarIntent(LProjection);

    if AChange.Action = nteaInherit then
    begin
      InheritNyxDataToolbarIntent(RetainNyxControl(LRaw));
      FreeAndNil(LContext);
      ResolveOwner(ADocument, AChange.Owner, LRaw, LContext, LProjection);
      LIntent := GetNyxDataToolbarIntent(LProjection);
    end
    else
    begin
      LIntent := AChange.Intent;
    end;

    if LIntent.Mode <> ntiConfigured then
    begin
      Exit;
    end;
    LOptions := LIntent.Options;
    AdmitOptions(ADocument, LContext, LProjection, LOptions);
    CheckRoles(LProjection, False);
    LSort := LProjection.Part(NyxPart('sort'));
    { During inheritance the selector still carries the previous local domain.
      Use its previous intent only for exact choice-vector provenance. }
    LDomain := NyxDataToolbarSortDomain(LProjection, LOptions, LPrevious);
    LValue := LSort.Prop(NyxAttributeName(atValue));
    LCanReset := LSort.Props.IndexOfName(NyxAttributeName(atValue)) < 0;

    if LPrevious.Mode = ntiConfigured then
    begin
      LCanReset := LCanReset or (LValue = LPrevious.Options.BaselineChoice.Name);
      for LIndex := 0 to LPrevious.Options.Count - 1 do
      begin
        LCanReset := LCanReset or (LValue = LPrevious.Options.ChoiceAt(LIndex).Reference.Name);
      end;
    end;
    try
      LDomain.Admit(NyxData(LValue));
    except
      on ENyxContract do
      begin

        if not LCanReset then
        begin
          raise;
        end;
        LValue := LOptions.BaselineChoice.Name;
      end;
    end;
    LItems := LOptions.BaselineChoice.Name;
    for LIndex := 0 to LOptions.Count - 1 do
    begin
      LItems := LItems + #10 + LOptions.ChoiceAt(LIndex).Reference.Name;
    end;
    LWritable := WritableRole(ADocument, LRaw, LSort, 'sort');
    SetNyxDataToolbarSortDomain(RetainNyxControl(LWritable), LProjection, LOptions, LPrevious);
    LWritable.SetProp(NyxAttributeName(atItems), LItems).
      SetProp(NyxAttributeName(atValue), LValue);

    if OptionalPart(LProjection, 'clear') = nil then
    begin
      LWritable := AppendHost(ADocument, LRaw);
      LClear := NewNyxButton(FreeIdentity(ADocument, LRaw.ID + '-clear'));
      LClear.Configure.Text('Clear').AccessibleName('Clear search and sorting').
        PartName(NyxPart('clear')).OnClick(NyxSemantic(nseClear)).Done;
      LWritable.Add(LClear.Node);
    end;

    ApplyQueryPresentation(ADocument, LRaw, LProjection, LOptions.QueryPresentation);

    if AChange.Action = nteaConfigure then
    begin
      SetNyxDataToolbarIntent(RetainNyxControl(LRaw), LIntent);
    end;
    FreeAndNil(LContext);
    ResolveOwner(ADocument, AChange.Owner, LRaw, LContext, LProjection);
    CheckRoles(LProjection, True);
    CheckQueryRoles(LProjection, LOptions.QueryPresentation);
    AdmitOptions(ADocument, LContext, LProjection, LOptions);
  finally
    LContext.Free;
  end;
end;
