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

{ Private implementation of nyx.source. The managed builder is a declarative
  Pascal subset with an exact ownership frame. Helpers outside that frame remain
  ordinary compiler-owned Pascal. Replaying a fresh document is essential:
  omitting a constructor, default or metadata statement must remove its meaning
  rather than leave an invisible value in a clone of the accepted design. }

function SpecializedKind(const AName, APrefix: TNyxText;
  out AKind: TNyxKind): Boolean;
var
  LKind: TNyxKind;
  LOrdinal: Integer;
begin
  { Ordinary declaration/construction reads use the closed immutable vocabulary
    rather than walking the complete catalog for each local in each merge frame.
    The fallback preserves the private helper's former alternate-prefix meaning. }
  LOrdinal := -1;

  if APrefix = 'NewNyx' then
  begin
    LOrdinal := GFactoryKinds.IndexOf(LowerCase(AName));
  end
  else if APrefix = 'TNyx' then
  begin
    LOrdinal := GClassKinds.IndexOf(LowerCase(AName));
  end
  else if APrefix = 'INyx' then
  begin
    LOrdinal := GInterfaceKinds.IndexOf(LowerCase(AName));
  end
  else
  begin
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin

      if SameText(AName, APrefix + GKindStems[LKind]) then
      begin
        AKind := LKind;
        Exit(True);
      end;
    end;
    Exit(False);
  end;

  if LOrdinal < 0 then
  begin
    Exit(False);
  end;
  AKind := TNyxKind(LOrdinal);
  Result := True;
end;

function ControlDeclaration(const AName: TNyxText): Boolean;
var
  LKind: TNyxKind;
  LName: TNyxText;
begin
  LName := LowerCase(AName);
  Result := (LName = 'tnyxnode') or (LName = 'inyxnode') or
    (LName = 'inyxcontrol') or (LName = 'inyxcaptioncontrol') or
    (LName = 'inyxlayoutcontrol') or (LName = 'inyxtextinput') or
    (LName = 'inyxbooleaninput') or (LName = 'inyxintegerinput') or
    (LName = 'inyximagecontrol') or (LName = 'inyxchoicecontrol') or
    (LName = 'inyxreferencecontrol') or SpecializedKind(AName, 'INyx', LKind);
end;

function FactoryAssignable(const ADeclaration: TNyxText;
  const AControl: INyxControl; AKind: TNyxKind; ASpecialized, ALegacy: Boolean): Boolean;
var
  LName: TNyxText;
  LDeclaredKind: TNyxKind;
begin
  { These are Pascal assignment rules, not a runtime downcast. A dynamic factory
    returns INyxControl even when its object happens to implement INyxBadge.
    Admission must reject an assignment that the real compiler would reject. }
  LName := LowerCase(ADeclaration);

  if ALegacy then
  begin
    Exit(LName = 'tnyxnode');
  end;

  if (LName = 'inyxcontrol') or (LName = 'inyxnode') then
  begin
    Exit(True);
  end;

  if not ASpecialized then
  begin
    Exit(False);
  end;

  if SpecializedKind(ADeclaration, 'INyx', LDeclaredKind) then
  begin
    Exit(LDeclaredKind = AKind);
  end;
  Result := False;

  if LName = 'inyxcaptioncontrol' then
  begin
    Result := Supports(AControl, INyxCaptionControl);
  end
  else if LName = 'inyxlayoutcontrol' then
  begin
    Result := Supports(AControl, INyxLayoutControl);
  end
  else if LName = 'inyxtextinput' then
  begin
    Result := Supports(AControl, INyxTextInput);
  end
  else if LName = 'inyxbooleaninput' then
  begin
    Result := Supports(AControl, INyxBooleanInput);
  end
  else if LName = 'inyxintegerinput' then
  begin
    Result := Supports(AControl, INyxIntegerInput);
  end
  else if LName = 'inyximagecontrol' then
  begin
    Result := Supports(AControl, INyxImageControl);
  end
  else if LName = 'inyxchoicecontrol' then
  begin
    Result := Supports(AControl, INyxChoiceControl);
  end
  else if LName = 'inyxreferencecontrol' then
  begin
    Result := Supports(AControl, INyxReferenceControl);
  end;
end;

procedure TConfigurationReader.Declarations;
var
  LNames: TNyxStrings;
  LName: TNyxText;
  LType: TNyxText;
  LIndex: Integer;
  LCount: Integer;
  LKind: TNyxStateKind;
  LIsState: Boolean;
begin
  FLocals := nil;
  FStates := nil;

  if not At('var') then
  begin
    Exit;
  end;
  Expect('var');
  LNames := TNyxStrings.Create;
  try
    while not At('begin') do
    begin
      LNames.Clear;
      repeat

        if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
        begin
          Fail('Expected a typed local declaration');
        end;
        LName := FTokens[FCursor].Text;
        ValidateLocalName(FSource, LName, FTokens[FCursor].Position);
        LNames.Add(LName);
        Inc(FCursor);

        if not At(',') then
        begin
          Break;
        end;
        Expect(',');
      until False;
      Expect(':');

      if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
      begin
        Fail('Declare a Nyx interface, descriptor or scalar reference type');
      end;
      LType := FTokens[FCursor].Text;
      Inc(FCursor);
      Expect(';');
      LIsState := False;
      for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
      begin

        if SameText(LType, CStateTypes[LKind]) then
        begin
          LIsState := True;
          Break;
        end;
      end;

      if not LIsState and not ControlDeclaration(LType) then
      begin
        Fail('Unsupported managed local type: ' + LType);
      end;
      for LIndex := 0 to LNames.Count - 1 do
      begin
        LName := LNames[LIndex];

        if (LocalIndex(LName) >= 0) or (StateIndex(LName) >= 0) then
        begin
          Fail('Control and state declarations require unique Pascal local names');
        end;

        if Length(FLocals) + Length(FStates) >= NyxMaximumNodes * 2 then
        begin
          Fail('Local declarations exceed the project budget');
        end;

        if LIsState then
        begin
          LCount := Length(FStates);
          SetLength(FStates, LCount + 1);
          FStates[LCount].Name := LName;
          FStates[LCount].Kind := LKind;
          FStates[LCount].Key := '';
          FStates[LCount].Assigned := False;
          FStateNames.AddFirst(LowerCase(LName), LCount);
        end
        else
        begin
          LCount := Length(FLocals);
          SetLength(FLocals, LCount + 1);
          FLocals[LCount].Name := LName;
          FLocals[LCount].DeclaredType := LType;
          FLocals[LCount].ID := '';
          FLocals[LCount].Created := False;
          FLocals[LCount].Admitted := False;
          FLocals[LCount].Configured := False;
          FLocalNames.AddFirst(LowerCase(LName), LCount);
        end;
      end;
    end;
    SetLength(FControls, Length(FLocals));
  finally
    LNames.Free;
  end;
end;

procedure TConfigurationReader.ConstructControl(AIndex: Integer);
var
  LName: TNyxText;
  LArgs: TValues;
  LKind: TNyxKind;
  LConstruction: TNyxConstruction;
  LIDIndex: Integer;
  LSpecialized: Boolean;
  LLegacy: Boolean;
  LDynamic: Boolean;
  LClassConstructor: Boolean;
  LControl: INyxControl;
  LNode: TNyxNode;
begin

  if FLocals[AIndex].Created then
  begin
    Fail('Construct a control local once; use a separate local for another control');
  end;
  Inc(FCursor);
  Expect(':=');

  if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
  begin
    Fail('A control assignment requires a typed Nyx factory');
  end;
  LName := FTokens[FCursor].Text;
  Inc(FCursor);
  LKind := Low(TNyxKind);
  LLegacy := SameText(LName, 'TNyxNode');
  LSpecialized := SpecializedKind(LName, 'NewNyx', LKind);
  LClassConstructor := SpecializedKind(LName, 'TNyx', LKind);
  LSpecialized := LSpecialized or LClassConstructor;
  LDynamic := SameText(LName, 'NewNyxBuiltinControl');

  if LLegacy or LClassConstructor then
  begin
    Expect('.');
    Expect('Create');
  end
  else if not LSpecialized and not LDynamic and not SameText(LName, 'NewNyxControl') then
  begin
    Fail('Use a specialized NewNyx factory or the explicit descriptor factory');
  end;
  LArgs := Arguments;
  LIDIndex := 0;
  LConstruction := ncoDefault;

  if not LSpecialized then
  begin
    LIDIndex := 1;
  end;

  if (Length(LArgs) <= LIDIndex) or (LArgs[LIDIndex].Kind <> vkText) then
  begin
    Fail('Supply an explicit text identity to the control factory');
  end;

  if LArgs[LIDIndex].Text = '' then
  begin
    Fail('Managed source requires a stable nonempty control identity');
  end;

  if LSpecialized or LDynamic then
  begin

    if LDynamic then
    begin

      if LArgs[0].Kind <> vkKind then
      begin
        Fail('NewNyxBuiltinControl requires a TNyxKind');
      end;
      LKind := TNyxKind(LArgs[0].Ordinal);
    end;

    if Length(LArgs) = LIDIndex + 2 then
    begin

      if LArgs[LIDIndex + 1].Kind <> vkConstruction then
      begin
        Fail('Recipe construction requires TNyxConstruction');
      end;
      LConstruction := TNyxConstruction(LArgs[LIDIndex + 1].Ordinal);
    end
    else if Length(LArgs) <> LIDIndex + 1 then
    begin
      Fail('A typed factory accepts identity and optional construction mode');
    end;
    if (LConstruction = ncoDefault) and (LKind >= nkLabeledButton) and
      (LKind <> nkSlotOverride) then
    begin

      if FRecipeCatalog = nil then
      begin
        FRecipeCatalog := TNyxCatalog.Create;
      end;
      LNode := FRecipeCatalog.NewNode(LKind, LArgs[LIDIndex].Text);
      try
        LControl := RetainNyxControl(LNode);
      finally
        { Retention acquires an independent reference; the new tree's original
          ownership token is released on success and every failure path. }
        ReleaseNyxNode(LNode);
      end;
    end
    else
    begin
      LControl := NewNyxBuiltinControl(LKind, LArgs[LIDIndex].Text, LConstruction);
    end;
  end
  else
  begin

    if Length(LArgs) <> 2 then
    begin
      Fail('The descriptor constructor accepts kind and identity');
    end;

    if LLegacy then
    begin
      case LArgs[0].Kind of
        vkText: LNode := TNyxNode.Create(LArgs[0].Text, LArgs[1].Text);
        vkKind: LNode := TNyxNode.Create(TNyxKind(LArgs[0].Ordinal), LArgs[1].Text);
        vkCustomKind: LNode := TNyxNode.Create(NyxCustomKind(LArgs[0].Text), LArgs[1].Text);
      else
        Fail('TNyxNode.Create requires a kind enum, reference or legacy text');
      end;
      try
        LControl := RetainNyxControl(LNode);
      except
        LNode.Free;
        raise;
      end;
      LNode.ReleaseOwnership;
    end
    else
    begin

      if LArgs[0].Kind <> vkCustomKind then
      begin
        Fail('NewNyxControl requires a distinct NyxCustomKind reference');
      end;
      LControl := NewNyxControl(NyxCustomKind(LArgs[0].Text), LArgs[1].Text);
    end;
  end;

  if not FactoryAssignable(FLocals[AIndex].DeclaredType, LControl, LKind,
    LSpecialized, LLegacy) then
  begin
    Fail('The factory result cannot be assigned to ' + FLocals[AIndex].DeclaredType);
  end;

  if FControlIDs.IndexOf(LControl.ID) >= 0 then
  begin
    Fail('Control identities must be unique');
  end;
  FControlIDs.AddFirst(LControl.ID, AIndex);
  { WithText is the specialized fluent method, retaining its exact interface.
    It is safe before adoption because this local owns the retained candidate. }
  while At('.') do
  begin
    Expect('.');
    Expect('WithText');

    if not LSpecialized then
    begin
      Fail('WithText belongs to a specialized factory result');
    end;
    LArgs := Arguments;

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkText) then
    begin
      Fail('WithText requires text');
    end;
    LControl.Node.Configure.Text(LArgs[0].Text);
  end;
  Expect(';');
  FControls[AIndex] := LControl;
  FLocals[AIndex].ID := LControl.ID;
  FLocals[AIndex].Created := True;
end;

procedure TConfigurationReader.OwnControl(AIndex: Integer; const AMethod: TNyxText);
var
  LChild: Integer;
  LPosition: Integer;
  LValue: TValue;
begin
  Inc(FCursor, 3);
  Expect('(');
  LPosition := -1;

  if SameText(AMethod, 'Insert') then
  begin
    LValue := Expression;

    if LValue.Kind <> vkInteger then
    begin
      Fail('Insert requires an Integer position');
    end;
    TryNyxStateInteger(LValue.Text, LPosition);
    Expect(',');
  end;

  if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
  begin
    Fail('Ownership calls require a declared control local');
  end;
  LChild := LocalIndex(FTokens[FCursor].Text);

  if (LChild < 0) or not FLocals[LChild].Created then
  begin
    Fail('Construct the child before admitting it to an owner');
  end;

  if FLocals[LChild].Admitted then
  begin
    Fail('A control can have one owner; move its ownership statement instead');
  end;
  Inc(FCursor);
  Expect(')');
  Expect(';');

  if AIndex < 0 then
  begin

    if SameText(AMethod, 'AddPage') then
    begin
      FDocument.AddPage(FControls[LChild]);
    end
    else if SameText(AMethod, 'AddComponent') then
    begin
      FDocument.AddComponent(FControls[LChild]);
    end
    else
    begin
      Fail('The document owns roots through AddPage or AddComponent');
    end;
  end
  else
  begin

    if not FLocals[AIndex].Admitted then
    begin
      Fail('Admit the parent before adding descendants');
    end;

    if SameText(AMethod, 'Add') then
    begin
      FControls[AIndex].Add(FControls[LChild]);
    end
    else if SameText(AMethod, 'Insert') then
    begin
      FControls[AIndex].Insert(LPosition, FControls[LChild]);
    end
    else
    begin
      Fail('Controls own children through Add or Insert');
    end;
  end;
  FLocals[LChild].Admitted := True;
end;

procedure TConfigurationReader.PresentationDefaults;
var
  LArgs: TValues;
  LReference: TNyxPresentationRef;
  LDefinition: TNyxPresentationCondition;
begin
  Inc(FCursor, 3);
  Expect('.');
  Expect('Define');
  LArgs := Arguments;
  Expect(';');

  if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkPresentationRef) or
    not (LArgs[1].Kind in [vkViewportCondition, vkPresentationCondition]) then
  begin
    Fail('Presentations.Define requires a presentation reference and typed condition');
  end;
  LReference := NyxPresentation(LArgs[0].Text);

  if LArgs[1].Kind = vkViewportCondition then
  begin
    LDefinition := TNyxPresentationCondition.Automatic(LArgs[1].ViewportCondition);
  end
  else
  begin
    LDefinition := LArgs[1].PresentationCondition;
  end;

  if (LDefinition.Activation = npaAutomatic) and LDefinition.Viewport.IsAny then
  begin
    Fail('A named automatic presentation needs a viewport condition');
  end;

  if FApply then
  begin

    if FDocument.Presentations.Contains(LReference) then
    begin
      Fail('Define each presentation once in the declarative builder');
    end;
    FDocument.Presentations.Define(LReference, LDefinition);
  end;
end;

procedure TConfigurationReader.BuilderStatement;
var
  LLocal: Integer;
  LState: Integer;
  LMethod: TNyxText;
  LValue: TValue;
  LDocument: Boolean;
begin

  if At('NyxCallbacks') then
  begin
    Callbacks;
    Exit;
  end;

  if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
  begin
    Fail('Expected a typed builder statement');
  end;
  LDocument := At('Result');
  LLocal := LocalIndex(FTokens[FCursor].Text);
  LState := StateIndex(FTokens[FCursor].Text);

  if (LState >= 0) and (FCursor + 1 < Length(FTokens)) and
    (FTokens[FCursor + 1].Text = ':=') then
  begin
    ReferenceAssignment(LState);
    Exit;
  end;

  if (LLocal >= 0) and (FCursor + 1 < Length(FTokens)) and
    (FTokens[FCursor + 1].Text = ':=') then
  begin
    ConstructControl(LLocal);
    Exit;
  end;

  if not LDocument and ((LLocal < 0) or not FLocals[LLocal].Admitted) then
  begin
    Fail('Declare, construct and admit the control before using its fluent members');
  end;

  if not LDocument and SameText(FLocals[LLocal].DeclaredType, 'INyxNode') then
  begin
    Fail('Use INyxControl or a specialized interface for fluent member access');
  end;

  if (FCursor + 2 >= Length(FTokens)) or (FTokens[FCursor + 1].Text <> '.') or
    (FTokens[FCursor + 2].Kind <> tkWord) then
  begin
    Fail('Expected a public Nyx builder member');
  end;
  LMethod := FTokens[FCursor + 2].Text;

  if LDocument and SameText(LMethod, 'Title') then
  begin

    if FTitleSeen then
    begin
      Fail('Keep one Result.Title assignment');
    end;
    Inc(FCursor, 3);
    Expect(':=');
    LValue := Expression;

    if LValue.Kind <> vkText then
    begin
      Fail('The project title requires text');
    end;
    Expect(';');
    FDocument.Title := LValue.Text;
    FTitleSeen := True;
  end
  else if LDocument and SameText(LMethod, 'State') then
  begin
    Defaults;
  end
  else if LDocument and SameText(LMethod, 'Collections') then
  begin
    CollectionDefaults;
  end
  else if LDocument and SameText(LMethod, 'Presentations') then
  begin
    PresentationDefaults;
  end
  else if SameText(LMethod, 'Extensions') then
  begin
    Extensions(LLocal);
  end
  else if not LDocument and SameText(LMethod, 'Configure') then
  begin
    Configure(LLocal);
  end
  else if not LDocument and SameText(LMethod, 'Binds') then
  begin
    Bindings(LLocal);
  end
  else if not LDocument and SameText(LMethod, 'Contract') then
  begin
    Contract(LLocal);
  end
  else if SameText(LMethod, 'Add') or SameText(LMethod, 'Insert') or
    SameText(LMethod, 'AddPage') or SameText(LMethod, 'AddComponent') then
  begin
    OwnControl(LLocal, LMethod);
  end
  else
  begin
    Fail('Unsupported managed builder member: ' + LMethod);
  end;
end;

procedure TConfigurationReader.Reconstruct;
var
  LIndex: Integer;
begin
  { Parse every token in the frame. Looking only for recognized statements would
    silently ignore conditions, alternate ownership or hidden executable code. }
  FCursor := 0;
  FTitleSeen := False;
  FOwnedTry := False;
  Expect('function');
  Expect('BuildNyxDocument');
  Expect(':');
  Expect('TNyxDocument');
  Expect(';');
  Declarations;
  Expect('begin');
  Expect('Result');
  Expect(':=');
  Expect('TNyxDocument');
  Expect('.');
  Expect('Create');

  if At('(') then
  begin
    Expect('(');
    Expect(')');
  end;
  Expect(';');
  Expect('try');
  FOwnedTry := True;
  while not At('except') do
  begin
    BuilderStatement;
  end;
  Expect('except');
  FOwnedTry := False;
  Expect('Result');
  Expect('.');
  Expect('Free');

  if At('(') then
  begin
    Expect('(');
    Expect(')');
  end;
  Expect(';');
  Expect('raise');
  Expect(';');
  Expect('end');
  Expect(';');
  Expect('end');
  Expect(';');

  if FCursor <> Length(FTokens) then
  begin
    Fail('Keep application helpers outside the managed views boundary');
  end;

  if not FTitleSeen then
  begin
    Fail('Keep the Result.Title assignment');
  end;
  for LIndex := 0 to Length(FLocals) - 1 do
  begin

    if FLocals[LIndex].Created and not FLocals[LIndex].Admitted then
    begin
      Fail('Admit every constructed control to the document: ' + FLocals[LIndex].Name);
    end;
  end;
end;
