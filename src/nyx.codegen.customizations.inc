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

{ Included after the shared typed property emitter. A customization changes only
  explicit edited properties: unrelated computed values continue to be supplied
  by the original handwritten builder on every execution. }

function CustomizationVariable(ADocument: TNyxDocument; ANode: TNyxNode): TNyxText;
var
  LOrdinal: Integer;
  LMatches: Integer;
  LStem: TNyxText;

  procedure Visit(ACurrent: TNyxNode);
  var
    LIndex: Integer;
  begin

    if ControlStem(ACurrent.ID) + ControlStem(ACurrent.Kind) = LStem then
    begin
      Inc(LMatches);

      if ACurrent = ANode then
      begin
        LOrdinal := LMatches;
      end;
    end;
    for LIndex := 0 to ACurrent.Count - 1 do
    begin
      Visit(ACurrent.Children[LIndex]);
    end;
  end;

var
  LIndex: Integer;
begin
  LStem := ControlStem(ANode.ID) + ControlStem(ANode.Kind);
  LOrdinal := 0;
  LMatches := 0;
  for LIndex := 0 to ADocument.Count - 1 do
  begin
    Visit(ADocument.Pages[LIndex]);
  end;
  for LIndex := 0 to ADocument.ComponentCount - 1 do
  begin
    Visit(ADocument.Components[LIndex]);
  end;
  Result := 'L' + LStem;

  if LMatches > 1 then
  begin
    Result := Result + TNyxText(IntToStr(LOrdinal));
  end;
end;

function CustomizationProperty(ADocument: TNyxDocument; ANode: TNyxNode;
  const AKey: TNyxText): TNyxText;
var
  LKey: TNyxText;
  LPlatform: TNyxPlatform;
  LAttribute: TNyxAttribute;
  LViewport: TNyxViewportCondition;
  LPresentation: TNyxPresentationRef;
  LPresentationPlatform: TNyxPlatform;
  LPresentationAttribute: TNyxAttribute;
  LKind: TNyxKind;
begin

  if not TryNyxKind(ANode.Kind, LKind) or (LKind = nkSlotOverride) then
  begin
    raise ENyxModel.Create('Visual customization requires a specialized built-in control');
  end;
  LKey := AKey;
  LPlatform := npfAny;
  LViewport := TNyxViewportCondition.Any;
  LPresentation := Default(TNyxPresentationRef);
  Result := '  ' + CustomizationVariable(ADocument, ANode) + '.Configure' + #10;

  if TryNyxPlatformKey(AKey, LPlatform, LAttribute) then
  begin
    LKey := NyxAttributeName(LAttribute);
  end
  else if TryNyxViewportKey(AKey, LViewport, LPlatform, LAttribute) then
  begin
    LKey := NyxAttributeName(LAttribute);
  end;

  if TryNyxPresentationKey(AKey, LPresentation,
    LPresentationPlatform, LPresentationAttribute) then
  begin
    LPlatform := LPresentationPlatform;
    LKey := NyxAttributeName(LPresentationAttribute);
    Result := Result + '    .WhenPresentation(NyxPresentation(' +
      PascalString(LPresentation.Name) + '))' + #10;
  end
  else if not LViewport.IsAny then
  begin
    Result := Result + '    .WhenViewport(' + LViewport.Pascal + ')' + #10;
  end;

  if LPlatform <> npfAny then
  begin
    Result := Result + '    .ForPlatform(' + NyxPlatformSymbol(LPlatform) + ')' + #10;
  end;
  Result := Result + '    .' + ConfigurationCall(ANode, LKey, ANode.Prop(AKey)) + #10 +
    '    .Done;' + #10;
end;

class function TNyxCodegen.CustomizationChanges(ABefore, AAfter: TNyxDocument):
  TNyxCustomizationBlocks;
var
  LCopy: TNyxDocument;
  LCount: Integer;
  LIndex: Integer;

  procedure Add(const AID, AKey, APascal: TNyxText);
  begin
    SetLength(Result, LCount + 1);
    Result[LCount].ControlID := AID;
    Result[LCount].PropertyName := AKey;
    Result[LCount].Pascal := APascal;
    Inc(LCount);
  end;

  procedure Visit(ANode: TNyxNode);
  var
    LBefore: TNyxNode;
    LCopyNode: TNyxNode;
    LProp: Integer;
    LChild: Integer;
    LKey: TNyxText;
  begin
    LBefore := ABefore.Find(ANode.ID);
    LCopyNode := LCopy.Find(ANode.ID);

    if (LBefore = nil) or (LCopyNode = nil) or (LBefore.Kind <> ANode.Kind) then
    begin
      raise ENyxModel.Create('Structural visual customization is not implemented');
    end;
    for LProp := 0 to LBefore.Props.Count - 1 do
    begin

      if ANode.Props.IndexOfName(LBefore.Props.Names[LProp]) < 0 then
      begin
        raise ENyxModel.Create('Property removal requires an explicit customization writer');
      end;
    end;
    for LProp := 0 to ANode.Props.Count - 1 do
    begin
      LKey := ANode.Props.Names[LProp];

      if (LBefore.Props.IndexOfName(LKey) < 0) or (LBefore.Prop(LKey) <> ANode.Prop(LKey)) then
      begin
        Add(ANode.ID, LKey, CustomizationProperty(AAfter, ANode, LKey));
      end;
    end;
    LCopyNode.Props.Assign(ANode.Props);
    for LChild := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LChild]);
    end;
  end;

begin

  if (ABefore = nil) or (AAfter = nil) then
  begin
    raise ENyxModel.Create('Customization requires independent before/after documents');
  end;
  Result := nil;
  LCount := 0;
  LCopy := ABefore.Clone;
  try

    if ABefore.Title <> AAfter.Title then
    begin
      Add('', 'title', '  ADocument.Title := ' + PascalString(AAfter.Title) + ';' + #10);
    end;
    LCopy.Title := AAfter.Title;
    for LIndex := 0 to AAfter.Count - 1 do
    begin
      Visit(AAfter.Pages[LIndex]);
    end;
    for LIndex := 0 to AAfter.ComponentCount - 1 do
    begin
      Visit(AAfter.Components[LIndex]);
    end;
    { Comparing the whole canonical meaning catches every unhandled family:
      roots, ownership/order, state, resources, contracts, menus and extensions.
      No partially handled command can escape as a successful source proposal. }

    if TNyxCodec.Encode(LCopy) <> TNyxCodec.Encode(AAfter) then
    begin
      raise ENyxModel.Create('This visual command needs a broader customization writer');
    end;
  finally
    LCopy.Free;
  end;
end;

class function TNyxCodegen.CustomizationRoutine(ADocument: TNyxDocument;
  const ABlocks: TNyxCustomizationBlocks): TNyxText;
var
  LLines: TNyxStrings;
  LDeclared: TNyxStrings;
  LIndex: Integer;
  LNode: TNyxNode;
  LKind: TNyxKind;
  LVariable: TNyxText;
  LIdentity: TNyxText;
  LCharacter: Integer;
begin
  LLines := TNyxStrings.Create;
  LDeclared := TNyxStrings.Create;
  try
    LLines.Add('procedure CustomizeNyxDocument(ADocument: TNyxDocument);');
    for LIndex := 0 to High(ABlocks) do
    begin

      if (ABlocks[LIndex].ControlID = '') or
        (LDeclared.IndexOf(ABlocks[LIndex].ControlID) >= 0) then
      begin
        Continue;
      end;
      LNode := ADocument.Find(ABlocks[LIndex].ControlID);

      if (LNode = nil) or not TryNyxKind(LNode.Kind, LKind) then
      begin
        raise ENyxModel.Create('Managed customization control no longer exists');
      end;

      if LDeclared.Count = 0 then
      begin
        LLines.Add('var');
      end;
      LDeclared.Add(LNode.ID);
      LLines.Add('  ' + CustomizationVariable(ADocument, LNode) + ': INyx' +
        ControlStem(LNode.Kind) + ';');
    end;
    LLines.Add('begin');
    for LIndex := 0 to LDeclared.Count - 1 do
    begin
      LNode := ADocument.Find(LDeclared[LIndex]);
      LVariable := CustomizationVariable(ADocument, LNode);
      LLines.Add('  ' + LVariable + ' := RequireNyxControl(ADocument, NyxControl(' +
        PascalString(LNode.ID) + '), nk' + ControlStem(LNode.Kind) + ') as INyx' +
        ControlStem(LNode.Kind) + ';');
    end;
    for LIndex := 0 to High(ABlocks) do
    begin
      LIdentity := ABlocks[LIndex].ControlID + ABlocks[LIndex].PropertyName;
      for LCharacter := 1 to Length(LIdentity) do
      begin

        if Ord(LIdentity[LCharacter]) < 32 then
        begin
          raise ENyxModel.Create('Customization identities cannot contain control characters');
        end;
      end;
      { These ordinary comments are exact editor ownership boundaries, not
        behavior switches. Quoted identities protect punctuation and Unicode. }
      LLines.Add('');
      LLines.Add('  // nyx:customize ' + PascalString(ABlocks[LIndex].ControlID) + ' ' +
        PascalString(ABlocks[LIndex].PropertyName));
      LLines.Add(TrimRight(ABlocks[LIndex].Pascal));
    end;
    LLines.Add('  // nyx:customizations-statements-end');
    LLines.Add('end;');
    Result := LLines.Text;
  finally
    LDeclared.Free;
    LLines.Free;
  end;
end;
