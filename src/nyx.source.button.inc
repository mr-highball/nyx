{ Included by nyx.source. Only immutable public constructors and the managed
  presentation facade are replayed. No renderer, image decoder or arbitrary
  Pascal callback executes while admitting source into a candidate document. }
function TConfigurationReader.ButtonPresentationConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
begin
  Result := Default(TValue);

  if AName = 'nyxglyphcell' then
  begin
    LArgs := Arguments(4);

    if (Length(LArgs) <> 4) or (LArgs[0].Kind <> vkInteger) or
      (LArgs[1].Kind <> vkInteger) or (LArgs[2].Kind <> vkInteger) or
      (LArgs[3].Kind <> vkInteger) then
    begin
      Fail('NyxGlyphCell requires columns, rows, column and row Integers');
    end;
    Result.Kind := vkGlyphCell;
    Result.GlyphCell := NyxGlyphCell(StrToInt(LArgs[0].Text), StrToInt(LArgs[1].Text),
      StrToInt(LArgs[2].Text), StrToInt(LArgs[3].Text));
    Exit;
  end;
  Result.Kind := vkButtonPresentation;
  Result.ButtonPresentation := NyxButtonPresentation;

  if At('(') then
  begin
    LArgs := Arguments(0);

    if Length(LArgs) <> 0 then
    begin
      Fail('NyxButtonPresentation takes no constructor arguments');
    end;
  end;
  while At('.') do
  begin
    Inc(FCursor);
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments(1);

    if Length(LArgs) <> 1 then
    begin
      Fail('Button presentation methods require one typed argument');
    end;

    if (LMethod = 'glyph') and (LArgs[0].Kind = vkImageSource) then
    begin
      Result.ButtonPresentation := Result.ButtonPresentation.Glyph(LArgs[0].ImageSource);
    end
    else if (LMethod = 'glyph') and (LArgs[0].Kind = vkResourceImage) then
    begin
      Result.ButtonPresentation := Result.ButtonPresentation.Glyph(LArgs[0].ResourceImage);
    end
    else if (LMethod = 'cell') and (LArgs[0].Kind = vkGlyphCell) then
    begin
      Result.ButtonPresentation := Result.ButtonPresentation.Cell(LArgs[0].GlyphCell);
    end
    else if (LMethod = 'glyphsize') and (LArgs[0].Kind = vkInteger) then
    begin
      Result.ButtonPresentation := Result.ButtonPresentation.GlyphSize(StrToInt(LArgs[0].Text));
    end
    else if (LMethod = 'glyphgap') and (LArgs[0].Kind = vkInteger) then
    begin
      Result.ButtonPresentation := Result.ButtonPresentation.GlyphGap(StrToInt(LArgs[0].Text));
    end
    else if (LMethod = 'content') and (LArgs[0].Kind = vkButtonContent) then
    begin
      Result.ButtonPresentation := Result.ButtonPresentation.Content(TNyxButtonContent(LArgs[0].Ordinal));
    end
    else if (LMethod = 'density') and (LArgs[0].Kind = vkButtonDensity) then
    begin
      Result.ButtonPresentation := Result.ButtonPresentation.Density(TNyxButtonDensity(LArgs[0].Ordinal));
    end
    else if (LMethod = 'alignment') and (LArgs[0].Kind = vkButtonAlignment) then
    begin
      Result.ButtonPresentation := Result.ButtonPresentation.Alignment(TNyxButtonContentAlignment(LArgs[0].Ordinal));
    end
    else if (LMethod = 'fontweight') and (LArgs[0].Kind = vkButtonWeight) then
    begin
      Result.ButtonPresentation := Result.ButtonPresentation.FontWeight(TNyxButtonFontWeight(LArgs[0].Ordinal));
    end
    else if (LMethod = 'glyphtone') and (LArgs[0].Kind = vkGlyphTone) then
    begin
      Result.ButtonPresentation := Result.ButtonPresentation.GlyphTone(TNyxGlyphTone(LArgs[0].Ordinal));
    end
    else
    begin
      Fail('Unknown button presentation method or incorrect argument type: ' + LMethod);
    end;
  end;
end;

procedure TConfigurationReader.ButtonConfiguration;
var
  LLocal: Integer;
  LNode: TNyxNode;
  LScope: TNyxNodeConfig;
  LPlatform: TNyxPlatform;
  LViewport: TNyxViewportCondition;
  LPresentation: TNyxPresentationRef;
  LMethod: TNyxText;
  LArgs: TValues;
begin
  Expect('NyxButtonConfigure');
  Expect('(');
  LLocal := LocalIndex(FTokens[FCursor].Text);

  if (LLocal < 0) or not FLocals[LLocal].Admitted then
  begin
    Fail('Button presentation requires an already owned control');
  end;
  LNode := AdmittedNode(LLocal);
  Inc(FCursor);
  Expect('.');
  Expect('Configure');
  LPlatform := npfAny;
  LViewport := TNyxViewportCondition.Any;
  LPresentation := Default(TNyxPresentationRef);
  while At('.') do
  begin
    Inc(FCursor);
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments(1);

    if Length(LArgs) <> 1 then
    begin
      Fail('Choose a typed button configuration scope');
    end;

    if (LMethod = 'forplatform') and (LArgs[0].Kind = vkPlatform) then
    begin
      LPlatform := TNyxPlatform(LArgs[0].Ordinal);
    end
    else if (LMethod = 'whenviewport') and (LArgs[0].Kind in [vkViewportWidth, vkViewportCondition]) then
    begin
      LPresentation := Default(TNyxPresentationRef);
      LViewport := LArgs[0].ViewportCondition;

      if LArgs[0].Kind = vkViewportWidth then
      begin
        LViewport := TNyxViewportCondition.FromWidth(LArgs[0].ViewportWidth);
      end;
    end
    else if (LMethod = 'whenpresentation') and (LArgs[0].Kind = vkPresentationRef) then
    begin
      LPresentation := NyxPresentation(LArgs[0].Text);
      LViewport := TNyxViewportCondition.Any;
    end
    else
    begin
      Fail('Button configuration accepts only typed scope selectors');
    end;
  end;
  Expect(')');
  Expect('.');
  Expect('Presentation');
  LArgs := Arguments(1);

  if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkButtonPresentation) then
  begin
    Fail('Presentation requires NyxButtonPresentation');
  end;

  if FApply then
  begin
    LScope := LNode.Configure.ForPlatform(LPlatform);

    if LPresentation.Defined then
    begin
      LScope := LScope.WhenPresentation(LPresentation);
    end
    else
    begin
      LScope := LScope.WhenViewport(LViewport);
    end;
    LScope.ButtonPresentation(LArgs[0].ButtonPresentation);
  end;

  if At('.') then
  begin
    { Done returns this exact selected base scope. Continue through the ordinary
      checked method reader, so later setters retain both order and selectors. }
    Inc(FCursor);
    Expect('Done');

    if At('.') then
    begin
      ConfigureMethods(LLocal, LPlatform, LViewport, LPresentation);
      Exit;
    end;
  end;
  Expect(';');
end;
