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


{ Included by nyx.source.pas. Public resource expressions are evaluated into
  immutable copied values; document paths and executable file imports refuse. }

function ResourceLocale(const AName: TNyxText): TNyxLocaleRef;
begin

  if AName = '' then
  begin
    Exit(NyxDefaultLocale);
  end;
  Result := NyxLocale(AName);
end;

function TConfigurationReader.ResourceConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LDefinition: INyxResourceDefinition;
  LLabels: TNyxResourceLabels;
  LDiscoveryAvailable: Boolean;

  function IntegerValue(const AValue: TValue): Integer;
  begin

    if (AValue.Kind <> vkInteger) or not TryNyxStateInteger(AValue.Text, Result) then
    begin
      Fail('Resource operation requires a signed 32-bit integer');
    end;
  end;

begin
  Result := Default(TValue);

  if AName = 'nyxresourcecache' then
  begin
    Result.Kind := vkResourceCache;
    Result.ResourceCache := NyxResourceCache;
    while At('.') do
    begin
      Inc(FCursor);

      if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
      begin
        Fail('Expected a fluent cache policy choice');
      end;
      LMethod := LowerCase(FTokens[FCursor].Text);
      Inc(FCursor);
      LArgs := nil;

      if At('(') then
      begin
        LArgs := Arguments(1);
      end;

      if (LMethod = 'bypass') and (Length(LArgs) = 0) then
      begin
        Result.ResourceCache := Result.ResourceCache.Bypass;
      end
      else if (LMethod = 'memory') and (Length(LArgs) = 0) then
      begin
        Result.ResourceCache := Result.ResourceCache.Memory;
      end
      else if (LMethod = 'persistent') and (Length(LArgs) = 0) then
      begin
        Result.ResourceCache := Result.ResourceCache.Persistent;
      end
      else if (Length(LArgs) = 1) and (LArgs[0].Kind = vkInteger) and (LMethod = 'freshfor') then
      begin
        Result.ResourceCache := Result.ResourceCache.FreshFor(IntegerValue(LArgs[0]));
      end
      else if (Length(LArgs) = 1) and (LArgs[0].Kind = vkInteger) and (LMethod = 'stalefor') then
      begin
        Result.ResourceCache := Result.ResourceCache.StaleFor(IntegerValue(LArgs[0]));
      end
      else if (Length(LArgs) = 1) and (LArgs[0].Kind = vkInteger) and (LMethod = 'maximumbytes') then
      begin
        Result.ResourceCache := Result.ResourceCache.MaximumBytes(IntegerValue(LArgs[0]));
      end
      else if (Length(LArgs) = 1) and (LArgs[0].Kind = vkResourceServerPolicy) and
        (LMethod = 'serverpolicy') then
      begin
        Result.ResourceCache := Result.ResourceCache.ServerPolicy(
          TNyxResourceServerPolicy(LArgs[0].Ordinal));
      end
      else
      begin
        Fail('Cache policies require typed choices and bounded durations/bytes');
      end;
    end;
    Exit;
  end;
  LArgs := nil;

  if (AName <> 'nyxresourcelabels') or At('(') then
  begin
    LArgs := Arguments(2);
  end;

  if AName = 'nyxresourcelabels' then
  begin

    if Length(LArgs) <> 0 then
    begin
      Fail('Resource labels start with an empty typed set');
    end;
    LLabels := NyxResourceLabels;
    while At('.') do
    begin
      Inc(FCursor);
      LMethod := LowerCase(FTokens[FCursor].Text);
      Inc(FCursor);
      LArgs := Arguments(1);

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkResourceLabel) then
      begin
        Fail('Resource label sets require typed label references');
      end;

      if LMethod = 'add' then
      begin
        LLabels := LLabels.Add(NyxResourceLabel(LArgs[0].Text));
      end
      else if LMethod = 'remove' then
      begin
        LLabels := LLabels.Remove(NyxResourceLabel(LArgs[0].Text));
      end
      else
      begin
        Fail('Resource label sets support Add/Remove');
      end;
    end;
    Result.Kind := vkResourceLabels;
    Result.Data := LLabels.ToData;
    Exit;
  end;

  if AName = 'nyxresourcediscovery' then
  begin

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkResourceDefinition) then
    begin
      Fail('Resource discovery requires an admitted typed definition');
    end;
    LDefinition := NyxResourceFromData(LArgs[0].ResourceDefinitionData);
  end
  else if AName = 'nyxhostedresource' then
  begin

    if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkResourceKind) or
      (LArgs[1].Kind <> vkResourceURL) then
    begin
      Fail('Hosted resources require a file-kind enum and typed URL');
    end;
    LDefinition := NyxHostedResource(TNyxResourceKind(LArgs[0].Ordinal),
      NyxResourceURL(LArgs[1].Text));
  end
  else
  begin

    if Length(LArgs) <> 1 then
    begin
      Fail('A resource constructor requires one typed payload or reference');
    end;

    if AName = 'nyxresourceurl' then
    begin

      if LArgs[0].Kind <> vkText then
      begin
        Fail('NyxResourceURL requires exact URI text');
      end;
      Result.Kind := vkResourceURL;
      Result.Text := NyxResourceURL(LArgs[0].Text).Address;
      Exit;
    end;

    if AName = 'nyxresourceimage' then
    begin

      if LArgs[0].Kind <> vkResourceRef then
      begin
        Fail('NyxResourceImage requires a typed resource reference');
      end;
      Result.Kind := vkResourceImage;
      Result.ResourceImage := NyxResourceImage(NyxResourceRef(LArgs[0].Text));
      while At('.') do
      begin
        Inc(FCursor);

        if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) or
          not SameText(FTokens[FCursor].Text, 'Localize') then
        begin
          Fail('Image resources support only typed Localize');
        end;
        Inc(FCursor);
        LArgs := Arguments(2);

        if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkResourceLocale) or
          (LArgs[1].Kind <> vkResourceLocale) then
        begin
          Fail('Image Localize requires two typed locale references');
        end;
        Result.ResourceImage := Result.ResourceImage.Localize(
          ResourceLocale(LArgs[0].Text), ResourceLocale(LArgs[1].Text));
      end;
      Exit;
    end;

    if AName = 'nyxresourcevalue' then
    begin

      if LArgs[0].Kind <> vkResourceRef then
      begin
        Fail('NyxResourceValue requires a typed resource reference');
      end;
      Result.Kind := vkResourceValue;
      Result.ResourceValue := NyxResourceValue(NyxResourceRef(LArgs[0].Text));
      while At('.') do
      begin
        Inc(FCursor);

        if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
        begin
          Fail('Expected a fluent resource selector');
        end;
        LMethod := LowerCase(FTokens[FCursor].Text);
        Inc(FCursor);
        LArgs := nil;

        if At('(') then
        begin
          LArgs := Arguments(2);
        end;

        if (LMethod = 'field') and (Length(LArgs) = 1) and (LArgs[0].Kind = vkText) then
        begin
          Result.ResourceValue := Result.ResourceValue.Field(LArgs[0].Text);
        end
        else if (LMethod = 'item') and (Length(LArgs) = 1) and (LArgs[0].Kind = vkInteger) then
        begin
          Result.ResourceValue := Result.ResourceValue.Item(IntegerValue(LArgs[0]));
        end
        else if (LMethod = 'localize') and (Length(LArgs) = 2) and
          (LArgs[0].Kind = vkResourceLocale) and (LArgs[1].Kind = vkResourceLocale) then
        begin
          Result.ResourceValue := Result.ResourceValue.Localize(
            ResourceLocale(LArgs[0].Text), ResourceLocale(LArgs[1].Text));
        end
        else if Length(LArgs) = 0 then
        begin

          if LMethod = 'astext' then
          begin
            Result.ResourceValue := Result.ResourceValue.AsText;
          end
          else if LMethod = 'asboolean' then
          begin
            Result.ResourceValue := Result.ResourceValue.AsBoolean;
          end
          else if LMethod = 'asinteger' then
          begin
            Result.ResourceValue := Result.ResourceValue.AsInteger;
          end
          else if LMethod = 'asnumber' then
          begin
            Result.ResourceValue := Result.ResourceValue.AsNumber;
          end
          else
          begin
            Fail('Unknown resource scalar selector ' + LMethod);
          end;
        end
        else
        begin
          Fail('Resource selectors require exact field/item/locale arguments');
        end;
      end;
      Exit;
    end;

    if (AName = 'nyxresourceref') or (AName = 'nyxlocale') or (AName = 'nyxresourcelabel') or
      (AName = 'nyxdecodebase64') or (AName = 'nyxtextresource') then
    begin

      if LArgs[0].Kind <> vkText then
      begin
        Fail('Resource names, text and Base64 require exact text data');
      end;

      if AName = 'nyxresourceref' then
      begin
        Result.Kind := vkResourceRef;
        Result.Text := NyxResourceRef(LArgs[0].Text).Name;
        Exit;
      end;

      if AName = 'nyxlocale' then
      begin
        Result.Kind := vkResourceLocale;
        Result.Text := NyxLocale(LArgs[0].Text).Name;
        Exit;
      end;

      if AName = 'nyxresourcelabel' then
      begin
        Result.Kind := vkResourceLabel;
        Result.Text := NyxResourceLabel(LArgs[0].Text).Name;
        Exit;
      end;

      if AName = 'nyxdecodebase64' then
      begin
        Result.Kind := vkBytes;
        Result.Bytes := NyxDecodeBase64(LArgs[0].Text);
        Exit;
      end;
      LDefinition := NyxTextResource(LArgs[0].Text);
    end
    else if AName = 'nyxjsonresource' then
    begin

      if LArgs[0].Kind = vkText then
      begin
        LDefinition := NyxJSONResource(LArgs[0].Text);
      end
      else if LArgs[0].Kind = vkData then
      begin
        LDefinition := NyxJSONResource(LArgs[0].Data);
      end
      else
      begin
        Fail('A JSON resource requires UTF-8 file text or typed data');
      end;
    end
    else if AName = 'nyximageresource' then
    begin

      if LArgs[0].Kind <> vkImageSource then
      begin
        Fail('An image resource requires a typed embedded source');
      end;
      LDefinition := NyxImageResource(LArgs[0].ImageSource);
    end
    else
    begin

      if (AName <> 'nyxbinaryresource') or (LArgs[0].Kind <> vkBytes) then
      begin
        Fail('A binary resource requires typed decoded bytes');
      end;
      LDefinition := NyxBinaryResource(LArgs[0].Bytes);
    end;
  end;
  Result.Kind := vkResourceDefinition;
  { Factory/adaptation results expose discovery. Base-interface configuration
    keeps its existing return contract; a later label call must explicitly use
    NyxResourceDiscovery, just as actual Pascal compilation requires. }
  LDiscoveryAvailable := True;
  while At('.') do
  begin
    Inc(FCursor);
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments(2);

    if (LMethod = 'cache') and (Length(LArgs) = 1) and (LArgs[0].Kind = vkResourceCache) then
    begin
      LDefinition := LDefinition.Cache(LArgs[0].ResourceCache);
      LDiscoveryAvailable := False;
    end
    else if (LMethod = 'fallback') and (Length(LArgs) = 1) and
      (LArgs[0].Kind = vkResourceDefinition) then
    begin
      LDefinition := LDefinition.Fallback(NyxResourceFromData(LArgs[0].ResourceDefinitionData));
      LDiscoveryAvailable := False;
    end
    else if (LMethod = 'tagged') and (Length(LArgs) = 1) and
      (LArgs[0].Kind = vkResourceLabel) and LDiscoveryAvailable then
    begin
      LDefinition := NyxResourceDiscovery(LDefinition).Tagged(NyxResourceLabel(LArgs[0].Text));
    end
    else if (LMethod = 'withlabels') and (Length(LArgs) = 1) and
      (LArgs[0].Kind = vkResourceLabels) and LDiscoveryAvailable then
    begin
      LDefinition := NyxResourceDiscovery(LDefinition).WithLabels(
        TNyxResourceLabels.FromData(LArgs[0].Data));
    end
    else if (LMethod = 'describe') and (Length(LArgs) = 2) and
      (LArgs[0].Kind = vkText) and (LArgs[1].Kind = vkText) then
    begin
      LDefinition := LDefinition.Describe(LArgs[0].Text, LArgs[1].Text);
      LDiscoveryAvailable := False;
    end
    else
    begin
      Fail('Resource configuration requires typed Cache/Fallback/labels or exact Describe text');
    end;
  end;
  Result.ResourceDefinitionData := LDefinition.ToData;
end;

function TConfigurationReader.ResourceRowsConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LPath: TNyxResourcePath;
  LFamily: TNyxStateKind;
  LIndex: Integer;
begin
  Result := Default(TValue);

  if AName = 'nyxresourcerows' then
  begin
    LArgs := Arguments(1);

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkResourceRef) then
    begin
      Fail('Resource rows require a typed resource reference');
    end;
    Result.Kind := vkResourceRows;
    Result.ResourceRows := NyxResourceRows(NyxResourceRef(LArgs[0].Text));
  end
  else
  begin
    Result.Kind := vkResourcePath;
    Result.ResourcePath := NyxResourcePath;
  end;
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a typed resource row or path operation');
    end;
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments(2);

    if (LMethod = 'field') and (Length(LArgs) = 1) and (LArgs[0].Kind = vkText) then
    begin

      if Result.Kind = vkResourcePath then
      begin
        Result.ResourcePath := Result.ResourcePath.Field(LArgs[0].Text);
      end
      else
      begin
        Result.ResourceRows := Result.ResourceRows.Field(LArgs[0].Text);
      end;
    end
    else if (LMethod = 'item') and (Length(LArgs) = 1) and
      (LArgs[0].Kind = vkInteger) and TryNyxStateInteger(LArgs[0].Text, LIndex) then
    begin

      if Result.Kind = vkResourcePath then
      begin
        Result.ResourcePath := Result.ResourcePath.Item(LIndex);
      end
      else
      begin
        Result.ResourceRows := Result.ResourceRows.Item(LIndex);
      end;
    end
    else if (Result.Kind = vkResourceRows) and (LMethod = 'identity') and
      (Length(LArgs) = 1) and (LArgs[0].Kind = vkResourcePath) then
    begin
      Result.ResourceRows := Result.ResourceRows.Identity(LArgs[0].ResourcePath);
    end
    else if (Result.Kind = vkResourceRows) and (Length(LArgs) in [1, 2]) and
      (LArgs[0].Kind = vkCollectionField) then
    begin
      LFamily := TNyxStateKind(LArgs[0].Ordinal);

      if LMethod <> LowerCase(NyxStateKindName(LFamily)) then
      begin
        Fail('Resource row field method must match its strong Pascal family');
      end;
      LPath := NyxResourcePath.Field(LArgs[0].Text);

      if Length(LArgs) = 2 then
      begin

        if LArgs[1].Kind <> vkResourcePath then
        begin
          Fail('Resource row field requires a typed structural path');
        end;
        LPath := LArgs[1].ResourcePath;
      end;
      case LFamily of
        nskText:
          begin
            Result.ResourceRows := Result.ResourceRows.Text(NyxTextField(LArgs[0].Text), LPath);
          end;
        nskBoolean:
          begin
            Result.ResourceRows := Result.ResourceRows.Boolean(NyxBooleanField(LArgs[0].Text), LPath);
          end;
        nskInteger:
          begin
            Result.ResourceRows := Result.ResourceRows.Integer(NyxIntegerField(LArgs[0].Text), LPath);
          end;
        nskNumber:
          begin
            Result.ResourceRows := Result.ResourceRows.Number(NyxNumberField(LArgs[0].Text), LPath);
          end;
      end;
    end
    else
    begin
      Fail('Unsupported resource row/path operation or argument family');
    end;
  end;
end;

procedure TConfigurationReader.ResourceCollectionDefaults;
var
  LArgs: TValues;
  LKey: TNyxCollectionRef;
begin
  Inc(FCursor, 3);
  Expect('.');
  Expect('Define');
  LArgs := Arguments(2);
  Expect(';');

  if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkCollectionKey) or
    (LArgs[1].Kind <> vkResourceRows) then
  begin
    Fail('ResourceCollections.Define requires a typed collection key and row recipe');
  end;
  LArgs[1].ResourceRows.Validate;

  if FApply then
  begin
    LKey := NyxCollection(LArgs[0].Text);

    if FDocument.Collections.Has(LKey) then
    begin
      Fail('Define each static or resource-backed collection once in the managed builder');
    end;
    FDocument.ResourceCollections.Define(LKey, LArgs[1].ResourceRows);
  end;
end;

procedure TConfigurationReader.ResourceDefaults;
var
  LArgs: TValues;
  LReference: TNyxResourceRef;
  LLocale: TNyxLocaleRef;
  LDefinitionIndex: Integer;
begin
  Inc(FCursor, 3);
  repeat
    Expect('.');
    Expect('Define');
    LArgs := Arguments(3);

    if not (Length(LArgs) in [2, 3]) or (LArgs[0].Kind <> vkResourceRef) then
    begin
      Fail('Resources.Define requires an exact typed reference and definition');
    end;
    LDefinitionIndex := High(LArgs);
    LReference := NyxResourceRef(LArgs[0].Text);
    LLocale := NyxDefaultLocale;

    if Length(LArgs) = 3 then
    begin

      if LArgs[1].Kind <> vkResourceLocale then
      begin
        Fail('A localized resource requires a typed locale reference');
      end;
      LLocale := NyxLocale(LArgs[1].Text);
    end;

    if LArgs[LDefinitionIndex].Kind <> vkResourceDefinition then
    begin
      Fail('Resources.Define requires a managed immutable resource definition');
    end;

    if FApply then
    begin

      if FDocument.Resources.Contains(LReference, LLocale) then
      begin
        Fail('Define each exact resource/locale once in the managed builder');
      end;
      FDocument.Resources.Define(LReference, LLocale,
        NyxResourceFromData(LArgs[LDefinitionIndex].ResourceDefinitionData));
    end;
  until not At('.');
  Expect(';');
end;
