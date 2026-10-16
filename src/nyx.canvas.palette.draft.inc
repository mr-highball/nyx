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
{ Included by nyx.canvas.palette. Only disposable copied form data is retained. }

function TNyxCanvasPaletteDraft.GetDefined: Boolean;
begin
  Result := FEditor <> '';
end;

function TNyxCanvasPaletteDraft.ToData: TNyxDataValue;
var
  LValues: TNyxDataValue;
begin
  LValues := NyxNull;

  if Defined then
  begin
    LValues := FValues.Copy;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('editor', NyxData(FEditor)), NyxField('context', NyxData(FContext)),
    NyxField('values', LValues), NyxField('expanded', NyxData(FExpanded))]);
end;

class function TNyxCanvasPaletteDraft.FromData(const AData: TNyxDataValue): TNyxCanvasPaletteDraft;
var
  LPacket: TNyxDataValue;
  LIndex: Integer;
  LKey: TNyxText;
  LGroup: TNyxPaletteGroup;
begin
  Result := Default(TNyxCanvasPaletteDraft);

  if (AData.Kind <> ndObject) or (AData.Count <> 5) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Canvas draft requires its exact copied input packet');
  end;
  Result.FEditor := AData.Field('editor').AsText;
  Result.FContext := AData.Field('context').AsText;
  Result.FValues := AData.Field('values').Copy;
  Result.FExpanded := AData.Field('expanded').AsBoolean;

  if not Result.Defined then
  begin

    if (Result.FContext <> '') or (Result.FValues.Kind <> ndNull) or Result.FExpanded then
    begin
      raise ENyxModel.Create('An empty canvas draft cannot carry retained fields');
    end;
    Exit;
  end;

  if (NyxUTF8ByteCount(Result.FContext) > MaximumNyxCanvasDefinitionBytes) or
    (Result.FValues.Kind <> ndObject) or (Result.FValues.Count <> 10) then
  begin
    raise ENyxModel.Create('Canvas draft exceeds its supported form boundary');
  end;
  LPacket := TNyxDataValue.ParseJSON(Result.FContext);

  if (LPacket.Kind <> ndObject) or (LPacket.Count <> 6) or
    (LPacket.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Canvas draft context is not a palette snapshot');
  end;
  for LIndex := 0 to Result.FValues.Count - 1 do
  begin
    LKey := Result.FValues.Key(LIndex);

    if (Pos(Result.FEditor + '-', LKey) <> 1) or
      (Pos('|', Copy(LKey, Length(Result.FEditor) + 2, MaxInt)) > 0) or
      (Pos('|' + Copy(LKey, Length(Result.FEditor) + 2, MaxInt) + '|',
        '|search|group|choice|item|scene|layer|left|top|width|height|') = 0) or
      (Result.FValues.Field(LKey).Kind <> ndText) then
    begin
      raise ENyxModel.Create('Canvas draft contains an unrelated or non-text field');
    end;
  end;

  if not TryNyxPaletteGroup(Result.FValues.Field(Result.FEditor + '-group').AsText, LGroup) then
  begin
    raise ENyxModel.Create('Canvas draft has an unknown component group');
  end;
end;

procedure TNyxCanvasPaletteDraft.Clear;
begin
  FEditor := '';
  FContext := '';
  FValues := Default(TNyxDataValue);
  FExpanded := False;
end;

procedure TNyxCanvasPaletteDraft.Capture(const AEditor: TNyxText; ARoot: TNyxNode);
var
  LForm: TNyxNode;
  LFields: array of TNyxDataField;
  LCount: Integer;

  procedure Collect(ANode: TNyxNode);
  var
    LIndex: Integer;
  begin

    if IsPaletteField(LForm, ANode) then
    begin
      LCount := Length(LFields);
      SetLength(LFields, LCount + 1);
      LFields[LCount] := NyxField(ANode.ID, NyxData(ANode.Prop(NyxAttributeName(atValue))));
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Collect(ANode.Children[LIndex]);
    end;
  end;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AEditor);

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CPalette)) then
  begin
    Exit;
  end;
  LFields := nil;
  Collect(LForm);
  FEditor := AEditor;
  FContext := Packet(LForm).ToJSON;
  FValues := NyxObject(LFields);
  FExpanded := NyxDisclosureExpanded(LForm.Find(LForm.ID + '-placement'));
end;

function TNyxCanvasPaletteDraft.Restore(ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LNode: TNyxNode;
  LIndex: Integer;
begin
  Result := False;

  if (FEditor = '') or (ARoot = nil) then
  begin
    Exit;
  end;
  LForm := ARoot.Find(FEditor);

  if LForm = nil then
  begin
    Exit;
  end;

  if not LForm.Extensions.Has(NyxExtension(CPalette)) or (Packet(LForm).ToJSON <> FContext) then
  begin
    Clear;
    Exit;
  end;
  for LIndex := 0 to FValues.Count - 1 do
  begin
    LNode := LForm.Find(FValues.Key(LIndex));

    if LNode = nil then
    begin
      raise ENyxModel.Create('Canvas palette field changed within its captured context');
    end;
    LNode.Configure.Value(FValues.Field(FValues.Key(LIndex)).AsText).Done;
  end;
  Filter(LForm);
  LForm.Find(LForm.ID + '-placement').Configure.Value(FExpanded).Done;
  Result := True;
end;
