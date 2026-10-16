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
{ Included by nyx.canvas.editor. This record retains data, never a live form. }

function TNyxCanvasPrecisionDraft.GetDefined: Boolean;
begin
  Result := FEditor <> '';
end;

procedure TNyxCanvasPrecisionDraft.Clear;
begin
  FEditor := '';
  FContext := '';
  FValues := Default(TNyxDataValue);
end;

function TNyxCanvasPrecisionDraft.ToData: TNyxDataValue;
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
    NyxField('values', LValues)]);
end;

class function TNyxCanvasPrecisionDraft.FromData(
  const AData: TNyxDataValue): TNyxCanvasPrecisionDraft;
var
  LContext: TNyxDataValue;
  LDefinition: INyxCanvasDefinition;
  LIndex: Integer;
begin
  Result := Default(TNyxCanvasPrecisionDraft);

  if (AData.Kind <> ndObject) or (AData.Count <> 4) or
    (AData.Field('version').AsInteger <> 1) or
    (NyxUTF8ByteCount(AData.ToJSON) > 2 * MaximumNyxCanvasDefinitionBytes) then
  begin
    raise ENyxModel.Create('Precision draft requires its bounded copied input packet');
  end;
  Result.FEditor := AData.Field('editor').AsText;
  Result.FContext := AData.Field('context').AsText;
  Result.FValues := AData.Field('values').Copy;

  if not Result.Defined then
  begin

    if (Result.FContext <> '') or (Result.FValues.Kind <> ndNull) then
    begin
      raise ENyxModel.Create('An empty precision draft cannot carry fields');
    end;
    Exit;
  end;
  LContext := TNyxDataValue.ParseJSON(Result.FContext);

  if (LContext.Kind <> ndObject) or (LContext.Count <> 2) or
    (LContext.Field('scene').AsText = '') or
    (Result.FValues.Kind <> ndObject) or (Result.FValues.Count <> 18) then
  begin
    raise ENyxModel.Create('Precision draft context or fields are invalid');
  end;
  LDefinition := NyxCanvasDefinitionFromData(LContext.Field('definition'));

  if not LDefinition.Camera.Defined then
  begin
    raise ENyxModel.Create('Precision draft requires an admitted camera');
  end;
  for LIndex := 0 to High(CPrecisionKeys) do
  begin

    if Result.FValues.Field(CPrecisionKeys[LIndex]).Kind <> ndText then
    begin
      raise ENyxModel.Create('Precision draft requires eighteen text fields');
    end;
  end;
end;

procedure TNyxCanvasPrecisionDraft.Capture(const AEditor: TNyxText; ARoot: TNyxNode);
var
  LForm: TNyxNode;
  LDefinition: INyxCanvasDefinition;
  LContext: TNyxDataValue;
  LValues: TNyxDataValue;
  LCandidate: TNyxCanvasPrecisionDraft;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AEditor);

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CPrecisionEditor)) then
  begin
    Exit;
  end;
  LContext := LForm.Extensions.Value(NyxExtension(CPrecisionEditor));
  LValues := PrecisionValues(LForm);
  LDefinition := NyxCanvasDefinitionFromData(LContext.Field('definition'));

  if PrecisionDefaults(LDefinition).ToJSON = LValues.ToJSON then
  begin

    if (FEditor = AEditor) and (FContext = LContext.ToJSON) then
    begin
      Clear;
    end;
    Exit;
  end;
  LCandidate := Default(TNyxCanvasPrecisionDraft);
  LCandidate.FEditor := AEditor;
  LCandidate.FContext := LContext.ToJSON;
  LCandidate.FValues := LValues;

  if NyxUTF8ByteCount(LCandidate.ToData.ToJSON) > 2 * MaximumNyxCanvasDefinitionBytes then
  begin
    raise ENyxModel.Create('Precision draft exceeds its copied input budget');
  end;
  Self := LCandidate;
end;

function TNyxCanvasPrecisionDraft.Restore(ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LContext: TNyxDataValue;
  LPrevious: TNyxDataValue;
  LDefinition: INyxCanvasDefinition;
  LProposed: INyxCanvasDefinition;
  LNode: TNyxNode;
  LIndex: Integer;
begin
  Result := False;

  if not Defined or (ARoot = nil) then
  begin
    Exit;
  end;
  LForm := ARoot.Find(FEditor);

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CPrecisionEditor)) then
  begin
    Exit;
  end;
  LContext := LForm.Extensions.Value(NyxExtension(CPrecisionEditor));

  if LContext.ToJSON <> FContext then
  begin
    LPrevious := TNyxDataValue.ParseJSON(FContext);

    if LPrevious.Field('scene').AsText = LContext.Field('scene').AsText then
    begin
      LDefinition := NyxCanvasDefinitionFromData(LPrevious.Field('definition'));
      { Consume only this exact completed proposal. Unrelated scene changes or
        invalid unfinished numbers park the copied draft instead of discarding it. }
      try
        LProposed := PrecisionProposal(LDefinition, FValues);

        if LProposed.ToData.ToJSON = LContext.Field('definition').ToJSON then
        begin
          Clear;
        end;
      except
        on ENyxModel do
        begin
          { Unfinished text is valid draft data. }
        end;
        on ENyxCanvasPrecision do
        begin
          { Invalid unfinished settings remain parked in their exact context. }
        end;
        on ENyxCanvasScene do
        begin
          { A partially edited guide identity is retained as text, not admitted. }
        end;
        on ENyxCanvasGeometry do
        begin
          { Invalid extents remain available in the original exact context. }
        end;
      end;
    end;
    Exit;
  end;
  for LIndex := 0 to High(CPrecisionFields) do
  begin
    LNode := LForm.Find(NyxCanvasPrecisionPart(FEditor, CPrecisionFields[LIndex]).ID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Precision fields changed within their captured context');
    end;
    LNode.Configure.Value(FValues.Field(CPrecisionKeys[LIndex]).AsText).Done;
  end;
  Result := True;
end;
