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

{ Included by nyx.studio.agents. The temporary realized context is owned here;
  returned rows contain detached text windows and no runtime/input handles. }
function TNyxAgentSession.ChoiceDetails(ANode: TNyxNode;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LChoices: INyxChoiceItems;
  LItem: TNyxChoiceItem;
  LScope: TNyxText;
  LOffset: Integer;
  LLimit: Integer;
  LTextOffset: Integer;
  LTextLimit: Integer;
  LTotal: Integer;
  LIndex: Integer;
  LCount: Integer;
  LItems: array of TNyxDataValue;

  function TextWindow(const AText: TNyxText): TNyxDataValue;
  var
    LScalars: Integer;
    LValue: TNyxText;
  begin
    LValue := TextSpan(AText, LTextOffset, LTextLimit, LScalars);
    Result := NyxObject([NyxField('value', NyxData(LValue)),
      NyxField('offset', NyxData(LTextOffset)),
      NyxField('totalScalars', NyxData(LScalars)),
      NyxField('truncated', NyxData((LTextOffset > 0) or
        (LScalars > LTextOffset + LTextLimit)))]);
  end;

begin
  LScope := TextArgument(AArguments, 'choiceScope', 'effective');

  if (LScope <> 'local') and (LScope <> 'effective') then
  begin
    raise ENyxModel.Create('Choice scope must be local or effective');
  end;
  LOffset := IntegerArgument(AArguments, 'choiceOffset', 0, 0, NyxMaximumChoiceItems);
  LLimit := IntegerArgument(AArguments, 'choiceLimit', 8, 1, 16);
  LTextOffset := IntegerArgument(AArguments, 'textOffset', 0, 0, 1000000);
  LTextLimit := IntegerArgument(AArguments, 'textLimit', 80, 1, 2048);
  LContext := RealizeNyxContext(FSession.Document, ANode, LProjection);
  try

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkSelect)) then
    begin
      raise ENyxModel.Create('Choice inspection requires an authored select or reusable select descriptor');
    end;
    LChoices := LProjection.ChoiceItems;

    if LScope = 'local' then
    begin
      LChoices := ANode.ChoiceItems;
    end;
    LTotal := 0;

    if LChoices <> nil then
    begin
      LTotal := LChoices.Count;
    end;
    SetLength(LItems, LLimit);
    LCount := 0;
    for LIndex := LOffset to LTotal - 1 do
    begin

      if LCount = LLimit then
      begin
        Break;
      end;
      LItem := LChoices.Item(LIndex);
      LItems[LCount] := NyxObject([NyxField('index', NyxData(LIndex)),
        NyxField('key', TextWindow(LItem.Key.Value)),
        NyxField('caption', TextWindow(LItem.Caption))]);
      Inc(LCount);
    end;
    SetLength(LItems, LCount);
    Result := NyxObject([NyxField('scope', NyxData(LScope)),
      NyxField('localDeclared', NyxData(ANode.HasChoiceItems)),
      NyxField('defined', NyxData(LChoices <> nil)),
      NyxField('total', NyxData(LTotal)), NyxField('offset', NyxData(LOffset)),
      NyxField('items', NyxArray(LItems))]);
  finally
    LContext.Free;
  end;
end;
