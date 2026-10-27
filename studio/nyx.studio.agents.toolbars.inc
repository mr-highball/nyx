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

{ Included by nyx.studio.agents. The realized context is owned only for this
  read; its borrowed projection never escapes. Responses contain authored policy
  data, not query actors, editor baselines or executable runtime handles. }
function TNyxAgentSession.DataToolbarDetails(ANode: TNyxNode;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LLocal: TNyxDataToolbarIntent;
  LEffective: TNyxDataToolbarIntent;
  LIntent: TNyxDataToolbarIntent;
  LOptions: TNyxDataToolbarOptions;
  LWire: TNyxDataValue;
  LPolicy: TNyxDataValue;
  LChoice: TNyxToolbarSortChoice;
  LQuery: TNyxCollectionQuery;
  LSort: TNyxDataValue;
  LContextOptions: TNyxDataValue;
  LSorts: array of TNyxDataValue;
  LKeys: array of TNyxDataValue;
  LScope: TNyxText;
  LSortOffset: Integer;
  LSortLimit: Integer;
  LKeyOffset: Integer;
  LKeyLimit: Integer;
  LTextOffset: Integer;
  LTextLimit: Integer;
  LIndex: Integer;
  LKey: Integer;
  LCount: Integer;
  LKeyCount: Integer;
const
  CLocalModes: array[TNyxDataToolbarIntentMode] of TNyxText =
    ('inherited', 'disabled', 'configured');
  CEffectiveModes: array[TNyxDataToolbarIntentMode] of TNyxText =
    ('static', 'disabled', 'configured');

  function TextWindow(const AText: TNyxText): TNyxDataValue;
  var
    LTotal: Integer;
    LValue: TNyxText;
  begin
    LValue := TextSpan(AText, LTextOffset, LTextLimit, LTotal);
    Result := NyxObject([NyxField('value', NyxData(LValue)),
      NyxField('offset', NyxData(LTextOffset)), NyxField('totalScalars', NyxData(LTotal)),
      NyxField('truncated', NyxData((LTextOffset > 0) or
        (LTotal > LTextOffset + LTextLimit)))]);
  end;

begin
  LScope := TextArgument(AArguments, 'toolbarScope', 'effective');

  if (LScope <> 'local') and (LScope <> 'effective') then
  begin
    raise ENyxModel.Create('Toolbar scope must be local or effective');
  end;
  LSortOffset := IntegerArgument(AArguments, 'toolbarSortOffset', 0, 0, 63);
  LSortLimit := IntegerArgument(AArguments, 'toolbarSortLimit', 4, 1, 8);
  LKeyOffset := IntegerArgument(AArguments, 'toolbarKeyOffset', 0, 0, NyxMaximumQuerySorts);
  LKeyLimit := IntegerArgument(AArguments, 'toolbarKeyLimit', 4, 1, NyxMaximumQuerySorts);
  LTextOffset := IntegerArgument(AArguments, 'textOffset', 0, 0, 1000000);
  LTextLimit := IntegerArgument(AArguments, 'textLimit', 512, 1, 2048);
  LLocal := GetNyxDataToolbarIntent(ANode);
  LContext := RealizeNyxContext(FSession.Document, ANode, LProjection);
  try

    if (LProjection = nil) or (LProjection.Kind <> NyxKindName(nkDataToolbar)) then
    begin
      raise ENyxModel.Create('DataToolbar inspection requires an authored toolbar or reusable descriptor');
    end;
    LEffective := GetNyxDataToolbarIntent(LProjection);
    LIntent := LEffective;

    if LScope = 'local' then
    begin
      LIntent := LLocal;
    end;
    LPolicy := NyxNull;

    if LIntent.Mode = ntiConfigured then
    begin
      LOptions := LIntent.Options;
      LWire := LOptions.ToData;
      SetLength(LSorts, LSortLimit);
      LCount := 0;
      for LIndex := LSortOffset to LOptions.Count - 1 do
      begin

        if LCount = LSortLimit then
        begin
          Break;
        end;
        LChoice := LOptions.ChoiceAt(LIndex);
        LQuery := LChoice.Query;
        SetLength(LKeys, LKeyLimit);
        LKeyCount := 0;
        for LKey := LKeyOffset to LQuery.SortCount - 1 do
        begin

          if LKeyCount = LKeyLimit then
          begin
            Break;
          end;
          LSort := LQuery.SortAt(LKey).ToData;
          LKeys[LKeyCount] := NyxObject([NyxField('index', NyxData(LKey)),
            NyxField('field', TextWindow(LSort.Field('field').AsText)),
            NyxField('kind', LSort.Field('kind')),
            NyxField('direction', LSort.Field('direction')),
            NyxField('textComparison', LSort.Field('textComparison'))]);
          Inc(LKeyCount);
        end;
        SetLength(LKeys, LKeyCount);
        LSorts[LCount] := NyxObject([NyxField('index', NyxData(LIndex)),
          NyxField('name', TextWindow(LChoice.Reference.Name)),
          NyxField('keyTotal', NyxData(LQuery.SortCount)),
          NyxField('keyOffset', NyxData(LKeyOffset)), NyxField('keys', NyxArray(LKeys))]);
        Inc(LCount);
      end;
      SetLength(LSorts, LCount);
      LContextOptions := LOptions.QueryPresentation.ToData;
      { Presentation captions already have a public 256-scalar ceiling. All open
        identities/names above and below use exact scalar windows, never silently
        returning a truncated name as a usable reference. }
      LPolicy := NyxObject([NyxField('target', TextWindow(LOptions.Target.ID)),
        NyxField('searchField', TextWindow(LOptions.SearchField.Name)),
        NyxField('match', LWire.Field('match')), NyxField('comparison', LWire.Field('comparison')),
        NyxField('baseline', TextWindow(LOptions.BaselineChoice.Name)),
        NyxField('queryContext', LContextOptions), NyxField('sortTotal', NyxData(LOptions.Count)),
        NyxField('sortOffset', NyxData(LSortOffset)), NyxField('sorts', NyxArray(LSorts))]);
    end;
    Result := NyxObject([NyxField('scope', NyxData(LScope)),
      NyxField('localDeclared', NyxData(HasNyxDataToolbarIntent(ANode))),
      NyxField('localMode', NyxData(CLocalModes[LLocal.Mode])),
      NyxField('effectiveMode', NyxData(CEffectiveModes[LEffective.Mode])),
      NyxField('policy', LPolicy)]);
  finally
    LContext.Free;
  end;
end;
