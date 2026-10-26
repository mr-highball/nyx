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

{ Included by nyx.source. Only copied typed public descriptors are replayed.
  ConfigureNyxDataToolbar is intentionally not executable source grammar:
  replacing a linked tree would invalidate specialized child references. }
function TConfigurationReader.DataToolbarConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LComparison: TNyxQueryTextComparison;
begin
  Result := Default(TValue);

  if AName = 'nyxdatatoolbarqueryoptions' then
  begin
    Result.Kind := vkToolbarQueryOptions;
    Result.ToolbarQueryOptions := NyxDataToolbarQueryOptions;

    if At('(') then
    begin
      LArgs := Arguments(0);
    end;
    while At('.') do
    begin
      Expect('.');

      if FCursor >= Length(FTokens) then
      begin
        Fail('Expected a typed query presentation method');
      end;
      LMethod := LowerCase(FTokens[FCursor].Text);
      Inc(FCursor);
      LArgs := Arguments(1);

      if Length(LArgs) <> 1 then
      begin
        Fail('Query presentation methods require exactly one typed argument');
      end;

      if LMethod = 'display' then
      begin

        if LArgs[0].Kind <> vkToolbarQueryDisplay then
        begin
          Fail('Display requires TNyxDataToolbarQueryDisplay');
        end;
        Result.ToolbarQueryOptions := Result.ToolbarQueryOptions.Display(
          TNyxDataToolbarQueryDisplay(LArgs[0].Ordinal));
      end
      else if (LMethod = 'caption') or (LMethod = 'buttontext') then
      begin

        if LArgs[0].Kind <> vkText then
        begin
          Fail('Query presentation captions require Pascal text');
        end;

        if LMethod = 'caption' then
        begin
          Result.ToolbarQueryOptions := Result.ToolbarQueryOptions.Caption(LArgs[0].Text);
        end
        else
        begin
          Result.ToolbarQueryOptions := Result.ToolbarQueryOptions.ButtonText(LArgs[0].Text);
        end;
      end
      else
      begin
        Fail('Unknown typed query presentation method');
      end;
    end;
    Exit;
  end;

  if AName = 'tnyxdatatoolbarintent' then
  begin
    Result.Kind := vkToolbarIntent;
    Expect('.');

    if At('Disabled') then
    begin
      Inc(FCursor);

      if At('(') then
      begin
        LArgs := Arguments(0);
      end;
      Result.ToolbarIntent := TNyxDataToolbarIntent.Disabled;
      Exit;
    end;
    Expect('Configured');
    LArgs := Arguments(1);

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkToolbarOptions) then
    begin
      Fail('Configured DataToolbar intent requires typed target/field/query options');
    end;
    Result.ToolbarIntent := TNyxDataToolbarIntent.Configured(LArgs[0].ToolbarOptions);
    Exit;
  end;

  if AName = 'nyxtoolbarsort' then
  begin
    LArgs := Arguments(1);

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkText) then
    begin
      Fail('NyxToolbarSort requires one open presentation name');
    end;
    Result.Kind := vkToolbarSortRef;
    Result.Text := NyxToolbarSort(LArgs[0].Text).Name;
    Exit;
  end;
  LArgs := Arguments(2);

  if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkControlRef) or
    (LArgs[1].Kind <> vkCollectionField) or (LArgs[1].Ordinal <> Ord(nskText)) then
  begin
    Fail('NyxDataToolbarOptions requires a control reference and a typed text field');
  end;
  Result.Kind := vkToolbarOptions;
  Result.ToolbarOptions := NyxDataToolbarOptions(NyxControl(LArgs[0].Text),
    NyxTextField(LArgs[1].Text));
  while At('.') do
  begin
    Expect('.');

    if FCursor >= Length(FTokens) then
    begin
      Fail('Expected a typed DataToolbar options method');
    end;
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);

    if LMethod = 'querycontext' then
    begin
      LArgs := Arguments(1);

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkToolbarQueryOptions) then
      begin
        Fail('QueryContext requires typed query presentation options');
      end;
      Result.ToolbarOptions := Result.ToolbarOptions.QueryContext(LArgs[0].ToolbarQueryOptions);
    end
    else if LMethod = 'baseline' then
    begin
      LArgs := Arguments(1);

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkToolbarSortRef) then
      begin
        Fail('Baseline requires a typed toolbar sort reference');
      end;
      Result.ToolbarOptions := Result.ToolbarOptions.Baseline(NyxToolbarSort(LArgs[0].Text));
    end
    else if LMethod = 'searchmode' then
    begin
      LArgs := Arguments;

      if not (Length(LArgs) in [1, 2]) or (LArgs[0].Kind <> vkToolbarSearchMode) then
      begin
        Fail('SearchMode requires TNyxToolbarSearchMode and optional text comparison');
      end;
      LComparison := nqtAsciiInsensitive;

      if Length(LArgs) = 2 then
      begin

        if LArgs[1].Kind <> vkQueryTextComparison then
        begin
          Fail('SearchMode comparison requires TNyxQueryTextComparison');
        end;
        LComparison := TNyxQueryTextComparison(LArgs[1].Ordinal);
      end;
      Result.ToolbarOptions := Result.ToolbarOptions.SearchMode(
        TNyxToolbarSearchMode(LArgs[0].Ordinal), LComparison);
    end
    else if LMethod = 'sort' then
    begin
      LArgs := Arguments(2);

      if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkToolbarSortRef) then
      begin
        Fail('Sort requires a toolbar choice and a typed sort-only query');
      end;

      if LArgs[1].Kind = vkCollectionQuery then
      begin
        Result.ToolbarOptions := Result.ToolbarOptions.Sort(
          NyxToolbarSort(LArgs[0].Text), LArgs[1].CollectionQuery);
      end
      else if LArgs[1].Kind = vkCollectionSort then
      begin
        Result.ToolbarOptions := Result.ToolbarOptions.Sort(
          NyxToolbarSort(LArgs[0].Text), LArgs[1].CollectionSort);
      end
      else
      begin
        Fail('Sort requires NyxCollectionQuery ordering or NyxSort');
      end;
    end
    else
    begin
      Fail('Unknown typed DataToolbar options method');
    end;
  end;
end;
