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

{ Included by nyx.source. Typed selection choices are replayed without calling
  arbitrary application code or accepting raw string substitutes for enums. }
function TConfigurationReader.SelectionOptions: TValue;
var
  LMethod: TNyxText;
  LArgs: TValues;
begin
  Result := Default(TValue);
  Result.Kind := vkSelectionOptions;
  Expect('.');
  Expect('Defaults');
  Result.SelectionOptions := TNyxSelectionOptions.Defaults;

  if At('(') then
  begin
    LArgs := Arguments;

    if Length(LArgs) <> 0 then
    begin
      Fail('Selection Defaults takes no arguments');
    end;
  end;
  while At('.') do
  begin
    Expect('.');

    if FCursor >= Length(FTokens) then
    begin
      Fail('Expected a typed selection policy method');
    end;
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments(1);

    if Length(LArgs) <> 1 then
    begin
      Fail('Selection methods require one typed argument');
    end;

    if (LMethod = 'styling') and (LArgs[0].Kind = vkSelectionStyling) then
    begin
      Result.SelectionOptions := Result.SelectionOptions.Styling(
        TNyxSelectionStyling(LArgs[0].Ordinal));
    end
    else if (LMethod = 'selected') and (LArgs[0].Kind = vkVariant) then
    begin
      Result.SelectionOptions := Result.SelectionOptions.Selected(TNyxVariant(LArgs[0].Ordinal));
    end
    else if (LMethod = 'unselected') and (LArgs[0].Kind = vkVariant) then
    begin
      Result.SelectionOptions := Result.SelectionOptions.Unselected(TNyxVariant(LArgs[0].Ordinal));
    end
    else
    begin
      Fail('Wrong selection policy method or argument family');
    end;
  end;
end;
