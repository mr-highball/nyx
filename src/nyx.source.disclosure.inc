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

{ Included by nyx.source. Only public immutable policy methods are evaluated;
  part references stay distinct from display/application text. }
function TConfigurationReader.DisclosureOptions: TValue;
var
  LMethod: TNyxText;
  LArgs: TValues;
begin
  Result := Default(TValue);
  Result.Kind := vkDisclosureOptions;
  Expect('.');
  Expect('Defaults');
  Result.DisclosureOptions := TNyxDisclosureOptions.Defaults;

  if At('(') then
  begin
    LArgs := Arguments;

    if Length(LArgs) <> 0 then
    begin
      Fail('Disclosure Defaults takes no arguments');
    end;
  end;
  while At('.') do
  begin
    Expect('.');

    if FCursor >= Length(FTokens) then
    begin
      Fail('Expected a typed disclosure policy method');
    end;
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments(1);

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkPart) then
    begin
      Fail('Disclosure methods require one distinct part reference');
    end;

    if LMethod = 'header' then
    begin
      Result.DisclosureOptions := Result.DisclosureOptions.Header(NyxPart(LArgs[0].Text));
    end
    else if LMethod = 'body' then
    begin
      Result.DisclosureOptions := Result.DisclosureOptions.Body(NyxPart(LArgs[0].Text));
    end
    else
    begin
      Fail('Unknown typed disclosure policy method');
    end;
  end;
end;
