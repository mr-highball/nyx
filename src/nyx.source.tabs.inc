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

{ Included by nyx.source. The reader evaluates public immutable values only.
  It never enters callbacks, a renderer or an arbitrary application function. }
function TConfigurationReader.TabsOptions: TValue;
var
  LMethod: TNyxText;
  LArgs: TValues;
begin
  Result := Default(TValue);
  Result.Kind := vkTabsOptions;
  Expect('.');
  Expect('Defaults');
  Result.TabsOptions := TNyxTabsOptions.Defaults;

  if At('(') then
  begin
    LArgs := Arguments;

    if Length(LArgs) <> 0 then
    begin
      Fail('Tab Defaults takes no arguments');
    end;
  end;
  while At('.') do
  begin
    Expect('.');

    if FCursor >= Length(FTokens) then
    begin
      Fail('Expected a typed tab policy method');
    end;
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := Arguments(1);

    if Length(LArgs) <> 1 then
    begin
      Fail('Tab policy methods require one typed argument');
    end;

    if (LMethod = 'initial') and (LArgs[0].Kind = vkControlRef) then
    begin
      Result.TabsOptions := Result.TabsOptions.Initial(NyxControl(LArgs[0].Text));
    end
    else if (LMethod = 'activation') and (LArgs[0].Kind = vkTabActivation) then
    begin
      Result.TabsOptions := Result.TabsOptions.Activation(TNyxTabActivation(LArgs[0].Ordinal));
    end
    else if (LMethod = 'orientation') and (LArgs[0].Kind = vkTabOrientation) then
    begin
      Result.TabsOptions := Result.TabsOptions.Orientation(TNyxTabOrientation(LArgs[0].Ordinal));
    end
    else if (LMethod = 'wrap') and (LArgs[0].Kind = vkBoolean) then
    begin
      Result.TabsOptions := Result.TabsOptions.Wrap(LArgs[0].Ordinal <> 0);
    end
    else
    begin
      Fail('Wrong tab policy method or argument family');
    end;
  end;
end;
