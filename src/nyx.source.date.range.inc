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

{ Included by nyx.source. Only the two public immutable factories are admitted;
  source reconstruction never evaluates a caller's arbitrary Pascal function. }
function TConfigurationReader.DateRangeOptions: TValue;
var
  LMethod: TNyxText;
  LArgs: TValues;
begin
  Result := Default(TValue);
  Result.Kind := vkDateRangeOptions;
  Expect('.');

  if FCursor >= Length(FTokens) then
  begin
    Fail('Expected a typed date-range options factory');
  end;
  LMethod := LowerCase(FTokens[FCursor].Text);
  Inc(FCursor);

  if LMethod = 'ordered' then
  begin
    Result.DateRangeOptions := TNyxDateRangeOptions.Ordered;
  end
  else if LMethod = 'independent' then
  begin
    Result.DateRangeOptions := TNyxDateRangeOptions.Independent;
  end
  else
  begin
    Fail('Date-range options require Ordered or Independent');
  end;

  if At('(') then
  begin
    LArgs := Arguments(0);

    if Length(LArgs) <> 0 then
    begin
      Fail('Date-range options factories take no arguments');
    end;
  end;
end;
