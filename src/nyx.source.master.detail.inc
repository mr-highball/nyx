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

{ Included by nyx.source. These copied typed factories retain authored field
  identity without executing runtime attachment or arbitrary Pascal code. }
function TConfigurationReader.MasterDetailConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
begin
  Result := Default(TValue);

  if AName = 'tnyxmasterdetailintent' then
  begin
    Result.Kind := vkMasterDetailIntent;
    Expect('.');

    if At('Disabled') then
    begin
      Inc(FCursor);

      if At('(') then
      begin
        LArgs := Arguments(0);
      end;
      Result.MasterDetailIntent := TNyxMasterDetailIntent.Disabled;
      Exit;
    end;
    Expect('Configured');
    LArgs := Arguments(1);

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkMasterDetailOptions) then
    begin
      Fail('Configured MasterDetail intent requires typed field options');
    end;
    Result.MasterDetailIntent := TNyxMasterDetailIntent.Configured(LArgs[0].MasterDetailOptions);
    Exit;
  end;
  LArgs := Arguments(2);

  if (Length(LArgs) <> 2) or
    (LArgs[0].Kind <> vkCollectionField) or (LArgs[0].Ordinal <> Ord(nskText)) or
    (LArgs[1].Kind <> vkCollectionField) or (LArgs[1].Ordinal <> Ord(nskText)) then
  begin
    Fail('NyxMasterDetailOptions requires two typed text field references');
  end;
  Result.Kind := vkMasterDetailOptions;
  Result.MasterDetailOptions := NyxMasterDetailOptions(
    NyxTextField(LArgs[0].Text), NyxTextField(LArgs[1].Text));
end;
