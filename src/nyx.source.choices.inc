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
{ Strict literal choice replay. Only immutable public key/list builders are
  interpreted. No provider callbacks, widget constructors or arbitrary Pascal
  expressions execute while reconstructing an independently owned candidate. }
function TConfigurationReader.ChoiceConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LItems: INyxChoiceItems;
  LMethod: TNyxText;
begin
  Result := Default(TValue);

  if AName = 'nyxchoice' then
  begin
    LArgs := Arguments(1);

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkText) then
    begin
      Fail('NyxChoice requires exact text, including an explicit empty value');
    end;
    Result.Kind := vkChoiceRef;
    Result.Text := NyxChoice(LArgs[0].Text).Value;
    Exit;
  end;

  if At('(') then
  begin
    LArgs := Arguments;

    if Length(LArgs) <> 0 then
    begin
      Fail('Literal NewNyxChoices starts empty and uses fluent WithChoice rows');
    end;
  end;
  LItems := NewNyxChoices;
  while At('.') do
  begin
    Inc(FCursor);
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);

    if LMethod <> 'withchoice' then
    begin
      Fail('An immutable choice list supports WithChoice only');
    end;
    LArgs := Arguments(2);

    if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkChoiceRef) or
      (LArgs[1].Kind <> vkText) then
    begin
      Fail('WithChoice requires a typed NyxChoice key and exact caption text');
    end;
    LItems := LItems.WithChoice(NyxChoice(LArgs[0].Text), LArgs[1].Text);
  end;
  Result.Kind := vkChoiceItems;
  Result.Data := LItems.ToData;
end;

procedure TConfigurationReader.ChoiceAttachment;
var
  LLocal: Integer;
  LValue: TValue;
begin
  Expect('SetNyxChoiceItems');
  Expect('(');

  if FCursor >= Length(FTokens) then
  begin
    Fail('Choice attachment requires an already owned control local');
  end;
  LLocal := LocalIndex(FTokens[FCursor].Text);

  if (LLocal < 0) or not FLocals[LLocal].Admitted then
  begin
    Fail('Choice attachment requires an already owned control local');
  end;
  Inc(FCursor);
  Expect(',');
  LValue := Expression;

  if LValue.Kind <> vkChoiceItems then
  begin
    Fail('Choice attachment requires immutable typed choice items');
  end;
  Expect(')');
  Expect(';');

  if FApply then
  begin
    AdmittedNode(LLocal).SetChoiceItems(NyxChoicesFromData(LValue.Data));
  end;
end;
