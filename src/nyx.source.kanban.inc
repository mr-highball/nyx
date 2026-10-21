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

{ Typed automatic-movement grammar. Values are built through public immutable
  factories; this reader never executes configuration or attachment routines. }
function TConfigurationReader.KanbanConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
begin
  Result := Default(TValue);

  if AName = 'tnyxkanbanintent' then
  begin
    Result.Kind := vkKanbanIntent;
    Expect('.');

    if At('Disabled') then
    begin
      Inc(FCursor);

      if At('(') then
      begin
        LArgs := Arguments(0);
      end;
      Result.KanbanIntent := TNyxKanbanIntent.Disabled;
      Exit;
    end;
    Expect('Configured');
    LArgs := Arguments(1);

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkKanbanOptions) then
    begin
      Fail('Configured Kanban intent requires complete typed options');
    end;
    Result.KanbanIntent := TNyxKanbanIntent.Configured(LArgs[0].KanbanOptions);
    Exit;
  end;

  if AName = 'nyxkanbanlane' then
  begin
    LArgs := Arguments(1);

    if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkText) then
    begin
      Fail('NyxKanbanLane requires one open lane name');
    end;
    Result.Kind := vkKanbanLane;
    Result.Text := NyxKanbanLane(LArgs[0].Text).Name;
    Exit;
  end;
  LArgs := Arguments(3);

  if (Length(LArgs) <> 3) or (LArgs[0].Kind <> vkCollectionKey) or
    (LArgs[1].Kind <> vkCollectionField) or (LArgs[1].Ordinal <> Ord(nskText)) or
    (LArgs[2].Kind <> vkCollectionField) or (LArgs[2].Ordinal <> Ord(nskText)) then
  begin
    Fail('NyxKanbanOptions requires a collection and distinct typed text fields');
  end;
  Result.Kind := vkKanbanOptions;
  Result.KanbanOptions := NyxKanbanOptions(NyxCollection(LArgs[0].Text),
    NyxTextField(LArgs[1].Text), NyxTextField(LArgs[2].Text));
  while At('.') do
  begin
    Expect('.');

    if FCursor >= Length(FTokens) then
    begin
      Fail('Expected a typed Kanban options method');
    end;
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);

    if LMethod = 'lane' then
    begin
      LArgs := Arguments(2);

      if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkKanbanLane) or
        (LArgs[1].Kind <> vkText) then
      begin
        Fail('Kanban Lane requires a lane reference and caption');
      end;
      Result.KanbanOptions := Result.KanbanOptions.Lane(
        NyxKanbanLane(LArgs[0].Text), LArgs[1].Text);
    end
    else if LMethod = 'scoped' then
    begin
      LArgs := Arguments(1);

      if (Length(LArgs) <> 1) or (LArgs[0].Kind <> vkCollectionScope) then
      begin
        Fail('Kanban Scoped requires a typed collection scope');
      end;
      Result.KanbanOptions := Result.KanbanOptions.Scoped(
        TNyxCollectionScope(LArgs[0].Ordinal));
    end
    else
    begin
      Fail('Unknown typed Kanban options method');
    end;
  end;
end;
