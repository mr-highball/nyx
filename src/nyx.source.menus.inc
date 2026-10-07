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

{ Private declarative menu grammar. Every expression calls the public typed
  immutable builder; source admission never creates a target host or executes a
  command. The ordinary expression-depth/argument budgets also bound this path. }

function TConfigurationReader.MenuConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LIndex: Integer;
  LChecked: Boolean;
  LEnabled: Boolean;
  LDismissals: TNyxPopoverDismissals;
  LDefinition: INyxMenuDefinition;

  procedure Require(ACount: Integer; AKind: TValueKind);
  begin

    if (Length(LArgs) <> ACount) or (LArgs[0].Kind <> AKind) then
    begin
      Fail('Wrong typed argument for menu expression ' + LMethod);
    end;
  end;

  function IntegerAt(AIndex: Integer): Integer;
  begin

    if (AIndex >= Length(LArgs)) or (LArgs[AIndex].Kind <> vkInteger) or
      not TryNyxStateInteger(LArgs[AIndex].Text, Result) then
    begin
      Fail('Menu dimensions/timing require portable Integer values');
    end;
  end;

begin
  Result := Default(TValue);
  LMethod := AName;

  if SameText(AName, 'NyxTypeAhead') then
  begin
    LArgs := nil;

    if At('(') then
    begin
      LArgs := Arguments;
    end;

    if Length(LArgs) <> 0 then
    begin
      Fail('NyxTypeAhead takes no arguments');
    end;
    Result.Kind := vkTypeAheadOptions;
    Result.TypeAheadOptions := NyxTypeAhead;
  end
  else
  begin
    LArgs := Arguments;

    if SameText(AName, 'NewNyxMenuDefinition') then
    begin
      Require(2, vkRootRef);

      if LArgs[1].Kind <> vkMenuOptions then
      begin
        Fail('NewNyxMenuDefinition requires a typed menu policy');
      end;
      Result.Kind := vkMenuDefinition;
      LDefinition := NewNyxMenuDefinition(LArgs[0].RootRef,
        LArgs[1].MenuOptions);
    end
    else
    begin
      Require(1, vkText);

      if SameText(AName, 'NyxMenu') then
      begin
        Result.Kind := vkMenuOptions;
        Result.MenuOptions := NyxMenu(LArgs[0].Text);
      end
      else
      begin
        Result.Kind := vkPopoverOptions;
        Result.PopoverOptions := NyxPopover(LArgs[0].Text);
      end;
    end;
  end;
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected an immutable menu builder method');
    end;
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);

    if (Result.Kind = vkPopoverOptions) and (LMethod = 'dismisson') then
    begin
      Expect('(');
      LArgs := ArrayArguments(2);
      Expect(')');
      LDismissals := [];
      for LIndex := 0 to High(LArgs) do
      begin

        if LArgs[LIndex].Kind <> vkPopoverDismissal then
        begin
          Fail('DismissOn requires TNyxPopoverDismissal enum members');
        end;
        Include(LDismissals, TNyxPopoverDismissal(LArgs[LIndex].Ordinal));
      end;
      Result.PopoverOptions := Result.PopoverOptions.DismissOn(LDismissals);
      Continue;
    end;
    { Radio has five distinct typed arguments; keep the ordinary three-argument
      configuration bound everywhere outside this immutable item builder. }
    LArgs := Arguments(5);
    case Result.Kind of
      vkTypeAheadOptions:
        begin

          if LMethod = 'enabled' then
          begin
            Require(1, vkBoolean);
            Result.TypeAheadOptions := Result.TypeAheadOptions.Enabled(LArgs[0].Ordinal <> 0);
          end
          else if LMethod = 'windowmilliseconds' then
          begin
            Require(1, vkInteger);
            Result.TypeAheadOptions := Result.TypeAheadOptions.WindowMilliseconds(IntegerAt(0));
          end
          else if LMethod = 'match' then
          begin
            Require(1, vkTypeAheadMatch);
            Result.TypeAheadOptions := Result.TypeAheadOptions.Match(
              TNyxTypeAheadMatch(LArgs[0].Ordinal));
          end
          else
          begin
            Fail('Unsupported typeahead builder method');
          end;
        end;
      vkPopoverOptions:
        begin

          if LMethod = 'placement' then
          begin

            if (Length(LArgs) < 1) or (Length(LArgs) > 2) or
              (LArgs[0].Kind <> vkPopoverSide) then
            begin
              Fail('Placement requires side/alignment enums');
            end;
            Result.PopoverOptions := Result.PopoverOptions.Placement(
              TNyxPopoverSide(LArgs[0].Ordinal));

            if Length(LArgs) = 2 then
            begin

              if LArgs[1].Kind <> vkPopoverAlignment then
              begin
                Fail('Placement alignment requires its own enum');
              end;
              Result.PopoverOptions := Result.PopoverOptions.Placement(
                TNyxPopoverSide(LArgs[0].Ordinal), TNyxPopoverAlignment(LArgs[1].Ordinal));
            end;
          end
          else if (LMethod = 'size') or (LMethod = 'spacing') then
          begin
            Require(2, vkInteger);

            if LMethod = 'size' then
            begin
              Result.PopoverOptions := Result.PopoverOptions.Size(IntegerAt(0), IntegerAt(1));
            end
            else
            begin
              Result.PopoverOptions := Result.PopoverOptions.Spacing(IntegerAt(0), IntegerAt(1));
            end;
          end
          else if LMethod = 'sizing' then
          begin
            Require(1, vkPopoverSizing);
            Result.PopoverOptions := Result.PopoverOptions.Sizing(
              TNyxPopoverSizing(LArgs[0].Ordinal));
          end
          else if LMethod = 'focus' then
          begin
            Require(1, vkPart);
            Result.PopoverOptions := Result.PopoverOptions.Focus(NyxPart(LArgs[0].Text));
          end
          else
          begin
            Fail('Unsupported popover builder method');
          end;
        end;
      vkMenuOptions:
        begin

          if LMethod = 'opening' then
          begin
            Require(1, vkMenuOpening);
            Result.MenuOptions := Result.MenuOptions.Opening(TNyxMenuOpening(LArgs[0].Ordinal));
          end
          else if LMethod = 'wrap' then
          begin
            Require(1, vkBoolean);
            Result.MenuOptions := Result.MenuOptions.Wrap(LArgs[0].Ordinal <> 0);
          end
          else if LMethod = 'typeahead' then
          begin
            Require(1, vkTypeAheadOptions);
            Result.MenuOptions := Result.MenuOptions.TypeAhead(LArgs[0].TypeAheadOptions);
          end
          else if LMethod = 'presentation' then
          begin
            Require(1, vkPopoverOptions);
            Result.MenuOptions := Result.MenuOptions.Presentation(LArgs[0].PopoverOptions);
          end
          else
          begin
            Fail('Unsupported menu policy method');
          end;
        end;
      vkMenuDefinition:
        begin

          if (Length(LArgs) = 0) or (LArgs[0].Kind <> vkPart) then
          begin
            Fail('A menu item requires a typed named part');
          end;
          LChecked := False;
          LEnabled := True;

          if (LMethod = 'action') or (LMethod = 'submenu') then
          begin

            if (Length(LArgs) < 2) or (Length(LArgs) > 3) then
            begin
              Fail('Action/Submenu requires a part, reference and optional enable flag');
            end;

            if Length(LArgs) = 3 then
            begin

              if LArgs[2].Kind <> vkBoolean then
              begin
                Fail('Menu enable flag requires Boolean');
              end;
              LEnabled := LArgs[2].Ordinal <> 0;
            end;

            if (LMethod = 'action') and (LArgs[1].Kind = vkMenuCommand) then
            begin
              LDefinition := LDefinition.Action(NyxPart(LArgs[0].Text),
                NyxMenuCommand(LArgs[1].Text), LEnabled);
            end
            else if (LMethod = 'submenu') and (LArgs[1].Kind = vkMenuRef) then
            begin
              LDefinition := LDefinition.Submenu(NyxPart(LArgs[0].Text),
                NyxMenuRef(LArgs[1].Text), LEnabled);
            end
            else
            begin
              Fail('Menu command and submenu references are distinct types');
            end;
          end
          else if (LMethod = 'check') or (LMethod = 'radio') then
          begin
            LIndex := 2;

            if LMethod = 'radio' then
            begin
              LIndex := 3;
            end;

            if (Length(LArgs) < LIndex + 1) or (Length(LArgs) > LIndex + 2) or
              (LArgs[1].Kind <> vkMenuCommand) or (LArgs[LIndex].Kind <> vkBoolean) then
            begin
              Fail('Check/Radio requires a command and typed Boolean defaults');
            end;
            LChecked := LArgs[LIndex].Ordinal <> 0;

            if Length(LArgs) = LIndex + 2 then
            begin

              if LArgs[LIndex + 1].Kind <> vkBoolean then
              begin
                Fail('Menu enable flag requires Boolean');
              end;
              LEnabled := LArgs[LIndex + 1].Ordinal <> 0;
            end;

            if LMethod = 'check' then
            begin
              LDefinition := LDefinition.Check(NyxPart(LArgs[0].Text),
                NyxMenuCommand(LArgs[1].Text), LChecked, LEnabled);
            end
            else
            begin

              if LArgs[2].Kind <> vkMenuGroup then
              begin
                Fail('Radio requires a distinct menu group reference');
              end;
              LDefinition := LDefinition.Radio(NyxPart(LArgs[0].Text),
                NyxMenuCommand(LArgs[1].Text), NyxMenuGroup(LArgs[2].Text), LChecked, LEnabled);
            end;
          end
          else if LMethod = 'separator' then
          begin
            Require(1, vkPart);
            LDefinition := LDefinition.Separator(NyxPart(LArgs[0].Text));
          end
          else
          begin
            Fail('Unsupported menu definition method');
          end;
        end;
      else
        begin
          Fail('Menu expression lost its typed builder family');
        end;
    end;
  end;

  if Result.Kind = vkMenuDefinition then
  begin
    Result.Ordinal := Length(FMenuDefinitions);
    SetLength(FMenuDefinitions, Result.Ordinal + 1);
    FMenuDefinitions[Result.Ordinal] := LDefinition;
  end;
end;

procedure TConfigurationReader.MenuDefaults;
var
  LArgs: TValues;
begin
  Inc(FCursor, 3);
  repeat
    Expect('.');
    Expect('Define');
    LArgs := Arguments;

    if (Length(LArgs) <> 2) or (LArgs[0].Kind <> vkMenuRef) or
      (LArgs[1].Kind <> vkMenuDefinition) then
    begin
      Fail('Menus.Define requires a typed reference and immutable definition');
    end;

    if FApply then
    begin

      if FDocument.Menus.Contains(NyxMenuRef(LArgs[0].Text)) then
      begin
        Fail('Define each menu reference once in the managed builder');
      end;
      FDocument.Menus.Define(NyxMenuRef(LArgs[0].Text),
        FMenuDefinitions[LArgs[1].Ordinal]);
    end;
  until not At('.');
  Expect(';');
end;
