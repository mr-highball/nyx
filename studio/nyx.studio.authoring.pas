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

unit nyx.studio.authoring;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.state,
  nyx.binding.types;

type
  { Text notation is an authoring choice, never a different stored scalar kind.
    Escaped text uses one JSON string literal so even NUL can be edited exactly
    through ordinary browser and native text widgets. }
  TNyxStudioStateInput = (ssiText, ssiEscapedText, ssiBoolean, ssiInteger, ssiNumber);
  TNyxStudioStateCommand = (sscDefault, sscRename, sscRemove);
  TNyxStudioBindingCommand = (sbcChoose, sbcClear, sbcInherit);

const
  { Explicit shell transport metadata. Controllers decode closed choices back
    into enums; open state keys stay exact data and never become widget IDs. }
  NyxStudioStateKey = 'studio-state-key';
  NyxStudioStateInputKey = 'studio-state-input';
  NyxStudioStateCommandKey = 'studio-state-command';
  NyxStudioBindingTargetKey = 'studio-binding-target';
  NyxStudioBindingCommandKey = 'studio-binding-command';
  NyxStudioBindingOwnerKey = 'studio-binding-owner';
  NyxStudioStateToggleID = 'action-state';
  NyxStudioBindingsToggleID = 'action-bindings';
  NyxStudioNewStateNameID = 'state-new-name';
  NyxStudioNewStateInputID = 'state-new-type';
  NyxStudioNewStateValueID = 'state-new-value';
  NyxStudioAddStateID = 'action-add-state';
  NyxStudioBindingTargetID = 'binding-target';
  NyxStudioBindingFlowID = 'binding-direction';

function NyxStudioStateInputName(AInput: TNyxStudioStateInput): TNyxText;
function TryNyxStudioStateInput(const AName: TNyxText;
  out AInput: TNyxStudioStateInput): Boolean;
function NyxStudioStateInputKind(AInput: TNyxStudioStateInput): TNyxStateKind;
function NyxStudioStateInputFor(const AValue: TNyxStateValue): TNyxStudioStateInput;
{ Display and parse one tagged default without locale changes or coercion.
  Wrong Boolean spelling, fractional integers, incomplete/nonfinite numbers and
  non-string escaped literals raise before any session/history mutation. }
function NyxStudioStateEditorText(const AValue: TNyxStateValue): TNyxText;
function ParseNyxStudioStateInput(AInput: TNyxStudioStateInput;
  const AText: TNyxText): TNyxStateValue;
function NyxStudioStateCommandName(ACommand: TNyxStudioStateCommand): TNyxText;
function TryNyxStudioStateCommand(const AName: TNyxText;
  out ACommand: TNyxStudioStateCommand): Boolean;
function NyxStudioBindingCommandName(ACommand: TNyxStudioBindingCommand): TNyxText;
function TryNyxStudioBindingCommand(const AName: TNyxText;
  out ACommand: TNyxStudioBindingCommand): Boolean;
function TryNyxStudioBindingTarget(const ATitle: TNyxText;
  out ATarget: TNyxBindingProperty): Boolean;
function NyxStudioBindingDirectionTitle(ADirection: TNyxBindingDirection): TNyxText;
function TryNyxStudioBindingDirection(const ATitle: TNyxText;
  out ADirection: TNyxBindingDirection): Boolean;

implementation

uses
  SysUtils,
  fpjson,
  nyx.json,
  nyx.schema;

const
  CInputs: array[TNyxStudioStateInput] of TNyxText = (
    'Text', 'Text (escaped)', 'Boolean', 'Integer', 'Number');
  CStateCommands: array[TNyxStudioStateCommand] of TNyxText = ('default', 'rename', 'remove');
  CBindingCommands: array[TNyxStudioBindingCommand] of TNyxText = ('choose', 'clear', 'inherit');

function NyxStudioStateInputName(AInput: TNyxStudioStateInput): TNyxText;
begin
  Result := CInputs[AInput];
end;

function TryNyxStudioStateInput(const AName: TNyxText;
  out AInput: TNyxStudioStateInput): Boolean;
var
  LInput: TNyxStudioStateInput;
begin
  AInput := ssiText;
  for LInput := Low(TNyxStudioStateInput) to High(TNyxStudioStateInput) do
  begin

    if CInputs[LInput] = AName then
    begin
      AInput := LInput;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxStudioStateInputKind(AInput: TNyxStudioStateInput): TNyxStateKind;
const
  CKinds: array[TNyxStudioStateInput] of TNyxStateKind = (
    nskText, nskText, nskBoolean, nskInteger, nskNumber);
begin
  Result := CKinds[AInput];
end;

function NyxStudioStateInputFor(const AValue: TNyxStateValue): TNyxStudioStateInput;
begin
  Result := ssiText;
  case AValue.Kind of
    nskText:
      begin

        if Pos(#0, AValue.TextValue) > 0 then
        begin
          Result := ssiEscapedText;
        end;
      end;
    nskBoolean:
      begin
        Result := ssiBoolean;
      end;
    nskInteger:
      begin
        Result := ssiInteger;
      end;
    nskNumber:
      begin
        Result := ssiNumber;
      end;
  end;
end;

function NyxStudioStateEditorText(const AValue: TNyxStateValue): TNyxText;
var
  LJSON: TJSONString;
begin
  Result := '';
  case AValue.Kind of
    nskText:
      begin
        Result := AValue.TextValue;

        if NyxStudioStateInputFor(AValue) = ssiEscapedText then
        begin
          LJSON := TJSONString.Create(AValue.TextValue);
          try
            Result := LJSON.AsJSON;
          finally
            LJSON.Free;
          end;
        end;
      end;
    nskBoolean:
      begin
        Result := 'false';

        if AValue.BooleanValue then
        begin
          Result := 'true';
        end;
      end;
    nskInteger:
      begin
        Result := IntToStr(AValue.IntegerValue);
      end;
    nskNumber:
      begin
        Result := AValue.NumberText;
      end;
  end;
end;

function ParseNyxStudioStateInput(AInput: TNyxStudioStateInput;
  const AText: TNyxText): TNyxStateValue;
var
  LJSON: TJSONData;
  LInteger: Integer;
  LNumber: Double;
begin
  Result := TNyxStateValue.FromText('');
  case AInput of
    ssiText:
      begin
        Result := TNyxStateValue.FromText(AText);
      end;
    ssiEscapedText:
      begin
        LJSON := DecodeNyxJSON(AText);
        try

          if LJSON.JSONType <> jtString then
          begin
            raise ENyxState.Create('Escaped text requires one quoted string');
          end;
          Result := TNyxStateValue.FromText(LJSON.AsString);
        finally
          LJSON.Free;
        end;
      end;
    ssiBoolean:
      begin

        if (AText <> 'true') and (AText <> 'false') then
        begin
          raise ENyxState.Create('Boolean default requires true or false');
        end;
        Result := TNyxStateValue.FromBoolean(AText = 'true');
      end;
    ssiInteger:
      begin

        if not TryNyxInteger(AText, LInteger) then
        begin
          raise ENyxState.Create('Integer default requires a whole signed 32-bit value');
        end;
        Result := TNyxStateValue.FromInteger(LInteger);
      end;
    ssiNumber:
      begin

        if not TryNyxStateNumber(AText, LNumber) then
        begin
          raise ENyxState.Create('Number default requires a complete finite decimal');
        end;
        Result := TNyxStateValue.FromNumber(LNumber);
      end;
  end;
end;

function NyxStudioStateCommandName(ACommand: TNyxStudioStateCommand): TNyxText;
begin
  Result := CStateCommands[ACommand];
end;

function TryNyxStudioStateCommand(const AName: TNyxText;
  out ACommand: TNyxStudioStateCommand): Boolean;
var
  LCommand: TNyxStudioStateCommand;
begin
  ACommand := sscDefault;
  for LCommand := Low(TNyxStudioStateCommand) to High(TNyxStudioStateCommand) do
  begin

    if CStateCommands[LCommand] = AName then
    begin
      ACommand := LCommand;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxStudioBindingCommandName(ACommand: TNyxStudioBindingCommand): TNyxText;
begin
  Result := CBindingCommands[ACommand];
end;

function TryNyxStudioBindingCommand(const AName: TNyxText;
  out ACommand: TNyxStudioBindingCommand): Boolean;
var
  LCommand: TNyxStudioBindingCommand;
begin
  ACommand := sbcChoose;
  for LCommand := Low(TNyxStudioBindingCommand) to High(TNyxStudioBindingCommand) do
  begin

    if CBindingCommands[LCommand] = AName then
    begin
      ACommand := LCommand;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TryNyxStudioBindingTarget(const ATitle: TNyxText;
  out ATarget: TNyxBindingProperty): Boolean;
var
  LTarget: TNyxBindingProperty;
begin
  ATarget := bpValue;
  for LTarget := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
  begin

    if NyxBindingPropertyTitle(LTarget) = ATitle then
    begin
      ATarget := LTarget;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NyxStudioBindingDirectionTitle(ADirection: TNyxBindingDirection): TNyxText;
const
  CTitles: array[TNyxBindingDirection] of TNyxText = ('State to control', 'Read and write');
begin
  Result := CTitles[ADirection];
end;

function TryNyxStudioBindingDirection(const ATitle: TNyxText;
  out ADirection: TNyxBindingDirection): Boolean;
var
  LDirection: TNyxBindingDirection;
begin
  ADirection := bdFromState;
  for LDirection := Low(TNyxBindingDirection) to High(TNyxBindingDirection) do
  begin

    if NyxStudioBindingDirectionTitle(LDirection) = ATitle then
    begin
      ADirection := LDirection;
      Exit(True);
    end;
  end;
  Result := False;
end;

end.
