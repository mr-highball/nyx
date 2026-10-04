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

unit nyx.studio.commands;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.types,
  nyx.text,
  nyx.model,
  nyx.studio.session;

{ Portable routing of Nyx shell events into undoable typed authoring commands.
  Nodes/session are borrowed; keys are exact transport data and closed metadata
  is decoded into enums before mutation. False means the event belongs to
  presentation/another command. Rejection raises and preserves accepted history.
  A value-binding choice requires the current shell root's typed flow field. }
function RouteNyxStudioAuthoring(ASession: TNyxStudioSession; ANode: TNyxNode;
  AEvent: TNyxTrigger; AShellRoot: TNyxNode = nil): Boolean;

{ Source commands share one portable controller boundary. Target adapters feed
  normal Nyx code-editor/button events; rejected drafts retain accepted history. }
function RouteNyxStudioSource(ASession: TNyxStudioSession; ANode: TNyxNode;
  AEvent: TNyxTrigger): Boolean;
{ Current inspector fields carry admitted schema keys at the transport boundary.
  Both adapters use the same undoable property command and paired source check. }
function RouteNyxStudioProperty(ASession: TNyxStudioSession; ANode: TNyxNode;
  AEvent: TNyxTrigger): Boolean;

implementation

uses
  nyx.state,
  nyx.binding.types,
  nyx.studio.authoring,
  nyx.studio.collections;

function RouteNyxStudioSource(ASession: TNyxStudioSession; ANode: TNyxNode;
  AEvent: TNyxTrigger): Boolean;
type
  TSourceCommand = (scApply, scRestore);
var
  LCommand: TSourceCommand;
begin
  Result := False;

  if (ASession = nil) or (ANode = nil) then
  begin
    raise ENyxModel.Create('Source authoring requires a session and event source');
  end;

  if (AEvent = ntChange) and (ANode.ID = 'studio-code') then
  begin
    ASession.SetSourceDraft(ANode.Prop('value'));
    Exit(True);
  end;

  if AEvent <> ntClick then
  begin
    Exit;
  end;

  if ANode.ID = 'action-apply-source' then
  begin
    LCommand := scApply;
  end
  else if ANode.ID = 'action-reset-source' then
  begin
    LCommand := scRestore;
  end
  else
  begin
    Exit;
  end;
  case LCommand of
    scApply: ASession.ApplySourceDraft;
    scRestore: ASession.DiscardSourceDraft;
  end;
  Result := True;
end;

function RouteNyxStudioProperty(ASession: TNyxStudioSession; ANode: TNyxNode;
  AEvent: TNyxTrigger): Boolean;
begin
  Result := False;

  if (ASession = nil) or (ANode = nil) then
  begin
    raise ENyxModel.Create('Property authoring requires a session and event source');
  end;

  if (AEvent = ntChange) and (ANode.Prop('prop-key') <> '') then
  begin
    ASession.SetProperty(ANode.Prop('prop-key'), ANode.Prop('value'));
    Result := True;
  end;
end;

function RouteNyxStudioAuthoring(ASession: TNyxStudioSession; ANode: TNyxNode;
  AEvent: TNyxTrigger; AShellRoot: TNyxNode): Boolean;
var
  LStateCommand: TNyxStudioStateCommand;
  LBindingCommand: TNyxStudioBindingCommand;
  LInput: TNyxStudioStateInput;
  LTarget: TNyxBindingProperty;
  LDirection: TNyxBindingDirection;
  LValue: TNyxStateValue;
  LFlow: TNyxNode;
  LProjection: TNyxNode;
  LSpec: TNyxBindingSpec;
begin
  Result := False;

  if (ASession = nil) or (ANode = nil) then
  begin
    raise ENyxModel.Create('Authoring requires a session and event source');
  end;

  if RouteNyxStudioCollection(ASession, ANode, AEvent) then
  begin
    Exit(True);
  end;

  if ANode.Prop(NyxStudioStateCommandKey) <> '' then
  begin

    if not TryNyxStudioStateCommand(ANode.Prop(NyxStudioStateCommandKey), LStateCommand) then
    begin
      raise ENyxState.Create('Unknown state authoring command');
    end;
    case LStateCommand of
      sscDefault:
        begin

          if AEvent <> ntChange then
          begin
            Exit;
          end;

          if not TryNyxStudioStateInput(ANode.Prop(NyxStudioStateInputKey), LInput) then
          begin
            raise ENyxState.Create('Unknown state input type');
          end;
          LValue := ASession.Document.State.Value(ANode.Prop(NyxStudioStateKey));

          if NyxStudioStateInputKind(LInput) <> LValue.Kind then
          begin
            raise ENyxState.Create('State default editor cannot change its declared type');
          end;
          LValue := ParseNyxStudioStateInput(LInput, ANode.Prop('value'));
          ASession.SetStateValues([NyxStateAssign(ANode.Prop(NyxStudioStateKey), LValue)]);
        end;
      sscRename:
        begin

          if AEvent <> ntChange then
          begin
            Exit;
          end;
          ASession.RenameState(ANode.Prop(NyxStudioStateKey), ANode.Prop('value'));
        end;
      sscRemove:
        begin

          if AEvent <> ntClick then
          begin
            Exit;
          end;
          ASession.RemoveState(ANode.Prop(NyxStudioStateKey));
        end;
    end;
    Exit(True);
  end;

  if (AEvent = ntClick) and (ANode.ID = NyxStudioAddStateID) then
  begin

    if (AShellRoot = nil) or (AShellRoot.Find(NyxStudioNewStateNameID) = nil) or
      (AShellRoot.Find(NyxStudioNewStateInputID) = nil) or
      (AShellRoot.Find(NyxStudioNewStateValueID) = nil) then
    begin
      raise ENyxState.Create('New default requires its current authoring form');
    end;

    if not TryNyxStudioStateInput(AShellRoot.Find(NyxStudioNewStateInputID).Prop('value'), LInput) then
    begin
      raise ENyxState.Create('Unknown new default type');
    end;
    LValue := ParseNyxStudioStateInput(LInput,
      AShellRoot.Find(NyxStudioNewStateValueID).Prop('value'));
    ASession.CreateState(AShellRoot.Find(NyxStudioNewStateNameID).Prop('value'), LValue);
    Exit(True);
  end;

  if (AEvent = ntChange) and (ANode.ID = NyxStudioBindingFlowID) then
  begin

    if ANode.Prop(NyxStudioBindingOwnerKey) <> ASession.SelectedID then
    begin
      raise ENyxState.Create('Binding selection changed; use its current inspector');
    end;

    if not TryNyxStudioBindingDirection(ANode.Prop('value'), LDirection) then
    begin
      raise ENyxState.Create('Unknown binding flow');
    end;
    LProjection := ASession.SelectedProjection;
    try

      if (LProjection <> nil) and LProjection.FindBinding(bpValue, LSpec) then
      begin
        ASession.SetBinding(TNyxBindingSpec.Bound(bpValue, LSpec.StateName,
          LSpec.ValueKind, LDirection));
        Result := True;
      end;
    finally
      LProjection.Free;
    end;
    Exit;
  end;

  if (AEvent <> ntClick) or (ANode.Prop(NyxStudioBindingCommandKey) = '') then
  begin
    Exit;
  end;

  if ANode.Prop(NyxStudioBindingOwnerKey) <> ASession.SelectedID then
  begin
    raise ENyxState.Create('Binding selection changed; use its current inspector');
  end;

  if not TryNyxStudioBindingCommand(ANode.Prop(NyxStudioBindingCommandKey), LBindingCommand) or
    not TryNyxBindingProperty(ANode.Prop(NyxStudioBindingTargetKey), LTarget) then
  begin
    raise ENyxState.Create('Unknown binding authoring command');
  end;
  case LBindingCommand of
    sbcChoose:
      begin
        LValue := ASession.Document.State.Value(ANode.Prop(NyxStudioStateKey));
        LDirection := bdFromState;

        if LTarget = bpValue then
        begin
          LFlow := nil;

          if AShellRoot <> nil then
          begin
            LFlow := AShellRoot.Find(NyxStudioBindingFlowID);
          end;

          if (LFlow = nil) or
            not TryNyxStudioBindingDirection(LFlow.Prop('value'), LDirection) then
          begin
            raise ENyxState.Create('Value binding requires an admitted flow choice');
          end;
        end;
        ASession.SetBinding(TNyxBindingSpec.Bound(LTarget, ANode.Prop(NyxStudioStateKey),
          LValue.Kind, LDirection));
      end;
    sbcClear:
      begin
        ASession.SetBinding(TNyxBindingSpec.Clear(LTarget));
      end;
    sbcInherit:
      begin
        ASession.InheritBinding(LTarget);
      end;
  end;
  Result := True;
end;

end.
