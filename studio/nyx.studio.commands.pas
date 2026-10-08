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

type
  { Capture never admits a pair or retains a node. Presentation-only flow and
    name events are consumed separately from immutable queued document intent. }
  TNyxStudioAuthoringCapture = (sacNone, sacNameDraft, sacPresentation, sacEdit);

{ Decode the shared Project/Bindings controls into copied, typed intent.
  Pending descriptors qualify rapid consecutive flow edits before publication.
  Partial scalar input remains text until the independent processor parses it. }
function CaptureNyxStudioAuthoring(ASession: TNyxStudioSession; ANode: TNyxNode;
  AEvent: TNyxTrigger; AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): TNyxStudioAuthoringCapture;

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
  nyx.studio.collections,
  nyx.theme.editor,
  nyx.image.editor;

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

function CaptureNyxStudioAuthoring(ASession: TNyxStudioSession; ANode: TNyxNode;
  AEvent: TNyxTrigger; AShellRoot: TNyxNode; const APending: TNyxStudioPendingDesign;
  out AEdit: TNyxStudioDesignEdit): TNyxStudioAuthoringCapture;
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
  LInherit: Boolean;
  LThemeChange: TNyxThemeEditorChange;
  LImageChange: TNyxImageEditorChange;
begin
  Result := sacNone;
  AEdit := Default(TNyxStudioDesignEdit);

  if (ASession = nil) or (ANode = nil) then
  begin
    raise ENyxModel.Create('Authoring requires a session and event source');
  end;

  AEdit.Selection := ASession.SelectedID;
  AEdit.View := ASession.ActiveViewID;

  if (AEvent = ntClick) and CaptureNyxImageEditor(ANode, AShellRoot, LImageChange) then
  begin
    LProjection := ASession.SelectedProjection;
    try

      if (LImageChange.Owner <> ASession.SelectedID) or
        (NyxImageEditorBaseline(ASession.Selected, LProjection) <> LImageChange.Baseline) then
      begin
        raise ENyxModel.Create('Image changed; review the current picture before applying');
      end;
    finally
      LProjection.Free;
    end;
    AEdit.Action := sdaImage;
    AEdit.Image := LImageChange;
    Exit(sacEdit);
  end;

  if (AEvent = ntClick) and CaptureNyxThemeEditor(ANode, AShellRoot, LThemeChange) then
  begin

    if NyxThemeEditorBaseline(ASession.Document) <> LThemeChange.Baseline then
    begin
      raise ENyxModel.Create('Theme changed; review the current palette before applying');
    end;
    AEdit.Action := sdaTheme;
    AEdit.Theme := LThemeChange.Tokens;
    AEdit.ThemeReset := LThemeChange.Reset;
    AEdit.ThemeBaseline := LThemeChange.Baseline;
    Exit(sacEdit);
  end;

  if ANode.Prop(NyxStudioStateCommandKey) <> '' then
  begin

    if not TryNyxStudioStateCommand(ANode.Prop(NyxStudioStateCommandKey), LStateCommand) then
    begin
      raise ENyxState.Create('Unknown state authoring command');
    end;

    if not TryNyxStudioStateInput(ANode.Prop(NyxStudioStateInputKey), LInput) then
    begin
      raise ENyxState.Create('Unknown state input type');
    end;
    AEdit.Name := ANode.Prop(NyxStudioStateKey);
    AEdit.StateInput := LInput;
    AEdit.Value := ANode.Prop('value');
    case LStateCommand of
      sscDefault:
        begin

          if AEvent <> ntChange then
          begin
            Exit;
          end;

          AEdit.Action := sdaSetStateDefault;
        end;
      sscRename:
        begin

          if AEvent <> ntClick then
          begin
            Exit;
          end;
          LFlow := nil;

          if AShellRoot <> nil then
          begin
            LFlow := AShellRoot.Find(ANode.Prop(NyxStudioStateNameInputKey));
          end;

          if (LFlow = nil) or (LFlow.Prop(NyxStudioStateKey) <> AEdit.Name) then
          begin
            raise ENyxState.Create('Rename requires its current exact state name field');
          end;
          AEdit.Action := sdaRenameStateDefault;
          AEdit.Value := LFlow.Prop('value');
        end;
      sscRemove:
        begin

          if AEvent <> ntClick then
          begin
            Exit;
          end;
          AEdit.Action := sdaRemoveStateDefault;
        end;
      sscRenameDraft:
        begin

          if AEvent = ntChange then
          begin
            Exit(sacNameDraft);
          end;
          Exit;
        end;
    end;
    Exit(sacEdit);
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
    AEdit.Action := sdaCreateStateDefault;
    AEdit.StateInput := LInput;
    AEdit.Name := AShellRoot.Find(NyxStudioNewStateNameID).Prop('value');
    AEdit.Value := AShellRoot.Find(NyxStudioNewStateValueID).Prop('value');
    Exit(sacEdit);
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
    if APending.Binding(AEdit.Selection, bpValue, LSpec, LInherit) then
    begin

      if LInherit then
      begin
        raise ENyxState.Create('Wait for inherited binding admission before changing its flow');
      end;
    end
    else
    begin
      LSpec := TNyxBindingSpec.Clear(bpValue);
      LProjection := ASession.SelectedProjection;
      try

        if LProjection <> nil then
        begin
          LProjection.FindBinding(bpValue, LSpec);
        end;
      finally
        LProjection.Free;
      end;
    end;

    if LSpec.Cleared then
    begin
      Exit(sacPresentation);
    end;
    AEdit.Action := sdaSetBinding;
    AEdit.Binding := TNyxBindingSpec.Bound(bpValue, LSpec.StateName,
      LSpec.ValueKind, LDirection);
    Exit(sacEdit);
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
        AEdit.Action := sdaSetBinding;
        AEdit.Binding := TNyxBindingSpec.Bound(LTarget, ANode.Prop(NyxStudioStateKey),
          LValue.Kind, LDirection);
      end;
    sbcClear:
      begin
        AEdit.Action := sdaSetBinding;
        AEdit.Binding := TNyxBindingSpec.Clear(LTarget);
      end;
    sbcInherit:
      begin
        AEdit.Action := sdaInheritBinding;
        AEdit.Binding := TNyxBindingSpec.Clear(LTarget);
      end;
  end;
  Result := sacEdit;
end;

function RouteNyxStudioAuthoring(ASession: TNyxStudioSession; ANode: TNyxNode;
  AEvent: TNyxTrigger; AShellRoot: TNyxNode): Boolean;
var
  LEdit: TNyxStudioDesignEdit;
  LCapture: TNyxStudioAuthoringCapture;
  LValue: TNyxStateValue;
begin

  if RouteNyxStudioCollection(ASession, ANode, AEvent) then
  begin
    Exit(True);
  end;
  LCapture := CaptureNyxStudioAuthoring(ASession, ANode, AEvent, AShellRoot,
    Default(TNyxStudioPendingDesign), LEdit);
  Result := LCapture <> sacNone;

  if LCapture <> sacEdit then
  begin
    Exit;
  end;

  if LEdit.Action in [sdaSetStateDefault, sdaRenameStateDefault, sdaRemoveStateDefault] then
  begin
    LValue := ASession.Document.State.Value(LEdit.Name);

    if LValue.Kind <> NyxStudioStateInputKind(LEdit.StateInput) then
    begin
      raise ENyxState.Create('State editor cannot change its declared type');
    end;
  end;
  case LEdit.Action of
    sdaSetStateDefault:
      begin
        ASession.SetStateValues([NyxStateAssign(LEdit.Name,
          ParseNyxStudioStateInput(LEdit.StateInput, LEdit.Value))]);
      end;
    sdaCreateStateDefault:
      begin
        ASession.CreateState(LEdit.Name, ParseNyxStudioStateInput(LEdit.StateInput, LEdit.Value));
      end;
    sdaRenameStateDefault:
      begin
        ASession.RenameState(LEdit.Name, LEdit.Value);
      end;
    sdaRemoveStateDefault:
      begin
        ASession.RemoveState(LEdit.Name);
      end;
    sdaSetBinding:
      begin
        ASession.SetBinding(LEdit.Binding);
      end;
    sdaInheritBinding:
      begin
        ASession.InheritBinding(LEdit.Binding.Target);
      end;
  else
    begin
      raise ENyxModel.Create('Captured authoring intent is outside its typed contract');
    end;
  end;
end;

end.
