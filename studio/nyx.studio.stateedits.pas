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

unit nyx.studio.stateedits;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.state, nyx.binding.types, nyx.data,
  nyx.studio.projects;

const
  NyxMaximumStateBindingChanges = 32;

type
  { A typed identity for an authored default. The scalar family travels with its
    open name, so a stale name cannot silently adopt another family's meaning.
    It owns values only; no store, document, control or subscription is retained. }
  TNyxStudioStateRef = record
  private
    FName: TNyxText;
    FKind: TNyxStateKind;
  public
    property Name: TNyxText read FName;
    property Kind: TNyxStateKind read FKind;
  end;

  { Binding owners are exact authored IDs, including explicit named-part
    overrides. They are distinct from state names and reusable-definition refs. }
  TNyxStudioBindingOwner = record
  private
    FID: TNyxText;
  public
    property ID: TNyxText read FID;
  end;

  TNyxStateBindingChangeKind = (sbcCreateDefault, sbcSetDefault,
    sbcRenameDefault, sbcRemoveDefault, sbcSetBinding, sbcInheritBinding);

  { Closed immutable authoring intent. Construction validates copied scalar
    values/descriptors. Full reference/domain/ownership admission happens on the
    independent complete candidate, never against caller-owned mutable nodes. }
  TNyxStateBindingChange = record
  private
    FKind: TNyxStateBindingChangeKind;
    FState: TNyxStudioStateRef;
    FNewState: TNyxStudioStateRef;
    FValue: TNyxStateValue;
    FOwner: TNyxStudioBindingOwner;
    FBinding: TNyxBindingSpec;
    FTarget: TNyxBindingProperty;
  public
    property Kind: TNyxStateBindingChangeKind read FKind;
    property State: TNyxStudioStateRef read FState;
    property NewState: TNyxStudioStateRef read FNewState;
    property Value: TNyxStateValue read FValue;
    property Owner: TNyxStudioBindingOwner read FOwner;
    property Binding: TNyxBindingSpec read FBinding;
    property Target: TNyxBindingProperty read FTarget;
  end;

  { Owns copied changes. Candidate owns its temporary session and invokes the
    ordinary Studio commands in order. Any failure frees all temporary work.
    The supplied pair is untouched; the caller publishes the final pair once,
    producing one paired Undo step. Pending Pascal drafts refuse this semantic
    mutation; inspection can still read the accepted design independently. }
  INyxStateBindingPatch = interface(IInterface)
    ['{27263AD0-39C0-42A8-A885-9F3733B7F4EE}']
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function ToData: TNyxDataValue;
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

{ Convert a public typed state reference to owned editor intent, retaining its
  exact name and scalar family. Empty/invalid names refuse at construction. }
function NyxStudioState(const ARef: TNyxTextStateRef): TNyxStudioStateRef; overload;
function NyxStudioState(const ARef: TNyxBooleanStateRef): TNyxStudioStateRef; overload;
function NyxStudioState(const ARef: TNyxIntegerStateRef): TNyxStudioStateRef; overload;
function NyxStudioState(const ARef: TNyxNumberStateRef): TNyxStudioStateRef; overload;
{ Admit an exact authored identity; existence is checked on the fresh candidate. }
function NyxBindingOwner(const AID: TNyxText): TNyxStudioBindingOwner;
{ NyxStateValue's four typed reference/value overloads supply these assignments.
  Exists=False is refused; removal uses its separate explicit constructor.
  Create refuses a duplicate name during candidate admission. }
function NyxCreateDefault(const AValue: TNyxStateAssignment): TNyxStateBindingChange;
{ Set requires an existing exact name and matching scalar family; it never
  silently creates a default or converts another family's stored value. }
function NyxSetDefault(const AValue: TNyxStateAssignment): TNyxStateBindingChange;
{ Rename preserves family/value/order and migrates every authored reference,
  including pages and reusable definitions. Occupied destination names refuse. }
function NyxRenameDefault(const AFrom, ATo: TNyxStudioStateRef): TNyxStateBindingChange;
{ Remove requires an existing matching family. Remaining bindings make complete
  admission fail; place dependent clear/inherit changes earlier in the group. }
function NyxRemoveDefault(const ARef: TNyxStudioStateRef): TNyxStateBindingChange;
{ A cleared descriptor records deliberate unbinding. Inherit removes a local
  descriptor; these choices retain different reusable override semantics. }
function NyxBindControl(const AOwner: TNyxStudioBindingOwner;
  const ASpec: TNyxBindingSpec): TNyxStateBindingChange; overload;
{ Default Pascal authoring derives the scalar family from its typed reference.
  The descriptor overload above is the explicit interchange/advanced boundary.
  Two-way direction remains restricted to Value by common descriptor admission. }
function NyxBindControl(const AOwner: TNyxStudioBindingOwner;
  ATarget: TNyxBindingProperty; const AState: TNyxTextStateRef;
  ADirection: TNyxBindingDirection): TNyxStateBindingChange; overload;
function NyxBindControl(const AOwner: TNyxStudioBindingOwner;
  ATarget: TNyxBindingProperty; const AState: TNyxBooleanStateRef;
  ADirection: TNyxBindingDirection): TNyxStateBindingChange; overload;
function NyxBindControl(const AOwner: TNyxStudioBindingOwner;
  ATarget: TNyxBindingProperty; const AState: TNyxIntegerStateRef;
  ADirection: TNyxBindingDirection): TNyxStateBindingChange; overload;
function NyxBindControl(const AOwner: TNyxStudioBindingOwner;
  ATarget: TNyxBindingProperty; const AState: TNyxNumberStateRef;
  ADirection: TNyxBindingDirection): TNyxStateBindingChange; overload;
function NyxInheritBinding(const AOwner: TNyxStudioBindingOwner;
  ATarget: TNyxBindingProperty): TNyxStateBindingChange;
{ Own 1..32 copied ordered operations; the caller retains no mutable patch data. }
function NyxStateBindingPatch(const AChanges: array of TNyxStateBindingChange):
  INyxStateBindingPatch;
{ Exact JSON interchange boundary. Unknown fields, wrong primitives, invalid
  enums and unsupported operation shapes refuse before constructing a session. }
function ReadNyxStateBindingPatch(const AChanges: TNyxDataValue): INyxStateBindingPatch;
{ Discoverable argument schema for the semantic tool. Primitive types agree with
  the strict decoder; optional workspace/review routing is added by MCP alone. }
function NyxStateAgentSchema: TNyxDataValue;
{ Encode one validated scalar at the explicit JSON response boundary. }
function NyxStateValueData(const AValue: TNyxStateValue): TNyxDataValue;

implementation

uses
  nyx.model, nyx.studio.session;

type
  TNyxStateBindingPatch = class(TInterfacedObject, INyxStateBindingPatch)
  private
    FChanges: array of TNyxStateBindingChange;
  public
    constructor Create(const AChanges: array of TNyxStateBindingChange);
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function ToData: TNyxDataValue;
    function GetCount: Integer;
  end;

function StateRef(const AName: TNyxText; AKind: TNyxStateKind): TNyxStudioStateRef;
begin
  Result := Default(TNyxStudioStateRef);
  Result.FName := NyxTextState(AName).Name;
  Result.FKind := AKind;
end;

function NyxStudioState(const ARef: TNyxTextStateRef): TNyxStudioStateRef;
begin
  Result := StateRef(ARef.Name, nskText);
end;

function NyxStudioState(const ARef: TNyxBooleanStateRef): TNyxStudioStateRef;
begin
  Result := StateRef(ARef.Name, nskBoolean);
end;

function NyxStudioState(const ARef: TNyxIntegerStateRef): TNyxStudioStateRef;
begin
  Result := StateRef(ARef.Name, nskInteger);
end;

function NyxStudioState(const ARef: TNyxNumberStateRef): TNyxStudioStateRef;
begin
  Result := StateRef(ARef.Name, nskNumber);
end;

function NyxBindingOwner(const AID: TNyxText): TNyxStudioBindingOwner;
begin
  Result.FID := NyxComponent(AID).Name;
end;

function DefaultChange(AKind: TNyxStateBindingChangeKind;
  const AValue: TNyxStateAssignment): TNyxStateBindingChange;
begin

  if not AValue.Exists then
  begin
    raise ENyxState.Create('An authored default requires a present typed value');
  end;
  AValue.Value.Validate;
  Result := Default(TNyxStateBindingChange);
  Result.FKind := AKind;
  Result.FState := StateRef(AValue.Key, AValue.Value.Kind);
  Result.FValue := AValue.Value.Copy;
end;

function NyxCreateDefault(const AValue: TNyxStateAssignment): TNyxStateBindingChange;
begin
  Result := DefaultChange(sbcCreateDefault, AValue);
end;

function NyxSetDefault(const AValue: TNyxStateAssignment): TNyxStateBindingChange;
begin
  Result := DefaultChange(sbcSetDefault, AValue);
end;

function NyxRenameDefault(const AFrom, ATo: TNyxStudioStateRef): TNyxStateBindingChange;
begin

  if AFrom.Kind <> ATo.Kind then
  begin
    raise ENyxState.Create('Renaming a default cannot change its scalar family');
  end;
  Result := Default(TNyxStateBindingChange);
  Result.FKind := sbcRenameDefault;
  Result.FState := StateRef(AFrom.Name, AFrom.Kind);
  Result.FNewState := StateRef(ATo.Name, ATo.Kind);
end;

function NyxRemoveDefault(const ARef: TNyxStudioStateRef): TNyxStateBindingChange;
begin
  Result := Default(TNyxStateBindingChange);
  Result.FKind := sbcRemoveDefault;
  Result.FState := StateRef(ARef.Name, ARef.Kind);
end;

function NyxBindControl(const AOwner: TNyxStudioBindingOwner;
  const ASpec: TNyxBindingSpec): TNyxStateBindingChange;
begin
  ASpec.Validate;
  Result := Default(TNyxStateBindingChange);
  Result.FKind := sbcSetBinding;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FBinding := ASpec.Copy;
  Result.FTarget := ASpec.Target;
end;

function NyxInheritBinding(const AOwner: TNyxStudioBindingOwner;
  ATarget: TNyxBindingProperty): TNyxStateBindingChange;
begin
  TNyxBindingSpec.Clear(ATarget).Validate;
  Result := Default(TNyxStateBindingChange);
  Result.FKind := sbcInheritBinding;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FTarget := ATarget;
end;

function NyxBindControl(const AOwner: TNyxStudioBindingOwner;
  ATarget: TNyxBindingProperty; const AState: TNyxTextStateRef;
  ADirection: TNyxBindingDirection): TNyxStateBindingChange;
begin
  Result := NyxBindControl(AOwner,
    TNyxBindingSpec.Bound(ATarget, AState.Name, nskText, ADirection));
end;

function NyxBindControl(const AOwner: TNyxStudioBindingOwner;
  ATarget: TNyxBindingProperty; const AState: TNyxBooleanStateRef;
  ADirection: TNyxBindingDirection): TNyxStateBindingChange;
begin
  Result := NyxBindControl(AOwner,
    TNyxBindingSpec.Bound(ATarget, AState.Name, nskBoolean, ADirection));
end;

function NyxBindControl(const AOwner: TNyxStudioBindingOwner;
  ATarget: TNyxBindingProperty; const AState: TNyxIntegerStateRef;
  ADirection: TNyxBindingDirection): TNyxStateBindingChange;
begin
  Result := NyxBindControl(AOwner,
    TNyxBindingSpec.Bound(ATarget, AState.Name, nskInteger, ADirection));
end;

function NyxBindControl(const AOwner: TNyxStudioBindingOwner;
  ATarget: TNyxBindingProperty; const AState: TNyxNumberStateRef;
  ADirection: TNyxBindingDirection): TNyxStateBindingChange;
begin
  Result := NyxBindControl(AOwner,
    TNyxBindingSpec.Bound(ATarget, AState.Name, nskNumber, ADirection));
end;

function NyxStateValueData(const AValue: TNyxStateValue): TNyxDataValue;
begin
  AValue.Validate;
  case AValue.Kind of
    nskText: Result := NyxData(AValue.TextValue);
    nskBoolean: Result := NyxData(AValue.BooleanValue);
    nskInteger: Result := NyxData(AValue.IntegerValue);
    nskNumber: Result := NyxData(AValue.NumberValue);
  end;
end;

constructor TNyxStateBindingPatch.Create(const AChanges: array of TNyxStateBindingChange);
var
  LIndex: Integer;
  LChange: TNyxStateBindingChange;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > NyxMaximumStateBindingChanges) then
  begin
    raise ENyxState.Create('State/binding groups require 1..32 changes');
  end;
  SetLength(FChanges, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    LChange := AChanges[LIndex];
    case LChange.Kind of
      sbcCreateDefault:
        LChange := NyxCreateDefault(NyxStateAssign(LChange.State.Name, LChange.Value));
      sbcSetDefault:
        LChange := NyxSetDefault(NyxStateAssign(LChange.State.Name, LChange.Value));
      sbcRenameDefault:
        LChange := NyxRenameDefault(LChange.State, LChange.NewState);
      sbcRemoveDefault:
        LChange := NyxRemoveDefault(LChange.State);
      sbcSetBinding:
        LChange := NyxBindControl(LChange.Owner, LChange.Binding);
      sbcInheritBinding:
        LChange := NyxInheritBinding(LChange.Owner, LChange.Target);
    end;
    { Explicit field copies keep nested records independent under pas2js.
      Text/primitive snapshots own their values and contain no COM interfaces. }
    FChanges[LIndex].FKind := LChange.Kind;
    FChanges[LIndex].FState.FName := LChange.State.FName;
    FChanges[LIndex].FState.FKind := LChange.State.Kind;
    FChanges[LIndex].FNewState.FName := LChange.NewState.FName;
    FChanges[LIndex].FNewState.FKind := LChange.NewState.Kind;
    FChanges[LIndex].FValue := LChange.Value.Copy;
    FChanges[LIndex].FOwner.FID := LChange.Owner.ID;
    FChanges[LIndex].FBinding := LChange.Binding.Copy;
    FChanges[LIndex].FTarget := LChange.Target;
  end;
end;

function TNyxStateBindingPatch.GetCount: Integer;
begin
  Result := Length(FChanges);
end;

function TNyxStateBindingPatch.Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
var
  LSession: TNyxStudioSession;
  LChange: TNyxStateBindingChange;
  LNode: TNyxNode;
  LIndex: Integer;
begin
  LSession := TNyxStudioSession.Create(APair);
  try

    if LSession.DraftSource <> LSession.Source then
    begin
      raise ENyxState.Create('Resolve the pending Pascal draft before semantic state/binding edits');
    end;
    for LIndex := 0 to High(FChanges) do
    begin
      LChange := FChanges[LIndex];

      if LChange.Kind in [sbcSetDefault, sbcRenameDefault, sbcRemoveDefault] then
      begin

        if not LSession.Document.State.Has(LChange.State.Name) or
          (LSession.Document.State.Value(LChange.State.Name).Kind <> LChange.State.Kind) then
        begin
          raise ENyxState.Create('The exact authored default or its scalar family changed');
        end;
      end;
      case LChange.Kind of
        sbcCreateDefault:
          LSession.CreateState(LChange.State.Name, LChange.Value);
        sbcSetDefault:
          LSession.SetStateValues([NyxStateAssign(LChange.State.Name, LChange.Value)]);
        sbcRenameDefault:
          LSession.RenameState(LChange.State.Name, LChange.NewState.Name);
        sbcRemoveDefault:
          LSession.RemoveState(LChange.State.Name);
        sbcSetBinding, sbcInheritBinding:
          begin
            LNode := LSession.Document.Find(LChange.Owner.ID);

            if LNode = nil then
            begin
              raise ENyxState.Create('The exact authored binding owner is missing');
            end;
            while LNode.Parent <> nil do
            begin
              LNode := LNode.Parent;
            end;
            LSession.Activate(LNode.ID);
            LSession.Select(LChange.Owner.ID);

            if LChange.Kind = sbcSetBinding then
            begin
              LSession.SetBinding(LChange.Binding);
            end
            else
            begin
              LSession.InheritBinding(LChange.Target);
            end;
          end;
      end;
    end;
    Result := LSession.ProjectSnapshot;
  finally
    LSession.Free;
  end;
end;

function TNyxStateBindingPatch.ToData: TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LChange: TNyxStateBindingChange;
  LIndex: Integer;
begin
  SetLength(LValues, Length(FChanges));
  for LIndex := 0 to High(FChanges) do
  begin
    LChange := FChanges[LIndex];
    case LChange.Kind of
      sbcCreateDefault, sbcSetDefault:
        begin
          LValues[LIndex] := NyxObject([
            NyxField('op', NyxData('set')),
            NyxField('name', NyxData(LChange.State.Name)),
            NyxField('kind', NyxData(NyxStateKindName(LChange.State.Kind))),
            NyxField('value', NyxStateValueData(LChange.Value))]);

          if LChange.Kind = sbcCreateDefault then
          begin
            LValues[LIndex] := NyxObject([
              NyxField('op', NyxData('create')), NyxField('name', NyxData(LChange.State.Name)),
              NyxField('kind', NyxData(NyxStateKindName(LChange.State.Kind))),
              NyxField('value', NyxStateValueData(LChange.Value))]);
          end;
        end;
      sbcRenameDefault:
        LValues[LIndex] := NyxObject([
          NyxField('op', NyxData('rename')), NyxField('name', NyxData(LChange.State.Name)),
          NyxField('kind', NyxData(NyxStateKindName(LChange.State.Kind))),
          NyxField('to', NyxData(LChange.NewState.Name))]);
      sbcRemoveDefault:
        LValues[LIndex] := NyxObject([
          NyxField('op', NyxData('remove')), NyxField('name', NyxData(LChange.State.Name)),
          NyxField('kind', NyxData(NyxStateKindName(LChange.State.Kind)))]);
      sbcSetBinding:
        begin

          if LChange.Binding.Cleared then
          begin
            LValues[LIndex] := NyxObject([
              NyxField('op', NyxData('clear-binding')), NyxField('owner', NyxData(LChange.Owner.ID)),
              NyxField('target', NyxData(NyxBindingPropertyName(LChange.Target)))]);
          end
          else
          begin
            LValues[LIndex] := NyxObject([
              NyxField('op', NyxData('bind')), NyxField('owner', NyxData(LChange.Owner.ID)),
              NyxField('target', NyxData(NyxBindingPropertyName(LChange.Target))),
              NyxField('name', NyxData(LChange.Binding.StateName)),
              NyxField('kind', NyxData(NyxStateKindName(LChange.Binding.ValueKind))),
              NyxField('direction', NyxData(NyxBindingDirectionName(LChange.Binding.Direction)))]);
          end;
        end;
      sbcInheritBinding:
        LValues[LIndex] := NyxObject([
          NyxField('op', NyxData('inherit-binding')), NyxField('owner', NyxData(LChange.Owner.ID)),
          NyxField('target', NyxData(NyxBindingPropertyName(LChange.Target)))]);
    end;
  end;
  Result := NyxArray(LValues);
end;

function NyxStateBindingPatch(const AChanges: array of TNyxStateBindingChange):
  INyxStateBindingPatch;
begin
  Result := TNyxStateBindingPatch.Create(AChanges);
end;

procedure Fields(const AValue: TNyxDataValue; const AAllowed: TNyxText; ACount: Integer);
var
  LIndex: Integer;
begin

  if (AValue.Kind <> ndObject) or (AValue.Count <> ACount) then
  begin
    raise ENyxState.Create('State/binding operation requires its exact fields');
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    if (Pos('|', AValue.Key(LIndex)) > 0) or
      (Pos('|' + AValue.Key(LIndex) + '|', AAllowed) = 0) then
    begin
      raise ENyxState.Create('Unknown state/binding operation field');
    end;
  end;
end;

function ReadKind(const AValue: TNyxDataValue): TNyxStateKind;
var
  LKind: TNyxStateKind;
begin
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin

    if NyxStateKindName(LKind) = AValue.AsText then
    begin
      Exit(LKind);
    end;
  end;
  raise ENyxState.Create('Choose a supported scalar family');
end;

function ReadValue(AKind: TNyxStateKind; const AData: TNyxDataValue): TNyxStateValue;
begin
  case AKind of
    nskText: Result := TNyxStateValue.FromText(AData.AsText);
    nskBoolean: Result := TNyxStateValue.FromBoolean(AData.AsBoolean);
    nskInteger: Result := TNyxStateValue.FromInteger(AData.AsInteger);
    nskNumber: Result := TNyxStateValue.FromNumber(AData.AsNumber);
  end;
end;

function ReadNyxStateBindingPatch(const AChanges: TNyxDataValue): INyxStateBindingPatch;
var
  LChanges: array of TNyxStateBindingChange;
  LWire: TNyxDataValue;
  LName: TNyxText;
  LKind: TNyxStateKind;
  LTarget: TNyxBindingProperty;
  LDirection: TNyxBindingDirection;
  LAssignment: TNyxStateAssignment;
  LIndex: Integer;
begin

  if (AChanges.Kind <> ndArray) or (AChanges.Count < 1) or
    (AChanges.Count > NyxMaximumStateBindingChanges) then
  begin
    raise ENyxState.Create('State/binding groups require 1..32 changes');
  end;
  SetLength(LChanges, AChanges.Count);
  for LIndex := 0 to AChanges.Count - 1 do
  begin
    LWire := AChanges.Item(LIndex);
    LName := LWire.Field('op').AsText;

    if (LName = 'create') or (LName = 'set') then
    begin
      Fields(LWire, '|op|name|kind|value|', 4);
      LKind := ReadKind(LWire.Field('kind'));
      LAssignment := NyxStateAssign(LWire.Field('name').AsText, ReadValue(LKind, LWire.Field('value')));

      if LName = 'create' then
      begin
        LChanges[LIndex] := NyxCreateDefault(LAssignment);
      end
      else
      begin
        LChanges[LIndex] := NyxSetDefault(LAssignment);
      end;
    end
    else if LName = 'rename' then
    begin
      Fields(LWire, '|op|name|kind|to|', 4);
      LKind := ReadKind(LWire.Field('kind'));
      LChanges[LIndex] := NyxRenameDefault(StateRef(LWire.Field('name').AsText, LKind),
        StateRef(LWire.Field('to').AsText, LKind));
    end
    else if LName = 'remove' then
    begin
      Fields(LWire, '|op|name|kind|', 3);
      LChanges[LIndex] := NyxRemoveDefault(StateRef(LWire.Field('name').AsText,
        ReadKind(LWire.Field('kind'))));
    end
    else if (LName = 'bind') or (LName = 'clear-binding') or (LName = 'inherit-binding') then
    begin

      if not TryNyxBindingProperty(LWire.Field('target').AsText, LTarget) then
      begin
        raise ENyxState.Create('Choose a supported binding target');
      end;

      if LName = 'bind' then
      begin
        Fields(LWire, '|op|owner|target|name|kind|direction|', 6);

        if not TryNyxBindingDirection(LWire.Field('direction').AsText, LDirection) then
        begin
          raise ENyxState.Create('Choose a supported binding direction');
        end;
        LChanges[LIndex] := NyxBindControl(NyxBindingOwner(LWire.Field('owner').AsText),
          TNyxBindingSpec.Bound(LTarget, LWire.Field('name').AsText,
            ReadKind(LWire.Field('kind')), LDirection));
      end
      else
      begin
        Fields(LWire, '|op|owner|target|', 3);

        if LName = 'clear-binding' then
        begin
          LChanges[LIndex] := NyxBindControl(NyxBindingOwner(LWire.Field('owner').AsText),
            TNyxBindingSpec.Clear(LTarget));
        end
        else
        begin
          LChanges[LIndex] := NyxInheritBinding(NyxBindingOwner(LWire.Field('owner').AsText), LTarget);
        end;
      end;
    end
    else
    begin
      raise ENyxState.Create('Unknown state/binding operation');
    end;
  end;
  Result := NyxStateBindingPatch(LChanges);
end;

function NyxStateAgentSchema: TNyxDataValue;
const
  CPrimitives: array[TNyxStateKind] of TNyxText =
    ('string', 'boolean', 'integer', 'number');
var
  LChanges: array of TNyxDataValue;
  LTargets: array of TNyxDataValue;
  LKinds: array of TNyxDataValue;
  LKind: TNyxStateKind;
  LTarget: TNyxBindingProperty;
  LScalar: TNyxDataValue;
  LChangeSchema: TNyxDataValue;
begin
  SetLength(LChanges, 7);
  SetLength(LKinds, Ord(High(TNyxStateKind)) + 1);
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin
    LKinds[Ord(LKind)] := NyxData(NyxStateKindName(LKind));
    LScalar := NyxObject([NyxField('type', NyxData(CPrimitives[LKind]))]);

    if LKind = nskInteger then
    begin
      LScalar := NyxObject([NyxField('type', NyxData('integer')),
        NyxField('minimum', NyxData(Low(Integer))),
        NyxField('maximum', NyxData(High(Integer)))]);
    end;
    LChanges[Ord(LKind)] := NyxObject([
      NyxField('type', NyxData('object')),
      NyxField('properties', NyxObject([
        NyxField('op', NyxObject([NyxField('enum', NyxArray([NyxData('create'), NyxData('set')]))])),
        NyxField('name', NyxObject([NyxField('type', NyxData('string')),
          NyxField('minLength', NyxData(1))])),
        NyxField('kind', NyxObject([NyxField('const', NyxData(NyxStateKindName(LKind)))])),
        NyxField('value', LScalar)])),
      NyxField('required', NyxArray([NyxData('op'), NyxData('name'), NyxData('kind'), NyxData('value')])),
      NyxField('additionalProperties', NyxData(False))]);
  end;
  SetLength(LTargets, Ord(High(TNyxBindingProperty)) + 1);
  for LTarget := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
  begin
    LTargets[Ord(LTarget)] := NyxData(NyxBindingPropertyName(LTarget));
  end;
  LChanges[4] := TNyxDataValue.ParseJSON('{"type":"object","oneOf":[' +
    '{"properties":{"op":{"const":"rename"},"name":{"type":"string","minLength":1},"kind":{"enum":["text","boolean","integer","number"]},"to":{"type":"string","minLength":1}},"required":["op","name","kind","to"],"additionalProperties":false},' +
    '{"properties":{"op":{"const":"remove"},"name":{"type":"string","minLength":1},"kind":{"enum":["text","boolean","integer","number"]}},"required":["op","name","kind"],"additionalProperties":false}]}');
  LChanges[5] := NyxObject([NyxField('type', NyxData('object')),
    NyxField('properties', NyxObject([
      NyxField('op', NyxObject([NyxField('const', NyxData('bind'))])),
      NyxField('owner', NyxObject([NyxField('type', NyxData('string')), NyxField('minLength', NyxData(1))])),
      NyxField('target', NyxObject([NyxField('enum', NyxArray(LTargets))])),
      NyxField('name', NyxObject([NyxField('type', NyxData('string')), NyxField('minLength', NyxData(1))])),
      NyxField('kind', NyxObject([NyxField('enum', NyxArray(LKinds))])),
      NyxField('direction', NyxObject([NyxField('enum', NyxArray([NyxData('from-state'), NyxData('two-way')]))]))])),
    NyxField('required', NyxArray([NyxData('op'), NyxData('owner'), NyxData('target'),
      NyxData('name'), NyxData('kind'), NyxData('direction')])),
    NyxField('additionalProperties', NyxData(False))]);
  LChanges[6] := NyxObject([NyxField('type', NyxData('object')),
    NyxField('properties', NyxObject([
      NyxField('op', NyxObject([NyxField('enum', NyxArray([NyxData('clear-binding'), NyxData('inherit-binding')]))])),
      NyxField('owner', NyxObject([NyxField('type', NyxData('string')), NyxField('minLength', NyxData(1))])),
      NyxField('target', NyxObject([NyxField('enum', NyxArray(LTargets))]))])),
    NyxField('required', NyxArray([NyxData('op'), NyxData('owner'), NyxData('target')])),
    NyxField('additionalProperties', NyxData(False))]);
  LChangeSchema := NyxObject([NyxField('type', NyxData('array')),
    NyxField('minItems', NyxData(1)), NyxField('maxItems', NyxData(NyxMaximumStateBindingChanges)),
    NyxField('items', NyxObject([NyxField('oneOf', NyxArray(LChanges))]))]);
  { Read alternatives retain their individual budgets. The mutation alternative
    deliberately requires both the exact revision and a successful-retry key. }
  Result := TNyxDataValue.ParseJSON('{"type":"object","oneOf":[' +
    '{"properties":{"mode":{"const":"defaults"},"offset":{"type":"integer","minimum":0,"maximum":1024},"limit":{"type":"integer","minimum":1,"maximum":50},"filter":{"type":"string"}},"required":["mode"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"value"},"name":{"type":"string","minLength":1},"offset":{"type":"integer","minimum":0,"maximum":1048576},"count":{"type":"integer","minimum":1,"maximum":4096}},"required":["mode","name"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"bindings"},"owner":{"type":"string","minLength":1},"offset":{"type":"integer","minimum":0,"maximum":19},"limit":{"type":"integer","minimum":1,"maximum":20}},"required":["mode","owner"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"apply"},"expectedRevision":{"type":"integer","minimum":1},"operationId":{"type":"string","minLength":1,"maxLength":120},"changes":' +
      LChangeSchema.ToJSON + '},"required":["mode","expectedRevision","operationId","changes"],"additionalProperties":false}]}');
end;

end.
