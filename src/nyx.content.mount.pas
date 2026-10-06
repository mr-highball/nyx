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
unit nyx.content.mount;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.contract, nyx.binding.types,
  nyx.model, nyx.composition, nyx.containers, nyx.editing, nyx.scheduler;

type
  { A mounted view owns this independent authored blueprint. It contains only
    its isolated root, all transitive recipes and copied document defaults.
    Document/Root accessors borrow it for adapter staging; callers must neither
    mutate nor release them. Destruction retains no source/document/UI receiver. }
  TNyxContentBlueprint = class
  private
    FDocument: TNyxDocument;
    function GetRoot: TNyxNode;
  public
    constructor Create(ADocument: TNyxDocument; ARoot: TNyxNode);
    destructor Destroy; override;
    function Realize(const AFrame: TNyxViewFrame;
      const AMeasurements: INyxContainerSnapshot): TNyxNode;
    property Document: TNyxDocument read FDocument;
    property Root: TNyxNode read GetRoot;
  end;

  { Copied identity, never a node handle. Explicit contiguous named part paths
    within the same reusable scope are shared across alternative recipes.
    Anonymous paths use exact runtime identity instead of guessing by caption,
    binding key, sibling index or control type. }
  TNyxContentIdentity = record
    Scope: TNyxText;
    PartPath: TNyxText;
    RuntimeID: TNyxText;
    function Same(const AOther: TNyxContentIdentity): Boolean;
  end;

  { Target-independent saved input state. Adapters own arrays of these values;
    no record contains an interface, widget, borrowed tree or mutable store.
    Exact domains/binding descriptors and accepted baselines govern draft reuse.
    Selection uses Unicode scalar offsets, preserving supplementary characters. }
  TNyxContentFaceState = record
    Identity: TNyxContentIdentity;
    ProjectionKind: TNyxText;
    Domain: TNyxValueDomain;
    Bindings: array of TNyxBindingSpec;
    Accepted: TNyxText;
    Text: TNyxText;
    Selection: TNyxTextSelection;
    HasText: Boolean;
    Focused: Boolean;
    ScrollLeft: Double;
    ScrollTop: Double;
  end;
  TNyxContentFaceStates = array of TNyxContentFaceState;

  TNyxContentUpdate = procedure of object;
  { Synchronous callbacks may pump the native UI queue. This managed guard
    prevents queued structural work from retiring their borrowed controls.
    Local guard references survive renderer disposal; Retire revokes the weak
    idle receiver before destruction, so Leave needs no renderer access. }
  INyxContentGuard = interface(IInterface)
    ['{0B21AA8F-3D07-466C-92ED-9D02011DA97F}']
    procedure Enter;
    procedure Leave;
    procedure Retire;
    function GetBusy: Boolean;
    property Busy: Boolean read GetBusy;
  end;
  { A queued managed work port borrows a receiver until Retire. The adapter
    retires it before releasing itself. Queues retain only the port/execution,
    never the renderer, preventing cycles and calls into a retired mount. }
  INyxContentWork = interface(INyxWork)
    ['{8712F497-0E97-449C-8461-8909CD63B165}']
    procedure Retire;
  end;

{ Nil when the isolated view has no structural choices. Ordinary views pay no
  persistent snapshot cost; all authored source data stays independently owned. }
function NewNyxContentBlueprint(ADocument: TNyxDocument;
  ARoot: TNyxNode): TNyxContentBlueprint;
function NewNyxContentWork(AUpdate: TNyxContentUpdate): INyxContentWork;
function NewNyxContentGuard(AIdle: TNyxContentUpdate): INyxContentGuard;
function NyxContentIdentity(ANode: TNyxNode): TNyxContentIdentity;
function CaptureNyxContentFace(ANode: TNyxNode): TNyxContentFaceState;
{ A matching logical identity with a different domain or descriptor is a new
  field. Ambiguous saved identities raise before any control/state publication. }
function NyxContentFaceIndex(ANode: TNyxNode;
  const AStates: TNyxContentFaceStates): Integer;
{ Copy accepted unbound scalar values into a detached candidate. Bound fields
  continue through the same live store; no defaults are imported into that store. }
procedure RestoreNyxContentValues(ARoot: TNyxNode;
  const AStates: TNyxContentFaceStates);
{ Compare structure/constructor identity independently of live scalar values.
  A geometry update selecting the same recipe keeps its actual control objects. }
function SameNyxContentStructure(AFirst, ASecond: TNyxNode): Boolean;
{ Canonical exact measurements for publishers in a realized tree. Missing boxes
  remain distinct from zero-sized boxes. No target geometry or pointer is kept. }
function NyxContentMeasurements(ARoot: TNyxNode;
  const AMeasurements: INyxContainerSnapshot): TNyxText;

implementation

uses nyx.schema;

type
  TContentGuard = class(TInterfacedObject, INyxContentGuard)
  private
    FDepth: Integer;
    FIdle: TNyxContentUpdate;
    FRetired: Boolean;
  public
    constructor Create(AIdle: TNyxContentUpdate);
    procedure Enter;
    procedure Leave;
    procedure Retire;
    function GetBusy: Boolean;
  end;

  TContentWork = class(TInterfacedObject, INyxWork, INyxContentWork)
  private
    FUpdate: TNyxContentUpdate;
  public
    constructor Create(AUpdate: TNyxContentUpdate);
    procedure Execute(const AExecution: INyxExecution);
    procedure Retire;
  end;

constructor TNyxContentBlueprint.Create(ADocument: TNyxDocument; ARoot: TNyxNode);
begin
  inherited Create;
  FDocument := CloneNyxViewDocument(ADocument, ARoot);
end;

constructor TContentGuard.Create(AIdle: TNyxContentUpdate);
begin
  inherited Create;
  FIdle := AIdle;
end;

procedure TContentGuard.Enter;
begin

  if FRetired or (FDepth >= 128) then
  begin
    raise ENyxModel.Create('Structural callback guard is retired or too deeply nested');
  end;
  Inc(FDepth);
end;

procedure TContentGuard.Leave;
begin

  if FDepth = 0 then
  begin
    raise ENyxModel.Create('Structural callback guard is unbalanced');
  end;
  Dec(FDepth);

  if (FDepth = 0) and not FRetired and Assigned(FIdle) then
  begin
    FIdle;
  end;
end;

procedure TContentGuard.Retire;
begin
  FRetired := True;
  FIdle := nil;
end;

function TContentGuard.GetBusy: Boolean;
begin
  Result := FRetired or (FDepth <> 0);
end;

function NewNyxContentGuard(AIdle: TNyxContentUpdate): INyxContentGuard;
begin
  Result := TContentGuard.Create(AIdle);
end;

destructor TNyxContentBlueprint.Destroy;
begin
  FDocument.Free;
  inherited Destroy;
end;

function TNyxContentBlueprint.GetRoot: TNyxNode;
begin
  Result := FDocument.Pages[0];
end;

function TNyxContentBlueprint.Realize(const AFrame: TNyxViewFrame;
  const AMeasurements: INyxContainerSnapshot): TNyxNode;
begin
  Result := RealizeNyxView(FDocument, Root, AFrame, AMeasurements);
end;

function NewNyxContentBlueprint(ADocument: TNyxDocument;
  ARoot: TNyxNode): TNyxContentBlueprint;
begin
  Result := nil;

  if not ADocument.HasContentRules then
  begin
    Exit;
  end;
  Result := TNyxContentBlueprint.Create(ADocument, ARoot);

  if not Result.Document.HasContentRules then
  begin
    FreeAndNil(Result);
  end;
end;

constructor TContentWork.Create(AUpdate: TNyxContentUpdate);
begin
  inherited Create;
  FUpdate := AUpdate;
end;

procedure TContentWork.Execute(const AExecution: INyxExecution);
var
  LUpdate: TNyxContentUpdate;
begin
  LUpdate := FUpdate;

  if Assigned(LUpdate) and not AExecution.Cancelled then
  begin
    LUpdate;
  end;
end;

procedure TContentWork.Retire;
begin
  FUpdate := nil;
end;

function NewNyxContentWork(AUpdate: TNyxContentUpdate): INyxContentWork;
begin
  Result := TContentWork.Create(AUpdate);
end;

function TNyxContentIdentity.Same(const AOther: TNyxContentIdentity): Boolean;
begin
  Result := (Scope = AOther.Scope) and (PartPath = AOther.PartPath) and
    (RuntimeID = AOther.RuntimeID);
end;

function NyxContentIdentity(ANode: TNyxNode): TNyxContentIdentity;
var
  LNode: TNyxNode;
begin
  Result := Default(TNyxContentIdentity);
  Result.RuntimeID := ANode.ID;

  if ANode.Prop('part') = '' then
  begin
    Exit;
  end;
  Result.Scope := ANode.InstanceScopeID;
  Result.PartPath := ANode.Prop('part');
  LNode := ANode.Parent;
  while (LNode <> nil) and (LNode.InstanceScopeID = Result.Scope) do
  begin
    { The instance root ends the path. Its recipe source ID/name may differ. }

    if (LNode.Parent = nil) or (LNode.Parent.InstanceScopeID <> Result.Scope) then
    begin
      Break;
    end;

    if LNode.Prop('part') = '' then
    begin
      Result := Default(TNyxContentIdentity);
      Result.RuntimeID := ANode.ID;
      Exit;
    end;
    Result.PartPath := LNode.Prop('part') + TNyxText('/') + Result.PartPath;
    LNode := LNode.Parent;
  end;

  Result.Scope := ANode.RuntimeInstanceOwner.ID;
  Result.RuntimeID := '';
end;

function CaptureNyxContentFace(ANode: TNyxNode): TNyxContentFaceState;
var
  LIndex: Integer;
begin
  Result := Default(TNyxContentFaceState);
  Result.Identity := NyxContentIdentity(ANode);
  Result.ProjectionKind := ANode.ProjectionKind;
  Result.Domain := NyxNodeValueDomain(ANode);
  Result.Accepted := ANode.Prop('value');
  SetLength(Result.Bindings, ANode.BindingCount);
  for LIndex := 0 to High(Result.Bindings) do
  begin
    Result.Bindings[LIndex] := ANode.Bindings[LIndex];
  end;
end;

function NyxContentFaceIndex(ANode: TNyxNode;
  const AStates: TNyxContentFaceStates): Integer;
var
  LIdentity: TNyxContentIdentity;
  LDomain: TNyxValueDomain;
  LIndex: Integer;
  LBinding: Integer;
  LFound: Integer;
begin
  Result := -1;
  LFound := -1;
  LIdentity := NyxContentIdentity(ANode);
  for LIndex := 0 to High(AStates) do
  begin

    if not LIdentity.Same(AStates[LIndex].Identity) then
    begin
      Continue;
    end;

    if LFound >= 0 then
    begin
      raise ENyxModel.Create('Ambiguous presentation continuity identity');
    end;
    LFound := LIndex;
  end;

  if LFound < 0 then
  begin
    Exit;
  end;
  LDomain := NyxNodeValueDomain(ANode);

  if (LDomain.ToData.ToJSON <> AStates[LFound].Domain.ToData.ToJSON) or
    (ANode.BindingCount <> Length(AStates[LFound].Bindings)) or
    (not LDomain.Defined and (ANode.ProjectionKind <> AStates[LFound].ProjectionKind)) then
  begin
    Exit;
  end;
  for LBinding := 0 to ANode.BindingCount - 1 do
  begin

    if not ANode.Bindings[LBinding].Same(AStates[LFound].Bindings[LBinding]) then
    begin
      Exit;
    end;
  end;
  Result := LFound;
end;

procedure RestoreNyxContentValues(ARoot: TNyxNode;
  const AStates: TNyxContentFaceStates);
var
  LIndex: Integer;
  LChild: Integer;
  LBinding: Integer;
  LBound: Boolean;
begin
  LIndex := NyxContentFaceIndex(ARoot, AStates);

  if (LIndex >= 0) and AStates[LIndex].Domain.Defined then
  begin
    LBound := False;
    for LBinding := 0 to ARoot.BindingCount - 1 do
    begin
      LBound := LBound or ((ARoot.Bindings[LBinding].Target = bpValue) and
        not ARoot.Bindings[LBinding].Cleared);
    end;

    if not LBound then
    begin
      ARoot.SetProp('value', AStates[LIndex].Accepted);
    end;
  end;
  for LChild := 0 to ARoot.Count - 1 do
  begin
    RestoreNyxContentValues(ARoot.Children[LChild], AStates);
  end;
end;

function SameNyxContentStructure(AFirst, ASecond: TNyxNode): Boolean;
var
  LIndex: Integer;
begin
  Result := (AFirst.ID = ASecond.ID) and (AFirst.Kind = ASecond.Kind) and
    (AFirst.ProjectionKind = ASecond.ProjectionKind) and
    (AFirst.InstanceScopeID = ASecond.InstanceScopeID) and
    (AFirst.RecipeOwner.ID = ASecond.RecipeOwner.ID) and
    (AFirst.Count = ASecond.Count);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to AFirst.Count - 1 do
  begin

    if not SameNyxContentStructure(AFirst.Children[LIndex], ASecond.Children[LIndex]) then
    begin
      Exit(False);
    end;
  end;
end;

function NyxContentMeasurements(ARoot: TNyxNode;
  const AMeasurements: INyxContainerSnapshot): TNyxText;
var
  LItems: array of TNyxDataValue;

  procedure Visit(ANode: TNyxNode);
  var
    LChild: Integer;
    LWidth: Double;
    LHeight: Double;
    LPresent: Boolean;
  begin

    if ANode.QueryContainer.Defined then
    begin
      LWidth := 0;
      LHeight := 0;
      LPresent := False;

      if AMeasurements <> nil then
      begin
        LPresent := AMeasurements.TrySize(ANode.ID, LWidth, LHeight);
      end;
      SetLength(LItems, Length(LItems) + 1);
      LItems[High(LItems)] := NyxArray([NyxData(ANode.ID), NyxData(LPresent),
        NyxData(LWidth), NyxData(LHeight)]);
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LChild]);
    end;
  end;

begin
  LItems := nil;
  Visit(ARoot);
  Result := NyxArray(LItems).ToJSON;
end;

end.
