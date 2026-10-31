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
unit nyx.binding.projection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.types, nyx.model, nyx.binding.types;

type
  { Closed scalar presentation targets. A changed extension or other property
    has no invented classification and requires the ordinary complete Sync. }
  TNyxBindingProjectionProperties = set of TNyxBindingProperty;

  { Immutable copied synchronization hints, never state or target authority.
    Values are read from the already admitted live root by its synchronous
    target receiver. Retaining this observation retains no node, state, widget
    or renderer and cannot authorize a later publication into another view. }
  INyxBindingProjectionChanges = interface(IInterface)
    ['{A0237544-D963-4C74-BDC8-E9C5B8AC4662}']
    function GetCount: Integer;
    function ControlAt(AIndex: Integer): TNyxControlRef;
    function PropertiesAt(AIndex: Integer): TNyxBindingProjectionProperties;
    property Count: Integer read GetCount;
  end;

  { Detached observation owns independent property/shape/contract data only.
    Changes reads a borrowed tree and refuses uncertain shape, identity, changed
    contracts or property classification. This is not candidate admission or a
    proof of live control identity; the coordinator/adapter retain those checks.
    Runtime interaction policies use their pure UI-thread getter contract; only
    effective Boolean results are copied, never a policy or its owner. }
  INyxBindingProjectionCapture = interface(IInterface)
    ['{33BB65E8-5F81-4EAC-9252-F28AE9C28F63}']
    function Changes(ARoot: TNyxNode): INyxBindingProjectionChanges;
  end;

{ Observe a complete current tree without retaining it. The caller owns its
  tree throughout each call. This helper neither evaluates bindings nor skips
  validation. Nil roots refuse; unchanged complete projections return Count=0. }
function CaptureNyxBindingProjection(ARoot: TNyxNode): INyxBindingProjectionCapture;

implementation

uses SysUtils, nyx.text, nyx.state;

type
  { Copied binding descriptors detect changed contracts without retaining a
    source node or replacing complete current candidate admission. }
  TProjectionContracts = array of TNyxBindingSpec;
  { One detached node owns property/contract data and effective interaction
    flags. Runtime policy getters are pure UI-thread reads by their contract;
    copied Booleans retain no policy owner, borrowed address or source tree. }
  TProjectionFrame = record
    Control: TNyxControlRef;
    Kind: TNyxText;
    Children: Integer;
    Properties: TNyxStrings;
    Bindings: TProjectionContracts;
    Enabled: Boolean;
    ReadOnly: Boolean;
  end;
  { Private flattened preorder storage owns each frame's Properties collection. }
  TProjectionFrames = array of TProjectionFrame;
  { One classified changed control owns only its copied identity and enum set. }
  TProjectionChange = record
    Control: TNyxControlRef;
    Properties: TNyxBindingProjectionProperties;
  end;
  { Private change vector is never exposed for mutation by a target receiver. }
  TProjectionChanges = array of TProjectionChange;
  { Read-only result copies its vector and returns independent enum sets on
    both compilers. No caller can rewrite an observation retained by a peer. }
  TProjectionChangeList = class(TInterfacedObject, INyxBindingProjectionChanges)
  private
    FItems: TProjectionChanges;
    procedure CheckIndex(AIndex: Integer);
  public
    constructor Create(const AItems: TProjectionChanges);
    function GetCount: Integer;
    function ControlAt(AIndex: Integer): TNyxControlRef;
    function PropertiesAt(AIndex: Integer): TNyxBindingProjectionProperties;
  end;
  { UI-owner snapshot copies properties, shape and binding descriptors without
    cloning resource catalogs or collection definitions. Partial construction
    and failed comparison release every owned string list. }
  TProjectionCapture = class(TInterfacedObject, INyxBindingProjectionCapture)
  private
    FFrames: TProjectionFrames;
    procedure Observe(ANode: TNyxNode);
  public
    constructor Create(ARoot: TNyxNode);
    destructor Destroy; override;
    function Changes(ARoot: TNyxNode): INyxBindingProjectionChanges;
  end;

constructor TProjectionChangeList.Create(const AItems: TProjectionChanges);
begin
  inherited Create;
  FItems := Copy(AItems, 0, Length(AItems));
end;

procedure TProjectionChangeList.CheckIndex(AIndex: Integer);
begin

  if (AIndex < 0) or (AIndex >= Length(FItems)) then
  begin
    raise ENyxState.Create('Projection change index is outside its observation');
  end;
end;

function TProjectionChangeList.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TProjectionChangeList.ControlAt(AIndex: Integer): TNyxControlRef;
begin
  CheckIndex(AIndex);
  Result := FItems[AIndex].Control;
end;

function TProjectionChangeList.PropertiesAt(AIndex: Integer): TNyxBindingProjectionProperties;
var
  LProperty: TNyxBindingProperty;
begin
  CheckIndex(AIndex);
  Result := [];
  for LProperty := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
  begin

    if LProperty in FItems[AIndex].Properties then
    begin
      Include(Result, LProperty);
    end;
  end;
end;

constructor TProjectionCapture.Create(ARoot: TNyxNode);
begin
  inherited Create;

  if ARoot = nil then
  begin
    raise ENyxState.Create('Projection observation requires a current root');
  end;
  Observe(ARoot);
end;

destructor TProjectionCapture.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FFrames) do
  begin
    FFrames[LIndex].Properties.Free;
  end;
  inherited Destroy;
end;

procedure TProjectionCapture.Observe(ANode: TNyxNode);
var
  LIndex: Integer;
  LChild: Integer;
  LBinding: Integer;
begin
  LIndex := Length(FFrames);
  SetLength(FFrames, LIndex + 1);
  FFrames[LIndex] := Default(TProjectionFrame);
  FFrames[LIndex].Control := NyxControl(ANode.ID);
  FFrames[LIndex].Kind := ANode.Kind;
  FFrames[LIndex].Children := ANode.Count;
  FFrames[LIndex].Properties := TNyxStrings.Create;
  FFrames[LIndex].Properties.Assign(ANode.Props);
  FFrames[LIndex].Enabled := ANode.Prop(NyxBindingPropertyName(bpEnabled), 'true') <> 'false';
  FFrames[LIndex].ReadOnly := ANode.Prop(NyxBindingPropertyName(bpReadOnly), 'false') = 'true';
  SetLength(FFrames[LIndex].Bindings, ANode.BindingCount);
  for LBinding := 0 to ANode.BindingCount - 1 do
  begin
    FFrames[LIndex].Bindings[LBinding] := ANode.Bindings[LBinding].Copy;
  end;
  for LChild := 0 to ANode.Count - 1 do
  begin
    Observe(ANode.Children[LChild]);
  end;
end;

function TProjectionCapture.Changes(ARoot: TNyxNode): INyxBindingProjectionChanges;
var
  LItems: TProjectionChanges;
  LFrame: Integer;

  function Compare(ANode: TNyxNode): Boolean;
  var
    LBefore: TNyxStrings;
    LProperties: TNyxBindingProjectionProperties;
    LProperty: TNyxBindingProperty;
    LIndex: Integer;
    LChild: Integer;
    LItem: Integer;
    LKey: TNyxText;

    function ChangedProperty(const AKey: TNyxText): Boolean;
    begin
      Result := TryNyxBindingProperty(AKey, LProperty);

      if Result then
      begin
        Include(LProperties, LProperty);
      end;
    end;

  begin
    Result := False;

    if (ANode = nil) or (LFrame >= Length(FFrames)) then
    begin
      Exit;
    end;

    if (ANode.ID <> FFrames[LFrame].Control.ID) or
      (ANode.Kind <> FFrames[LFrame].Kind) or
      (ANode.Count <> FFrames[LFrame].Children) or
      (ANode.BindingCount <> Length(FFrames[LFrame].Bindings)) then
    begin
      Exit;
    end;
    for LIndex := 0 to ANode.BindingCount - 1 do
    begin

      if not ANode.Bindings[LIndex].Same(FFrames[LFrame].Bindings[LIndex]) then
      begin
        Exit;
      end;
    end;
    LProperties := [];

    if FFrames[LFrame].Enabled <>
      (ANode.Prop(NyxBindingPropertyName(bpEnabled), 'true') <> 'false') then
    begin
      Include(LProperties, bpEnabled);
    end;

    if FFrames[LFrame].ReadOnly <>
      (ANode.Prop(NyxBindingPropertyName(bpReadOnly), 'false') = 'true') then
    begin
      Include(LProperties, bpReadOnly);
    end;
    LBefore := FFrames[LFrame].Properties;
    Inc(LFrame);
    for LIndex := 0 to LBefore.Count - 1 do
    begin
      LKey := LBefore.Names[LIndex];

      if (ANode.Props.IndexOfName(LKey) < 0) or
        (Copy(LBefore[LIndex], Length(LKey) + 2, MaxInt) <> ANode.StoredProp(LKey)) then
      begin

        if not ChangedProperty(LKey) then
        begin
          Exit;
        end;
      end;
    end;
    for LIndex := 0 to ANode.Props.Count - 1 do
    begin
      LKey := ANode.Props.Names[LIndex];

      if (LBefore.IndexOfName(LKey) < 0) and not ChangedProperty(LKey) then
      begin
        Exit;
      end;
    end;

    if LProperties <> [] then
    begin
      LItem := Length(LItems);
      SetLength(LItems, LItem + 1);
      LItems[LItem] := Default(TProjectionChange);
      LItems[LItem].Control := NyxControl(ANode.ID);
      LItems[LItem].Properties := LProperties;
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin

      if not Compare(ANode.Children[LChild]) then
      begin
        Exit;
      end;
    end;
    Result := True;
  end;

begin
  Result := nil;
  LFrame := 0;

  if Compare(ARoot) and (LFrame = Length(FFrames)) then
  begin
    Result := TProjectionChangeList.Create(LItems);
  end;
end;

function CaptureNyxBindingProjection(ARoot: TNyxNode): INyxBindingProjectionCapture;
begin
  Result := TProjectionCapture.Create(ARoot);
end;

end.
