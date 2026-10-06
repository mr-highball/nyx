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

unit nyx.studio.edits;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.catalog, nyx.designer.resize;

type
  { Closed semantic operation vocabulary. JSON names are admitted once at the
    transport boundary; internal behavior never dispatches arbitrary properties
    or method names. A whole immutable patch builds one detached candidate. }
  TNyxDesignOperation = (doCreate, doUpdate, doMove, doDelete, doTitle, doTokens,
    doDerive, doInstance, doOverride, doInherit, doPlace, doPlaceNew);

  { Relative placement avoids fragile sibling indices. Inside appends to the
    exact target container; before/after refer to the target's current owner.
    The candidate resolves positions after detaching a moved control. }
  TNyxPlacement = (nplInside, nplBefore, nplAfter);

  { Exact authored size intent. Values cross workers without UI/model handles.
    The existing grouped scalar patch owns all candidate/property validation. }
  TNyxResizeChange = record
  private
    FControl: TNyxControlRef;
    FAxis: TNyxResizeAxis;
    FSize: TNyxResizeSize;
    FPlatform: TNyxPlatform;
  public
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxResizeChange; static;
    function SameChange(const AOther: TNyxResizeChange): Boolean;
    { Set touched axes to explicit pixels/Automatic. Clear positive main-axis
      weight only when that axis is resized; unchanged-axis sizing is retained. }
    function Operation(AClearFlex: Boolean): TNyxDataValue;
    property Control: TNyxControlRef read FControl;
    property Axis: TNyxResizeAxis read FAxis;
    property Size: TNyxResizeSize read FSize;
    property Platform: TNyxPlatform read FPlatform;
  end;

  { An immutable, value-only placement command. A blank kind means move; a
    constructed kind means create from the catalog. Neither intent retains a
    node, widget, renderer or document. Default records are refused. }
  TNyxPlacementChange = record
  private
    FDefined: Boolean;
    FControl: TNyxControlRef;
    FTarget: TNyxControlRef;
    FKind: TNyxKindRef;
    FPlacement: TNyxPlacement;
  public
    { Strict persistence/worker boundary, not a default authoring API. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxPlacementChange; static;
    function SameChange(const AOther: TNyxPlacementChange): Boolean;
    property Control: TNyxControlRef read FControl;
    property Target: TNyxControlRef read FTarget;
    property Kind: TNyxKindRef read FKind;
    property Placement: TNyxPlacement read FPlacement;
  end;

  { Copied typed reusable intents. IDs, definition references and named paths
    have separate families; override behavior is a closed Pascal enum. Mutable
    caller arrays are copied into a patch and never retained by reference. }
  TNyxReusableChange = record
  private
    FOperation: TNyxDesignOperation;
    FControl, FOwner: TNyxControlRef;
    FDefinition: TNyxComponentRef;
    FPath: TNyxPartRef;
    FMode: TNyxOverrideMode;
    FIndex: Integer;
    FIdentities: array of TNyxIdentityAssignment;
  end;

  INyxDesignPatch = interface
    ['{6B582F67-B92C-4191-9798-7CB64402280E}']
    { Borrow both arguments. The caller owns the returned document, including on
      publication rejection; exceptions release all partially constructed nodes. }
    function Candidate(ADocument: TNyxDocument; ACatalog: TNyxCatalog): TNyxDocument;
  end;

{ Decode 1..64 operations. Primitive property values retain their JSON scalar
  type; unknown fields/operations fail. IDs and custom kind names are user data.
  Schema/property/document admission also runs on the complete detached result. }
function ReadNyxDesignPatch(const AOperations: TNyxDataValue): INyxDesignPatch;
{ Derive copies an exact subtree without altering it or replacing its uses.
  Every descendant needs an explicit destination; the definition owns the copy. }
function NyxDeriveComponent(const ASource: TNyxControlRef;
  const ADefinition: TNyxComponentRef;
  const AIdentities: array of TNyxIdentityAssignment): TNyxReusableChange;
{ Instantiate adds a reference, not a copy of the definition. -1 appends; other
  positions are exact. AParent remains the owner of the new authored reference. }
function NyxInstantiateComponent(const ADefinition: TNyxComponentRef;
  const AControl, AParent: TNyxControlRef; AIndex: Integer = -1): TNyxReusableChange;
{ Override names the exact instance-owned descriptor. A repeated path requires
  the same descriptor ID. Payload changes use ordinary create/move/delete in the
  same grouped transaction; incomplete replace/append rules refuse at admission. }
function NyxOverrideComponentPart(const AInstance, ADescriptor: TNyxControlRef;
  const APath: TNyxPartRef; AMode: TNyxOverrideMode): TNyxReusableChange;
{ Restore inheritance removes only the exact matching local descriptor/payload.
  Missing or changed owner/path/identity refuses; definitions remain untouched. }
function NyxInheritComponentPart(const AInstance, ADescriptor: TNyxControlRef;
  const APath: TNyxPartRef): TNyxReusableChange;
{ Copy 1..64 typed commands into the existing design transaction engine. Source,
  drafts, revision and paired Undo remain owned by the ordinary Studio session. }
function NyxReusablePatch(const AChanges: array of TNyxReusableChange): INyxDesignPatch;
{ Move the exact authored control. Roots, cycles, self-placement, leaf targets
  and inherited instance content refuse without changing the accepted pair. }
function NyxPlaceControl(const AControl, ATarget: TNyxControlRef;
  APlacement: TNyxPlacement): TNyxPlacementChange;
{ Create an independently owned catalog control/recipe at an exact location.
  AControl must be unused. Root creation remains a separate editor operation. }
function NyxPlaceNewControl(AKind: TNyxKind; const AControl, ATarget: TNyxControlRef;
  APlacement: TNyxPlacement): TNyxPlacementChange; overload;
function NyxPlaceNewControl(const AKind: TNyxKindRef;
  const AControl, ATarget: TNyxControlRef;
  APlacement: TNyxPlacement): TNyxPlacementChange; overload;
{ Copy 1..64 placement intents into the ordinary grouped candidate engine. }
function NyxPlacementPatch(const AChanges: array of TNyxPlacementChange): INyxDesignPatch;
{ Stable closed names at persistence and inspector boundaries only. }
function NyxPlacementName(APlacement: TNyxPlacement): TNyxText;
function ReadNyxPlacement(const AName: TNyxText): TNyxPlacement;
{ Default scope changes portable dimensions. A concrete scope explicitly authors
  that target's override, without erasing another target's independent policy. }
function NyxResizeControl(const AControl: TNyxControlRef; AAxis: TNyxResizeAxis;
  const ASize: TNyxResizeSize; APlatform: TNyxPlatform = npfAny): TNyxResizeChange;
{ Borrow an effective realized parent. Shared one-axis intent must make the
  same weight decision on both targets; callers refuse divergent flow scopes. }
function NyxResizeReleasesWeight(AParent: TNyxNode; AAxis: TNyxResizeAxis;
  APlatform: TNyxPlatform): Boolean;

implementation

uses
  nyx.schema, nyx.design.tokens, nyx.composition;

function NyxResizeReleasesWeight(AParent: TNyxNode; AAxis: TNyxResizeAxis;
  APlatform: TNyxPlatform): Boolean;
var
  LLayout: TNyxText;
begin

  if AParent = nil then
  begin
    raise ENyxModel.Create('Resize allocation requires an effective parent');
  end;
  LLayout := NyxLayout(AParent, APlatform);
  Result := (AAxis = nraBoth) or ((LLayout = 'row') and (AAxis = nraWidth)) or
    ((LLayout = 'column') and (AAxis = nraHeight));
end;

function NyxResizeControl(const AControl: TNyxControlRef; AAxis: TNyxResizeAxis;
  const ASize: TNyxResizeSize; APlatform: TNyxPlatform): TNyxResizeChange;
begin

  if (AControl.ID = '') or not ASize.Defined or
    (Ord(AAxis) < Ord(Low(TNyxResizeAxis))) or
    (Ord(AAxis) > Ord(High(TNyxResizeAxis))) or
    (Ord(APlatform) < Ord(Low(TNyxPlatform))) or
    (Ord(APlatform) > Ord(High(TNyxPlatform))) then
  begin
    raise ENyxModel.Create('Resize requires an exact control, dimensions and typed scope');
  end;
  Result := Default(TNyxResizeChange);
  Result.FControl := AControl;
  Result.FAxis := AAxis;
  Result.FSize := ASize;
  Result.FPlatform := APlatform;
end;

function TNyxResizeChange.ToData: TNyxDataValue;
begin
  NyxResizeControl(FControl, FAxis, FSize, FPlatform);
  Result := NyxObject([NyxField('control', NyxData(FControl.ID)),
    NyxField('axis', NyxData(Ord(FAxis))), NyxField('width', NyxData(FSize.Width)),
    NyxField('height', NyxData(FSize.Height)), NyxField('platform', NyxData(Ord(FPlatform)))]);
end;

class function TNyxResizeChange.FromData(const AData: TNyxDataValue): TNyxResizeChange;
var
  LAxis: Integer;
  LPlatform: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 5) then
  begin
    raise ENyxModel.Create('Resize payload requires its five exact fields');
  end;
  LAxis := AData.Field('axis').AsInteger;
  LPlatform := AData.Field('platform').AsInteger;

  if (LAxis < Ord(Low(TNyxResizeAxis))) or (LAxis > Ord(High(TNyxResizeAxis))) or
    (LPlatform < Ord(Low(TNyxPlatform))) or (LPlatform > Ord(High(TNyxPlatform))) then
  begin
    raise ENyxModel.Create('Resize payload contains an unknown closed choice');
  end;
  Result := NyxResizeControl(NyxControl(AData.Field('control').AsText),
    TNyxResizeAxis(LAxis), NyxResizeSize(AData.Field('width').AsInteger,
    AData.Field('height').AsInteger), TNyxPlatform(LPlatform));
end;

function TNyxResizeChange.SameChange(const AOther: TNyxResizeChange): Boolean;
begin
  Result := (FControl.ID = AOther.FControl.ID) and (FAxis = AOther.FAxis) and
    FSize.SameSize(AOther.FSize) and (FPlatform = AOther.FPlatform);
end;

function TNyxResizeChange.Operation(AClearFlex: Boolean): TNyxDataValue;
var
  LFields: array of TNyxDataField;

  procedure Add(AAttribute: TNyxAttribute; const AValue: TNyxDataValue);
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField(NyxPlatformKey(FPlatform, AAttribute), AValue);
  end;

begin
  ToData;
  LFields := nil;

  if FAxis in [nraWidth, nraBoth] then
  begin
    Add(atWidth, NyxData(FSize.Width));
    Add(atWidthSizing, NyxData(NyxSizingName(nsAutomatic)));
  end;

  if FAxis in [nraHeight, nraBoth] then
  begin
    Add(atHeight, NyxData(FSize.Height));
    Add(atHeightSizing, NyxData(NyxSizingName(nsAutomatic)));
  end;

  if AClearFlex then
  begin
    Add(atFlex, NyxData(0));
  end;
  Result := NyxObject([NyxField('op', NyxData('update')),
    NyxField('id', NyxData(FControl.ID)), NyxField('properties', NyxObject(LFields))]);
end;

type
  TDesignOperation = record
    Operation: TNyxDesignOperation;
    ID: TNyxText;
    Kind: TNyxText;
    Parent: TNyxText;
    Index: Integer;
    Root: TNyxText;
    Properties: TNyxDataValue;
    Source: TNyxText;
    Component: TNyxComponentRef;
    Path: TNyxPartRef;
    Mode: TNyxOverrideMode;
    Identities: array of TNyxIdentityAssignment;
    Placement: TNyxPlacement;
  end;

  TDesignPatch = class(TInterfacedObject, INyxDesignPatch)
  private
    FOperations: array of TDesignOperation;
  public
    function Candidate(ADocument: TNyxDocument; ACatalog: TNyxCatalog): TNyxDocument;
  end;

function NyxPlacementName(APlacement: TNyxPlacement): TNyxText;
begin
  { Ordinal dispatch preserves explicit foreign-cast refusal and avoids treating
    the rejection path as unreachable under the matched compiler's enum range. }
  case Ord(APlacement) of
    Ord(nplInside): Result := 'inside';
    Ord(nplBefore): Result := 'before';
    Ord(nplAfter): Result := 'after';
    else
    begin
      raise ENyxModel.Create('Placement requires inside, before or after');
    end;
  end;
end;

function ReadNyxPlacement(const AName: TNyxText): TNyxPlacement;
var
  LPlacement: TNyxPlacement;
begin
  for LPlacement := Low(TNyxPlacement) to High(TNyxPlacement) do
  begin

    if AName = NyxPlacementName(LPlacement) then
    begin
      Exit(LPlacement);
    end;
  end;
  raise ENyxModel.Create('Placement requires inside, before or after');
end;

function NyxPlaceControl(const AControl, ATarget: TNyxControlRef;
  APlacement: TNyxPlacement): TNyxPlacementChange;
begin

  if (AControl.ID = '') or (ATarget.ID = '') or
    (Ord(APlacement) < Ord(Low(TNyxPlacement))) or
    (Ord(APlacement) > Ord(High(TNyxPlacement))) then
  begin
    raise ENyxModel.Create('Placement requires exact identities and a closed location');
  end;
  Result := Default(TNyxPlacementChange);
  Result.FDefined := True;
  Result.FControl := AControl;
  Result.FTarget := ATarget;
  Result.FPlacement := APlacement;
end;

function NyxPlaceNewControl(AKind: TNyxKind; const AControl, ATarget: TNyxControlRef;
  APlacement: TNyxPlacement): TNyxPlacementChange;
begin
  Result := NyxPlaceNewControl(NyxCustomKind(NyxKindName(AKind)), AControl, ATarget,
    APlacement);
end;

function NyxPlaceNewControl(const AKind: TNyxKindRef;
  const AControl, ATarget: TNyxControlRef;
  APlacement: TNyxPlacement): TNyxPlacementChange;
begin

  if AKind.Name = '' then
  begin
    raise ENyxModel.Create('New placement requires a catalog kind');
  end;
  Result := NyxPlaceControl(AControl, ATarget, APlacement);
  Result.FKind := AKind;
end;

function TNyxPlacementChange.ToData: TNyxDataValue;
var
  LOperation: TNyxText;
begin

  if not FDefined then
  begin
    raise ENyxModel.Create('Placement command was not constructed');
  end;
  LOperation := 'place';

  if FKind.Name <> '' then
  begin
    LOperation := 'place-new';
  end;
  Result := NyxObject([NyxField('op', NyxData(LOperation)),
    NyxField('id', NyxData(FControl.ID)), NyxField('target', NyxData(FTarget.ID)),
    NyxField('placement', NyxData(NyxPlacementName(FPlacement)))]);

  if FKind.Name <> '' then
  begin
    Result := NyxObject([NyxField('op', NyxData(LOperation)),
      NyxField('id', NyxData(FControl.ID)), NyxField('target', NyxData(FTarget.ID)),
      NyxField('placement', NyxData(NyxPlacementName(FPlacement))),
      NyxField('kind', NyxData(FKind.Name))]);
  end;
end;

class function TNyxPlacementChange.FromData(const AData: TNyxDataValue): TNyxPlacementChange;
var
  LOperation: TNyxText;
begin

  if AData.Kind <> ndObject then
  begin
    raise ENyxModel.Create('Placement requires an exact object');
  end;
  LOperation := AData.Field('op').AsText;

  if (LOperation = 'place') and (AData.Count = 4) then
  begin
    Exit(NyxPlaceControl(NyxControl(AData.Field('id').AsText),
      NyxControl(AData.Field('target').AsText), ReadNyxPlacement(AData.Field('placement').AsText)));
  end;

  if (LOperation = 'place-new') and (AData.Count = 5) then
  begin
    Exit(NyxPlaceNewControl(NyxCustomKind(AData.Field('kind').AsText),
      NyxControl(AData.Field('id').AsText), NyxControl(AData.Field('target').AsText),
      ReadNyxPlacement(AData.Field('placement').AsText)));
  end;
  raise ENyxModel.Create('Placement requires its exact closed shape');
end;

function TNyxPlacementChange.SameChange(const AOther: TNyxPlacementChange): Boolean;
begin
  Result := (FDefined = AOther.FDefined) and (FControl.ID = AOther.FControl.ID) and
    (FTarget.ID = AOther.FTarget.ID) and (FKind.Name = AOther.FKind.Name) and
    (FPlacement = AOther.FPlacement);
end;

function NyxPlacementPatch(const AChanges: array of TNyxPlacementChange): INyxDesignPatch;
var
  LOwner: TDesignPatch;
  LIndex: Integer;
begin

  if (Length(AChanges) < 1) or (Length(AChanges) > 64) then
  begin
    raise ENyxModel.Create('A transaction requires 1..64 operations');
  end;
  LOwner := TDesignPatch.Create;
  Result := LOwner;
  SetLength(LOwner.FOperations, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin

    if not AChanges[LIndex].FDefined then
    begin
      raise ENyxModel.Create('Placement command was not constructed');
    end;
    LOwner.FOperations[LIndex].Operation := doPlace;

    if AChanges[LIndex].Kind.Name <> '' then
    begin
      LOwner.FOperations[LIndex].Operation := doPlaceNew;
    end;
    LOwner.FOperations[LIndex].ID := AChanges[LIndex].Control.ID;
    LOwner.FOperations[LIndex].Parent := AChanges[LIndex].Target.ID;
    LOwner.FOperations[LIndex].Kind := AChanges[LIndex].Kind.Name;
    LOwner.FOperations[LIndex].Placement := AChanges[LIndex].Placement;
  end;
end;

function NyxDeriveComponent(const ASource: TNyxControlRef;
  const ADefinition: TNyxComponentRef;
  const AIdentities: array of TNyxIdentityAssignment): TNyxReusableChange;
var
  LIndex: Integer;
begin
  Result := Default(TNyxReusableChange);
  Result.FOperation := doDerive;
  Result.FOwner := ASource;
  Result.FDefinition := ADefinition;
  SetLength(Result.FIdentities, Length(AIdentities));
  for LIndex := 0 to High(AIdentities) do
  begin
    Result.FIdentities[LIndex] := AIdentities[LIndex];
  end;
end;

function NyxInstantiateComponent(const ADefinition: TNyxComponentRef;
  const AControl, AParent: TNyxControlRef; AIndex: Integer): TNyxReusableChange;
begin

  if AIndex < -1 then
  begin
    raise ENyxModel.Create('Instantiation index must be -1 or an exact nonnegative position');
  end;
  Result := Default(TNyxReusableChange);
  Result.FOperation := doInstance;
  Result.FDefinition := ADefinition;
  Result.FControl := AControl;
  Result.FOwner := AParent;
  Result.FIndex := AIndex;
end;

function NyxOverrideComponentPart(const AInstance, ADescriptor: TNyxControlRef;
  const APath: TNyxPartRef; AMode: TNyxOverrideMode): TNyxReusableChange;
begin

  if not (AMode in [noProperties, noAppend, noPrepend, noReplace, noRemove]) then
  begin
    raise ENyxModel.Create('Override requires a closed operation');
  end;
  Result := Default(TNyxReusableChange);
  Result.FOperation := doOverride;
  Result.FOwner := AInstance;
  Result.FControl := ADescriptor;
  Result.FPath := APath;
  Result.FMode := AMode;
end;

function NyxInheritComponentPart(const AInstance, ADescriptor: TNyxControlRef;
  const APath: TNyxPartRef): TNyxReusableChange;
begin
  Result := NyxOverrideComponentPart(AInstance, ADescriptor, APath, noProperties);
  Result.FOperation := doInherit;
end;

function NyxReusablePatch(const AChanges: array of TNyxReusableChange): INyxDesignPatch;
var
  LOwner: TDesignPatch;
  LIndex, LMap: Integer;
  LOperation: TDesignOperation;
begin

  if (Length(AChanges) < 1) or (Length(AChanges) > 64) then
  begin
    raise ENyxModel.Create('A transaction requires 1..64 operations');
  end;
  LOwner := TDesignPatch.Create;
  Result := LOwner;
  SetLength(LOwner.FOperations, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    LOperation := Default(TDesignOperation);
    LOperation.Operation := AChanges[LIndex].FOperation;

    if not (LOperation.Operation in [doDerive, doInstance, doOverride, doInherit]) then
    begin
      raise ENyxModel.Create('Reusable transaction contains an unconstructed command');
    end;
    LOperation.ID := AChanges[LIndex].FControl.ID;
    LOperation.Parent := AChanges[LIndex].FOwner.ID;
    LOperation.Source := AChanges[LIndex].FOwner.ID;
    LOperation.Component := AChanges[LIndex].FDefinition;
    LOperation.Path := AChanges[LIndex].FPath;
    LOperation.Mode := AChanges[LIndex].FMode;
    LOperation.Index := AChanges[LIndex].FIndex;
    LOperation.Properties := NyxObject([]);
    SetLength(LOperation.Identities, Length(AChanges[LIndex].FIdentities));
    for LMap := 0 to High(LOperation.Identities) do
    begin
      LOperation.Identities[LMap] := AChanges[LIndex].FIdentities[LMap];
    end;
    LOwner.FOperations[LIndex] := LOperation;
  end;
end;

function HasField(const AObject: TNyxDataValue; const AKey: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AObject.Count - 1 do
  begin

    if AObject.Key(LIndex) = AKey then
    begin
      Exit(True);
    end;
  end;
end;

procedure CheckFields(const AObject: TNyxDataValue; const AAllowed: TNyxText);
var
  LIndex: Integer;
begin

  if AObject.Kind <> ndObject then
  begin
    raise ENyxModel.Create('A semantic operation must be an object');
  end;
  for LIndex := 0 to AObject.Count - 1 do
  begin

    if Pos('|' + AObject.Key(LIndex) + '|', AAllowed) = 0 then
    begin
      raise ENyxModel.Create('Unknown operation field: ' + AObject.Key(LIndex));
    end;
  end;
end;

function ReadNyxDesignPatch(const AOperations: TNyxDataValue): INyxDesignPatch;
var
  LOwner: TDesignPatch;
  LIndex: Integer;
  LWire: TNyxDataValue;
  LName: TNyxText;
  LOperation: TDesignOperation;
  LMap: TNyxDataValue;
  LMapIndex: Integer;
  LMode: TNyxOverrideMode;
  LFound: Boolean;
  LPlacement: TNyxPlacementChange;
begin

  if (AOperations.Kind <> ndArray) or (AOperations.Count < 1) or
    (AOperations.Count > 64) then
  begin
    raise ENyxModel.Create('A transaction requires 1..64 operations');
  end;
  LOwner := TDesignPatch.Create;
  Result := LOwner;
  SetLength(LOwner.FOperations, AOperations.Count);
  for LIndex := 0 to AOperations.Count - 1 do
  begin
    LWire := AOperations.Item(LIndex);
    LOperation := Default(TDesignOperation);
    LOperation.Index := -1;
    LOperation.Properties := NyxObject([]);
    LName := LWire.Field('op').AsText;

    if (LName = 'place') or (LName = 'place-new') then
    begin
      LPlacement := TNyxPlacementChange.FromData(LWire);
      LOperation.Operation := doPlace;

      if LName = 'place-new' then
      begin
        LOperation.Operation := doPlaceNew;
      end;
      LOperation.ID := LPlacement.Control.ID;
      LOperation.Parent := LPlacement.Target.ID;
      LOperation.Kind := LPlacement.Kind.Name;
      LOperation.Placement := LPlacement.Placement;
    end
    else if LName = 'create' then
    begin
      LOperation.Operation := doCreate;
      CheckFields(LWire, '|op|id|kind|parent|index|root|properties|');
      LOperation.Kind := LWire.Field('kind').AsText;

      if HasField(LWire, 'root') then
      begin
        { A root and a child placement describe different ownership contracts.
          Refuse ambiguous input rather than silently ignoring its placement. }

        if HasField(LWire, 'parent') or HasField(LWire, 'index') then
        begin
          raise ENyxModel.Create('Root creation cannot also specify parent/index');
        end;
        LOperation.Root := LWire.Field('root').AsText;

        if (LOperation.Root <> 'page') and (LOperation.Root <> 'component') then
        begin
          raise ENyxModel.Create('Root must be page or component');
        end;
      end
      else
      begin
        LOperation.Parent := LWire.Field('parent').AsText;
      end;
      LOperation.ID := LWire.Field('id').AsText;
    end
    else if LName = 'derive' then
    begin
      LOperation.Operation := doDerive;
      CheckFields(LWire, '|op|source|id|identities|');
      LOperation.Source := LWire.Field('source').AsText;
      LOperation.Component := NyxComponent(LWire.Field('id').AsText);
      LMap := LWire.Field('identities');

      if LMap.Kind <> ndObject then
      begin
        raise ENyxModel.Create('Derivation identities require an exact source-to-destination object');
      end;
      SetLength(LOperation.Identities, LMap.Count);
      for LMapIndex := 0 to LMap.Count - 1 do
      begin
        LOperation.Identities[LMapIndex] := NyxIdentity(NyxControl(LMap.Key(LMapIndex)),
          NyxControl(LMap.Field(LMap.Key(LMapIndex)).AsText));
      end;
    end
    else if LName = 'instance' then
    begin
      LOperation.Operation := doInstance;
      CheckFields(LWire, '|op|id|component|parent|index|');
      LOperation.ID := LWire.Field('id').AsText;
      LOperation.Component := NyxComponent(LWire.Field('component').AsText);
      LOperation.Parent := LWire.Field('parent').AsText;
    end
    else if (LName = 'override') or (LName = 'inherit') then
    begin
      LOperation.Operation := doInherit;
      LOperation.ID := LWire.Field('id').AsText;
      LOperation.Parent := LWire.Field('instance').AsText;
      LOperation.Path := NyxPart(LWire.Field('path').AsText);

      if LName = 'override' then
      begin
        CheckFields(LWire, '|op|id|instance|path|mode|');
        LOperation.Operation := doOverride;
        LFound := False;
        for LMode := Low(TNyxOverrideMode) to High(TNyxOverrideMode) do
        begin

          if NyxOverrideName(LMode) = LWire.Field('mode').AsText then
          begin
            LOperation.Mode := LMode;
            LFound := True;
            Break;
          end;
        end;

        if not LFound then
        begin
          raise ENyxModel.Create('Override mode must be properties, append, prepend, replace or remove');
        end;
      end
      else
      begin
        CheckFields(LWire, '|op|id|instance|path|');
      end;
    end
    else if LName = 'update' then
    begin
      LOperation.Operation := doUpdate;
      CheckFields(LWire, '|op|id|properties|');
      LOperation.ID := LWire.Field('id').AsText;
      LOperation.Properties := LWire.Field('properties');
    end
    else if LName = 'move' then
    begin
      LOperation.Operation := doMove;
      CheckFields(LWire, '|op|id|parent|index|');
      LOperation.ID := LWire.Field('id').AsText;
      LOperation.Parent := LWire.Field('parent').AsText;
    end
    else if LName = 'delete' then
    begin
      LOperation.Operation := doDelete;
      CheckFields(LWire, '|op|id|');
      LOperation.ID := LWire.Field('id').AsText;
    end
    else if LName = 'title' then
    begin
      LOperation.Operation := doTitle;
      CheckFields(LWire, '|op|value|');
      LOperation.ID := LWire.Field('value').AsText;
    end
    else if LName = 'tokens' then
    begin
      LOperation.Operation := doTokens;
      CheckFields(LWire, '|op|values|');
      LOperation.Properties := LWire.Field('values');
    end
    else
    begin
      raise ENyxModel.Create('Unknown semantic operation: ' + LName);
    end;

    if HasField(LWire, 'index') then
    begin
      LOperation.Index := LWire.Field('index').AsInteger;

      if LOperation.Index < 0 then
      begin
        raise ENyxModel.Create('Explicit insertion index cannot be negative');
      end;
    end;

    if (LOperation.Operation = doCreate) and HasField(LWire, 'properties') then
    begin
      LOperation.Properties := LWire.Field('properties');
    end;

    if LOperation.Properties.Kind <> ndObject then
    begin
      raise ENyxModel.Create('Properties/tokens require typed object values');
    end;
    LOwner.FOperations[LIndex] := LOperation;
  end;
end;

procedure ConfigureNode(ANode: TNyxNode; ADocument: TNyxDocument;
  const AProperties: TNyxDataValue);
var
  LInfos: TNyxPropertyInfos;
  LIndex: Integer;
  LInfoIndex: Integer;
  LKey: TNyxText;
  LValue: TNyxDataValue;
  LText: TNyxText;
begin
  { This node belongs exclusively to a detached candidate. Stage the complete
    scalar payload before resolving metadata: selectors such as input type,
    projection and reusable definition can change the value domain or available
    properties in the same operation. JSON member order has no semantic meaning.
    Keep the original typed values for admission below; staging a spelling never
    authorizes a string as a number/Boolean or publishes an unknown property. }
  for LIndex := 0 to AProperties.Count - 1 do
  begin
    LKey := AProperties.Key(LIndex);
    LValue := AProperties.Field(LKey);
    case LValue.Kind of
      ndNull:
        begin
          LText := '';
        end;
      ndText:
        begin
          LText := LValue.AsText;
        end;
      ndBoolean:
        begin

          if LValue.AsBoolean then
          begin
            LText := 'true';
          end
          else
          begin
            LText := 'false';
          end;
        end;
      ndNumber:
        begin
          LText := LValue.AsDecimal.Text;
        end;
      else
      begin
        raise ENyxModel.Create('Component properties require scalar JSON values');
      end;
    end;
    ANode.SetProp(LKey, LText);
  end;
  LInfos := NyxProperties(ANode, ADocument);
  for LIndex := 0 to AProperties.Count - 1 do
  begin
    LKey := AProperties.Key(LIndex);
    LInfoIndex := 0;
    while (LInfoIndex < Length(LInfos)) and (LInfos[LInfoIndex].Key <> LKey) do
    begin
      Inc(LInfoIndex);
    end;

    if LInfoIndex = Length(LInfos) then
    begin
      raise ENyxModel.Create('Property is not published for this component: ' + LKey);
    end;
    LValue := AProperties.Field(LKey);

    if LValue.Kind = ndNull then
    begin
      LText := '';
    end
    else
    begin
      case LInfos[LInfoIndex].ValueType of
        npBoolean:
          begin

            if LValue.AsBoolean then
            begin
              LText := 'true';
            end
            else
            begin
              LText := 'false';
            end;
          end;
        npInteger: LText := IntToStr(LValue.AsInteger);
        npNumber: LText := LValue.AsDecimal.Text;
        else
        begin
          LText := LValue.AsText;
        end;
      end;
    end;
    { SetProp is the explicit schema/serialization boundary. Agent strings cannot
      bypass their published scalar types; the entire document validates below. }
    ANode.SetProp(LKey, LText);
  end;
end;

function RequireNode(ADocument: TNyxDocument; const AID: TNyxText): TNyxNode;
begin
  Result := ADocument.Find(AID);

  if Result = nil then
  begin
    raise ENyxModel.Create('Component does not exist: ' + AID);
  end;
end;

{ Exact container admission for relative placement. Reusable references own
  override descriptors rather than ordinary children. A customized layout part
  accepts content; a properties-only descriptor becomes an append rule when
  content arrives, matching ordinary inspector insertion behavior. }
procedure RequirePlacementContainer(ADocument: TNyxDocument; ACatalog: TNyxCatalog;
  AParent: TNyxNode);
var
  LIndex: Integer;
  LRuntime: TNyxNode;
  LInfo: TNyxPrimitiveInfo;
begin

  if (AParent.ProjectionKind = NyxKindName(nkComponent)) and
    (AParent.Prop('component') <> '') then
  begin
    raise ENyxModel.Create('Customize a named layout part before placing instance content');
  end;

  if AParent.Kind = 'slot-override' then
  begin

    if (AParent.Parent = nil) or
      ((AParent.Prop('mode') <> 'properties') and
       (AParent.Prop('mode') <> 'append') and (AParent.Prop('mode') <> 'prepend')) then
    begin
      raise ENyxModel.Create('Placement requires an editable appended layout part');
    end;
    LRuntime := RealizeNyxView(ADocument, AParent.Parent);
    try

      if not FindNyxPrimitive(LRuntime.Part(AParent.Prop('path')).ProjectionKind, LInfo) or
        not LInfo.Container then
      begin
        raise ENyxModel.Create('This named part is a leaf and cannot contain controls');
      end;
    finally
      LRuntime.Free;
    end;
    Exit;
  end;
  LIndex := ACatalog.IndexOf(AParent.Kind);

  if (LIndex < 0) or not ACatalog[LIndex].Container then
  begin
    raise ENyxModel.Create('Inside placement requires an exact editable container');
  end;
end;

function TDesignPatch.Candidate(ADocument: TNyxDocument;
  ACatalog: TNyxCatalog): TNyxDocument;
var
  LIndex: Integer;
  LChildIndex: Integer;
  LInsert: Integer;
  LOperation: TDesignOperation;
  LNode: TNyxNode;
  LParent: TNyxNode;
  LAncestor: TNyxNode;
  LRule: TNyxNode;
  LTarget: TNyxNode;
begin
  Result := ADocument.Clone;
  try
    for LIndex := 0 to High(FOperations) do
    begin
      LOperation := FOperations[LIndex];
      case LOperation.Operation of
        doPlace, doPlaceNew:
          begin
            LTarget := RequireNode(Result, LOperation.Parent);
            LParent := LTarget;

            if LOperation.Placement <> nplInside then
            begin
              LParent := LTarget.Parent;
            end;

            if LParent = nil then
            begin
              raise ENyxModel.Create('Relative placement cannot reorder document roots');
            end;
            RequirePlacementContainer(Result, ACatalog, LParent);

            if LOperation.Operation = doPlace then
            begin
              LNode := RequireNode(Result, LOperation.ID);

              if (LNode.Parent = nil) or (LNode.Kind = 'slot-override') then
              begin
                raise ENyxModel.Create('Placement moves authored controls, not roots or part descriptors');
              end;

              if LNode = LTarget then
              begin
                raise ENyxModel.Create('A control cannot be placed relative to itself');
              end;
              LAncestor := LParent;
              while LAncestor <> nil do
              begin

                if LAncestor = LNode then
                begin
                  raise ENyxModel.Create('Placement would create an ownership cycle');
                end;
                LAncestor := LAncestor.Parent;
              end;
              LChildIndex := 0;
              while LNode.Parent.Children[LChildIndex] <> LNode do
              begin
                Inc(LChildIndex);
              end;
              LNode.Parent.Extract(LChildIndex);
            end
            else
            begin

              if Result.Find(LOperation.ID) <> nil then
              begin
                raise ENyxModel.Create('New placement identity is already occupied');
              end;

              if (LOperation.Kind = NyxKindName(nkPage)) or
                (LOperation.Kind = NyxKindName(nkComponent)) or
                (LOperation.Kind = 'slot-override') then
              begin
                raise ENyxModel.Create('Placement creates palette controls; use dedicated root/instance operations');
              end;
              LNode := ACatalog.NewNode(LOperation.Kind, LOperation.ID);
            end;
            try
              { Resolve after extraction so a same-parent move cannot shift the
                target accidentally. Both the target and parent are still owned
                by the independent candidate; no accepted node is borrowed. }
              LInsert := LParent.Count;

              if LOperation.Placement <> nplInside then
              begin
                LInsert := 0;
                while LParent.Children[LInsert] <> LTarget do
                begin
                  Inc(LInsert);
                end;

                if LOperation.Placement = nplAfter then
                begin
                  Inc(LInsert);
                end;
              end;
              LParent.Insert(LInsert, LNode);
              LNode := nil;

              if (LParent.Kind = 'slot-override') and (LParent.Prop('mode') = 'properties') then
              begin
                LParent.SetProp('mode', NyxOverrideName(noAppend));
              end;
            finally
              LNode.Free;
            end;
          end;
        doDerive:
          begin
            LNode := CloneNyxReusableDefinition(Result,
              RequireNode(Result, LOperation.Source), LOperation.Component,
              LOperation.Identities);
            try
              Result.AddComponent(LNode);
              LNode := nil;
            finally
              LNode.Free;
            end;
          end;
        doInstance:
          begin

            if (LOperation.ID = '') or (Result.Find(LOperation.ID) <> nil) or
              (Result.FindComponent(LOperation.Component.Name) = nil) then
            begin
              raise ENyxModel.Create('Instantiation requires a new identity and an existing reusable definition');
            end;
            LParent := RequireNode(Result, LOperation.Parent);
            LNode := TNyxNode.Create(nkComponent, LOperation.ID);
            try
              LNode.Configure.Component(LOperation.Component).Done;
              LInsert := LOperation.Index;

              if LInsert < 0 then
              begin
                LInsert := LParent.Count;
              end;
              LParent.Insert(LInsert, LNode);
              LNode := nil;
            finally
              LNode.Free;
            end;
          end;
        doOverride, doInherit:
          begin
            LNode := RequireNode(Result, LOperation.Parent);

            if (LNode.ProjectionKind <> NyxKindName(nkComponent)) or
              (LOperation.ID = '') then
            begin
              raise ENyxModel.Create('Part editing requires a reusable instance and exact descriptor identity');
            end;
            LRule := nil;
            for LChildIndex := 0 to LNode.Count - 1 do
            begin

              if LNode.Children[LChildIndex].Prop('path') = LOperation.Path.Name then
              begin
                LRule := LNode.Children[LChildIndex];
                Break;
              end;
            end;

            if (LRule <> nil) and (LRule.ID <> LOperation.ID) then
            begin
              raise ENyxModel.Create('The exact part descriptor identity changed');
            end;

            if LOperation.Operation = doInherit then
            begin

              if LRule = nil then
              begin
                raise ENyxModel.Create('Restored inheritance requires an existing local part descriptor');
              end;
              LNode.Remove(LRule);
            end
            else
            begin

              if (LRule = nil) and (Result.Find(LOperation.ID) <> nil) then
              begin
                raise ENyxModel.Create('Part descriptor identity is already occupied');
              end;
              LNode.OverridePart(LOperation.Path, LOperation.Mode).Named(LOperation.ID);
            end;
          end;
        doCreate:
          begin

            if Result.Find(LOperation.ID) <> nil then
            begin
              raise ENyxModel.Create('Component ID is already in use: ' + LOperation.ID);
            end;
            LNode := ACatalog.NewNode(LOperation.Kind, LOperation.ID);
            try
              if LOperation.Root = 'page' then
              begin
                Result.AddPage(LNode);
              end
              else if LOperation.Root = 'component' then
              begin
                Result.AddComponent(LNode);
              end
              else
              begin
                LParent := RequireNode(Result, LOperation.Parent);
                LInsert := LOperation.Index;

                if LInsert < 0 then
                begin
                  LInsert := LParent.Count;
                end;
                LParent.Insert(LInsert, LNode);
              end;
              LNode := nil;
              { Ownership transfers before metadata lookup so declared ancestor
                field domains and reusable-part context participate in admission.
                Failure releases the entire candidate, never the accepted tree. }
              ConfigureNode(RequireNode(Result, LOperation.ID), Result,
                LOperation.Properties);
            finally
              LNode.Free;
            end;
          end;
        doUpdate: ConfigureNode(RequireNode(Result, LOperation.ID), Result,
          LOperation.Properties);
        doMove, doDelete:
          begin
            LNode := RequireNode(Result, LOperation.ID);

            if LNode.Parent = nil then
            begin
              raise ENyxModel.Create('Page/component roots cannot be moved or deleted by a control operation');
            end;

            if LOperation.Operation = doDelete then
            begin
              LNode.Parent.Remove(LNode);
            end
            else
            begin
              LParent := RequireNode(Result, LOperation.Parent);
              LAncestor := LParent;
              while LAncestor <> nil do
              begin

                if LAncestor = LNode then
                begin
                  raise ENyxModel.Create('Moving a component would create a cycle');
                end;
                LAncestor := LAncestor.Parent;
              end;
              LChildIndex := 0;
              while LNode.Parent.Children[LChildIndex] <> LNode do
              begin
                Inc(LChildIndex);
              end;
              LNode.Parent.Extract(LChildIndex);
              try
                LInsert := LOperation.Index;

                if LInsert < 0 then
                begin
                  LInsert := LParent.Count;
                end;
                LParent.Insert(LInsert, LNode);
                LNode := nil;
              finally
                LNode.Free;
              end;
            end;
          end;
        doTitle: Result.Title := LOperation.ID;
        doTokens: SetNyxDesignTokens(Result, LOperation.Properties);
      end;
    end;
    Result.Validate;
    ValidateNyxDocumentProperties(Result);
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

end.
