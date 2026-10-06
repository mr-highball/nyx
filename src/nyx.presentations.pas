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
unit nyx.presentations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.responsive, nyx.containers;

const
  NyxMaximumPresentations = 64;
  NyxPresentationsWireField = 'presentations';
  { Version-four nodes carry long named scopes as array values. Older native
    fpjson object-key hashing truncates names at 255 bytes; application names
    must never be routed through that implementation limit. }
  NyxPresentationRulesWireField = 'presentationRules';

type
  ENyxPresentation = class(Exception);
  INyxPresentationSnapshot = interface;

  { Exact open application name, independent of Pascal identifiers, target
    widgets and document pointers. Default is absent; reading it refuses. }
  TNyxPresentationRef = record
  private
    FName: TNyxText;
    function GetName: TNyxText;
    function GetDefined: Boolean;
  public
    property Name: TNyxText read GetName;
    property Defined: Boolean read GetDefined;
  end;

  { A presentation is either selected automatically by available host/container space or
    explicitly by the application/editor. Manual definitions deliberately carry
    no hidden viewport predicate. The default record is an automatic Any value;
    registry admission refuses it because ordinary configuration owns defaults. }
  TNyxPresentationActivation = (npaAutomatic, npaManual);
  TNyxPresentationCondition = record
  private
    FActivation: TNyxPresentationActivation;
    FViewport: TNyxViewportCondition;
    FContainer: TNyxContainerRef;
  public
    class function Automatic(const AViewport: TNyxViewportCondition): TNyxPresentationCondition; static;
    { Match the nearest eligible named ancestor's measured content box. Missing
      publishers/measurements remain inactive; self is never its own container. }
    class function Within(const AContainer: TNyxContainerRef;
      const ACondition: TNyxViewportCondition): TNyxPresentationCondition; static;
    class function Manual: TNyxPresentationCondition; static;
    function Same(const AOther: TNyxPresentationCondition): Boolean;
    function Caption: TNyxText;
    function Pascal: TNyxText;
    property Activation: TNyxPresentationActivation read FActivation;
    { Manual returns Any here for geometry inspection only. It never becomes
      active through Matches; selection must be evaluated separately. }
    property Viewport: TNyxViewportCondition read FViewport;
    property Container: TNyxContainerRef read FContainer;
  end;

  { Copied, exclusive view-local manual choice. Automatic host presentations
    continue to apply. None restores automatic/default configuration. No tree,
    registry or UI lifetime is retained; every mounted owner validates a choice
    against its own immutable definition snapshot before changing presentation. }
  TNyxPresentationSelection = record
  private
    FReference: TNyxPresentationRef;
  public
    class function None: TNyxPresentationSelection; static;
    class function Use(const AReference: TNyxPresentationRef): TNyxPresentationSelection; static;
    procedure Validate(const APresentations: INyxPresentationSnapshot);
    { After an admitted definition refresh, retain an exact manual choice or
      clear one whose definition was removed/replaced by automatic activation.
      This is copied presentation state, never an authored mutation. }
    function Reconciled(const APresentations: INyxPresentationSnapshot): TNyxPresentationSelection;
    function Matches(const AReference: TNyxPresentationRef): Boolean;
    function Same(const AOther: TNyxPresentationSelection): Boolean;
    property Reference: TNyxPresentationRef read FReference;
  end;

  { Portable managed view capability. Applications need neither renderer class
    nor target directives to select a named configuration. The interface does
    not own its view; retained capabilities become inert after mount retirement.
    Calls that read/change selection then raise ENyxPresentation. }
  INyxPresentationView = interface(IInterface)
    ['{F7D16481-F0C5-478B-98E4-49D7D2B7A301}']
    function GetConnected: Boolean;
    function GetSelection: TNyxPresentationSelection;
    function Select(const AReference: TNyxPresentationRef): INyxPresentationView;
    function Automatic: INyxPresentationView;
    property Connected: Boolean read GetConnected;
    property Selection: TNyxPresentationSelection read GetSelection;
  end;

  { Adapter-owned lease, deliberately separate from the application capability.
    Retirement clears both borrowed receivers before any target/tree is freed. }
  INyxPresentationViewOwner = interface(INyxPresentationView)
    ['{60ADFA46-832E-4D6B-ADE5-324E41A20C44}']
    procedure Retire;
  end;
  TNyxPresentationRead = function: TNyxPresentationSelection of object;
  TNyxPresentationApply = procedure(const ASelection: TNyxPresentationSelection) of object;

  { Immutable independent registry snapshot. It retains copied values only,
    so rendered/reusable views can outlive a document without a backreference.
    Index and unknown-name reads refuse; definition order remains stable. }
  INyxPresentationSnapshot = interface(IInterface)
    ['{77665F5F-047D-4D03-A3B7-77FCE8A259F1}']
    function GetCount: Integer;
    function Reference(AIndex: Integer): TNyxPresentationRef;
    function Contains(const AReference: TNyxPresentationRef): Boolean;
    function Condition(const AReference: TNyxPresentationRef): TNyxViewportCondition;
    { Complete typed definition. The compatibility Condition accessor refuses
      manual definitions rather than treating them as an always-on viewport. }
    function Definition(const AReference: TNyxPresentationRef): TNyxPresentationCondition;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

  { Document-owned managed authoring registry. Define validates before mutation,
    replacing a condition at its original position. Remove does not rewrite
    controls; document admission must reject remaining references. Snapshots and
    clones own independent arrays, including on pas2js. No subscribers/tree/UI
    objects are retained. Runtime stores never mutate these authored defaults. }
  INyxPresentations = interface(INyxPresentationSnapshot)
    ['{1DCBE42C-C25D-4098-AC69-90FEC22B9E8B}']
    procedure Define(const AReference: TNyxPresentationRef;
      const ACondition: TNyxViewportCondition); overload;
    procedure Define(const AReference: TNyxPresentationRef;
      const ACondition: TNyxPresentationCondition); overload;
    procedure Remove(const AReference: TNyxPresentationRef);
    function Snapshot: INyxPresentationSnapshot;
    function Clone: INyxPresentations;
  end;

{ Names admit 1..128 Unicode scalars, refusing malformed/control/blank text.
  Names are case-sensitive, retain exact encoding and need not be identifiers. }
function NyxPresentation(const AName: TNyxText): TNyxPresentationRef;
function NewNyxPresentations: INyxPresentations;
{ Target boundary only. Receivers enforce UI-thread, current mount and exact
  definition admission. They remain borrowed until the owner retires the lease;
  the managed capability never forms a reference cycle back to the view. }
function NewNyxPresentationView(ARead: TNyxPresentationRead;
  AApply: TNyxPresentationApply): INyxPresentationViewOwner;
{ Strict structured boundary: legacy automatic-only version 1 and explicit
  automatic/manual version 2. Unknown/missing fields, duplicate names, manual
  viewport predicates and budget excess refuse before returning a candidate. }
function NyxPresentationsFromData(const AData: TNyxDataValue): INyxPresentations;
{ One strict definition at the structured editor/MCP boundary; it uses the
  same admission as a complete registry rather than a second condition grammar. }
function NyxPresentationDefinition(const AReference: TNyxPresentationRef;
  const ACondition: TNyxViewportCondition): TNyxDataValue; overload;
function NyxPresentationDefinition(const AReference: TNyxPresentationRef;
  const ACondition: TNyxPresentationCondition): TNyxDataValue; overload;
procedure ReadNyxPresentationDefinition(const AData: TNyxDataValue;
  out AReference: TNyxPresentationRef; out ACondition: TNyxViewportCondition); overload;
procedure ReadNyxPresentationDefinition(const AData: TNyxDataValue;
  out AReference: TNyxPresentationRef; out ACondition: TNyxPresentationCondition); overload;
{ Canonical reserved property namespace. Only presentation attributes are
  admitted. Target selection stays orthogonal to the document-owned condition. }
function NyxPresentationKey(const AReference: TNyxPresentationRef;
  APlatform: TNyxPlatform; AAttribute: TNyxAttribute): TNyxText;
function TryNyxPresentationKey(const AKey: TNyxText;
  out AReference: TNyxPresentationRef; out APlatform: TNyxPlatform;
  out AAttribute: TNyxAttribute): Boolean;
{ Resolve either anonymous or named scopes without changing property order.
  A well-formed named rule requires a known definition; absent context refuses.
  Malformed/non-responsive keys return False for the admission owner to reject. }
function TryNyxResponsiveKey(const AKey: TNyxText;
  const APresentations: INyxPresentationSnapshot; out ACondition: TNyxViewportCondition;
  out APlatform: TNyxPlatform; out AAttribute: TNyxAttribute): Boolean;
{ Complete automatic/manual rule inspection. Anonymous viewport scopes remain
  automatic. The legacy responsive accessor refuses a manual definition. }
function TryNyxPresentationRule(const AKey: TNyxText;
  const APresentations: INyxPresentationSnapshot; out ACondition: TNyxPresentationCondition;
  out APlatform: TNyxPlatform; out AAttribute: TNyxAttribute): Boolean;

implementation

type
  TPresentationView = class(TInterfacedObject, INyxPresentationView, INyxPresentationViewOwner)
  private
    FRead: TNyxPresentationRead;
    FApply: TNyxPresentationApply;
    procedure RequireConnected;
  public
    constructor Create(ARead: TNyxPresentationRead; AApply: TNyxPresentationApply);
    function GetConnected: Boolean;
    function GetSelection: TNyxPresentationSelection;
    function Select(const AReference: TNyxPresentationRef): INyxPresentationView;
    function Automatic: INyxPresentationView;
    procedure Retire;
  end;
  TPresentationEntry = record
    Reference: TNyxPresentationRef;
    Condition: TNyxPresentationCondition;
  end;

constructor TPresentationView.Create(ARead: TNyxPresentationRead; AApply: TNyxPresentationApply);
begin
  inherited Create;

  if not Assigned(ARead) or not Assigned(AApply) then
  begin
    raise ENyxPresentation.Create('A presentation view requires both borrowed receivers');
  end;
  FRead := ARead;
  FApply := AApply;
end;

function NewNyxPresentationView(ARead: TNyxPresentationRead;
  AApply: TNyxPresentationApply): INyxPresentationViewOwner;
begin
  Result := TPresentationView.Create(ARead, AApply);
end;

procedure TPresentationView.RequireConnected;
begin

  if not GetConnected then
  begin
    raise ENyxPresentation.Create('This presentation view has retired');
  end;
end;

function TPresentationView.GetConnected: Boolean;
begin
  Result := Assigned(FRead) and Assigned(FApply);
end;

function TPresentationView.GetSelection: TNyxPresentationSelection;
begin
  RequireConnected;
  Result := FRead();
end;

function TPresentationView.Select(const AReference: TNyxPresentationRef): INyxPresentationView;
begin
  RequireConnected;
  FApply(TNyxPresentationSelection.Use(AReference));
  Result := Self;
end;

function TPresentationView.Automatic: INyxPresentationView;
begin
  RequireConnected;
  FApply(TNyxPresentationSelection.None);
  Result := Self;
end;

procedure TPresentationView.Retire;
begin
  FRead := nil;
  FApply := nil;
end;
type
  TPresentationEntries = array of TPresentationEntry;

  TPresentationSnapshot = class(TInterfacedObject, INyxPresentationSnapshot)
  protected
    FEntries: TPresentationEntries;
    function IndexOf(const AReference: TNyxPresentationRef): Integer;
  public
    constructor Create(const AEntries: TPresentationEntries);
    function GetCount: Integer;
    function Reference(AIndex: Integer): TNyxPresentationRef;
    function Contains(const AReference: TNyxPresentationRef): Boolean;
    function Condition(const AReference: TNyxPresentationRef): TNyxViewportCondition;
    function Definition(const AReference: TNyxPresentationRef): TNyxPresentationCondition;
    function ToData: TNyxDataValue;
  end;

  TPresentations = class(TPresentationSnapshot, INyxPresentations)
  public
    procedure Define(const AReference: TNyxPresentationRef;
      const ACondition: TNyxViewportCondition); overload;
    procedure Define(const AReference: TNyxPresentationRef;
      const ACondition: TNyxPresentationCondition); overload;
    procedure Remove(const AReference: TNyxPresentationRef);
    function Snapshot: INyxPresentationSnapshot;
    function Clone: INyxPresentations;
  end;

function TNyxPresentationRef.GetName: TNyxText;
begin

  if FName = '' then
  begin
    raise ENyxPresentation.Create('Construct a presentation reference first');
  end;
  Result := FName;
end;

class function TNyxPresentationCondition.Automatic(
  const AViewport: TNyxViewportCondition): TNyxPresentationCondition;
begin
  Result := Default(TNyxPresentationCondition);
  Result.FViewport := AViewport;
end;

class function TNyxPresentationCondition.Manual: TNyxPresentationCondition;
begin
  Result := Automatic(TNyxViewportCondition.Any);
  Result.FActivation := npaManual;
end;

class function TNyxPresentationCondition.Within(const AContainer: TNyxContainerRef;
  const ACondition: TNyxViewportCondition): TNyxPresentationCondition;
begin

  if not AContainer.Defined then
  begin
    raise ENyxContainer.Create('A container presentation needs a named publisher');
  end;
  Result := Automatic(ACondition);
  Result.FContainer := AContainer;
end;

function TNyxPresentationCondition.Same(const AOther: TNyxPresentationCondition): Boolean;
begin
  Result := (FActivation = AOther.FActivation) and FViewport.Same(AOther.FViewport) and
    (FContainer.Defined = AOther.FContainer.Defined);

  if Result and FContainer.Defined then
  begin
    Result := FContainer.Name = AOther.FContainer.Name;
  end;
end;

function TNyxPresentationCondition.Caption: TNyxText;
begin

  if FActivation = npaManual then
  begin
    Exit('Manual selection');
  end;
  Result := FViewport.Caption;

  if FContainer.Defined then
  begin
    Result := TNyxText('Within ') + FContainer.Name + TNyxText(' / ') + Result;
  end;
end;

function TNyxPresentationCondition.Pascal: TNyxText;
begin

  if FActivation = npaManual then
  begin
    Exit('TNyxPresentationCondition.Manual');
  end;

  if FContainer.Defined then
  begin
    Exit('TNyxPresentationCondition.Within(' + FContainer.Pascal + ', ' +
      FViewport.PascalCondition + ')');
  end;
  { Retain the concise existing overload for automatic authored source. }
  Result := FViewport.PascalCondition;
end;

class function TNyxPresentationSelection.None: TNyxPresentationSelection;
begin
  Result := Default(TNyxPresentationSelection);
end;

class function TNyxPresentationSelection.Use(
  const AReference: TNyxPresentationRef): TNyxPresentationSelection;
begin
  AReference.GetName;
  Result := None;
  Result.FReference := AReference;
end;

procedure TNyxPresentationSelection.Validate(const APresentations: INyxPresentationSnapshot);
begin

  if not FReference.Defined then
  begin
    Exit;
  end;

  if (APresentations = nil) or not APresentations.Contains(FReference) then
  begin
    raise ENyxPresentation.Create('Selected presentation is not defined in this view');
  end;

  if APresentations.Definition(FReference).Activation <> npaManual then
  begin
    raise ENyxPresentation.Create('Only manual presentations can be selected explicitly');
  end;
end;

function TNyxPresentationSelection.Matches(const AReference: TNyxPresentationRef): Boolean;
begin
  Result := FReference.Defined and AReference.Defined;

  if Result then
  begin
    Result := FReference.Name = AReference.Name;
  end;
end;

function TNyxPresentationSelection.Reconciled(
  const APresentations: INyxPresentationSnapshot): TNyxPresentationSelection;
begin
  Result := None;

  if FReference.Defined and (APresentations <> nil) and APresentations.Contains(FReference) then
  begin

    if APresentations.Definition(FReference).Activation = npaManual then
    begin
      Result := Self;
    end;
  end;
end;

function TNyxPresentationSelection.Same(const AOther: TNyxPresentationSelection): Boolean;
begin
  Result := not FReference.Defined and not AOther.FReference.Defined;

  if FReference.Defined and AOther.FReference.Defined then
  begin
    Result := FReference.Name = AOther.FReference.Name;
  end;
end;

function TNyxPresentationRef.GetDefined: Boolean;
begin
  Result := FName <> '';
end;

function NyxPresentation(const AName: TNyxText): TNyxPresentationRef;
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
  LNonblank: Boolean;
begin
  LIndex := 1;
  LCount := 0;
  LNonblank := False;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) or (LScalar < 32) or
      ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      raise ENyxPresentation.Create('Presentation names require valid printable Unicode');
    end;
    Inc(LCount);
    LNonblank := LNonblank or not NyxScalarWhitespace(LScalar);
  end;

  if not LNonblank or (LCount > 128) then
  begin
    raise ENyxPresentation.Create('Presentation names require 1..128 meaningful Unicode scalars');
  end;
  Result := Default(TNyxPresentationRef);
  Result.FName := AName;
end;

constructor TPresentationSnapshot.Create(const AEntries: TPresentationEntries);
var
  LIndex: Integer;
begin
  inherited Create;
  SetLength(FEntries, Length(AEntries));
  for LIndex := 0 to High(AEntries) do
  begin
    FEntries[LIndex] := AEntries[LIndex];
  end;
end;

function TPresentationSnapshot.IndexOf(const AReference: TNyxPresentationRef): Integer;
var
  LIndex: Integer;
  LName: TNyxText;
begin
  LName := AReference.Name;
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].Reference.Name = LName then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

function TPresentationSnapshot.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TPresentationSnapshot.Reference(AIndex: Integer): TNyxPresentationRef;
begin

  if (AIndex < 0) or (AIndex >= Length(FEntries)) then
  begin
    raise ENyxPresentation.Create('Presentation index is outside the registry');
  end;
  Result := FEntries[AIndex].Reference;
end;

function TPresentationSnapshot.Contains(const AReference: TNyxPresentationRef): Boolean;
begin
  Result := IndexOf(AReference) >= 0;
end;

function TPresentationSnapshot.Condition(const AReference: TNyxPresentationRef): TNyxViewportCondition;
var
  LDefinition: TNyxPresentationCondition;
begin
  LDefinition := Definition(AReference);

  if (LDefinition.Activation <> npaAutomatic) or LDefinition.Container.Defined then
  begin
    raise ENyxPresentation.Create('Only host presentations have a viewport predicate');
  end;
  Result := LDefinition.Viewport;
end;

function TPresentationSnapshot.Definition(
  const AReference: TNyxPresentationRef): TNyxPresentationCondition;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AReference);

  if LIndex < 0 then
  begin
    raise ENyxPresentation.Create('Unknown presentation: ' + AReference.Name);
  end;
  Result := FEntries[LIndex].Condition;
end;

procedure TPresentations.Define(const AReference: TNyxPresentationRef;
  const ACondition: TNyxViewportCondition);
begin
  Define(AReference, TNyxPresentationCondition.Automatic(ACondition));
end;

procedure TPresentations.Define(const AReference: TNyxPresentationRef;
  const ACondition: TNyxPresentationCondition);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AReference);
  { Ordinary unscoped configuration owns the default presentation. Named
    automatic presentations need a predicate, rather than an always-on alias. }

  if (ACondition.Activation = npaAutomatic) and ACondition.Viewport.IsAny then
  begin
    raise ENyxPresentation.Create('A named presentation needs a viewport condition');
  end;

  if LIndex < 0 then
  begin

    if Length(FEntries) = NyxMaximumPresentations then
    begin
      raise ENyxPresentation.Create('Presentation definition budget exceeded');
    end;
    LIndex := Length(FEntries);
    SetLength(FEntries, LIndex + 1);
  end;
  FEntries[LIndex].Reference := AReference;
  FEntries[LIndex].Condition := ACondition;
end;

procedure TPresentations.Remove(const AReference: TNyxPresentationRef);
var
  LIndex: Integer;
  LCursor: Integer;
begin
  LIndex := IndexOf(AReference);

  if LIndex < 0 then
  begin
    raise ENyxPresentation.Create('Cannot remove an undefined presentation');
  end;
  for LCursor := LIndex to High(FEntries) - 1 do
  begin
    FEntries[LCursor] := FEntries[LCursor + 1];
  end;
  SetLength(FEntries, Length(FEntries) - 1);
end;

function TPresentations.Snapshot: INyxPresentationSnapshot;
begin
  Result := TPresentationSnapshot.Create(FEntries);
end;

function TPresentations.Clone: INyxPresentations;
begin
  Result := TPresentations.Create(FEntries);
end;

function NewNyxPresentations: INyxPresentations;
var
  LEmpty: TPresentationEntries;
begin
  LEmpty := nil;
  Result := TPresentations.Create(LEmpty);
end;

function TPresentationSnapshot.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LVersion: Integer;
  LCondition: TNyxViewportCondition;
begin
  LVersion := 1;
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].Condition.Container.Defined then
    begin
      LVersion := 3;
    end
    else if (LVersion < 2) and (FEntries[LIndex].Condition.Activation = npaManual) then
    begin
      LVersion := 2;
    end;
  end;
  SetLength(LItems, Length(FEntries));
  for LIndex := 0 to High(FEntries) do
  begin
    LCondition := FEntries[LIndex].Condition.Viewport;
    SetLength(LFields, 5 + LVersion);
    LFields[0] := NyxField('name', NyxData(FEntries[LIndex].Reference.Name));
    LFields[1] := NyxField('widthMinimum', NyxData(LCondition.WidthMinimum));
    LFields[2] := NyxField('widthMaximum', NyxData(LCondition.WidthMaximum));
    LFields[3] := NyxField('heightMinimum', NyxData(LCondition.HeightMinimum));
    LFields[4] := NyxField('heightMaximum', NyxData(LCondition.HeightMaximum));
    LFields[5] := NyxField('orientation', NyxData(NyxViewportOrientationName(LCondition.OrientationValue)));

    if LVersion >= 2 then
    begin

      if FEntries[LIndex].Condition.Activation = npaManual then
      begin
        LFields[6] := NyxField('activation', NyxData('manual'));
      end
      else
      begin
        LFields[6] := NyxField('activation', NyxData('automatic'));
      end;
    end;

    if LVersion = 3 then
    begin
      LFields[7] := NyxField('container', NyxData(''));

      if FEntries[LIndex].Condition.Container.Defined then
      begin
        LFields[7] := NyxField('container', NyxData(FEntries[LIndex].Condition.Container.Name));
      end;
    end;
    LItems[LIndex] := NyxObject(LFields);
  end;
  Result := NyxObject([NyxField('version', NyxData(LVersion)),
    NyxField('definitions', NyxArray(LItems))]);
end;

function NyxPresentationsFromData(const AData: TNyxDataValue): INyxPresentations;
const
  CFields: array[0..5] of TNyxText = ('name', 'widthMinimum', 'widthMaximum',
    'heightMinimum', 'heightMaximum', 'orientation');
var
  LDefinitions: TNyxDataValue;
  LEntry: TNyxDataValue;
  LIndex: Integer;
  LField: Integer;
  LKnown: Boolean;
  LReference: TNyxPresentationRef;
  LCondition: TNyxViewportCondition;
  LOrientation: TNyxViewportOrientation;
  LWidthMinimum: Integer;
  LWidthMaximum: Integer;
  LHeightMinimum: Integer;
  LHeightMaximum: Integer;
  LVersion: Integer;
  LActivation: TNyxText;
  LContainer: TNyxText;

  procedure Reject;
  begin
    raise ENyxPresentation.Create('Invalid presentation definitions');
  end;

begin

  if (AData.Kind <> ndObject) or (AData.Count <> 2) then
  begin
    Reject;
  end;
  LVersion := AData.Field('version').AsInteger;

  if not (LVersion in [1, 2, 3]) then
  begin
    Reject;
  end;
  LDefinitions := AData.Field('definitions');

  if (LDefinitions.Kind <> ndArray) or (LDefinitions.Count > NyxMaximumPresentations) then
  begin
    Reject;
  end;
  Result := NewNyxPresentations;
  for LIndex := 0 to LDefinitions.Count - 1 do
  begin
    LEntry := LDefinitions.Item(LIndex);

    if (LEntry.Kind <> ndObject) or (LEntry.Count <> Length(CFields) + LVersion - 1) then
    begin
      Reject;
    end;
    for LField := Low(CFields) to High(CFields) do
    begin
      LEntry.Field(CFields[LField]);
    end;
    LReference := NyxPresentation(LEntry.Field('name').AsText);

    if Result.Contains(LReference) then
    begin
      Reject;
    end;
    LWidthMinimum := LEntry.Field('widthMinimum').AsInteger;
    LWidthMaximum := LEntry.Field('widthMaximum').AsInteger;
    LHeightMinimum := LEntry.Field('heightMinimum').AsInteger;
    LHeightMaximum := LEntry.Field('heightMaximum').AsInteger;
    LCondition := TNyxViewportCondition.Any;

    if LWidthMaximum = 0 then
    begin
      LCondition := LCondition.WidthAtLeast(LWidthMinimum);
    end
    else
    begin
      LCondition := LCondition.WidthBetween(LWidthMinimum, LWidthMaximum);
    end;

    if LHeightMaximum = 0 then
    begin
      LCondition := LCondition.HeightAtLeast(LHeightMinimum);
    end
    else
    begin
      LCondition := LCondition.HeightBetween(LHeightMinimum, LHeightMaximum);
    end;
    LKnown := False;
    for LOrientation := Low(TNyxViewportOrientation) to High(TNyxViewportOrientation) do
    begin

      if NyxViewportOrientationName(LOrientation) = LEntry.Field('orientation').AsText then
      begin
        LCondition := LCondition.Orientation(LOrientation);
        LKnown := True;
        Break;
      end;
    end;

    if not LKnown then
    begin
      Reject;
    end;
    LActivation := 'automatic';

    if LVersion >= 2 then
    begin
      LActivation := LEntry.Field('activation').AsText;
    end;
    LContainer := '';

    if LVersion = 3 then
    begin
      LContainer := LEntry.Field('container').AsText;
    end;

    if LActivation = 'manual' then
    begin

      if not LCondition.IsAny or (LContainer <> '') then
      begin
        Reject;
      end;
      Result.Define(LReference, TNyxPresentationCondition.Manual);
    end
    else if LActivation = 'automatic' then
    begin

      if LContainer <> '' then
      begin
        Result.Define(LReference, TNyxPresentationCondition.Within(NyxContainer(LContainer), LCondition));
      end
      else
      begin
        Result.Define(LReference, LCondition);
      end;
    end
    else
    begin
      Reject;
    end;
  end;
end;

function EscapeName(const AName: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  { Copy storage units unchanged: native text is UTF-8 and browser text UTF-16.
    ASCII delimiters are unambiguous in both; ANSI RTL replacement would lose
    supplementary application names on native Windows. }
  Result := '';
  for LIndex := 1 to Length(AName) do
  begin
    case AName[LIndex] of
      '%': Result := Result + TNyxText('%25');
      ':': Result := Result + TNyxText('%3A');
      '=': Result := Result + TNyxText('%3D');
    else
      Result := Result + Copy(AName, LIndex, 1);
    end;
  end;
end;

function NyxPresentationDefinition(const AReference: TNyxPresentationRef;
  const ACondition: TNyxViewportCondition): TNyxDataValue;
begin
  Result := NyxPresentationDefinition(AReference, TNyxPresentationCondition.Automatic(ACondition));
end;

function NyxPresentationDefinition(const AReference: TNyxPresentationRef;
  const ACondition: TNyxPresentationCondition): TNyxDataValue;
var
  LDefinitions: INyxPresentations;
begin
  LDefinitions := NewNyxPresentations;
  LDefinitions.Define(AReference, ACondition);
  Result := LDefinitions.ToData.Field('definitions').Item(0);
end;

procedure ReadNyxPresentationDefinition(const AData: TNyxDataValue;
  out AReference: TNyxPresentationRef; out ACondition: TNyxViewportCondition);
var
  LDefinition: TNyxPresentationCondition;
begin
  ReadNyxPresentationDefinition(AData, AReference, LDefinition);

  if (LDefinition.Activation <> npaAutomatic) or LDefinition.Container.Defined then
  begin
    raise ENyxPresentation.Create('Only host presentations have a viewport predicate');
  end;
  ACondition := LDefinition.Viewport;
end;

procedure ReadNyxPresentationDefinition(const AData: TNyxDataValue;
  out AReference: TNyxPresentationRef; out ACondition: TNyxPresentationCondition);
var
  LDefinitions: INyxPresentations;
  LVersion: Integer;
begin
  AReference := Default(TNyxPresentationRef);
  ACondition := TNyxPresentationCondition.Automatic(TNyxViewportCondition.Any);
  LVersion := 1;

  if AData.Count = 7 then
  begin
    LVersion := 2;
  end;

  if AData.Count = 8 then
  begin
    LVersion := 3;
  end;
  LDefinitions := NyxPresentationsFromData(NyxObject([
    NyxField('version', NyxData(LVersion)), NyxField('definitions', NyxArray([AData]))]));
  AReference := LDefinitions.Reference(0);
  ACondition := LDefinitions.Definition(AReference);
end;

function UnescapeName(const AName: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  LIndex := 1;
  while LIndex <= Length(AName) do
  begin

    if Copy(AName, LIndex, 3) = '%25' then
    begin
      Result := Result + TNyxText('%');
      Inc(LIndex, 3);
    end
    else if Copy(AName, LIndex, 3) = '%3A' then
    begin
      Result := Result + TNyxText(':');
      Inc(LIndex, 3);
    end
    else if Copy(AName, LIndex, 3) = '%3D' then
    begin
      Result := Result + TNyxText('=');
      Inc(LIndex, 3);
    end
    else
    begin
      Result := Result + Copy(AName, LIndex, 1);
      Inc(LIndex);
    end;
  end;
end;

function NyxPresentationKey(const AReference: TNyxPresentationRef;
  APlatform: TNyxPlatform; AAttribute: TNyxAttribute): TNyxText;
begin

  if not NyxPlatformAttribute(AAttribute) then
  begin
    raise ENyxPresentation.Create('Only presentation attributes can vary by presentation');
  end;
  Result := TNyxText('@nyx.presentation:') + EscapeName(AReference.Name) + TNyxText(':') +
    NyxPlatformName(APlatform) + TNyxText(':') + NyxAttributeName(AAttribute);
end;

function TryNyxPresentationKey(const AKey: TNyxText;
  out AReference: TNyxPresentationRef; out APlatform: TNyxPlatform;
  out AAttribute: TNyxAttribute): Boolean;
const
  CPrefix = '@nyx.presentation:';
var
  LStart: Integer;
  LEnd: Integer;
  LName: TNyxText;
  LPlatformName: TNyxText;
  LPlatform: TNyxPlatform;
  LFound: Boolean;
begin
  Result := False;
  AReference := Default(TNyxPresentationRef);
  APlatform := npfAny;
  AAttribute := atText;

  if Copy(AKey, 1, Length(CPrefix)) <> CPrefix then
  begin
    Exit;
  end;
  LStart := Length(CPrefix) + 1;
  LEnd := LStart;
  while (LEnd <= Length(AKey)) and (AKey[LEnd] <> ':') do
  begin
    Inc(LEnd);
  end;
  LName := Copy(AKey, LStart, LEnd - LStart);
  LStart := LEnd + 1;
  LEnd := LStart;
  while (LEnd <= Length(AKey)) and (AKey[LEnd] <> ':') do
  begin
    Inc(LEnd);
  end;
  LPlatformName := Copy(AKey, LStart, LEnd - LStart);
  LFound := False;
  for LPlatform := npfAny to npfNativeLCL do
  begin

    if LPlatformName = NyxPlatformName(LPlatform) then
    begin
      APlatform := LPlatform;
      LFound := True;
      Break;
    end;
  end;

  if not LFound or not TryNyxAttribute(Copy(AKey, LEnd + 1, MaxInt), AAttribute) or
    not NyxPlatformAttribute(AAttribute) then
  begin
    Exit;
  end;
  LName := UnescapeName(LName);
  try
    AReference := NyxPresentation(LName);
    Result := AKey = NyxPresentationKey(AReference, APlatform, AAttribute);
  except
    on ENyxPresentation do
    begin
      AReference := Default(TNyxPresentationRef);
    end;
  end;
end;

function TryNyxResponsiveKey(const AKey: TNyxText;
  const APresentations: INyxPresentationSnapshot; out ACondition: TNyxViewportCondition;
  out APlatform: TNyxPlatform; out AAttribute: TNyxAttribute): Boolean;
var
  LCondition: TNyxPresentationCondition;
begin
  Result := TryNyxPresentationRule(AKey, APresentations, LCondition, APlatform, AAttribute);
  ACondition := LCondition.Viewport;

  if Result and ((LCondition.Activation = npaManual) or LCondition.Container.Defined) then
  begin
    raise ENyxPresentation.Create('Use typed presentation-rule inspection for non-host definitions');
  end;
end;

function TryNyxPresentationRule(const AKey: TNyxText;
  const APresentations: INyxPresentationSnapshot; out ACondition: TNyxPresentationCondition;
  out APlatform: TNyxPlatform; out AAttribute: TNyxAttribute): Boolean;
var
  LReference: TNyxPresentationRef;
  LViewport: TNyxViewportCondition;
begin
  Result := TryNyxViewportKey(AKey, LViewport, APlatform, AAttribute);
  ACondition := TNyxPresentationCondition.Automatic(LViewport);

  if Result then
  begin
    Exit;
  end;
  Result := TryNyxPresentationKey(AKey, LReference, APlatform, AAttribute);

  if Result then
  begin

    if APresentations = nil then
    begin
      raise ENyxPresentation.Create('Named presentation rules require document definitions');
    end;
    ACondition := APresentations.Definition(LReference);
  end;
end;

end.
