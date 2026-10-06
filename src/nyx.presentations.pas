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
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.responsive;

const
  NyxMaximumPresentations = 64;
  NyxPresentationsWireField = 'presentations';
  { Version-four nodes carry long named scopes as array values. Older native
    fpjson object-key hashing truncates names at 255 bytes; application names
    must never be routed through that implementation limit. }
  NyxPresentationRulesWireField = 'presentationRules';

type
  ENyxPresentation = class(Exception);

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

  { Immutable independent registry snapshot. It retains copied values only,
    so rendered/reusable views can outlive a document without a backreference.
    Index and unknown-name reads refuse; definition order remains stable. }
  INyxPresentationSnapshot = interface(IInterface)
    ['{6A55BCF5-6D19-41D2-818A-95D707CE9370}']
    function GetCount: Integer;
    function Reference(AIndex: Integer): TNyxPresentationRef;
    function Contains(const AReference: TNyxPresentationRef): Boolean;
    function Condition(const AReference: TNyxPresentationRef): TNyxViewportCondition;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

  { Document-owned managed authoring registry. Define validates before mutation,
    replacing a condition at its original position. Remove does not rewrite
    controls; document admission must reject remaining references. Snapshots and
    clones own independent arrays, including on pas2js. No subscribers/tree/UI
    objects are retained. Runtime stores never mutate these authored defaults. }
  INyxPresentations = interface(INyxPresentationSnapshot)
    ['{64A0A770-131F-4703-8B9A-C510961310EC}']
    procedure Define(const AReference: TNyxPresentationRef;
      const ACondition: TNyxViewportCondition);
    procedure Remove(const AReference: TNyxPresentationRef);
    function Snapshot: INyxPresentationSnapshot;
    function Clone: INyxPresentations;
  end;

{ Names admit 1..128 Unicode scalars, refusing malformed/control/blank text.
  Names are case-sensitive, retain exact encoding and need not be identifiers. }
function NyxPresentation(const AName: TNyxText): TNyxPresentationRef;
function NewNyxPresentations: INyxPresentations;
{ Strict version-1 structured boundary. Unknown/missing fields, duplicate names,
  malformed choices and budget excess refuse before returning a candidate. }
function NyxPresentationsFromData(const AData: TNyxDataValue): INyxPresentations;
{ One strict definition at the structured editor/MCP boundary; it uses the
  same admission as a complete registry rather than a second condition grammar. }
function NyxPresentationDefinition(const AReference: TNyxPresentationRef;
  const ACondition: TNyxViewportCondition): TNyxDataValue;
procedure ReadNyxPresentationDefinition(const AData: TNyxDataValue;
  out AReference: TNyxPresentationRef; out ACondition: TNyxViewportCondition);
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

implementation

type
  TPresentationEntry = record
    Reference: TNyxPresentationRef;
    Condition: TNyxViewportCondition;
  end;
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
    function ToData: TNyxDataValue;
  end;

  TPresentations = class(TPresentationSnapshot, INyxPresentations)
  public
    procedure Define(const AReference: TNyxPresentationRef;
      const ACondition: TNyxViewportCondition);
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
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AReference);
  { Ordinary unscoped configuration owns the default presentation. Named
    automatic presentations need a predicate, rather than an always-on alias. }

  if ACondition.IsAny then
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
  LIndex: Integer;
  LCondition: TNyxViewportCondition;
begin
  SetLength(LItems, Length(FEntries));
  for LIndex := 0 to High(FEntries) do
  begin
    LCondition := FEntries[LIndex].Condition;
    LItems[LIndex] := NyxObject([
      NyxField('name', NyxData(FEntries[LIndex].Reference.Name)),
      NyxField('widthMinimum', NyxData(LCondition.WidthMinimum)),
      NyxField('widthMaximum', NyxData(LCondition.WidthMaximum)),
      NyxField('heightMinimum', NyxData(LCondition.HeightMinimum)),
      NyxField('heightMaximum', NyxData(LCondition.HeightMaximum)),
      NyxField('orientation', NyxData(NyxViewportOrientationName(LCondition.OrientationValue)))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
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

  procedure Reject;
  begin
    raise ENyxPresentation.Create('Invalid presentation definitions');
  end;

begin

  if (AData.Kind <> ndObject) or (AData.Count <> 2) or
    (AData.Field('version').AsInteger <> 1) then
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

    if (LEntry.Kind <> ndObject) or (LEntry.Count <> Length(CFields)) then
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
    Result.Define(LReference, LCondition);
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
  LDefinitions: INyxPresentations;
begin
  AReference := Default(TNyxPresentationRef);
  ACondition := TNyxViewportCondition.Any;
  LDefinitions := NyxPresentationsFromData(NyxObject([
    NyxField('version', NyxData(1)), NyxField('definitions', NyxArray([AData]))]));
  AReference := LDefinitions.Reference(0);
  ACondition := LDefinitions.Condition(AReference);
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
  LReference: TNyxPresentationRef;
begin
  Result := TryNyxViewportKey(AKey, ACondition, APlatform, AAttribute);

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
    ACondition := APresentations.Condition(LReference);
  end;
end;

end.
