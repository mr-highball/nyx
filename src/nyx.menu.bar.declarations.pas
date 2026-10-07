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

unit nyx.menu.bar.declarations;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.menu.types, nyx.menu.declarations, nyx.data;

const
  NyxMenuBarWireField = 'menuBar';
  NyxMaximumMenuBarHeadings = 64;

type
  { Immutable authored heading. Menu is an exact document reference; Enabled is
    a logical runtime default, independent of physical control enablement. }
  INyxMenuBarHeading = interface(IInterface)
    ['{71000707-BA22-4531-9030-000000000001}']
    function GetPart: TNyxPartRef;
    function GetMenu: TNyxMenuRef;
    function GetEnabled: Boolean;
    property Part: TNyxPartRef read GetPart;
    property Menu: TNyxMenuRef read GetMenu;
    property IsEnabled: Boolean read GetEnabled;
  end;

  { Portable immutable grouping attached to a specialized row. Every Heading
    returns a fresh independent builder; earlier plans retain their ordering.
    Empty intermediate plans are valid, but attachment requires 1..64 distinct
    named headings. Two headings may reuse a menu definition: mounted families
    own independent runtime state. This declaration owns no tree or renderer. }
  INyxMenuBarDefinition = interface(IInterface)
    ['{71000707-BA22-4531-9030-000000000002}']
    function GetOptions: TNyxMenuBarOptions;
    function GetCount: Integer;
    function Item(AIndex: Integer): INyxMenuBarHeading;
    function Heading(const APart: TNyxPartRef; const AMenu: TNyxMenuRef;
      AEnabled: Boolean = True): INyxMenuBarDefinition;
    function ToData: TNyxDataValue;
    property Options: TNyxMenuBarOptions read GetOptions;
    property Count: Integer read GetCount;
  end;

function NewNyxMenuBarDefinition(const AOptions: TNyxMenuBarOptions):
  INyxMenuBarDefinition;
{ Admission copies even foreign implementations through public scalar getters.
  Nil, empty, oversized, duplicate or invalid plans refuse without publishing.
  Returned immutable plans and items can outlive their source node/document. }
function CopyNyxMenuBarDefinition(const ADefinition: INyxMenuBarDefinition):
  INyxMenuBarDefinition;
{ Strict versioned persistence/semantic boundary, using native scalar kinds and
  closed search choices. Unknown/missing fields and string coercion refuse. }
function NyxMenuBarDefinitionFromData(const AData: TNyxDataValue):
  INyxMenuBarDefinition;

implementation

uses nyx.errors, nyx.typeahead;

type
  TBarHeading = class(TInterfacedObject, INyxMenuBarHeading)
  private
    FPart: TNyxPartRef;
    FMenu: TNyxMenuRef;
    FEnabled: Boolean;
  public
    function GetPart: TNyxPartRef;
    function GetMenu: TNyxMenuRef;
    function GetEnabled: Boolean;
  end;

  TBarDefinition = class(TInterfacedObject, INyxMenuBarDefinition)
  private
    FOptions: TNyxMenuBarOptions;
    FHeadings: array of INyxMenuBarHeading;
  public
    constructor Create(const AOptions: TNyxMenuBarOptions);
    function GetOptions: TNyxMenuBarOptions;
    function GetCount: Integer;
    function Item(AIndex: Integer): INyxMenuBarHeading;
    function Heading(const APart: TNyxPartRef; const AMenu: TNyxMenuRef;
      AEnabled: Boolean = True): INyxMenuBarDefinition;
    function ToData: TNyxDataValue;
  end;

function TBarHeading.GetPart: TNyxPartRef;
begin
  Result := Default(TNyxPartRef);
  Result.Name := FPart.Name;
end;

function TBarHeading.GetMenu: TNyxMenuRef;
begin
  Result := Default(TNyxMenuRef);
  Result.Name := FMenu.Name;
end;

function TBarHeading.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

constructor TBarDefinition.Create(const AOptions: TNyxMenuBarOptions);
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions;
end;

function TBarDefinition.GetOptions: TNyxMenuBarOptions;
begin
  Result := FOptions;
end;

function TBarDefinition.GetCount: Integer;
begin
  Result := Length(FHeadings);
end;

function TBarDefinition.Item(AIndex: Integer): INyxMenuBarHeading;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ENyxModel.Create('Menu bar heading index is outside its declaration');
  end;
  Result := FHeadings[AIndex];
end;

function TBarDefinition.Heading(const APart: TNyxPartRef;
  const AMenu: TNyxMenuRef; AEnabled: Boolean): INyxMenuBarDefinition;
var
  LEntry: TBarHeading;
  LEntryOwner: INyxMenuBarHeading;
  LCopy: TBarDefinition;
  LIndex: Integer;
  LCount: Integer;
begin
  LCount := GetCount;
  LEntry := TBarHeading.Create;
  LEntryOwner := LEntry;
  LEntry.FPart := NyxPart(APart.Name);
  LEntry.FMenu := NyxMenuRef(AMenu.Name);
  LEntry.FEnabled := AEnabled;

  if (LEntry.FPart.Name = '') or (LCount >= NyxMaximumMenuBarHeadings) then
  begin
    raise ENyxModel.Create('Menu bar requires bounded nonempty named headings');
  end;
  for LIndex := 0 to LCount - 1 do
  begin

    if FHeadings[LIndex].Part.Name = LEntry.FPart.Name then
    begin
      raise ENyxModel.Create('Menu bar heading part is already declared');
    end;
  end;
  LCopy := TBarDefinition.Create(FOptions);
  Result := LCopy;
  SetLength(LCopy.FHeadings, LCount + 1);
  for LIndex := 0 to LCount - 1 do
  begin
    LCopy.FHeadings[LIndex] := FHeadings[LIndex];
  end;
  { Cache the index explicitly: pas2js interface-array setters must receive a
    scalar ordinal rather than a parameterless method reference. }
  LCopy.FHeadings[LCount] := LEntryOwner;
end;

function NewNyxMenuBarDefinition(const AOptions: TNyxMenuBarOptions):
  INyxMenuBarDefinition;
begin
  Result := TBarDefinition.Create(AOptions);
end;

function CopyNyxMenuBarDefinition(const ADefinition: INyxMenuBarDefinition):
  INyxMenuBarDefinition;
var
  LIndex: Integer;
  LCount: Integer;
  LHeading: INyxMenuBarHeading;
begin

  if ADefinition = nil then
  begin
    raise ENyxModel.Create('Menu bar definition is required');
  end;
  LCount := ADefinition.Count;

  if (LCount < 1) or (LCount > NyxMaximumMenuBarHeadings) then
  begin
    raise ENyxModel.Create('Attached menu bar requires 1..64 headings');
  end;
  Result := NewNyxMenuBarDefinition(ADefinition.Options);
  for LIndex := 0 to LCount - 1 do
  begin
    LHeading := ADefinition.Item(LIndex);

    if LHeading = nil then
    begin
      raise ENyxModel.Create('Menu bar returned no authored heading');
    end;
    Result := Result.Heading(LHeading.Part, LHeading.Menu, LHeading.IsEnabled);
  end;
end;

function TBarDefinition.ToData: TNyxDataValue;
const
  CMatch: array[TNyxTypeAheadMatch] of TNyxText = ('folded', 'exact');
var
  LIndex: Integer;
  LItems: array of TNyxDataValue;
begin
  SetLength(LItems, GetCount);
  for LIndex := 0 to GetCount - 1 do
  begin
    LItems[LIndex] := NyxObject([
      NyxField('part', NyxData(FHeadings[LIndex].Part.Name)),
      NyxField('menu', NyxData(FHeadings[LIndex].Menu.Name)),
      NyxField('enabled', NyxData(FHeadings[LIndex].IsEnabled))]);
  end;
  Result := NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('options', NyxObject([
      NyxField('label', NyxData(FOptions.Caption)),
      NyxField('wrap', NyxData(FOptions.Wraps)),
      NyxField('hoverSwitch', NyxData(FOptions.Hovers)),
      NyxField('searchEnabled', NyxData(FOptions.Search.IsEnabled)),
      NyxField('searchWindowMS', NyxData(FOptions.Search.WindowMS)),
      NyxField('searchMatch', NyxData(CMatch[FOptions.Search.MatchMode]))])),
    NyxField('headings', NyxArray(LItems))]);
end;

function NyxMenuBarDefinitionFromData(const AData: TNyxDataValue):
  INyxMenuBarDefinition;
var
  LOptions: TNyxDataValue;
  LItems: TNyxDataValue;
  LHeading: TNyxDataValue;
  LMatch: TNyxTypeAheadMatch;
  LChoice: TNyxText;
  LIndex: Integer;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 3) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Unsupported menu bar declaration');
  end;
  LOptions := AData.Field('options');
  LItems := AData.Field('headings');

  if (LOptions.Kind <> ndObject) or (LOptions.Count <> 6) or
    (LItems.Kind <> ndArray) or (LItems.Count < 1) or
    (LItems.Count > NyxMaximumMenuBarHeadings) then
  begin
    raise ENyxModel.Create('Menu bar requires exact policy fields and bounded headings');
  end;
  LChoice := LOptions.Field('searchMatch').AsText;

  if LChoice = 'folded' then
  begin
    LMatch := ntmFolded;
  end
  else if LChoice = 'exact' then
  begin
    LMatch := ntmExact;
  end
  else
  begin
    raise ENyxModel.Create('Unknown menu bar search choice');
  end;
  Result := NewNyxMenuBarDefinition(NyxMenuBar(LOptions.Field('label').AsText)
    .Wrap(LOptions.Field('wrap').AsBoolean)
    .HoverSwitch(LOptions.Field('hoverSwitch').AsBoolean)
    .TypeAhead(NyxTypeAhead.Enabled(LOptions.Field('searchEnabled').AsBoolean)
      .WindowMilliseconds(LOptions.Field('searchWindowMS').AsInteger).Match(LMatch)));
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LHeading := LItems.Item(LIndex);

    if (LHeading.Kind <> ndObject) or (LHeading.Count <> 3) then
    begin
      raise ENyxModel.Create('Menu bar heading requires exactly its typed fields');
    end;
    Result := Result.Heading(NyxPart(LHeading.Field('part').AsText),
      NyxMenuRef(LHeading.Field('menu').AsText), LHeading.Field('enabled').AsBoolean);
  end;
end;

end.
