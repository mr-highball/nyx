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

unit nyx.studio.inspector.groups;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.model, nyx.controls;

type
  { Closed editor roles, separate from component kinds and exported UI state. }
  TNyxInspectorGroup = (nigComponent, nigData, nigResponsive, nigProperties, nigTabPages);
  { Immutable copied presentation choices. Updates allocate/copy the fixed array
    explicitly on both compilers. No session, document or widget is retained. }
  TNyxInspectorGroups = record
  private
    FExpanded: array[TNyxInspectorGroup] of Boolean;
  public
    { Preserve existing discoverability until the user chooses to collapse. }
    class function Defaults: TNyxInspectorGroups; static;
    function IsExpanded(AGroup: TNyxInspectorGroup): Boolean;
    function Expanded(AGroup: TNyxInspectorGroup; AExpanded: Boolean): TNyxInspectorGroups;
    { Capture a mounted disclosure from its own node or containing section root.
      An absent role retains its choice;
      a present wrong identity/contract refuses before changing this value. }
    function Captured(AGroup: TNyxInspectorGroup; ARoot: TNyxNode): TNyxInspectorGroups;
    { Exact version-one Boolean persistence; unknown/missing/wrong fields refuse. }
    function ToData: TNyxDataValue;
    class function FromData(const AValue: TNyxDataValue): TNyxInspectorGroups; static;
  end;

{ Stable private editor identities and English chrome captions, selected through
  a typed role. Application names/text are never inferred from these values. }
function NyxInspectorGroupID(AGroup: TNyxInspectorGroup): TNyxText;
{ Decode an exact internal chrome identity without guessing application names. }
function TryNyxInspectorGroup(const AID: TNyxText; out AGroup: TNyxInspectorGroup): Boolean;
function NewNyxInspectorGroup(AGroup: TNyxInspectorGroup;
  const AState: TNyxInspectorGroups): INyxDisclosure;
{ Adopt a nonempty body into the owned shell; empty optional groups are omitted.
  The supplied interface retains the independent group through this call. }
procedure AppendNyxInspectorGroup(AParent: TNyxNode; const AGroup: INyxDisclosure);

implementation

uses
  nyx.disclosure;

const
  CKeys: array[TNyxInspectorGroup] of TNyxText =
    ('component', 'data', 'responsive', 'properties', 'tabPages');
  CIDs: array[TNyxInspectorGroup] of TNyxText =
    ('inspector-group-component', 'inspector-group-data', 'inspector-group-responsive',
      'inspector-group-properties', 'inspector-tab-pages-section');
  CTitles: array[TNyxInspectorGroup] of TNyxText =
    ('Component options', 'Data and bindings', 'Responsive layout', 'Properties', 'Tab pages');

class function TNyxInspectorGroups.Defaults: TNyxInspectorGroups;
var
  LGroup: TNyxInspectorGroup;
begin
  Result := Default(TNyxInspectorGroups);
  for LGroup := Low(TNyxInspectorGroup) to High(TNyxInspectorGroup) do
  begin
    Result.FExpanded[LGroup] := True;
  end;
end;

function TNyxInspectorGroups.IsExpanded(AGroup: TNyxInspectorGroup): Boolean;
begin
  Result := FExpanded[AGroup];
end;

function TNyxInspectorGroups.Expanded(AGroup: TNyxInspectorGroup;
  AExpanded: Boolean): TNyxInspectorGroups;
var
  LGroup: TNyxInspectorGroup;
begin
  Result := Defaults;
  for LGroup := Low(TNyxInspectorGroup) to High(TNyxInspectorGroup) do
  begin
    Result.FExpanded[LGroup] := FExpanded[LGroup];
  end;
  Result.FExpanded[AGroup] := AExpanded;
end;

function TNyxInspectorGroups.Captured(AGroup: TNyxInspectorGroup;
  ARoot: TNyxNode): TNyxInspectorGroups;
var
  LDisclosure: TNyxNode;
begin
  Result := Self;

  if ARoot = nil then
  begin
    Exit;
  end;

  LDisclosure := ARoot.Find(NyxInspectorGroupID(AGroup));

  if LDisclosure = nil then
  begin
    Exit;
  end;

  if not NyxIsDisclosure(LDisclosure) then
  begin
    raise ENyxModel.Create('Inspector presentation requires its exact disclosure role');
  end;
  Result := Expanded(AGroup, NyxDisclosureExpanded(LDisclosure));
end;

function TNyxInspectorGroups.ToData: TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LGroup: TNyxInspectorGroup;
begin
  SetLength(LFields, Ord(High(TNyxInspectorGroup)) + 2);
  LFields[0] := NyxField('version', NyxData(1));
  for LGroup := Low(TNyxInspectorGroup) to High(TNyxInspectorGroup) do
  begin
    LFields[Ord(LGroup) + 1] := NyxField(CKeys[LGroup], NyxData(FExpanded[LGroup]));
  end;
  Result := NyxObject(LFields);
end;

class function TNyxInspectorGroups.FromData(const AValue: TNyxDataValue): TNyxInspectorGroups;
var
  LGroup: TNyxInspectorGroup;
begin

  if (AValue.Kind <> ndObject) or (AValue.Count <> Ord(High(TNyxInspectorGroup)) + 2) or
    (AValue.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Unsupported inspector presentation choices');
  end;
  Result := Defaults;
  for LGroup := Low(TNyxInspectorGroup) to High(TNyxInspectorGroup) do
  begin
    Result.FExpanded[LGroup] := AValue.Field(CKeys[LGroup]).AsBoolean;
  end;
end;

function NyxInspectorGroupID(AGroup: TNyxInspectorGroup): TNyxText;
begin
  Result := CIDs[AGroup];
end;

function TryNyxInspectorGroup(const AID: TNyxText; out AGroup: TNyxInspectorGroup): Boolean;
var
  LGroup: TNyxInspectorGroup;
begin
  AGroup := Low(TNyxInspectorGroup);
  for LGroup := Low(TNyxInspectorGroup) to High(TNyxInspectorGroup) do
  begin

    if AID = CIDs[LGroup] then
    begin
      AGroup := LGroup;
      Exit(True);
    end;
  end;
  Result := False;
end;

function NewNyxInspectorGroup(AGroup: TNyxInspectorGroup;
  const AState: TNyxInspectorGroups): INyxDisclosure;
begin
  Result := NewNyxDisclosure(NyxInspectorGroupID(AGroup));
  Result.WithText(CTitles[AGroup]);
  Result.Configure.Value(AState.IsExpanded(AGroup)).Surface(False).Padding(0).Gap(10).Done;
end;

procedure AppendNyxInspectorGroup(AParent: TNyxNode; const AGroup: INyxDisclosure);
begin

  if (AParent = nil) or (AGroup = nil) then
  begin
    raise ENyxModel.Create('Inspector grouping requires its owning shell and public disclosure');
  end;

  if AGroup.BodyColumn.Node.Count > 0 then
  begin
    AParent.Add(AGroup);
  end;
end;

end.
