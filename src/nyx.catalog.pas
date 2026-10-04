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

unit nyx.catalog;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.types,
  nyx.catalog.labels,
  SysUtils,
  nyx.model,
  nyx.schema;

type
  { Palette metadata is portable. Category is a legacy authoring hint, Container
    describes child admission, and Kind remains an extensible string key rather
    than a closed enum that forces every extension to patch Nyx itself. }
  TNyxComponentInfo = record
    Kind: TNyxText;
    Title: TNyxText;
    Category: TNyxText;
    Discovery: TNyxComponentDiscovery;
    Container: Boolean;
  end;

  { Registry and initial design defaults. RegisterKind rejects accidental
    duplicate registration. A registry entry describes an authorable kind;
    adapters must independently advertise/implement its target behavior. }
  TNyxCatalog = class
  private
    FItems: array of TNyxComponentInfo;
    FRecipes: array of TNyxNode;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TNyxComponentInfo;
  public
    constructor Create;
    destructor Destroy; override;
    { Text overloads admit wire/legacy metadata. New extension authoring uses a
      distinct kind reference; captions/categories are ordinary user data. }
    procedure RegisterKind(const AKind, ATitle, ACategory: TNyxText;
      AContainer: Boolean); overload;
    procedure RegisterKind(const AKind: TNyxKindRef; const ATitle, ACategory: TNyxText;
      AContainer: Boolean); overload;
    { RegisterRecipe copies the borrowed template into the registry. Its owned
      named parts become ordinary editable descendants when NewNode instantiates
      it. Derive by customizing NewNode(baseKind, id), then registering that tree
      as a new recipe; the original recipe and instances remain unchanged. }
    procedure RegisterRecipe(const AKind, ATitle, ACategory: TNyxText;
      ATemplate: TNyxNode); overload;
    procedure RegisterRecipe(const AKind: TNyxKindRef; const ATitle, ACategory: TNyxText;
      ATemplate: TNyxNode); overload;
    function IndexOf(const AKind: TNyxText): Integer;
    { Assign one typed intent home and a small set of cross-cutting search labels
      to an existing entry. Description explains the creator's intent for help
      and search; aliases supply additional search terms. Unknown kinds
      or pgAll raise before changing metadata. Returns Self for fluent setup. }
    function Describe(const AKind: TNyxKindRef; AGroup: TNyxPaletteGroup;
      ALabels: TNyxComponentLabels; const AAliases: TNyxText = '';
      const ADescription: TNyxText = ''): TNyxCatalog;
    { Query terms are whitespace-separated AND matches over kind, title, intent
      group, labels, aliases and creator descriptions. Empty queries match all
      entries in AGroup.
      This read-only operation never instantiates or mutates a component. }
    function Matches(AIndex: Integer; const AQuery: TNyxText;
      AGroup: TNyxPaletteGroup = pgAll): Boolean;
    function NewNode(const AKind, AID: TNyxText): TNyxNode; overload;
    { Built-in controls use enums, custom names use distinct kind references.
      The text overload remains the explicit metadata/legacy boundary. }
    function NewNode(AKind: TNyxKind; const AID: TNyxText): TNyxNode; overload;
    function NewNode(const AKind: TNyxKindRef; const AID: TNyxText): TNyxNode; overload;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TNyxComponentInfo read GetItem; default;
  end;

implementation

uses
  nyx.recipes;

constructor TNyxCatalog.Create;
var
  LIndex: Integer;
  LInfo: TNyxPrimitiveInfo;
begin
  inherited Create;
  { Palette, inspector and target admission share one primitive definition.
    Reusable instances reference a definition; they do not own ignored children. }
  for LIndex := 0 to NyxPrimitiveCount - 1 do
  begin
    LInfo := NyxPrimitiveInfo(LIndex);
    RegisterKind(LInfo.Kind, LInfo.Title, LInfo.Category, LInfo.Container);
  end;
  RegisterNyxRecipes(Self);
end;

destructor TNyxCatalog.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FRecipes) - 1 do
  begin
    FRecipes[LIndex].Free;
  end;
  inherited Destroy;
end;

procedure TNyxCatalog.RegisterRecipe(const AKind: TNyxKindRef;
  const ATitle, ACategory: TNyxText; ATemplate: TNyxNode);
begin
  RegisterRecipe(AKind.Name, ATitle, ACategory, ATemplate);
end;

procedure TNyxCatalog.RegisterRecipe(const AKind, ATitle, ACategory: TNyxText;
  ATemplate: TNyxNode);
var
  LCopy: TNyxNode;
  LInfo: TNyxPrimitiveInfo;
  LContainer: Boolean;
begin

  if ATemplate = nil then
  begin
    raise ENyxModel.Create('Compound recipe template is required');
  end;
  { Bound/admit the borrowed tree before cloning, so a hostile deep template
    cannot overflow Clone's recursion before the ordinary tree budget is checked. }
  ValidateNyxPropertyTree(ATemplate);
  LCopy := ATemplate.Clone;
  try
    LContainer := LCopy.Count > 0;

    if FindNyxPrimitive(LCopy.ProjectionKind, LInfo) then
    begin
      LContainer := LInfo.Container;

      if not LContainer and (LCopy.Count > 0) and
        (LCopy.ProjectionKind <> 'component') then
      begin
        raise ENyxModel.Create('Compose leaf controls inside a layout recipe: ' + LCopy.Kind);
      end;
    end;
    RegisterKind(AKind, ATitle, ACategory, LContainer);
    FRecipes[Count - 1] := LCopy;
  except
    LCopy.Free;
    raise;
  end;
end;

function TNyxCatalog.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TNyxCatalog.GetItem(AIndex: Integer): TNyxComponentInfo;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ERangeError.Create('Component index out of range');
  end;
  Result := FItems[AIndex];
end;

function TNyxCatalog.IndexOf(const AKind: TNyxText): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to Count - 1 do
  begin

    if FItems[LIndex].Kind = AKind then
    begin
      Exit(LIndex);
    end;
  end;
end;

procedure TNyxCatalog.RegisterKind(const AKind: TNyxKindRef;
  const ATitle, ACategory: TNyxText; AContainer: Boolean);
begin
  RegisterKind(AKind.Name, ATitle, ACategory, AContainer);
end;

procedure TNyxCatalog.RegisterKind(const AKind, ATitle, ACategory: TNyxText;
  AContainer: Boolean);
var
  LIndex: Integer;
  LKind: TNyxKind;
  LGroup: TNyxPaletteGroup;
begin

  if AKind = '' then
  begin
    raise ENyxModel.Create('Component kind is required');
  end;

  if IndexOf(AKind) >= 0 then
  begin
    raise ENyxModel.Create('Component kind already registered: ' + AKind);
  end;
  LIndex := Count;
  SetLength(FItems, LIndex + 1);
  SetLength(FRecipes, LIndex + 1);
  FItems[LIndex].Kind := AKind;
  FItems[LIndex].Title := ATitle;
  FItems[LIndex].Category := ACategory;
  FItems[LIndex].Container := AContainer;
  FItems[LIndex].Discovery.Group := pgOther;
  FItems[LIndex].Discovery.Labels := [];
  FItems[LIndex].Discovery.Aliases := '';
  FItems[LIndex].Discovery.Description := '';

  if TryNyxKind(AKind, LKind) then
  begin
    FItems[LIndex].Discovery := NyxDefaultComponentDiscovery(LKind);
  end
  else if TryNyxPaletteGroup(ACategory, LGroup) and (LGroup <> pgAll) then
  begin
    { Admit existing extension category captions at the legacy boundary. New
      extension code calls Describe with enums instead of behavioral strings. }
    FItems[LIndex].Discovery.Group := LGroup;
  end;
end;

function TNyxCatalog.Describe(const AKind: TNyxKindRef; AGroup: TNyxPaletteGroup;
  ALabels: TNyxComponentLabels; const AAliases, ADescription: TNyxText): TNyxCatalog;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AKind.Name);

  if LIndex < 0 then
  begin
    raise ENyxModel.Create('Unknown component kind: ' + AKind.Name);
  end;

  if AGroup = pgAll then
  begin
    raise ENyxModel.Create('All groups is a filter, not a component group');
  end;
  FItems[LIndex].Discovery.Group := AGroup;
  FItems[LIndex].Discovery.Labels := ALabels;
  FItems[LIndex].Discovery.Aliases := AAliases;
  FItems[LIndex].Discovery.Description := ADescription;
  Result := Self;
end;

function TNyxCatalog.Matches(AIndex: Integer; const AQuery: TNyxText;
  AGroup: TNyxPaletteGroup): Boolean;
var
  LInfo: TNyxComponentInfo;
  LLabel: TNyxComponentLabel;
  LHaystack: TNyxText;
  LQuery: TNyxText;
  LStart: Integer;
  LCursor: Integer;
begin
  LInfo := GetItem(AIndex);
  Result := False;

  if (AGroup <> pgAll) and (AGroup <> LInfo.Discovery.Group) then
  begin
    Exit;
  end;
  { Legacy category text is retained for extension compatibility. Specific
    purpose labels, rather than every internal recipe part, drive discovery. }
  LHaystack := LInfo.Kind + ' ' + LInfo.Title + ' ' + LInfo.Category + ' ' +
    NyxPaletteGroupName(LInfo.Discovery.Group) + ' ' + LInfo.Discovery.Aliases + ' ' +
    LInfo.Discovery.Description;
  for LLabel := Low(TNyxComponentLabel) to High(TNyxComponentLabel) do
  begin

    if LLabel in LInfo.Discovery.Labels then
    begin
      LHaystack := LHaystack + ' ' + NyxComponentLabelName(LLabel) + ' ' +
        NyxComponentLabelAliases(LLabel);
    end;
  end;
  LHaystack := LowerCase(LHaystack);
  LQuery := LowerCase(Trim(AQuery));
  LCursor := 1;
  while LCursor <= Length(LQuery) do
  begin
    while (LCursor <= Length(LQuery)) and (LQuery[LCursor] <= ' ') do
    begin
      Inc(LCursor);
    end;
    LStart := LCursor;
    while (LCursor <= Length(LQuery)) and (LQuery[LCursor] > ' ') do
    begin
      Inc(LCursor);
    end;

    if (LCursor > LStart) and
      (Pos(Copy(LQuery, LStart, LCursor - LStart), LHaystack) = 0) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function TNyxCatalog.NewNode(AKind: TNyxKind; const AID: TNyxText): TNyxNode;
begin
  Result := NewNode(NyxKindName(AKind), AID);
end;

function TNyxCatalog.NewNode(const AKind: TNyxKindRef; const AID: TNyxText): TNyxNode;
begin
  Result := NewNode(AKind.Name, AID);
end;

function TNyxCatalog.NewNode(const AKind, AID: TNyxText): TNyxNode;
var
  LIndex: Integer;
  LPartIndex: Integer;
  LPartSerial: Integer;
  LInstanceID: TNyxText;

  procedure QualifyParts(ANode: TNyxNode);
  var
    LChildIndex: Integer;
  begin
    Inc(LPartSerial);
    ANode.Named(LInstanceID + '-part-' + IntToStr(LPartSerial));
    for LChildIndex := 0 to ANode.Count - 1 do
    begin
      QualifyParts(ANode.Children[LChildIndex]);
    end;
  end;
begin
  { Construct through one shared factory so Studio and hand-written code start
    with the same layout/value defaults. Item lists use LF; tables use tabs for
    cells. Custom recipes may override these properties after construction. }
  LIndex := IndexOf(AKind);

  if LIndex < 0 then
  begin
    raise ENyxModel.Create('Unknown component kind: ' + AKind);
  end;
  Result := TNyxNode.Create(AKind, AID);
  LInstanceID := Result.ID;
  try

    if FRecipes[LIndex] <> nil then
    begin
      LPartSerial := 0;
      Result.Props.Assign(FRecipes[LIndex].Props);
      { The recipe root owns contracts, extension data and bindings just as its
        descendants do. Copy those values too; sharing them or only copying
        properties would silently erase a compound's public type declarations. }
      Result.Extensions.Assign(FRecipes[LIndex].Extensions);
      for LPartIndex := 0 to FRecipes[LIndex].BindingCount - 1 do
      begin
        Result.SetBinding(FRecipes[LIndex].Bindings[LPartIndex]);
      end;
      { Preserve the semantic extension name and the physical primitive
        independently. This survives JSON/Pascal without retaining this registry. }
      Result.Configure.CustomProjection(NyxCustomKind(FRecipes[LIndex].ProjectionKind)).Done;

      if FRecipes[LIndex].Count > 0 then
      begin
        Result.Configure.Compound(True).Done;
      end;
      for LPartIndex := 0 to FRecipes[LIndex].Count - 1 do
      begin
        Result.Add(FRecipes[LIndex].Children[LPartIndex].Clone);
        QualifyParts(Result.Children[Result.Count - 1]);
      end;
      Exit;
    end;

    if FItems[LIndex].Container then
    begin
      Result.Configure.Layout(nlColumn).Gap(12).Padding(16).Done;

      if (AKind = NyxKindName(nkRow)) or (AKind = NyxKindName(nkToolbar)) then
      begin
        Result.Configure.Layout(nlRow).Done;
      end;

      if AKind = NyxKindName(nkGrid) then
      begin
        Result.Configure.Layout(nlGrid).Columns(2).Done;
      end;
    end
    else if AKind <> NyxKindName(nkComponent) then
    begin
      Result.Configure.Text(FItems[LIndex].Title).Done;
    end;

    if (AKind = NyxKindName(nkInput)) or (AKind = NyxKindName(nkMemo)) then
    begin
      Result.Configure.Placeholder('Enter a value...').Done;
    end;

    if (AKind = NyxKindName(nkSelect)) or (AKind = NyxKindName(nkList)) or (AKind = NyxKindName(nkTree)) then
    begin
      Result.Configure.Items('First item' + #10 + 'Second item' + #10 + 'Third item').Done;
    end;

    if AKind = NyxKindName(nkTable) then
    begin
      Result.Configure.Items('Name' + #9 + 'Status' + #10 + 'Example' + #9 + 'Ready').Done;
    end;

    if AKind = NyxKindName(nkButton) then
    begin
      Result.Configure.Variant(nvPrimary).Done;
    end;

    if AKind = NyxKindName(nkCodeEditor) then
    begin
      Result.Configure.Text('Pascal source').Value('')
        .Height(240).ReadOnly(False).Done;
    end;

    if (AKind = NyxKindName(nkProgress)) or (AKind = NyxKindName(nkSlider)) or (AKind = NyxKindName(nkSpin)) then
    begin
      Result.Configure.Value(50).Minimum(0).Maximum(100).Done;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
