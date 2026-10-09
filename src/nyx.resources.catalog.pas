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
unit nyx.resources.catalog;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.resources, nyx.collections, nyx.collections.view, nyx.collections.query,
  nyx.publication;

type
  TNyxResourceCatalogKinds = set of TNyxResourceKind;
  TNyxResourceCatalogLocales = (rclAny, rclDefault, rclLocalized);
  TNyxResourceCatalogSources = (rcsAny, rcsEmbedded, rcsHosted);
  TNyxResourceLabelMatch = (rlmAll, rlmAny);

  { Independent runtime discovery policy. Search covers names, titles, creator
    descriptions, locale names and creator labels. Its default folds ASCII only; callers may
    request exact scalar matching. Kinds combines alternatives, while search,
    locale, source and label restrictions combine as intersections. Kinds([]) matches
    nothing; AnyKind removes that restriction. Repeated setters replace their
    own restriction. No catalog, payload, document or target is retained. }
  TNyxResourceCatalogQuery = record
  private
    FSearch: TNyxText;
    FComparison: TNyxQueryTextComparison;
    FKinds: TNyxResourceCatalogKinds;
    FKindFilter: Boolean;
    FLocales: TNyxResourceCatalogLocales;
    FSources: TNyxResourceCatalogSources;
    FLabels: TNyxResourceLabels;
    FLabelMatch: TNyxResourceLabelMatch;
  public
    function Search(const AText: TNyxText;
      AComparison: TNyxQueryTextComparison = nqtAsciiInsensitive): TNyxResourceCatalogQuery;
    function Kinds(AKinds: TNyxResourceCatalogKinds): TNyxResourceCatalogQuery;
    function AnyKind: TNyxResourceCatalogQuery;
    function Locales(ALocales: TNyxResourceCatalogLocales): TNyxResourceCatalogQuery;
    function Sources(ASources: TNyxResourceCatalogSources): TNyxResourceCatalogQuery;
    { Tagged adds another required exact label. Labels replaces the complete
      label selector and chooses all/any matching; an empty set clears it.
      The ordinary collection predicate count/depth/data budgets still apply. }
    function Tagged(const ALabel: TNyxResourceLabelRef): TNyxResourceCatalogQuery;
    function Labels(const ALabels: TNyxResourceLabels;
      AMatch: TNyxResourceLabelMatch = rlmAll): TNyxResourceCatalogQuery;
    { Convert to the ordinary typed collection policy at its explicit extension
      boundary. Adapters still use the same query/selection/keyboard contracts. }
    function ToQuery: TNyxCollectionQuery;
  end;

  { A copied resource/locale identity, independent of list order and labels.
    It retains no catalog, payload, document, view, widget or callback receiver. }
  TNyxResourceCatalogChoice = record
    Reference: TNyxResourceRef;
    Locale: TNyxLocaleRef;
  end;

  { Managed metadata provider for an ordinary Nyx list. Its independent runtime
    store never changes authored defaults or retains asset bytes/definitions.
    Names and locales are exact; generated row IDs are scoped to the supplied
    collection key and stay assigned to the same pair for this provider's life.
    Removed rows cannot be chosen; reappearance restores their logical identity.
    Release/create a provider to start another independent project scope.
    View exposes the normal typed query/selection/typeahead contracts. Refresh,
    filtering and publication follow the collection/view's UI-thread confinement. }
  INyxResourceCatalog = interface(IInterface)
    ['{B8D1B346-49B9-4A8D-AB56-259BA72E553A}']
    function GetView: INyxCollectionView;
    { Change only this view's independent query. Hidden selection membership is
      retained; invalid policies refuse through the collection query contract. }
    function Filter(const AQuery: TNyxResourceCatalogQuery): INyxResourceCatalog;
    { Reserve an independent metadata candidate; nil means exact current rows.
      Catalog is borrowed only during preparation. No notification/admission
      occurs until the ordinary publication coordinator validates/installs it.
      A preparation keeps this provider alive and retires on abandonment. }
    function Prepare(const ACatalog: INyxResources): INyxPreparedPublication;
    { Publish one candidate. Validation failure preserves rows/identities;
      notification failure explicitly reports already-published data. }
    procedure Refresh(const ACatalog: INyxResources);
    { Resolve only an existing exact row at the optional expected data revision.
      Foreign/missing/stale/tampered rows raise before returning a choice.
      This revision identifies metadata, not payload contents. Source/binding
      edits must also guard their owning project's accepted revision/context. }
    function Choice(const AItem: TNyxItemRef;
      AExpectedRevision: Integer = -1): TNyxResourceCatalogChoice;
    { Find a current exact pair, returning an undefined reference when absent.
      This uses source identities even when a query hides the corresponding row. }
    function Find(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): TNyxItemRef;
    property View: INyxCollectionView read GetView;
  end;

{ The caller supplies an explicit collection scope; no global counter/name is
  invented. Alternative managed providers may implement INyxResourceCatalog. }
function NewNyxResourceCatalog(const AKey: TNyxCollectionRef;
  const ACatalog: INyxResources): INyxResourceCatalog;
{ Starts with all kinds/locales/sources and no search restriction. }
function NyxResourceCatalogQuery: TNyxResourceCatalogQuery;

implementation

uses SysUtils, nyx.bytes, nyx.collections.view.types, nyx.resource.sources;

const
  CName = 'resource';
  CLocale = 'locale';
  CTitle = 'title';
  CDescription = 'description';
  CKind = 'kind';
  CHosted = 'hosted';
  CLabels = 'labels';
  CLabelIndex = 'label-index';

{ Base64's alphabet excludes the separator. Match a complete framed UTF-8 token,
  rather than guessing identities from display delimiters or JSON substrings.
  This private runtime index never replaces the original portable labels. }
function LabelToken(const ALabel: TNyxResourceLabelRef): TNyxText;
begin
  Result := '|' + NyxEncodeBase64(NyxEncodeUTF8(ALabel.Name)) + '|';
end;

type
  TEntry = record
    Choice: TNyxResourceCatalogChoice;
    Item: TNyxItemRef;
  end;
  TEntries = array of TEntry;
  TCatalog = class;
  TChange = class(TInterfacedObject, INyxPreparedPublication)
  private
    FOwner: TCatalog;
    FLease: INyxResourceCatalog;
    FStore: INyxPreparedPublication;
    FEntries: TEntries;
    FInstalled: Boolean;
  public
    constructor Create(AOwner: TCatalog; const AEntries: TEntries;
      const AStore: INyxPreparedPublication);
    destructor Destroy; override;
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  TCatalog = class(TInterfacedObject, INyxResourceCatalog)
  private
    FKey: TNyxCollectionRef;
    FStore: INyxCollection;
    FView: INyxCollectionView;
    FEntries: TEntries;
    FPreparing: Boolean;
    function GetView: INyxCollectionView;
  public
    constructor Create(const AKey: TNyxCollectionRef);
    function Filter(const AQuery: TNyxResourceCatalogQuery): INyxResourceCatalog;
    function Prepare(const ACatalog: INyxResources): INyxPreparedPublication;
    procedure Refresh(const ACatalog: INyxResources);
    function Choice(const AItem: TNyxItemRef;
      AExpectedRevision: Integer = -1): TNyxResourceCatalogChoice;
    function Find(const AReference: TNyxResourceRef;
      const ALocale: TNyxLocaleRef): TNyxItemRef;
  end;

function NyxResourceCatalogQuery: TNyxResourceCatalogQuery;
begin
  Result := Default(TNyxResourceCatalogQuery);
  Result.FComparison := nqtAsciiInsensitive;
end;

function TNyxResourceCatalogQuery.Search(const AText: TNyxText;
  AComparison: TNyxQueryTextComparison): TNyxResourceCatalogQuery;
begin

  if (Ord(AComparison) < Ord(Low(TNyxQueryTextComparison))) or
    (Ord(AComparison) > Ord(High(TNyxQueryTextComparison))) then
  begin
    raise ENyxResource.Create('Unknown resource search comparison');
  end;
  Result := Self;
  Result.FSearch := AText;
  Result.FComparison := AComparison;
end;

function TNyxResourceCatalogQuery.Kinds(
  AKinds: TNyxResourceCatalogKinds): TNyxResourceCatalogQuery;
begin
  Result := Self;
  Result.FKinds := AKinds;
  Result.FKindFilter := True;
end;

function TNyxResourceCatalogQuery.AnyKind: TNyxResourceCatalogQuery;
begin
  Result := Self;
  Result.FKindFilter := False;
end;

function TNyxResourceCatalogQuery.Locales(
  ALocales: TNyxResourceCatalogLocales): TNyxResourceCatalogQuery;
begin

  if (Ord(ALocales) < Ord(Low(TNyxResourceCatalogLocales))) or
    (Ord(ALocales) > Ord(High(TNyxResourceCatalogLocales))) then
  begin
    raise ENyxResource.Create('Unknown resource locale filter');
  end;
  Result := Self;
  Result.FLocales := ALocales;
end;

function TNyxResourceCatalogQuery.Sources(
  ASources: TNyxResourceCatalogSources): TNyxResourceCatalogQuery;
begin

  if (Ord(ASources) < Ord(Low(TNyxResourceCatalogSources))) or
    (Ord(ASources) > Ord(High(TNyxResourceCatalogSources))) then
  begin
    raise ENyxResource.Create('Unknown resource source filter');
  end;
  Result := Self;
  Result.FSources := ASources;
end;

function TNyxResourceCatalogQuery.Tagged(
  const ALabel: TNyxResourceLabelRef): TNyxResourceCatalogQuery;
begin
  Result := Self;
  Result.FLabels := FLabels.Add(ALabel);
  Result.FLabelMatch := rlmAll;
end;

function TNyxResourceCatalogQuery.Labels(const ALabels: TNyxResourceLabels;
  AMatch: TNyxResourceLabelMatch): TNyxResourceCatalogQuery;
begin

  if (Ord(AMatch) < Ord(Low(TNyxResourceLabelMatch))) or
    (Ord(AMatch) > Ord(High(TNyxResourceLabelMatch))) then
  begin
    raise ENyxResource.Create('Unknown resource label matching policy');
  end;
  Result := Self;
  Result.FLabels := ALabels.Copy;
  Result.FLabelMatch := AMatch;
end;

function TNyxResourceCatalogQuery.ToQuery: TNyxCollectionQuery;
var
  LFilter: INyxCollectionPredicate;
  LAlternatives: INyxCollectionPredicate;
  LKind: TNyxResourceKind;
  LLabels: array of INyxCollectionPredicate;
  LIndex: Integer;

  { A balanced tree admits larger sets without accumulating linear predicate
    depth. The ordinary query engine still owns aggregate admission budgets. }
  function JoinLabels(AFirst, ALast: Integer): INyxCollectionPredicate;
  var
    LMiddle: Integer;
    LLeft: INyxCollectionPredicate;
    LRight: INyxCollectionPredicate;
  begin

    if AFirst = ALast then
    begin
      Exit(LLabels[AFirst]);
    end;
    LMiddle := AFirst + (ALast - AFirst) div 2;
    LLeft := JoinLabels(AFirst, LMiddle);
    LRight := JoinLabels(LMiddle + 1, ALast);

    if FLabelMatch = rlmAll then
    begin
      Result := LLeft.AndAlso(LRight);
    end
    else
    begin
      Result := LLeft.OrElse(LRight);
    end;
  end;

  procedure Intersect(const APredicate: INyxCollectionPredicate);
  begin

    if LFilter = nil then
    begin
      LFilter := APredicate;
    end
    else
    begin
      LFilter := LFilter.AndAlso(APredicate);
    end;
  end;

begin
  LFilter := nil;

  if FSearch <> '' then
  begin
    LAlternatives := NyxWhere(NyxTextField(CName)).Contains(FSearch, FComparison)
      .OrElse(NyxWhere(NyxTextField(CTitle)).Contains(FSearch, FComparison))
      .OrElse(NyxWhere(NyxTextField(CDescription)).Contains(FSearch, FComparison))
      .OrElse(NyxWhere(NyxTextField(CLocale)).Contains(FSearch, FComparison))
      .OrElse(NyxWhere(NyxTextField(CLabels)).Contains(FSearch, FComparison));
    Intersect(LAlternatives);
  end;

  if FKindFilter then
  begin
    LAlternatives := nil;
    for LKind := Low(TNyxResourceKind) to High(TNyxResourceKind) do
    begin

      if LKind in FKinds then
      begin

        if LAlternatives = nil then
        begin
          LAlternatives := NyxWhere(NyxIntegerField(CKind)).EqualTo(Ord(LKind));
        end
        else
        begin
          LAlternatives := LAlternatives.OrElse(
            NyxWhere(NyxIntegerField(CKind)).EqualTo(Ord(LKind)));
        end;
      end;
    end;

    if LAlternatives = nil then
    begin
      LAlternatives := NyxWhere(NyxIntegerField(CKind)).EqualTo(-1);
    end;
    Intersect(LAlternatives);
  end;

  if FLocales = rclDefault then
  begin
    Intersect(NyxWhere(NyxTextField(CLocale)).EqualTo(''));
  end
  else if FLocales = rclLocalized then
  begin
    Intersect(NyxWhere(NyxTextField(CLocale)).NotEqualTo(''));
  end;

  if FSources <> rcsAny then
  begin
    Intersect(NyxWhere(NyxBooleanField(CHosted)).EqualTo(FSources = rcsHosted));
  end;

  if FLabels.Count > 0 then
  begin
    SetLength(LLabels, FLabels.Count);
    for LIndex := 0 to FLabels.Count - 1 do
    begin
      LLabels[LIndex] := NyxWhere(NyxTextField(CLabelIndex)).Contains(LabelToken(FLabels.Item(LIndex)));
    end;
    Intersect(JoinLabels(0, High(LLabels)));
  end;
  Result := NyxCollectionQuery;

  if LFilter <> nil then
  begin
    Result := Result.Where(LFilter);
  end;
end;

{ The empty locale is a valid default identity, rather than a named locale.
  Copy through the appropriate public constructor on both compilers. }
function CopyLocale(const ALocale: TNyxLocaleRef): TNyxLocaleRef;
begin
  Result := NyxDefaultLocale;

  if ALocale.Name <> '' then
  begin
    Result := NyxLocale(ALocale.Name);
  end;
end;

constructor TChange.Create(AOwner: TCatalog; const AEntries: TEntries;
  const AStore: INyxPreparedPublication);
begin
  inherited Create;
  FOwner := AOwner;
  FLease := AOwner;
  FEntries := AEntries;
  FStore := AStore;
end;

destructor TChange.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TChange.Validate;
begin

  if (FOwner = nil) or FInstalled then
  begin
    raise ENyxResource.Create('Resource catalog preparation is not pending');
  end;
  FStore.Validate;
end;

procedure TChange.Install;
begin
  { Both the identity ledger and rows publish before any observer. Array adoption
    is allocation-free; no mutable caller array survives preparation. }
  FOwner.FEntries := FEntries;
  FStore.Install;
  FInstalled := True;
end;

procedure TChange.Notify;
begin
  FStore.Notify;
end;

procedure TChange.Retire;
begin

  if FOwner = nil then
  begin
    Exit;
  end;
  FStore.Retire;
  FStore := nil;
  FEntries := nil;
  FOwner.FPreparing := False;
  FOwner := nil;
  FLease := nil;
end;

constructor TCatalog.Create(const AKey: TNyxCollectionRef);
var
  LSchema: TNyxCollectionSchema;
begin
  inherited Create;
  FKey := AKey;
  LSchema := NyxCollectionSchema.Text(NyxTextField(CName), '')
    .Text(NyxTextField(CLocale), '').Text(NyxTextField(CTitle), '')
    .Text(NyxTextField(CDescription), '').Integer(NyxIntegerField(CKind), 0)
    .Boolean(NyxBooleanField(CHosted), False)
    .Text(NyxTextField(CLabels), '').Text(NyxTextField(CLabelIndex), '');
  FStore := NewNyxCollection(FKey, LSchema);
  FView := NewNyxCollectionView(FStore, NyxCollectionView(FKey)
    .Column(NyxTextField(CTitle), 'Resource'), cpList);
end;

function TCatalog.GetView: INyxCollectionView;
begin
  Result := FView;
end;

function TCatalog.Filter(const AQuery: TNyxResourceCatalogQuery): INyxResourceCatalog;
begin
  FView.ConfigureQuery(AQuery.ToQuery);
  Result := Self;
end;

function TCatalog.Prepare(const ACatalog: INyxResources): INyxPreparedPublication;
var
  LEntries: TEntries;
  LRows: array of TNyxCollectionItem;
  LBefore: INyxCollectionSnapshot;
  LCandidate: INyxCollectionSnapshot;
  LTransient: INyxCollection;
  LAtomic: INyxAtomicCollection;
  LPublication: INyxPreparedPublication;
  LReference: TNyxResourceRef;
  LLocale: TNyxLocaleRef;
  LDefinition: INyxResourceDefinition;
  LTitle: TNyxText;
  LIndex: Integer;
  LEntry: Integer;
  LEqual: Boolean;
  LLabels: TNyxResourceLabels;
  LLabelNames: TNyxText;
  LLabelIndex: TNyxText;
  LLabel: Integer;
begin
  Result := nil;

  if FPreparing or (ACatalog = nil) then
  begin
    raise ENyxResource.Create('Resource catalog requires an idle provider and catalog');
  end;
  FPreparing := True;
  try
    { Copy every record explicitly: pas2js arrays/records must not alias a ledger
      which a rejected candidate or caller can later mutate. }
    SetLength(LEntries, Length(FEntries));
    for LIndex := 0 to High(FEntries) do
    begin
      LEntries[LIndex].Choice.Reference := NyxResourceRef(FEntries[LIndex].Choice.Reference.Name);
      LEntries[LIndex].Choice.Locale := CopyLocale(FEntries[LIndex].Choice.Locale);
      LEntries[LIndex].Item := NyxItem(FKey, FEntries[LIndex].Item.ID);
    end;
    SetLength(LRows, ACatalog.Count);
    for LIndex := 0 to ACatalog.Count - 1 do
    begin
      LReference := ACatalog.Reference(LIndex);
      LLocale := ACatalog.Locale(LIndex);
      LEntry := 0;
      while (LEntry < Length(LEntries)) and
        ((LEntries[LEntry].Choice.Reference.Name <> LReference.Name) or
        (LEntries[LEntry].Choice.Locale.Name <> LLocale.Name)) do
      begin
        Inc(LEntry);
      end;

      if LEntry = Length(LEntries) then
      begin
        SetLength(LEntries, LEntry + 1);
        LEntries[LEntry].Choice.Reference := NyxResourceRef(LReference.Name);
        LEntries[LEntry].Choice.Locale := CopyLocale(LLocale);
        LEntries[LEntry].Item := NyxItem(FKey, 'resource-' + TNyxText(IntToStr(LEntry)));
      end;
      LDefinition := ACatalog.Definition(LReference, LLocale);
      LLabels := NyxResourceLabelsOf(LDefinition);
      LLabelNames := '';
      LLabelIndex := '';
      for LLabel := 0 to LLabels.Count - 1 do
      begin
        LLabelNames := LLabelNames + LLabels.Item(LLabel).Name + #10;
        LLabelIndex := LLabelIndex + LabelToken(LLabels.Item(LLabel));
      end;
      LTitle := LReference.Name;

      if LDefinition.Title <> '' then
      begin
        LTitle := LDefinition.Title + ' (' + LTitle + ')';
      end;

      if LLocale.Name <> '' then
      begin
        LTitle := LTitle + ' / ' + LLocale.Name;
      end;
      LRows[LIndex] := NyxCollectionItem(LEntries[LEntry].Item)
        .WithValue(NyxTextField(CName), LReference.Name)
        .WithValue(NyxTextField(CLocale), LLocale.Name)
        .WithValue(NyxTextField(CTitle), LTitle)
        .WithValue(NyxTextField(CDescription), LDefinition.Description)
        .WithValue(NyxIntegerField(CKind), Ord(LDefinition.Kind))
        .WithValue(NyxBooleanField(CHosted), LDefinition.Source.Kind = rskHosted)
        .WithValue(NyxTextField(CLabels), LLabelNames)
        .WithValue(NyxTextField(CLabelIndex), LLabelIndex);
    end;
    LBefore := FStore.Snapshot;
    LTransient := NewNyxCollection(FKey, LBefore.Schema, LRows);
    LCandidate := LTransient.Snapshot;
    LEqual := LBefore.Count = LCandidate.Count;
    LIndex := 0;
    while LEqual and (LIndex < LBefore.Count) do
    begin
      LEqual := LBefore.ItemAt(LIndex).SameItem(LCandidate.ItemAt(LIndex));
      Inc(LIndex);
    end;

    if LEqual then
    begin
      FPreparing := False;
      Exit;
    end;
    { Coordinated publication is an optional interface, not a parent of the
      ordinary store contract. Query it explicitly on both compilers. }

    if not Supports(FStore, INyxAtomicCollection, LAtomic) then
    begin
      raise ENyxResource.Create('Resource catalog requires an atomic metadata store');
    end;
    LPublication := LAtomic.PrepareAssign(LCandidate, LBefore.Revision);
    Result := TChange.Create(Self, LEntries, LPublication);
  except
    FPreparing := False;
    raise;
  end;
end;

procedure TCatalog.Refresh(const ACatalog: INyxResources);
var
  LChange: INyxPreparedPublication;
begin
  LChange := Prepare(ACatalog);

  if LChange <> nil then
  begin
    PublishNyxGroup([LChange]);
  end;
end;

function TCatalog.Choice(const AItem: TNyxItemRef;
  AExpectedRevision: Integer): TNyxResourceCatalogChoice;
var
  LSnapshot: INyxCollectionSnapshot;
  LRow: TNyxCollectionItem;
  LIndex: Integer;
begin
  LSnapshot := FStore.Snapshot;

  if (AExpectedRevision >= 0) and (AExpectedRevision <> LSnapshot.Revision) then
  begin
    raise ENyxResource.Create('Resource catalog selection is stale');
  end;
  LRow := LSnapshot.ItemAt(LSnapshot.IndexOf(AItem));
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].Item.ID = AItem.ID then
    begin
      Result.Reference := NyxResourceRef(FEntries[LIndex].Choice.Reference.Name);
      Result.Locale := CopyLocale(FEntries[LIndex].Choice.Locale);

      if (LRow.GetValue(NyxTextField(CName)) <> Result.Reference.Name) or
        (LRow.GetValue(NyxTextField(CLocale)) <> Result.Locale.Name) then
      begin
        raise ENyxResource.Create('Resource catalog row identity was changed externally');
      end;
      Exit;
    end;
  end;
  raise ENyxResource.Create('Unknown resource catalog identity');
end;

function TCatalog.Find(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): TNyxItemRef;
var
  LIndex: Integer;
  LSnapshot: INyxCollectionSnapshot;
begin
  Result := Default(TNyxItemRef);
  LSnapshot := FStore.Snapshot;
  for LIndex := 0 to High(FEntries) do
  begin

    if (FEntries[LIndex].Choice.Reference.Name = AReference.Name) and
      (FEntries[LIndex].Choice.Locale.Name = ALocale.Name) and
      (LSnapshot.IndexOf(FEntries[LIndex].Item) >= 0) then
    begin
      Result := NyxItem(FKey, FEntries[LIndex].Item.ID);
      Exit;
    end;
  end;
end;

function NewNyxResourceCatalog(const AKey: TNyxCollectionRef;
  const ACatalog: INyxResources): INyxResourceCatalog;
var
  LOwner: INyxResourceCatalog;
begin
  { Own the provider before initial publication creates/releases an additional
    preparation lease. pas2js does not use native constructor reference-count
    protection; publication inside that constructor can retire the provider. }
  LOwner := TCatalog.Create(AKey);
  LOwner.Refresh(ACatalog);
  Result := LOwner;
end;

end.
