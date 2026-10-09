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
program nyx_resource_catalog_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.resources, nyx.resource.sources, nyx.resources.catalog, nyx.collections,
  nyx.collections.view, nyx.publication
  {$ifdef PAS2JS}, Web{$endif};

type
  { Borrowed subscriber qualifies candidate rejection and committed notification
    separately. It reads the public provider while publication is notifying;
    all row identities must already resolve to the newly admitted ledger. }
  TObserver = class
  public
    Catalog: INyxResourceCatalog;
    Reject: Boolean;
    FailNotify: Boolean;
    Calls: Integer;
    procedure Validate(const ACandidate: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure Changed(const ACollection: INyxCollection;
      const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxResource.Create('Resource catalog: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TObserver.Validate(const ACandidate: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin

  if Reject then
  begin
    raise ENyxResource.Create('Creator refused the catalog candidate');
  end;
end;

procedure TObserver.Changed(const ACollection: INyxCollection;
  const AChanges: INyxCollectionChanges);
var
  LSnapshot: INyxCollectionSnapshot;
  LChoice: TNyxResourceCatalogChoice;
  LIndex: Integer;
begin
  Inc(Calls);
  LSnapshot := ACollection.Snapshot;
  for LIndex := 0 to LSnapshot.Count - 1 do
  begin
    LChoice := Catalog.Choice(LSnapshot.ItemAt(LIndex).Ref, LSnapshot.Revision);
    Check(LChoice.Reference.Name = LSnapshot.ItemAt(LIndex).GetValue(NyxTextField('resource')),
      'notification observes installed rows and identity ledger together');
  end;

  if FailNotify then
  begin
    raise ENyxResource.Create('Creator notification failed after publication');
  end;
end;

procedure Run;
var
  LResources: INyxResources;
  LNext: INyxResources;
  LCatalog: INyxResourceCatalog;
  LOther: INyxResourceCatalog;
  LRetainedView: INyxCollectionView;
  LToken: INyxCollectionSubscription;
  LChange: INyxPreparedPublication;
  LObserver: TObserver;
  LBefore: INyxCollectionSnapshot;
  LChoice: TNyxResourceCatalogChoice;
  LOriginal: TNyxItemRef;
  LLocaleItem: TNyxItemRef;
  LNewItem: TNyxItemRef;
  LRejected: Boolean;
  LName: TNyxText;
  LCalls: Integer;
  LPolicy: TNyxResourceCatalogQuery;
begin
  LName := 'Project notes / 🌙';
  LResources := NewNyxResources.Define(NyxResourceRef(LName),
    NyxTextResource(StringOfChar('x', 32768)).Describe('Notes', 'A portable help description 🌙'))
    .Define(NyxResourceRef(LName), NyxLocale('en-US'), NyxTextResource('Localized content'))
    .Define(NyxResourceRef(LName + ' / en-US'), NyxTextResource('A distinct exact resource'));
  LCatalog := NewNyxResourceCatalog(NyxCollection('catalog-review'), LResources);
  LOriginal := LCatalog.Find(NyxResourceRef(LName), NyxDefaultLocale);
  LLocaleItem := LCatalog.Find(NyxResourceRef(LName), NyxLocale('en-US'));
  Check(LOriginal.Defined and LLocaleItem.Defined and (LOriginal.ID <> LLocaleItem.ID),
    'same resource name and different locales have distinct stable identities');
  Check(LCatalog.Find(NyxResourceRef(LName + ' / en-US'), NyxDefaultLocale).ID <> LLocaleItem.ID,
    'display delimiter collisions never collapse resource/locale identity');
  LChoice := LCatalog.Choice(LOriginal);
  Check((LChoice.Reference.Name = LName) and (LChoice.Locale.Name = ''),
    'supplementary text and default locale survive metadata projection');
  Check(LCatalog.View.Store.Snapshot.DataBytes < 4096,
    'large embedded payload is not retained in the metadata dataset');
  LPolicy := NyxResourceCatalogQuery.Kinds([nrkText]).Locales(rclDefault);
  LCatalog.Filter(LPolicy.Search('HELP'));
  Check(LCatalog.View.Snapshot.Count = 1,
    'search includes creator descriptions and intersects typed kind/locale filters');
  LCatalog.Filter(LPolicy);
  Check(LCatalog.View.Snapshot.Count = 2,
    'derived search policy does not alter its independent base');
  LCatalog.Filter(NyxResourceCatalogQuery.Kinds([]));
  Check(LCatalog.View.Snapshot.Count = 0, 'an explicitly empty kind set matches no rows');
  LCatalog.Filter(NyxResourceCatalogQuery.Kinds([]).AnyKind.Locales(rclLocalized));
  Check(LCatalog.View.Snapshot.Count = 1, 'AnyKind removes the kind restriction independently');
  LCatalog.Filter(NyxResourceCatalogQuery.Sources(rcsHosted));
  Check(LCatalog.View.Snapshot.Count = 0, 'hosted category does not misclassify embedded resources');
  LCatalog.Filter(NyxResourceCatalogQuery.Sources(rcsEmbedded));
  Check(LCatalog.View.Snapshot.Count = 3, 'embedded category covers every current source');
  LCatalog.Refresh(LResources.Clone.Define(NyxResourceRef('remote-notes'),
    NyxHostedResource(nrkText, NyxResourceURL('https://example.test/notes'))));
  LCatalog.Filter(NyxResourceCatalogQuery.Sources(rcsHosted));
  Check((LCatalog.View.Snapshot.Count = 1) and
    (LCatalog.Choice(LCatalog.View.Snapshot.ItemAt(0).Ref).Reference.Name = 'remote-notes'),
    'hosted source metadata is discoverable without fetching its payload');
  LCatalog.Refresh(LResources);
  LCatalog.Filter(NyxResourceCatalogQuery);
  LCatalog.View.Store.Update(LCatalog.View.Store.Snapshot.ItemAt(0)
    .WithValue(NyxTextField('resource'), 'external-change'));
  LRejected := False;
  try
    LCatalog.Choice(LOriginal);
  except
    on LException: ENyxResource do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'externally altered row identity cannot authorize a resource choice');
  LCatalog.Refresh(LResources);
  LOther := NewNyxResourceCatalog(NyxCollection('other-catalog'), LResources);
  LRejected := False;
  try
    LCatalog.Choice(LOther.Find(NyxResourceRef(LName), NyxDefaultLocale));
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'another explicit collection scope cannot choose this resource');
  LObserver := TObserver.Create;
  try
    LObserver.Catalog := LCatalog;
    LToken := LCatalog.View.Store.Subscribe({$ifdef PAS2JS}@{$endif}LObserver.Changed,
      {$ifdef PAS2JS}@{$endif}LObserver.Validate);
    LBefore := LCatalog.View.Store.Snapshot;
    LCalls := LObserver.Calls;
    LCatalog.Refresh(LResources);
    Check((LBefore.Revision = LCatalog.View.Store.Snapshot.Revision) and
      (LCalls = LObserver.Calls), 'equal metadata makes no revision or notification');
    { Resource registries own mutable membership; independent candidates clone
      that membership while sharing only immutable payload definitions. }
    LNext := LResources.Clone.Define(NyxResourceRef('new-file'), NyxJSONResource('{"ready":true}'));
    LChange := LCatalog.Prepare(LNext);
    Check(not LCatalog.Find(NyxResourceRef('new-file'), NyxDefaultLocale).Defined,
      'prepared identity remains private before publication');
    LChange := nil;
    Check(LCatalog.Prepare(LResources) = nil, 'abandonment releases the provider reservation');
    LObserver.Reject := True;
    LRejected := False;
    try
      LCatalog.Refresh(LNext);
    except
      on LException: ENyxResource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LBefore.Revision = LCatalog.View.Store.Snapshot.Revision) and
      not LCatalog.Find(NyxResourceRef('new-file'), NyxDefaultLocale).Defined,
      'validator refusal preserves the exact accepted rows and identity ledger');
    LObserver.Reject := False;
    LCatalog.Refresh(LNext);
    LNewItem := LCatalog.Find(NyxResourceRef('new-file'), NyxDefaultLocale);
    Check(LNewItem.Defined and (LCatalog.Find(NyxResourceRef(LName), NyxDefaultLocale).ID = LOriginal.ID),
      'successful insertion preserves surviving resource identities');
    LRejected := False;
    try
      LCatalog.Choice(LOriginal, LBefore.Revision);
    except
      on LException: ENyxResource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'old selection data revision is explicitly refused');
    LCatalog.View.Select(LOriginal);
    LCatalog.Refresh(LNext.Clone.Remove(NyxResourceRef(LName), NyxDefaultLocale));
    Check(not LCatalog.View.HasSelection and
      not LCatalog.Find(NyxResourceRef(LName), NyxDefaultLocale).Defined,
      'removal prunes selected membership and refuses retired resources');
    LCatalog.Refresh(LNext);
    Check(LCatalog.Find(NyxResourceRef(LName), NyxDefaultLocale).ID = LOriginal.ID,
      'reappearance restores the same logical resource identity');
    LObserver.FailNotify := True;
    LRejected := False;
    try
      LCatalog.Refresh(LNext.Clone.Define(NyxResourceRef('published-file'), NyxTextResource('Published')));
    except
      on LException: ENyxPublicationNotification do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and LCatalog.Find(NyxResourceRef('published-file'), NyxDefaultLocale).Defined,
      'failed observer explicitly reports committed publication without losing its identity');
    LObserver.FailNotify := False;
  finally
    LChange := nil;
    LToken := nil;
    LObserver.Catalog := nil;
    LObserver.Free;
  end;
  { Independent view/store can outlive the provider; its rows retain values,
    without keeping source definitions or any UI owner alive. }
  LRetainedView := LCatalog.View;
  LOther := nil;
  LCatalog := nil;
  LResources := nil;
  LNext := nil;
  Check(LRetainedView.Store.Snapshot.Count = 5,
    'independent runtime view survives provider and source catalog release');
  LRetainedView := nil;
  LResources := NewNyxResources.Define(NyxResourceRef('lease-test'), NyxTextResource('Accepted'));
  LCatalog := NewNyxResourceCatalog(NyxCollection('preparation-lease'), LResources);
  LRetainedView := LCatalog.View;
  LChange := LCatalog.Prepare(LResources.Clone.Define(
    NyxResourceRef('lease-addition'), NyxTextResource('Prepared')));
  LCatalog := nil;
  PublishNyxGroup([LChange]);
  Check(LRetainedView.Store.Snapshot.Count = 2,
    'preparation lease keeps its provider alive after the caller releases ownership');
  LChange := nil;
  LRetainedView := nil;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' portable resource catalog checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-test-result', 'passed');
    document.body.setAttribute('data-catalog-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-test-result', 'failed');
      document.body.setAttribute('data-catalog-error', LException.Message);
      {$else}ExitCode := 1;{$endif}
    end;
  end;
end.
