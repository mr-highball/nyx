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

program nyx_resource_companion;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.bytes, nyx.types, nyx.resources,
  nyx.resources.rows, nyx.resource.sources, nyx.binding.types, nyx.collections,
  nyx.collections.view.types, nyx.studio.edits, nyx.studio.stateedits,
  nyx.studio.collectionedits, nyx.studio.resourceedits, nyx.studio.transactions;

const
  CCreate = 'create';
  CTitle = 'title';
  CCopy: TNyxText = '{"headline":"Put your project files to work",' +
    '"prompt":"Give your project a name","rows":[' +
    '{"id":"design","item":"Design notes","amount":3.125},' +
    '{"id":"assets","item":"Shared assets","amount":6.5}]}';
  { Owned PNG sample already used by the maintained image/resource admission
    consumers. Its real framing/checksums remain mandatory; no external files
    or image library are required to construct this embedded definition. }
  CPNG: TNyxText = 'iVBORw0KGgoAAAANSUhEUgAAAGQAAAAyEAIAAAB1xzWqAAAACXBIWXMAAAAAAAAAAACdYiYyAAABMElEQVR4nO3OsQ0AIAzAsP7/dOEEtsgSGTxnduf2fbE/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDxwO6T+sr8laFkAAAAABJRU5ErkJggg==';

{ The explicitly serialized editor boundary uses enum control kinds and distinct
  control identities. Scalar properties below are JSON wire values, not a second
  UI contract. Studio admits them through its normal schema and owned candidate.
  A page is a root; every other control requires its exact parent reference. }
function CreateControl(AKind: TNyxKind; const AControl, AParent: TNyxControlRef;
  const AProperties: TNyxDataValue): TNyxDataValue;
begin

  if AKind = nkPage then
  begin
    Result := NyxObject([NyxField('op', NyxData(CCreate)),
      NyxField('kind', NyxData(NyxKindName(AKind))), NyxField('id', NyxData(AControl.ID)),
      NyxField('root', NyxData('page')), NyxField('properties', AProperties)]);
  end
  else
  begin
    Result := NyxObject([NyxField('op', NyxData(CCreate)),
      NyxField('kind', NyxData(NyxKindName(AKind))), NyxField('id', NyxData(AControl.ID)),
      NyxField('parent', NyxData(AParent.ID)), NyxField('properties', AProperties)]);
  end;
end;

{ Return copied immutable transaction intent for an EMPTY owned review/project.
  No document, filename, compiler configuration, renderer or server is retained.
  Resource names/tags/field names are open application data. Binding behavior,
  row types, control kinds and cache policies use the public Pascal contracts.
  The final group defines every dependency before its corresponding binding. }
function ResourceLibrary: INyxProjectTransaction;
var
  LHome: TNyxControlRef;
  LNoParent: TNyxControlRef;
  LBrand: INyxResourceDiscovery;
  LCopy: INyxResourceDefinition;
  LNotes: INyxResourceDefinition;
  LPacked: INyxResourceDefinition;
  LHosted: INyxResourceDefinition;
  LDesign: INyxDesignPatch;
  LResources: INyxResourcePatch;
  LCollections: INyxCollectionPatch;
begin
  LHome := NyxControl('library-home');
  LNoParent := Default(TNyxControlRef);
  LDesign := ReadNyxDesignPatch(NyxArray([
    NyxObject([NyxField('op', NyxData(CTitle)), NyxField('value', NyxData('Resource library'))]),
    CreateControl(nkPage, LHome, LNoParent,
      NyxObject([NyxField('gap', NyxData(16)), NyxField('padding', NyxData(24))])),
    CreateControl(nkBadge, NyxControl('library-badge'), LHome,
      NyxObject([NyxField('text', NyxData('PROJECT RESOURCES'))])),
    CreateControl(nkHeading, NyxControl('library-headline'), LHome, NyxObject([])),
    CreateControl(nkImage, NyxControl('library-brand'), LHome,
      NyxObject([NyxField('alt', NyxData('Embedded project swatch')),
        NyxField('width', NyxData(100)), NyxField('height', NyxData(50))])),
    CreateControl(nkInput, NyxControl('library-project-name'), LHome,
      NyxObject([NyxField('text', NyxData('Project name'))])),
    CreateControl(nkTable, NyxControl('library-table'), LHome, NyxObject([])),
    CreateControl(nkLabel, NyxControl('library-notes'), LHome, NyxObject([]))]));

  LCopy := NyxJSONResource(CCopy)
    .Tagged(NyxResourceLabel('Onboarding')).Tagged(NyxResourceLabel('Copy'))
    .Describe('Shared project copy', 'Onboarding captions, prompts and typed table rows.');
  LNotes := NyxTextResource('Your project files belong beside your design.')
    .Tagged(NyxResourceLabel('Help')).Tagged(NyxResourceLabel('Onboarding'))
    .Describe('Project notes', 'Plain English help packed with the project.');
  LPacked := NyxBinaryResource(NyxDecodeBase64('AAH/'))
    .Tagged(NyxResourceLabel('Data')).Tagged(NyxResourceLabel('Export'))
    .Describe('Packed data', 'Three exact binary bytes for an application to consume.');
  LBrand := NyxResourceDiscovery(NyxResourceFromBytes(nrkImage, NyxDecodeBase64(CPNG)));
  LHosted := NyxHostedResource(nrkJSON, NyxResourceURL('https://example.com/project-copy.json'))
    .Tagged(NyxResourceLabel('Onboarding')).Tagged(NyxResourceLabel('Copy'))
    .Describe('Hosted English copy', 'Example URL with embedded fallback; configure your own host.')
    .Fallback(NyxJSONResource(CCopy))
    .Cache(NyxResourceCache.Persistent.FreshFor(600).StaleFor(90).MaximumBytes(65536));
  LResources := NyxResourcePatch([
    NyxDefineResource(NyxResourceRef('copy'), NyxDefaultLocale, LCopy),
    NyxDefineResource(NyxResourceRef('copy'), NyxLocale('en-US'), LHosted),
    NyxDefineResource(NyxResourceRef('notes'), NyxDefaultLocale, LNotes),
    NyxDefineResource(NyxResourceRef('packed'), NyxDefaultLocale, LPacked),
    NyxDefineResource(NyxResourceRef('brand'), NyxDefaultLocale,
      LBrand.Tagged(NyxResourceLabel('Brand')).Tagged(NyxResourceLabel('Onboarding'))
        .Describe('Project swatch', 'An embedded PNG shared by project controls.')),
    NyxBindResource(NyxControl('library-headline'), bpText,
      NyxResourceValue(NyxResourceRef('copy')).Field('headline')),
    NyxBindResource(NyxControl('library-project-name'), bpPlaceholder,
      NyxResourceValue(NyxResourceRef('copy')).Field('prompt')),
    NyxBindResource(NyxControl('library-notes'), bpText, NyxResourceValue(NyxResourceRef('notes'))),
    NyxBindResourceImage(NyxControl('library-brand'), NyxResourceImage(NyxResourceRef('brand'))),
    NyxDefineResourceRows(NyxCollection('library-rows'),
      NyxResourceRows(NyxResourceRef('copy')).Field('rows')
        .Identity(NyxResourcePath.Field('id')).Text(NyxTextField('item'))
        .Number(NyxNumberField('amount')))]);
  LCollections := NyxCollectionPatch([
    NyxBindCollection(NyxBindingOwner('library-table'), cpTable,
      NyxCollectionView(NyxCollection('library-rows'))
        .Column(NyxTextField('item'), 'Item').Column(NyxNumberField('amount'), 'Amount'))]);
  Result := NyxProjectTransaction([
    NyxDesignStep(LDesign), NyxResourceStep(LResources), NyxCollectionStep(LCollections)]);
end;

begin
  { Stdout is explicit semantic JSON intent for nyx_transaction.operations.
    This tool neither connects to Studio nor starts a listener or compiler job. }
  try
    WriteLn(ResourceLibrary.ToData.ToJSON);
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, LException.ClassName, ': ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
