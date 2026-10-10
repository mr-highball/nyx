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

unit nyx.resource.mapping.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.model, nyx.controls, nyx.resources, nyx.resources.rows,
  nyx.collections, nyx.collections.view.types, nyx.bytes, nyx.resource.sources, nyx.images;

const
  { English starter records; exact decimal spelling remains resource data. }
  NyxMappingInitial: TNyxText = '{"batches":[{"people":[{"key":["ada"],"literal.name":"Ada","score":9,"ready":true,"ratio":1.2500},{"key":["sam"],"literal.name":"Sam","score":7,"ready":false,"ratio":0.5}]}]}';
  NyxMappingUpdated: TNyxText = '{"batches":[{"people":[{"key":["ada"],"literal.name":"Ada 🌙","score":10,"ready":false,"ratio":1.5},{"key":["sam"],"literal.name":"Sam","score":8,"ready":true,"ratio":0.75}]}]}';
  { The same owned PNG used by resource admission and the English MCP companion.
    Include every resource family in the existing full/page/reusable source gate,
    rather than qualifying only JSON/text recipes with a narrower builder. }
  NyxMappingPNG: TNyxText = 'iVBORw0KGgoAAAANSUhEUgAAAGQAAAAyEAIAAAB1xzWqAAAACXBIWXMAAAAAAAAAAACdYiYyAAABMElEQVR4nO3OsQ0AIAzAsP7/dOEEtsgSGTxnduf2fbE/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDxwO6T+sr8laFkAAAAABJRU5ErkJggg==';

{ A complete saved contract, without copying rows into collection defaults.
  Structural field/item steps preserve literal dots and array identities.
  Caller owns the document; managed controls are adopted by its owned tree. }
function NyxMappingRecipe: TNyxResourceRows;
function NyxMappingWorkshop: TNyxDocument;

implementation

function NyxMappingRecipe: TNyxResourceRows;
begin
  Result := NyxResourceRows(NyxResourceRef('team')).Field('batches').Item(0).Field('people')
    .Identity(NyxResourcePath.Field('key').Item(0))
    .Text(NyxTextField('name'), NyxResourcePath.Field('literal.name'))
    .Integer(NyxIntegerField('score'))
    .Boolean(NyxBooleanField('ready'))
    .Number(NyxNumberField('ratio'));
end;

function NyxMappingWorkshop: TNyxDocument;
var
  LDocument: TNyxDocument;
  LPage: INyxColumn;
  LCard: INyxColumn;
  LTable: INyxTable;
  LInstance: INyxComponent;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Team resource workshop';
    LDocument.Resources.Define(NyxResourceRef('team'), NyxJSONResource(NyxMappingInitial)
      .Describe('Team records', 'Stable identities and typed values for team tables.'));
    LDocument.Resources.Define(NyxResourceRef('team'), NyxLocale('en-GB'),
      NyxJSONResource(NyxMappingUpdated));
    LDocument.Resources.Define(NyxResourceRef('copy'), NyxTextResource('Build for tomorrow'));
    LDocument.Resources.Define(NyxResourceRef('notes'),
      NyxTextResource(TNyxText('Notes / 🌙') + TNyxText(#0) + TNyxText(#13#10))
        .Tagged(NyxResourceLabel('Help')).Tagged(NyxResourceLabel('Team')));
    LDocument.Resources.Define(NyxResourceRef('packed'),
      NyxBinaryResource(NyxDecodeBase64('AAH/')).Tagged(NyxResourceLabel('Data')));
    LDocument.Resources.Define(NyxResourceRef('brand'),
      NyxImageResource(NyxEmbeddedImage(nimPNG, NyxMappingPNG))
        .Tagged(NyxResourceLabel('Brand')).Describe('Team swatch', 'An embedded project image.'));
    { This is authored fallback/source evidence, never a hosted transport test.
      Application consumers explicitly use on-demand loading below. }
    LDocument.Resources.Define(NyxResourceRef('remote-guide'),
      NyxResourceDiscovery(NyxHostedResource(nrkJSON, NyxResourceURL('https://example.com/team-guide.json'))
        .Cache(NyxResourceCache.Persistent.FreshFor(600).StaleFor(90)
          .MaximumBytes(65536).ServerPolicy(rcspOverride))
        .Fallback(NyxJSONResource('{"literal.prompt":"Choose a team name","exact":9007199254740993}')
          .Tagged(NyxResourceLabel('Fallback'))))
        .Tagged(NyxResourceLabel('Help')).Tagged(NyxResourceLabel('Team'))
        .Describe('Team guide', 'Hosted copy with independently labelled embedded fallback.'));
    LDocument.ResourceCollections.Define(NyxCollection('people'), NyxMappingRecipe);
    LDocument.Collections.Define(NyxCollection('choices'),
      NyxCollectionSchema.Text(NyxTextField('name'), ''), []);

    LCard := NewNyxColumn('team-card', ncoDescriptor);
    LTable := NewNyxTable('card-table');
    LTable.Binds.Collection(NyxCollectionView(NyxCollection('people')).Scoped(csInstance)
      .Column(NyxTextField('name'), 'Name', cmEditable)
      .Column(NyxIntegerField('score'), 'Score')).Done;
    LCard.Add(LTable);
    LCard.Add(NewNyxLabel('card-guide').Binds.Text(
      NyxResourceValue(NyxResourceRef('remote-guide')).Field('literal.prompt')
        .Localize(NyxDefaultLocale, NyxDefaultLocale)).Done);
    LCard.Add(NewNyxInput('card-prompt').Binds.Placeholder(
      NyxResourceValue(NyxResourceRef('remote-guide')).Field('literal.prompt')).Done);
    LCard.Add(NewNyxImage('card-brand').Binds.Image(
      NyxResourceImage(NyxResourceRef('brand'))).Done);
    LDocument.AddComponent(LCard);

    LPage := NewNyxColumn('home');
    LPage.Configure.Padding(24).Gap(12).Done;
    LPage.Add(NewNyxLabel('headline').Binds.Text(NyxResourceValue(NyxResourceRef('copy'))).Done);
    LTable := NewNyxTable('people-table');
    LTable.Binds.Collection(NyxCollectionView(NyxCollection('people'))
      .Column(NyxTextField('name'), 'Name', cmEditable)
      .Column(NyxIntegerField('score'), 'Score')
      .Column(NyxBooleanField('ready'), 'Ready')
      .Column(NyxNumberField('ratio'), 'Ratio')).Done;
    LPage.Add(LTable);
    LInstance := NewNyxComponent('first-card');
    LInstance.Configure.Component(NyxComponent('team-card')).Done;
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('second-card');
    LInstance.Configure.Component(NyxComponent('team-card')).Done;
    LPage.Add(LInstance);
    LDocument.AddPage(LPage);

    LPage := NewNyxColumn('details');
    LTable := NewNyxTable('detail-table');
    LTable.Binds.Collection(NyxCollectionView(NyxCollection('people'))
      .Column(NyxTextField('name'), 'Name')).Done;
    LPage.Add(LTable);
    LDocument.AddPage(LPage);
    LDocument.Validate;
    Result := LDocument;
  except
    LDocument.Free;
    raise;
  end;
end;

end.
