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


unit nyx.test.resource.workbench;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model;

const
  WorkbenchJSON = '{"literal.dot":"Your resource workbench","prompt":"Choose a project name","rows":[{"id":"canvas","item":"Canvas","amount":3.125},{"id":"studio","item":"Studio","amount":6.5}],"ready":true}';
  WorkbenchNotes = 'Your project files belong beside your design.';
  WorkbenchCopyTitle = 'Workbench copy';
  WorkbenchCopyHelp = 'Captions, prompts and table rows for this project.';
  WorkbenchNotesTitle = 'Project notes';
  WorkbenchNotesHelp = 'Plain text packed with the design.';
  WorkbenchPackedTitle = 'Packed data';
  WorkbenchPackedHelp = 'Three exact binary bytes kept with the project.';

{ Checks the reconstructed authored contract, separately from actual controls.
  The unchanged MCP companion and UI-emitted builders consume the same checks.
  No document is mutated and no runtime store is mistaken for authored defaults. }
function CheckNyxResourceWorkbench(ADocument: TNyxDocument): Integer;

implementation

uses SysUtils, nyx.bytes, nyx.resources, nyx.resources.rows,
  nyx.collections, nyx.binding.types;

function CheckNyxResourceWorkbench(ADocument: TNyxDocument): Integer;
var
  LBinding: TNyxBindingSpec;
  LBytes: TNyxBytes;
  LRows: INyxCollectionSnapshot;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Resource workbench reconstruction: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  Check(ADocument.Title = 'Resource workbench', 'project identity');
  Check(ADocument.Resources.Count = 3, 'three common file kinds');
  Check(ADocument.Resources.Definition(NyxResourceRef('copy'), NyxDefaultLocale)
    .ToData.Field('content').AsText =
    WorkbenchJSON, 'exact JSON source tokens');
  Check(ADocument.Resources.Definition(NyxResourceRef('copy'), NyxDefaultLocale).Title =
    WorkbenchCopyTitle, 'creator title');
  Check(ADocument.Resources.Definition(NyxResourceRef('copy'), NyxDefaultLocale).Description =
    WorkbenchCopyHelp, 'creator help');
  Check(ADocument.Resources.Definition(NyxResourceRef('notes'), NyxDefaultLocale).Text =
    WorkbenchNotes, 'packed UTF-8 text');
  LBytes := ADocument.Resources.Definition(NyxResourceRef('packed'), NyxDefaultLocale).Bytes;
  Check((Length(LBytes) = 3) and (LBytes[0] = 0) and (LBytes[1] = 1) and
    (LBytes[2] = 255), 'exact arbitrary binary bytes');
  Check(ADocument.Find('workshop-headline').FindBinding(bpText, LBinding), 'caption binding');
  Check(LBinding.ResourceValue.Path.ToData.ToJSON = NyxResourcePath.Field('literal.dot').ToData.ToJSON,
    'literal dotted key stays structural');
  Check(ADocument.Find('project-name').FindBinding(bpPlaceholder, LBinding), 'prompt binding');
  Check(LBinding.ResourceValue.Path.ToData.ToJSON = NyxResourcePath.Field('prompt').ToData.ToJSON,
    'prompt selector');
  Check(ADocument.Find('workshop-notes').FindBinding(bpText, LBinding) and
    (LBinding.ResourceValue.Reference.Name = 'notes'), 'plain text root selector');
  Check(ADocument.ResourceCollections.HasSource(NyxCollection('workshop-rows')), 'saved row relationship');
  Check(ADocument.Collections.Snapshot(NyxCollection('workshop-rows')).Count = 0,
    'authored empty defaults remain separate from runtime rows');
  LRows := ADocument.ResourceCollections.Source(NyxCollection('workshop-rows')).Read(
    ADocument.Resources, NyxCollection('workshop-rows'), NyxDefaultLocale, NyxDefaultLocale);
  Check(LRows.Count = 2, 'two detached runtime seed rows');
  Check((LRows.ItemAt(0).Ref.ID = 'canvas') and (LRows.ItemAt(1).Ref.ID = 'studio'),
    'explicit stable row identities');
end;

end.
