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
{ Selection-state admission and rollback reuse the exact reconstructed companion.
  These checks exercise the common library contract on both compilers, separately
  from ordinary controller/physical-input checks; the borrowed document stays exact. }
function CheckNyxResourceSelection(ADocument: TNyxDocument): Integer;

implementation

uses SysUtils, nyx.bytes, nyx.resources, nyx.resources.rows,
  nyx.collections, nyx.binding.types, nyx.resources.editor, nyx.controls,
  nyx.codec;

type
  TSelectionSync = class
  public
    Calls: Integer;
    RefuseOnce: Boolean;
    procedure Synchronize;
  end;

procedure TSelectionSync.Synchronize;
begin
  Inc(Calls);

  if RefuseOnce then
  begin
    RefuseOnce := False;
    raise Exception.Create('Intentional resource selection synchronization refusal');
  end;
end;

function CheckNyxResourceSelection(ADocument: TNyxDocument): Integer;
const
  CForm = 'selection-form';
var
  LShell: TNyxDocument;
  LChangedCatalog: TNyxDocument;
  LForm: INyxCard;
  LParent: INyxColumn;
  LName: TNyxNode;
  LAdded: TNyxNode;
  LObserver: TSelectionSync;
  LBefore: TNyxText;
  LDocumentBefore: TNyxText;
  LDraft: TNyxResourceEditorDraft;
  LRefused: Boolean;
  LSelection: TNyxResourceEditorSelection;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Resource selection: ' + AReason);
    end;
    Inc(Result);
  end;
begin
  Result := 0;
  LShell := TNyxDocument.Create;
  LObserver := TSelectionSync.Create;
  try
    LDocumentBefore := TNyxCodec.Encode(ADocument);
    LParent := NewNyxColumn('selection-parent');
    LShell.AddPage(LParent);
    LForm := NewNyxResourceEditor(CForm, ADocument.Resources, NyxNewResourceSelection);
    LParent.Add(LForm);
    LName := LForm.Node.Find(NyxResourceEditorFieldID(CForm, refName));
    LName.Configure.Hint('A creator-owned hint').Width(260).Done;
    LForm.Configure.Padding(17).Done;
    LForm.Add(NewNyxButton('selection-custom-action').WithText('Creator action'));
    LAdded := LForm.Node.Find('selection-custom-action');
    LSelection := NyxResourceSelection(NyxResourceRef('copy'), NyxDefaultLocale);
    Check(TrySelectNyxResourceEditor(LForm.Node, ADocument.Resources, LSelection,
      nil, nil, {$ifdef PAS2JS}@{$endif}LObserver.Synchronize), 'exact context admits Open');
    Check((LObserver.Calls = 1) and
      (LForm.Node.Find(NyxResourceEditorFieldID(CForm, refName)) = LName) and
      (LForm.Node.Find('selection-custom-action') = LAdded), 'owned nodes and descendants are retained');
    Check((LName.Prop('hint') = 'A creator-owned hint') and (LName.Prop('width') = '260') and
      (LForm.Node.Prop('padding') = '17'), 'creator presentation remains independent');
    Check((LName.Prop('value') = 'copy') and (LName.Prop('readonly') = 'true') and
      (ReadNyxResourceEditor(LForm.Node).ToData.ToJSON =
        ADocument.Resources.Definition(NyxResourceRef('copy'), NyxDefaultLocale).ToData.ToJSON),
      'Open supplies the exact JSON proposal and locks existing identity');
    LDraft.Capture(CForm, LForm.Node);
    Check(TrySelectNyxResourceEditor(LForm.Node, ADocument.Resources, NyxNewResourceSelection),
      'New is an independent retained proposal');
    Check((LName.Prop('value') = '') and (LName.Prop('readonly') <> 'true') and
      not LDraft.Restore(LForm.Node), 'New unlocks identity and refuses the former selection draft');
    Check(TrySelectNyxResourceEditor(LForm.Node, ADocument.Resources,
      NyxResourceSelection(NyxResourceRef('notes'), NyxDefaultLocale)) and
      (ReadNyxResourceEditor(LForm.Node).Text = WorkbenchNotes), 'Open text preserves exact contents');
    Check(TrySelectNyxResourceEditor(LForm.Node, ADocument.Resources,
      NyxResourceSelection(NyxResourceRef('packed'), NyxDefaultLocale)) and
      (ReadNyxResourceEditor(LForm.Node).ToData.ToJSON =
        ADocument.Resources.Definition(NyxResourceRef('packed'), NyxDefaultLocale).ToData.ToJSON),
      'Open binary preserves exact Base64 and closed kind');
    LBefore := TNyxCodec.Encode(LShell);
    Check(not TrySelectNyxResourceEditor(LForm.Node, ADocument.Resources,
      NyxResourceSelection(NyxResourceRef('missing'), NyxDefaultLocale)) and
      (TNyxCodec.Encode(LShell) = LBefore), 'missing variant refuses without partial state');
    Check(not TrySelectNyxResourceEditor(LForm.Node, ADocument.Resources, LSelection,
      ADocument.Find('workshop-headline'), ADocument.Find('workshop-headline')) and
      (TNyxCodec.Encode(LShell) = LBefore), 'changed selected-owner context refuses atomically');
    Check(not TrySelectNyxResourceEditor(LForm.Node, nil, LSelection) and
      (TNyxCodec.Encode(LShell) = LBefore), 'missing catalog refuses atomically');
    LChangedCatalog := ADocument.Clone;
    try
      LChangedCatalog.Resources.Define(NyxResourceRef('notes'), NyxTextResource('Changed notes'));
      Check(not TrySelectNyxResourceEditor(LForm.Node, LChangedCatalog.Resources, LSelection) and
        (TNyxCodec.Encode(LShell) = LBefore), 'changed catalog refuses before proposal mutation');
    finally
      LChangedCatalog.Free;
    end;
    LRefused := False;
    LObserver.RefuseOnce := True;
    try
      TrySelectNyxResourceEditor(LForm.Node, ADocument.Resources, LSelection,
        nil, nil, {$ifdef PAS2JS}@{$endif}LObserver.Synchronize);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LObserver.Calls = 3) and (TNyxCodec.Encode(LShell) = LBefore),
      'target failure restores the exact proposal before rollback synchronization');
    Check(TNyxCodec.Encode(ADocument) = LDocumentBefore,
      'selection operations leave accepted resources and bindings exact');
  finally
    LObserver.Free;
    LForm := nil;
    LParent := nil;
    LShell.Free;
  end;
end;

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
