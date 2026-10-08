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


program nyx_resource_authoring_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.bytes, nyx.data, nyx.images,
  nyx.resources, nyx.resources.editor, nyx.resources.rows.editor,
  nyx.resources.rows, nyx.collections, nyx.collections.registry,
  nyx.resources.import, nyx.resource.sources,
  nyx.model, nyx.controls, nyx.codec, nyx.codegen, nyx.schema, nyx.events,
  nyx.binding.types, nyx.binding, nyx.studio.session, nyx.studio.commands,
  nyx.studio.projects, nyx.studio.sourcejobs, nyx.studio.presentation
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.resources.import.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Graphics, IntfGraphics,
    FPWritePNG, LazFileUtils, nyx.studio.lcl, nyx.resources.import.lcl{$endif};

const
  CEditor = 'studio-resource-editor';
  CData = '{"literal.dot":"Your resource workshop 🌙","prompt":"Choose a name","rows":[{"id":"row-one","value":3.125}],"ready":true}';
  { Small Pascal-created PNG retained from the maintained image prerequisite. }
  CPNG = 'iVBORw0KGgoAAAANSUhEUgAAAGQAAAAyEAIAAAB1xzWqAAAACXBIWXMAAAAAAAAAAACdYiYyAAABMElEQVR4nO3OsQ0AIAzAsP7/dOEEtsgSGTxnduf2fbE/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDxwO6T+sr8laFkAAAAABJRU5ErkJggg==';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}
  { Opt-in progress stays beside the real assertion. Flush before checking so
    a native host dialog or stuck callback leaves its precise last boundary
    visible to the caller instead of hiding buffered diagnostic output. }

  if GetEnvironmentVariable('NYX_RESOURCE_TEST_TRACE') = '1' then
  begin
    WriteLn('Resource check / ', GChecks + 1, ' / ', AReason);
    Flush(Output);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create('Resource authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

function Workshop: TNyxDocument;
var
  LDocument: TNyxDocument;
  LPage: INyxColumn;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Resource workshop';
    LPage := NewNyxColumn('resource-home');
    LPage.Configure.Padding(24).Gap(14).Done;
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxLabel('workshop-headline').WithText('Create something wonderful'));
    LPage.Add(NewNyxInput('project-name').Configure.Placeholder('Project name').Done);
    Result := LDocument;
  except
    LDocument.Free;
    raise;
  end;
end;

procedure Put(AForm: TNyxNode; AField: TNyxResourceEditorField; const AValue: TNyxText);
begin

  if AField in [refBind, refFallback] then
  begin
    AForm.Find(NyxResourceEditorFieldID(CEditor, AField)).Configure.Value(AValue = 'true').Done;
  end
  else
  begin
    AForm.Find(NyxResourceEditorFieldID(CEditor, AField)).Configure.Value(AValue).Done;
  end;
end;

procedure Shared;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LForm: INyxCard;
  LFresh: INyxCard;
  LChange: TNyxResourceEditorChange;
  LDraft: TNyxResourceEditorDraft;
  LCopy: TNyxResourceEditorDraft;
  LPreference: TNyxStudioPresentation;
  LLoaded: TNyxStudioPresentation;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LDecoded: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LDefinition: INyxResourceDefinition;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LKind: TNyxResourceKind;
  LRefused: Boolean;
  {$ifndef PAS2JS}
  LOutput: TFileStream;
  LText: TNyxText;
  {$endif}
begin
  LDocument := Workshop;
  LSession := TNyxStudioSession.Create;
  LSchemas := CaptureNyxSchemas;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    for LKind := Low(TNyxResourceKind) to High(TNyxResourceKind) do
    begin
      LForm := NewNyxResourceEditor(CEditor, LDocument.Resources, NyxNewResourceSelection);
      case LKind of
        nrkImage: LDefinition := NyxImageResource(NyxEmbeddedImage(nimPNG, CPNG));
        nrkJSON: LDefinition := NyxJSONResource(CData);
        nrkText: LDefinition := NyxTextResource('Text 🌙' + TNyxText(#0));
        nrkBinary: LDefinition := NyxBinaryResource(NyxDecodeBase64('AAH/'));
      end;
      case LKind of
        nrkImage: Put(LForm.Node, refKind, 'Image');
        nrkJSON: Put(LForm.Node, refKind, 'JSON');
        nrkText: Put(LForm.Node, refKind, 'Text');
        nrkBinary: Put(LForm.Node, refKind, 'Binary');
      end;
      ProposeNyxResourceEditor(LForm.Node, LDefinition);
      Check(ReadNyxResourceEditor(LForm.Node).ToData.ToJSON = LDefinition.ToData.ToJSON,
        'proposal retains exact family bytes: ' + NyxResourceKindName(LKind));
    end;
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'all previews leave accepted defaults alone');
    LForm := NewNyxResourceEditor(CEditor, LDocument.Resources, NyxNewResourceSelection,
      LDocument.Find('workshop-headline'), LDocument.Find('workshop-headline'));
    Put(LForm.Node, refName, 'copy');
    Put(LForm.Node, refKind, 'JSON');
    ProposeNyxResourceEditor(LForm.Node, NyxJSONResource(CData));
    Put(LForm.Node, refTitle, 'Workshop copy 🌙');
    Put(LForm.Node, refDescription, 'Captions and prompts supplied by the project.');
    Put(LForm.Node, refBind, 'true');
    Put(LForm.Node, refTarget, NyxBindingPropertyTitle(bpText));
    Put(LForm.Node, refPath, 'Root["literal.dot"] / text');
    LDraft.Capture(CEditor, LForm.Node);
    LCopy := TNyxResourceEditorDraft.FromData(LDraft.ToData);
    LPreference := DefaultNyxStudioPresentation;
    LPreference.ResourcesVisible := True;
    LPreference.ResourceDraft := LCopy;
    LLoaded := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LPreference));
    Check(LLoaded.ResourcesVisible and
      (LLoaded.ResourceDraft.ToData.ToJSON = LCopy.ToData.ToJSON), 'preferences retain exact copied resource draft');
    LFresh := NewNyxResourceEditor(CEditor, LDocument.Resources, NyxNewResourceSelection,
      LDocument.Find('workshop-headline'), LDocument.Find('workshop-headline'));
    Check(LCopy.Restore(LFresh.Node), 'independent chrome restores discovered structural choices');
    Check(CaptureNyxResourceEditor(LFresh.Node.Find(NyxResourceEditorActionID(CEditor, reaApply)),
      LFresh.Node, LChange), 'Apply captures a typed file and scalar binding');
    Check(LChange.Binding.ResourceValue.Path.ToData.ToJSON =
      NyxResourcePath.Field('literal.dot').ToData.ToJSON, 'literal dotted key remains structural');
    Check(TNyxResourceEditorChange.FromData(LChange.ToData).ToData.ToJSON = LChange.ToData.ToJSON,
      'immutable complete processor descriptor round-trips exactly');
    LSession.Load(LBefore);
    LSession.Select('workshop-headline');
    Check(CaptureNyxStudioAuthoring(LSession, LFresh.Node.Find(
      NyxResourceEditorActionID(CEditor, reaApply)), ntClick, LFresh.Node,
      Default(TNyxStudioPendingDesign), LEdit) = sacEdit, 'ordinary shared controller captures resource command');
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LDecoded := ReadNyxStudioDesignRequest(LRequest.ToData);
    Check(LRequest.SameRequest(LDecoded), 'version 15 isolated request retains complete proposal');
    LPrepared := PrepareNyxStudioDesign(LDecoded, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'isolated preparation validates resource and consumer');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied, 'one paired publication accepts');
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(Pos('NyxResourceValue', LSession.Source) > 0, 'source uses fluent typed resource references');
    LSession.Undo;
    Check(LSession.Save = LBefore, 'one Undo restores original document');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'one Redo restores exact resource/source pair');
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(LPrepared.Diagnostic.Defined, 'stale catalog refuses an old mounted Apply');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscRejected,
      'stale preparation cannot change paired history');
    LFresh := NewNyxResourceEditor(CEditor, LSession.Document.Resources,
      NyxResourceSelection(NyxResourceRef('copy'), NyxDefaultLocale));
    Check(not LCopy.Restore(LFresh.Node), 'changed accepted catalog refuses copied draft restoration');
    Check(CaptureNyxResourceEditor(LFresh.Node.Find(NyxResourceEditorActionID(CEditor, reaRemove)),
      LFresh.Node, LChange), 'Remove captures exact opened variant');
    LEdit.Resource := LChange;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(LPrepared.Diagnostic.Defined, 'removing a referenced file refuses atomically');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscRejected, 'refused removal preserves consumers');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'failed removal retains complete pair');
    LSession.SetSourceDraft(LSession.Source + TNyxText(#10) + TNyxText('// Pending application work.'));
    LRefused := False;
    try
      LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LSession.ProjectSnapshot.Pending,
      'resource Apply refuses a pending application draft without discarding it');
    LSession.DiscardSourceDraft;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter,
      'discarding the test draft restores the complete accepted pair');
    Put(LFresh.Node, refSource, 'Hosted URL');
    Put(LFresh.Node, refURL, 'https://example.com/copy.json');
    Put(LFresh.Node, refCache, 'Persistent');
    Put(LFresh.Node, refFresh, '600');
    Put(LFresh.Node, refStale, '90');
    Put(LFresh.Node, refServer, 'Override in private Nyx cache');
    ProposeNyxResourceEditor(LFresh.Node, NyxJSONResource(CData));
    LDefinition := ReadNyxResourceEditor(LFresh.Node);
    Check((LDefinition.Source.Kind = rskHosted) and
      (LDefinition.Source.CachePolicy.Server = rcspOverride) and
      (LDefinition.Source.CachePolicy.FreshSeconds = 600) and
      (LDefinition.FallbackDefinition.Data.ToJSON = TNyxDataValue.ParseJSON(CData).ToJSON),
      'hosted import retains URL, tunable override and explicit fallback');
    Put(LFresh.Node, refFresh, 'partial');
    LRefused := False;
    try
      ReadNyxResourceEditor(LFresh.Node);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (EncodeNyxProject(LSession.ProjectSnapshot) = LAfter),
      'invalid whole seconds refuse without accepted edits');
    LDraft.Capture(CEditor, LFresh.Node);
    LCopy := TNyxResourceEditorDraft.FromData(LDraft.ToData);
    LFresh := NewNyxResourceEditor(CEditor, LSession.Document.Resources,
      NyxResourceSelection(NyxResourceRef('copy'), NyxDefaultLocale));
    Check(LCopy.Restore(LFresh.Node) and
      (LFresh.Node.Find(NyxResourceEditorFieldID(CEditor, refFresh)).Prop('value') = 'partial'),
      'unsubmitted invalid numeric input survives chrome without admission');
    {$ifndef PAS2JS}
    if ParamCount > 0 then
    begin
      LText := LSession.Source;
      LOutput := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LOutput.WriteBuffer(Pointer(LText)^, Length(LText));
      finally
        LOutput.Free;
      end;
    end;
    {$endif}
  finally
    LPrepared := nil;
    LSchemas := nil;
    LDefinition := nil;
    LForm := nil;
    LFresh := nil;
    LSession.Free;
    LDocument.Free;
  end;
end;


procedure RowDrafts;
const
  CRowEditor = 'studio-resource-rows';
var
  LDocument: TNyxDocument;
  LForm: INyxCard;
  LFresh: INyxCard;
  LDraft: TNyxResourceRowsDraft;
  LPresentation: TNyxStudioPresentation;
  LCopy: TNyxStudioPresentation;
  LRows: TNyxResourceRows;
  LChange: TNyxResourceEditorChange;
  LBefore: TNyxText;
  LRefused: Boolean;

  procedure PutRow(AField: TNyxResourceRowsField; const AValue: TNyxText);
  begin
    LForm.Node.Find(NyxResourceRowsFieldID(CRowEditor, AField)).Configure.Value(AValue).Done;
  end;

  procedure Click(AAction: TNyxResourceRowsAction);
  begin
    Check(HandleNyxResourceRowsEditor(LForm.Node.Find(
      NyxResourceRowsActionID(CRowEditor, AAction)), LForm.Node), 'shared form handles copied row presentation');
  end;

begin
  LDocument := Workshop;
  LForm := nil;
  LFresh := nil;
  try
    LDocument.Resources.Define(NyxResourceRef('copy'), NyxJSONResource(CData));
    LRows := NyxResourceRows(NyxResourceRef('copy')).Field('rows')
      .Identity(NyxResourcePath.Field('id')).Number(NyxNumberField('amount'), NyxResourcePath.Field('value'));
    LDocument.ResourceCollections.Define(NyxCollection('saved'), LRows);
    LBefore := TNyxCodec.Encode(LDocument);
    LForm := NewNyxResourceRowsEditor(CRowEditor, LDocument.Resources, LDocument.Collections);
    Check(CaptureNyxResourceRowsEditor(LForm.Node.Find(
      NyxResourceRowsActionID(CRowEditor, raApply)), LForm.Node, LChange),
      'opening saved relationship captures the shared paired command');
    Check((LChange.CollectionName = 'saved') and (LChange.RowsData.ToJSON = LRows.ToData.ToJSON),
      'saved source retains exact structural paths and numeric family');
    Check(TNyxResourceEditorChange.FromData(LChange.ToData).ToData.ToJSON = LChange.ToData.ToJSON,
      'saved row command crosses the ordinary isolated worker codec');
    PutRow(rrFieldName, 'partially authored 🌙');
    Click(raEditField);
    Check(LForm.Node.Find(NyxResourceRowsFieldID(CRowEditor, rrFieldType)).Prop('value') = 'Number',
      'editing a saved field retains its explicit Pascal numeric family');
    PutRow(rrName, 'draft name 🌙');
    LDraft.Capture(CRowEditor, LForm.Node);
    LPresentation := DefaultNyxStudioPresentation;
    LPresentation.ResourceRowsDraft := LDraft;
    LCopy := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LPresentation));
    LFresh := NewNyxResourceRowsEditor(CRowEditor, LDocument.Resources, LDocument.Collections);
    Check(LCopy.ResourceRowsDraft.Restore(LFresh.Node) and
      (LFresh.Node.Find(NyxResourceRowsFieldID(CRowEditor, rrName)).Prop('value') = TNyxText('draft name 🌙')),
      'per-project presentation preserves an unsubmitted Unicode row name');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'draft/discovery leaves the entire design unchanged');
    Click(raDiscover);
    PutRow(rrDataset, NyxResourcePath.Field('rows').ToData.ToJSON);
    Click(raInspect);
    PutRow(rrIdentity, NyxResourcePath.Field('id').ToData.ToJSON);
    PutRow(rrFieldName, 'extra');
    PutRow(rrFieldType, 'Text');
    PutRow(rrFieldPath, NyxResourcePath.Field('id').ToData.ToJSON);
    Click(raSetField);
    Click(raRemoveField);
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'mapping edits/removal remain presentation until Apply');
    LRefused := False;
    PutRow(rrFieldType, 'Imaginary');
    try
      Click(raSetField);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore),
      'unknown field family refuses without changing the saved design');
    LDocument.Resources.Define(NyxResourceRef('other'), NyxTextResource('Changed context'));
    LFresh := NewNyxResourceRowsEditor(CRowEditor, LDocument.Resources, LDocument.Collections);
    Check(not LDraft.Restore(LFresh.Node), 'changed catalog refuses stale draft restoration before writing fields');
  finally
    LFresh := nil;
    LForm := nil;
    LDocument.Free;
  end;
end;

{$ifndef PAS2JS}
type
  TControlAccess = class(TControl);
  { Only the OS dialog is replaced. The real UTF-8 file adapter and ordinary
    native Studio callback, fields, isolated processor and rendering run. }
  TFilePicker = class(TInterfacedObject, INyxResourcePicker)
  private
    FReply: TNyxResourcePickReply;
    FKind: TNyxResourceKind;
  public
    procedure Pick(AKind: TNyxResourceKind; AReply: TNyxResourcePickReply);
    procedure Cancel;
    procedure Deliver(const APath: TNyxText);
  end;
  TTestStudio = class(TNyxNativeStudio)
  protected
    function CreateResourcePicker: INyxResourcePicker; override;
  end;

var
  GPicker: TFilePicker;
  GPickerLease: INyxResourcePicker;

procedure TFilePicker.Pick(AKind: TNyxResourceKind; AReply: TNyxResourcePickReply);
begin
  FKind := AKind;
  FReply := AReply;
end;

procedure TFilePicker.Cancel;
begin
  FReply := nil;
end;

procedure TFilePicker.Deliver(const APath: TNyxText);
var
  LReply: TNyxResourcePickReply;
  LDefinition: INyxResourceDefinition;
begin
  LDefinition := ReadNyxResourceFile(APath, FKind);
  LReply := FReply;
  FReply := nil;

  if Assigned(LReply) then
  begin
    LReply(rpsSelected, LDefinition, '');
  end;
end;

function TTestStudio.CreateResourcePicker: INyxResourcePicker;
begin
  Result := GPickerLease;
end;

procedure NativeStudio;
var
  LWindow: TForm;
  LStudio: TTestStudio;
  LSeed: TNyxDocument;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LPath: TNyxText;
  LStream: THandleStream;
  LHandle: THandle;
  LBytes: TNyxBytes;
  LBox: TCheckBox;
  LBinding: TNyxBindingSpec;

  procedure Ready;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize(0);

      if GetTickCount64 - LStarted > 15000 then
      begin
        raise Exception.Create('Resource Studio did not retire: ' + LStudio.Status);
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceCommands.Busy;
  end;

  procedure Click(const AID: TNyxText);
  var
    LControl: TControl;
  begin
    LControl := LStudio.ShellView.ControlFor(AID);
    Check(LControl <> nil, 'ordinary mounted command: ' + AID);
    TControlAccess(LControl).Click;
    Ready;
  end;

  procedure TextField(AField: TNyxResourceEditorField; const AValue: TNyxText);
  var
    LField: TCustomEdit;
  begin
    LField := TCustomEdit(LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, AField)));
    Check(LField <> nil, 'ordinary text field exists');
    LField.Text := AValue;
    Ready;
  end;

  procedure Choice(AField: TNyxResourceEditorField; const AValue: TNyxText);
  var
    LChoice: TComboBox;
  begin
    LChoice := TComboBox(LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, AField)));
    LChoice.ItemIndex := LChoice.Items.IndexOf(AValue);
    Check(LChoice.ItemIndex >= 0, 'ordinary choice exists: ' + AValue);
    LChoice.OnChange(LChoice);
    Ready;
  end;

  procedure RowText(AField: TNyxResourceRowsField; const AValue: TNyxText);
  var
    LEdit: TCustomEdit;
  begin
    LEdit := TCustomEdit(LStudio.ShellView.InputFor(
      NyxResourceRowsFieldID('studio-resource-rows', AField)));
    Check(LEdit <> nil, 'ordinary row-authoring text control exists');
    LEdit.Text := AValue;
    Ready;
  end;

  procedure RowChoice(AField: TNyxResourceRowsField; const AValue: TNyxText);
  var
    LChoice: TComboBox;
  begin
    LChoice := TComboBox(LStudio.ShellView.InputFor(
      NyxResourceRowsFieldID('studio-resource-rows', AField)));
    Check(LChoice <> nil, 'ordinary row-authoring choice control exists');
    LChoice.ItemIndex := LChoice.Items.IndexOf(AValue);
    Check(LChoice.ItemIndex >= 0, 'row choice exists: ' + AValue);
    LChoice.OnChange(LChoice);
    Ready;
  end;

  procedure Capture(const APath: String);
  var
    LBitmap: TBitmap;
    LImage: TLazIntfImage;
  begin
    Ready;
    LWindow.Repaint;
    LBitmap := TBitmap.Create;
    LImage := nil;
    try
      LBitmap.SetSize(LWindow.ClientWidth, LWindow.ClientHeight);
      LWindow.PaintTo(LBitmap.Canvas, 0, 0);
      LImage := LBitmap.CreateIntfImage;
      LImage.SaveToFile(APath);
    finally
      LImage.Free;
      LBitmap.Free;
    end;
  end;

begin
  LWindow := TForm.CreateNew(nil);
  LStudio := nil;
  LSeed := Workshop;
  GPicker := TFilePicker.Create;
  GPickerLease := GPicker;
  try
    LPath := TNyxText(ExtractFilePath(ParamStr(1))) + TNyxText('copy-🌙.dat');
    LBytes := NyxEncodeUTF8(CData);
    LHandle := FileCreateUTF8(LPath);
    Check(LHandle <> THandle(-1), 'UTF-8 arbitrary-extension resource file opens');
    LStream := THandleStream.Create(LHandle);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
      FileClose(LHandle);
    end;
    Check(ReadNyxResourceFile(LPath, nrkJSON).Data.ToJSON =
      TNyxDataValue.ParseJSON(CData).ToJSON, 'native import retains exact JSON and Unicode filename');
    LWindow.SetBounds(40, 40, 1240, 820);
    LWindow.Show;
    LStudio := TTestStudio.Create(LWindow, '');
    LPair := NyxProjectPair(TNyxCodec.Encode(LSeed), TNyxCodegen.Generate(LSeed));
    LStudio.LoadProject(LPair);
    LStudio.Session.Select('workshop-headline');
    LStudio.Run;
    Ready;
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Click('action-resources-toggle');
    TextField(refName, 'copy');
    Choice(refKind, 'JSON');
    Click(NyxResourceEditorActionID(CEditor, reaImport));
    GPicker.Deliver(LPath);
    Ready;
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'real imported preview changes no accepted project');
    TextField(refTitle, 'Workshop copy 🌙');
    LBox := TCheckBox(LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, refBind)));
    LBox.Checked := True;
    LBox.OnChange(LBox);
    Ready;
    Choice(refTarget, NyxBindingPropertyTitle(bpText));
    Choice(refPath, 'Root["literal.dot"] / text');
    Click('action-code');
    Check(LStudio.ShellView.Root.Find(NyxResourceEditorFieldID(CEditor, refTitle)).Prop('value') =
      TNyxText('Workshop copy 🌙'), 'real chrome rebuild retains imported proposal');
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'ordinary isolated resource Apply: ' + LStudio.Status);
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Check(LStudio.Session.Document.Resources.Definition(NyxResourceRef('copy'), NyxDefaultLocale).Title =
      TNyxText('Workshop copy 🌙'), 'actual accepted catalog retains creator metadata');
    Check(LStudio.Session.Document.Find('workshop-headline').FindBinding(bpText, LBinding),
      'actual accepted caption has its authored resource binding');
    Check(TLabel(LStudio.CanvasView.ControlFor('workshop-headline')).Caption =
      TNyxText('Your resource workshop 🌙'), 'ordinary existing native caption reads accepted resource: ' +
      NyxData(TNyxText(TLabel(LStudio.CanvasView.ControlFor('workshop-headline')).Caption)).ToJSON);
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'ordinary one Undo restores original pair');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter, 'ordinary one Redo restores exact pair');
    LStudio.Session.Select('project-name');
    LStudio.RequestRefresh;
    Ready;
    Click(CEditor + TNyxText('-entry-0'));
    LBox := TCheckBox(LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, refBind)));
    LBox.Checked := True;
    LBox.OnChange(LBox);
    Ready;
    Choice(refTarget, NyxBindingPropertyTitle(bpPlaceholder));
    Choice(refPath, 'Root["prompt"] / text');
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'existing file can bind a second control');
    Check(TCustomEdit(LStudio.CanvasView.InputFor('project-name')).TextHint = TNyxText('Choose a name'),
      'ordinary input prompt reads selected JSON field');
    LBefore := LAfter;
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'prompt binding has one paired Undo');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter, 'prompt binding has one paired Redo');
    RowText(rrName, 'workshop-rows');
    RowChoice(rrResource, NyxData('copy').ToJSON);
    Click(NyxResourceRowsActionID('studio-resource-rows', raDiscover));
    RowChoice(rrDataset, NyxResourcePath.Field('rows').ToData.ToJSON);
    Click(NyxResourceRowsActionID('studio-resource-rows', raInspect));
    RowChoice(rrIdentity, NyxResourcePath.Field('id').ToData.ToJSON);
    RowText(rrFieldName, 'amount');
    RowChoice(rrFieldType, 'Number');
    RowChoice(rrFieldPath, NyxResourcePath.Field('value').ToData.ToJSON);
    Click(NyxResourceRowsActionID('studio-resource-rows', raSetField));
    Click('action-code');
    Check(LStudio.ShellView.Root.Find(NyxResourceRowsFieldID('studio-resource-rows', rrName))
      .Prop('value') = 'workshop-rows', 'actual chrome rebuild retains unsubmitted row proposal');
    LBefore := LAfter;
    Click(NyxResourceRowsActionID('studio-resource-rows', raApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'ordinary row Apply uses the isolated source processor');
    Check(LStudio.Session.Document.ResourceCollections.HasSource(NyxCollection('workshop-rows')),
      'ordinary Studio stores the typed relationship beside its empty schema');
    Check(Pos('ResourceCollections.Define', LStudio.Session.Source) > 0,
      'ordinary Studio crafts typed row authoring beside the design');
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'row Apply is one paired Undo');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter, 'row Apply is one exact paired Redo');
    Click(NyxResourceRowsActionID('studio-resource-rows', raLoad));
    Click(NyxResourceRowsActionID('studio-resource-rows', raDetach));
    Check(not LStudio.Session.Document.ResourceCollections.HasSource(NyxCollection('workshop-rows')) and
      (LStudio.Session.Document.Collections.Snapshot(NyxCollection('workshop-rows')).Count = 1),
      'ordinary detach keeps authored data as static rows');
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'one paired Undo restores the exact relationship after detach');
    TScrollBox(LStudio.ShellView.ControlFor('studio-left')).ScrollInView(
      LStudio.ShellView.InputFor(NyxResourceRowsFieldID('studio-resource-rows', rrFieldType)));

    if ParamCount > 1 then
    begin
      Capture(ChangeFileExt(ParamStr(2), '-rows.png'));
    end;
    TScrollBox(LStudio.ShellView.ControlFor('studio-left')).ScrollInView(
      LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, refContent)));

    if ParamCount > 1 then
    begin
      Capture(ParamStr(2));
    end;
    LWindow.SetBounds(40, 40, 390, 700);
    Ready;
    Click('action-panel-project');
    Check(LStudio.ShellView.Root.Find(CEditor) <> nil, 'native compact Project uses same reusable form');
    TScrollBox(LStudio.ShellView.ControlFor('studio-left')).ScrollInView(
      LStudio.ShellView.InputFor(NyxResourceRowsFieldID('studio-resource-rows', rrFieldType)));

    if ParamCount > 2 then
    begin
      Capture(ChangeFileExt(ParamStr(3), '-rows.png'));
    end;
    TScrollBox(LStudio.ShellView.ControlFor('studio-left')).ScrollInView(
      LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, refContent)));

    if ParamCount > 2 then
    begin
      Capture(ParamStr(3));
    end;
    Click(NyxResourceEditorActionID(CEditor, reaNew));
    TextField(refName, 'late');
    Choice(refKind, 'JSON');
    Click(NyxResourceEditorActionID(CEditor, reaImport));
    TextField(refTitle, 'Changed while chooser was open');
    GPicker.Deliver(LPath);
    Ready;
    Check(Pos('proposal changed', LStudio.Status) > 0, 'late file reply refuses changed raw proposal');
    Check(TLabel(LStudio.ShellView.ControlFor('studio-status')).Caption = LStudio.Status,
      'ordinary native footer visibly reports import refusal');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter, 'late import retains accepted pair');
    Click(NyxResourceEditorActionID(CEditor, reaImport));
    { Deliberately deliver before a queued repaint. Mounted old context alone
      cannot authorize a file reply for the newly selected control. }
    LStudio.Session.Select('workshop-headline');
    GPicker.Deliver(LPath);
    Ready;
    Check(Pos('selected control changed', LStudio.Status) > 0,
      'late file reply refuses a changed selection before chrome repaint: ' + LStudio.Status);
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'selection-race refusal preserves complete accepted pair');
    Click('action-panel-design');
    Check(Pos('Canvas grips', LStudio.Status) = 0,
      'returning to the compact canvas reconnects guides for current selection');
    Click('action-panel-project');
    Click(NyxResourceEditorActionID(CEditor, reaImport));
    LStudio.LoadProject(LPair);
    Ready;
    GPicker.Deliver(LPath);
    Ready;
    Check(Pos('earlier project', LStudio.Status) > 0, 'late reply refuses replacement project');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LPair),
      'late import cannot edit replacement project');
  finally
    LStudio.Free;
    GPickerLease := nil;
    GPicker := nil;
    LSeed.Free;
    LWindow.Free;
  end;
end;
{$else}
type
  TResourceInputEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;

procedure BrowserControls;
var
  LSeed: TNyxDocument;
  LShell: TNyxDocument;
  LPage: INyxColumn;
  LForm: INyxCard;
  LView: TNyxBrowserRenderer;
  LInput: TJSHTMLTextAreaElement;
  LOptions: TJSObject;
  LPicker: INyxResourcePicker;
begin
  LSeed := Workshop;
  LShell := TNyxDocument.Create;
  LView := TNyxBrowserRenderer.Create;
  try
    LPage := NewNyxColumn('resource-form');
    LShell.AddPage(LPage);
    LForm := NewNyxResourceEditor(CEditor, LSeed.Resources, NyxNewResourceSelection);
    LPage.Add(LForm);
    LView.Render(LShell, LShell.Pages[0], TJSHTMLElement(document.body));
    LInput := TJSHTMLTextAreaElement(LView.InputFor(NyxResourceEditorFieldID(CEditor, refContent)));
    LInput.value := 'Your browser resource workshop';
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LInput.dispatchEvent(TResourceInputEvent.new('input', LOptions));
    LInput.dispatchEvent(TResourceInputEvent.new('change', LOptions));
    Check(ReadNyxResourceEditor(LForm.Node).Text = TNyxText('Your browser resource workshop'),
      'ordinary browser memo feeds typed resource capture');
    LPicker := NewNyxBrowserResourcePicker;
    LPicker.Cancel;
    LPicker := nil;
  finally
    LView.Free;
    LForm := nil;
    LPage := nil;
    LShell.Free;
    LSeed.Free;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    Shared;
    RowDrafts;
    {$ifdef PAS2JS}BrowserControls;{$else}NativeStudio;{$endif}
    WriteLn('PASS / resource authoring / ', GChecks, ' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'passed');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result', 'failed');{$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
