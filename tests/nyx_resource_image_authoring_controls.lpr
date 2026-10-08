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


program nyx_resource_image_authoring_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.bytes, nyx.data, nyx.images,
  nyx.image.fixtures, nyx.resources, nyx.resources.editor, nyx.resources.import,
  nyx.binding.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.studio.presentation, nyx.studio.projects, nyx.generated.view
  {$ifdef PAS2JS}, Web, nyx.test.image.studio.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, Graphics, IntfGraphics,
    FPWritePNG, nyx.studio.lcl, nyx.studio.sourcejobs, nyx.resources.import.lcl{$endif};

const
  CEditor = 'studio-resource-editor';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  {$ifndef PAS2JS}
  WriteLn('Image resource check / ', GChecks + 1, ' / ', AReason);
  Flush(Output);
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ Build detached malformed/migration packets without changing the captured
  baseline. Omitted keys retain every other scalar and exact ordered value. }
function ReplaceField(const AValue: TNyxDataValue; const AName: TNyxText;
  const AReplacement: TNyxDataValue; AOmit: Boolean = False): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
begin
  SetLength(LFields, AValue.Count);
  LCount := 0;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    if AValue.Key(LIndex) = AName then
    begin

      if AOmit then
      begin
        Continue;
      end;
      LFields[LCount] := NyxField(AName, AReplacement);
    end
    else
    begin
      LFields[LCount] := NyxField(AValue.Key(LIndex), AValue.Field(AValue.Key(LIndex)));
    end;
    Inc(LCount);
  end;
  SetLength(LFields, LCount);
  Result := NyxObject(LFields);
end;

procedure Shared;
var
  LSeed: TNyxDocument;
  LForm: INyxCard;
  LFresh: INyxCard;
  LChange: TNyxResourceEditorChange;
  LDraft: TNyxResourceEditorDraft;
  LCopy: TNyxResourceEditorDraft;
  LData: TNyxDataValue;
  LLegacy: TNyxDataValue;
  LPacket: TNyxDataValue;
  LValues: array of TNyxDataValue;
  LPreference: TNyxStudioPresentation;
  LLoaded: TNyxStudioPresentation;
  LIndex: Integer;
  LRefused: Boolean;
  LBefore: TNyxText;
begin
  LSeed := BuildNyxDocument;
  try
    LBefore := TNyxCodec.Encode(LSeed);
    LForm := NewNyxResourceEditor(CEditor, LSeed.Resources, NyxNewResourceSelection,
      LSeed.Find('hero-image'), LSeed.Find('hero-image'));
    LForm.Node.Find(NyxResourceEditorFieldID(CEditor, refName)).Configure.Value('cover').Done;
    LForm.Node.Find(NyxResourceEditorFieldID(CEditor, refKind)).Configure.Value('Image').Done;
    ProposeNyxResourceEditor(LForm.Node, NyxImageResource(NyxEmbeddedImage(nimPNG, ImagePNG)));
    LForm.Node.Find(NyxResourceEditorFieldID(CEditor, refBind)).Configure.Value(True).Done;
    RefreshNyxResourceEditor(LForm.Node, True);
    Check(ReadNyxResourceEditorImageLocale(LForm.Node) = reilSelected, 'new forms preserve pinned default');
    Check(CaptureNyxResourceEditor(LForm.Node.Find(NyxResourceEditorActionID(CEditor, reaApply)),
      LForm.Node, LChange) and LChange.Binding.ResourceImage.Localized and
      not LChange.Binding.ResourceImage.Locale.Defined, 'default variant is an explicit typed pin');
    SetNyxResourceEditorImageLocale(LForm.Node, reilRuntime);
    Check(CaptureNyxResourceEditor(LForm.Node.Find(NyxResourceEditorActionID(CEditor, reaApply)),
      LForm.Node, LChange), 'runtime locale choice captures one copied command');
    LChange := TNyxResourceEditorChange.FromData(LChange.ToData);
    Check(not LChange.Binding.ResourceImage.Localized and
      (LChange.Binding.ResourceImage.Reference.Name = 'cover'), 'source-worker boundary retains runtime inheritance');
    LForm.Node.Find(NyxResourceEditorFieldID(CEditor, refContent)).Configure.Value('unfinished 🌙').Done;
    LDraft.Capture(CEditor, LForm.Node);
    LData := LDraft.ToData;
    Check((LData.Field('version').AsInteger = 2) and
      (LData.Field('values').Count = 19), 'current draft explicitly versions its nineteen fields');
    LCopy := TNyxResourceEditorDraft.FromData(LData);
    LFresh := NewNyxResourceEditor(CEditor, LSeed.Resources, NyxNewResourceSelection,
      LSeed.Find('hero-image'), LSeed.Find('hero-image'));
    Check(LCopy.Restore(LFresh.Node) and
      (ReadNyxResourceEditorImageLocale(LFresh.Node) = reilRuntime) and
      (LFresh.Node.Find(NyxResourceEditorFieldID(CEditor, refContent)).Prop('value') =
      TNyxText('unfinished 🌙')),
      'parked draft restores exact unfinished bytes and locale intent');
    SetLength(LValues, 18);
    for LIndex := 0 to High(LValues) do
    begin
      LValues[LIndex] := LData.Field('values').Item(LIndex);
    end;
    LLegacy := ReplaceField(ReplaceField(LData, 'version', NyxNull, True),
      'values', NyxArray(LValues));
    LCopy := TNyxResourceEditorDraft.FromData(LLegacy);
    Check(LCopy.Restore(LFresh.Node) and
      (ReadNyxResourceEditorImageLocale(LFresh.Node) = reilSelected), 'legacy draft migrates only to historical pinning');
    LRefused := False;
    try
      LCopy := TNyxResourceEditorDraft.FromData(ReplaceField(LData, 'values', NyxArray(LValues)));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'current draft refuses missing explicit locale choice');
    SetLength(LValues, 19);
    for LIndex := 0 to High(LValues) do
    begin
      LValues[LIndex] := LData.Field('values').Item(LIndex);
    end;
    LValues[18] := NyxData('Invented locale policy');
    LRefused := False;
    try
      LCopy := TNyxResourceEditorDraft.FromData(ReplaceField(LData, 'values', NyxArray(LValues)));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'unknown choice refuses before any restored field is written');
    LPreference := DefaultNyxStudioPresentation;
    LPreference.ResourceDraft := LDraft;
    LLoaded := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LPreference));
    Check(LLoaded.ResourceDraft.Restore(LFresh.Node) and
      (ReadNyxResourceEditorImageLocale(LFresh.Node) = reilRuntime), 'current presentation retains explicit pending intent');
    LPacket := TNyxDataValue.ParseJSON(EncodeNyxStudioPresentation(LPreference));
    LPacket := ReplaceField(ReplaceField(LPacket, 'version', NyxData(9)), 'resourceDraft', LLegacy);
    LLoaded := DecodeNyxStudioPresentation(LPacket.ToJSON);
    Check(LLoaded.ResourceDraft.Restore(LFresh.Node) and
      (ReadNyxResourceEditorImageLocale(LFresh.Node) = reilSelected), 'version-nine preferences migrate legacy draft exactly');
    LRefused := False;
    try
      LLoaded := DecodeNyxStudioPresentation(ReplaceField(LPacket, 'version', NyxData(10)).ToJSON);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'current presentation refuses disguised legacy draft');
    Check(TNyxCodec.Encode(LSeed) = LBefore, 'all proposal and migration checks preserve semantic seed');
  finally
    LFresh := nil;
    LForm := nil;
    LSeed.Free;
  end;
end;

{$ifndef PAS2JS}
type
  TControlAccess = class(TControl);
  { Only the OS chooser is substituted. Admission reads the actual file and the
    mounted ordinary controller receives the original asynchronous callback. }
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
  LRuntime: TNyxText;
  LPinned: TNyxText;
  LBinding: TNyxBindingSpec;
  LPath: String;
  LStream: TFileStream;
  LBytes: TNyxBytes;

  procedure Ready;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize(0);

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Resource image source command did not retire: ' + LStudio.Status);
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
    LInput: TCustomEdit;
  begin
    LInput := TCustomEdit(LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, AField)));
    Check(LInput <> nil, 'ordinary resource text input exists');
    LInput.Text := AValue;
    Ready;
  end;

  procedure Choice(AField: TNyxResourceEditorField; const AValue: TNyxText);
  var
    LInput: TComboBox;
  begin
    LInput := TComboBox(LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, AField)));
    Check(LInput <> nil, 'ordinary resource choice exists');
    LInput.ItemIndex := LInput.Items.IndexOf(AValue);
    Check(LInput.ItemIndex >= 0, 'visible choice: ' + AValue);
    LInput.OnChange(LInput);
    Ready;
  end;

  procedure Bind;
  var
    LInput: TCheckBox;
  begin
    LInput := TCheckBox(LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, refBind)));
    Check(LInput <> nil, 'ordinary binding checkbox exists');
    LInput.Checked := True;
    LInput.OnChange(LInput);
    Ready;
  end;

  procedure Capture;
  var
    LBitmap: TBitmap;
    LImage: TLazIntfImage;
    LInput: TWinControl;
    LContainer: TControl;
    LPreview: TControl;
    LPoint: TPoint;
  begin
    LInput := TWinControl(LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, refImageLocale)));
    LContainer := LStudio.ShellView.ControlFor('studio-left');
    Check((LInput <> nil) and (LContainer is TScrollBox), 'native resource selector has its ordinary scrolling host');
    LPreview := LStudio.ShellView.ControlFor(CEditor+'-image-preview');
    Check(LPreview <> nil, 'native resource preview is mounted');
    TScrollBox(LContainer).ScrollInView(LPreview);
    LPoint := LInput.ClientToParent(Point(0, 0), TWinControl(LContainer));
    Check((LPoint.Y >= 0) and (LPoint.Y + LInput.Height <= LContainer.ClientHeight),
      'native locale choice is inside its viewport before capture');
    Ready;
    LWindow.Repaint;
    LBitmap := TBitmap.Create;
    LImage := nil;
    try
      LBitmap.SetSize(LWindow.ClientWidth, LWindow.ClientHeight);
      LWindow.PaintTo(LBitmap.Canvas, 0, 0);
      LImage := LBitmap.CreateIntfImage;
      LImage.SaveToFile(ParamStr(2));
    finally
      LImage.Free;
      LBitmap.Free;
    end;
  end;

begin
  LWindow := TForm.CreateNew(nil);
  LStudio := nil;
  LSeed := BuildNyxDocument;
  GPicker := TFilePicker.Create;
  GPickerLease := GPicker;
  try
    LPath := ExtractFilePath(ParamStr(1)) + 'cover.dat';
    { Use a different encoded source from the original PNG hero. The live
      consumer assertion below detects a missing binding/renderer publication. }
    LBytes := NyxEmbeddedImage(nimJPEG, ImageJPEG).Bytes;
    LStream := TFileStream.Create(LPath, fmCreate);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    LWindow.SetBounds(40, 40, 1240, 820);
    LWindow.Show;
    LStudio := TTestStudio.Create(LWindow, '');
    LPair := NyxProjectPair(TNyxCodec.Encode(LSeed), TNyxCodegen.Generate(LSeed));
    LStudio.LoadProject(LPair);
    LStudio.Session.Select('hero-image');
    LStudio.Run;
    Ready;
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Click('action-resources-toggle');
    TextField(refName, 'cover');
    Choice(refKind, 'Image');
    Click(NyxResourceEditorActionID(CEditor, reaImport));
    GPicker.Deliver(LPath);
    Ready;
    Check(LStudio.ShellView.Root.Find(CEditor+'-image-preview').Prop('src') =
      NyxEmbeddedImage(nimJPEG, ImageJPEG).ToWire, 'real file admission paints the resource preview');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'import is a copied proposal only');
    Bind;
    Choice(refImageLocale, 'Follow application locale');
    Click('action-code');
    Check(ReadNyxResourceEditorImageLocale(LStudio.ShellView.Root.Find(CEditor)) = reilRuntime,
      'source chrome rebuild retains unsubmitted locale intent');
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'ordinary source processor admits definition plus image binding');
    Check(LStudio.Session.Document.Find('hero-image').FindBinding(bpImage, LBinding) and
      not LBinding.ResourceImage.Localized, 'runtime binding accepted on specialized image');
    Check(LStudio.CanvasView.Root.Find('hero-image').Prop('src') =
      NyxEmbeddedImage(nimJPEG, ImageJPEG).ToWire, 'bound native canvas replaces its original PNG source');
    LRuntime := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Check(Pos('.Image(NyxResourceImage(', LStudio.Session.Source) > 0, 'crafted source uses typed image binding');
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'one Undo restores original complete pair');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LRuntime, 'Redo restores exact runtime-bound pair');
    Click(CEditor+'-entry-0');
    Check(ReadNyxResourceEditorImageLocale(LStudio.ShellView.Root.Find(CEditor)) = reilRuntime,
      'reopening bound resource preserves existing runtime intent');
    Bind;
    Choice(refImageLocale, 'Use this variant');
    Click('action-code');
    Check(ReadNyxResourceEditorImageLocale(LStudio.ShellView.Root.Find(CEditor)) = reilSelected,
      'second chrome rebuild preserves explicit pin proposal');
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'explicit default pin passes ordinary source admission');
    Check(LStudio.Session.Document.Find('hero-image').FindBinding(bpImage, LBinding) and
      LBinding.ResourceImage.Localized and not LBinding.ResourceImage.Locale.Defined,
      'accepted default pin remains distinct from runtime inheritance');
    LPinned := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LRuntime, 'pin Undo restores runtime intent and exact source');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LPinned, 'pin Redo restores exact explicit default');
    Click(CEditor+'-entry-0');
    Bind;
    Capture;
    LBytes := NyxEncodeUTF8(LStudio.Session.Source);
    LStream := TFileStream.Create(ParamStr(1), fmCreate);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    Check(LStudio.Session.Document.Resources.Count = 1, 'both authoring operations retain one packed resource');
  finally
    LStudio.Free;
    Check(LWindow.ControlCount = 0, 'explicit native controller retirement removes owned views');
    LWindow.Free;
    GPickerLease := nil;
    GPicker := nil;
    LSeed.Free;
  end;
end;
{$endif}

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    Shared;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-resource-image-form-checks', IntToStr(GChecks));
    RunNyxResourceImageStudioQualification;
    {$else}
    NativeStudio;
    WriteLn('PASS / ordinary image resource authoring / ', GChecks, ' checks');
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
