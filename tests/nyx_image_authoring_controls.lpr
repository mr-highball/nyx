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
program nyx_image_authoring_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.data, nyx.images,
  nyx.image.editor, nyx.image.import, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.source, nyx.schema, nyx.events, nyx.image.fixtures,
  nyx.studio.session, nyx.studio.commands, nyx.studio.projects,
  nyx.studio.sourcejobs, nyx.studio.presentation, nyx.generated.view,
  {$ifdef PAS2JS}JS, Web, nyx.render.browser, nyx.image.import.browser;
  {$else}Interfaces, Forms, Controls, StdCtrls, Graphics, IntfGraphics,
    FPWritePNG, nyx.studio.lcl, nyx.image.import.lcl;{$endif}

const
  CEditor = 'inspector-image';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Image authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Shared;
var
  LDocument: TNyxDocument;
  LForm: INyxCard;
  LFresh: INyxCard;
  LOwner: TNyxNode;
  LProjection: TNyxNode;
  LDraft: TNyxImageEditorDraft;
  LCopy: TNyxImageEditorDraft;
  LChange: TNyxImageEditorChange;
  LEdit: TNyxStudioDesignEdit;
  LSession: TNyxStudioSession;
  LRequest: TNyxStudioDesignRequest;
  LDecoded: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LSource: TNyxImageSource;
  LBytes: TNyxImageBytes;
  LRefused: Boolean;
  LPreference: TNyxStudioPresentation;
  LLoaded: TNyxStudioPresentation;
  {$ifndef PAS2JS}
  LOutput: TFileStream;
  LText: TNyxText;
  {$endif}
begin
  LDocument := BuildNyxDocument;
  LSession := nil;
  LProjection := nil;
  LSchemas := CaptureNyxSchemas;
  try
    Check(LDocument.Title = 'Image workshop', 'unchanged English semantic companion');
    LOwner := LDocument.Find('hero-image');
    Check(LOwner <> nil, 'semantic image is present');
    LForm := NewNyxImageEditor(CEditor, LOwner, LOwner);
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := NyxEmbeddedImage(nimJPEG, ImageJPEG);
    LBytes := LSource.Bytes;
    Check(NyxImportedImage(LBytes).ToWire = LSource.ToWire, 'header detection imports exact JPEG bytes');
    LBytes[0] := 0;
    LRefused := False;
    try
      NyxImportedImage(LBytes);
    except
      on ENyxImage do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'corrupt import refuses independently');
    Check(NyxImportedImageBase64(ImagePNG).Format = nimPNG, 'MIME-independent PNG import');
    Check(NyxImportedImageBase64(ImageJPEG).Format = nimJPEG, 'MIME-independent JPEG import');
    LRefused := False;
    try
      LSource := NyxEmbeddedImage(nimPNG, 'invalid Base64');
    except
      on ENyxImage do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSource.ToWire = NyxEmbeddedImage(nimJPEG, ImageJPEG).ToWire),
      'failed typed source assignment retains the previous immutable resource');
    LForm.Node.Find(NyxImageEditorFieldID(CEditor, iefInlineBase64)).Configure.Value(ImagePNG).Done;
    Check(ReadNyxImageEditorInline(LForm.Node).ToWire = NyxEmbeddedImage(nimPNG, ImagePNG).ToWire,
      'canonical pasted PNG becomes a packed typed source');
    LForm.Node.Find(NyxImageEditorFieldID(CEditor, iefInlineBase64)).Configure.Value(LSource.ToWire).Done;
    LRefused := False;
    try
      ReadNyxImageEditorInline(LForm.Node);
    except
      on ENyxImage do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore),
      'pasted data URL with a mismatched format refuses without accepted changes');
    LForm.Node.Find(NyxImageEditorFieldID(CEditor, iefInlineFormat)).Configure.Value('JPEG').Done;
    Check(ReadNyxImageEditorInline(LForm.Node).ToWire = LSource.ToWire,
      'complete pasted JPEG data URL retains exact packed bytes');
    LForm.Node.Find(NyxImageEditorFieldID(CEditor, iefInlineBase64)).Configure.Value(ImageJPEG).Done;
    ProposeNyxImageEditorSource(LForm.Node, ReadNyxImageEditorInline(LForm.Node));
    LForm.Node.Find(NyxImageEditorFieldID(CEditor, iefAlternative)).Configure
      .Value('A colorful banner / 🌙 / '+TNyxText(#39)+'quoted'+TNyxText(#39)).Done;
    LForm.Node.Find(NyxImageEditorFieldID(CEditor, iefFit)).Configure.Value(NyxImageFitName(nifCover)).Done;
    LForm.Node.Find(NyxImageEditorFieldID(CEditor, iefHorizontal)).Configure.Value(NyxImageAnchorName(niaEnd)).Done;
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'preview and choices create no design/history');
    LDraft.Capture(CEditor, LForm.Node);
    LCopy := TNyxImageEditorDraft.FromData(LDraft.ToData);
    LPreference := DefaultNyxStudioPresentation;
    LPreference.ImageDraft := LCopy;
    LLoaded := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LPreference));
    Check(LLoaded.ImageDraft.ToData.ToJSON = LCopy.ToData.ToJSON, 'per-workspace preference retains exact proposal');
    LFresh := NewNyxImageEditor(CEditor, LOwner, LOwner);
    Check(LLoaded.ImageDraft.Restore(LFresh.Node), 'proposal survives independent chrome rebuild');
    Check(LFresh.Node.Find(CEditor+'-preview').Prop('src') = LSource.ToWire, 'restored preview uses exact copied source');
    Check(LFresh.Node.Find(NyxImageEditorFieldID(CEditor, iefInlineBase64)).Prop('value') = ImageJPEG,
      'copied workspace/chrome draft retains the exact pasted resource');
    LFresh.Node.Find(NyxImageEditorFieldID(CEditor, iefFit)).Configure.Value('unsupported').Done;
    LRefused := False;
    try
      CaptureNyxImageEditor(LFresh.Node.Find(NyxImageEditorActionID(CEditor, ieaApply)), LFresh.Node, LChange);
    except
      on ENyxImage do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore), 'invalid choice refuses without accepted change');
    Check(LCopy.Restore(LFresh.Node), 'independent draft repairs an invalid unsubmitted choice');
    Check(CaptureNyxImageEditor(LFresh.Node.Find(NyxImageEditorActionID(CEditor, ieaApply)), LFresh.Node, LChange),
      'mounted Apply captures one typed proposal');
    Check((LChange.Fit = nifCover) and (LChange.Horizontal = niaEnd) and
      (LChange.Source.ToWire = LSource.ToWire), 'specialized sizing/source remain typed');
    Check(TNyxImageEditorChange.FromData(LChange.ToData).ToData.ToJSON = LChange.ToData.ToJSON,
      'copied processor change round-trips exactly');
    LSession := TNyxStudioSession.Create;
    LSession.Load(LBefore);
    LSession.Select('hero-image');
    Check(CaptureNyxStudioAuthoring(LSession, LFresh.Node.Find(
      NyxImageEditorActionID(CEditor, ieaApply)), ntClick, LFresh.Node,
      Default(TNyxStudioPendingDesign), LEdit) = sacEdit, 'ordinary shared controller captures grouped image edit');
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LDecoded := ReadNyxStudioDesignRequest(LRequest.ToData);
    Check(LRequest.SameRequest(LDecoded), 'new immutable ticket round-trips');
    LPrepared := PrepareNyxStudioDesign(LDecoded, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'isolated preparation admits complete image proposal');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied, 'paired image publication applies');
    LAfter := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(Pos('nimJPEG', LSession.Source) > 0, 'crafted source uses a typed image constructor');
    Check(Pos('nifCover', LSession.Source) > 0, 'crafted source retains closed fit enum');
    LSession.Undo;
    Check(LSession.Save = LBefore, 'one Undo restores complete original image design');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'one Redo restores exact accepted pair');
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(LPrepared.Diagnostic.Defined, 'old mounted baseline refuses queued overwrite');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscRejected, 'stale image is a normal admission refusal');
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAfter, 'stale refusal retains pair/history');
    LOwner.Configure.AlternativeText('Changed baseline').Done;
    LFresh := NewNyxImageEditor(CEditor, LOwner, LOwner);
    Check(not LCopy.Restore(LFresh.Node) and not LCopy.Defined, 'changed inherited/local context retires proposal');
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
    LForm := nil;
    LFresh := nil;
    LProjection.Free;
    LSession.Free;
    LDocument.Free;
  end;
end;

{$ifndef PAS2JS}
type
  TControlAccess = class(TControl);
  { Test adapter substitutes the user's OS chooser only. The actual bounded
    UTF-8 file read, PNG/JPEG admission, native decoding and Studio callback run. }
  TFilePicker = class(TInterfacedObject, INyxImagePicker)
  private
    FReply: TNyxImagePickReply;
  public
    procedure Pick(AReply: TNyxImagePickReply);
    procedure Cancel;
    procedure Deliver(const AFileName: TNyxText);
  end;
  TTestStudio = class(TNyxNativeStudio)
  protected
    function CreateImagePicker: INyxImagePicker; override;
  end;

var
  GPicker: TFilePicker;
  GPickerLease: INyxImagePicker;

procedure TFilePicker.Pick(AReply: TNyxImagePickReply);
begin
  FReply := AReply;
end;

procedure TFilePicker.Cancel;
begin
  FReply := nil;
end;

procedure TFilePicker.Deliver(const AFileName: TNyxText);
var
  LReply: TNyxImagePickReply;
  LSource: TNyxImageSource;
begin
  LSource := ReadNyxImageFile(AFileName);
  LReply := FReply;
  FReply := nil;

  if Assigned(LReply) then
  begin
    LReply(ipsSelected, LSource, '');
  end;
end;

function TTestStudio.CreateImagePicker: INyxImagePicker;
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
  LSource: TNyxImageSource;
  LBytes: TNyxImageBytes;
  LStream: TFileStream;
  LPath: TNyxText;
  LField: TCustomEdit;
  LChoice: TComboBox;
  LPNG: TNyxImageSource;
  LRefused: Boolean;

  procedure Ready;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Image Studio did not retire: '+LStudio.Status);
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceCommands.Busy;
  end;

  procedure Click(const AID: TNyxText);
  var
    LControl: TControl;
  begin
    LControl := LStudio.ShellView.ControlFor(AID);
    Check(LControl <> nil, 'ordinary command is mounted: '+AID);
    TControlAccess(LControl).Click;
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
  LSeed := BuildNyxDocument;
  GPicker := TFilePicker.Create;
  GPickerLease := GPicker;
  try
    LPath := TNyxText(ExtractFilePath(ParamStr(1))) + TNyxText('import-🌙.dat');
    LSource := NyxEmbeddedImage(nimJPEG, ImageJPEG);
    LBytes := LSource.Bytes;
    LStream := TFileStream.Create(LPath, fmCreate);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    Check(ReadNyxImageFile(LPath).ToWire = LSource.ToWire,
      'native UTF-8 filename imports exact bytes independent of extension');
    LWindow.SetBounds(40,40,1240,820);
    LWindow.Show;
    LStudio := TTestStudio.Create(LWindow, '');
    LPair := NyxProjectPair(TNyxCodec.Encode(LSeed), TNyxCodegen.Generate(LSeed));
    LStudio.LoadProject(LPair);
    LStudio.Session.Select('hero-image');
    LStudio.Run;
    Ready;
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Check(not LStudio.Session.ProjectSnapshot.Pending, 'fresh semantic pair has no pending Pascal draft');
    Check(LStudio.ShellView.Root.Find('inspector-src') = nil, 'image has a typed form instead of a raw source input');
    Click(NyxImageEditorActionID(CEditor, ieaImport));
    GPicker.Deliver(LPath);
    Ready;
    Check(LStudio.ShellView.Root.Find(CEditor+'-preview').Prop('src') = LSource.ToWire,
      'actual Import callback updates only the reusable proposal preview');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'ordinary imported preview makes no document/history edit');
    LField := TCustomEdit(LStudio.ShellView.InputFor(NyxImageEditorFieldID(CEditor, iefInlineBase64)));
    Check(LField <> nil, 'real pasted Base64 memo exists');
    LField.Text := ImagePNG;
    Click(NyxImageEditorActionID(CEditor, ieaInline));
    Check(LStudio.ShellView.Root.Find(CEditor+'-preview').Prop('src') =
      NyxEmbeddedImage(nimPNG, ImagePNG).ToWire, 'ordinary pasted Base64 preview uses native decoded PNG');
    LField.Text := 'invalid Base64';
    Click(NyxImageEditorActionID(CEditor, ieaInline));
    Check((EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore) and
      (LStudio.ShellView.Root.Find(CEditor+'-preview').Prop('src') =
      NyxEmbeddedImage(nimPNG, ImagePNG).ToWire), 'invalid pasted resource retains prior preview and accepted pair');
    { Refusal may rebuild the form. Reacquire actual controls after that repaint. }
    LField := TCustomEdit(LStudio.ShellView.InputFor(NyxImageEditorFieldID(CEditor, iefInlineBase64)));
    LField.Text := LSource.ToWire;
    LChoice := TComboBox(LStudio.ShellView.InputFor(NyxImageEditorFieldID(CEditor, iefInlineFormat)));
    LChoice.ItemIndex := LChoice.Items.IndexOf('JPEG');
    LChoice.OnChange(LChoice);
    Click(NyxImageEditorActionID(CEditor, ieaInline));
    Check((LStudio.ShellView.Root.Find(CEditor+'-preview').Prop('src') = LSource.ToWire) and
      (EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore),
      'actual data URL preview produces a packed proposal without editing history');
    LField := TCustomEdit(LStudio.ShellView.InputFor(NyxImageEditorFieldID(CEditor, iefAlternative)));
    Check(LField <> nil, 'real alternative-text input exists');
    LField.Text := 'A new banner / 🌙';
    LChoice := TComboBox(LStudio.ShellView.InputFor(NyxImageEditorFieldID(CEditor, iefFit)));
    LChoice.ItemIndex := LChoice.Items.IndexOf(NyxImageFitName(nifCover));
    LChoice.OnChange(LChoice);
    Click('action-code');
    Check(LStudio.ShellView.Root.Find(CEditor+'-preview').Prop('src') = LSource.ToWire,
      'source pane refresh retains unsubmitted imported bytes');
    Check(LStudio.ShellView.Root.Find(NyxImageEditorFieldID(CEditor, iefAlternative)).Prop('value') =
      TNyxText('A new banner / 🌙'), 'chrome refresh retains exact proposed text');
    Click(NyxImageEditorActionID(CEditor, ieaApply));
    Check(LStudio.SourceCommands.State = nssApplied,
      'ordinary Apply reaches isolated paired queue: ' + LStudio.Status);
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Check(LAfter <> LBefore, 'accepted image pair changes');
    Check(LStudio.Session.Document.Find('hero-image').Prop('src') = LSource.ToWire, 'accepted image retains exact imported source');
    Check(Pos('nimJPEG', LStudio.Session.Source) > 0, 'actual Studio source uses specialized image authoring');
    if ParamCount > 1 then
    begin
      Capture(ParamStr(2));
    end;
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore, 'ordinary one Undo restores exact complete pair');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter, 'ordinary Redo restores imported pair');
    LWindow.SetBounds(40,40,390,700);
    Ready;
    Click('action-panel-inspector');
    Check(LStudio.ShellView.Root.Find(CEditor) <> nil, 'compact Inspector uses the same reusable image form');
    if ParamCount > 2 then
    begin
      Capture(ParamStr(3));
    end;
    Click(NyxImageEditorActionID(CEditor, ieaClear));
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter, 'Clear remains an unsubmitted proposal');
    Click(NyxImageEditorActionID(CEditor, ieaApply));
    Check(LStudio.Session.Document.Find('hero-image').Prop('src') = '', 'explicit Apply clears image');
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter, 'one Undo restores cleared image and source');
    Click(NyxImageEditorActionID(CEditor, ieaImport));
    LStudio.LoadProject(LPair);
    Ready;
    GPicker.Deliver(LPath);
    Ready;
    Check(Pos('earlier project', LStudio.Status)>0, 'late import refuses changed project identity');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LPair),
      'late import cannot touch replacement project');
    { A decoder error is separate from header admission and cannot produce a
      successful file proposal. Use the maintained PNG with a wrong CRC. }
    LPNG := NyxEmbeddedImage(nimPNG, ImagePNG);
    LBytes := LPNG.Bytes;
    LBytes[High(LBytes)] := LBytes[High(LBytes)] xor 1;
    LStream := TFileStream.Create(LPath, fmCreate);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
    LSource := NyxNoImage;
    LRefused := False;
    try
      LSource := ReadNyxImageFile(LPath);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSource.Kind = nisEmpty), 'native decoder refusal publishes no source');
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
  TImageInputEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;

procedure BrowserControls;
var
  LSeed: TNyxDocument;
  LShell: TNyxDocument;
  LPage: INyxPage;
  LForm: INyxCard;
  LView: TNyxBrowserRenderer;
  LInput: TJSHTMLInputElement;
  LOptions: TJSObject;
  LChange: TNyxImageEditorChange;
  LPicker: INyxImagePicker;
begin
  LSeed := BuildNyxDocument;
  LShell := TNyxDocument.Create;
  LView := TNyxBrowserRenderer.Create;
  try
    LPage := NewNyxPage('image-form');
    LShell.AddPage(LPage);
    LForm := NewNyxImageEditor(CEditor, LSeed.Find('hero-image'), LSeed.Find('hero-image'));
    LPage.Add(LForm);
    LView.Render(LShell, LShell.Pages[0], TJSHTMLElement(document.body));
    ProposeNyxImageEditorSource(LForm.Node, NyxEmbeddedImage(nimJPEG, ImageJPEG));
    LView.Sync;
    LInput := TJSHTMLInputElement(LView.InputFor(NyxImageEditorFieldID(CEditor, iefAlternative)));
    LInput.value := 'A browser banner / 🌙';
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LInput.dispatchEvent(TImageInputEvent.new('input', LOptions));
    LInput.dispatchEvent(TImageInputEvent.new('change', LOptions));
    Check(CaptureNyxImageEditor(LForm.Node.Find(NyxImageEditorActionID(CEditor, ieaApply)),
      LView.Root, LChange) and (LChange.AlternativeText = 'A browser banner / 🌙'),
      'browser control input reaches typed image capture');
    Check(LView.ElementFor(CEditor+'-preview') is TJSHTMLImageElement,
      'ordinary browser preview owns its image element');
    LPicker := NewNyxBrowserImagePicker;
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
    {$ifdef PAS2JS}BrowserControls;{$else}NativeStudio;{$endif}
    WriteLn('PASS / image authoring / ',GChecks,' checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-test-result','passed');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ',LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-test-result','failed');{$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
