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


program nyx_resource_workbench_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, Interfaces, Forms, Controls, StdCtrls, Grids, Graphics,
  IntfGraphics, FPWritePNG, nyx.text, nyx.types, nyx.bytes, nyx.data,
  nyx.resources, nyx.resources.editor, nyx.resources.rows.editor,
  nyx.resources.import, nyx.resources.import.lcl, nyx.collections,
  nyx.binding.types, nyx.model, nyx.codec, nyx.codegen, nyx.studio.projects,
  nyx.studio.lcl, nyx.studio.sourcejobs, nyx.generated.view,
  nyx.test.resource.workbench;

const
  CEditor = 'studio-resource-editor';
  CRowEditor = 'studio-resource-rows';
  { An opt-in first-open workload isolates the ordinary Resources presentation.
    The normal invocation still exercises the complete paired authoring journey. }
  CProfileOpen = '--profile-open';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  WriteLn('Resource workbench / ', GChecks + 1, ' / ', AReason);
  Flush(Output);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

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
  LAfter: TNyxText;
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LPath: TNyxText;
  LTable: TStringGrid;
  LBitmap: TBitmap;
  LImage: TLazIntfImage;

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
        raise Exception.Create('Resource workbench did not retire: ' + LStudio.Status +
          ' / presentation=' + BoolToStr(LStudio.PresentationPending, True) +
          ' / source=' + BoolToStr(LStudio.SourceBusy, True) +
          ' / paints=' + IntToStr(LStudio.PaintCount));
      end;
      Sleep(1);
    until not LStudio.PresentationPending and not LStudio.SourceCommands.Busy;
  end;

  procedure Click(const AID: TNyxText);
  var
    LControl: TControl;
    LRetainedInput: TControl;
    LRetainedRoot: TNyxNode;
  begin
    LRetainedInput := nil;
    LRetainedRoot := nil;

    if (AID = NyxResourceEditorActionID(CEditor, reaNew)) or
      (Pos(CEditor + '-entry-', AID) = 1) then
    begin
      LRetainedInput := LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, refContent));
      LRetainedRoot := LStudio.ShellView.RootFor(CEditor);
    end;
    LControl := LStudio.ShellView.ControlFor(AID);
    Check(LControl <> nil, 'ordinary mounted command: ' + AID);
    TControlAccess(LControl).Click;
    Ready;

    if LRetainedInput <> nil then
    begin
      Check((LStudio.ShellView.RootFor(CEditor) = LRetainedRoot) and
        (LStudio.ShellView.InputFor(NyxResourceEditorFieldID(CEditor, refContent)) = LRetainedInput),
        'New/Open retains the actual shell and resource input');
    end;
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


  { Only adapter inputs are driven. The admitted design originates in MCP,
    Apply runs the ordinary source processor and history compares complete pairs. }
  procedure Select(const AID: TNyxText);
  var
    LChrome: TControl;
    LResourceDraft: TNyxText;
    LCode: TControl;
  begin
    LChrome := LStudio.ShellView.ControlFor('action-undo');
    LResourceDraft := TCustomEdit(LStudio.ShellView.InputFor(
      NyxResourceEditorFieldID(CEditor, refContent))).Text;
    LCode := LStudio.CodeView.InputFor('studio-code');
    LStudio.Session.Select(AID);
    LStudio.RequestRefresh;
    Ready;
    Check(LStudio.ShellView.ControlFor('action-undo') = LChrome,
      'changed Inspector preserves the actual Chrome command');
    { The Resources apply target changes with selection, so its event scope may
      need replacement. Its pending content must survive that admitted change. }
    Check(TCustomEdit(LStudio.ShellView.InputFor(
      NyxResourceEditorFieldID(CEditor, refContent))).Text = LResourceDraft,
      'changed binding target preserves the exact Resources draft');
    Check(LStudio.CodeView.InputFor('studio-code') = LCode,
      'changed Inspector preserves the independent Pascal input');
  end;

  procedure RowValue(AField: TNyxResourceRowsField; const AValue: TNyxText);
  var
    LControl: TControl;
    LChoice: TComboBox;
  begin
    LControl := LStudio.ShellView.InputFor(NyxResourceRowsFieldID(CRowEditor, AField));
    Check(LControl <> nil, 'mounted row input');

    if LControl is TComboBox then
    begin
      LChoice := TComboBox(LControl);
      LChoice.ItemIndex := LChoice.Items.IndexOf(AValue);
      Check(LChoice.ItemIndex >= 0, 'visible structural choice: ' + AValue);
      LChoice.OnChange(LChoice);
    end
    else if LControl is TCheckBox then
    begin
      TCheckBox(LControl).Checked := AValue = 'true';
      TCheckBox(LControl).OnChange(LControl);
    end
    else
    begin
      TCustomEdit(LControl).Text := AValue;
    end;
    Ready;
  end;

  procedure ImportFile(const AName, AKind, ATitle, AHelp: TNyxText;
    const AContent: TNyxBytes);
  begin
    Click(NyxResourceEditorActionID(CEditor, reaNew));
    Check(not TCustomEdit(LStudio.ShellView.InputFor(
      NyxResourceEditorFieldID(CEditor, refName))).ReadOnly, 'New unlocks an independent resource name');
    TextField(refName, AName);
    Choice(refKind, AKind);
    LPath := TNyxText(ExtractFilePath(ParamStr(1))) + AName + TNyxText('.dat');
    LStream := TFileStream.Create(LPath, fmCreate);
    try
      LStream.WriteBuffer(AContent[0], Length(AContent));
    finally
      LStream.Free;
    end;
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Click(NyxResourceEditorActionID(CEditor, reaImport));
    GPicker.Deliver(LPath);
    Ready;
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'real file import is a copied proposal');
    TextField(refTitle, ATitle);
    TextField(refDescription, AHelp);
  end;

  procedure History(const APrevious: TNyxText);
  begin
    LAfter := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Check(LAfter <> APrevious, 'Apply publishes a changed pair');
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = APrevious,
      'one Undo restores the exact previous design and source');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'one Redo restores the exact new design and source');
  end;

begin
  LWindow := TForm.CreateNew(nil);
  LStudio := nil;
  LSeed := BuildNyxDocument;
  GPicker := TFilePicker.Create;
  GPickerLease := GPicker;
  try
    LWindow.SetBounds(40, 40, 1240, 820);
    LWindow.Show;
    LStudio := TTestStudio.Create(LWindow, '');
    LPair := NyxProjectPair(TNyxCodec.Encode(LSeed), TNyxCodegen.Generate(LSeed));
    LStudio.LoadProject(LPair);
    LStudio.Session.Select('workshop-headline');
    LStudio.Run;
    Ready;
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LPair),
      'ordinary Studio retains the unchanged MCP seed');
    Click('action-resources-toggle');

    if ParamStr(3) = CProfileOpen then
    begin
      Check(LStudio.ShellView.Root.Find(CEditor) <> nil,
        'profiling opens the real public Resources form');
      Exit;
    end;
    ImportFile('copy', 'JSON', WorkbenchCopyTitle, WorkbenchCopyHelp, NyxEncodeUTF8(WorkbenchJSON));
    Bind;
    Choice(refTarget, NyxBindingPropertyTitle(bpText));
    Choice(refPath, 'Root["literal.dot"] / text');
    Click('action-code');
    Check(LStudio.ShellView.Root.Find(NyxResourceEditorFieldID(CEditor, refDescription))
      .Prop('value') = WorkbenchCopyHelp, 'chrome retains the imported creator help');
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'ordinary source admission publishes JSON plus caption');
    Check(TLabel(LStudio.CanvasView.ControlFor('workshop-headline')).Caption = 'Your resource workbench',
      'actual native caption reads the resource');
    History(LBefore);
    Select('project-name');
    Click(CEditor + '-entry-0');
    Bind;
    Choice(refTarget, NyxBindingPropertyTitle(bpPlaceholder));
    Choice(refPath, 'Root["prompt"] / text');
    LBefore := LAfter;
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    Check(TCustomEdit(LStudio.CanvasView.InputFor('project-name')).TextHint = 'Choose a project name',
      'actual input prompt reads the same file');
    History(LBefore);
    RowValue(rrName, 'workshop-rows');
    RowValue(rrResource, NyxData('copy').ToJSON);
    Click(NyxResourceRowsActionID(CRowEditor, raDiscover));
    RowValue(rrDataset, NyxResourcePath.Field('rows').ToData.ToJSON);
    Click(NyxResourceRowsActionID(CRowEditor, raInspect));
    RowValue(rrIdentity, NyxResourcePath.Field('id').ToData.ToJSON);
    RowValue(rrFieldName, 'item');
    RowValue(rrFieldType, 'Text');
    RowValue(rrFieldPath, NyxResourcePath.Field('item').ToData.ToJSON);
    Click(NyxResourceRowsActionID(CRowEditor, raSetField));
    RowValue(rrFieldName, 'amount');
    RowValue(rrFieldType, 'Number');
    RowValue(rrFieldPath, NyxResourcePath.Field('amount').ToData.ToJSON);
    Click(NyxResourceRowsActionID(CRowEditor, raSetField));
    LBefore := LAfter;
    Click(NyxResourceRowsActionID(CRowEditor, raApply));
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'existing empty defaults refuse replacement without explicit consent');
    RowValue(rrReplaceStatic, 'true');
    Click('action-code');
    Check(LStudio.ShellView.Root.Find(NyxResourceRowsFieldID(CRowEditor, rrReplaceStatic))
      .Prop('value') = 'true', 'consent and row draft survive chrome rebuild');
    Click(NyxResourceRowsActionID(CRowEditor, raApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'ordinary source admission publishes typed row relationship');
    LTable := TStringGrid(LStudio.CanvasView.ControlFor('workshop-table'));
    Check((LTable.RowCount = 3) and (LTable.Cells[0, 1] = 'Canvas') and
      (LTable.Cells[0, 2] = 'Studio') and (LTable.Cells[1, 1] = '3.125') and
      (LTable.Cells[1, 2] = '6.5'), 'actual native table displays two exact resource rows');
    History(LBefore);
    Click(NyxResourceRowsActionID(CRowEditor, raLoad));
    Click(NyxResourceRowsActionID(CRowEditor, raDetach));
    Check(not LStudio.Session.Document.ResourceCollections.HasSource(NyxCollection('workshop-rows')) and
      (LStudio.Session.Document.Collections.Snapshot(NyxCollection('workshop-rows')).Count = 2),
      'detach materializes two defaults while retaining the project file');
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LAfter,
      'detach Undo restores the complete row relationship');
    Select('workshop-notes');
    ImportFile('notes', 'Text', WorkbenchNotesTitle, WorkbenchNotesHelp, NyxEncodeUTF8(WorkbenchNotes));
    Bind;
    Choice(refTarget, NyxBindingPropertyTitle(bpText));
    Choice(refPath, 'File text / text');
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'ordinary source processor admits packed text: ' + LStudio.Status);
    Check(TLabel(LStudio.CanvasView.ControlFor('workshop-notes')).Caption = WorkbenchNotes,
      'actual plain text caption reads the packed file');
    History(LBefore);
    SetLength(LBytes, 3);
    LBytes[0] := 0;
    LBytes[1] := 1;
    LBytes[2] := 255;
    ImportFile('packed', 'Binary', WorkbenchPackedTitle, WorkbenchPackedHelp, LBytes);
    Click(NyxResourceEditorActionID(CEditor, reaApply));
    Check(LStudio.SourceCommands.State = nssApplied, 'arbitrary bytes admit without an invented scalar binding');
    History(LBefore);
    Inc(GChecks, CheckNyxResourceWorkbench(LStudio.Session.Document));
    LBytes := NyxEncodeUTF8(LStudio.Session.Source);
    LStream := TFileStream.Create(ParamStr(1), fmCreate);
    try
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    finally
      LStream.Free;
    end;
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
  finally
    LStudio.Free;
    Check(LWindow.ControlCount = 0, 'retirement removes owned controller views');
    LWindow.Free;
    GPickerLease := nil;
    GPicker := nil;
    LSeed.Free;
  end;
end;

begin
  try
    Application.Initialize;
    NativeStudio;
    WriteLn('PASS / ordinary resource workbench / ', GChecks, ' checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
