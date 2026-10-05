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
program nyx_canvas_queue_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, Graphics,
  IntfGraphics, FPWritePNG, nyx.text, nyx.types, nyx.model, nyx.state, nyx.data,
  nyx.studio.projects, nyx.studio.session, nyx.studio.sourcejobs,
  nyx.studio.lcl, nyx.test.source.canvas;

type
  TControlAccess = class(TControl);
  { Genuine UI callback failures must fail the journey, including couriers
    drained during editor retirement. This observer owns no editor or field. }
  TFailureObserver = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GObserver: TFailureObserver;
  GChecks: Integer;

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.Message;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if (GObserver.Error <> '') or not ACondition then
  begin
    raise ENyxModel.Create('Canvas controls: ' + AReason + ' / ' + GObserver.Error);
  end;
  Inc(GChecks);
end;

procedure Pump;
begin
  Application.ProcessMessages;
  CheckSynchronize;
  Application.ProcessMessages;
end;

procedure Ready;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise ENyxModel.Create('Canvas queue did not retire / ' + GStudio.Status);
    end;
    Sleep(1);
  until not GStudio.SourceCommands.Busy;
  Pump;
end;

procedure Click(const AID: TNyxText);
begin
  TControlAccess(GStudio.ShellView.ControlFor(AID)).Click;
  Ready;
end;

function Field(const AOwner: TNyxText; const APart: TNyxText = ''): TNyxNode;
begin
  Result := NyxCanvasSourceField(GStudio.CanvasView.Root, AOwner, APart);
  Check(Result <> nil, 'Actual canvas resolves the requested ordinary/named field');
end;

function Memo(const AOwner: TNyxText; const APart: TNyxText = ''): TMemo;
var
  LField: TNyxNode;
begin
  LField := Field(AOwner, APart);
  Result := TMemo(GStudio.CanvasView.InputFor(LField.ID, niRuntime));
end;

procedure NumericInput(const AValue: TNyxText);
var
  LInput: TEdit;
begin
  LInput := TEdit(GStudio.CanvasView.InputFor('quantity', niRuntime));
  GStudio.CanvasView.Reveal('quantity', niRuntime);
  LInput.SetFocus;
  LInput.Text := AValue;
  Check(Assigned(LInput.OnEditingDone), 'Numeric field uses its genuine editing-complete callback');
  LInput.OnEditingDone(LInput);
  Ready;
end;

procedure Capture(const AName: TNyxText);
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GForm.ClientWidth, GForm.ClientHeight);
    GForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

procedure Run;
var
  LSeed: TNyxProjectPair;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LReply: TMemo;
  LCode: TMemo;
  LFace: TNyxNode;
  LRuntimeID: TNyxText;
  LDefinition: TNyxText;
  LDraft: TNyxText;
  LPending: TNyxStudioPendingDesign;
  LOldEdit: TNyxStudioDesignEdit;
  LRefused: Boolean;
begin

  if ParamCount <> 1 then
  begin
    raise ENyxModel.Create('Supply the owned canvas control artifact directory');
  end;
  ForceDirectories(ParamStr(1));
  Application.Initialize;
  GObserver := TFailureObserver.Create;
  Application.OnException := GObserver.Failed;
  GForm := TForm.Create(nil);
  GForm.SetBounds(20, 20, 1280, 900);
  GForm.Show;
  GStudio := TNyxNativeStudio.Create(GForm, IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
  try
    LSeed := NyxCanvasSourceFixture;
    GStudio.LoadProject(LSeed);
    GStudio.Run;
    Pump;
    Capture('canvas-queue-desktop');
    Click('action-code');
    LCode := TMemo(GStudio.CodeView.InputFor('studio-code'));
    LDraft := GStudio.Session.Source + TNyxText(#10 + '// Unsent application helper / 🌙');
    LCode.Text := LDraft;
    Check(GStudio.Session.DraftSource = LDraft, 'Genuine source input retains its exact pending draft');
    LBefore := GStudio.Session.ProjectSnapshot;
    LDefinition := TNyxDataValue.ParseJSON(LBefore.Design).Field('components').ToJSON;
    LFace := Field('first', 'body/editor');
    LRuntimeID := LFace.ID;
    LReply := Memo('first', 'body/editor');
    GStudio.CanvasView.Reveal(LRuntimeID, niRuntime);
    LReply.SetFocus;
    LReply.Text := 'First physical reply / 🌙';
    LReply.Text := 'Second physical reply';
    LReply.Text := 'Latest physical reply / 🌙';
    LReply.SelStart := 5;
    LReply.SelLength := 0;
    Check(GStudio.SourceCommands.Busy and not GStudio.Session.CanUndo and
      (GStudio.Session.Source = LBefore.Source), 'Physical input submits intent without synchronous publication');
    LPending := GStudio.SourceCommands.PendingDesign;
    Check((Length(LPending.CanvasValues) = 2) and
      (LPending.CanvasValues[1].Value = TNyxText('Latest physical reply / 🌙')),
      'Only adjacent waiting edits coalesce; active and latest proposals stay ordered');
    LPending.CanvasValues[1].Value := 'Private snapshot mutation';
    Check(GStudio.SourceCommands.PendingDesign.CanvasValues[1].Value =
      TNyxText('Latest physical reply / 🌙'), 'Presentation snapshot cannot mutate queued intent');
    Ready;
    Check((GStudio.SourceCommands.State = nssApplied) and
      (Memo('first', 'body/editor') = LReply) and
      (LReply.Text = TNyxText('Latest physical reply / 🌙')),
      'Actual inherited memo retains its control and latest admitted text');
    Check((Screen.ActiveControl = LReply) and (LReply.SelStart = 5),
      'Completed queued projection retains actual native focus and caret');
    Check((GStudio.Session.DraftSource = LDraft) and
      (GStudio.CodeView.InputFor('studio-code') = LCode), 'Canvas publication retains exact draft and source control');
    Check(TNyxDataValue.ParseJSON(GStudio.Session.Save).Field('components').ToJSON = LDefinition,
      'Physical nested field edits leave both reusable definitions independent');
    Check(Memo('second', 'body/editor').Text = 'English template default',
      'Sibling instance keeps its template default');
    LAfter := GStudio.Session.ProjectSnapshot;
    Click('action-undo');
    Check(Memo('first', 'body/editor').Text = TNyxText('First physical reply / 🌙'),
      'One Undo restores the active edit, excluding superseded waiting text');
    Click('action-undo');
    Check((GStudio.Session.Save = LBefore.Design) and
      (GStudio.Session.Source = LBefore.Source) and not GStudio.Session.CanUndo,
      'Two Undo steps restore the complete initial pair');
    Click('action-redo');
    Click('action-redo');
    Check((GStudio.Session.Save = LAfter.Design) and (GStudio.Session.Source = LAfter.Source),
      'Two Redo steps reproduce the exact coalesced canvas pair');

    Click('view-page-1');
    NumericInput('not an integer');
    Check((GStudio.SourceCommands.State = nssRejected) and
      (GStudio.Session.Document.State.GetValue(NyxIntegerState('quantity')) = 2) and
      (TEdit(GStudio.CanvasView.InputFor('quantity')).Text = '2'),
      'Wrong numeric input restores its actual accepted field and typed default / state ' +
      IntToStr(Ord(GStudio.SourceCommands.State)) + ' / ' + GStudio.Status + ' / field ' +
      TEdit(GStudio.CanvasView.InputFor('quantity')).Text + ' / default ' +
      IntToStr(GStudio.Session.Document.State.GetValue(NyxIntegerState('quantity'))));
    NumericInput('100');
    Check((GStudio.SourceCommands.State = nssRejected) and
      (GStudio.Session.Document.State.GetValue(NyxIntegerState('quantity')) = 2),
      'Out-of-range input refuses without a partial default');
    NumericInput('3');
    Check((GStudio.SourceCommands.State = nssApplied) and
      (GStudio.Session.Document.State.GetValue(NyxIntegerState('quantity')) = 3),
      'Editing-complete updates an actual bound Integer default');
    Memo('bound-reply').Text := 'Shared physical reply / 🌙';
    Ready;
    Check((GStudio.Session.Document.State.GetValue(NyxTextState('reply')) =
      TNyxText('Shared physical reply / 🌙')) and
      (GStudio.Session.Document.Find('bound-reply').Prop('value') = 'Explicit fallback'),
      'Actual two-way memo updates state without changing its authored fallback');
    Check(Memo('projected-reply').Text = TNyxText('Shared physical reply / 🌙'),
      'Accepted default reprojects another actual field');
    LBefore := GStudio.Session.ProjectSnapshot;
    Memo('projected-reply').Text := 'Forbidden one-way edit';
    Ready;
    Check((GStudio.SourceCommands.State = nssRejected) and
      (GStudio.Session.Save = LBefore.Design) and (GStudio.Session.Source = LBefore.Source) and
      (Memo('projected-reply').Text = TNyxText('Shared physical reply / 🌙')),
      'One-way field refuses and restores actual text with exact paired history');
    Memo('locked-reply').Text := 'Forbidden read-only edit';
    Pump;
    Check((GStudio.Session.Save = LBefore.Design) and
      (Memo('locked-reply').Text = TNyxText('Shared physical reply / 🌙')),
      'State-projected ReadOnly suppresses physical proposal admission');

    Click('view-page-0');
    LFace := Field('first', 'body/editor');
    LOldEdit := GStudio.Session.CaptureCanvasValue(LFace, npfNativeLCL, GStudio.Session.CommandContext);
    LReply := Memo('first', 'body/editor');
    LReply.Text := 'Retired active value';
    LReply.Text := 'Retired waiting value';
    GStudio.LoadProject(LSeed);
    LReply.Text := 'Old mounted field after same-ID reload';
    Check(not GStudio.Session.CanUndo and (GStudio.Session.Source = LSeed.Source),
      'Old mounted input cannot edit the same IDs after project replacement');
    Pump;
    Memo('first', 'body/editor').Text := 'New loaded project value';
    Memo('first', 'body/editor').Text := 'New loaded project latest';
    LRefused := False;
    try
      GStudio.SourceCommands.Edit(LOldEdit);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Retained old intent refuses before it can coalesce away current input');
    Ready;
    Check(Memo('first', 'body/editor').Text = 'New loaded project latest',
      'Only current-load proposals publish after older workers drain');
    Click('action-undo');

    if GStudio.Session.Source <> LSeed.Source then
    begin
      { If the retired worker drained before the first new proposal, that first
        proposal was already active and owns its separate Undo. Otherwise both
        new waiting proposals correctly coalesced behind the retired worker. }
      Check(Memo('first', 'body/editor').Text = 'New loaded project value',
        'Separate active current-load edit retains its precise history value');
      Click('action-undo');
    end;
    Check((GStudio.Session.Source = LSeed.Source) and (GStudio.Session.Save = LSeed.Design) and
      not GStudio.Session.CanUndo, 'Current-load paired Undo excludes all retired work');
    GStudio.LoadProject(LSeed);
    Pump;
    GForm.ClientWidth := 390;
    GStudio.RequestRefresh;
    Pump;
    Capture('canvas-queue-390');
    Memo('first', 'body/editor').Text := 'Discard during retirement';
    Check(GStudio.SourceCommands.Busy, 'Actual retirement starts with live isolated preparation');
    FreeAndNil(GStudio);
    Pump;
    Check(GObserver.Error = '', 'Retired worker cannot call freed canvas, source or editor receivers');
  finally
    GStudio.Free;
    GStudio := nil;
    GForm.Free;
    Application.OnException := nil;
    GObserver.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' actual native queued canvas checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
