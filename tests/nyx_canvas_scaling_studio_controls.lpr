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
program nyx_canvas_scaling_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, Interfaces, Forms, Controls, StdCtrls,
  nyx.text, nyx.files, nyx.files.lcl, nyx.types, nyx.model, nyx.codec,
  nyx.canvas.types, nyx.canvas.editor, nyx.canvas.workspace,
  nyx.studio.lcl, nyx.studio.projects, nyx.studio.projectstore,
  nyx.studio.sourcejobs, nyx.studio.session, nyx.studio.inspector,
  nyx.studio.menu, nyx.json.workshop, nyx.test.capture.lcl;

type
  { Access only standard native input methods; no Studio/controller override. }
  TControlInput = class(TControl);

  { Deferred native exceptions fail the finite journey instead of opening a
    modal. The application revokes this borrowed receiver before final release. }
  TFailures = class
  public
    MessageText: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GStudio: TNyxNativeStudio;
  GHost: TForm;
  GFailures: TFailures;
  GDirectory: TNyxText;
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TFailures.Failed(ASender: TObject; AException: Exception);
begin
  MessageText := AException.Message;
end;

{ Await the actual owned queue and visible paint. No timeout starts a second
  process/command; an unsettled owner fails with its current visible status. }
procedure Settle;
var
  LStarted: QWord;
  LTurns: Integer;
begin
  LStarted := GetTickCount64;
  LTurns := 0;
  repeat
    Application.ProcessMessages;
    CheckSynchronize;
    Application.Idle(False);

    if GFailures.MessageText <> '' then
    begin
      raise Exception.Create(GFailures.MessageText);
    end;

    if not GStudio.SourceBusy and not GStudio.PresentationPending then
    begin
      Inc(LTurns);
    end
    else
    begin
      LTurns := 0;
    end;

    if GetTickCount64 - LStarted > 120000 then
    begin
      raise Exception.Create('Studio did not settle: ' + GStudio.Status);
    end;
    Sleep(10);
  until LTurns >= 3;
end;

function Pair: TNyxText;
begin
  Result := EncodeNyxProject(GStudio.ProjectSnapshot);
end;

procedure Click(const AID: TNyxText);
var
  LControl: TControl;
begin
  Check(GStudio.ShellView.RootFor(AID) <> nil, 'shell command is mounted: ' + AID);
  GStudio.ShellView.ViewFor(AID).Reveal(AID, niRuntime);
  LControl := GStudio.ShellView.ControlFor(AID);
  Check((LControl <> nil) and LControl.IsVisible and LControl.Enabled,
    'shell command is reachable: ' + AID);
  TControlInput(LControl).Click;
  Settle;
end;

procedure Choose(const AValue: TNyxText);
var
  LID: TNyxText;
  LInput: TComboBox;
begin
  LID := NyxCanvasScalingPart('inspector-canvas-scaling', ncspChoice).ID;
  Check(GStudio.ShellView.RootFor(LID) <> nil, 'ordinary Inspector exposes the reusable scaling form');
  GStudio.ShellView.ViewFor(LID).Reveal(LID, niRuntime);
  Check(GStudio.ShellView.InputFor(LID) is TComboBox, 'scaling choice is a real native select');
  LInput := TComboBox(GStudio.ShellView.InputFor(LID));
  LInput.ItemIndex := LInput.Items.IndexOf(AValue);
  Check(LInput.ItemIndex >= 0, 'the closed scaling choice is available');
  LInput.OnChange(LInput);
  Settle;
  Check(GStudio.ShellView.Root.Find(LID).Prop(NyxAttributeName(atValue)) = AValue,
    'the actual adapter retains the selected proposal');
end;

function Capture: TNyxStudioDesignEdit;
var
  LID: TNyxText;
begin
  LID := NyxCanvasScalingPart('inspector-canvas-scaling', ncspApply).ID;
  Check(CaptureNyxCanvasInspector(GStudio.Session, GStudio.ShellView.Root.Find(LID),
    GStudio.ShellView.RootFor(LID), Result), 'ordinary scaling Apply captures the paired transaction');
end;

procedure Apply;
begin
  Click(NyxCanvasScalingPart('inspector-canvas-scaling', ncspApply).ID);
  Check(GStudio.SourceCommands.State = nssApplied, 'paired scaling worker applies successfully: ' + GStudio.Status);
end;

{ The stock public menu renders independently owned Nyx forms. Locate only
  visible captions within its exact titled native window, as a user would. }
function MenuButton(AParent: TWinControl; const ACaption: TNyxText): TControl;
var
  LIndex: Integer;
  LChild: TControl;
begin
  Result := nil;
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin
    LChild := AParent.Controls[LIndex];

    if LChild.IsVisible and ((TControlInput(LChild).Caption = ACaption) or
      (Pos(ACaption + ' ', TControlInput(LChild).Caption) = 1)) then
    begin
      Exit(LChild);
    end;

    if LChild is TWinControl then
    begin
      Result := MenuButton(TWinControl(LChild), ACaption);

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure ClickMenu(const ATitle, ACaption: TNyxText);
var
  LIndex: Integer;
  LButton: TControl;
begin
  LButton := nil;
  for LIndex := 0 to Screen.FormCount - 1 do
  begin

    if Screen.Forms[LIndex].Visible and (Screen.Forms[LIndex].Caption = ATitle) then
    begin
      LButton := MenuButton(Screen.Forms[LIndex], ACaption);
      Break;
    end;
  end;
  Check((LButton <> nil) and LButton.Enabled, 'ordinary action menu exposes ' + ACaption);
  TControlInput(LButton).Click;
  Settle;
end;

procedure Run;
var
  LSeed: TNyxProjectPair;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LDraft: TNyxText;
  LStale: TNyxStudioDesignEdit;
  LCurrent: TNyxStudioDesignEdit;
  LScene: TNyxCanvasRef;
  LCanvas: TNyxNode;
  LName: TEdit;
  LStore: TNyxProjectStore;
  LRevision: TNyxText;
begin
  GDirectory := IncludeTrailingPathDelimiter(ParamStr(2));
  Check(DirectoryExists(GDirectory), 'caller provides an existing external output directory');
  LSeed := DecodeNyxProject(ReadNyxTextFile(ParamStr(1)).Text);
  GHost := TForm.CreateNew(nil);
  GHost.ClientWidth := StrToIntDef(ParamStr(3), 1280);
  GHost.ClientHeight := 900;
  GHost.Show;
  GStudio := TNyxNativeStudio.Create(GHost, GDirectory);
  GStudio.LoadProject(LSeed);
  GStudio.Run;
  Settle;
  WriteLn('STAGE Studio mounted');
  Flush(Output);
  LCanvas := GStudio.Session.Document.Find(JSONWorkshopCanvas.ID).Part(NyxPart('workspace'));
  Check(LCanvas <> nil, 'the accepted Workshop owns its actual authored workspace part');
  LScene := LCanvas.CanvasScene;
  { Set the initial selection through the public controller. The changes below
    use actual Inspector/menu controls, not patched definitions or generated text. }
  GStudio.Session.Select(LCanvas.ID);
  GStudio.RequestRefresh;
  Settle;

  if GStudio.ShellView.RootFor('action-panel-inspector') <> nil then
  begin
    Click('action-panel-inspector');
  end;
  LBefore := Pair;
  Choose('Uniform zoom');
  Check(Pair = LBefore, 'choosing a zoom policy alone changes no authored pair');

  if ParamStr(4) = '--inspect' then
  begin
    { The read-only capture path reuses this same mounted Inspector without
      repeating source-worker/history work solely to obtain its visible form. }
    SaveNyxNativeCapture(GHost, GDirectory + 'inspector.png', ncmPrint);
    Exit;
  end;
  LStale := Capture;
  Apply;
  LAfter := Pair;
  Check((LAfter <> LBefore) and
    (GStudio.Session.Document.Canvases.Definition(LScene).Scaling = nccsUniform),
    'ordinary Inspector publishes the typed uniform policy');
  Check(Pos('.WithScaling(nccsUniform)', GStudio.ProjectSnapshot.Source) > 0,
    'the accepted Pascal uses the crafted fluent enum');
  WriteLn('STAGE Uniform applied');
  Flush(Output);
  Click('action-undo');
  Check(Pair = LBefore, 'one ordinary Undo restores the exact original project/Pascal');
  Click('action-redo');
  Check(Pair = LAfter, 'one ordinary Redo restores the exact uniform pair');
  GStudio.SourceCommands.Edit(LStale);
  Settle;
  Check((GStudio.SourceCommands.State = nssRejected) and (Pair = LAfter),
    'a queued stale scaling proposal refuses without changing the accepted pair');
  WriteLn('STAGE History and stale refusal checked');
  Flush(Output);

  Choose('Keep size');
  LCurrent := Capture;
  LDraft := GStudio.ProjectSnapshot.Source + #10 + TNyxText('// unfinished behavior / 🌙');
  GStudio.Session.SetSourceDraft(LDraft);
  LBefore := Pair;
  GStudio.SourceCommands.Edit(LCurrent);
  Settle;
  Check((GStudio.SourceCommands.State = nssRejected) and (Pair = LBefore) and
    GStudio.Session.SourceDraftPending,
    'an unfinished Pascal draft refuses design changes without losing Unicode text');
  WriteLn('STAGE Unfinished Pascal retained');
  Flush(Output);
  GStudio.Session.DiscardSourceDraft;
  GStudio.RequestRefresh;
  Settle;
  Choose('Keep size');
  Apply;
  Check(GStudio.Session.Document.Canvases.Definition(LScene).Scaling = nccsLogicalSize,
    'the previous fixed-size policy remains independently selectable');
  Click('action-undo');
  Check(Pair = LAfter, 'Undo of the earlier policy recovers the exact uniform pair');
  WriteLn('STAGE Earlier policy and exact recovery checked');
  Flush(Output);

  if GStudio.ShellView.RootFor('action-panel-project') <> nil then
  begin
    Click('action-panel-project');
  end;
  Click(NyxStudioActionMenuID);
  ClickMenu('Component actions', 'Project');
  ClickMenu('Project', 'Open project');
  Check(GStudio.ShellView.InputFor('project-file-name') is TEdit, 'ordinary project file panel is mounted');
  LName := TEdit(GStudio.ShellView.InputFor('project-file-name'));
  LName.Text := 'uniform-workshop';
  LName.OnChange(LName);
  Settle;
  Click('action-project-save');
  LStore := TNyxProjectStore.Create(GDirectory);
  try
    Check(LStore.ReadProject('uniform-workshop', LRevision) = LAfter,
      'ordinary Save writes the exact uniform design/Pascal pair');
  finally
    LStore.Free;
  end;
  Click('action-undo');
  Check(Pair <> LAfter, 'the current project differs before ordinary Open');
  Click('action-project-open');
  Check((Pair <> LAfter) and
    (GStudio.ShellView.Root.Find('project-conflict-warning') <> nil),
    'ordinary Open first retains unsaved work and exposes its warned replacement');
  Click('action-project-use-remote');
  Check(Pair = LAfter, 'ordinary Open restores the exact saved uniform pair');
  WriteNyxTextFile(GDirectory + 'accepted.nyxproject',
    NyxTextFile('accepted.nyxproject', Pair));
  WriteNyxTextFile(GDirectory + 'nyx.generated.json.workshop.pas',
    NyxTextFile('nyx.generated.json.workshop.pas', GStudio.ProjectSnapshot.Source));
  SaveNyxNativeCapture(GHost, GDirectory + 'studio.png', ncmPrint);
end;

begin
  Application.Initialize;
  GFailures := TFailures.Create;
  Application.OnException := GFailures.Failed;
  try
    try
      Run;

      if ParamStr(4) = '--inspect' then
      begin
        WriteLn('PASS ', GChecks, ' ordinary Studio scaling Inspector capture checks');
      end
      else
      begin
        WriteLn('PASS ', GChecks, ' ordinary Studio scaling/paired Save/Open checks');
      end;
    except
      on LException: Exception do
      begin
        WriteLn('FAIL ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
      end;
    end;
  finally
    GStudio.Free;
    GHost.Free;
    Application.OnException := nil;
    GFailures.Free;
  end;
end.
