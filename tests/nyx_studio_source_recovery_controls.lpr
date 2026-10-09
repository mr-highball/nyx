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
program nyx_studio_source_recovery_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data, nyx.editing,
  nyx.editing.lcl, nyx.render.lcl, nyx.studio.lcl, nyx.studio.projects,
  nyx.studio.agents, nyx.studio.sections, nyx.behavior, nyx.events, nyx.scheduler,
  nyx.test.capture.lcl;

type
  TControlAccess = class(TControl);
  { An embedding host may refuse a new physical child. This later candidate
    insertion occurs after the independent source pane has parked. The host
    uses the ordinary virtual LCL insertion contract; Studio is not patched. }
  TRefusalForm = class(TForm)
  public
    procedure InsertControl(AControl: TControl; AIndex: Integer); override;
  end;
  { The subscription owns this observer through an interface. It borrows no
    renderer or session, and records only real source-input notifications. }
  TSourceObserver = class(TNyxEventCallback)
  public
    Inputs: Integer;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GFailParking: Boolean;
  GParkingRefused: Boolean;
  GSourceParkedAtRefusal: Boolean;
  GStudio: TNyxNativeStudio; { borrowed only during the actual owning Run scope }
  GChecks: Integer;

procedure TSourceObserver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if AEvent.Trigger = ntChange then
  begin
    Inc(Inputs);
  end;
end;

procedure TRefusalForm.InsertControl(AControl: TControl; AIndex: Integer);
begin

  if GFailParking and (AControl is TPanel) then
  begin
    GFailParking := False;
    GParkingRefused := True;
    GSourceParkedAtRefusal := (GStudio <> nil) and (GStudio.SourceView.Root <> nil) and
      not GStudio.SourceView.ControlFor(GStudio.SourceView.Root.ID).IsVisible;
    raise ENyxModel.Create('Intentional source parking refusal');
  end;
  inherited InsertControl(AControl, AIndex);
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Source host recovery: ' + AReason);
  end;
  Inc(GChecks);
  WriteLn('PASS ', GChecks, ' / ', AReason);
  Flush(Output);
end;

procedure AwaitStudio(AStudio: TNyxNativeStudio);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    CheckSynchronize;
    Application.ProcessMessages;

    if GetTickCount64 - LStarted > 60000 then
    begin
      raise ENyxModel.Create('Source host preparation timed out / ' + AStudio.Status);
    end;
    Sleep(1);
  until not AStudio.PresentationPending and not AStudio.SourceBusy;
end;

procedure Click(AStudio: TNyxNativeStudio; const AID: TNyxText);
begin
  TControlAccess(AStudio.ShellView.ControlFor(AID)).Click;
end;

procedure Run(const AEvidenceRoot: TNyxText);
var
  LWindow: TForm;
  LStudio: TNyxNativeStudio;
  LCore: TNyxAgentSession;
  LObserver: TSourceObserver;
  LLease: INyxEventCallback;
  LInputToken: INyxEventSubscription;
  LChromeToken: INyxEventSubscription;
  LInputCount: Integer;
  LCanUndo: Boolean;
  LCanRedo: Boolean;
  LSelectedID: TNyxText;
  LCode: TCustomMemo;
  LChrome: TNyxNode;
  LSourceRoot: TNyxNode;
  LCodeRoot: TNyxNode;
  LCanvasRoot: TNyxNode;
  LCanvasHost: TWinControl;
  LSourceHost: TWinControl;
  LDraft: TNyxText;
  LPhysical: TNyxText;
  LPair: TNyxText;
  LSelection: TNyxTextSelection;
  LAfter: TNyxTextSelection;
begin
  LWindow := TRefusalForm.CreateNew(nil);
  LWindow.Caption := 'Nyx source workspace recovery';
  LWindow.SetBounds(20, 20, 1240, 820);
  LStudio := nil;
  LCore := nil;
  LObserver := TSourceObserver.Create;
  LLease := LObserver;
  try
    ForceDirectories(AEvidenceRoot);
    LWindow.Show;
    LStudio := TNyxNativeStudio.Create(LWindow,
      IncludeTrailingPathDelimiter(AEvidenceRoot) + 'project');
    GStudio := LStudio;
    LStudio.Run;
    AwaitStudio(LStudio);
    LCore := TNyxAgentSession.Create(LStudio.Session.ProjectSnapshot);
    Check(LCore.Call('nyx_session', 'source host qualification', NyxObject([]))
      .Field('revision').AsInteger = 1, 'owned semantic session supplies the accepted baseline');
    { The host owns ordinary persisted presentation preferences. Establish the
      required visible source state rather than toggle a previous run's state. }

    if LStudio.CodeView.Root = nil then
    begin
      Click(LStudio, 'action-code');
      AwaitStudio(LStudio);
    end;
    Check(LStudio.CodeView.Root <> nil, 'source workspace is visible / ' + LStudio.Status);
    LCode := TCustomMemo(LStudio.CodeView.InputFor('studio-code'));
    LInputToken := LStudio.CodeView.Events.On(NyxControlEvents('studio-code', niRuntime),
      ntChange).Subscribe(LLease);
    LChromeToken := LStudio.ShellView.Events.On(NyxControlEvents('action-code', niRuntime),
      ntClick).Subscribe(LLease);
    LDraft := LStudio.Session.Source + #10 + TNyxText('{ Unfinished thought / 🌙');
    LCode.Text := LDraft;
    AwaitStudio(LStudio);
    Check((LStudio.Session.DraftSource <> LStudio.Session.Source) and
      (LStudio.Session.DraftSource = LDraft),
      'ordinary source input retains its exact incomplete Unicode proposal');
    Check(LObserver.Inputs > 0, 'actual source input reaches the existing event router');
    LCode := TCustomMemo(LStudio.CodeView.InputFor('studio-code'));
    LCode.SetFocus;
    SelectNyxLCLText(LCode, NyxTextSelection(LDraft, 12, 23));
    LSelection := CaptureNyxLCLSelection(LCode);
    Check(LCode.Focused and LSelection.Defined, 'source field has physical focus and a typed range');
    LChrome := LStudio.ShellView.SectionRoot(nssChrome);
    LSourceRoot := LStudio.SourceView.Root;
    LCodeRoot := LStudio.CodeView.Root;
    LCanvasRoot := LStudio.CanvasView.Root;
    LCanvasHost := LStudio.CanvasView.ControlFor(LCanvasRoot.ID).Parent.Parent;
    LSourceHost := LStudio.SourceView.ControlFor(LSourceRoot.ID).Parent.Parent;
    LPair := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    { Native multiline controls may expose their widgetset's line endings.
      Preserve that exact physical observation separately from portable Pascal. }
    LPhysical := NyxLCLInputText(LCode);
    LInputCount := LObserver.Inputs;
    LCanUndo := LStudio.Session.CanUndo;
    LCanRedo := LStudio.Session.CanRedo;
    LSelectedID := LStudio.Session.SelectedID;
    GFailParking := True;
    Click(LStudio, 'action-code');
    AwaitStudio(LStudio);
    Check(GParkingRefused and GSourceParkedAtRefusal and
      (Pos('Intentional source parking refusal', LStudio.Status) > 0),
      'ordinary complete replacement reaches and reports the source extension refusal');
    Check((LStudio.ShellView.SectionRoot(nssChrome) = LChrome) and
      (LStudio.SourceView.Root = LSourceRoot) and (LStudio.CodeView.Root = LCodeRoot),
      'refusal preserves all exact independently owned roots');
    Check(LStudio.CodeView.InputFor('studio-code') = LCode,
      'refusal preserves the actual borrowed source editor before dereference');
    Check(LStudio.SourceView.ControlFor(LSourceRoot.ID).Parent.Parent = LSourceHost,
      'refusal returns the source pane to its exact prior host');
    Check((LStudio.CanvasView.Root = LCanvasRoot) and
      (LStudio.CanvasView.ControlFor(LCanvasRoot.ID).Parent.Parent = LCanvasHost),
      'refusal returns the exact independently owned design canvas');
    Check(NyxLCLInputText(LCode) = LPhysical, 'refusal preserves the exact physical draft');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LPair,
      'refusal preserves the exact accepted/source/draft pair');
    LAfter := CaptureNyxLCLSelection(LCode);
    Check(LCode.Focused and LAfter.SameRange(LSelection),
      'refusal restores nested source editor focus and range');
    Check(LInputToken.Active and LChromeToken.Active and (LObserver.Inputs = LInputCount),
      'recovery preserves callback scopes and suppresses replay input notifications');
    Check((LStudio.Session.CanUndo = LCanUndo) and (LStudio.Session.CanRedo = LCanRedo) and
      (LStudio.Session.SelectedID = LSelectedID),
      'recovery preserves exact history availability and editor selection');
    LWindow.Repaint;
    SaveNyxNativeCapture(LWindow,
      IncludeTrailingPathDelimiter(AEvidenceRoot) + 'native-live.png', ncmPrint);
    WriteLn('PASS ', GChecks, ' actual nested source host checks');
  finally
    GFailParking := False;
    GStudio := nil;
    { Cancel tokens before the real views retire. The observer's sole lease is
      released afterward; no borrowed control survives its owning Studio. }

    if LInputToken <> nil then
    begin
      LInputToken.Cancel;
    end;

    if LChromeToken <> nil then
    begin
      LChromeToken.Cancel;
    end;
    LInputToken := nil;
    LChromeToken := nil;
    LStudio.Free;
    LCore.Free;
    LLease := nil;
    LWindow.Free;
  end;
end;

begin
  Application.Initialize;
  try

    if ParamCount <> 1 then
    begin
      raise ENyxModel.Create('Supply one owned source-recovery evidence directory');
    end;
    Run(ParamStr(1));
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
