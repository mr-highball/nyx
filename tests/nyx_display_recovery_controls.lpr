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
program nyx_display_recovery_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.render.lcl, nyx.view.recovery, nyx.studio.lcl, nyx.studio.view,
  nyx.studio.sourcejobs, nyx.studio.projects, nyx.test.capture.lcl;

type
  TControlAccess = class(TControl);

var
  GRefuse: Boolean;
  GRefusals: Integer;
  GChecks: Integer;

function HeadingFace(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := TLabel.Create(AOwner);
end;

procedure UpdateHeading(ANode: TNyxNode; AFace: TControl);
begin

  if GRefuse and (ANode.ID = 'display-title') and
    (ANode.Prop('text') = 'A refreshed design.') then
  begin
    Inc(GRefusals);
    raise ENyxModel.Create('Intentional design display refusal');
  end;
  TLabel(AFace).Caption := ANode.Prop('text');
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Display recovery: ' + AReason);
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
      raise ENyxModel.Create('Display preparation timed out / ' + AStudio.Status);
    end;
    Sleep(1);
  until not AStudio.SourceBusy and not AStudio.PresentationPending;
end;

procedure Click(AStudio: TNyxNativeStudio; const AID: TNyxText);
begin

  if (AStudio.SourceView.Root <> nil) and (AStudio.SourceView.Root.Find(AID) <> nil) then
  begin
    TControlAccess(AStudio.SourceView.ControlFor(AID)).Click;
    Exit;
  end;
  TControlAccess(AStudio.ShellView.ControlFor(AID)).Click;
end;

procedure Run(const AEvidenceRoot: TNyxText);
var
  LWindow: TForm;
  LStudio: TNyxNativeStudio;
  LCode: TCustomMemo;
  LBefore: TNyxText;
  LChanged: TNyxText;
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LPair: TNyxProjectPair;
  LCommitted: TNyxText;
  LSelected: TNyxText;
begin
  ForceDirectories(AEvidenceRoot);
  LWindow := TForm.CreateNew(nil);
  LWindow.SetBounds(20, 20, 1240, 820);
  LStudio := nil;
  try
    LWindow.Show;
    LStudio := TNyxNativeStudio.Create(LWindow,
      IncludeTrailingPathDelimiter(AEvidenceRoot) + 'project');
    LStudio.CanvasView.RegisterFactory(NyxKindName(nkHeading), HeadingFace, UpdateHeading);
    { Own a precise ordinary authored seed rather than assume a starter recipe's
      projected control identity. No actual user project enters this fixture. }
    LDocument := TNyxDocument.Create;
    try
      LDocument.Title := 'Display recovery review';
      LPage := NewNyxPage('home');
      LDocument.AddPage(LPage);
      LPage.Add(NewNyxHeading('display-title').WithText('Make something wonderful.'));
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    finally
      LPage := nil;
      LDocument.Free;
    end;
    LStudio.LoadProject(LPair);
    LStudio.Run;
    AwaitStudio(LStudio);
    Check(LStudio.CanvasView.Root.Find('display-title') <> nil,
      'ordinary native projection exposes the exact owned heading identity');

    if LStudio.CodeView.Root = nil then
    begin
      Click(LStudio, 'action-code');
      AwaitStudio(LStudio);
    end;
    LCode := TCustomMemo(LStudio.CodeView.InputFor('studio-code'));
    LBefore := LStudio.Session.Source;
    LChanged := StringReplace(LBefore, 'Make something wonderful.',
      'A refreshed design.', [rfReplaceAll]);
    Check(LChanged <> LBefore, 'ordinary source owns the visible heading proposal');
    GRefuse := True;
    LCode.Text := LChanged;
    Click(LStudio, 'action-apply-source');
    AwaitStudio(LStudio);
    Check((LStudio.SourceCommands.State = nssApplied) and
      (LStudio.Session.Source = LChanged) and LStudio.Session.CanUndo and
      not LStudio.Session.ProjectSnapshot.Pending,
      'accepted Pascal and design commit together with one Undo available');
    Check((GRefusals > 0) and
      (LStudio.CanvasView.Root.Find('display-title').Prop('text') = 'Make something wonderful.') and
      (Pos('Intentional design display refusal', LStudio.Status) > 0),
      'real adapter refusal retains an older displayed canvas and reports the failure / refusals=' +
      IntToStr(GRefusals) + ' / text=' + LStudio.CanvasView.Root.Find('display-title').Prop('text') +
      ' / kind=' + LStudio.CanvasView.Root.Find('display-title').Kind +
      ' / projection=' + LStudio.CanvasView.Root.Find('display-title').ProjectionKind +
      ' / status=' + LStudio.Status);
    Check((LStudio.ShellView.Root.Find(NyxStudioDisplayRecoveryID) <> nil) and
      LStudio.ShellView.ControlFor(NyxStudioDisplayRecoveryID).IsVisible,
      'ordinary editor exposes its Nyx recovery notice');
    Check(TLabel(LStudio.SourceView.ControlFor('studio-source-status')).Caption =
      LStudio.SourceCommands.Message,
      'source status reports accepted admission independently of failed display');
    LCommitted := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    Check(not LStudio.CanvasView.ControlFor('home').Parent.Enabled,
      'stale canvas host disables target input without editing authored properties');
    LSelected := LStudio.Session.SelectedID;
    TControlAccess(LStudio.CanvasView.ControlFor('display-title')).Click;
    AwaitStudio(LStudio);
    Check((LStudio.Session.SelectedID = LSelected) and
      (EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LCommitted),
      'even a programmatic stale face click cannot edit the new accepted model');
    LWindow.Repaint;
    SaveNyxNativeCapture(LWindow,
      IncludeTrailingPathDelimiter(AEvidenceRoot) + 'native-stale.png', ncmPrint);
    Click(LStudio, NyxViewRecoveryRetryID(NyxStudioDisplayRecoveryID));
    AwaitStudio(LStudio);
    Check(LStudio.ShellView.ControlFor(NyxViewRecoveryRetryID(NyxStudioDisplayRecoveryID)).Enabled and
      LStudio.ShellView.ControlFor(NyxStudioDisplayRecoveryID).IsVisible and
      (EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LCommitted),
      'a refused retry remains available and preserves the exact pair');
    GRefuse := False;
    Click(LStudio, NyxViewRecoveryRetryID(NyxStudioDisplayRecoveryID));
    AwaitStudio(LStudio);
    Check((LStudio.CanvasView.Root.Find('display-title').Prop('text') = 'A refreshed design.') and
      not LStudio.ShellView.ControlFor(NyxStudioDisplayRecoveryID).IsVisible and
      LStudio.CanvasView.ControlFor('home').Parent.Enabled,
      'ordinary retry renders the admitted design and retires the notice');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LCommitted,
      'display retry preserves the exact accepted/source/draft pair');
    Click(LStudio, 'action-undo');
    AwaitStudio(LStudio);
    Check((LStudio.Session.Source = LPair.Source) and not LStudio.Session.CanUndo and
      (LStudio.Session.Save = LPair.Design),
      'one Undo still restores the complete exact original pair');
    LWindow.Repaint;

    SaveNyxNativeCapture(LWindow,
      IncludeTrailingPathDelimiter(AEvidenceRoot) + 'native-live.png', ncmPrint);
    WriteLn('PASS ', GChecks, ' actual display admission observations');
  finally
    GRefuse := False;
    LStudio.Free;
    LWindow.Free;
  end;
end;

begin
  Application.Initialize;
  try

    if ParamCount <> 1 then
    begin
      raise ENyxModel.Create('Supply an owned display recovery evidence directory');
    end;
    Run(TNyxText(ParamStr(1)));
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
