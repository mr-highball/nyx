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

program nyx_studio_menu_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, LCLType,
  nyx.text, nyx.model, nyx.codec, nyx.studio.projects, nyx.studio.lcl,
  nyx.studio.help, nyx.studio.menu, nyx.split.lcl, nyx.generated.view;

type
  TControlAccess = class(TWinControl);
  TGripAccess = class(TNyxLCLSplitGrip);

function ReadSource(const APath: String): TNyxText;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size = 0) or (LFile.Size > 1048576) then
    begin
      raise Exception.Create('Companion exceeds source fixture budget');
    end;
    SetLength(Result, LFile.Size);
    LFile.ReadBuffer(Result[1], Length(Result));
  finally
    LFile.Free;
  end;
end;

function MenuWindow(const ATitle: String = 'Component actions'): TForm;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to Screen.FormCount - 1 do
  begin

    if Screen.Forms[LIndex].Visible and
      (Screen.Forms[LIndex].Caption = ATitle) then
    begin
      Exit(Screen.Forms[LIndex]);
    end;
  end;
end;

{ Borrow a physically mounted ordinary Nyx button by its English caption. The
  presenter owns the tree; the fixture releases no native control independently. }
function Button(AParent: TWinControl; const ACaption: String): TWinControl;
var
  LIndex: Integer;
  LChild: TControl;
begin
  Result := nil;
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin
    LChild := AParent.Controls[LIndex];

    if (LChild is TWinControl) and
      ((TControlAccess(LChild).Caption = ACaption) or
        (Pos(ACaption + ' ', TControlAccess(LChild).Caption) = 1)) then
    begin
      Exit(TWinControl(LChild));
    end;

    if LChild is TWinControl then
    begin
      Result := Button(TWinControl(LChild), ACaption);

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure Settle(AStudio: TNyxNativeStudio);
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;
    CheckSynchronize;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Native Studio did not finish its presentation');
    end;
    Sleep(1);
  until not AStudio.PresentationPending;
end;

procedure Require(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

var
  LForm: TForm;
  LStudio: TNyxNativeStudio;
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LBefore: TNyxText;
  LHelp: TForm;
  LCanvasHeight: Integer;
  LSplit: TNyxLCLSplitView;
  LKey: Word;
begin
  LForm := nil;
  LStudio := nil;
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Use native Studio menu <exact companion.pas> <fixture directory>');
    end;
    Application.Initialize;
    LForm := TForm.CreateNew(nil);
    LForm.SetBounds(30, 30, 1280, 940);
    LForm.Show;
    LStudio := TNyxNativeStudio.Create(LForm, ParamStr(2));
    LDocument := BuildNyxDocument;
    try
      LSource := ReadSource(ParamStr(1));
      LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), LSource));
    finally
      LDocument.Free;
    end;
    LStudio.Session.Select('menu-cut');
    LStudio.Run;
    Settle(LStudio);
    Require(LStudio.ShellView.ControlFor(NyxStudioActionMenuID) <> nil,
      'Ordinary Studio mounts its public action menu');
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    TControlAccess(LStudio.ShellView.ControlFor(NyxStudioActionMenuID)).Click;
    Application.ProcessMessages;
    LHelp := MenuWindow;
    Require((LHelp <> nil) and LForm.Enabled, 'Actual native Studio opens a nonmodal menu');
    Require((Screen.ActiveControl <> nil) and
      (TControlAccess(Screen.ActiveControl).Caption = 'Undo'),
      'Disabled Undo remains the initial keyboard focus');
    TControlAccess(Screen.ActiveControl).Click;
    Require((MenuWindow <> nil) and not LStudio.Session.CanUndo,
      'Disabled Undo cannot dispatch an editor operation');
    Require(Button(LHelp, 'Inspect') <> nil,
      'Actual menu exposes its typed Inspector branch');
    TControlAccess(Button(LHelp, 'Inspect')).Click;
    Application.ProcessMessages;
    LHelp := MenuWindow('Inspect');
    Require((LHelp <> nil) and (MenuWindow <> nil), 'Parent remains open beside the native child');
    TControlAccess(Button(LHelp, 'Events')).Click;
    Settle(LStudio);
    Require((MenuWindow = nil) and (LStudio.ShellView.ControlFor('event-click-add') <> nil),
      'Typed menu command reaches the ordinary event Inspector');
    TControlAccess(LStudio.ShellView.ControlFor(NyxStudioActionMenuID)).Click;
    LHelp := MenuWindow;
    Require((LHelp <> nil) and (Button(LHelp, 'Inspect') <> nil),
      'Menu can reopen after the Inspector refresh');
    TControlAccess(Button(LHelp, 'Inspect')).Click;
    Application.ProcessMessages;
    LHelp := MenuWindow('Inspect');
    Require(LHelp <> nil, 'Inspector child can reopen independently');
    TControlAccess(Button(LHelp, 'About this component')).Click;
    Application.ProcessMessages;
    Require((MenuWindow = nil) and (Screen.ActiveControl <> nil) and
      (TControlAccess(Screen.ActiveControl).Caption = 'Close'),
      'Menu completion opens public contextual component help');
    TControlAccess(Screen.ActiveControl).Click;
    Application.ProcessMessages;
    Require(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Menu navigation preserves the exact accepted project/source pair');
    { The ordinary native controller consumes the same available-space policy.
      Small native hosts qualify allocation without claiming phone hardware. }
    LForm.SetBounds(30, 30, 390, 740);
    Application.ProcessMessages;
    Settle(LStudio);
    Require((LStudio.ShellView.ControlFor('studio-panelbar') <> nil) and
      not LStudio.ShellView.ControlFor('action-build-app').Visible,
      'Compact native Studio moves advanced chrome into its public menu');
    TControlAccess(LStudio.ShellView.ControlFor('action-panel-design')).Click;
    Settle(LStudio);
    LCanvasHeight := LStudio.ShellView.ControlFor('studio-canvas-wrap').Height;
    WriteLn('Native compact canvas / ', LCanvasHeight, ' of ', LForm.ClientHeight);
    Require(LCanvasHeight > LForm.ClientHeight * 0.50,
      'Compact native canvas retains at least half of the host');
    TControlAccess(LStudio.ShellView.ControlFor('action-canvas-expand')).Click;
    Settle(LStudio);
    WriteLn('Native expanded canvas / ',
      LStudio.ShellView.ControlFor('studio-canvas-wrap').Height, ' of ', LForm.ClientHeight);
    Require(LStudio.ShellView.ControlFor('studio-canvas-wrap').Height >=
      LForm.ClientHeight * 0.85, 'Expanded native design receives the available host');
    TControlAccess(LStudio.ShellView.ControlFor('action-workspace-restore')).Click;
    Settle(LStudio);
    Require(LStudio.ShellView.ControlFor('studio-canvas-wrap').Height = LCanvasHeight,
      'Restore returns the previous native allocation');
    TControlAccess(LStudio.ShellView.ControlFor(NyxStudioActionMenuID)).Click;
    Application.ProcessMessages;
    LHelp := MenuWindow;
    Require((LHelp <> nil) and (Button(LHelp, 'Project') <> nil),
      'Workspace project branch is physically mounted');
    TControlAccess(Button(LHelp, 'Project')).Click;
    Application.ProcessMessages;
    LHelp := MenuWindow('Project');
    Require((LHelp <> nil) and (Button(LHelp, 'Agents and sync') <> nil),
      'Compact native menu exposes optional workspace tools');
    TControlAccess(Button(LHelp, 'Agents and sync')).Click;
    Settle(LStudio);
    Require(LStudio.ShellView.ControlFor('studio-details-split') <> nil,
      'Native details mount in the shared public resizable split');
    LSplit := TNyxLCLSplitView(LStudio.ShellView.ControlFor('studio-details-split'));
    LCanvasHeight := LStudio.ShellView.ControlFor('studio-canvas-wrap').Height;
    LKey := VK_HOME;
    TGripAccess(LSplit.Grip).KeyDown(LKey, []);
    Application.ProcessMessages;
    Require((LKey = 0) and
      (LStudio.ShellView.Root.Find('studio-details-split').Prop('split-position') = '15'),
      'Native detail grip consumes Home and publishes its typed resize');
    Require(LStudio.ShellView.ControlFor('studio-canvas-wrap').Height > LCanvasHeight,
      'Native keyboard grip gives space back to the design');
    TControlAccess(LStudio.ShellView.ControlFor('action-details-toggle')).Click;
    Settle(LStudio);
    Require(LStudio.ShellView.Root.Find('studio-details-split') = nil,
      'Native details collapse without replacing the design');
    Require(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Native workspace allocation preserves the exact accepted pair');
    WriteLn('PASS 22 ordinary native Studio menu/workspace checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
  LStudio.Free;
  LForm.Free;
end.
