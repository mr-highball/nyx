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

program nyx_resize_studio;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, Interfaces, Forms, Controls, StdCtrls, Graphics,
  IntfGraphics, FPWritePNG, LCLType, nyx.text, nyx.types, nyx.model,
  nyx.designer.resize, nyx.gestures.lcl, nyx.studio.projects,
  nyx.studio.lcl, nyx.studio.resize, nyx.studio.sourcejobs, nyx.test.resize;

type
  TControlAccess = class(TControl);
  TWinControlAccess = class(TWinControl);
  TObserver = class
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AError: Exception);
  end;
var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LObserver: TObserver;
  LChecks: Integer;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LGrip: TControl;
  LCode: TControl;
  LMemo: TMemo;
  LSize: TNyxResizeSize;
  LMemoChange: TNotifyEvent;

procedure TObserver.Failed(ASender: TObject; AError: Exception);
begin
  Failure := TNyxText(AError.Message);
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Actual Studio resize: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure Pump;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;

    if LObserver.Failure <> '' then
    begin
      raise Exception.Create(LObserver.Failure);
    end;

    if not LStudio.PresentationPending and not LStudio.SourceCommands.Busy then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Size gesture publication did not finish');
    end;
    Sleep(1);
  until False;
end;

procedure Click(const AID: TNyxText);
begin
  TControlAccess(LStudio.ShellView.ControlFor(AID)).Click;
  Pump;
end;

procedure BeginGrip(const AID: TNyxText);
begin
  WriteLn('Studio resize / begin ', AID);
  Flush(Output);
  LGrip := LStudio.ShellView.ControlFor(AID);
  Check(LGrip <> nil, 'ordinary public grip is mounted');
  TControlAccess(LGrip).OnMouseDown(LGrip, mbLeft, [ssLeft], 5, 5);
end;

procedure MoveGrip(AX, AY: Integer; AShift: TShiftState = [ssLeft]);
begin
  TControlAccess(LGrip).OnMouseMove(LGrip, AShift, AX, AY);
end;

procedure EndGrip(AX, AY: Integer; AShift: TShiftState = []);
begin
  TControlAccess(LGrip).OnMouseUp(LGrip, mbLeft, AShift, AX, AY);
  Check(not NyxLCLHasPointerCapture(LGrip), 'real pointer-up releases capture');
  Pump;
  WriteLn('Studio resize / status ', LStudio.Status);
  Flush(Output);
end;

procedure Key(AKey: Word; AShift: TShiftState = []);
begin
  TWinControlAccess(LGrip).OnKeyDown(LGrip, AKey, AShift);
  Check(AKey = 0, 'handled resize keyboard shortcut consumes the native default');
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
    LBitmap.SetSize(LForm.ClientWidth, LForm.ClientHeight);
    LForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName, LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

begin
  try
    Application.Initialize;
    LObserver := TObserver.Create;
    Application.OnException := LObserver.Failed;
    LForm := TForm.CreateNew(nil);
    LForm.SetBounds(20, 20, 1280, 900);
    LForm.Show;
    LStudio := TNyxNativeStudio.Create(LForm,
      IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
    try
      LStudio.LoadProject(NyxResizeFixture);
      LStudio.Run;
      Pump;
      TControlAccess(LStudio.CanvasView.ControlFor('notes-editor')).Click;
      Pump;
      Check(LStudio.Session.SelectedID = 'notes-editor', 'actual canvas selects the authored memo');
      Click('action-code');
      LCode := LStudio.CodeView.InputFor('studio-code');
      LMemo := TMemo(LStudio.CanvasView.InputFor('notes-editor'));
      { Deliberately withhold a native value notification to qualify physical
        draft retention. This does not simulate or qualify a hardware IME. }
      LMemoChange := LMemo.OnChange;
      LMemo.OnChange := nil;
      try
        LMemo.Text := 'An uncommitted English control draft.';
      finally
        LMemo.OnChange := LMemoChange;
      end;
      LMemo.SelStart := 3;
      LMemo.SelLength := 5;
      LSize := LStudio.CanvasView.SizeFor('notes-editor', niDesign);
      Check((LSize.Width = 308) and (LSize.Height = 120),
        'public geometry reads allocated weight, not the stale authored width');
      LBefore := LStudio.Session.ProjectSnapshot;
      BeginGrip(NyxStudioResizeBothID);
      Check(NyxLCLHasPointerCapture(LGrip), 'real Win32 adapter captures the Nyx grip');
      MoveGrip(33, 40);
      Check((Pos('336', LStudio.Status) > 0) and (Pos('152', LStudio.Status) > 0),
        'ordinary status displays snapped dimensions before publication');
      Check(not LStudio.SourceCommands.Busy and
        (EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore)),
        'pointer preview creates no worker job or history');
      Check((LMemo.Text = 'An uncommitted English control draft.') and
        (LMemo.SelStart = 3) and (LMemo.SelLength = 5), 'preview preserves canvas input and selection');
      EndGrip(33, 40);
      Check((LStudio.Session.Selected.Prop('width') = '336') and
        (LStudio.Session.Selected.Prop('height') = '152') and
        (LStudio.Session.Selected.Prop('flex') = '0'), 'release publishes both dimensions as one edit');
      Check((LStudio.CanvasView.SizeFor('notes-editor').Width = 336) and
        (LStudio.CanvasView.SizeFor('notes-editor').Height = 152), 'accepted dimensions reach actual controls');
      Check(LStudio.CodeView.InputFor('studio-code') = LCode, 'paired resize retains Pascal editor identity');
      Check(LStudio.CanvasView.InputFor('notes-editor') = LMemo, 'paired resize retains canvas editor identity');
      Check((LMemo.Text = 'An uncommitted English control draft.') and
        (LMemo.SelStart = 3) and (LMemo.SelLength = 5), 'paired resize retains uncommitted canvas text');
      Check(Pos('handwritten English resize helper comment.', LStudio.Session.Source) > 0,
        'source helpers remain handcrafted');
      LAfter := LStudio.Session.ProjectSnapshot;
      Click('action-undo');
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'one ordinary Undo restores both files');
      Click('action-redo');
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LAfter),
        'one ordinary Redo restores both files');

      LBefore := LStudio.Session.ProjectSnapshot;
      BeginGrip(NyxStudioResizeWidthID);
      MoveGrip(90, 5);
      Key(VK_ESCAPE);
      EndGrip(90, 5);
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'Escape followed by release never publishes the canceled candidate');
      BeginGrip(NyxStudioResizeWidthID);
      EndGrip(5, 5);
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'tap/release is a true no-op');
      BeginGrip(NyxStudioResizeBothID);
      MoveGrip(500, 500);
      EndGrip(500, 500);
      Check((LStudio.Session.Selected.Prop('width') = '400') and
        (LStudio.Session.Selected.Prop('height') = '240'), 'explicit bounds clamp both axes');
      Click('action-undo');
      BeginGrip(NyxStudioResizeBothID);
      MoveGrip(18, 26, [ssLeft, ssAlt]);
      EndGrip(18, 26, [ssAlt]);
      Check((LStudio.Session.Selected.Prop('width') = '349') and
        (LStudio.Session.Selected.Prop('height') = '173'), 'Alt bypasses snapping through release');
      Click('action-undo');

      LGrip := LStudio.ShellView.ControlFor(NyxStudioResizeWidthID);
      Key(VK_RIGHT);
      Pump;
      Check((LStudio.Session.Selected.Prop('width') = '344') and
        (LStudio.Session.Selected.Prop('height') = '152'), 'width keyboard step retains the other dimension');
      Click('action-undo');
      LGrip := LStudio.ShellView.ControlFor(NyxStudioResizeHeightID);
      Key(VK_DOWN, [ssShift]);
      Pump;
      Check((LStudio.Session.Selected.Prop('width') = '336') and
        (LStudio.Session.Selected.Prop('height') = '232'), 'Shift keyboard step uses the public policy');
      Click('action-undo');

      LBefore := LStudio.Session.ProjectSnapshot;
      BeginGrip(NyxStudioResizeBothID);
      MoveGrip(25, 25);
      LStudio.Session.SetTitle('Changed while resizing');
      LAfter := LStudio.Session.ProjectSnapshot;
      EndGrip(25, 25);
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LAfter),
        'accepted source/design change permanently cancels the old pair lease');
      LStudio.Session.Undo;
      LStudio.Session.SetSourceDraft(LStudio.Session.Source + #10 + '{ Pending English source draft. }');
      LBefore := LStudio.Session.ProjectSnapshot;
      BeginGrip(NyxStudioResizeWidthID);
      EndGrip(45, 5);
      Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'pending Pascal draft prevents new gestures');
      LStudio.Session.DiscardSourceDraft;
      LStudio.Session.Select('notes-editor');
      LStudio.Run;
      Pump;
      Capture('resize-desktop.png');
      LForm.ClientWidth := 390;
      LForm.ClientHeight := 900;
      Pump;
      Click('action-panel-inspector');
      Capture('resize-compact.png');
      Check(LStudio.ShellView.ControlFor(NyxStudioResizeBothID) <> nil,
        'compact Inspector retains reusable touch-sized grips');
    finally
      LStudio.Free;
      LForm.Free;
      Application.OnException := nil;
      LObserver.Free;
    end;
    WriteLn('PASS ', LChecks, ' actual Win32 Studio resize checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
