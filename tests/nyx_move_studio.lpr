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
program nyx_move_studio;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Types,
  Graphics, IntfGraphics, FPWritePNG, LCLType, nyx.designer.resize, nyx.designer.move,
  nyx.text, nyx.types, nyx.model, nyx.codec, nyx.generated.view,
  nyx.studio.projects, nyx.studio.lcl, nyx.studio.resize, nyx.studio.move;

type
  TControlAccess = class(TControl);
  TWinControlAccess = class(TWinControl);
var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LDocument: TNyxDocument;
  LStream: TFileStream;
  LSource: TNyxText;
  LPair, LBefore, LAfter: TNyxProjectPair;
  LMemo: TMemo;
  LNotify: TNotifyEvent;
  LGrip: TControl;
  LChecks: Integer;
  LGripOrigin, LPoint: TPoint;
  LKey: Word;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create('Native move journey: ' + AReason);
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

    if not LStudio.PresentationPending and not LStudio.SourceCommands.Busy then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Guide publication did not finish');
    end;
    Sleep(1);
  until False;
end;

procedure Click(const AID: TNyxText);
begin
  TControlAccess(LStudio.ShellView.ControlFor(AID)).Click;
  Pump;
end;

function Ink(AParent: TWinControl; AIndex: Integer): TControl;
var
  LIndex: Integer;
  LChild: TControl;
begin
  Result := nil;
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin
    LChild := AParent.Controls[LIndex];

    if LChild.Name = 'NyxResizeEdge' + IntToStr(AIndex) then
    begin
      Exit(LChild);
    end;

    if LChild is TWinControl then
    begin
      Result := Ink(TWinControl(LChild), AIndex);

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure Start;
begin
  LBefore := LStudio.Session.ProjectSnapshot;
  LGrip := LStudio.CanvasView.CanvasMoveControl;
  Check(LGrip.Visible and TWinControl(LGrip).CanFocus,
    'the actual canvas move grip is visible and focusable');
  LGripOrigin := LGrip.ClientToScreen(Point(5, 5));
  TControlAccess(LGrip).OnMouseDown(LGrip, mbLeft, [ssLeft], 5, 5);
end;

procedure Move(AX, AY: Integer; AAlt: Boolean = False);
var
  LShift: TShiftState;
begin
  LShift := [ssLeft];

  if AAlt then
  begin
    Include(LShift, ssAlt);
  end;
  LPoint := LGrip.ScreenToClient(Point(LGripOrigin.X + AX, LGripOrigin.Y + AY));
  TControlAccess(LGrip).OnMouseMove(LGrip, LShift, LPoint.X, LPoint.Y);
end;

procedure Finish(AX, AY: Integer; AAlt: Boolean = False);
var
  LShift: TShiftState;
begin
  LShift := [];

  if AAlt then
  begin
    Include(LShift, ssAlt);
  end;
  LPoint := LGrip.ScreenToClient(Point(LGripOrigin.X + AX, LGripOrigin.Y + AY));
  TControlAccess(LGrip).OnMouseUp(LGrip, mbLeft, LShift, LPoint.X, LPoint.Y);
  Pump;
end;

procedure Retained;
begin
  Check(LStudio.CanvasView.InputFor('notes-editor') = LMemo, 'same native input survives');
  Check((LMemo.Text = 'Independent English draft.') and (LMemo.SelStart = 5) and
    (LMemo.SelLength = 4), 'live text and exact range survive');
end;

procedure CaptureGuide;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LPoint: TPoint;
  LGuide: TControl;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    { Win32 PaintTo includes its caption/frame. Compose every real guide in
      the same window plane, rather than mixing client and window origins. }
    LBitmap.SetSize(LForm.Width, LForm.Height);
    LForm.PaintTo(LBitmap.Canvas, 0, 0);
    LStudio.CanvasView.PaintResizePreview(LBitmap.Canvas, Point(LForm.Left, LForm.Top));
    LGuide := Ink(LForm, 4);
    LPoint := LGuide.ClientToScreen(Point(LGuide.Width div 2, 0));
    Dec(LPoint.X, LForm.Left);
    Dec(LPoint.Y, LForm.Top);
    Check(ColorToRGB(LBitmap.Canvas.Pixels[LPoint.X, LPoint.Y]) =
      ColorToRGB(TPanel(LGuide).Color), 'actual native guide paints its accent pixel');
    LImage := TLazIntfImage.Create(0, 0);
    LImage.LoadFromBitmap(LBitmap.Handle, LBitmap.MaskHandle);
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ExtractFileDir(ExpandFileName(ParamStr(1)))) +
      'move-native.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

begin

  if ParamCount <> 2 then
  begin
    raise Exception.Create('Supply private project directory and unchanged MCP source');
  end;
  LStream := TFileStream.Create(ParamStr(2), fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LSource, LStream.Size);

    if Length(LSource) > 0 then
    begin
      LStream.ReadBuffer(LSource[1], Length(LSource));
    end;
  finally
    LStream.Free;
  end;
  LDocument := BuildNyxDocument;
  try
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LDocument.Free;
  end;
  Application.Initialize;
  Application.CaptureExceptions := False;
  LForm := TForm.CreateNew(nil);
  LForm.SetBounds(20, 20, 1280, 900);
  LForm.Show;
  LStudio := TNyxNativeStudio.Create(LForm, ParamStr(1));
  try
    LStudio.LoadProject(LPair);
    LStudio.Run;
    Pump;
    TControlAccess(LStudio.CanvasView.ControlFor('notes-editor')).Click;
    Pump;
    LMemo := TMemo(LStudio.CanvasView.InputFor('notes-editor'));
    LNotify := LMemo.OnChange;
    LMemo.OnChange := nil;
    try
      LMemo.Text := 'Independent English draft.';
    finally
      LMemo.OnChange := LNotify;
    end;
    LMemo.SelStart := 5;
    LMemo.SelLength := 4;
    Start;
    Move(23, 36);
    Check((Pos('40, 70 px', LStudio.Status) > 0) and
      (Pos('aligns with other-editor', LStudio.Status) > 0), 'actual canvas pointer maps and snaps both origins');
    Check(Ink(LForm, 4).Visible and (Ink(LForm, 4).Width = 1) and
      Ink(LForm, 6).Visible and (Ink(LForm, 6).Height = 1), 'both real alignment lines paint');
    CaptureGuide;
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'preview retains the exact accepted pair');
    Retained;
    Finish(23, 36);
    Check((LStudio.Session.Selected.Prop('left') = '40') and
      (LStudio.Session.Selected.Prop('top') = '70'), 'ordinary isolated processor commits both snapped origins');
    Check((LStudio.Session.Selected.Prop('width') = '200') and
      (LStudio.Session.Selected.Prop('height') = '120'), 'movement retains authored dimensions');
    Retained;
    LAfter := LStudio.Session.ProjectSnapshot;
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore), 'one Undo restores exact pair');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LAfter), 'one Redo restores exact pair');
    Click('action-undo');
    Start;
    Move(23, 36, True);
    Check(not Ink(LForm, 4).Visible and not Ink(LForm, 6).Visible, 'Alt removes guide ink');
    Finish(23, 36, True);
    Check((LStudio.Session.Selected.Prop('left') = '43') and
      (LStudio.Session.Selected.Prop('top') = '66'), 'Alt keeps the exact unsnapped origin');
    Click('action-undo');
    Start;
    Move(23, 36);
    LKey := VK_ESCAPE;
    TWinControlAccess(LGrip).OnKeyDown(LGrip, LKey, []);
    Finish(23, 36);
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'Escape cancels the gesture before release without history');
    LGrip := LStudio.CanvasView.CanvasMoveControl;
    LKey := VK_RIGHT;
    TWinControlAccess(LGrip).OnKeyDown(LGrip, LKey, []);
    Pump;
    Check((LStudio.Session.Selected.Prop('left') = '28') and
      (LStudio.Session.Selected.Prop('top') = '30'), 'arrow moves by its exact step from an off-grid origin');
    Click('action-undo');
    LForm.ClientWidth := 390;
    LStudio.Run;
    Pump;
    Click('action-panel-design');
    Check(LStudio.CanvasView.CanvasMoveControl <> nil,
      'compact native Design retains its public move grip with Inspector hidden');
    Retained;
    WriteLn('PASS ', LChecks, ' actual Win32 Studio move checks');
  finally
    LStudio.Free;
    LForm.Free;
  end;
end.
