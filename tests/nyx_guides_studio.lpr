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
program nyx_guides_studio;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Types,
  Graphics, IntfGraphics, FPWritePNG, nyx.designer.resize,
  nyx.text, nyx.types, nyx.model, nyx.codec, nyx.generated.view,
  nyx.studio.projects, nyx.studio.lcl, nyx.studio.resize;

type
  TControlAccess = class(TControl);
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

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create('Native guide journey: ' + AReason);
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
  LGrip := LStudio.ShellView.ControlFor(NyxStudioResizeBothID);
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
  TControlAccess(LGrip).OnMouseMove(LGrip, LShift, 5 + AX, 5 + AY);
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
  TControlAccess(LGrip).OnMouseUp(LGrip, mbLeft, LShift, 5 + AX, 5 + AY);
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
      'guides-native.png', LWriter);
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
    Move(14, 22);
    Check((Pos('217', LStudio.Status) > 0) and (Pos('143', LStudio.Status) > 0) and
      (Pos('matches other-editor', LStudio.Status) > 0), 'actual siblings supply both guide matches');
    Check(Ink(LForm, 4).Visible and Ink(LForm, 5).Visible and
      (Ink(LForm, 4).Width = 217) and (Ink(LForm, 5).Width = 217),
      'equal widths paint two real native measurement faces');
    Check(not Ink(LForm, 4).Enabled and not TWinControl(Ink(LForm, 4)).TabStop,
      'guide paint cannot intercept input or keyboard focus');
    CaptureGuide;
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'preview never writes the accepted pair');
    Retained;
    Finish(14, 22);
    Check((LStudio.Session.Selected.Prop('width') = '217') and
      (LStudio.Session.Selected.Prop('height') = '143'), 'ordinary processor publishes snapped dimensions');
    Check(not Ink(LForm, 4).Visible, 'release clears transient geometry');
    Retained;
    LAfter := LStudio.Session.ProjectSnapshot;
    Click('action-undo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore), 'one Undo restores exact pair');
    Click('action-redo');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LAfter), 'one Redo restores exact pair');
    Click('action-undo');
    Start;
    Move(27, 0);
    Check(Pos('aligns with reference-button', LStudio.Status) > 0, 'absolute layout supports a real positive-edge guide');
    Check(Ink(LForm, 4).Visible and (Ink(LForm, 4).Width = 1) and not Ink(LForm, 5).Visible,
      'edge guide paints one actual line');
    Finish(27, 0);
    Check(LStudio.Session.Selected.Prop('width') = '230', 'edge alignment publishes its exact bounded size');
    Click('action-undo');
    Start;
    Move(14, 22, True);
    Check(not Ink(LForm, 4).Visible and not Ink(LForm, 6).Visible, 'Alt removes guide ink');
    Finish(14, 22, True);
    Check((LStudio.Session.Selected.Prop('width') = '214') and
      (LStudio.Session.Selected.Prop('height') = '142'), 'Alt retains unrounded dimensions');
    Click('action-undo');
    Start;
    Move(14, 22);
    LForm.ClientWidth := 1100;
    Application.ProcessMessages;
    Finish(14, 22);
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'changed neighboring host geometry refuses the captured gesture');
    Retained;
    LForm.ClientWidth := 390;
    LStudio.Run;
    Pump;
    Click('action-panel-design');
    Check(LStudio.CanvasView.CanvasResizeControl(nraBoth) <> nil,
      'compact native Design retains canvas grips with its Inspector hidden');
    Retained;
    WriteLn('PASS ', LChecks, ' actual Win32 Studio alignment checks');
  finally
    LStudio.Free;
    LForm.Free;
  end;
end.
