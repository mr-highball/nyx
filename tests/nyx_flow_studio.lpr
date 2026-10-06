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
program nyx_flow_studio;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Types,
  Graphics, IntfGraphics, FPWritePNG, nyx.text, nyx.model, nyx.codec,
  nyx.generated.view, nyx.studio.projects, nyx.studio.lcl, nyx.studio.drag;

type
  TControlAccess = class(TControl);

var
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GEditor: TControl;
  GChecks: Integer;
  GSource: TControl;
  GTarget: TControl;
  GDrag: TDragObject;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Native flow placement: ' + AReason);
  end;
  Inc(GChecks);
  WriteLn('CHECK ', GChecks, ' / ', AReason);
  Flush(Output);
end;

procedure Pump;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Application.ProcessMessages;

    if not GStudio.PresentationPending and not GStudio.SourceCommands.Busy then
    begin
      Exit;
    end;

    if GetTickCount64 - LStarted > 30000 then
    begin
      raise Exception.Create('Ordinary flow preparation did not finish');
    end;
    Sleep(1);
  until False;
end;

procedure Click(const AID: TNyxText);
begin
  TControlAccess(GStudio.ShellView.ControlFor(AID)).Click;
  Pump;
end;

procedure Select(const AID: TNyxText);
begin
  TControlAccess(GStudio.CanvasView.ControlFor(AID)).Click;
  Pump;
end;

procedure Automatic;
var
  LChoice: TComboBox;
begin
  LChoice := TComboBox(GStudio.ShellView.InputFor(NyxStudioDropPositionID));
  LChoice.ItemIndex := LChoice.Items.IndexOf(String(NyxStudioAutomaticPlacement));
  Check(LChoice.ItemIndex >= 0, 'automatic is an ordinary Nyx select choice');
  LChoice.OnChange(LChoice);
end;

function IndexInParent(const AID: TNyxText): Integer;
var
  LNode: TNyxNode;
  LIndex: Integer;
begin
  Result := -1;
  LNode := GStudio.Session.Document.Find(AID);

  if (LNode <> nil) and (LNode.Parent <> nil) then
  begin
    for LIndex := 0 to LNode.Parent.Count - 1 do
    begin

      if LNode.Parent.Children[LIndex] = LNode then
      begin
        Exit(LIndex);
      end;
    end;
  end;
end;

function Ink(AParent: TWinControl): TPanel;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin

    if AParent.Controls[LIndex].Name = 'NyxResizeEdge0' then
    begin
      Exit(TPanel(AParent.Controls[LIndex]));
    end;

    if AParent.Controls[LIndex] is TWinControl then
    begin
      Result := Ink(TWinControl(AParent.Controls[LIndex]));

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
end;

{ Real registered LCL source/target slots. This qualifies the host callbacks,
  not physical hardware, native hit-testing or another widgetset's drag manager. }
procedure Start(const ASource, ATarget: TNyxText; AX, AY: Integer;
  AExpected: Boolean = True);
var
  LAccept: Boolean;
  LBefore: TNyxProjectPair;
begin
  GSource := GStudio.ShellView.ControlFor(ASource);
  GTarget := GStudio.CanvasView.ControlFor(ATarget);
  Check((GSource <> nil) and (GTarget <> nil), 'both actual controls are mounted');
  GDrag := nil;
  LBefore := GStudio.Session.ProjectSnapshot;
  TControlAccess(GSource).OnStartDrag(GSource, GDrag);
  Check(GDrag <> nil, 'source offers a local native lease');
  LAccept := False;
  TControlAccess(GTarget).OnDragOver(GTarget, GDrag, AX, AY, dsDragEnter, LAccept);
  Check(LAccept = AExpected, 'actual enter applies the automatic ownership policy');
  TControlAccess(GTarget).OnDragOver(GTarget, GDrag, AX, AY, dsDragMove, LAccept);
  Check(LAccept = AExpected, 'actual over retains that agreement');
  Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
    'preview changes neither accepted file');
  Check(GStudio.CodeView.InputFor('studio-code') = GEditor, 'preview retains source editor identity');
end;

procedure Finish(AX, AY: Integer; ADrop: Boolean = True);
var
  LSource: TControl;
begin
  LSource := GSource;
  try

    if ADrop then
    begin
      TControlAccess(GTarget).OnDragDrop(GTarget, GDrag, AX, AY);
    end;
    TControlAccess(LSource).OnEndDrag(LSource, GTarget, AX, AY);
  finally
    FreeAndNil(GDrag);
  end;
  Pump;
end;

procedure Capture;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LPoint: TPoint;
  LInk: TPanel;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GForm.Width, GForm.Height);
    GForm.PaintTo(LBitmap.Canvas, 0, 0);
    GStudio.CanvasView.PaintResizePreview(LBitmap.Canvas, Point(GForm.Left, GForm.Top));
    LInk := Ink(GForm);
    LPoint := LInk.ClientToScreen(Point(1, 1));
    Dec(LPoint.X, GForm.Left);
    Dec(LPoint.Y, GForm.Top);
    Check(ColorToRGB(LBitmap.Canvas.Pixels[LPoint.X, LPoint.Y]) = ColorToRGB(LInk.Color),
      'actual insertion strip paints its accent pixel');
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ExtractFileDir(ExpandFileName(ParamStr(1)))) +
      'flow-native.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

var
  LDocument: TNyxDocument;
  LStream: TFileStream;
  LSource: TNyxText;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LOriginalLeft: Integer;
begin
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Supply private projects and unchanged semantic companion');
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
      LBefore := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    finally
      LDocument.Free;
    end;
    Application.Initialize;
    Application.CaptureExceptions := False;
    GForm := TForm.CreateNew(nil);
    GForm.SetBounds(20, 20, 1280, 900);
    GForm.Show;
    GStudio := TNyxNativeStudio.Create(GForm, ParamStr(1));
    try
      WriteLn('Loading semantic companion');
      Flush(Output);
      GStudio.LoadProject(LBefore);
      WriteLn('Mounting ordinary Studio');
      Flush(Output);
      GStudio.Run;
      WriteLn('Pumping initial presentation');
      Flush(Output);
      Pump;
      Click('action-code');
      GEditor := GStudio.CodeView.InputFor('studio-code');
      Check(GEditor <> nil, 'source editor is realized');
      Select('notes-editor');
      Automatic;
      Start(NyxStudioDragMoveID, 'reference-button', 40, 2);
      Check((Pos('Drop before', GStudio.Status) > 0) and Ink(GForm).Visible and
        (Ink(GForm).Height = 3), 'actual column target paints a before insertion line');
      Capture;
      Finish(40, 2);
      Check((GStudio.Session.Document.Find('notes-editor').Parent.ID = 'right-layout') and
        (IndexInParent('notes-editor') < IndexInParent('reference-button')),
        'ordinary isolated publication reparents before the chosen sibling');
      Check(GStudio.Session.Document.Find('notes-editor').Prop('value') = 'Keep this English draft.',
        'reparenting retains the accepted memo content');
      LAfter := GStudio.Session.ProjectSnapshot;
      Click('action-undo');
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'one Undo restores both exact files');
      Click('action-redo');
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LAfter),
        'one Redo restores both exact files');
      Click('action-undo');
      Select('notes-editor');
      Start(NyxStudioDragMoveID, 'reference-button', 40, 2);
      LOriginalLeft := GTarget.Left;
      GTarget.Left := LOriginalLeft + 1;
      Finish(40, 2);
      GTarget.Left := LOriginalLeft;
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'a changed actual physical face cancels before publication');
      Start(NyxStudioDragMoveID, 'reference-button', 40, 2);
      Finish(40, 2, False);
      Check(not Ink(GForm).Visible and
        (EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore)),
        'ending without a drop clears paint and preserves history');
      Select('left-layout');
      Start(NyxStudioDragMoveID, 'right-layout', GStudio.CanvasView.ControlFor('right-layout').Width - 2, 100);
      Check((Pos('Drop after', GStudio.Status) > 0) and (Ink(GForm).Width = 3),
        'actual row container paints a horizontal trailing insertion line');
      Finish(GTarget.Width - 2, 100);
      Check(IndexInParent('left-layout') > IndexInParent('right-layout'), 'automatic row drop reorders siblings');
      Click('action-undo');
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'row reordering has one exact paired Undo');
      Select('notes-editor');
      Start(NyxStudioDragMoveID, 'shared-instance/shared-action', 20, 2, False);
      Finish(20, 2, False);
      Check(EncodeNyxProject(GStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
        'inherited primitive cannot silently acquire authored siblings');
      Check(GStudio.CodeView.InputFor('studio-code') = GEditor, 'all gestures/history retain source control');
      WriteLn('PASS ', GChecks, ' actual Win32 flow Studio checks');
    finally
      FreeAndNil(GDrag);
      GStudio.Free;
      GForm.Free;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
