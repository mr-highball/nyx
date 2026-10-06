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
program nyx_responsive_studio;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, Interfaces, Forms, Controls, StdCtrls, Spin,
  nyx.text, nyx.types, nyx.model, nyx.codec, nyx.generated.view,
  nyx.studio.projects, nyx.studio.lcl, nyx.studio.inspector;

type
  TControlAccess = class(TControl);

var
  LStudio: TNyxNativeStudio;
  LForm: TForm;
  LDocument: TNyxDocument;
  LFile: TFileStream;
  LSource: TNyxText;
  LPair: TNyxProjectPair;
  LBefore: TNyxProjectPair;
  LMemo: TMemo;
  LNotify: TNotifyEvent;
  LMinimum: TSpinEdit;
  LMaximum: TSpinEdit;
  LHeightMaximum: TSpinEdit;
  LOrientation: TComboBox;
  LLayout: TComboBox;
  LChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
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

    if GetTickCount64 - LStarted > 15000 then
    begin
      raise Exception.Create('Studio responsive admission did not finish');
    end;
    Sleep(1);
  until False;
end;

begin

  if ParamCount <> 2 then
  begin
    raise Exception.Create('Supply private project directory and unchanged semantic companion');
  end;
  LFile := TFileStream.Create(ParamStr(2), fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LSource, LFile.Size);

    if Length(LSource) > 0 then
    begin
      LFile.ReadBuffer(LSource[1], Length(LSource));
    end;
  finally
    LFile.Free;
  end;
  LDocument := BuildNyxDocument;
  try
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LDocument.Free;
  end;
  Application.Initialize;
  { Report callback failures to qualification instead of opening an unattended
    widgetset exception dialog. The Studio still uses its ordinary worker. }
  Application.CaptureExceptions := False;
  LForm := TForm.CreateNew(nil);
  LForm.SetBounds(20, 20, 1280, 900);
  LForm.Show;
  LStudio := TNyxNativeStudio.Create(LForm, ParamStr(1));
  try
    LStudio.LoadProject(LPair);
    LStudio.Run;
    Pump;
    TControlAccess(LStudio.CanvasView.ControlFor('workspace')).Click;
    Pump;
    Check(LStudio.Session.SelectedID = 'workspace', 'Real canvas selection exposes its layout inspector');
    LMemo := TMemo(LStudio.CanvasView.InputFor('notes-editor'));
    LNotify := LMemo.OnChange;
    LMemo.OnChange := nil;
    try
      LMemo.Text := 'Retain this independent English draft.';
    finally
      LMemo.OnChange := LNotify;
    end;
    LMemo.SelStart := 5;
    LMemo.SelLength := 4;
    LMaximum := TSpinEdit(LStudio.ShellView.InputFor(NyxStudioViewportMaximumID));
    LMinimum := TSpinEdit(LStudio.ShellView.InputFor(NyxStudioViewportMinimumID));
    LMinimum.Value := 0;
    LMaximum.Value := 0;
    LHeightMaximum := TSpinEdit(LStudio.ShellView.InputFor(NyxStudioViewportHeightMaximumID));
    LHeightMaximum.Value := 300;
    LOrientation := TComboBox(LStudio.ShellView.InputFor(NyxStudioViewportOrientationID));
    LOrientation.ItemIndex := LOrientation.Items.IndexOf('landscape');

    if Assigned(LOrientation.OnChange) then
    begin
      LOrientation.OnChange(LOrientation);
    end;
    LLayout := TComboBox(LStudio.ShellView.InputFor(NyxStudioViewportLayoutID));
    LLayout.ItemIndex := LLayout.Items.IndexOf('row');

    if Assigned(LLayout.OnChange) then
    begin
      LLayout.OnChange(LLayout);
    end;
    Pump;
    LBefore := LStudio.Session.ProjectSnapshot;
    TControlAccess(LStudio.ShellView.ControlFor(NyxStudioViewportApplyID)).Click;
    Pump;
    Check(Pos('TNyxViewportCondition.Any.HeightBelow(300).Orientation(nvoLandscape)',
      LStudio.Session.ProjectSnapshot.Source) > 0,
      'Real Nyx Inspector button reaches the ordinary paired processor');
    Check(LStudio.Session.Document.Find('workspace').Prop(
      '@nyx.viewport-size:0:0:0:300:landscape:any:layout') = 'row',
      'The Inspector applies the captured enum choice');
    Check(LStudio.CanvasView.InputFor('notes-editor') = LMemo,
      'Responsive authoring retains the existing actual input');
    Check(LMemo.Text = 'Retain this independent English draft.', 'Responsive authoring retains independent input');
    Check((LMemo.SelStart = 5) and (LMemo.SelLength = 4), 'Responsive authoring retains exact range');
    TControlAccess(LStudio.ShellView.ControlFor('action-undo')).Click;
    Pump;
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'One actual editor Undo restores the exact paired document');
    Check(LStudio.CanvasView.InputFor('notes-editor') = LMemo,
      'Rule Undo retains the same mounted input');
    Check(LMemo.Text = 'Retain this independent English draft.', 'Rule Undo retains the independent draft');
    WriteLn('PASS ', LChecks, ' actual Studio responsive authoring checks');
  finally
    LStudio.Free;
    LForm.Free;
  end;
end.
