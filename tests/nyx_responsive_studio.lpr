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
  nyx.text, nyx.types, nyx.model, nyx.codec, nyx.generated.view, nyx.presentations,
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
    WriteLn('Native presentation: load project');
    Flush(Output);
    LStudio.LoadProject(LPair);
    WriteLn('Native presentation: mount Studio');
    Flush(Output);
    LStudio.Run;
    WriteLn('Native presentation: settle initial presentation');
    Flush(Output);
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
    {$ifdef NYX_PRESENTATION_CONSUMER}
    WriteLn('Native presentation: update definition');
    Flush(Output);
    LBefore := LStudio.Session.ProjectSnapshot;
    TEdit(LStudio.ShellView.InputFor(NyxStudioPresentationNameID)).Text := 'compact';
    TSpinEdit(LStudio.ShellView.InputFor(NyxStudioViewportMaximumID)).Value := 900;
    TSpinEdit(LStudio.ShellView.InputFor(NyxStudioViewportHeightMaximumID)).Value := 0;
    LOrientation := TComboBox(LStudio.ShellView.InputFor(NyxStudioViewportOrientationID));
    LOrientation.ItemIndex := LOrientation.Items.IndexOf('any');
    TControlAccess(LStudio.ShellView.ControlFor(NyxStudioPresentationDefineID)).Click;
    Pump;
    Check(LStudio.Session.Document.Presentations.Condition(NyxPresentation('compact')).WidthMaximum = 900,
      'Actual native Inspector updates one shared definition through its paired processor');
    Check(LStudio.CanvasView.InputFor('notes-editor') = LMemo, 'Shared definition editing retains the native Studio input');
    Check(LMemo.Text = 'Retain this independent English draft.', 'Shared definition editing retains native Studio text');
    Check((LMemo.SelStart = 5) and (LMemo.SelLength = 4), 'Shared definition editing retains native Studio range');
    TControlAccess(LStudio.ShellView.ControlFor('action-undo')).Click;
    Pump;
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'One actual native Undo restores the shared definition and its exact source');
    WriteLn('Native presentation: add override');
    Flush(Output);
    LLayout := TComboBox(LStudio.ShellView.InputFor(NyxStudioPresentationAttributeID));
    LLayout.ItemIndex := LLayout.Items.IndexOf('visible');

    if Assigned(LLayout.OnChange) then
    begin
      LLayout.OnChange(LLayout);
    end;
    TControlAccess(LStudio.ShellView.ControlFor(NyxStudioPresentationUseID)).Click;
    WriteLn('Native presentation: waiting for add');
    Flush(Output);
    Pump;
    Check(LStudio.Session.Selected.Props.IndexOfName(
      NyxPresentationKey(NyxPresentation('compact'), npfAny, atVisible)) >= 0,
      'Actual native Inspector adds a typed presentation override');
    WriteLn('Native presentation: reset override');
    Flush(Output);
    LLayout := TComboBox(LStudio.ShellView.InputFor(NyxStudioPresentationAttributeID));
    LLayout.ItemIndex := LLayout.Items.IndexOf('visible');

    if Assigned(LLayout.OnChange) then
    begin
      LLayout.OnChange(LLayout);
    end;
    TControlAccess(LStudio.ShellView.ControlFor(NyxStudioPresentationResetID)).Click;
    Pump;
    Check(LStudio.Session.Selected.Props.IndexOfName(
      NyxPresentationKey(NyxPresentation('compact'), npfAny, atVisible)) < 0,
      'Actual native Inspector resets only the requested presentation override');
    {$endif}
    {$ifdef NYX_MANUAL_CONSUMER}
    LBefore := LStudio.Session.ProjectSnapshot;
    LLayout := TComboBox(LStudio.ShellView.InputFor(NyxStudioPresentationPreviewID));
    LLayout.ItemIndex := LLayout.Items.IndexOf('Manual / wide workspace');
    LLayout.OnChange(LLayout);
    Pump;
    Check(LStudio.CanvasView.ControlFor('notes-editor').Top =
      LStudio.CanvasView.ControlFor('other-editor').Top, 'Actual native preview chooser selects the manual row');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'Manual preview switching writes no paired source or history');
    LLayout := TComboBox(LStudio.ShellView.InputFor(NyxStudioPresentationPreviewID));
    LLayout.ItemIndex := LLayout.Items.IndexOf('Manual / focused');
    LLayout.OnChange(LLayout);
    Pump;
    Check(LStudio.CanvasView.ControlFor('other-editor').Top >
      LStudio.CanvasView.ControlFor('notes-editor').Top, 'Another actual native preview choice restores a column');
    Check((LStudio.CanvasView.InputFor('notes-editor') = LMemo) and
      (LMemo.Text = 'Retain this independent English draft.'), 'Ordinary preview switching retains native input and live text');
    Check((LMemo.SelStart = 5) and (LMemo.SelLength = 4), 'Ordinary preview switching retains native text selection');
    LLayout := TComboBox(LStudio.ShellView.InputFor(NyxStudioPresentationPreviewID));
    LLayout.ItemIndex := LLayout.Items.IndexOf('Automatic / defaults');
    LLayout.OnChange(LLayout);
    Pump;
    Check(not LStudio.CanvasView.Presentations.Selection.Reference.Defined,
      'Ordinary native preview chooser clears the exclusive manual choice');
    TEdit(LStudio.ShellView.InputFor(NyxStudioPresentationNameID)).Text := 'reading';
    LLayout := TComboBox(LStudio.ShellView.InputFor(NyxStudioPresentationActivationID));
    LLayout.ItemIndex := LLayout.Items.IndexOf('manual');
    LLayout.OnChange(LLayout);
    TControlAccess(LStudio.ShellView.ControlFor(NyxStudioPresentationDefineID)).Click;
    Pump;
    Check(LStudio.Session.Document.Presentations.Definition(NyxPresentation('reading')).Activation = npaManual,
      'Actual native Inspector adds a manual definition through its ordinary paired worker');
    Check(Pos('TNyxPresentationCondition.Manual', LStudio.Session.ProjectSnapshot.Source) > 0,
      'Actual manual Inspector output uses a crafted typed construct');
    TControlAccess(LStudio.ShellView.ControlFor('action-undo')).Click;
    Pump;
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'One actual native Undo restores the exact source before manual definition editing');
    Check(LStudio.CanvasView.InputFor('notes-editor') = LMemo,
      'Undo of manual definition editing retains the native canvas input');
    { Exercise the new field through the actual Nyx Inspector and paired
      processor. The physical container consumer separately qualifies allocated
      geometry; this missing publisher deliberately remains inactive. }
    TEdit(LStudio.ShellView.InputFor(NyxStudioPresentationNameID)).Text := 'compact';
    TEdit(LStudio.ShellView.InputFor(NyxStudioPresentationContainerID)).Text := 'workspace space';
    LLayout := TComboBox(LStudio.ShellView.InputFor(NyxStudioPresentationActivationID));
    LLayout.ItemIndex := LLayout.Items.IndexOf('automatic');
    LLayout.OnChange(LLayout);
    TControlAccess(LStudio.ShellView.ControlFor(NyxStudioPresentationDefineID)).Click;
    Pump;
    Check(LStudio.Session.Document.Presentations.Definition(NyxPresentation('compact')).Container.Name =
      'workspace space', 'Actual native Inspector captures the exact named container');
    Check(Pos('TNyxPresentationCondition.Within(NyxContainer(''workspace space'')',
      LStudio.Session.ProjectSnapshot.Source) > 0, 'Native Inspector generates the fluent container construct');
    Check((LStudio.CanvasView.InputFor('notes-editor') = LMemo) and
      (LMemo.Text = 'Retain this independent English draft.'), 'Container authoring retains the actual native input');
    TControlAccess(LStudio.ShellView.ControlFor('action-undo')).Click;
    Pump;
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'One native Undo restores the exact pair before container definition editing');
    {$endif}
    WriteLn('PASS ', LChecks, ' actual Studio responsive authoring checks');
  finally
    LStudio.Free;
    LForm.Free;
  end;
end.
