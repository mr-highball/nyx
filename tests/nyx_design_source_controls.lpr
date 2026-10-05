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

program nyx_design_source_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls,
  Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec, nyx.codegen,
  nyx.schema, nyx.studio.session, nyx.studio.projects, nyx.studio.lcl,
  nyx.studio.sourcejobs;

type
  TControlAccess = class(TControl);
  { The real UI timer establishes message-loop service while independent
    preparation runs. It does not emulate a worker or alter its completion. }
  TObservation = class
  public
    Failure: TNyxText;
    Ticks: Integer;
    PreparingTicks: Integer;
    procedure Failed(ASender: TObject; AException: Exception);
    procedure Tick(ASender: TObject);
  end;

var
  GStudio: TNyxNativeStudio;
  GForm: TForm;
  GObservation: TObservation;
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue or (GObservation.Failure <> '') then
  begin
    raise ENyxModel.Create('Design controls: ' + AReason + ' / ' + GObservation.Failure);
  end;
  Inc(GChecks);
end;

procedure TObservation.Failed(ASender: TObject; AException: Exception);
begin
  Failure := AException.Message;
end;

procedure TObservation.Tick(ASender: TObject);
begin
  Inc(Ticks);

  if (GStudio <> nil) and GStudio.SourceCommands.Busy then
  begin
    Inc(PreparingTicks);
  end;
end;

procedure Pump;
begin
  CheckSynchronize;
  Application.ProcessMessages;

  if GObservation.Failure <> '' then
  begin
    raise ENyxModel.Create(GObservation.Failure);
  end;
end;

procedure Ready;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStarted > 90000 then
    begin
      raise ENyxModel.Create('Design processor did not retire / ' + GStudio.Status);
    end;

    if GStudio.SourceCommands.Busy then
    begin
      Sleep(1);
    end;
  until not GStudio.SourceCommands.Busy;
  Pump;
end;

procedure Click(const AID: TNyxText);
var
  LControl: TControl;
  LPaints: Integer;
begin
  LControl := GStudio.ShellView.ControlFor(AID);
  Check(LControl <> nil, 'Actual Nyx control exists / ' + AID);
  LPaints := GStudio.PaintCount;
  TControlAccess(LControl).Click;
  Check(GStudio.PaintCount = LPaints, 'Widget callback leaves rendering deferred / ' + AID);
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
    LBitmap.SetSize(GForm.ClientWidth, GForm.ClientHeight);
    GForm.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.png', LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;

function SizedPair(AControls: Integer): TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LIndex: Integer;
  LSource: TNyxText;
  LFixture: TNyxStudioSession;
begin
  LDocument := TNyxDocument.Create;
  LFixture := TNyxStudioSession.Create;
  try
    { Exact original workload, including its crafted local, Unicode technical
      note and unchanged expression. Visible captions remain English. Source
      bytes are measured after the same two original public visual commands. }
    LDocument.Title := 'Source workspace';
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxLabel('message').WithText('Before edit'));
    LPage.Add(NewNyxLabel('stable').WithText('Stable caption'));
    for LIndex := 2 to AControls - 1 do
    begin
      LPage.Add(NewNyxLabel('caption-' + IntToStr(LIndex))
        .WithText('Caption ' + IntToStr(LIndex)));
    end;
    LDocument.AddPage(LPage);
    LSource := TNyxCodegen.Generate(LDocument);
    LSource := StringReplace(LSource, 'LMessageLabel', 'LMessageCaption', [rfReplaceAll]);
    LSource := StringReplace(LSource, 'LMessageCaption :=',
      TNyxText('{ Keep this note / 🌙 / 漢字 }') + #10 + '    LMessageCaption :=', [rfReplaceAll]);
    LSource := StringReplace(LSource, '''Stable caption''',
      '''Stable '' + ''caption''', [rfReplaceAll]);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    LFixture.LoadProject(Result);
    LFixture.Select('message');
    LFixture.SetProperty('text', 'After edit');
    LFixture.Select('home');
    LFixture.AddControl(NewNyxButton('added-action').WithText('Continue'));
    Result := LFixture.ProjectSnapshot;
  finally
    LFixture.Free;
    LDocument.Free;
  end;
end;

function Intent(AAction: TNyxStudioDesignAction; const AName, AValue: TNyxText):
  TNyxStudioDesignEdit;
begin
  Result := Default(TNyxStudioDesignEdit);
  Result.Action := AAction;
  Result.Selection := GStudio.Session.SelectedID;
  Result.View := GStudio.Session.ActiveViewID;
  Result.Name := AName;
  Result.Value := AValue;
end;

procedure Journey(AControls, ABytes: Integer);
var
  LBefore: TNyxProjectPair;
  LFinal: TNyxProjectPair;
  LField: TCustomEdit;
  LCode: TControl;
  LCaption: TControl;
  LInspector: TControl;
  LTimer: TTimer;
  LStarted: QWord;
  LDispatchMS: QWord;
  LCompletionMS: QWord;
  LTicks: Integer;
  LPreparingTicks: Integer;
  LPaints: Integer;
  LSelection: TNyxText;
begin
  LBefore := SizedPair(AControls);
  Check(Length(LBefore.Source) = ABytes, 'Original accepted source size / ' + IntToStr(AControls));
  GForm := TForm.Create(nil);
  GForm.ClientWidth := 1100;
  GForm.ClientHeight := 780;
  GStudio := TNyxNativeStudio.Create(GForm,
    IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects-' + IntToStr(AControls));
  LTimer := TTimer.Create(nil);
  try
    GStudio.LoadProject(LBefore);
    GStudio.Run;
    GForm.Show;
    Pump;
    GStudio.Session.Select('message');
    GStudio.RequestRefresh;
    Pump;
    Click('action-code');
    Pump;
    LCode := GStudio.CodeView.InputFor('studio-code');
    LField := TCustomEdit(GStudio.ShellView.InputFor('inspector-text'));
    LInspector := LField;
    LCaption := GStudio.CanvasView.ControlFor('message');
    GStudio.ShellView.Reveal('inspector-text');
    LField.SetFocus;
    LTicks := GObservation.Ticks;
    LPreparingTicks := GObservation.PreparingTicks;
    LTimer.Interval := 15;
    LTimer.OnTimer := GObservation.Tick;
    LTimer.Enabled := True;
    LPaints := GStudio.PaintCount;
    LStarted := GetTickCount64;
    LField.Text := 'First queued caption';
    LField.Text := 'Final queued caption';
    LField.Text := 'Final queued caption!';
    LField.SelStart := 4;
    LDispatchMS := GetTickCount64 - LStarted;
    Check(GStudio.SourceCommands.Busy and (GStudio.PaintCount = LPaints),
      'Real inspector callbacks enqueue preparation without replacing their notifying controls');
    Check((GStudio.Session.Source = LBefore.Source) and
      (GStudio.Session.Document.Find('message').Prop('text') = 'After edit'),
      'Pending visual input leaves both accepted owners unchanged');
    Check(GStudio.SourceCommands.PendingDesign.PropertyValue('message', 'text', LSelection) and
      (LSelection = 'Final queued caption!'), 'Pending presentation owns the latest adjacent field value');
    LStarted := GetTickCount64;
    Pump;
    LField := TCustomEdit(GStudio.ShellView.InputFor('inspector-text'));
    Check((LField.Text = 'Final queued caption!') and LField.Focused and (LField.SelStart = 4),
      'Preparing refresh restores current inspector text, focus and caret by identity');
    Ready;
    LCompletionMS := GetTickCount64 - LStarted;
    LTimer.Enabled := False;
    LField := TCustomEdit(GStudio.ShellView.InputFor('inspector-text'));
    Check((LField.Text = 'Final queued caption!') and LField.Focused and (LField.SelStart = 4),
      'Completion preserves inspector input selection');
    Check(GStudio.ShellView.InputFor('inspector-text') = LInspector,
      'Scalar source preparation/publication retains the actual inspector control');
    Check((GStudio.CanvasView.ControlFor('message') = LCaption) and
      (TLabel(LCaption).Caption = 'Final queued caption!'),
      'Retained canvas control paints the exact newly admitted caption');
    Check((GStudio.SourceCommands.State = nssApplied) and
      (GStudio.Session.Document.Find('message').Prop('text') = 'Final queued caption!'),
      'FIFO first/latest property candidates publish their final exact meaning');
    Check(GStudio.CodeView.InputFor('studio-code') = LCode,
      'Design preparation/publication retains the independent Nyx source control');
    Check((Pos('LMessageCaption', GStudio.Session.Source) > 0) and
      (Pos(TNyxText('Keep this note / 🌙 / 漢字'), GStudio.Session.Source) > 0) and
      (Pos('''Stable '' + ''caption''', GStudio.Session.Source) > 0),
      'Complete source retains crafted locals, exact technical Unicode and unchanged expressions');
    Check((GStudio.Session.Document.Pages[0].Count = AControls + 1) and
      (GStudio.CanvasView.ViewViewport.Y.Extent > GForm.ClientHeight),
      'Publication retains every original descendant and complete logical canvas');
    Check(GObservation.PreparingTicks > LPreparingTicks,
      'Actual UI timer runs while a design candidate remains in flight');
    LFinal := GStudio.Session.ProjectSnapshot;
    Click('action-undo');
    Pump;
    Check(GStudio.Session.Document.Find('message').Prop('text') = 'First queued caption',
      'One Undo removes the coalesced latest field command');
    Click('action-undo');
    Pump;
    Check((GStudio.Session.Source = LBefore.Source) and (GStudio.Session.Save = LBefore.Design) and
      not GStudio.Session.CanUndo, 'Second Undo restores the exact original pair without hidden intermediates');
    Click('action-redo');
    Pump;
    Click('action-redo');
    Pump;
    Check((GStudio.Session.Source = LFinal.Source) and (GStudio.Session.Save = LFinal.Design),
      'Paired Redo restores the exact final processor result');
    WriteLn(AControls, ',', ABytes, ',', LDispatchMS, ',', LCompletionMS, ',', GObservation.Ticks - LTicks);

    if AControls = 128 then
    begin
      { Presentation captures show English design text. Private Unicode remains
        in the persisted companion, outside the visible source pane. }
      Click('action-code');
      Pump;
      Capture('design-source-desktop');
      GForm.ClientWidth := 390;
      GForm.ClientHeight := 800;
      Pump;
      Click('action-panel-project');
      Pump;
      LField := TCustomEdit(GStudio.ShellView.InputFor('project-title'));
      GStudio.ShellView.Reveal('project-title');
      LField.SetFocus;
      LField.Text := 'English component review';
      LField.SelStart := 7;
      Ready;
      LField := TCustomEdit(GStudio.ShellView.InputFor('project-title'));
      Check((LField.Text = 'English component review') and LField.Focused and (LField.SelStart = 7),
        'Compact title preparation retains field focus and exact paired title');
      GStudio.Session.Select('home');
      Click('palette-button');
      Ready;
      Check((GStudio.Session.Selected.Kind = NyxKindName(nkButton)) and
        (GStudio.ShellView.Root.Find('studio-canvas') <> nil),
        'Actual compact palette completion returns to Design with its new component');
      Capture('design-source-390');
      GForm.ClientWidth := 1100;
      Pump;
      GStudio.Session.Select('message');
      GStudio.RequestRefresh;
      Pump;
      LBefore := GStudio.Session.ProjectSnapshot;
      Click('action-duplicate');
      Click('action-delete');
      Ready;
      Check((GStudio.Session.Document.Find('message') = nil) and
        (GStudio.Session.Selected <> nil) and
        (GStudio.Session.SelectedID <> 'message'),
        'Queued structural commands retain captured targets instead of retargeting selection');
      Click('action-undo');
      Pump;
      Click('action-undo');
      Pump;
      Check((GStudio.Session.Source = LBefore.Source) and (GStudio.Session.Save = LBefore.Design),
        'Structural FIFO commands own two exact paired Undo entries');
      GStudio.SourceCommands.Edit(Intent(sdaTitle, '', 'Stale design title'));
      GStudio.Session.SetTitle('Independent accepted title');
      LBefore := GStudio.Session.ProjectSnapshot;
      Ready;
      Check((GStudio.SourceCommands.State = nssStale) and
        (GStudio.Session.Source = LBefore.Source) and (GStudio.Session.Save = LBefore.Design),
        'Actual controller refuses stale candidate after independent accepted mutation');
      GStudio.SourceCommands.Edit(Intent(sdaTitle, '', 'Cancelled design title'));
      GStudio.SourceCommands.Cancel;
      Ready;
      Check((GStudio.Session.Source = LBefore.Source) and (GStudio.Session.Save = LBefore.Design),
        'Cancelled design candidate cannot publish');
    end;
    GStudio.SourceCommands.Edit(Intent(sdaTitle, '', 'Retired design title'));
    FreeAndNil(GStudio);
    Pump;
    Check(GObservation.Failure = '', 'Design worker retirement cannot deliver to destroyed controls');
  finally
    LTimer.Free;
    FreeAndNil(GStudio);
    FreeAndNil(GForm);
  end;
end;

begin
  GObservation := TObservation.Create;
  try
    try

      if (ParamCount < 1) or (ParamCount > 2) then
      begin
        raise ENyxModel.Create('Supply an owned artifact directory');
      end;
      ForceDirectories(ParamStr(1));
      Application.Initialize;
      Application.OnException := GObservation.Failed;
      WriteLn('controls,source-bytes,three-input-ms,completion-ms,ui-ticks');
      { A bounded original-size diagnostic may select one existing workload.
        Default qualification still executes ALL sizes, unchanged. A selected
        diagnostic is never evidence for omitted sizes or complete parity. }
      if ParamCount > 1 then
      begin
        case StrToInt(ParamStr(2)) of
          128: Journey(128, 25094);
          512: Journey(512, 98822);
          2048: Journey(2048, 400022);
        else
          begin
            raise ENyxModel.Create('Diagnostic size must be 128, 512 or 2048');
          end;
        end;
      end
      else
      begin
        Journey(128, 25094);
        Journey(512, 98822);
        Journey(2048, 400022);
      end;
      WriteLn('PASS ', GChecks, ' actual native design/source checks');
    except
      on LException: Exception do
      begin
        WriteLn('FAIL ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
      end;
    end;
  finally
    FreeAndNil(GStudio);
    FreeAndNil(GForm);
    GObservation.Free;
  end;
end.
