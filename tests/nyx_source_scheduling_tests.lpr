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
program nyx_source_scheduling_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, StdCtrls,
  Graphics, IntfGraphics, FPWritePNG,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codegen,
  nyx.schema, nyx.scheduler, nyx.source.preparation,
  nyx.studio.lcl, nyx.studio.sourcejobs;

type
  TControlAccess = class(TControl);
  TThreadPreparation = class(TInterfacedObject, INyxWork)
  public
    Source: TNyxText;
    Schemas: INyxSchemaSnapshot;
    Prepared: INyxPreparedSource;
    ThreadID: TThreadID;
    procedure Execute(const AExecution: INyxExecution);
  end;
  TFailureObserver = class
  public
    Failure: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;

var
  GEditor: TNyxNativeStudio;
  GForm: TForm;
  GChecks: Integer;
  GFailure: TFailureObserver;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition or (GFailure.Failure <> '') then
  begin
    raise ENyxModel.Create('Source scheduling: ' + AReason + ' / ' + GFailure.Failure);
  end;
  Inc(GChecks);
end;

procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Failure := AException.Message;
end;

procedure TThreadPreparation.Execute(const AExecution: INyxExecution);
begin
  ThreadID := GetCurrentThreadID;
  Prepared := PrepareNyxSource(Source, Schemas);
end;

procedure Pump;
begin
  CheckSynchronize;
  Application.ProcessMessages;
end;

procedure Ready;
var
  LStarted: QWord;
begin
  LStarted := GetTickCount64;
  repeat
    Pump;

    if GetTickCount64 - LStarted > 20000 then
    begin
      raise ENyxModel.Create('Source command did not retire: ' + GEditor.Status);
    end;

    if GEditor.SourceCommands.State = nssPreparing then
    begin
      Sleep(1);
    end;
  until GEditor.SourceCommands.State <> nssPreparing;
  Pump;
end;

procedure Click(const AID: TNyxText);
var
  LControl: TControl;
  LPaints: Integer;
begin
  LControl := GEditor.ShellView.ControlFor(AID);
  Check(LControl <> nil, 'actual Nyx button exists: ' + AID);
  LPaints := GEditor.PaintCount;
  TControlAccess(LControl).Click;
  Check(LPaints = GEditor.PaintCount, 'button callback leaves painting deferred');
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

procedure CheckVisibleStatus;
var
  LControl: TControl;
  LTop: Integer;
begin
  LControl := GEditor.ShellView.ControlFor('studio-source-status');
  Check(LControl <> nil, 'source status is an actual mounted Nyx control');
  LTop := LControl.ClientToScreen(Point(0, 0)).Y - GForm.ClientToScreen(Point(0, 0)).Y;
  Check(LControl.Visible and (LTop >= 0) and
    (LTop + LControl.Height <= GForm.ClientHeight),
    'source operation status is inside the actual editor viewport');
end;

procedure WorkerEnvironment;
var
  LDocument: TNyxDocument;
  LWork: TThreadPreparation;
  LLease: INyxWork;
  LScheduler: INyxScheduler;
  LExecution: INyxExecution;
  LProperty: TNyxPropertyInfo;
  LStarted: QWord;
begin
  LScheduler := NewNyxScheduler;
  LDocument := TNyxDocument.Create;
  try
    LDocument.AddPage(NewNyxColumn('worker-home').Add(
      NewNyxControl(NyxCustomKind('worker-context-fixture'), 'creator-control')
        .Configure.CustomProjection(NyxCustomKind(NyxKindName(nkColumn))).Gap(1).Done));
    LWork := TThreadPreparation.Create;
    LLease := LWork;
    LWork.Source := TNyxCodegen.Generate(LDocument);
    LWork.Schemas := CaptureNyxSchemas;
    LExecution := LScheduler.Submit(LLease, neThreaded);
    LProperty := Default(TNyxPropertyInfo);
    LProperty.Key := 'gap';
    LProperty.Title := 'Creator spacing';
    LProperty.ValueType := npInteger;
    LProperty.Minimum := 4;
    LProperty.Maximum := 9;
    RegisterNyxSchema(NyxCustomKind('worker-context-fixture'), [LProperty], []);
    LStarted := GetTickCount64;
    repeat
      Pump;

      if GetTickCount64 - LStarted > 20000 then
      begin
        raise ENyxModel.Create('Native preparation worker did not finish');
      end;
      Sleep(1);
    until not (LExecution.Status in [nesPending, nesRunning]);
    Check(LExecution.Status = nesSucceeded, 'native scheduler completes isolated work');
    Check(LWork.ThreadID <> MainThreadID, 'source preparation executes on an actual worker');
    Check(not LWork.Prepared.Diagnostic.Defined, 'worker retains the captured older creator rules');
    Check(NyxSchemaRevision <> LWork.Schemas.Revision,
      'concurrent creator publication remains visible to the main environment');
  finally
    LScheduler.Shutdown;
    LExecution := nil;
    LLease := nil;
    LScheduler := nil;
    LDocument.Free;
  end;
end;

var
  LBefore: TNyxText;
  LFirst: TNyxText;
  LLatest: TNyxText;
  LCode: TMemo;
begin
  GFailure := TFailureObserver.Create;
  try

    if ParamCount <> 1 then
    begin
      raise ENyxModel.Create('Supply an owned artifact directory');
    end;
    ForceDirectories(ParamStr(1));
    Application.Initialize;
    Application.OnException := GFailure.Failed;
    WorkerEnvironment;
    GForm := TForm.Create(nil);
    GForm.ClientWidth := 1100;
    GForm.ClientHeight := 780;
    GEditor := TNyxNativeStudio.Create(GForm, IncludeTrailingPathDelimiter(ParamStr(1)) + 'projects');
    GEditor.Run;
    GForm.Show;
    Pump;
    Click('action-code');
    Pump;
    LCode := TMemo(GEditor.CodeView.InputFor('studio-code'));
    LBefore := GEditor.Session.Source;
    LFirst := StringReplace(LBefore, 'Untitled project', 'First request', [rfReplaceAll]);
    LLatest := StringReplace(LBefore, 'Untitled project', 'Source scheduling review', [rfReplaceAll]);
    LCode.Text := LFirst;
    Click('action-apply-source');
    Check((GEditor.SourceCommands.State = nssPreparing) and
      GEditor.Session.ProjectSnapshot.Pending and (GEditor.Session.Source = LBefore),
      'Apply returns with an immutable candidate in flight and the accepted pair intact');
    LCode.Text := LLatest;
    Click('action-apply-source');
    Check(GEditor.Session.Source = LBefore, 'queued latest request publishes no intermediate pair');
    Ready;
    Check((GEditor.SourceCommands.State = nssApplied) and
      (GEditor.Session.Source = LLatest) and
      (GEditor.Session.Document.Title = 'Source scheduling review'),
      'actual editor admits the latest coalesced request');
    Check(GEditor.CodeView.InputFor('studio-code') = LCode,
      'preparation/status/completion retain the mounted Nyx code control');
    Check(Pos('Pascal applied', GEditor.Status) > 0, 'completion has a visible editor status');
    CheckVisibleStatus;
    Capture('source-scheduling-desktop');
    Click('action-undo');
    Pump;
    Check((GEditor.Session.Source = LBefore) and not GEditor.Session.CanUndo,
      'one actual Undo restores the pair with no intermediate request in history');
    Click('action-redo');
    Pump;
    Check(GEditor.Session.Source = LLatest, 'actual Redo restores the final exact source');
    LCode.Text := LLatest + #10 + '''unfinished';
    Click('action-apply-source');
    Ready;
    Check((GEditor.SourceCommands.State = nssRejected) and
      GEditor.Session.SourceDiagnostic.Defined and (GEditor.Session.Source = LLatest),
      'actual invalid draft exposes diagnostics while retaining the accepted pair');
    Click('action-reset-source');
    Pump;
    Check(not GEditor.Session.ProjectSnapshot.Pending, 'Restore cancels preparation and restores accepted Pascal');
    LCode.Text := StringReplace(LLatest, 'Source scheduling review', 'Cancelled request', [rfReplaceAll]);
    Click('action-apply-source');
    Click('action-reset-source');
    Pump;
    Check((GEditor.Session.Source = LLatest) and not GEditor.Session.ProjectSnapshot.Pending,
      'a cancelled request cannot erase or replace the accepted pair');
    GForm.ClientWidth := 390;
    GForm.ClientHeight := 800;
    Pump;
    CheckVisibleStatus;
    Capture('source-scheduling-390');
    LCode.Text := StringReplace(LLatest, 'Source scheduling review', 'Retired request', [rfReplaceAll]);
    Click('action-apply-source');
    FreeAndNil(GEditor);
    Pump;
    Check(GFailure.Failure = '', 'teardown drains preparation without a callback into freed editor controls');
    WriteLn('PASS ', GChecks, ' native source scheduling/control checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      FreeAndNil(GEditor);
      FreeAndNil(GForm);
      GFailure.Free;
      Halt(1);
    end;
  end;
  GEditor.Free;
  GForm.Free;
  GFailure.Free;
end.
