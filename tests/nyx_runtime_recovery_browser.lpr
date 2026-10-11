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

program nyx_runtime_recovery_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Web, nyx.text, nyx.studio.sourcecompilation,
  nyx.studio.recovery.browser, nyx.studio.recovery.probe.browser,
  nyx.studio.recovery.status;

type
  { Qualification observer only. The actual adapter owns HTTP/compiler/worker
    identity and lifetime; this port prints no source, credentials or drafts.
    It owns no operation back, so the browser's managed callback has no cycle. }
  TRecoveryObserver = class(TInterfacedObject, INyxBrowserRecoveryPort)
  private
    FExecuted: Integer;
    FPublished: Integer;
    FSessions: Integer;
  public
    procedure Progress(APhase: TNyxBrowserRecoveryPhase;
      AAcceptedUnits, ATotalUnits, ASessions: Integer);
    procedure Finished(AState: TNyxSourceCompilationState;
      const AMessage: TNyxText);
  end;

  { Readiness uses its actual product adapter before execution. In particular,
    a fixture served beneath /builds/ must still address the owning service's
    root API. This port owns no operation, credentials, source or editor model. }
  TReadinessObserver = class(TInterfacedObject, INyxRuntimeRecoveryProbePort)
  public
    procedure Inspected(AState: TNyxSourceCompilationState;
      const AStatus: TNyxRuntimeRecoveryStatus; const AMessage: TNyxText);
  end;

var
  GOperation: INyxSourceCompilation;
  GProbe: INyxSourceCompilation;

procedure TRecoveryObserver.Progress(APhase: TNyxBrowserRecoveryPhase;
  AAcceptedUnits, ATotalUnits, ASessions: Integer);
const
  CPhase: array[TNyxBrowserRecoveryPhase] of TNyxText = ('connecting',
    'reading', 'compiling', 'executing', 'publishing', 'cancelling',
    'completed', 'cancelled', 'failed');
begin
  FSessions := ASessions;

  if APhase = brpExecuting then
  begin
    Inc(FExecuted);
  end;

  if APhase = brpPublishing then
  begin
    Inc(FPublished);
  end;
  document.body.setAttribute('data-runtime-recovery-phase', CPhase[APhase]);
  document.body.setAttribute('data-runtime-recovery-accepted', IntToStr(AAcceptedUnits));
  document.body.setAttribute('data-runtime-recovery-units', IntToStr(ATotalUnits));
  document.body.setAttribute('data-runtime-recovery-sessions', IntToStr(ASessions));
  document.body.textContent := 'Saved project recovery: ' + CPhase[APhase] +
    ' (' + IntToStr(AAcceptedUnits) + '/' + IntToStr(ATotalUnits) + ' units)';
end;

procedure TRecoveryObserver.Finished(AState: TNyxSourceCompilationState;
  const AMessage: TNyxText);
begin

  if (AState = scsCompleted) and (FExecuted > 0) and
    (FPublished = FExecuted) and (FSessions > 0) then
  begin
    document.body.setAttribute('data-runtime-recovery', 'passed');
  end
  else
  begin
    document.body.setAttribute('data-runtime-recovery', 'failed');
    document.body.setAttribute('data-event-error', AMessage);
  end;

  if FExecuted = 0 then
  begin
    document.body.textContent := 'Qualification requires a retained checkpoint on an owning browser recovery host. ' + AMessage;
  end
  else
  begin
    document.body.textContent := AMessage;
  end;

  if GOperation <> nil then
  begin
    document.body.setAttribute('data-runtime-recovery-state', IntToStr(Ord(GOperation.State)));
  end;
end;

procedure TReadinessObserver.Inspected(AState: TNyxSourceCompilationState;
  const AStatus: TNyxRuntimeRecoveryStatus; const AMessage: TNyxText);
begin

  if (AState <> scsCompleted) or not AStatus.Pending or
    (AStatus.Sessions = 0) or (AStatus.Units = 0) then
  begin
    document.body.setAttribute('data-runtime-recovery', 'failed');
    document.body.setAttribute('data-event-error',
      'Qualification requires actual pending recovery readiness. ' + AMessage);
    Exit;
  end;
  document.body.setAttribute('data-runtime-recovery-probe', 'passed');
  GOperation := StartNyxBrowserRuntimeRecovery(TRecoveryObserver.Create);
end;

begin
  document.body.setAttribute('data-runtime-recovery', 'running');
  GProbe := InspectNyxBrowserRuntimeRecovery(TReadinessObserver.Create);
end.
