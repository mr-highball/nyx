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
program nyx_source_worker_failure_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, JS, Web, nyx.text, nyx.studio.session, nyx.studio.projects,
  nyx.studio.sourcejobs;

type
  { One page owns the session, source queues and polling timer. Notifications
    copy values only; queue disposal happens after its callback has returned.
    The inert helper exercises the unchanged real thirty-second deadline. }
  TReview = class
  private
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FBefore: TNyxText;
    FDraft: TNyxText;
    FCase: Integer;
    FChecks: Integer;
    FTimer: NativeInt;
    FDeadline: Double;
    procedure Check(AValue: Boolean; const AReason: TNyxText);
    procedure Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
    procedure StartCase;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Tick;
  end;

var
  GReview: TReview;

constructor TReview.Create;
begin
  inherited Create;
  FTimer := -1;
  FSession := TNyxStudioSession.Create;
  FDraft := FSession.Source + #10 + '{ Retain this unfinished idea: a moon 🌙. }';
  FSession.SetSourceDraft(FDraft);
  FBefore := EncodeNyxProject(FSession.ProjectSnapshot);
  StartCase;
end;

destructor TReview.Destroy;
begin

  if FTimer >= 0 then
  begin
    window.clearTimeout(FTimer);
  end;
  FCommands.Free;
  FSession.Free;
  inherited Destroy;
end;

procedure TReview.Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(FChecks);
end;

procedure TReview.Changed(AState: TNyxSourceCommandState; const AMessage: TNyxText);
begin
  document.body.setAttribute('data-worker-check-phase', IntToStr(FCase));
  document.body.setAttribute('data-worker-check-message', AMessage);
end;

procedure TReview.StartCase;
var
  LURL: TNyxText;
begin
  FCommands.Free;
  FCommands := nil;
  case FCase of
    0: LURL := 'missing-nyx-worker-for-regression.js';
    1: LURL := 'nyx_source_worker_inert.js';
  else
    LURL := 'nyx_source_worker.js';
  end;
  FCommands := TNyxSourceCommands.Create(FSession, @Changed, LURL);
  FDeadline := TJSDate.now + 45000;
  FCommands.Apply;
  FTimer := window.setTimeout(@Tick, 20);
end;

procedure TReview.Tick;
begin
  FTimer := -1;
  try

    if FCommands.Busy then
    begin

      if TJSDate.now >= FDeadline then
      begin
        raise Exception.Create('Source queue failed to reach a terminal outcome');
      end;
      FTimer := window.setTimeout(@Tick, 50);
      Exit;
    end;

    if FCase < 2 then
    begin
      Check(FCommands.State = nssFailed, 'A worker host failure has its own terminal state');
      Check(EncodeNyxProject(FSession.ProjectSnapshot) = FBefore,
        'Worker failure preserves exact accepted design/source and draft/base');

      if FCase = 0 then
      begin
        Check(Pos('Editor processor failed', FCommands.Message) = 1,
          'Missing worker is reported as failure rather than timeout');
        Check(Pos('timed out', FCommands.Message) = 0,
          'An immediate script failure does not claim that its deadline elapsed');
      end
      else
      begin
        Check(Pos('Editor processor timed out', FCommands.Message) = 1,
          'An inert worker reports the actual thirty-second timeout');
      end;
      Inc(FCase);
      StartCase;
      Exit;
    end;
    Check(FCommands.State = nssApplied, 'A healthy worker remains usable after both failures');
    Check(FSession.Source = FDraft, 'Healthy Apply admits the exact supplementary Unicode draft');
    Check(not FSession.SourceDraftPending, 'Successful Apply clears only the admitted pending draft');
    document.body.setAttribute('data-worker-check-status', 'passed');
    document.body.setAttribute('data-worker-checks', IntToStr(FChecks));
    FCommands.Free;
    FCommands := nil;
    FSession.Free;
    FSession := nil;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-worker-check-status', 'failed');
      document.body.setAttribute('data-worker-check-error', LException.Message);
      FCommands.Free;
      FCommands := nil;
    end;
  end;
end;

begin
  document.body.setAttribute('data-worker-check-status', 'running');
  GReview := TReview.Create;
end.
