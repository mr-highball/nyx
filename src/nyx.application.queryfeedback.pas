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

unit nyx.application.queryfeedback;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.state, nyx.scheduler, nyx.application.context,
  nyx.linq, nyx.linq.execution;

type
  { Caller-owned/localizable failure and cancellation messages. Success and
    discard retain the result port's existing text, including stale-source
    explanations. Empty failure diagnostics still receive a useful message. }
  TNyxQueryFeedbackText = record
  private
    FFailurePrefix: TNyxText;
    FEmptyFailure: TNyxText;
    FCancelled: TNyxText;
  public
    class function Defaults: TNyxQueryFeedbackText; static;
    function FailurePrefix(const AText: TNyxText): TNyxQueryFeedbackText;
    function EmptyFailure(const AText: TNyxText): TNyxQueryFeedbackText;
    function Cancelled(const AText: TNyxText): TNyxQueryFeedbackText;
  end;

{ Captures the mounted generation and exact request before returning an owned
  terminal port. Typed text/request state references must already exist in the
  context; admission reads them on UI and refuses mismatched types. Publication
  requires one terminal outcome on UI. Retired views/contexts and superseded
  requests return False before touching state; success/discard return True and
  preserve existing result text. Failure/cancellation update only ATarget.
  The port owns its context lease and generation, never a widget/document/store.
  Dropping it releases ownership without cancelling application work. }
function NyxQueryFeedback(const AContext: INyxApplicationContext;
  const ATarget: TNyxTextStateRef; const ARequest: TNyxIntegerStateRef;
  AExpectedRequest: Integer; const AText: TNyxQueryFeedbackText):
  INyxQueryResultPort<TNyxQueryOutcome>;

implementation

type
  TQueryFeedbackPort = class(TInterfacedObject, INyxQueryResultPort<TNyxQueryOutcome>)
  private
    FContext: INyxApplicationContext;
    FScheduler: INyxScheduler;
    FGeneration: INyxCancellationScope;
    FTarget: TNyxTextStateRef;
    FRequest: TNyxIntegerStateRef;
    FExpectedRequest: Integer;
    FText: TNyxQueryFeedbackText;
  public
    constructor Create(const AContext: INyxApplicationContext;
      const ATarget: TNyxTextStateRef; const ARequest: TNyxIntegerStateRef;
      AExpectedRequest: Integer; const AText: TNyxQueryFeedbackText);
    function Publish(const AValues: TNyxSequenceArray<TNyxQueryOutcome>): Boolean;
  end;

class function TNyxQueryFeedbackText.Defaults: TNyxQueryFeedbackText;
begin
  Result.FFailurePrefix := 'Could not calculate: ';
  Result.FEmptyFailure := 'The calculation failed without a diagnostic.';
  Result.FCancelled := 'Calculation cancelled.';
end;

function TNyxQueryFeedbackText.FailurePrefix(const AText: TNyxText): TNyxQueryFeedbackText;
begin
  Result := Self;
  Result.FFailurePrefix := AText;
end;

function TNyxQueryFeedbackText.EmptyFailure(const AText: TNyxText): TNyxQueryFeedbackText;
begin
  Result := Self;
  Result.FEmptyFailure := AText;
end;

function TNyxQueryFeedbackText.Cancelled(const AText: TNyxText): TNyxQueryFeedbackText;
begin
  Result := Self;
  Result.FCancelled := AText;
end;

constructor TQueryFeedbackPort.Create(const AContext: INyxApplicationContext;
  const ATarget: TNyxTextStateRef; const ARequest: TNyxIntegerStateRef;
  AExpectedRequest: Integer; const AText: TNyxQueryFeedbackText);
begin
  inherited Create;

  if AContext = nil then
  begin
    raise ENyxQuery.Create('Query feedback requires an active application context');
  end;
  FScheduler := AContext.Scheduler;
  FScheduler.RequireUI;
  { Validate exact existing typed slots without retaining their live state. }
  AContext.State.GetValue(ATarget);
  AContext.State.GetValue(ARequest);
  FGeneration := AContext.Generation;
  FContext := AContext;
  FTarget := ATarget;
  FRequest := ARequest;
  FExpectedRequest := AExpectedRequest;
  FText := AText;
end;

function TQueryFeedbackPort.Publish(const AValues: TNyxSequenceArray<TNyxQueryOutcome>): Boolean;
var
  LOutcome: TNyxQueryOutcome;
  LMessage: TNyxText;
begin
  FScheduler.RequireUI;
  Result := False;

  if FGeneration.Cancelled or not FContext.Active then
  begin
    Exit;
  end;

  if FContext.State.GetValue(FRequest) <> FExpectedRequest then
  begin
    Exit;
  end;

  if Length(AValues) <> 1 then
  begin
    raise ENyxQuery.Create('Query feedback requires exactly one terminal outcome');
  end;
  LOutcome := AValues[0];
  case LOutcome.Phase of
    nqpFailed:
    begin
      LMessage := LOutcome.Failure;

      if LMessage = '' then
      begin
        LMessage := FText.FEmptyFailure;
      end;
      FContext.State.SetValue(FTarget, FText.FFailurePrefix + LMessage);
    end;
    nqpCancelled:
    begin
      FContext.State.SetValue(FTarget, FText.FCancelled);
    end;
    nqpSucceeded, nqpDiscarded:
    begin
      { The result destination owns success and stale-source presentation. }
    end;
  else
    raise ENyxQuery.Create('Query feedback requires a terminal query phase');
  end;
  Result := True;
end;

function NyxQueryFeedback(const AContext: INyxApplicationContext;
  const ATarget: TNyxTextStateRef; const ARequest: TNyxIntegerStateRef;
  AExpectedRequest: Integer; const AText: TNyxQueryFeedbackText):
  INyxQueryResultPort<TNyxQueryOutcome>;
begin
  Result := TQueryFeedbackPort.Create(AContext, ATarget, ARequest, AExpectedRequest, AText);
end;

end.
