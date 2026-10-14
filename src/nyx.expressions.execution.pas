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

unit nyx.expressions.execution;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.state, nyx.expressions, nyx.linq, nyx.linq.execution,
  nyx.scheduler, nyx.application.context;

const
  NyxMaximumExpressionAssignments = 256;

type
  { One immutable typed destination/expression pair. Factories normalize a copied
    tree; no application/store/input frame is retained. The specialized overloads
    prevent implicit family conversion. ToData is unnecessary for execution. }
  INyxExpressionAssignment = interface
    ['{198F3107-738F-48D8-B59B-6D442149B5E1}']
    function GetName: TNyxText;
    function GetExpression: INyxExpression;
    property Name: TNyxText read GetName;
    property Expression: INyxExpression read GetExpression;
  end;

  { Capture and submission are UI-only. All destinations must already exist with
    the exact family, and every dependency is validated before queue admission.
    Worker enumeration reads only copied expression trees and an owned immutable
    input frame. Expressions in a group all read the SAME initial frame, never
    preceding outputs. Duplicate destinations and empty/oversized groups refuse.

    Delivery uses the application's revocable context, current page generation,
    exact store identity and captured revision. Stale state fails; retired pages/
    applications discard/cancel. One Apply publishes all assignments atomically
    through ordinary validators/bindings. No raw state/control crosses the worker.
    A receiver exception after commit remains a failed run with committed state,
    as documented by TNyxState.Apply; no rollback is fabricated. Ordinary state
    callbacks must not destroy their store/application inside that callback.

    Default defers on UI on both targets. Native neThreaded uses a real worker;
    the browser explicitly refuses it. neAsynchronous follows scheduler capability.
    Returned ordinary query runs support cancellation and terminal observation.
    Dropping a run does not cancel; application teardown revokes delivery. }
  TNyxExpressionExecution = class
  public
    class function Submit(const AContext: INyxApplicationContext;
      const AAssignments: array of INyxExpressionAssignment;
      APolicy: TNyxExecutionPolicy = neUIQueue): INyxQueryRun; static;
  end;

function NyxCompute(const ATarget: TNyxTextStateRef;
  const AExpression: INyxTextExpression): INyxExpressionAssignment; overload;
function NyxCompute(const ATarget: TNyxBooleanStateRef;
  const AExpression: INyxBooleanExpression): INyxExpressionAssignment; overload;
function NyxCompute(const ATarget: TNyxIntegerStateRef;
  const AExpression: INyxIntegerExpression): INyxExpressionAssignment; overload;
function NyxCompute(const ATarget: TNyxNumberStateRef;
  const AExpression: INyxNumberExpression): INyxExpressionAssignment; overload;

implementation

type
  TExpressionAssignment = class(TInterfacedObject, INyxExpressionAssignment)
  private
    FName: TNyxText;
    FExpression: INyxExpression;
  public
    constructor Create(const AName: TNyxText; AKind: TNyxStateKind;
      const AExpression: INyxExpression);
    function GetName: TNyxText;
    function GetExpression: INyxExpression;
  end;

  { This selector is the only worker payload: no scheduler, UI capability or
    live state is retained. Its input frame belongs exclusively to this run. }
  TExpressionSelector = class(TInterfacedObject,
    INyxSelector<INyxExpressionAssignment, TNyxStateAssignment>)
  private
    FInputs: INyxExpressionInputs;
  public
    constructor Create(const AInputs: INyxExpressionInputs);
    function Project(const AValue: INyxExpressionAssignment): TNyxStateAssignment;
  end;

  { Only the UI courier owns this weak application port. ExpectedStore is an
    identity comparison, never dereferenced after context retirement. The context
    lease is revoked by its application before freeing the borrowed store. }
  TExpressionPort = class(TInterfacedObject, INyxQueryResultPort<TNyxStateAssignment>)
  private
    FContext: INyxApplicationContext;
    FScheduler: INyxScheduler;
    FGeneration: INyxCancellationScope;
    FExpectedStore: TNyxState;
    FRevision: Integer;
  public
    constructor Create(const AContext: INyxApplicationContext;
      const AScheduler: INyxScheduler; const AGeneration: INyxCancellationScope;
      AState: TNyxState; ARevision: Integer);
    function Publish(const AValues: TNyxSequenceArray<TNyxStateAssignment>): Boolean;
  end;

constructor TExpressionAssignment.Create(const AName: TNyxText; AKind: TNyxStateKind;
  const AExpression: INyxExpression);
begin
  inherited Create;

  if AExpression = nil then
  begin
    raise ENyxExpression.Create('Computed assignment requires a typed expression');
  end;
  FName := AName;
  FExpression := NyxExpressionFromData(AExpression.ToData);

  if FExpression.Kind <> AKind then
  begin
    raise ENyxExpression.Create('Computed assignment must retain its destination family');
  end;
end;

function TExpressionAssignment.GetName: TNyxText;
begin
  Result := FName;
end;

function TExpressionAssignment.GetExpression: INyxExpression;
begin
  Result := FExpression;
end;

function NyxCompute(const ATarget: TNyxTextStateRef;
  const AExpression: INyxTextExpression): INyxExpressionAssignment;
begin
  Result := TExpressionAssignment.Create(ATarget.Name, nskText, AExpression);
end;

function NyxCompute(const ATarget: TNyxBooleanStateRef;
  const AExpression: INyxBooleanExpression): INyxExpressionAssignment;
begin
  Result := TExpressionAssignment.Create(ATarget.Name, nskBoolean, AExpression);
end;

function NyxCompute(const ATarget: TNyxIntegerStateRef;
  const AExpression: INyxIntegerExpression): INyxExpressionAssignment;
begin
  Result := TExpressionAssignment.Create(ATarget.Name, nskInteger, AExpression);
end;

function NyxCompute(const ATarget: TNyxNumberStateRef;
  const AExpression: INyxNumberExpression): INyxExpressionAssignment;
begin
  Result := TExpressionAssignment.Create(ATarget.Name, nskNumber, AExpression);
end;

constructor TExpressionSelector.Create(const AInputs: INyxExpressionInputs);
begin
  inherited Create;
  FInputs := AInputs;
end;

function TExpressionSelector.Project(const AValue: INyxExpressionAssignment): TNyxStateAssignment;
begin
  Result := NyxStateAssign(AValue.Name, AValue.Expression.Value(FInputs));
end;

constructor TExpressionPort.Create(const AContext: INyxApplicationContext;
  const AScheduler: INyxScheduler; const AGeneration: INyxCancellationScope;
  AState: TNyxState; ARevision: Integer);
begin
  inherited Create;
  FContext := AContext;
  FScheduler := AScheduler;
  FGeneration := AGeneration;
  FExpectedStore := AState;
  FRevision := ARevision;
end;

function TExpressionPort.Publish(const AValues: TNyxSequenceArray<TNyxStateAssignment>): Boolean;
var
  LState: TNyxState;
begin
  FScheduler.RequireUI;
  Result := FContext.Active and not FGeneration.Cancelled;

  if not Result then
  begin
    Exit;
  end;
  LState := FContext.State;

  if LState <> FExpectedStore then
  begin
    raise ENyxExpression.Create('Application state owner changed before computed delivery');
  end;
  LState.Apply(AValues, FRevision);
end;

class function TNyxExpressionExecution.Submit(const AContext: INyxApplicationContext;
  const AAssignments: array of INyxExpressionAssignment;
  APolicy: TNyxExecutionPolicy): INyxQueryRun;
var
  LState: TNyxState;
  LCapturedState: TNyxState;
  LRevision: Integer;
  LScheduler: INyxScheduler;
  LGeneration: INyxCancellationScope;
  LInputs: INyxExpressionInputs;
  LValues: array of INyxExpressionAssignment;
  LExpression: INyxExpression;
  LSelector: INyxSelector<INyxExpressionAssignment, TNyxStateAssignment>;
  LPort: INyxQueryResultPort<TNyxStateAssignment>;
  LSequence: INyxSequence<TNyxStateAssignment>;
  LIndex: Integer;
  LPrevious: Integer;
  LName: TNyxText;
begin

  if (AContext = nil) or (Length(AAssignments) = 0) or
    (Length(AAssignments) > NyxMaximumExpressionAssignments) then
  begin
    raise ENyxExpression.Create('Expression execution requires an active context and 1..256 assignments');
  end;
  LScheduler := AContext.Scheduler;
  LScheduler.RequireUI;
  LScheduler.Admit(APolicy);
  LState := AContext.State;
  LGeneration := AContext.Generation;

  if (LState = nil) or (LGeneration = nil) or LGeneration.Cancelled then
  begin
    raise ENyxExpression.Create('Expression execution requires a live state/page generation');
  end;
  LRevision := LState.Revision;
  LInputs := CaptureNyxExpressionInputs(LState);
  LCapturedState := LState.Clone;
  try
    SetLength(LValues, Length(AAssignments));
    for LIndex := 0 to High(AAssignments) do
    begin

      if AAssignments[LIndex] = nil then
      begin
        raise ENyxExpression.Create('Computed assignment is missing');
      end;
      LName := AAssignments[LIndex].Name;
      LExpression := AAssignments[LIndex].Expression;

      if (LExpression = nil) or not LCapturedState.Has(LName) then
      begin
        raise ENyxExpression.Create('Computed destination/expression is missing: ' + LName);
      end;
      LValues[LIndex] := TExpressionAssignment.Create(LName,
        LCapturedState.Value(LName).Kind, LExpression);
      LValues[LIndex].Expression.ValidateInputs(LInputs);
      for LPrevious := 0 to LIndex - 1 do
      begin

        if LValues[LPrevious].Name = LName then
        begin
          raise ENyxExpression.Create('Computed destinations must be distinct: ' + LName);
        end;
      end;
    end;
    { Alternate assignment implementations may execute caller code from getters.
      Refuse any reentrant context change before adopting work. }
    if not AContext.Active or LGeneration.Cancelled then
    begin
      raise ENyxExpression.Create('Application retired during computed capture');
    end;

    if (AContext.State <> LState) or (LState.Revision <> LRevision) then
    begin
      raise ENyxExpression.Create('Application state changed during computed capture');
    end;
    LSelector := TExpressionSelector.Create(LInputs);
    LSequence := TNyxSelect<INyxExpressionAssignment, TNyxStateAssignment>.From(
      TNyxSequence<INyxExpressionAssignment>.From(LValues), LSelector);
    LPort := TExpressionPort.Create(AContext, LScheduler, LGeneration, LState, LRevision);
    Result := TNyxQuery<TNyxStateAssignment>.Submit(LSequence, LScheduler, LPort,
      APolicy, LGeneration);
  finally
    LCapturedState.Free;
  end;
end;

end.
