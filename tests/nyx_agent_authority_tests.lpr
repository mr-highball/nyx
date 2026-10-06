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
program nyx_agent_authority_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.authority.fixture,
  nyx.studio.agents, nyx.studio.reviews, nyx.studio.workspaces
  {$IFDEF PAS2JS}, Web{$ENDIF};

type
  TAuthorityRoute = (arPrimary, arReview, arProject);

  { Owns only independent private model fixtures. Target is borrowed from the
    appropriate manager; neither a renderer nor a transport listener is created. }
  TModelAuthorityDriver = class(TNyxAuthorityDriver)
  private
    FPrimary: TNyxAgentSession;
    FReviews: TNyxReviewWorkspaces;
    FWorkspaces: TNyxStudioWorkspaces;
    FTarget: TNyxAgentSession;
    FRoute: TAuthorityRoute;
    FReview: TNyxReviewRef;
    FWorkspace: TNyxWorkspaceRef;
    FActor: TNyxText;
  public
    constructor Create(ARoute: TAuthorityRoute);
    destructor Destroy; override;
    function Call(const ATool, AOwner: TNyxText;
      const AArguments: TNyxDataValue): TNyxDataValue; override;
    function Snapshot: TNyxDataValue; override;
    { Only changes user-visible activity text, keeping connection authority. }
    procedure RenameActor(const AActor: TNyxText);
  end;

const
  CActor: TNyxText = 'Same visible actor 😀';

constructor TModelAuthorityDriver.Create(ARoute: TAuthorityRoute);
var
  LCreated: TNyxDataValue;
begin
  inherited Create;
  FRoute := ARoute;
  FActor := CActor;
  FPrimary := TNyxAgentSession.Create;
  FReviews := TNyxReviewWorkspaces.Create(FPrimary);
  FWorkspaces := TNyxStudioWorkspaces.Create(FPrimary, 'owned-authority-registry');
  FTarget := FPrimary;

  if FRoute = arReview then
  begin
    FReview := FReviews.CreateReview('connection-one', CActor,
      'Independent authority review', nrbAccepted, FPrimary.Revision);
    FTarget := FReviews.Resolve('connection-one', FReview);
  end
  else if FRoute = arProject then
  begin
    LCreated := FWorkspaces.Manage('connection-one', CActor, NyxObject([
      NyxField('mode', NyxData('create')), NyxField('operationId', NyxData('create-project')),
      NyxField('expectedRevision', NyxData(FPrimary.Revision)),
      NyxField('base', NyxData('accepted')), NyxField('label', NyxData('Authority project'))]));
    FWorkspace := NyxWorkspace(LCreated.Field('workspace').AsText);
    FTarget := FWorkspaces.Find(FWorkspace);
  end;
end;

destructor TModelAuthorityDriver.Destroy;
begin
  FWorkspaces.Free;
  FReviews.Free;
  FPrimary.Free;
  inherited Destroy;
end;

function TModelAuthorityDriver.Call(const ATool, AOwner: TNyxText;
  const AArguments: TNyxDataValue): TNyxDataValue;
begin

  if FRoute = arProject then
  begin
    Result := FWorkspaces.Call(ATool, AOwner, FActor, NyxWithWorkspace(AArguments, FWorkspace));
  end
  else
  begin
    Result := FReviews.Call(ATool, AOwner, FActor, NyxWithReview(AArguments, FReview));
  end;
end;

procedure TModelAuthorityDriver.RenameActor(const AActor: TNyxText);
begin
  FActor := AActor;
end;

procedure VerifyPrivateReview(ADriver: TModelAuthorityDriver; var AChecks: Integer);
var
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LBefore: TNyxDataValue;
  LRefused: Boolean;

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(AChecks);
  end;

begin
  LRequest := NyxObject([
    NyxField('expectedRevision', ADriver.Snapshot.Field('session').Field('revision')),
    NyxField('operationId', NyxData('owned-review-edit')),
    NyxField('operations', NyxArray([NyxObject([
      NyxField('op', NyxData('title')), NyxField('value', NyxData('A private idea'))])]))]);
  LReceipt := ADriver.Call('nyx_transaction', 'connection-one', LRequest);
  ADriver.RenameActor('Renamed visible actor 😀');
  Check(ADriver.Call('nyx_transaction', 'connection-one', LRequest).ToJSON = LReceipt.ToJSON,
    'Renaming a review actor retains its original connection receipt');
  LBefore := ADriver.Snapshot;
  LRefused := False;
  try
    ADriver.Call('nyx_transaction', 'connection-two', LRequest);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Foreign connection cannot operate the private review');
  Check(ADriver.Snapshot.Field('project').AsText = LBefore.Field('project').AsText,
    'Foreign review refusal retains exact paired content');
  Check(ADriver.Snapshot.Field('session').Field('revision').ToJSON =
    LBefore.Field('session').Field('revision').ToJSON,
    'Foreign review refusal retains revision');
end;

function TModelAuthorityDriver.Snapshot: TNyxDataValue;
begin
  Result := FTarget.Exchange(NyxObject([NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(0))]));
end;

var
  LDriver: TModelAuthorityDriver;
  LRoute: TAuthorityRoute;
  LChecks: Integer;
begin
  LChecks := 0;
  try
    for LRoute := Low(TAuthorityRoute) to High(TAuthorityRoute) do
    begin
      LDriver := TModelAuthorityDriver.Create(LRoute);
      try
        if LRoute = arReview then
        begin
          VerifyPrivateReview(LDriver, LChecks);
        end
        else
        begin
          VerifyNyxAgentAuthority(LDriver, LChecks);
        end;
      finally
        LDriver.Free;
      end;
    end;
    {$IFDEF PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' authority checks';
    document.body.setAttribute('data-nyx-authority', 'passed');
    document.body.setAttribute('data-nyx-authority-checks', IntToStr(LChecks));
    {$ELSE}
    WriteLn('PASS ', LChecks, ' authority checks');
    {$ENDIF}
  except
    on LException: Exception do
    begin
      {$IFDEF PAS2JS}
      document.body.textContent := 'FAIL: ' + LException.Message;
      document.body.setAttribute('data-nyx-authority', 'failed');
      {$ELSE}
      WriteLn('FAIL: ', LException.Message);
      ExitCode := 1;
      {$ENDIF}
    end;
  end;
end.
