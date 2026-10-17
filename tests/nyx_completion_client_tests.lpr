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

program nyx_completion_client_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.data, nyx.pascal.names, nyx.pascal.project,
  nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion,
  nyx.studio.completion, nyx.studio.exchange, nyx.studio.workspaces
  {$ifdef PAS2JS}, Web{$endif};

type
  { Deterministic asynchronous exchange seam: Post captures but never invokes.
    Flush represents a later UI turn; cancellation clears the borrowed callback.
    This substitutes sockets and qualifies no native/browser HTTP transport. }
  TExchange = class(TNyxStudioEditorExchange)
  public
    Pending: TNyxEditorReply;
    Body: TNyxText;
    Token: TNyxText;
    Posts: Integer;
    Cancels: Integer;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
    procedure Flush(AStatus: Integer; const AText: TNyxText);
  end;

  { Managed receiver retains only detached result/query data. It has no client,
    exchange, widget or session back-reference. }
  TReceiver = class(TInterfacedObject, INyxStudioCompletionReply)
  public
    Count: Integer;
    Query: INyxPascalCompletionRequest;
    ResultValue: INyxPascalCompletionResult;
    procedure Receive(const AQuery: INyxPascalCompletionRequest;
      const AResult: INyxPascalCompletionResult);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

procedure TExchange.Post(AConnect: Boolean; const AToken, ABody: TNyxText;
  AReply: TNyxEditorReply);
begin

  if AConnect then
  begin
    raise Exception.Create('Completion cannot acquire authority through connect');
  end;
  Token := AToken;
  Body := ABody;
  Pending := AReply;
  Inc(Posts);
end;

procedure TExchange.CancelRequest;
begin
  Pending := nil;
  Inc(Cancels);
end;

procedure TExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin
  raise Exception.Create('Completion client must not own a synchronization timer');
end;

procedure TExchange.CancelTick;
begin
end;

procedure TExchange.Flush(AStatus: Integer; const AText: TNyxText);
var
  LReply: TNyxEditorReply;
begin
  LReply := Pending;
  Pending := nil;

  if Assigned(LReply) then
  begin
    LReply(AStatus, AText);
  end;
end;

procedure TReceiver.Receive(const AQuery: INyxPascalCompletionRequest;
  const AResult: INyxPascalCompletionResult);
begin
  Inc(Count);
  Query := AQuery;
  ResultValue := AResult;
end;

procedure Run;
const
  CAccepted: TNyxText = 'unit client.fixture; interface implementation end.';
  CBuffer: TNyxText = '// Client Unicode 🚀 𐐷' + #10 + 'LBadge.WithTe';
  CDetail: TNyxText = 'A typed method 🚀';
var
  LExchange: TExchange;
  LClient: TNyxStudioCompletionClient;
  LReceiver: TReceiver;
  LRetained: INyxStudioCompletionReply;
  LQuery: INyxPascalCompletionRequest;
  LSources: INyxPascalSourceSnapshot;
  LPacket: TNyxDataValue;
  LArgs: TNyxDataValue;
  LPage: TNyxDataValue;
  LEnvelope: TNyxDataValue;
  LID: TNyxText;
  LRejected: Boolean;
begin
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(
    NyxPascalUnit('client.fixture'), CAccepted))
    .WithBuffer(TNyxPascalSourceRef.Main, CBuffer);
  { The actual request factory validates scalar bounds; choose a stable scalar
    position independently of native UTF-8/browser UTF-16 storage lengths. }
  LQuery := NewNyxPascalCompletionRequest(LSources).At(TNyxPascalSourceRef.Main, 5).Take(8);
  LExchange := TExchange.Create;
  LClient := TNyxStudioCompletionClient.Create(LExchange);
  LReceiver := TReceiver.Create;
  LRetained := LReceiver;
  try
    LClient.Request('private-fixture-capability', NyxPrimaryWorkspace, 4, LQuery, LRetained);
    Check(LClient.Busy and (LReceiver.Count = 0) and (LExchange.Posts = 1),
      'Post is asynchronous and retains a managed receiver until the later turn');
    LPacket := TNyxDataValue.ParseJSON(LExchange.Body);
    LArgs := NyxStudioCompletionArguments(LPacket, LID);
    Check((LArgs.Field('mode').AsText = 'complete') and
      (LArgs.Field('expectedRevision').AsInteger = 4) and
      (LArgs.Field('caret').AsInteger = 5) and
      (LArgs.Field('snapshot').AsText = 'editing'),
      'Private selectors preserve exact acknowledged revision, origin and caret');
    Check((Pos(CBuffer, LExchange.Body) = 0) and (Length(LExchange.Body) < 1024) and
      (LExchange.Token = 'private-fixture-capability'),
      'Selectors stay small and authority travels separately from source-free packet data');

    LPage := NyxObject([NyxField('revision', NyxData(4)), NyxField('status', NyxData('ready')),
      NyxField('message', NyxData('')), NyxField('total', NyxData(1)),
      NyxField('replacement', NyxObject([NyxField('offset', NyxData(5)),
        NyxField('expected', NyxData(''))])),
      NyxField('items', NyxArray([NyxObject([NyxField('name', NyxData('WithText')),
        NyxField('insert', NyxData('WithText')), NyxField('detail', NyxData(CDetail)),
        NyxField('kind', NyxData('method'))])]))]);
    LEnvelope := NyxStudioCompletionEnvelope(LPage, LID, NyxPrimaryWorkspace);
    LExchange.Flush(200, LEnvelope.ToJSON);
    Check(not LClient.Busy and (LReceiver.Count = 1) and
      (LReceiver.ResultValue.Status = pcsReady) and
      (LReceiver.ResultValue.Item(0).Kind = pckMethod) and
      (LReceiver.ResultValue.Item(0).Detail = CDetail) and
      LReceiver.Query.Matches(LSources), 'Later delivery preserves bounded typed rows and the caller-owned frozen query');

    LClient.Request('private-fixture-capability', NyxPrimaryWorkspace, 4, LQuery, LRetained);
    LClient.Cancel;
    LExchange.Flush(200, LEnvelope.ToJSON);
    Check(not LClient.Busy and (LReceiver.Count = 1), 'Cancelled local delivery never reaches a retired receiver');
    LClient.Request('private-fixture-capability', NyxPrimaryWorkspace, 4, LQuery, LRetained);
    LExchange.Flush(200, LEnvelope.ToJSON);
    Check((LReceiver.Count = 2) and (LReceiver.ResultValue.Status = pcsFailed),
      'A foreign request identity cannot become a ready page');
    LClient.Request('private-fixture-capability', NyxPrimaryWorkspace, 4, LQuery, LRetained);
    LExchange.Flush(500, 'PRIVATE-SERVER-PATH-MUST-NOT-ESCAPE');
    Check((LReceiver.ResultValue.Status = pcsFailed) and
      (Pos('PRIVATE-SERVER-PATH', LReceiver.ResultValue.MessageText) = 0),
      'Transport failures provide scoped feedback without leaking server diagnostics');

    LClient.Request('private-fixture-capability', NyxPrimaryWorkspace, 5, LQuery, LRetained);
    LPacket := TNyxDataValue.ParseJSON(LExchange.Body);
    LEnvelope := NyxStudioCompletionEnvelope(LPage, LPacket.Field('requestId').AsText,
      NyxPrimaryWorkspace);
    LExchange.Flush(200, LEnvelope.ToJSON);
    Check(LReceiver.ResultValue.Status = pcsFailed,
      'A correctly correlated response still refuses another source revision');
    LClient.Request('private-fixture-capability', NyxPrimaryWorkspace, 4, LQuery, LRetained);
    LPacket := TNyxDataValue.ParseJSON(LExchange.Body);
    LEnvelope := NyxStudioCompletionEnvelope(LPage, LPacket.Field('requestId').AsText,
      NyxWorkspace('foreign-workspace'));
    LExchange.Flush(200, LEnvelope.ToJSON);
    Check(LReceiver.ResultValue.Status = pcsFailed,
      'A correctly correlated response still refuses another project');

    LRejected := False;
    try
      NyxStudioCompletionArguments(NyxObject([
        NyxField('op', NyxData('completion')), NyxField('requestId', NyxData('bad')),
        NyxField('expectedRevision', NyxData(4)), NyxField('caret', NyxData(5)),
        NyxField('operator', NyxData(True))]), LID);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LID = ''), 'Private packets cannot assert their own authority class');
  finally
    LClient.Free;
    LRetained := nil;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' operator completion client checks';
    {$else}
    WriteLn('PASS ', GChecks, ' operator completion client checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
