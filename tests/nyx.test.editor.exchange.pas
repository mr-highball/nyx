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

unit nyx.test.editor.exchange;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.agents, nyx.studio.exchange;

type
  { Deterministic qualification clock/transport for the real portable editor
    protocol. It borrows one independent agent session, owns request bytes and
    response snapshots, and never calls a receiver inside Post. Tests choose the
    actual reply/tick order to exercise races without starting an HTTP listener.
    This does not qualify authentication, sockets, elapsed timer delay or widgets. }
  TNyxTestEditorExchange = class(TNyxStudioEditorExchange)
  private
    FSession: TNyxAgentSession;
    FReply: TNyxEditorReply;
    FTick: TNyxEditorTick;
    FConnect: Boolean;
    FBody: TNyxDataValue;
    FResponse: TNyxText;
    FStatus: Integer;
    FPrepared: Boolean;
    FPosts: Integer;
    FCommits: Integer;
    FSchedules: Integer;
    FDelay: Integer;
    FLegacyResourceObservations: Boolean;
  public
    constructor Create(ASession: TNyxAgentSession);
    destructor Destroy; override;
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); override;
    procedure CancelRequest; override;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); override;
    procedure CancelTick; override;
    { Execute protocol admission now but retain its immutable reply. AFull
      requests a legitimate complete observation at after=0, making an unchanged
      older frame visible when testing unsent-draft protection. }
    procedure PrepareReply(AFull: Boolean = False);
    procedure Deliver;
    procedure FireTick;
    function RequestPending: Boolean;
    function TickPending: Boolean;
    { Copied bounded request inspection qualifies optional-field negotiation.
      LegacyResourceObservations simulates a peer predating selected-resource
      support: it omits the capability and refuses the unknown request member.
      Modern positive replies always come from the real session protocol. }
    function PendingBody: TNyxDataValue;
    property LegacyResourceObservations: Boolean read FLegacyResourceObservations
      write FLegacyResourceObservations;
    property Posts: Integer read FPosts;
    property Commits: Integer read FCommits;
    property Schedules: Integer read FSchedules;
    property Delay: Integer read FDelay;
  end;

implementation

constructor TNyxTestEditorExchange.Create(ASession: TNyxAgentSession);
begin
  inherited Create;

  if ASession = nil then
  begin
    raise Exception.Create('A test editor exchange requires its independent session');
  end;
  FSession := ASession;
end;

destructor TNyxTestEditorExchange.Destroy;
begin
  CancelRequest;
  CancelTick;
  inherited Destroy;
end;

procedure TNyxTestEditorExchange.Post(AConnect: Boolean;
  const AToken, ABody: TNyxText; AReply: TNyxEditorReply);
begin

  if Assigned(FReply) then
  begin
    raise Exception.Create('The test transport already owns an undelivered request');
  end;
  FConnect := AConnect;
  FBody := TNyxDataValue.ParseJSON(ABody);
  FReply := AReply;
  FPrepared := False;
  Inc(FPosts);
end;

procedure TNyxTestEditorExchange.CancelRequest;
begin
  FReply := nil;
  FResponse := '';
  FPrepared := False;
end;

procedure TNyxTestEditorExchange.Schedule(ADelayMS: Integer; ATick: TNyxEditorTick);
begin

  if ADelayMS < 0 then
  begin
    raise Exception.Create('A scheduled editor tick requires a nonnegative delay');
  end;
  FTick := ATick;
  FDelay := ADelayMS;
  Inc(FSchedules);
end;

procedure TNyxTestEditorExchange.CancelTick;
begin
  FTick := nil;
end;

procedure TNyxTestEditorExchange.PrepareReply(AFull: Boolean);
var
  LState: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LCount: Integer;
begin

  if not Assigned(FReply) or FPrepared then
  begin
    raise Exception.Create('Prepare requires one unprepared owned request');
  end;
  FStatus := 200;
  try

    if FLegacyResourceObservations then
    begin
      for LIndex := 0 to FBody.Count - 1 do
      begin

        if FBody.Key(LIndex) = 'resource' then
        begin
          raise Exception.Create('Historical resource observer refuses an unknown selector');
        end;
      end;
    end;
    LState := FSession.Exchange(FBody);

    if FBody.Field('op').AsText = 'commit' then
    begin
      Inc(FCommits);
    end;

    if AFull then
    begin
      LState := FSession.Exchange(NyxObject([
        NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
    end;

    if FLegacyResourceObservations then
    begin
      SetLength(LFields, LState.Count);
      LCount := 0;
      for LIndex := 0 to LState.Count - 1 do
      begin

        if LState.Key(LIndex) <> 'resourceRuntimeSelection' then
        begin
          LFields[LCount] := NyxField(LState.Key(LIndex), LState.Field(LState.Key(LIndex)));
          Inc(LCount);
        end;
      end;
      SetLength(LFields, LCount);
      LState := NyxObject(LFields);
    end;

    if FConnect then
    begin
      LState := NyxObject([NyxField('token', NyxData('fixture-owned')),
        NyxField('endpoint', NyxData('')), NyxField('state', LState)]);
    end;
    FResponse := LState.ToJSON;
  except
    on LException: Exception do
    begin
      FStatus := 409;
      FResponse := NyxObject([NyxField('error',
        NyxData(TNyxText(LException.Message)))]).ToJSON;
    end;
  end;
  FPrepared := True;
end;

procedure TNyxTestEditorExchange.Deliver;
var
  LReply: TNyxEditorReply;
begin

  if not Assigned(FReply) then
  begin
    raise Exception.Create('Deliver requires an owned request');
  end;

  if not FPrepared then
  begin
    PrepareReply;
  end;
  LReply := FReply;
  FReply := nil;
  LReply(FStatus, FResponse);
end;

procedure TNyxTestEditorExchange.FireTick;
var
  LTick: TNyxEditorTick;
begin
  LTick := FTick;
  FTick := nil;

  if Assigned(LTick) then
  begin
    LTick;
  end;
end;

function TNyxTestEditorExchange.RequestPending: Boolean;
begin
  Result := Assigned(FReply);
end;

function TNyxTestEditorExchange.PendingBody: TNyxDataValue;
begin
  Result := FBody.Copy;
end;

function TNyxTestEditorExchange.TickPending: Boolean;
begin
  Result := Assigned(FTick);
end;

end.
