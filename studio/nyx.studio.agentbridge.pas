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

unit nyx.studio.agentbridge;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.studio.session,
  nyx.studio.projects, nyx.studio.agents, nyx.studio.agentview;

type
  TNyxAgentRefresh = procedure(AContentChanged: Boolean) of object;

  { Browser controller for the private editor exchange. Borrows its ordinary
    Studio session; server owns authoritative paired history. Local publications
    queue in order, each becoming one server history command. A revision race
    freezes synchronization and retains local pair/draft/queue for explicit
    operator resolution. Observing requests are small until revision changes. }
  TNyxStudioAgentBridge = class
  private
    FSession: TNyxStudioSession;
    FView: TNyxStudioAgentView;
    FRequest: TJSXMLHttpRequest;
    FToken: TNyxText;
    FKnownFrame: TNyxText;
    FSentFrame: TNyxText;
    FSent: TNyxDataValue;
    FQueue: array of TNyxDataValue;
    FQueueSizes: array of Integer;
    FQueueUnits: Integer;
    FTimer: NativeInt;
    FApplying: Boolean;
    FEnabled: Boolean;
    FAcceptRemote: Boolean;
    FActivitySerial: Integer;
    FInitialProject: TNyxText;
    FProtectLocal: Boolean;
    FWarning: TNyxText;
    FOnRefresh: TNyxAgentRefresh;
    function Frame: TNyxText;
    procedure Send(const AMessage: TNyxDataValue; AConnect: Boolean = False);
    procedure Ready;
    procedure Tick;
    procedure Schedule;
    procedure Queue(const AMessage: TNyxDataValue);
  public
    constructor Create(ASession: TNyxStudioSession; ARefresh: TNyxAgentRefresh);
    destructor Destroy; override;
    procedure Connect;
    { Call after ordinary editor publications, including draft typing. Does not
      poll or reconcile complete source while the editor has remained unchanged. }
    procedure RecordLocal;
    procedure Configure(APermission: TNyxAgentPermission);
    procedure History(const ADirection: TNyxText);
    procedure CompilerReport(const AReport: TNyxText);
    procedure Pause;
    procedure AcceptRemote;
    function State: TNyxStudioAgentView;
    { Exact locally retained pair/frame must match the acknowledged service
      frame before server diagnostics can borrow this observer's source text.
      Pending local publications and protected conflicts cannot grant navigation. }
    function SourceSynchronized: Boolean;
    property Applying: Boolean read FApplying;
    property Enabled: Boolean read FEnabled;
  end;

implementation

function TNyxStudioAgentBridge.SourceSynchronized: Boolean;
begin
  Result := FView.Connected and not FView.Conflict and
    (Length(FQueue) = 0) and (Frame = FKnownFrame);
end;

constructor TNyxStudioAgentBridge.Create(ASession: TNyxStudioSession;
  ARefresh: TNyxAgentRefresh);
begin
  inherited Create;
  FSession := ASession;
  FView := DefaultNyxStudioAgentView;
  FTimer := -1;
  FInitialProject := EncodeNyxProject(FSession.ProjectSnapshot);
  FOnRefresh := ARefresh;
end;

destructor TNyxStudioAgentBridge.Destroy;
begin
  FEnabled := False;

  if FTimer >= 0 then
  begin
    window.clearTimeout(FTimer);
  end;

  if FRequest <> nil then
  begin
    FRequest.onreadystatechange := nil;
    FRequest.abort;
  end;
  FOnRefresh := nil;
  FSession := nil;
  inherited Destroy;
end;

function TNyxStudioAgentBridge.State: TNyxStudioAgentView;
begin
  Result := FView;
end;

function TNyxStudioAgentBridge.Frame: TNyxText;
begin
  Result := NyxObject([
    NyxField('project', NyxData(EncodeNyxProject(FSession.ProjectSnapshot))),
    NyxField('selection', NyxData(FSession.SelectedID)),
    NyxField('view', NyxData(FSession.ActiveViewID))]).ToJSON;
end;

procedure TNyxStudioAgentBridge.Connect;
var
  LFrame: TNyxDataValue;
begin

  if FRequest <> nil then
  begin
    Exit;
  end;
  FEnabled := True;
  FView.Conflict := False;
  FView.Connected := False;
  FView.Status := 'Connecting agent session';
  FKnownFrame := Frame;
  FProtectLocal := EncodeNyxProject(FSession.ProjectSnapshot) <> FInitialProject;
  LFrame := TNyxDataValue.ParseJSON(FKnownFrame);
  Send(NyxObject([NyxField('op', NyxData('claim')),
    NyxField('project', LFrame.Field('project')),
    NyxField('selection', LFrame.Field('selection')),
    NyxField('view', LFrame.Field('view'))]), True);
end;

procedure TNyxStudioAgentBridge.Send(const AMessage: TNyxDataValue; AConnect: Boolean);
var
  LURL: TNyxText;
begin
  FSent := AMessage.Copy;
  FSentFrame := FKnownFrame;
  FView.Busy := True;
  FRequest := TJSXMLHttpRequest.new;
  FRequest.onreadystatechange := @Ready;
  LURL := 'api/agents';

  if AConnect then
  begin
    LURL := 'api/agents/connect';
  end;
  FRequest.open('POST', LURL, True);
  FRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');

  if not AConnect then
  begin
    FRequest.setRequestHeader('X-Nyx-Editor', FToken);
  end;
  FRequest.send(AMessage.ToJSON);
end;

procedure TNyxStudioAgentBridge.Queue(const AMessage: TNyxDataValue);
var
  LSize: Integer;
  LLast: Integer;
  LPrevious: TNyxProjectPair;
  LCurrent: TNyxProjectPair;
  LMayMerge: Boolean;
begin
  LSize := Length(AMessage.ToJSON);
  LLast := High(FQueue);
  LMayMerge := (LLast >= 0) and (AMessage.Field('op').AsText = 'commit');

  if LMayMerge and (FRequest <> nil) and (LLast = 0) then
  begin
    LMayMerge := FSent.Field('op').AsText <> 'commit';
  end;

  if LMayMerge and (FQueue[LLast].Field('op').AsText = 'commit') then
  begin
    LCurrent := DecodeNyxProject(AMessage.Field('project').AsText);
    LPrevious := DecodeNyxProject(FQueue[LLast].Field('project').AsText);
    { Coalesce only unsent draft updates of the SAME accepted pair. Content
      publications remain ordered, one per history command. Never replace an
      in-flight queue head whose acknowledgement would discard the newer draft. }

    if LCurrent.Pending and (LCurrent.Design = LPrevious.Design) and
      (LCurrent.Source = LPrevious.Source) and
      (LSize <= 8 * 1024 * 1024 - FQueueUnits + FQueueSizes[LLast]) then
    begin
      Dec(FQueueUnits, FQueueSizes[LLast]);
      FQueue[LLast] := AMessage.Copy;
      FQueueSizes[LLast] := LSize;
      Inc(FQueueUnits, LSize);
      Exit;
    end;
  end;

  if (Length(FQueue) >= 100) or (LSize > 8 * 1024 * 1024 - FQueueUnits) then
  begin
    FView.Conflict := True;
    FView.Status := 'Agent synchronization queue is full; local work is retained';
    Exit;
  end;
  SetLength(FQueue, Length(FQueue) + 1);
  FQueue[High(FQueue)] := AMessage.Copy;
  SetLength(FQueueSizes, Length(FQueueSizes) + 1);
  FQueueSizes[High(FQueueSizes)] := LSize;
  Inc(FQueueUnits, LSize);
end;

procedure TNyxStudioAgentBridge.RecordLocal;
var
  LFrame: TNyxText;
  LData: TNyxDataValue;
begin

  if not FEnabled or FApplying or FView.Conflict then
  begin
    Exit;
  end;
  LFrame := Frame;

  if LFrame = FKnownFrame then
  begin
    Exit;
  end;
  LData := TNyxDataValue.ParseJSON(LFrame);
  Queue(NyxObject([NyxField('op', NyxData('commit')),
    NyxField('project', LData.Field('project')),
    NyxField('selection', LData.Field('selection')), NyxField('view', LData.Field('view'))]));
  FKnownFrame := LFrame;
  if FRequest = nil then
  begin
    Tick;
  end;
end;

procedure TNyxStudioAgentBridge.Configure(APermission: TNyxAgentPermission);
begin
  Queue(NyxObject([NyxField('op', NyxData('configure')),
    NyxField('permission', NyxData(NyxAgentPermissionName(APermission)))]));
  Tick;
end;

procedure TNyxStudioAgentBridge.History(const ADirection: TNyxText);
begin
  RecordLocal;
  Queue(NyxObject([NyxField('op', NyxData('history')),
    NyxField('direction', NyxData(ADirection))]));
  Tick;
end;

procedure TNyxStudioAgentBridge.CompilerReport(const AReport: TNyxText);
begin

  if not FEnabled then
  begin
    Exit;
  end;
  Queue(NyxObject([NyxField('op', NyxData('report')), NyxField('report', NyxData(AReport))]));
end;

procedure TNyxStudioAgentBridge.Pause;
begin
  FEnabled := False;
  FView.Connected := False;
  FView.Status := 'Agent sync paused; your local work is retained';

  if FTimer >= 0 then
  begin
    window.clearTimeout(FTimer);
  end;
  FTimer := -1;
  { A paused observer must not apply an already-in-flight response after the
    operator chose to keep local work. Server admission may finish independently. }

  if FRequest <> nil then
  begin
    FRequest.onreadystatechange := nil;
    FRequest.abort;
    FRequest := nil;
  end;
  FView.Busy := False;
end;

procedure TNyxStudioAgentBridge.AcceptRemote;
begin
  FQueue := nil;
  FQueueSizes := nil;
  FQueueUnits := 0;
  FAcceptRemote := True;
  FView.Conflict := False;
  Send(NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(0))]));
end;

procedure TNyxStudioAgentBridge.Schedule;
begin

  if FEnabled and not FView.Conflict and (FTimer < 0) then
  begin
    if Length(FQueue) > 0 then
    begin
      FTimer := window.setTimeout(@Tick, 25);
    end
    else
    begin
      FTimer := window.setTimeout(@Tick, 500);
    end;
  end;
end;

procedure TNyxStudioAgentBridge.Tick;
var
  LMessage: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LOperation: TNyxText;
  LFieldCount: Integer;
begin

  if FTimer >= 0 then
  begin
    window.clearTimeout(FTimer);
  end;
  FTimer := -1;

  if not FEnabled or FView.Conflict then
  begin
    Exit;
  end;

  if (FRequest <> nil) or not FView.Connected then
  begin
    Schedule;
    Exit;
  end;

  if Length(FQueue) > 0 then
  begin
    LMessage := FQueue[0];
    LOperation := LMessage.Field('op').AsText;
    LFieldCount := LMessage.Count;
    SetLength(LFields, LFieldCount + 1);
    for LIndex := 0 to LFieldCount - 1 do
    begin
      LFields[LIndex] := NyxField(LMessage.Key(LIndex), LMessage.Field(LMessage.Key(LIndex)));
    end;
    LFields[LFieldCount] := NyxField('after', NyxData(FView.Revision));

    if (LOperation = 'commit') or (LOperation = 'history') then
    begin
      SetLength(LFields, Length(LFields) + 1);
      LFields[High(LFields)] := NyxField('expectedRevision', NyxData(FView.Revision));
    end;
    Send(NyxObject(LFields));
  end
  else
  begin
    Send(NyxObject([NyxField('op', NyxData('observe')), NyxField('after', NyxData(FView.Revision))]));
  end;
end;

procedure TNyxStudioAgentBridge.Ready;
var
  LData: TNyxDataValue;
  LState: TNyxDataValue;
  LSummary: TNyxDataValue;
  LPair: TNyxProjectPair;
  LRemoteFrame: TNyxText;
  LOperation: TNyxText;
  LChanged: Boolean;
  LRefresh: Boolean;
  LIndex: Integer;
  LStatus: Integer;
  LText: TNyxText;
  LPermission: TNyxText;
begin

  if (FRequest = nil) or (FRequest.readyState <> 4) then
  begin
    Exit;
  end;
  LStatus := FRequest.status;
  LText := FRequest.responseText;
  FRequest.onreadystatechange := nil;
  FRequest := nil;
  FView.Busy := False;
  LChanged := False;
  LRefresh := False;
  FApplying := True;
  try
    try

      if LStatus <> 200 then
      begin
        if LText <> '' then
        begin
          LData := TNyxDataValue.ParseJSON(LText);

          if NyxAgentHas(LData, 'error') then
          begin
            raise Exception.Create('Agent sync refused: ' + LData.Field('error').AsText + '. Local work is retained');
          end;
        end;
        raise Exception.Create('Agent sync unavailable; local work is retained');
      end;
      LData := TNyxDataValue.ParseJSON(LText);
      LOperation := FSent.Field('op').AsText;

      if LOperation = 'claim' then
      begin
        FToken := LData.Field('token').AsText;
        FView.Endpoint := LData.Field('endpoint').AsText;
        if NyxAgentHas(LData, 'warning') then
        begin
          FWarning := LData.Field('warning').AsText;
        end;
        LState := LData.Field('state');
      end
      else
      begin
        LState := LData;
      end;
      LSummary := LState.Field('session');
      LRefresh := (LSummary.Field('activitySequence').AsInteger <>
        FActivitySerial) or not FView.Connected;
      FActivitySerial := LSummary.Field('activitySequence').AsInteger;
      FView.Connected := True;
      LPermission := LSummary.Field('permission').AsText;

      if LPermission = 'disabled' then
      begin
        FView.Permission := apDisabled;
      end
      else if LPermission = 'readOnly' then
      begin
        FView.Permission := apReadOnly;
      end
      else if LPermission = 'edit' then
      begin
        FView.Permission := apEdit;
      end
      else
      begin
        raise Exception.Create('Invalid agent permission response');
      end;
      FView.Activity := LState.Field('activity').Copy;

      if NyxAgentHas(LState, 'reviews') then
      begin
        FView.Reviews := LState.Field('reviews').Copy;
      end;

      if NyxAgentHas(LState, 'compiler') then
      begin
        FView.Compiler := LState.Field('compiler').Copy;
      end;

      if (LOperation <> 'observe') and (LOperation <> 'claim') and (Length(FQueue) > 0) then
      begin
        Dec(FQueueUnits, FQueueSizes[0]);
        for LIndex := 1 to High(FQueue) do
        begin
          FQueue[LIndex - 1] := FQueue[LIndex];
          FQueueSizes[LIndex - 1] := FQueueSizes[LIndex];
        end;
        SetLength(FQueue, Length(FQueue) - 1);
        SetLength(FQueueSizes, Length(FQueueSizes) - 1);
      end;

      if NyxAgentHas(LState, 'project') then
      begin
        LRemoteFrame := NyxObject([NyxField('project', LState.Field('project')),
          NyxField('selection', LSummary.Field('selection')),
          NyxField('view', LSummary.Field('view'))]).ToJSON;

        if (LOperation = 'observe') or (LOperation = 'claim') or
          ((LOperation = 'history') and (Length(FQueue) = 0)) then
        begin

          if (LOperation = 'claim') and FProtectLocal and
            (LRemoteFrame <> FSentFrame) and not FAcceptRemote then
          begin
            raise Exception.Create('A different shared design is active; your recovered/local project and draft are retained');
          end;

          if not FAcceptRemote and (Frame <> FSentFrame) and
            (LRemoteFrame <> FSentFrame) then
          begin
            raise Exception.Create('Shared revision changed while you edited; local work and draft are retained');
          end;

          if (Length(FQueue) = 0) or FAcceptRemote then
          begin
            LPair := DecodeNyxProject(LState.Field('project').AsText);
            LChanged := EncodeNyxProject(FSession.ProjectSnapshot) <> EncodeNyxProject(LPair);

            if LChanged then
            begin
              FSession.LoadProject(LPair);
            end;

            if LSummary.Field('view').AsText <> '' then
            begin
              FSession.Activate(LSummary.Field('view').AsText);
            end;

            if LSummary.Field('selection').AsText <> '' then
            begin
              FSession.Select(LSummary.Field('selection').AsText);
            end;
            FKnownFrame := Frame;
          end;
        end;
      end;
      FAcceptRemote := False;
      FView.Revision := LSummary.Field('revision').AsInteger;
      FView.Status := 'Agents ' + NyxAgentPermissionName(FView.Permission) + ' · revision ' + IntToStr(FView.Revision);
      if FWarning <> '' then
      begin
        FView.Status := FView.Status + ' · ' + FWarning;
      end;

      if Length(FQueue) > 0 then
      begin
        FView.Status := 'Synchronizing editor changes';
      end;
      FView.Conflict := False;
      Schedule;
    except
      on LException: Exception do
      begin
        FView.Conflict := True;
        FView.Status := LException.Message;
        LRefresh := True;
      end;
    end;

    if Assigned(FOnRefresh) and (LRefresh or LChanged) then
    begin
      FOnRefresh(LChanged);
    end;
  finally
    FApplying := False;
  end;
end;

end.
