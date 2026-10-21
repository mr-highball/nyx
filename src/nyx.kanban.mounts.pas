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

unit nyx.kanban.mounts;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.model, nyx.kanban, nyx.collections.bindings, nyx.events, nyx.scheduler;

type
  { Parallel managed vectors avoid interfaces inside value records on pas2js.
    They retain prepared/lifetime owners, never a platform widget or renderer. }
  TNyxKanbanMounts = array of INyxPreparedKanbanBinding;
  { Exact realized identities, independent of captions, row positions and the
    source definition's unqualified component name. }
  TNyxKanbanMountIDs = array of TNyxText;

function NyxHasKanbanMounts(ARoot: TNyxNode): Boolean;
procedure PrepareNyxKanbanMounts(ARoot: TNyxNode;
  const AViews: INyxCollectionBindings; const AEventView: INyxPreparedEventView;
  const AScheduler: INyxScheduler; ARefresh: TNyxKanbanRefresh;
  out AMounts: TNyxKanbanMounts; out AIDs: TNyxKanbanMountIDs);
{ Outward view-readiness admission belongs to Validate, before old retirement.
  The later fence inspects the prepared owners, without host work. }
procedure ValidateNyxKanbanMounts(const AMounts: TNyxKanbanMounts);
procedure CheckNyxKanbanMounts(const AMounts: TNyxKanbanMounts);
procedure InstallNyxKanbanMounts(const AMounts: TNyxKanbanMounts);
procedure RevokeNyxKanbanMounts(const AMounts: TNyxKanbanMounts);
procedure DisconnectNyxKanbanMounts(var AMounts: TNyxKanbanMounts);
function FindNyxKanbanMount(const AMounts: TNyxKanbanMounts;
  const AIDs: TNyxKanbanMountIDs; const AID: TNyxText): INyxKanbanBinding;

implementation

uses
  SysUtils, nyx.controls, nyx.kanban.intent, nyx.publication;

function NyxHasKanbanMounts(ARoot: TNyxNode): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if ARoot = nil then
  begin
    Exit;
  end;

  if HasNyxKanbanIntent(ARoot) then
  begin
    Exit(True);
  end;
  for LIndex := 0 to ARoot.Count - 1 do
  begin

    if NyxHasKanbanMounts(ARoot.Children[LIndex]) then
    begin
      Exit(True);
    end;
  end;
end;

procedure PrepareNyxKanbanMounts(ARoot: TNyxNode;
  const AViews: INyxCollectionBindings; const AEventView: INyxPreparedEventView;
  const AScheduler: INyxScheduler; ARefresh: TNyxKanbanRefresh;
  out AMounts: TNyxKanbanMounts; out AIDs: TNyxKanbanMountIDs);
var
  LMounts: TNyxKanbanMounts;
  LIDs: TNyxKanbanMountIDs;

  procedure Visit(ANode: TNyxNode);
  var
    LControl: INyxControl;
    LBoard: INyxKanbanBoard;
    LIntent: TNyxKanbanIntent;
    LPlan: INyxPreparedKanbanBinding;
    LCount: Integer;
    LIndex: Integer;
  begin

    if HasNyxKanbanIntent(ANode) then
    begin
      LIntent := GetNyxKanbanIntent(ANode);
      LControl := RetainNyxControl(ANode);

      if not Supports(LControl, INyxKanbanBoard, LBoard) then
      begin
        raise ENyxKanban.Create('Automatic movement requires a specialized Kanban board');
      end;

      if LIntent.Mode = nkiDisabled then
      begin
        LPlan := PrepareNyxKanbanDisabled(LBoard, AEventView, AScheduler);
      end
      else
      begin
        LPlan := PrepareNyxKanbanBinding(LBoard, LIntent.Options, AViews,
          AEventView, AScheduler, ARefresh);
      end;
      LCount := Length(LMounts);
      SetLength(LMounts, LCount + 1);
      SetLength(LIDs, LCount + 1);
      LMounts[LCount] := LPlan;
      LIDs[LCount] := ANode.ID;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LIndex]);
    end;
  end;

begin
  AMounts := nil;
  AIDs := nil;
  LMounts := nil;
  LIDs := nil;

  if ARoot = nil then
  begin
    Exit;
  end;
  try
    { Keep traversal state local. Besides atomic output publication, this
      avoids capturing Pascal out-argument cells in pas2js nested procedures. }
    Visit(ARoot);
    AMounts := LMounts;
    AIDs := LIDs;
  except
    DisconnectNyxKanbanMounts(LMounts);
    raise;
  end;
end;

procedure ValidateNyxKanbanMounts(const AMounts: TNyxKanbanMounts);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AMounts) do
  begin
    AMounts[LIndex].Validate;
  end;
end;

procedure CheckNyxKanbanMounts(const AMounts: TNyxKanbanMounts);
var
  LIndex: Integer;
  LFence: INyxPublicationReadiness;
begin
  for LIndex := 0 to High(AMounts) do
  begin

    if not Supports(AMounts[LIndex], INyxPublicationReadiness, LFence) then
    begin
      raise ENyxKanban.Create('Prepared Kanban owner has no final readiness fence');
    end;
    LFence.CheckReady;
  end;
end;

procedure InstallNyxKanbanMounts(const AMounts: TNyxKanbanMounts);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AMounts) do
  begin
    AMounts[LIndex].Install;
  end;
end;

procedure RevokeNyxKanbanMounts(const AMounts: TNyxKanbanMounts);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(AMounts) do
  begin
    AMounts[LIndex].Revoke;
  end;
end;

procedure DisconnectNyxKanbanMounts(var AMounts: TNyxKanbanMounts);
var
  LIndex: Integer;
  LFailure: TNyxText;
begin
  LFailure := '';
  RevokeNyxKanbanMounts(AMounts);
  for LIndex := 0 to High(AMounts) do
  begin
    try
      AMounts[LIndex].Disconnect;
    except
      on E: Exception do
      begin

        if LFailure = '' then
        begin
          {$ifdef PAS2JS}
          LFailure := E.Message;
          {$else}
          LFailure := UTF8Encode(UnicodeString(E.Message));
          {$endif}
        end;
      end;
    end;
  end;
  AMounts := nil;

  if LFailure <> '' then
  begin
    raise ENyxKanban.Create('Kanban retirement completed with cleanup failure: ' + LFailure);
  end;
end;

function FindNyxKanbanMount(const AMounts: TNyxKanbanMounts;
  const AIDs: TNyxKanbanMountIDs; const AID: TNyxText): INyxKanbanBinding;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to High(AMounts) do
  begin

    if AIDs[LIndex] = AID then
    begin
      Exit(AMounts[LIndex].Binding);
    end;
  end;
end;

end.
