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
unit nyx.operation.panel;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.model, nyx.controls, nyx.behavior;

type
  { Presentation for a host-owned operation. A panel never starts work, holds
    a transport, or grants permission. The host validates its current operation
    and the retained control before honoring an action. Zero/default is hidden. }
  TNyxOperationPhase = (nopHidden, nopWaiting, nopRunning, nopCancelling,
    nopCompleted, nopCancelled, nopFailed);
  TNyxOperationAction = (noaStart, noaCancel, noaRetry, noaConfigure, noaCheck,
    noaDismiss);
  TNyxOperationActions = set of TNyxOperationAction;
  { Owns text and integer progress only; no borrowed view/model or callbacks.
    Counters represent completed units rather than rounded percentages. }
  TNyxOperationPresentation = record
  private
    FPhase: TNyxOperationPhase;
    FTitle: TNyxText;
    FDetail: TNyxText;
    FCompleted: Integer;
    FTotal: Integer;
    FActions: TNyxOperationActions;
  public
    class function New(const ATitle: TNyxText; APhase: TNyxOperationPhase;
      const ADetail: TNyxText): TNyxOperationPresentation; static;
    { Reject negative counters or completed > total before producing a copy. }
    function Progress(ACompleted, ATotal: Integer): TNyxOperationPresentation;
    function Actions(AValue: TNyxOperationActions): TNyxOperationPresentation;
    property Phase: TNyxOperationPhase read FPhase;
    property Title: TNyxText read FTitle;
    property Detail: TNyxText read FDetail;
    property Completed: Integer read FCompleted;
    property Total: Integer read FTotal;
    property AvailableActions: TNyxOperationActions read FActions;
  end;

{ A managed compound made from specialized label/progress/button controls.
  Named fixed parts support theming and extensions; descendants have independent
  ordinary Nyx ownership. No DOM, LCL or operation lifetime leaks into the model. }
function NewNyxOperationPanel(const AID: TNyxText;
  const AState: TNyxOperationPresentation): INyxPanel;
{ Pure identity derivation; an ID is never authority to execute an operation. }
function NyxOperationActionID(const AID: TNyxText;
  AAction: TNyxOperationAction): TNyxText;
{ Refuses disabled/hidden or detached controls. The caller must additionally
  check its mounted root and current execution context. }
function NyxOperationPanelAction(ANode: TNyxNode; const AID: TNyxText;
  out AAction: TNyxOperationAction): Boolean; overload;
{ Ordinary compound dispatch names the compound as Source and the physical part
  as Origin. Resolve only that exact origin within the delivered source; never
  look up a different tree by an event ID. Mounted/context checks remain host-owned. }
function NyxOperationPanelAction(ASource: TNyxNode; const AEvent: TNyxEventInfo;
  const AID: TNyxText; out AAction: TNyxOperationAction): Boolean; overload;
{ Validate all fixed parts and their direct ownership before changing any part.
  Missing/retyped/reparented parts refuse atomically; extension parts are retained.
  The caller explicitly synchronizes its renderer after this model update. }
procedure RestoreNyxOperationPanel(ARoot: TNyxNode;
  const AState: TNyxOperationPresentation);

implementation

const
  CActionPart: array[TNyxOperationAction] of TNyxText =
    ('start', 'cancel', 'retry', 'configure', 'check', 'dismiss');
  CActionCaption: array[TNyxOperationAction] of TNyxText =
    ('Start', 'Cancel', 'Retry', 'Compiler settings', 'Check status', 'Dismiss');

class function TNyxOperationPresentation.New(const ATitle: TNyxText;
  APhase: TNyxOperationPhase; const ADetail: TNyxText): TNyxOperationPresentation;
begin
  Result := Default(TNyxOperationPresentation);
  Result.FTitle := ATitle;
  Result.FPhase := APhase;
  Result.FDetail := ADetail;
end;

function TNyxOperationPresentation.Progress(ACompleted,
  ATotal: Integer): TNyxOperationPresentation;
begin

  if (ACompleted < 0) or (ATotal < 0) or (ACompleted > ATotal) then
  begin
    raise ENyxModel.Create('Operation progress requires 0 <= completed <= total');
  end;
  Result := Self;
  Result.FCompleted := ACompleted;
  Result.FTotal := ATotal;
end;

function TNyxOperationPresentation.Actions(
  AValue: TNyxOperationActions): TNyxOperationPresentation;
begin
  Result := Self;
  Result.FActions := AValue;
end;

function NyxOperationActionID(const AID: TNyxText;
  AAction: TNyxOperationAction): TNyxText;
begin
  Result := AID + TNyxText('-') + CActionPart[AAction];
end;

function NewNyxOperationPanel(const AID: TNyxText;
  const AState: TNyxOperationPresentation): INyxPanel;
var
  LLabel: INyxLabel;
  LProgress: INyxProgress;
  LActions: INyxRow;
  LButton: INyxButton;
  LAction: TNyxOperationAction;
begin
  Result := NewNyxPanel(AID);
  Result.Configure.Layout(nlColumn).Padding(12).Gap(8).Surface(True).Compound(True).Done;
  LLabel := NewNyxLabel(AID + TNyxText('-title'));
  LLabel.Configure.PartName(NyxPart('title')).Done;
  Result.Add(LLabel);
  LLabel := NewNyxLabel(AID + TNyxText('-detail'));
  LLabel.Configure.PartName(NyxPart('detail')).Done;
  Result.Add(LLabel);
  LProgress := NewNyxProgress(AID + TNyxText('-progress'));
  LProgress.Configure.PartName(NyxPart('progress')).Done;
  Result.Add(LProgress);
  LActions := NewNyxRow(AID + TNyxText('-actions'));
  LActions.Configure.PartName(NyxPart('actions')).Gap(8).Done;
  Result.Add(LActions);
  for LAction := Low(TNyxOperationAction) to High(TNyxOperationAction) do
  begin
    LButton := NewNyxButton(NyxOperationActionID(AID, LAction));
    LButton.Configure.Text(CActionCaption[LAction])
      .PartName(NyxPart(CActionPart[LAction])).Done;
    LActions.Add(LButton);
  end;
  RestoreNyxOperationPanel(Result.Node, AState);
end;

function NyxOperationPanelAction(ANode: TNyxNode; const AID: TNyxText;
  out AAction: TNyxOperationAction): Boolean;
var
  LAction: TNyxOperationAction;
  LRow: TNyxNode;
  LRoot: TNyxNode;
begin
  Result := False;
  AAction := noaCheck;

  if (ANode = nil) or (ANode.Kind <> NyxKindName(nkButton)) or
    (ANode.Prop('visible', 'true') = 'false') or
    (ANode.Prop('enabled', 'true') = 'false') then
  begin
    Exit;
  end;
  LRow := ANode.Parent;

  if LRow = nil then
  begin
    Exit;
  end;
  LRoot := LRow.Parent;

  if (LRoot = nil) or (LRoot.ID <> AID) or
    (LRoot.Kind <> NyxKindName(nkPanel)) or
    (LRoot.Prop('visible', 'true') = 'false') or
    (LRoot.Prop('enabled', 'true') = 'false') or
    (LRow.ID <> AID + TNyxText('-actions')) or
    (LRow.Prop('visible', 'true') = 'false') or
    (LRow.Prop('enabled', 'true') = 'false') or
    (LRow.Kind <> NyxKindName(nkRow)) then
  begin
    Exit;
  end;
  for LAction := Low(TNyxOperationAction) to High(TNyxOperationAction) do
  begin

    if ANode.ID = NyxOperationActionID(AID, LAction) then
    begin
      AAction := LAction;
      Exit(True);
    end;
  end;
end;

function NyxOperationPanelAction(ASource: TNyxNode; const AEvent: TNyxEventInfo;
  const AID: TNyxText; out AAction: TNyxOperationAction): Boolean;
var
  LOrigin: TNyxNode;
begin
  Result := False;
  AAction := noaCheck;

  if (ASource = nil) or (AEvent.Trigger <> ntClick) or
    (AEvent.SourceID <> ASource.ID) then
  begin
    Exit;
  end;
  LOrigin := ASource;

  if ASource.ID = AID then
  begin
    LOrigin := ASource.Find(AEvent.OriginID);
  end
  else if AEvent.OriginID <> ASource.ID then
  begin
    Exit;
  end;
  Result := NyxOperationPanelAction(LOrigin, AID, AAction);
end;

procedure RestoreNyxOperationPanel(ARoot: TNyxNode;
  const AState: TNyxOperationPresentation);
var
  LTitle: TNyxNode;
  LDetail: TNyxNode;
  LProgress: TNyxNode;
  LRow: TNyxNode;
  LButtons: array[TNyxOperationAction] of TNyxNode;
  LAction: TNyxOperationAction;
  LMaximum: Integer;
begin

  if (ARoot = nil) or (ARoot.Kind <> NyxKindName(nkPanel)) then
  begin
    raise ENyxModel.Create('Operation presentation requires its owning panel');
  end;
  LTitle := ARoot.Find(ARoot.ID + TNyxText('-title'));
  LDetail := ARoot.Find(ARoot.ID + TNyxText('-detail'));
  LProgress := ARoot.Find(ARoot.ID + TNyxText('-progress'));
  LRow := ARoot.Find(ARoot.ID + TNyxText('-actions'));

  if (LTitle = nil) or (LDetail = nil) or (LProgress = nil) or (LRow = nil) then
  begin
    raise ENyxModel.Create('Operation panel is missing a fixed part');
  end;

  if (LTitle.Kind <> NyxKindName(nkLabel)) or
    (LDetail.Kind <> NyxKindName(nkLabel)) or
    (LProgress.Kind <> NyxKindName(nkProgress)) or
    (LRow.Kind <> NyxKindName(nkRow)) or
    (LTitle.Parent <> ARoot) or (LDetail.Parent <> ARoot) or
    (LProgress.Parent <> ARoot) or (LRow.Parent <> ARoot) then
  begin
    raise ENyxModel.Create('Operation panel parts differ from their owned contract');
  end;
  for LAction := Low(TNyxOperationAction) to High(TNyxOperationAction) do
  begin
    LButtons[LAction] := LRow.Find(NyxOperationActionID(ARoot.ID, LAction));

    if (LButtons[LAction] = nil) or
      (LButtons[LAction].Kind <> NyxKindName(nkButton)) or
      (LButtons[LAction].Parent <> LRow) then
    begin
      raise ENyxModel.Create('Operation panel actions differ from their owned contract');
    end;
  end;
  LMaximum := AState.Total;

  if LMaximum = 0 then
  begin
    LMaximum := 1;
  end;
  ARoot.Configure.Visible(AState.Phase <> nopHidden).Done;
  LTitle.Configure.Text(AState.Title).Done;
  LDetail.Configure.Text(AState.Detail).Visible(AState.Detail <> '').Done;
  LProgress.Configure.Minimum(0).Maximum(LMaximum).Value(AState.Completed)
    .Visible(AState.Total > 0).AccessibleName(AState.Title + TNyxText(' progress')).Done;
  for LAction := Low(TNyxOperationAction) to High(TNyxOperationAction) do
  begin
    LButtons[LAction].Configure.Visible(LAction in AState.AvailableActions)
      .Enabled(LAction in AState.AvailableActions).Done;
  end;
end;

end.
