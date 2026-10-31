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
unit nyx.collections.lcl.tree;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Classes, ComCtrls, StdCtrls, Controls, LCLType,
  nyx.gestures.lcl, nyx.collections.mount;

type
  { Borrowed entered boundary runs before LCL detaches its editor or clears its
    editing item. Cancellation can mean navigation; ExplicitCancellation keeps
    deliberate Escape/host discard distinct from that dormant-edit handoff. }
  TNyxTreeEditingEnd = procedure(Sender: TObject; ACancel: Boolean) of object;
  { Flags-only borrowed admission for a new native label edit/commit. Existing
    composition and deferred model maps cannot grant stale row authority. }
  TNyxTreeEditReady = function: Boolean of object;

  { The standard LCL Tree still owns its nodes and cached TEdit. This adapter
    exposes entered editor boundaries without a second editor or model. Native
    callbacks borrow the mount and are revoked before it retires. Its physical
    frame protects the COMPLETE LCL Begin/EndEditing stack, including LCL's write
    to the borrowed node after OnEdited returns. Input retains flags only. }
  TNyxCollectionTreeView = class(TTreeView)
  private
    FCollectionFrame: INyxNativeGestureFrame;
    FCollectionInput: INyxCollectionInputLifetime;
    FEditorReady: TNotifyEvent;
    FEditorEnded: TNotifyEvent;
    FEditorEnding: TNyxTreeEditingEnd;
    FRestoringEditor: Boolean;
    FEditReady: TNyxTreeEditReady;
    FEditorDepth: Integer;
    FExplicitCancel: Boolean;
  protected
    procedure BeginEditing(ANode: TTreeNode); override;
    procedure EndEditing(ACancel: Boolean = False); override;
    procedure EditorKeyDown(Sender: TObject; var AKey: Word; AShift: TShiftState); override;
  public
    procedure ConnectCollectionEditor(const AFrame: INyxNativeGestureFrame;
      const AInput: INyxCollectionInputLifetime; AReady, AEnded: TNotifyEvent;
      AEnding: TNyxTreeEditingEnd; AEditReady: TNyxTreeEditReady);
    procedure DisconnectCollectionEditor;
    { Adapter-only continuation under an entered model refresh. It requires the
      SAME current incarnation while its temporary Ready shield is raised. }
    procedure RestoreCollectionEditor(ANode: TTreeNode);
    { Entered LCL editor stack only; ordinary selection/disclosure is separate. }
    function EditorDispatching: Boolean;
    { LCL also cancels when moving selection. Only Escape or an explicit host
      cancellation discards work; navigation can preserve a dormant draft. }
    function ExplicitCancellation: Boolean;
    procedure FinishCollectionEditor(ACancel: Boolean);
  end;

{ These borrowed reads use LCL's protected editor API. Nil means no active label
  editor; neither function creates a control, begins editing or retains a node.
  The caller retains its physical frame and rechecks input after host callouts. }
function NyxLCLTreeEditor(ATree: TTreeView): TCustomEdit;
function NyxLCLTreeEditingItem(ATree: TTreeView): TTreeNode;
{ Finish through the ordinary LCL method; Cancel=True never calls OnEdited or
  installs draft text. Existing OnEditingEnd callbacks keep their host meaning. }
procedure FinishNyxLCLTreeEditor(ATree: TTreeView; ACancel: Boolean);

implementation

type
  { Access bridge for the standard LCL protected FEditor/EditingItem/EndEditing
    API. It borrows an existing Tree and is never constructed or persisted. }
  TTreeEditorAccess = class(TTreeView);

function NyxLCLTreeEditor(ATree: TTreeView): TCustomEdit;
begin
  Result := nil;

  if (ATree <> nil) and ATree.IsEditing and
    (TTreeEditorAccess(ATree).FEditor <> nil) and
    (TTreeEditorAccess(ATree).FEditor.Parent = ATree) then
  begin
    Result := TTreeEditorAccess(ATree).FEditor;
  end;
end;

function NyxLCLTreeEditingItem(ATree: TTreeView): TTreeNode;
begin
  Result := nil;

  if NyxLCLTreeEditor(ATree) <> nil then
  begin
    Result := TTreeEditorAccess(ATree).EditingItem;
  end;
end;

procedure FinishNyxLCLTreeEditor(ATree: TTreeView; ACancel: Boolean);
begin

  if ATree <> nil then
  begin

    if ATree is TNyxCollectionTreeView then
    begin
      TNyxCollectionTreeView(ATree).FinishCollectionEditor(ACancel);
    end
    else
    begin
      TTreeEditorAccess(ATree).EndEditing(ACancel);
    end;
  end;
end;

procedure TNyxCollectionTreeView.ConnectCollectionEditor(
  const AFrame: INyxNativeGestureFrame; const AInput: INyxCollectionInputLifetime;
  AReady, AEnded: TNotifyEvent; AEnding: TNyxTreeEditingEnd;
  AEditReady: TNyxTreeEditReady);
begin
  FCollectionFrame := AFrame;
  FCollectionInput := AInput;
  FEditorReady := AReady;
  FEditorEnded := AEnded;
  FEditorEnding := AEnding;
  FEditReady := AEditReady;
end;

procedure TNyxCollectionTreeView.DisconnectCollectionEditor;
begin
  FEditorReady := nil;
  FEditorEnded := nil;
  FEditorEnding := nil;
  FEditReady := nil;
  FCollectionInput := nil;
  FCollectionFrame := nil;
end;

procedure TNyxCollectionTreeView.BeginEditing(ANode: TTreeNode);
var
  LFrame: INyxNativeGestureFrame;
  LInput: INyxCollectionInputLifetime;
begin
  LFrame := FCollectionFrame;
  LInput := FCollectionInput;

  if (LInput <> nil) and
    (not LInput.Current or (not FRestoringEditor and not LInput.Ready)) then
  begin
    Exit;
  end;

  if not FRestoringEditor and Assigned(FEditReady) and not FEditReady() then
  begin
    Exit;
  end;

  if LFrame <> nil then
  begin
    LFrame.Enter;
  end;
  Inc(FEditorDepth);
  try
    inherited BeginEditing(ANode);

    if (LInput <> nil) and not LInput.Current then
    begin
      inherited EndEditing(True);
      Exit;
    end;

    if IsEditing and Assigned(FEditorReady) then
    begin
      FEditorReady(Self);
    end;
  finally
    Dec(FEditorDepth);
    { No borrowed Self/host access follows the frame's retirement boundary. }

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

function TNyxCollectionTreeView.EditorDispatching: Boolean;
begin
  Result := FEditorDepth > 0;
end;

function TNyxCollectionTreeView.ExplicitCancellation: Boolean;
begin
  Result := FExplicitCancel;
end;

procedure TNyxCollectionTreeView.FinishCollectionEditor(ACancel: Boolean);
var
  LPrevious: Boolean;
  LFrame: INyxNativeGestureFrame;
begin
  LPrevious := FExplicitCancel;
  LFrame := FCollectionFrame;

  if LFrame <> nil then
  begin
    LFrame.Enter;
  end;
  FExplicitCancel := ACancel;
  try
    EndEditing(ACancel);
  finally
    { EndEditing may retire the owner. Restore this flag before the OUTER
      physical frame leaves, rather than touching Self after its final release. }
    FExplicitCancel := LPrevious;

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

procedure TNyxCollectionTreeView.EditorKeyDown(Sender: TObject;
  var AKey: Word; AShift: TShiftState);
var
  LPrevious: Boolean;
  LFrame: INyxNativeGestureFrame;
begin
  LPrevious := FExplicitCancel;
  FExplicitCancel := AKey = VK_ESCAPE;
  LFrame := FCollectionFrame;

  if LFrame <> nil then
  begin
    LFrame.Enter;
  end;
  try
    inherited EditorKeyDown(Sender, AKey, AShift);
  finally
    FExplicitCancel := LPrevious;

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

procedure TNyxCollectionTreeView.RestoreCollectionEditor(ANode: TTreeNode);
var
  LFrame: INyxNativeGestureFrame;
begin
  LFrame := FCollectionFrame;

  if LFrame <> nil then
  begin
    LFrame.Enter;
  end;
  FRestoringEditor := True;
  try
    BeginEditing(ANode);
  finally
    FRestoringEditor := False;

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

procedure TNyxCollectionTreeView.EndEditing(ACancel: Boolean);
var
  LFrame: INyxNativeGestureFrame;
  LWasEditing: Boolean;
begin
  LFrame := FCollectionFrame;
  LWasEditing := IsEditing;

  if LWasEditing and not ACancel and Assigned(FEditReady) and not FEditReady() then
  begin
    Exit;
  end;

  if LFrame <> nil then
  begin
    LFrame.Enter;
  end;

  if LWasEditing then
  begin
    Inc(FEditorDepth);
  end;
  try

    if LWasEditing and Assigned(FEditorEnding) then
    begin
      FEditorEnding(Self, ACancel);
    end;
    inherited EndEditing(ACancel);

    if LWasEditing and Assigned(FEditorEnded) then
    begin
      FEditorEnded(Self);
    end;
  finally

    if LWasEditing then
    begin
      Dec(FEditorDepth);
    end;

    if LFrame <> nil then
    begin
      LFrame.Leave;
    end;
  end;
end;

end.
