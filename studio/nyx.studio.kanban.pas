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

unit nyx.studio.kanban;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.model, nyx.kanban.editor;

{ Internal design-patch operation. ADocument MUST be the caller's detached
  candidate, never its accepted document. All three lane bindings, missing
  initial faces and complete intent change together; the existing patch owner
  performs final document/schema/source admission and owns rollback/history. }
procedure ApplyNyxKanbanDesignChange(ADocument: TNyxDocument;
  const AChange: TNyxKanbanEditorChange);

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.controls, nyx.composition, nyx.kanban,
  nyx.kanban.intent, nyx.collections.view.types;

const
  CPath = 'path';
  CMode = 'mode';
  CRoot = '.';
  CItems = 'items';
  CMove = 'move';
  CMoveCaption = 'Move selected card here';

type
  { Raw parts borrow one independent context for this synchronous operation.
    Each copied view spec owns its own behavioral and cosmetic values. }
  TKanbanRolePlans = record
    Lanes: array[0..2] of TNyxNode;
    Items: array[0..2] of TNyxNode;
    Specs: array[0..2] of TNyxCollectionViewSpec;
    MissingMove: array[0..2] of Boolean;
  end;

function LanePath(AIndex: Integer): TNyxText;
begin
  Result := 'lane-' + TNyxText(IntToStr(AIndex));
end;

function FullPath(const ATarget: TNyxKanbanEditorTarget;
  const ARelative: TNyxText): TNyxPartRef;
begin
  Result := NyxPart(ARelative);

  if ATarget.Path.Name <> CRoot then
  begin
    Result := NyxPart(ATarget.Path.Name + '/' + ARelative);
  end;
end;

function Inside(ANode, AOwner: TNyxNode): Boolean;
begin
  Result := False;
  while ANode <> nil do
  begin

    if ANode = AOwner then
    begin
      Exit(True);
    end;
    ANode := ANode.Parent;
  end;
end;

function FreeID(ADocument: TNyxDocument; const ABase: TNyxText): TNyxText;
var
  LSerial: Integer;
begin
  Result := ABase;
  LSerial := 1;
  while ADocument.Find(Result) <> nil do
  begin
    Inc(LSerial);

    if LSerial > NyxMaximumNodes + 1 then
    begin
      raise ENyxKanban.Create('Kanban authoring identity budget exhausted');
    end;
    Result := ABase + '-' + TNyxText(IntToStr(LSerial));
  end;
end;

function FindRule(AReference: TNyxNode; const APath: TNyxPartRef): TNyxNode;
var
  LIndex: Integer;
begin
  Result := nil;
  for LIndex := 0 to AReference.Count - 1 do
  begin

    if (AReference.Children[LIndex].Kind = NyxKindName(nkSlotOverride)) and
      (AReference.Children[LIndex].StoredProp(CPath) = APath.Name) then
    begin

      if Result <> nil then
      begin
        raise ENyxKanban.Create('Kanban role override is ambiguous');
      end;
      Result := AReference.Children[LIndex];
    end;
  end;
end;

function WritableRule(ADocument: TNyxDocument; AReference: TNyxNode;
  const APath: TNyxPartRef; const ABase: TNyxText): TNyxNode;
var
  LID: TNyxText;
begin
  Result := FindRule(AReference, APath);

  if Result = nil then
  begin
    LID := FreeID(ADocument, ABase);
    { Omitting a mode preserves an existing append/prepend/replace payload. }
    Result := AReference.OverridePart(APath).Named(LID);
  end;

  if Result.StoredProp(CMode) = NyxOverrideName(noRemove) then
  begin
    raise ENyxKanban.Create('Restore this required Kanban role before configuring its board');
  end;
end;

function WritableItems(ADocument: TNyxDocument; AOwner, AReference: TNyxNode;
  const ATarget: TNyxKanbanEditorTarget; AIndex: Integer;
  AProjection: TNyxNode): TNyxNode;
var
  LPath: TNyxPartRef;
  LColumn: TNyxNode;
  LPayload: TNyxNode;
begin

  if AReference = nil then
  begin
    LColumn := AOwner.Part(NyxPart(LanePath(AIndex)));

    if LColumn.ProjectionKind = NyxKindName(nkComponent) then
    begin
      Exit(WritableRule(ADocument, LColumn, NyxPart(CItems), 'kanban-items-binding'));
    end;
    Exit(LColumn.Part(NyxPart(CItems)));
  end;
  LPath := FullPath(ATarget, LanePath(AIndex) + '/' + CItems);
  Result := FindRule(AReference, LPath);

  if Result <> nil then
  begin
    Exit(WritableRule(ADocument, AReference, LPath, 'kanban-items-binding'));
  end;
  LPayload := ADocument.Find(AProjection.SourceID);

  if (LPayload <> nil) and Inside(LPayload, AReference) and
    (LPayload.Kind <> NyxKindName(nkSlotOverride)) then
  begin
    { A local replacement's actual authored child is safe to update. The same
      source in a shared definition is never inside this selecting reference. }
    Exit(LPayload);
  end;
  Result := WritableRule(ADocument, AReference, LPath, 'kanban-items-binding');
end;

function AppendHost(ADocument: TNyxDocument; ANode: TNyxNode): TNyxNode;
var
  LDepth: Integer;
begin
  Result := ANode;
  for LDepth := 0 to NyxMaximumTreeDepth do
  begin

    if Result.ProjectionKind = NyxKindName(nkComponent) then
    begin
      Result := WritableRule(ADocument, Result, NyxPart(CRoot), 'kanban-lane-actions');
    end;

    if Result.Kind <> NyxKindName(nkSlotOverride) then
    begin

      if Result.ProjectionKind <> NyxKindName(nkColumn) then
      begin
        raise ENyxKanban.Create('Kanban Move requires its authored lane container');
      end;
      Exit;
    end;

    if Result.StoredProp(CMode) = NyxOverrideName(noReplace) then
    begin

      if Result.Count <> 1 then
      begin
        raise ENyxKanban.Create('Kanban replacement requires one exact local payload');
      end;
      Result := Result.Children[0];
      Continue;
    end;

    if Result.StoredProp(CMode) = NyxOverrideName(noProperties) then
    begin
      { This is the ordinary InsertNode promotion: keep properties and payload,
        making the new authored child participate in the effective container. }
      Result.SetProp(CMode, NyxOverrideName(noAppend));
    end;

    if (Result.StoredProp(CMode) <> NyxOverrideName(noAppend)) and
      (Result.StoredProp(CMode) <> NyxOverrideName(noPrepend)) then
    begin
      raise ENyxKanban.Create('This Kanban lane cannot accept an authored Move face');
    end;
    Exit;
  end;
  raise ENyxKanban.Create('Kanban authored lane exceeds its existing depth budget');
end;

procedure AddMove(ADocument: TNyxDocument; AOwner, AReference: TNyxNode;
  const ATarget: TNyxKanbanEditorTarget; AIndex: Integer;
  ALane: TNyxNode; const AOptions: TNyxKanbanOptions);
var
  LHost: TNyxNode;
  LPath: TNyxPartRef;
  LButton: INyxButton;
begin

  if AReference = nil then
  begin
    LHost := AOwner.Part(NyxPart(LanePath(AIndex)));
  end
  else
  begin
    LPath := FullPath(ATarget, LanePath(AIndex));
    LHost := FindRule(AReference, LPath);

    if LHost = nil then
    begin
      LHost := ADocument.Find(ALane.SourceID);

      if (LHost = nil) or not Inside(LHost, AReference) then
      begin
        LHost := WritableRule(ADocument, AReference, LPath, 'kanban-lane-actions');
      end;
    end;
  end;
  LHost := AppendHost(ADocument, LHost);
  LButton := NewNyxButton(FreeID(ADocument, 'kanban-move-button'));
  LButton.WithText(CMoveCaption);
  LButton.Configure.PartName(NyxPart(CMove)).AccessibleName(
    'Move selected card to ' + AOptions.CaptionAt(AIndex)).Done;
  LHost.Add(LButton.Node);
end;

procedure PlanRoles(ABoard: TNyxNode; const AOptions: TNyxKanbanOptions;
  out APlans: TKanbanRolePlans);
var
  LIndex: Integer;
  LChild: Integer;
  LMove: TNyxNode;
begin
  APlans := Default(TKanbanRolePlans);
  for LIndex := 0 to 2 do
  begin
    APlans.Lanes[LIndex] := ABoard.Part(NyxPart(LanePath(LIndex)));
    APlans.Items[LIndex] := APlans.Lanes[LIndex].Part(NyxPart(CItems));

    if (APlans.Lanes[LIndex].ProjectionKind <> NyxKindName(nkColumn)) or
      (APlans.Items[LIndex].ProjectionKind <> NyxKindName(nkList)) then
    begin
      raise ENyxKanban.Create('Kanban requires its three column/list roles');
    end;
    APlans.Specs[LIndex] := NyxKanbanEditorLaneSpec(AOptions, LIndex,
      APlans.Items[LIndex].CollectionView);
    LMove := nil;
    for LChild := 0 to APlans.Lanes[LIndex].Count - 1 do
    begin

      if APlans.Lanes[LIndex].Children[LChild].StoredProp(
        NyxAttributeName(atPart)) = CMove then
      begin

        if LMove <> nil then
        begin
          raise ENyxKanban.Create('Kanban Move role is ambiguous');
        end;
        LMove := APlans.Lanes[LIndex].Children[LChild];
      end;
    end;

    if (LMove <> nil) and (LMove.ProjectionKind <> NyxKindName(nkButton)) then
    begin
      raise ENyxKanban.Create('The Kanban Move role must remain a button');
    end;
    APlans.MissingMove[LIndex] := LMove = nil;
  end;
end;

procedure ApplyNyxKanbanDesignChange(ADocument: TNyxDocument;
  const AChange: TNyxKanbanEditorChange);
var
  LTarget: TNyxKanbanEditorTarget;
  LIntent: TNyxKanbanIntent;
  LOptions: TNyxKanbanOptions;
  LOwner: TNyxNode;
  LReference: TNyxNode;
  LContext: TNyxNode;
  LBoard: TNyxNode;
  LWritable: TNyxNode;
  LControl: INyxControl;
  LPlans: TKanbanRolePlans;
  LIndex: Integer;
begin
  ValidateNyxKanbanEditorChange(ADocument, AChange);
  LTarget := AChange.Target;
  LOwner := ADocument.FindControl(LTarget.Owner);
  LReference := nil;

  if LTarget.Instance.ID <> '' then
  begin
    LReference := ADocument.FindControl(LTarget.Instance);
  end;
  LIntent := AChange.Intent;

  if LIntent.Mode = nkiDisabled then
  begin
    SetNyxKanbanIntent(RetainNyxControl(LOwner), LIntent);
    Exit;
  end;

  if LIntent.Mode = nkiUndefined then
  begin
    InheritNyxKanbanIntent(RetainNyxControl(LOwner));
  end;
  LContext := RealizeNyxContext(ADocument, LOwner, LBoard);
  try

    if LIntent.Mode = nkiUndefined then
    begin
      LIntent := GetNyxKanbanIntent(LBoard);
    end;

    if LIntent.Mode = nkiConfigured then
    begin
      LOptions := LIntent.Options;
      PlanRoles(LBoard, LOptions, LPlans);
      for LIndex := 0 to 2 do
      begin
        LWritable := WritableItems(ADocument, LOwner, LReference, LTarget,
          LIndex, LPlans.Items[LIndex]);

        if not LPlans.Items[LIndex].HasCollectionView then
        begin
          LControl := RetainNyxControl(LWritable);
          LControl.Configure.DragSource(True).DropTarget(True).Done;
        end;
        LWritable.SetCollectionView(LPlans.Specs[LIndex]);

        if LPlans.MissingMove[LIndex] then
        begin
          AddMove(ADocument, LOwner, LReference, LTarget, LIndex,
            LPlans.Lanes[LIndex], LOptions);
        end;
      end;
    end;

    if AChange.Intent.Mode = nkiConfigured then
    begin
      SetNyxKanbanIntent(RetainNyxControl(LOwner), AChange.Intent);
    end;
  finally
    LContext.Free;
  end;
end;

end.
