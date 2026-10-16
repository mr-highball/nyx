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
unit nyx.canvas.editing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model,
  nyx.canvas.types, nyx.canvas.declarations, nyx.canvas.route.types;

const
  { One semantic scene edit is bounded independently of a full saved definition.
    Related edits still share the caller's candidate and single history command. }
  MaximumNyxCanvasConnectionEdits = 32;

type
  { Closed candidate operations. Connection edits address an exact scene/item/
    control; their appended ordinals preserve existing typed operation values. }
  TNyxCanvasChangeKind = (nccDefineScene, nccRemoveScene, nccAttachScene,
    nccDetachScene, nccEmptyScene, nccSetConnections, nccInheritConnections);
  { Immutable copied design intent. No control/document/view is retained, and
    no interface is embedded in this portable record. Definition data is admitted
    before publication. Apply changes a caller-owned candidate; final whole-
    document validation is required after the complete grouped operation. }
  TNyxCanvasChange = record
  private
    FDefined: Boolean;
    FKind: TNyxCanvasChangeKind;
    FScene: TNyxCanvasRef;
    FItem: TNyxCanvasItemRef;
    FOwner: TNyxControlRef;
    FDefinition: TNyxDataValue;
  public
    function ToData: TNyxDataValue;
    class function FromData(const AValue: TNyxDataValue): TNyxCanvasChange; static;
    procedure Apply(ADocument: TNyxDocument);
    property Defined: Boolean read FDefined;
    property Kind: TNyxCanvasChangeKind read FKind;
    property Scene: TNyxCanvasRef read FScene;
    property Owner: TNyxControlRef read FOwner;
  end;

{ Public typed operations normalize arguments before returning a copied intent.
  Related template/definition/control changes must share one candidate/history
  command. Removing referenced content or definitions refuses final admission. }
function NyxDefineCanvas(const AScene: TNyxCanvasRef;
  const ADefinition: INyxCanvasDefinition): TNyxCanvasChange;
function NyxRemoveCanvas(const AScene: TNyxCanvasRef): TNyxCanvasChange;
function NyxAttachCanvas(const AOwner: TNyxControlRef;
  const AScene: TNyxCanvasRef): TNyxCanvasChange;
function NyxDetachCanvas(const AOwner: TNyxControlRef): TNyxCanvasChange;
{ Explicit empty is distinct from Detach, which restores template inheritance. }
function NyxEmptyCanvas(const AOwner: TNyxControlRef): TNyxCanvasChange;
{ Replace one to thirty-two distinct per-item/control masks in an existing scene.
  Empty route sets deliberately disable inheritance. The copied intent retains
  no builder or application; Apply preserves geometry and every unrelated mask. }
function NyxSetCanvasConnections(const AScene: TNyxCanvasRef;
  const AChanges: array of TNyxCanvasRouteChange): TNyxCanvasChange;
{ Remove one existing saved mask, restoring that item's authored root routes.
  Missing masks refuse instead of silently inventing an undoable operation. }
function NyxInheritCanvasConnections(const AScene: TNyxCanvasRef;
  const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef): TNyxCanvasChange;

implementation

uses
  nyx.composition, nyx.bytes;

function NyxRemoveCanvas(const AScene: TNyxCanvasRef): TNyxCanvasChange;
begin
  Result := Default(TNyxCanvasChange);
  Result.FDefined := True;
  Result.FKind := nccRemoveScene;
  Result.FScene := NyxCanvasRef(AScene.Name);
end;

function NyxDefineCanvas(const AScene: TNyxCanvasRef;
  const ADefinition: INyxCanvasDefinition): TNyxCanvasChange;
begin

  if ADefinition = nil then
  begin
    raise ENyxModel.Create('Canvas definition change requires an immutable plan');
  end;
  Result := NyxRemoveCanvas(AScene);
  Result.FKind := nccDefineScene;
  Result.FDefinition := NyxCanvasDefinitionFromData(ADefinition.ToData).ToData;
end;

function NyxDetachCanvas(const AOwner: TNyxControlRef): TNyxCanvasChange;
begin
  Result := Default(TNyxCanvasChange);
  Result.FDefined := True;
  Result.FKind := nccDetachScene;
  Result.FOwner := NyxControl(AOwner.ID);
end;

function NyxAttachCanvas(const AOwner: TNyxControlRef;
  const AScene: TNyxCanvasRef): TNyxCanvasChange;
begin
  Result := NyxDetachCanvas(AOwner);
  Result.FKind := nccAttachScene;
  Result.FScene := NyxCanvasRef(AScene.Name);
end;

function NyxEmptyCanvas(const AOwner: TNyxControlRef): TNyxCanvasChange;
begin
  Result := NyxDetachCanvas(AOwner);
  Result.FKind := nccEmptyScene;
end;

function NyxSetCanvasConnections(const AScene: TNyxCanvasRef;
  const AChanges: array of TNyxCanvasRouteChange): TNyxCanvasChange;
var
  LChanges: TNyxCanvasRouteChanges;
  LValues: array of TNyxDataValue;
  LIndex: Integer;
  LPrior: Integer;
begin

  if (Length(AChanges) < 1) or (Length(AChanges) > MaximumNyxCanvasConnectionEdits) then
  begin
    raise ENyxModel.Create('Canvas connection edit requires one to thirty-two replacements');
  end;
  Result := NyxRemoveCanvas(AScene);
  Result.FKind := nccSetConnections;
  SetLength(LChanges, Length(AChanges));
  SetLength(LValues, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    LChanges[LIndex] := TNyxCanvasRouteChange.FromData(AChanges[LIndex].ToData);
    for LPrior := 0 to LIndex - 1 do
    begin

      if (LChanges[LPrior].Item.Name = LChanges[LIndex].Item.Name) and
        (LChanges[LPrior].Control.ID = LChanges[LIndex].Control.ID) then
      begin
        raise ENyxModel.Create('Canvas connection edit repeats an item/control');
      end;
    end;
    LValues[LIndex] := NyxCanvasRoutes(LChanges[LIndex].Item, LChanges[LIndex].Control,
      LChanges[LIndex].Routes).ToData;
  end;
  Result.FDefinition := NyxArray(LValues);

  if NyxUTF8ByteCount(Result.FDefinition.ToJSON) > MaximumNyxCanvasDefinitionBytes then
  begin
    raise ENyxModel.Create('Canvas connection edit exceeds the saved definition byte budget');
  end;
end;

function NyxInheritCanvasConnections(const AScene: TNyxCanvasRef;
  const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef): TNyxCanvasChange;
begin
  Result := NyxRemoveCanvas(AScene);
  Result.FKind := nccInheritConnections;
  Result.FItem := NyxCanvasItem(AItem.Name);
  Result.FOwner := NyxControl(AControl.ID);
end;

function TNyxCanvasChange.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    raise ENyxModel.Create('Canvas change is absent');
  end;
  case FKind of
    nccDefineScene:
      begin
        Result := NyxObject([NyxField('op', NyxData('canvas-define')),
          NyxField('scene', NyxData(FScene.Name)), NyxField('definition', FDefinition)]);
      end;
    nccRemoveScene:
      begin
        Result := NyxObject([NyxField('op', NyxData('canvas-remove')),
          NyxField('scene', NyxData(FScene.Name))]);
      end;
    nccAttachScene:
      begin
        Result := NyxObject([NyxField('op', NyxData('canvas-attach')),
          NyxField('id', NyxData(FOwner.ID)), NyxField('scene', NyxData(FScene.Name))]);
      end;
    nccDetachScene:
      begin
        Result := NyxObject([NyxField('op', NyxData('canvas-detach')),
          NyxField('id', NyxData(FOwner.ID))]);
      end;
    nccEmptyScene:
      begin
        Result := NyxObject([NyxField('op', NyxData('canvas-empty')),
          NyxField('id', NyxData(FOwner.ID))]);
      end;
    nccSetConnections:
      begin
        Result := NyxObject([NyxField('op', NyxData('canvas-connections-set')),
          NyxField('scene', NyxData(FScene.Name)), NyxField('changes', FDefinition)]);
      end;
    nccInheritConnections:
      begin
        Result := NyxObject([NyxField('op', NyxData('canvas-connections-inherit')),
          NyxField('scene', NyxData(FScene.Name)), NyxField('item', NyxData(FItem.Name)),
          NyxField('control', NyxData(FOwner.ID))]);
      end;
  end;
end;

class function TNyxCanvasChange.FromData(const AValue: TNyxDataValue): TNyxCanvasChange;
var
  LOperation: TNyxText;
  LChanges: TNyxCanvasRouteChanges;
  LArray: TNyxDataValue;
  LIndex: Integer;

  procedure Fields(const ANames: array of TNyxText);
  var
    LIndex: Integer;
    LName: Integer;
    LFound: Boolean;
  begin

    if (AValue.Kind <> ndObject) or (AValue.Count <> Length(ANames)) then
    begin
      raise ENyxModel.Create('Canvas change requires exact operation fields');
    end;
    for LIndex := 0 to AValue.Count - 1 do
    begin
      LFound := False;
      for LName := 0 to High(ANames) do
      begin
        LFound := LFound or (AValue.Key(LIndex) = ANames[LName]);
      end;

      if not LFound then
      begin
        raise ENyxModel.Create('Unknown canvas change field');
      end;
    end;
  end;

begin
  LOperation := AValue.Field('op').AsText;

  if LOperation = 'canvas-define' then
  begin
    Fields(['op', 'scene', 'definition']);
    Result := NyxDefineCanvas(NyxCanvasRef(AValue.Field('scene').AsText),
      NyxCanvasDefinitionFromData(AValue.Field('definition')));
  end
  else if LOperation = 'canvas-remove' then
  begin
    Fields(['op', 'scene']);
    Result := NyxRemoveCanvas(NyxCanvasRef(AValue.Field('scene').AsText));
  end
  else if LOperation = 'canvas-attach' then
  begin
    Fields(['op', 'id', 'scene']);
    Result := NyxAttachCanvas(NyxControl(AValue.Field('id').AsText),
      NyxCanvasRef(AValue.Field('scene').AsText));
  end
  else if LOperation = 'canvas-detach' then
  begin
    Fields(['op', 'id']);
    Result := NyxDetachCanvas(NyxControl(AValue.Field('id').AsText));
  end
  else if LOperation = 'canvas-empty' then
  begin
    Fields(['op', 'id']);
    Result := NyxEmptyCanvas(NyxControl(AValue.Field('id').AsText));
  end
  else if LOperation = 'canvas-connections-set' then
  begin
    Fields(['op', 'scene', 'changes']);
    LArray := AValue.Field('changes');

    if (LArray.Kind <> ndArray) or (LArray.Count < 1) or
      (LArray.Count > MaximumNyxCanvasConnectionEdits) then
    begin
      raise ENyxModel.Create('Canvas connection edit requires a bounded replacement array');
    end;
    SetLength(LChanges, LArray.Count);
    for LIndex := 0 to High(LChanges) do
    begin
      LChanges[LIndex] := TNyxCanvasRouteChange.FromData(LArray.Item(LIndex));
    end;
    Result := NyxSetCanvasConnections(NyxCanvasRef(AValue.Field('scene').AsText), LChanges);
  end
  else if LOperation = 'canvas-connections-inherit' then
  begin
    Fields(['op', 'scene', 'item', 'control']);
    Result := NyxInheritCanvasConnections(NyxCanvasRef(AValue.Field('scene').AsText),
      NyxCanvasItem(AValue.Field('item').AsText), NyxControl(AValue.Field('control').AsText));
  end
  else
  begin
    raise ENyxModel.Create('Unknown canvas change operation');
  end;
end;

procedure TNyxCanvasChange.Apply(ADocument: TNyxDocument);
var
  LNode: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LChanges: TNyxCanvasRouteChanges;
  LIndex: Integer;
begin

  if not FDefined or (ADocument = nil) then
  begin
    raise ENyxModel.Create('Canvas change requires a candidate document and admitted intent');
  end;

  if FKind in [nccAttachScene, nccDetachScene, nccEmptyScene] then
  begin
    LNode := ADocument.Find(FOwner.ID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Scene attachment requires an existing canvas control');
    end;
    LContext := RealizeNyxContext(ADocument, LNode, LProjection);
    try

      if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkCanvas)) then
      begin
        raise ENyxModel.Create('Scene declaration requires an effective canvas target');
      end;
    finally
      LContext.Free;
    end;

    if FKind = nccAttachScene then
    begin
      LNode.CanvasScene := FScene;
    end
    else if FKind = nccEmptyScene then
    begin
      LNode.SetCanvasScene(Default(TNyxCanvasRef));
    end
    else
    begin

      if not LNode.HasCanvasScene then
      begin
        raise ENyxModel.Create('This canvas has no scene attachment to remove');
      end;
      LNode.RemoveCanvasScene;
    end;
    Exit;
  end;
  case FKind of
    nccDefineScene:
      begin
        ADocument.Canvases.Define(FScene, NyxCanvasDefinitionFromData(FDefinition));
      end;
    nccRemoveScene:
      begin
        ADocument.Canvases.Remove(FScene);
      end;
    nccSetConnections:
      begin
        SetLength(LChanges, FDefinition.Count);
        for LIndex := 0 to High(LChanges) do
        begin
          LChanges[LIndex] := TNyxCanvasRouteChange.FromData(FDefinition.Item(LIndex));
        end;
        ADocument.Canvases.Define(FScene,
          NyxCanvasConnections(ADocument.Canvases.Definition(FScene)).Controls(LChanges).Done);
      end;
    nccInheritConnections:
      begin
        ADocument.Canvases.Define(FScene,
          NyxCanvasConnections(ADocument.Canvases.Definition(FScene)).Inherit(FItem, FOwner).Done);
      end;
  else
    raise ENyxModel.Create('Canvas change operation is unavailable');
  end;
end;

end.
