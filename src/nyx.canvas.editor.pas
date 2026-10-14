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
unit nyx.canvas.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls,
  nyx.canvas.types, nyx.canvas.editing;

type
  { Copied, uncommitted choice. The form owns no editor/session/document.
    The owning host rechecks Baseline and publishes Change through its usual
    revision-aware grouped command. }
  TNyxCanvasEditorChange = record
    Owner: TNyxControlRef;
    Baseline: TNyxText;
    Change: TNyxCanvasChange;
  end;

{ Bounded scene membership/reference baseline; no entire scene/document dump. }
function NyxCanvasEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
{ Public reusable Nyx Card/Select/Button form; selecting a choice does not mutate
  the authored document. Empty workspace and exact scene names remain distinct. }
function NewNyxCanvasSceneEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
{ Unrelated input returns False. An owned Apply button captures only an admitted
  choice from the exact mounted form; malformed/missing fields refuse. }
function CaptureNyxCanvasSceneEditor(AButton, ARoot: TNyxNode;
  out AChange: TNyxCanvasEditorChange): Boolean;

implementation

uses
  nyx.composition;

const
  CEditor = 'nyx.canvas-editor';
  CEmpty = 'Empty workspace';
  CInherit = 'Use inherited scene';
  CScenePrefix = 'Scene / ';

function NyxCanvasEditorBaseline(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef): TNyxText;
var
  LNode: TNyxNode;
  LNames: array of TNyxDataValue;
  LIndex: Integer;
  LScene: TNyxDataValue;
  LEffective: TNyxDataValue;
  LContext: TNyxNode;
  LProjection: TNyxNode;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Scene editor requires its source document');
  end;
  LNode := ADocument.Find(AOwner.ID);

  if LNode = nil then
  begin
    raise ENyxModel.Create('Scene editor requires an authored canvas control');
  end;
  LContext := RealizeNyxContext(ADocument, LNode, LProjection);
  try

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkCanvas)) then
    begin
      raise ENyxModel.Create('Scene editor requires an effective canvas control');
    end;
    LEffective := NyxNull;

    if LProjection.CanvasScene.Defined then
    begin
      LEffective := NyxData(LProjection.CanvasScene.Name);
    end;
  finally
    LContext.Free;
  end;
  LScene := NyxNull;

  if LNode.CanvasScene.Defined then
  begin
    LScene := NyxData(LNode.CanvasScene.Name);
  end;
  SetLength(LNames, ADocument.Canvases.Count);
  for LIndex := 0 to High(LNames) do
  begin
    LNames[LIndex] := NyxData(ADocument.Canvases.Reference(LIndex).Name);
  end;
  Result := NyxObject([NyxField('owner', NyxData(AOwner.ID)),
    NyxField('scene', LScene), NyxField('localDeclared', NyxData(LNode.HasCanvasScene)),
    NyxField('effectiveScene', LEffective), NyxField('choices', NyxArray(LNames))]).ToJSON;
end;

function NewNyxCanvasSceneEditor(const AID: TNyxText; const AOwner: TNyxControlRef;
  ADocument: TNyxDocument): INyxCard;
var
  LBaseline: TNyxText;
  LItems: TNyxText;
  LSelected: TNyxText;
  LIndex: Integer;
  LNode: TNyxNode;
  LButton: INyxButton;
begin
  LBaseline := NyxCanvasEditorBaseline(ADocument, AOwner);
  LItems := CInherit + #10 + CEmpty;
  LSelected := CInherit;
  LNode := ADocument.Find(AOwner.ID);
  for LIndex := 0 to ADocument.Canvases.Count - 1 do
  begin
    LItems := LItems + #10 + CScenePrefix + ADocument.Canvases.Reference(LIndex).Name;
  end;

  if LNode.CanvasScene.Defined then
  begin
    LSelected := CScenePrefix + LNode.CanvasScene.Name;
  end
  else if LNode.HasCanvasScene then
  begin
    LSelected := CEmpty;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), NyxData(LBaseline));
  Result.Add(NewNyxLabel(AID + '-title').WithText('Canvas scene'));
  Result.Add(NewNyxSelect(AID + '-choice').Configure.Text('Saved scene')
    .Items(LItems).Value(LSelected).Done);
  LButton := NewNyxButton(AID + '-apply');
  LButton.WithText('Use scene');
  Result.Add(LButton);
end;

function CaptureNyxCanvasSceneEditor(AButton, ARoot: TNyxNode;
  out AChange: TNyxCanvasEditorChange): Boolean;
var
  LForm: TNyxNode;
  LChoice: TNyxNode;
  LBaseline: TNyxDataValue;
  LNames: TNyxDataValue;
  LValue: TNyxText;
  LIndex: Integer;
  LFound: Boolean;
begin
  Result := False;
  AChange := Default(TNyxCanvasEditorChange);

  if (AButton = nil) or (ARoot = nil) or (ARoot.Find(AButton.ID) <> AButton) then
  begin
    Exit;
  end;
  LForm := AButton.Parent;

  if (LForm = nil) or not LForm.Extensions.Has(NyxExtension(CEditor)) or
    (AButton.ID <> LForm.ID + '-apply') then
  begin
    Exit;
  end;
  AChange.Baseline := LForm.Extensions.Value(NyxExtension(CEditor)).AsText;
  LBaseline := TNyxDataValue.ParseJSON(AChange.Baseline);
  AChange.Owner := NyxControl(LBaseline.Field('owner').AsText);
  LChoice := LForm.Find(LForm.ID + '-choice');

  if LChoice = nil then
  begin
    raise ENyxModel.Create('Scene choice field is unavailable');
  end;
  LValue := LChoice.Prop(NyxAttributeName(atValue));

  if LValue = CInherit then
  begin
    AChange.Change := NyxDetachCanvas(AChange.Owner);
  end
  else if LValue = CEmpty then
  begin
    AChange.Change := NyxEmptyCanvas(AChange.Owner);
  end
  else
  begin
    LNames := LBaseline.Field('choices');
    LFound := False;
    for LIndex := 0 to LNames.Count - 1 do
    begin

      if LValue = CScenePrefix + LNames.Item(LIndex).AsText then
      begin
        AChange.Change := NyxAttachCanvas(AChange.Owner, NyxCanvasRef(LNames.Item(LIndex).AsText));
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxModel.Create('Choose one exact saved scene from this mounted form');
    end;
  end;
  Result := True;
end;

end.
