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
unit nyx.scroll.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.controls, nyx.scroll.presentation;

const
  NyxScrollEditorBaselineKey = 'scroll-editor-baseline';
  NyxScrollEditorValueKey = 'scroll-editor-value';

type
  { One immediate typed property proposal. The exact local/effective baseline
    prevents a stale mounted selector from editing a later selected component.
    This value retains no owner, shell, renderer or callback references. }
  TNyxScrollEditorChange = record
    Owner: TNyxText;
    Baseline: TNyxText;
    Wire: TNyxText;
  end;

function NyxScrollEditorBaseline(AOwner, AProjection: TNyxNode): TNyxText;
{ Ordinary select/spin authoring. Each committed field change is one property
  edit; there is no cross-field Apply draft to lose while switching panels.
  Changing the numeric minimum selects the minimum-thickness sizing mode. }
function NewNyxScrollEditor(const AID: TNyxText;
  AOwner, AProjection: TNyxNode): INyxColumn;
function CaptureNyxScrollEditor(AField, ARoot: TNyxNode;
  out AChange: TNyxScrollEditorChange): Boolean;

implementation

uses SysUtils, nyx.types, nyx.data;

const
  CMarkerKey = 'scroll-editor';
  COwnerKey = 'scroll-editor-owner';
  CValueKey = NyxScrollEditorValueKey;
  CDefault = 'Platform default';
  CFollow = 'Follow content zoom';
  CKeep = 'Keep normal size';
  CFloor = 'Minimum visible thickness';

function NyxScrollEditorBaseline(AOwner, AProjection: TNyxNode): TNyxText;
begin

  if (AOwner = nil) or (AProjection = nil) then
  begin
    raise ENyxModel.Create('Scroll editor requires its authored owner and effective projection');
  end;
  Result := NyxObject([
    NyxField('owner', NyxData(AOwner.ID)),
    NyxField('local', NyxData(AOwner.Prop(NyxAttributeName(atScrollPresentation)))),
    NyxField('localPresent', NyxData(
      AOwner.Props.IndexOfName(NyxAttributeName(atScrollPresentation)) >= 0)),
    NyxField('effective', NyxData(AProjection.Prop(NyxAttributeName(atScrollPresentation))))]).ToJSON;
end;

function NewNyxScrollEditor(const AID: TNyxText;
  AOwner, AProjection: TNyxNode): INyxColumn;
var
  LValue: TNyxScrollPresentation;
  LChoice: TNyxText;
  LMinimum: INyxSpin;
begin
  LValue := AProjection.ScrollPresentation;
  LChoice := CDefault;

  if AProjection.Prop(NyxAttributeName(atScrollPresentation)) <> '' then
  begin
    case LValue.SizingMode of
      nssFollowZoom: LChoice := CFollow;
      nssKeepSize: LChoice := CKeep;
      nssMinimumThickness: LChoice := CFloor;
    end;
  end;
  Result := NewNyxColumn(AID);
  Result.Configure.Gap(8).Done;
  Result.Node.SetProp(CMarkerKey, 'true').SetProp(COwnerKey, AOwner.ID)
    .SetProp(NyxScrollEditorBaselineKey, NyxScrollEditorBaseline(AOwner, AProjection))
    .SetProp(CValueKey, LValue.ToData.ToJSON);
  Result.Add(NewNyxSelect(AID + '-sizing').Configure.Text('Scrollbars')
    .Items(CDefault + #10 + CFollow + #10 + CKeep + #10 + CFloor).Value(LChoice).Done);
  LMinimum := NewNyxSpin(AID + '-minimum');
  LMinimum.Configure.Text('Minimum thickness (displayed pixels)')
    .Minimum(1).Maximum(NyxMaximumScrollThickness).Value(LValue.PhysicalMinimum).Done;
  Result.Add(LMinimum);
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Changing the minimum selects minimum sizing. Small controls can limit available space. ' +
    'Pixel sizing requires a supported native face or a measured browser capability.'));
end;

function CaptureNyxScrollEditor(AField, ARoot: TNyxNode;
  out AChange: TNyxScrollEditorChange): Boolean;
var
  LEditor: TNyxNode;
  LValue: TNyxScrollPresentation;
  LNumber: Integer;
  LText: TNyxText;
begin
  Result := False;
  AChange := Default(TNyxScrollEditorChange);

  if (AField = nil) or (ARoot = nil) then
  begin
    Exit;
  end;
  LEditor := AField.Parent;

  if (LEditor = nil) or (LEditor.Prop(CMarkerKey) <> 'true') then
  begin
    Exit;
  end;

  if (ARoot.Find(AField.ID) <> AField) or
    (LEditor.Kind <> NyxKindName(nkColumn)) or
    (LEditor.Prop(COwnerKey) = '') or (LEditor.Prop(NyxScrollEditorBaselineKey) = '') then
  begin
    raise ENyxModel.Create('Scroll editor no longer belongs to the current form');
  end;
  LValue := TNyxScrollPresentation.FromData(TNyxDataValue.ParseJSON(LEditor.Prop(CValueKey)));
  LText := AField.Prop('value');

  if (AField.ID = LEditor.ID + '-sizing') and (AField.Kind = NyxKindName(nkSelect)) then
  begin

    if LText = CDefault then
    begin
      AChange.Wire := '';
    end
    else
    begin

      if LText = CFollow then
      begin
        LValue := LValue.Sizing(nssFollowZoom);
      end
      else if LText = CKeep then
      begin
        LValue := LValue.Sizing(nssKeepSize);
      end
      else if LText = CFloor then
      begin
        LValue := LValue.Sizing(nssMinimumThickness);
      end
      else
      begin
        raise ENyxModel.Create('Select an available scrollbar sizing choice');
      end;
      AChange.Wire := LValue.ToData.ToJSON;
    end;
  end
  else if (AField.ID = LEditor.ID + '-minimum') and (AField.Kind = NyxKindName(nkSpin)) then
  begin

    if not TryStrToInt(LText, LNumber) then
    begin
      raise ENyxModel.Create('Enter a whole number for the minimum scrollbar thickness');
    end;
    AChange.Wire := LValue.MinimumThickness(LNumber).ToData.ToJSON;
  end
  else
  begin
    Exit;
  end;
  AChange.Owner := LEditor.Prop(COwnerKey);
  AChange.Baseline := LEditor.Prop(NyxScrollEditorBaselineKey);
  Result := True;
end;

end.
unit nyx.scroll.presentation;
