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

unit nyx.selection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls;

type
  { Derived scalar selection always publishes Boolean Pressed. Variant styling
    can follow this policy or remain under the component creator's control. }
  TNyxSelectionStyling = (nssVariants, nssAuthored);
  { Immutable value policy for direct naSelect parts of a scalar compound.
    Defaults preserve existing selected-primary/unselected-default behavior.
    Authored mode never rewrites a part's Variant; custom adapters/styles can
    consume Pressed independently. No document, view or callback is retained. }
  TNyxSelectionOptions = record
  private
    FStyling: TNyxSelectionStyling;
    FSelected: TNyxVariant;
    FUnselected: TNyxVariant;
  public
    class function Defaults: TNyxSelectionOptions; static;
    { Each fluent operation returns a copy. Invalid enum casts refuse before
      the result can be stored on a component; the original remains unchanged. }
    function Styling(AValue: TNyxSelectionStyling): TNyxSelectionOptions;
    function Selected(AValue: TNyxVariant): TNyxSelectionOptions;
    function Unselected(AValue: TNyxVariant): TNyxSelectionOptions;
    procedure Validate;
    { Strict version-one persistence boundary. Unknown, missing, extra or
      wrongly typed fields/choices refuse; authored code uses the enums above. }
    function ToData: TNyxDataValue;
    class function FromData(const AValue: TNyxDataValue): TNyxSelectionOptions; static;
    property StylingMode: TNyxSelectionStyling read FStyling;
    property SelectedVariant: TNyxVariant read FSelected;
    property UnselectedVariant: TNyxVariant read FUnselected;
  end;

const
  NyxSelectionOptionsKey = 'nyx.selection';

{ Copies the validated immutable policy into the control's owned known
  extension. Nil refuses. Complete candidate admission checks scalar ownership. }
procedure SetNyxSelectionOptions(const AControl: INyxControl;
  const AOptions: TNyxSelectionOptions);
{ Borrows only this call's node. Missing policy returns the legacy defaults;
  malformed declared policy refuses instead of silently falling back. }
function NyxSelectionOptions(ANode: TNyxNode): TNyxSelectionOptions;

implementation

function ReadVariant(const AText: TNyxText): TNyxVariant;
var
  LVariant: TNyxVariant;
begin
  for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
  begin

    if NyxVariantName(LVariant) = AText then
    begin
      Exit(LVariant);
    end;
  end;
  raise ENyxModel.Create('Unknown selection variant');
end;

class function TNyxSelectionOptions.Defaults: TNyxSelectionOptions;
begin
  Result.FStyling := nssVariants;
  Result.FSelected := nvPrimary;
  Result.FUnselected := nvDefault;
end;

function TNyxSelectionOptions.Styling(AValue: TNyxSelectionStyling): TNyxSelectionOptions;
begin
  Result := Self;
  Result.FStyling := AValue;
  Result.Validate;
end;

function TNyxSelectionOptions.Selected(AValue: TNyxVariant): TNyxSelectionOptions;
begin
  Result := Self;
  Result.FSelected := AValue;
  Result.Validate;
end;

function TNyxSelectionOptions.Unselected(AValue: TNyxVariant): TNyxSelectionOptions;
begin
  Result := Self;
  Result.FUnselected := AValue;
  Result.Validate;
end;

procedure TNyxSelectionOptions.Validate;
begin

  if (Ord(FStyling) < Ord(Low(TNyxSelectionStyling))) or
    (Ord(FStyling) > Ord(High(TNyxSelectionStyling))) or
    (Ord(FSelected) < Ord(Low(TNyxVariant))) or
    (Ord(FSelected) > Ord(High(TNyxVariant))) or
    (Ord(FUnselected) < Ord(Low(TNyxVariant))) or
    (Ord(FUnselected) > Ord(High(TNyxVariant))) then
  begin
    raise ENyxModel.Create('Unknown selection styling or variant');
  end;
end;

function TNyxSelectionOptions.ToData: TNyxDataValue;
const
  CStyling: array[TNyxSelectionStyling] of TNyxText = ('variants', 'authored');
begin
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('styling', NyxData(CStyling[FStyling])),
    NyxField('selected', NyxData(NyxVariantName(FSelected))),
    NyxField('unselected', NyxData(NyxVariantName(FUnselected)))]);
end;

class function TNyxSelectionOptions.FromData(const AValue: TNyxDataValue): TNyxSelectionOptions;
var
  LStyling: TNyxText;
begin

  if (AValue.Kind <> ndObject) or (AValue.Count <> 4) or
    (AValue.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Selection policy requires exact version-one fields');
  end;
  LStyling := AValue.Field('styling').AsText;

  if LStyling = 'variants' then
  begin
    Result.FStyling := nssVariants;
  end
  else if LStyling = 'authored' then
  begin
    Result.FStyling := nssAuthored;
  end
  else
  begin
    raise ENyxModel.Create('Unknown selection styling');
  end;

  Result.FSelected := ReadVariant(AValue.Field('selected').AsText);
  Result.FUnselected := ReadVariant(AValue.Field('unselected').AsText);
  Result.Validate;
end;

procedure SetNyxSelectionOptions(const AControl: INyxControl;
  const AOptions: TNyxSelectionOptions);
begin

  if AControl = nil then
  begin
    raise ENyxModel.Create('Selection policy requires its control');
  end;
  AControl.Extensions.SetValue(NyxExtension(NyxSelectionOptionsKey), AOptions.ToData);
end;

function NyxSelectionOptions(ANode: TNyxNode): TNyxSelectionOptions;
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Selection policy requires its node');
  end;
  Result := TNyxSelectionOptions.Defaults;

  if ANode.Extensions.Has(NyxExtension(NyxSelectionOptionsKey)) then
  begin
    Result := TNyxSelectionOptions.FromData(
      ANode.Extensions.Value(NyxExtension(NyxSelectionOptionsKey)));
  end;
end;

end.
