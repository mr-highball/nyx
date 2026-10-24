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
unit nyx.collections.presentation.controls;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections.arrangement, nyx.collections.presentation,
  nyx.collections.view, nyx.collections.alignment;

type
  { Detached effective density and text policies from ONE canonical columns
    capture. All getters use copied values; paint never queries a view, store,
    selection or runtime preference owner. An absent policy stays absent, so
    existing tables retain their original metrics and text path. Explicit
    runtime overrides are reported independently of authored presence. }
  TNyxTablePresentationObservation = record
  private
    FTable: TNyxTablePresentationPolicy;
    FFields: array of TNyxColumnIdentity;
    FText: array of TNyxColumnTextPolicy;
    FAlignment: array of TNyxColumnAlignmentPolicy;
    FSerial: Integer;
    FHasRuntimeOverrides: Boolean;
    function GetCount: Integer;
    function GetTable: TNyxTablePresentationPolicy;
  public
    class function Read(const ACapture: INyxCollectionColumnsCapture):
      TNyxTablePresentationObservation; static;
    function Copy: TNyxTablePresentationObservation;
    function FieldAt(ACanonical: Integer): TNyxColumnIdentity;
    function TextAt(ACanonical: Integer): TNyxColumnTextPolicy;
    function AlignmentAt(ACanonical: Integer): TNyxColumnAlignmentPolicy;
    property Count: Integer read GetCount;
    property TablePolicy: TNyxTablePresentationPolicy read GetTable;
    property Serial: Integer read FSerial;
    property HasRuntimeOverrides: Boolean read FHasRuntimeOverrides;
  end;

implementation

uses SysUtils, nyx.collections, nyx.collections.view.types;

function TNyxTablePresentationObservation.GetCount: Integer;
begin
  Result := Length(FFields);
end;

function TNyxTablePresentationObservation.GetTable: TNyxTablePresentationPolicy;
begin
  Result := FTable.Copy;
end;

function TNyxTablePresentationObservation.FieldAt(ACanonical: Integer): TNyxColumnIdentity;
begin

  if (ACanonical < 0) or (ACanonical >= Count) then
  begin
    raise EArgumentException.Create('Presentation field is outside its captured vector');
  end;
  Result := FFields[ACanonical];
end;

function TNyxTablePresentationObservation.TextAt(ACanonical: Integer): TNyxColumnTextPolicy;
begin

  if (ACanonical < 0) or (ACanonical >= Count) then
  begin
    raise EArgumentException.Create('Text policy is outside its captured vector');
  end;
  Result := FText[ACanonical].Copy;
end;

function TNyxTablePresentationObservation.AlignmentAt(
  ACanonical: Integer): TNyxColumnAlignmentPolicy;
begin

  if (ACanonical < 0) or (ACanonical >= Count) then
  begin
    raise EArgumentException.Create('Alignment policy is outside its captured vector');
  end;
  Result := FAlignment[ACanonical].Copy;
end;

function TNyxTablePresentationObservation.Copy: TNyxTablePresentationObservation;
var
  LValue: TNyxTablePresentationObservation;
  I: Integer;
begin
  LValue := Self;
  LValue.FFields := nil;
  LValue.FText := nil;
  LValue.FAlignment := nil;
  SetLength(LValue.FFields, Count);
  SetLength(LValue.FText, Count);
  SetLength(LValue.FAlignment, Count);
  for I := 0 to Count - 1 do
  begin
    LValue.FFields[I] := FFields[I];
    LValue.FText[I] := FText[I].Copy;
    LValue.FAlignment[I] := FAlignment[I].Copy;
  end;
  Result := LValue;
end;

class function TNyxTablePresentationObservation.Read(
  const ACapture: INyxCollectionColumnsCapture): TNyxTablePresentationObservation;
var
  LCapture: INyxCollectionColumnsCapture;
  LPresentation: INyxCollectionPresentationCapture;
  LSpec: TNyxCollectionViewSpec;
  LPreferences: TNyxTablePresentationPreferences;
  LValue: TNyxTablePresentationObservation;
  I: Integer;
begin
  LCapture := ACapture;

  if LCapture = nil then
  begin
    raise ENyxCollection.Create('Presentation requires an exact columns capture');
  end;
  LSpec := LCapture.Spec;
  LValue := Default(TNyxTablePresentationObservation);
  LValue.FSerial := LCapture.Serial;
  LPreferences := NyxTablePresentationPreferences;

  if Supports(LCapture, INyxCollectionPresentationCapture, LPresentation) then
  begin
    LPreferences := LPresentation.Preferences;

    if (LPresentation.Serial <> LValue.FSerial) or
      (LPresentation.Spec.ToData.ToJSON <> LSpec.ToData.ToJSON) then
    begin
      raise ENyxCollection.Create('Presentation differs from its original columns capture');
    end;
  end
  else

  if LSpec.HasPresentationPolicy then
  begin
    raise ENyxCollection.Create('This columns capture has no authored presentation support');
  end;
  SetLength(LValue.FFields, LSpec.Count);
  SetLength(LValue.FText, LSpec.Count);
  SetLength(LValue.FAlignment, LSpec.Count);
  for I := 0 to LSpec.Count - 1 do
  begin
    LValue.FFields[I] := LSpec.ColumnAt(I).Identity;
  end;
  LPreferences.ValidateFields(LValue.FFields);
  LValue.FHasRuntimeOverrides := LPreferences.HasDensity or (LPreferences.Count > 0) or
    (LPreferences.AlignmentCount > 0);
  LValue.FTable := LPreferences.ResolveTable(LSpec.TablePresentationPolicy);
  for I := 0 to LSpec.Count - 1 do
  begin
    LValue.FText[I] := LPreferences.ResolveColumn(LValue.FFields[I],
      LSpec.ColumnAt(I).TextPolicy);
    LValue.FAlignment[I] := LPreferences.ResolveAlignment(LValue.FFields[I],
      LSpec.ColumnAt(I).AlignmentPolicy);
  end;
  Result := LValue;
end;

end.
