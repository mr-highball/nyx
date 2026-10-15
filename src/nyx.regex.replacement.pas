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


unit nyx.regex.replacement;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.regex.types;

type
  { Replacement semantics are structural, never a dollar/backslash substitution
    language. Literal text including $1 or \1 remains literal text. }
  TNyxRegexReplacementKind = (nrpkLiteral, nrpkCapture);
  { Copied immutable replacement part. A default record is undefined; only an
    initialized template produces parts. Capture selectors are typed references. }
  TNyxRegexReplacementPart = record
  private
    FDefined: Boolean;
    FKind: TNyxRegexReplacementKind;
    FLiteral: TNyxText;
    FCapture: TNyxRegexCaptureRef;
  public
    property Defined: Boolean read FDefined;
    property Kind: TNyxRegexReplacementKind read FKind;
    property Text: TNyxText read FLiteral;
    property Reference: TNyxRegexCaptureRef read FCapture;
  end;
  { Independent fluent replacement recipe. An initialized empty recipe deletes
    matches; a default record refuses. Appending copies its vector so prior
    recipes remain unchanged. No compiled pattern or invocation is retained.
    Runtime factories validate selectors against their exact compiled pattern. }
  TNyxRegexReplacement = record
  private
    FDefined: Boolean;
    FParts: array of TNyxRegexReplacementPart;
    function Append(const APart: TNyxRegexReplacementPart): TNyxRegexReplacement;
  public
    function Literal(const AText: TNyxText): TNyxRegexReplacement;
    function Capture(AIndex: Integer): TNyxRegexReplacement; overload;
    function Capture(const AName: TNyxRegexGroupRef): TNyxRegexReplacement; overload;
    function Copy: TNyxRegexReplacement;
    function Count: Integer;
    function Part(AIndex: Integer): TNyxRegexReplacementPart;
    procedure Validate;
    function ToData: TNyxDataValue;
    { The owning compiled pattern's instruction budget bounds untrusted part
      counts before allocation; exact literal text and selector order survive. }
    class function FromData(const AData: TNyxDataValue;
      AMaximumParts: Integer = 65536): TNyxRegexReplacement; static;
  end;

function NyxRegexReplacement: TNyxRegexReplacement;

implementation

uses nyx.flow.types, nyx.bytes;

function NyxRegexReplacement: TNyxRegexReplacement;
begin
  Result := Default(TNyxRegexReplacement);
  Result.FDefined := True;
end;

procedure TNyxRegexReplacement.Validate;
var
  LIndex: Integer;
begin

  if not FDefined then
  begin
    raise ENyxRegex.Create('Regex replacement recipe is undefined');
  end;
  for LIndex := 0 to High(FParts) do
  begin

    if not FParts[LIndex].FDefined then
    begin
      raise ENyxRegex.Create('Regex replacement contains an undefined part');
    end;

    if FParts[LIndex].FKind = nrpkCapture then
    begin
      FParts[LIndex].FCapture.ToData;
    end
    else
    begin
      NyxUTF8ByteCount(FParts[LIndex].FLiteral);
    end;
  end;
end;

function TNyxRegexReplacement.Copy: TNyxRegexReplacement;
var
  LIndex: Integer;
begin
  Validate;
  Result := NyxRegexReplacement;
  SetLength(Result.FParts, Length(FParts));
  for LIndex := 0 to High(FParts) do
  begin
    Result.FParts[LIndex] := FParts[LIndex];
  end;
end;

function TNyxRegexReplacement.Append(const APart: TNyxRegexReplacementPart): TNyxRegexReplacement;
begin
  Result := Copy;
  SetLength(Result.FParts, Length(FParts) + 1);
  Result.FParts[High(Result.FParts)] := APart;
end;

function TNyxRegexReplacement.Literal(const AText: TNyxText): TNyxRegexReplacement;
var
  LPart: TNyxRegexReplacementPart;
begin
  NyxUTF8ByteCount(AText);
  LPart := Default(TNyxRegexReplacementPart);
  LPart.FDefined := True;
  LPart.FKind := nrpkLiteral;
  LPart.FLiteral := AText;
  Result := Append(LPart);
end;

function TNyxRegexReplacement.Capture(AIndex: Integer): TNyxRegexReplacement;
var
  LPart: TNyxRegexReplacementPart;
begin
  LPart := Default(TNyxRegexReplacementPart);
  LPart.FDefined := True;
  LPart.FKind := nrpkCapture;
  LPart.FCapture := NyxRegexCapture(AIndex);
  Result := Append(LPart);
end;

function TNyxRegexReplacement.Capture(const AName: TNyxRegexGroupRef): TNyxRegexReplacement;
var
  LPart: TNyxRegexReplacementPart;
begin
  LPart := Default(TNyxRegexReplacementPart);
  LPart.FDefined := True;
  LPart.FKind := nrpkCapture;
  LPart.FCapture := NyxRegexCapture(AName);
  Result := Append(LPart);
end;

function TNyxRegexReplacement.Count: Integer;
begin

  if not FDefined then
  begin
    raise ENyxRegex.Create('Regex replacement recipe is undefined');
  end;
  Result := Length(FParts);
end;

function TNyxRegexReplacement.Part(AIndex: Integer): TNyxRegexReplacementPart;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxRegex.Create('Regex replacement part index is unavailable');
  end;
  Result := FParts[AIndex];
end;

function TNyxRegexReplacement.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  Validate;
  SetLength(LItems, Length(FParts));
  for LIndex := 0 to High(FParts) do
  begin

    if FParts[LIndex].FKind = nrpkLiteral then
    begin
      LItems[LIndex] := NyxObject([NyxField('literal', NyxData(FParts[LIndex].FLiteral))]);
    end
    else
    begin
      LItems[LIndex] := NyxObject([NyxField('capture', FParts[LIndex].FCapture.ToData)]);
    end;
  end;
  Result := NyxArray(LItems);
end;

class function TNyxRegexReplacement.FromData(const AData: TNyxDataValue;
  AMaximumParts: Integer): TNyxRegexReplacement;
var
  LIndex: Integer;
  LPart: TNyxDataValue;
begin

  if (AMaximumParts < 1) or (AData.Kind <> ndArray) or (AData.Count > AMaximumParts) then
  begin
    raise ENyxRegexLimit.Create('Regex replacement part vector exceeds its admitted budget');
  end;
  Result := NyxRegexReplacement;
  SetLength(Result.FParts, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin
    LPart := AData.Item(LIndex);
    Result.FParts[LIndex].FDefined := True;

    if (LPart.Kind = ndObject) and (LPart.Count = 1) and (LPart.Key(0) = 'literal') then
    begin
      Result.FParts[LIndex].FKind := nrpkLiteral;
      Result.FParts[LIndex].FLiteral := LPart.Field('literal').AsText;
    end
    else
    begin
      ValidateNyxFlowFields(LPart, ['capture']);
      Result.FParts[LIndex].FKind := nrpkCapture;
      Result.FParts[LIndex].FCapture := TNyxRegexCaptureRef.FromData(LPart.Field('capture'));
    end;
  end;
  Result.Validate;
end;

end.
