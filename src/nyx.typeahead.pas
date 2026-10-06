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

unit nyx.typeahead;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text;

type
  { Exact preserves case; Folded uses Unicode 17 default full folding. Matching
    never normalizes, translates or modifies authored labels. }
  TNyxTypeAheadMatch = (ntmFolded, ntmExact);

  { Immutable fluent runtime policy. Times are monotonic milliseconds. The
    factory supplies a one-second window, Unicode folding and enabled search. }
  TNyxTypeAheadOptions = record
  private
    FDefined: Boolean;
    FEnabled: Boolean;
    FWindowMS: Integer;
    FMatch: TNyxTypeAheadMatch;
  public
    { Return a copy with runtime search enabled/disabled; Self stays unchanged. }
    function Enabled(AValue: Boolean): TNyxTypeAheadOptions;
    { Inter-key timeout, 1..60000 ms. Invalid copies raise EArgumentException. }
    function WindowMilliseconds(AValue: Integer): TNyxTypeAheadOptions;
    { Closed matching choice; unknown enum values refuse during validation. }
    function Match(AValue: TNyxTypeAheadMatch): TNyxTypeAheadOptions;
    { Undefined records, invalid windows and enum ordinals refuse. }
    procedure Validate;
    property IsEnabled: Boolean read FEnabled;
    property WindowMS: Integer read FWindowMS;
    property MatchMode: TNyxTypeAheadMatch read FMatch;
  end;

  { Pure borrowed reader, used only within Find. It must not mutate the candidate
    order or reenter the search. Search state retains no labels, controls or store. }
  TNyxTypeAheadLabel = function(AIndex: Integer): TNyxText of object;

  { Each control owns an independent managed search. Runtime buffer/focus/time
    expire on reset/disconnect and never enter a document or its Undo history.
    Find returns a zero-based visible candidate, or -1 without a match. Repeated
    single keys cycle; an extended prefix tests the current match first. Explicit
    times make both adapters and tests consume the same portable algorithm. }
  INyxTypeAhead = interface(IInterface)
    ['{D306DE20-C49F-4D7C-AB5B-6C8E05B28049}']
    procedure Reset;
    { Count is nonnegative; focus is -1 or an in-range visible index. A finite
      nonnegative monotonic time and assigned pure reader are required, even
      for an empty order. Invalid arguments raise without changing the buffer.
      Reader exceptions propagate. Named/nonprintable keys reset without match. }
    function Find(const ACharacter: TNyxText; ATimeMS: Double; ACount, AFocus: Integer;
      ALabel: TNyxTypeAheadLabel): Integer;
  end;

function NyxTypeAhead: TNyxTypeAheadOptions;
{ Validate before creating an independent reference-counted search. It retains
  only value-owned policy/buffer/time, never the caller or its borrowed reader. }
function NewNyxTypeAhead(const AOptions: TNyxTypeAheadOptions): INyxTypeAhead;
{ Keyboard adapter admission: exactly one printable Unicode scalar. Named keys,
  control codes, whitespace and malformed encodings belong to other defaults. }
function NyxTypeAheadCharacter(const AText: TNyxText): Boolean;

implementation

uses SysUtils, Math, nyx.text.search;

type
  TTypeAhead = class(TInterfacedObject, INyxTypeAhead)
  private
    FOptions: TNyxTypeAheadOptions;
    FPrefix: TNyxText;
    FCharacters: Integer;
    FTimeMS: Double;
    FHasTime: Boolean;
    FExpectedFocus: Integer;
  public
    constructor Create(const AOptions: TNyxTypeAheadOptions);
    procedure Reset;
    function Find(const ACharacter: TNyxText; ATimeMS: Double; ACount, AFocus: Integer;
      ALabel: TNyxTypeAheadLabel): Integer;
  end;

function NyxTypeAhead: TNyxTypeAheadOptions;
begin
  Result.FDefined := True;
  Result.FEnabled := True;
  Result.FWindowMS := 1000;
  Result.FMatch := ntmFolded;
end;

procedure TNyxTypeAheadOptions.Validate;
begin

  if not FDefined or (FWindowMS < 1) or (FWindowMS > 60000) or
    (Ord(FMatch) < Ord(Low(TNyxTypeAheadMatch))) or
    (Ord(FMatch) > Ord(High(TNyxTypeAheadMatch))) then
  begin
    raise EArgumentException.Create('Typeahead requires a defined policy and window 1..60000 ms');
  end;
end;

function TNyxTypeAheadOptions.Enabled(AValue: Boolean): TNyxTypeAheadOptions;
begin
  Validate;
  Result := Self;
  Result.FEnabled := AValue;
end;

function TNyxTypeAheadOptions.WindowMilliseconds(AValue: Integer): TNyxTypeAheadOptions;
begin
  Validate;
  Result := Self;
  Result.FWindowMS := AValue;
  Result.Validate;
end;

function TNyxTypeAheadOptions.Match(AValue: TNyxTypeAheadMatch): TNyxTypeAheadOptions;
begin
  Validate;
  Result := Self;
  Result.FMatch := AValue;
  Result.Validate;
end;

function NyxTypeAheadCharacter(const AText: TNyxText): Boolean;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  LIndex := 1;
  LScalar := 0;
  Result := NyxNextScalar(AText, LIndex, LScalar) and
    (LIndex > Length(AText)) and (LScalar >= 32) and
    not ((LScalar >= 127) and (LScalar <= 159)) and
    not NyxScalarWhitespace(LScalar);
end;

constructor TTypeAhead.Create(const AOptions: TNyxTypeAheadOptions);
begin
  inherited Create;
  AOptions.Validate;
  FOptions := AOptions;
  Reset;
end;

function NewNyxTypeAhead(const AOptions: TNyxTypeAheadOptions): INyxTypeAhead;
begin
  Result := TTypeAhead.Create(AOptions);
end;

procedure TTypeAhead.Reset;
begin
  FPrefix := '';
  FCharacters := 0;
  FTimeMS := 0;
  FHasTime := False;
  FExpectedFocus := -1;
end;

function TTypeAhead.Find(const ACharacter: TNyxText; ATimeMS: Double;
  ACount, AFocus: Integer; ALabel: TNyxTypeAheadLabel): Integer;
var
  LCharacter: TNyxText;
  LStart: Integer;
  LOffset: Integer;
  LIndex: Integer;
  LLabel: TNyxText;
  LExtend: Boolean;
begin

  if IsNan(ATimeMS) or IsInfinite(ATimeMS) or (ATimeMS < 0) or
    (ACount < 0) or (AFocus < -1) or (AFocus >= ACount) or not Assigned(ALabel) then
  begin
    raise EArgumentException.Create('Typeahead requires finite time and a valid visible candidate range');
  end;
  Result := -1;

  if (ACount = 0) or not FOptions.IsEnabled or not NyxTypeAheadCharacter(ACharacter) then
  begin
    Reset;
    Exit;
  end;

  if FHasTime and ((ATimeMS < FTimeMS) or (ATimeMS - FTimeMS >= FOptions.WindowMS) or
    (AFocus <> FExpectedFocus)) then
  begin
    Reset;
  end;
  LCharacter := ACharacter;

  if FOptions.MatchMode = ntmFolded then
  begin
    LCharacter := NyxFoldText(ACharacter);
  end;
  LExtend := (FCharacters > 0) and not ((FCharacters = 1) and (FPrefix = LCharacter));

  if FCharacters >= 64 then
  begin
    { Bound transient keyboard state even if a device never pauses. }
    Reset;
    LExtend := False;
  end;

  if LExtend then
  begin
    FPrefix := FPrefix + LCharacter;
    Inc(FCharacters);
    LStart := AFocus;
  end
  else
  begin
    FPrefix := LCharacter;
    FCharacters := 1;
    LStart := AFocus + 1;
  end;

  if LStart < 0 then
  begin
    LStart := 0;
  end;
  FTimeMS := ATimeMS;
  FHasTime := True;
  FExpectedFocus := AFocus;
  LIndex := LStart;

  if LIndex >= ACount then
  begin
    LIndex := 0;
  end;
  for LOffset := 0 to ACount - 1 do
  begin
    LLabel := ALabel(LIndex);

    if ((FOptions.MatchMode = ntmFolded) and NyxStartsWithFolded(LLabel, FPrefix)) or
      ((FOptions.MatchMode = ntmExact) and (Copy(LLabel, 1, Length(FPrefix)) = FPrefix)) then
    begin
      FExpectedFocus := LIndex;
      Exit(LIndex);
    end;
    { Avoid adding start+offset: a valid large virtual candidate range can
      overflow that sum before reaching a nearby match under checked FPC. }
    Inc(LIndex);

    if LIndex = ACount then
    begin
      LIndex := 0;
    end;
  end;
end;

end.
