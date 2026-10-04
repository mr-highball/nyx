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

unit nyx.studio.compiler;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text;

type
  { Closed compiler meanings; file names and messages remain open user text. }
  TNyxCompilerSeverity = (csError, csFatal, csWarning, csHint, csNote, csInfo);

  { Immutable location/message value. Line/Column retain the compiler's original
    one-based UTF-8 byte coordinates. Navigable means SourceLine/SourceColumn
    locate an exact unchanged region in the submitted companion; that column is
    a Unicode scalar column for the public editor adapters. Unmapped positions
    remain useful diagnostics, never guesses at a different editable source. }
  TNyxCompilerDiagnostic = record
  private
    FFile: TNyxText;
    FMessage: TNyxText;
    FSeverity: TNyxCompilerSeverity;
    FLine: Integer;
    FColumn: Integer;
    FSourceLine: Integer;
    FSourceColumn: Integer;
    function GetNavigable: Boolean;
  public
    property FileName: TNyxText read FFile;
    property Message: TNyxText read FMessage;
    property Severity: TNyxCompilerSeverity read FSeverity;
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
    property SourceLine: Integer read FSourceLine;
    property SourceColumn: Integer read FSourceColumn;
    property Navigable: Boolean read GetNavigable;
  end;

  { Reference-counted immutable build result, independent of documents/widgets.
    Source is the exact submitted companion snapshot, not the current editor.
    Item returns independent values; implementations may supply other providers.
    Encode/Decode are the versioned HTTP boundary. Raw compiler logs remain
    available separately, including lines outside recognized diagnostic syntax. }
  INyxCompilerReport = interface
    ['{12C41B2C-04D0-4A5D-BC42-7E9602AE2ECA}']
    function GetSource: TNyxText;
    function GetCount: Integer;
    function Item(AIndex: Integer): TNyxCompilerDiagnostic;
    function Encode: TNyxText;
    property Source: TNyxText read GetSource;
    property Count: Integer read GetCount;
  end;

function NyxCompilerSeverityName(ASeverity: TNyxCompilerSeverity): TNyxText;
{ Both installed FPC and native pas2js compilers report UTF-8 byte columns.
  ACompanionFile is the admitted job's complete source path. The service requests
  full native paths with -vb; a different or shortened path is not guessed.
  Exact unchanged frame regions map isolated views back to submitted helpers;
  changed builders, dependencies and wrapper/global errors are not navigable.
  At most 512 recognized records are retained; the complete bounded log remains. }
function ReadNyxCompilerReport(const ASubmitted, ACompiled, ACompanionFile,
  ALog: TNyxText): INyxCompilerReport;
{ Admit the complete version-1 shape, closed severities and positive paired
  coordinates before exposing a report. Malformed packets raise without a result. }
function DecodeNyxCompilerReport(const AMessage: TNyxText): INyxCompilerReport;

implementation

uses
  SysUtils,
  nyx.data,
  nyx.source;

const
  SeverityNames: array[TNyxCompilerSeverity] of TNyxText = (
    'Error', 'Fatal', 'Warning', 'Hint', 'Note', 'Info');
  MaximumDiagnostics = 512;

type
  TCompilerReport = class(TInterfacedObject, INyxCompilerReport)
  private
    FSource: TNyxText;
    FItems: array of TNyxCompilerDiagnostic;
  public
    function GetSource: TNyxText;
    function GetCount: Integer;
    function Item(AIndex: Integer): TNyxCompilerDiagnostic;
    function Encode: TNyxText;
    procedure Add(const AItem: TNyxCompilerDiagnostic);
  end;

  { Operation-owned source mapper. It borrows no trees and treats regions as
    exact text values. No line delta is inferred across a changed builder. }
  TSourceMap = class
  private
    FOriginal: TNyxText;
    FCompiled: TNyxText;
    FOriginalPrefix: TNyxText;
    FOriginalBody: TNyxText;
    FOriginalSuffix: TNyxText;
    FCompiledPrefix: TNyxText;
    FCompiledBody: TNyxText;
    FCompiledSuffix: TNyxText;
  public
    constructor Create(const AOriginal, ACompiled: TNyxText);
    function Map(ALine, AByteColumn: Integer;
      out ASourceLine, ASourceColumn: Integer): Boolean;
  end;

function NyxCompilerSeverityName(ASeverity: TNyxCompilerSeverity): TNyxText;
begin
  Result := SeverityNames[ASeverity];
end;

function TNyxCompilerDiagnostic.GetNavigable: Boolean;
begin
  Result := (FSourceLine > 0) and (FSourceColumn > 0);
end;

function NormalizePath(const APath: TNyxText): TNyxText;
var
  LParts: TNyxStrings;
  LStart: Integer;
  LIndex: Integer;
begin
  { Preserve non-ASCII filesystem spelling at the native ANSI RTL boundary. }
  LParts := TNyxStrings.Create;
  try
    LStart := 1;
    for LIndex := 1 to Length(APath) do
    begin

      if APath[LIndex] = '\' then
      begin
        LParts.Add(Copy(APath, LStart, LIndex - LStart));
        LParts.Add('/');
        LStart := LIndex + 1;
      end;
    end;
    LParts.Add(Copy(APath, LStart, MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function SameCompilerPath(const ALeft, ARight: TNyxText): Boolean;
var
  LIndex: Integer;
  LLeft: Integer;
  LRight: Integer;
begin
  Result := False;

  if Length(ALeft) <> Length(ARight) then
  begin
    Exit;
  end;
  for LIndex := 1 to Length(ALeft) do
  begin
    LLeft := Ord(ALeft[LIndex]);
    LRight := Ord(ARight[LIndex]);

    if (LLeft >= Ord('A')) and (LLeft <= Ord('Z')) then
    begin
      Inc(LLeft, 32);
    end;

    if (LRight >= Ord('A')) and (LRight <= Ord('Z')) then
    begin
      Inc(LRight, 32);
    end;

    if LLeft <> LRight then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

function LeafName(const APath: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := NormalizePath(APath);
  for LIndex := Length(Result) downto 1 do
  begin

    if Result[LIndex] = '/' then
    begin
      Exit(Copy(Result, LIndex + 1, MaxInt));
    end;
  end;
end;

{ Return exact scalar coordinates for an admitted storage offset. CRLF is one
  newline; an offset inside any UTF-8/UTF-16 scalar or CRLF pair is refused. }
function Coordinates(const AText: TNyxText; APosition: Integer;
  out ALine, AColumn: Integer): Boolean;
var
  LIndex: Integer;
  LScalar: Integer;
begin
  Result := False;
  ALine := 1;
  AColumn := 1;
  LIndex := 1;

  if (APosition < 1) or (APosition > Length(AText) + 1) then
  begin
    Exit;
  end;
  while LIndex < APosition do
  begin

    if (AText[LIndex] = #13) and (LIndex < Length(AText)) and
      (AText[LIndex + 1] = #10) then
    begin
      Inc(LIndex, 2);
      Inc(ALine);
      AColumn := 1;
    end
    else
    begin

      if not NyxNextScalar(AText, LIndex, LScalar) then
      begin
        Exit;
      end;

      if LScalar = 10 then
      begin
        Inc(ALine);
        AColumn := 1;
      end
      else
      begin
        Inc(AColumn);
      end;
    end;
  end;
  Result := LIndex = APosition;
end;

constructor TSourceMap.Create(const AOriginal, ACompiled: TNyxText);
begin
  inherited Create;
  FOriginal := AOriginal;
  FCompiled := ACompiled;

  if FOriginal <> FCompiled then
  begin
    SplitNyxSourceFrame(FOriginal, FOriginalPrefix, FOriginalBody, FOriginalSuffix);
    SplitNyxSourceFrame(FCompiled, FCompiledPrefix, FCompiledBody, FCompiledSuffix);
  end;
end;

function TSourceMap.Map(ALine, AByteColumn: Integer;
  out ASourceLine, ASourceColumn: Integer): Boolean;
var
  LIndex: Integer;
  LLine: Integer;
  LBytes: Integer;
  LScalar: Integer;
  LPosition: Integer;
  LBodyEnd: Integer;
begin
  Result := False;
  ASourceLine := 0;
  ASourceColumn := 0;

  if (ALine < 1) or (AByteColumn < 1) then
  begin
    Exit;
  end;
  LIndex := 1;
  LLine := 1;
  while (LIndex <= Length(FCompiled)) and (LLine < ALine) do
  begin

    if FCompiled[LIndex] = #10 then
    begin
      Inc(LLine);
    end;
    Inc(LIndex);
  end;

  if LLine <> ALine then
  begin
    Exit;
  end;
  LBytes := 1;
  while LBytes < AByteColumn do
  begin

    if (LIndex > Length(FCompiled)) or (FCompiled[LIndex] = #10) or
      (FCompiled[LIndex] = #13) then
    begin
      Exit;
    end;

    if not NyxNextScalar(FCompiled, LIndex, LScalar) then
    begin
      Exit;
    end;

    if LScalar < $80 then
    begin
      Inc(LBytes);
    end
    else if LScalar < $800 then
    begin
      Inc(LBytes, 2);
    end
    else if LScalar < $10000 then
    begin
      Inc(LBytes, 3);
    end
    else
    begin
      Inc(LBytes, 4);
    end;
  end;

  if LBytes <> AByteColumn then
  begin
    Exit;
  end;
  LPosition := LIndex;

  if FOriginal <> FCompiled then
  begin
    LBodyEnd := Length(FCompiledPrefix) + Length(FCompiledBody);

    if LPosition <= Length(FCompiledPrefix) then
    begin

      if FOriginalPrefix <> FCompiledPrefix then
      begin
        Exit;
      end;
    end
    else if LPosition <= LBodyEnd then
    begin

      if FOriginalBody <> FCompiledBody then
      begin
        Exit;
      end;
      Inc(LPosition, Length(FOriginalPrefix) - Length(FCompiledPrefix));
    end
    else
    begin

      if FOriginalSuffix <> FCompiledSuffix then
      begin
        Exit;
      end;
      Inc(LPosition, Length(FOriginalPrefix) + Length(FOriginalBody) - LBodyEnd);
    end;
  end;
  Result := Coordinates(FOriginal, LPosition, ASourceLine, ASourceColumn);

  if not Result then
  begin
    ASourceLine := 0;
    ASourceColumn := 0;
  end;
end;

function TCompilerReport.GetSource: TNyxText;
begin
  Result := FSource;
end;

function TCompilerReport.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TCompilerReport.Item(AIndex: Integer): TNyxCompilerDiagnostic;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ERangeError.Create('Invalid compiler diagnostic index');
  end;
  Result := FItems[AIndex];
end;

procedure TCompilerReport.Add(const AItem: TNyxCompilerDiagnostic);
var
  LIndex: Integer;
begin
  LIndex := GetCount;
  SetLength(FItems, LIndex + 1);
  FItems[LIndex] := AItem;
end;

function TCompilerReport.Encode: TNyxText;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LItem: TNyxCompilerDiagnostic;
begin
  SetLength(LItems, GetCount);
  for LIndex := 0 to GetCount - 1 do
  begin
    LItem := Item(LIndex);
    LItems[LIndex] := NyxObject([
      NyxField('file', NyxData(LItem.FileName)),
      NyxField('severity', NyxData(Ord(LItem.Severity))),
      NyxField('message', NyxData(LItem.Message)),
      NyxField('line', NyxData(LItem.Line)),
      NyxField('column', NyxData(LItem.Column)),
      NyxField('sourceLine', NyxData(LItem.SourceLine)),
      NyxField('sourceColumn', NyxData(LItem.SourceColumn))]);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('source', NyxData(FSource)), NyxField('items', NyxArray(LItems))]).ToJSON;
end;

function ReadNyxCompilerReport(const ASubmitted, ACompiled, ACompanionFile,
  ALog: TNyxText): INyxCompilerReport;
var
  LReport: TCompilerReport;
  LOwner: INyxCompilerReport;
  LLines: TNyxStrings;
  LMap: TSourceMap;
  LItem: TNyxCompilerDiagnostic;
  LLine: TNyxText;
  LFile: TNyxText;
  LMarker: TNyxText;
  LCoordinates: TNyxText;
  LIndex: Integer;
  LPosition: Integer;
  LOpen: Integer;
  LComma: Integer;
  LSeverity: TNyxCompilerSeverity;
  LKnown: Boolean;
begin
  LReport := TCompilerReport.Create;
  LOwner := LReport;
  LReport.FSource := ASubmitted;
  LLines := TNyxStrings.Create;
  LMap := nil;
  try
    LLines.Text := ALog;
    for LIndex := 0 to LLines.Count - 1 do
    begin
      LLine := LLines[LIndex];

      for LSeverity := Low(TNyxCompilerSeverity) to High(TNyxCompilerSeverity) do
      begin
        LMarker := SeverityNames[LSeverity] + ': ';
        LPosition := Pos(LMarker, LLine);

        if LPosition = 0 then
        begin
          Continue;
        end;
        LItem := Default(TNyxCompilerDiagnostic);
        LItem.FSeverity := LSeverity;
        LItem.FMessage := Copy(LLine, LPosition + Length(LMarker), MaxInt);
        LFile := '';

        if (LPosition > 3) and (Copy(LLine, LPosition - 2, 2) = ') ') then
        begin
          LOpen := LPosition - 3;
          while (LOpen > 0) and (LLine[LOpen] <> '(') do
          begin
            Dec(LOpen);
          end;

          if LOpen > 0 then
          begin
            LCoordinates := Copy(LLine, LOpen + 1, LPosition - LOpen - 3);
            LComma := Pos(',', LCoordinates);
            LItem.FLine := StrToIntDef(Copy(LCoordinates, 1, LComma - 1), 0);
            LItem.FColumn := StrToIntDef(Copy(LCoordinates, LComma + 1, MaxInt), 0);

            if (LComma > 0) and (LItem.FLine > 0) and (LItem.FColumn > 0) then
            begin
              LFile := NormalizePath(Copy(LLine, 1, LOpen - 1));
              LItem.FFile := LeafName(LFile);
            end
            else
            begin
              LItem.FLine := 0;
              LItem.FColumn := 0;
            end;
          end;
        end;
        LKnown := (LFile <> '') and
          SameCompilerPath(LFile, NormalizePath(ACompanionFile));

        if LKnown then
        begin

          if LMap = nil then
          begin
            LMap := TSourceMap.Create(ASubmitted, ACompiled);
          end;
          LMap.Map(LItem.FLine, LItem.FColumn, LItem.FSourceLine, LItem.FSourceColumn);
        end;

        if LReport.GetCount < MaximumDiagnostics then
        begin
          LReport.Add(LItem);
        end;
        Break;
      end;
    end;
    Result := LOwner;
  finally
    LMap.Free;
    LLines.Free;
  end;
end;

function DecodeNyxCompilerReport(const AMessage: TNyxText): INyxCompilerReport;
var
  LReport: TCompilerReport;
  LOwner: INyxCompilerReport;
  LData: TNyxDataValue;
  LItems: TNyxDataValue;
  LValue: TNyxDataValue;
  LItem: TNyxCompilerDiagnostic;
  LIndex: Integer;
  LSeverity: Integer;
  LPosition: Integer;
  LLine: Integer;
  LColumn: Integer;
begin
  LData := TNyxDataValue.ParseJSON(AMessage);

  if (LData.Kind <> ndObject) or (LData.Count <> 3) or
    (LData.Field('version').AsInteger <> 1) then
  begin
    raise EArgumentException.Create('Invalid compiler diagnostic protocol');
  end;
  LItems := LData.Field('items');

  if (LItems.Kind <> ndArray) or (LItems.Count > MaximumDiagnostics) then
  begin
    raise EArgumentException.Create('Compiler diagnostic record budget exceeded');
  end;
  LReport := TCompilerReport.Create;
  LOwner := LReport;
  LReport.FSource := LData.Field('source').AsText;
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LValue := LItems.Item(LIndex);

    if (LValue.Kind <> ndObject) or (LValue.Count <> 7) then
    begin
      raise EArgumentException.Create('Invalid compiler diagnostic fields');
    end;
    LSeverity := LValue.Field('severity').AsInteger;

    if (LSeverity < Ord(Low(TNyxCompilerSeverity))) or
      (LSeverity > Ord(High(TNyxCompilerSeverity))) then
    begin
      raise EArgumentException.Create('Unknown compiler diagnostic severity');
    end;
    LItem := Default(TNyxCompilerDiagnostic);
    LItem.FSeverity := TNyxCompilerSeverity(LSeverity);
    LItem.FFile := LValue.Field('file').AsText;
    LItem.FMessage := LValue.Field('message').AsText;
    LItem.FLine := LValue.Field('line').AsInteger;
    LItem.FColumn := LValue.Field('column').AsInteger;
    LItem.FSourceLine := LValue.Field('sourceLine').AsInteger;
    LItem.FSourceColumn := LValue.Field('sourceColumn').AsInteger;

    if (LItem.FLine < 0) or (LItem.FColumn < 0) or
      ((LItem.FLine = 0) <> (LItem.FColumn = 0)) or
      (LItem.FSourceLine < 0) or (LItem.FSourceColumn < 0) or
      ((LItem.FSourceLine = 0) <> (LItem.FSourceColumn = 0)) or
      (LItem.Navigable and ((LItem.FFile = '') or (LItem.FLine = 0))) then
    begin
      raise EArgumentException.Create('Invalid compiler diagnostic coordinates');
    end;

    if LItem.Navigable then
    begin
      LPosition := NyxTextPosition(LReport.FSource, LItem.SourceLine, LItem.SourceColumn);

      if not Coordinates(LReport.FSource, LPosition, LLine, LColumn) or
        (LLine <> LItem.SourceLine) or (LColumn <> LItem.SourceColumn) then
      begin
        raise EArgumentException.Create('Compiler source position is outside its snapshot');
      end;
    end;
    LReport.Add(LItem);
  end;
  Result := LOwner;
end;

end.
