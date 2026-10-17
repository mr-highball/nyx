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
  SysUtils, nyx.text, nyx.pascal.project, nyx.pascal.drafts;

const
  { Shared bounded report/receipt count; raw logs remain separately available. }
  NyxCompilerMaximumDiagnostics = 512;

type
  { Further input after explicit EOF is a lifecycle error, with the retained
    immutable snapshots and already sealed reader left unchanged. }
  ENyxCompilerReportSealed = class(Exception);

  { Closed compiler meanings; file names and messages remain open user text. }
  TNyxCompilerSeverity = (csError, csFatal, csWarning, csHint, csNote, csInfo);

  { Immutable location/message value. Line/Column retain the compiler's original
    one-based UTF-8 byte coordinates. Navigable means SourceLine/SourceColumn
    locate an exact unchanged region in SourceFile; that column is a Unicode
    scalar column for the public editor adapters. Main retains its original
    companion mapping. Helper sites require an exact staged path and retained
    submitted helper text. Unmapped positions
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
    FSourceFile: TNyxPascalSourceRef;
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
    property SourceFile: TNyxPascalSourceRef read FSourceFile;
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

  { Optional immutable navigation context. Main is the report's Source; helper
    text is retained only for mapped locations, never borrowed from an editor.
    This describes compiler input, not execution or permission to replace text.
    Missing helpers refuse. Callers must compare the exact current target buffer
    before navigating, independently of unrelated unfinished buffers. }
  INyxCompilerSourceReport = interface(INyxCompilerReport)
    ['{68FE918B-457C-45BE-B2E0-CA2300000018}']
    function SourceText(const AFile: TNyxPascalSourceRef): TNyxText;
  end;

function NyxCompilerSeverityName(ASeverity: TNyxCompilerSeverity): TNyxText;
type
  { Independent indices keep presentation sorting separate from report identity. }
  TNyxCompilerDiagnosticIndices = array of Integer;

  { One invocation's incremental text decoder, with no document/widget ownership.
    Native chunks may split UTF-8 bytes; browser chunks may split UTF-16 scalars.
    Only complete CR/LF lines are admitted until Finish seals the final fragment.
    Append/Finish return whether recognized records were added. The input budget
    is one Mi text units (native bytes/browser UTF-16 units), with 512 records.
    Snapshot returns an independent immutable report, never a live mutable view.
    Calls are serialized by the owning worker; interfaces may outlive that worker. }
  INyxCompilerReportReader = interface
    ['{D2AA1554-50AB-47EE-91BE-3D2B06268664}']
    function Append(const AChunk: TNyxText): Boolean;
    function Finish: Boolean;
    function Snapshot: INyxCompilerReport;
  end;

{ Capture exact mapping context before a compiler starts. No compiler, callback,
  process, UI or transport is created by this portable parser. }
function NewNyxCompilerReportReader(const ASubmitted, ACompiled,
  ACompanionFile: TNyxText): INyxCompilerReportReader;

{ Capture helpers before compilation. Only an exact full path below the explicit
  staging directory maps; dependency leaves, shortened paths and linked host
  guesses remain unmapped. A nil project preserves the legacy main-only shape. }
function NewNyxProjectCompilerReportReader(const ASubmitted, ACompiled,
  ACompanionFile, AInputDirectory: TNyxText;
  const AInputs: INyxPascalProject): INyxCompilerReportReader;

{ Stable presentation order: errors/fatals, warnings, then hints/notes/info.
  The immutable report retains its original compiler order and item identities.
  Bounded queries should expose actionable errors before dependency chatter. }
function NyxCompilerDiagnosticOrder(const AReport: INyxCompilerReport):
  TNyxCompilerDiagnosticIndices;
{ Both installed FPC and native pas2js compilers report UTF-8 byte columns.
  ACompanionFile is the admitted job's complete source path. The service requests
  full native paths with -vb; a different or shortened path is not guessed.
  Exact unchanged frame regions map isolated views back to submitted helpers;
  changed builders, dependencies and wrapper/global errors are not navigable.
  At most 512 recognized records are retained; the complete bounded log remains. }
function ReadNyxCompilerReport(const ASubmitted, ACompiled, ACompanionFile,
  ALog: TNyxText): INyxCompilerReport;
{ Complete-log counterpart of the same project-aware incremental parser. }
function ReadNyxProjectCompilerReport(const ASubmitted, ACompiled, ACompanionFile,
  AInputDirectory, ALog: TNyxText; const AInputs: INyxPascalProject): INyxCompilerReport;
{ Version one keeps the exact companion-only layout. Version two adds only helper
  snapshots used by mapped sites, within a four-MiB UTF-8 context budget. Closed
  roles, membership, byte/scalar coordinates and complete fields are checked;
  malformed packets raise without exposing a partial report. }
function DecodeNyxCompilerReport(const AMessage: TNyxText): INyxCompilerReport;

implementation

uses
  nyx.data,
  nyx.source, nyx.pascal.names, nyx.bytes;

const
  SeverityNames: array[TNyxCompilerSeverity] of TNyxText = (
    'Error', 'Fatal', 'Warning', 'Hint', 'Note', 'Info');
  MaximumDiagnostics = NyxCompilerMaximumDiagnostics;
  MaximumDiagnosticSourceBytes = 4 * 1024 * 1024;

type
  { One copied helper snapshot used by at least one mapped diagnostic. Entries
    detach with report snapshots; no project/provider or filesystem is retained. }
  TCompilerSourceEntry = record
    FileReference: TNyxPascalSourceRef;
    Text: TNyxText;
  end;
  { Immutable after publication. Readers build private instances and clone the
    record array for snapshots; open text values retain ordinary value semantics. }
  TCompilerReport = class(TInterfacedObject, INyxCompilerReport, INyxCompilerSourceReport)
  private
    FSource: TNyxText;
    FItems: array of TNyxCompilerDiagnostic;
    FSources: array of TCompilerSourceEntry;
    FSourceBytes: Integer;
  public
    function GetSource: TNyxText;
    function GetCount: Integer;
    function Item(AIndex: Integer): TNyxCompilerDiagnostic;
    function Encode: TNyxText;
    procedure Add(const AItem: TNyxCompilerDiagnostic);
    function SourceText(const AFile: TNyxPascalSourceRef): TNyxText;
    function SourceIndex(const AFile: TNyxPascalSourceRef): Integer;
    function RetainSource(const AFile: TNyxPascalSourceRef; const AText: TNyxText): Boolean;
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

  { Mutable state stays private to one invocation. The reader owns frozen input
    values and its lazy main map; snapshots detach both records and used helper
    contexts. No report borrows the reader, input provider or staged filesystem. }
  TCompilerReportReader = class(TInterfacedObject, INyxCompilerReportReader)
  private
    FReport: TCompilerReport;
    FOwner: INyxCompilerReport;
    FMap: TSourceMap;
    FCompiled: TNyxText;
    FCompanionFile: TNyxText;
    FInputs: INyxPascalProject;
    FInputDirectory: TNyxText;
    FPending: TNyxText;
    FScanned: Integer;
    FUnits: Integer;
    FFinished: Boolean;
    procedure ReadLine(const ALine: TNyxText);
    procedure MapHelper(var AItem: TNyxCompilerDiagnostic; const APath: TNyxText);
    procedure CaptureInputs(const AInputDirectory: TNyxText; const AInputs: INyxPascalProject);
  public
    constructor Create(const ASubmitted, ACompiled, ACompanionFile: TNyxText);
    destructor Destroy; override;
    function Append(const AChunk: TNyxText): Boolean;
    function Finish: Boolean;
    function Snapshot: INyxCompilerReport;
  end;

function NyxCompilerSeverityName(ASeverity: TNyxCompilerSeverity): TNyxText;
begin
  Result := SeverityNames[ASeverity];
end;

function NyxCompilerDiagnosticOrder(const AReport: INyxCompilerReport):
  TNyxCompilerDiagnosticIndices;
var
  LGroup: Integer;
  LIndex: Integer;
  LCount: Integer;
  LSeverity: TNyxCompilerSeverity;
  LMatches: Boolean;
begin
  Result := nil;

  if AReport = nil then
  begin
    Exit;
  end;
  SetLength(Result, AReport.Count);
  LCount := 0;
  for LGroup := 0 to 2 do
  begin
    for LIndex := 0 to AReport.Count - 1 do
    begin
      LSeverity := AReport.Item(LIndex).Severity;
      case LGroup of
        0: LMatches := LSeverity in [csError, csFatal];
        1: LMatches := LSeverity = csWarning;
        2: LMatches := LSeverity in [csHint, csNote, csInfo];
      end;

      if LMatches then
      begin
        Result[LCount] := LIndex;
        Inc(LCount);
      end;
    end;
  end;
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

{ FPC may insert an explicit current-directory segment in an otherwise complete
  diagnostic path. Removing /./ preserves that exact file; resolving parent
  traversal could cross a borrowed host link and therefore remains forbidden. }
function NormalizeHelperPath(const APath: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  Result := NormalizePath(APath);

  if (Pos('/../', Result) > 0) or
    (Copy(Result, Length(Result) - 2, 3) = '/..') then
  begin
    Exit('');
  end;
  LPosition := Pos('/./', Result);
  while LPosition > 0 do
  begin
    Result := Copy(Result, 1, LPosition) + Copy(Result, LPosition + 3, MaxInt);
    LPosition := Pos('/./', Result);
  end;

  if Copy(Result, Length(Result) - 1, 2) = '/.' then
  begin
    Result := Copy(Result, 1, Length(Result) - 2);
  end;
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
  LSources: array of TNyxDataValue;
  LFields: array of TNyxDataField;
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

    if Length(FSources) > 0 then
    begin
      SetLength(LFields, 8);
      LFields[0] := NyxField('file', NyxData(LItem.FileName));
      LFields[1] := NyxField('severity', NyxData(Ord(LItem.Severity)));
      LFields[2] := NyxField('message', NyxData(LItem.Message));
      LFields[3] := NyxField('line', NyxData(LItem.Line));
      LFields[4] := NyxField('column', NyxData(LItem.Column));
      LFields[5] := NyxField('sourceLine', NyxData(LItem.SourceLine));
      LFields[6] := NyxField('sourceColumn', NyxData(LItem.SourceColumn));
      LFields[7] := NyxField('sourceFile', NyxData(SourceIndex(LItem.SourceFile)));
      LItems[LIndex] := NyxObject(LFields);
    end;
  end;

  if Length(FSources) > 0 then
  begin
    SetLength(LSources, Length(FSources));
    for LIndex := 0 to High(FSources) do
    begin
      LSources[LIndex] := NyxObject([
        NyxField('role', NyxData(Ord(FSources[LIndex].FileReference.Role))),
        NyxField('name', NyxData(FSources[LIndex].FileReference.Name)),
        NyxField('text', NyxData(FSources[LIndex].Text))]);
    end;
    Exit(NyxObject([NyxField('version', NyxData(2)),
      NyxField('source', NyxData(FSource)), NyxField('items', NyxArray(LItems)),
      NyxField('sources', NyxArray(LSources))]).ToJSON);
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('source', NyxData(FSource)), NyxField('items', NyxArray(LItems))]).ToJSON;
end;

function TCompilerReport.SourceIndex(const AFile: TNyxPascalSourceRef): Integer;
var
  LIndex: Integer;
begin
  AFile.Validate;

  if AFile.Role = npsMain then
  begin
    Exit(0);
  end;
  for LIndex := 0 to High(FSources) do
  begin

    if FSources[LIndex].FileReference.SameFile(AFile) then
    begin
      Exit(LIndex + 1);
    end;
  end;
  Result := -1;
end;

function TCompilerReport.SourceText(const AFile: TNyxPascalSourceRef): TNyxText;
var
  LIndex: Integer;
begin
  LIndex := SourceIndex(AFile);

  if LIndex < 0 then
  begin
    raise EArgumentException.Create('Compiler location has no captured source file');
  end;

  if LIndex = 0 then
  begin
    Exit(FSource);
  end;
  Result := FSources[LIndex - 1].Text;
end;

function TCompilerReport.RetainSource(const AFile: TNyxPascalSourceRef;
  const AText: TNyxText): Boolean;
var
  LIndex: Integer;
  LBytes: Integer;
begin
  LIndex := SourceIndex(AFile);

  if LIndex >= 0 then
  begin

    if SourceText(AFile) <> AText then
    begin
      raise EArgumentException.Create('Compiler file snapshot changed within one report');
    end;
    Exit(True);
  end;
  LBytes := NyxUTF8ByteCount(AText);
  Result := (LBytes <= MaximumDiagnosticSourceBytes - FSourceBytes) and
    (Length(FSources) < MaximumDiagnostics);

  if not Result then
  begin
    Exit;
  end;
  LIndex := Length(FSources);
  SetLength(FSources, LIndex + 1);
  FSources[LIndex].FileReference := AFile;
  FSources[LIndex].Text := AText;
  Inc(FSourceBytes, LBytes);
end;

procedure TCompilerReportReader.MapHelper(var AItem: TNyxCompilerDiagnostic;
  const APath: TNyxText);
var
  LIndex: Integer;
  LInput: INyxPascalInput;
  LFile: TNyxPascalSourceRef;
  LMap: TSourceMap;
  LLine: Integer;
  LColumn: Integer;
  LPath: TNyxText;
begin

  if (FInputs = nil) or (FInputDirectory = '') then
  begin
    Exit;
  end;
  LPath := NormalizeHelperPath(APath);
  for LIndex := 0 to FInputs.Count - 1 do
  begin
    LInput := FInputs.Input(LIndex);

    if LPath <> FInputDirectory + LInput.Name then
    begin
      Continue;
    end;

    if LInput.Kind = npiUnit then
    begin
      LFile := TNyxPascalSourceRef.UnitFile(LInput.UnitReference);
    end
    else
    begin
      LFile := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude(LInput.Name));
    end;
    LMap := TSourceMap.Create(LInput.Text, LInput.Text);
    try

      if LMap.Map(AItem.Line, AItem.Column, LLine, LColumn) and
        FReport.RetainSource(LFile, LInput.Text) then
      begin
        AItem.FSourceFile := LFile;
        AItem.FSourceLine := LLine;
        AItem.FSourceColumn := LColumn;
      end;
    finally
      LMap.Free;
    end;
    { Excess context keeps the readable raw diagnostic, without an unsafe site. }
    Exit;
  end;
end;

procedure TCompilerReportReader.ReadLine(const ALine: TNyxText);
var
  LItem: TNyxCompilerDiagnostic;
  LFile: TNyxText;
  LMarker: TNyxText;
  LCoordinates: TNyxText;
  LPosition: Integer;
  LOpen: Integer;
  LComma: Integer;
  LSeverity: TNyxCompilerSeverity;
  LKnown: Boolean;
begin

  if FReport.GetCount >= MaximumDiagnostics then
  begin
    Exit;
  end;
  for LSeverity := Low(TNyxCompilerSeverity) to High(TNyxCompilerSeverity) do
  begin
    LMarker := SeverityNames[LSeverity] + ': ';
    LPosition := Pos(LMarker, ALine);

    if LPosition = 0 then
    begin
      Continue;
    end;
    LItem := Default(TNyxCompilerDiagnostic);
    LItem.FSeverity := LSeverity;
    LItem.FMessage := Copy(ALine, LPosition + Length(LMarker), MaxInt);
    LFile := '';

    if (LPosition > 3) and (Copy(ALine, LPosition - 2, 2) = ') ') then
    begin
      LOpen := LPosition - 3;
      while (LOpen > 0) and (ALine[LOpen] <> '(') do
      begin
        Dec(LOpen);
      end;

      if LOpen > 0 then
      begin
        LCoordinates := Copy(ALine, LOpen + 1, LPosition - LOpen - 3);
        LComma := Pos(',', LCoordinates);
        LItem.FLine := StrToIntDef(Copy(LCoordinates, 1, LComma - 1), 0);
        LItem.FColumn := StrToIntDef(Copy(LCoordinates, LComma + 1, MaxInt), 0);

        if (LComma > 0) and (LItem.FLine > 0) and (LItem.FColumn > 0) then
        begin
          LFile := NormalizePath(Copy(ALine, 1, LOpen - 1));
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
      SameCompilerPath(LFile, NormalizePath(FCompanionFile));

    if LKnown then
    begin

      if FMap = nil then
      begin
        FMap := TSourceMap.Create(FReport.FSource, FCompiled);
      end;
      FMap.Map(LItem.FLine, LItem.FColumn, LItem.FSourceLine, LItem.FSourceColumn);
    end
    else
    begin
      MapHelper(LItem, LFile);
    end;

    if FReport.GetCount < MaximumDiagnostics then
    begin
      FReport.Add(LItem);
    end;
    Break;
  end;
end;

constructor TCompilerReportReader.Create(const ASubmitted, ACompiled,
  ACompanionFile: TNyxText);
begin
  inherited Create;
  FReport := TCompilerReport.Create;
  FOwner := FReport;
  FReport.FSource := ASubmitted;
  FCompiled := ACompiled;
  FCompanionFile := ACompanionFile;
end;

destructor TCompilerReportReader.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

function TCompilerReportReader.Append(const AChunk: TNyxText): Boolean;
const
  CMaximumUnits = 1024 * 1024;
var
  LBefore: Integer;
  LStart: Integer;
  LIndex: Integer;
begin

  if FFinished then
  begin
    raise ENyxCompilerReportSealed.Create('Compiler message input is already sealed');
  end;

  if Length(AChunk) > CMaximumUnits - FUnits then
  begin
    raise EArgumentException.Create('Incremental compiler log budget exceeded');
  end;
  Inc(FUnits, Length(AChunk));
  LBefore := FReport.GetCount;

  if LBefore >= MaximumDiagnostics then
  begin
    Exit(False);
  end;
  FPending := FPending + AChunk;
  LStart := 1;
  for LIndex := FScanned + 1 to Length(FPending) do
  begin

    if FPending[LIndex] in [#10, #13] then
    begin
      ReadLine(Copy(FPending, LStart, LIndex - LStart));
      LStart := LIndex + 1;
    end;
  end;
  FPending := Copy(FPending, LStart, MaxInt);
  FScanned := Length(FPending);

  if FReport.GetCount >= MaximumDiagnostics then
  begin
    FPending := '';
  end;
  Result := FReport.GetCount <> LBefore;
end;

function TCompilerReportReader.Finish: Boolean;
var
  LBefore: Integer;
begin

  if FFinished then
  begin
    Exit(False);
  end;
  LBefore := FReport.GetCount;
  ReadLine(FPending);
  FPending := '';
  FFinished := True;
  Result := FReport.GetCount <> LBefore;
end;

function TCompilerReportReader.Snapshot: INyxCompilerReport;
var
  LReport: TCompilerReport;
begin
  LReport := TCompilerReport.Create;
  Result := LReport;
  LReport.FSource := FReport.FSource;
  LReport.FItems := Copy(FReport.FItems);
  LReport.FSources := Copy(FReport.FSources);
  LReport.FSourceBytes := FReport.FSourceBytes;
end;

function NewNyxCompilerReportReader(const ASubmitted, ACompiled,
  ACompanionFile: TNyxText): INyxCompilerReportReader;
begin
  Result := TCompilerReportReader.Create(ASubmitted, ACompiled, ACompanionFile);
end;

function NewNyxProjectCompilerReportReader(const ASubmitted, ACompiled,
  ACompanionFile, AInputDirectory: TNyxText;
  const AInputs: INyxPascalProject): INyxCompilerReportReader;
var
  LReader: TCompilerReportReader;
begin
  LReader := TCompilerReportReader.Create(ASubmitted, ACompiled, ACompanionFile);
  Result := LReader;
  LReader.CaptureInputs(AInputDirectory, AInputs);
end;

procedure TCompilerReportReader.CaptureInputs(const AInputDirectory: TNyxText;
  const AInputs: INyxPascalProject);
begin

  if AInputs = nil then
  begin
    Exit;
  end;
  FInputs := CaptureNyxPascalProject(AInputs);

  if FInputs.Source <> FReport.FSource then
  begin
    raise EArgumentException.Create('Compiler helpers belong to different submitted Pascal');
  end;
  FInputDirectory := NormalizeHelperPath(AInputDirectory);

  if (FInputDirectory = '') or not ((FInputDirectory[1] = '/') or
    ((Length(FInputDirectory) >= 3) and (FInputDirectory[2] = ':') and
      (FInputDirectory[3] = '/'))) then
  begin
    raise EArgumentException.Create('Compiler helper mapping requires its full staging directory');
  end;

  if FInputDirectory[Length(FInputDirectory)] <> '/' then
  begin
    FInputDirectory := FInputDirectory + '/';
  end;
end;

function ReadNyxProjectCompilerReport(const ASubmitted, ACompiled, ACompanionFile,
  AInputDirectory, ALog: TNyxText; const AInputs: INyxPascalProject): INyxCompilerReport;
var
  LReader: TCompilerReportReader;
  LOwner: INyxCompilerReportReader;
  LLines: TNyxStrings;
  LIndex: Integer;
begin
  LReader := TCompilerReportReader.Create(ASubmitted, ACompiled, ACompanionFile);
  LOwner := LReader;
  LReader.CaptureInputs(AInputDirectory, AInputs);
  LLines := TNyxStrings.Create;
  try
    LLines.Text := ALog;
    for LIndex := 0 to LLines.Count - 1 do
    begin
      LReader.ReadLine(LLines[LIndex]);
    end;
    Result := LOwner.Snapshot;
  finally
    LLines.Free;
  end;
end;

function ReadNyxCompilerReport(const ASubmitted, ACompiled, ACompanionFile,
  ALog: TNyxText): INyxCompilerReport;
var
  LReader: TCompilerReportReader;
  LOwner: INyxCompilerReportReader;
  LLines: TNyxStrings;
  LIndex: Integer;
begin
  LReader := TCompilerReportReader.Create(ASubmitted, ACompiled, ACompanionFile);
  LOwner := LReader;
  LLines := TNyxStrings.Create;
  try
    { Legacy complete-log admission retains its existing caller budget. Both
      paths share the exact parser and lazy source mapping, without reprocessing
      the accumulated log on every streamed chunk. }
    LLines.Text := ALog;
    for LIndex := 0 to LLines.Count - 1 do
    begin
      LReader.ReadLine(LLines[LIndex]);
    end;
    Result := LOwner.Snapshot;
  finally
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
  LVersion: Integer;
  LSources: TNyxDataValue;
  LFile: TNyxPascalSourceRef;
  LName: TNyxText;
  LText: TNyxText;
  LSourceIndex: Integer;
  LUsed: array of Boolean;
  LMap: TSourceMap;
begin
  LData := TNyxDataValue.ParseJSON(AMessage);

  if LData.Kind <> ndObject then
  begin
    raise EArgumentException.Create('Invalid compiler diagnostic protocol');
  end;
  LVersion := LData.Field('version').AsInteger;

  if ((LVersion <> 1) and (LVersion <> 2)) or
    ((LVersion = 1) and (LData.Count <> 3)) or
    ((LVersion = 2) and (LData.Count <> 4)) then
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

  if LVersion = 2 then
  begin
    LSources := LData.Field('sources');

    if (LSources.Kind <> ndArray) or (LSources.Count < 1) or
      (LSources.Count > MaximumDiagnostics) then
    begin
      raise EArgumentException.Create('Invalid compiler source context');
    end;
    SetLength(LUsed, LSources.Count);
    for LIndex := 0 to LSources.Count - 1 do
    begin
      LValue := LSources.Item(LIndex);

      if (LValue.Kind <> ndObject) or (LValue.Count <> 3) then
      begin
        raise EArgumentException.Create('Invalid compiler source context fields');
      end;
      LName := LValue.Field('name').AsText;
      case LValue.Field('role').AsInteger of
        Ord(npsUnit):
          begin

            if Copy(LName, Length(LName) - 3, 4) <> '.pas' then
            begin
              raise EArgumentException.Create('Compiler unit context requires its .pas leaf');
            end;
            LFile := TNyxPascalSourceRef.UnitFile(
              NyxPascalUnit(Copy(LName, 1, Length(LName) - 4)));
          end;
        Ord(npsInclude):
          begin
            LFile := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude(LName));
          end;
        else
          begin
            raise EArgumentException.Create('Unknown compiler helper source role');
          end;
      end;

      if (LReport.SourceIndex(LFile) >= 0) or
        not LReport.RetainSource(LFile, LValue.Field('text').AsText) then
      begin
        raise EArgumentException.Create('Duplicate or excessive compiler source context');
      end;
    end;
  end;
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LValue := LItems.Item(LIndex);

    if (LValue.Kind <> ndObject) or (LValue.Count <> 6 + LVersion) then
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

    if LVersion = 2 then
    begin
      LSourceIndex := LValue.Field('sourceFile').AsInteger;

      if (LSourceIndex < 0) or (LSourceIndex > Length(LReport.FSources)) then
      begin
        raise EArgumentException.Create('Compiler source context index is outside its report');
      end;

      if LSourceIndex > 0 then
      begin
        LItem.FSourceFile := LReport.FSources[LSourceIndex - 1].FileReference;
        LUsed[LSourceIndex - 1] := True;

        if not LItem.Navigable or (LItem.FileName <> LItem.SourceFile.Name) then
        begin
          raise EArgumentException.Create('Compiler helper location has inconsistent source context');
        end;
      end;
    end;

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
      LText := LReport.SourceText(LItem.SourceFile);
      LPosition := NyxTextPosition(LText, LItem.SourceLine, LItem.SourceColumn);

      if not Coordinates(LText, LPosition, LLine, LColumn) or
        (LLine <> LItem.SourceLine) or (LColumn <> LItem.SourceColumn) then
      begin
        raise EArgumentException.Create('Compiler source position is outside its snapshot');
      end;

      if LItem.SourceFile.Role <> npsMain then
      begin
        LMap := TSourceMap.Create(LText, LText);
        try

          if not LMap.Map(LItem.Line, LItem.Column, LLine, LColumn) or
            (LLine <> LItem.SourceLine) or (LColumn <> LItem.SourceColumn) then
          begin
            raise EArgumentException.Create('Compiler helper byte/scalar coordinates do not match');
          end;
        finally
          LMap.Free;
        end;
      end;
    end;
    LReport.Add(LItem);
  end;
  for LIndex := 0 to High(LUsed) do
  begin

    if not LUsed[LIndex] then
    begin
      raise EArgumentException.Create('Compiler source context has no mapped diagnostic');
    end;
  end;
  Result := LOwner;
end;

end.
