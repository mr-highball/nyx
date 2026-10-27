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

program nyx_studio_mcp;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.studio.mcpconfig, nyx.test.mcp.client
  {$ifdef MSWINDOWS}, Windows, ShellApi{$endif};

type
  { Command spellings are confined to this process boundary. Design edits still
    enter the server's typed, revision-aware command pipeline through real MCP.
    Project import explicitly replaces the named workspace through one retained
    transport; it cannot implicitly address the primary design. }
  TClientCommand = (ccRegister, ccTools, ccCall, ccImportProject);

  { Preserve an owned structured semantic refusal and exit code independently
    of local argument, file and transport failures. No client or ticket survives
    in this exception; the ordinary outer finally closes their connection. }
  EClientToolRefusal = class(Exception)
  private
    FReply: TNyxDataValue;
  public
    constructor Create(const AReply: TNyxDataValue);
    property Reply: TNyxDataValue read FReply;
  end;

{ Windows owns GetCommandLineW's text; CommandLineToArgvW allocates only the
  parsed pointer/string block. Copy each argument into owned UTF-8 and release
  that block with LocalFree. ANSI ParamStr has already lost supplementary paths.
  POSIX argv bytes are copied/tagged locally; no process codepage is changed. }
function CommandArguments: TNyxStrings;
var
  LIndex: Integer;
  {$ifdef MSWINDOWS}
  LArguments: PLPWSTR;
  LCount: Longint;
  {$else}
  LRaw: RawByteString;
  {$endif}
begin
  Result := TNyxStrings.Create;
  try
    {$ifdef MSWINDOWS}
    LArguments := CommandLineToArgvW(GetCommandLineW, @LCount);

    if LArguments = nil then
    begin
      raise Exception.Create('Cannot read the Unicode command line');
    end;
    try
      for LIndex := 0 to LCount - 1 do
      begin
        Result.Add(UTF8Encode(UnicodeString(PLPWSTR(PtrUInt(LArguments) +
          PtrUInt(LIndex) * SizeOf(PWideChar))^)));
      end;
    finally
      LocalFree(HLOCAL(LArguments));
    end;
    {$else}
    for LIndex := 0 to ParamCount do
    begin
      LRaw := RawByteString(ParamStr(LIndex));
      SetCodePage(LRaw, CP_UTF8, False);
      Result.Add(TNyxText(LRaw));
    end;
    {$endif}
  except
    Result.Free;
    raise;
  end;
end;

function Argument(AArguments: TNyxStrings; AIndex: Integer): TNyxText;
begin
  Result := '';

  if (AIndex >= 0) and (AIndex < AArguments.Count) then
  begin
    Result := AArguments[AIndex];
  end;
end;

{ The RTL file-stream filename overload accepts ANSI on Windows. Open the exact
  UTF-16 path instead and borrow its handle through THandleStream. This function
  owns/closes the OS handle, including size/read failures, and bounds allocation
  before reading. Native POSIX file APIs consume the owned UTF-8 path bytes. }
function ReadInputFile(const AFileName: TNyxText; AMaximumBytes: Integer): TNyxText;
var
  LHandle: THandle;
  LStream: THandleStream;
begin
  {$ifdef MSWINDOWS}
  LHandle := CreateFileW(PWideChar(UTF8Decode(AFileName)), GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

  if LHandle = INVALID_HANDLE_VALUE then
  {$else}
  LHandle := FileOpen(RawByteString(AFileName), fmOpenRead or fmShareDenyWrite);

  if LHandle = THandle(-1) then
  {$endif}
  begin
    raise EFOpenError.Create('Cannot open the input file');
  end;
  LStream := nil;
  try
    LStream := THandleStream.Create(LHandle);

    if (LStream.Size < 1) or (LStream.Size > AMaximumBytes) then
    begin
      raise EArgumentException.Create('Input file size exceeds the admitted byte range');
    end;
    SetLength(Result, Integer(LStream.Size));
    LStream.ReadBuffer(Result[1], Length(Result));
  finally
    LStream.Free;
    {$ifdef MSWINDOWS}CloseHandle(LHandle);{$else}FileClose(LHandle);{$endif}
  end;
end;

function Command(const AText: TNyxText): TClientCommand;
begin

  if AText = 'register' then
  begin
    Exit(ccRegister);
  end;

  if AText = 'tools' then
  begin
    Exit(ccTools);
  end;

  if AText = 'call' then
  begin
    Exit(ccCall);
  end;

  if AText = 'import-project' then
  begin
    Exit(ccImportProject);
  end;
  raise Exception.Create('Use register <repository> <config.toml>, tools <repository> [tool], ' +
    'call <repository> <tool> <arguments.json>, or import-project <repository> ' +
    '<workspace> <revision> <project-file> <operation-id>');
end;

function HasField(const AValue: TNyxDataValue; const AName: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;

  if AValue.Kind <> ndObject then
  begin
    Exit;
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    if AValue.Key(LIndex) = AName then
    begin
      Exit(True);
    end;
  end;
end;

constructor EClientToolRefusal.Create(const AReply: TNyxDataValue);
begin
  inherited Create('Semantic project import refused');
  FReply := AReply;
end;

function ResponseValue(const AResponse: TNyxDataValue): TNyxDataValue;
begin
  Result := AResponse;

  if HasField(AResponse, 'structuredContent') then
  begin
    Result := AResponse.Field('structuredContent');
  end;
end;

{ Staging requests get independent opaque retry identities. Only the final
  publication uses the caller's identity. No timed-out request is auto-retried;
  the caller resolves an ambiguous Apply against its workspace/revision. }
function NewOperation: TNyxText;
var
  LID: TGUID;
begin

  if CreateGUID(LID) <> 0 then
  begin
    raise Exception.Create('Could not create an import operation identity');
  end;
  Result := TNyxText(GUIDToString(LID));
end;

function ImportArguments(const AMode, AWorkspace, AOperation: TNyxText;
  ARevision: Integer; const AExtra: array of TNyxDataField): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, 4 + Length(AExtra));
  LFields[0] := NyxField('mode', NyxData(AMode));
  LFields[1] := NyxField('workspace', NyxData(AWorkspace));
  LFields[2] := NyxField('expectedRevision', NyxData(ARevision));
  LFields[3] := NyxField('operationId', NyxData(AOperation));
  for LIndex := 0 to High(AExtra) do
  begin
    LFields[4 + LIndex] := AExtra[LIndex];
  end;
  Result := NyxObject(LFields);
end;

function ProjectRequest(AClient: TNyxMCPTestClient;
  const AArguments: TNyxDataValue): TNyxDataValue;
var
  LResponse: TNyxDataValue;
begin
  LResponse := AClient.Tool('nyx_project', AArguments);
  Result := ResponseValue(LResponse);

  if HasField(LResponse, 'isError') and LResponse.Field('isError').AsBoolean then
  begin
    raise EClientToolRefusal.Create(Result);
  end;
end;

{ A local file is byte input to the existing transfer protocol, never a server
  path. Validate its complete UTF-8 before reserving an upload, then keep scalar
  offsets separate from byte positions. Chunk/review tickets intentionally die
  when this one authenticated connection closes. Literal matching admission
  cannot silently execute uploaded Pascal or choose one divergent document half. }
function ImportProject(AClient: TNyxMCPTestClient; const AWorkspace: TNyxText;
  ARevision: Integer; const AFileName, AOperation: TNyxText): TNyxDataValue;
const
  { Current project-transfer wire bounds; these do not limit library resources. }
  CMaximumBytes = 4194304;
  CChunkScalars = 4096;
var
  LPacket: TNyxText;
  LReply: TNyxDataValue;
  LImport: TNyxText;
  LTicket: TNyxText;
  LIndex: Integer;
  LStart: Integer;
  LScalar: Integer;
  LChunkScalars: Integer;
  LOffset: Integer;
begin

  if (AWorkspace = '') or (ARevision < 1) or (AOperation = '') then
  begin
    raise EArgumentException.Create('Import requires an explicit workspace, revision and operation identity');
  end;
  LPacket := ReadInputFile(AFileName, CMaximumBytes);
  LIndex := 1;
  while LIndex <= Length(LPacket) do
  begin

    if not NyxNextScalar(LPacket, LIndex, LScalar) then
    begin
      raise EArgumentException.Create('Project import requires valid UTF-8');
    end;
  end;
  LReply := ProjectRequest(AClient, ImportArguments('begin-import', AWorkspace,
    NewOperation, ARevision, [NyxField('bytes', NyxData(Length(LPacket)))]));
  LReply := LReply.Field('projectImport');
  LImport := LReply.Field('import').AsText;
  LIndex := 1;
  LOffset := 0;
  while LIndex <= Length(LPacket) do
  begin
    LStart := LIndex;
    LChunkScalars := 0;
    while (LIndex <= Length(LPacket)) and (LChunkScalars < CChunkScalars) do
    begin

      if not NyxNextScalar(LPacket, LIndex, LScalar) then
      begin
        raise EArgumentException.Create('Project import requires valid UTF-8');
      end;
      Inc(LChunkScalars);
    end;
    LReply := ProjectRequest(AClient, ImportArguments('append-import', AWorkspace,
      NewOperation, ARevision, [NyxField('import', NyxData(LImport)),
        NyxField('offset', NyxData(LOffset)),
        NyxField('text', NyxData(Copy(LPacket, LStart, LIndex - LStart)))]));
    LReply := LReply.Field('projectImport');
    Inc(LOffset, LChunkScalars);

    if (LReply.Field('nextOffset').AsInteger <> LOffset) or
      (LReply.Field('receivedBytes').AsInteger <> LIndex - 1) then
    begin
      raise Exception.Create('Project import transfer accounting does not match its input');
    end;
  end;
  LReply := ProjectRequest(AClient, ImportArguments('review-import', AWorkspace,
    NewOperation, ARevision, [NyxField('import', NyxData(LImport)),
      NyxField('resolution', NyxData('match')), NyxField('admission', NyxData('literal'))]));
  LReply := LReply.Field('projectImport');
  LTicket := LReply.Field('reviewID').AsText;

  if LTicket = '' then
  begin
    raise Exception.Create('Project import did not return a ready review ticket');
  end;
  Result := ProjectRequest(AClient, ImportArguments('apply', AWorkspace,
    AOperation, ARevision, [NyxField('import', NyxData(LImport)),
      NyxField('reviewID', NyxData(LTicket))]));
end;

var
  LCommand: TClientCommand;
  LRepository: TNyxText;
  LConfiguration: TNyxText;
  LClient: TNyxMCPTestClient;
  LArguments: TNyxDataValue;
  LResult: TNyxDataValue;
  LOutput: TNyxDataValue;
  LTool: TNyxText;
  LIndex: Integer;
  LExit: Integer;
  LFound: Boolean;
  LCommandLine: TNyxStrings;
  LArgumentCount: Integer;
begin
  LClient := nil;
  LCommandLine := nil;
  LExit := 0;
  try
    LCommandLine := CommandArguments;
    LArgumentCount := LCommandLine.Count - 1;
    LCommand := Command(Argument(LCommandLine, 1));

    if LArgumentCount < 2 then
    begin
      raise Exception.Create('An explicit Studio repository is required');
    end;
    LRepository := IncludeTrailingPathDelimiter(ExpandFileName(Argument(LCommandLine, 2)));
    LConfiguration := LRepository + '.codex' + PathDelim + 'config.toml';

    if LCommand = ccRegister then
    begin

      if LArgumentCount <> 3 then
      begin
        raise Exception.Create('Registration requires the chosen Codex config.toml path');
      end;
      NyxMCPRegisterCodex(LRepository, ExpandFileName(Argument(LCommandLine, 3)));
      WriteLn('Nyx MCP registered; Studio will refresh this managed entry on launch.');
    end
    else
    begin
      { Reuse the qualified native protocol consumer used by HTTP integration
        tests. This developer/demo tool is not a dependency of the product UI.
        Only an explicit command mutates a document; discovery never claims one. }
      NyxMCPConfigBlock(NyxMCPReadBytes(LConfiguration));
      LClient := TNyxMCPTestClient.Create(LConfiguration, 'Scooty semantic client');
      try

        if LCommand = ccTools then
        begin

          if (LArgumentCount < 2) or (LArgumentCount > 3) then
          begin
            raise Exception.Create('Tool discovery requires the repository and an optional exact tool');
          end;
          LTool := Argument(LCommandLine, 3);
          LFound := False;
          LResult := LClient.RPC('tools/list', NyxObject([])).Field('result').Field('tools');
          for LIndex := 0 to LResult.Count - 1 do
          begin
            LOutput := LResult.Item(LIndex);

            if (LTool = '') or (LOutput.Field('name').AsText = LTool) then
            begin
              LFound := True;
              WriteLn(NyxObject([
                NyxField('name', LOutput.Field('name')),
                NyxField('description', LOutput.Field('description')),
                NyxField('inputSchema', LOutput.Field('inputSchema'))]).ToJSON);
            end;
          end;

          if not LFound then
          begin
            raise Exception.Create('The requested semantic tool is not advertised by this Studio');
          end;
        end
        else if LCommand = ccImportProject then
        begin

          if LArgumentCount <> 6 then
          begin
            raise Exception.Create('Project import requires repository, workspace, revision, file and operation identity');
          end;
          LOutput := ImportProject(LClient, Argument(LCommandLine, 3),
            StrToInt(Argument(LCommandLine, 4)), Argument(LCommandLine, 5),
            Argument(LCommandLine, 6));
          WriteLn(LOutput.ToJSON);
        end
        else
        begin

          if LArgumentCount <> 4 then
          begin
            raise Exception.Create('Calls require an exact tool name and an arguments JSON file');
          end;
          LTool := Argument(LCommandLine, 3);

          if Pos('nyx_', LTool) <> 1 then
          begin
            raise Exception.Create('Only Nyx semantic tools are supported');
          end;
          LArguments := TNyxDataValue.ParseJSON(ReadInputFile(Argument(LCommandLine, 4), 4194304));

          if LArguments.Kind <> ndObject then
          begin
            raise Exception.Create('Tool arguments must be a JSON object');
          end;
          LResult := LClient.Tool(LTool, LArguments);
          LOutput := ResponseValue(LResult);
          WriteLn(LOutput.ToJSON);

          if HasField(LResult, 'isError') and LResult.Field('isError').AsBoolean then
          begin
            LExit := 2;
          end;
        end;
      finally
        { Close once and never retry a timed-out mutation. A caller can resolve
          ambiguous delivery with the same server operationId and exact payload. }
        LClient.Close;
      end;
    end;
  except
    on LRefusal: EClientToolRefusal do
    begin
      WriteLn(LRefusal.Reply.ToJSON);
      LExit := 2;
    end;
    on LException: Exception do
    begin
      WriteLn(StdErr, 'Nyx MCP: ', LException.Message);
      LExit := 1;
    end;
  end;
  LClient.Free;
  LCommandLine.Free;
  ExitCode := LExit;
end.
