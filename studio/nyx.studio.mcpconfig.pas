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

unit nyx.studio.mcpconfig;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, nyx.text;

const
  NyxMCPConfigBegin = '# BEGIN NYX STUDIO MCP (managed locally)';
  NyxMCPConfigEnd = '# END NYX STUDIO MCP (managed locally)';

{ Native host configuration, never portable document data. The publisher changes
  only its marked block, retains unrelated bytes, refuses unmanaged/ambiguous
  entries and backs up exact previous bytes before atomic replacement. Paths,
  credentials and backups stay local; errors deliberately omit their contents. }
function NyxMCPReadBytes(const APath: TNyxText): TNyxText;
function NyxMCPConfigBlock(const AConfiguration: TNyxText): TNyxText;
procedure NyxMCPPublishBlock(const APath, ABlock: TNyxText);

{ Explicit enrollment into a user-selected Codex config.toml. Studio still always
  publishes its project entry. Enrollment stores only the chosen absolute path in
  ignored .local/codex-mcp-registration.json; each launch refreshes both entries.
  Registration does not grant editor permissions or promise client hot reload. }
procedure NyxMCPRegisterCodex(const ARepository, AConfiguration: TNyxText);
procedure NyxMCPRefreshRegistration(const ARepository, ABlock: TNyxText);

implementation

uses
  nyx.data;

{$ifdef WINDOWS}
function NyxMoveFileExW(AExisting, AReplacement: PWideChar;
  AFlags: LongWord): LongBool; stdcall; external 'kernel32.dll' name 'MoveFileExW';
{$endif}

function NyxMCPReadBytes(const APath: TNyxText): TNyxText;
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try

    if LFile.Size > 4 * 1024 * 1024 then
    begin
      raise Exception.Create('Local MCP configuration exceeds its byte budget');
    end;
    SetLength(Result, LFile.Size);
    SetCodePage(RawByteString(Result), CP_UTF8, False);

    if LFile.Size > 0 then
    begin
      LFile.ReadBuffer(Result[1], LFile.Size);
    end;
  finally
    LFile.Free;
  end;
end;

procedure WriteBytes(const APath, AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

procedure PublishBytes(const APath, AOriginal, AText: TNyxText; AExisted: Boolean);
var
  LID: TGUID;
  LTemp: TNyxText;
  LPublished: Boolean;
begin
  ForceDirectories(ExtractFileDir(APath));
  CreateGUID(LID);
  LTemp := APath + '.nyx-new-' + GUIDToString(LID);
  try
    WriteBytes(LTemp, AText);

    if FileExists(APath) <> AExisted then
    begin
      raise Exception.Create('Codex configuration changed during registration; existing file retained');
    end;

    if AExisted then
    begin

      if NyxMCPReadBytes(APath) <> AOriginal then
      begin
        raise Exception.Create('Codex configuration changed during registration; existing file retained');
      end;
      WriteBytes(APath + '.nyx-backup', AOriginal);
    end;
    {$ifdef WINDOWS}
    LPublished := NyxMoveFileExW(PWideChar(UTF8Decode(LTemp)), PWideChar(UTF8Decode(APath)), $9);
    {$else}
    LPublished := RenameFile(LTemp, APath);
    {$endif}

    if not LPublished then
    begin
      raise Exception.Create('Could not publish local Codex MCP configuration');
    end;
  finally

    if FileExists(LTemp) then
    begin
      DeleteFile(LTemp);
    end;
  end;
end;

procedure BlockBounds(const AText: TNyxText; out AStart, AFinish: Integer);
begin
  AStart := Pos(NyxMCPConfigBegin, AText);
  AFinish := Pos(NyxMCPConfigEnd, AText);

  if (AStart > 0) <> (AFinish > 0) then
  begin
    raise Exception.Create('Incomplete Nyx-managed Codex configuration; existing file retained');
  end;

  if AStart > 0 then
  begin

    if (AFinish <= AStart) or
      (Pos(NyxMCPConfigBegin, Copy(AText, AStart + Length(NyxMCPConfigBegin), MaxInt)) > 0) or
      (Pos(NyxMCPConfigEnd, Copy(AText, AFinish + Length(NyxMCPConfigEnd), MaxInt)) > 0) then
    begin
      raise Exception.Create('Ambiguous Nyx-managed Codex configuration; existing file retained');
    end;
    { Markers must occupy entire lines. A similar phrase inside a TOML string
      must not authorize replacing an unrelated part of the operator's file. }

    if ((AStart > 1) and (AText[AStart - 1] <> #10)) or
      ((AFinish > 1) and (AText[AFinish - 1] <> #10)) or
      ((AStart + Length(NyxMCPConfigBegin) <= Length(AText)) and
        not (AText[AStart + Length(NyxMCPConfigBegin)] in [#10, #13])) or
      ((AFinish + Length(NyxMCPConfigEnd) <= Length(AText)) and
        not (AText[AFinish + Length(NyxMCPConfigEnd)] in [#10, #13])) then
    begin
      raise Exception.Create('Invalid Nyx-managed Codex marker boundary; existing file retained');
    end;
  end;
end;

function NyxMCPConfigBlock(const AConfiguration: TNyxText): TNyxText;
var
  LStart: Integer;
  LFinish: Integer;
begin
  BlockBounds(AConfiguration, LStart, LFinish);

  if LStart = 0 then
  begin
    raise Exception.Create('A generated Nyx MCP configuration is required');
  end;
  Result := Copy(AConfiguration, LStart, LFinish + Length(NyxMCPConfigEnd) - LStart);

  if Pos('[mcp_servers.nyx_studio]', Result) = 0 then
  begin
    raise Exception.Create('The managed Nyx MCP entry is missing');
  end;
end;

procedure NyxMCPPublishBlock(const APath, ABlock: TNyxText);
var
  LOriginal: TNyxText;
  LText: TNyxText;
  LStart: Integer;
  LFinish: Integer;
  LExisted: Boolean;
begin

  if NyxMCPConfigBlock(ABlock) <> ABlock then
  begin
    raise Exception.Create('Unexpected bytes outside the supplied Nyx MCP block');
  end;
  LExisted := FileExists(APath);
  LOriginal := '';

  if LExisted then
  begin
    LOriginal := NyxMCPReadBytes(APath);
  end;
  BlockBounds(LOriginal, LStart, LFinish);
  LText := LOriginal;

  if LStart > 0 then
  begin
    LText := Copy(LOriginal, 1, LStart - 1) +
      Copy(LOriginal, LFinish + Length(NyxMCPConfigEnd), MaxInt);
  end;

  { A conservative name guard also covers quoted TOML keys and inline tables.
    This is a bounded block publisher, not a general TOML rewriting engine. It
    may refuse an unrelated occurrence, but must never overwrite an unmanaged
    alternative spelling or add a duplicate table to the operator's file. }

  if Pos('nyx_studio', LText) > 0 then
  begin
    raise Exception.Create('An unmanaged nyx_studio MCP entry exists; existing file retained');
  end;

  if LStart > 0 then
  begin
    LText := Copy(LOriginal, 1, LStart - 1) + ABlock +
      Copy(LOriginal, LFinish + Length(NyxMCPConfigEnd), MaxInt);
  end
  else
  begin
    LText := LOriginal + LineEnding + LineEnding + ABlock + LineEnding;
  end;
  PublishBytes(APath, LOriginal, LText, LExisted);
end;

function RegistrationPath(const ARepository: TNyxText): TNyxText;
begin
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ARepository)) +
    '.local' + PathDelim + 'codex-mcp-registration.json';
end;

function ConfigurationPath(const APath: TNyxText): TNyxText;
begin
  Result := ExpandFileName(APath);

  if (Result <> APath) or (ExtractFileName(Result) <> 'config.toml') then
  begin
    raise Exception.Create('Codex enrollment requires an absolute config.toml path');
  end;
end;

procedure NyxMCPRegisterCodex(const ARepository, AConfiguration: TNyxText);
var
  LPath: TNyxText;
  LTarget: TNyxText;
  LBlock: TNyxText;
  LOriginal: TNyxText;
  LExisted: Boolean;
begin
  LTarget := ConfigurationPath(ExpandFileName(AConfiguration));
  LPath := IncludeTrailingPathDelimiter(ExpandFileName(ARepository)) +
    '.codex' + PathDelim + 'config.toml';
  LBlock := NyxMCPConfigBlock(NyxMCPReadBytes(LPath));
  NyxMCPPublishBlock(LTarget, LBlock);
  LPath := RegistrationPath(ARepository);
  LExisted := FileExists(LPath);
  LOriginal := '';

  if LExisted then
  begin
    LOriginal := NyxMCPReadBytes(LPath);
  end;
  PublishBytes(LPath, LOriginal, NyxObject([
    NyxField('configPath', NyxData(LTarget))]).ToJSON, LExisted);
end;

procedure NyxMCPRefreshRegistration(const ARepository, ABlock: TNyxText);
var
  LPath: TNyxText;
  LRegistration: TNyxDataValue;
begin
  LPath := RegistrationPath(ARepository);

  if not FileExists(LPath) then
  begin
    Exit;
  end;
  LRegistration := TNyxDataValue.ParseJSON(NyxMCPReadBytes(LPath));

  if (LRegistration.Kind <> ndObject) or (LRegistration.Count <> 1) or
    (LRegistration.Key(0) <> 'configPath') then
  begin
    raise Exception.Create('Invalid local Codex registration; existing configuration retained');
  end;
  LPath := ConfigurationPath(LRegistration.Field('configPath').AsText);
  NyxMCPPublishBlock(LPath, ABlock);
end;

end.
