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

unit nyx.studio.sourceconfiguration.native;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.sourceconfiguration;

{ Read bounded exact UTF-8 settings bytes from an ordinary local file. Missing
  returns empty; malformed/nonordinary/unreadable files raise. Decoding settings
  is separate, so a malformed but readable file can retain its expected bytes. }
function ReadNyxLocalSourceSettings(const APath: TNyxText): TNyxText;
{ Publish settings after checking the captured exact previous file/existence.
  Changed files refuse rather than overwrite another editor. A unique temporary
  sibling is flushed and atomically renamed; this owns only that temporary file.
  A persistent ordinary .lock sibling serializes cooperating writers; its handle
  is borrowed by no compiler and released on every exit. Busy writers refuse.
  Windows uses replacement/write-through; other hosts use same-filesystem rename.
  This persists hints only, never an enabled flag, source or document history. }
procedure SaveNyxLocalSourceSettings(const APath: TNyxText;
  const ASettings: TNyxLocalSourceSettings; APreviouslyPresent: Boolean;
  const APrevious: TNyxText);

implementation

uses Classes, SysUtils, nyx.bytes, nyx.model, nyx.studio.release
  {$ifdef MSWINDOWS}, Windows{$else}, BaseUnix{$endif};

{$ifdef MSWINDOWS}
function NyxSourceSettingsMoveFileExW(AExisting, AReplacement: PWideChar;
  AFlags: LongWord): LongBool; stdcall; external 'kernel32.dll' name 'MoveFileExW';
{$endif}

procedure ValidateSettingsFile(const APath: TNyxText);
var
  {$ifdef MSWINDOWS}
  LAttributes: DWORD;
  {$else}
  LAttributes: LongInt;
  {$endif}
begin
  ValidateNyxStudioDirectoryPath(ExtractFileDir(ExpandFileName(APath)), True);
  {$ifdef MSWINDOWS}
  LAttributes := GetFileAttributesW(PWideChar(UTF8Decode(APath)));

  if (LAttributes <> INVALID_FILE_ATTRIBUTES) and
    ((LAttributes and (FILE_ATTRIBUTE_DIRECTORY or FILE_ATTRIBUTE_REPARSE_POINT)) <> 0) then
  begin
    raise ENyxModel.Create('Local source settings require an ordinary file');
  end;
  {$else}
  LAttributes := FileGetAttr(APath);

  if (LAttributes <> -1) and ((LAttributes and (faDirectory or faSymLink)) <> 0) then
  begin
    raise ENyxModel.Create('Local source settings refuse a symbolic link');
  end;
  {$endif}
end;

function ReadNyxLocalSourceSettings(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  Result := '';
  ValidateSettingsFile(APath);

  if not FileExists(APath) then
  begin
    Exit;
  end;
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try

    if LStream.Size > 65536 then
    begin
      raise ENyxModel.Create('Local source settings exceed 64 KiB');
    end;
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) <> 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure SaveNyxLocalSourceSettings(const APath: TNyxText;
  const ASettings: TNyxLocalSourceSettings; APreviouslyPresent: Boolean;
  const APrevious: TNyxText);
var
  LIdentity: TGUID;
  LTemporary: TNyxText;
  LBytes: TNyxBytes;
  LStream: TFileStream;
  LLock: TFileStream;
  LLockPath: TNyxText;
  LPublished: Boolean;
begin
  ValidateSettingsFile(APath);

  if not ForceDirectories(ExtractFileDir(APath)) then
  begin
    raise ENyxModel.Create('Cannot create the local source settings directory');
  end;
  CreateGUID(LIdentity);
  LTemporary := APath + '.new-' + GUIDToString(LIdentity);
  LBytes := NyxEncodeUTF8(ASettings.Encode);
  LLock := nil;
  try
    LLockPath := APath + '.lock';
    ValidateSettingsFile(LLockPath);

    if FileExists(LLockPath) then
    begin
      LLock := TFileStream.Create(LLockPath, fmOpenReadWrite or fmShareExclusive);
    end
    else
    begin
      LLock := TFileStream.Create(LLockPath, fmCreate or fmShareExclusive);
    end;
    {$ifndef MSWINDOWS}

    if (fpFlock(LLock.Handle, LOCK_EX or LOCK_NB) <> 0) or
      (fpFcntl(LLock.Handle, F_SETFD, 1) <> 0) then
    begin
      raise ENyxModel.Create('Local source settings are already owned or cannot lock');
    end;
    {$endif}
    LStream := TFileStream.Create(LTemporary, fmCreate or fmShareExclusive);
    try

      if Length(LBytes) <> 0 then
      begin
        LStream.WriteBuffer(LBytes[0], Length(LBytes));
      end;

      if not FileFlush(LStream.Handle) then
      begin
        raise ENyxModel.Create('Cannot flush the local source settings');
      end;
    finally
      LStream.Free;
    end;

    if (FileExists(APath) <> APreviouslyPresent) or
      (ReadNyxLocalSourceSettings(APath) <> APrevious) then
    begin
      raise ENyxModel.Create('Local source settings changed in another editor; current strategy retained');
    end;
    {$ifdef MSWINDOWS}
    LPublished := NyxSourceSettingsMoveFileExW(PWideChar(UTF8Decode(LTemporary)),
      PWideChar(UTF8Decode(APath)), MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH);
    {$else}
    LPublished := RenameFile(LTemporary, APath);
    {$endif}

    if not LPublished then
    begin
      raise ENyxModel.Create('Cannot publish the local source settings');
    end;
  finally
    { This exact freshly created sibling is the sole cleanup target. }

    if FileExists(LTemporary) then
    begin
      SysUtils.DeleteFile(LTemporary);
    end;
    LLock.Free;
  end;
end;

end.
