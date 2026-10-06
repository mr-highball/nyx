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

program nyx_studio_release_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.studio.release;

type
  TManifestFault = (mfTraversal, mfAbsolutePath, mfDuplicatePath, mfDigest,
    mfVersion, mfCompilerPath, mfExtraField, mfEmptyInventory);

const
  CRevision = '0123456789abcdef0123456789abcdef01234567';
  CFixtureFiles: array[0..14] of String = (
    'LICENSE', 'web/index.html', 'web/agent-preview.html', 'web/rtl.js',
    'web/nyx_studio.js', 'web/nyx_source_worker.js', 'web/nyx_studio_preview.js',
    'src/nyx.model.pas', 'src/nyx.content.pas', 'src/nyx.content.editor.pas',
    'studio/nyx_studio_server.lpr', 'studio/nyx_studio.lpr', 'studio/nyx_source_worker.lpr',
    'studio/nyx.studio.release.pas', 'bin/nyx_studio_server.exe');

var
  LPackage: TNyxText;
  LFixture: TNyxText;
  LOriginal: TNyxDataValue;
  LFault: TManifestFault;
  LIndex: Integer;
  LChecks: Integer;
  LRefused: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

procedure Put(const AMember, AText: TNyxText);
var
  LStream: TFileStream;
begin
  { Only this new fixture is writable. No real package/service file is changed.
    Fixture executables contain inert text and are never executed. }
  LStream := TFileStream.Create(LFixture +
    StringReplace(AMember, '/', PathDelim, [rfReplaceAll]), fmCreate);
  try
    LStream.WriteBuffer(AText[1], Length(AText));
  finally
    LStream.Free;
  end;
end;

procedure RequireVerifyRefusal;
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    VerifyNyxStudioRelease(LFixture);
  except
    on LError: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Invalid release was accepted');
end;

function FaultedManifest(AFault: TManifestFault): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LEntry: TNyxDataValue;
  LPath: TNyxText;
  LDigest: TNyxText;
  LCompiler: TNyxText;
  LVersion: Integer;
  LItemIndex: Integer;
begin
  SetLength(LItems, LOriginal.Field('files').Count);
  for LItemIndex := 0 to High(LItems) do
  begin
    LItems[LItemIndex] := LOriginal.Field('files').Item(LItemIndex).Copy;
  end;
  LEntry := LItems[0];
  LPath := LEntry.Field('path').AsText;
  LDigest := LEntry.Field('md5').AsText;
  LCompiler := '3.2.0';
  LVersion := 1;
  case AFault of
    mfTraversal:
      LPath := '../LICENSE';
    mfAbsolutePath:
      LPath := 'C:/outside/LICENSE';
    mfDigest:
      LDigest := '00000000000000000000000000000000';
    mfVersion:
      LVersion := 2;
    mfCompilerPath:
      LCompiler := 'C:/private/fpc.exe';
    mfDuplicatePath, mfExtraField, mfEmptyInventory:
      begin
        { These change array/object shape below; scalar fields remain exact. }
      end;
  end;
  LItems[0] := NyxObject([
    NyxField('path', NyxData(LPath)),
    NyxField('bytes', LEntry.Field('bytes')),
    NyxField('md5', NyxData(LDigest))]);

  if AFault = mfDuplicatePath then
  begin
    LItems[1] := LItems[0].Copy;
  end;

  if AFault = mfEmptyInventory then
  begin
    SetLength(LItems, 0);
  end;
  Result := NyxObject([
    NyxField('version', NyxData(LVersion)),
    NyxField('revision', NyxData(CRevision)),
    NyxField('compilers', NyxObject([
      NyxField('fpc', NyxData(LCompiler)), NyxField('pas2js', NyxData('3.3.1'))])),
    NyxField('files', NyxArray(LItems))]);

  if AFault = mfExtraField then
  begin
    Result := NyxObject([
      NyxField('version', Result.Field('version')),
      NyxField('revision', Result.Field('revision')),
      NyxField('compilers', Result.Field('compilers')),
      NyxField('files', Result.Field('files')),
      NyxField('private-profile', NyxData('must refuse'))]);
  end;
end;

begin
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Usage: <real-package> <new-fixture-directory>');
    end;
    LPackage := ExpandFileName(ParamStr(1));
    LFixture := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));
    LOriginal := VerifyNyxStudioRelease(LPackage);
    Check(LOriginal.Field('files').Count > Length(CFixtureFiles),
      'The real package must include its complete compiler-source snapshot');
    Check(not DirectoryExists(LFixture), 'Fixture destination must be new');
    Check(CreateDir(ExcludeTrailingPathDelimiter(LFixture)), 'Cannot create owned fixture');
    Check(CreateDir(LFixture + 'src') and CreateDir(LFixture + 'studio') and
      CreateDir(LFixture + 'web') and CreateDir(LFixture + 'bin'), 'Cannot create fixture layout');
    for LIndex := Low(CFixtureFiles) to High(CFixtureFiles) do
    begin
      Put(CFixtureFiles[LIndex], 'fixture');
    end;
    { Dedicated supplementary text qualifies byte fingerprinting without changing
      the English product demos or claiming that these inert sources compile. }
    Put('src/nyx.model.pas', '// Exact UTF-8 supplementary text: 🚀');
    SealNyxStudioRelease(LFixture, CRevision, '3.2.0', '3.3.1');
    LOriginal := VerifyNyxStudioRelease(LFixture);
    Check(LOriginal.Field('files').Count = Length(CFixtureFiles), 'Fixture closure differs');

    LRefused := False;
    try
      SealNyxStudioRelease(LFixture, CRevision, '3.2.0', '3.3.1');
    except
      on LError: ENyxStudioRelease do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Sealed release was overwritten');
    LRefused := False;
    try
      PrepareNyxStudioRelease(LPackage, LPackage + PathDelim + 'web' + PathDelim + 'rtl.js',
        ExcludeTrailingPathDelimiter(LFixture));
    except
      on LError: ENyxStudioRelease do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Existing release directory was overwritten');

    Put('web/rtl.js', 'changed');
    RequireVerifyRefusal;
    Put('web/rtl.js', 'fixture');
    Put('web/nyx_studio.js', 'extra bytes');
    RequireVerifyRefusal;
    Put('web/nyx_studio.js', 'fixture');
    Check(DeleteFile(LFixture + 'web' + PathDelim + 'nyx_source_worker.js'),
      'Cannot remove the owned missing-worker fixture');
    RequireVerifyRefusal;
    Put('web/nyx_source_worker.js', 'fixture');
    Put('web/private-token.txt', 'never distribute');
    RequireVerifyRefusal;
    Check(DeleteFile(LFixture + 'web' + PathDelim + 'private-token.txt'),
      'Cannot retire the owned extra-file fixture');
    Check(CreateDir(LFixture + '.local'), 'Cannot create the private-directory fixture');
    RequireVerifyRefusal;
    Check(RemoveDir(LFixture + '.local'), 'Cannot retire the empty private-directory fixture');
    Check(CreateDir(LFixture + 'src' + PathDelim + 'nyx.extra.pas'),
      'Cannot create the misleading source-directory fixture');
    RequireVerifyRefusal;
    Check(RemoveDir(LFixture + 'src' + PathDelim + 'nyx.extra.pas'),
      'Cannot retire the empty misleading directory');
    for LFault := Low(TManifestFault) to High(TManifestFault) do
    begin
      Put('release.nyx', FaultedManifest(LFault).ToJSON);
      RequireVerifyRefusal;
    end;
    Put('release.nyx', LOriginal.ToJSON);
    Check(VerifyNyxStudioRelease(LFixture).ToJSON = LOriginal.ToJSON,
      'Refusal must retain the original artifact bytes');
    Check(VerifyNyxStudioRelease(LPackage).Field('revision').AsText <> '',
      'The real release must remain independently verifiable');
    WriteLn('PASS ', LChecks, ' release preparation/integrity checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.ClassName, ': ', LError.Message);
      ExitCode := 1;
    end;
  end;
end.
