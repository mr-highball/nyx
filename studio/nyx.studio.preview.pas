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

unit nyx.studio.preview;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.studio.builds;

type
  { Immutable admitted compiler artifact. RelativePath is a service-owned job
    resource, never a shell command, user control property or local filename.
    ByteCount/MD5 detect delivery mismatch; MD5 is not authentication. }
  TNyxCompiledArtifact = record
  private
    FTarget: TNyxBuildTarget;
    FRelativePath: TNyxText;
    FJob: TNyxText;
    FMD5: TNyxText;
    FByteCount: Integer;
  public
    property Target: TNyxBuildTarget read FTarget;
    property RelativePath: TNyxText read FRelativePath;
    property Job: TNyxText read FJob;
    property MD5: TNyxText read FMD5;
    property ByteCount: Integer read FByteCount;
  end;

{ Explicit compiler wire boundary. Admit only a succeeded/current result whose
  exact artifact appears once in its byte manifest. Reject traversal, alternate
  resource names, malformed GUIDs/fingerprints and downloads over 32 MiB. Caller
  must also compare its local accepted pair/profile before preparing and launching. }
function AdmitNyxCompiledArtifact(const AResult: TNyxDataValue): TNyxCompiledArtifact;

implementation

uses
  SysUtils;

function AdmitNyxCompiledArtifact(const AResult: TNyxDataValue): TNyxCompiledArtifact;
var
  LPath: TNyxText;
  LBuild: TNyxText;
  LFile: TNyxText;
  LManifest: TNyxDataValue;
  LItem: TNyxDataValue;
  LIndex: Integer;
  LMatches: Integer;
  LIdentity: TGUID;
begin
  Result := Default(TNyxCompiledArtifact);

  if (AResult.Field('state').AsText <> 'succeeded') or
    not AResult.Field('currentSource').AsBoolean or
    not AResult.Field('currentOutput').AsBoolean then
  begin
    raise Exception.Create('Compile the current accepted project and output before running a preview');
  end;
  Result.FTarget := ParseNyxBuildTarget(AResult.Field('target').AsText);
  Result.FJob := AResult.Field('job').AsText;
  LPath := AResult.Field('artifact').AsText;
  LBuild := Copy(LPath, 8, 40);
  LFile := 'index.html';

  if Result.FTarget = btNativeLCL then
  begin
    LFile := 'nyx_native.exe';
  end;

  if (Copy(LPath, 1, 7) <> 'builds/') or (Copy(LBuild, 1, 4) <> 'job-') or
    (LPath <> 'builds/' + LBuild + '/' + LFile) or (Result.FJob = '') then
  begin
    raise Exception.Create('Compiled artifact requires its exact admitted job resource');
  end;
  LIdentity := StringToGUID('{' + Copy(LBuild, 5, 36) + '}');
  { StringToGUID validates syntax; a byte-exact round trip also refuses alternate
    separators accepted by a provider. Case remains the service's original text. }

  if UpperCase(Copy(GUIDToString(LIdentity), 2, 36)) <> UpperCase(Copy(LBuild, 5, 36)) then
  begin
    raise Exception.Create('Compiled artifact has an invalid job directory');
  end;
  LManifest := AResult.Field('manifest');

  if (LManifest.Kind <> ndArray) or (LManifest.Count > 8) then
  begin
    raise Exception.Create('Compiled artifact requires a bounded byte manifest');
  end;
  LMatches := 0;
  for LIndex := 0 to LManifest.Count - 1 do
  begin
    LItem := LManifest.Item(LIndex);

    if LItem.Field('path').AsText = LPath then
    begin
      Inc(LMatches);
      Result.FByteCount := LItem.Field('bytes').AsInteger;
      Result.FMD5 := LItem.Field('md5').AsText;
    end;
  end;

  if (LMatches <> 1) or (Result.FByteCount < 1) or
    (Result.FByteCount > 32 * 1024 * 1024) or (Length(Result.FMD5) <> 32) then
  begin
    raise Exception.Create('Compiled artifact requires one exact bounded manifest entry');
  end;
  for LIndex := 1 to Length(Result.FMD5) do
  begin

    if not (Result.FMD5[LIndex] in ['0'..'9', 'a'..'f']) then
    begin
      raise Exception.Create('Compiled artifact fingerprint is malformed');
    end;
  end;
  Result.FRelativePath := LPath;
end;

end.
