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

unit nyx.test.resource.failures;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.bytes;

type
  { Closed actual-hosted failure sequence. Each payload is independently owned;
    NotFound means an absent file, not a fabricated transport reply. }
  TNyxResourceFailure = (nrfHealthy, nrfNotFound, nrfMalformed, nrfWrongType,
    nrfOversized, nrfCorrected);

function NyxResourceFailureName(AValue: TNyxResourceFailure): TNyxText;
function NyxResourceFailureBytes(AValue: TNyxResourceFailure): TNyxBytes;

{$ifndef PAS2JS}
type
  { Owns permission to change only copy.json in an explicitly admitted fixture
    directory. The exact three-field marker binds that directory to its HTTP URL.
    Construction requires the healthy baseline; every subsequent change compares
    the previous bytes/absence first. No recursive operations, arbitrary paths,
    user cache/profile, source tree or service lifecycle are involved. }
  TNyxResourceFailureFile = class
  private
    FPath: String;
    FExpected: TNyxBytes;
    FAbsent: Boolean;
  public
    { Initialize only a nonexistent private directory with the healthy byte
      fixture and its exact URL-bound marker. No existing directory is adopted
      or overwritten. Used by maintained native/browser gate orchestration. }
    class procedure Prepare(const ADirectory: String; const AURL: TNyxText); static;
    constructor Create(const ADirectory: String; const AURL: TNyxText);
    { Atomic replacement publishes one complete reply; NotFound removes only the
      previously compared owned file. Refusal never overwrites unexpected bytes.
      A failed journey leaves its last payload for diagnosis; successful callers
      restore Healthy explicitly before releasing this owner. }
    procedure Apply(AValue: TNyxResourceFailure);
  end;
{$endif}

implementation

uses SysUtils, nyx.data
  {$ifndef PAS2JS}, Classes{$ifdef MSWINDOWS}, Windows{$endif}{$endif};

function NyxResourceFailureName(AValue: TNyxResourceFailure): TNyxText;
const
  CNames: array[TNyxResourceFailure] of TNyxText =
    ('healthy', 'not-found', 'malformed', 'wrong-type', 'oversized', 'corrected');
begin
  Result := CNames[AValue];
end;

function NyxResourceFailureBytes(AValue: TNyxResourceFailure): TNyxBytes;
var
  LText: TNyxText;
begin
  case AValue of
    nrfHealthy:
      begin
        LText := '{"headline":"Keep creating 🌙","prompt":"Project name 🌙","count":3}' + #10;
      end;
    nrfNotFound:
      begin
        Exit(nil);
      end;
    nrfMalformed:
      begin
        LText := '{broken';
      end;
    nrfWrongType:
      begin
        LText := '{"headline":"This must not appear","prompt":123,"count":3}';
      end;
    nrfOversized:
      begin
        LText := TNyxText(StringOfChar(' ', 512)) + '{"headline":"Too large"}';
      end;
    nrfCorrected:
      begin
        LText := '{"headline":"Back to creating 🌙","prompt":"A refreshed project 🌙","count":4}';
      end;
  end;
  Result := NyxEncodeUTF8(LText);
end;

{$ifndef PAS2JS}
function ReadOwnedBytes(const APath: String): TNyxBytes;
var
  LStream: TFileStream;
begin
  Result := nil;
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try

    if (LStream.Size < 1) or (LStream.Size > 4096) then
    begin
      raise Exception.Create('Owned failure fixture requires 1..4096 bytes');
    end;
    SetLength(Result, LStream.Size);
    LStream.ReadBuffer(Result[0], Length(Result));
  finally
    LStream.Free;
  end;
end;

function SameBytes(const ALeft, ARight: TNyxBytes): Boolean;
var
  LIndex: Integer;
begin
  Result := Length(ALeft) = Length(ARight);

  if not Result then
  begin
    Exit;
  end;
  for LIndex := 0 to Length(ALeft) - 1 do
  begin

    if ALeft[LIndex] <> ARight[LIndex] then
    begin
      Exit(False);
    end;
  end;
end;

class procedure TNyxResourceFailureFile.Prepare(const ADirectory: String;
  const AURL: TNyxText);
var
  LRoot: String;
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LRoot := ExpandFileName(ADirectory);

  if DirectoryExists(LRoot) or FileExists(LRoot) then
  begin
    raise Exception.Create('Failure fixture preparation requires a nonexistent private directory');
  end;

  if not ForceDirectories(LRoot) then
  begin
    raise Exception.Create('Cannot prepare the owned failure fixture directory');
  end;
  LRoot := IncludeTrailingPathDelimiter(LRoot);
  LBytes := NyxResourceFailureBytes(nrfHealthy);
  LStream := TFileStream.Create(LRoot + 'copy.json', fmCreate);
  try
    LStream.WriteBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  LBytes := NyxEncodeUTF8(NyxObject([NyxField('version', NyxData(1)),
    NyxField('service', NyxData('nyx-resource-failure-qualification')),
    NyxField('url', NyxData(AURL))]).ToJSON);
  LStream := TFileStream.Create(LRoot + 'resource-failure-qualification.json', fmCreate);
  try
    LStream.WriteBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
end;

constructor TNyxResourceFailureFile.Create(const ADirectory: String; const AURL: TNyxText);
var
  LRoot: String;
  LMarker: TNyxDataValue;
begin
  inherited Create;
  LRoot := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory));
  LMarker := TNyxDataValue.ParseJSON(NyxDecodeUTF8(
    ReadOwnedBytes(LRoot + 'resource-failure-qualification.json')));

  if (LMarker.Count <> 3) or (LMarker.Field('version').AsInteger <> 1) or
    (LMarker.Field('service').AsText <> 'nyx-resource-failure-qualification') or
    (LMarker.Field('url').AsText <> AURL) then
  begin
    raise Exception.Create('Failure fixture directory belongs to a different qualification');
  end;
  FPath := LRoot + 'copy.json';
  FExpected := NyxResourceFailureBytes(nrfHealthy);

  if not SameBytes(ReadOwnedBytes(FPath), FExpected) then
  begin
    raise Exception.Create('Failure fixture requires its exact healthy baseline');
  end;
end;

procedure TNyxResourceFailureFile.Apply(AValue: TNyxResourceFailure);
var
  LCandidate: String;
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin

  if FAbsent then
  begin

    if FileExists(FPath) then
    begin
      raise Exception.Create('Expected absent owned failure file has been replaced');
    end;
  end
  else if not SameBytes(ReadOwnedBytes(FPath), FExpected) then
  begin
    raise Exception.Create('Owned failure reply changed outside this journey');
  end;

  if AValue = nrfNotFound then
  begin

    if not SysUtils.DeleteFile(FPath) then
    begin
      raise Exception.Create('Cannot remove the compared owned failure reply');
    end;
    FAbsent := True;
    FExpected := nil;
    Exit;
  end;
  LCandidate := FPath + '.candidate';

  if FileExists(LCandidate) then
  begin
    raise Exception.Create('Unexpected failure candidate file already exists');
  end;
  LBytes := NyxResourceFailureBytes(AValue);
  LStream := TFileStream.Create(LCandidate, fmCreate);
  try
    LStream.WriteBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  {$ifdef MSWINDOWS}

  if not MoveFileEx(PChar(LCandidate), PChar(FPath), MOVEFILE_REPLACE_EXISTING or MOVEFILE_WRITE_THROUGH) then
  {$else}

  if not RenameFile(LCandidate, FPath) then
  {$endif}
  begin
    raise Exception.Create('Cannot atomically replace the owned failure reply');
  end;
  FExpected := LBytes;
  FAbsent := False;
end;
{$endif}

end.
