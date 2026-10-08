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


program nyx_resource_runtime_server;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.data, nyx.studio.directories, nyx.studio.server;

var
  LDirectories: TNyxStudioDirectories;
  LServer: TNyxStudioServer;
  LPort: Integer;
  LMarker: TNyxText;
  LFile: TFileStream;
  LAdmission: TNyxDataValue;
begin

  if (ParamCount < 3) or (ParamCount > 4) then
  begin
    raise Exception.Create('Supply source root, owned runtime home, loopback port and optional staged web root');
  end;
  LPort := StrToInt(ParamStr(3));

  if (LPort < 1024) or (LPort >= 65535) then
  begin
    raise Exception.Create('Runtime qualification requires two admitted loopback ports');
  end;
  { No existing editor, runtime or enrollment file is shared. Compiler units stay
    borrowed from the current source tree; all writable output belongs here. }
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(ParamStr(2)).EnrollingProject(ParamStr(2));

  if ParamCount = 4 then
  begin
    LDirectories := LDirectories.ServingFrom(ParamStr(4));
  end;

  if LDirectories.RuntimeRoot = LDirectories.SourceRoot then
  begin
    raise Exception.Create('Qualification cannot enroll or write into the source root');
  end;

  if DirectoryExists(LDirectories.RuntimeRoot + '.local') then
  begin
    { Restart only an origin-matching qualification home. An ordinary Studio's
      existing private directory refuses before its server/config is constructed. }
    LFile := TFileStream.Create(LDirectories.RuntimeRoot +
      '.local' + PathDelim + 'runtime-qualification.json', fmOpenRead or fmShareDenyNone);
    try

      if (LFile.Size < 1) or (LFile.Size > 1024) then
      begin
        raise Exception.Create('Qualification marker requires bounded data');
      end;
      SetLength(LMarker, LFile.Size);
      LFile.ReadBuffer(LMarker[1], Length(LMarker));
    finally
      LFile.Free;
    end;
    LAdmission := TNyxDataValue.ParseJSON(LMarker);

    if (LAdmission.Field('version').AsInteger <> 1) or
      (LAdmission.Field('service').AsText <> 'nyx-resource-runtime-qualification') or
      (LAdmission.Field('origin').AsText <> 'http://127.0.0.1:' + IntToStr(LPort)) then
    begin
      raise Exception.Create('Qualification runtime belongs to a different service');
    end;
  end;
  LServer := TNyxStudioServer.Create(LDirectories, LPort, '127.0.0.1', LPort + 1);
  try
    { The destructive fixture admits only this explicit isolated test host.
      A regular Studio has no marker and cannot accidentally become its target. }
    LMarker := NyxObject([NyxField('version', NyxData(1)),
      NyxField('service', NyxData('nyx-resource-runtime-qualification')),
      NyxField('origin', NyxData('http://127.0.0.1:' + IntToStr(LPort)))]).ToJSON;
    LFile := TFileStream.Create(LDirectories.RuntimeRoot +
      '.local' + PathDelim + 'runtime-qualification.json', fmCreate);
    try
      LFile.WriteBuffer(LMarker[1], Length(LMarker));
    finally
      LFile.Free;
    end;
    LServer.Run;
  finally
    LServer.Free;
  end;
end.
