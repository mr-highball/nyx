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

program nyx_studio_release;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.studio.release;

const
  CPrepareCommand = 'prepare';
  CSealCommand = 'seal';
  CVerifyCommand = 'verify';

var
  LManifest: TNyxDataValue;
begin
  { Arguments are an explicit filesystem/build boundary. The program never
    launches a listener, changes enrollment or selects a running project. }
  try

    if (ParamCount = 4) and (ParamStr(1) = CPrepareCommand) then
    begin
      PrepareNyxStudioRelease(ParamStr(2), ParamStr(3), ParamStr(4));
      WriteLn('{"prepared":true,"sealed":false}');
    end
    else if (ParamCount = 5) and (ParamStr(1) = CSealCommand) then
    begin
      SealNyxStudioRelease(ParamStr(2), ParamStr(3), ParamStr(4), ParamStr(5));
      LManifest := VerifyNyxStudioRelease(ParamStr(2));
      WriteLn(NyxObject([
        NyxField('verified', NyxData(True)),
        NyxField('revision', LManifest.Field('revision')),
        NyxField('files', NyxData(LManifest.Field('files').Count))]).ToJSON);
    end
    else if (ParamCount = 2) and (ParamStr(1) = CVerifyCommand) then
    begin
      LManifest := VerifyNyxStudioRelease(ParamStr(2));
      WriteLn(NyxObject([
        NyxField('verified', NyxData(True)),
        NyxField('revision', LManifest.Field('revision')),
        NyxField('files', NyxData(LManifest.Field('files').Count))]).ToJSON);
    end
    else
    begin
      raise ENyxStudioRelease.Create(
        'Usage: prepare <repository> <matched-rtl> <new-directory> | ' +
        'seal <directory> <revision> <fpc-version> <pas2js-version> | verify <directory>');
    end;
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, LException.ClassName + ': ' + LException.Message);
      ExitCode := 1;
    end;
  end;
end.
