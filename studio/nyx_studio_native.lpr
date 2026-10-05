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

program nyx_studio_native;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Forms, SysUtils, nyx.text, nyx.studio.lcl, nyx.studio.workspaces;

var
  GHost: TForm;
  GStudio: TNyxNativeStudio;
  LDirectory: TNyxText;
  LWorkspace: TNyxWorkspaceRef;
begin
  Application.Initialize;
  Application.Title := 'Nyx Studio';
  Application.CreateForm(TForm, GHost);
  GHost.Caption := 'Nyx Studio';
  GHost.SetBounds(80, 80, 1280, 900);
  LDirectory := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + 'projects';

  if ParamCount > 0 then
  begin
    LDirectory := ParamStr(1);
  end;
  GStudio := TNyxNativeStudio.Create(GHost, LDirectory);
  try
    GStudio.Run;

    if ParamCount > 1 then
    begin
      LWorkspace := NyxPrimaryWorkspace;

      if ParamCount > 2 then
      begin
        LWorkspace := NyxWorkspace(ParamStr(3));
      end;
      GStudio.ConnectService(ParamStr(2), LWorkspace);
    end;
    Application.Run;
  finally
    GStudio.Free;
  end;
end.
