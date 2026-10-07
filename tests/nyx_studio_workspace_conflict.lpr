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

program nyx_studio_workspace_conflict;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Web, nyx.text, nyx.data, nyx.studio.session, nyx.studio.projects,
  nyx.studio.browser;

var
  GStudio: TNyxStudio;
  LLocal: TNyxStudioSession;
begin
  { This program is hosted only on the isolated fixture origin and fresh browser
    profile. Seed the ordinary public recovery boundary with an independent local
    pair, reproducing the user's retained-device conflict without script injection
    into Studio. The shared design is authored separately through semantic MCP. }
  LLocal := TNyxStudioSession.Create;
  try
    LLocal.SetTitle('My local design');
    window.localStorage.setItem('nyx-studio-project-v2', NyxObject([
      NyxField('version', NyxData(2)),
      NyxField('name', NyxData('')),
      NyxField('boundName', NyxData('')),
      NyxField('revision', NyxData('')),
      NyxField('import', NyxData('')),
      NyxField('importName', NyxData('')),
      NyxField('importRevision', NyxData('')),
      NyxField('project', NyxData(EncodeNyxProject(LLocal.ProjectSnapshot)))]).ToJSON);
  finally
    LLocal.Free;
  end;
  GStudio := TNyxStudio.Create;
  GStudio.Run(True);
end.

