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
unit nyx.studio.theme;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.model, nyx.theme.editor, nyx.studio.session;

{ Presets change only a copied editor proposal. Verify the owning session's exact
  local declaration before prefilling. A stale button never accepts a document.
  Controllers restore/sync the proposal through the existing mounted public form. }
function PrepareNyxStudioThemePreset(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; out ADraft: TNyxThemeEditorDraft): Boolean;

implementation

function PrepareNyxStudioThemePreset(ASession: TNyxStudioSession;
  AButton, AShellRoot: TNyxNode; out ADraft: TNyxThemeEditorDraft): Boolean;
begin
  Result := PrepareNyxThemeEditorPreset(AButton, AShellRoot, ADraft);

  if not Result then
  begin
    Exit;
  end;

  if (ASession = nil) or not NyxThemeEditorMatches(AShellRoot,
    'studio-theme-editor', ASession.Document) then
  begin
    ADraft.Clear;
    raise ENyxModel.Create('Theme changed; review the current palette before choosing a preset');
  end;
end;

end.
