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

program nyx_studio_compiler_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Web, nyx.text, nyx.studio.browser, nyx.test.source.managed;

var
  GStudio: TNyxStudio;
  GPhase: Integer;
  GChecks: Integer;
  GPolls: Integer;
  GSource: TNyxText;
  GEditor: TJSHTMLTextAreaElement;
  GAction: TJSHTMLButtonElement;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
end;

procedure Click(const AID: TNyxText);
begin

  if Find(AID) = nil then
  begin
    raise Exception.Create('Missing compiler journey control: ' + AID);
  end;
  Find(AID).click;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Poll;
var
  LStatus: TNyxText;
  LTarget: TNyxText;
begin
  try
    Inc(GPolls);

    if GPolls > 600 then
    begin
      raise Exception.Create('Compiler journey exceeded its functional poll budget');
    end;
    LStatus := '';

    if Find('studio-agents-status') <> nil then
    begin
      LStatus := Find('studio-agents-status').textContent;
    end;
    case GPhase of
      0:
        begin

          if Pos('revision', LStatus) > 0 then
          begin
            Click('action-code');
            Click('action-reset-source');
            GSource := TJSHTMLTextAreaElement(Find('studio-code')).value;
            { Repeated fixture runs keep one deliberate helper error. They must
              not introduce duplicate procedure declarations into shared files. }
            GSource := StringReplace(GSource,
              #10 + 'procedure BrokenApplicationHelper;' + #10 + 'begin' + #10 +
              '  { 🌙漢字 } MissingApplicationFunction;' + #10 + 'end;', '', [rfReplaceAll]);
            GSource := EditNyxManagedFixture(GSource, #10 + 'end.',
              #10 + 'procedure BrokenApplicationHelper;' + #10 + 'begin' + #10 +
              '  { 🌙漢字 } MissingApplicationFunction;' + #10 + 'end;' + #10 + 'end.');
            GEditor := TJSHTMLTextAreaElement(Find('studio-code'));
            GEditor.value := GSource;
            GEditor.dispatchEvent(TJSEvent.new('change'));
            Click('action-apply-source');
            Check(Find('studio-source-diagnostic') = nil, 'Companion admitted without a source-reader diagnostic');
            Check(TJSHTMLTextAreaElement(Find('studio-code')).value = GSource,
              'Accepted authored helper stays beside the exact builder');
            Click('action-outputs');
            LTarget := 'output-browser';

            if Pos('target=lcl', window.location.search) > 0 then
            begin
              LTarget := 'output-lcl';
            end;
            Click(LTarget);
            Click('action-build-app');
            GPhase := 1;
          end;
        end;
      1:
        begin
          GAction := TJSHTMLButtonElement(document.querySelector('button[data-node^="action-compiler-diagnostic-"]'));

          if GAction <> nil then
          begin
            Check(Pos('MissingApplicationFunction', Find('studio-compiler-diagnostics').textContent) > 0,
              'Actual delegated compiler error appears through Nyx controls');
            Check(not GAction.disabled, 'Current compiler location can be navigated');
            GAction.click;
            GEditor := TJSHTMLTextAreaElement(Find('studio-code'));
            Check(GEditor.selectionStart = Pos('MissingApplicationFunction;', GSource) - 1,
              'Compiler UTF-8 byte column navigates to exact supplementary Unicode source');
            Check(GEditor.value = GSource, 'Navigation leaves accepted Pascal intact');
            GAction := TJSHTMLButtonElement(document.querySelector('button[data-node^="action-compiler-diagnostic-"]'));
            GEditor.value := GSource + #10 + '// new pending draft 🌙漢字';
            GEditor.dispatchEvent(TJSEvent.new('change'));
            Check(GAction.disabled, 'Typing a differing draft disables old compiler location');
            Check(GEditor.value = GSource + #10 + '// new pending draft 🌙漢字',
              'Compiler guard leaves the exact pending draft intact');
            GPhase := 2;
          end;
        end;
      2:
        begin

          if (Pos('Synchronizing', LStatus) = 0) and (Pos('revision', LStatus) > 0) then
          begin
            document.body.setAttribute('data-nyx-compiler-journey', 'passed');
            document.body.setAttribute('data-nyx-compiler-checks', IntToStr(GChecks));
            Exit;
          end;
        end;
    end;
    window.setTimeout(@Poll, 100);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-compiler-journey', 'failed');
      document.body.setAttribute('data-nyx-compiler-error', LException.Message);
      document.body.setAttribute('data-nyx-compiler-phase', IntToStr(GPhase));
      if Find('studio-agents-status') <> nil then
      begin
        document.body.setAttribute('data-nyx-compiler-agent-status', Find('studio-agents-status').textContent);
      end;
      if Find('studio-status') <> nil then
      begin
        document.body.setAttribute('data-nyx-compiler-status', Find('studio-status').textContent);
      end;
    end;
  end;
end;

begin
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  GStudio.ConnectAgents;
  Click('action-agents');
  window.setTimeout(@Poll, 100);
end.
