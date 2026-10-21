{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
program nyx_studio_chrome_preview;

{$mode delphi}{$H+}{$codepage utf8}

uses Web, nyx.studio.browser;

var
  GStudio: TNyxStudio;
begin
  { Ordinary Studio, with recovery/agent attachment disabled for this disposable
    visual review. It uses the real shared controls and browser adapter; no
    alternate HTML shell or HTTP compiler/MCP service is created by the test. }
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  document.body.setAttribute('data-nyx-chrome-preview', 'ready');
end.
