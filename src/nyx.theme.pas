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

unit nyx.theme;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text;

type
  { Semantic theme tokens have the same meaning on both targets. Native adapters
    use color/spacing fields; browser adapters translate them into CSS variables.
    Consumers may replace fields before creating a renderer to theme a subtree. }
  TNyxTheme = class
  public
    Background: TNyxText;
    Surface: TNyxText;
    Text: TNyxText;
    Muted: TNyxText;
    Border: TNyxText;
    Accent: TNyxText;
    AccentText: TNyxText;
    Radius: Integer;
    { Surface and compact control radii are distinct semantic metrics. The
      default 12/8 pixel relationship is shared by browser and native widgets. }
    ControlRadius: Integer;
    FontSize: Integer;
    constructor Create(ADark: Boolean = False);
    { Tokens use canonical #RRGGBB colors and logical pixel metrics. Validate
      before mounting either target, so malformed themes produce the same domain
      diagnostic rather than browser CSS fallback or silent native black. }
    procedure Validate;
    { RootSelector is trusted library/application CSS, never design input.
      Renderer-qualified hosts keep palettes/fonts independent for nested views;
      common component recipes consume variables from their nearest host. }
    function CSS(const ARootSelector: TNyxText = '.nyx-root'): TNyxText;
  end;

{ Decode a canonical RGB token as 0..$FFFFFF independently of widgetset or CSS.
  Invalid input raises ENyxModel. Native adapters translate this portable value
  into their platform color representation only at the rendering boundary. }
function NyxThemeRGB(const AValue: TNyxText): Integer;

implementation

uses
  SysUtils,
  nyx.model;

function NyxThemeRGB(const AValue: TNyxText): Integer;
var
  LIndex: Integer;
  LDigit: Integer;
begin

  if (Length(AValue) <> 7) or (AValue[1] <> '#') then
  begin
    raise ENyxModel.Create('Theme color requires #RRGGBB: ' + AValue);
  end;
  Result := 0;
  for LIndex := 2 to 7 do
  begin
    LDigit := Pos(AValue[LIndex], '0123456789abcdef') - 1;

    if LDigit < 0 then
    begin
      LDigit := Pos(AValue[LIndex], '0123456789ABCDEF') - 1;
    end;

    if LDigit < 0 then
    begin
      raise ENyxModel.Create('Theme color requires hexadecimal digits: ' + AValue);
    end;
    Result := Result * 16 + LDigit;
  end;
end;

procedure TNyxTheme.Validate;
begin
  NyxThemeRGB(Background);
  NyxThemeRGB(Surface);
  NyxThemeRGB(Text);
  NyxThemeRGB(Muted);
  NyxThemeRGB(Border);
  NyxThemeRGB(Accent);
  NyxThemeRGB(AccentText);

  if (Radius < 0) or (Radius > 1000) then
  begin
    raise ENyxModel.Create('Theme radius must contain 0..1000 logical pixels');
  end;

  if (FontSize < 1) or (FontSize > 256) then
  begin
    raise ENyxModel.Create('Theme font size must contain 1..256 logical pixels');
  end;

  if (ControlRadius < 0) or (ControlRadius > 1000) then
  begin
    raise ENyxModel.Create('Theme control radius must contain 0..1000 logical pixels');
  end;
end;

constructor TNyxTheme.Create(ADark: Boolean);
begin
  inherited Create;
  Background := '#f3f5fa';
  Surface := '#ffffff';
  Text := '#202737';
  Muted := '#667085';
  Border := '#dfe3ec';
  Accent := '#6858e8';
  AccentText := '#ffffff';
  Radius := 12;
  ControlRadius := 8;
  FontSize := 14;

  if ADark then
  begin
    Background := '#13151e';
    Surface := '#1e2230';
    Text := '#edf0f8';
    Muted := '#a2abc0';
    Border := '#343a4e';
    Accent := '#a69aff';
    AccentText := '#161324';
  end;
end;

function TNyxTheme.CSS(const ARootSelector: TNyxText): TNyxText;
begin
  Validate;
  { Scope recipes to .nyx-root so an embedded Nyx view does not reset the host
    application's controls. Use system fonts and local vector-free decoration;
    there are no remote font, icon or stylesheet dependencies. }
  Result :=
    ARootSelector + '{--nyx-bg:' + Background + ';--nyx-surface:' + Surface +
    ';--nyx-text:' + Text + ';--nyx-muted:' + Muted + ';--nyx-border:' + Border +
    ';--nyx-accent:' + Accent + ';--nyx-on-accent:' + AccentText +
    ';--nyx-radius:' + IntToStr(Radius) + 'px;--nyx-control-radius:' +
    IntToStr(ControlRadius) + 'px;color:var(--nyx-text);' +
    'font:' + IntToStr(FontSize) + 'px/1.5 system-ui,Segoe UI,sans-serif;}' +
    '.nyx-root *{box-sizing:border-box;min-width:0;}' +
    '.nyx-node{position:relative;max-width:100%;}' +
    { The embedded host, rather than the outer browser window, owns automatic
      row wrapping. Explicit Wrap/NoWrap inline policies take precedence. }
    '.nyx-root{container-type:inline-size;}' +
    '.nyx-root .nyx-flow-row{align-items:safe center;}' +
    { Actual direction wins over the original primitive's row/toolbar class.
      Responsive row-to-column transitions share the native automatic stretch
      policy, including leading alignment of explicitly sized children. }
    '.nyx-root .nyx-flow-column{align-items:stretch;}' +
    '.nyx-flow-column>.nyx-node,.nyx-flow-row>.nyx-node{flex-shrink:0;}' +
    '.nyx-root .nyx-aligned>.nyx-node{align-self:auto;}' +
    '@container(max-width:600px){.nyx-flow-row{flex-wrap:wrap;}}' +
    '.nyx-page,.nyx-column,.nyx-panel,.nyx-card,.nyx-group,.nyx-scroll,' +
    '.nyx-component,.nyx-tab{display:flex;flex-direction:column;gap:12px;}' +
    '.nyx-row,.nyx-toolbar{display:flex;flex-direction:row;gap:12px;align-items:center;}' +
    '.nyx-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px;}' +
    '.nyx-page{background:var(--nyx-bg);padding:24px;min-height:100%;}' +
    '.nyx-scroll{overflow:auto;min-height:0;min-width:0;}' +
    '.nyx-scroll>*{flex-shrink:0;}' +
    '.nyx-card,.nyx-panel,.nyx-group{background:var(--nyx-surface);' +
    'border:1px solid var(--nyx-border);border-radius:var(--nyx-radius);padding:20px;}' +
    '.nyx-card{box-shadow:0 4px 18px #20273708;}' +
    '.nyx-heading{margin:0;font-size:24px;line-height:1.25;letter-spacing:-.5px;}' +
    '.nyx-label{margin:0;color:var(--nyx-muted);}' +
    '.nyx-button,.nyx-link{font:inherit;cursor:pointer;}' +
    '.nyx-button{border:1px solid var(--nyx-border);border-radius:var(--nyx-control-radius);' +
    'background:var(--nyx-surface);color:var(--nyx-text);padding:10px 18px;' +
    'font-weight:600;transition:filter .15s,box-shadow .15s;align-self:flex-start;}' +
    '.nyx-button[data-variant=primary]{background:var(--nyx-accent);' +
    'border-color:var(--nyx-accent);color:var(--nyx-on-accent);}' +
    '.nyx-button:hover{filter:brightness(.96);}.nyx-button:active{transform:translateY(1px);}' +
    '.nyx-link{color:var(--nyx-accent);text-decoration:underline;background:none;border:0;}' +
    '.nyx-field{display:flex;flex-direction:column;gap:6px;}' +
    { A declared main-axis weight admits smaller allocations than intrinsic
      content. The field's actual editor fills that allocation while its caption
      retains ordinary text height. Removing the class restores natural sizing. }
    '.nyx-root .nyx-flex{min-height:0;}' +
    '.nyx-flex.nyx-field>.nyx-input-control,' +
    '.nyx-flex.nyx-field>.nyx-select-control,' +
    '.nyx-flex.nyx-field>.nyx-memo-control{flex:1;min-height:0;}' +
    '.nyx-field-caption{font-size:12px;font-weight:600;color:var(--nyx-muted);}' +
    '.nyx-input-control,.nyx-memo-control,.nyx-select-control{font:inherit;width:100%;' +
    'padding:10px 12px;color:var(--nyx-text);background:var(--nyx-surface);' +
    'border:1px solid var(--nyx-border);border-radius:var(--nyx-control-radius);}' +
    '.nyx-memo-control{resize:vertical;min-height:90px;}' +
    '.nyx-check{display:flex;gap:8px;align-items:center;cursor:pointer;}' +
    '.nyx-check input{accent-color:var(--nyx-accent);width:17px;height:17px;}' +
    '.nyx-slider input{accent-color:var(--nyx-accent);width:100%;}' +
    '.nyx-badge{display:inline-flex;align-self:flex-start;padding:4px 10px;' +
    'border-radius:30px;background:color-mix(in srgb,var(--nyx-accent) 12%,transparent);' +
    'color:var(--nyx-accent);font-size:11px;font-weight:700;letter-spacing:.5px;}' +
    '.nyx-alert{border-left:3px solid var(--nyx-accent);padding:14px;' +
    'background:var(--nyx-surface);border-radius:8px;}' +
    '.nyx-progress{accent-color:var(--nyx-accent);width:100%;height:12px;}' +
    '.nyx-separator{width:100%;border:0;border-top:1px solid var(--nyx-border);margin:4px 0;}' +
    '.nyx-spacer{min-height:16px;flex:1;}.nyx-image{object-fit:contain;}' +
    '.nyx-avatar{display:grid;place-items:center;border-radius:50%;width:48px;height:48px;' +
    'background:var(--nyx-accent);color:var(--nyx-on-accent);font-weight:700;}' +
    '.nyx-list{padding:0;margin:0;list-style:none;overflow:auto;}' +
    '.nyx-list li,.nyx-tree summary{padding:10px;border-bottom:1px solid var(--nyx-border);}' +
    '.nyx-table{border-collapse:collapse;width:100%;background:var(--nyx-surface);}' +
    '.nyx-table th,.nyx-table td{text-align:left;padding:10px;border-bottom:1px solid var(--nyx-border);}' +
    '.nyx-code{font:13px/1.6 Consolas,monospace;white-space:pre-wrap;overflow:auto;' +
    'padding:16px;background:var(--nyx-surface);border-radius:8px;}' +
    '.nyx-code-editor{font:13px/1.6 Consolas,monospace;white-space:pre;tab-size:2;' +
    'width:100%;min-height:120px;resize:vertical;padding:12px;'
      + 'color:var(--nyx-text);background:var(--nyx-surface);'
      + 'border:1px solid var(--nyx-border);border-radius:8px;}' +
    '.nyx-tabs{display:flex;flex-wrap:wrap;gap:8px;}.nyx-tabs>.nyx-tab{flex:1;min-width:160px;}' +
    '.nyx-root :focus-visible{outline:3px solid var(--nyx-accent);outline-offset:3px;}' +
    '.nyx-root [disabled]{opacity:.5;cursor:default;}' +
    '.nyx-design .nyx-node{cursor:pointer;}.nyx-design .nyx-selected{' +
    'outline:2px solid var(--nyx-accent);outline-offset:3px;}' +
    '@media(max-width:600px){.nyx-grid{grid-template-columns:1fr;}' +
    '.nyx-page{padding:16px;}}';
end;

end.
