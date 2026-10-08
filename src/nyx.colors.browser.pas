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
unit nyx.colors.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, JS, Web, nyx.text, nyx.colors, nyx.contract;

type
  { A renderer owns this UI-thread adapter; callbacks/Editor are borrowed.
    It creates only the grouped DOM face, never descriptors/application state.
    Publication is one typed completion after native chooser confirmation. }
  TNyxBrowserColorCommit = procedure(const AValue: TNyxRGBColor) of object;

  TNyxBrowserColorField = class
  private
    FEditor: TJSHTMLInputElement;
    FGroup: TJSHTMLElement;
    FPicker: TJSHTMLInputElement;
    FDomain: TNyxValueDomain;
    FValue: TNyxRGBColor;
    FCommit: TNyxBrowserColorCommit;
    FConnected: Boolean;
    FEditable: Boolean;
    FRevision: Integer;
    FOpeningRevision: Integer;
    function Opening(AEvent: TJSMouseEvent): Boolean;
    function Commit(AEvent: TEventListenerEvent): Boolean;
  public
    { Editor must already be parented; nil/unparented input refuses. The group
      replaces its physical location without replacing its identity or buffer. }
    constructor Create(AEditor: TJSHTMLInputElement; ACommit: TNyxBrowserColorCommit);
    destructor Destroy; override;
    { Synchronize copied accepted value/domain and effective ancestor policy.
      Never overwrite an unfinished Editor draft. A changed accepted context
      revokes an already open chooser proposal; the opaque native dialog itself
      remains browser-owned. Unsupported alpha/gamut are explicitly omitted. }
    procedure Sync(const AValue: TNyxRGBColor; const ADomain: TNyxValueDomain;
      AEnabled, AReadOnly, AVisible: Boolean; const AName: TNyxText);
    procedure Disconnect;
    property Editor: TJSHTMLInputElement read FEditor;
    property Picker: TJSHTMLInputElement read FPicker;
    property Group: TJSHTMLElement read FGroup;
  end;

implementation

constructor TNyxBrowserColorField.Create(AEditor: TJSHTMLInputElement;
  ACommit: TNyxBrowserColorCommit);
begin
  inherited Create;

  if (AEditor = nil) or (AEditor.parentNode = nil) then
  begin
    raise EArgumentException.Create('A browser color field requires a parented editor');
  end;
  FEditor := AEditor;
  FCommit := ACommit;
  FDomain := NyxRGBDomain.Definition;
  FValue := NyxNoColor;
  FOpeningRevision := -1;
  FGroup := TJSHTMLElement(document.createElement('span'));
  FGroup.className := 'nyx-color-controls';
  FGroup.setAttribute('role', 'group');
  FEditor.parentNode.insertBefore(FGroup, FEditor);
  FGroup.appendChild(FEditor);
  FPicker := TJSHTMLInputElement(document.createElement('input'));
  FPicker._type := 'color';
  FPicker.className := 'nyx-color-picker';
  FPicker.setAttribute('colorspace', 'limited-srgb');
  FPicker.value := '#000000';
  FGroup.appendChild(FPicker);
  FConnected := True;
  FPicker.addEventListener('click', @Opening);
  FPicker.addEventListener('change', @Commit);
end;

function TNyxBrowserColorField.Opening(AEvent: TJSMouseEvent): Boolean;
begin
  Result := FConnected and FEditable;

  if not Result then
  begin
    AEvent.preventDefault;
    Exit;
  end;
  FOpeningRevision := FRevision;
end;

function TNyxBrowserColorField.Commit(AEvent: TEventListenerEvent): Boolean;
var
  LValue: TNyxRGBColor;
  LCommit: TNyxBrowserColorCommit;
begin
  Result := True;

  if not FConnected or not FEditable or (FOpeningRevision <> FRevision) then
  begin
    Exit;
  end;
  LValue := TNyxRGBColor.FromText(TNyxText(FPicker.value));
  try
    FDomain.ReadWire(LValue.ToText);
  except
    on LException: ENyxContract do
    begin
      FEditor.setAttribute('aria-invalid', 'true');
      FEditor.title := LException.Message;
      Exit;
    end;
  end;
  LCommit := FCommit;
  FOpeningRevision := -1;
  { Application completion may retire the whole view/adapter. Locals own the
    copied value and callback; no borrowed editor or Self is touched afterward. }

  if Assigned(LCommit) then
  begin
    LCommit(LValue);
  end;
end;

procedure TNyxBrowserColorField.Sync(const AValue: TNyxRGBColor;
  const ADomain: TNyxValueDomain; AEnabled, AReadOnly, AVisible: Boolean;
  const AName: TNyxText);
var
  LDomain: TNyxValueDomain;
  LChanged: Boolean;
begin

  if not FConnected then
  begin
    Exit;
  end;
  LDomain := NyxRGBDomain(ADomain).Definition;
  LChanged := (LDomain.ToData.ToJSON <> FDomain.ToData.ToJSON) or
    (AValue.ToText <> FValue.ToText) or
    (FEditable <> (AEnabled and not AReadOnly and AVisible));

  if LChanged then
  begin
    Inc(FRevision);
    FOpeningRevision := -1;
  end;
  FDomain := LDomain;
  FValue := AValue;
  FEditable := AEnabled and not AReadOnly and AVisible;
  FPicker.disabled := not FEditable;
  FPicker.setAttribute('aria-label', 'Choose color for ' + AName);
  FGroup.setAttribute('aria-label', AName);

  if AValue.Defined then
  begin

    if FOpeningRevision <> FRevision then
    begin
      { An unchanged refresh must not reset a native chooser's proposal. The
        exact text editor and accepted model remain independently authoritative. }
      FPicker.value := NyxRGB(AValue.Red, AValue.Green, AValue.Blue).ToText;
    end;
    FGroup.setAttribute('data-nyx-color-defined', 'true');
  end
  else
  begin

    if FOpeningRevision <> FRevision then
    begin
      FPicker.value := '#000000';
    end;
    FGroup.setAttribute('data-nyx-color-defined', 'false');
  end;
  FPicker.title := 'Choose a color';

  if not AValue.Defined then
  begin
    FPicker.title := 'No color selected; choose a color';
  end;
end;

procedure TNyxBrowserColorField.Disconnect;
begin

  if not FConnected then
  begin
    Exit;
  end;
  FConnected := False;
  FCommit := nil;
  FPicker.removeEventListener('click', @Opening);
  FPicker.removeEventListener('change', @Commit);
  FPicker.disabled := True;
  FEditor := nil;
end;

destructor TNyxBrowserColorField.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

end.
