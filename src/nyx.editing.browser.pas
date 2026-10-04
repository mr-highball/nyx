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

unit nyx.editing.browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

interface

uses JS, Web, SysUtils, nyx.text, nyx.editing;

type
  { Correct typed DOM bindings. The installed pas2js RTL exposes InputType as
    "input"; the standard member is inputType. Keep that correction here without
    editing dependency source. Nullable data retains absent versus empty text. }
  TNyxDOMInputEvent = class external name 'InputEvent' (TJSUIEvent)
  public
    inputType: String;
    data: JSValue;
    isComposing: Boolean;
  end;
  TNyxDOMCompositionEvent = class external name 'CompositionEvent' (TJSUIEvent)
  public
    data: String;
  end;

function NyxBrowserInputText(AInput: TJSHTMLElement): TNyxText;
function CaptureNyxBrowserSelection(AInput: TJSHTMLElement): TNyxTextSelection;
function CaptureNyxBrowserEditing(AInput: TJSHTMLElement; APhase: TNyxEditingPhase;
  AEvent: TEventListenerEvent; AComposing: Boolean): TNyxEditingSnapshot;
{ Select admitted scalar boundaries without focusing, scrolling or writing text.
  Missing selection support is diagnosed, never approximated as a caret. }
procedure SelectNyxBrowserText(AInput: TJSHTMLElement;
  const ASelection: TNyxTextSelection);

implementation

type
  TNyxDOMTextControl = class external name 'HTMLElement' (TJSHTMLElement)
  public
    value: String;
    selectionStart: JSValue;
    selectionEnd: JSValue;
    selectionDirection: String;
    procedure setSelectionRange(AStart, AFinish: NativeInt; const ADirection: String);
  end;

function NyxBrowserInputText(AInput: TJSHTMLElement): TNyxText;
begin

  if not (AInput is TJSHTMLInputElement) and not (AInput is TJSHTMLTextAreaElement) then
  begin
    raise EArgumentException.Create('Editing requires a mounted text input');
  end;
  Result := TNyxDOMTextControl(AInput).value;
end;

function CaptureNyxBrowserSelection(AInput: TJSHTMLElement): TNyxTextSelection;
var
  LControl: TNyxDOMTextControl;
  LDirection: TNyxTextDirection;
begin
  Result := Default(TNyxTextSelection);

  if AInput = nil then
  begin
    Exit;
  end;
  LControl := TNyxDOMTextControl(AInput);

  if not isInteger(LControl.selectionStart) or not isInteger(LControl.selectionEnd) then
  begin
    Exit;
  end;
  LDirection := ntdUnknown;
  case LControl.selectionDirection of
    'none':
      begin
        LDirection := ntdNone;
      end;
    'forward':
      begin
        LDirection := ntdForward;
      end;
    'backward':
      begin
        LDirection := ntdBackward;
      end;
  end;
  try
    Result := NyxTextSelectionUTF16(NyxBrowserInputText(AInput),
      NativeInt(LControl.selectionStart), NativeInt(LControl.selectionEnd), LDirection);
  except
    on LException: EArgumentException do
    begin
      { A browser API can deliberately place a caret inside a surrogate. That
        cannot be represented as a scalar range; retain the edit but explicitly
        leave selection undefined rather than moving a user's caret. }
      Result := Default(TNyxTextSelection);
    end;
  end;
end;

function CaptureNyxBrowserEditing(AInput: TJSHTMLElement; APhase: TNyxEditingPhase;
  AEvent: TEventListenerEvent; AComposing: Boolean): TNyxEditingSnapshot;
var
  LIntent: TNyxEditIntent;
  LWire: TNyxText;
  LData: TNyxText;
  LHasData: Boolean;
  LCanCancel: Boolean;
begin
  LIntent := neiUnknown;
  LWire := '';
  LData := '';
  LHasData := False;
  LCanCancel := False;

  if AEvent is TNyxDOMInputEvent then
  begin
    LWire := TNyxDOMInputEvent(AEvent).inputType;
    TryNyxEditIntent(LWire, LIntent);
    LHasData := isString(TNyxDOMInputEvent(AEvent).data);

    if LHasData then
    begin
      LData := String(TNyxDOMInputEvent(AEvent).data);
    end;
    AComposing := AComposing or TNyxDOMInputEvent(AEvent).isComposing;
  end
  else if AEvent is TNyxDOMCompositionEvent then
  begin
    LData := TNyxDOMCompositionEvent(AEvent).data;
    LHasData := True;
    LIntent := neiInsertCompositionText;
  end;

  if (AEvent <> nil) and (APhase = nepBeforeEdit) then
  begin
    LCanCancel := AEvent.cancelable and not AComposing;
  end;
  Result := NyxEditingSnapshot(APhase, LIntent, NyxBrowserInputText(AInput),
    LData, LHasData, CaptureNyxBrowserSelection(AInput), AComposing, LCanCancel, LWire);
end;

procedure SelectNyxBrowserText(AInput: TJSHTMLElement;
  const ASelection: TNyxTextSelection);
var
  LText: TNyxText;
  LDirection: String;
begin

  if not ASelection.Defined or not CaptureNyxBrowserSelection(AInput).Defined then
  begin
    raise EArgumentException.Create('The mounted input has no scalar selection support');
  end;
  LText := NyxBrowserInputText(AInput);
  { Re-admit against this physical value before any platform mutation. }
  NyxTextSelection(LText, ASelection.Start, ASelection.Finish, ASelection.Direction);
  LDirection := 'none';

  if ASelection.Direction = ntdForward then
  begin
    LDirection := 'forward';
  end
  else if ASelection.Direction = ntdBackward then
  begin
    LDirection := 'backward';
  end;
  TNyxDOMTextControl(AInput).setSelectionRange(
    NyxTextUTF16Offset(LText, ASelection.Start),
    NyxTextUTF16Offset(LText, ASelection.Finish), LDirection);
end;

end.

