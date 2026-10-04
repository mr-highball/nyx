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
unit nyx.test.keyboard.browser;

{$mode delphi}{$H+}
{$codepage utf8}
{$modeswitch externalclass}

interface

uses
  JS,
  Web,
  nyx.text,
  nyx.types;

{ Creates an owned platform event for real DOM-control dispatch. Browser key
  names are deliberately admitted at this test bridge, not application behavior.
  Synthetic events prove listener routing/cancellation; they do not claim trusted
  OS input or browser-generated default clicks. }
function NyxTestKeyboard(ATrigger: TNyxTrigger; const AKey: TNyxText;
  AModifiers: TNyxKeyModifiers = []; ARepeating: Boolean = False;
  AComposing: Boolean = False): TJSKeyboardEvent;

implementation

type
  { Older Web declarations expose only EventInit. The standard KeyboardEvent
    constructor accepts a dictionary; keep that missing binding Pascal-only. }
  TNyxTestKeyboardEvent = class external name 'KeyboardEvent' (TJSKeyboardEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;

function NyxTestKeyboard(ATrigger: TNyxTrigger; const AKey: TNyxText;
  AModifiers: TNyxKeyModifiers; ARepeating, AComposing: Boolean): TJSKeyboardEvent;
var
  LOptions: TJSObject;
  LName: String;
begin
  LName := 'keydown';

  if ATrigger = ntKeyUp then
  begin
    LName := 'keyup';
  end;
  LOptions := TJSObject.new;
  LOptions['key'] := AKey;
  LOptions['ctrlKey'] := nmControl in AModifiers;
  LOptions['altKey'] := nmAlt in AModifiers;
  LOptions['shiftKey'] := nmShift in AModifiers;
  LOptions['metaKey'] := nmMeta in AModifiers;
  LOptions['repeat'] := ARepeating;
  LOptions['isComposing'] := AComposing;
  LOptions['bubbles'] := True;
  LOptions['cancelable'] := True;
  Result := TNyxTestKeyboardEvent.new(LName, LOptions);
end;

end.
