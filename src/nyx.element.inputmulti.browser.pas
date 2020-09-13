{ nyx

  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit nyx.element.inputmulti.browser;

{$mode delphi}

interface

uses
  web,
  nyx.types,
  nyx.element.browser,
  nyx.element.InputMulti,
  Classes;

type

  { TNyxElementInputMultiBrowserImpl }
  (*
    base implementation for all browser nyx InputMultis
  *)
  TNyxElementInputMultiBrowserImpl = class(TNyxElementInputMultiBaseImpl, INyxElementBrowser, INyxElementInputMulti)
  strict private
    FBrowser : TNyxElementBrowserImpl;
    FLines : TStringList;
    FIgnoreKeyDown : Boolean;

    procedure LinesChange(Sender : TObject);
    procedure BeforeLinesChange(Sender : TObject);

    (*
      js handler for key down inside the input
    *)
    procedure BeforeChangeHandler(Event : TEventListenerEvent);

    (*
      js handler for when the text changes
    *)
    procedure ChangeHandler(Event : TEventListenerEvent);
  protected
    function GetBrowser: TNyxElementBrowserImpl;

    type

      { TBrowserElementComponent }
      (*
        component which handles the INyxElement contract
      *)
      TBrowserElementComponent = class(TNyxElementBrowserImpl)
      strict protected
        function DoCreateElement: TJSElement; override;
        function DoGetSelf: INyxElement; override;

        procedure DoInitializeCSS(const ACSSHelper: TObject); override;
      public
        Parent : TNyxElementInputMultiBrowserImpl;
      end;

  strict protected
    procedure DoSetText(const AValue: String); override;
    function DoGetText: String; override;

    function DoGetEnabled: Boolean; override;
    procedure DoSetEnabled(const AValue: Boolean); override;

    function DoGetVisible: Boolean; override;
    procedure DoSetVisible(const AValue: Boolean); override;

    function DoGetLines: TStrings; override;

    (*
      because we use composition, we need to redirect calls
    *)
    procedure DoUpdateHeight; override;
    procedure DoUpdateWidth; override;
    procedure DoUpdateMode; override;

    function DoGetContainer: INyxContainer; override;
    procedure DoSetContainer(const AValue: INyxContainer); override;

  public
    property BrowserElementImpl : TNyxElementBrowserImpl read GetBrowser implements INyxElementBrowser;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation
uses
  webwidget,
  nyx.utils.browser.css;

{ TNyxElementInputMultiBrowserImpl }

procedure TNyxElementInputMultiBrowserImpl.LinesChange(Sender: TObject);
begin
  //set here directly to avoid double property notifies
  FBrowser.JSElement.textContent := FLines.Text;
  DoPropertyNotify(puAfterUpdate, imLines);
end;

procedure TNyxElementInputMultiBrowserImpl.BeforeLinesChange(Sender: TObject);
begin
  DoPropertyNotify(puBeforeUpdate, imLines);
end;

procedure TNyxElementInputMultiBrowserImpl.BeforeChangeHandler(
  Event: TEventListenerEvent);
begin
  if not FIgnoreKeyDown then
  begin
    FIgnoreKeyDown := True;
    DoPropertyNotify(puBeforeUpdate, imLines);
  end;
end;

procedure TNyxElementInputMultiBrowserImpl.ChangeHandler(
  Event: TEventListenerEvent);
begin
  DoPropertyNotify(puAfterUpdate, imLines);
  FIgnoreKeyDown := False;
end;

function TNyxElementInputMultiBrowserImpl.GetBrowser: TNyxElementBrowserImpl;
begin
  Result := FBrowser;
end;

procedure TNyxElementInputMultiBrowserImpl.DoSetText(const AValue: String);
begin
  TJSHTMLTextAreaElement(FBrowser.JSElement).value := AValue;
end;

function TNyxElementInputMultiBrowserImpl.DoGetText: String;
begin
  Result := TJSHTMLTextAreaElement(FBrowser.JSElement).value;
end;

function TNyxElementInputMultiBrowserImpl.DoGetEnabled: Boolean;
begin
  Result := not TJSHTMLTextAreaElement(FBrowser.JSElement).disabled;
end;

procedure TNyxElementInputMultiBrowserImpl.DoSetEnabled(const AValue: Boolean);
begin
  TJSHTMLTextAreaElement(FBrowser.JSElement).disabled := not AValue;
end;

function TNyxElementInputMultiBrowserImpl.DoGetVisible: Boolean;
begin
  Result := FBrowser.Visible;
end;

procedure TNyxElementInputMultiBrowserImpl.DoSetVisible(const AValue: Boolean);
begin
  FBrowser.Visible := AValue;
end;

function TNyxElementInputMultiBrowserImpl.DoGetLines: TStrings;
begin
  FLines.OnChanging := nil;
  FLines.OnChange := nil;
  try
    FLines.Text := FBrowser.JSElement.textContent;
    Result := FLines;
  finally
    FLines.OnChanging := @BeforeLinesChange;
    FLines.OnChange := @LinesChange;
  end;
end;

procedure TNyxElementInputMultiBrowserImpl.DoUpdateHeight;
begin
  FBrowser.Size.UpdateHeight(Size.Height);
end;

procedure TNyxElementInputMultiBrowserImpl.DoUpdateWidth;
begin
  FBrowser.Size.UpdateWidth(Size.Width);
end;

procedure TNyxElementInputMultiBrowserImpl.DoUpdateMode;
begin
  FBrowser.Size.UpdateMode(Size.Mode);
end;

function TNyxElementInputMultiBrowserImpl.DoGetContainer: INyxContainer;
begin
  Result := FBrowser.Container;
end;

procedure TNyxElementInputMultiBrowserImpl.DoSetContainer(
  const AValue: INyxContainer);
begin
  FBrowser.Container := AValue;
end;

function TNyxElementInputMultiBrowserImpl.TBrowserElementComponent.DoCreateElement: TJSElement;
begin
  Result := document.createElement('textarea');
end;

function TNyxElementInputMultiBrowserImpl.TBrowserElementComponent.DoGetSelf: INyxElement;
begin
  Result := Parent as INyxElement;
end;

procedure TNyxElementInputMultiBrowserImpl.TBrowserElementComponent.DoInitializeCSS
  (const ACSSHelper: TObject);
begin
  inherited DoInitializeCSS(ACSSHelper);

  //default textarea seems to have resize specified, so to keep it
  //the same for standard, we'll disable (this can be enabled by caller if desired in css)
  TNyxCSSHelper(ACSSHelper).Upsert('resize', 'none');
end;

constructor TNyxElementInputMultiBrowserImpl.Create;
begin
  inherited Create;
  FIgnoreKeyDown := False; //init to false to capture the first key down
  FLines := TStringList.Create;
  FLines.OnChanging := @BeforeLinesChange;
  FLines.OnChange := @LinesChange;

  FBrowser := TBrowserElementComponent.Create;
  FBrowser.JSElement.addEventListener(sEventKeyDown, @BeforeChangeHandler);
  FBrowser.JSElement.addEventListener(sEventKeyUp, @ChangeHandler);
  TBrowserElementComponent(FBrowser).Parent := Self;

  BindEvents(FBrowser);
end;

destructor TNyxElementInputMultiBrowserImpl.Destroy;
begin
  FLines.Free;
  FBrowser.Free;
  inherited Destroy;
end;

end.

