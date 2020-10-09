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
unit nyx.element.statictext.browser;

{$mode delphi}

interface

uses
  web,
  nyx.types,
  nyx.element.browser,
  nyx.element.statictext;

type

  { TNyxElementStaticTextBrowserImpl }
  (*
    base implementation for all browser nyx StaticTexts
  *)
  TNyxElementStaticTextBrowserImpl = class(TNyxElementStaticTextBaseImpl, INyxElementBrowser, INyxElementStaticText)
  strict private
    FBrowser : TNyxElementBrowserImpl;
    FDisabled : Boolean;
    FFormat : TStaticTextFormats;

    procedure AdjustDisabledOpacity;
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
      public
        Parent : TNyxElementStaticTextBrowserImpl;
      end;

  strict protected
    procedure DoSetText(const AValue: String); override;
    function DoGetText: String; override;

    function DoGetEnabled: Boolean; override;
    procedure DoSetEnabled(const AValue: Boolean); override;

    function DoGetVisible: Boolean; override;
    procedure DoSetVisible(const AValue: Boolean); override;

    procedure DoSetFormat(const AValue: TStaticTextFormats); override;
    function DoGetFormat: TStaticTextFormats; override;

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
  nyx.utils.browser.css;

{ TNyxElementStaticTextBrowserImpl }

procedure TNyxElementStaticTextBrowserImpl.AdjustDisabledOpacity;
var
  LCSS: TNyxCSSHelper;
  LElement : INyxElementBrowser;
begin
  LCSS := TNyxCSSHelper.Create;
  try
    LElement := DoGetSelf as INyxElementBrowser;
    LCSS.CopyFromElement(LElement);

    if FDisabled then
      LCSS.Upsert('opacity', '0.6')
    else
      LCSS.Delete('opacity');

    LCSS.CopyToElement(LElement);
  finally
    LCSS.Free;
  end;
end;

function TNyxElementStaticTextBrowserImpl.GetBrowser: TNyxElementBrowserImpl;
begin
  Result := FBrowser;
end;

procedure TNyxElementStaticTextBrowserImpl.DoSetText(const AValue: String);
begin
  FBrowser.JSElement.innerText := AValue;
end;

function TNyxElementStaticTextBrowserImpl.DoGetText: String;
begin
  Result := FBrowser.JSElement.innerText;
end;

function TNyxElementStaticTextBrowserImpl.DoGetEnabled: Boolean;
begin
  //todo - paragraph tag doesn't have disabled in browser? using a field instead
  //       but what should be done to element then? remove event?
  Result := FDisabled;
end;

procedure TNyxElementStaticTextBrowserImpl.DoSetEnabled(const AValue: Boolean);
begin
  FDisabled := AValue;
  AdjustDisabledOpacity;
end;

function TNyxElementStaticTextBrowserImpl.DoGetVisible: Boolean;
begin
  Result := FBrowser.Visible;
end;

procedure TNyxElementStaticTextBrowserImpl.DoSetVisible(const AValue: Boolean);
begin
  FBrowser.Visible := AValue;
end;

procedure TNyxElementStaticTextBrowserImpl.DoSetFormat(
  const AValue: TStaticTextFormats);
var
  LTextDec : String = '';
  LFontWeight : String = 'normal';
  LFontStyle : String = 'normal';
  LCSS : TNyxCSSHelper;
  LElement: INyxElementBrowser;
begin
  LCSS := TNyxCSSHelper.Create;
  LElement := DoGetSelf as INyxElementBrowser;
  try
    LCSS.CopyFromElement(LElement);
    FFormat := AValue;

    if sfBold in FFormat then
      LFontWeight := 'bold';

    if sfItalic in FFormat then
      LFontStyle := 'italic';

    if sfStrikeThrough in FFormat then
      LTextDec := 'line-through';

    if sfUnderline in FFormat then
      LTextDec := LTextDec + ' underline';

    LCSS.Upsert('text-decoration', LTextDec);
    LCSS.Upsert('font-weight', LFontWeight);
    LCSS.Upsert('font-style', LFontStyle);

    LCSS.CopyToElement(LElement);
  finally
    LCSS.Free;
  end;
end;

function TNyxElementStaticTextBrowserImpl.DoGetFormat: TStaticTextFormats;
begin
  Result := FFormat;
end;

procedure TNyxElementStaticTextBrowserImpl.DoUpdateHeight;
begin
  FBrowser.Size.UpdateHeight(Size.Height);
end;

procedure TNyxElementStaticTextBrowserImpl.DoUpdateWidth;
begin
  FBrowser.Size.UpdateWidth(Size.Width);
end;

procedure TNyxElementStaticTextBrowserImpl.DoUpdateMode;
begin
  FBrowser.Size.UpdateMode(Size.Mode);
end;

function TNyxElementStaticTextBrowserImpl.DoGetContainer: INyxContainer;
begin
  Result := FBrowser.Container;
end;

procedure TNyxElementStaticTextBrowserImpl.DoSetContainer(
  const AValue: INyxContainer);
begin
  FBrowser.Container := AValue;
end;

function TNyxElementStaticTextBrowserImpl.TBrowserElementComponent.DoCreateElement: TJSElement;
begin
  Result := document.createElement('p');
end;

function TNyxElementStaticTextBrowserImpl.TBrowserElementComponent.DoGetSelf: INyxElement;
begin
  Result := Parent as INyxElement;
end;

constructor TNyxElementStaticTextBrowserImpl.Create;
begin
  inherited Create;
  FBrowser := TBrowserElementComponent.Create;
  TBrowserElementComponent(FBrowser).Parent := Self;
  FDisabled := False;
  FFormat := [];
  BindEvents(FBrowser);
end;

destructor TNyxElementStaticTextBrowserImpl.Destroy;
begin
  FBrowser.Free;
  inherited Destroy;
end;

end.

