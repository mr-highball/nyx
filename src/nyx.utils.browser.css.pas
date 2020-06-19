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
unit nyx.utils.browser.css;

{$mode delphi}

interface

uses
  Classes,
  SysUtils;

type

  { TNyxCSSHelper }

  TNyxCSSHelper = class
  strict private
    FList : TStringList;
    function GetCSS: String;
    procedure SetCSS(const AValue: String);
  strict protected
  public
    (*
      the css string being manipulated. this will be modified
      when CRUD methods are called
    *)
    property CSS : String read GetCSS write SetCSS;

    (*
      updates or inserts a new css attribute
    *)
    procedure Upsert(const AAttribute, AValue : String);

    (*
      returns true if the attribute currently exists in the css string
    *)
    function Exists(const AAttribute : String) : Boolean;

    (*
      removes an attribute from the css string if it exists
    *)
    procedure Delete(const AAttribute : String);

    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TNyxCSSHelper }

function TNyxCSSHelper.GetCSS: String;
begin
  Result := FList.DelimitedText;
end;

procedure TNyxCSSHelper.SetCSS(const AValue: String);
begin
  FList.DelimitedText := AValue;
end;

procedure TNyxCSSHelper.Upsert(const AAttribute, AValue: String);
begin
  if Trim(AAttribute) = '' then
    Exit;

  //if the attribute exists we either delete in case of an empty value
  //or update the current value to the new one
  if Exists(AAttribute) then
  begin
    if Trim(AValue) = '' then
      Delete(AAttribute) //call delete in case future events etc...
    else
      FList.Values[AAttribute] := AValue;
  end
  //otherwise we had if the value isn't empty
  else
    if Trim(AValue) <> '' then
      FList.AddPair(AAttribute, AValue);
end;

function TNyxCSSHelper.Exists(const AAttribute: String): Boolean;
begin
  Result := FList.IndexOfName(AAttribute) >= 0;
end;

procedure TNyxCSSHelper.Delete(const AAttribute: String);
begin
  if not Exists(AAttribute) then
    Exit;

  FList.Delete(FList.IndexOfName(AAttribute));
end;

constructor TNyxCSSHelper.Create;
begin
  FList := TStringList.Create;
  FList.NameValueSeparator := ':'; //css pair delimiter
  FList.Delimiter := ';'; //css attribute delimiter
end;

destructor TNyxCSSHelper.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

end.

