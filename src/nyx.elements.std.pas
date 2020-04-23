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
unit nyx.elements.std;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  fgl,
  nyx.types;

type

  { TNyxElementsStdImpl }
  (*
    standard elements collection implementation
  *)
  TNyxElementsStdImpl = class(TNyxElementsBaseImpl)
  strict private
    FItems : TFPGInterfacedObjectList<INyxElement>;
  strict protected
    function DoGetCount : Integer; override;

    function DoGetItem(const AIndex : Integer; out Item : INyxElement;
      out Error : String) : Boolean; override;

    function DoAddAtem(const AItem : INyxElement; out Index : Integer;
      out Error : String) : Boolean; override;

    function DoRemoveItem(const AIndex : Integer; out Item : INyxElement;
      out Error : String) : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TNyxElementsStdImpl }

function TNyxElementsStdImpl.DoGetCount: Integer;
begin
  Result := FItems.Count;
end;

function TNyxElementsStdImpl.DoGetItem(const AIndex: Integer; out
  Item: INyxElement; out Error: String): Boolean;
begin
  Result := False;
  Item := nil;
  try
    Item := FItems[AIndex];

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

function TNyxElementsStdImpl.DoAddAtem(const AItem: INyxElement; out
  Index: Integer; out Error: String): Boolean;
begin
  Result := False;
  Index := -1;
  try
    Index := FItems.Add(AItem);

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

function TNyxElementsStdImpl.DoRemoveItem(const AIndex: Integer; out
  Item: INyxElement; out Error: String): Boolean;
begin
  Result := False;
  Item := nil;
  try
    Item := FItems[AIndex];
    FItems.Delete(AIndex);

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

constructor TNyxElementsStdImpl.Create;
begin
  inherited Create;
  FItems := TFPGInterfacedObjectList<INyxElement>.Create;
end;

destructor TNyxElementsStdImpl.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

end.

