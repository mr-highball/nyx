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
unit nyx.elements.browser;

{$mode delphi}

interface

uses
  SysUtils,
  nyx.types;

type

  { TNyxElementsBrowserImpl }
  (*
    elements implementation for the browser

    note:
      as of writing, pas2js does not have support for array of interface
      or any of the generic lists which handle the reference counting, so
      this is why one was handrolled. it's meant to simple... so perhaps not
      the most optmized for performance
  *)
  TNyxElementsBrowserImpl = class(TNyxElementsBaseImpl)
  strict private
    FItems : array of Pointer;
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
  end;

implementation

{ TNyxElementsBrowserImpl }

function TNyxElementsBrowserImpl.DoGetCount: Integer;
begin
  Result := Length(FItems);
end;

function TNyxElementsBrowserImpl.DoGetItem(const AIndex: Integer; out
  Item: INyxElement; out Error: String): Boolean;
begin
  Result := False;
  Item := nil;
  try
    //range check
    if (AIndex < 0) or (AIndex > High(FItems)) then
    begin
      Error := Format('%d is out of bounds', [AIndex]);
      Exit;
    end;

    //cast the item to an element which will increment the ref count
    Item := INyxElement(Pointer(FItems[AIndex]));
    Item._AddRef;

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

function TNyxElementsBrowserImpl.DoAddAtem(const AItem: INyxElement; out
  Index: Integer; out Error: String): Boolean;
begin
  Result := False;
  Index := -1;
  try
    //for now don't add unassigned elements, but this might change if
    //a reason comes up
    if not Assigned(AItem) then
    begin
      Error := 'AItem is nil';
      Exit;
    end;

    //manually increment the reference counter
    AItem._AddRef;

    //add the element and record the index
    SetLength(FItems, Succ(Length(FItems)));
    FItems[High(FItems)] := Pointer(AItem);
    Index := High(FItems);

    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

function TNyxElementsBrowserImpl.DoRemoveItem(const AIndex: Integer; out
  Item: INyxElement; out Error: String): Boolean;
var
  LTmp: Pointer;
begin
  Result := False;
  Item := nil;
  try
    //first get the item
    Item := GetItem(AIndex);

    //decrement ref
    INyxElement(FItems[AIndex])._Release;

    //now perform the remove
    if AIndex <> High(FItems) then
    begin
      //swap positions with last item
      LTmp := FItems[High(FItems)];
      FItems[High(FItems)] := FItems[AIndex];
      FItems[AIndex] := LTmp;
    end;

    //drop the end
    SetLength(FItems, Pred(Length(FItems)));

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

constructor TNyxElementsBrowserImpl.Create;
begin
  SetLength(FItems, 0);
  inherited Create;
end;

end.

