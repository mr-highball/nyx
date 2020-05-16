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
unit nyx.utils.observe;

{$mode delphi}

interface

uses
  Classes,
  SysUtils;

type

  TObserverArray = array of Pointer;

  { TNyxObservationHelper }
  (*
    the observation helper consolidates some of the
    code required for allowing observer with specialized event types
  *)
  TNyxObservationHelper = class(TObject)
  strict private
    FObserve : TStringList;
  private
  strict protected
  public

    (*
      returns a list of all observers of a particular event code
    *)
    function ObserversByEvent(const AEventCode : Integer) : TObserverArray;

    (*
      adds an observer of a specific event code and return the ID used
      for lookup
    *)
    function Observe(const AEventCode : Integer;
      const AObserver : Pointer) : String;

    (*
      removes a specific observer
    *)
    procedure RemoveByID(const AID : String);

    (*
      clears observer list
    *)
    procedure Clear;

    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TNyxObservationHelper }

function TNyxObservationHelper.ObserversByEvent(const AEventCode: Integer): TObserverArray;
var
  I, LVal: Integer;
begin
  SetLength(Result, 0);

  for I := 0 to Pred(FObserve.Count) do
  begin
    LVal := StrToInt(FObserve.ValueFromIndex[I]);

    //if we find a matching observer add to the result
    if LVal = AEventCode then
    begin
      SetLength(Result, Succ(Length(Result)));
      Result[High(Result)] := FObserve.Objects[I];
    end;
  end;
end;

function TNyxObservationHelper.Observe(const AEventCode: Integer;
  const AObserver: Pointer): String;
var
  LGUID: TGUID;
begin
  //use a guid as the ID
  CreateGUID(LGUID);
  Result := GUIDToString(LGUID);

  //add the observer as an "object"
  FObserve.AddObject(
    Result + '=' + IntToStr(AEventCode), //name=value
    TObject(AObserver) //cast to object for storage
  );
end;

procedure TNyxObservationHelper.RemoveByID(const AID: String);
var
  I: Integer;
begin
  I := FObserve.IndexOfName(AID);

  //remove if found
  if I >= 0 then
    FObserve.Delete(I);
end;

procedure TNyxObservationHelper.Clear;
begin
  FObserve.Clear;
end;

constructor TNyxObservationHelper.Create;
begin
  FObserve := TStringList.Create;
end;

destructor TNyxObservationHelper.Destroy;
begin
  Clear;
  FObserve.Free;
  inherited Destroy;
end;

end.

