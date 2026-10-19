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
unit interfaces.style.fixture;

{$interfaces corba}
{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { A mode declaration must retain the already authored CORBA contract. }
  IModeRetained = interface
    function ReadValue: LongInt;
  end;
  {$interfaces default}
  { Its RTTI observes this compiler invocation's actual initial default. }
  IInitialDefault = interface
  end;
  {$interfaces com}
  { COM implicitly supplies the compiler's IUnknown contract. }
  IComValue = interface
    ['{A5B7D2CC-5B93-497D-A1B4-CE409151615F}']
    function ReadValue: LongInt;
  end;
  {$push}
  {$interfaces corba}
  { Interface style is independent of the saved local-switch frame. }
  IWithinPush = interface
    function ReadValue: LongInt;
  end;
  {$pop}
  { POP must not restore the earlier COM style. }
  IAfterPop = interface
    function ReadValue: LongInt;
  end;
  {$interfaces default}
  { DEFAULT returns to the same invocation's initial style after local scopes. }
  IRestoredDefault = interface
  end;
  { No IUnknown implementation is present: these three real contracts are raw. }
  TCorbaValue = class(TObject, IModeRetained, IWithinPush, IAfterPop)
    Value: LongInt;
    function ReadValue: LongInt;
  end;
  { The real COM implementation supplies and executes its inherited refcounts. }
  TComValue = class(TInterfacedObject, IComValue)
    Value: LongInt;
    function ReadValue: LongInt;
  end;

{ Independent compiler observation: 1 is COM, 2 is CORBA, 0 is unsupported.
  This is an oracle value, not a Nyx enum or a fabricated profile default. }
function CompilerInitialInterfaceStyle: LongInt;
{ Qualify mode/stack/default rules and execute raw/COM calls to 42. }
function CompilerInterfaceStyleValue: LongInt;

implementation

uses TypInfo;

function TCorbaValue.ReadValue: LongInt;
begin
  Result := Value;
end;

function TComValue.ReadValue: LongInt;
begin
  Result := Value;
end;

function CompilerInitialInterfaceStyle: LongInt;
begin
  Result := 0;

  if PTypeInfo(TypeInfo(IInitialDefault))^.Kind = tkInterface then Result := 1;

  if PTypeInfo(TypeInfo(IInitialDefault))^.Kind = tkInterfaceRaw then Result := 2;
end;

function CompilerInterfaceStyleValue: LongInt;
var
  LRawObject: TCorbaValue;
  LComObject: TComValue;
  LRaw: IModeRetained;
  LAfterPop: IAfterPop;
  LCom: IComValue;
  LAdded: LongInt;
  LReleased: LongInt;
begin
  Result := -1;

  if (PTypeInfo(TypeInfo(IModeRetained))^.Kind <> tkInterfaceRaw) or
    (PTypeInfo(TypeInfo(IWithinPush))^.Kind <> tkInterfaceRaw) or
    (PTypeInfo(TypeInfo(IAfterPop))^.Kind <> tkInterfaceRaw) or
    (PTypeInfo(TypeInfo(IComValue))^.Kind <> tkInterface) or
    (PTypeInfo(TypeInfo(IRestoredDefault))^.Kind <> PTypeInfo(TypeInfo(IInitialDefault))^.Kind) or
    (CompilerInitialInterfaceStyle = 0) then Exit;
  LRawObject := TCorbaValue.Create;
  try
    LRawObject.Value := 1;
    LRaw := LRawObject;
    LAfterPop := LRawObject;
    LComObject := TComValue.Create;
    LComObject.Value := 41;
    LCom := LComObject;
    try
      LAdded := LCom._AddRef;
      LReleased := LCom._Release;

      if (LAdded <> 2) or (LReleased <> 1) then Exit;
      Result := LCom.ReadValue + LRaw.ReadValue;

      if LAfterPop.ReadValue <> 1 then Result := -2;
    finally
      LCom := nil;
    end;
  finally
    LAfterPop := nil;
    LRaw := nil;
    LRawObject.Free;
  end;
end;

end.
