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
unit nyx.objects;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Reference-counted base with Delphi construction lifetime on both targets.
    The native RTL already protects temporary Self interface leases before a
    constructor returns. pas2js acquires an equivalent internal reference on the
    first such lease, even when a descendant omits inherited Create. Successful
    most-derived construction retires only that reference; real interface owners
    keep their normal counts and raw class callers still own their returned
    object. Failed construction uses the compiler's ordinary destruction path.
    Descendants overriding AfterConstruction must call the inherited method.
    This guard does not make an escaped, unsuccessfully constructed object valid. }
  TNyxInterfacedObject = class(TInterfacedObject)
  {$ifdef PAS2JS}
  private
    FConstructed: Boolean;
    FConstructionReference: Boolean;
  protected
    function _AddRef: Integer; override;
  public
    procedure AfterConstruction; override;
  {$endif}
  end;

implementation

{$ifdef PAS2JS}
function TNyxInterfacedObject._AddRef: Integer;
begin

  if not FConstructed and not FConstructionReference then
  begin
    { Virtual interface entry reaches this guard without an inherited
      constructor. Pin only when construction actually borrows an interface. }
    FConstructionReference := True;
    inherited _AddRef;
  end;
  Result := inherited _AddRef;
end;

procedure TNyxInterfacedObject.AfterConstruction;
begin
  inherited AfterConstruction;
  FConstructed := True;

  if FConstructionReference then
  begin
    FConstructionReference := False;
    { Match native RTL construction: decrement without _Release so a raw class
      return with no external owner is still alive for its caller to own/free. }
    Dec(fRefCount);
  end;
end;
{$endif}

end.
