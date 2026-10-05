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
unit nyx.root.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text;

type
  ENyxRoot = class(Exception);
  { Root placement belongs to the document, independently of a control kind.
    A reusable definition may itself be a column, card or custom control. }
  TNyxRootKind = (nrPage, nrReusable);

  { Immutable, exact, case-sensitive identity. A root reference cannot silently
    denote a descendant or another root partition. Names are open Unicode data,
    never a Pascal identifier or renderer address. Default records are absent;
    reading their identity refuses instead of selecting the first page. }
  TNyxRootRef = record
  private
    FKind: TNyxRootKind;
    FName: TNyxText;
    FAssigned: Boolean;
    function GetName: TNyxText;
    function GetKind: TNyxRootKind;
  public
    property Name: TNyxText read GetName;
    property Kind: TNyxRootKind read GetKind;
    property Assigned: Boolean read FAssigned;
  end;

function NyxRoot(AKind: TNyxRootKind; const AName: TNyxText): TNyxRootRef;
function NyxPageRoot(const AName: TNyxText): TNyxRootRef;
function NyxReusableRoot(const AName: TNyxText): TNyxRootRef;
{ Closed wire spelling is used only at persistence/editor transport boundaries. }
function NyxRootKindName(AKind: TNyxRootKind): TNyxText;

implementation

function TNyxRootRef.GetName: TNyxText;
begin

  if not FAssigned then
  begin
    raise ENyxRoot.Create('Construct a page or reusable root reference first');
  end;
  Result := FName;
end;

function TNyxRootRef.GetKind: TNyxRootKind;
begin
  GetName;
  Result := FKind;
end;

function NyxRoot(AKind: TNyxRootKind; const AName: TNyxText): TNyxRootRef;
var
  LIndex: Integer;
  LScalar: Integer;
begin

  if not (AKind in [nrPage, nrReusable]) or (AName = '') then
  begin
    raise ENyxRoot.Create('A root reference requires a closed kind and nonempty name');
  end;
  LIndex := 1;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) then
    begin
      raise ENyxRoot.Create('Malformed Unicode in root reference');
    end;
  end;
  Result := Default(TNyxRootRef);
  Result.FKind := AKind;
  Result.FName := AName;
  Result.FAssigned := True;
end;

function NyxPageRoot(const AName: TNyxText): TNyxRootRef;
begin
  Result := NyxRoot(nrPage, AName);
end;

function NyxReusableRoot(const AName: TNyxText): TNyxRootRef;
begin
  Result := NyxRoot(nrReusable, AName);
end;

function NyxRootKindName(AKind: TNyxRootKind): TNyxText;
begin
  { Validate the ordinal at this boundary as well as declared enum members:
    explicit casts/foreign bridges must still reach the rejection branch. }
  case Ord(AKind) of
    Ord(nrPage): Result := 'page';
    Ord(nrReusable): Result := 'component';
    else
      raise ENyxRoot.Create('Unknown root kind');
  end;
end;

end.
