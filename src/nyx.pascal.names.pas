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
unit nyx.pascal.names;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text;

type
  { Distinct portable Pascal namespace. Names retain their authored case but
    compare case insensitively in compiler contracts. No host path or source
    text is retained; construction rejects non-ASCII and reserved namespaces. }
  TNyxPascalUnitRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;

{ Use the same generated-unit admission as source imports. This lower-level
  identity owner lets immutable project values and source workspaces share a
  type without an interface dependency cycle. }
function NyxPascalUnit(const AName: TNyxText): TNyxPascalUnitRef;

implementation

uses nyx.model, nyx.codegen;

function NyxPascalUnit(const AName: TNyxText): TNyxPascalUnitRef;
var
  LIndex: Integer;
begin
  for LIndex := 1 to Length(AName) do
  begin

    if not (AName[LIndex] in ['A'..'Z', 'a'..'z', '0'..'9', '_', '.']) then
    begin
      raise ENyxModel.Create('Pascal imports require an ASCII unit namespace');
    end;
  end;
  TNyxCodegen.AdmitUnitName(AName);
  Result.FName := AName;
end;

end.
