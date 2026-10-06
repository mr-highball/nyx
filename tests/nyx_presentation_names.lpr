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
program nyx_presentation_names;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.presentations, nyx.composition,
  nyx.generated.view
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LName: TNyxText;
  LIndex: Integer;
begin
  { Consume the actual compiled generated unit, independently of the source
    recognizer. Supplementary qualification names do not alter English demos. }
  LName := 'Wide:%=';
  for LIndex := 1 to 121 do
  begin
    LName := LName + NyxScalarText($1F319);
  end;
  LDocument := BuildNyxDocument;
  LRoot := nil;
  try

    if (LDocument.Presentations.Count <> 3) or
      (LDocument.Presentations.Reference(2).Name <> LName) then
    begin
      raise Exception.Create('Generated compilation changed the exact scalar-budget name');
    end;
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LRoot.ApplyViewport(390, 700, npfBrowser);

    if LRoot.Find('notes-editor').Prop('visible') <> 'false' then
    begin
      raise Exception.Create('Compiled Unicode reference failed its named condition');
    end;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS 2 compiled Unicode presentation checks';
    document.body.setAttribute('data-result', 'passed');
    document.body.setAttribute('data-checks', '2');
    {$else}
    WriteLn('PASS 2 compiled Unicode presentation checks');
    {$endif}
  finally
    LRoot.Free;
    LDocument.Free;
  end;
end.
