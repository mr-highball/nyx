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

unit nyx.test.files;

{$mode delphi}{$H+}{$codepage utf8}

interface

function RunNyxTextFileTests: Integer;

implementation

uses SysUtils, nyx.text, nyx.bytes, nyx.files, nyx.model, nyx.codec, nyx.codegen,
  nyx.studio.files, nyx.studio.projects;

function RunNyxTextFileTests: Integer;
const
  CExactText: TNyxText = 'Exact text / 😀 / 漢字' + #0 + #10;
  CExactName: TNyxText = 'notes-🌙.txt';
var
  LChecks: Integer;
  LFile: INyxTextFile;
  LBytes: TNyxBytes;
  LSelection: TNyxTextFileSelection;
  LExtended: TNyxTextFileSelection;
  LFiles: TNyxTextFiles;
  LPair: TNyxProjectPair;
  LDocument: TNyxDocument;
  LIndex: Integer;
  LRejected: Boolean;
  LUnused: TNyxText;
  LNames: array[0..4] of TNyxText;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Text files: ' + AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LSelection := NyxTextFileSelection.Allow('nyx');
  LExtended := LSelection.Allow('PAS').UpToFiles(2);
  Check((LSelection.ExtensionCount = 1) and not LSelection.Accepts('source.pas'),
    'Extending a copied selection never changes its origin on either compiler');
  Check(LExtended.Accepts('source.PAS') and (LExtended.MaximumFiles = 2),
    'Closed limits and case-insensitive extension matching retain exact filename text');
  LBytes := NyxEncodeUTF8(CExactText);
  LFile := NyxTextFileBytes(CExactName, LBytes);
  LBytes[0] := 0;
  Check((LFile.Name = CExactName) and
    (LFile.Text = CExactText),
    'Immutable file retains supplementary Unicode and NUL independently of borrowed bytes');
  Check(LFile.ByteCount = NyxUTF8ByteCount(LFile.Text),
    'Byte budget counts UTF-8 interchange rather than native/browser storage units');
  LFile := NyxTextFile('empty.txt', '');
  Check((LFile.Text = '') and (LFile.ByteCount = 0), 'An empty text file is distinct from no payload');
  LNames[0] := '';
  LNames[1] := '../file.txt';
  LNames[2] := 'bad:name.txt';
  LNames[3] := 'ending.';
  LNames[4] := 'bad' + #0 + '.txt';
  for LIndex := 0 to High(LNames) do
  begin
    LRejected := False;
    try
      LFile := NyxTextFile(LNames[LIndex], 'Retained input');
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'Invalid portable leaf refuses before immutable publication');
  end;
  SetLength(LBytes, 2);
  LBytes[0] := $C0;
  LBytes[1] := $80;
  LRejected := False;
  try
    LFile := NyxTextFileBytes('invalid.txt', LBytes);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Malformed UTF-8 never becomes browser replacement characters');

  LDocument := TNyxDocument.Create;
  try
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
  finally
    LDocument.Free;
  end;
  LPair.Pending := True;
  LPair.Draft := 'An independent unfinished idea / 😀';
  LPair.DraftBase := 'An exact saved baseline / 🚀';
  LFiles := NyxStudioProjectBackup(LPair);
  Check(ReadNyxStudioProjectFiles(LFiles) = EncodeNyxProject(LPair),
    'Project backup round trip retains accepted pair and independent draft/base');
  LFiles := NyxStudioProjectCompanion(LPair);
  Check((LFiles[0].Text = LPair.Design) and (LFiles[1].Text = LPair.Source),
    'Adjacent export contains accepted files without substituting unfinished input');
  LFile := LFiles[0];
  LFiles[0] := LFiles[1];
  LFiles[1] := LFile;
  Check(ReadNyxStudioProjectFiles(LFiles) =
    EncodeNyxProject(NyxProjectPair(LPair.Design, LPair.Source)),
    'Reversed paired-file selection reconstructs one exact accepted packet');
  LFiles[1] := LFiles[0];
  LRejected := False;
  try
    LUnused := ReadNyxStudioProjectFiles(LFiles);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Duplicate Pascal membership cannot silently replace missing design');
  LFiles := nil;
  SetLength(LFiles, 1);
  LFiles[0] := NyxTextFile('retained.nyxproject', 'Malformed input is retained exactly');
  Check(ReadNyxStudioProjectFiles(LFiles) = LFiles[0].Text,
    'Complete malformed backup remains available for explicit admission/refusal review');
  Result := LChecks;
end;

end.
