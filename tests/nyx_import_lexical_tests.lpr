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


program nyx_import_lexical_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses
  SysUtils, nyx.text, nyx.source
  {$ifdef PAS2JS}, Web{$endif};

var
  LChecks: Integer;
  LSource: TNyxText;
  LEdited: TNyxText;
  LClause: TNyxImportClause;
  LIndex: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

procedure Refuse(const AText: TNyxText);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    ReadNyxImports(AText, nisInterface);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Ambiguous/invalid import clause refuses');
end;

begin
  try
    LSource := 'unit review.imports;' + #10 + '{$mode delphi}{$H+}{$codepage utf8}' + #10 +
      'interface' + #10 + 'uses' + #10 +
      '  Alpha {first 🌙}, Beta (*middle*), Nyx {part}.Text // last 🦉' + #10 +
      '  ;' + #10 + 'const Note = ''implementation uses Wrong;'';' + #10 +
      'implementation' + #10 + '// retained uses Wrong;' + #10 + 'end.';
    LClause := ReadNyxImports(LSource, nisInterface);
    Check((LClause.Count = 3) and (LClause.UnitAt(2).Name = 'Nyx.Text'), 'Qualified namespace across normal comments');
    Check(LClause.LineAt(0) = 5, 'Exact one-based source line');
    LEdited := EditNyxImport(LSource, nisInterface, niaAdd, NyxPascalUnit('Math'));
    Check((Pos('// last 🦉', LEdited) > 0) and (ReadNyxImports(LEdited, nisInterface).Count = 4),
      'Append retains trailing line comment and authored order');
    for LIndex := 0 to 2 do
    begin
      LEdited := EditNyxImport(LSource, nisInterface, niaRemove, LClause.UnitAt(LIndex));
      Check((ReadNyxImports(LEdited, nisInterface).Count = 2) and
        (Pos('{first 🌙}', LEdited) > 0) and (Pos('(*middle*)', LEdited) > 0) and
        (Pos('{part}', LEdited) > 0) and (Pos('// last 🦉', LEdited) > 0),
        'First/middle/last removal preserves all comments including namespace parts');
    end;
    LEdited := EditNyxImport(LSource, nisImplementation, niaAdd, NyxPascalUnit('Math'));
    Check((ReadNyxImports(LEdited, nisImplementation).Count = 1) and
      (ReadNyxImports(LEdited, nisInterface).Count = 3), 'Create independent absent implementation clause');
    LEdited := EditNyxImport(LEdited, nisImplementation, niaRemove, NyxPascalUnit('mAtH'));
    Check((ReadNyxImports(LEdited, nisImplementation).Count = 0) and
      (Pos('// retained uses Wrong;', LEdited) > 0), 'Last removal retains section comments and compiler casing');

    Refuse('unit x; {$mode delphi}{$codepage utf8} interface uses A, a; implementation end.');
    Refuse('unit x; {$mode delphi}{$codepage utf8} interface uses A in ''path.pas''; implementation end.');
    Refuse('unit x; {$mode delphi}{$codepage utf8} interface uses A {$ifdef FPC}, B {$endif}; implementation end.');
    Refuse('unit x; {$mode delphi}{$codepage utf8} {$ifdef FPC} interface uses A; {$endif} implementation end.');
    Refuse('unit x; {$mode delphi}{$codepage utf8} interface (*$ifdef FPC*) uses A; (*$endif*) implementation end.');
    Refuse('unit x; {$mode delphi}{$codepage utf8} interface uses A,; implementation end.');
    Refuse('unit x; {$mode delphi}{$codepage utf8} interface uses A implementation end.');
    Refuse('unit x; {$mode delphi}{$codepage utf8} interface uses &type; implementation end.');
    LEdited := EditNyxImport(LSource, nisInterface, niaRemove, NyxPascalUnit('Alpha'));
    LEdited := EditNyxImport(LEdited, nisInterface, niaRemove, NyxPascalUnit('Beta'));
    LEdited := EditNyxImport(LEdited, nisInterface, niaRemove, NyxPascalUnit('nyx.text'));
    Check((ReadNyxImports(LEdited, nisInterface).Count = 0) and
      (Pos('{part}', LEdited) > 0), 'Removing complete clause retains every comment');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-import-lexical', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' lexical import checks';
    {$else}
    WriteLn('PASS ', LChecks, ' lexical import checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-import-lexical', 'failed');
      document.body.textContent := LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
