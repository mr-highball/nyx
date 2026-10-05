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

program nyx_declaration_lexical_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.controls, nyx.codegen, nyx.source
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LBase: TNyxText;
  LSource: TNyxText;
  LAfter: TNyxText;
  LDefinition: TNyxRoutineDeclaration;
  LRoutine: TNyxRoutineSource;
  LSite: TNyxRoutineDeclarationSource;
  LChecks: Integer;
  LRejected: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Declaration lexical: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure RefuseRemove(const ASource, AName, AReason: TNyxText);
var
  LOriginal: TNyxRoutineSource;
  LDeclaration: TNyxRoutineDeclarationSource;
  LFailed: Boolean;
begin
  LFailed := False;
  try
    LOriginal := ReadNyxRoutineSource(ASource, NyxRoutine(AName));
    LDeclaration := ReadNyxRoutineDeclaration(ASource, NyxRoutine(AName));
    RemoveNyxRoutineDeclaration(ASource, NyxRoutine(AName),
      LOriginal.Signature, LOriginal.Code, LDeclaration.Declaration);
  except
    on Exception do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, AReason);
end;

begin
  LDocument := nil;
  LPage := nil;
  try
    LDocument := TNyxDocument.Create;
    LPage := NewNyxPage('declaration-workshop');
    LDocument.AddPage(LPage);
    LBase := TNyxCodegen.Generate(LDocument);
    LDefinition := NyxRoutineDeclaration(nrFunction, NyxRoutine('LimitText'), rvInterface,
      'function LimitText(const AValue: TNyxText): Integer;',
      #10 + 'begin' + #10 + TNyxText('  { Qualification comment 🌙 }') + #10 +
      '  Result := NyxTextScalarCount(AValue);' + #10 + 'end;');
    LSource := AddNyxRoutineDeclaration(LBase, LDefinition);
    LSite := ReadNyxRoutineDeclaration(LSource, NyxRoutine('limittext'));
    Check((LSite.Visibility = rvInterface) and (LSite.Declaration = LDefinition.Signature) and
      (LSite.Line > 0), 'public creation owns one exact interface counterpart');
    LRoutine := ReadNyxRoutineSource(LSource, NyxRoutine('LimitText'));
    Check((LRoutine.Code = LDefinition.Code) and (LRoutine.Signature = LDefinition.Signature),
      'created implementation retains exact Unicode/signature');
    LAfter := RemoveNyxRoutineDeclaration(LSource, LRoutine.Routine,
      LRoutine.Signature, LRoutine.Code, LSite.Declaration);
    Check(Pos('LimitText', LAfter) = 0, 'paired removal retires interface and implementation together');
    LDefinition := NyxRoutineDeclaration(nrProcedure, NyxRoutine('PrivateNote'), rvImplementation,
      'procedure PrivateNote;', #10 + 'begin' + #10 + 'end;');
    LSource := AddNyxRoutineDeclaration(LBase, LDefinition);
    LSite := ReadNyxRoutineDeclaration(LSource, LDefinition.Routine);
    Check((LSite.Declaration = '') and (LSite.Line = 0) and
      (LSite.Visibility = rvImplementation), 'private creation has explicit empty counterpart');
    LRoutine := ReadNyxRoutineSource(LSource, LDefinition.Routine);
    LAfter := RemoveNyxRoutineDeclaration(LSource, LRoutine.Routine,
      LRoutine.Signature, LRoutine.Code, '');
    Check(Pos('PrivateNote', LAfter) = 0, 'private helper removal preserves ordinary source');
    LRejected := False;
    try
      AddNyxRoutineDeclaration(LSource, LDefinition);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'duplicate creation refuses without guessing visibility');
    LDefinition := NyxRoutineDeclaration(nrProcedure, NyxRoutine('Caller'), rvImplementation,
      'procedure Caller;', #10 + 'begin PrivateNote; end;');
    LSource := AddNyxRoutineDeclaration(LSource, LDefinition);
    RefuseRemove(LSource, 'PrivateNote', 'retained lexical caller blocks removal');
    LSource := AddNyxRoutineDeclaration(LBase,
      NyxRoutineDeclaration(nrProcedure, NyxRoutine('PrivateNote'), rvImplementation,
      'procedure PrivateNote;', #10 + 'begin end;'));
    LSource := StringReplace(LSource, NyxViewsBegin,
      '{$if Declared(PrivateNote)}' + #10 + 'const NotePresent = True;' + #10 +
      '{$endif}' + #10 + NyxViewsBegin, []);
    RefuseRemove(LSource, 'PrivateNote', 'retained compiler directive references block removal');
    LDefinition := NyxRoutineDeclaration(nrProcedure, NyxRoutine('SelfNote'), rvImplementation,
      'procedure SelfNote;', #10 + 'begin SelfNote; end;');
    LSource := AddNyxRoutineDeclaration(LBase, LDefinition);
    LRoutine := ReadNyxRoutineSource(LSource, LDefinition.Routine);
    LAfter := RemoveNyxRoutineDeclaration(LSource, LRoutine.Routine,
      LRoutine.Signature, LRoutine.Code, '');
    Check(Pos('SelfNote', LAfter) = 0, 'recursive calls inside retired ownership do not become retained callers');
    LSource := StringReplace(LBase, 'implementation',
      '// Kept beside the following section' + #10 + 'implementation', []);
    LDefinition := NyxRoutineDeclaration(nrFunction, NyxRoutine('Caption'), rvInterface,
      'function Caption: TNyxText;', #10 + 'begin Result := ''English caption''; end;');
    LAfter := AddNyxRoutineDeclaration(LSource, LDefinition);
    Check(Pos('// Kept beside the following section' + #10 + 'implementation', LAfter) > 0,
      'creation preserves leading section comments and adjacency');
    LSource := AddNyxRoutineDeclaration(LBase, LDefinition);
    LSource := StringReplace(LSource, LDefinition.Signature,
      'function Caption(AValue: Integer): TNyxText;', []);
    RefuseRemove(LSource, 'Caption', 'divergent interface signature ownership refuses');
    RefuseRemove(LBase, 'BuildNyxDocument', 'managed infrastructure cannot be removed');
    LRejected := False;
    try
      NyxRoutineDeclaration(nrFunction, NyxRoutine('Wrong'), rvInterface,
        'function Other: Integer;', #10 + 'begin Result := 1; end;');
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'typed identity must match signature source');
    LRejected := False;
    try
      NyxRoutineDeclaration(nrFunction, NyxRoutine('Wrong'), rvInterface,
        'function Wrong: Integer;', #10 + 'begin Result := 1; end;' +
        #10 + 'procedure Injected; begin end;');
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'sibling source injection refuses');
    LRejected := False;
    try
      NyxRoutineDeclaration(nrProcedure, NyxRoutine('TThing.Method'), rvInterface,
        'procedure TThing.Method;', #10 + 'begin end;');
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unit-helper creation never guesses class-member ownership');
    LSource := StringReplace(LBase, 'implementation', '{$ifdef A}' + #10 + 'implementation', []) +
      #10 + '{$endif}';
    LRejected := False;
    try
      AddNyxRoutineDeclaration(LSource, LDefinition);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'conditional insertion gap refuses');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-declaration-lexical', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' declaration lexical checks';
    {$else}
    WriteLn('PASS ', LChecks, ' declaration lexical checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-declaration-lexical', 'failed');
      document.body.textContent := LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  LPage := nil;
  LDocument.Free;
end.
