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

program nyx_routine_lexical_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.controls, nyx.codegen, nyx.source
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LBase: TNyxText;
  LSource: TNyxText;
  LCode: TNyxText;
  LAfter: TNyxText;
  LCatalog: TNyxRoutineCatalog;
  LRoutine: TNyxRoutineSource;
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Routine lexical: ' + AReason);
  end;
  Inc(LChecks);
end;

function WithHelpers(const AHelpers: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(NyxViewsBegin, LBase);
  Result := Copy(LBase, 1, LPosition - 1) + AHelpers +
    Copy(LBase, LPosition, MaxInt - LPosition);
end;

procedure Refuse(const ASource, AName, ACode: TNyxText; const AReason: TNyxText);
var
  LRejected: Boolean;
  LOriginal: TNyxRoutineSource;
begin
  LRejected := False;
  try
    LOriginal := ReadNyxRoutineSource(ASource, NyxRoutine(AName));
    ReplaceNyxRoutineImplementation(ASource, NyxRoutine(AName), LOriginal.Code, ACode);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AReason);
end;

begin
  LDocument := nil;
  LPage := nil;
  try
    LDocument := TNyxDocument.Create;
    LPage := NewNyxPage('lexical-workshop');
    LDocument.AddPage(LPage);
    LBase := TNyxCodegen.Generate(LDocument);
    LSource := WithHelpers(
      'type TLocal = class' + #10 + 'public' + #10 +
      '  procedure DeclarationOnly;' + #10 + 'end;' + #10 +
      'type TProcedureValue = procedure;' + #10 +
      'var GlobalProcedure: procedure;' + #10 +
      '// function Decoy: Integer; begin end;' + #10 +
      'function Calculate(const AValue: Integer): Integer;' + #10 +
      'type TRow = record Value: Integer; end;' + #10 +
      'var LRow: TRow;' + #10 +
      '  function LocalDouble: Integer;' + #10 +
      '  begin Result := AValue * 2; end;' + #10 +
      'begin' + #10 + '  LRow.Value := LocalDouble;' + #10 +
      '  try' + #10 + '    case AValue of' + #10 +
      '      1: Result := LRow.Value;' + #10 +
      '      else repeat LRow.Value := LRow.Value - 1; until LRow.Value <= 0;' + #10 +
      '    end;' + #10 + '  finally' + #10 + '    Result := LRow.Value;' + #10 +
      '  end;' + #10 + 'end;' + #10 + #10 +
      'class function TLocal.Caption: TNyxText;' + #10 +
      'begin Result := ''procedure Fake; begin end;''; end;' + #10 + #10 +
      'constructor TLocal.Create;' + #10 + 'begin inherited Create; end;' + #10 +
      'destructor TLocal.Destroy;' + #10 + 'begin inherited Destroy; end;' + #10);
    LCatalog := ReadNyxRoutines(LSource);
    Check((LCatalog.Count >= 5) and
      (LCatalog.Item(0).Routine.Name = 'Calculate') and
      (LCatalog.Item(1).Routine.Name = 'TLocal.Caption'),
      'type method/procedural declarations, nested routines and decoys are excluded');
    LRoutine := ReadNyxRoutineSource(LSource, NyxRoutine('cALCULATE'));
    Check(LRoutine.Editable and (Pos('function LocalDouble', LRoutine.Code) > 0) and
      (Pos('try', LRoutine.Code) > 0), 'nested declarations and blocks belong to one exact implementation');
    Check(LRoutine.Signature = 'function Calculate(const AValue: Integer): Integer;',
      'parameters and function result signature stay exact');
    LCode := #10 + 'begin' + #10 + TNyxText('  { retained qualification 🌙 }') + #10 +
      '  Result := AValue + 3;' + #10 + 'end;';
    LAfter := ReplaceNyxRoutineImplementation(LSource, NyxRoutine('Calculate'), LRoutine.Code, LCode);
    Check(ReadNyxRoutineSource(LAfter, NyxRoutine('Calculate')).Code = LCode,
      'exact Unicode replacement retains function identity');
    Check(ReadNyxRoutineSource(LAfter, NyxRoutine('TLocal.Caption')).Code =
      ReadNyxRoutineSource(LSource, NyxRoutine('TLocal.Caption')).Code,
      'replacement leaves qualified sibling bytes unchanged');
    Check((ReadNyxRoutineSource(LSource, NyxRoutine('TLocal.Create')).Kind = nrConstructor) and
      (ReadNyxRoutineSource(LSource, NyxRoutine('TLocal.Destroy')).Kind = nrDestructor),
      'constructor/destructor methods have specialized routine kinds');
    Refuse(LSource, 'LocalDouble', LCode, 'nested local routines cannot become independent targets');
    Refuse(LSource, 'Calculate', LCode + #10 + 'procedure Injected; begin end;',
      'second sibling implementation refuses');
    Refuse(LSource, 'Calculate', LCode + ' // swallowing comment',
      'trailing comment cannot swallow the next helper');
    Refuse(LSource, 'Calculate', #10 + '{$ifdef A}' + #10 + LCode + #10 + '{$endif}',
      'new conditional implementation refuses');
    LSource := WithHelpers('function Choice: Integer; overload;' + #10 +
      'begin Result := 1; end;' + #10 + 'function Choice(AValue: Integer): Integer; overload;' + #10 +
      'begin Result := AValue; end;' + #10);
    LCatalog := ReadNyxRoutines(LSource);
    Check(not LCatalog.Item(0).Editable and not LCatalog.Item(1).Editable,
      'duplicate overload identities remain visible with refusal reasons');
    Refuse(LSource, 'Choice', LCode, 'overloaded identity is never guessed');
    LSource := WithHelpers('{$ifdef A}' + #10 + 'function Conditional: Integer;' + #10 +
      'begin Result := 1; end;' + #10 + '{$endif}' + #10);
    Check(not ReadNyxRoutineSource(LSource, NyxRoutine('Conditional')).Editable,
      'enclosing conditional owns the routine');
    Refuse(LSource, 'Conditional', LCode, 'enclosing conditional mutation refuses');
    LSource := WithHelpers('function Directed: Integer;' + #10 +
      'begin' + #10 + '(*$R-*)' + #10 + 'Result := 1;' + #10 + 'end;' + #10);
    Refuse(LSource, 'Directed', LCode, 'parenthesis compiler directive refuses');
    LSource := WithHelpers('procedure Declared; forward;' + #10 +
      'procedure ExternalBody; external ''other'';' + #10);
    LCatalog := ReadNyxRoutines(LSource);
    Check(not LCatalog.Item(0).Editable and not LCatalog.Item(1).Editable,
      'forward/external declarations are queryable without invented bodies');
    Refuse(LSource, 'Declared', LCode, 'forward declaration cannot be edited');
    Refuse(LBase, 'BuildNyxDocument', LCode, 'managed design body has protected ownership');
    LSource := WithHelpers('class constructor TLocal.Start;' + #10 + 'begin end;' + #10 +
      'class destructor TLocal.Stop;' + #10 + 'begin end;' + #10);
    Check(ReadNyxRoutineSource(LSource, NyxRoutine('TLocal.Start')).Signature =
      'class constructor TLocal.Start;', 'class constructors retain their complete prefix');
    Check(ReadNyxRoutineSource(LSource, NyxRoutine('TLocal.Stop')).Kind = nrDestructor,
      'class destructor is not mistaken for a type block');
    LSource := WithHelpers('function &Result: Integer;' + #10 + 'begin Result := 2; end;' + #10);
    Check(ReadNyxRoutineSource(LSource, NyxRoutine('Result')).Signature =
      'function &Result: Integer;', 'escaped Pascal identity retains its exact authored signature');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-routine-lexical', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' routine lexical checks';
    {$else}
    WriteLn('PASS ', LChecks, ' routine lexical checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-routine-lexical', 'failed');
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
