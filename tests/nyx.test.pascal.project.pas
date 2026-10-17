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
unit nyx.test.pascal.project;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.pascal.project;

{ Preserve the existing complete runtime companion and its callback classes.
  A typed helper unit and includes supply only the authored plan default; two
  different defaults therefore share exact main source but independent inputs.
  Main include guards complete construction cardinality in actual entry points. }
function CompiledRuntimeProject(const ASource, ADefault: TNyxText): INyxPascalProject;

implementation

uses nyx.source, nyx.codegen;

function ReplaceOnce(const AText, ABefore, AAfter: TNyxText): TNyxText;
var
  LAt: Integer;
begin
  LAt := Pos(ABefore, AText);

  if LAt = 0 then
  begin
    raise ENyxSource.CreateAt('The exact owned project fixture insertion point is absent', AText, 1);
  end;
  Result := Copy(AText, 1, LAt - 1) + AAfter + Copy(AText, LAt + Length(ABefore), MaxInt);
end;

function CompiledRuntimeProject(const ASource, ADefault: TNyxText): INyxPascalProject;
var
  LMain: TNyxText;
  LUnit: TNyxPascalUnitRef;
  LParts: TNyxStrings;
  LHelper: TNyxText;
  LBody: TNyxText;
begin
  LUnit := NyxPascalUnit(NyxCompanionUnitName(ASource));
  LMain := ReplaceOnce(ASource, TNyxCodegen.TextLiteral(ADefault), 'ReadProjectPlan');
  LMain := EditNyxImport(LMain, nisImplementation, niaAdd, NyxPascalUnit('project.values'));
  LMain := ReplaceOnce(LMain, NyxViewsBegin,
    '{$I ui-plan.inc}' + #10#10 + NyxViewsBegin);
  LParts := TNyxStrings.Create;
  try
    LParts.Add('unit project.values;' + #10 + '{$mode delphi}{$H+}{$codepage utf8}' + #10);
    LParts.Add('interface' + #10 + 'uses nyx.text;' + #10);
    LParts.Add('function ProjectPlan: TNyxText;' + #10);
    LParts.Add('implementation' + #10 + '{$I plan-values.inc}' + #10 + 'end.' + #10);
    LHelper := LParts.Join;
    LParts.Clear;
    LParts.Add('function ProjectPlan: TNyxText;' + #10 + 'begin' + #10 + '  Result := ');
    LParts.Add(TNyxCodegen.TextLiteral(ADefault));
    LParts.Add(';' + #10 + 'end;' + #10);
    LBody := LParts.Join;
  finally
    LParts.Free;
  end;
  Result := NewNyxPascalProject(LUnit, LMain)
    .WithUnit(NyxPascalUnit('project.values'), LHelper)
    .WithInclude(NyxPascalInclude('ui-plan.inc'),
      'var' + #10 + '  GProjectConstructions: Integer;' + #10#10 +
      '{ Each actual executable constructs the complete context only once. }' + #10 +
      'function ReadProjectPlan: TNyxText;' + #10 + 'begin' + #10 +
      '  Inc(GProjectConstructions);' + #10#10 +
      '  if GProjectConstructions <> 1 then' + #10 + '  begin' + #10 +
      '    raise Exception.Create(''Construct the captured project only once'');' + #10 +
      '  end;' + #10 + '  Result := ProjectPlan;' + #10 + 'end;' + #10)
    .WithInclude(NyxPascalInclude('plan-values.inc'), LBody);
end;

end.
