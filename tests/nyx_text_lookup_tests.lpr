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
program nyx_text_lookup_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text
  {$ifdef PAS2JS}, Web{$endif};

const
  CItems: array[0..17] of TNyxText = (
    'text=Welcome', 'Text=Distinct case', 'text=Second occurrence',
    'tex=Shorter', 'textual=Longer', 'unseparated', '', '=Empty prefix',
    'equal=a=b', 'equ=later', '🌙=Supplementary', '🌙🌟=Two scalars',
    'é=Composed', 'é=Decomposed', 'embedded' + #0 + 'key=value',
    #0 + '=Zero scalar', 'long-name=' + #0 + 'value', 'x=y=z');

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ Names is the independent public definition of a name. Lookup must return
  its first exact occurrence through every mutation, including non-pair items.
  The oracle deliberately allocates Names: only the product lookup is optimized. }
procedure Qualify(AStrings: TNyxStrings);
var
  LItem: Integer;
  LQuery: Integer;
  LExpected: Integer;
  LName: TNyxText;
begin
  for LQuery := -3 to High(CItems) do
  begin

    if LQuery = -3 then
    begin
      LName := '';
    end
    else if LQuery = -2 then
    begin
      LName := 'missing';
    end
    else if LQuery = -1 then
    begin
      LName := 'text=Welcome';
    end
    else
    begin
      LName := CItems[LQuery];

      if Pos('=', LName) > 0 then
      begin
        LName := Copy(LName, 1, Pos('=', LName) - 1);
      end;
    end;
    LExpected := -1;
    for LItem := 0 to AStrings.Count - 1 do
    begin

      if AStrings.Names[LItem] = LName then
      begin
        LExpected := LItem;
        Break;
      end;
    end;
    Check(AStrings.IndexOfName(LName) = LExpected,
      'Exact first name differs from Names for corpus query ' + IntToStr(LQuery));
  end;
end;

procedure Run;
var
  LOriginal: TNyxStrings;
  LCopy: TNyxStrings;
  LIndex: Integer;
  LLongName: TNyxText;
begin
  LOriginal := TNyxStrings.Create;
  LCopy := TNyxStrings.Create;
  try
    Qualify(LOriginal);
    for LIndex := 0 to High(CItems) do
    begin
      LOriginal.Add(CItems[LIndex]);
    end;
    Qualify(LOriginal);
    Check((LOriginal.IndexOfName('text') = 0) and
      (LOriginal.IndexOfName('Text') = 1), 'Names retain case and first duplicate');
    Check((LOriginal.IndexOfName('é') = 12) and
      (LOriginal.IndexOfName('é') = 13), 'Names preserve exact Unicode normalization');
    LCopy.Assign(LOriginal);
    LCopy[0] := 'replacement=Owned';
    LCopy.Delete(1);
    Qualify(LCopy);
    Check((LCopy.IndexOfName('text') = 1) and
      (LOriginal.IndexOfName('text') = 0), 'Replace/delete preserve independent ownership');
    LCopy.Clear;
    Qualify(LCopy);
    LCopy.Add('sole');
    Qualify(LCopy);
    LCopy[0] := 'sole=';
    Qualify(LCopy);
    Check((LCopy.IndexOfName('') = -1) and (LCopy.IndexOfName('sole') = 0),
      'A name requires a separator; the value may be empty');
    LLongName := '';
    for LIndex := 1 to 512 do
    begin
      LLongName := LLongName + '🌙';
    end;
    LCopy.Clear;
    LCopy.Add(LLongName + '=Long name');
    Check(LCopy.IndexOfName('') = -1, 'Large separator offsets preserve empty-name lookup');
    Check(LCopy.IndexOfName(LLongName) = 0, 'Long supplementary names match exact storage units');
    Check(LCopy.IndexOfName(LLongName + '=Long name') = -1,
      'A complete long item cannot masquerade as a name');
  finally
    LCopy.Free;
    LOriginal.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' exact text lookup checks';
    document.body.setAttribute('data-result', 'passed');
    document.body.setAttribute('data-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' exact text lookup and ownership checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.body.setAttribute('data-error', LException.Message);
      {$else}
      WriteLn(StdErr, 'FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
