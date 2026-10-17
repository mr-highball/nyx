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
program nyx_pascal_project_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.source, nyx.pascal.project
  {$ifdef PAS2JS}, Web{$endif};

type
  { An alternate mutable caller, not the owned implementation. Its deliberately
    false byte count/empty Validate cannot grant admission. The captured value
    must remain independent after the caller changes or releases this provider. }
  TMutableProject = class(TInterfacedObject, INyxPascalProject)
  public
    Value: INyxPascalProject;
    MissingInput: Boolean;
    ExtraCount: Integer;
    function MainUnit: TNyxPascalUnitRef;
    function Source: TNyxText;
    function Limits: TNyxPascalProjectLimits;
    function Count: Integer;
    function Input(AIndex: Integer): INyxPascalInput;
    function ByteCount: Integer;
    function WithSource(const ASource: TNyxText): INyxPascalProject;
    function WithUnit(const AUnit: TNyxPascalUnitRef;
      const ASource: TNyxText): INyxPascalProject;
    function WithInclude(const AInclude: TNyxPascalIncludeRef;
      const ASource: TNyxText): INyxPascalProject;
    function WithoutUnit(const AUnit: TNyxPascalUnitRef): INyxPascalProject;
    function WithoutInclude(const AInclude: TNyxPascalIncludeRef): INyxPascalProject;
    procedure Validate;
  end;

var
  GChecks: Integer;

function TMutableProject.MainUnit: TNyxPascalUnitRef;
begin
  Result := Value.MainUnit;
end;

function TMutableProject.Source: TNyxText;
begin
  Result := Value.Source;
end;

function TMutableProject.Limits: TNyxPascalProjectLimits;
begin
  Result := Value.Limits;
end;

function TMutableProject.Count: Integer;
begin
  Result := Value.Count + ExtraCount;
end;

function TMutableProject.Input(AIndex: Integer): INyxPascalInput;
begin
  Result := nil;

  if not MissingInput then
  begin
    Result := Value.Input(AIndex);
  end;
end;

function TMutableProject.ByteCount: Integer;
begin
  Result := 0;
end;

function TMutableProject.WithSource(const ASource: TNyxText): INyxPascalProject;
begin
  Result := Value.WithSource(ASource);
end;

function TMutableProject.WithUnit(const AUnit: TNyxPascalUnitRef;
  const ASource: TNyxText): INyxPascalProject;
begin
  Result := Value.WithUnit(AUnit, ASource);
end;

function TMutableProject.WithInclude(const AInclude: TNyxPascalIncludeRef;
  const ASource: TNyxText): INyxPascalProject;
begin
  Result := Value.WithInclude(AInclude, ASource);
end;

function TMutableProject.WithoutUnit(const AUnit: TNyxPascalUnitRef): INyxPascalProject;
begin
  Result := Value.WithoutUnit(AUnit);
end;

function TMutableProject.WithoutInclude(const AInclude: TNyxPascalIncludeRef): INyxPascalProject;
begin
  Result := Value.WithoutInclude(AInclude);
end;

procedure TMutableProject.Validate;
begin
end;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function UnitSource(const AName, ABody: TNyxText): TNyxText;
var
  LParts: TNyxStrings;
begin
  LParts := TNyxStrings.Create;
  try
    LParts.Add('unit ' + AName + ';' + #10);
    LParts.Add('{$mode delphi}{$H+}{$codepage utf8}' + #10);
    LParts.Add('interface' + #10 + 'implementation' + #10);
    LParts.Add(ABody);
    LParts.Add(#10 + 'end.' + #10);
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function Refuses(const AProject: INyxPascalProject): Boolean;
var
  LCaptured: INyxPascalProject;
begin
  Result := False;
  try
    LCaptured := CaptureNyxPascalProject(AProject);
  except
    on E: ENyxPascalProject do
    begin
      Result := True;
    end;
    on E: ENyxSource do
    begin
      Result := True;
    end;
  end;
  LCaptured := nil;
end;

procedure Run;
const
  CInclude: TNyxText = 'const Caption: String = ''A moon 🌙 and a letter 𐐷'';';
  CBadIncludes: array[0..8] of TNyxText = ('../outside.inc', 'dir/file.inc',
    'C:outside.inc', 'NUL.inc', 'COM1.extra.inc', '.hidden.inc',
    'bad..inc', 'file.pas', 'bad name.inc');
  CExternal: array[0..5] of TNyxText = ('{$I ../outside.inc}', '{$R picture.res}',
    '{$link external.o}', '{$unitpath C:/private}',
    'uses Other in ''outside.pas'';', '{$include missing.inc}');
var
  LBase: INyxPascalProject;
  LComplete: INyxPascalProject;
  LChanged: INyxPascalProject;
  LCaptured: INyxPascalProject;
  LForeign: TMutableProject;
  LForeignOwner: INyxPascalProject;
  LLimits: TNyxPascalProjectLimits;
  LMain: TNyxText;
  LHelper: TNyxText;
  LFailed: Boolean;
  LIndex: Integer;
  LIncludes: TNyxStrings;
  LInclude: TNyxPascalIncludeRef;
begin
  LMain := UnitSource('App.Main', '{$I main-body.inc}');
  LHelper := UnitSource('App.Helper', '{$include ''shared.inc''}');
  LBase := NewNyxPascalProject(NyxPascalUnit('App.Main'), LMain);
  Check(LBase.Count = 0, 'An initial snapshot contains only its separate main unit');
  Check(Refuses(LBase), 'A proposed snapshot cannot compile an absent include');
  LComplete := LBase.WithUnit(NyxPascalUnit('App.Helper'), LHelper)
    .WithInclude(NyxPascalInclude('shared.inc'), CInclude)
    .WithInclude(NyxPascalInclude('main-body.inc'), 'uses App.Helper;');
  LCaptured := CaptureNyxPascalProject(LComplete);
  Check((LCaptured.Count = 3) and (LBase.Count = 0),
    'A complete capture retains independent ordered auxiliary membership');
  Check((LCaptured.MainUnit.Name = 'App.Main') and (LCaptured.Source = LMain),
    'Capture retains the exact main namespace and source');
  Check((LCaptured.Input(0).Kind = npiUnit) and
    (LCaptured.Input(0).UnitReference.Name = 'App.Helper'), 'Unit identity stays strongly typed');
  Check((LCaptured.Input(1).Kind = npiInclude) and
    (LCaptured.Input(1).Text = CInclude), 'An include preserves supplementary source exactly');
  Check(LCaptured.ByteCount = NyxUTF8ByteCount(LMain) + NyxUTF8ByteCount(LHelper) +
    NyxUTF8ByteCount(CInclude) + NyxUTF8ByteCount('uses App.Helper;'),
    'Project accounting counts exact UTF-8 bytes on either compiler');
  LChanged := LComplete.WithInclude(NyxPascalInclude('shared.inc'), 'const Value = 2;');
  Check((LChanged.Count = LComplete.Count) and (LComplete.Input(1).Text = CInclude),
    'Replacing an include preserves earlier snapshots and authored ordering');
  LChanged := LChanged.WithoutUnit(NyxPascalUnit('App.Helper'));
  Check((LChanged.Count = 2) and (LChanged.Input(0).Name = 'shared.inc') and
    (LComplete.Count = 3), 'Removing a unit changes only the new snapshot');
  LChanged := LChanged.WithoutInclude(NyxPascalInclude('shared.inc'));
  Check((LChanged.Count = 1) and (LComplete.Input(1).Text = CInclude),
    'Removing an include never clears another snapshot vector');
  Check(LChanged.WithoutInclude(NyxPascalInclude('absent.inc')).Count = 1,
    'Removing an absent include is idempotent');
  LChanged := LComplete.WithSource(UnitSource('Wrong.Main', ''));
  Check(Refuses(LChanged) and (LComplete.Source = LMain), 'A mismatched main namespace refuses independently');
  LChanged := LComplete.WithUnit(NyxPascalUnit('App.Helper'), UnitSource('Other.Helper', ''));
  Check(Refuses(LChanged), 'An auxiliary file must declare its captured namespace');
  LFailed := False;
  try
    LChanged := LComplete.WithInclude(NyxPascalInclude('SHARED.inc'), 'const Value = 2;');
  except
    on E: ENyxPascalProject do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LComplete.Input(1).Text = CInclude), 'Case-only aliases refuse before publication');
  LFailed := False;
  try
    LChanged := LComplete.WithUnit(NyxPascalUnit('App.Main'), LMain);
  except
    on E: ENyxPascalProject do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed, 'Auxiliary inputs cannot replace the main unit');
  for LIndex := Low(CBadIncludes) to High(CBadIncludes) do
  begin
    LFailed := False;
    try
      LInclude := NyxPascalInclude(CBadIncludes[LIndex]);
    except
      on E: ENyxPascalProject do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Paths/device aliases and non-include leaves refuse');
  end;
  for LIndex := Low(CExternal) to High(CExternal) do
  begin
    LChanged := LComplete.WithSource(UnitSource('App.Main', CExternal[LIndex]));
    Check(Refuses(LChanged), 'Only inventory-backed literal include leaves supplement existing rules');
  end;
  LIncludes := TNyxStrings.Create;
  try
    LIncludes.Add('main-body.inc');
    Check(NyxCompanionUnitName(LMain, LIncludes) = 'App.Main', 'Explicit owned include inventory admits its leaf');
    LFailed := False;
    try
      NyxCompanionUnitName(LMain);
    except
      on E: ENyxSource do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'The original single-unit admission still refuses all external includes');
    ValidateNyxPascalFileReferences('{$I+}{$R-}', LIncludes);
    ValidateNyxPascalFileReferences('const Literal = ''{$I outside.inc}'';', LIncludes);
    Check(True, 'Runtime checks and directive-looking string data keep their original semantics');
  finally
    LIncludes.Free;
  end;
  LChanged := NewNyxPascalProject(NyxPascalUnit('App.Casing'),
    UnitSource('App.Casing', '{$INCLUDE ''Mixed.inc''}'))
      .WithInclude(NyxPascalInclude('Mixed.inc'), 'const Value = 1;');
  Check(not Refuses(LChanged), 'Directive command folding preserves exact include filename case');
  LChanged := LChanged.WithSource(UnitSource('App.Casing', '{$I mixed.inc}'));
  Check(Refuses(LChanged), 'An include must match the exact captured portable filename');
  LLimits := TNyxPascalProjectLimits.Defaults.UpToFiles(1);
  LChanged := NewNyxPascalProject(NyxPascalUnit('App.Main'), UnitSource('App.Main', ''), LLimits);
  LFailed := False;
  try
    LChanged := LChanged.WithInclude(NyxPascalInclude('one.inc'), 'const Value = 1;');
  except
    on E: ENyxPascalProject do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LChanged.Count = 0), 'A file-count refusal retains the complete prior snapshot');
  LLimits := TNyxPascalProjectLimits.Defaults.UpToBytes(NyxUTF8ByteCount(LMain));
  LChanged := NewNyxPascalProject(NyxPascalUnit('App.Main'), LMain, LLimits);
  LFailed := False;
  try
    LChanged := LChanged.WithInclude(NyxPascalInclude('one.inc'), 'x');
  except
    on E: ENyxPascalProject do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and (LChanged.Count = 0), 'Complete byte admission precedes an auxiliary change');
  LForeign := TMutableProject.Create;
  LForeignOwner := LForeign;
  LForeign.Value := LComplete;
  LCaptured := CaptureNyxPascalProject(LForeignOwner);
  LForeign.Value := LChanged;
  Check((LCaptured.Count = 3) and (LCaptured.Input(1).Text = CInclude),
    'Captured input text and membership survive alternate provider mutation');
  Check(Refuses(LForeignOwner), 'A foreign empty Validate cannot admit an incomplete inventory');
  LForeign.Value := LComplete;
  LForeign.MissingInput := True;
  Check(Refuses(LForeignOwner), 'Empty alternate input entries refuse before staging');
  LForeign.MissingInput := False;
  LForeign.ExtraCount := 4096;
  Check(Refuses(LForeignOwner), 'A foreign count cannot bypass caller-selected budgets');
  LForeignOwner := nil;
  Check(LCaptured.Input(1).Text = CInclude, 'Frozen input survives alternate provider retirement');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-pascal-project', 'passed');
    document.body.setAttribute('data-pascal-project-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' captured Pascal project checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-pascal-project', 'failed');
      document.body.setAttribute('data-pascal-project-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
