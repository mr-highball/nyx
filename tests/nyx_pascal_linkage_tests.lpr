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
program nyx_pascal_linkage_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.lexing, nyx.pascal.translation, nyx.pascal.syntax,
  nyx.pascal.syntax.translated, linkage.fixture;

var
  GChecks: Integer;
  {$ifndef PAS2JS}
  GLayout: TCompilerLayoutSizes;
  {$endif}

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Translation(const ASource: TNyxText): INyxPascalTranslation;
begin
  Result := NewNyxPascalTranslation(NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('source.pas'), ASource), NyxPascalPhysicalFile('source.pas'),
    NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
      .CompleteDefines(True), TNyxPascalTranslationOptions.Defaults);
end;

function View(const ASource: TNyxText): INyxPascalTranslatedSyntax;
begin
  Result := NewNyxPascalTranslatedSyntax(Translation(ASource), TNyxPascalSyntaxOptions.Defaults);
end;

function Decl(const ASyntax: INyxPascalSyntaxSnapshot; const AName: TNyxText;
  AScope: Integer = 0): TNyxPascalSyntaxDeclaration;
var
  LAt: Integer;
begin
  LAt := ASyntax.Named(AScope, AName);

  if LAt < 0 then
  begin
    raise Exception.Create('Expected original declaration is absent');
  end;
  Result := ASyntax.Declaration(LAt);
end;

{ These single-file fixtures map analysis coordinates back through the public
  origin table. Synthetic token separators are never compared as authored text. }
function OriginalText(const AView: INyxPascalTranslatedSyntax;
  const ASpan: TNyxPascalSpan): TNyxText;
var
  LFirst: TNyxPascalSyntaxOrigin;
  LLast: TNyxPascalSyntaxOrigin;
begin
  LFirst := AView.Origin(AView.OriginAt(ASpan.Start));
  LLast := AView.Origin(AView.OriginAt(ASpan.Finish - 1));

  if LFirst.Occurrence <> LLast.Occurrence then
  begin
    raise Exception.Create('The fixture span needs more than one original occurrence');
  end;
  Result := NyxPascalOccurrenceLexical(AView.Source, LFirst.Occurrence).Slice(
    NyxPascalSpan(LFirst.PhysicalSpan.Start, LLast.PhysicalSpan.Finish - LFirst.PhysicalSpan.Start));
end;

procedure LinkageAndScopes;
var
  LView: INyxPascalTranslatedSyntax;
  LSyntax: INyxPascalSyntaxSnapshot;
  LDecl: TNyxPascalSyntaxDeclaration;
  LText: TNyxText;
  LAt: Integer;
  LCancel: INyxCancellationScope;
  LRefused: Boolean;
begin
  LView := View('unit Surface; {$modeswitch cvar} interface var PublicValue:LongInt; public name ''linked''; ' +
    'NextValue:LongInt; function First:Integer; function Second:Integer; implementation end.');
  LSyntax := LView.Syntax;
  Check(LSyntax.DiagnosticCount = 0, 'Public variable suffixes do not corrupt ordinary interface parsing');
  LDecl := Decl(LSyntax, 'PublicValue');
  Check((LDecl.Visibility = psavInterface) and (LDecl.Linkage = [plfPublic]) and
    (LDecl.Scope = 0), 'Linker publication is distinct from member visibility');
  Check(OriginalText(LView, LDecl.LinkageSpan) = TNyxText('public name ''linked'';'),
    'Exact linkage spelling and operand remain attached to the original declaration');
  Check((Decl(LSyntax, 'NextValue').Visibility = psavInterface) and
    (Decl(LSyntax, 'First').Scope = 0) and (Decl(LSyntax, 'Second').Scope = 0),
    'Following variables and routine prototypes retain sibling scope and visibility');
  LView := View('unit Names; {$mode objfpc} interface implementation function One:Integer; ' +
    'public name ''one''; begin Result:=1; end; function Two:Integer; ' +
    '[public,alias:''two'']; begin Result:=2; end; end.');
  LSyntax := LView.Syntax;
  Check(LSyntax.DiagnosticCount = 0, 'Direct and bracket routine linkage retain their real bodies');
  Check((Decl(LSyntax, 'One').Linkage = [plfPublic]) and
    (Decl(LSyntax, 'Two').Linkage = [plfPublic, plfAlias, plfPlatformAttributes]),
    'Typed flags retain each declared linkage family');
  Check((Decl(LSyntax, 'One').Scope = 0) and (Decl(LSyntax, 'Two').Scope = 0),
    'Public routine modifiers cannot nest their following sibling');
  LView := View('unit Controls; interface type TControl=class private Value:Integer; ' +
    'public procedure Act; protected procedure Follow; end; implementation end.');
  LSyntax := LView.Syntax;
  LAt := Decl(LSyntax, 'TControl').NestedScope;
  Check((Decl(LSyntax, 'Act', LAt).Visibility = psavPublic) and
    (Decl(LSyntax, 'Follow', LAt).Visibility = psavProtected) and
    (Decl(LSyntax, 'Value', LAt).Visibility = psavPrivate),
    'Class visibility sections remain owned by their member scope');
  Check(Decl(LSyntax, 'Act', LAt).Linkage = [], 'A class PUBLIC section is not a method linker suffix');
  LView := View('unit Native; interface var A,B:Integer; cvar; external name ''shared''; ' +
    'procedure P; compilerproc; procedure Q; [internproc:12]; implementation end.');
  LSyntax := LView.Syntax;
  Check((Decl(LSyntax, 'A').Linkage = [plfCVariable, plfExternal]) and
    (Decl(LSyntax, 'B').Linkage = Decl(LSyntax, 'A').Linkage),
    'Grouped declarations retain independent entries with shared native suffix intent');
  Check((plfCompilerHelper in Decl(LSyntax, 'P').Linkage) and
    (plfIntrinsic in Decl(LSyntax, 'Q').Linkage), 'Compiler-specific suffixes retain explicit source classification');
  LView := View('unit Unknown; interface procedure P; [unknown_attribute:1]; ' +
    'procedure Q; implementation end.');
  Check((LView.Syntax.DiagnosticCount > 0) and
    (psdfUnsupported in Decl(LView.Syntax, 'P').Flags) and (Decl(LView.Syntax, 'Q').Scope = 0),
    'Unknown platform attributes retain uncertainty without consuming the following routine');
  LView := View('unit Broken; interface var A,B:Integer; public name ''x'' implementation end.');
  Check((psdfIncomplete in Decl(LView.Syntax, 'A').Flags) and
    (psdfIncomplete in Decl(LView.Syntax, 'B').Flags), 'Malformed grouped suffixes preserve incomplete ownership on every entry');
  LView := View(TNyxText('unit Text; interface var Value:Integer; public name ''Rocket 🚀''; implementation end.'));
  LDecl := Decl(LView.Syntax, 'Value');
  Check(OriginalText(LView, LDecl.LinkageSpan) = TNyxText('public name ''Rocket 🚀'';'),
    'Supplementary symbol spelling survives exact scalar ranges without ABI acceptance');
  LText := 'unit Many; interface var Value:Integer; public name ''value''; ';
  for LAt := 1 to 512 do
  begin
    LText := LText + 'procedure Sibling' + TNyxText(IntToStr(LAt)) + '; ';
  end;
  LText := LText + 'implementation end.';
  LView := NewNyxPascalTranslatedSyntax(Translation(LText),
    TNyxPascalSyntaxOptions.Defaults.UpToDepth(16));
  Check((LView.Syntax.DiagnosticCount = 0) and (Decl(LView.Syntax, 'Sibling512').Scope = 0),
    'Hundreds of siblings use bounded structural depth instead of accidental recursive bodies');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LRefused := False;
  try
    LView := NewNyxPascalTranslatedSyntax(Translation(LText),
      TNyxPascalSyntaxOptions.Defaults, LCancel);
  except
    on ENyxPascalAnalysisCancelled do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Declaration linkage preserves atomic analysis cancellation');
end;

procedure LayoutArguments;
var
  LSource: INyxPascalTranslation;
  LPolicy: TNyxPascalCompilerPolicies;
begin
  LSource := Translation('{$MINENUMSIZE 1 this saves memory}{$PACKRECORDS 1 packed fields}' +
    '{$PACKSET 1 compact set} Marker');
  LPolicy := NyxPascalCompilerPolicies(LSource.State(LSource.StateCount - 1));
  Check(LSource.Resolved and (LPolicy.EnumStorage = pes1) and
    (LPolicy.RecordAlignment = pra1) and (LPolicy.SetPacking = psp1),
    'Known layout policies consume their value while retaining explanatory comment text');
  LSource := Translation('{$Z+ wide enum intent}');
  Check(LSource.Resolved and
    (NyxPascalCompilerPolicies(LSource.State(LSource.StateCount - 1)).EnumStorage = pes4),
    'The existing positive Z alias uses the parsed argument with trailing prose');
  LSource := Translation('{$Z- narrow enum intent}');
  Check(LSource.Resolved and
    (NyxPascalCompilerPolicies(LSource.State(LSource.StateCount - 1)).EnumStorage = pes1),
    'The existing negative Z alias uses the parsed argument with trailing prose');
  LSource := Translation('{$MINENUMSIZE invalid 1}');
  Check(not LSource.Resolved, 'An invalid leading policy value cannot be rescued by trailing text');
  LSource := Translation('{$MINENUMSIZE 1oops}');
  Check(not LSource.Resolved, 'Layout comments require a whitespace-delimited leading argument');
  LSource := Translation('{$if (1=1) and (2=3)}{$define WRONG}{$endif}');
  Check(LSource.Resolved and (LSource.State(LSource.StateCount - 1).Symbol(
    NyxPascalDefine('WRONG')).IsDefined = ppkFalse), 'Condition expressions are never truncated as policy comments');
end;

{$ifndef PAS2JS}
{ Read the same complete unit that the native compiler admitted, preserving its
  byte boundary and native-only directive branches. No export prefix is selected. }
procedure WholeInput;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
  LText: TNyxText;
  LView: INyxPascalTranslatedSyntax;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
    'tests/linkage.fixture.pas', fmOpenRead or fmShareDenyWrite);
  try

    if (LStream.Size < 1) or (LStream.Size > 65536) then
    begin
      raise Exception.Create('The original fixture exceeds its owned input budget');
    end;
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  LText := NyxDecodeUTF8(LBytes);
  LView := View(LText);
  Check(LView.Source.Resolved and (LView.Syntax.DiagnosticCount = 0),
    'Whole unchanged compiled input passes ordinary source translation and syntax');
  Check((Decl(LView.Syntax, 'SharedValue').Linkage = [plfPublic]) and
    (Decl(LView.Syntax, 'FollowingValue').Visibility = psavInterface),
    'Actual public-variable linkage preserves its following interface variable');
end;
{$endif}

begin
  try
    Check(CompilerLinkageValue = 42, 'Whole compiler-admitted linkage and layout fixture executes its expected behavior');
    {$ifndef PAS2JS}
    GLayout := CompilerLayoutSizes;
    Check((GLayout.SmallEnumBytes = 1) and (GLayout.WideEnumBytes = 4) and
      (GLayout.PackedRecordBytes = 5) and (GLayout.PackedSetBytes = 1),
      'Actual compiler sizes agree with all four layout directives that carry trailing prose');
    {$endif}
    LinkageAndScopes;
    LayoutArguments;
    {$ifndef PAS2JS}WholeInput;{$endif}
    WriteLn('PASS ', GChecks, ' declaration linkage and layout argument checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
