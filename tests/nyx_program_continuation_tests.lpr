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

program nyx_program_continuation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.types, nyx.model, nyx.codec,
  nyx.source, nyx.codegen, nyx.programs.types, nyx.programs.contract,
  nyx.programs.declarations, nyx.programs.canvas, nyx.generated.view,
  nyx.studio.projects
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;
  {$ifdef PAS2JS}GRequest: TJSXMLHttpRequest;{$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Program continuation: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Count an exact retained call independently of inserted imports or wrappers;
  source length is not a stable offset into a continued handwritten unit. }
function Occurrences(const AText, ANeedle: TNyxText): Integer;
var
  LRemaining: TNyxText;
  LAt: Integer;
begin
  Result := 0;
  LRemaining := AText;
  repeat
    LAt := Pos(ANeedle, LRemaining);

    if LAt = 0 then
    begin
      Exit;
    end;
    Inc(Result);
    LRemaining := Copy(LRemaining, LAt + Length(ANeedle), MaxInt);
  until False;
end;

function TextContract: TNyxProgramContract;
begin
  Result := NyxProgramContract.Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText));
end;

{$ifndef PAS2JS}
procedure Save(const ADirectory, AName, AText: TNyxText);
var
  LBytes: TNyxBytes;
  LFile: TFileStream;
begin
  ForceDirectories(ADirectory);
  LBytes := NyxEncodeUTF8(AText);
  LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ADirectory) + AName, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
end;
{$endif}

procedure Run(const ASource: TNyxText);
var
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LNext: TNyxText;
  LHelpers: TNyxText;
  LStable: TNyxText;
  LPosition: Integer;
  LBuilder: Integer;
  LStage: Integer;
  LDefinition: INyxProgramDefinition;
  LRefused: Boolean;
  {$ifndef PAS2JS}LDirectory: TNyxText;{$endif}
begin
  LBefore := BuildNyxDocument;
  LAfter := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LWorkspace.AcceptExecuted(LBefore, ASource);
    Check(LWorkspace.Render(LBefore) = ASource, 'actual compiled base retains exact handwritten bytes');
    LPosition := Pos('type' + #10 + '  { Handwritten application recipes', ASource);
    LBuilder := Pos('function BuildNyxDocument: TNyxDocument;' + #10 + 'var', ASource);
    Check((LPosition > 0) and (LBuilder > LPosition), 'independent helper boundary is explicit');
    LHelpers := Copy(ASource, LPosition, LBuilder - LPosition);
    LStable := LBefore.Programs.Definition(NyxProgramRef('Stable helper')).ToData.ToJSON;
    LSource := ASource;
    {$ifndef PAS2JS}
    Check(ParamCount = 2, 'supply exact fixture source and owned export directory');
    Save(ParamStr(2), 'initial.nyxpair', EncodeNyxProject(NyxProjectPair(TNyxCodec.Encode(LBefore), ASource)));
    {$endif}
    for LStage := 1 to 5 do
    begin
      LAfter := LBefore.Clone;
      case LStage of
        1:
        begin
          LDefinition := LBefore.Programs.Definition(NyxProgramRef('Greeting'));
          LAfter.Programs.Define(NyxProgramRef('Greeting'), NewNyxProgramDefinition(
            'begin Caption := ''Welcome, '' + Name end.', LDefinition.Contract,
            LDefinition.Limits.Steps(5000)));
        end;
        2:
        begin
          LAfter.Programs.Define(NyxProgramRef('Canvas layout'), NewNyxProgramDefinition(
            'begin MoveItem(SceneItem(0), 12, 8) end.', NyxCanvasProgramContract));
          LAfter.Programs.Remove(NyxProgramRef('Greeting'));
        end;
        3:
        begin
          LAfter.Programs.Define(NyxProgramRef('Greeting'), NewNyxProgramDefinition(
            'begin Caption := ''Hello again, '' + Name end.',
            NyxProgramContract.Input(NyxProgramSymbol('Name'), NyxProgramType(npkText))
              .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText)),
            NyxProgramLimits.Steps(8192)));
        end;
        4:
        begin
          LAfter.Find('workshop-title').Configure.Text('A saved Pascal program workshop').Done;
          LAfter.Programs.Remove(NyxProgramRef('Canvas layout'));
        end;
        5:
        begin
          LAfter.Programs.Remove(NyxProgramRef('Greeting'));
          LAfter.Programs.Define(NyxProgramRef('Derived'), NewNyxProgramDefinition(
            'begin Caption := ''A derived behavior'' end.', TextContract, NyxProgramLimits.Steps(4096)));
        end;
      end;
      LAfter.Validate;
      LNext := CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
      {$ifndef PAS2JS}
      LDirectory := IncludeTrailingPathDelimiter(ParamStr(2)) + 'proposal-' + IntToStr(LStage);
      Save(LDirectory, 'nyx.generated.view.pas', LNext);
      {$endif}
      Check(Pos(LHelpers, LNext) > 0, 'exact original helper bytes survive stage ' + IntToStr(LStage));
      Check(LAfter.Programs.Definition(NyxProgramRef('Stable helper')).ToData.ToJSON = LStable,
        'untouched computed definition remains exact');
      Check(Occurrences(LNext, 'Programs.Define(NyxProgramRef(''Stable helper'')') = 1,
        'continuations do not flatten untouched helpers');
      Check(Pos('// nyx:continuation ''' + TNyxText(IntToStr(LStage)) + '''', LNext) > 0,
        'chronological program changes receive independent stages');
      {$ifndef PAS2JS}
      LDirectory := IncludeTrailingPathDelimiter(ParamStr(2)) + 'stage-' + IntToStr(LStage);
      Save(LDirectory, 'nyx.generated.view.pas', LNext);
      Save(LDirectory, 'design.nyx', TNyxCodec.Encode(LAfter));
      {$endif}
      LSource := LNext;
      LBefore.Free;
      LBefore := LAfter;
      LAfter := nil;
    end;
    LPosition := Pos('Kept by a helper', LSource);
    Check(LPosition > 0, 'future helper text is still computed');
    LNext := Copy(LSource, 1, LPosition - 1) + TNyxText('Still computed tomorrow') +
      Copy(LSource, LPosition + Length('Kept by a helper'), MaxInt);
    LAfter := LBefore.Clone;
    LAfter.Programs.Define(NyxProgramRef('Stable helper'), NewNyxProgramDefinition(
      'begin Caption := ''Still computed tomorrow'' end.', TextContract,
      NyxProgramLimits.Steps(1040).Nesting(24)));
    {$ifndef PAS2JS}
    LDirectory := IncludeTrailingPathDelimiter(ParamStr(2)) + 'stage-6';
    Save(LDirectory, 'nyx.generated.view.pas', LNext);
    Save(LDirectory, 'design.nyx', TNyxCodec.Encode(LAfter));
    {$endif}
    FreeAndNil(LAfter);
    LAfter := LBefore.Clone;
    LAfter.Programs.Remove(NyxProgramRef('Stable helper'));
    LAfter.Programs.Define(NyxProgramRef('Stable helper'),
      LBefore.Programs.Definition(NyxProgramRef('Stable helper')));
    LRefused := False;
    try
      CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LWorkspace.Capture.Source = ASource),
      'unexpressed registry ordering refuses the complete candidate');
  finally
    LAfter.Free;
    LWorkspace.Free;
    LBefore.Free;
  end;
end;

procedure Execute(const ASource: TNyxText);
begin
  try
    Run(ASource);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-program-continuation', 'passed');
    document.body.setAttribute('data-program-continuation-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' handwritten program continuation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-program-continuation', 'failed');
      document.body.setAttribute('data-program-continuation-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

{$ifdef PAS2JS}
function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;

  if GRequest.status = 200 then
  begin
    Execute(GRequest.responseText);
  end
  else
  begin
    document.body.setAttribute('data-program-continuation', 'failed');
    document.body.setAttribute('data-program-continuation-error', 'Exact handwritten source unavailable');
  end;
  GRequest := nil;
end;
{$else}
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'nyx.generated.view.pas');
  GRequest.onload := @Loaded;
  GRequest.send;
  {$else}
  LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
  try
    SetLength(LBytes, LFile.Size);

    if Length(LBytes) > 0 then
    begin
      LFile.ReadBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
  Execute(NyxDecodeUTF8(LBytes));
  {$endif}
end.
