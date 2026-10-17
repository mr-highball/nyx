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

program nyx_pascal_completion_host_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.editing, nyx.pascal.names, nyx.pascal.project,
  nyx.pascal.drafts, nyx.pascal.sources, nyx.pascal.completion,
  nyx.studio.completion.native;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Find(const AResult: INyxPascalCompletionResult; const AName: TNyxText): Integer;
var
  LAt: Integer;
begin
  Result := -1;
  for LAt := 0 to AResult.Count - 1 do
  begin

    if AResult.Item(LAt).Name = AName then
    begin
      Exit(LAt);
    end;
  end;
end;

{ Export only the exact proposed buffer for separate compiler qualification;
  this consumer never executes it or grants it accepted-project provenance. }
procedure ExportText(const APath: String; const AText: TNyxText);
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LFile := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
end;

procedure Run;
const
  CAccepted: TNyxText = 'unit assist.consumer; {$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface uses nyx.controls; procedure Run; implementation' + #10 +
    'procedure Run; var LBadge: INyxBadge; begin end; end.';
  CPrefix: TNyxText = 'unit assist.consumer; {$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface uses nyx.controls; procedure Run; implementation' + #10 +
    'procedure Run; var LBadge: INyxBadge; begin' + #10 +
    '// A friendly badge 🚀 𐐷' + #10 +
    '  LBadge := NewNyxBadge(''status'');' + #10 + '  LBadge.';
  CSuffix: TNyxText = #10 + 'end; end.';
  CIncludeOwner: TNyxText = 'unit assist.owner; {$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'interface uses nyx.controls; procedure Run; implementation' + #10 +
    'procedure Run; var LBadge: INyxBadge; begin {$I body.inc} end; end.';
var
  LHost: TNyxPascalCompletionHost;
  LProvider: INyxPascalCompletionProvider;
  LRequest: INyxPascalCompletionRequest;
  LResult: INyxPascalCompletionResult;
  LPage: INyxPascalCompletionResult;
  LProject: INyxPascalProject;
  LSources: INyxPascalSourceSnapshot;
  LPreview: INyxPascalSourceSnapshot;
  LMain: TNyxPascalSourceRef;
  LInclude: TNyxPascalSourceRef;
  LOwner: TNyxPascalSourceRef;
  LAt: Integer;
  LCaret: Integer;
  LRejected: Boolean;
begin

  if ParamCount <> 6 then
  begin
    raise Exception.Create('Expected optional helper, SDK, compiler, FPC/Lazarus sources and owned scratch paths');
  end;
  LHost := TNyxPascalCompletionHost.Using(ParamStr(1), ParamStr(2), ParamStr(3),
    ParamStr(4), ParamStr(5), ParamStr(6));
  LProvider := NewNyxNativePascalCompletionProvider(LHost);
  LProject := NewNyxPascalProject(NyxPascalUnit('assist.consumer'), CAccepted);
  LMain := TNyxPascalSourceRef.Main;
  LSources := CaptureNyxPascalSources(LProject).WithBuffer(LMain, CPrefix + CSuffix);
  LRequest := NewNyxPascalCompletionRequest(LSources)
    .At(LMain, NyxTextScalarCount(CPrefix)).Take(50);
  LResult := LProvider.Complete(LRequest);
  Check(LResult.Status = pcsReady, 'Actual SDK member query must resolve: ' + LResult.MessageText);
  LAt := Find(LResult, 'WithText');
  Check((LAt >= 0) and (Find(LResult, 'Node') >= 0),
    'Compiler-context resolution finds specialized and inherited INyxBadge members');
  Check((LResult.Item(LAt).Kind = pckMethod) and
    (Pos('INyxBadge', LResult.Item(LAt).Detail) > 0) and
    (Pos('AText', LResult.Item(LAt).Detail) > 0),
    'Typed method suggestions retain a usable specialized Pascal signature');
  Check((LResult.ExpectedText = '') and
    (LResult.ReplaceStart = NyxTextScalarCount(CPrefix)),
    'Engine byte coordinates map back to the exact scalar caret after supplementary text');
  Check((LResult.Count = LResult.Total) and (LResult.Total > 20),
    'Full bounded SDK page has a real inherited member catalog');
  for LAt := 0 to LResult.Count - 1 do
  begin
    Check((Pos(ParamStr(2), String(LResult.Item(LAt).Detail)) = 0) and
      (Pos('FILENAME=', LResult.Item(LAt).Detail) = 0),
      'Public descriptions contain signatures rather than private CodeTools debug paths');
  end;
  LPage := LProvider.Complete(LRequest.Take(3).Skip(3));
  Check((LPage.Status = pcsReady) and (LPage.Count = 3) and
    (LPage.Total = LResult.Total) and (LPage.Item(0).Name = LResult.Item(3).Name),
    'Separate process queries retain a stable bounded SDK page order');

  LSources := LSources.WithBuffer(LMain, CPrefix + 'WithTexx(''Ready'');' + CSuffix);
  LRequest := NewNyxPascalCompletionRequest(LSources)
    .At(LMain, NyxTextScalarCount(CPrefix) + 6).Take(5);
  LResult := LProvider.Complete(LRequest);
  LAt := Find(LResult, 'WithText');
  Check((LResult.Status = pcsReady) and (LAt >= 0) and
    (LResult.ExpectedText = 'WithTexx'), 'Unfinished middle-of-identifier text resolves without losing its suffix');
  LPreview := LResult.Preview(LAt, LSources, LCaret);
  Check((LPreview.Text(LMain) = CPrefix + 'WithText(''Ready'');' + CSuffix) and
    (LPreview.AcceptedText(LMain) = CAccepted) and
    (LCaret = NyxTextScalarCount(CPrefix) + 8),
    'Actual engine result proposes exact source and caret without admitting a source checkpoint');
  ExportText(IncludeTrailingPathDelimiter(ParamStr(6)) + 'assist.consumer.pas', LPreview.Text(LMain));
  ExportText(IncludeTrailingPathDelimiter(ParamStr(6)) + 'completion_companion.lpr',
    'program completion_companion; {$mode delphi}{$H+}{$codepage utf8}' + #10 +
    'uses assist.consumer; begin Run; end.');
  LRejected := False;
  try
    LResult.Preview(LAt, LSources.WithBuffer(LMain, CPrefix + 'Other' + CSuffix), LCaret);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LCaret = 0), 'Actual engine results refuse after their source changes');

  LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('body.inc'));
  LOwner := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('assist.owner'));
  LProject := LProject.WithUnit(NyxPascalUnit('assist.owner'), CIncludeOwner)
    .WithInclude(NyxPascalInclude('body.inc'), 'LBadge.WithText(''Ready'');');
  LSources := CaptureNyxPascalSources(LProject).WithBuffer(LInclude, 'LBadge.WithTe');
  LRequest := NewNyxPascalCompletionRequest(LSources).At(LInclude, 13).From(LOwner);
  LResult := LProvider.Complete(LRequest);
  Check((LResult.Status = pcsReady) and (Find(LResult, 'WithText') >= 0),
    'Captured include completion resolves through its explicit owning unit');
  LResult := LProvider.Complete(LRequest.From(LMain));
  Check(LResult.Status <> pcsReady,
    'An unrelated unit cannot invent an include translation-unit context');

  LProvider := NewNyxNativePascalCompletionProvider(LHost.WithinMilliseconds(1));
  LResult := LProvider.Complete(LRequest);
  Check((LResult.Status = pcsFailed) and
    (Pos('time budget', LResult.MessageText) > 0),
    'Expired query retires and joins its exact process family before failure escapes');
  LHost := TNyxPascalCompletionHost.Using('', '', '', '', '', '');
  LResult := NewNyxNativePascalCompletionProvider(LHost).Complete(LRequest);
  Check((LResult.Status = pcsUnavailable) and (LResult.Count = 0),
    'An absent optional engine leaves a normal unavailable capability');
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' actual compiler-context completion checks');
  except
    on E: Exception do
    begin
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
    end;
  end;
end.
