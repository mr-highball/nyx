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
program nyx_focused_context_builds;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, fpjson, nyx.text, nyx.bytes, nyx.data, nyx.types,
  nyx.model, nyx.codec, nyx.codegen, nyx.composition, nyx.source,
  nyx.studio.outputs, nyx.studio.builds, nyx.studio.buildexecutor,
  nyx.studio.directories, nyx.studio.sourceprojection, nyx.studio.compiler,
  nyx.test.focused.context;

type
  { Synchronous compiler-only observer. Independent report values outlive each
    invocation; no renderer, source tree or runtime receiver is retained. }
  TFocusedCompilerProgress = class(TInterfacedObject, INyxCompilerProgress)
  private
    FLast: INyxCompilerReport;
    FRuntimeMessage: Boolean;
  public
    procedure Clear;
    procedure Publish(const AReport: INyxCompilerReport);
    property Last: INyxCompilerReport read FLast;
    property RuntimeMessage: Boolean read FRuntimeMessage;
  end;

procedure TFocusedCompilerProgress.Clear;
begin
  FLast := nil;
  FRuntimeMessage := False;
end;

procedure TFocusedCompilerProgress.Publish(const AReport: INyxCompilerReport);
var
  LIndex: Integer;
begin
  FLast := AReport;
  for LIndex := 0 to AReport.Count - 1 do
  begin
    FRuntimeMessage := FRuntimeMessage or
      (Pos('Runtime log only: source construction', AReport.Item(LIndex).Message) > 0);
  end;
end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ Read exact bytes only from the explicitly owned toolchain/job paths. UTF-8
  decoding belongs to this boundary, never an ANSI string collection. }
function ReadText(const APath: String): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;

procedure SaveText(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;

{ Insert fixture helpers without routing supplementary source through the ANSI
  RTL StringReplace overload. Copy preserves the portable UTF-8 text contract. }
function ReplaceOnce(const AText, ABefore, AAfter: TNyxText): TNyxText;
var
  LAt: Integer;
begin
  LAt := Pos(ABefore, AText);

  if LAt = 0 then
  begin
    raise Exception.Create('The exact owned source insertion point is absent');
  end;
  Result := Copy(AText, 1, LAt - 1) + AAfter + Copy(AText, LAt + Length(ABefore), MaxInt);
end;

procedure Run;
const
  CViews: array[0..3] of TNyxText = ('home', 'Wrapper', 'focused-memo', FocusedUnicodeID);
var
  LTools: TNyxDataValue;
  LOutputs: TNyxOutputConfiguration;
  LDirectories: TNyxStudioDirectories;
  LExecutor: TNyxBuildExecutor;
  LProgress: TFocusedCompilerProgress;
  LProgressOwner: INyxCompilerProgress;
  LCompilerReport: INyxCompilerReport;
  LDocument: TNyxDocument;
  LProjected: TNyxDocument;
  LExpected: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LFrame: TNyxSourceCheckpoint;
  LProjection: INyxSourceProjectionBuild;
  LResults: TJSONArray;
  LReply: TJSONObject;
  LBefore: TNyxText;
  LLiteral: TNyxText;
  LHandwritten: TNyxText;
  LHelper: TNyxText;
  LDirectory: TNyxText;
  LTarget: TNyxBuildTarget;
  LViewIndex: Integer;
  LFailed: Boolean;

  { The ordinary backend executor owns each admitted compiler job. Validate
    complete saved design and exact source policy, not only compiler exit zero. }
  procedure BuildAndCheck(ADocument: TNyxDocument; const AScope, AView, ASource: TNyxText;
    const AFrame: TNyxSourceCheckpoint);
  var
    LProgram: TNyxText;
  begin
    LProgress.Clear;
    LReply := LExecutor.Build(ADocument, NyxBuildTargetName(LTarget), AScope,
      AView, ASource, AFrame);
    try

      if not LReply.Get('ok', False) then
      begin
        raise Exception.Create('Focused compiler job failed: ' + LReply.Get('log', ''));
      end;
      Check(True, 'actual requested target compiler accepted the complete focus harness');
      LCompilerReport := DecodeNyxCompilerReport(TNyxText(LReply.Objects['diagnostics'].AsJSON));
      Check(((LCompilerReport.Count = 0) and (LProgress.Last = nil)) or
        ((LProgress.Last <> nil) and (LProgress.Last.Encode = LCompilerReport.Encode)),
        'real compiler progress reconstructs the exact final independent diagnostic report');
      LDirectory := LDirectories.Jobs + LReply.Get('build', '') + PathDelim;
      LExpected := nil;
      try

        if AScope = 'view' then
        begin
          LExpected := CloneNyxViewDocument(ADocument, ADocument.Find(AView));
        end
        else
        begin
          LExpected := ADocument.Clone;
        end;
        Check(ReadText(LDirectory + 'design.nyx') = TNyxCodec.Encode(LExpected),
          'compiler job retains the exact complete independently focused design');

        if LTarget = btBrowser then
        begin
          LProgram := ReadText(LDirectory + 'nyx_preview.lpr');
        end
        else
        begin
          LProgram := ReadText(LDirectory + 'nyx_native.lpr');
        end;
        Check((Pos('ConfigureView(TNyxApplicationView.Focused(', LProgram) > 0) =
          (AScope = 'view'), 'only view jobs explicitly select focus in the generated entry point');

        if AFrame.Origin = nsoExecuted then
        begin
          Check(ReadText(LDirectory + 'nyx.generated.view.pas') = ASource,
            'executed source retains its exact handwritten helper and builder bytes');
          Check(Pos('CloneNyxViewDocument(LConstruction, LView)', LProgram) > 0,
            'executed view constructs the original application once before isolation');
        end;
      finally
        FreeAndNil(LExpected);
      end;
      LResults.Add(LReply);
      LReply := nil;
    finally
      LReply.Free;
    end;
  end;

begin

  if (ParamCount <> 4) or DirectoryExists(ParamStr(3)) or FileExists(ParamStr(3)) then
  begin
    raise Exception.Create('Supply repository, existing toolchain, NEW owned runtime and native platform');
  end;
  LTools := TNyxDataValue.ParseJSON(ReadText(ParamStr(2)));
  LDirectories := TNyxStudioDirectories.ForRepository(ParamStr(1))
    .RunningIn(ParamStr(3)).EnrollingProject(ParamStr(3));
  LOutputs := TNyxOutputConfiguration.Create;
  LExecutor := nil;
  LDocument := nil;
  LProjected := nil;
  LExpected := nil;
  LWorkspace := nil;
  LResults := TJSONArray.Create;
  try
    LOutputs.SetField('fpc', LTools.Field('LCL_FPC').AsText);
    LOutputs.SetField('lazarus', LTools.Field('LAZARUS').AsText);
    LOutputs.SetField('platform', ParamStr(4));
    LOutputs.SetField('widgetset', 'win32');
    LOutputs.SetField('pas2js', LTools.Field('PAS2JS').AsText);
    LOutputs.SetField('runtime', LTools.Field('PAS2JS_RUNTIME').AsText);
    LExecutor := TNyxBuildExecutor.Create(LDirectories, LOutputs.Encode);
    LProgress := TFocusedCompilerProgress.Create;
    LProgressOwner := LProgress;
    LExecutor.ConfigureProgress(LProgressOwner);
    LDocument := FocusedContextDocument;
    LBefore := TNyxCodec.Encode(LDocument);
    LLiteral := TNyxCodegen.Generate(LDocument);
    LFrame := Default(TNyxSourceCheckpoint);
    for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
    begin
      for LViewIndex := Low(CViews) to High(CViews) do
      begin
        BuildAndCheck(LDocument, 'view', CViews[LViewIndex], LLiteral, LFrame);
      end;
      BuildAndCheck(LDocument, 'application', '', LLiteral, LFrame);
    end;
    LHandwritten := ReplaceOnce(LLiteral, '''Focused workspace''', 'FocusedCaption');
    Check(LHandwritten <> LLiteral, 'handwritten fixture replaces the exact generated title expression');
    LHelper := 'var' + #10 + '  GConstructions: Integer;' + #10#10 +
      '{ Called once by each actual complete construction. }' + #10 +
      'function FocusedCaption: TNyxText;' + #10 +
      'begin' + #10 + '  Inc(GConstructions);' + #10 +
      '  WriteLn(''Warning: Runtime log only: source construction'');' + #10#10 +
      '  if GConstructions <> 1 then' + #10 +
      '  begin' + #10 +
      '    raise ENyxModel.Create(''Construct the application only once'');' + #10 +
      '  end;' + #10 + '  Result := ''Focused workspace'';' + #10 + 'end;' + #10;
    LHandwritten := ReplaceOnce(LHandwritten, #10 + 'implementation' + #10,
      #10 + 'implementation' + #10#10 + LHelper);
    LProgress.Clear;
    LProjection := LExecutor.ProjectSource(LHandwritten, NyxPascalUnit('nyx.generated.view'),
      btNativeLCL, spcChecked);
    Check((LProjection.Projection.State = spsExecuted) and
      (LProjection.Projection.Design = LBefore), 'actual checked handwritten constructor reproduces exact accepted design');
    Check(Pos('0 unfreed memory blocks', LProjection.RuntimeLog) > 0,
      'executed constructor retires its complete owned source tree');
    Check(((LProjection.Projection.Report.Count = 0) and (LProgress.Last = nil)) or
      ((LProgress.Last <> nil) and
        (LProgress.Last.Encode = LProjection.Projection.Report.Encode)),
      'native constructor compilation preserves exact streamed compiler diagnostics');
    Check((Pos('Warning: Runtime log only: source construction', LProjection.RuntimeLog) > 0) and
      not LProgress.RuntimeMessage, 'constructor stdout cannot impersonate a compiler message');
    LProjected := LProjection.Projection.CopyDocument;
    LWorkspace := TNyxSourceWorkspace.Create;
    LWorkspace.AcceptExecuted(LProjected, LHandwritten);
    LFrame := LWorkspace.Capture(LProjected);
    for LTarget := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
    begin
      BuildAndCheck(LProjected, 'view', 'Wrapper', LHandwritten, LFrame);
    end;
    LFailed := False;
    try
      LReply := LExecutor.Build(LProjected, 'browser', 'view', 'Wrapper',
        LHandwritten + #10 + '{ unfinished draft }', LFrame);
      LReply.Free;
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LWorkspace.Capture.Source = LHandwritten) and
      (TNyxCodec.Encode(LDocument) = LBefore) and (TNyxCodec.Encode(LProjected) = LBefore),
      'mismatched accepted source refuses while both complete sources/checkpoints remain exact');
    SaveText(IncludeTrailingPathDelimiter(ParamStr(3)) + 'focused-builds.json', LResults.AsJSON);
  finally
    LProjection := nil;
    LResults.Free;
    LWorkspace.Free;
    LExpected.Free;
    LProjected.Free;
    LDocument.Free;
    LExecutor.Free;
    LProgressOwner := nil;
    LOutputs.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' actual focused/application compiler checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
