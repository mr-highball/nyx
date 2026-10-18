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
program nyx_pascal_bootstrap_native;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.pascal.context, nyx.pascal.compilertrace,
  nyx.studio.pascalprofiles;

const
  CNames: array[0..8] of TNyxText = ('Char', 'AnsiChar', 'WideChar', 'Comp',
    'Integer', 'LongInt', 'Pointer', 'NativeInt', 'UTF8String');
var
  GChecks: Integer;
  GProbe: INyxPascalCompilerProbe;
  GContext: INyxPascalCompilerContext;
  GAt: Integer;
  GFact: TNyxPascalSizeFact;
  GTranscript: TNyxText;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
end;

{ The host retained this exact ASCII numbered compiler transcript in the owned
  fresh directory. Byte reads avoid a native ANSI collection/codepage conversion. }
function ReadTranscript(const APath: TNyxText): TNyxText;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(String(APath), fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LStream.Size);

    if Length(Result) > 0 then
    begin
      LStream.ReadBuffer(Result[1], Length(Result));
    end;
  finally
    LStream.Free;
  end;
end;

procedure Refuses(const AProbe: INyxPascalCompilerProbe;
  const ATranscript: TNyxText; AExitCode: Integer; const AReason: String);
var
  LContext: INyxPascalCompilerContext;
  LRejected: Boolean;
begin
  LRejected := False;
  try
    LContext := ReadNyxFPCCompilerContext(AProbe, 'system.pp', ATranscript,
      AExitCode, TNyxPascalCompilerTraceOptions.Defaults);
  except
    on ENyxPascalContext do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LContext = nil), AReason);
end;

procedure TranscriptGuards;
var
  LStop: TNyxText;
  LAt: Integer;
  LNormal: INyxPascalCompilerProbe;
begin
  LStop := 'NYX_CONTEXT_' + GProbe.Identity.Name + '_BOOTSTRAP_STOP';
  GTranscript := ReadTranscript(TNyxText(ParamStr(2)) + '/compiler-profile.log');
  LAt := Pos(LStop, GTranscript);
  Check(LAt > 0, 'The retained compiler transcript contains its exact stop identity');
  Check(ReadNyxFPCCompilerContext(GProbe, 'system.pp', GTranscript, 1,
    TNyxPascalCompilerTraceOptions.Defaults).SameFacts(GContext),
    'Replaying the exact joined intentional-stop transcript retains every fact');
  Refuses(GProbe, GTranscript, 0, 'Bootstrap facts require the intentional nonzero exit');
  Refuses(GProbe, GTranscript, 2, 'An unrelated compiler failure cannot publish root facts');
  Refuses(GProbe, StringReplace(GTranscript, String(LStop), 'FOREIGN_STOP', [rfReplaceAll]), 1,
    'A foreign fatal marker cannot complete the owned packet');
  Refuses(GProbe, StringReplace(GTranscript, 'system.pp(', 'foreign.pp(', [rfReplaceAll]), 1,
    'A different source file cannot supply owned bootstrap notes or termination');
  Refuses(GProbe, GTranscript + #10 + 'system.pp(1,1) Fatal: (2022) User defined: ' + LStop, 1,
    'A duplicate terminal marker refuses the complete context');
  Refuses(GProbe, StringReplace(GTranscript, 'Fatal: (1018) Compilation aborted', '', []), 1,
    'Missing compiler abort confirmation refuses the complete context');
  Refuses(GProbe, GTranscript + #10 + 'system.pp(1,1) Error: (5000) unexpected', 1,
    'Additional actual compiler errors refuse even after the expected stop');
  Refuses(GProbe, Copy(GTranscript, 1, LAt - 1), 1,
    'A truncated compiler stream cannot supply partial primitive facts');
  LNormal := NewNyxFPCCompilerProbe(NyxPascalDefine('BOOTSTRAP_TYPES'), ppmFPC);
  for LAt := 0 to High(CNames) do
  begin
    LNormal := NyxFPCProbeSizeOf(LNormal, NyxPascalSizeReference(CNames[LAt]));
  end;
  Refuses(LNormal, GTranscript, 1,
    'An ordinary successful-program request cannot admit bootstrap failure evidence');
end;

begin
  try
    Check(ParamCount = 2, 'Supply configured FPC and a fresh owned output directory');
    GProbe := NewNyxFPCSystemBootstrapProbe(NyxPascalDefine('BOOTSTRAP_TYPES'));
    for GAt := 0 to High(CNames) do
    begin
      GProbe := NyxFPCProbeSizeOf(GProbe, NyxPascalSizeReference(CNames[GAt]));
    end;
    GContext := CaptureNyxFPCCompilerContext(TNyxText(ParamStr(1)), TNyxText(ParamStr(2)),
      GProbe, TNyxPascalCompilerTraceOptions.Defaults);
    Check(NyxPascalCompilerScope(GContext) = pcsoSystemBootstrap,
      'A complete intentional-stop packet retains compiler bootstrap provenance');
    for GAt := 0 to High(CNames) do
    begin
      GFact := NyxPascalCapturedSize(GContext, NyxPascalSizeReference(CNames[GAt]));
      WriteLn('bootstrap / ', CNames[GAt], ' state=', Ord(GFact.State));

      if GFact.State = pssKnown then
      begin
        WriteLn('bootstrap bytes / ', CNames[GAt], ' / ', GFact.Bytes);
      end;
    end;
    Check(NyxPascalCapturedSize(GContext, NyxPascalSizeReference('AnsiChar')).Bytes = SizeOf(AnsiChar),
      'Actual bootstrap AnsiChar agrees with the configured native compiler');
    Check(NyxPascalCapturedSize(GContext, NyxPascalSizeReference('WideChar')).Bytes = SizeOf(WideChar),
      'Actual bootstrap WideChar agrees with the configured native compiler');
    Check(CaptureNyxPascalCompilerContext(GContext).SameFacts(GContext),
      'Freezing retains the separate evidence namespace and exact packet');
    Check(not FileExists(ParamStr(2) + '/system.ppu'),
      'The bootstrap probe emits no replacement System unit');
    TranscriptGuards;
    WriteLn('PASS ', GChecks, ' native compiler bootstrap checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
    end;
  end;
end.
