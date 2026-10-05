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

program nyx_source_benchmark;

{$mode delphi}{$H+}
{$codepage utf8}

{ Correctness-gated source workspace measurements use the same public session on
  FPC and pas2js. Fixture construction and checks are outside timed operations.
  Each size preserves crafted locals/Unicode comments/unchanged expressions and
  exact paired undo/redo, including a rejected retained draft. These timings cover
  portable document/source work, not painting, OS input or compiler latency. }

uses
  SysUtils,
  {$ifdef PAS2JS}
  Web,
  {$else}
  Classes, nyx.studio.sourcejobs,
  {$endif}
  nyx.text,
  nyx.types,
  nyx.controls,
  nyx.model,
  nyx.codec,
  nyx.codegen,
  nyx.source,
  nyx.studio.session;

const
  SourceBenchmarkControlsPrefix = '?controls=';
  SourceScheduledArgument = '--scheduled';

var
  GScheduled: Boolean;

function Clock: Double;
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

{$ifndef PAS2JS}
{ Use the same admitted command as the actual native editor. The original sizes,
  source, visual/structural commands and preservation gates stay unchanged.
  Submission excludes worker admission; total Apply still includes publication.
  Pulses witness a serviced main loop, not canvas painting or usable input. }
function ScheduledApply(ASession: TNyxStudioSession;
  out ASubmit: Double; out APulses: Integer): TNyxSourceCommandState;
var
  LCommands: TNyxSourceCommands;
  LStarted: Double;
begin
  LCommands := TNyxSourceCommands.Create(ASession, nil);
  try
    LStarted := Clock;
    LCommands.Apply;
    ASubmit := Clock - LStarted;
    APulses := 0;
    while LCommands.State = nssPreparing do
    begin
      CheckSynchronize;
      Inc(APulses);

      if Clock - LStarted > 30000 then
      begin
        raise ENyxModel.Create('Scheduled source benchmark exceeded its deadline');
      end;
      Sleep(1);
    end;
    Result := LCommands.State;
  finally
    LCommands.Free;
  end;
end;
{$endif}

procedure Require(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Source benchmark: ' + AMessage);
  end;
end;

function ReplaceText(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition, LStart: Integer;
  LParts: TNyxStrings;
begin
  LStart := 1;
  LParts := TNyxStrings.Create;
  try
    repeat
      LPosition := Pos(ABefore, Copy(ASource, LStart, Length(ASource)));

      if LPosition = 0 then
      begin
        LParts.Add(Copy(ASource, LStart, Length(ASource)));
        Break;
      end;
      Inc(LPosition, LStart - 1);
      LParts.Add(Copy(ASource, LStart, LPosition - LStart));
      LParts.Add(AAfter);
      LStart := LPosition + Length(ABefore);
    until False;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function UTF8Bytes(const AText: TNyxText): Integer;
var
  LIndex, LScalar: Integer;
begin
  Result := 0;
  LIndex := 1;
  while LIndex <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise Exception.Create('Benchmark source must be valid Unicode');
    end;

    if LScalar < $80 then
    begin
      Inc(Result);
    end
    else if LScalar < $800 then
    begin
      Inc(Result, 2);
    end
    else if LScalar < $10000 then
    begin
      Inc(Result, 3);
    end
    else
    begin
      Inc(Result, 4);
    end;
  end;
end;

function Number(AMilliseconds: Double): TNyxText;
var
  LFormat: TFormatSettings;
begin
  {$ifdef PAS2JS}
  LFormat := TFormatSettings.Invariant;
  {$else}
  LFormat := DefaultFormatSettings;
  {$endif}
  LFormat.DecimalSeparator := '.';
  Result := FloatToStrF(AMilliseconds, ffFixed, 12, 3, LFormat);
end;

{$ifdef NYX_SOURCE_PROFILE}
function ProfileText(AControls: Integer; const AOperation: TNyxText): TNyxText;
const
  StageNames: array[TNyxSourceProfileStage] of TNyxText = (
    'old-generation', 'symbols', 'names', 'legacy', 'prune', 'scaffold',
    'partition', 'metadata-split', 'metadata-merge', 'frame-merge', 'verify',
    'lex', 'symbol-read', 'checkpoint', 'commit', 'save', 'restore-document',
    'snapshot', 'restore-workspace', 'render', 'render-encode', 'candidate',
    'candidate-validate', 'candidate-encode');
var
  LStage: TNyxSourceProfileStage;
  LSamples: TNyxSourceProfileSamples;
  LParts: TNyxStrings;
begin
  LSamples := ReadNyxSourceProfile;
  LParts := TNyxStrings.Create;
  try
    for LStage := Low(TNyxSourceProfileStage) to High(TNyxSourceProfileStage) do
    begin
      LParts.Add('profile,' + IntToStr(AControls) + ',' + AOperation + ',' +
        StageNames[LStage] + ',' + Number(LSamples[LStage].Milliseconds) + ',' +
        IntToStr(LSamples[LStage].Calls) + #10);
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;
{$endif}

function Measure(AControls: Integer): TNyxText;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LSession: TNyxStudioSession;
  LSource, LCrafted, LBefore, LBeforeDesign, LDraft: TNyxText;
  LIndex, LBytes: Integer;
  LStart, LGenerate, LApply, LVisual, LStructural, LReject, LHistory: Double;
  LRejected: Boolean;
  {$ifndef PAS2JS}
  LApplySubmit, LRejectSubmit: Double;
  LApplyPulses, LRejectPulses: Integer;
  {$endif}
  {$ifdef NYX_SOURCE_PROFILE}
  LVisualProfile: TNyxText;
  LStructuralProfile: TNyxText;
  LApplyProfile: TNyxText;
  LHistoryProfile: TNyxText;
  {$endif}
begin
  LDocument := TNyxDocument.Create;
  LSession := TNyxStudioSession.Create;
  try
    LDocument.Title := 'Source workspace';
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxLabel('message').WithText('Before edit'));
    LPage.Add(NewNyxLabel('stable').WithText('Stable caption'));
    for LIndex := 2 to AControls - 1 do
    begin
      LPage.Add(NewNyxLabel('caption-' + IntToStr(LIndex))
        .WithText('Caption ' + IntToStr(LIndex)));
    end;
    LDocument.AddPage(LPage);
    LStart := Clock;
    LSource := TNyxCodegen.Generate(LDocument);
    LGenerate := Clock - LStart;
    Require(Pos('LMessageLabel', LSource) > 0, 'purposeful local exists');
    LCrafted := ReplaceText(LSource, 'LMessageLabel', 'LMessageCaption');
    LCrafted := ReplaceText(LCrafted, 'LMessageCaption :=',
      '{ Keep this note / 🌙 / 漢字 }' + #10 + '    LMessageCaption :=');
    LCrafted := ReplaceText(LCrafted, '''Stable caption''', '''Stable '' + ''caption''');
    LSession.Load(TNyxCodec.Encode(LDocument));
    LSession.SetSourceDraft(LCrafted);
    {$ifdef NYX_SOURCE_PROFILE}ResetNyxSourceProfile(@Clock);{$endif}
    LStart := Clock;
    {$ifndef PAS2JS}

    if GScheduled then
    begin
      Require(ScheduledApply(LSession, LApplySubmit, LApplyPulses) = nssApplied,
        'scheduled admission publishes a complete pair');
    end
    else
    {$endif}
    begin
      LSession.ApplySourceDraft;
    end;
    LApply := Clock - LStart;
    {$ifdef NYX_SOURCE_PROFILE}LApplyProfile := ProfileText(AControls, 'apply');{$endif}
    Require((LSession.Source = LCrafted) and (LSession.Save = TNyxCodec.Encode(LDocument)),
      'crafted Apply publishes the exact matching pair');

    LSession.Select('message');
    {$ifdef NYX_SOURCE_PROFILE}ResetNyxSourceProfile(@Clock);{$endif}
    LStart := Clock;
    LSession.SetProperty('text', 'After edit');
    LVisual := Clock - LStart;
    {$ifdef NYX_SOURCE_PROFILE}LVisualProfile := ProfileText(AControls, 'visual');{$endif}
    LBefore := LSession.Source;
    Require((Pos('Keep this note / 🌙 / 漢字', LBefore) > 0) and
      (Pos('LMessageCaption: INyxLabel', LBefore) > 0) and
      (Pos('''Stable '' + ''caption''', LBefore) > 0),
      'visual change preserves authored names/comments/unchanged expression');

    LSession.Select('home');
    {$ifdef NYX_SOURCE_PROFILE}ResetNyxSourceProfile(@Clock);{$endif}
    LStart := Clock;
    LSession.AddControl(NewNyxButton('added-action').WithText('Continue'));
    LStructural := Clock - LStart;
    {$ifdef NYX_SOURCE_PROFILE}LStructuralProfile := ProfileText(AControls, 'structural');{$endif}
    Require((LSession.Document.Find('added-action') <> nil) and
      (Pos('Keep this note / 🌙 / 漢字', LSession.Source) > 0),
      'structural edit retains crafted source');

    LBefore := LSession.Source;
    LBeforeDesign := LSession.Save;
    LSession.Undo;
    {$ifdef NYX_SOURCE_PROFILE}ResetNyxSourceProfile(@Clock);{$endif}
    LStart := Clock;
    LSession.Redo;
    LSession.Undo;
    LSession.Redo;
    LHistory := Clock - LStart;
    {$ifdef NYX_SOURCE_PROFILE}LHistoryProfile := ProfileText(AControls, 'history');{$endif}
    Require((LSession.Source = LBefore) and (LSession.Save = LBeforeDesign),
      'history restores the exact source/design pair');

    LDraft := ReplaceText(LBefore, '''After edit''', 'False');
    Require(LDraft <> LBefore, 'wrong-type draft changes an existing expression');
    LSession.SetSourceDraft(LDraft);
    LRejected := False;
    LStart := Clock;
    try
      {$ifndef PAS2JS}

      if GScheduled then
      begin
        LRejected := ScheduledApply(LSession, LRejectSubmit, LRejectPulses) = nssRejected;
      end
      else
      {$endif}
      begin
        LSession.ApplySourceDraft;
      end;
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    LReject := Clock - LStart;
    Require(LRejected and (LSession.Source = LBefore) and
      (LSession.Save = LBeforeDesign) and (LSession.DraftSource = LDraft) and
      LSession.SourceDiagnostic.Defined and (LSession.SourceDiagnostic.Line > 1),
      'rejected typed draft retains exact pair and navigable diagnostic');
    LBytes := UTF8Bytes(LBefore);
    Result := IntToStr(AControls) + ',' + IntToStr(LBytes) + ',' + Number(LGenerate) + ',' +
      Number(LApply) + ',' + Number(LVisual) + ',' + Number(LStructural) + ',' +
      Number(LReject) + ',' + Number(LHistory);
    {$ifndef PAS2JS}

    if GScheduled then
    begin
      Result := Result + ',' + Number(LApplySubmit) + ',' + Number(LRejectSubmit) +
        ',' + IntToStr(LApplyPulses);
    end;
    {$endif}
    {$ifdef NYX_SOURCE_PROFILE}
    Result := Result + #10 + LApplyProfile + LVisualProfile + LStructuralProfile + LHistoryProfile;
    {$endif}
  finally
    LSession.Free;
    LPage := nil;
    LDocument.Free;
  end;
end;

var
  LCSV: TNyxText;
  LControls: Integer;

procedure Append(AControls: Integer);
var
  LRow: TNyxText;
begin
  LRow := Measure(AControls) + #10;
  LCSV := LCSV + LRow;
  {$ifdef PAS2JS}
  document.body.textContent := LCSV;
  {$else}
  Write(LRow);
  Flush(Output);
  {$endif}
end;

begin
  try
    LCSV := 'controls,source_utf8_bytes,generate_ms,apply_ms,visual_ms,structural_ms,reject_ms,history_ms' + #10;
    LControls := 0;
    {$ifdef PAS2JS}

    if window.location.search <> '' then
    begin

      if Copy(window.location.search, 1, Length(SourceBenchmarkControlsPrefix)) <>
        SourceBenchmarkControlsPrefix then
      begin
        raise EArgumentException.Create('Use ?controls=128, 512 or 2048');
      end;
      LControls := StrToInt(Copy(window.location.search, 11, MaxInt));
    end;
    {$else}
    GScheduled := (ParamCount > 0) and (ParamStr(1) = SourceScheduledArgument);

    if GScheduled then
    begin
      LCSV := Copy(LCSV, 1, Length(LCSV) - 1) +
        ',apply_submit_ms,reject_submit_ms,apply_main_loop_pulses' + #10;
    end;
    Write(LCSV);
    Flush(Output);

    if GScheduled then
    begin

      if ParamCount > 1 then
      begin
        LControls := StrToInt(ParamStr(2));
      end;
    end
    else if ParamCount > 0 then
    begin
      LControls := StrToInt(ParamStr(1));
    end;
    {$endif}

    if (LControls <> 0) and (LControls <> 128) and (LControls <> 512) and (LControls <> 2048) then
    begin
      raise EArgumentException.Create('Measured sizes are 128, 512 and 2048 controls');
    end;

    if LControls = 0 then
    begin
      Append(128);
      Append(512);
      Append(2048);
    end
    else
    begin
      Append(LControls);
    end;
    {$ifdef PAS2JS}
    document.body.textContent := LCSV;
    document.body.setAttribute('data-source-benchmark', 'passed');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-source-benchmark', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      Halt(1);
      {$endif}
    end;
  end;
end.
