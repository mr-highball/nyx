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

program nyx_output_reconciliation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.model, nyx.studio.outputs
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure SetSetting(AProfile: TNyxOutputConfiguration; ASetting: TNyxOutputSetting;
  AValue: Integer);
const
  CPrefix: TNyxText = 'setting-🌙-';
begin
  case ASetting of
    nosFileSaving:
      begin
        AProfile.AllowFileSaving(Odd(AValue));
      end;
    nosFileOpening:
      begin
        AProfile.AllowFileOpening(Odd(AValue));
      end;
    else
      begin
        AProfile.SetField(NyxOutputFields[Ord(ASetting)], CPrefix + TNyxText(IntToStr(AValue)));
      end;
  end;
end;

function Setting(AProfile: TNyxOutputConfiguration; ASetting: TNyxOutputSetting): TNyxText;
begin
  case ASetting of
    nosFileSaving:
      begin
        Result := BoolToStr(AProfile.FileSaving, True);
      end;
    nosFileOpening:
      begin
        Result := BoolToStr(AProfile.FileOpening, True);
      end;
    else
      begin
        Result := AProfile.Field(NyxOutputFields[Ord(ASetting)]);
      end;
  end;
end;

procedure Qualify(AChanged: TNyxOutputSetting);
var
  LCurrent: TNyxOutputConfiguration;
  LLoaded: TNyxOutputConfiguration;
  LSubmitted: TNyxOutputConfiguration;
  LEdits: TNyxOutputEditState;
  LSetting: TNyxOutputSetting;
  LOriginal: TNyxText;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LCurrent := TNyxOutputConfiguration.Create;
  LLoaded := TNyxOutputConfiguration.Create;
  LSubmitted := nil;
  LEdits := TNyxOutputEditState.Create;
  try
    for LSetting := Low(TNyxOutputSetting) to High(TNyxOutputSetting) do
    begin
      SetSetting(LCurrent, LSetting, 0);
      SetSetting(LLoaded, LSetting, 1);
      LEdits.Mark(LSetting);
    end;
    LOriginal := Setting(LCurrent, AChanged);
    LEdits.BeginRequest(osiReload, LCurrent);
    SetSetting(LCurrent, AChanged, 1);
    LEdits.Mark(AChanged);
    SetSetting(LCurrent, AChanged, 0);
    LEdits.Mark(AChanged);
    LEdits.Accept(LCurrent, LLoaded);
    Check(LEdits.Dirty and not LEdits.Pending,
      'successful reload preserves newer edit intent even after a changed-back value');
    for LSetting := Low(TNyxOutputSetting) to High(TNyxOutputSetting) do
    begin

      if LSetting = AChanged then
      begin
        Check(Setting(LCurrent, LSetting) = LOriginal, 'reload retains the exact newer local value');
      end
      else
      begin
        Check(Setting(LCurrent, LSetting) = Setting(LLoaded, LSetting),
          'reload adopts each untouched saved setting despite preexisting dirty intent');
      end;
    end;

    LEdits.BeginRequest(osiSave, LCurrent);
    LSubmitted := TNyxOutputConfiguration.Decode(LEdits.Sent);
    SetSetting(LCurrent, AChanged, 1);
    LEdits.Mark(AChanged);
    SetSetting(LCurrent, AChanged, 0);
    LEdits.Mark(AChanged);
    LEdits.Accept(LCurrent, LSubmitted);
    Check(LEdits.Dirty and (LCurrent.Encode = LSubmitted.Encode),
      'exact save acknowledgment cannot erase same-value edits made in flight');
    LEdits.BeginRequest(osiSave, LCurrent);
    LEdits.Accept(LCurrent, LSubmitted);
    Check(not LEdits.Dirty, 'a later explicit Apply acknowledges the retained field intent');
    FreeAndNil(LSubmitted);

    SetSetting(LCurrent, AChanged, 0);
    LEdits.Mark(AChanged);
    for LSetting := Low(TNyxOutputSetting) to High(TNyxOutputSetting) do
    begin

      if LSetting <> AChanged then
      begin
        SetSetting(LLoaded, LSetting, 2);
      end;
    end;
    LEdits.BeginRequest(osiObserve, LCurrent);
    LEdits.Accept(LCurrent, LLoaded);
    Check(LEdits.Dirty and (Setting(LCurrent, AChanged) = LOriginal),
      'ordinary observation retains dirty fields while loading independent clean fields');
    for LSetting := Low(TNyxOutputSetting) to High(TNyxOutputSetting) do
    begin

      if LSetting <> AChanged then
      begin
        Check(Setting(LCurrent, LSetting) = Setting(LLoaded, LSetting),
          'ordinary observation actually updates each independent clean field');
      end;
    end;
    LBefore := LCurrent.Encode;
    LEdits.BeginRequest(osiReload, LCurrent);
    LEdits.AbortRequest;
    Check(LEdits.Dirty and not LEdits.Pending and (LCurrent.Encode = LBefore),
      'failed or cancelled reload retains values and prior dirty state');

    LEdits.BeginRequest(osiSave, LCurrent);
    LRefused := False;
    try
      LEdits.Accept(LCurrent, LLoaded);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LEdits.Pending and LEdits.Dirty and (LCurrent.Encode = LBefore),
      'a different server snapshot cannot falsely acknowledge a save');
    LRefused := False;
    try
      LEdits.BeginRequest(osiReload, LCurrent);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LEdits.Intent = osiSave) and (LEdits.Sent = LBefore),
      'overlapping request refusal preserves the exact original pending intent');
    LEdits.AbortRequest;
    LRefused := False;
    try
      LEdits.MarkField('unknown-setting');
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and LEdits.Dirty and (LCurrent.Encode = LBefore),
      'unknown transport field names grant no new typed setting');
  finally
    LEdits.Free;
    LSubmitted.Free;
    LLoaded.Free;
    LCurrent.Free;
  end;
end;

procedure Run;
var
  LSetting: TNyxOutputSetting;
begin
  for LSetting := Low(TNyxOutputSetting) to High(TNyxOutputSetting) do
  begin
    Qualify(LSetting);
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-output-reconciliation', 'passed');
    document.body.setAttribute('data-output-reconciliation-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' output reconciliation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-output-reconciliation', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
