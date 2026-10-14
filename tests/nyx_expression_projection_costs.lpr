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

program nyx_expression_projection_costs;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.state, nyx.expressions, nyx.binding,
  nyx.binding.types, nyx.model, nyx.controls, nyx.composition, nyx.codec,
  nyx.generated.view
  {$ifdef PAS2JS}, JS, Web{$else}, Classes{$endif};

const
  CConsumers = 32;
  CUpdates = 20;

function Clock: Double;
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

procedure Verify;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LState: TNyxState;
  LLive: TNyxLiveBindings;
  LLabel: INyxLabel;
  LCaption: INyxTextExpression;
  LProfile: TNyxExpressionProfile;
  LStarted: Double;
  LElapsed: Double;
  LBefore: TNyxText;
  LIndex: Integer;
  LUpdate: Integer;
  LRefused: Boolean;
  LRevision: Integer;
begin
  LDocument := BuildNyxDocument;
  LRoot := nil;
  LState := nil;
  LLive := nil;
  try
    LCaption := NyxTextExpression('Total: ').Append(
      NyxNumberExpression(NyxNumberState('price')).Times(
        NyxIntegerExpression(NyxIntegerState('quantity')).AsNumber)
        .Plus(NyxNumberExpression(NyxNumberState('tax'))).AsText);
    for LIndex := 0 to CConsumers - 1 do
    begin
      LLabel := NewNyxLabel('cost-label-' + TNyxText(IntToStr(LIndex)));
      LLabel.Binds.Text(LCaption).Done;
      LDocument.Pages[0].Add(LLabel);
    end;
    LDocument.Validate;
    LBefore := TNyxCodec.Encode(LDocument);
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LState := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LState);
    LLive.Activate;
    ResetNyxExpressionProfile;
    LStarted := Clock;
    for LUpdate := 1 to CUpdates do
    begin
      LState.SetValue(NyxIntegerState('quantity'), LUpdate);

      if LRoot.Find('cost-label-31').Prop('text') <>
        'Total: ' + TNyxStateValue.FromNumber(12.5 * LUpdate + 2.5).NumberText then
      begin
        raise Exception.Create('Measured complete projection has the wrong caption');
      end;
    end;
    LElapsed := Clock - LStarted;
    LProfile := ReadNyxExpressionProfile;

    if (LProfile.Evaluations > 2 * CConsumers * CUpdates) or
      (LProfile.Admissions > 5 * CConsumers * CUpdates) or
      (LProfile.InputFrames <> 2 * CUpdates) then
    begin
      raise Exception.Create('Complete staged/live projection exceeds its repeated-work budget');
    end;
    LRevision := LState.Revision;
    LRefused := False;
    try
      LState.Apply([NyxStateValue(NyxIntegerState('quantity'), 2),
        NyxStateValue(NyxNumberState('price'), 1.7e308)]);
    except
      on ENyxExpression do
      begin
        LRefused := True;
      end;
    end;

    if not LRefused or (LState.Revision <> LRevision) or
      (LRoot.Find('cost-label-31').Prop('text') <> 'Total: 252.5') or
      (TNyxCodec.Encode(LDocument) <> LBefore) then
    begin
      raise Exception.Create('Measured candidate failure must retain state/UI/defaults');
    end;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-expression-costs', 'passed');
    document.body.textContent :=
    {$else}
    WriteLn(
    {$endif}
      'PASS projection costs / consumers=' + IntToStr(CConsumers) +
      ' / updates=' + IntToStr(CUpdates) +
      ' / admissions=' + IntToStr(LProfile.Admissions) +
      ' / evaluations=' + IntToStr(LProfile.Evaluations) +
      ' / inputFrames=' + IntToStr(LProfile.InputFrames) +
      ' / milliseconds=' + IntToStr(Round(LElapsed))
      {$ifndef PAS2JS}){$endif};
  finally
    LLive.Free;
    LState.Free;
    ReleaseNyxNode(LRoot);
    LDocument.Free;
  end;
end;

begin
  try
    Verify;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-expression-costs', 'failed');
      document.body.setAttribute('data-expression-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
