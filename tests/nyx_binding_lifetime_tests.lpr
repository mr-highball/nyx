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

program nyx_binding_lifetime_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils,
  nyx.text,
  nyx.state,
  nyx.binding.lifetime
  {$ifdef PAS2JS}, Web{$endif};

type
  { This receiver is owned by each case and outlives every raw state and holder.
    It observes real virtual state destruction, never a manufactured release. }
  TOwnedStateCounts = class
  public
    Created: Integer;
    Destroyed: Integer;
  end;

  { A real mutable state whose virtual destructor records the holder's final
    release. Its borrowed counter must outlive every lease in the test case. }
  TCountedBindingState = class(TNyxState)
  private
    FCounts: TOwnedStateCounts;
  public
    constructor Create(ACounts: TOwnedStateCounts);
    destructor Destroy; override;
  end;

var
  GChecks: Integer;

constructor TCountedBindingState.Create(ACounts: TOwnedStateCounts);
begin
  inherited Create;
  FCounts := ACounts;
  Inc(FCounts.Created);
end;

destructor TCountedBindingState.Destroy;
begin
  Inc(FCounts.Destroyed);
  inherited Destroy;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxState.Create('Binding owned-state lifetime: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure SelfAssignOwner(var AOwner: INyxBindingOwnedState);
begin
  { The source/destination are deliberately the sole surviving same slot. }
  AOwner := AOwner;
end;

procedure SharingCases;
var
  LCounts: TOwnedStateCounts;
  LRaw, LBorrowed: TNyxState;
  LAccepted, LEntered, LSuccessor, LCopy: INyxBindingOwnedState;
  LRevision: Integer;
begin
  LCounts := TOwnedStateCounts.Create;
  LRaw := nil;
  try
    LRaw := TCountedBindingState.Create(LCounts);
    LRaw.SetValue(NyxTextState('value'), 'Original owned value / 🌙');
    LBorrowed := LRaw;
    LAccepted := OwnNyxBindingState(LRaw);
    LRaw := nil;
    Check((LAccepted.State = LBorrowed) and (LCounts.Created = 1) and
      (LCounts.Destroyed = 0),
      'successful adoption retains the exact state without cloning or destroying it');
    LEntered := LAccepted;
    LSuccessor := LAccepted;
    LCopy := LEntered;
    Check((LEntered = LAccepted) and (LSuccessor = LAccepted) and
      (LCopy = LAccepted) and (LSuccessor.State = LBorrowed),
      'entered and successor generations explicitly share the SAME managed owner');
    LAccepted := nil;
    LSuccessor := nil;
    LCopy := nil;
    Check((LCounts.Destroyed = 0) and
      (LEntered.State.GetValue(NyxTextState('value')) = TNyxText('Original owned value / 🌙')),
      'old entered holder keeps the store alive after accepted and successor owners clear');
    SelfAssignOwner(LEntered);
    Check((LEntered <> nil) and (LEntered.State = LBorrowed) and
      (LCounts.Destroyed = 0),
      'self-assignment preserves the sole remaining exact owner');
    LRevision := LEntered.State.Revision;
    LEntered.State.SetValue(NyxTextState('value'), 'Still usable after successor retirement');
    Check((LEntered.State = LBorrowed) and (LEntered.State.Revision = LRevision + 1) and
      (LEntered.State.GetValue(NyxTextState('value')) = 'Still usable after successor retirement'),
      'surviving holder exposes the same ordinary mutable store and revision contract');
    LEntered := nil;
    LBorrowed := nil;
    Check(LCounts.Destroyed = 1,
      'final entered holder release destroys its state exactly once');
    LEntered := nil;
    LAccepted := nil;
    LSuccessor := nil;
    Check(LCounts.Destroyed = 1,
      'repeated nil assignment cannot destroy the state again');
  finally
    LAccepted := nil;
    LSuccessor := nil;
    LCopy := nil;
    LEntered := nil;
    LRaw.Free;
    LCounts.Free;
  end;
end;

procedure NilRefusalCases;
var
  LCounts: TOwnedStateCounts;
  LCaller, LRaw: TNyxState;
  LOwner: INyxBindingOwnedState;
  LRefused: Boolean;
begin
  LCounts := TOwnedStateCounts.Create;
  LCaller := nil;
  LRaw := nil;
  try
    LCaller := TCountedBindingState.Create(LCounts);
    LCaller.SetValue(NyxTextState('caller'), 'Caller still owns this state');
    LRaw := TCountedBindingState.Create(LCounts);
    LRaw.SetValue(NyxTextState('accepted'), 'Existing holder survives refusal');
    LOwner := OwnNyxBindingState(LRaw);
    LRaw := nil;
    LRefused := False;
    try
      LOwner := OwnNyxBindingState(nil);
    except
      on LException: ENyxState do
      begin
        LRefused := Pos('requires an actual state', LException.Message) > 0;
      end;
    end;
    Check(LRefused and (LOwner <> nil) and (LCounts.Destroyed = 0) and
      (LOwner.State.GetValue(NyxTextState('accepted')) = 'Existing holder survives refusal'),
      'nil factory refusal preserves the previously assigned holder and its state');
    Check((LCaller.GetValue(NyxTextState('caller')) = 'Caller still owns this state') and
      (LCounts.Created = 2) and (LCounts.Destroyed = 0),
      'refusal adopts or releases no independently caller-owned state');
    LOwner := nil;
    Check((LCounts.Destroyed = 1) and
      (LCaller.GetValue(NyxTextState('caller')) = 'Caller still owns this state'),
      'accepted holder release frees only its explicitly adopted state');
    FreeAndNil(LCaller);
    Check(LCounts.Destroyed = 2,
      'unadopted state remains the raw caller responsibility and frees exactly once');
  finally
    LOwner := nil;
    LRaw.Free;
    LCaller.Free;
    LCounts.Free;
  end;
end;

begin
  try
    SharingCases;
    NilRefusalCases;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' binding owned-state lifetime checks';
    document.body.setAttribute('data-binding-lifetime-tests', 'passed');
    document.body.setAttribute('data-binding-lifetime-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' binding owned-state lifetime checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-binding-lifetime-tests', 'failed');
      document.body.setAttribute('data-binding-lifetime-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
