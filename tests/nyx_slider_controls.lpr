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

program nyx_slider_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.state, nyx.data, nyx.contract,
  nyx.model, nyx.controls, nyx.sliders, nyx.schema, nyx.codec, nyx.codegen,
  nyx.source, nyx.events, nyx.behavior, nyx.scheduler, nyx.test.sliders,
  {$ifdef NYX_COMPILED_SLIDER}nyx.generated.slider,{$endif}
  {$ifdef PAS2JS}JS, Web, nyx.render.browser;
  {$else}Interfaces, Forms, Controls, ComCtrls, LCLIntf, LCLType,
    {$ifdef MSWINDOWS}Windows,{$endif}nyx.render.lcl;{$endif}

type
  { Snapshot-only receiver. The final physical edit retires its renderer;
    the callback's copied numeric payload remains usable afterward. }
  TSliderObserver = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifdef PAS2JS}
  TSliderEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  {$endif}

const
  { Compare Doubles with a declared Double baseline. Native extended literal
    evaluation must not accidentally test a different precision contract. }
  CExactGain: Double = 0.123456789012345;

var
  GChecks: Integer;
  GCalls: Integer;
  GSnapshot: TNyxEventInfo;
  GRetire: Boolean;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  {$else}
  GRenderer: TNyxLCLRenderer;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Sliders: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TSliderObserver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(GCalls);
  GSnapshot := AEvent.Copy;
  Check(AEvent.HasValue, 'accepted slider callback has a value');
  Check(AEvent.Value.AsNumber = GRenderer.State.GetValue(NyxNumberState('gain')),
    'callback observes committed numeric store');

  if GRetire then
  begin
    GRenderer.Free;
    GRenderer := nil;
  end;
end;

procedure Shared;
var
  LScale: TNyxSliderScale;
  LValue: TNyxSliderValue;
  LDomain: TNyxValueDomain;
  LBefore: TNyxText;
  LRefused: Boolean;
  LSlider: INyxSlider;
  LIndex: Integer;
begin
  LScale := TNyxSliderScale.Create(NyxNumberDomain.Range(-1, 1).Definition, 0, 100, 400);
  Check((LScale.Minimum = -1) and (LScale.Maximum = 1) and
    (LScale.MaximumPosition = 400), 'declared bounds override legacy defaults');
  Check(LScale.ValueAt(0) = '-1', 'minimum endpoint is exact');
  Check(LScale.ValueAt(400) = '1', 'maximum endpoint is exact');
  Check(LScale.ValueAt(225) = '0.125', 'fractional tick is numeric');
  Check(LScale.PositionOf('0.125') = 225, 'fractional position mapping');
  LValue.Accept(LScale, '0.123456789012345');
  Check(LValue.ReadPosition(LValue.Position) = '0.123456789012345',
    'off-tick accepted value remains exact');
  LValue.Accept(LScale, '0.125000');
  Check(LValue.ReadPosition(225) = '0.125000', 'no-op retains exact accepted wire spelling');
  Check(LValue.ReadPosition(0) = '-1', 'a changed thumb emits semantic minimum');
  LRefused := False;
  try
    LValue.Accept(LScale, '2');
  except
    on LException: ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LValue.ReadPosition(225) = '0.125000'), 'refused publication leaves accepted cache exact');
  LDomain := NyxIntegerDomain.Choices([30, -12, 0]).Definition;
  LBefore := LDomain.ToData.ToJSON;
  LScale := TNyxSliderScale.Create(LDomain);
  Check((LScale.MaximumPosition = 2) and (LScale.Minimum = -12) and (LScale.Maximum = 30),
    'numeric choices govern scale bounds');
  Check((LScale.ValueAt(0) = '-12') and (LScale.ValueAt(1) = '0') and
    (LScale.ValueAt(2) = '30'), 'integer choices use sorted exact ticks');
  Check(LDomain.ToData.ToJSON = LBefore, 'scale sorting leaves authored choice order untouched');
  Check(LScale.PositionOf('30') = 2, 'choice ordinal mapping');
  LScale := TNyxSliderScale.Create(NyxNumberDomain.Choices([2.0, -0.5, 0.0, 0.125]).Definition);
  Check((LScale.MaximumPosition = 3) and (LScale.ValueAt(2) = '0.125'),
    'fractional choices retain numeric identity');
  LRefused := False;
  try
    LScale.PositionOf('0.1');
  except
    on LException: ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'a value between choices refuses');

  for LIndex := 0 to 1 do
  begin
    LRefused := False;
    try

      if LIndex = 0 then
      begin
        LScale.ValueAt(-1);
      end
      else
      begin
        LScale.ValueAt(4);
      end;
    except
      on LException: ENyxContract do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'out-of-range physical position refuses');
  end;
  LScale := TNyxSliderScale.Create(NyxIntegerDomain.Range(-3, 3).Definition);
  Check((LScale.MaximumPosition = 6) and (LScale.ValueAt(1) = '-2'),
    'ordinary integer ranges retain unit increments');
  LScale := TNyxSliderScale.Create(NyxIntegerDomain.Range(Low(Integer), High(Integer)).Definition);
  Check((LScale.MaximumPosition = 1000) and (LScale.ValueAt(0) = '-2147483648') and
    (LScale.ValueAt(1000) = '2147483647'), 'large signed integer spans avoid overflow');
  LScale := TNyxSliderScale.Create(NyxNumberDomain.Range(-1e308, 1e308).Definition);
  Check(LScale.PositionOf('0') = 500, 'opposite extreme bounds avoid overflow');
  Check(LScale.ValueAt(500) = '0', 'extreme interpolation stays finite');
  LScale := TNyxSliderScale.Create(NyxNumberDomain.Range(-5e-324, 5e-324).Definition);
  Check(LScale.PositionOf('0') = 500, 'tiny opposite bounds avoid underflow into zero denominator');
  Check(LScale.ValueAt(500) = '0', 'subnormal interpolation remains an admitted Number');
  LScale := TNyxSliderScale.Create(NyxNumberDomain.Range(0.125, 0.125).Definition);
  Check((LScale.MaximumPosition = 0) and (LScale.ValueAt(0) = '0.125'),
    'single-value range is not divided by zero');
  LScale := TNyxSliderScale.Create(NyxIntegerDomain.Choices([-12]).Definition);
  Check((LScale.MaximumPosition = 0) and (LScale.ValueAt(0) = '-12'), 'single choice has one physical position');
  LRefused := False;
  try
    LScale := TNyxSliderScale.Create(NyxTextDomain.Definition);
  except
    on LException: ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'non-numeric slider family refuses');

  for LIndex := 0 to 1 do
  begin
    LRefused := False;
    try
      LScale := TNyxSliderScale.Create(NyxNumberDomain.Definition, 0, 100, LIndex * 1000001);
    except
      on LException: ENyxContract do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'invalid physical resolution refuses');
  end;
  LSlider := NewNyxSlider('typed-slider');
  LSlider.Contract.Value(NyxNumberDomain.Range(0, 1));
  Check(LSlider.WithNumber(0.125).NumberValue = 0.125, 'specialized fluent numeric slider authoring');
  LSlider.Configure.SliderIntervals(1000000);
  Check(LSlider.Node.Prop('slider-intervals') = '1000000', 'public maximum resolution is admitted');
  ValidateNyxProperties(LSlider.Node);
  LBefore := LSlider.Node.Prop('slider-intervals');
  LRefused := False;
  try
    LSlider.Configure.SliderIntervals(0);
  except
    on LException: ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSlider.Node.Prop('slider-intervals') = LBefore),
    'fluent resolution refusal retains its baseline');
  LSlider := NewNyxSlider('signed-slider');
  LSlider.Contract.Value(NyxIntegerDomain.Range(Low(Integer), High(Integer)));
  LSlider.Value := Low(Integer);
  ValidateNyxProperties(LSlider.Node);
  Check(LSlider.Value = Low(Integer), 'specialized Integer slider retains the complete signed domain');
end;

procedure Controls;
var
  LDocument: TNyxDocument;
  LOriginal: TNyxText;
  LSource: TNyxText;
  LReplayed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LRefused: Boolean;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$else}
  LHost: TForm;
  {$endif}

  function Position(const AID: TNyxText): Integer;
  begin
    {$ifdef PAS2JS}
    Result := StrToInt(TJSHTMLInputElement(GRenderer.InputFor(AID, niRuntime)).value);
    {$else}
    Result := TTrackBar(GRenderer.InputFor(AID, niRuntime)).Position;
    {$endif}
  end;

  procedure Move(const AID: TNyxText; APosition: Integer);
  {$ifdef PAS2JS}
  var
    LInput: TJSHTMLInputElement;
    LOptions: TJSObject;
  {$else}
  var
    LInput: TTrackBar;
  {$endif}
  begin
    {$ifdef PAS2JS}
    LInput := TJSHTMLInputElement(GRenderer.InputFor(AID, niRuntime));
    LInput.value := IntToStr(APosition);
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LInput.dispatchEvent(TSliderEvent.new('change', LOptions));
    {$else}
    LInput := TTrackBar(GRenderer.InputFor(AID, niRuntime));
    LInput.Position := APosition;
    { Setters may already dispatch OnChange. Explicit slot consumption also
      qualifies no-op callbacks without counting platform notifications twice. }

    if GRenderer <> nil then
    begin
      LInput.OnChange(LInput);
    end;
    {$endif}
  end;

begin
  {$ifdef NYX_COMPILED_SLIDER}
  LDocument := nyx.generated.slider.BuildNyxDocument;
  {$else}
  LDocument := NewNyxSliderCompanion;
  {$endif}
  LWorkspace := nil;
  LReplayed := nil;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 640, 420);
  LHost.Show;
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  try
    LOriginal := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('.SliderIntervals(400)', LSource) > 0) and
      (Pos('INyxSlider', LSource) > 0) and (Pos('NyxNumberDomain.Range(-1, 1)', LSource) > 0),
      'generated authoring uses specialized types and numeric policies');
    LReplayed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LReplayed) = LOriginal, 'typed slider source reconstructs exact design');
    GRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    Check(Position('fractional-slider') = 225, 'actual control positions off-tick fraction without narrowing it');
    Move('fractional-slider', 225);
    Check(GRenderer.State.GetValue(NyxNumberState('gain')) = CExactGain,
      'no-op actual host callback preserves exact bound Number');
    {$ifdef PAS2JS}
    Check(TJSHTMLInputElement(GRenderer.InputFor('choice-slider', niRuntime)).getAttribute('aria-valuemin') = '-12',
      'DOM accessibility minimum has numeric meaning');
    Check(TJSHTMLInputElement(GRenderer.InputFor('choice-slider', niRuntime)).getAttribute('aria-valuemax') = '30',
      'DOM accessibility maximum has numeric meaning');
    Check(TJSHTMLInputElement(GRenderer.InputFor('fractional-slider', niRuntime)).getAttribute('step') = '1',
      'real HTML range consumes ordinal unit steps');
    {$endif}
    GRenderer.Events.On(NyxControlEvents('fractional-slider', niRuntime), ntChange)
      .Subscribe(TSliderObserver.Create);
    Move('fractional-slider', 200);
    Check((GRenderer.State.GetValue(NyxNumberState('gain')) = 0) and
      (GSnapshot.Value.AsNumber = 0), 'thumb midpoint publishes numeric zero');
    Move('fractional-slider', 0);
    Check(GRenderer.State.GetValue(NyxNumberState('gain')) = -1, 'actual minimum publishes declared negative bound');
    Move('fractional-slider', 400);
    Check(GRenderer.State.GetValue(NyxNumberState('gain')) = 1, 'actual maximum publishes declared bound');
    GRenderer.State.SetValue(NyxNumberState('gain'), 0.123456789012345);
    Check(Position('fractional-slider') = 225, 'programmatic off-tick Number reaches existing control');
    Move('choice-slider', 0);
    Check(GRenderer.State.GetValue(NyxIntegerState('level')) = -12, 'integer choice tick publishes negative numeric choice');
    Move('choice-slider', 2);
    Check(GRenderer.State.GetValue(NyxIntegerState('level')) = 30, 'integer choice endpoint publishes choice, not ordinal');
    Move('number-choice-slider', 2);
    Check(GRenderer.State.GetValue(NyxNumberState('blend')) = 0.125, 'fractional choice reaches typed store');
    {$ifdef PAS2JS}
    TJSHTMLInputElement(GRenderer.InputFor('choice-slider', niRuntime)).focus;
    Check(document.activeElement = GRenderer.InputFor('choice-slider', niRuntime),
      'actual HTML range accepts ordinary focus');
    {$else}
    TTrackBar(GRenderer.InputFor('choice-slider', niRuntime)).SetFocus;
    Check(TTrackBar(GRenderer.InputFor('choice-slider', niRuntime)).Focused,
      'real native range accepts focus');
    {$ifdef MSWINDOWS}
    Windows.SendMessage(TTrackBar(GRenderer.InputFor('choice-slider', niRuntime)).Handle, WM_KEYDOWN, VK_HOME, 0);
    Check(GRenderer.State.GetValue(NyxIntegerState('level')) = -12, 'native Home chooses first allowed value');
    Windows.SendMessage(TTrackBar(GRenderer.InputFor('choice-slider', niRuntime)).Handle, WM_KEYDOWN, VK_END, 0);
    Check(GRenderer.State.GetValue(NyxIntegerState('level')) = 30, 'native End chooses last allowed value');
    Windows.SendMessage(TTrackBar(GRenderer.InputFor('choice-slider', niRuntime)).Handle, WM_KEYDOWN, VK_LEFT, 0);
    Check(GRenderer.State.GetValue(NyxIntegerState('level')) = 0, 'native Left moves by one allowed choice');
    Windows.SendMessage(TTrackBar(GRenderer.InputFor('choice-slider', niRuntime)).Handle, WM_KEYDOWN, VK_RIGHT, 0);
    Check(GRenderer.State.GetValue(NyxIntegerState('level')) = 30, 'native Right moves by one allowed choice');
    {$endif}
    {$endif}
    GRenderer.Root.Find('choice-slider').Contract.Value(NyxIntegerDomain.Range(Low(Integer), High(Integer)));
    GRenderer.State.SetValue(NyxIntegerState('level'), Low(Integer));
    Check(Position('choice-slider') = 0, 'actual range maps signed minimum to bounded ordinal');
    GRenderer.State.SetValue(NyxIntegerState('level'), High(Integer));
    Check(Position('choice-slider') = 1000, 'actual range maps signed maximum to bounded ordinal');
    Move('choice-slider', 500);
    Check(GRenderer.State.GetValue(NyxIntegerState('level')) = 0, 'actual large range publishes numeric midpoint');
    LRefused := False;
    try
      GRenderer.State.SetValue(NyxNumberState('blend'), 0.1);
    except
      on LException: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (GRenderer.State.GetValue(NyxNumberState('blend')) = 0.125) and
      (Position('number-choice-slider') = 2), 'invalid choice store publication is atomic');
    GRenderer.Root.Find('number-choice-slider').Contract.Value(NyxNumberDomain.Choices([0.125]));
    GRenderer.Sync;
    Check(Position('number-choice-slider') = 0, 'actual singleton choice remaps to its only thumb position');
    Move('number-choice-slider', 0);
    Check(GRenderer.State.GetValue(NyxNumberState('blend')) = 0.125,
      'single-choice physical callback remains a numeric no-op');
    GRenderer.Root.Find('fractional-slider').Configure.ReadOnly(True);
    GRenderer.Sync;
    Move('fractional-slider', 0);
    Check((GRenderer.State.GetValue(NyxNumberState('gain')) = CExactGain) and
      (Position('fractional-slider') = 225), 'read-only host draft restores exact accepted value');
    GRenderer.Root.Find('fractional-slider').Configure.ReadOnly(False).Enabled(False);
    GRenderer.Sync;
    Move('fractional-slider', 400);
    Check((GRenderer.State.GetValue(NyxNumberState('gain')) = CExactGain) and
      (Position('fractional-slider') = 225), 'disabled host draft restores accepted value');
    GRenderer.Root.Find('fractional-slider').Configure.Enabled(True).SliderIntervals(800);
    GRenderer.Sync;
    Check(Position('fractional-slider') = 449, 'runtime resolution change remaps current value');
    Check(GRenderer.State.GetValue(NyxNumberState('gain')) = CExactGain,
      'resolution change leaves store exact');
    Move('fractional-slider', 449);
    Check(GRenderer.State.GetValue(NyxNumberState('gain')) = CExactGain,
      'remapped no-op callback preserves accepted value');
    Check(TNyxCodec.Encode(LDocument) = LOriginal, 'runtime edits leave authored document/defaults exact');
    Check(GCalls = 3, 'only accepted physical changes emit semantic callbacks');
    GRenderer.Root.Find('fractional-slider').Configure.SliderIntervals(1000000);
    GRenderer.Sync;
    Check(Position('fractional-slider') = 561728, 'actual maximum resolution retains bounded thumb coordinates');
    {$ifndef PAS2JS}
    Check(TTrackBar(GRenderer.InputFor('fractional-slider', niRuntime)).Frequency = 100000,
      'native maximum resolution uses bounded visible marker count');
    {$endif}
    Move('fractional-slider', 561728);
    Check(GRenderer.State.GetValue(NyxNumberState('gain')) = CExactGain,
      'actual maximum-resolution no-op retains exact state');
    GRetire := True;
    Move('fractional-slider', 0);
    Check((GRenderer = nil) and (GSnapshot.Value.AsNumber = -1),
      'numeric callback survives whole-renderer retirement');
  finally
    GRetire := False;
    GRenderer.Free;
    GRenderer := nil;
    LWorkspace.Free;
    LReplayed.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    {$else}
    LHost.Free;
    {$endif}
    LDocument.Free;
  end;
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    Shared;
    Controls;
    WriteLn('PASS ', GChecks, ' shared and actual slider checks');
    {$ifdef PAS2JS}document.body.setAttribute('data-result', 'pass');{$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}document.body.setAttribute('data-result', 'fail');{$endif}
      {$ifndef PAS2JS}ExitCode := 1;{$endif}
    end;
  end;
end.
