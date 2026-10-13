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

program nyx_selection_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.state, nyx.data,
  nyx.selection, nyx.codec, nyx.codegen, nyx.test.selection,
  nyx.generated.view
  {$ifdef NYX_COMPILED_SELECTION}, nyx.generated.selection{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.theme
  {$else}, Interfaces, Classes, Forms, Controls, Graphics, IntfGraphics,
    FPWritePNG, LCLIntf, LCLType, nyx.render.lcl, nyx.widgets.lcl{$endif};

{$ifndef PAS2JS}
type
  TButtonAccess = class(TNyxLCLButton);
{$endif}

var
  GDocument: TNyxDocument;
  GExpected: TNyxDocument;
  GChecks: Integer;
  GBefore: TNyxText;
  GSource: TNyxText;
  {$ifdef PAS2JS}
  GHost: TJSHTMLElement;
  GRenderer: TNyxBrowserRenderer;
  GTimer: Integer;
  {$else}
  GHost: TForm;
  GRenderer: TNyxLCLRenderer;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Step(const AOwner: TNyxText; AIndex: Integer): TNyxNode;
var
  LOwnerID: TNyxText;
  LRoot: TNyxNode;
begin
  LOwnerID := AOwner;

  if (AOwner = 'first') or (AOwner = 'second') then
  begin
    { Realized reusable roots include the authored definition identity. The
      reference's design ID alone is not an exact physical/runtime ID. }
    LOwnerID := AOwner + '/brand-steps';
  end;
  LRoot := GRenderer.Root.Find(LOwnerID);

  if LRoot = nil then
  begin
    raise Exception.Create('Selection fixture owner was not realized: ' + LOwnerID);
  end;
  Result := LRoot.Part(NyxPart('step-' + IntToStr(AIndex)));
end;

procedure ClickStep(const AOwner: TNyxText; AIndex: Integer);
var
  LStep: TNyxNode;
begin
  LStep := Step(AOwner, AIndex);
  {$ifdef PAS2JS}
  GRenderer.ElementFor(LStep.ID).click;
  {$else}
  TButtonAccess(GRenderer.ControlFor(LStep.ID)).Click;
  Application.ProcessMessages;
  {$endif}
end;

procedure Cleanup;
begin
  GRenderer.Free;
  GRenderer := nil;
  {$ifdef PAS2JS}

  if GHost <> nil then
  begin
    GHost.remove;
  end;
  GHost := nil;
  {$else}
  FreeAndNil(GHost);
  {$endif}
  FreeAndNil(GExpected);
  FreeAndNil(GDocument);
end;

procedure Finish;
begin
  Cleanup;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-selection-checks', IntToStr(GChecks));
  document.body.setAttribute('data-selection', 'passed');
  {$else}
  WriteLn('PASS ', GChecks, ' actual selection policy/source/lifetime checks');
  {$endif}
end;

{$ifdef PAS2JS}
procedure AwaitCapture;
begin

  if document.body.getAttribute('data-capture-observed') = 'selection-styles' then
  begin
    window.clearInterval(GTimer);
    GTimer := -1;
    Finish;
  end;
end;
{$else}
procedure Capture;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LDC: HDC;
begin

  if ParamCount <> 2 then
  begin
    Exit;
  end;
  { Only displayed client pixels of this owned visible window are read.
    Target visuals are never inferred from an injected PaintTo/Print call. }
  GHost.BringToFront;
  GHost.Update;
  Application.ProcessMessages;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(GHost.ClientWidth, GHost.ClientHeight);
    LDC := GetDC(GHost.Handle);
    try

      if not BitBlt(LBitmap.Canvas.Handle, 0, 0, LBitmap.Width, LBitmap.Height,
        LDC, 0, 0, SRCCOPY) then
      begin
        raise Exception.Create('Owned selection client capture failed');
      end;
    finally
      ReleaseDC(GHost.Handle, LDC);
    end;
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(ParamStr(2), LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;
{$endif}

procedure Run;
{$ifdef PAS2JS}
var
  LTheme: TNyxTheme;
  LStyle: TJSHTMLElement;
{$else}
var
  LFile: TFileStream;
{$endif}
begin
  GDocument := nyx.generated.view.BuildNyxDocument;
  EnrichNyxSelectionFixture(GDocument);
  {$ifdef NYX_COMPILED_SELECTION}
  GExpected := GDocument;
  GDocument := nyx.generated.selection.BuildNyxDocument;
  Check(TNyxCodec.Encode(GDocument) = TNyxCodec.Encode(GExpected),
    'compiled crafted source reconstructs exact authored policies and overrides');
  FreeAndNil(GExpected);
  {$endif}
  GBefore := TNyxCodec.Encode(GDocument);
  GSource := TNyxCodegen.Generate(GDocument, 'nyx.generated.selection');
  {$ifndef PAS2JS}

  if ParamCount >= 1 then
  begin
    LFile := TFileStream.Create(ParamStr(1), fmCreate);
    try
      LFile.WriteBuffer(GSource[1], Length(GSource));
    finally
      LFile.Free;
    end;
  end;
  {$endif}
  Inc(GChecks, RunNyxSelectionContracts(GDocument));
  {$ifdef PAS2JS}
  LTheme := TNyxTheme.Create;
  try
    LStyle := TJSHTMLElement(document.createElement('style'));
    LStyle.textContent := LTheme.CSS;
    document.head.appendChild(LStyle);
  finally
    LTheme.Free;
  end;
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.cssText := 'width:100%;min-height:650px;';
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  {$else}
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(40, 40, 900, 700);
  GRenderer := TNyxLCLRenderer.Create;
  {$endif}
  GRenderer.Render(GDocument, GDocument.Pages[0], GHost);
  {$ifndef PAS2JS}
  GHost.Show;
  Application.ProcessMessages;
  {$endif}
  Check((Step('journey-stepper', 1).Prop('variant') = '') and
    (Step('journey-stepper', 2).Prop('variant') = 'primary'),
    'accepted default uses caller-selected reverse variants');
  Check((Step('first', 1).Prop('variant') = 'primary') and
    (Step('first', 2).Prop('variant') = 'secondary'),
    'authored reusable variants survive initial binding projection');
  Check((Step('second', 1).Prop('variant') = '') and
    (Step('second', 3).Prop('variant') = 'primary'),
    'local instance policy remains independent of its definition');
  {$ifdef PAS2JS}
  Check(GRenderer.ElementFor(Step('journey-stepper', 1).ID).getAttribute('data-variant') = '',
    'actual browser face carries the selected caller variant');
  {$else}
  Check(TButtonAccess(GRenderer.ControlFor(Step('journey-stepper', 1).ID)).Color <>
    TButtonAccess(GRenderer.ControlFor(Step('journey-stepper', 2).ID)).Color,
    'actual native faces use distinct selected and unselected theme colors');
  {$endif}
  ClickStep('journey-stepper', 2);
  Check(GRenderer.State.GetValue(NyxIntegerState('journey-position')) = 2,
    'physical step click publishes the ordinary two-way Integer binding');
  Check((Step('journey-stepper', 1).Prop('variant') = 'primary') and
    (Step('journey-stepper', 2).Prop('variant') = ''),
    'physical selection applies the copied custom variant policy');
  GRenderer.State.SetValue(NyxIntegerState('journey-position'), 3);
  Check((Step('journey-stepper', 3).Prop('pressed') = 'true') and
    (Step('journey-stepper', 3).Prop('variant') = ''),
    'programmatic state changes project selection and styling together');
  ClickStep('first', 2);
  Check((GRenderer.State.GetValue(NyxIntegerState('first.selection')) = 2) and
    (GRenderer.State.GetValue(NyxIntegerState('second.selection')) = 3),
    'physical reusable selection changes only its independent store key');
  Check((Step('first', 1).Prop('variant') = 'primary') and
    (Step('first', 2).Prop('variant') = 'secondary') and
    (Step('first', 2).Prop('pressed') = 'true'),
    'authored mode preserves creator styling while Boolean selection updates');
  {$ifdef PAS2JS}
  Check(GRenderer.ElementFor(Step('first', 2).ID).getAttribute('aria-pressed') = 'true',
    'actual authored browser part retains accepted pressed semantics');
  {$else}
  Check(TButtonAccess(GRenderer.ControlFor(Step('first', 1).ID)).Color <>
    TButtonAccess(GRenderer.ControlFor(Step('first', 2).ID)).Color,
    'actual authored native colors retain the creator distinction after selection');
  {$endif}
  GRenderer.State.SetValue(NyxBooleanState('first.readonly'), True);
  ClickStep('first', 4);
  Check(GRenderer.State.GetValue(NyxIntegerState('first.selection')) = 2,
    'selection styling cannot bypass inherited readonly admission');
  GRenderer.State.SetValue(NyxIntegerState('first.selection'), 4);
  Check((Step('first', 4).Prop('pressed') = 'true') and
    (Step('first', 1).Prop('variant') = 'primary'),
    'programmatic readonly selection still preserves authored styles');
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'runtime presentation never changes the authored document');
  {$ifdef PAS2JS}

  if window.location.hash = '#review' then
  begin
    { The owning trusted-input harness consumes exact physical identities only.
      This independent page stays mounted until its browser context closes. }
    document.body.setAttribute('data-selection-step-three', Step('journey-stepper', 3).ID);
    document.body.setAttribute('data-selection-step-four', Step('journey-stepper', 4).ID);
    document.body.setAttribute('data-selection-first-two', Step('first', 2).ID);
    document.body.setAttribute('data-selection-first-four', Step('first', 4).ID);
    document.body.setAttribute('data-selection-second-two', Step('second', 2).ID);
    document.body.setAttribute('data-selection-second-three', Step('second', 3).ID);
    document.body.setAttribute('data-selection-review', 'ready');
    Exit;
  end;
  document.body.setAttribute('data-capture-checkpoint', 'selection-styles');
  GTimer := window.setInterval(@AwaitCapture, 25);
  {$else}
  Capture;
  Finish;
  {$endif}
end;

begin
  GExpected := nil;
  {$ifdef PAS2JS}GTimer := -1;{$else}Application.Initialize;{$endif}
  try
    Run;
  except
    on LException: Exception do
    begin
      {$ifndef PAS2JS}DumpExceptionBackTrace(Output);{$endif}
      Cleanup;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-selection', 'failed');
      document.body.setAttribute('data-selection-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
