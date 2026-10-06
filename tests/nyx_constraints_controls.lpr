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


program nyx_constraints_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.generated.view,
  {$ifdef PAS2JS}Web, nyx.render.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls,
    Graphics, IntfGraphics, FPWritePNG, nyx.render.lcl;{$endif}

type
  {$ifdef PAS2JS}TRenderer = TNyxBrowserRenderer; TFace = TJSHTMLElement;
  {$else}TRenderer = TNyxLCLRenderer; TFace = TControl;{$endif}
var
  LRenderer: TRenderer;
  LDocument: TNyxDocument;
  LChecks: Integer;
  LNotesFace: TFace;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LMemo: TJSHTMLTextAreaElement;
  {$else}
  LHost: TForm;
  LMemo: TMemo;
  LStream: TFileStream;
  LExpected: TNyxText;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Actual size constraints: ' + AReason);
  end;
  Inc(LChecks);
end;

function Face(const AID: TNyxText): TFace;
begin
  {$ifdef PAS2JS}Result := LRenderer.ElementFor(AID);
  {$else}Result := LRenderer.ControlFor(AID);{$endif}
end;

function Width(const AID: TNyxText): Integer;
begin
  {$ifdef PAS2JS}Result := Round(Face(AID).offsetWidth);
  {$else}Result := Face(AID).Width;{$endif}
end;

function Height(const AID: TNyxText): Integer;
begin
  {$ifdef PAS2JS}Result := Round(Face(AID).offsetHeight);
  {$else}Result := Face(AID).Height;{$endif}
end;

function Top(const AID: TNyxText): Integer;
begin
  {$ifdef PAS2JS}
  Result := Round(Face(AID).getBoundingClientRect.top -
    Face(LRenderer.Root.Find(AID).Parent.ID).getBoundingClientRect.top);
  {$else}Result := Face(AID).Top;{$endif}
end;

procedure Near(AActual, AExpected: Integer; const AReason: TNyxText);
begin
  Check(Abs(AActual - AExpected) <= 1, AReason + ': ' + IntToStr(AActual) +
    ' expected ' + IntToStr(AExpected));
end;

procedure CheckRetention;
begin
  Check(Face('notes-editor') = LNotesFace, 'size changes retain the actual input owner');
  {$ifdef PAS2JS}
  Check(document.activeElement = LMemo, 'size changes retain browser focus');
  Check((LMemo.value = 'A retained English draft.') and
    (LMemo.selectionStart = 2) and (LMemo.selectionEnd = 7),
    'size changes retain browser text and selection');
  {$else}
  Check(LHost.ActiveControl = LMemo, 'size changes retain native focus');
  Check((TNyxText(LMemo.Text) = 'A retained English draft.') and
    (LMemo.SelStart = 2) and (LMemo.SelLength = 5),
    'size changes retain native text and selection');
  {$endif}
end;

{$ifndef PAS2JS}
procedure Capture;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    LBitmap.SetSize(LHost.ClientWidth, LHost.ClientHeight);
    LHost.PaintTo(LBitmap.Canvas, 0, 0);
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

begin
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    { This is the exact semantic export, compiled unchanged and mounted through
      public adapters. Descriptor tests do not substitute actual geometry. }
    LDocument := BuildNyxDocument;
    LRenderer := TRenderer.Create;
    {$ifdef PAS2JS}
    LHost := TJSHTMLElement(document.createElement('main'));
    LHost.style.setProperty('width', '800px');
    document.body.appendChild(LHost);
    {$else}
    LHost := TForm.CreateNew(nil);
    LHost.SetBounds(20, 20, 800, 900);
    LHost.Show;
    {$endif}
    try
      {$ifndef PAS2JS}
      LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
      try
        SetLength(LExpected, LStream.Size);

        if LExpected <> '' then
        begin
          LStream.ReadBuffer(LExpected[1], Length(LExpected));
        end;
      finally
        LStream.Free;
      end;
      Check(TNyxCodec.Encode(LDocument) = LExpected, 'unchanged source reconstructs the exact paired design');
      {$endif}
      LRenderer.Render(LDocument, LDocument.Find('home'), LHost, False);
      Near(Width('notes-editor'), 300, 'capped row editor uses its admitted maximum');
      Near(Width('side-caption'), 80, 'capped sibling retains its maximum');
      Near(Height('notes-editor'), 48, 'fill height respects its explicit maximum');
      Near(Width('minimum-caption'), 80, 'mixed violations redistribute to the minimum sibling');
      Near(Width('maximum-caption'), 20, 'mixed violations freeze the maximum sibling');
      Near(Height('first-editor'), 40, 'column maximum caps its first weight');
      Near(Height('second-editor'), 150, 'column redistributes remaining space');
      Near(Top('second-editor'), 50, 'column positions use bounded heights');
      Near(Width('bounded-caption'), 140, 'fixed width respects its maximum');
      Near(Height('bounded-caption'), 40, 'intrinsic height respects its minimum');
      Near(Width('zero-space'), 0, 'explicit zero width remains zero');
      Near(Height('zero-space'), 0, 'explicit zero height remains zero');
      Near(Width('overflow-caption'), 500, 'minimum can exceed containing width');
      Near(Top('second-action'), 42, 'wrapping uses weighted hypothetical minima');
      {$ifdef PAS2JS}Near(Width('platform-badge'), 80, 'browser override reaches its actual badge');
      LMemo := TJSHTMLTextAreaElement(LRenderer.InputFor('notes-editor'));
      LMemo.value := 'A retained English draft.';
      LMemo.dispatchEvent(TJSEvent.new('input'));
      LMemo.focus;
      LMemo.selectionStart := 2;
      LMemo.selectionEnd := 7;
      {$else}Near(Width('platform-badge'), 90, 'native override reaches its actual badge');
      LMemo := TMemo(LRenderer.InputFor('notes-editor'));
      LMemo.Text := 'A retained English draft.';
      LMemo.OnChange(LMemo);
      LMemo.SetFocus;
      LMemo.SelStart := 2;
      LMemo.SelLength := 5;
      {$endif}
      LNotesFace := Face('notes-editor');

      LRenderer.Root.Find('notes-editor').Configure.Clear(atMaximumWidth);
      LRenderer.Sync;
      Near(Width('notes-editor'), 310, 'clearing maximum redistributes the remaining row space');
      CheckRetention;
      LRenderer.Root.Find('notes-row').Configure.Width(250);
      LRenderer.Sync;
      Near(Width('notes-editor'), 200, 'minimum owns its share in a smaller containing row');
      Near(Width('side-caption'), 40, 'other weight receives only remaining space');
      CheckRetention;
      LRenderer.Root.Find('side-caption').Configure.Visible(False);
      LRenderer.Sync;
      Near(Width('notes-editor'), 250, 'hidden sibling releases bounds and gap');
      CheckRetention;
      LRenderer.Root.Find('side-caption').Configure.Visible(True);
      LRenderer.Root.Find('notes-row').Configure.Width(400);
      LRenderer.Root.Find('notes-editor').Configure.MaximumWidth(300).Clear(atMaximumHeight);
      LRenderer.Sync;
      Near(Height('notes-editor'), 100, 'clearing height maximum restores fill');
      CheckRetention;
      {$ifdef PAS2JS}
      LHost.style.setProperty('width', '390px');
      {$else}
      LHost.ClientWidth := 390;
      Application.ProcessMessages;
      {$endif}
      LRenderer.Sync;
      Near(Width('home'), 390, 'narrow host reaches its actual logical extent');
      Near(Width('overflow-caption'), 500, 'explicit minimum can overflow a narrow parent');
      CheckRetention;
      {$ifndef PAS2JS}

      if ParamCount > 1 then
      begin
        Capture;
      end;
      {$endif}
      WriteLn('PASS ', LChecks, ' actual compiled size constraint checks');
      {$ifdef PAS2JS}document.body.setAttribute('data-nyx-constraints-controls', 'passed');{$endif}
    finally
      LRenderer.Free;
      LRenderer := nil;
      LDocument.Free;
      LDocument := nil;
      {$ifndef PAS2JS}LHost.Free; LHost := nil;{$endif}
    end;
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-constraints-controls', 'failed');
      document.body.textContent := E.Message;
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
