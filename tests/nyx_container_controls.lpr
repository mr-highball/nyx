{ Copyright (c) mr-highball. SPDX-License-Identifier: MIT.
  Actual browser/LCL consumers of the unchanged semantic MCP companion.
  Live allocation changes use Nyx configuration; no DOM geometry overrides. }
program nyx_container_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.generated.view
  {$ifdef PAS2JS}, Web, nyx.render.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, ExtCtrls, Graphics,
  IntfGraphics, FPWritePNG, nyx.render.lcl{$endif};

var
  GDocument: TNyxDocument;
  GBefore: TNyxText;
  GChecks: Integer;
  {$ifdef PAS2JS}
  GRenderer: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  GInput: TJSHTMLTextAreaElement;
  GStep: Integer;
  {$else}
  GRenderer: TNyxLCLRenderer;
  GHost: TPanel;
  GForm: TForm;
  GInput: TMemo;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Identity(const AInstance, APart: TNyxText): TNyxText;
begin
  Result := NyxQualifiedID(AInstance, APart);
end;

procedure CheckLayout(const AInstance: TNyxText; ACompact: Boolean);
var
  LBody, LMemo, LPrompt: TNyxNode;
  {$ifdef PAS2JS}LFirst, LSecond: TJSDOMRect;{$else}LFirst, LSecond: TControl;{$endif}
begin
  LBody := GRenderer.Root.Find(Identity(AInstance, 'card-body'));
  LMemo := GRenderer.Root.Find(Identity(AInstance, 'card-notes'));
  LPrompt := GRenderer.Root.Find(Identity(AInstance, 'card-prompt'));
  Check((LBody <> nil) and (LMemo <> nil) and (LPrompt <> nil), 'Qualified reusable parts exist');
  {$ifdef PAS2JS}
  LFirst := GRenderer.ElementFor(LPrompt.ID, niRuntime).getBoundingClientRect;
  LSecond := GRenderer.ElementFor(LMemo.ID, niRuntime).getBoundingClientRect;
  {$else}
  LFirst := GRenderer.ControlFor(LPrompt.ID, niRuntime);
  LSecond := GRenderer.ControlFor(LMemo.ID, niRuntime);
  {$endif}

  if ACompact then
  begin
    Check(LBody.Prop('layout') = 'column', 'Small allocated content box selects compact layout');
    Check(LSecond.Top >= LFirst.Top + LFirst.Height, 'Actual compact controls form a column');
  end
  else
  begin
    Check(LBody.Prop('layout') = 'row', 'Large allocated content box retains wide layout');
    Check(LSecond.Left >= LFirst.Left + LFirst.Width, 'Actual wide controls form a row');
  end;
end;

procedure Initial;
begin
  CheckLayout('small-card', True);
  CheckLayout('large-card', False);
  Check(GRenderer.Root.Find('frame-status').Prop('text') = 'Wide frame',
    'An externally stretched full-size publisher starts in landscape');
  Check(GRenderer.Root.Find('outside-status').Prop('text') = 'Outside the cards',
    'Absent ancestor never falls back to the whole viewport');
  Check(GRenderer.Root.Find(Identity('small-card', 'adaptive-card')).Prop('width') = '',
    'Publisher receives flex allocation rather than a fixture-only fixed width');
  GInput := {$ifdef PAS2JS}TJSHTMLTextAreaElement{$else}TMemo{$endif}(
    GRenderer.InputFor(Identity('large-card', 'card-notes'), niRuntime));
  {$ifdef PAS2JS}
  GInput.value := 'Keep this independent English draft.';
  GInput.focus;
  GInput.selectionStart := 5;
  GInput.selectionEnd := 9;
  {$else}
  GInput.Text := 'Keep this independent English draft.';
  GInput.SetFocus;
  GInput.SelStart := 5;
  GInput.SelLength := 4;
  {$endif}
  { The outer view remains 800 logical pixels wide. Only this row's allocated
    space changes; the two recipe instances share no measurement/runtime state. }
  GRenderer.Root.Find('card-row').Configure.Width(450).Done;
  GRenderer.Root.Find('frame-row').Configure.Width(150).Done;
  GRenderer.Sync;
end;

procedure Narrow;
begin
  CheckLayout('small-card', True);
  CheckLayout('large-card', True);
  Check(GRenderer.Root.Find('frame-status').Prop('text') = 'Tall frame',
    'Allocated width changes orientation while externally allocated height stays fixed');
  Check(GRenderer.InputFor(Identity('large-card', 'card-notes'), niRuntime) = GInput,
    'Live allocation retains the actual input object');
  Check({$ifdef PAS2JS}GInput.value{$else}GInput.Text{$endif} =
    'Keep this independent English draft.', 'Live allocation retains the editable draft');
  Check({$ifdef PAS2JS}(GInput.selectionStart = 5) and (GInput.selectionEnd = 9)
    {$else}(GInput.SelStart = 5) and (GInput.SelLength = 4){$endif},
    'Live allocation retains the selected text range');
  Check({$ifdef PAS2JS}document.activeElement = GInput{$else}GForm.ActiveControl = GInput{$endif},
    'Live allocation retains focus');
  Check({$ifdef PAS2JS}GHost.clientWidth{$else}GHost.ClientWidth{$endif} = 800,
    'The whole-view viewport is unchanged');
  GRenderer.Root.Find('card-row').Configure.Width(660).Done;
  GRenderer.Root.Find('frame-row').Configure.Width(360).Done;
  GRenderer.Sync;
end;

procedure Restored;
begin
  CheckLayout('large-card', False);
  Check(GRenderer.Root.Find('frame-status').Prop('text') = 'Wide frame',
    'Full-size orientation restores on the same mounted face');
  Check(TNyxCodec.Encode(GDocument) = GBefore, 'Runtime allocation never rewrites the accepted document');
  Check(GRenderer.Root.Find(Identity('small-card', 'card-notes')).Prop('value') =
    'The same card adapts here.', 'Another reusable instance retains independent input state');
end;

{$ifdef PAS2JS}
procedure Step;
begin
  try
    case GStep of
      0: Initial;
      1: Narrow;
      2:
        begin
          Restored;
          document.body.setAttribute('data-nyx-responsive-controls', 'passed');
          document.body.setAttribute('data-nyx-responsive-checks', IntToStr(GChecks));
          Exit;
        end;
    end;
    Inc(GStep);
    window.setTimeout(@Step, 120);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-responsive-controls', 'failed');
      document.body.setAttribute('data-nyx-responsive-error', LException.Message);
    end;
  end;
end;
{$else}
procedure Pump;
begin
  Application.ProcessMessages;
  Application.ProcessMessages;
end;

procedure Capture;
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := TFPWriterPNG.Create;
  try
    LBitmap.SetSize(GForm.Width, GForm.Height);
    GForm.PaintTo(LBitmap.Canvas, GForm.Left, GForm.Top);
    LImage := LBitmap.CreateIntfImage;
    LImage.SaveToFile(ParamStr(1), LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
end;
{$endif}

begin
  GDocument := BuildNyxDocument;
  GBefore := TNyxCodec.Encode(GDocument);
  {$ifdef PAS2JS}
  GHost := TJSHTMLElement(document.createElement('div'));
  GHost.style.setProperty('width', '800px');
  GHost.style.setProperty('height', '740px');
  document.body.appendChild(GHost);
  GRenderer := TNyxBrowserRenderer.Create;
  GRenderer.Render(GDocument, GDocument.Find('container-room'), GHost, False);
  window.setTimeout(@Step, 120);
  {$else}
  Application.Initialize;
  Application.CaptureExceptions := False;
  GForm := TForm.Create(nil);
  GRenderer := TNyxLCLRenderer.Create;
  try
    GForm.Caption := 'Room for ideas';
    GForm.ClientWidth := 800;
    GForm.ClientHeight := 740;
    GHost := TPanel.Create(GForm);
    GHost.Parent := GForm;
    GHost.Align := alClient;
    GHost.BevelOuter := bvNone;
    GForm.Show;
    GRenderer.Render(GDocument, GDocument.Find('container-room'), GHost, False);
    Pump;
    Initial;
    Pump;
    Narrow;
    Pump;
    Restored;
    Capture;
    WriteLn('PASS ', GChecks, ' actual LCL container allocation/retained-input checks');
  finally
    GRenderer.Free;
    GDocument.Free;
    GForm.Free;
  end;
  {$endif}
end.
