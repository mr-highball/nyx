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

program nyx_confirmation_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$IFDEF PAS2JS}{$modeswitch externalclass}{$ENDIF}

uses
  {$IFDEF PAS2JS}JS, Web, nyx.confirmation.browser, nyx.render.browser,
  {$ELSE}Interfaces, Classes, Forms, Controls, StdCtrls, LCLType, LCLIntf, Types,
    Graphics, IntfGraphics, FPWritePNG, nyx.confirmation.lcl, nyx.render.lcl,{$ENDIF}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.behavior,
  nyx.events, nyx.scheduler, nyx.confirmation, nyx.modal, nyx.generated.view;

type
  TCompletionObserver = class(TNyxEventCallback)
  public
    Marker: Integer;
    constructor Create(AMarker: Integer);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$IFDEF PAS2JS}
  TCancelableEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;
  {$ELSE}
  TControlAccess = class(TControl);
  TClosingObserver = class
  public
    procedure WindowHide(ASender: TObject);
  end;
  {$ENDIF}

var
  GChecks: Integer;
  GOrder: TNyxText;
  GConfirmed: Integer;
  GCancelled: Integer;
  GExtra: Integer;
  GRelease: Boolean;
  GReopen: Boolean;
  GTransitionRefusals: Integer;
  GSnapshot: TNyxEventInfo;
  GDocument: TNyxDocument;
  GBackground: TNyxDocument;
  GTemplate: INyxConfirmationDialog;
  GPage: INyxPage;
  GInvoker: INyxButton;
  GRetained: INyxConfirmationDialog;
  GCancelSubscription: INyxEventSubscription;
  GConfirmSubscription: INyxEventSubscription;
  GExtraSubscription: INyxEventSubscription;
  {$IFDEF PAS2JS}
  GPrompt: INyxBrowserConfirmation;
  GView: TNyxBrowserRenderer;
  GHost: TJSHTMLElement;
  {$ELSE}
  GPrompt: INyxLCLConfirmation;
  GView: TNyxLCLRenderer;
  GHost: TForm;
  GClosingObserver: TClosingObserver;
  {$ENDIF}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Confirmation: ' + AReason);
  end;
  Inc(GChecks);
end;

{$IFNDEF PAS2JS}
procedure TClosingObserver.WindowHide(ASender: TObject);
begin
  try
    GPrompt.Open(NyxConfirmation('Reentrant invoker'));
  except
    on E: ENyxModel do
    begin
      Inc(GTransitionRefusals);
    end;
  end;
end;
{$ENDIF}

constructor TCompletionObserver.Create(AMarker: Integer);
begin
  inherited Create;
  Marker := AMarker;
end;

procedure TCompletionObserver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  GSnapshot := AEvent.Copy;

  if Marker = 5 then
  begin
    GPrompt := nil;
    Exit;
  end;

  if Marker = 6 then
  begin
    try
      GPrompt.Open(NyxConfirmation('Reentrant invoker'));
    except
      on E: ENyxModel do
      begin
        Inc(GTransitionRefusals);
      end;
    end;
    Exit;
  end;

  if Marker = 0 then
  begin
    Inc(GExtra);
    Exit;
  end;
  GOrder := GOrder + TNyxText(IntToStr(Marker));

  if AEvent.IsNamed(NyxSemantic(nseConfirm)) then
  begin
    Inc(GConfirmed);
  end
  else
  begin
    Inc(GCancelled);
  end;

  if GReopen then
  begin
    GReopen := False;
    GPrompt.Open(NyxConfirmation('Remove this project?'));
  end;

  if GRelease then
  begin
    GRelease := False;
    GPrompt := nil;
  end;
end;

procedure Pump;
begin
  {$IFNDEF PAS2JS}
  Application.ProcessMessages;
  {$ENDIF}
end;

function NewPrompt: {$IFDEF PAS2JS}INyxBrowserConfirmation{$ELSE}INyxLCLConfirmation{$ENDIF};
begin
  {$IFDEF PAS2JS}
  Result := NewNyxBrowserConfirmation(GTemplate);
  {$ELSE}
  Result := NewNyxLCLConfirmation(GHost, GTemplate);
  {$ENDIF}
end;

procedure ReleaseDuringInitialFocus;
begin
  { Compiler-managed factory temporaries retire at the procedure boundary.
    Observe final host retirement only after this scope has returned. }
  GPrompt := NewPrompt;
  GPrompt.Events.OnAfterEnter(NyxControlEvents(GPrompt.Content.ActionsCancelButton.ID))
    .Subscribe(TCompletionObserver.Create(5));
  GPrompt.Open(NyxConfirmation('Remove this project?'));
  Check(GPrompt = nil, 'initial focus callback can release the presenter safely');
end;

procedure FocusInvoker;
begin
  {$IFDEF PAS2JS}
  NyxFocusWithoutScroll(GView.FocusFor('open-confirmation'));
  {$ELSE}
  GHost.BringToFront;
  GView.FocusFor('open-confirmation').SetFocus;
  {$ENDIF}
  Pump;
end;

function InvokerFocused: Boolean;
begin
  {$IFDEF PAS2JS}
  Result := document.activeElement = GView.FocusFor('open-confirmation');
  {$ELSE}
  Result := Screen.ActiveControl = GView.FocusFor('open-confirmation');
  {$ENDIF}
end;

function Focused(const AID: TNyxText): Boolean;
begin
  {$IFDEF PAS2JS}
  Result := document.activeElement = GPrompt.Renderer.FocusFor(AID);
  {$ELSE}
  Result := Screen.ActiveControl = GPrompt.Renderer.FocusFor(AID);
  {$ENDIF}
end;

function HostOpen: Boolean;
begin
  { Scope the retained host observation here. pas2js releases temporary
    interfaces at procedure exit; a whole-journey temporary would deliberately
    keep the otherwise disposed host alive until the journey returns. }
  Result := GPrompt.Host.IsOpen;
end;

function CompactHost: Boolean;
begin
  {$IFDEF PAS2JS}
  Result := GPrompt.Host.Element.getBoundingClientRect.height <= 362;
  {$ELSE}
  Result := GPrompt.Host.Control.Height <= 360;
  {$ENDIF}
end;

function HostTitle: TNyxText;
begin
  {$IFDEF PAS2JS}
  Result := GPrompt.Host.Element.getAttribute('aria-label');
  {$ELSE}
  Result := TForm(GPrompt.Host.Control).Caption;
  {$ENDIF}
end;

function ActionFits(const AID: TNyxText): Boolean;
var
  {$IFDEF PAS2JS}
  LFace: TJSDOMRect;
  LHost: TJSDOMRect;
  {$ELSE}
  LFace: TControl;
  LPoint: TPoint;
  LHostPoint: TPoint;
  {$ENDIF}
begin
  {$IFDEF PAS2JS}
  LFace := GPrompt.Renderer.ElementFor(AID).getBoundingClientRect;
  LHost := GPrompt.Host.Element.getBoundingClientRect;
  Result := (LFace.width > 0) and (LFace.height > 0) and
    (LFace.top >= LHost.top) and (LFace.bottom <= LHost.bottom) and
    (LFace.left >= LHost.left) and (LFace.right <= LHost.right);
  {$ELSE}
  LFace := GPrompt.Renderer.ControlFor(AID);
  LPoint := LFace.ClientToScreen(Point(0, 0));
  LHostPoint := GPrompt.Host.Control.ClientToScreen(Point(0, 0));
  Result := LFace.Visible and (LFace.Width > 0) and (LFace.Height > 0) and
    (LPoint.Y >= LHostPoint.Y) and
    (LPoint.Y + LFace.Height <= LHostPoint.Y + GPrompt.Host.Control.ClientHeight) and
    (LPoint.X >= LHostPoint.X) and
    (LPoint.X + LFace.Width <= LHostPoint.X + GPrompt.Host.Control.ClientWidth);
  {$ENDIF}
end;

procedure Click(const AID: TNyxText);
begin
  {$IFDEF PAS2JS}
  GPrompt.Renderer.ElementFor(AID).click;
  {$ELSE}
  TControlAccess(GPrompt.Renderer.ControlFor(AID)).Click;
  {$ENDIF}
  Pump;
end;

procedure Escape;
var
  {$IFDEF PAS2JS}
  LOptions: TJSObject;
  LEvent: TJSEvent;
  {$ELSE}
  LKey: Word;
  {$ENDIF}
begin
  {$IFDEF PAS2JS}
  LOptions := TJSObject.new;
  LOptions['cancelable'] := True;
  LEvent := TCancelableEvent.new('cancel', LOptions);
  GPrompt.Host.Element.dispatchEvent(LEvent);
  Check(LEvent.defaultPrevented, 'browser cancel remains controller owned');
  {$ELSE}
  LKey := VK_ESCAPE;
  TForm(GPrompt.Host.Control).OnKeyDown(GPrompt.Host.Control, LKey, []);
  Check(LKey = 0, 'native Escape is consumed');
  {$ENDIF}
  Pump;
end;

procedure Capture;
{$IFNDEF PAS2JS}
var
  LBitmap: TBitmap;
  LImage: TLazIntfImage;
  LWriter: TFPWriterPNG;
  LBounds: TRect;
{$ENDIF}
begin
  {$IFNDEF PAS2JS}
  GPrompt.Host.Control.Repaint;
  Application.ProcessMessages;
  WriteLn('Modal geometry: host ', GPrompt.Host.Control.Width, 'x',
    GPrompt.Host.Control.Height, ' / client ', GPrompt.Host.Control.ClientWidth, 'x',
    GPrompt.Host.Control.ClientHeight, ' / root ',
    GPrompt.Renderer.ControlFor(GPrompt.Content.ID).Width, 'x',
    GPrompt.Renderer.ControlFor(GPrompt.Content.ID).Height, ' / cancel ',
    GPrompt.Renderer.ControlFor(GPrompt.Content.ActionsCancelButton.ID).Width, 'x',
    GPrompt.Renderer.ControlFor(GPrompt.Content.ActionsCancelButton.ID).Height);
  LBitmap := TBitmap.Create;
  LImage := nil;
  LWriter := nil;
  try
    { Win32 PaintTo includes native decorations. A client-sized bitmap crops
      otherwise visible actions; capture the actual window rectangle instead. }
    Check(GetWindowRect(GPrompt.Host.Control.Handle, LBounds) <> 0,
      'native capture obtains its actual window rectangle');
    LBitmap.SetSize(LBounds.Right - LBounds.Left, LBounds.Bottom - LBounds.Top);
    GPrompt.Host.Control.PaintTo(LBitmap.Canvas, 0, 0);
    LImage := LBitmap.CreateIntfImage;
    LWriter := TFPWriterPNG.Create;
    LImage.SaveToFile(ParamStr(1), LWriter);
  finally
    LWriter.Free;
    LImage.Free;
    LBitmap.Free;
  end;
  {$ENDIF}
end;

procedure Journey;
var
  LOther: INyxConfirmation;
  LOptions: TNyxConfirmationOptions;
  LOriginal: TNyxText;
  LFailed: Boolean;
begin
  GDocument := BuildNyxDocument;
  GTemplate := RetainNyxControl(GDocument.Pages[0].Find('remove-project')) as INyxConfirmationDialog;
  GBackground := TNyxDocument.Create;
  GPage := NewNyxPage('background');
  GBackground.AddPage(GPage);
  GInvoker := NewNyxButton('open-confirmation');
  GInvoker.Configure.Text('Review project removal').Done;
  GPage.Add(GInvoker);
  {$IFDEF PAS2JS}
  GHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(GHost);
  GView := TNyxBrowserRenderer.Create;
  {$ELSE}
  Application.Initialize;
  GHost := TForm.CreateNew(nil);
  GHost.SetBounds(20, 20, 1100, 800);
  GHost.Show;
  GView := TNyxLCLRenderer.Create;
  {$ENDIF}
  GView.Render(GBackground, GPage.Node, GHost);
  Pump;
  FocusInvoker;
  Check(InvokerFocused, 'ordinary Nyx invoker takes focus');
  GPrompt := NewPrompt;
  GRetained := GPrompt.Content;
  LOther := NewPrompt;
  LOriginal := GTemplate.TitleHeading.Text;
  GPrompt.Content.TitleHeading.Text := 'Independent content';
  Check((GTemplate.TitleHeading.Text = LOriginal) and
    (LOther.Content.TitleHeading.Text = LOriginal), 'template and two presenters own independent parts');
  GPrompt.Content.TitleHeading.Text := LOriginal;
  LOther := nil;
  Check(GPrompt.Content.ActionsRow.Count = 3, 'semantic companion retains arbitrary added action');
  Check(GPrompt.Result = ncrNotOpened, 'initial presentation has no result');

  GConfirmSubscription := GPrompt.OnConfirm.Subscribe(TCompletionObserver.Create(1));
  GPrompt.OnConfirm.Subscribe(TCompletionObserver.Create(2));
  GCancelSubscription := GPrompt.OnCancel.Subscribe(TCompletionObserver.Create(3));
  GExtraSubscription := GPrompt.Events.On(NyxControlEvents('learn-more'), ntClick)
    .Subscribe(TCompletionObserver.Create(0));
  Check(GPrompt.OnConfirm.Count = 2, 'multiple callbacks use ordinary event streams');

  LFailed := False;
  try
    GPrompt.Open(Default(TNyxConfirmationOptions));
  except
    on E: ENyxModel do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and not GPrompt.IsOpen and (GPrompt.Result = ncrNotOpened),
    'invalid options refuse without result/background publication');
  LOptions := NyxConfirmation('Remove this project?').Focus(NyxPart('missing'));
  LFailed := False;
  try
    GPrompt.Open(LOptions);
  except
    on E: ENyxModel do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and not GPrompt.IsOpen, 'missing named focus refuses');
  GPrompt.Content.ActionsCancelButton.Configure.Enabled(False).Done;
  LFailed := False;
  try
    GPrompt.Open(NyxConfirmation('Remove this project?'));
  except
    on E: ENyxModel do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and not HostOpen and InvokerFocused,
    'unavailable initial focus refuses without leaving the owner isolated');
  GPrompt.Content.ActionsCancelButton.Configure.Enabled(True).Done;

  GPrompt.Open(NyxConfirmation('Remove this project?'));
  Pump;
  Check(GPrompt.IsOpen and HostOpen and (GPrompt.Result = ncrPending),
    'actual modal awaits an explicit result');
  Check(Focused(GPrompt.Content.ActionsCancelButton.ID), 'safe Cancel part receives initial focus');
  Check(ActionFits(GPrompt.Content.ActionsCancelButton.ID) and
    ActionFits(GPrompt.Content.ActionsConfirmButton.ID) and ActionFits('learn-more'),
    'content sizing keeps all three actions fully visible');
  {$IFDEF PAS2JS}
  Check(HostTitle = 'Remove this project?',
    'standard dialog has its accessible title');
  Check(CompactHost,
    'compact height cap applies to the real dialog');
  {$ELSE}
  Check(not GHost.Enabled, 'owning native window is inert while awaiting a result');
  Check(CompactHost, 'compact height cap applies to the real window');
  {$ENDIF}
  Capture;
  LFailed := False;
  try
    GPrompt.Open(NyxConfirmation('Another title'));
  except
    on E: ENyxModel do
    begin
      LFailed := True;
    end;
  end;
  Check(LFailed and GPrompt.IsOpen and (GPrompt.Result = ncrPending),
    'duplicate opening refuses without replacing the active prompt');
  Click('learn-more');
  Check((GExtra = 1) and GPrompt.IsOpen and (GConfirmed = 0),
    'custom action uses ordinary events without resolving confirmation');
  Escape;
  Check(not GPrompt.IsOpen and not HostOpen and
    (GPrompt.Result = ncrCancelled) and (GCancelled = 1), 'Escape resolves and notifies Cancel once');
  Check(InvokerFocused, 'Escape returns focus to the surviving Nyx invoker');
  GPrompt.Close;
  Check(GCancelled = 1, 'closing an already resolved prompt is silent');

  FocusInvoker;
  GPrompt.Open(NyxConfirmation('Remove this project?'));
  GOrder := '';
  Click(GPrompt.Content.ActionsConfirmButton.ID);
  Check(not GPrompt.IsOpen and (GPrompt.Result = ncrConfirmed) and
    (GConfirmed = 2) and (GOrder = '12'), 'Confirm closes then invokes registrations in order');
  Check(GSnapshot.IsNamed(NyxSemantic(nseConfirm)) and
    (GSnapshot.SourceID = 'remove-project'), 'completion owns exact semantic source/name');
  Check(InvokerFocused, 'Confirm restores the surviving invoker');
  GPrompt.Open(NyxConfirmation('Remove this project?'));
  Click(GPrompt.Content.ActionsCancelButton.ID);
  Check((GPrompt.Result = ncrCancelled) and (GCancelled = 2), 'visible Cancel shares typed cancellation');
  GPrompt.Open(NyxConfirmation('Remove this project?'));
  GPrompt.Close;
  Check((GCancelled = 2) and (GPrompt.Result = ncrCancelled), 'programmatic close spends no callback');

  GPrompt.Open(NyxConfirmation('Remove this project?'));
  GReopen := True;
  Click(GPrompt.Content.ActionsConfirmButton.ID);
  Check(GPrompt.IsOpen and (GPrompt.Result = ncrPending) and
    Focused(GPrompt.Content.ActionsCancelButton.ID), 'completion can reopen after the previous window closes');
  GPrompt.Close;

  GPrompt.Open(NyxConfirmation('Remove this project?'));
  GRelease := True;
  Click(GPrompt.Content.ActionsConfirmButton.ID);
  Check(GPrompt = nil, 'completion may release the presenter safely');
  Check(not GConfirmSubscription.Active and not GCancelSubscription.Active and
    not GExtraSubscription.Active, 'retained subscriptions retire with the presentation');
  Check(GRetained.TitleHeading.Text = LOriginal, 'retained specialized content survives presenter destruction');
  Check(GTemplate.TitleHeading.Text = LOriginal, 'presentation never changes the original semantic design');

  GPrompt := NewPrompt;
  GPrompt.OnCancel.Subscribe(TCompletionObserver.Create(3));
  GPrompt.Open(NyxConfirmation('Remove this project?'));
  GRelease := True;
  Escape;
  Check(GPrompt = nil, 'Escape completion can release its controller and host safely');

  GPrompt := NewPrompt;
  FocusInvoker;
  GPrompt.Open(NyxConfirmation('Remove this project?'));
  GView.Unmount;
  GPrompt.Close;
  Check(not HostOpen and not GPrompt.IsOpen,
    'removing the invoking control while open leaves a safe cancelled presentation');
  GPrompt := nil;
  GView.Render(GBackground, GPage.Node, GHost);
  Pump;

  GPrompt := NewPrompt;
  FocusInvoker;
  {$IFDEF PAS2JS}
  GExtraSubscription := GView.Events.OnAfterEnter(NyxControlEvents('open-confirmation'))
    .Subscribe(TCompletionObserver.Create(6));
  {$ELSE}
  { Win32 may retain the invoker's active-control slot across form activation,
    without another OnEnter. Its real window-hide callback exercises the same
    unfinished close boundary without fabricating a focus notification. }
  GClosingObserver := TClosingObserver.Create;
  TForm(GPrompt.Host.Control).OnHide := GClosingObserver.WindowHide;
  {$ENDIF}
  GPrompt.Open(NyxConfirmation('Remove this project?'));
  GPrompt.Close;
  Check((GTransitionRefusals > 0) and not GPrompt.IsOpen and InvokerFocused,
    'host/focus callbacks cannot reopen during an unfinished close');
  {$IFDEF PAS2JS}
  GExtraSubscription.Cancel;
  {$ELSE}
  TForm(GPrompt.Host.Control).OnHide := nil;
  FreeAndNil(GClosingObserver);
  {$ENDIF}
  GPrompt := nil;

  ReleaseDuringInitialFocus;
  Pump;

  {$IFDEF PAS2JS}
  Check(document.querySelectorAll('dialog[data-nyx-modal]').length = 0,
    'both independently owned target hosts retire');
  { The final visible review is an ordinary live presenter. Earlier teardown
    checks above ran on actual controls, rather than inferring it from a photo. }
  GPrompt := NewPrompt;
  GPrompt.Open(NyxConfirmation('Remove this project?'));
  document.body.setAttribute('data-confirmation-checks', IntToStr(GChecks));
  document.body.setAttribute('data-confirmation', 'passed');
  {$ELSE}
  Check(GHost.Enabled, 'native owner is restored after presenter teardown');
  WriteLn('PASS ', GChecks, ' actual native confirmation checks');
  {$ENDIF}
end;

begin
  try
    Journey;
  except
    on E: Exception do
    begin
      {$IFDEF PAS2JS}
      document.body.setAttribute('data-confirmation', 'failed');
      document.body.setAttribute('data-confirmation-error', E.Message);
      {$ELSE}
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
      {$ENDIF}
    end;
  end;
  {$IFNDEF PAS2JS}
  GPrompt := nil;
  GRetained := nil;
  GConfirmSubscription := nil;
  GCancelSubscription := nil;
  GExtraSubscription := nil;
  GView.Free;
  GInvoker := nil;
  GPage := nil;
  GBackground.Free;
  GTemplate := nil;
  GDocument.Free;
  GHost.Free;
  {$ENDIF}
end.
