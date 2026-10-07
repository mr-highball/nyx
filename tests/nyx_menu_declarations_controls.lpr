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

program nyx_menu_declarations_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$IFDEF PAS2JS}JS, Web, nyx.application.browser, nyx.menu.browser,
  {$ELSE}Interfaces, Classes, Forms, Controls, StdCtrls, nyx.application.lcl,
    nyx.menu.lcl, nyx.studio.lcl, nyx.studio.projects,{$ENDIF}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.controls,
  nyx.menu, nyx.behavior, nyx.events, nyx.scheduler,
  {$IFDEF NYX_MCP_MENU}nyx.generated.view{$ELSE}nyx.generated.menu{$ENDIF};

type
  { The router owns this callback. It copies only the detached completion and
    does not retain the application, document or menu back into its owner. }
  TCompletion = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$IFNDEF PAS2JS}TControlAccess = class(TWinControl);{$ENDIF}

var
  GApplication: {$IFDEF PAS2JS}TNyxBrowserApplication{$ELSE}TNyxLCLApplication{$ENDIF};
  GDocument: TNyxDocument;
  GMenu: INyxMenu;
  GDensity: INyxMenu;
  GToken: INyxEventSubscription;
  GLast: TNyxMenuInvocation;
  GTarget: TNyxText;
  GCalls: Integer;
  GChecks: Integer;
  GWire: TNyxText;
  {$IFDEF PAS2JS}GResume: NativeInt;{$ENDIF}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure TCompletion.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  GLast := NyxMenuInvocation(AEvent);
  GTarget := AEvent.TargetID;
  Inc(GCalls);
end;

procedure Pump;
begin
  {$IFNDEF PAS2JS}Application.ProcessMessages;{$ENDIF}
end;

procedure OpenInvoker;
begin
  {$IFDEF PAS2JS}
  GApplication.View.FocusFor('open-actions').click;
  {$ELSE}
  TButton(GApplication.View.FocusFor('open-actions')).Click;
  {$ENDIF}
  Pump;
end;

procedure ClickPart(const AMenu: INyxMenu; const APart: TNyxPartRef);
begin
  { Target controls emit the ordinary renderer input. Calling Menu.Open or
    Events.Dispatch here would fail to qualify the mounted application binding. }
  {$IFDEF PAS2JS}
  (AMenu as INyxBrowserMenu).Presentation.Renderer
    .FocusFor(AMenu.Content.Part(APart).ID).click;
  {$ELSE}
  TButton((AMenu as INyxLCLMenu).Presentation.Renderer
    .FocusFor(AMenu.Content.Part(APart).ID)).Click;
  {$ENDIF}
  Pump;
end;

procedure Cleanup;
begin
  {$IFDEF PAS2JS}

  if GResume <> 0 then
  begin
    window.clearInterval(GResume);
    GResume := 0;
  end;
  {$ENDIF}
  GToken := nil;
  GDensity := nil;
  GMenu := nil;
  FreeAndNil(GApplication);
  FreeAndNil(GDocument);
end;

procedure Fail(const AMessage: TNyxText);
begin
  {$IFDEF PAS2JS}
  document.body.setAttribute('data-result', 'failed');
  document.body.setAttribute('data-event-error', AMessage);
  {$ELSE}
  WriteLn('FAIL ', AMessage);
  DumpExceptionBackTrace(Output);
  ExitCode := 1;
  {$ENDIF}
  Cleanup;
end;

{$IFNDEF PAS2JS}
{ Exercise ordinary Studio with the same accepted compiled source. This owns a
  separate local project directory and never talks to the observing LAN server. }
procedure StudioJourney;
var
  LForm: TForm;
  LStudio: TNyxNativeStudio;
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LBefore: TNyxText;
  LFile: TFileStream;
  LIndex: Integer;

  procedure Settle;
  var
    LStarted: QWord;
  begin
    LStarted := GetTickCount64;
    repeat
      Application.ProcessMessages;
      CheckSynchronize;

      if GetTickCount64 - LStarted > 30000 then
      begin
        raise Exception.Create('Studio menu presentation exceeded its budget');
      end;
      Sleep(1);
    until not LStudio.PresentationPending;
  end;

  function HasMenu: Boolean;
  var
    LFormIndex: Integer;
  begin
    Result := False;
    for LFormIndex := 0 to Screen.FormCount - 1 do
    begin

      if Screen.Forms[LFormIndex].Visible and
        (Screen.Forms[LFormIndex].Caption = 'Thoughtful actions') then
      begin
        Exit(True);
      end;
    end;
  end;

begin

  if ParamCount <> 2 then
  begin
    Exit;
  end;
  LForm := TForm.CreateNew(nil);
  LStudio := nil;
  LDocument := nil;
  try
    LForm.SetBounds(30, 30, 1280, 940);
    LForm.Show;
    LStudio := TNyxNativeStudio.Create(LForm, ParamStr(2));
    LDocument := BuildNyxDocument;
    LFile := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LSource, LFile.Size);
      LFile.ReadBuffer(LSource[1], Length(LSource));
    finally
      LFile.Free;
    end;
    LStudio.LoadProject(NyxProjectPair(TNyxCodec.Encode(LDocument), LSource));
    LStudio.Session.Select('open-actions');
    LStudio.Run;
    Settle;
    LBefore := EncodeNyxProject(LStudio.Session.ProjectSnapshot);
    TControlAccess(LStudio.CanvasView.FocusFor('open-actions')).Click;
    Settle;
    Check(not HasMenu, 'Studio design selection does not invoke a declared menu');
    TControlAccess(LStudio.ShellView.ControlFor('action-preview')).Click;
    Settle;
    TControlAccess(LStudio.CanvasView.FocusFor('open-actions')).Click;
    Application.ProcessMessages;
    Check(HasMenu, 'Ordinary native Studio Interact opens its persisted menu');
    for LIndex := 0 to Screen.FormCount - 1 do
    begin

      if Screen.Forms[LIndex].Visible and
        (Screen.Forms[LIndex].Caption = 'Thoughtful actions') then
      begin
        Screen.Forms[LIndex].Close;
      end;
    end;
    TControlAccess(LStudio.ShellView.ControlFor('action-preview')).Click;
    Settle;
    TControlAccess(LStudio.CanvasView.FocusFor('open-actions')).Click;
    Settle;
    Check(not HasMenu, 'Returning to Studio design mode retires menu invocation');
    Check(EncodeNyxProject(LStudio.Session.ProjectSnapshot) = LBefore,
      'Studio menu interaction retains the exact accepted project/source pair');
  finally
    LStudio.Free;
    LDocument.Free;
    LForm.Free;
  end;
end;
{$ENDIF}

procedure Finish;
var
  LCallback: INyxEventSubscription;
begin
  ClickPart(GDensity, NyxPart('compact'));
  Check(not GMenu.IsOpen and not GDensity.IsOpen,
    'A nested mounted command closes its complete family');
  Check(GDensity.Checked(NyxPart('compact')) and not GDensity.Checked(NyxPart('roomy')),
    'The nested mounted radio updates independent exclusive runtime state');
  Check((GCalls = 2) and (GLast.Command.Name = 'compact') and
    GLast.HasChecked and GLast.Checked and (GTarget = 'open-actions'),
    'Nested command reaches the invoking application control with a typed snapshot');
  Check(TNyxCodec.Encode(GDocument) = GWire,
    'Runtime check/radio interaction does not edit document defaults');
  GDensity := nil;
  GMenu := nil;
  { The ordinary application remount retires bindings before releasing controls.
    A previously registered callback still belongs to its runtime event router. }
  GApplication.ShowPage('home');
  Pump;
  GMenu := GApplication.Menus.Menu(NyxControl('open-actions'));
  OpenInvoker;
  Check(GMenu.IsOpen and GMenu.Checked(NyxPart('guides')),
    'Application remount binds the saved defaults to an independent family');
  GMenu.Close;
  LCallback := GToken;
  Cleanup;
  Check(not LCallback.Active,
    'Application retirement cancels router callbacks without retaining the tree');
  LCallback := nil;
  {$IFNDEF PAS2JS}StudioJourney;{$ENDIF}
  WriteLn('PASS ', GChecks, ' generated menu application checks');
  {$IFDEF PAS2JS}
  document.body.setAttribute('data-result', 'passed');
  document.body.setAttribute('data-checks', IntToStr(GChecks));
  {$ENDIF}
end;

{$IFDEF PAS2JS}
procedure AwaitCapture;
begin

  if document.body.getAttribute('data-capture-observed') <> 'declared-menu-family' then
  begin
    Exit;
  end;
  window.clearInterval(GResume);
  GResume := 0;
  try
    Finish;
  except
    on LException: Exception do
    begin
      Fail(LException.Message);
    end;
  end;
end;
{$ENDIF}

procedure Run;
{$IFDEF PAS2JS}
var
  LHost: TJSHTMLElement;
{$ENDIF}
begin
  { This exact unit is exported by the source/semantic journey and is compiled
    unchanged for both targets. The fixture supplies no handwritten menu plan. }
  GDocument := BuildNyxDocument;
  GWire := TNyxCodec.Encode(GDocument);
  {$IFDEF PAS2JS}
  GApplication := TNyxBrowserApplication.Create;
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  GApplication.Run(GDocument, LHost);
  {$ELSE}
  GApplication := TNyxLCLApplication.Create;
  GApplication.Mount(GDocument);
  GApplication.Window.SetBounds(40, 40, 900, 740);
  GApplication.Window.Show;
  {$ENDIF}
  Pump;
  Check(GApplication.Menus.Count = 1,
    'Ordinary application automatically binds its declared mounted invoker');
  GMenu := GApplication.Menus.Menu(NyxControl('open-actions'));
  GToken := GApplication.View.Events.OnNamed(NyxControlEvents('open-actions'),
    NyxSemantic(nseActivate))
    .Subscribe(TCompletion.Create);
  OpenInvoker;
  Check(GMenu.IsOpen, 'Mounted generated invoker opens its saved menu');
  Check(GMenu.Checked(NyxPart('guides')), 'Saved check defaults reach the managed host');
  ClickPart(GMenu, NyxPart('guides'));
  Check(not GMenu.IsOpen and not GMenu.Checked(NyxPart('guides')),
    'Mounted check activation closes and changes independent state');
  Check((GCalls = 1) and (GLast.Command.Name = 'show-guides') and
    GLast.HasChecked and not GLast.Checked and (GTarget = 'open-actions'),
    'Application OnActivate receives the exact typed menu command and source');
  Check((GToken.LastExecution <> nil) and (GToken.LastExecution.Failure = ''),
    'The forwarded application callback completes successfully');
  OpenInvoker;
  ClickPart(GMenu, NyxPart('density'));
  GDensity := GMenu.Submenu(NyxPart('density'));
  Check(GMenu.IsOpen and (GDensity <> nil) and GDensity.IsOpen,
    'Mounted branch resolves saved submenu content and policy');
  Check(GDensity.Checked(NyxPart('roomy')) and not GDensity.Checked(NyxPart('compact')),
    'Saved nested radio defaults reach actual controls');
  {$IFDEF PAS2JS}
  document.body.setAttribute('data-capture-checkpoint', 'declared-menu-family');
  GResume := window.setInterval(@AwaitCapture, 30);
  {$ELSE}
  Finish;
  {$ENDIF}
end;

begin
  {$IFNDEF PAS2JS}Application.Initialize;{$ENDIF}
  try
    Run;
  except
    on LException: Exception do
    begin
      Fail(LException.Message);
    end;
  end;
end.
