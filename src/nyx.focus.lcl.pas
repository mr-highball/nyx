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

unit nyx.focus.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Controls, Forms;

type
  { Independently owned, UI-thread-only focus return for a native component
    action. Context/editor/current and host focus are weak observations through
    LCL free notifications. This helper never owns widgets, retains a document
    or creates a renderer/tree cycle. The caller owns it with try/finally; the
    context must not own it because a callback may destroy it during this call. }
  TNyxLCLFocusReturn = class(TComponent)
  private
    FContext: TComponent;
    FEditor: TWinControl;
    FBefore: TWinControl;
    FHostBefore: TWinControl;
    FApplicationMoved: Boolean;
    FCaptured: Boolean;
    FRestored: Boolean;
    function GetContextAlive: Boolean;
    procedure ReleaseBefore;
    procedure ReleaseHostBefore;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    { Both borrowed arguments are required. Nil raises EArgumentException. }
    constructor CreateFor(AContext: TComponent; AEditor: TWinControl);
    destructor Destroy; override;
    { Call immediately before the application's commit callback, before any
      conceal action that can itself restore native focus. This establishes
      the baseline without moving it. }
    procedure Capture;
    { Sample after commit callbacks and immediately before hiding a popup.
      Native Hide may automatically restore the host's previous active control;
      that is distinct from an application's explicit move during the commit. }
    procedure BeforeConceal;
    { At most one default restoration per Capture/BeforeConceal pair. An
      application's explicit move to another live control wins. Destroyed,
      destroying, hidden/disabled or otherwise unfocusable targets refuse
      silently. No widget access follows
      SetFocus: its native focus callbacks may destroy context/editor. Exceptions
      propagate; the caller's finally still removes every weak notification. }
    procedure Restore;
    { A caller may inspect its context only while this is True, on the UI thread.
      Adapter-specific disconnect state remains the caller's additional guard. }
    property ContextAlive: Boolean read GetContextAlive;
  end;

implementation

constructor TNyxLCLFocusReturn.CreateFor(AContext: TComponent; AEditor: TWinControl);
begin
  inherited Create(nil);

  if (AContext = nil) or (AEditor = nil) then
  begin
    raise EArgumentException.Create('Native focus return requires a context and editor');
  end;
  FContext := AContext;
  FEditor := AEditor;
  FContext.FreeNotification(Self);

  if FEditor <> FContext then
  begin
    FEditor.FreeNotification(Self);
  end;
end;

destructor TNyxLCLFocusReturn.Destroy;
begin
  ReleaseHostBefore;
  ReleaseBefore;

  if FEditor <> nil then
  begin
    FEditor.RemoveFreeNotification(Self);
  end;

  if (FContext <> nil) and (FContext <> FEditor) then
  begin
    FContext.RemoveFreeNotification(Self);
  end;
  inherited Destroy;
end;

procedure TNyxLCLFocusReturn.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);

  if AOperation = opRemove then
  begin

    if AComponent = FContext then
    begin
      FContext := nil;
    end;

    if AComponent = FEditor then
    begin
      FEditor := nil;
    end;

    if AComponent = FBefore then
    begin
      FBefore := nil;
    end;

    if AComponent = FHostBefore then
    begin
      FHostBefore := nil;
    end;
  end;
end;

function TNyxLCLFocusReturn.GetContextAlive: Boolean;
begin
  Result := (FContext <> nil) and (FEditor <> nil) and
    not (csDestroying in FContext.ComponentState) and
    not (csDestroying in FEditor.ComponentState);
end;

procedure TNyxLCLFocusReturn.ReleaseBefore;
begin

  if (FBefore <> nil) and (FBefore <> FEditor) and (FBefore <> FContext) then
  begin
    FBefore.RemoveFreeNotification(Self);
  end;
  FBefore := nil;
end;

procedure TNyxLCLFocusReturn.ReleaseHostBefore;
begin

  if (FHostBefore <> nil) and (FHostBefore <> FEditor) and
    (FHostBefore <> FContext) and (FHostBefore <> FBefore) then
  begin
    FHostBefore.RemoveFreeNotification(Self);
  end;
  FHostBefore := nil;
end;

procedure TNyxLCLFocusReturn.Capture;
var
  LHost: TCustomForm;
begin
  ReleaseHostBefore;
  ReleaseBefore;
  FCaptured := False;

  if not ContextAlive then
  begin
    Exit;
  end;
  FBefore := Screen.ActiveControl;

  if (FBefore <> nil) and (FBefore <> FEditor) and (FBefore <> FContext) then
  begin
    FBefore.FreeNotification(Self);
  end;
  LHost := GetParentForm(FEditor);

  if LHost <> nil then
  begin
    FHostBefore := LHost.ActiveControl;

    if (FHostBefore <> nil) and (FHostBefore <> FEditor) and
      (FHostBefore <> FContext) and (FHostBefore <> FBefore) then
    begin
      FHostBefore.FreeNotification(Self);
    end;
  end;
  FCaptured := True;
  FRestored := False;
  FApplicationMoved := False;
end;

procedure TNyxLCLFocusReturn.BeforeConceal;
var
  LCurrent: TWinControl;
begin
  LCurrent := Screen.ActiveControl;
  FApplicationMoved := (LCurrent <> nil) and (LCurrent <> FBefore) and
    (LCurrent <> FEditor);
end;

procedure TNyxLCLFocusReturn.Restore;
var
  LEditor: TWinControl;
  LCurrent: TWinControl;
begin

  if not FCaptured or FRestored or FApplicationMoved or not ContextAlive then
  begin
    Exit;
  end;
  FRestored := True;
  LEditor := FEditor;
  LCurrent := Screen.ActiveControl;

  if (LCurrent = LEditor) or ((LCurrent <> nil) and (LCurrent <> FBefore) and
    (LCurrent <> FHostBefore)) or
    not LEditor.CanSetFocus then
  begin
    Exit;
  end;
  LEditor.SetFocus;
end;

end.
