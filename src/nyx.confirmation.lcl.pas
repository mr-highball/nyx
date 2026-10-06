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

unit nyx.confirmation.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, Controls, Forms, nyx.text, nyx.model, nyx.controls, nyx.theme,
  nyx.behavior, nyx.events, nyx.confirmation, nyx.modal.lcl, nyx.render.lcl;

type
  { Renderer/control observations are borrowed; Host returns a retained managed
    interface, closed and disconnected after presenter retirement. Owner must
    outlive both presenter and any retained host. Unmount precedes host release. }
  INyxLCLConfirmation = interface(INyxConfirmation)
    ['{271BE5DA-8A62-4F45-A8BC-001006000003}']
    function GetRenderer: TNyxLCLRenderer;
    function GetHost: INyxLCLModalHost;
    property Renderer: TNyxLCLRenderer read GetRenderer;
    property Host: INyxLCLModalHost read GetHost;
  end;

{ Copies template including independently owned parts. Optional theme and native
  owner are borrowed and must outlive this managed presentation. }
function NewNyxLCLConfirmation(AOwner: TWinControl;
  const ATemplate: INyxConfirmationDialog;
  ATheme: TNyxTheme = nil): INyxLCLConfirmation;

implementation

uses
  Math, nyx.types, nyx.interaction;

type
  { LCL controls may be deleted while a prompt is open. FreeNotification tracks
    the invoker weakly; no stale widget pointer or reference cycle is retained. }
  TNyxConfirmationFocus = class(TComponent)
  private
    FControl: TWinControl;
  protected
    procedure Notification(AComponent: TComponent;
      AOperation: TOperation); override;
  public
    destructor Destroy; override;
    procedure Capture;
    procedure Restore;
  end;

  TNyxLCLConfirmation = class(TNyxConfirmationPresenter,
    INyxConfirmation, INyxLCLConfirmation)
  private
    FRenderer: TNyxLCLRenderer;
    FHost: INyxLCLModalHost;
    FFocus: TNyxConfirmationFocus;
    procedure Semantic(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  protected
    procedure Present(const AOptions: TNyxConfirmationOptions;
      const AFocusID: TNyxText); override;
    procedure Conceal; override;
  public
    constructor Create(AOwner: TWinControl;
      const ATemplate: INyxConfirmationDialog; ATheme: TNyxTheme);
    destructor Destroy; override;
    function GetRenderer: TNyxLCLRenderer;
    function GetHost: INyxLCLModalHost;
    function GetEvents: INyxEvents; override;
  end;

destructor TNyxConfirmationFocus.Destroy;
begin

  if FControl <> nil then
  begin
    FControl.RemoveFreeNotification(Self);
  end;
  inherited Destroy;
end;

procedure TNyxConfirmationFocus.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);

  if (AOperation = opRemove) and (AComponent = FControl) then
  begin
    FControl := nil;
  end;
end;

procedure TNyxConfirmationFocus.Capture;
begin

  if FControl <> nil then
  begin
    FControl.RemoveFreeNotification(Self);
  end;
  FControl := Screen.ActiveControl;

  if FControl <> nil then
  begin
    FControl.FreeNotification(Self);
  end;
end;

procedure TNyxConfirmationFocus.Restore;
var
  LControl: TWinControl;
begin
  LControl := FControl;
  FControl := nil;

  if LControl <> nil then
  begin
    LControl.RemoveFreeNotification(Self);

    if LControl.CanSetFocus then
    begin
      LControl.SetFocus;
    end;
  end;
end;

constructor TNyxLCLConfirmation.Create(AOwner: TWinControl;
  const ATemplate: INyxConfirmationDialog; ATheme: TNyxTheme);
begin
  inherited Create(ATemplate);
  FHost := NewNyxLCLModalHost(AOwner);
  FHost.OnDismiss := Dismiss;
  FFocus := TNyxConfirmationFocus.Create(nil);
  FRenderer := TNyxLCLRenderer.Create(ATheme);
  FRenderer.OnEvent := Semantic;
end;

destructor TNyxLCLConfirmation.Destroy;
begin

  if FHost <> nil then
  begin
    FHost.OnDismiss := nil;
    Conceal;
  end;

  if FRenderer <> nil then
  begin
    FRenderer.OnEvent := nil;
    FRenderer.Free;
  end;
  FFocus.Free;
  FHost := nil;
  inherited Destroy;
end;

procedure TNyxLCLConfirmation.Semantic(ANode: TNyxNode;
  const AEvent: TNyxEventInfo);
begin
  Accept(AEvent);
end;

procedure TNyxLCLConfirmation.Present(const AOptions: TNyxConfirmationOptions;
  const AFocusID: TNyxText);
var
  LFocus: TWinControl;
  LHeight: Integer;
begin
  FFocus.Capture;
  FRenderer.Render(Document, GetContent.Node, FHost.Control);
  LFocus := FRenderer.FocusFor(AFocusID, niDesign);

  if (LFocus = nil) or not NyxInteractionPolicy(
    FRenderer.Root.Find(AFocusID)).CanIssueCommand then
  begin
    raise ENyxModel.Create('Confirmation initial part has no available focus face');
  end;
  try
    FHost.Show(AOptions.Modal);
    { Show changes the actual allocation/font context. Settle ordinary Nyx
      layout synchronously before measuring; no nested message pump is used. }
    FRenderer.Sync;

    if AOptions.SizeMode = ncsContent then
    begin
      { Native height includes caption/border; logical content measurement is
        borrowed only after the actual host has published its width. }
      LHeight := Max(120, FRenderer.ControlFor(GetContent.ID).Height +
        FHost.Control.Height - FHost.Control.ClientHeight);

      if AOptions.Modal.HeightLimit <> 0 then
      begin
        LHeight := Min(LHeight, AOptions.Modal.HeightLimit);
      end;
      FHost.Show(AOptions.Modal.MaximumHeight(LHeight));
      FRenderer.Sync;
    end;

    if not LFocus.CanSetFocus then
    begin
      raise ENyxModel.Create('Confirmation initial part cannot receive focus');
    end;
    LFocus.SetFocus;

    if Screen.ActiveControl <> LFocus then
    begin
      raise ENyxModel.Create('Confirmation initial focus was refused');
    end;
  except
    Conceal;
    raise;
  end;
end;

procedure TNyxLCLConfirmation.Conceal;
begin
  FHost.Hide;

  if FFocus <> nil then
  begin
    FFocus.Restore;
  end;
end;

function TNyxLCLConfirmation.GetRenderer: TNyxLCLRenderer;
begin
  Result := FRenderer;
end;

function TNyxLCLConfirmation.GetHost: INyxLCLModalHost;
begin
  Result := FHost;
end;

function TNyxLCLConfirmation.GetEvents: INyxEvents;
begin
  Result := FRenderer.Events;
end;

function NewNyxLCLConfirmation(AOwner: TWinControl;
  const ATemplate: INyxConfirmationDialog; ATheme: TNyxTheme): INyxLCLConfirmation;
begin
  Result := TNyxLCLConfirmation.Create(AOwner, ATemplate, ATheme);
end;

end.
