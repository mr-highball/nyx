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


unit nyx.modal.lcl;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Controls, nyx.modal;

type
  { Borrow Control while this host lives; move/unmount the ordinary Nyx view
    before releasing the interface. The owning window must outlive this host. }
  INyxLCLModalHost = interface(INyxModalHost)
    ['{271BE5DA-8A62-4F45-A8BC-001005005003}']
    function GetControl: TWinControl;
    property Control: TWinControl read GetControl;
  end;

{ AOwner is borrowed until this managed host retires. Nil refuses with ENyxModel;
  the nearest owning form is isolated while open and restored by silent Hide. }
function NewNyxLCLModalHost(AOwner: TWinControl): INyxLCLModalHost;

implementation

uses
  Classes, Forms, LCLType, SysUtils, Types, Math, nyx.model, nyx.text;

type
  { Modeless message pumping keeps Studio processors alive; disabling the exact
    owner supplies modal input isolation without a nested blocking ShowModal.
    The previous Enabled state is restored, including already disabled owners. }
  TNyxLCLModalHost = class(TInterfacedObject, INyxModalHost, INyxLCLModalHost)
  private
    FOwner: TWinControl;
    FWindow: TForm;
    FOwnerEnabled: Boolean;
    FOpen: Boolean;
    FViewportPercent: Integer;
    FWidthLimit: Integer;
    FOnDismiss: TNyxModalDismiss;
    procedure WindowClose(Sender: TObject; var AAction: TCloseAction);
    procedure WindowKey(Sender: TObject; var AKey: Word; AShift: TShiftState);
    procedure Dismiss;
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    function GetOpen: Boolean;
    function GetOnDismiss: TNyxModalDismiss;
    procedure SetOnDismiss(AValue: TNyxModalDismiss);
    function GetControl: TWinControl;
    procedure Show(const AOptions: TNyxModalOptions);
    procedure Hide;
  end;

constructor TNyxLCLModalHost.Create(AOwner: TWinControl);
begin
  inherited Create;

  if AOwner = nil then
  begin
    raise ENyxModel.Create('A native modal requires an owning window');
  end;
  FOwner := GetParentForm(AOwner);

  if FOwner = nil then
  begin
    FOwner := AOwner;
  end;
  FWindow := TForm.CreateNew(nil);
  FWindow.BorderStyle := bsSizeable;
  FWindow.Position := poDesigned;
  FWindow.KeyPreview := True;
  FWindow.OnClose := WindowClose;
  FWindow.OnKeyDown := WindowKey;
end;

destructor TNyxLCLModalHost.Destroy;
begin
  FOnDismiss := nil;
  Hide;
  FWindow.Free;
  inherited Destroy;
end;

function TNyxLCLModalHost.GetOpen: Boolean;
begin
  Result := FOpen;
end;

function TNyxLCLModalHost.GetOnDismiss: TNyxModalDismiss;
begin
  Result := FOnDismiss;
end;

procedure TNyxLCLModalHost.SetOnDismiss(AValue: TNyxModalDismiss);
begin
  FOnDismiss := AValue;
end;

function TNyxLCLModalHost.GetControl: TWinControl;
begin
  Result := FWindow;
end;

procedure TNyxLCLModalHost.Show(const AOptions: TNyxModalOptions);
var
  LOptions: TNyxModalOptions;
  LWidth: Integer;
  LHeight: Integer;
  LOrigin: TPoint;
begin
  LOptions := AOptions.Viewport(AOptions.ViewportPercent).MaximumWidth(AOptions.WidthLimit);
  FWindow.Caption := LOptions.Title;

  if not FOpen or (LOptions.ViewportPercent <> FViewportPercent) or
    (LOptions.WidthLimit <> FWidthLimit) then
  begin
    { Repeated equal options preserve manual resizing. An explicit new geometry
      still applies through the same portable Show contract without reopening. }
    LWidth := Min(LOptions.WidthLimit, FOwner.ClientWidth * LOptions.ViewportPercent div 100);
    LHeight := FOwner.ClientHeight * LOptions.ViewportPercent div 100;
    LOrigin := FOwner.ClientToScreen(Point(0, 0));
    FWindow.SetBounds(LOrigin.X + (FOwner.ClientWidth - LWidth) div 2,
      LOrigin.Y + (FOwner.ClientHeight - LHeight) div 2, LWidth, LHeight);
    FViewportPercent := LOptions.ViewportPercent;
    FWidthLimit := LOptions.WidthLimit;
  end;

  if not FOpen then
  begin
    FOwnerEnabled := FOwner.Enabled;
    FOwner.Enabled := False;
    FOpen := True;
    try
      FWindow.Show;
    except
      Hide;
      raise;
    end;
  end;
end;

procedure TNyxLCLModalHost.Hide;
begin

  if FOpen then
  begin
    FOpen := False;
    FWindow.Hide;
    FOwner.Enabled := FOwnerEnabled;
  end;
end;

procedure TNyxLCLModalHost.Dismiss;
begin

  if Assigned(FOnDismiss) then
  begin
    FOnDismiss;
  end
  else
  begin
    Hide;
  end;
end;

procedure TNyxLCLModalHost.WindowClose(Sender: TObject; var AAction: TCloseAction);
begin
  AAction := caNone;
  Dismiss;
end;

procedure TNyxLCLModalHost.WindowKey(Sender: TObject; var AKey: Word; AShift: TShiftState);
begin

  if AKey = VK_ESCAPE then
  begin
    AKey := 0;
    Dismiss;
  end;
end;

function NewNyxLCLModalHost(AOwner: TWinControl): INyxLCLModalHost;
begin
  Result := TNyxLCLModalHost.Create(AOwner);
end;

end.
