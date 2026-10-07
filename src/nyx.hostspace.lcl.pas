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
unit nyx.hostspace.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Controls, nyx.hostspace;

{ Borrow a client host, using logical client pixels and LCL's additional resize
  handlers; its existing OnResize is not replaced. No separate visual occlusion
  is reported by LCL, so both fitting policies use the same client rectangle.
  Host destruction disconnects automatically. No control is owned/created. }
function NewNyxLCLHostSpace(AHost: TWinControl;
  const AOptions: TNyxHostSizingOptions): INyxHostSpace;
{ A nil host refuses. The returned observation owns values only. }
function ReadNyxLCLHostSpace(AHost: TWinControl): TNyxHostSpaceSnapshot;

implementation

uses Classes, SysUtils;

type
  TNyxLCLHostSpace = class;
  { The interface owner owns this notification link; it never owns the host.
    FreeNotification supplies safe cancellation if a consumer frees its client
    first. The link's borrowed back pointer is cleared before link retirement. }
  TNyxHostSpaceLink = class(TComponent)
  public
    Observer: TNyxLCLHostSpace;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  end;
  TNyxLCLHostSpace = class(TNyxHostSpaceObserver)
  private
    FHost: TWinControl;
    FLink: TNyxHostSpaceLink;
    procedure Resized(ASender: TObject);
    procedure HostRetired;
  protected
    function Capture: TNyxHostSpaceSnapshot; override;
  public
    constructor Create(AHost: TWinControl; const AOptions: TNyxHostSizingOptions);
    destructor Destroy; override;
    procedure Disconnect; override;
  end;

function ReadNyxLCLHostSpace(AHost: TWinControl): TNyxHostSpaceSnapshot;
begin

  if AHost = nil then
  begin
    raise EArgumentException.Create('A client host is required');
  end;
  Result := NyxHostSpace(AHost.ClientWidth, AHost.ClientHeight, AHost.ClientHeight, 1);
end;

procedure TNyxHostSpaceLink.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);

  if (Observer <> nil) and (AOperation = opRemove) and
    (AComponent = Observer.FHost) then
  begin
    Observer.HostRetired;
  end;
end;

constructor TNyxLCLHostSpace.Create(AHost: TWinControl;
  const AOptions: TNyxHostSizingOptions);
begin
  inherited Create;
  Initialize(AOptions, ReadNyxLCLHostSpace(AHost));
  FHost := AHost;
  FLink := TNyxHostSpaceLink.Create(nil);
  FLink.Observer := Self;
  FHost.FreeNotification(FLink);
  FHost.AddHandlerOnResize(Resized);
end;

destructor TNyxLCLHostSpace.Destroy;
begin
  Disconnect;

  if FLink <> nil then
  begin
    FLink.Observer := nil;
    FLink.Free;
  end;
  inherited Destroy;
end;

function TNyxLCLHostSpace.Capture: TNyxHostSpaceSnapshot;
begin
  Result := ReadNyxLCLHostSpace(FHost);
end;

procedure TNyxLCLHostSpace.Resized(ASender: TObject);
begin
  Refresh;
end;

procedure TNyxLCLHostSpace.HostRetired;
begin
  { Notification occurs during control destruction. Its handler list will also
    retire; do not mutate that list through a dying control. }
  FHost := nil;
  inherited Disconnect;
end;

procedure TNyxLCLHostSpace.Disconnect;
begin

  if FHost <> nil then
  begin
    FHost.RemoveHandlerOnResize(Resized);
    FHost.RemoveFreeNotification(FLink);
    FHost := nil;
  end;
  inherited Disconnect;
end;

function NewNyxLCLHostSpace(AHost: TWinControl;
  const AOptions: TNyxHostSizingOptions): INyxHostSpace;
begin
  Result := TNyxLCLHostSpace.Create(AHost, AOptions);
end;

end.
