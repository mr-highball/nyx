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
unit nyx.viewport.binding;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.scheduler, nyx.composition, nyx.viewport.context;

type
  { Borrowed adapter receiver returns actual local logical allocation, or false
    for an unmounted view. It never substitutes the enclosing viewport dimensions. }
  TNyxViewportAllocation = function(out AWidth, AHeight: Double): Boolean of object;
  { Readiness is a local flag check; it must not pump UI work or publish a view. }
  TNyxViewportReady = function: Boolean of object;
  { Ordinary adapter synchronization, invoked only by deferred UI work. }
  TNyxViewportRefresh = procedure of object;

  { Shared adapter controller. Source is optional runtime host configuration;
    nil retains the allocated-host default. Frame keeps physical allocation and
    presentation viewport distinct. Context exports the resolved presentation
    viewport, so nested consumers inherit the same meaning through a stable port.
    Observe publishes current geometry; Wake coalesces deferred updates after
    dispatch/publication becomes ready. Suspend revokes this mount's port/work
    but retains Source for an explicit remount. Retire revokes all borrowed
    adapter receivers. Neither operation owns a document or target control. }
  INyxViewportBinding = interface(IInterface)
    ['{0E3407CA-57E8-49EB-BDF8-A3A3F78F3A1E}']
    function GetSource: INyxViewportContext;
    procedure SetSource(const ASource: INyxViewportContext);
    function GetContext: INyxViewportContext;
    function Frame(AWidth, AHeight: Double): TNyxViewFrame;
    procedure CheckFrame(const AFrame: TNyxViewFrame);
    procedure Observe;
    procedure Wake;
    procedure Suspend;
    procedure Retire;
    function GetFailure: TNyxText;
    property Source: INyxViewportContext read GetSource write SetSource;
    property Context: INyxViewportContext read GetContext;
    property Failure: TNyxText read GetFailure;
  end;

{ All callbacks remain borrowed until Retire. The caller retires this controller
  before destroying the adapter, including when refresh itself disposes that
  adapter reentrantly. The entered controller keeps its own independent lease. }
function NewNyxViewportBinding(const AScheduler: INyxScheduler; APlatform: TNyxPlatform;
  ARead: TNyxViewportAllocation; AReady: TNyxViewportReady;
  ARefresh: TNyxViewportRefresh): INyxViewportBinding;

implementation

uses SysUtils, nyx.content.mount;

type
  { Owns the source subscription, output writer and weak queued work port.
    Local managed leases survive a refresh that destroys its borrowed renderer;
    retirement clears receivers before canceling external observations/work. }
  TViewportBinding = class(TInterfacedObject, INyxViewportBinding)
  private
    FScheduler: INyxScheduler;
    FPlatform: TNyxPlatform;
    FRead: TNyxViewportAllocation;
    FReady: TNyxViewportReady;
    FRefresh: TNyxViewportRefresh;
    FSource: INyxViewportContext;
    FSubscription: INyxViewportSubscription;
    FOutput: INyxViewportContextOwner;
    FWork: INyxContentWork;
    FExecution: INyxExecution;
    FActive: Boolean;
    FDirty: Boolean;
    FQueued: Boolean;
    FFailure: TNyxText;
    procedure Changed(const AViewport: TNyxViewportContextSnapshot);
    procedure Flush;
    procedure RequireActive;
    procedure Subscribe;
  public
    constructor Create(const AScheduler: INyxScheduler; APlatform: TNyxPlatform;
      ARead: TNyxViewportAllocation; AReady: TNyxViewportReady;
      ARefresh: TNyxViewportRefresh);
    destructor Destroy; override;
    function GetSource: INyxViewportContext;
    procedure SetSource(const ASource: INyxViewportContext);
    function GetContext: INyxViewportContext;
    function Frame(AWidth, AHeight: Double): TNyxViewFrame;
    procedure CheckFrame(const AFrame: TNyxViewFrame);
    procedure Observe;
    procedure Wake;
    procedure Suspend;
    procedure Retire;
    function GetFailure: TNyxText;
  end;

constructor TViewportBinding.Create(const AScheduler: INyxScheduler;
  APlatform: TNyxPlatform; ARead: TNyxViewportAllocation;
  AReady: TNyxViewportReady; ARefresh: TNyxViewportRefresh);
begin
  inherited Create;

  if (AScheduler = nil) or not Assigned(ARead) or not Assigned(AReady) or
    not Assigned(ARefresh) or (APlatform = npfAny) then
  begin
    raise ENyxViewportContext.Create('Viewport binding requires its scheduler, target and live adapter');
  end;
  AScheduler.RequireUI;
  TNyxViewFrame.At(0, 0, APlatform);
  FScheduler := AScheduler;
  FPlatform := APlatform;
  FRead := ARead;
  FReady := AReady;
  FRefresh := ARefresh;
  FActive := True;
end;

destructor TViewportBinding.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TViewportBinding.RequireActive;
begin

  if not FActive then
  begin
    raise ENyxViewportContext.Create('The viewport binding is retired');
  end;
  FScheduler.RequireUI;
end;

function TViewportBinding.GetSource: INyxViewportContext;
begin
  RequireActive;
  Result := FSource;
end;

procedure TViewportBinding.SetSource(const ASource: INyxViewportContext);
var
  LSnapshot: TNyxViewportContextSnapshot;
begin
  RequireActive;

  if ASource = FSource then
  begin
    Exit;
  end;

  if ASource <> nil then
  begin

    if (FOutput <> nil) and (ASource = FOutput.Context) then
    begin
      raise ENyxViewportContext.Create('A view cannot consume its own viewport output');
    end;
    LSnapshot := ASource.Snapshot;
    RequireActive;

    if not ASource.Active or not LSnapshot.Defined then
    begin
      raise ENyxViewportContext.Create('An enclosing viewport must be live and observed');
    end;
    RequireActive;
  end;
  FSubscription := nil;
  FSource := ASource;
  FDirty := True;
  FFailure := '';
  Observe;
  Wake;
end;

function TViewportBinding.Frame(AWidth, AHeight: Double): TNyxViewFrame;
var
  LSource: INyxViewportContext;
  LSnapshot: TNyxViewportContextSnapshot;
begin
  RequireActive;
  Result := TNyxViewFrame.At(AWidth, AHeight, FPlatform);
  LSource := FSource;

  if LSource <> nil then
  begin
    LSnapshot := LSource.Snapshot;
    RequireActive;

    if not LSource.Active or not LSnapshot.Defined then
    begin
      raise ENyxViewportContext.Create('The enclosing presentation viewport is unavailable');
    end;
    RequireActive;
    Result := Result.WithinViewport(LSnapshot.Width, LSnapshot.Height);
  end;
end;

procedure TViewportBinding.CheckFrame(const AFrame: TNyxViewFrame);
var
  LCurrent: TNyxViewFrame;
begin
  LCurrent := Frame(AFrame.Width, AFrame.Height);

  if AFrame.Selection.Reference.Defined then
  begin
    LCurrent := LCurrent.Selecting(AFrame.Selection.Reference);
  end;

  if not LCurrent.Same(AFrame) then
  begin
    raise ENyxViewportContext.Create('The presentation viewport changed before publication');
  end;
end;

procedure TViewportBinding.Subscribe;
begin

  if (FSource <> nil) and (FSubscription = nil) then
  begin
    FSubscription := FSource.Subscribe({$ifdef PAS2JS}@{$endif}Changed);
  end;
end;

procedure TViewportBinding.Observe;
var
  LLease: INyxViewportBinding;
  LWidth: Double;
  LHeight: Double;
  LFrame: TNyxViewFrame;
begin
  RequireActive;

  if (FSource = nil) and (FOutput = nil) then
  begin
    Exit;
  end;
  LLease := Self;

  if not FRead(LWidth, LHeight) then
  begin
    Exit;
  end;
  RequireActive;
  LFrame := Frame(LWidth, LHeight);
  Subscribe;
  RequireActive;

  if FOutput <> nil then
  begin
    FOutput.Publish(LFrame.ViewportWidth, LFrame.ViewportHeight);
  end;
end;

function TViewportBinding.GetContext: INyxViewportContext;
var
  LLease: INyxViewportBinding;
begin
  RequireActive;
  LLease := Self;

  if FOutput = nil then
  begin
    FOutput := NewNyxViewportContext(FScheduler);
  end;
  Observe;
  RequireActive;
  Result := FOutput.Context;
end;

procedure TViewportBinding.Changed(const AViewport: TNyxViewportContextSnapshot);
begin

  if not FActive then
  begin
    Exit;
  end;
  FDirty := True;
  Wake;
end;

procedure TViewportBinding.Wake;
var
  LLease: INyxViewportBinding;
begin

  if not FActive or not FDirty or FQueued then
  begin
    Exit;
  end;
  FScheduler.RequireUI;
  LLease := Self;

  if not FReady then
  begin
    Exit;
  end;
  RequireActive;

  if FWork = nil then
  begin
    FWork := NewNyxContentWork({$ifdef PAS2JS}@{$endif}Flush);
  end;
  FQueued := True;
  try
    FExecution := FScheduler.Submit(FWork, neUIQueue);
  except
    FQueued := False;
    raise;
  end;
end;

procedure TViewportBinding.Flush;
var
  LLease: INyxViewportBinding;
begin
  LLease := Self;
  FQueued := False;
  FExecution := nil;

  if not FActive or not FDirty or not FReady then
  begin
    Exit;
  end;
  FDirty := False;
  try
    FRefresh;

    if FActive then
    begin
      FFailure := '';
      Wake;
    end;
  except
    on LException: Exception do
    begin

      if FActive then
      begin
        FFailure := TNyxText(LException.Message);
      end;
      raise;
    end;
  end;
end;

procedure TViewportBinding.Suspend;
begin
  FScheduler.RequireUI;
  FSubscription := nil;

  if FOutput <> nil then
  begin
    FOutput.Retire;
    FOutput := nil;
  end;

  if FWork <> nil then
  begin
    FWork.Retire;
    FWork := nil;
  end;

  if FExecution <> nil then
  begin
    FExecution.Cancel;
    FExecution := nil;
  end;
  FQueued := False;
  FDirty := False;
end;

procedure TViewportBinding.Retire;
begin

  if not FActive then
  begin
    Exit;
  end;
  FScheduler.RequireUI;
  FActive := False;
  FRead := nil;
  FReady := nil;
  FRefresh := nil;
  Suspend;
  FSource := nil;
end;

function TViewportBinding.GetFailure: TNyxText;
begin
  RequireActive;
  Result := FFailure;
end;

function NewNyxViewportBinding(const AScheduler: INyxScheduler; APlatform: TNyxPlatform;
  ARead: TNyxViewportAllocation; AReady: TNyxViewportReady;
  ARefresh: TNyxViewportRefresh): INyxViewportBinding;
begin
  Result := TViewportBinding.Create(AScheduler, APlatform, ARead, AReady, ARefresh);
end;

end.
