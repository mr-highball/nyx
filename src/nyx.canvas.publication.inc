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

type
  { Shared UI-only camera/frame adopter for the two physical adapters. Present
    owns its lifetime and every staged mount; this object borrows that view.
    The former snapshot is retained before Install so adopting the new one cannot
    destroy a creator object between group installs. Scene/state reservations and
    all allocation happen outside Install. No program/application is retained. }
  TCanvasPresentation = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    Owner: TCanvasView;
    Camera: TNyxCanvasCamera;
    Snapshot: INyxCanvasSnapshot;
    FormerSnapshot: INyxCanvasSnapshot;
    Mounts: TCanvasMounts;
    Scope: INyxCancellationScope;
    ExpectedRevision: Integer;
    Validated: Boolean;
    Installed: Boolean;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

procedure TCanvasPresentation.Validate;
begin
  { This is the final validator. Caller-controlled cancellation is read before
    the coordinator rechecks every owner's callback-free readiness fence. }

  if (Scope <> nil) and Scope.Cancelled then
  begin
    raise ENyxCanvasPublicationCancelled.Create('Canvas publication cancelled');
  end;
  Validated := True;
  CheckReady;
end;

procedure TCanvasPresentation.CheckReady;
begin

  if not Validated or Installed or (Owner = nil) or not Owner.FConnected then
  begin
    raise ENyxCanvasPublicationCancelled.Create('Canvas presentation owner retired');
  end;

  if not Owner.FUpdating or (Owner.FPresentationRevision <> ExpectedRevision) or
    (Owner.FSurface.ClientWidth <> Camera.ViewportWidth) or
    (Owner.FSurface.ClientHeight <> Camera.ViewportHeight) then
  begin
    raise ENyxCanvasScene.Create('Canvas presentation changed during coordinated admission');
  end;
end;

procedure TCanvasPresentation.Install;
begin
  { No calls to renderer, scene, DOM/LCL setters or external getters are allowed
    here. Every group model is adopted before notification can run. }
  Owner.FMounts := Mounts;
  Owner.FSnapshot := Snapshot;
  Owner.FCamera := Camera;
  Inc(Owner.FPresentationRevision);
  Installed := True;
end;

procedure TCanvasPresentation.Notify;
begin
  { Present reconciles physical faces after all model notifications, including
    when one receiver failed. Physical callbacks therefore see the whole frame. }
end;

procedure TCanvasPresentation.Retire;
begin
  Owner := nil;
end;

function TCanvasView.ObservePresentation(
  const AObserver: INyxCanvasPresentationObserver): INyxCancellationScope;
begin
  RequireConnected;
  Result := FPresentationObservers.Subscribe(AObserver);
end;

function TCanvasView.ApplyCoordinatedLayout(const APlan: INyxCanvasLayoutPlan;
  const ACamera: TNyxCanvasCamera; AExpectedPresentation: Integer;
  const ACompanion: INyxPreparedPublication;
  const ACancellation: INyxCancellationScope): Boolean;
var
  LView: INyxCanvasWorkspaceView;
  LPlan: INyxCanvasLayoutPlan;
  LCompanion: INyxPreparedPublication;
  LScope: INyxCancellationScope;
  LCamera: TNyxCanvasCamera;
begin
  LView := Self;
  LPlan := APlan;
  LCompanion := ACompanion;
  LScope := ACancellation;
  LCamera := ACamera;

  if (LPlan = nil) or (LCompanion = nil) then
  begin
    raise ENyxCanvasScene.Create('Coordinated layout requires a plan and complete companion');
  end;
  FScheduler.RequireUI;

  if not FConnected then
  begin
    Exit(False);
  end;
  try
    Result := Present(LCamera, True, LPlan, AExpectedPresentation, LScope, LCompanion);
  except
    on E: ENyxCanvasPublicationCancelled do
    begin
      Result := False;
    end;
  end;
end;
