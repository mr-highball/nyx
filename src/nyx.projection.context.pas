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
unit nyx.projection.context;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.publication, nyx.events;

{ Couple one successor event generation with the target's reversible retained
  form preview. Target must implement final readiness, retain its old model/input
  snapshots until Install, and restore them on uninstalled Retire. It owns no
  caller document. Its Install updates only preallocated target bookkeeping;
  Notify may schedule the newly accepted presentation after every group installs.

  The returned participant permits repeated Validate only as a readiness check:
  a renderer validates before its physical preview and the enclosing section
  group validates/fences again before committing all members. Abandonment keeps
  the old event generation and rolls back the target; installed retirement never
  restores an old model. Rollback finishes while dispatch is still fenced.
  The first participant installs the event generation
  without callbacks, before the target adopts that precomputed revision.
  Preview admission refuses pending/running old event work before ANY physical
  change and blocks fresh dispatch until publication/rollback. Refusal leaves
  the old queued work intact; callers may use their ordinary replacement path. }
function NewNyxProjectionContextPublication(const AEvents: INyxEvents;
  const ATarget: INyxPreparedPublication): INyxPreparedPublication;

implementation

uses SysUtils;

type
  { Owns a canonical prepared group, whose event plan and target port have no
    ownership edge back into this wrapper. The final fence is captured once;
    validation never replays source admission or refreshes the original witness.
    Reentrant retirement marks this wrapper unavailable immediately, then waits
    for its entered child phase to return before detaching that child's group. }
  TContextPublication = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  private
    FGroup: INyxPreparedPublication;
    FTarget: INyxPreparedPublication;
    FFence: INyxPublicationReadiness;
    FPreview: INyxEventViewPreview;
    FValidated: Boolean;
    FEntering: Boolean;
    FRetireRequested: Boolean;
    FRetired: Boolean;
    procedure DoRetire;
    procedure FinishPhase(AFailed: Boolean);
  public
    constructor Create(const AEvents: INyxEvents; const ATarget: INyxPreparedPublication);
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

constructor TContextPublication.Create(const AEvents: INyxEvents;
  const ATarget: INyxPreparedPublication);
var
  LEvents: INyxEvents;
  LTarget: INyxPreparedPublication;
  LPreparation: INyxEventViewPreparation;
  LTargetFence: INyxPublicationReadiness;
  LEvent: INyxPreparedEventView;
  LFence: INyxPublicationReadiness;
  LPreview: INyxEventViewPreview;
begin
  inherited Create;
  LEvents := AEvents;
  LTarget := ATarget;

  if (LEvents = nil) or (LTarget = nil) or
    not Supports(LEvents, INyxEventViewPreparation, LPreparation) or
    not Supports(LTarget, INyxPublicationReadiness, LTargetFence) then
  begin
    raise ENyxPublication.Create('Context refresh requires prepared events and a fenced target');
  end;
  LEvent := LPreparation.PrepareView;

  if not Supports(LEvent, INyxEventViewPreview, LPreview) then
  begin
    raise ENyxPublication.Create('Context refresh requires quiescent event preview admission');
  end;
  FPreview := LPreview;
  FTarget := LTarget;
  FGroup := PrepareNyxGroup([LEvent, LTarget]);

  if not Supports(FGroup, INyxPublicationReadiness, LFence) then
  begin
    raise ENyxPublication.Create('Context refresh group requires a final readiness fence');
  end;
  FFence := LFence;
end;

destructor TContextPublication.Destroy;
begin
  try
    DoRetire;
  except
    { Explicit retirement reports rollback failure. Destruction cannot escape
      before FPC releases this wrapper's fields and object storage. }
  end;
  inherited Destroy;
end;

procedure TContextPublication.Validate;
var
  LLease: INyxPreparedPublication;
  LGroup: INyxPreparedPublication;
  LPreview: INyxEventViewPreview;
  LFailed: Boolean;
begin
  LLease := Self;

  if FRetired or FRetireRequested or FEntering then
  begin
    raise ENyxPublication.Create('Context validation requires a live, unentered publication');
  end;
  LGroup := FGroup;
  LPreview := FPreview;
  FEntering := True;
  LFailed := False;
  try
    try

      if not FValidated then
      begin
        LGroup.Validate;

        if FRetireRequested then
        begin
          raise ENyxPublication.Create('Context publication retired during validation');
        end;

        if not LPreview.TryBeginPreview then
        begin
          raise ENyxPublication.Create('Context refresh waits for old event work to finish');
        end;

        if FRetireRequested then
        begin
          raise ENyxPublication.Create('Context publication retired during preview admission');
        end;
        FValidated := True;
      end;
      CheckReady;
    except
      LFailed := True;
      raise;
    end;
  finally
    FinishPhase(LFailed);
  end;
end;

procedure TContextPublication.CheckReady;
begin

  if FRetired or FRetireRequested or not FValidated then
  begin
    raise ENyxPublication.Create('Context publication has no validated live preview');
  end;
  FFence.CheckReady;
end;

procedure TContextPublication.Install;
begin

  if not FRetired and not FRetireRequested and not FEntering and (FGroup <> nil) then
  begin
    FGroup.Install;
  end;
end;

procedure TContextPublication.Notify;
var
  LLease: INyxPreparedPublication;
  LGroup: INyxPreparedPublication;
  LFailed: Boolean;
begin
  LLease := Self;

  if FRetired or FRetireRequested or FEntering or (FGroup = nil) then
  begin
    raise ENyxPublication.Create('A retired context publication cannot notify');
  end;
  LGroup := FGroup;
  FEntering := True;
  LFailed := False;
  try
    try
      LGroup.Notify;
    except
      LFailed := True;
      raise;
    end;
  finally
    FinishPhase(LFailed);
  end;
end;

procedure TContextPublication.FinishPhase(AFailed: Boolean);
begin
  FEntering := False;

  if FRetireRequested then
  begin
    try
      DoRetire;
    except

      if not AFailed then
      begin
        raise;
      end;
    end;
  end;
end;

procedure TContextPublication.DoRetire;
var
  LGroup: INyxPreparedPublication;
  LTarget: INyxPreparedPublication;
  LFailed: Boolean;
begin

  if FRetired then
  begin
    Exit;
  end;
  FRetired := True;
  LGroup := FGroup;
  LTarget := FTarget;
  FGroup := nil;
  FTarget := nil;
  FFence := nil;
  FPreview := nil;

  { Restore the old target BEFORE retiring the event preview. Physical rollback
    can enter focus/parenting callbacks; old-context dispatch remains fenced
    through that entire operation. The group's later repeated target retirement
    is harmless by the public idempotent contract. Always release its event
    reservation even if explicit target rollback fails. }
  LFailed := False;
  try
    try

      if LTarget <> nil then
      begin
        LTarget.Retire;
      end;
    except
      LFailed := True;
      raise;
    end;
  finally

    if LGroup <> nil then
    begin
      try
        LGroup.Retire;
      except

        if not LFailed then
        begin
          raise;
        end;
      end;
    end;
  end;
end;

procedure TContextPublication.Retire;
var
  LLease: INyxPreparedPublication;
begin
  LLease := Self;

  if FEntering then
  begin
    { The child may still be traversing its participant array. Mark the outer
      receiver terminal, but keep that exact group intact until return. }
    FRetireRequested := True;
    Exit;
  end;
  DoRetire;
end;

function NewNyxProjectionContextPublication(const AEvents: INyxEvents;
  const ATarget: INyxPreparedPublication): INyxPreparedPublication;
begin
  Result := TContextPublication.Create(AEvents, ATarget);
end;

end.
