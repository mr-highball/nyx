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

unit nyx.canvas.routes;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.canvas.scene,
  nyx.flow.routes, nyx.canvas.route.types;

type
  { Source-compatible runtime name for the shared persistence-neutral intent.
    The canonical value owns inert data only, never a document or view token. }
  TNyxCanvasRouteChange = nyx.canvas.route.types.TNyxCanvasRouteChange;

  { The same detached vector is shared by saved plans and runtime export. }
  TNyxCanvasRouteChanges = nyx.canvas.route.types.TNyxCanvasRouteChanges;

  { Optional UI-owned runtime route authoring. RouteContext returns an owned
    document plus its borrowed root; free that document after constructing a
    public Nyx form. The context contains inert declarations/defaults, never live
    runtime values or executable creator authority. The caller may capture
    detached per-item overrides for explicit export; authored source/history are
    unchanged. Apply validates one exact scene/shared graph-route revision and
    publishes all affected fragments together without remounting controls.
    Missing authored targets, stale content, previews and unsupported creator
    scenes refuse. Current observation publication has a 2048-slot budget and
    preserves surviving external order. All methods/final release belong to UI. }
  INyxCanvasRouteEditing = interface(IInterface)
    ['{2BB8D3EA-0635-49DC-A2F9-418CE44B12F5}']
    function RouteContext(const AItem: TNyxCanvasItemRef;
      out AOwnedRoot: TNyxNode): TNyxDocument;
    function CaptureRoutes: TNyxCanvasRouteChanges;
    function GetRouteRevision: Integer;
    procedure ApplyRouteChanges(const AChanges: array of TNyxCanvasRouteChange;
      AExpectedScene, AExpectedRoutes: Integer);
    property RouteRevision: Integer read GetRouteRevision;
  end;

  { Adapter-owned immutable-after-publication overlay. Clone/Define work only
    on a detached candidate; the owning view swaps the complete class pointer.
    Entries retain independent opaque content tokens and inert route data, never
    scene/control/application receivers. Removed/recreated same-name items cannot
    inherit an old token's routes. This helper supplies shared target semantics,
    not a second graph, event router or document history engine. }
  TNyxCanvasRouteOverrides = class
  private
    FEntries: array of TObject;
  public
    destructor Destroy; override;
    { Supplying a scene copies only entries with its current content lifetime.
      This reclaims retired intent before the next publication's active budget.
      Nil preserves all inert entries for an independent exact snapshot. }
    function Clone(const AScene: INyxCanvasScene = nil): TNyxCanvasRouteOverrides;
    function HasItem(const AItem: TNyxCanvasItemRef): Boolean;
    procedure Define(const AScene: INyxCanvasScene; const AChange: TNyxCanvasRouteChange);
    procedure Configure(const AItem: TNyxCanvasItemRef; const AIdentity: IInterface;
      ADocument: TNyxDocument; ARoot: TNyxNode);
    function Capture(const AScene: INyxCanvasScene): TNyxCanvasRouteChanges;
  end;

{ Whole typed route replacement for one exact authored control in one scene item.
  Reusable instances remain independent; empty routes explicitly mask defaults. }
function NyxCanvasRoutes(const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef;
  const ARoutes: INyxFlowRoutes): TNyxCanvasRouteChange;
{ Strict optional lifetime admission. A creator scene must supply its own stable
  non-receiving token; node pointers and names alone cannot prove replacement. }
function NyxCanvasContentIdentity(const AScene: INyxCanvasScene;
  const AItem: TNyxCanvasItemRef): IInterface;

implementation

uses nyx.flow.route.model;

type
  { Private one-entry lease. Identity owns only an opaque lifetime marker;
    Change has inert immutable data. The enclosing overlay owns this object. }
  TRouteEntry = class
  public
    Identity: IInterface;
    Change: TNyxCanvasRouteChange;
  end;

function NyxCanvasRoutes(const AItem: TNyxCanvasItemRef; const AControl: TNyxControlRef;
  const ARoutes: INyxFlowRoutes): TNyxCanvasRouteChange;
begin
  Result := nyx.canvas.route.types.NyxCanvasRoutes(AItem, AControl, ARoutes);
end;

function NyxCanvasContentIdentity(const AScene: INyxCanvasScene;
  const AItem: TNyxCanvasItemRef): IInterface;
var
  LIdentity: INyxCanvasContentIdentity;
  LToken: IInterface;
begin
  Result := nil;

  if (AScene = nil) or not Supports(AScene, INyxCanvasContentIdentity, LIdentity) or
    not LIdentity.TryContentIdentity(AItem, LToken) or (LToken = nil) then
  begin
    raise ENyxCanvasScene.Create('Canvas route editing requires an exact active content lifetime');
  end;
  Result := LToken;
end;

destructor TNyxCanvasRouteOverrides.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FEntries) do
  begin
    FEntries[LIndex].Free;
  end;
  inherited Destroy;
end;

function TNyxCanvasRouteOverrides.Clone(const AScene: INyxCanvasScene): TNyxCanvasRouteOverrides;
var
  LIndex: Integer;
  LCount: Integer;
  LEntry: TRouteEntry;
  LSource: TRouteEntry;
  LIdentity: INyxCanvasContentIdentity;
  LToken: IInterface;
begin
  LIdentity := nil;

  if (AScene <> nil) and not Supports(AScene, INyxCanvasContentIdentity, LIdentity) then
  begin
    raise ENyxCanvasScene.Create('Route retirement requires exact content lifetime identity');
  end;
  Result := TNyxCanvasRouteOverrides.Create;
  try
    SetLength(Result.FEntries, Length(FEntries));
    LCount := 0;
    for LIndex := 0 to High(FEntries) do
    begin
      LSource := TRouteEntry(FEntries[LIndex]);

      if LIdentity <> nil then
      begin
        LToken := nil;

        if not LIdentity.TryContentIdentity(LSource.Change.Item, LToken) or
          (LToken <> LSource.Identity) then
        begin
          Continue;
        end;
      end;
      LEntry := TRouteEntry.Create;
      Result.FEntries[LCount] := LEntry;
      Inc(LCount);
      LEntry.Identity := LSource.Identity;
      LEntry.Change := TNyxCanvasRouteChange.FromData(LSource.Change.ToData);
    end;
    SetLength(Result.FEntries, LCount);
  except
    Result.Free;
    raise;
  end;
end;

function TNyxCanvasRouteOverrides.HasItem(const AItem: TNyxCanvasItemRef): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to High(FEntries) do
  begin

    if TRouteEntry(FEntries[LIndex]).Change.Item.Name = AItem.Name then
    begin
      Exit(True);
    end;
  end;
end;

procedure TNyxCanvasRouteOverrides.Define(const AScene: INyxCanvasScene;
  const AChange: TNyxCanvasRouteChange);
var
  LChange: TNyxCanvasRouteChange;
  LIdentity: IInterface;
  LEntry: TRouteEntry;
  LIndex: Integer;
begin
  LChange := TNyxCanvasRouteChange.FromData(AChange.ToData);
  LIdentity := NyxCanvasContentIdentity(AScene, LChange.Item);
  for LIndex := 0 to High(FEntries) do
  begin
    LEntry := TRouteEntry(FEntries[LIndex]);

    if (LEntry.Change.Item.Name = LChange.Item.Name) and
      (LEntry.Change.Control.ID = LChange.Control.ID) then
    begin
      LEntry.Identity := LIdentity;
      LEntry.Change := LChange;
      Exit;
    end;
  end;

  if Length(FEntries) = 2048 then
  begin
    raise ENyxCanvasScene.Create('A workspace admits at most 2048 explicit route overrides');
  end;
  LEntry := TRouteEntry.Create;
  try
    LEntry.Identity := LIdentity;
    LEntry.Change := LChange;
    SetLength(FEntries, Length(FEntries) + 1);
    FEntries[High(FEntries)] := LEntry;
  except
    LEntry.Free;
    raise;
  end;
end;

procedure TNyxCanvasRouteOverrides.Configure(const AItem: TNyxCanvasItemRef;
  const AIdentity: IInterface; ADocument: TNyxDocument; ARoot: TNyxNode);
var
  LEntry: TRouteEntry;
  LControl: TNyxNode;
  LIndex: Integer;
begin

  if (ADocument = nil) or (ARoot = nil) or not ADocument.Contains(ARoot) or
    (AIdentity = nil) then
  begin
    raise ENyxCanvasScene.Create('Runtime route context requires an owned authored root and lifetime');
  end;
  for LIndex := 0 to High(FEntries) do
  begin
    LEntry := TRouteEntry(FEntries[LIndex]);

    if (LEntry.Change.Item.Name <> AItem.Name) or (LEntry.Identity <> AIdentity) then
    begin
      Continue;
    end;
    LControl := ARoot.Find(LEntry.Change.Control.ID);

    if LControl = nil then
    begin
      raise ENyxCanvasScene.Create('A runtime route target is absent from this authored item');
    end;
    SetNyxFlowRoutes(LControl, LEntry.Change.Routes);
  end;
end;

function TNyxCanvasRouteOverrides.Capture(const AScene: INyxCanvasScene): TNyxCanvasRouteChanges;
var
  LIdentity: INyxCanvasContentIdentity;
  LToken: IInterface;
  LEntry: TRouteEntry;
  LIndex: Integer;
  LCount: Integer;
begin

  if (AScene = nil) or not Supports(AScene, INyxCanvasContentIdentity, LIdentity) then
  begin
    raise ENyxCanvasScene.Create('Runtime route capture requires content lifetime identity');
  end;
  Result := nil;
  SetLength(Result, Length(FEntries));
  LCount := 0;
  for LIndex := 0 to High(FEntries) do
  begin
    LEntry := TRouteEntry(FEntries[LIndex]);
    LToken := nil;

    if LIdentity.TryContentIdentity(LEntry.Change.Item, LToken) and (LToken = LEntry.Identity) then
    begin
      Result[LCount] := TNyxCanvasRouteChange.FromData(LEntry.Change.ToData);
      Inc(LCount);
    end;
  end;
  SetLength(Result, LCount);
end;

end.
