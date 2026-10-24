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

{ nyx — native query forms use the ordinary managed Nyx popover host. }
unit nyx.collections.query.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Controls, nyx.theme, nyx.collections.query.controls, nyx.popover.lcl;

type
  { Optional target observation for real input/accessibility integration. The
    returned managed host owns its renderer; no borrowed widget is serialized
    into the portable workbench or retained after target retirement. }
  INyxLCLQueryFormPresenter = interface(INyxQueryFormPresenter)
    ['{66C27837-7F0D-4B31-A506-5DA22878B76A}']
    function GetPresentation: INyxLCLPopover;
    property Presentation: INyxLCLPopover read GetPresentation;
  end;

{ The collection mount owns/revokes this weak factory before freeing the grid.
  Theme and anchor are borrowed for that exact mount, never by a queued command. }
function NewNyxLCLQueryFormFactory(AAnchor: TWinControl;
  ATheme: TNyxTheme): INyxQueryFormPresenterFactory;

implementation

uses
  SysUtils, nyx.text, nyx.model, nyx.root.types, nyx.events, nyx.popover.types,
  nyx.popover, nyx.collections, nyx.collections.mount,
  nyx.collections.arrangement.workbench, nyx.collections.query.editor;

type
  { Retained by forms as a flags-only weak lifetime lease. Optional arranged
    input owns only independent readonly facts, never its mount. Configure it
    before the first form; existing legacy forms cannot be retrofitted safely.
    This factory retains no form or renderer and creates no ownership cycle. }
  TLCLQueryFactory = class(TInterfacedObject, INyxQueryFormPresenterFactory,
    INyxCollectionQueryInputFactory)
  private
    FInput: INyxCollectionInputLifetime;
    FCreateStarted: Boolean;
  public
    Active: Boolean;
    Anchor: TWinControl;
    Theme: TNyxTheme;
    constructor Create(AAnchor: TWinControl; ATheme: TNyxTheme);
    destructor Destroy; override;
    function CreateForm(AColumn: Integer; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxQueryFormPresenter;
    procedure ConfigureInput(const AInput: INyxCollectionInputLifetime);
    procedure Revoke;
  end;

  { Captures the REAL realized form, whose existing live binding owner mirrors
    accepted physical text/select/checkbox changes. Authored Content is a clone
    and must never be mistaken for the user's current draft. Cached Events may
    decorate generation capture, but the exact native Presentation stays intact;
    the host never retains this form or its forwarding event surface. }
  TLCLQueryForm = class(TInterfacedObject, INyxQueryFormPresenter, INyxLCLQueryFormPresenter)
  private
    FTarget: TLCLQueryFactory;
    FLease: INyxQueryFormPresenterFactory;
    FHost: INyxLCLPopover;
    FEvents: INyxEvents;
    FConnected: Boolean;
  public
    constructor Create(ATarget: TLCLQueryFactory; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef; const AInput: INyxCollectionInputLifetime);
    destructor Destroy; override;
    function GetEvents: INyxEvents;
    function GetConnected: Boolean;
    function GetOpen: Boolean;
    function GetPresentation: INyxLCLPopover;
    function Capture(const AActionID: TNyxText; out AChange: TNyxQueryEditorChange): Boolean;
    function CaptureDraft: TNyxQueryEditorDraft;
    procedure SetStatus(const AText: TNyxText);
    procedure Show(const AOptions: TNyxPopoverOptions);
    procedure Close;
    procedure Disconnect;
  end;

constructor TLCLQueryFactory.Create(AAnchor: TWinControl; ATheme: TNyxTheme);
begin
  inherited Create;

  if AAnchor = nil then
  begin
    raise ENyxCollection.Create('A native query form requires its exact table anchor');
  end;
  Anchor := AAnchor;
  Theme := ATheme;
  Active := True;
end;

destructor TLCLQueryFactory.Destroy;
begin
  Revoke;
  inherited Destroy;
end;

procedure TLCLQueryFactory.Revoke;
begin
  Active := False;
  Anchor := nil;
  Theme := nil;
  FInput := nil;
end;

procedure TLCLQueryFactory.ConfigureInput(
  const AInput: INyxCollectionInputLifetime);
var
  LInput: INyxCollectionInputLifetime;
begin
  LInput := AInput;

  if not Active or (LInput = nil) then
  begin
    raise ENyxCollection.Create('A native query factory requires its live exact input lifetime');
  end;

  if FInput <> nil then
  begin

    if FInput <> LInput then
    begin
      raise ENyxCollection.Create('A native query factory cannot change its input owner');
    end;
    Exit;
  end;

  if FCreateStarted then
  begin
    raise ENyxCollection.Create('Configure native query input before creating any form');
  end;
  FInput := LInput;
end;

function TLCLQueryFactory.CreateForm(AColumn: Integer; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): INyxQueryFormPresenter;
var
  LKeep: INyxQueryFormPresenterFactory;
  LInput: INyxCollectionInputLifetime;
begin
  LKeep := Self;
  LInput := FInput;

  if not Active or (Anchor = nil) or (AColumn < -1) then
  begin
    raise ENyxCollection.Create('The native table query anchor has retired');
  end;
  { Snapshot input before the host constructor can call target/theme code.
    A reentrant ConfigureInput must never bless an already-entered legacy form. }
  FCreateStarted := True;
  Result := TLCLQueryForm.Create(Self, ADocument, ARoot, LInput);
end;

constructor TLCLQueryForm.Create(ATarget: TLCLQueryFactory; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AInput: INyxCollectionInputLifetime);
var
  LInput: INyxCollectionInputLifetime;
  LHost: INyxLCLPopover;
  LEvents: INyxEvents;
  LAdmission: INyxPopoverOpenAdmission;
  LAnchor: TWinControl;
begin
  inherited Create;
  LInput := AInput;
  FTarget := ATarget;
  FLease := ATarget;
  LAnchor := ATarget.Anchor;
  { The existing host clones the document during construction. Its owned state,
    controls, focus recovery and event generation share normal Nyx lifetimes. }
  LHost := NewNyxLCLPopover(LAnchor, ADocument, ARoot, ATarget.Theme);
  LEvents := LHost.Events;

  if LInput <> nil then
  begin

    if not Supports(LHost, INyxPopoverOpenAdmission, LAdmission) then
    begin
      raise ENyxCollection.Create('The native query presentation has no open-lifetime admission');
    end;
    LEvents := NewNyxArrangementQueryEvents(LEvents, LInput, LAdmission);
  end;

  if not ATarget.Active or (ATarget.Anchor <> LAnchor) or
    (ATarget.FInput <> LInput) then
  begin
    raise ENyxCollection.Create('The native query factory retired during form construction');
  end;
  { These assignments publish only the exact original local construction.
    Observation is available before Show; generation capture remains closed. }
  FHost := LHost;
  FEvents := LEvents;
  FConnected := True;
end;

destructor TLCLQueryForm.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TLCLQueryForm.GetConnected: Boolean;
begin
  Result := FConnected and (FTarget <> nil) and FTarget.Active and
    (FHost <> nil) and (FEvents <> nil);
end;

function TLCLQueryForm.GetOpen: Boolean;
begin
  Result := GetConnected and FHost.IsOpen;
end;

function TLCLQueryForm.GetPresentation: INyxLCLPopover;
begin
  Result := FHost;
end;

function TLCLQueryForm.GetEvents: INyxEvents;
begin
  Result := nil;

  if GetConnected then
  begin
    Result := FEvents;
  end;
end;

function TLCLQueryForm.Capture(const AActionID: TNyxText;
  out AChange: TNyxQueryEditorChange): Boolean;
var
  LRoot: TNyxNode;
begin
  AChange := Default(TNyxQueryEditorChange);
  Result := False;

  if not GetConnected then
  begin
    Exit;
  end;
  LRoot := FHost.Renderer.Root;

  if LRoot <> nil then
  begin
    Result := CaptureNyxQueryEditor(LRoot.Find(AActionID), LRoot, AChange);
  end;
end;

function TLCLQueryForm.CaptureDraft: TNyxQueryEditorDraft;
begin
  Result := Default(TNyxQueryEditorDraft);

  if GetConnected then
  begin
    Result.Capture(NyxQueryWorkbenchEditorID, FHost.Renderer.Root);
  end;
end;

procedure TLCLQueryForm.SetStatus(const AText: TNyxText);
var
  LKeep: INyxQueryFormPresenter;
  LHost: INyxLCLPopover;
begin
  LKeep := Self;

  if not GetConnected then
  begin
    Exit;
  end;
  LHost := FHost;
  LHost.State.SetValue(NyxQueryFormStatusState, AText);
end;

procedure TLCLQueryForm.Show(const AOptions: TNyxPopoverOptions);
var
  LKeep: INyxQueryFormPresenter;
  LHost: INyxLCLPopover;
  LRoot: TNyxNode;
  LFocusID: TNyxText;
begin
  LKeep := Self;

  if not GetConnected then
  begin
    raise ENyxCollection.Create('The native table query form has retired');
  end;
  LHost := FHost;

  if LHost.IsOpen then
  begin
    { Showing the same draft preserves its live controls, but an explicit focus
      request still matters after a table click or former-form retirement. }
    AOptions.Validate;

    if AOptions.InitialFocus.Name <> '' then
    begin
      LRoot := LHost.Renderer.Root;

      if LRoot = nil then
      begin
        raise ENyxCollection.Create('The native query form has no live focus root');
      end;
      LFocusID := LRoot.Part(AOptions.InitialFocus).ID;
      { Last outward call: ordinary focus callbacks can retire this form. The
        managed host lease survives, and no borrowed control is used afterward. }
      LHost.Renderer.FocusWithoutScroll(LFocusID, niRuntime);
    end;
    Exit;
  end;
  LHost.Open(AOptions);
end;

procedure TLCLQueryForm.Close;
var
  LKeep: INyxQueryFormPresenter;
  LHost: INyxLCLPopover;
begin
  LKeep := Self;
  LHost := FHost;

  if LHost <> nil then
  begin
    LHost.Close;
  end;
end;

procedure TLCLQueryForm.Disconnect;
var
  LHost: INyxLCLPopover;
  LEvents: INyxEvents;
  LFactory: INyxQueryFormPresenterFactory;
begin
  FConnected := False;
  LHost := FHost;
  LEvents := FEvents;
  LFactory := FLease;
  FHost := nil;
  FEvents := nil;
  FTarget := nil;
  FLease := nil;
  { Reentrant Close observes a fully disconnected receiver. Local leases keep
    the exact original forwarding/host/factory owners until cleanup unwinds. }

  if LHost <> nil then
  begin
    LHost.Close;
  end;
end;

function NewNyxLCLQueryFormFactory(AAnchor: TWinControl;
  ATheme: TNyxTheme): INyxQueryFormPresenterFactory;
begin
  Result := TLCLQueryFactory.Create(AAnchor, ATheme);
end;

end.
