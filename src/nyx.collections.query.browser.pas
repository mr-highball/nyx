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

{ nyx — browser query forms use the ordinary managed Nyx popover host. }
unit nyx.collections.query.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Web, nyx.theme, nyx.collections.query.controls, nyx.popover.browser;

type
  { Optional target observation for real input/accessibility integration. The
    returned managed host owns its renderer; no borrowed widget is serialized
    into the portable workbench or retained after target retirement. }
  INyxBrowserQueryFormPresenter = interface(INyxQueryFormPresenter)
    ['{02B2CC51-D27D-4F52-A31E-390497F60964}']
    function GetPresentation: INyxBrowserPopover;
    property Presentation: INyxBrowserPopover read GetPresentation;
  end;

{ The collection mount owns/revokes this weak factory before freeing the grid.
  Theme and anchor are borrowed for that exact mount, never by a queued command. }
function NewNyxBrowserQueryFormFactory(AAnchor: TJSHTMLElement;
  ATheme: TNyxTheme): INyxQueryFormPresenterFactory;

implementation

uses
  SysUtils, nyx.text, nyx.model, nyx.root.types, nyx.events, nyx.popover.types,
  nyx.popover, nyx.collections, nyx.collections.mount,
  nyx.collections.arrangement.workbench, nyx.collections.query.editor,
  nyx.render.browser;

type
  { Retained by forms as a flags-only weak lifetime lease. Optional arranged
    input owns only independent readonly facts, never its mount. Configure it
    before the first form; existing legacy forms cannot be retrofitted safely.
    This factory retains no form or renderer and creates no ownership cycle. }
  TBrowserQueryFactory = class(TInterfacedObject, INyxQueryFormPresenterFactory,
    INyxCollectionQueryInputFactory)
  private
    FInput: INyxCollectionInputLifetime;
    FCreateStarted: Boolean;
  public
    Active: Boolean;
    Anchor: TJSHTMLElement;
    Theme: TNyxTheme;
    constructor Create(AAnchor: TJSHTMLElement; ATheme: TNyxTheme);
    destructor Destroy; override;
    function CreateForm(AColumn: Integer; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxQueryFormPresenter;
    procedure ConfigureInput(const AInput: INyxCollectionInputLifetime);
    procedure Revoke;
  end;

  { Captures the REAL realized form, whose existing live binding owner mirrors
    accepted physical text/select/checkbox changes. Authored Content is a clone
    and must never be mistaken for the user's current draft. Cached Events may
    decorate generation capture, but the exact browser Presentation stays intact;
    the host never retains this form or its forwarding event surface. }
  TBrowserQueryForm = class(TInterfacedObject, INyxQueryFormPresenter, INyxBrowserQueryFormPresenter)
  private
    FTarget: TBrowserQueryFactory;
    FLease: INyxQueryFormPresenterFactory;
    FHost: INyxBrowserPopover;
    FEvents: INyxEvents;
    FConnected: Boolean;
  public
    constructor Create(ATarget: TBrowserQueryFactory; AAnchor: TJSHTMLElement; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef; const AInput: INyxCollectionInputLifetime);
    destructor Destroy; override;
    function GetEvents: INyxEvents;
    function GetConnected: Boolean;
    function GetOpen: Boolean;
    function GetPresentation: INyxBrowserPopover;
    function Capture(const AActionID: TNyxText; out AChange: TNyxQueryEditorChange): Boolean;
    function CaptureDraft: TNyxQueryEditorDraft;
    procedure SetStatus(const AText: TNyxText);
    procedure Show(const AOptions: TNyxPopoverOptions);
    procedure Close;
    procedure Disconnect;
  end;

constructor TBrowserQueryFactory.Create(AAnchor: TJSHTMLElement; ATheme: TNyxTheme);
begin
  inherited Create;

  if AAnchor = nil then
  begin
    raise ENyxCollection.Create('A browser query form requires its exact table anchor');
  end;
  Anchor := AAnchor;
  Theme := ATheme;
  Active := True;
end;

destructor TBrowserQueryFactory.Destroy;
begin
  Revoke;
  inherited Destroy;
end;

procedure TBrowserQueryFactory.Revoke;
begin
  Active := False;
  Anchor := nil;
  Theme := nil;
  FInput := nil;
end;

procedure TBrowserQueryFactory.ConfigureInput(
  const AInput: INyxCollectionInputLifetime);
var
  LInput: INyxCollectionInputLifetime;
begin
  LInput := AInput;

  if not Active or (LInput = nil) then
  begin
    raise ENyxCollection.Create('A browser query factory requires its live exact input lifetime');
  end;

  if FInput <> nil then
  begin

    if FInput <> LInput then
    begin
      raise ENyxCollection.Create('A browser query factory cannot change its input owner');
    end;
    Exit;
  end;

  if FCreateStarted then
  begin
    raise ENyxCollection.Create('Configure browser query input before creating any form');
  end;
  FInput := LInput;
end;

function TBrowserQueryFactory.CreateForm(AColumn: Integer; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): INyxQueryFormPresenter;
var
  LAnchor: TJSHTMLElement;
  LTableAnchor: TJSHTMLElement;
  LKeep: INyxQueryFormPresenterFactory;
  LInput: INyxCollectionInputLifetime;
begin
  LKeep := Self;
  LInput := FInput;
  LTableAnchor := Anchor;

  if not Active or (LTableAnchor = nil) or (AColumn < -1) then
  begin
    raise ENyxCollection.Create('The browser table query anchor has retired');
  end;
  { Reserve construction before DOM getters can reenter configuration, retaining
    the original input rather than reading a newly configured incarnation. }
  FCreateStarted := True;
  LAnchor := LTableAnchor;

  if AColumn >= 0 then
  begin
    LAnchor := TJSHTMLElement(LTableAnchor.querySelector('[data-nyx-query-column="' +
      IntToStr(AColumn) + '"]'));
  end;

  if (LAnchor = nil) or not Web.document.body.contains(LAnchor) then
  begin
    raise ENyxCollection.Create('The browser query column is no longer mounted');
  end;

  if not Active or (Anchor <> LTableAnchor) or (FInput <> LInput) then
  begin
    raise ENyxCollection.Create('The browser query factory retired during anchor lookup');
  end;
  Result := TBrowserQueryForm.Create(Self, LAnchor, ADocument, ARoot, LInput);
end;

constructor TBrowserQueryForm.Create(ATarget: TBrowserQueryFactory; AAnchor: TJSHTMLElement; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef; const AInput: INyxCollectionInputLifetime);
var
  LInput: INyxCollectionInputLifetime;
  LHost: INyxBrowserPopover;
  LEvents: INyxEvents;
  LAdmission: INyxPopoverOpenAdmission;
  LTableAnchor: TJSHTMLElement;
begin
  inherited Create;
  LInput := AInput;
  FTarget := ATarget;
  FLease := ATarget;
  LTableAnchor := ATarget.Anchor;
  { The existing host clones the document during construction. Its owned state,
    controls, focus recovery and event generation share normal Nyx lifetimes. }
  LHost := NewNyxBrowserPopover(AAnchor, ADocument, ARoot, ATarget.Theme);
  LEvents := LHost.Events;

  if LInput <> nil then
  begin

    if not Supports(LHost, INyxPopoverOpenAdmission, LAdmission) then
    begin
      raise ENyxCollection.Create('The browser query presentation has no open-lifetime admission');
    end;
    LEvents := NewNyxArrangementQueryEvents(LEvents, LInput, LAdmission);
  end;

  if not ATarget.Active or (ATarget.Anchor <> LTableAnchor) or
    (ATarget.FInput <> LInput) then
  begin
    raise ENyxCollection.Create('The browser query factory retired during form construction');
  end;
  { These assignments publish only the exact original local construction.
    Observation is available before Show; generation capture remains closed. }
  FHost := LHost;
  FEvents := LEvents;
  FConnected := True;
end;

destructor TBrowserQueryForm.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TBrowserQueryForm.GetConnected: Boolean;
begin
  Result := FConnected and (FTarget <> nil) and FTarget.Active and
    (FHost <> nil) and (FEvents <> nil);
end;

function TBrowserQueryForm.GetOpen: Boolean;
begin
  Result := GetConnected and FHost.IsOpen;
end;

function TBrowserQueryForm.GetPresentation: INyxBrowserPopover;
begin
  Result := FHost;
end;

function TBrowserQueryForm.GetEvents: INyxEvents;
begin
  Result := nil;

  if GetConnected then
  begin
    Result := FEvents;
  end;
end;

function TBrowserQueryForm.Capture(const AActionID: TNyxText;
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

function TBrowserQueryForm.CaptureDraft: TNyxQueryEditorDraft;
begin
  Result := Default(TNyxQueryEditorDraft);

  if GetConnected then
  begin
    Result.Capture(NyxQueryWorkbenchEditorID, FHost.Renderer.Root);
  end;
end;

procedure TBrowserQueryForm.SetStatus(const AText: TNyxText);
var
  LKeep: INyxQueryFormPresenter;
  LHost: INyxBrowserPopover;
begin
  LKeep := Self;

  if not GetConnected then
  begin
    Exit;
  end;
  LHost := FHost;
  LHost.State.SetValue(NyxQueryFormStatusState, AText);
end;

procedure TBrowserQueryForm.Show(const AOptions: TNyxPopoverOptions);
var
  LKeep: INyxQueryFormPresenter;
  LHost: INyxBrowserPopover;
  LRoot: TNyxNode;
  LFocus: TJSHTMLElement;
  LFocusID: TNyxText;
begin
  LKeep := Self;

  if not GetConnected then
  begin
    raise ENyxCollection.Create('The browser table query form has retired');
  end;
  LHost := FHost;

  if LHost.IsOpen then
  begin
    { Reuse the mounted draft while honoring focus explicitly. This also lets
      adoption recover focus after a former form returns it to its invoker. }
    AOptions.Validate;

    if AOptions.InitialFocus.Name <> '' then
    begin
      LRoot := LHost.Renderer.Root;

      if LRoot = nil then
      begin
        raise ENyxCollection.Create('The browser query form has no live focus root');
      end;
      LFocusID := LRoot.Part(AOptions.InitialFocus).ID;
      LFocus := LHost.Renderer.FocusFor(LFocusID, niRuntime);

      if LFocus = nil then
      begin
        raise ENyxCollection.Create('The browser query field has no focus face');
      end;
      { Focus callbacks may retire the receiver; keep only the host lease and
        make no target/control access after this outward call. }
      NyxFocusWithoutScroll(LFocus);
    end;
    Exit;
  end;
  LHost.Open(AOptions);
end;

procedure TBrowserQueryForm.Close;
var
  LKeep: INyxQueryFormPresenter;
  LHost: INyxBrowserPopover;
begin
  LKeep := Self;
  LHost := FHost;

  if LHost <> nil then
  begin
    LHost.Close;
  end;
end;

procedure TBrowserQueryForm.Disconnect;
var
  LHost: INyxBrowserPopover;
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

function NewNyxBrowserQueryFormFactory(AAnchor: TJSHTMLElement;
  ATheme: TNyxTheme): INyxQueryFormPresenterFactory;
begin
  Result := TBrowserQueryFactory.Create(AAnchor, ATheme);
end;

end.
