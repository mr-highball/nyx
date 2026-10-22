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
  nyx.collections, nyx.collections.query.editor, nyx.render.browser;

type
  { Retained by forms as a flags-only weak lifetime lease. This factory never
    retains a form or renderer, so its small managed lease cannot form a cycle. }
  TBrowserQueryFactory = class(TInterfacedObject, INyxQueryFormPresenterFactory)
  public
    Active: Boolean;
    Anchor: TJSHTMLElement;
    Theme: TNyxTheme;
    constructor Create(AAnchor: TJSHTMLElement; ATheme: TNyxTheme);
    destructor Destroy; override;
    function CreateForm(AColumn: Integer; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxQueryFormPresenter;
    procedure Revoke;
  end;

  { Captures the REAL realized form, whose existing live binding owner mirrors
    accepted physical text/select/checkbox changes. Authored Content is a clone
    and must never be mistaken for the user's current draft. }
  TBrowserQueryForm = class(TInterfacedObject, INyxQueryFormPresenter, INyxBrowserQueryFormPresenter)
  private
    FTarget: TBrowserQueryFactory;
    FLease: INyxQueryFormPresenterFactory;
    FHost: INyxBrowserPopover;
    FConnected: Boolean;
  public
    constructor Create(ATarget: TBrowserQueryFactory; AAnchor: TJSHTMLElement; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef);
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
end;

function TBrowserQueryFactory.CreateForm(AColumn: Integer; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): INyxQueryFormPresenter;
var
  LAnchor: TJSHTMLElement;
begin

  if not Active or (Anchor = nil) or (AColumn < -1) then
  begin
    raise ENyxCollection.Create('The browser table query anchor has retired');
  end;
  LAnchor := Anchor;

  if AColumn >= 0 then
  begin
    LAnchor := TJSHTMLElement(Anchor.querySelector('[data-nyx-query-column="' +
      IntToStr(AColumn) + '"]'));
  end;

  if (LAnchor = nil) or not Web.document.body.contains(LAnchor) then
  begin
    raise ENyxCollection.Create('The browser query column is no longer mounted');
  end;
  Result := TBrowserQueryForm.Create(Self, LAnchor, ADocument, ARoot);
end;

constructor TBrowserQueryForm.Create(ATarget: TBrowserQueryFactory; AAnchor: TJSHTMLElement; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef);
begin
  inherited Create;
  FTarget := ATarget;
  FLease := ATarget;
  { The existing host clones the document during construction. Its owned state,
    controls, focus recovery and event generation share normal Nyx lifetimes. }
  FHost := NewNyxBrowserPopover(AAnchor, ADocument, ARoot, ATarget.Theme);
  FConnected := True;
end;

destructor TBrowserQueryForm.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TBrowserQueryForm.GetConnected: Boolean;
begin
  Result := FConnected and FTarget.Active and (FHost <> nil);
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
    Result := FHost.Events;
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
begin
  FConnected := False;
  LHost := FHost;
  FHost := nil;
  try

    if LHost <> nil then
    begin
      LHost.Close;
    end;
  finally
    FTarget := nil;
    FLease := nil;
  end;
end;

function NewNyxBrowserQueryFormFactory(AAnchor: TJSHTMLElement;
  ATheme: TNyxTheme): INyxQueryFormPresenterFactory;
begin
  Result := TBrowserQueryFactory.Create(AAnchor, ATheme);
end;

end.
