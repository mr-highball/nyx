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
  nyx.collections, nyx.collections.query.editor;

type
  { Retained by forms as a flags-only weak lifetime lease. This factory never
    retains a form or renderer, so its small managed lease cannot form a cycle. }
  TLCLQueryFactory = class(TInterfacedObject, INyxQueryFormPresenterFactory)
  public
    Active: Boolean;
    Anchor: TWinControl;
    Theme: TNyxTheme;
    constructor Create(AAnchor: TWinControl; ATheme: TNyxTheme);
    destructor Destroy; override;
    function CreateForm(AColumn: Integer; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): INyxQueryFormPresenter;
    procedure Revoke;
  end;

  { Captures the REAL realized form, whose existing live binding owner mirrors
    accepted physical text/select/checkbox changes. Authored Content is a clone
    and must never be mistaken for the user's current draft. }
  TLCLQueryForm = class(TInterfacedObject, INyxQueryFormPresenter, INyxLCLQueryFormPresenter)
  private
    FTarget: TLCLQueryFactory;
    FLease: INyxQueryFormPresenterFactory;
    FHost: INyxLCLPopover;
    FConnected: Boolean;
  public
    constructor Create(ATarget: TLCLQueryFactory; ADocument: TNyxDocument;
      const ARoot: TNyxRootRef);
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
end;

function TLCLQueryFactory.CreateForm(AColumn: Integer; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): INyxQueryFormPresenter;
begin

  if not Active or (Anchor = nil) or (AColumn < -1) then
  begin
    raise ENyxCollection.Create('The native table query anchor has retired');
  end;
  Result := TLCLQueryForm.Create(Self, ADocument, ARoot);
end;

constructor TLCLQueryForm.Create(ATarget: TLCLQueryFactory; ADocument: TNyxDocument;
  const ARoot: TNyxRootRef);
begin
  inherited Create;
  FTarget := ATarget;
  FLease := ATarget;
  { The existing host clones the document during construction. Its owned state,
    controls, focus recovery and event generation share normal Nyx lifetimes. }
  FHost := NewNyxLCLPopover(ATarget.Anchor, ADocument, ARoot, ATarget.Theme);
  FConnected := True;
end;

destructor TLCLQueryForm.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TLCLQueryForm.GetConnected: Boolean;
begin
  Result := FConnected and FTarget.Active and (FHost <> nil);
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
    Result := FHost.Events;
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

function NewNyxLCLQueryFormFactory(AAnchor: TWinControl;
  ATheme: TNyxTheme): INyxQueryFormPresenterFactory;
begin
  Result := TLCLQueryFactory.Create(AAnchor, ATheme);
end;

end.
