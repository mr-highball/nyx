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
unit nyx.view.sections.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Controls, nyx.model, nyx.state, nyx.theme, nyx.behavior, nyx.view.sections,
  nyx.render.lcl, nyx.viewport.context;

type
  { Borrowed configurator runs for each fresh candidate, before Render.
    Configure target factories only; do not pump queues, schedule work or retain
    the candidate. Its receiver must outlive the section and pending handles. }
  TNyxLCLSectionConfigure = procedure(ARenderer: TNyxLCLRenderer) of object;

  { Managed native section owner. Its accepted renderer is a borrowed lookup
    projection, while the caller's slot/theme and configurator receiver remain
    borrowed for the complete lifetime of this owner and pending changes. }
  INyxLCLViewSection = interface(INyxViewSection)
    ['{A949C907-AC0B-4B4F-883C-476746278879}']
    { Borrow current renderer for control lookup/event subscription. Do not
      Render/Unmount/MoveHost it directly; publication owns those lifetimes.
      Pending/current renderer-owned runtime stores must never be borrowed by
      their replacements; use a caller-owned store or independent defaults. }
    function GetRenderer: TNyxLCLRenderer;
    property Renderer: TNyxLCLRenderer read GetRenderer;
  end;

{ Host is an empty dedicated child of a live native parent. It and its parent
  outlive section/change handles. Private owned presentation planes remain under
  this stable slot through preparation, preview and rollback; only visibility
  changes, so accepted and candidate descendant HWNDs are never reparented.
  Observer is managed; host/configurator receivers remain explicitly borrowed.
  A nonnil theme is borrowed and must outlive the section and pending handles;
  ordinary renderer admission snapshots it into each prepared view.
  Replacing a section creates a fresh view; unmentioned sections retain drafts.
  AViewport optionally supplies enclosing presentation space without changing
  local allocation/container boxes. Nil keeps the allocated-host default. }
function NewNyxLCLViewSection(const AReference: TNyxViewSectionRef; AHost: TWinControl;
  AConfigure: TNyxLCLSectionConfigure = nil;
  const AObserver: INyxViewSectionObserver = nil;
  ATheme: TNyxTheme = nil; const AViewport: INyxViewportContext = nil): INyxLCLViewSection;

implementation

uses
  SysUtils, Forms, ExtCtrls, nyx.editing, nyx.editing.lcl, nyx.schema;

type
  { Target aliases stay inside this native adapter and its shared state machine. }
  TSectionHost = TWinControl;
  { Focus is borrowed only while the accepted renderer remains owned. }
  TSectionFocus = TWinControl;
  { The slot's original parent is an exact publication-allocation fence. }
  TSectionParent = TWinControl;
  { Every plane contains an ordinary independently owned native renderer. }
  TSectionRenderer = TNyxLCLRenderer;
  { A caller-owned configurator must outlive all prepared publications. }
  TSectionConfigure = TNyxLCLSectionConfigure;
  { Private recognized child of one stable slot. The marker borrows its exact
    section, whose publication leases outlive every pending plane. The section
    owns its accepted plane; a publication owns only its unaccepted candidate or
    the previous plane transferred at commit. No reverse interface cycle exists. }
  TSectionPlane = class(TPanel)
  private
    FSlot: TWinControl;
    FSection: TObject;
  public
    constructor CreateFor(ASlot: TWinControl; ASection: TObject);
  end;

constructor TSectionPlane.CreateFor(ASlot: TWinControl; ASection: TObject);
begin
  inherited Create(nil);
  FSlot := ASlot;
  FSection := ASection;
  Visible := False;
  BevelOuter := bvNone;
  Font.Assign(ASlot.Font);
  SetBounds(0, 0, ASlot.ClientWidth, ASlot.ClientHeight);
  Anchors := [akLeft, akTop, akRight, akBottom];
  Parent := ASlot;
end;

function SectionParent(AHost: TSectionHost): TSectionParent;
begin
  Result := AHost.Parent;
end;

function SectionWidth(AHost: TSectionHost): Integer;
begin
  Result := AHost.ClientWidth;
end;

function SectionHeight(AHost: TSectionHost): Integer;
begin
  Result := AHost.ClientHeight;
end;

function ValidSectionHost(AHost: TSectionHost): Boolean;
begin
  Result := (AHost <> nil) and (AHost.Parent <> nil) and (AHost.ControlCount = 0);
end;

function NewSectionPlane(AHost: TSectionHost; ASection: TObject): TSectionHost;
begin
  Result := TSectionPlane.CreateFor(AHost, ASection);
end;

procedure FreeSectionParking(var AHost: TSectionHost);
begin
  FreeAndNil(AHost);
end;

function SectionPlaneMatches(APlane, AHost: TSectionHost;
  ASection: TObject): Boolean;
begin
  Result := (APlane is TSectionPlane) and
    (TSectionPlane(APlane).FSlot = AHost) and
    (TSectionPlane(APlane).FSection = ASection) and
    (APlane.Parent = AHost) and (APlane.Left = 0) and (APlane.Top = 0) and
    (APlane.Width = AHost.ClientWidth) and (APlane.Height = AHost.ClientHeight);
end;

function SectionRendererMatches(APlane: TSectionHost;
  ARenderer: TSectionRenderer): Boolean;
var
  LControl: TControl;
begin
  Result := False;

  if (APlane = nil) or (ARenderer = nil) or (ARenderer.Root = nil) then
  begin
    Exit;
  end;
  ARenderer.Events.Scheduler.RequireUI;
  LControl := ARenderer.ControlFor(ARenderer.Root.ID);
  Result := (LControl.Parent <> nil) and (LControl.Parent.Parent = APlane) and
    (APlane.ControlCount = 1);
end;

function SectionPlacementMatches(AHost: TSectionHost;
  ARenderer: TSectionRenderer; ASection: TObject; ACurrent: TSectionHost;
  APreview: TSectionHost = nil): Boolean;
var
  LIndex: Integer;
  LPlane: TSectionHost;
  LCurrentFound: Boolean;
  LPreviewFound: Boolean;
begin
  Result := False;

  if (AHost = nil) or (AHost.Parent = nil) then
  begin
    Exit;
  end;
  LCurrentFound := ACurrent = nil;
  LPreviewFound := APreview = nil;
  for LIndex := 0 to AHost.ControlCount - 1 do
  begin

    if not (AHost.Controls[LIndex] is TSectionPlane) then
    begin
      Exit; { Never ignore arbitrary caller-owned children. }
    end;
    LPlane := TSectionHost(AHost.Controls[LIndex]);

    if not SectionPlaneMatches(LPlane, AHost, ASection) then
    begin
      Exit;
    end;

    if LPlane = ACurrent then
    begin
      LCurrentFound := True;

      if LPlane.Visible <> (APreview = nil) then
      begin
        Exit;
      end;
    end
    else if LPlane = APreview then
    begin
      LPreviewFound := True;

      if not LPlane.Visible then
      begin
        Exit;
      end;
    end
    else if LPlane.Visible then
    begin
      Exit; { Only this exact preview may expose an unaccepted candidate. }
    end;
  end;
  Result := LCurrentFound and LPreviewFound and
    (((ARenderer = nil) and (ACurrent = nil)) or
      SectionRendererMatches(ACurrent, ARenderer));
end;

procedure CaptureSectionFocus(AHost: TSectionHost; out AFocus: TSectionFocus;
  out ASelection: TNyxTextSelection);
var
  LParent: TWinControl;
begin
  AFocus := nil;
  ASelection := Default(TNyxTextSelection);
  LParent := Screen.ActiveControl;
  while LParent <> nil do
  begin

    if LParent = AHost then
    begin
      AFocus := Screen.ActiveControl;
      ASelection := CaptureNyxLCLSelection(AFocus);
      Exit;
    end;
    LParent := LParent.Parent;
  end;
end;

procedure FinishSectionPreview(ARenderer: TSectionRenderer);
begin
  ARenderer.Sync;
end;

{$include nyx.view.sections.adapter.inc}

function NewNyxLCLViewSection(const AReference: TNyxViewSectionRef; AHost: TWinControl;
  AConfigure: TNyxLCLSectionConfigure; const AObserver: INyxViewSectionObserver;
  ATheme: TNyxTheme; const AViewport: INyxViewportContext):
  INyxLCLViewSection;
begin
  Result := TSection.Create(AReference, AHost, AConfigure, AObserver, ATheme, AViewport);
end;

end.
