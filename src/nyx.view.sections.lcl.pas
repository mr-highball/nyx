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
  Controls, nyx.model, nyx.state, nyx.theme, nyx.behavior, nyx.view.sections, nyx.render.lcl;

type
  { Borrowed configurator runs for each fresh candidate, before Render.
    Configure target factories only; do not pump queues, schedule work or retain
    the candidate. Its receiver must outlive the section and pending handles. }
  TNyxLCLSectionConfigure = procedure(ARenderer: TNyxLCLRenderer) of object;

  INyxLCLViewSection = interface(INyxViewSection)
    ['{A949C907-AC0B-4B4F-883C-476746278879}']
    { Borrow current renderer for control lookup/event subscription. Do not
      Render/Unmount/MoveHost it directly; publication owns those lifetimes.
      Pending/current renderer-owned runtime stores must never be borrowed by
      their replacements; use a caller-owned store or independent defaults. }
    function GetRenderer: TNyxLCLRenderer;
    property Renderer: TNyxLCLRenderer read GetRenderer;
  end;

{ Host is an empty dedicated child of a live native parent. Its parent must also
  outlive section/change handles because hidden parking hosts are siblings.
  Observer is managed; host/configurator receivers remain explicitly borrowed.
  A nonnil theme is borrowed and must outlive the section and pending handles;
  ordinary renderer admission snapshots it into each prepared view.
  Replacing a section creates a fresh view; unmentioned sections retain drafts. }
function NewNyxLCLViewSection(const AReference: TNyxViewSectionRef; AHost: TWinControl;
  AConfigure: TNyxLCLSectionConfigure = nil;
  const AObserver: INyxViewSectionObserver = nil;
  ATheme: TNyxTheme = nil): INyxLCLViewSection;

implementation

uses
  SysUtils, Forms, ExtCtrls, nyx.editing, nyx.editing.lcl, nyx.schema;

type
  TSectionHost = TWinControl;
  TSectionFocus = TWinControl;
  TSectionParent = TWinControl;
  TSectionRenderer = TNyxLCLRenderer;
  TSectionConfigure = TNyxLCLSectionConfigure;

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

function NewSectionParking(AHost: TSectionHost): TSectionHost;
var
  LPanel: TPanel;
begin
  LPanel := TPanel.Create(nil);
  try
    LPanel.Visible := False;
    LPanel.BevelOuter := bvNone;
    LPanel.Parent := AHost.Parent;
    LPanel.Font.Assign(AHost.Font);
    LPanel.SetBounds(AHost.Left, AHost.Top, AHost.ClientWidth, AHost.ClientHeight);
    Result := LPanel;
  except
    LPanel.Free;
    raise;
  end;
end;

procedure FreeSectionParking(var AHost: TSectionHost);
begin
  FreeAndNil(AHost);
end;

function SectionPlacementMatches(AHost: TSectionHost; ARenderer: TSectionRenderer): Boolean;
var
  LControl: TControl;
begin
  Result := False;

  if (AHost = nil) or (AHost.Parent = nil) then
  begin
    Exit;
  end;

  if ARenderer = nil then
  begin
    Exit(AHost.ControlCount = 0);
  end;
  ARenderer.Events.Scheduler.RequireUI;
  LControl := ARenderer.ControlFor(ARenderer.Root.ID);
  Result := (LControl.Parent <> nil) and (LControl.Parent.Parent = AHost) and
    (AHost.ControlCount = 1);
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

procedure RestoreSectionFocus(AFocus: TSectionFocus; const ASelection: TNyxTextSelection);
begin

  if (AFocus <> nil) and AFocus.CanFocus then
  begin
    AFocus.SetFocus;

    if ASelection.Defined then
    begin
      SelectNyxLCLText(AFocus, ASelection);
    end;
  end;
end;

procedure FinishSectionPreview(ARenderer: TSectionRenderer);
begin
  ARenderer.Sync;
end;

{$include nyx.view.sections.adapter.inc}

function NewNyxLCLViewSection(const AReference: TNyxViewSectionRef; AHost: TWinControl;
  AConfigure: TNyxLCLSectionConfigure; const AObserver: INyxViewSectionObserver;
  ATheme: TNyxTheme):
  INyxLCLViewSection;
begin
  Result := TSection.Create(AReference, AHost, AConfigure, AObserver, ATheme);
end;

end.
