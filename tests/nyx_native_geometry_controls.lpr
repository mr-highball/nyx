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
program nyx_native_geometry_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Types, Forms, Controls, ExtCtrls, StdCtrls,
  Windows, CommCtrl, nyx.text, nyx.gestures.lcl, nyx.layout.panel.lcl;

type
  { Fixture-owned revocation flags retain only the existing retirement frame.
    Native panels retain this interface; it never refers back to a control. }
  TMountLifetime = class(TInterfacedObject, INyxLCLLayoutLifetime)
  public
    Active: Boolean;
    Frame: INyxNativeGestureFrame;
    function Current: Boolean;
    function GetFrame: INyxNativeGestureFrame;
  end;
  { One synchronous native callback challenge. Each case has fresh windows;
    no command or borrowed handle is carried between test incarnations. }
  TCallbackAction = (caObserve, caChangeSibling, caRecreateSibling, caRetire);
  { Copied user input state, independent of native handle/control lifetime.
    Recreating a parent window is compared with ordinary LCL behavior rather
    than assuming the widgetset keeps keyboard focus through destruction. }
  TInputSnapshot = record
    Focused: Boolean;
    Text: TNyxText;
    SelectionStart: Integer;
    SelectionLength: Integer;
  end;
  { Native subclass context borrowed only while the form owns its windows.
    The observer forwards the original widgetset receiver and removes itself
    on native destruction. A replacement has its own independent mount. }
  TGeometryCase = class
  public
    Host: TForm;
    Root: TNyxLCLLayoutPanel;
    Panels: array[0..2] of TNyxLCLLayoutPanel;
    Editor: TEdit;
    Lifetime: TMountLifetime;
    Lease: INyxLCLLayoutLifetime;
    Replacement: TNyxLCLLayoutPanel;
    Action: TCallbackAction;
    Armed: Boolean;
    Calls: Integer;
    GroupObserved: Boolean;
    Error: TNyxText;
    constructor Create;
    destructor Destroy; override;
    procedure NativeChanging;
    procedure IgnoreResize(ASender: TObject);
    procedure Failed(ASender: TObject; AError: Exception);
    procedure Resize;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

function TMountLifetime.Current: Boolean;
begin
  Result := Active;
end;

function TMountLifetime.GetFrame: INyxNativeGestureFrame;
begin
  Result := Frame;
end;

function ObserveGeometry(AWindow: HWND; AMessage: UINT; AWParam: WPARAM;
  ALParam: LPARAM; ASubclassID: UINT_PTR; AData: DWORD_PTR): LRESULT; stdcall;
begin

  if AMessage = WM_WINDOWPOSCHANGING then
  begin
    TGeometryCase(Pointer(AData)).NativeChanging;
  end;

  if AMessage = WM_NCDESTROY then
  begin
    RemoveWindowSubclass(AWindow, @ObserveGeometry, ASubclassID);
  end;
  Result := DefSubclassProc(AWindow, AMessage, AWParam, ALParam);
end;

{ Exact native rectangle in the direct parent's client coordinates. These
  zero-border panels intentionally avoid any LCL/native frame approximation. }
procedure CheckGeometry(AControl: TWinControl; const AMessage: TNyxText);
var
  LRect: TRect;
  LClient: TRect;
begin
  LRect := Types.Rect(0, 0, 0, 0);
  LClient := LRect;
  Check(GetWindowRect(AControl.Handle, LRect), AMessage + ': outer read');
  MapWindowPoints(0, AControl.Parent.Handle, LRect, 2);
  Check(Types.EqualRect(LRect, AControl.BoundsRect), AMessage + ': actual position and size');
  Check(Windows.GetClientRect(AControl.Handle, LClient), AMessage + ': client read');
  Check((LClient.Right = AControl.ClientWidth) and (LClient.Bottom = AControl.ClientHeight),
    AMessage + ': actual client area');
end;

function NewPanel(AParent: TWinControl;
  const ALifetime: INyxLCLLayoutLifetime): TNyxLCLLayoutPanel;
begin
  Result := TNyxLCLLayoutPanel.Create(AParent);
  Result.ConnectLayout(ALifetime);
  Result.BevelOuter := bvNone;
  Result.Caption := '';
  Result.Parent := AParent;
end;

constructor TGeometryCase.Create;
var
  LIndex: Integer;
begin
  inherited Create;
  Lifetime := TMountLifetime.Create;
  Lease := Lifetime;
  Lifetime.Active := True;
  Lifetime.Frame := NewNyxNativeGestureFrame;
  Host := TForm.CreateNew(nil);
  Host.ClientWidth := 640;
  Host.ClientHeight := 480;
  Root := NewPanel(Host, Lease);
  Root.SetBounds(10, 10, 600, 420);
  for LIndex := 0 to High(Panels) do
  begin
    Panels[LIndex] := NewPanel(Root, Lease);
    Panels[LIndex].SetBounds(12, 12 + LIndex * 110, 500, 90);
  end;
  Editor := TEdit.Create(Panels[1]);
  Editor.Parent := Panels[1];
  Editor.SetBounds(15, 12, 260, 30);
  Editor.Text := 'A partially edited draft';
  Application.OnException := Failed;
  Host.Show;
  Editor.SetFocus;
  Editor.SelStart := 3;
  Editor.SelLength := 7;
  Application.ProcessMessages;
  Check(SetWindowSubclass(Panels[0].Handle, @ObserveGeometry, 81, DWORD_PTR(Self)),
    'Attach native observer without replacing the widgetset receiver');
end;

destructor TGeometryCase.Destroy;
begin
  Application.OnException := nil;
  Host.Free;
  Lease := nil;
  inherited Destroy;
end;

procedure TGeometryCase.Failed(ASender: TObject; AError: Exception);
begin
  Error := TNyxText(AError.ClassName) + ': ' + TNyxText(AError.Message);
end;

procedure TGeometryCase.IgnoreResize(ASender: TObject);
begin
  { A caller-owned direct geometry callback requires ordinary realization. }
end;

procedure TGeometryCase.NativeChanging;
begin

  if not Armed then
  begin
    Exit;
  end;
  Armed := False;
  Inc(Calls);
  GroupObserved := Lifetime.Frame.Dispatching;
  case Action of
    caObserve:
      begin
        { Observation preserves the original native delivery. }
      end;
    caChangeSibling:
      Panels[1].SetBounds(29, 125, 310, 62);
    caRecreateSibling:
      RecreateWnd(Panels[1]);
    caRetire:
      begin
        Lifetime.Active := False;
        Lifetime.Frame.Retire(Root);
        Root := nil;
        Replacement := NewPanel(Host, nil);
        Replacement.SetBounds(35, 30, 345, 240);
      end;
  end;
end;

procedure TGeometryCase.Resize;
var
  LIndex: Integer;
begin
  Host.DisableAutoSizing;
  try
    for LIndex := 0 to High(Panels) do
    begin
      Panels[LIndex].SetBounds(18, 14 + LIndex * 110, 515, 95);
    end;
    Armed := True;
  finally
    Host.EnableAutoSizing;
  end;
  Application.ProcessMessages;
  Host.Update;
  Check(Error = '', 'Native callbacks must not hide an application exception: ' + Error);
  Check(Calls = 1, 'Native callback challenge executes once');
end;

function RunCase(AAction: TCallbackAction; AFallback: Boolean): TInputSnapshot;
var
  LCase: TGeometryCase;
  LIndex: Integer;
begin
  Result := Default(TInputSnapshot);
  LCase := TGeometryCase.Create;
  try
    LCase.Action := AAction;

    if AFallback then
    begin
      LCase.Panels[1].OnResize := LCase.IgnoreResize;
    end;
    LCase.Resize;
    Check(LCase.GroupObserved <> AFallback,
      'Eligible siblings enter their shared frame; custom callbacks use ordinary LCL');

    if AAction = caRetire then
    begin
      Check(LCase.Replacement <> nil, 'Revocation admits an independent successor');
      CheckGeometry(LCase.Replacement, 'Successor remains untouched by old native commands');
      Check(Types.EqualRect(LCase.Replacement.BoundsRect, Types.Rect(35, 30, 380, 270)),
        'Successor retains its own allocation');
    end
    else
    begin
      for LIndex := 0 to High(LCase.Panels) do
      begin
        CheckGeometry(LCase.Panels[LIndex], 'Displayed sibling ' + IntToStr(LIndex));
      end;

      if AAction = caChangeSibling then
      begin
        Check(Types.EqualRect(LCase.Panels[1].BoundsRect, Types.Rect(29, 125, 339, 187)),
          'Reentrant caller geometry wins over the earlier copied native command');
      end;
      Check((LCase.Editor.Text = 'A partially edited draft') and
        (LCase.Editor.SelStart = 3) and (LCase.Editor.SelLength = 7),
        'Native draft and selection survive geometry publication/recreation');

      if AAction <> caRecreateSibling then
      begin
        Check(LCase.Editor.Focused, 'Geometry publication preserves keyboard focus');
      end;
      Result.Focused := LCase.Editor.Focused;
      Result.Text := LCase.Editor.Text;
      Result.SelectionStart := LCase.Editor.SelStart;
      Result.SelectionLength := LCase.Editor.SelLength;
    end;
  finally
    LCase.Free;
    Application.ProcessMessages;
  end;
end;

{ Incompatible native cohorts must retain stock publication, even when a
  sibling without the customization is the first panel in the realization walk. }
procedure RunFallback(ABeveledSibling: Boolean);
var
  LCase: TGeometryCase;
  LForeign: TPanel;
  LIndex: Integer;
begin
  LCase := TGeometryCase.Create;
  try

    if ABeveledSibling then
    begin
      LCase.Panels[1].BevelInner := bvLowered;
    end
    else
    begin
      LForeign := TPanel.Create(LCase.Root);
      LForeign.Parent := LCase.Root;
      LForeign.SetBounds(3, 380, 80, 20);
    end;
    LCase.Resize;
    Check(not LCase.GroupObserved, 'Incompatible sibling cohorts use ordinary LCL');
    for LIndex := 0 to High(LCase.Panels) do
    begin
      CheckGeometry(LCase.Panels[LIndex], 'Ordinary fallback sibling ' + IntToStr(LIndex));
    end;
    Check(LCase.Editor.Focused and (LCase.Editor.Text = 'A partially edited draft') and
      (LCase.Editor.SelStart = 3) and (LCase.Editor.SelLength = 7),
      'Fallback retains the focused draft and text selection');
  finally
    LCase.Free;
    Application.ProcessMessages;
  end;
end;

var
  LOrdinaryRecreation: TInputSnapshot;
  LGroupedRecreation: TInputSnapshot;
begin
  try
    Application.Initialize;
    RunCase(caObserve, False);
    RunCase(caChangeSibling, False);
    LOrdinaryRecreation := RunCase(caRecreateSibling, True);
    LGroupedRecreation := RunCase(caRecreateSibling, False);
    Check((LOrdinaryRecreation.Focused = LGroupedRecreation.Focused) and
      (LOrdinaryRecreation.Text = LGroupedRecreation.Text) and
      (LOrdinaryRecreation.SelectionStart = LGroupedRecreation.SelectionStart) and
      (LOrdinaryRecreation.SelectionLength = LGroupedRecreation.SelectionLength),
      'Reentrant native recreation retains the same input state as ordinary LCL');
    RunCase(caRetire, False);
    RunCase(caObserve, True);
    RunFallback(True);
    RunFallback(False);
    WriteLn('PASS ', GChecks, ' native grouped geometry checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL ', LError.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
