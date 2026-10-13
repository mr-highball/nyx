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

unit nyx.tabs.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, LCLType, Graphics,
  nyx.text, nyx.model, nyx.tabs, nyx.scheduler;

type
  TNyxLCLTabChange = procedure(const AChange: TNyxTabTransition) of object;
  { Native composition reuses real LCL buttons, page control and tab sheets.
    Separate header focus permits manual activation without selecting a panel.
    The owning control retires children; the renderer owns the copied source
    state/lifetime and never rebuilds a notifying header during selection. }
  TNyxLCLTabHost = class(TPanel)
  private
    FBar: TScrollBox;
    FPages: TPageControl;
    FHeaders: array of TButton;
    FSheets: array of TTabSheet;
    FState: TNyxTabState;
    FOnChange: TNyxLCLTabChange;
    FMovingFocus: Boolean;
    FLifetime: INyxCancellationScope;
    function HeaderIndex(ASender: TObject): Integer;
    procedure HeaderClick(ASender: TObject);
    procedure HeaderEnter(ASender: TObject);
    procedure HeaderKey(ASender: TObject; var AKey: Word; AShift: TShiftState);
    procedure Present;
    procedure Apply(const AChange: TNyxTabTransition; AFocus: Boolean);
  protected
    procedure Resize; override;
  public
    constructor CreateForTabs(AOwner: TComponent; ANode: TNyxNode);
    destructor Destroy; override;
    function PageHost(AIndex: Integer): TTabSheet;
    function Header(AIndex: Integer): TButton;
    procedure Arrange;
    property State: TNyxTabState read FState;
    property OnChange: TNyxLCLTabChange read FOnChange write FOnChange;
  end;

implementation

constructor TNyxLCLTabHost.CreateForTabs(AOwner: TComponent; ANode: TNyxNode);
var
  LIndex: Integer;
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Caption := '';
  FState := TNyxTabState.Create(ANode);
  FLifetime := NewNyxCancellationScope;
  FBar := TScrollBox.Create(Self);
  FBar.Parent := Self;
  FBar.BorderStyle := bsNone;
  FPages := TPageControl.Create(Self);
  FPages.Parent := Self;
  FPages.ShowTabs := False;
  FPages.TabStop := False;
  SetLength(FHeaders, FState.Count);
  SetLength(FSheets, FState.Count);
  for LIndex := 0 to FState.Count - 1 do
  begin
    FHeaders[LIndex] := TButton.Create(Self);
    FHeaders[LIndex].Parent := FBar;
    FHeaders[LIndex].Caption := FState.Entry(LIndex).Caption;
    FHeaders[LIndex].OnClick := HeaderClick;
    FHeaders[LIndex].OnEnter := HeaderEnter;
    FHeaders[LIndex].OnKeyDown := HeaderKey;
    FSheets[LIndex] := TTabSheet.Create(Self);
    FSheets[LIndex].PageControl := FPages;
    FSheets[LIndex].Caption := FState.Entry(LIndex).Caption;
  end;
  Present;
  Arrange;
end;

destructor TNyxLCLTabHost.Destroy;
var
  LIndex: Integer;
begin

  if FLifetime <> nil then
  begin
    FLifetime.Cancel;
  end;
  FOnChange := nil;
  for LIndex := 0 to High(FHeaders) do
  begin

    if FHeaders[LIndex] <> nil then
    begin
      FHeaders[LIndex].OnClick := nil;
      FHeaders[LIndex].OnEnter := nil;
      FHeaders[LIndex].OnKeyDown := nil;
    end;
  end;
  FState.Free;
  FState := nil;
  inherited Destroy;
end;

function TNyxLCLTabHost.HeaderIndex(ASender: TObject): Integer;
var
  LIndex: Integer;
begin
  Result := -1;

  if not Enabled then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FHeaders) do
  begin

    if (ASender = FHeaders[LIndex]) and FState.Entry(LIndex).Available then
    begin
      Exit(LIndex);
    end;
  end;
end;

procedure TNyxLCLTabHost.Present;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FHeaders) do
  begin
    FHeaders[LIndex].Enabled := FState.Entry(LIndex).Available;
    FHeaders[LIndex].Visible := FState.Entry(LIndex).Visible;
    FHeaders[LIndex].TabStop := FState.FocusIndex = LIndex;
    FHeaders[LIndex].Font.Style := [];

    if FState.SelectedIndex = LIndex then
    begin
      FHeaders[LIndex].Font.Style := [fsBold];
    end;
  end;
  FPages.ActivePageIndex := FState.SelectedIndex;
end;

procedure TNyxLCLTabHost.Apply(const AChange: TNyxTabTransition; AFocus: Boolean);
var
  LNotify: TNyxLCLTabChange;
  LLease: INyxCancellationScope;
begin
  LLease := FLifetime;
  Present;

  if AFocus and (FState.FocusIndex >= 0) and FHeaders[FState.FocusIndex].CanFocus then
  begin
    FMovingFocus := True;
    try
      FHeaders[FState.FocusIndex].SetFocus;
    finally

      if not LLease.Cancelled then
      begin
        FMovingFocus := False;
      end;
    end;
  end;

  if LLease.Cancelled then
  begin
    Exit;
  end;
  LNotify := FOnChange;

  if AChange.SelectionChanged and Assigned(LNotify) then
  begin
    LNotify(AChange);
  end;
end;

procedure TNyxLCLTabHost.HeaderClick(ASender: TObject);
var
  LIndex: Integer;
begin
  LIndex := HeaderIndex(ASender);

  if LIndex >= 0 then
  begin
    Apply(FState.Activate(FState.Entry(LIndex).Reference), False);
  end;
end;

procedure TNyxLCLTabHost.HeaderEnter(ASender: TObject);
var
  LIndex: Integer;
begin

  if FMovingFocus then
  begin
    Exit;
  end;
  LIndex := HeaderIndex(ASender);

  if LIndex >= 0 then
  begin
    Apply(FState.Focus(FState.Entry(LIndex).Reference), False);
  end;
end;

procedure TNyxLCLTabHost.HeaderKey(ASender: TObject; var AKey: Word; AShift: TShiftState);
var
  LMovement: TNyxTabMovement;
  LHasMovement: Boolean;
  LChange: TNyxTabTransition;
begin

  if (HeaderIndex(ASender) < 0) or (AShift * [ssCtrl, ssAlt, ssMeta, ssSuper] <> []) then
  begin
    Exit;
  end;
  LHasMovement := True;
  LMovement := ntmNext;

  if AKey = VK_HOME then
  begin
    LMovement := ntmFirst;
  end
  else if AKey = VK_END then
  begin
    LMovement := ntmLast;
  end
  else if ((FState.Options.OrientationMode = ntoHorizontal) and (AKey = VK_LEFT)) or
    ((FState.Options.OrientationMode = ntoVertical) and (AKey = VK_UP)) then
  begin
    LMovement := ntmPrevious;
  end
  else if ((FState.Options.OrientationMode = ntoHorizontal) and (AKey = VK_RIGHT)) or
    ((FState.Options.OrientationMode = ntoVertical) and (AKey = VK_DOWN)) then
  begin
    LMovement := ntmNext;
  end
  else
  begin
    LHasMovement := False;
  end;

  if LHasMovement then
  begin
    LChange := FState.Move(LMovement);
  end
  else if (AKey = VK_RETURN) or (AKey = VK_SPACE) then
  begin
    LChange := FState.ActivateFocused;
  end
  else
  begin
    Exit;
  end;
  AKey := 0;
  Apply(LChange, LHasMovement);
end;

procedure TNyxLCLTabHost.Resize;
begin
  inherited Resize;

  if FPages <> nil then
  begin
    Arrange;
  end;
end;

procedure TNyxLCLTabHost.Arrange;
var
  LIndex: Integer;
  LHeaderWidth: Integer;
  LHeaderHeight: Integer;
  LOffset: Integer;
  LRemaining: Integer;
begin

  if (FPages = nil) or (FState = nil) then
  begin
    Exit;
  end;
  LHeaderHeight := 56;
  LOffset := 4;

  if FState.Options.OrientationMode = ntoVertical then
  begin
    LHeaderWidth := 160;

    if LHeaderWidth > ClientWidth then
    begin
      LHeaderWidth := ClientWidth;
    end;
    FBar.HorzScrollBar.Visible := False;
    FBar.VertScrollBar.Visible := True;
    FBar.SetBounds(0, 0, LHeaderWidth, ClientHeight);
    FPages.SetBounds(LHeaderWidth, 0, ClientWidth - LHeaderWidth, ClientHeight);
    for LIndex := 0 to High(FHeaders) do
    begin
      LRemaining := LHeaderWidth - 8;

      if LRemaining < 0 then
      begin
        LRemaining := 0;
      end;
      FHeaders[LIndex].SetBounds(4, LOffset, LRemaining, 32);

      if FHeaders[LIndex].Visible then
      begin
        Inc(LOffset, 36);
      end;
    end;
  end
  else
  begin
    LHeaderWidth := 120;
    FBar.HorzScrollBar.Visible := True;
    FBar.VertScrollBar.Visible := False;

    if LHeaderHeight > ClientHeight then
    begin
      LHeaderHeight := ClientHeight;
    end;
    FBar.SetBounds(0, 0, ClientWidth, LHeaderHeight);
    FPages.SetBounds(0, LHeaderHeight, ClientWidth, ClientHeight - LHeaderHeight);
    for LIndex := 0 to High(FHeaders) do
    begin
      FHeaders[LIndex].SetBounds(LOffset, 2, LHeaderWidth - 8, 32);

      if FHeaders[LIndex].Visible then
      begin
        Inc(LOffset, LHeaderWidth);
      end;
    end;
  end;
end;

function TNyxLCLTabHost.PageHost(AIndex: Integer): TTabSheet;
begin
  FState.Entry(AIndex);
  Result := FSheets[AIndex];
end;

function TNyxLCLTabHost.Header(AIndex: Integer): TButton;
begin
  FState.Entry(AIndex);
  Result := FHeaders[AIndex];
end;

end.
