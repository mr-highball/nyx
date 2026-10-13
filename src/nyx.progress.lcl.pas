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

unit nyx.progress.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, Controls, ComCtrls, ExtCtrls, Graphics, nyx.theme, nyx.widgets.lcl;

type
  { Standard LCL numeric progress remains the owning native control. A small
    noninteractive child paints unknown activity because several widgetset/theme
    combinations accept marquee style but continue painting a percentage.
    No model, renderer, theme or scheduler is retained. Timer and painted child
    belong to this control and retire before its native handle. }
  TNyxLCLProgress = class(TProgressBar)
  private
    FActivity: TCustomControl;
    FTimer: TTimer;
    FIndeterminate: Boolean;
    FPhase: Integer;
    FTrackColor: TColor;
    FActivityColor: TColor;
    procedure SetIndeterminate(AValue: Boolean);
    procedure Tick(ASender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Copies admitted palette fields. Caller may release/change its theme after
      this method; no borrowed theme survives into the animation callback. }
    procedure ApplyTheme(ATheme: TNyxTheme);
    { Changing presentation preserves Min, Max and Position. The timer runs only
      while activity is shown; an ordinary Sync does not reset its phase. }
    property Indeterminate: Boolean read FIndeterminate write SetIndeterminate;
  end;

implementation

uses
  Math, SysUtils, LMessages, nyx.model;

type
  { This display is deliberately inert and has no input/event producer. Its
    borrowed owner is an ordinary LCL parent; ownership is not reference counted. }
  TNyxLCLProgressActivity = class(TCustomControl)
  private
    FProgress: TNyxLCLProgress;
    procedure ShowingChanged(var AMessage: TLMessage); message CM_SHOWINGCHANGED;
  protected
    procedure Paint; override;
  public
    constructor CreateActivity(AProgress: TNyxLCLProgress);
  end;

constructor TNyxLCLProgressActivity.CreateActivity(AProgress: TNyxLCLProgress);
begin
  inherited Create(AProgress);
  FProgress := AProgress;
  Parent := AProgress;
  Align := alClient;
  BorderSpacing.Around := 1;
  TabStop := False;
  AccessibleRole := larIgnore;
  DoubleBuffered := True;
end;

procedure TNyxLCLProgressActivity.ShowingChanged(var AMessage: TLMessage);
begin
  inherited;
  FProgress.FTimer.Enabled := FProgress.FIndeterminate and Showing;
end;

procedure TNyxLCLProgressActivity.Paint;
var
  LChunkWidth: Integer;
  LLeft: Integer;
begin
  Canvas.Brush.Color := FProgress.FTrackColor;
  Canvas.FillRect(ClientRect);
  LChunkWidth := Max(12, ClientWidth div 5);
  LLeft := (ClientWidth + LChunkWidth) * FProgress.FPhase div 100 - LChunkWidth;
  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := FProgress.FActivityColor;
  Canvas.RoundRect(LLeft, 1, LLeft + LChunkWidth, ClientHeight - 1, 6, 6);
end;

constructor TNyxLCLProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 40;
  FTimer.OnTimer := Tick;
  FTrackColor := clWindow;
  FActivityColor := clHighlight;
end;

destructor TNyxLCLProgress.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.OnTimer := nil;
  FreeAndNil(FActivity);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TNyxLCLProgress.ApplyTheme(ATheme: TNyxTheme);
begin

  if ATheme = nil then
  begin
    raise ENyxModel.Create('Native progress requires a theme');
  end;
  ATheme.Validate;
  FTrackColor := NyxLCLColor(ATheme.Surface);
  FActivityColor := NyxLCLColor(ATheme.Accent);

  if FActivity <> nil then
  begin
    FActivity.Invalidate;
  end;
end;

procedure TNyxLCLProgress.SetIndeterminate(AValue: Boolean);
begin

  if FIndeterminate = AValue then
  begin
    Exit;
  end;
  FIndeterminate := AValue;

  if AValue then
  begin
    Style := pbstMarquee;

    if FActivity = nil then
    begin
      FActivity := TNyxLCLProgressActivity.CreateActivity(Self);
    end;
    FActivity.Visible := True;
    FTimer.Enabled := FActivity.Showing;
  end
  else
  begin
    FTimer.Enabled := False;

    if FActivity <> nil then
    begin
      FActivity.Visible := False;
    end;
    Style := pbstNormal;
  end;
  Invalidate;
end;

procedure TNyxLCLProgress.Tick(ASender: TObject);
begin

  if not FIndeterminate or (FActivity = nil) or not FActivity.Showing then
  begin
    FTimer.Enabled := False;
    Exit;
  end;
  FPhase := (FPhase + 4) mod 100;
  FActivity.Invalidate;
end;

end.
