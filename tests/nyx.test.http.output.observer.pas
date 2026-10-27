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

unit nyx.test.http.output.observer;

{$mode delphi}{$H+}{$codepage utf8}

interface

implementation

uses Interfaces, Forms, Controls, StdCtrls, ExtCtrls, SysUtils, nyx.text, nyx.data;

type
  { Exposes the inherited ordinary control input; it neither owns widgets nor
    replaces callbacks, application stores, logic resolvers or HTTP transports. }
  TControlInput = class(TControl);

  { Opt-in observer linked by one test-only uses addition to a private copy of
    the emitted entry. Its service setup remains byte-identical. A UI timer
    borrows current controls only during each tick and starts/configures no host,
    resolver or application store. Disable before termination and retire the
    timer during finalization. Ordinary window-message delivery is a separate
    unaccepted test; this observer exercises inherited native control input. }
  THTTPOutputObserver = class
  private
    FTimer: TTimer;
    FStarted: QWord;
    FStage: Integer;
    FWidth: Integer;
    FURL: TNyxText;
    FFrame: array[0..1] of TNyxText;
    procedure Tick(ASender: TObject);
    procedure Finish(ASuccess: Boolean; const AMessage: TNyxText);
    procedure Failed(ASender: TObject; AException: Exception);
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  GObserver: THTTPOutputObserver;

function FindCaption(AParent: TWinControl; const AText: TNyxText): TControl;
var
  LIndex: Integer;
  LControl: TControl;
begin
  Result := nil;
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin
    LControl := AParent.Controls[LIndex];

    if TNyxText(TControlInput(LControl).Caption) = AText then
    begin
      Exit(LControl);
    end;

    if LControl is TWinControl then
    begin
      Result := FindCaption(TWinControl(LControl), AText);

      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
end;

procedure FindInputs(AParent: TWinControl; var AURL: TEdit; var ASource, APreview: TMemo);
var
  LIndex: Integer;
  LControl: TControl;
begin
  for LIndex := 0 to AParent.ControlCount - 1 do
  begin
    LControl := AParent.Controls[LIndex];

    if LControl is TMemo then
    begin

      if TMemo(LControl).ReadOnly then
      begin
        APreview := TMemo(LControl);
      end
      else
      begin
        ASource := TMemo(LControl);
      end;
    end
    else if LControl is TEdit then
    begin
      AURL := TEdit(LControl);
    end;

    if LControl is TWinControl then
    begin
      FindInputs(TWinControl(LControl), AURL, ASource, APreview);
    end;
  end;
end;

constructor THTTPOutputObserver.Create;
begin
  inherited Create;

  if ParamCount <> 2 then
  begin
    raise Exception.Create('Instrumented entry requires width and existing health URL');
  end;
  FWidth := StrToInt(ParamStr(1));
  FURL := TNyxText(ParamStr(2));
  FStarted := GetTickCount64;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 30;
  FTimer.OnTimer := Tick;
  { The generated entry supplies no exception receiver. Preserve asynchronous
    widget failures as failed qualification instead of opening a modal that
    prevents the bounded observer from returning its receipt. }
  Application.OnException := Failed;
  FTimer.Enabled := True;
end;

destructor THTTPOutputObserver.Destroy;
begin
  Application.OnException := nil;
  FTimer.Free;
  inherited Destroy;
end;

procedure THTTPOutputObserver.Failed(ASender: TObject; AException: Exception);
begin
  Finish(False, TNyxText(AException.ClassName + ': ' + AException.Message));
end;

procedure THTTPOutputObserver.Finish(ASuccess: Boolean; const AMessage: TNyxText);
begin
  FTimer.Enabled := False;

  if ASuccess then
  begin
    WriteLn('PASS emitted entry with native control observer / width ', FWidth, ' / ', AMessage);
  end
  else
  begin
    WriteLn('FAIL emitted entry at stage ', FStage, ' / ', AMessage);
    ExitCode := 1;
  end;
  Flush(Output);
  Application.Terminate;
end;

procedure THTTPOutputObserver.Tick(ASender: TObject);
var
  LIndex: Integer;
  LWindow: TForm;
  LFetch: TControl;
  LURL: TEdit;
  LSource: TMemo;
  LPreview: TMemo;
  LUnusedSource: TMemo;
  LUnusedPreview: TMemo;
  LHealth: TNyxDataValue;
begin
  FTimer.Enabled := False;
  try
    try

      if GetTickCount64 - FStarted > 60000 then
      begin
        raise Exception.Create('Control/HTTP observer exceeded its bounded runtime');
      end;
      LWindow := nil;
      LFetch := nil;
      for LIndex := 0 to Screen.FormCount - 1 do
      begin
        LFetch := FindCaption(Screen.Forms[LIndex], 'Load URL');

        if LFetch <> nil then
        begin
          LWindow := Screen.Forms[LIndex];
          Break;
        end;
      end;

      if (LWindow = nil) or (LFetch = nil) then
      begin
        Exit;
      end;
      LURL := nil;
      LSource := nil;
      LPreview := nil;
      FindInputs(LWindow, LURL, LSource, LPreview);
      { The URL is in the fetch row; ignore the Tool Canvas search field. }
      LURL := nil;
      LUnusedSource := nil;
      LUnusedPreview := nil;
      FindInputs(LFetch.Parent, LURL, LUnusedSource, LUnusedPreview);

      if (LURL = nil) or (LSource = nil) or (LPreview = nil) then
      begin
        raise Exception.Create('The emitted Workshop does not expose its input/result controls');
      end;
      case FStage of
        0:
          begin
            LWindow.ClientWidth := FWidth;
            LWindow.ClientHeight := 900;
          end;
        1:
          begin
            LURL.Text := String(FURL);
          end;
        2:
          begin
            TControlInput(LFetch).Click;
          end;
        3:
          begin

            if (Pos('nyx-studio-server', TNyxText(LSource.Text)) = 0) or
              (Pos('nyx-studio-server', TNyxText(LPreview.Text)) = 0) then
            begin
              Exit;
            end;
            FFrame[0] := TNyxText(LSource.Text);
            FFrame[1] := TNyxText(LPreview.Text);
            for LIndex := 0 to 1 do
            begin
              LHealth := TNyxDataValue.ParseJSON(FFrame[LIndex]);

              if not LHealth.Field('ok').AsBoolean or
                (LHealth.Field('service').AsText <> 'nyx-studio-server') or
                (LHealth.Field('protocol').AsInteger <> 1) then
              begin
                raise Exception.Create('The selected host did not publish the complete health frame');
              end;
            end;
            LURL.Text := '';
            TControlInput(LFetch).Click;
          end;
        4:
          begin

            if (TNyxText(LSource.Text) <> FFrame[0]) or
              (TNyxText(LPreview.Text) <> FFrame[1]) then
            begin
              raise Exception.Create('Empty URL input changed the complete accepted frame');
            end;
            Finish(True, 'selected real HTTP transport; exact response; invalid input preserves frame');
            Exit;
          end;
      end;
      Inc(FStage);
    except
      on LException: Exception do
      begin
        Finish(False, TNyxText(LException.Message));
      end;
    end;
  finally

    if not Application.Terminated then
    begin
      FTimer.Enabled := True;
    end;
  end;
end;

initialization
  GObserver := THTTPOutputObserver.Create;
finalization
  GObserver.Free;
end.
