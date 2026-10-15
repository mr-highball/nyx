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

program nyx_label_measurement_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, Math, Interfaces, Forms, Controls, StdCtrls, ExtCtrls, LCLIntf,
  LCLType, Graphics, nyx.text, nyx.model, nyx.controls, nyx.types, nyx.render.lcl;

type
  { Fresh independent widgetset oracle. It owns no borrowed label/parent and
    measures once on a scoped desktop context, with no Nyx result cache. }
  TOracleLabel = class(TLabel)
  public
    procedure SizeFor(ASource: TLabel; out AHeight: Integer);
  end;

var
  GChecks: Integer;

procedure TOracleLabel.SizeFor(ASource: TLabel; out AHeight: Integer);
var
  LContext: HDC;
  LWidth: Integer;
begin
  Font.PixelsPerInch := ASource.Font.PixelsPerInch;
  Font.Assign(ASource.Font);
  Caption := ASource.Caption;
  WordWrap := ASource.WordWrap;
  ShowAccelChar := ASource.ShowAccelChar;
  BiDiMode := ASource.BiDiMode;
  LContext := LCLIntf.GetDC(0);

  if LContext = 0 then
  begin
    raise Exception.Create('Widgetset oracle could not acquire a drawing context');
  end;
  try
    Canvas.Handle := LContext;
    try
      CalculateSize(Max(1, ASource.Width), LWidth, AHeight);
    finally
      Canvas.Handle := 0;
    end;
  finally
    LCLIntf.ReleaseDC(0, LContext);
  end;
end;

procedure Check(AValue: Boolean; const AMessage: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LForm: TForm;
  LHost: TPanel;
  LRenderer: TNyxLCLRenderer;
  LDoc: TNyxDocument;
  LPage: INyxPage;
  LOracle: TOracleLabel;
  LLabel: TLabel;
  LIndex: Integer;
  LPass: Integer;
  LHeight: Integer;
  LBottom: Integer;
begin
  LForm := TForm.CreateNew(nil);
  LDoc := TNyxDocument.Create;
  LRenderer := TNyxLCLRenderer.Create;
  LOracle := TOracleLabel.Create(nil);
  try
    LForm.SetBounds(20, 20, 700, 820);
    LHost := TPanel.Create(LForm);
    LHost.Parent := LForm;
    LHost.SetBounds(0, 0, 640, 780);
    LForm.Show;
    LPage := NewNyxPage('metrics');
    LPage.Configure.Padding(0).Gap(4).Align(ncaStretch).Done;
    for LIndex := 0 to 11 do
    begin
      LPage.Add(NewNyxLabel('caption-' + IntToStr(LIndex))
        .WithText('Keep & read this longer caption 🌙 across multiple measured visits.'));
    end;
    LDoc.AddPage(LPage);
    LRenderer.Render(LDoc, LDoc.Pages[0], LHost);
    for LPass := 0 to 3 do
    begin

      if LPass = 1 then
      begin
        LHost.Width := 240;
      end
      else if LPass = 2 then
      begin
        LHost.Width := 640;
        LDoc.Find('caption-0').Configure.Text('A changed caption' + TNyxText(#10 + 'with Unicode 🌙')).Done;

        if not LRenderer.TryRefresh(LDoc, LDoc.Pages[0], False) then
        begin
          LRenderer.Render(LDoc, LDoc.Pages[0], LHost);
        end;
      end;
      TLabel(LRenderer.ControlFor('caption-1')).Font.Height := -28;
      TLabel(LRenderer.ControlFor('caption-2')).Font.PixelsPerInch := 144;
      TLabel(LRenderer.ControlFor('caption-3')).BiDiMode := bdRightToLeft;
      TLabel(LRenderer.ControlFor('caption-4')).ShowAccelChar := False;
      TLabel(LRenderer.ControlFor('caption-5')).WordWrap := False;
      LRenderer.Sync;
      Application.ProcessMessages;
      LBottom := -1;
      for LIndex := 0 to 11 do
      begin
        LLabel := TLabel(LRenderer.ControlFor('caption-' + IntToStr(LIndex)));
        LOracle.SizeFor(LLabel, LHeight);
        Check(LLabel.Height = Max(28, LHeight + 4),
          'Native oracle pass ' + IntToStr(LPass) + ' / label ' + IntToStr(LIndex) +
          ' / width ' + IntToStr(LLabel.Width) + ' / actual ' + IntToStr(LLabel.Height) +
          ' / text ' + IntToStr(LHeight) + ' / font ' + IntToStr(LLabel.Font.Height));
        Check(LLabel.Top > LBottom, 'measured captions preserve nonoverlapping column allocation');
        LBottom := LLabel.Top + LLabel.Height - 1;
      end;
    end;
  finally
    LOracle.Free;
    LRenderer.Free;
    LDoc.Free;
    LPage := nil;
    LForm.Free;
  end;
end;

begin
  Application.Initialize;
  try
    Run;
    WriteLn('PASS ', GChecks, ' actual LCL label measurement/invalidation checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
