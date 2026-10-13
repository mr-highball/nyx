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

program nyx_tabs_demo_native;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Forms, StdCtrls, nyx.text, nyx.model,
  nyx.codec, nyx.generated.view, nyx.studio.projects, nyx.render.lcl,
  nyx.tabs.lcl, nyx.test.capture.lcl;

type
  TButtonAccess = class(TButton);

var
  LDocument: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LForm: TForm;
  LTabs: TNyxLCLTabHost;
  LMemo: TMemo;
  LInput: TFileStream;
  LPacket: TNyxText;
  LBefore: TNyxText;
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(LChecks);
end;

begin
  LDocument := nil;
  LRenderer := nil;
  LForm := nil;
  try

    if (ParamCount < 1) or (ParamCount > 2) then
    begin
      raise Exception.Create('Supply the exact MCP project and optional diagnostic PNG path');
    end;
    LInput := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LPacket, LInput.Size);

      if LPacket <> '' then
      begin
        LInput.ReadBuffer(LPacket[1], Length(LPacket));
      end;
    finally
      LInput.Free;
    end;
    LDocument := BuildNyxDocument;
    LBefore := TNyxCodec.Encode(LDocument);
    Check(LBefore = DecodeNyxProject(LPacket).Design, 'executed builder reproduces exact semantic design');
    Application.Initialize;
    LForm := TForm.CreateNew(nil);
    LForm.SetBounds(0, 0, 850, 700);
    LRenderer := TNyxLCLRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Find('ideas'), LForm);
    LForm.Show;
    Application.ProcessMessages;
    LTabs := TNyxLCLTabHost(LRenderer.ControlFor('idea-sections'));
    Check((LTabs.State.Count = 3) and (LTabs.Header(0).Caption = 'Plan') and
      (LTabs.Header(1).Caption = 'Details'), 'actual native headers use exact English companion captions');
    Check(LTabs.State.SelectedIndex = 0, 'native companion starts with Plan');
    LMemo := TMemo(LRenderer.InputFor('plan-notes'));
    LMemo.Text := 'An idea worth keeping';
    TButtonAccess(LTabs.Header(1)).Click;
    Check(LTabs.State.SelectedIndex = 1, 'native companion opens Details');
    TButtonAccess(LTabs.Header(0)).Click;
    Check(TMemo(LRenderer.InputFor('plan-notes')) = LMemo, 'native companion retains exact memo identity');
    Check(TNyxText(LMemo.Text) = 'An idea worth keeping', 'native companion retains unfinished notes');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'native runtime interaction preserves exact authored design');

    if ParamCount = 2 then
    begin
      { Print is diagnostic redraw evidence, not a physical display/DPI claim. }
      SaveNyxNativeCapture(LForm, ParamStr(2), ncmPrint);
    end;
    WriteLn('PASS ', LChecks, ' exact semantic native tab companion checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  LRenderer.Free;
  LForm.Free;
  LDocument.Free;
end.
