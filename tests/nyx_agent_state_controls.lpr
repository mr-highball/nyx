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

program nyx_agent_state_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Forms, Controls, StdCtrls, Classes, SysUtils,
  nyx.text, nyx.model, nyx.codec, nyx.state, nyx.binding.types,
  nyx.render.lcl, nyx.generated.view;

var
  LDocument: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LOtherRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LOtherHost: TForm;
  LMemo: TMemo;
  LReusableMemo: TMemo;
  LOtherReusableMemo: TMemo;
  LCheckbox: TCheckBox;
  LNumber: TEdit;
  LExpected: TNyxText;
  LStream: TFileStream;
  LCount: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxState.Create('Compiled semantic controls: ' + AReason);
  end;
  Inc(LCount);
end;

begin
  LDocument := nil;
  LRenderer := nil;
  LOtherRenderer := nil;
  LHost := nil;
  LOtherHost := nil;
  try
    try
      Application.Initialize;

      if ParamCount <> 1 then
      begin
        raise ENyxState.Create('Supply the exact semantic design artifact');
      end;
      LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
      try
        SetLength(LExpected, LStream.Size);

        if Length(LExpected) > 0 then
        begin
          LStream.ReadBuffer(LExpected[1], Length(LExpected));
        end;
      finally
        LStream.Free;
      end;
      { Execute the unchanged source emitted by the semantic boundary. This is
        not a regenerated lookalike or a store-only binding qualification. }
      LDocument := BuildNyxDocument;
      Check(TNyxCodec.Encode(LDocument) = LExpected, 'exact admitted companion reconstruction');
      Check(LDocument.State.GetValue(NyxTextState('qualification-text')) =
        TNyxText('A🌙') + #0 + TNyxText('éZ'), 'compiled supplementary/NUL default');
      LHost := TForm.CreateNew(nil);
      LHost.ClientWidth := 800;
      LHost.ClientHeight := 900;
      LRenderer := TNyxLCLRenderer.Create;
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
      LHost.Show;
      Application.ProcessMessages;
      LMemo := TMemo(LRenderer.InputFor('reply-memo'));
      LReusableMemo := TMemo(LRenderer.InputFor(
        LRenderer.Root.Find(NyxQualifiedID('first-card', 'reply-card')).Part('editor').ID));
      LOtherReusableMemo := TMemo(LRenderer.InputFor(
        LRenderer.Root.Find(NyxQualifiedID('second-card', 'reply-card')).Part('editor').ID));
      LCheckbox := TCheckBox(LRenderer.InputFor('remember-checkbox'));
      LNumber := TEdit(LRenderer.InputFor('ratio-input'));
      Check((TNyxText(LMemo.Text) = 'A thoughtful reply.') and
        (TNyxText(LReusableMemo.Text) = 'A thoughtful reply.') and
        (TNyxText(LOtherReusableMemo.Text) = 'A thoughtful reply.'),
        'renamed inherited bindings reach both independent reusable controls');
      Check(LCheckbox.Checked and (LNumber.Text = '0.375'), 'typed Boolean/number controls');
      Check(TNyxText(LRenderer.ControlFor('quantity-label').Caption) = '7',
        'typed Integer state projects as readable text');
      LMemo.Text := 'Written through a native memo.';
      Application.ProcessMessages;
      Check((LRenderer.State.GetValue(NyxTextState('response')) = TNyxText(LMemo.Text)) and
        (TNyxText(LReusableMemo.Text) = TNyxText(LMemo.Text)) and
        (TNyxText(LOtherReusableMemo.Text) = TNyxText(LMemo.Text)),
        'actual two-way memo input updates its shared runtime projections');
      Check(LDocument.State.GetValue(NyxTextState('response')) = 'A thoughtful reply.',
        'runtime input leaves authored defaults unchanged');
      LCheckbox.Checked := False;
      Check(not LRenderer.State.GetValue(NyxBooleanState('checked')), 'actual Boolean input');
      LNumber.Text := '0.625';
      LNumber.OnEditingDone(LNumber);
      Check(LRenderer.State.GetValue(NyxNumberState('ratio')) = 0.625, 'actual numeric input');
      LNumber.Text := 'invalid';
      LNumber.OnEditingDone(LNumber);
      Check((LRenderer.State.GetValue(NyxNumberState('ratio')) = 0.625) and
        (LNumber.Text = '0.625'), 'invalid numeric input restores the admitted value');
      LOtherHost := TForm.CreateNew(nil);
      LOtherHost.ClientWidth := 390;
      LOtherHost.ClientHeight := 844;
      LOtherRenderer := TNyxLCLRenderer.Create;
      LOtherRenderer.Render(LDocument, LDocument.Pages[0], LOtherHost);
      Check(LOtherRenderer.State.GetValue(NyxTextState('response')) = 'A thoughtful reply.',
        'another runtime receives independent authored defaults');
      LOtherRenderer.State.SetValue(NyxTextState('response'), 'Independent runtime.');
      Check(LRenderer.State.GetValue(NyxTextState('response')) = 'Written through a native memo.',
        'application stores remain independent');
      LRenderer.State.SetValue(NyxBooleanState('enabled'), False);
      Check(not LMemo.Enabled, 'bound parent policy disables the actual memo');
      LMemo.Text := 'Programmatic disabled proposal';
      Check(LRenderer.State.GetValue(NyxTextState('response')) = 'Written through a native memo.',
        'disabled control cannot write through the binding');
      Check(TNyxCodec.Encode(LDocument) = LExpected, 'all input leaves the exact authored design intact');
      WriteLn('PASS ', LCount, ' compiled semantic native control checks');
    except
      on LException: Exception do
      begin
        WriteLn('FAIL ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
      end;
    end;
  finally
    LOtherRenderer.Free;
    LRenderer.Free;
    LOtherHost.Free;
    LHost.Free;
    LDocument.Free;
  end;
end.
