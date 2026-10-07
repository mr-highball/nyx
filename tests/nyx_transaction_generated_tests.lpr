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

program nyx_transaction_generated_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, {$ifdef PAS2JS}Web,{$endif}
  nyx.text, nyx.model, nyx.state, nyx.binding, nyx.composition,
  nyx.collections, nyx.generated.view;

var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LState: TNyxState;
  LText: TNyxText;
  LCount: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Compiled transaction: ' + AReason);
  end;
  Inc(LCount);
end;

begin
  LDocument := nil;
  LRoot := nil;
  LState := nil;
  try
    try
      { Compile and execute the exact admitted companion, rather than compare
        generated text only. This is shared runtime projection, not physical
        target input or an observing full Studio qualification. }
      LDocument := BuildNyxDocument;
      LText := TNyxText('A calm 🌙 desk');
      Check(LDocument.State.Value('note').TextValue = LText, 'exact supplementary default reconstruction');
      Check(LDocument.State.Value('raw-note').TextValue = LText + TNyxText(#0),
        'exact unbound NUL reconstruction');
      Check(LDocument.Find('workspace-note').Parent.ID = 'side-stack', 'interleaved layout reconstruction');
      Check(LDocument.Collections.Snapshot(NyxCollection('work-items')).ItemAt(1)
        .GetValue(NyxIntegerField('priority')) = 3, 'exact typed collection reconstruction');
      Check(LDocument.Find('work-table').CollectionView.QueryPolicy.Defined, 'query reconstruction');
      LRoot := RealizeNyxView(LDocument, LDocument.Find('home'));
      LState := LDocument.State.Clone;
      ApplyNyxBindings(LRoot, LState);
      Check(LRoot.Find('workspace-note').Prop('value') = LText, 'compiled scalar binding projection');
      LState.Apply([NyxStateValue(NyxTextState('note'), 'An independent runtime value')]);
      ApplyNyxBindings(LRoot, LState);
      Check(LRoot.Find('workspace-note').Prop('value') = 'An independent runtime value',
        'compiled binding consumes the independent runtime store');
      Check(LDocument.State.Value('note').TextValue = LText, 'authored default remains unchanged');
      {$ifdef PAS2JS}
      document.body.textContent := 'PASS ' + IntToStr(LCount) + ' compiled transaction checks';
      document.body.setAttribute('data-nyx-transaction-generated', 'passed');
      {$else}
      WriteLn('PASS ', LCount, ' compiled transaction checks');
      {$endif}
    finally
      LState.Free;
      LRoot.Free;
      LDocument.Free;
    end;
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := LException.Message;
      document.body.setAttribute('data-nyx-transaction-generated', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
