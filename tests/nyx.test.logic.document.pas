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
unit nyx.test.logic.document;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model;

const
  LogicExample: TNyxText =
    '{"profile":{"name":"English demo","items":[1,2,3],"active":true}}';

function LogicDocument: TNyxDocument;

implementation

uses nyx.controls, nyx.types, nyx.state;

function LogicDocument: TNyxDocument;
var
  LPage: INyxPage;
  LCard: INyxCard;
  LMemo: INyxMemo;
  LPretty: INyxCode;
  LCompact: INyxLabel;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'JSON workshop';
    Result.State.SetValue(NyxTextState('source'), LogicExample);
    Result.State.SetValue(NyxTextState('pretty'), 'Run the graph to format your profile.');
    Result.State.SetValue(NyxTextState('compact'), 'No compact result yet.');
    Result.State.SetValue(NyxTextState('summary'), 'Ready');
    Result.State.SetValue(NyxTextState('guard'), '');
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('Make data useful'));
    LPage.Add(NewNyxLabel('intro').WithText(
      'Parse JSON, select a profile, format it, and combine the result through typed connections.'));
    LCard := NewNyxCard('source-card');
    LMemo := NewNyxMemo('source-memo').WithText('JSON source');
    LMemo.Configure.Height(150).Done;
    LMemo.Binds.Value(NyxTextState('source')).Done;
    LCard.Add(LMemo);
    LCard.Add(NewNyxButton('run-button').WithText('Run graph'));
    LPage.Add(LCard);
    LPretty := NewNyxCode('pretty-code');
    LPretty.Configure.Height(210).Done;
    LPretty.Binds.Text(NyxTextState('pretty')).Done;
    LPage.Add(LPretty);
    LCompact := NewNyxLabel('compact-label');
    LCompact.Binds.Text(NyxTextState('compact')).Done;
    LPage.Add(LCompact);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('other').Add(
      NewNyxHeading('retired-heading').WithText('The previous graph view is retired')));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
