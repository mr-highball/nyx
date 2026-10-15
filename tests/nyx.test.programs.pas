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



unit nyx.test.programs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.programs.contract, nyx.programs.execution;

const
  { English starter source is explicit behavior data. It is interpreted only
    when the caller consciously invokes CompileNyxProgram/Run, never by a control
    property or native compiler directive. Named/nested routines remain Pascal. }
  ProgramGreetingSource: TNyxText =
    'program Greeting;' + #10 +
    '' + #10 +
    'var' + #10 +
    '  Index: Integer;' + #10 +
    '' + #10 +
    '{ Build a greeting from this invocation''s text. Body borrows only the' + #10 +
    '  current routine''s Prefix and Name; no state escapes into another run. }' + #10 +
    'function Greeting(const Name: String): String;' + #10 +
    'var' + #10 +
    '  Prefix: String;' + #10 +
    '' + #10 +
    '  function Body: String;' + #10 +
    '  begin' + #10 +
    '    Result := Prefix + Trim(Name);' + #10 +
    '  end;' + #10 +
    '' + #10 +
    'begin' + #10 +
    '  Prefix := ''Hello, '';' + #10 +
    '  Result := Body() + ''!'';' + #10 +
    'end;' + #10 +
    '' + #10 +
    'begin' + #10 +
    '  Caption := Greeting(Source);' + #10 +
    '  Count := 0;' + #10 +
    '' + #10 +
    '  for Index := 1 to Length(Source) do' + #10 +
    '  begin' + #10 +
    '    Count := Count + 1;' + #10 +
    '  end;' + #10 +
    '' + #10 +
    '  { Keep the preview useful on smaller screens as its text changes. }' + #10 +
    '  PreviewWidth := Min(320, 160 + Count * 8);' + #10 +
    'end.';

{ Independent authored application for actual target input. Runtime applications
  own separate stores; no fixture controller or host object enters this document. }
function ProgramDocument: TNyxDocument;
function GreetingContract: TNyxProgramContract;
function GreetingBindings: INyxProgramStateBindings;

implementation

uses nyx.controls, nyx.state, nyx.programs.types, nyx.callbacks;

function GreetingContract: TNyxProgramContract;
begin
  Result := NyxProgramContract
    .Input(NyxProgramSymbol('Source'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Count'), NyxProgramType(npkInteger))
    .Output(NyxProgramSymbol('PreviewWidth'), NyxProgramType(npkInteger));
end;

function GreetingBindings: INyxProgramStateBindings;
begin
  Result := NyxProgramStateBindings
    .InputState(NyxProgramSymbol('Source'), NyxTextState('name'))
    .OutputState(NyxProgramSymbol('Caption'), NyxTextState('caption'))
    .OutputState(NyxProgramSymbol('Count'), NyxIntegerState('count'))
    .OutputState(NyxProgramSymbol('PreviewWidth'), NyxIntegerState('preview-width'));
end;

function ProgramDocument: TNyxDocument;
var
  LPage: INyxPage;
  LSource: INyxMemo;
  LRun: INyxButton;
  LPreview: INyxCard;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Pascal behavior workbench';
    Result.State.SetValue(NyxTextState('name'), 'Ada');
    Result.State.SetValue(NyxTextState('source'), ProgramGreetingSource);
    Result.State.SetValue(NyxTextState('caption'), 'Run your behavior');
    Result.State.SetValue(NyxIntegerState('count'), 0);
    Result.State.SetValue(NyxIntegerState('preview-width'), 240);
    Result.State.SetValue(NyxTextState('error'), '');
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(20).Gap(12).Done;
    LPage.Add(NewNyxHeading('title').WithText('Give your interface a little Pascal'));
    LPage.Add(NewNyxLabel('intro').WithText(
      'Edit the behavior and run it. A named routine writes the greeting; a loop sizes the preview.'));
    LPage.Add(NewNyxInput('name-input').WithText('Name')
      .Binds.Value(NyxTextState('name')).Done);
    LSource := NewNyxMemo('source-memo').WithText('Pascal behavior');
    LSource.Configure.Height(230).Done;
    LSource.Binds.Value(NyxTextState('source')).Done;
    LPage.Add(LSource);
    LRun := NewNyxButton('run-button').WithText('Run behavior');
    NyxCallbacks(LRun).OnClick.Add(NyxHandler('TProgramRunHandler'), NyxCallbackID('run-behavior'));
    LPage.Add(LRun);
    LPage.Add(NewNyxLabel('error-label').Binds.Text(NyxTextState('error')).Done);
    LPreview := NewNyxCard('preview-card');
    LPreview.Configure.Padding(16).Gap(8).Done;
    LPreview.Binds.Width(NyxIntegerState('preview-width')).Done;
    LPreview.Add(NewNyxHeading('caption-label').Binds.Text(NyxTextState('caption')).Done);
    LPreview.Add(NewNyxLabel('count-title').WithText('Characters'));
    LPreview.Add(NewNyxLabel('count-label').Binds.Text(NyxIntegerState('count')).Done);
    LPage.Add(LPreview);
    Result.AddPage(LPage);
    Result.AddPage(NewNyxPage('other').Add(NewNyxHeading('away').WithText('Another page')));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
