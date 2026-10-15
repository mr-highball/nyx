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

unit nyx.generated.view;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model;

function BuildNyxDocument: TNyxDocument;

implementation

uses nyx.text, nyx.types, nyx.controls, nyx.programs.types,
  nyx.programs.contract, nyx.programs.declarations;

type
  { Handwritten application recipes intentionally stay outside the managed
    builder. Their source, typed contract and limits are computed at construction;
    ordinary visual/semantic edits must retain every untouched helper byte. }
  TWorkshopPrograms = class
  public
    class function GreetingSource: TNyxText;
    class function StableSource: TNyxText;
    class function GreetingContract: TNyxProgramContract;
    class function TextContract: TNyxProgramContract;
    class function Limits: TNyxProgramLimits;
  end;

class function TWorkshopPrograms.GreetingSource: TNyxText;
begin
  Result := 'begin' + #10 +
    '  Caption := ''Hello, '' + Name + ''!'';' + #10 +
    'end.' + #10;
end;

class function TWorkshopPrograms.StableSource: TNyxText;
begin
  Result := 'begin Caption := ''Kept by a helper'' end.';
end;

class function TWorkshopPrograms.GreetingContract: TNyxProgramContract;
begin
  Result := NyxProgramContract
    .Input(NyxProgramSymbol('Name'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText));
end;

class function TWorkshopPrograms.TextContract: TNyxProgramContract;
begin
  Result := NyxProgramContract.Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText));
end;

class function TWorkshopPrograms.Limits: TNyxProgramLimits;
begin
  Result := NyxProgramLimits.Steps(1024 + 16).Nesting(16 + 8);
end;

function BuildNyxDocument: TNyxDocument;
var
  LPage: INyxPage;
  LCard: INyxCard;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Handwritten behavior workshop';
    LPage := NewNyxPage('home');
    LPage.Configure.Padding(12 + 4).Gap(6 * 2).Done;
    LPage.Add(NewNyxHeading('workshop-title').WithText(Result.Title));
    LPage.Add(NewNyxInput('name-input').Configure.Text('Your name').Value('Ada').Done);
    LPage.Add(NewNyxLabel('caption-preview').WithText('Ready for a greeting'));
    LPage.Add(NewNyxButton('run-program').WithText('Greet'));
    Result.AddPage(LPage);
    LCard := NewNyxCard('greeting-card');
    LCard.Add(NewNyxLabel('component-title').WithText('A reusable greeting'));
    LCard.Add(NewNyxMemo('component-notes').Configure.Text('Notes').Value('').Done);
    Result.AddComponent(LCard);
    LPage.Add(NewNyxComponent('card-instance').Configure.Component(NyxComponent('greeting-card')).Done);
    Result.Programs.Define(NyxProgramRef('Greeting'),
      NewNyxProgramDefinition(TWorkshopPrograms.GreetingSource,
        TWorkshopPrograms.GreetingContract, TWorkshopPrograms.Limits));
    Result.Programs.Define(NyxProgramRef('Stable helper'),
      NewNyxProgramDefinition(TWorkshopPrograms.StableSource,
        TWorkshopPrograms.TextContract, TWorkshopPrograms.Limits));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
