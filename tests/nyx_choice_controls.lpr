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
program nyx_choice_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.types, nyx.model, nyx.controls,
  nyx.choices, nyx.select.configuration, nyx.state, nyx.binding.types
  {$ifdef NYX_CHOICE_COMPILED}, nyx.fixture.choices{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Forms, StdCtrls, nyx.application.lcl{$endif};

type
  { Actual independently mounted application. Platform widgets remain borrowed
    through its ordinary View inspection facet and retire with this owner. }
  TTestApplication = {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};

const
  CSecond: TNyxText = 'second/🌙';
  CCaption: TNyxText = 'Same caption 🌙';

var
  GDocument: TNyxDocument;
  GFirst: TTestApplication;
  GPeer: TTestApplication;
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Selected(AApplication: TTestApplication): Integer;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLSelectElement(AApplication.View.InputFor('operation')).selectedIndex;
  {$else}
  Result := TComboBox(AApplication.View.InputFor('operation')).ItemIndex;
  {$endif}
end;

function Caption(AApplication: TTestApplication; AIndex: Integer): TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLSelectElement(AApplication.View.InputFor('operation')).options[AIndex].textContent;
  {$else}
  Result := TNyxText(RawByteString(TComboBox(AApplication.View.InputFor('operation')).Items[AIndex]));
  {$endif}
end;

procedure Pick(AIndex: Integer);
{$ifdef PAS2JS}
var
  LChoice: TJSHTMLSelectElement;
{$else}
var
  LChoice: TComboBox;
{$endif}
begin
  {$ifdef PAS2JS}
  LChoice := TJSHTMLSelectElement(GFirst.View.InputFor('operation'));
  LChoice.selectedIndex := AIndex;
  LChoice.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LChoice := TComboBox(GFirst.View.InputFor('operation'));
  LChoice.ItemIndex := AIndex;
  Check(Assigned(LChoice.OnChange), 'ordinary native select change handler is installed');
  LChoice.OnChange(LChoice);
  {$endif}
end;

{$ifdef NYX_CHOICE_COMPILED}
function BuildDocument: TNyxDocument;
begin
  Result := nyx.fixture.choices.BuildNyxDocument;
end;
{$else}
function BuildDocument: TNyxDocument;
var
  LPage: INyxPage;
  LSelect: INyxSelect;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Friendly choices';
    Result.State.SetValue(NyxTextState('operation'), CSecond);
    LSelect := NewNyxSelect('operation').WithText('Choose an operation');
    LSelect.Configure.Items('Legacy first' + #10 + 'Legacy second').Done;
    LSelect.Binds.Value(NyxTextState('operation'), bdTwoWay).Done;
    NyxSelectConfigure(LSelect).Choices(NewNyxChoices
      .WithChoice(NyxChoice(''), 'None')
      .WithChoice(NyxChoice('first'), CCaption)
      .WithChoice(NyxChoice(CSecond), CCaption));
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('Stable values, friendly captions'));
    LPage.Add(LSelect);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;
{$endif}

procedure Mount(AApplication: TTestApplication);
{$ifdef PAS2JS}
var
  LHost: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  AApplication.Run(GDocument, LHost);
  {$else}
  AApplication.Mount(GDocument);
  AApplication.Window.ClientWidth := StrToIntDef(ParamStr(1), 1280);
  AApplication.Window.ClientHeight := 900;
  AApplication.Window.Show;
  Application.ProcessMessages;
  {$endif}
end;

procedure Cases;
var
  LNode: TNyxNode;
  LRevision: Integer;
  {$ifdef PAS2JS}
  LInput: TJSHTMLElement;
  {$else}
  LInput: TObject;
  {$endif}
  LChoices: INyxChoiceItems;
begin
  GDocument := BuildDocument;
  GFirst := TTestApplication.Create;
  GPeer := TTestApplication.Create;
  Mount(GFirst);
  Mount(GPeer);
  Check((Selected(GFirst) = 2) and (Selected(GPeer) = 2),
    'both actual controls select a stable key independent from duplicate captions');
  Check((Caption(GFirst, 1) = CCaption) and (Caption(GFirst, 2) = CCaption),
    'both distinct choices paint the exact same Unicode caption');
  Pick(1);
  Check(GFirst.State.Value('operation').TextValue = 'first',
    'the first duplicate caption publishes its distinct first key');
  Pick(2);
  Check(GFirst.State.Value('operation').TextValue = CSecond,
    'the second duplicate caption publishes its exact Unicode key');
  LNode := GFirst.View.Root.Find('operation');
  LInput := GFirst.View.InputFor('operation');
  LRevision := GFirst.State.Revision;
  LNode.SetChoiceItems(NewNyxChoices
    .WithChoice(NyxChoice(CSecond), 'Renamed operation 🌙')
    .WithChoice(NyxChoice(''), 'None')
    .WithChoice(NyxChoice('first'), 'Another friendly caption'));
  GFirst.View.Sync;
  Check((Selected(GFirst) = 0) and
    (Caption(GFirst, 0) = TNyxText('Renamed operation 🌙')),
    'caption and order updates keep the physical selection on its stable key');
  Check((GFirst.View.InputFor('operation') = LInput) and
    (GFirst.State.Revision = LRevision),
    'choice refresh keeps the actual input and creates no value publication');
  GFirst.State.SetValue(NyxTextState('operation'), 'first');
  GFirst.View.Sync;
  Check(Selected(GFirst) = 2, 'programmatic keys select by key instead of caption or old index');
  GFirst.State.SetValue(NyxTextState('operation'), 'unknown');
  GFirst.View.Sync;
  Check((Selected(GFirst) = -1) and
    (GFirst.State.Value('operation').TextValue = 'unknown'),
    'unknown values remain unselected without an invented first choice');
  LRevision := GFirst.State.Revision;
  LChoices := NewNyxChoices.WithChoice(NyxChoice(''), 'An explicit empty value')
    .WithChoice(NyxChoice(CSecond), 'Second');
  LNode.SetChoiceItems(LChoices);
  GFirst.View.Sync;
  Check((Selected(GFirst) = -1) and (GFirst.State.Revision = LRevision),
    'a missing physical choice is distinct from selecting the explicit empty key');
  GFirst.State.SetValue(NyxTextState('operation'), '');
  GFirst.View.Sync;
  Check(Selected(GFirst) = 0, 'explicit programmatic empty keys select their own row');
  LNode.SetChoiceItems(NewNyxChoices);
  GFirst.View.Sync;
  Check((Selected(GFirst) = -1) and (GFirst.State.Value('operation').TextValue = ''),
    'an empty set clears physical options while retaining the accepted value');
  LNode.RemoveChoiceItems;
  GFirst.View.Sync;
  GFirst.State.SetValue(NyxTextState('operation'), 'Legacy second');
  GFirst.View.Sync;
  Check((Selected(GFirst) = 1) and (Caption(GFirst, 1) = 'Legacy second'),
    'removing the typed declaration restores the unchanged legacy Items contract');
  Pick(0);
  Check(GFirst.State.Value('operation').TextValue = 'Legacy first',
    'legacy caption-valued input remains compatible after typed removal');
  Check((Selected(GPeer) = 2) and (Caption(GPeer, 2) = CCaption) and
    (GPeer.State.Value('operation').TextValue = CSecond),
    'physical choices, values and replacements remain independent across applications');
  Check(GDocument.Find('operation').ChoiceItems.Count = 3,
    'runtime choice replacement never edits the authored document');
end;

procedure Retire;
begin
  FreeAndNil(GFirst);
  FreeAndNil(GPeer);
  FreeAndNil(GDocument);
end;

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  try
    Cases;
    Retire;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-choice-controls', 'passed');
    document.body.setAttribute('data-test-checks', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' choice control checks';
    {$else}
    WriteLn('PASS ', GChecks, ' choice control checks');
    {$endif}
  except
    on E: Exception do
    begin
      Retire;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-choice-controls', 'failed');
      document.body.textContent := 'FAIL after ' + IntToStr(GChecks) + ': ' + E.Message;
      {$else}
      WriteLn('FAIL after ', GChecks, ': ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
