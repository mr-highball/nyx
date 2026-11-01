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
program nyx_choice_authored_application;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.model, nyx.choices, nyx.generated.view
  {$ifdef PAS2JS}, JS, Web, nyx.application.browser
  {$else}, Interfaces, Forms, StdCtrls, nyx.application.lcl{$endif};

type
  { Owns the real emitted application's mount; widgets below remain borrowed. }
  TTarget = {$ifdef PAS2JS}TNyxBrowserApplication{$else}TNyxLCLApplication{$endif};

var
  GDocument: TNyxDocument;
  GFirst: TTarget;
  GPeer: TTarget;
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Mount(ATarget: TTarget);
{$ifdef PAS2JS}
var
  LHost: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('main'));
  document.body.appendChild(LHost);
  ATarget.Run(GDocument, LHost);
  {$else}
  ATarget.Mount(GDocument);
  ATarget.Window.ClientWidth := StrToIntDef(ParamStr(1), 1280);
  ATarget.Window.ClientHeight := 900;
  ATarget.Window.Show;
  Application.ProcessMessages;
  {$endif}
end;

function Selected(ATarget: TTarget): Integer;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLSelectElement(ATarget.View.InputFor('operation')).selectedIndex;
  {$else}
  Result := TComboBox(ATarget.View.InputFor('operation')).ItemIndex;
  {$endif}
end;

procedure Pick(AIndex: Integer);
{$ifdef PAS2JS}
var
  LInput: TJSHTMLSelectElement;
{$else}
var
  LInput: TComboBox;
{$endif}
begin
  {$ifdef PAS2JS}
  LInput := TJSHTMLSelectElement(GFirst.View.InputFor('operation'));
  LInput.selectedIndex := AIndex;
  LInput.dispatchEvent(TJSEvent.new('change'));
  {$else}
  LInput := TComboBox(GFirst.View.InputFor('operation'));
  LInput.ItemIndex := AIndex;
  Check(Assigned(LInput.OnChange), 'the emitted application has a live standard input handler');
  LInput.OnChange(LInput);
  {$endif}
end;

procedure Run;
var
  LCaption: TNyxText;
begin
  GDocument := BuildNyxDocument;
  Check((GDocument.Find('operation').ChoiceItems.Count = 4) and
    (GDocument.Find('operation').ChoiceItems.Item(2).Caption = TNyxText('Built with Nyx 🌙')),
    'the independently compiled constructor retains the ordinary Inspector result');
  GFirst := TTarget.Create;
  GPeer := TTarget.Create;
  Mount(GFirst);
  Mount(GPeer);
  Check((Selected(GFirst) = 2) and (Selected(GPeer) = 2), 'both actual mounts select the exact authored key');
  {$ifdef PAS2JS}
  LCaption := TJSHTMLSelectElement(GFirst.View.InputFor('operation')).options[2].textContent;
  {$else}
  LCaption := TNyxText(RawByteString(TComboBox(GFirst.View.InputFor('operation')).Items[2]));
  {$endif}
  Check(LCaption = TNyxText('Built with Nyx 🌙'), 'the actual control paints the edited Unicode caption');
  Pick(1);
  Check(GFirst.View.Root.Find('operation').Prop('value') = 'first', 'ordinary input publishes a stable key');
  Pick(0);
  Check((Selected(GFirst) = 0) and (GFirst.View.Root.Find('operation').Prop('value') = ''),
    'the explicit empty key remains a physical selection');
  Pick(3);
  Check(GFirst.View.Root.Find('operation').Prop('value') =
    TNyxText('line') + #13#10 + #0 + TNyxText('🌙'), 'ordinary input preserves the exact special key');
  Check((Selected(GPeer) = 2) and
    (GDocument.Find('operation').Prop('value') = TNyxText('second/🌙')),
    'runtime editing does not change the peer or authored document');
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
    Run;
    Retire;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-choice-application', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' authored choice application checks';
    {$else}
    WriteLn('PASS ', GChecks, ' authored choice application checks');
    {$endif}
  except
    on LException: Exception do
    begin
      Retire;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-choice-application', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
