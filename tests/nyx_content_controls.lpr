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
program nyx_content_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Forms, Controls, StdCtrls,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.content,
  nyx.state, nyx.behavior, nyx.presentations, nyx.generated.view
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser
  {$else}, nyx.render.lcl{$endif};

type
  {$ifdef PAS2JS}
  TRenderer = TNyxBrowserRenderer;
  TFace = TJSHTMLElement;
  THost = TJSHTMLElement;
  {$else}
  TRenderer = TNyxLCLRenderer;
  TFace = TControl;
  THost = TForm;
  TControlAccess = class(TControl);
  {$endif}

  { This receiver owns no renderer/document references. Every mount uses the
    same borrowed receiver; retired event ports must become inert. }
  TObservation = class
  public
    Clicks: Integer;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

var
  LChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(LChecks);
end;

procedure TObservation.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin

  if AEvent.Trigger = ntClick then
  begin
    Inc(Clicks);
  end;
end;

function Face(ARenderer: TRenderer; const AID: TNyxText): TFace;
var
  LKind: TNyxKind;
begin
  { Public target lookups refuse an unmounted identity. Test branch absence
    through the realized model before asking for an actual mounted control. }

  if ARenderer.Root.Find(AID) = nil then
  begin
    Exit(nil);
  end;

  if TryNyxKind(ARenderer.Root.Find(AID).ProjectionKind, LKind) and
    (LKind in [nkInput, nkMemo]) then
  begin
    Exit(ARenderer.InputFor(AID, niRuntime));
  end;
  {$ifdef PAS2JS}Result := ARenderer.ElementFor(AID);
  {$else}Result := ARenderer.ControlFor(AID);{$endif}
end;

function TextOf(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}Result := TJSHTMLInputElement(AFace).value;
  {$else}Result := TCustomEdit(AFace).Text;{$endif}
end;

procedure WriteText(AFace: TFace; const AValue: TNyxText);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AFace).value := AValue;
  AFace.dispatchEvent(TJSEvent.new('input'));
  {$else}
  TCustomEdit(AFace).Text := AValue;
  {$endif}
end;

procedure Click(AFace: TFace);
begin
  {$ifdef PAS2JS}AFace.click;
  {$else}TControlAccess(AFace).Click;{$endif}
end;

procedure SetSize(AHost: THost; AWidth: Integer);
begin
  {$ifdef PAS2JS}
  AHost.style.setProperty('width', IntToStr(AWidth) + 'px');
  AHost.style.setProperty('height', '700px');
  {$else}
  AHost.ClientWidth := AWidth;
  AHost.ClientHeight := 700;
  Application.ProcessMessages;
  {$endif}
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LRenderer: TRenderer;
  LOtherRenderer: TRenderer;
  LStore: TNyxState;
  LOtherStore: TNyxState;
  LObservation: TObservation;
  LHost: THost;
  LOtherHost: THost;
  LInput: TFace;
  LButton: TFace;
  LRoot: TNyxNode;
  LLease: INyxPresentationView;
  LCompactRecipe: TNyxText;
  LWideInputID: TNyxText;
  LCompactInputID: TNyxText;
  LValue: TNyxText;
  LRevision: Integer;
  LIndex: Integer;
  LRejected: Boolean;
begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  LDocument := BuildNyxDocument;
  LCandidate := nil;
  LRenderer := TRenderer.Create;
  LOtherRenderer := TRenderer.Create;
  LStore := LDocument.State.Clone;
  LOtherStore := LDocument.State.Clone;
  LObservation := TObservation.Create;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  LOtherHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  document.body.appendChild(LOtherHost);
  {$else}
  LHost := TForm.Create(nil);
  LOtherHost := TForm.Create(nil);
  {$endif}
  try
    { The generated companion owns its authored defaults/bindings. These extra
      buttons only instrument callback lifetime; they are explicit typed fixture
      additions, never represented as MCP-created designer content. }
    LCompactRecipe := LDocument.Find('workspace').Content.Rule(1).Component.Name;
    LDocument.Find('wide-form').Add(NewNyxButton('wide-action').WithText('Keep creating'));
    LDocument.Find(LCompactRecipe).Add(NewNyxButton('compact-action').WithText('Save idea'));
    LWideInputID := NyxQualifiedID('workspace', 'wide-name');
    LCompactInputID := NyxQualifiedID('workspace', 'compact-notes');
    LRenderer.OnEvent := {$ifdef PAS2JS}@{$endif}LObservation.Event;
    LStore.SetValue(NyxTextState('notes'), 'One writer');
    LOtherStore.SetValue(NyxTextState('notes'), 'Another writer');
    SetSize(LHost, 900);
    SetSize(LOtherHost, 390);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, False, LStore);
    LOtherRenderer.Render(LDocument, LDocument.Pages[0], LOtherHost, False, LOtherStore);
    {$ifndef PAS2JS}
    LHost.Show;
    LOtherHost.Show;
    Application.ProcessMessages;
    {$endif}
    LInput := Face(LRenderer, LWideInputID);
    Check(LInput <> nil, 'Wide host mounts the input recipe');
    Check(Face(LRenderer, LCompactInputID) = nil, 'Wide mount creates no inactive memo');
    Check(Face(LOtherRenderer, LCompactInputID) <> nil, 'Independent narrow host mounts the alternate memo');
    Check(Face(LOtherRenderer, LWideInputID) = nil, 'Narrow mount creates no hidden wide input');
    {$ifdef PAS2JS}
    Check(LInput.tagName = 'INPUT', 'Actual wide DOM control is an input');
    Check(Face(LOtherRenderer, LCompactInputID).tagName = 'TEXTAREA', 'Actual compact DOM control is a memo');
    {$else}
    Check(LInput is TCustomEdit, 'Actual wide native control is an edit');
    Check(Face(LOtherRenderer, LCompactInputID) is TMemo, 'Actual compact native control is a memo');
    {$endif}
    Check(TextOf(LInput) = 'One writer', 'Wide binding reads its current runtime store');
    Check(TextOf(Face(LOtherRenderer, LCompactInputID)) = 'Another writer', 'Concurrent compact binding uses an independent store');
    LValue := TNyxText('An idea ') + NyxScalarText($1F680);
    WriteText(LInput, LValue);
    Check(LStore.Value(NyxTextState('notes').Name).TextValue = LValue, 'Actual input editing commits exact supplementary text');
    Check(LOtherStore.Value(NyxTextState('notes').Name).TextValue = 'Another writer', 'Input edits cannot leak into another view store');
    LRevision := LStore.Revision;
    LLease := LRenderer.Presentations;
    for LIndex := 0 to 3 do
    begin
      SetSize(LHost, 390);
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost, False, LStore);
      Check(Face(LRenderer, LWideInputID) = nil, 'Explicit narrow remount retires the old control set');
      Check(TextOf(Face(LRenderer, LCompactInputID)) = LValue, 'New memo keeps the accepted live value');
      LButton := Face(LRenderer, NyxQualifiedID('workspace', 'compact-action'));
      Click(LButton);
      Check(LObservation.Clicks = LIndex * 2 + 1, 'Selected compact callback dispatches once');
      SetSize(LHost, 900);
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost, False, LStore);
      Check(Face(LRenderer, LCompactInputID) = nil, 'Explicit wide remount creates only the selected recipe');
      Check(TextOf(Face(LRenderer, LWideInputID)) = LValue, 'Reverse remount retains current state');
      Click(Face(LRenderer, NyxQualifiedID('workspace', 'wide-action')));
      Check(LObservation.Clicks = LIndex * 2 + 2, 'Selected wide callback dispatches once');
      Check(LStore.Revision = LRevision, 'Recipe remount imports no defaults and writes no state');
    end;
    Check(not LLease.Connected, 'Explicit public remount retires its prior presentation lease');
    LStore.SetValue(NyxTextState('notes'), 'Fresh accepted value');
    Check(TextOf(Face(LRenderer, LWideInputID)) = 'Fresh accepted value', 'New mount retains ordinary external state updates');
    LRoot := LRenderer.Root;
    LInput := Face(LRenderer, LWideInputID);
    LCandidate := LDocument.Clone;
    LCandidate.Find('workspace').Content.WhenPresentation(NyxPresentation('focused')).Use(NyxComponent('missing'));
    LRejected := False;
    try
      LRenderer.Render(LCandidate, LCandidate.Pages[0], LHost, False, LStore);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LRenderer.Root = LRoot) and (Face(LRenderer, LWideInputID) = LInput),
      'Invalid inactive branch preserves the exact mounted tree and control');
    Check(LRenderer.State = LStore, 'Refused candidate cannot change runtime store ownership');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-projection-refresh', 'passed');
    document.body.setAttribute('data-projection-refresh-checks', IntToStr(LChecks));
    {$endif}
    WriteLn('PASS ', LChecks, ' actual initial content/remount checks');
  finally
    LCandidate.Free;
    LRenderer.Free;
    LOtherRenderer.Free;
    LObservation.Free;
    LDocument.Free;
    LStore.Free;
    LOtherStore.Free;
    {$ifdef PAS2JS}
    LHost.remove;
    LOtherHost.remove;
    {$else}
    LHost.Free;
    LOtherHost.Free;
    {$endif}
  end;
end;

begin
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-refresh', 'failed');
      document.body.setAttribute('data-projection-refresh-error', LException.Message);
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
