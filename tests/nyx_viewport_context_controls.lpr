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
program nyx_viewport_context_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifndef PAS2JS}Interfaces, Classes, Forms, Controls, ExtCtrls, StdCtrls,
    nyx.editing.lcl,{$else}JS, Web, nyx.editing.browser,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.behavior, nyx.content, nyx.presentations, nyx.editing, nyx.contract,
  nyx.scheduler, nyx.viewport.context, nyx.view.sections, nyx.responsive, nyx.containers,
  nyx.publication, nyx.projection.refresh, nyx.data
  {$ifdef PAS2JS}, nyx.render.browser, nyx.view.sections.browser
  {$else}, nyx.render.lcl, nyx.view.sections.lcl{$endif};

type
  {$ifdef PAS2JS}
  { Ordinary browser renderer; fixture code uses no private adapter hooks. }
  TRenderer = TNyxBrowserRenderer;
  { Managed section whose physical renderer remains borrowed until replacement. }
  ISection = INyxBrowserViewSection;
  { Attached DOM allocation, owned by the fixture until all views are retired. }
  THost = TJSHTMLElement;
  { Borrowed actual input face, never retained across a structural replacement. }
  TFace = TJSHTMLElement;
  {$else}
  { Ordinary native renderer with the same presentation contract as the browser. }
  TRenderer = TNyxLCLRenderer;
  { Managed native section; its slot outlives pending and accepted publications. }
  ISection = INyxLCLViewSection;
  { Fixture-owned native allocation with independent enclosing/child widths. }
  THost = TPanel;
  { Borrowed native field; identity and HWND are checked only while it is live. }
  TFace = TWinControl;
  { Capture an unhandled widget exception before LCL opens an error dialog.
    This owned receiver outlives all fixture windows; any delivery fails the
    next settling boundary, preserving the original message and stack. }
  TWindowErrors = class
  public
    Failure: TNyxText;
    procedure Handle(Sender: TObject; AException: Exception);
  end;
  {$endif}

var
  GChecks: Integer;
  {$ifndef PAS2JS}GWindowErrors: TWindowErrors;{$endif}

{$ifndef PAS2JS}
procedure TWindowErrors.Handle(Sender: TObject; AException: Exception);
begin
  WriteLn('Unhandled native widget exception / ', AException.ClassName, ' / ', AException.Message);
  DumpExceptionBackTrace(Output);

  if Failure = '' then
  begin
    Failure := TNyxText(AException.ClassName) + ' / ' + TNyxText(AException.Message);
  end;
end;
{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
  WriteLn('PASS ', GChecks, ' / ', AReason);
end;

{$ifdef PAS2JS}
procedure Pause(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(True);
    end, 15);
end;
{$endif}

{ Service real layout and scheduler queues on both targets. This is a bounded
  settling window, followed by explicit state/face assertions; no private
  viewport setter or global browser-dimension override substitutes for resize. }
procedure Settle; {$ifdef PAS2JS}async;{$endif}
var
  LTurn: Integer;
begin
  for LTurn := 1 to 30 do
  begin
    {$ifdef PAS2JS}
    await(TJSPromise.resolve(TJSPromise.new(@Pause)));
    {$else}
    Application.ProcessMessages;
    CheckSynchronize(10);

    if GWindowErrors.Failure <> '' then
    begin
      raise Exception.Create(GWindowErrors.Failure);
    end;
    {$endif}
  end;
end;

procedure Width(AHost: THost; AWidth: Integer);
begin
  {$ifdef PAS2JS}AHost.style.setProperty('width', IntToStr(AWidth) + 'px');
  {$else}AHost.ClientWidth := AWidth;{$endif}
end;

function Field(ARenderer: TRenderer; const AID: TNyxText): TFace;
begin
  Result := TFace(ARenderer.InputFor(AID, niRuntime));
end;

procedure Draft(AFace: TFace; const AText: TNyxText; ANotify: Boolean);
begin
  {$ifdef PAS2JS}
  TJSHTMLInputElement(AFace).value := AText;

  if ANotify then
  begin
    AFace.dispatchEvent(TJSEvent.new('input'));
  end;
  {$else}
  TCustomEdit(AFace).Text := AText;
  {$endif}
end;

function TextOf(AFace: TFace): TNyxText;
begin
  {$ifdef PAS2JS}Result := NyxBrowserInputText(AFace);
  {$else}Result := NyxLCLInputText(AFace);{$endif}
end;

procedure Focus(AFace: TFace);
begin
  {$ifdef PAS2JS}AFace.focus;{$else}AFace.SetFocus;{$endif}
end;

function Focused(AFace: TFace): Boolean;
begin
  {$ifdef PAS2JS}Result := document.activeElement = AFace;
  {$else}Result := Screen.ActiveControl = AFace;{$endif}
end;

{ Each alternative deliberately changes field IDs but shares semantic parts.
  The normal structural-presentation pipeline must transfer text, numeric draft,
  focus and scalar selection; it may not keep the old DOM/native tree hidden. }
function ChildDocument: TNyxDocument;
var
  LPage: INyxPage;
  LBody: INyxColumn;
  LInput: INyxInput;
  LInstance: INyxComponent;
  LPrefix: TNyxText;
  LCompact: Boolean;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxPage('child');
    LPage.Configure.Layout(nlGrid).Columns(1).Padding(0)
      .Extension('viewport-form-context', 'one')
      .QueryContainer(NyxContainer('child space')).Containment(nccWidth)
      .WhenViewport(TNyxViewportWidth.Below(640)).Columns(2).Done;
    Result.AddPage(LPage);
    Result.Presentations.Define(NyxPresentation('local narrow'),
      TNyxPresentationCondition.Within(NyxContainer('child space'),
        TNyxViewportCondition.Any.WidthBelow(300)));
    for LCompact := False to True do
    begin
      LPrefix := 'wide';

      if LCompact then
      begin
        LPrefix := 'compact';
      end;
      LBody := NewNyxColumn(LPrefix + '-body');
      LInput := NewNyxInput(LPrefix + '-notes');
      LInput.Configure.Text('Notes').Value('A starting idea')
        .PartName(NyxPart('notes')).Done;
      LBody.Add(LInput);
      LInput := NewNyxInput(LPrefix + '-estimate');
      LInput.Configure.Text('Estimate').InputType(niNumber).Value(7.0)
        .PartName(NyxPart('estimate')).Done;
      LInput.Contract.Value(NyxNumberDomain.Range(0, 100));
      LBody.Add(LInput);
      Result.AddComponent(LBody);
      LBody := NewNyxColumn('local-' + LPrefix);
      LBody.Add(NewNyxLabel(LPrefix + '-local-label').WithText('Local container: ' + LPrefix));
      Result.AddComponent(LBody);
    end;
    LInstance := NewNyxComponent('editor');
    LInstance.Content.Use(NyxComponent('wide-body'))
      .WhenViewport(TNyxViewportWidth.Below(640)).Use(NyxComponent('compact-body')).Done;
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('local');
    LInstance.Content.Use(NyxComponent('local-wide'))
      .WhenPresentation(NyxPresentation('local narrow')).Use(NyxComponent('local-compact')).Done;
    LPage.Add(LInstance);
  except
    Result.Free;
    raise;
  end;
end;

procedure Run; {$ifdef PAS2JS}async;{$endif}
var
  LOuter: TRenderer;
  LGrandchild: TRenderer;
  LChild: ISection;
  LPrepared: INyxViewSectionChange;
  LOuterHost: THost;
  LChildHost: THost;
  LGrandHost: THost;
  LDocument: TNyxDocument;
  LPlain: TNyxDocument;
  LNext: TNyxDocument;
  LEffect: INyxPreparedPublication;
  LForms: TNyxProjectionFormContexts;
  LContext: INyxViewportContext;
  LFormer: INyxViewportContext;
  LRetired: INyxViewportContext;
  LRoot: TNyxNode;
  LNotes: TFace;
  LEstimate: TFace;
  LSelection: TNyxTextSelection;
  LText: TNyxText;
  LWire: TNyxText;
  LRevision: Integer;
  LRefused: Boolean;
  {$ifndef PAS2JS}
  LWindow: TForm;
  LHandle: PtrUInt;
  {$endif}
begin
  LOuter := TRenderer.Create;
  LGrandchild := TRenderer.Create;
  LChild := nil;
  LNext := nil;
  LDocument := ChildDocument;
  LPlain := TNyxDocument.Create;
  LPlain.AddPage(NewNyxPage('outer').Add(NewNyxLabel('title').WithText('A composed workspace')));
  LWire := TNyxCodec.Encode(LDocument);
  {$ifdef PAS2JS}
  LOuterHost := TJSHTMLElement(document.createElement('section'));
  LChildHost := TJSHTMLElement(document.createElement('section'));
  LGrandHost := TJSHTMLElement(document.createElement('section'));
  LOuterHost.style.cssText := 'height:620px;width:1240px;';
  LChildHost.style.cssText := 'position:absolute;top:60px;left:10px;width:240px;height:400px;';
  LGrandHost.style.cssText := 'position:absolute;top:60px;left:270px;width:180px;height:400px;';
  document.body.appendChild(LOuterHost);
  document.body.appendChild(LChildHost);
  document.body.appendChild(LGrandHost);
  {$else}
  LWindow := TForm.CreateNew(nil);
  LWindow.Caption := 'Nyx enclosing viewport qualification';
  LWindow.ClientWidth := 1280;
  LWindow.ClientHeight := 700;
  LOuterHost := TPanel.Create(LWindow);
  LOuterHost.Parent := LWindow;
  LOuterHost.BevelOuter := bvNone;
  LOuterHost.SetBounds(0, 0, 1240, 620);
  LChildHost := TPanel.Create(LWindow);
  LChildHost.Parent := LWindow;
  LChildHost.BevelOuter := bvNone;
  LChildHost.SetBounds(10, 60, 240, 400);
  LGrandHost := TPanel.Create(LWindow);
  LGrandHost.Parent := LWindow;
  LGrandHost.BevelOuter := bvNone;
  LGrandHost.SetBounds(270, 60, 180, 400);
  LWindow.Show;
  {$endif}
  try
    LOuter.Render(LPlain, LPlain.Pages[0], LOuterHost);
    LContext := LOuter.ViewportContext;
    Check(LContext.Active and LContext.Snapshot.Defined and
      (LContext.Snapshot.Width = 1240), 'ordinary rule-free outer renderer exports its actual allocation');
    {$ifdef PAS2JS}
    LChild := NewNyxBrowserViewSection(TNyxViewSectionRef.Named('child'), LChildHost,
      nil, nil, nil, LContext);
    {$else}
    LChild := NewNyxLCLViewSection(TNyxViewSectionRef.Named('child'), LChildHost,
      nil, nil, nil, LContext);
    {$endif}
    LPrepared := LChild.Prepare(LDocument, LDocument.Pages[0]);
    Check(PublishNyxViewSections([LPrepared]), 'enclosing-context child publishes through ordinary section admission');
    LPrepared := nil;
    LFormer := LChild.Renderer.ViewportContext;
    LGrandchild.PresentationViewport := LFormer;
    LGrandchild.Render(LDocument, LDocument.Pages[0], LGrandHost);
    {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
    Check((LChild.Root.Prop('columns') = '1') and
      (LChild.Root.Find(NyxQualifiedID('editor', 'wide-notes')) <> nil),
      'narrow section uses the enclosing wide scalar and structural presentation');
    Check((LGrandchild.Root.Prop('columns') = '1') and
      (LGrandchild.ViewportContext.Snapshot.Width = 1240),
      'nested context inherits wide presentation through another fixed-width host');
    Check(LChild.Root.Find(NyxQualifiedID('local', 'compact-local-label')) <> nil,
      'explicit named container remains local while enclosing presentation is wide');
    LRoot := LChild.Root;
    LNotes := Field(LChild.Renderer, NyxQualifiedID('editor', 'wide-notes'));
    LEstimate := Field(LChild.Renderer, NyxQualifiedID('editor', 'wide-estimate'));
    LText := 'A ' + NyxScalarText($1F319) + ' idea';
    Draft(LNotes, LText, True);
    Draft(LEstimate, '12.75', False);
    Focus(LNotes);
    LChild.Renderer.SetTextSelection(NyxQualifiedID('editor', 'wide-notes'),
      NyxTextSelection(LText, 2, 3, ntdUnknown));
    LSelection := LChild.Renderer.TextSelectionFor(NyxQualifiedID('editor', 'wide-notes'));
    {$ifndef PAS2JS}LHandle := LNotes.Handle;{$endif}
    Check((TextOf(LEstimate) = '12.75') and
      (LChild.Root.Find(NyxQualifiedID('editor', 'wide-estimate')).Prop('value') = '7'),
      'physical number draft remains independent of accepted model value');
    Width(LOuterHost, 1100);
    {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
    Check((LContext.Snapshot.Width = 1100) and (LFormer.Snapshot.Width = 1100) and
      (LGrandchild.ViewportContext.Snapshot.Width = 1100),
      'actual outer resize reaches all nested consumers without local-host resize');
    Check((LChild.Root = LRoot) and
      (Field(LChild.Renderer, NyxQualifiedID('editor', 'wide-notes')) = LNotes) and
      (TextOf(LEstimate) = '12.75') and Focused(LNotes) and
      LChild.Renderer.TextSelectionFor(NyxQualifiedID('editor', 'wide-notes')).SameRange(LSelection),
      'same-recipe resize retains exact input, draft, focus and supplementary range');
    {$ifndef PAS2JS}
    Check(PtrUInt(LNotes.Handle) = LHandle, 'same-recipe resize retains the actual native input window');
    {$endif}
    LNext := LDocument.Clone;
    LNext.Pages[0].Configure.Extension('viewport-form-context', 'two').Done;
    SetLength(LForms, 1);
    LForms[0] := TNyxProjectionFormContext.ForForm('child', 'child',
      [NyxExtension('viewport-form-context')]);
    LEffect := LChild.Renderer.TryPrepareFormRefresh(LNext, LNext.Pages[0], LForms);
    Check(LEffect <> nil, 'retained form uses the same enclosing viewport during preparation');
    Width(LOuterHost, 1050);
    {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
    LRefused := False;
    try
      PublishNyxGroup([LEffect]);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    LEffect.Retire;
    LEffect := nil;
    FreeAndNil(LNext);
    Check(LRefused and (LChild.Root = LRoot) and
      (LChild.Root.Prop('viewport-form-context') = 'one') and
      (TextOf(LEstimate) = '12.75') and (TextOf(LNotes) = LText),
      'outer-only resize refuses retained-form publication and restores exact independent drafts');
    LRevision := LChild.Revision;
    LPrepared := LChild.Prepare(LDocument, LDocument.Pages[0]);
    Width(LOuterHost, 1000);
    {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
    LRefused := not PublishNyxViewSections([LPrepared]);
    Check(LRefused and (LChild.Revision = LRevision) and (LChild.Root = LRoot) and
      (TextOf(LEstimate) = '12.75'),
      'outer-only resize refuses stale prepared section and preserves its accepted draft');
    LPrepared := nil;
    Width(LOuterHost, 390);
    {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
    Check((LChild.Root.Prop('columns') = '2') and
      (LChild.Root.Find(NyxQualifiedID('editor', 'compact-notes')) <> nil) and
      (LGrandchild.Root.Find(NyxQualifiedID('editor', 'compact-notes')) <> nil),
      'compact outer presentation changes scalar rules and both nested recipes / columns=' +
      LChild.Root.Prop('columns') + ' / child=' + LChild.Renderer.LastContentError +
      ' / nested=' + LGrandchild.LastContentError + ' / viewport=' +
      LChild.Renderer.PresentationViewportFailure);
    Check((TextOf(Field(LChild.Renderer, NyxQualifiedID('editor', 'compact-notes'))) = LText) and
      (TextOf(Field(LChild.Renderer, NyxQualifiedID('editor', 'compact-estimate'))) = '12.75') and
      Focused(Field(LChild.Renderer, NyxQualifiedID('editor', 'compact-notes'))) and
      LChild.Renderer.TextSelectionFor(NyxQualifiedID('editor', 'compact-notes')).SameRange(LSelection),
      'enclosing-context structural resize transfers exact text, draft, focus and Unicode selection');
    Check(LChild.Root.Find(NyxQualifiedID('local', 'compact-local-label')) <> nil,
      'local named container stays compact across the outer breakpoint');
    LGrandchild.PresentationViewport := nil;
    LPrepared := LChild.Prepare(LDocument, LDocument.Pages[0]);
    Check(PublishNyxViewSections([LPrepared]), 'explicit replacement admits a new child renderer');
    LPrepared := nil;
    Check(not LFormer.Active, 'replacement retires the former renderer output even when readers retain its port');
    LGrandchild.PresentationViewport := LChild.Renderer.ViewportContext;
    Width(LOuterHost, 1240);
    {$ifdef PAS2JS}await(Settle);{$else}Settle;{$endif}
    Check((LChild.Root.Prop('columns') = '1') and (LGrandchild.Root.Prop('columns') = '1') and
      (LFormer.Snapshot.Width = 390) and not LFormer.Active,
      'retired source remains inert while rebound successor returns to wide presentation');
    LRetired := LGrandchild.ViewportContext;
    LGrandchild.Unmount;
    Check(not LRetired.Active, 'explicit unmount retires an exported nested viewport');
    LGrandchild.Render(LDocument, LDocument.Pages[0], LGrandHost);
    Check(LGrandchild.ViewportContext.Active and
      (LGrandchild.ViewportContext <> LRetired), 'remount supplies a fresh live viewport port');
    Check(TNyxCodec.Encode(LDocument) = LWire,
      'host inheritance, resize and replacement leave authored persistence exact');
    Check((LChild.Renderer.PresentationViewportFailure = '') and
      (LGrandchild.PresentationViewportFailure = ''), 'both adapter controllers finish without deferred viewport failure');
    WriteLn('PASS ', GChecks, ' actual enclosing viewport checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-test-result', 'passed');
    document.body.setAttribute('data-viewport-checks', IntToStr(GChecks));
    {$endif}
  finally
    if LEffect <> nil then
    begin
      LEffect.Retire;
    end;
    LEffect := nil;
    LPrepared := nil;
    LGrandchild.Free;
    LChild := nil;
    LOuter.Free;
    LDocument.Free;
    LPlain.Free;
    LNext.Free;
    {$ifdef PAS2JS}
    LGrandHost.remove;
    LChildHost.remove;
    LOuterHost.remove;
    {$else}
    LWindow.Free;
    {$endif}
  end;
end;

{$ifdef PAS2JS}
procedure Start; async;
begin
  try
    await(Run);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-test-result', 'failed');
      document.body.setAttribute('data-viewport-error', LException.Message);
    end;
  end;
end;
{$endif}

begin
  {$ifdef PAS2JS}Start;{$else}
  Application.Initialize;
  GWindowErrors := TWindowErrors.Create;
  Application.OnException := GWindowErrors.Handle;
  try
    Run;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  Application.OnException := nil;
  GWindowErrors.Free;
  {$endif}
end.
