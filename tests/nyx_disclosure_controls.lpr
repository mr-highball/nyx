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

program nyx_disclosure_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.disclosure,
  nyx.state, nyx.behavior, nyx.codec, nyx.codegen, nyx.source, nyx.interaction,
  nyx.events, nyx.scheduler
  {$ifdef NYX_COMPILED_DISCLOSURE}, nyx.generated.disclosure{$endif}
  {$ifdef PAS2JS}, JS, Web, nyx.render.browser, nyx.theme
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.render.lcl{$endif};

type
  { The mounted fixture owns this callback subscription. Its borrowed renderer
    is used only before explicit cancellation or browser review-tab retirement. }
  TCloseFromKeyboard = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  { A focus callback may retire the entire view. The state used for this phase
    has an independent fixture owner so notification can finish after retirement. }
  TRetireFromFocus = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  { The caller may deliberately focus another control from its exit callback. }
  TRedirectFromExit = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifndef PAS2JS}TButtonAccess = class(TButton);{$endif}

var
  LDocument: TNyxDocument;
  LFold: INyxDisclosure;
  LInner: INyxDisclosure;
  LInstance: INyxComponent;
  LRoot: TNyxNode;
  LBody: TNyxNode;
  LHeader: TNyxNode;
  LMemo: TNyxNode;
  LNestedInput: TNyxNode;
  LNestedHeader: TNyxNode;
  LBefore: TNyxText;
  LGenerated: TNyxText;
  LWorkspace: TNyxSourceWorkspace;
  LCandidate: TNyxDocument;
  LDispatch: TNyxDispatch;
  LRefused: Boolean;
  {$ifndef PAS2JS}LStream: TFileStream;{$endif}
  LChecks: Integer;
  LKeyboardCallback: INyxEventCallback;
  LKeyboardSubscription: INyxEventSubscription;
  LFocusCallback: INyxEventCallback;
  LFocusSubscription: INyxEventSubscription;
  LExitSubscription: INyxEventSubscription;
  LExternalState: TNyxState;
  LRetired: Integer;
  LFocusSequence: TNyxText;
  LRedirected: Integer;
  {$ifdef PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LMemoFace: TJSHTMLTextAreaElement;
  LStyle: TJSHTMLElement;
  LTheme: TNyxTheme;
  {$else}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LMemoFace: TMemo;
  {$endif}

procedure TCloseFromKeyboard.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if AEvent.HasKeyboard and (AEvent.Keyboard.Key = nkF2Key) and
    (nmControl in AEvent.Keyboard.Modifiers) then
  begin
    NyxEventResponse(AExecution).Consume;
    LRenderer.State.SetValue(NyxBooleanState('first.open'), False);
  end;
end;

procedure TRetireFromFocus.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin

  if AEvent.Trigger = ntAfterExit then
  begin
    LFocusSequence := LFocusSequence + 'exit/';
  end
  else
  begin
    LFocusSequence := LFocusSequence + 'enter/';
    Inc(LRetired);
    FreeAndNil(LRenderer);
  end;
end;

procedure TRedirectFromExit.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
var
  LSecond: TNyxNode;
begin
  Inc(LRedirected);
  LSecond := LRenderer.Root.Find('second/settings').Part(NyxPart('header'));
  {$ifdef PAS2JS}
  LRenderer.ElementFor(LSecond.ID).focus;
  {$else}
  TWinControl(LRenderer.ControlFor(LSecond.ID)).SetFocus;
  {$endif}
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Disclosure controls: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure ClickHeader;
begin
  {$ifdef PAS2JS}
  LRenderer.ElementFor(LHeader.ID).click;
  {$else}
  TButtonAccess(LRenderer.ControlFor(LHeader.ID)).Click;
  {$endif}
end;

function BodyVisible: Boolean;
begin
  {$ifdef PAS2JS}
  Result := LRenderer.ElementFor(LBody.ID).style.getPropertyValue('display') <> 'none';
  {$else}
  Result := LRenderer.ControlFor(LBody.ID).Visible;
  {$endif}
end;

begin
  { Conditional browser review registration still has an explicit native
    cleanup baseline. This keeps both compiler dataflow and ownership clear. }
  LKeyboardCallback := nil;
  LKeyboardSubscription := nil;
  LFocusCallback := nil;
  LFocusSubscription := nil;
  LExitSubscription := nil;
  try
    {$ifndef PAS2JS}Application.Initialize;{$endif}
    LDocument := TNyxDocument.Create;
    LDocument.State.SetValue(NyxBooleanState('first.open'), False)
      .SetValue(NyxBooleanState('second.open'), True)
      .SetValue(NyxBooleanState('first.readonly'), False)
      .SetValue(NyxBooleanState('body.allowed'), True)
      .SetValue(NyxBooleanState('inner.open'), True);
    LFold := NewNyxDisclosure('settings');
    LFold.WithText('Advanced settings');
    LFold.Part(NyxPart('body')).Binds.Visible(NyxBooleanState('body.allowed')).Done;
    LFold.Part(NyxPart('body')).Add(NewNyxMemo('notes').Configure
      .PartName(NyxPart('notes')).Text('Notes').Value('A retained draft').Height(160).Done);
    LInner := NewNyxDisclosure('extra');
    LInner.Configure.PartName(NyxPart('extra')).Text('More details').Value(True).Done;
    LInner.Binds.Value(NyxBooleanState('inner.open')).Done;
    LInner.BodyColumn.Add(NewNyxInput('detail').Configure
      .PartName(NyxPart('detail')).Text('Detail').Done);
    LFold.BodyColumn.Add(LInner);
    LInner := nil;
    LDocument.AddComponent(LFold);
    LFold := nil;
    LDocument.AddPage(NewNyxPage('home').Configure.Layout(nlColumn).Gap(12).Done);
    LInstance := NewNyxComponent('first');
    LInstance.Configure.Component(NyxComponent('settings')).Done;
    LInstance.Binds.Value(NyxBooleanState('first.open')).ReadOnly(NyxBooleanState('first.readonly')).Done;
    LDocument.Pages[0].Add(LInstance);
    LInstance := NewNyxComponent('second');
    LInstance.Configure.Component(NyxComponent('settings')).Done;
    LInstance.Binds.Value(NyxBooleanState('second.open')).Done;
    LDocument.Pages[0].Add(LInstance);
    LInstance := nil;
    LBefore := TNyxCodec.Encode(LDocument);
    LGenerated := TNyxCodegen.Generate(LDocument, 'nyx.generated.disclosure');
    Check((Pos('INyxDisclosure', LGenerated) > 0) and
      (Pos('SetNyxDisclosureOptions', LGenerated) > 0), 'source uses specialized interfaces and typed policy');
    LWorkspace := TNyxSourceWorkspace.Create;
    LCandidate := LWorkspace.Candidate(LDocument, LGenerated);
    Check(TNyxCodec.Encode(LCandidate) = LBefore, 'ordinary source reconstructs the exact owned recipe');
    FreeAndNil(LCandidate);
    LRefused := False;
    try
      LCandidate := LWorkspace.Candidate(LDocument,
        StringReplace(LGenerated, '.Header(NyxPart(''header''))', '.Header(''header'')', []));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore),
      'ordinary source refuses untyped policy parts without changing the document');
    LRefused := False;
    try
      LCandidate := LWorkspace.Candidate(LDocument,
        StringReplace(LGenerated, '.Body(NyxPart(''body''))', '.Body(NyxPart(''missing''))', []));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore),
      'complete candidate admission refuses missing retained parts atomically');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      LStream := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LStream.WriteBuffer(LGenerated[1], Length(LGenerated));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
    {$ifdef NYX_COMPILED_DISCLOSURE}
    LCandidate := BuildNyxDocument;
    Check(TNyxCodec.Encode(LCandidate) = LBefore,
      'executed generated Pascal builds the exact independent document');
    LDocument.Free;
    LDocument := LCandidate;
    LCandidate := nil;
    {$endif}
    {$ifdef PAS2JS}
    LStyle := TJSHTMLElement(document.createElement('style'));
    LTheme := TNyxTheme.Create;
    try
      LStyle.textContent := LTheme.CSS;
    finally
      LTheme.Free;
    end;
    document.head.appendChild(LStyle);
    LRenderer := TNyxBrowserRenderer.Create;
    LHost := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(LHost);
    {$else}
    LRenderer := TNyxLCLRenderer.Create;
    LHost := TForm.Create(nil);
    LHost.SetBounds(0, 0, 700, 700);
    {$endif}
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    {$ifndef PAS2JS}LHost.Show; Application.ProcessMessages;{$endif}
    LRoot := LRenderer.Root.Find('first/settings');
    LBody := LRoot.Part(NyxPart('body'));
    LHeader := LRoot.Part(NyxPart('header'));
    LMemo := LRoot.Part(NyxPart('body/notes'));
    LNestedInput := LRoot.Part(NyxPart('body/extra/body/detail'));
    LNestedHeader := LRoot.Part(NyxPart('body/extra/header'));
    Check(not BodyVisible and not NyxInteractionPolicy(LMemo).CanIssueCommand,
      'closed content is hidden and cannot issue commands');
    {$ifdef PAS2JS}
    Check((LRenderer.ElementFor(LHeader.ID).textContent = 'Advanced settings') and
      (LRenderer.ElementFor(LHeader.ID).getAttribute('aria-expanded') = 'false') and
      (LRenderer.ElementFor(LHeader.ID).getAttribute('aria-controls') = LBody.ID),
      'actual header has the inherited caption and linked expansion semantics');
    LMemoFace := TJSHTMLTextAreaElement(LRenderer.InputFor(LMemo.ID));
    {$else}
    Check(TButton(LRenderer.ControlFor(LHeader.ID)).Caption = 'Advanced settings',
      'native header inherits the same caption');
    LMemoFace := TMemo(LRenderer.InputFor(LMemo.ID));
    {$endif}
    ClickHeader;
    Check(BodyVisible and LRenderer.State.GetValue(NyxBooleanState('first.open')), 'header toggles a complete two-way expansion update');
    LDispatch := DispatchNyxBehavior(LMemo, ntChange);
    Check(LDispatch.Source = LMemo, 'ordinary body input retains its event identity');
    {$ifdef PAS2JS}
    LMemoFace.value := 'Unfinished notes';
    {$else}
    LMemoFace.Text := 'Unfinished notes';
    {$endif}
    Check(NyxDisclosureExpanded(LRenderer.Root.Find('second/settings')), 'sibling expansion remains independent');
    ClickHeader;
    Check(not BodyVisible and not LRenderer.State.GetValue(NyxBooleanState('first.open')),
      'second activation closes the retained content');
    LRenderer.State.SetValue(NyxBooleanState('first.open'), True);
    Check(BodyVisible, 'programmatic state opens the same retained body');
    {$ifdef PAS2JS}LMemoFace.focus;{$else}LMemoFace.SetFocus;{$endif}
    LRenderer.State.SetValue(NyxBooleanState('first.open'), False);
    {$ifdef PAS2JS}
    Check(document.activeElement = LRenderer.ElementFor(LHeader.ID),
      'state-driven close recovers focus to its actual header');
    {$else}
    Check(TWinControl(LRenderer.ControlFor(LHeader.ID)).Focused,
      'state-driven close recovers focus to its actual header');
    {$endif}
    LRenderer.State.SetValue(NyxBooleanState('first.open'), True);
    {$ifdef PAS2JS}
    Check(document.activeElement = LRenderer.ElementFor(LHeader.ID),
      'programmatic reopen leaves the header focused');
    {$else}
    Check(TWinControl(LRenderer.ControlFor(LHeader.ID)).Focused,
      'programmatic reopen leaves the header focused');
    {$endif}
    LRenderer.State.SetValue(NyxBooleanState('body.allowed'), False);
    Check(not BodyVisible and LRenderer.State.GetValue(NyxBooleanState('first.open')),
      'caller visibility independently restricts an expanded body');
    LRenderer.State.SetValue(NyxBooleanState('body.allowed'), True);
    Check(BodyVisible, 'removing the caller restriction reveals the retained expanded body');
    {$ifdef PAS2JS}
    LRenderer.InputFor(LNestedInput.ID).focus;
    {$else}
    TWinControl(LRenderer.InputFor(LNestedInput.ID)).SetFocus;
    {$endif}
    LRenderer.State.SetValue(NyxBooleanState('inner.open'), False);
    {$ifdef PAS2JS}
    Check(document.activeElement = LRenderer.ElementFor(LNestedHeader.ID),
      'nested close recovers its own available header');
    {$else}
    Check(TWinControl(LRenderer.ControlFor(LNestedHeader.ID)).Focused,
      'nested close recovers its own available header');
    {$endif}
    LRenderer.State.SetValue(NyxBooleanState('first.open'), False);
    Check(NyxDisclosureClosingHeader(LNestedInput) = LHeader,
      'nested closed bodies resolve to the outermost available header');
    {$ifdef PAS2JS}
    Check(document.activeElement = LRenderer.ElementFor(LHeader.ID),
      'closing an outer section recovers focus from its nested header');
    {$else}
    Check(TWinControl(LRenderer.ControlFor(LHeader.ID)).Focused,
      'closing an outer section recovers focus from its nested header');
    {$endif}
    LRenderer.State.SetValue(NyxBooleanState('first.open'), True);
    LRenderer.State.SetValue(NyxBooleanState('inner.open'), True);
    {$ifdef PAS2JS}
    LRenderer.InputFor(LRenderer.Root.Find('second/settings').Part(NyxPart('body/notes')).ID).focus;
    {$else}
    TWinControl(LRenderer.InputFor(LRenderer.Root.Find('second/settings').Part(NyxPart('body/notes')).ID)).SetFocus;
    {$endif}
    LRenderer.State.SetValue(NyxBooleanState('first.open'), False);
    {$ifdef PAS2JS}
    Check(document.activeElement = LRenderer.InputFor(
      LRenderer.Root.Find('second/settings').Part(NyxPart('body/notes')).ID),
      'closing another section preserves unrelated editor focus');
    {$else}
    Check(TWinControl(LRenderer.InputFor(
      LRenderer.Root.Find('second/settings').Part(NyxPart('body/notes')).ID)).Focused,
      'closing another section preserves unrelated editor focus');
    {$endif}
    LRenderer.State.SetValue(NyxBooleanState('first.open'), True);
    {$ifdef PAS2JS}
    Check((LRenderer.InputFor(LMemo.ID) = LMemoFace) and (LMemoFace.value = 'Unfinished notes'),
      'browser retains exact memo identity and unfinished input');
    {$else}
    Check((LRenderer.InputFor(LMemo.ID) = LMemoFace) and (LMemoFace.Text = 'Unfinished notes'),
      'native retains exact memo identity and unfinished input');
    {$endif}
    LRenderer.State.SetValue(NyxBooleanState('first.readonly'), True);
    ClickHeader;
    Check(BodyVisible and LRenderer.State.GetValue(NyxBooleanState('first.open')),
      'read-only refusal preserves accepted expansion');
    {$ifdef PAS2JS}
    Check(LMemoFace.value = 'Unfinished notes', 'rejected header action preserves unrelated input');
    {$else}
    Check(LMemoFace.Text = 'Unfinished notes', 'rejected header action preserves unrelated input');
    {$endif}
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'runtime interaction retains exact authored defaults');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-disclosure', 'passed');
    document.body.setAttribute('data-nyx-disclosure-checks', IntToStr(LChecks));
    {$endif}
    WriteLn('PASS ', LChecks, ' actual disclosure checks');
    {$ifdef PAS2JS}

    if window.location.hash = '#review' then
    begin
      { Explicit browser review owns this mounted fixture until its tab closes.
        Ordinary qualification below still disposes every owner immediately. }
      LRenderer.State.SetValue(NyxBooleanState('first.readonly'), False);
      LRenderer.State.SetValue(NyxBooleanState('first.open'), False);
      LKeyboardCallback := TCloseFromKeyboard.Create;
      LKeyboardSubscription := LRenderer.Events.On(NyxControlEvents(LMemo.DesignID),
        ntKeyDown).Subscribe(LKeyboardCallback);
      document.body.setAttribute('data-disclosure-first-header', LHeader.ID);
      document.body.setAttribute('data-disclosure-first-memo', LMemo.ID);
      document.body.setAttribute('data-disclosure-second-header',
        LRenderer.Root.Find('second/settings').Part(NyxPart('header')).ID);
      document.body.setAttribute('data-nyx-disclosure-review', 'ready');
      Exit;
    end;
    {$endif}
    { This independent notification store outlives the view it will retire. }
    LExternalState := LDocument.State.Clone;
    LExternalState.SetValue(NyxBooleanState('first.open'), True);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, False, LExternalState);
    LRoot := LRenderer.Root.Find('first/settings');
    LHeader := LRoot.Part(NyxPart('header'));
    LMemo := LRoot.Part(NyxPart('body/notes'));
    {$ifdef PAS2JS}
    LRenderer.InputFor(LMemo.ID).focus;
    {$else}
    TWinControl(LRenderer.InputFor(LMemo.ID)).SetFocus;
    {$endif}
    LFocusCallback := TRedirectFromExit.Create;
    LExitSubscription := LRenderer.Events.On(NyxControlEvents(LMemo.DesignID),
      ntAfterExit).Subscribe(LFocusCallback);
    LExternalState.SetValue(NyxBooleanState('first.open'), False);
    {$ifdef PAS2JS}
    Check((LRedirected = 1) and (document.activeElement = LRenderer.ElementFor(
      LRenderer.Root.Find('second/settings').Part(NyxPart('header')).ID)),
      'the caller exit callback can redirect focus without recovery overriding it');
    {$else}
    Check((LRedirected = 1) and TWinControl(LRenderer.ControlFor(
      LRenderer.Root.Find('second/settings').Part(NyxPart('header')).ID)).Focused,
      'the caller exit callback can redirect focus without recovery overriding it');
    {$endif}
    LExitSubscription.Cancel;
    LExitSubscription := nil;
    LFocusCallback := nil;
    LExternalState.SetValue(NyxBooleanState('first.open'), True);
    {$ifdef PAS2JS}
    LRenderer.InputFor(LMemo.ID).focus;
    {$else}
    TWinControl(LRenderer.InputFor(LMemo.ID)).SetFocus;
    {$endif}
    LFocusCallback := TRetireFromFocus.Create;
    LFocusSubscription := LRenderer.Events.On(NyxControlEvents(LHeader.DesignID),
      ntAfterEnter).Subscribe(LFocusCallback);
    LExitSubscription := LRenderer.Events.On(NyxControlEvents(LMemo.DesignID),
      ntAfterExit).Subscribe(LFocusCallback);
    LExternalState.SetValue(NyxBooleanState('first.open'), False);
    Check((LRenderer = nil) and (LRetired = 1),
      'recovered focus callback can retire the renderer without a borrowed continuation');
    Check(LFocusSequence = 'exit/enter/',
      'settled close dispatches exactly one exit before one recovered enter');
    Check(not LExternalState.GetValue(NyxBooleanState('first.open')) and
      (TNyxCodec.Encode(LDocument) = LBefore),
      'focus retirement retains independently owned committed state and authored defaults');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-disclosure-checks', IntToStr(LChecks));
    {$endif}
    WriteLn('PASS ', LChecks, ' complete disclosure lifetime checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-nyx-disclosure', 'failed');
      document.body.textContent := 'FAIL ' + LException.Message;
      {$else}DumpExceptionBackTrace(Output);{$endif}
      ExitCode := 1;
    end;
  end;
  if LKeyboardSubscription <> nil then
  begin
    LKeyboardSubscription.Cancel;
  end;
  LKeyboardSubscription := nil;
  LKeyboardCallback := nil;
  if LFocusSubscription <> nil then
  begin
    LFocusSubscription.Cancel;
  end;
  LFocusSubscription := nil;
  if LExitSubscription <> nil then
  begin
    LExitSubscription.Cancel;
  end;
  LExitSubscription := nil;
  LFocusCallback := nil;
  LRenderer.Free;
  LExternalState.Free;
  {$ifndef PAS2JS}LHost.Free;{$endif}
  LWorkspace.Free;
  LCandidate.Free;
  LFold := nil;
  LInner := nil;
  LInstance := nil;
  LDocument.Free;
end.
