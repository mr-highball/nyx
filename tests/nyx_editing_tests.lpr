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
program nyx_editing_tests;

{$mode delphi}{$H+}{$codepage utf8}
{$ifdef PAS2JS}{$modeswitch externalclass}{$endif}

uses
  SysUtils, nyx.text, nyx.types, nyx.editing, nyx.model, nyx.codec, nyx.data,
  nyx.controls, nyx.behavior, nyx.events, nyx.callbacks, nyx.scheduler, nyx.event.payload,
  nyx.schema, nyx.codegen, nyx.studio.session, nyx.viewport,
  {$ifdef NYX_COMPILED_EDITING}nyx.editing.fixture,{$endif}
  {$ifdef PAS2JS}JS, Web, nyx.editing.browser, nyx.render.browser,
  nyx.test.keyboard.browser;
  {$else}Classes, Interfaces, Forms, Controls, StdCtrls, LMessages, Windows,
  nyx.editing.lcl, nyx.render.lcl;{$endif}

type
  { One managed probe records owned values. The renderer is borrowed only during
    the mounted journey. Deferred execution retains the callback, never widgets. }
  TEditingProbe = class(TNyxEventCallback, INyxCallbackFactory)
    Counts: array[TNyxTrigger] of Integer;
    Last: array[TNyxTrigger] of TNyxEventInfo;
    CanConsume: array[TNyxTrigger] of Boolean;
    Order: TNyxText;
    Consume: TNyxTrigger;
    Navigate: TNyxTrigger;
    {$ifdef PAS2JS}Renderer: TNyxBrowserRenderer;
    {$else}Renderer: TNyxLCLRenderer;{$endif}
    procedure Reset;
    function Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;
  {$ifdef PAS2JS}
  TInputEvent = class external name 'InputEvent' (TNyxDOMInputEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  TCompositionEvent = class external name 'CompositionEvent' (TNyxDOMCompositionEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;
  {$else}
  TNativeFailure = class
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  TAccess = class(TWinControl);
  {$endif}

const
  CInitial: TNyxText = 'A🌙é漢 / initial';
  CFinal: TNyxText = '完成 🌙 / final';
  CEditingTriggers: array[0..4] of TNyxTrigger =
    (ntBeforeEdit, ntCompositionStart, ntCompositionUpdate, ntCompositionEnd,
    ntTextSelectionChange);

var
  GChecks: Integer;
  GQueued: TEditingProbe;
  GQueuedOwner: INyxEventCallback;
  GQueuedSubscription: INyxEventSubscription;
  {$ifndef PAS2JS}GNativeFailure: TNativeFailure;{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Editing: ' + AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure TNativeFailure.Failed(ASender: TObject; AException: Exception);
begin
  WriteLn(StdErr, 'FAIL native callback: ', AException.Message);
  DumpExceptionBackTrace(StdErr);
  Flush(StdErr);
  Halt(1);
end;
{$endif}

procedure TEditingProbe.Reset;
var
  LTrigger: TNyxTrigger;
begin
  for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
  begin
    Counts[LTrigger] := 0;
    CanConsume[LTrigger] := False;
  end;
  Order := '';
  Consume := ntDesignSelect;
  Navigate := ntDesignSelect;
end;

function TEditingProbe.Resolve(const AHandler: TNyxHandlerRef): INyxEventCallback;
begin
  Result := Self;
end;

procedure TEditingProbe.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Counts[AEvent.Trigger]);
  Last[AEvent.Trigger] := AEvent.Copy;
  CanConsume[AEvent.Trigger] := NyxEventResponse(AExecution).CanConsume;
  Order := Order + NyxTriggerName(AEvent.Trigger) + '|';

  if AEvent.Trigger = Consume then
  begin
    NyxEventResponse(AExecution).Consume;
  end;

  if AEvent.Trigger = Navigate then
  begin
    Renderer.Unmount;
  end;
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LMemo: INyxMemo;
  LTrigger: TNyxTrigger;
begin
  Result := TNyxDocument.Create;
  LPage := NewNyxPage('home');
  LMemo := NewNyxMemo('reply-memo').WithText('Reply');
  LMemo.Value := CInitial;
  LMemo.Configure.Height(160);
  LPage.Add(LMemo);
  Result.AddPage(LPage.Node);
  for LTrigger in CEditingTriggers do
  begin
    NyxCallbacks(LMemo.Node).On(LTrigger).Add(NyxHandler('TEditingAction'),
      NyxCallbackID('editing-' + NyxTriggerName(LTrigger)));
  end;
  NyxCallbacks(LMemo.Node).OnBeforeTextInput.Add(NyxHandler('TEditingAction'),
    NyxCallbackID('before-text'));
  NyxCallbacks(LMemo.Node).OnTextInput.Add(NyxHandler('TEditingAction'),
    NyxCallbackID('text'));
  NyxCallbacks(LMemo.Node).OnAfterTextInput.Add(NyxHandler('TEditingAction'),
    NyxCallbackID('after-text'));
end;

procedure ContractChecks;
var
  LText: TNyxText;
  LMalformed: TNyxText;
  LIntent: TNyxEditIntent;
  LParsed: TNyxEditIntent;
  LSelection: TNyxTextSelection;
  LSnapshot: TNyxEditingSnapshot;
  LCopy: TNyxEventInfo;
  LEvent: TNyxEventInfo;
  LIndex: Integer;
  LBad: Boolean;
  LLegacySchema: TNyxEventSchema;
  LSchemaCopy: TNyxEventSchema;
begin
  Check(not Default(TNyxTextSelection).Defined and
    not Default(TNyxEditingSnapshot).Defined, 'undefined is distinct from an empty caret');
  for LIntent := neiInsertText to High(TNyxEditIntent) do
  begin
    Check(TryNyxEditIntent(NyxEditIntentName(LIntent), LParsed) and (LParsed = LIntent),
      'closed standard intention: ' + NyxEditIntentName(LIntent));
  end;
  Check(Ord(High(TNyxEditIntent)) = 46, 'all 46 Input Events Level 2 intentions admitted');
  Check(not TryNyxEditIntent('InsertText', LParsed) and (LParsed = neiUnknown),
    'wire names are case-sensitive');
  Check(not TryNyxEditIntent('future-edit', LParsed) and (LParsed = neiUnknown),
    'future wire names cannot silently choose an intention');
  LText := TNyxText('A🌙é漢') + #13#10 + #0;
  Check(NyxTextScalarCount(LText) = 8, 'scalar count retains combining marks, CRLF and NUL');
  for LIndex := 0 to 8 do
  begin
    Check(NyxTextScalarOffset(LText, NyxTextUTF16Offset(LText, LIndex)) = LIndex,
      'UTF-16 conversion preserves scalar boundary ' + IntToStr(LIndex));
  end;
  Check(NyxTextUTF16Offset(LText, 2) = 3, 'supplementary scalar uses two UTF-16 units');
  LBad := False;
  try
    NyxTextScalarOffset(LText, 2);
  except
    on EArgumentException do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'surrogate-interior offset is rejected');
  LBad := False;
  try
    LSelection := NyxTextSelection(LText, 3, 2);
  except
    on EArgumentException do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'reversed selection is rejected without mutation');
  LBad := False;
  try
    LSelection := NyxTextSelection(LText, 0, 9);
  except
    on EArgumentException do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'out-of-range selection rejected');
  {$ifdef PAS2JS}LMalformed := 'A' + TJSString.fromCharCode($D800);
  {$else}LMalformed := 'A' + TNyxText(#$F0#$80#$80);{$endif}
  LBad := False;
  try
    NyxTextUTF16Offset(LMalformed, 0);
  except
    on EArgumentException do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'zero-offset conversion still validates malformed suffix');
  LBad := False;
  try
    NyxEditingSnapshot(nepObservation, neiUnknown, '', '', False,
      Default(TNyxTextSelection), False, False, LMalformed);
  except
    on EArgumentException do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'unknown diagnostic identity still requires valid Unicode');
  { Simulate an older creator filling every former field without initializing
    the new context declaration. Its managed marker must still mean absent. }
  LLegacySchema.Trigger := ntNamed;
  LLegacySchema.Name := NyxEvent('legacy');
  LLegacySchema.Payload := Default(TNyxEventPayloadSpec);
  LLegacySchema.Title := 'Legacy';
  LLegacySchema.Description := 'Creator event';
  LLegacySchema.Browser := ncCustom;
  LLegacySchema.Native := ncCustom;
  LLegacySchema.PayloadOptional := False;
  LLegacySchema.Routes := nil;
  LLegacySchema.DeclaredProducer := True;
  Check(LLegacySchema.Contexts = [], 'old creator records cannot advertise uninitialized context bits');
  LLegacySchema.Contexts := [nctxEditing];
  LSchemaCopy := LLegacySchema.Copy;
  LLegacySchema.Contexts := [];
  Check(LSchemaCopy.Contexts = [nctxEditing], 'creator context declaration copies independently');
  LSelection := NyxTextSelection(LText, 1, 2, ntdBackward);
  LSnapshot := NyxEditingSnapshot(nepBeforeEdit, neiInsertFromPaste, LText,
    '👩‍💻', True, LSelection, False, True, 'insertFromPaste');
  LEvent := Default(TNyxEventInfo);
  LEvent.Value := NyxNull;
  LEvent.HasEditing := True;
  LEvent.Editing := LSnapshot;
  LCopy := LEvent.Copy;
  LEvent := Default(TNyxEventInfo);
  LText := 'replaced';
  Check(LCopy.HasEditing and LCopy.Editing.Defined and
    (LCopy.Editing.Text = TNyxText('A🌙é漢') + #13#10 + #0) and
    (LCopy.Editing.Data = '👩‍💻') and LCopy.Editing.Selection.SameRange(LSelection),
    'retained context owns exact text, data and range');
  LSnapshot := NyxEditingSnapshot(nepBeforeEdit, neiUnknown, '', '', False,
    Default(TNyxTextSelection), False, False, 'future-edit');
  Check((LSnapshot.Intent = neiUnknown) and (LSnapshot.WireIntent = 'future-edit'),
    'unknown physical intention remains diagnostic data');
  LSnapshot := NyxEditingSnapshot(nepInput, neiDeleteContentBackward, '', '', True,
    Default(TNyxTextSelection), False, False);
  Check(LSnapshot.HasData and (LSnapshot.Data = ''), 'empty insertion data is present');
  LBad := False;
  try
    NyxEditingSnapshot(nepBeforeEdit, neiInsertCompositionText, '', '', True,
      Default(TNyxTextSelection), True, True);
  except
    on EArgumentException do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'composition cannot manufacture a physical cancellation window');
  LBad := False;
  try
    NyxEditingSnapshot(nepInput, neiInsertText, '', '', True,
      Default(TNyxTextSelection), False, True);
  except
    on EArgumentException do
    begin
      LBad := True;
    end;
  end;
  Check(LBad, 'post-edit observation cannot cancel an earlier edit');
end;

procedure AuthoringChecks;
var
  LDocument: TNyxDocument;
  LMetadata: TNyxEventSchemas;
  LIndex: Integer;
  LCount: Integer;
  LSource: TNyxText;
  LSession: TNyxStudioSession;
  LHandler: TNyxHandlerRef;
  LLine: Integer;
  LWire: TNyxText;
  LLegacySource: TNyxText;
begin
  LDocument := Fixture;
  try
    LMetadata := NyxEventsMetadata(LDocument.Pages[0].Find('reply-memo'), LDocument);
    LCount := 0;
    for LIndex := 0 to High(LMetadata) do
    begin

      if LMetadata[LIndex].Trigger in
        [ntBeforeEdit, ntCompositionStart, ntCompositionUpdate, ntCompositionEnd,
        ntTextSelectionChange] then
      begin
        Inc(LCount);
        Check(nctxEditing in LMetadata[LIndex].Contexts, 'bounded typed editing context declared');

        if LMetadata[LIndex].Trigger = ntBeforeEdit then
        begin
          Check((LMetadata[LIndex].Browser = ncAvailable) and
            (LMetadata[LIndex].Native = ncMissing), 'native beforeinput is explicitly unavailable');
        end;
      end;
    end;
    Check(LCount = 5, 'the complete editing lifecycle is discoverable');
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('INyxMemo', LSource) > 0) and
      (Pos('.OnBeforeEdit', LSource) > 0) and
      (Pos('.OnCompositionEnd', LSource) > 0) and
      (Pos('nyx.editing', LSource) > 0), 'crafted specialized source exposes typed editing handlers');
  finally
    LDocument.Free;
  end;
  LSession := TNyxStudioSession.Create;
  try
    LSession.Select('project-name');
    { Existing companions predate nyx.editing. Preserve their frame while
      authoring adds the public import needed by handwritten typed helpers. }
    LLegacySource := StringReplace(LSession.Source, '  nyx.editing,' + #10, '', [rfReplaceAll]);
    Check(Pos('nyx.editing', LLegacySource) = 0, 'legacy fixture omits the new editing import');
    LSession.SetSourceDraft(LLegacySource);
    LSession.ApplySourceDraft;
    LWire := TNyxCodec.Encode(LSession.Document);
    for LIndex := 0 to High(CEditingTriggers) do
    begin
      LHandler := LSession.AddCallback(CEditingTriggers[LIndex], LLine);
      Check((LLine > 0) and (LSession.CallbackLine(LHandler) > 0) and
        (Pos('TODO', LSession.Source) > 0), 'Studio navigates to new typed editing TODO');
    end;
    Check(Pos('nyx.editing', LSession.Source) > 0,
      'authored editing callbacks upgrade the legacy public import frame');
    LSource := LSession.Source;
    for LIndex := 0 to High(CEditingTriggers) do
    begin
      LSession.Undo;
    end;
    Check(TNyxCodec.Encode(LSession.Document) = LWire, 'undo restores exact design after lifecycle authoring');
    for LIndex := 0 to High(CEditingTriggers) do
    begin
      LSession.Redo;
    end;
    Check(LSession.Source = LSource, 'redo restores exact authored companion');
  finally
    LSession.Free;
  end;
end;

procedure ControlChecks;
var
  LDocument: TNyxDocument;
  LProbe: TEditingProbe;
  LFactory: INyxCallbackFactory;
  LRetained: TNyxEventInfo;
  LSelection: TNyxTextSelection;
  LWire: TNyxText;
  LAccepted: TNyxText;
  LViewportBefore: TNyxViewportSnapshot;
  LKeySubscription: INyxEventSubscription;
  {$ifndef PAS2JS}LBadDirection: Boolean;{$endif}
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  LInput: TJSHTMLTextAreaElement;
  LRenderer: TNyxBrowserRenderer;
  {$else}
  LHost: TForm;
  LInput: TMemo;
  LRenderer: TNyxLCLRenderer;
  {$endif}

  procedure Mount;
  begin
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    {$ifdef PAS2JS}LInput := TJSHTMLTextAreaElement(LRenderer.InputFor('reply-memo'));
    {$else}
    LInput := TMemo(LRenderer.InputFor('reply-memo'));
    LInput.HandleNeeded;
    {$endif}
  end;

  procedure Draft(const AText: TNyxText; AComposing: Boolean);
  {$ifdef PAS2JS}
  var
    LOptions: TJSObject;
  {$endif}
  begin
    {$ifdef PAS2JS}
    LInput.value := AText;
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LOptions['inputType'] := 'insertText';

    if AComposing then
    begin
      LOptions['inputType'] := 'insertCompositionText';
    end;
    LOptions['isComposing'] := AComposing;
    LOptions['data'] := AText;
    LInput.dispatchEvent(TInputEvent.new('input', LOptions));
    {$else}LInput.Text := AText;{$endif}
  end;

  procedure Composition(APhase: TNyxEditingPhase; const AData: TNyxText = '';
    ADrain: Boolean = True);
  {$ifdef PAS2JS}
  var
    LOptions: TJSObject;
    LName: String;
  {$else}
  var
    LMessage: TLMessage;
  {$endif}
  begin
    { Actual adapter producers are driven here with platform event objects or
      messages. This verifies bridging and lifetime, not a physical IME device. }
    {$ifdef PAS2JS}
    LName := 'compositionstart';

    if APhase = nepCompositionUpdate then
    begin
      LName := 'compositionupdate';
    end
    else if APhase = nepCompositionEnd then
    begin
      LName := 'compositionend';
    end;
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LOptions['data'] := AData;
    LInput.dispatchEvent(TCompositionEvent.new(LName, LOptions));
    {$else}
    LMessage := Default(TLMessage);
    LMessage.Msg := WM_IME_STARTCOMPOSITION;

    if APhase = nepCompositionUpdate then
    begin
      LMessage.Msg := WM_IME_COMPOSITION;
    end
    else if APhase = nepCompositionEnd then
    begin
      LMessage.Msg := WM_IME_ENDCOMPOSITION;
    end;
    LInput.WindowProc(LMessage);

    if (APhase = nepCompositionEnd) and ADrain then
    begin
      Application.Idle(False);
    end;
    {$endif}
  end;

  procedure Shortcut;
  {$ifndef PAS2JS}
  var
    LKey: Word;
  {$endif}
  begin
    {$ifdef PAS2JS}
    LInput.dispatchEvent(NyxTestKeyboard(ntKeyDown, 'Enter', [nmControl], False));
    {$else}
    LKey := 13;
    TAccess(LInput).OnKeyDown(LInput, LKey, [ssCtrl]);
    {$endif}
  end;

  procedure SelectionChanged;
  begin
    {$ifdef PAS2JS}LInput.dispatchEvent(TJSEvent.new('select'));
    {$else}Application.Idle(False);{$endif}
  end;

  function PhysicalText: TNyxText;
  begin
    {$ifdef PAS2JS}Result := LInput.value;
    {$else}Result := LInput.Text;{$endif}
  end;

  {$ifdef PAS2JS}
  function BeforeEdit(const AIntent: String; ACancelable, AComposing, AHasData: Boolean): Boolean;
  var
    LOptions: TJSObject;
    LEvent: TInputEvent;
  begin
    LOptions := TJSObject.new;
    LOptions['bubbles'] := True;
    LOptions['cancelable'] := ACancelable;
    LOptions['inputType'] := AIntent;
    LOptions['isComposing'] := AComposing;

    if AHasData then
    begin
      LOptions['data'] := '';
    end;
    LEvent := TInputEvent.new('beforeinput', LOptions);
    LInput.dispatchEvent(LEvent);
    Result := LEvent.defaultPrevented;
  end;
  {$endif}

begin
  {$ifdef NYX_COMPILED_EDITING}LDocument := nyx.editing.fixture.BuildNyxDocument;
  {$else}LDocument := Fixture;{$endif}
  LWire := TNyxCodec.Encode(LDocument);
  LProbe := TEditingProbe.Create;
  LFactory := LProbe;
  LProbe.Reset;
  {$ifdef PAS2JS}
  LHost := TJSHTMLElement(document.createElement('section'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$else}
  LHost := TForm.Create(nil);
  LHost.SetBounds(0, 0, 640, 480);
  LRenderer := TNyxLCLRenderer.Create;
  {$endif}
  LProbe.Renderer := LRenderer;
  try
    Mount;
    BindNyxCallbacks(LDocument, LRenderer.Events, LFactory);
    LKeySubscription := LRenderer.Events.OnKeyPress(NyxControlEvents('reply-memo'))
      .Subscribe(LProbe);
    Check(LRenderer.EditingFor('reply-memo').Text = CInitial,
      'mounted observation owns physical Unicode value');
    LSelection := NyxTextSelection(PhysicalText, 1, 2,
      {$ifdef PAS2JS}ntdBackward{$else}ntdUnknown{$endif});
    LViewportBefore := LRenderer.ViewportFor('reply-memo');
    LRenderer.SetTextSelection('reply-memo', LSelection);
    Check(LRenderer.TextSelectionFor('reply-memo').SameRange(LSelection),
      'actual platform selection uses scalar boundaries');
    Check(LRenderer.ViewportFor('reply-memo').SamePosition(LViewportBefore),
      'selection setter does not scroll the physical viewport');
    {$ifndef PAS2JS}
    LBadDirection := False;
    try
      LRenderer.SetTextSelection('reply-memo',
        NyxTextSelection(PhysicalText, 0, 1, ntdBackward));
    except
      on EArgumentException do
      begin
        LBadDirection := True;
      end;
    end;
    Check(LBadDirection and LRenderer.TextSelectionFor('reply-memo').SameRange(LSelection),
      'unsupported native active-endpoint request fails before altering the range');
    {$endif}
    SelectionChanged;
    Check((LProbe.Counts[ntTextSelectionChange] = 1) and
      LProbe.Last[ntTextSelectionChange].HasEditing and
      LProbe.Last[ntTextSelectionChange].Editing.Selection.SameRange(LSelection),
      'range notification carries the matching physical text');
    SelectionChanged;
    Check(LProbe.Counts[ntTextSelectionChange] = 1, 'duplicate selection observations coalesce');
    Check(PhysicalText = CInitial, 'selection setter does not rewrite text');
    {$ifdef PAS2JS}
    Check(document.activeElement <> LInput, 'selection setter does not steal focus');
    LProbe.Reset;
    LProbe.Consume := ntBeforeEdit;
    Check(BeforeEdit('insertFromPaste', True, False, False), 'physical paste request can cancel');
    Check(LProbe.CanConsume[ntBeforeEdit] and
      (LProbe.Last[ntBeforeEdit].Editing.Intent = neiInsertFromPaste) and
      not LProbe.Last[ntBeforeEdit].Editing.HasData, 'pre-edit cancellation carries typed nullable data');
    Check(not BeforeEdit('historyUndo', False, False, True) and
      not LProbe.CanConsume[ntBeforeEdit] and LProbe.Last[ntBeforeEdit].Editing.HasData,
      'noncancelable history request remains an observation');
    Check(not BeforeEdit('insertCompositionText', True, True, True) and
      not LProbe.CanConsume[ntBeforeEdit], 'composition draft cannot consume its platform operation');
    LProbe.Reset;
    {$endif}
    Composition(nepCompositionStart);
    Shortcut;
    Check(LProbe.Counts[ntKeyPress] = 0, 'ordinary shortcut callbacks bypass an active IME');
    Check((LProbe.Counts[ntCompositionStart] = 1) and
      LProbe.Last[ntCompositionStart].Editing.Composing and
      not LProbe.CanConsume[ntCompositionStart], 'start establishes a signal-only IME context');
    Draft('中🌙 / draft', True);
    Composition(nepCompositionUpdate, '中🌙');
    Check((LProbe.Counts[ntChange] = 0) and (LProbe.Counts[ntTextInput] = 0) and
      (LRenderer.Root.Find('reply-memo').Prop('value') = CInitial),
      'IME draft has no premature accepted edit');
    Check(LRenderer.EditingFor('reply-memo').Composing and
      (LRenderer.EditingFor('reply-memo').Text = '中🌙 / draft'),
      'physical draft is queryable independently of accepted state');
    LRenderer.Sync;
    Check(PhysicalText = '中🌙 / draft', 'state refresh cannot overwrite an active composition');
    Check((LProbe.Counts[ntCompositionUpdate] = 1) and
      LProbe.Last[ntCompositionUpdate].Editing.Composing,
      'composition update preserves owned context');
    {$ifdef PAS2JS}
    Draft(CFinal, True);
    Composition(nepCompositionEnd, CFinal);
    {$else}
    Composition(nepCompositionEnd, '', False);
    Draft(CFinal, True);
    Check(LProbe.Counts[ntTextInput] = 0,
      'late native committed characters remain guarded until the UI drain');
    Application.Idle(False);
    {$endif}
    Check((LRenderer.Root.Find('reply-memo').Prop('value') = CFinal) and
      (LProbe.Counts[ntTextInput] = 1) and (LProbe.Counts[ntAfterTextInput] = 1),
      'final IME result is admitted exactly once');
    Check((LProbe.Counts[ntCompositionEnd] = 1) and
      not LProbe.Last[ntCompositionEnd].Editing.Composing and
      (LProbe.Last[ntCompositionEnd].Editing.Text = CFinal),
      'end owns the physical result after accepted admission');
    Check((LProbe.Last[ntTextInput].Editing.Phase = nepInput) and
      (LProbe.Last[ntTextInput].Editing.Intent = neiInsertCompositionText),
      'accepted edit retains composition intent');
    Shortcut;
    Check(LProbe.Counts[ntKeyPress] = 1, 'shortcuts resume after completed IME admission');
    LRetained := LProbe.Last[ntCompositionUpdate].Copy;
    LProbe.Reset;
    Draft(CFinal, False);
    Check(LProbe.Counts[ntTextInput] = 0, 'post-end duplicate input has no second admission');
    LProbe.Consume := ntBeforeTextInput;
    Composition(nepCompositionStart);
    Draft('拒否 🌙', True);
    Composition(nepCompositionEnd, '拒否 🌙');
    Check((PhysicalText = CFinal) and
      (LRenderer.Root.Find('reply-memo').Prop('value') = CFinal) and
      (LProbe.Counts[ntTextInput] = 0) and
      LProbe.Last[ntAfterTextInput].DefaultPrevented,
      'final model refusal restores accepted text without admitting the draft');
    Check((LProbe.Last[ntCompositionEnd].Editing.Text = '拒否 🌙') and
      not LProbe.Last[ntCompositionEnd].Editing.CanCancel,
      'refused final result remains an honest physical observation');
    LProbe.Reset;
    LProbe.Navigate := ntCompositionUpdate;
    Composition(nepCompositionStart);
    Draft('Navigation draft 🌙', True);
    Composition(nepCompositionUpdate, 'Navigation draft 🌙');
    Check((LProbe.Counts[ntCompositionUpdate] = 1) and
      (LProbe.Last[ntCompositionUpdate].Editing.Text = 'Navigation draft 🌙'),
      'composition callback can unmount its actual producer safely');
    Mount;
    Check(PhysicalText = CInitial, 'remount has no leaked composition guard or draft');
    LProbe.Reset;
    Composition(nepCompositionStart);
    GQueued := TEditingProbe.Create;
    GQueued.Reset;
    GQueuedOwner := GQueued;
    GQueuedSubscription := LRenderer.Events.OnCompositionUpdate(NyxControlEvents('reply-memo'))
      .Policy(neUIQueue).Subscribe(GQueuedOwner);
    Composition(nepCompositionUpdate, 'queued');
    LRenderer.Unmount;
    Check(LRetained.Editing.Text = '中🌙 / draft',
      'retained composition survives producer unmount');
    Check(TNyxCodec.Encode(LDocument) = LWire, 'physical runtime journeys never alter authored document');
    Mount;
    LProbe.Reset;
    LProbe.Navigate := ntTextInput;
    Draft('Navigation 🌙', False);
    Check((LProbe.Counts[ntTextInput] = 1) and
      (LProbe.Counts[ntAfterTextInput] = 0), 'text navigation stops later phases without stale controls');
    {$ifdef NYX_COMPILED_EDITING}
    LRenderer.Free;
    LRenderer := {$ifdef PAS2JS}TNyxBrowserRenderer{$else}TNyxLCLRenderer{$endif}.Create;
    LProbe.Renderer := LRenderer;
    Mount;
    BindNyxCallbacks(LDocument, LRenderer.Events);
    LAccepted := 'Compiled 🌙';
    Draft(LAccepted, False);
    Check((EditingCalls > 0) and EditingLast.HasEditing and
      (EditingLast.Editing.Text = LAccepted), 'compiled handwritten callback receives exact editing context');
    {$endif}
  finally
    LKeySubscription := nil;
    LProbe.Renderer := nil;
    LRenderer.Free;
    {$ifdef PAS2JS}LHost.remove;
    {$else}LHost.Free;{$endif}
    LFactory := nil;
    LDocument.Free;
  end;
end;

{$ifndef PAS2JS}
procedure ExportFixture;
var
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LStream: TFileStream;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LDocument := Fixture;
  try
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.editing.fixture');
    LSource := StringReplace(LSource, 'implementation' + #10,
      'function EditingCalls: Integer;' + #10 +
      'function EditingLast: TNyxEventInfo;' + #10 + #10 + 'implementation' + #10 +
      #10 + 'type' + #10 + '  TEditingAction = class(TNyxEventCallback)' + #10 +
      '    procedure Invoke(const AEvent: TNyxEventInfo;' + #10 +
      '      const AExecution: INyxExecution); override;' + #10 + '  end;' + #10 +
      #10 + 'var' + #10 + '  GCalls: Integer;' + #10 +
      '  GLast: TNyxEventInfo;' + #10 + #10 +
      '{ Handwritten companion preserves the exact owned editing observation. }' + #10 +
      'procedure TEditingAction.Invoke(const AEvent: TNyxEventInfo;' + #10 +
      '  const AExecution: INyxExecution);' + #10 + 'begin' + #10 +
      '  Inc(GCalls);' + #10 + '  GLast := AEvent.Copy;' + #10 + 'end;' + #10 +
      #10 + 'function EditingCalls: Integer;' + #10 + 'begin' + #10 +
      '  Result := GCalls;' + #10 + 'end;' + #10 + #10 +
      'function EditingLast: TNyxEventInfo;' + #10 + 'begin' + #10 +
      '  Result := GLast.Copy;' + #10 + 'end;' + #10, []);
    LSource := StringReplace(LSource, #10 + 'end.' + #10, #10 + 'initialization' + #10 +
      '  RegisterNyxCallback(NyxHandler(''TEditingAction''), TEditingAction);' + #10 +
      #10 + 'end.' + #10, []);
    LStream := TFileStream.Create(ParamStr(1), fmCreate);
    try
      LStream.WriteBuffer(LSource[1], Length(LSource));
    finally
      LStream.Free;
    end;
  finally
    LDocument.Free;
  end;
end;
{$endif}

procedure Finish;
begin
  try
    Check((GQueued.Counts[ntCompositionUpdate] = 0),
      'unmount cancels deferred composition callbacks');
    GQueuedSubscription := nil;
    GQueuedOwner := nil;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' editing-session checks';
    document.body.setAttribute('data-editing-tests', 'passed');
    {$else}
    ExportFixture;
    WriteLn('PASS ', GChecks, ' editing-session checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-editing-tests', 'failed');
      {$else}WriteLn('FAIL ', LException.Message); ExitCode := 1;{$endif}
    end;
  end;
end;

begin
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    GNativeFailure := TNativeFailure.Create;
    Application.OnException := GNativeFailure.Failed;
    {$endif}
    ContractChecks;
    AuthoringChecks;
    ControlChecks;
    {$ifdef PAS2JS}window.setTimeout(@Finish, 80);
    {$else}
    Application.ProcessMessages;
    CheckSynchronize(50);
    Finish;
    Application.OnException := nil;
    FreeAndNil(GNativeFailure);
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-editing-tests', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      Application.OnException := nil;
      FreeAndNil(GNativeFailure);
      ExitCode := 1;
      {$endif}
    end;
    {$ifdef PAS2JS}
    else
    begin
      document.body.textContent := 'FAIL browser host: ' + String(TJSObject(JSExceptValue)['stack']);
      document.body.setAttribute('data-editing-tests', 'failed');
    end;
    {$endif}
  end;
end.
