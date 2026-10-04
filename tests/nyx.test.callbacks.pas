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

unit nyx.test.callbacks;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model;

function RunNyxCallbackAuthoringTests: Integer;
{ Compiled companion fixture owns real handwritten callback classes and helpers. }
function CreateNyxCallbackFixture(out ASource: TNyxText): TNyxDocument;

implementation

uses
  SysUtils,
  nyx.types,
  nyx.data,
  nyx.codec,
  nyx.codegen,
  nyx.composition,
  nyx.schema,
  nyx.source,
  nyx.callbacks,
  nyx.scheduler,
  nyx.events,
  nyx.studio.session,
  nyx.studio.view,
  nyx.studio.inspector;

function ReplaceText(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LParts: TNyxStrings;
  LPosition: Integer;
begin
  LPosition := Pos(ABefore, ASource);

  if LPosition = 0 then
  begin
    raise ENyxModel.Create('Callback fixture replacement was not found');
  end;
  LParts := TNyxStrings.Create;
  try
    LParts.Add(Copy(ASource, 1, LPosition - 1));
    LParts.Add(AAfter);
    LParts.Add(Copy(ASource, LPosition + Length(ABefore), MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function CallbackDesign: TNyxDocument;
var
  LPage: TNyxNode;
  LDefinition: TNyxNode;
begin
  Result := TNyxDocument.Create;
  Result.Title := 'Callback workshop / 🌙 漢字';
  LPage := TNyxNode.Create(nkPage, 'home');
  Result.AddPage(LPage);
  LPage.Add(TNyxNode.Create(nkMemo, 'reply-memo')
    .Configure.Text('Reply').Value('Owned reply / 🌙').Done);
  LPage.Add(TNyxNode.Create(nkLabel, 'reply-help-label')
    .Configure.Text('Click for reply guidance').Done);
  Result.AddPage(TNyxNode.Create(nkPage, 'other'));
  LDefinition := TNyxNode.Create(nkColumn, 'reply-template');
  Result.AddComponent(LDefinition);
  LDefinition.Configure.Compound(True).Done;
  LDefinition.Add(TNyxNode.Create(nkMemo, 'template-reply')
    .Configure.PartName(NyxPart('reply')).Value('Inherited / 漢字').Done);
  LPage.Add(TNyxNode.Create(nkComponent, 'first-reply')
    .Configure.Component(NyxComponent('reply-template')).Done);
  LPage.Add(TNyxNode.Create(nkComponent, 'second-reply')
    .Configure.Component(NyxComponent('reply-template')).Done);
end;

function CreateNyxCallbackFixture(out ASource: TNyxText): TNyxDocument;
var
  LSession: TNyxStudioSession;
  LBase: TNyxDocument;
  LLine: Integer;
  LFirst: TNyxHandlerRef;
  LSecond: TNyxHandlerRef;
  LThird: TNyxHandlerRef;
  LLabel: TNyxHandlerRef;
  LMemoClick: TNyxHandlerRef;
  LKeyDown: TNyxHandlerRef;
  LKeyUp: TNyxHandlerRef;
begin
  LSession := TNyxStudioSession.Create;
  LBase := CallbackDesign;
  try
    LSession.Load(TNyxCodec.Encode(LBase));
    LSession.Select('reply-memo');
    LFirst := LSession.AddCallback(ntAfterEnter, LLine);
    LSecond := LSession.AddCallback(ntAfterEnter, LLine);
    LMemoClick := LSession.AddCallback(ntClick, LLine);
    LKeyDown := LSession.AddCallback(ntKeyDown, LLine);
    LKeyUp := LSession.AddCallback(ntKeyUp, LLine);
    LSession.Select('template-reply');
    LThird := LSession.AddCallback(ntAfterEnter, LLine);
    LSession.Select('reply-help-label');
    LLabel := LSession.AddCallback(ntClick, LLine);
    ASource := LSession.Source;
    ASource := ReplaceText(ASource, 'unit nyx.generated.view;',
      'unit nyx.callback.fixture;');
    ASource := ReplaceText(ASource, 'implementation' + #10,
      'function CallbackInvocations: Integer;' + #10 + #10 + 'implementation' + #10 +
      #10 + 'var GCallbackInvocations: Integer;' + #10);
    ASource := ReplaceText(ASource, NyxViewsEnd + #10,
      NyxViewsEnd + #10 + #10 + 'function CallbackInvocations: Integer;' + #10 +
      'begin' + #10 + '  Result := GCallbackInvocations;' + #10 + 'end;' + #10);
    ASource := ReplaceText(ASource, '// TODO: implement ' + LFirst.Name + '.',
      'Inc(GCallbackInvocations);');
    ASource := ReplaceText(ASource, '// TODO: implement ' + LSecond.Name + '.',
      'Inc(GCallbackInvocations, 10);');
    ASource := ReplaceText(ASource, '// TODO: implement ' + LThird.Name + '.',
      'Inc(GCallbackInvocations, 100);');
    ASource := ReplaceText(ASource, '// TODO: implement ' + LLabel.Name + '.',
      'Inc(GCallbackInvocations, 1000);');
    ASource := ReplaceText(ASource, '// TODO: implement ' + LMemoClick.Name + '.',
      'Inc(GCallbackInvocations, 10000);');
    { Ordinary handwritten bodies consume only Ctrl+Enter. Other text/editing
      keys retain native behavior, and repeats do not repeat the command. }
    ASource := ReplaceText(ASource, '// TODO: implement ' + LKeyDown.Name + '.',
      'if AEvent.HasKeyboard and' + #10 +
      '    AEvent.Keyboard.Matches(nkEnterKey, [nmControl]) then' + #10 +
      '  begin' + #10 +
      '    Inc(GCallbackInvocations, 20000);' + #10 +
      '    NyxEventResponse(AExecution).Consume;' + #10 +
      '  end;');
    ASource := ReplaceText(ASource, '// TODO: implement ' + LKeyUp.Name + '.',
      'if AEvent.HasKeyboard and' + #10 +
      '    AEvent.Keyboard.Matches(nkEnterKey, [nmControl]) then' + #10 +
      '  begin' + #10 +
      '    Inc(GCallbackInvocations, 30000);' + #10 +
      '  end;');
    Result := LSession.Document.Clone;
  finally
    LBase.Free;
    LSession.Free;
  end;
end;

function RunNyxKeyboardAuthoringTests: Integer;
var
  LSession: TNyxStudioSession;
  LBase: TNyxDocument;
  LLoaded: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LHandler: TNyxHandlerRef;
  LBefore: TNyxText;
  LBeforeSource: TNyxText;
  LAfter: TNyxText;
  LSource: TNyxText;
  LLine: Integer;
  LInfos: TNyxAuthoredEventInfos;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Keyboard authoring: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LSession := TNyxStudioSession.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LBase := CallbackDesign;
  try
    LBase.Find('reply-memo').Contract.Signal(ntKeyDown).Signal(ntKeyUp);
    LSession.Load(TNyxCodec.Encode(LBase));
    LSession.Select('reply-memo');
    LBefore := LSession.Save;
    LBeforeSource := LSession.Source;
    LHandler := LSession.AddCallback(ntKeyDown, LLine);
    Check((Pos('.OnKeyDown', LSession.Source) > 0) and
      (Pos('// TODO: implement ' + LHandler.Name + '.', LSession.Source) > 0),
      'keyboard Add creates a fluent descriptor and ordinary TODO implementation');
    Check(LSession.CallbackLine(LHandler) > 0, 'keyboard implementation has source navigation');
    LAfter := LSession.Save;
    LSession.Undo;
    Check((LSession.Save = LBefore) and (LSession.Source = LBeforeSource),
      'keyboard undo restores both design and exact companion');
    LSession.Redo;
    Check(LSession.Save = LAfter, 'keyboard redo preserves authored registration');
    LSession.AddCallback(ntKeyUp, LLine);
    LSession.SetCallbackPolicy(ntKeyUp, neUIQueue);
    LLoaded := TNyxCodec.Decode(LSession.Save);
    try
      LInfos := NyxAuthoredEvents(LLoaded.Find('reply-memo'));
      Check((Length(LInfos) = 2) and (LInfos[0].Trigger = ntKeyDown) and
        (LInfos[1].Trigger = ntKeyUp) and (LInfos[1].Policy = neUIQueue),
        'keyboard descriptors and policies survive wire persistence');
      Check((LLoaded.Find('reply-memo').Contract.EventAt(0).Trigger = ntKeyDown) and
        (LLoaded.Find('reply-memo').Contract.EventAt(1).Trigger = ntKeyUp),
        'keyboard semantic contracts survive wire persistence');
      LCandidate := LWorkspace.Candidate(LLoaded, LSession.Source);
      try
        Check(TNyxCodec.Encode(LCandidate) = LSession.Save,
          'specialized keyboard source reconstructs exact design meaning');
      finally
        LCandidate.Free;
      end;
    finally
      LLoaded.Free;
    end;
    LLoaded := CreateNyxCallbackFixture(LSource);
    try
      Check(Pos('AEvent.Keyboard.Matches(nkEnterKey, [nmControl])', LSource) > 0,
        'compiled fixture retains a handwritten typed shortcut');
      Check(Pos('NyxEventResponse(AExecution).Consume', LSource) > 0,
        'compiled fixture retains a typed synchronous input response');
      LCandidate := LWorkspace.Candidate(LLoaded, LSource);
      try
        Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LLoaded),
          'handwritten keyboard bodies preserve companion admission');
      finally
        LCandidate.Free;
      end;
    finally
      LLoaded.Free;
    end;
  finally
    LBase.Free;
    LWorkspace.Free;
    LSession.Free;
  end;
end;

function RunNyxCallbackAuthoringTests: Integer;
var
  LSession: TNyxStudioSession;
  LBase: TNyxDocument;
  LLoaded: TNyxDocument;
  LCandidate: TNyxDocument;
  LShell: TNyxDocument;
  LProjection: TNyxNode;
  LEvents: TNyxAuthoredEventInfos;
  LMetadata: TNyxEventSchemas;
  LProperties: TNyxPropertyInfos;
  LState: TNyxStudioViewState;
  LFirst: TNyxHandlerRef;
  LSecond: TNyxHandlerRef;
  LLine: Integer;
  LBefore: TNyxText;
  LBeforeSource: TNyxText;
  LSource: TNyxText;
  LRejected: Boolean;
  LWorkspace: TNyxSourceWorkspace;
  LRemoval: TNyxCallbackRemoval;
  LPending: TNyxCallbackRemoval;
  LEffect: TNyxInspectorEffect;
  LRouter: INyxEvents;
  LAttribute: TNyxAttribute;
  LIndex: Integer;
  LFound: Boolean;
  LPublishedProperties: array[0..0] of TNyxPropertyInfo;
  LPublishedEvents: array[0..0] of TNyxEventSchema;
  LCustom: TNyxNode;
  LSourceLines: TNyxStrings;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Callback authoring: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure Rebuild;
  begin
    FreeAndNil(LShell);
    LShell := BuildNyxStudioView(LSession, LState);
  end;

begin
  Result := RunNyxKeyboardAuthoringTests;
  LSession := TNyxStudioSession.Create;
  LBase := CallbackDesign;
  LShell := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LSession.Load(TNyxCodec.Encode(LBase));
    LSession.Select('reply-memo');
    LMetadata := NyxEventsMetadata(LSession.Selected);
    Check((Length(LMetadata) = 43) and (LMetadata[0].Title = 'OnClick') and
      (LMetadata[2].Title = 'OnAfterEnter') and
      (LMetadata[3].Title = 'OnAfterExit') and
      (LMetadata[4].Title = 'OnKeyDown') and (LMetadata[5].Title = 'OnKeyUp'),
      'memo exposes click, lifecycle, change, keyboard and editing callbacks');
    LProperties := NyxProperties(LSession.Selected);
    for LAttribute := Low(TNyxAttribute) to High(TNyxAttribute) do
    begin
      LFound := False;
      for LIndex := 0 to High(LProperties) do
      begin
        LFound := LFound or (LProperties[LIndex].Key = NyxAttributeName(LAttribute));
      end;
      Check(LFound, 'complete typed configuration metadata: ' + NyxAttributeName(LAttribute));
    end;
    { Initialize optional support as undefined; old creator schemas then inherit
      standard support or remain custom, without an uninitialized record flag. }
    LPublishedProperties[0] := Default(TNyxPropertyInfo);
    LPublishedProperties[0].Key := 'line-limit';
    LPublishedProperties[0].Title := 'Line limit';
    LPublishedProperties[0].ValueType := npInteger;
    LPublishedProperties[0].DefaultValue := '25';
    LPublishedProperties[0].Choices := '';
    LPublishedProperties[0].Minimum := 1;
    LPublishedProperties[0].Maximum := 500;
    LPublishedProperties[0].Advanced := False;
    LPublishedEvents[0].Trigger := ntAfterExit;
    LPublishedEvents[0].Title := 'OnAfterExit';
    LPublishedEvents[0].Description := 'Custom published exit callback';
    LPublishedEvents[0].Browser := ncCustom;
    LPublishedEvents[0].Native := ncCustom;
    RegisterNyxSchema(NyxCustomKind('callback-schema-fixture'),
      LPublishedProperties, LPublishedEvents);
    LPublishedProperties[0].Key := 'mutated-caller-property';
    LPublishedEvents[0].Title := 'Mutated caller event';
    LCustom := TNyxNode.Create(NyxCustomKind('callback-schema-fixture'), 'custom-memo');
    try
      LCustom.Configure.ProjectAs(nkMemo).Done;
      LProperties := NyxProperties(LCustom);
      LFound := False;
      for LIndex := 0 to High(LProperties) do
      begin
        LFound := LFound or ((LProperties[LIndex].Key = 'line-limit') and
          (LProperties[LIndex].Minimum = 1));
      end;
      Check(LFound, 'extension schema publishes owned typed properties');
      LMetadata := NyxEventsMetadata(LCustom);
      Check((LMetadata[3].Title = 'OnAfterExit') and
        (LMetadata[3].Native = ncCustom), 'extension callback metadata is an owned capability snapshot');
      LCustom.SetProp('line-limit', '0');
      LRejected := False;
      try
        ValidateNyxProperties(LCustom);
      except
        on Exception do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'published property bounds participate in portable admission');
    finally
      LCustom.Free;
    end;
    LBefore := LSession.Save;
    LBeforeSource := LSession.Source;
    LRejected := False;
    try
      LSession.SetProperty('layout', 'sideways');
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBefore) and
      (LSession.Source = LBeforeSource),
      'expanded enum properties reject invalid leaf configuration atomically');
    LFirst := LSession.AddCallback(ntAfterEnter, LLine);
    Check((LLine > 1) and (Pos('// TODO: implement ' + LFirst.Name + '.', LSession.Source) > 0),
      'add creates a readable managed callback TODO and source navigation');
    Check((Pos('class(TNyxEventCallback)', LSession.Source) > 0) and
      (Pos('RegisterNyxCallback(NyxHandler', LSession.Source) > 0),
      'handler has a reference-counted implementation and typed factory registration');
    LSession.Undo;
    Check((LSession.Save = LBefore) and (LSession.Source = LBeforeSource),
      'handler addition undoes design and companion as one command');
    LSession.Redo;
    Check(Pos(LFirst.Name, LSession.Source) > 0, 'redo restores the same handler identity');
    LSecond := LSession.AddCallback(ntAfterEnter, LLine);
    LSource := ReplaceText(LSession.Source, 'procedure ' + LSecond.Name + '.Invoke',
      'procedure' + #10 + '  ' + LSecond.Name + '.' + #10 + '  Invoke');
    LSource := ReplaceText(LSource, NyxViewsEnd + #10,
      NyxViewsEnd + #10 + '// procedure ' + LSecond.Name + '.Invoke' + #10);
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    LSourceLines := TNyxStrings.Create;
    try
      LSourceLines.Text := LSession.DraftSource;
      Check(LSourceLines[LSession.CallbackLine(LSecond) - 1] = 'procedure',
        'handler navigation ignores a comment and follows a reformatted implementation');
    finally
      LSourceLines.Free;
    end;
    LSession.Undo;
    LSession.SetCallbackPolicy(ntAfterEnter, neUIQueue);
    LEvents := NyxAuthoredEvents(LSession.Selected);
    Check((Length(LEvents[0].Callbacks) = 2) and
      (LEvents[0].Callbacks[0].Handler.Name = LFirst.Name) and
      (LEvents[0].Callbacks[1].Handler.Name = LSecond.Name) and
      (LEvents[0].Policy = neUIQueue), 'independent ordered registrations share one explicit policy');
    LEvents[0].Callbacks[0].Handler := NyxHandler('TChangedSnapshot');
    Check(NyxAuthoredEvents(LSession.Selected)[0].Callbacks[0].Handler.Name = LFirst.Name,
      'reader arrays never alias authored metadata');
    LLoaded := TNyxCodec.Decode(LSession.Save);
    try
      Check(TNyxCodec.Encode(LLoaded) = LSession.Save,
        'persistence retains callback identities, ordering and policy');
    finally
      LLoaded.Free;
    end;
    LCandidate := LWorkspace.Candidate(LSession.Document, LSession.Source);
    try
      Check(TNyxCodec.Encode(LCandidate) = LSession.Save,
        'specialized fluent callback source reproduces the accepted design');
    finally
      LCandidate.Free;
    end;
    LBefore := LSession.Save;
    LBeforeSource := LSession.Source;
    LRejected := False;
    try
      NyxCallbacks(LSession.Selected).OnAfterExit.Add(LFirst,
        NyxCallbackID(LFirst.Name));
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBefore),
      'duplicate registration fails without publishing partial metadata');
    LSource := ReplaceText(LBeforeSource, '.Policy(neUIQueue)',
      '.Policy(''asynchronous'')');
    LSession.SetSourceDraft(LSource);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Source = LBeforeSource) and
      (LSession.DraftSource = LSource), 'raw policy text is rejected with the draft retained');
    LRejected := False;
    try
      LSession.AddCallback(ntAfterExit, LLine);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBefore), 'adding a stub cannot overwrite a pending draft');
    LSession.DiscardSourceDraft;
    LState := DefaultNyxStudioViewState;
    LState.InspectorTab := nitEvents;
    Rebuild;
    Check((LShell.Find(NyxInspectorEventsID) <> nil) and
      (LShell.Find('event-after-enter-count').Prop('text') = '2 registrations'),
      'Nyx-built Events tab shows multiple registrations');
    LPending.Pending := False;
    Check(RouteNyxStudioEvents(LSession,
      LShell.Find('event-after-enter-callback-0-remove'), ntClick, LPending,
      LEffect, LLine, LRemoval) and (LEffect = nieRequestRemoval) and
      LRemoval.Pending and (LSession.Save = LBefore),
      'remove request exposes a warning without changing the design');
    LState.CallbackRemoval := LRemoval;
    LPending := LRemoval;
    Rebuild;
    Check(LShell.Find('event-removal-warning') <> nil, 'shared inspector renders its confirmation warning');
    RouteNyxStudioEvents(LSession, LShell.Find('event-removal-cancel'), ntClick,
      LPending, LEffect, LLine, LRemoval);
    Check((LEffect = nieCancelRemoval) and (LSession.Save = LBefore),
      'declining removal retains the registrations');
    { Source can change a handler while keeping the registration's stable ID.
      The old warning must not authorize removing that changed registration. }
    LSource := ReplaceText(LSession.Source, '.Add(NyxHandler(''' + LFirst.Name + ''')',
      '.Add(NyxHandler(''' + LSecond.Name + ''')');
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    LSource := LSession.Save;
    LRejected := False;
    try
      RouteNyxStudioEvents(LSession, LShell.Find('event-removal-confirm'), ntClick,
        LPending, LEffect, LLine, LRemoval);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LSource),
      'warning confirmation rejects a changed handler with the same registration ID');
    LSession.Undo;
    RouteNyxStudioEvents(LSession, LShell.Find('event-removal-confirm'), ntClick,
      LPending, LEffect, LLine, LRemoval);
    Check((LEffect = nieRemoved) and
      (Length(NyxAuthoredEvents(LSession.Selected)[0].Callbacks) = 1) and
      (Pos('procedure ' + LFirst.Name + '.Invoke', LSession.Source) > 0),
      'confirmed removal retains its Pascal implementation');
    LSession.Undo;
    Check(LSession.Save = LBefore, 'removal undo restores registration identity and order');
    LRouter := NewNyxEvents;
    LRejected := False;
    try
      BindNyxCallbacks(LSession.Document, LRouter);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and not LRouter.HasSubscribers(ntAfterEnter),
      'missing compiled handler fails before partial runtime registrations');

    LSession.Select('template-reply');
    LFirst := LSession.AddCallback(ntAfterEnter, LLine);
    LSession.Select('first-reply');
    LSession.CustomizePart('reply');
    LSession.RemoveCallback(ntAfterEnter, NyxCallbackID(LFirst.Name));
    LProjection := LSession.SelectedProjection;
    try
      Check(Length(NyxAuthoredEvents(LProjection)[0].Callbacks) = 0,
        'instance part removal suppresses inherited callbacks');
    finally
      LProjection.Free;
    end;
    LProjection := LSession.Document.Find('second-reply');
    LProjection := RealizeNyxView(LSession.Document, LProjection);
    try
      Check(Length(NyxAuthoredEvents(LProjection.Part('reply'))[0].Callbacks) = 1,
        'sibling retains its inherited callback');
    finally
      LProjection.Free;
    end;
    Check(Length(NyxAuthoredEvents(LSession.Document.Find('template-reply'))[0].Callbacks) = 1,
      'instance removal leaves reusable template unchanged');
    LBefore := LSession.Save;
    LBeforeSource := LSession.Source;
    LSession.Select('template-reply');
    LRejected := False;
    try
      RouteNyxStudioEvents(LSession, LShell.Find('event-removal-confirm'), ntClick,
        LPending, LEffect, LLine, LRemoval);
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    { A warning for an old registration must not silently target an inherited one. }
    Check(LRejected or (LSession.Save = LBefore), 'stale warning cannot remove a different registration');
    Check(LSession.CallbackLine(LSecond) > 1, 'existing callback source navigation resolves its implementation');
  finally
    LRouter := nil;
    LWorkspace.Free;
    LShell.Free;
    LBase.Free;
    LSession.Free;
  end;
end;

end.
