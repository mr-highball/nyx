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

{ Actual ordinary compiler queue and independently compiled typed event edits.
  No live editor, checkpoint or listener is mutated. Metadata construction is
  qualified here; physical input and scheduler execution have separate consumers. }
procedure EventJourney(AExecutor: TNyxBuildExecutor; const ACompiler: INyxSourceCompiler;
  const AOriginal: INyxSourceProjection; const AOutput: TNyxText);
const
  CHelper: TNyxText =
    'function NotebookPolicy: TNyxExecutionPolicy;' + #10 +
    'begin' + #10 +
    '  Result := neUIQueue;' + #10 +
    'end;' + #10 + #10;
var
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LSource: TNyxText;
  LBeforePair: TNyxProjectPair;
  LStages: array of TNyxDataValue;
  LBuild: INyxSourceProjectionBuild;
  LBase: INyxSourceProjection;
  LEdit: TNyxStudioDesignEdit;
  LIntent: TNyxStudioEventIntent;
  LHandler: TNyxHandlerRef;
  LOwner: TNyxText;
  LView: TNyxText;
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LCopy: TNyxDocument;
  LInfos: TNyxAuthoredEventInfos;
  LBody: TNyxHandlerSource;
  LPosition: Integer;
  LLine: Integer;
  LRejected: Boolean;

  function ReplaceFirst(const AText, AExpected, AReplacement: TNyxText): TNyxText;
  var
    LStart: Integer;
  begin
    LStart := Pos(AExpected, AText);
    Check(LStart > 0, 'event fixture finds exact portable source splice');
    Result := Copy(AText, 1, LStart - 1) + AReplacement +
      Copy(AText, LStart + Length(AExpected), MaxInt);
  end;

  procedure History(const AReason: TNyxText; const AAdded: TNyxHandlerRef;
    const ABody: TNyxHandlerSource; const AReplacement: TNyxText);
  var
    LAfterPair: TNyxProjectPair;
  begin
    LAfterPair := NyxProjectPair(LSession.Save, LSession.Source);
    SetLength(LStages, Length(LStages) + 1);
    LStages[High(LStages)] := NyxObject([
      NyxField('beforeSource', NyxData(LBeforePair.Source)),
      NyxField('beforeDesign', NyxData(LBeforePair.Design)),
      NyxField('afterSource', NyxData(LAfterPair.Source)),
      NyxField('afterDesign', NyxData(LAfterPair.Design)),
      NyxField('added', NyxData(AAdded.Name)),
      NyxField('bodyHandler', NyxData(ABody.Handler.Name)),
      NyxField('expectedBody', NyxData(ABody.Code)),
      NyxField('replacementBody', NyxData(AReplacement))]);
    Check(not LSession.SourceDraftPending, AReason + ' resolves exact accepted source');
    Check(Pos(CHelper, LSession.Source) > 0, AReason + ' retains the complete handwritten helper');
    LSession.Undo;
    Check((LSession.Save = LBeforePair.Design) and (LSession.Source = LBeforePair.Source),
      AReason + ' has one exact paired Undo');
    LSession.Redo;
    Check((LSession.Save = LAfterPair.Design) and (LSession.Source = LAfterPair.Source),
      AReason + ' has one exact paired Redo');
  end;

  procedure Queue(AAction: TNyxStudioEventAction; const AReason: TNyxText);
  begin
    LBeforePair := NyxProjectPair(LSession.Save, LSession.Source);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaEvent;
    LEdit.Selection := 'heading-1';
    LEdit.View := 'notebook-1';
    LEdit.Event.Action := AAction;
    LEdit.Event.Trigger := ntClick;

    if AAction = seaPolicy then
    begin
      LEdit.Event.Policy := neAsynchronous;
    end;

    if AAction = seaRemove then
    begin
      LEdit.Event.ID := NyxCallbackID(LHandler.Name);
      LEdit.Event.Handler := LHandler;
    end;
    LCommands.Edit(LEdit);
    Drain(LCommands);
    Check(LCommands.State = nssApplied, AReason + ': ' + LCommands.Message);

    if AAction = seaAdd then
    begin
      Check(LCommands.CompletedEvent(LIntent, LOwner, LView, LHandler),
        'actual compiled event result supplies source-navigation identity');
      Check((LOwner = 'heading-1') and (LView = 'notebook-1'),
        'completed callback retains its exact selected owner and view');
      History(AReason, LHandler, Default(TNyxHandlerSource), '');
    end
    else
    begin
      History(AReason, Default(TNyxHandlerRef), Default(TNyxHandlerSource), '');
    end;
  end;

  procedure Publish(const AReason: TNyxText);
  begin
    LBeforePair := NyxProjectPair(LSession.Save, LSession.Source);
    LSource := CustomizeNyxExecutedSource(LSession.Source, LSession.Document, LAfter);
    LBuild := AExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);
    Check((LBuild.Projection.State = spsExecuted) and
      (LBuild.Projection.Design = TNyxCodec.Encode(LAfter)),
      AReason + ' compiles and executes the complete proposal: ' + LBuild.Projection.Message);
    LSession.AdoptProjectedProject(NyxProjectPair(LBuild.Projection.Design, LSource),
      LBuild.Projection);
    History(AReason, Default(TNyxHandlerRef), Default(TNyxHandlerSource), '');
    LAfter.Free;
    LAfter := nil;
  end;

  procedure BrowserPacket(const ASource, APrefix: TNyxText);
  begin
    LBuild := AExecutor.ProjectSource(ASource, NyxPascalUnit('nyx.projection.fixture'),
      btBrowser, spcChecked);
    Check(LBuild.Projection.State = spsCompiled, APrefix + ' compiles for pas2js');
    SaveText(AOutput + 'web/' + APrefix + '-worker.json', NyxObject([
      NyxField('reference', NyxData(LBuild.Reference.Name)),
      NyxField('artifact', NyxData(LBuild.Artifact))]).ToJSON);
  end;
begin
  LSession := nil;
  LCommands := nil;
  LBefore := nil;
  LAfter := nil;
  LCopy := nil;
  try
    { Start with the ordinary managed customization scope. Callback ownership
      also protects an executed handwritten constructor without literal views;
      application handler classes remain outside all constructor/managed scopes. }
    LBefore := AOriginal.CopyDocument;
    LAfter := LBefore.Clone;
    LAfter.Title := 'Handwritten event notebook';
    LSource := CustomizeNyxExecutedSource(AOriginal.Source, LBefore, LAfter);
    LAfter.Free;
    LAfter := nil;
    LBefore.Free;
    LBefore := nil;
    LSource := AddNyxHandlerStub(LSource, NyxHandler('TNotebookClick'), ntClick, LLine);
    LSource := AddNyxHandlerStub(LSource, NyxHandler('TNotebookOther'), ntClick, LLine);
    { This existing application class has no local event registration. The
      disposable literal planner therefore cannot discover its reserved name. }
    LSource := AddNyxHandlerStub(LSource, NyxHandler('THeading1Click'), ntClick, LLine);
    LPosition := Pos('function BuildNyxOriginalDocument: TNyxDocument;' + #10 + 'var', LSource);
    Check(LPosition > 0, 'event fixture identifies handwritten constructor');
    Insert(CHelper, LSource, LPosition);
    LSource := ReplaceFirst(LSource, '    LDocument.Validate;',
      '    NyxCallbacks(LDocument.Find(''heading-1'')).OnClick.Policy(NotebookPolicy)' + #10 +
      '      .Add(NyxHandler(''TNotebookClick''), NyxCallbackID(''primary 🚀''))' + #10 +
      '      .Add(NyxHandler(''TNotebookOther''), NyxCallbackID(''secondary 𐐷''));' + #10 +
      '    NyxCallbacks(LDocument.Find(''heading-2'')).OnClick.Policy(NotebookPolicy);' + #10 +
      '    NyxCallbacks(LDocument.Find(''reusable-note'')).OnClick.Policy(NotebookPolicy)' + #10 +
      '      .Add(NyxHandler(''TNotebookClick''), NyxCallbackID(''inherited''));' + #10 +
      '    LDocument.Find(''notebook-1'').Add(NewNyxMemo(''reply'', ncoDescriptor));' + #10 +
      '    NyxCallbacks(LDocument.Find(''reply'')).OnBeforeKeyPress.Policy(NotebookPolicy)' + #10 +
      '      .Add(NyxHandler(''TNotebookClick''), NyxCallbackID(''before''));' + #10 +
      '    NyxCallbacks(LDocument.Find(''reply'')).OnAfterKeyPress.Policy(NotebookPolicy)' + #10 +
      '      .Add(NyxHandler(''TNotebookOther''), NyxCallbackID(''after''));' + #10 +
      '    NyxCallbacks(LDocument.Find(''reply'')).OnNamed(NyxEvent(''app.reply''))' + #10 +
      '      .Policy(NotebookPolicy).Add(NyxHandler(''TNotebookClick''), NyxCallbackID(''named''));' + #10 +
      '    LDocument.Validate;');
    LBuild := AExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);
    Check(LBuild.Projection.State = spsExecuted,
      'handwritten event baseline executes: ' + LBuild.Projection.Message);
    LBase := LBuild.Projection;
    LSession := TNyxStudioSession.Create;
    LSession.AdoptProjectedProject(NyxProjectPair(LBase.Design, LSource), LBase);
    LSession.Activate('notebook-1');
    LSession.Select('heading-1');
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LCommands.UseCompiler(ACompiler);
    Queue(seaAdd, 'add a compiled callback to handwritten Pascal');
    Check(LHandler.Name = 'THeading1Click2', 'handler naming reserves original application helpers');
    LBody := ReadNyxHandlerSource(LSession.Source, LHandler);
    Check((LBody.Line > 0) and (Pos('TODO', LBody.Code) > 0),
      'new callback has an owned editable TODO implementation and line');
    LInfos := NyxAuthoredEvents(LSession.Document.Find('heading-1'));
    Check((Length(LInfos[0].Callbacks) = 3) and (LInfos[0].Policy = neUIQueue),
      'adding preserves ordered siblings and helper-computed policy');
    LRejected := False;
    try
      AddNyxHandlerStub(LSession.Source, NyxHandler('TNotebookClick'), ntClick, LLine);
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'whole-source callback identity collisions refuse before insertion');
    LSource := LSession.Source;
    LPosition := Pos('{ nyx:continuation-begin }', LSource);
    Check(LPosition > 0, 'callback edit has its explicit executed continuation boundary');
    Insert(#10 + 'procedure TManagedCallback.Invoke(const AEvent: TNyxEventInfo;' + #10 +
      '  const AExecution: INyxExecution);' + #10 + 'begin' + #10 + 'end;' + #10,
      LSource, LPosition + Length('{ nyx:continuation-begin }'));
    LRejected := False;
    try
      ReadNyxHandlerSource(LSource, NyxHandler('TManagedCallback'));
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'callback implementations inside executed managed scopes refuse');
    LSource := LSession.Source;
    LPosition := Pos('{ nyx:customizations-end }', LSource);
    Check(LPosition > 0, 'nested executed property scope retains its closing boundary');
    Insert('procedure TManagedTailCallback.Invoke(const AEvent: TNyxEventInfo;' + #10 +
      '  const AExecution: INyxExecution);' + #10 + 'begin' + #10 + 'end;' + #10,
      LSource, LPosition);
    LRejected := False;
    try
      ReadNyxHandlerSource(LSource, NyxHandler('TManagedTailCallback'));
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'the later enclosing customization boundary also protects its tail');
    LBeforePair := NyxProjectPair(LSession.Save, LSession.Source);
    LSource := ReplaceNyxHandlerImplementation(LSession.Source, LHandler, LBody.Code,
      'begin' + #10 + '  { A crafted reply callback 🚀 𐐷 é. }' + #10 + 'end;');
    LBuild := AExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);
    Check(LBuild.Projection.State = spsExecuted, 'exact handler-body edit compiles with retained helpers');
    LSession.AdoptProjectedProject(NyxProjectPair(LBuild.Projection.Design, LSource),
      LBuild.Projection);
    History('edit an owned callback body', Default(TNyxHandlerRef), LBody,
      'begin' + #10 + '  { A crafted reply callback 🚀 𐐷 é. }' + #10 + 'end;');
    Queue(seaPolicy, 'change only the edited event policy');
    LAfter := LSession.Document.Clone;
    NyxCallbacks(LAfter.Find('heading-1')).OnClick.Move(NyxCallbackID(LHandler.Name), 0);
    Publish('move an exact callback identity before its retained siblings');
    LAfter := LSession.Document.Clone;
    NyxCallbacks(LAfter.Find('reply')).OnBeforeKeyPress
      .Add(NyxHandler('TNotebookOther'), NyxCallbackID('second-before'));
    NyxCallbacks(LAfter.Find('reply')).OnNamed(NyxEvent('app.reply'))
      .Add(NyxHandler('TNotebookOther'), NyxCallbackID('named-second')).Policy(neSequential);
    Publish('compose physical phases and exact named callback streams');
    LAfter := LSession.Document.Clone;
    NyxCallbacks(LAfter.Find('reply')).OnBeforeKeyPress
      .Remove(NyxCallbackID('before')).Add(NyxHandler('TNotebookOther'), NyxCallbackID('before'))
      .Move(NyxCallbackID('before'), 0);
    Publish('replace a registration handler without rewriting other implementations');
    SaveText(AOutput + 'web/event-source.pas', LSession.Source);
    SaveText(AOutput + 'web/event-design.json', LSession.Save);
    BrowserPacket(LSession.Source, 'event');
    LSource := ReplaceFirst(LSession.Source, '  Result := neUIQueue;',
      '  Result := neSequential;');
    LBuild := AExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btNativeLCL, spcChecked);
    Check(LBuild.Projection.State = spsExecuted, 'later policy helper change still compiles and executes');
    LCopy := LBuild.Projection.CopyDocument;
    LInfos := NyxAuthoredEvents(LCopy.Find('heading-1'));
    Check((LInfos[0].Policy = neAsynchronous) and (LInfos[0].Callbacks[0].ID.Name = LHandler.Name),
      'deliberate policy/order overrides survive future helper recomputation');
    LInfos := NyxAuthoredEvents(LCopy.Find('reply'));
    Check((LInfos[0].Policy = neSequential) and (Length(LInfos[0].Callbacks) = 2),
      'untouched policy in an edited stream recomputes from its original helper');
    LInfos := NyxAuthoredEvents(LCopy.Find('heading-2'));
    Check(LInfos[0].Policy = neSequential, 'unmodified event stream also recomputes');
    SaveText(AOutput + 'web/event-changed-source.pas', LSource);
    SaveText(AOutput + 'web/event-changed-design.json', LBuild.Projection.Design);
    BrowserPacket(LSource, 'event-changed');
    Queue(seaRemove, 'remove an exact callback while retaining its implementation');
    Check(Pos('A crafted reply callback 🚀 𐐷 é.', LSession.Source) > 0,
      'removal retains the complete authored handler implementation');
    LAfter := LSession.Document.Clone;
    NyxCallbacks(LAfter.Find('reply')).Clear;
    NyxCallbacks(LAfter.Find('note-1')).Clear;
    Publish('explicitly mask local and inherited callback streams');
    LAfter := LSession.Document.Clone;
    NyxCallbacks(LAfter.Find('reply')).Inherit;
    NyxCallbacks(LAfter.Find('note-1')).Inherit;
    Publish('restore callback absence and reusable inheritance');
    Check(not LSession.Document.Find('reply').Extensions.Has(NyxExtension(NyxCallbacksKey)),
      'inherited absence differs from an explicit empty descriptor');
    SaveText(AOutput + 'web/event-steps.json', NyxArray(LStages).ToJSON);
    LBefore := LSession.Document.Clone;
    LAfter := LBefore.Clone;
    LInfos := NyxAuthoredEvents(LAfter.Find('reusable-note'));
    SetLength(LInfos, 2);
    LInfos[1] := LInfos[0];
    LInfos[0] := Default(TNyxAuthoredEventInfo);
    LInfos[0].Trigger := ntAfterEnter;
    LInfos[0].Policy := neSequential;
    NyxCallbacks(LAfter.Find('reusable-note')).Metadata(EncodeNyxAuthoredEvents(LInfos));
    LRejected := False;
    try
      CustomizeNyxExecutedSource(LSession.Source, LBefore, LAfter);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unexpressed event descriptor reordering refuses the whole proposal');
    LAfter.Free;
    LAfter := LBefore.Clone;
    LInfos := nil;
    SetLength(LInfos, 1);
    LInfos[0].Trigger := ntAfterEnter;
    LInfos[0].Policy := neSequential;
    NyxCallbacks(LAfter.Find('reusable-note')).Metadata(EncodeNyxAuthoredEvents(LInfos));
    LRejected := False;
    try
      CustomizeNyxExecutedSource(LSession.Source, LBefore, LAfter);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unexpressed selective event descriptor removal refuses the whole proposal');
    LSession.SetSourceDraft(LSession.Source + #10 + '{ An unfinished callback idea }');
    LBeforePair := NyxProjectPair(LSession.Save, LSession.Source);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaEvent;
    LEdit.Selection := 'heading-1';
    LEdit.View := 'notebook-1';
    LEdit.Event.Action := seaPolicy;
    LEdit.Event.Trigger := ntClick;
    LEdit.Event.Policy := neSequential;
    LRejected := False;
    try
      LCommands.Edit(LEdit);
      Drain(LCommands);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check((LRejected or (LCommands.State = nssRejected)) and
      LSession.SourceDraftPending and (LSession.Source = LBeforePair.Source) and
      (LSession.Save = LBeforePair.Design),
      'unfinished source refuses callback publication and preserves the complete pair/draft');
  finally
    LCopy.Free;
    LAfter.Free;
    LBefore.Free;
    LCommands.Free;
    LSession.Free;
  end;
end;
