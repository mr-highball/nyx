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
program nyx_owned_completion_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, nyx.text, nyx.scheduler, nyx.editing, nyx.editing.history,
  nyx.pascal.names, nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources,
  nyx.pascal.context, nyx.pascal.lexing, nyx.pascal.files, nyx.pascal.binding,
  nyx.pascal.completion, nyx.pascal.completion.owned, nyx.pascal.completion.wire,
  nyx.pascal.completion.view;
const
  CBase: TNyxText = 'unit Consumer; {$mode delphi}{$H+}{$codepage utf8} interface implementation end.';
  CMembers: TNyxText = 'function WithText(const AText:String):TBadge; ' +
    'function WithTone(AValue:Integer):TBadge; ' +
    'class function Build:TBadge; ' +
    'type TInner=record Value:Integer; end;';
var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

function Facts: INyxPascalCompilerContext;
begin
  Result := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi)
    .WithSymbol(NyxPascalDefine('FPC'), ppdDefined)
    .WithSymbol(NyxPascalDefine('FPC_FULLVERSION'), ppdInteger, 30301)
    .CompleteDefines(True);
end;

function Files: INyxPascalFileCatalog;
begin
  Result := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('sdk/system.pas'),
      'unit System; {$mode delphi} interface type TObject=class end; ' +
      'IInterface=interface end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/objpas.pas'),
      'unit ObjPas; {$mode objfpc} interface type Integer=LongInt; implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/templates.pas'),
      'unit Templates; {$mode delphi} interface type TBadge=class public ' +
      '{$I badge-members.inc} end; ' +
      'TBox<T>=class public Value:T; end; ' +
      'TEscaped=record &type:Integer; end; implementation end.')
    .WithFile(NyxPascalPhysicalFile('sdk/badge-members.inc'), CMembers);
end;

function Provider(const AOptions: TNyxOwnedPascalCompletionOptions): INyxPascalCompletionProvider;
var
  LRoots: array of TNyxPascalPhysicalFile;
begin
  SetLength(LRoots, 3);
  LRoots[0] := NyxPascalPhysicalFile('sdk/system.pas');
  LRoots[1] := NyxPascalPhysicalFile('sdk/templates.pas');
  LRoots[2] := NyxPascalPhysicalFile('sdk/objpas.pas');
  Result := NewNyxOwnedPascalCompletionProvider(Facts, Files, LRoots, AOptions);
end;

function SourceFor(const AExpression: TNyxText): INyxPascalSourceSnapshot;
begin
  Result := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase))
    .WithBuffer(TNyxPascalSourceRef.Main,
      'unit Consumer; {$mode delphi} interface uses Templates; implementation ' +
      'procedure Run; var Badge:TBadge; Box:TBox<TBadge>; Escaped:TEscaped; ' +
      TNyxText('begin // 🚀 𐐷 exact physical line') + #13#10 +
      AExpression + '; // suffix ' + #0 + TNyxText(' 𐐷') + #13#10 + 'end; end.');
end;

function AtExpression(const ASources: INyxPascalSourceSnapshot;
  const AExpression: TNyxText; AInExpression: Integer): INyxPascalCompletionRequest;
var
  LText: TNyxText;
  LAt: Integer;
begin
  LText := ASources.Text(TNyxPascalSourceRef.Main);
  LAt := Pos(AExpression, LText);
  Check(LAt > 0, 'Fixture expression exists in exact source text');
  Result := NewNyxPascalCompletionRequest(ASources).At(TNyxPascalSourceRef.Main,
    NyxTextScalarCount(Copy(LText, 1, LAt - 1)) + AInExpression);
end;

function Metadata(const AResult: INyxPascalCompletionResult): INyxOwnedPascalCompletionResult;
begin
  Check(Supports(AResult, INyxOwnedPascalCompletionResult, Result),
    'Owned assistance preserves optional typed definition/gap metadata');
end;

procedure Ready(const AResult: INyxPascalCompletionResult; ACount, ATotal: Integer);
begin
  Check((AResult.Status = pcsReady) and (AResult.Count = ACount) and
    (AResult.Total = ATotal), 'Expected an exact bounded completion page: ' +
      IntToStr(Ord(AResult.Status)) + '/' + IntToStr(AResult.Count) + '/' +
      IntToStr(AResult.Total) + ' / ' + String(AResult.MessageText) + ' / ' +
      String(Metadata(AResult).RelatedName));
  Check(Metadata(AResult).Gap = pcgNone, 'A ready result has no unresolved gap');
end;

procedure MainQueries;
var
  LProvider: INyxPascalCompletionProvider;
  LSources: INyxPascalSourceSnapshot;
  LRequest: INyxPascalCompletionRequest;
  LReply: INyxPascalCompletionResult;
  LNext: INyxPascalCompletionResult;
  LPreview: INyxPascalSourceSnapshot;
  LDefinition: TNyxPascalCompletionDefinition;
  LChanged: INyxPascalSourceSnapshot;
  LCaret: Integer;
  LRejected: Boolean;
  LWire: TNyxText;
  LHistory: INyxTextEditHistory;
  LMenu: INyxPascalCompletionMenu;
  LTicket: TNyxPascalCompletionTicket;
  LOriginal: TNyxText;
begin
  LProvider := Provider(TNyxOwnedPascalCompletionOptions.Defaults);
  LSources := SourceFor('Badge.WithTexx');
  LRequest := AtExpression(LSources, 'Badge.WithTexx', 12);
  LReply := LProvider.Complete(LRequest);
  Ready(LReply, 1, 1);
  Check((LReply.Item(0).Name = TNyxText('WithText')) and
    (LReply.Item(0).InsertText = TNyxText('WithText')) and
    (LReply.Item(0).Kind = pckMethod) and
    (LReply.ExpectedText = TNyxText('WithTexx')),
    'Declared member suggestions replace the whole word around a middle caret');
  LDefinition := Metadata(LReply).Definition(0);
  Check((LDefinition.Root.Name = TNyxText('sdk/templates.pas')) and
    (LDefinition.FileReference.Name = TNyxText('sdk/badge-members.inc')) and
    (NewNyxPascalLexicalSnapshot(CMembers).Slice(LDefinition.Span) = TNyxText('WithText')),
    'Definition navigation keeps its exact SDK include and physical scalar span');
  LProvider := nil;
  Check(Metadata(LReply).DefinitionSource(0).Slice(LDefinition.Span) = TNyxText('WithText'),
    'Definition navigation retains captured physical text after provider retirement');
  LProvider := Provider(TNyxOwnedPascalCompletionOptions.Defaults);
  LPreview := LReply.Preview(0, LSources, LCaret);
  Check((Pos('Badge.WithText;', LPreview.Text(TNyxPascalSourceRef.Main)) > 0) and
    (LPreview.AcceptedText(TNyxPascalSourceRef.Main) = CBase) and
    (Pos('🚀 𐐷', LPreview.Text(TNyxPascalSourceRef.Main)) > 0) and
    (Pos(#0, LPreview.Text(TNyxPascalSourceRef.Main)) > 0),
    'Detached preview preserves accepted text, Unicode, NUL and physical suffix bytes');
  LWire := EncodeNyxCompletionReply(LReply, 'owned-1');
  LNext := DecodeNyxCompletionReply(LWire, 'owned-1', LRequest);
  Check((LNext.Total = 1) and (LNext.Item(0).Name = TNyxText('WithText')) and
    (LNext.ExpectedText = LReply.ExpectedText), 'The owned provider uses the unchanged guarded wire codec');
  LChanged := LSources.WithBuffer(TNyxPascalSourceRef.Main,
    LSources.Text(TNyxPascalSourceRef.Main) + #10);
  LRejected := False;
  try
    LReply.Preview(0, LChanged, LCaret);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'An edited buffer cannot accept an older owned completion');
  LOriginal := LSources.Text(TNyxPascalSourceRef.Main);
  LHistory := NewNyxTextEditHistory(NyxTextEditorFrame(LOriginal,
    NyxTextSelection(LOriginal, LRequest.Caret, LRequest.Caret)));
  LMenu := NewNyxPascalCompletionMenu(LHistory);
  LRequest := LMenu.BeginQuery(LSources, TNyxPascalSourceRef.Main,
    TNyxPascalSourceRef.Main, LTicket);
  LReply := LProvider.Complete(LRequest);
  Check(LMenu.Present(LTicket, LReply, LSources), 'The ordinary Nyx completion menu accepts an owned-engine answer');
  LPreview := LMenu.Accept(LSources);
  Check((LHistory.UndoCount = 1) and
    (LPreview.Text(TNyxPascalSourceRef.Main) = LHistory.Current.AuthoredText) and
    (Pos('Badge.WithText;', LHistory.Current.Text) > 0), 'Owned completion is one ordinary text transaction');
  Check(LHistory.Undo and (LHistory.Current.AuthoredText = LOriginal) and LHistory.Redo,
    'Ordinary Undo/Redo retains the exact pre-completion text and continuation');

  LSources := SourceFor('Box.Value.Wi');
  LRequest := AtExpression(LSources, 'Box.Value.Wi', 12).Take(1);
  LReply := LProvider.Complete(LRequest);
  Ready(LReply, 1, 2);
  LNext := LProvider.Complete(LRequest.Skip(1));
  Ready(LNext, 1, 2);
  Check((LReply.Item(0).Name = TNyxText('WithText')) and
    (LNext.Item(0).Name = TNyxText('WithTone')),
    'Generic property chains retain concrete member types and exact paged totals');
  LSources := SourceFor('TBadge.');
  LReply := LProvider.Complete(AtExpression(LSources, 'TBadge.', 7));
  Ready(LReply, 2, 2);
  Check((LReply.Item(0).Name = TNyxText('Build')) and
    (LReply.Item(1).Name = TNyxText('TInner')),
    'A type receiver exposes class and nested-type members rather than instance methods');
  LSources := SourceFor('Escaped.&typxx');
  LReply := LProvider.Complete(AtExpression(LSources, 'Escaped.&typxx', 11));
  Ready(LReply, 1, 1);
  Check((LReply.Item(0).Name = TNyxText('type')) and
    (LReply.Item(0).InsertText = TNyxText('&type')) and
    (LReply.ExpectedText = TNyxText('&typxx')),
    'Escaped keyword members retain their legal insertion and physical replacement range');
end;

procedure Includes;
var
  LProject: INyxPascalProject;
  LSources: INyxPascalSourceSnapshot;
  LProvider: INyxPascalCompletionProvider;
  LRequest: INyxPascalCompletionRequest;
  LReply: INyxPascalCompletionResult;
  LPreview: INyxPascalSourceSnapshot;
  LInclude: TNyxPascalSourceRef;
  LCaret: Integer;
  LHelperSources: INyxPascalSourceSnapshot;
  LHelper: TNyxPascalSourceRef;
begin
  LInclude := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('body.inc'));
  LProject := NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase)
    .WithInclude(NyxPascalInclude('body.inc'), '// accepted body');
  LSources := CaptureNyxPascalSources(LProject)
    .WithBuffer(TNyxPascalSourceRef.Main,
      'unit Consumer; {$mode delphi} interface uses Templates; implementation ' +
      'procedure Run; var Badge:TBadge; begin {$I body.inc} end; end.')
    .WithBuffer(LInclude, 'Badge.WithTexx;');
  LProvider := Provider(TNyxOwnedPascalCompletionOptions.Defaults);
  LRequest := NewNyxPascalCompletionRequest(LSources).At(LInclude, 12);
  LReply := LProvider.Complete(LRequest);
  Ready(LReply, 1, 1);
  LPreview := LReply.Preview(0, LSources, LCaret);
  Check((LPreview.Text(LInclude) = TNyxText('Badge.WithText;')) and
    (LPreview.Text(TNyxPascalSourceRef.Main) = LSources.Text(TNyxPascalSourceRef.Main)) and
    (LPreview.AcceptedText(LInclude) = TNyxText('// accepted body')),
    'Explicit include ownership maps suggestions without changing its owner or accepted bytes');
  LHelper := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('Helper'));
  LHelperSources := CaptureNyxPascalSources(LProject.WithUnit(NyxPascalUnit('Helper'),
    'unit Helper; {$mode delphi} interface implementation end.'))
    .WithBuffer(LHelper, 'unit Helper; {$mode delphi} interface uses Templates; implementation ' +
      'procedure Run; var Badge:TBadge; begin {$I body.inc} end; end.')
    .WithBuffer(LInclude, 'Badge.WithTexx;');
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LHelperSources)
    .At(LInclude, 12).From(LHelper));
  Ready(LReply, 1, 1);
  Check(LReply.Request.Owner.SameFile(LHelper),
    'An include can select its auxiliary unit owner without a doubled suffix or namespace confusion');
  LSources := LSources.WithBuffer(TNyxPascalSourceRef.Main,
    'unit Consumer; {$mode delphi} interface uses Templates; implementation ' +
    'procedure Run; var Badge:TBadge; begin {$I body.inc} {$I body.inc} end; end.');
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources).At(LInclude, 11));
  Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and
    (Metadata(LReply).Gap = pcgRepeatedInclude),
    'An ambiguous repeated include is never assigned a guessed occurrence');
  LSources := LSources.WithBuffer(TNyxPascalSourceRef.Main,
    'unit Consumer; {$mode delphi} interface uses Templates; implementation ' +
    'procedure Run; var Badge:TBadge; begin {$if false}{$I body.inc}{$endif} end; end.');
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources).At(LInclude, 11));
  Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and
    (Metadata(LReply).Gap = pcgPhysicalContext), 'An inactive include cannot borrow another lexical context');
end;

procedure Failures;
var
  LProvider: INyxPascalCompletionProvider;
  LSources: INyxPascalSourceSnapshot;
  LReply: INyxPascalCompletionResult;
  LPort: INyxCancellablePascalCompletionProvider;
  LCancel: INyxCancellationScope;
  LQuery: INyxPascalCompletionRequest;
begin
  LProvider := Provider(TNyxOwnedPascalCompletionOptions.Defaults);
  LSources := SourceFor('Badge.WithText(''hello'').Wi');
  LReply := LProvider.Complete(AtExpression(LSources, 'Badge.WithText(''hello'').Wi', 26));
  Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and
    (Metadata(LReply).Gap = pcgExpression), 'Call inference gaps never resolve a misleading tail identifier');
  LSources := SourceFor('Box[0].Wi');
  LReply := LProvider.Complete(AtExpression(LSources, 'Box[0].Wi', 9));
  Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and
    (Metadata(LReply).Gap = pcgExpression), 'Indexing remains an explicit inference gap');
  LSources := SourceFor('Badge.Wi');
  LQuery := AtExpression(LSources, 'Badge.Wi', 8);
  LProvider := Provider(TNyxOwnedPascalCompletionOptions.Defaults.UpToTokens(1));
  LReply := LProvider.Complete(LQuery);
  Check((LReply.Status = pcsUnavailable) and (Metadata(LReply).Gap = pcgBudget) and
    (LReply.Total = 0), 'Aggregate translation limits publish no partial rows');
  LProvider := Provider(TNyxOwnedPascalCompletionOptions.Defaults.UpToReceiverTokens(1));
  LReply := LProvider.Complete(LQuery);
  Check((LReply.Status = pcsUnavailable) and (Metadata(LReply).Gap = pcgBudget),
    'Receiver scanning has its own bounded work contract');
  LProvider := Provider(TNyxOwnedPascalCompletionOptions.Defaults);
  Supports(LProvider, INyxCancellablePascalCompletionProvider, LPort);
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LReply := LPort.CompleteWithCancellation(LQuery, LCancel);
  Check((LReply.Status = pcsCancelled) and (LReply.Count = 0) and
    (Metadata(LReply).Gap = pcgCancelled), 'Cancellation retires analysis without insertion rows');
  LSources := LSources.WithBuffer(TNyxPascalSourceRef.Main,
    'unit Other; {$mode delphi} interface implementation end.');
  LReply := LProvider.Complete(NewNyxPascalCompletionRequest(LSources));
  Check((LReply.Status = pcsUnavailable) and
    (Metadata(LReply).Gap = pcgSourceIdentity), 'Editing cannot silently rename accepted module membership');
  LSources := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase))
    .WithBuffer(TNyxPascalSourceRef.Main,
      'unit Consumer; {$mode delphi} interface uses Missing; implementation ' +
      'procedure Run; var Badge:TUnknown; begin Badge.Wi; end; end.');
  LReply := LProvider.Complete(AtExpression(LSources, 'Badge.Wi', 8));
  Check((LReply.Status = pcsUnavailable) and
    (Metadata(LReply).BindingIssue = pbiMissingModule) and
    (Metadata(LReply).RelatedName = TNyxText('Missing')),
    'Missing compiler/SDK inputs remain focused typed gaps rather than invented members');
  LSources := SourceFor('Integer.Wi');
  LReply := LProvider.Complete(AtExpression(LSources, 'Integer.Wi', 10));
  Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and
    (Metadata(LReply).BindingIssue = pbiUnsupportedType) and (LReply.MessageText <> ''),
    'Intrinsic receivers report a useful typed helper gap rather than an empty message');
end;

procedure CurrentHelpers;
var
  LProject: INyxPascalProject;
  LSources: INyxPascalSourceSnapshot;
  LHelper: TNyxPascalSourceRef;
  LProvider: INyxPascalCompletionProvider;
  LReply: INyxPascalCompletionResult;
  LChanged: INyxPascalSourceSnapshot;
  LRequest: INyxPascalCompletionRequest;
  LRejected: Boolean;
  LCaret: Integer;
begin
  LHelper := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('Helper'));
  LProject := NewNyxPascalProject(NyxPascalUnit('Consumer'), CBase)
    .WithUnit(NyxPascalUnit('Helper'),
      'unit Helper; {$mode delphi} interface type TData=record OldField:Integer; end; implementation end.');
  LSources := CaptureNyxPascalSources(LProject)
    .WithBuffer(LHelper,
      'unit Helper; {$mode delphi} interface type TData=record NewField:Integer; end; implementation end.')
    .WithBuffer(TNyxPascalSourceRef.Main,
      'unit Consumer; {$mode delphi} interface uses Helper; implementation ' +
      'procedure Run; var Data:TData; begin Data.Ne');
  LProvider := Provider(TNyxOwnedPascalCompletionOptions.Defaults);
  LRequest := AtExpression(LSources, 'Data.Ne', 7);
  LReply := LProvider.Complete(LRequest);
  Ready(LReply, 1, 1);
  Check((LReply.Item(0).Name = TNyxText('NewField')) and
    (Metadata(LReply).Definition(0).FileReference.Name = TNyxText('Helper.pas')),
    'Unfinished routine completion uses the current helper buffer and its physical declaration');
  LChanged := LReply.Preview(0, LSources, LCaret);
  Check((LChanged.Text(LHelper) = LSources.Text(LHelper)) and
    (LChanged.AcceptedText(LHelper) = LSources.AcceptedText(LHelper)),
    'A main-file suggestion preserves independent helper editing and accepted text');
  LChanged := LSources.WithBuffer(LHelper, LSources.Text(LHelper) + #10);
  LRejected := False;
  try
    LReply.Preview(0, LChanged, LCaret);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Changing any helper retires a main-file insertion proposal');
  LProvider := NewNyxOwnedPascalCompletionProvider(Facts.CompleteDefines(False), Files,
    [NyxPascalPhysicalFile('sdk/system.pas'), NyxPascalPhysicalFile('sdk/templates.pas')],
    TNyxOwnedPascalCompletionOptions.Defaults);
  LSources := LSources.WithBuffer(TNyxPascalSourceRef.Main,
    'unit Consumer; {$mode delphi} interface uses Helper; implementation ' +
    'procedure Run; var Data:TData; begin {$ifdef UNKNOWN_BRANCH}Data.Ne{$endif} end; end.');
  LReply := LProvider.Complete(AtExpression(LSources, 'Data.Ne', 7));
  Check((LReply.Status = pcsUnavailable) and (LReply.Count = 0) and
    (Metadata(LReply).Gap = pcgPhysicalContext),
    'Unknown conditional participation cannot manufacture a caret context');
end;

begin
  try
    MainQueries;
    Includes;
    Failures;
    CurrentHelpers;
    WriteLn('PASS ', GChecks, ' owned completion checks');
  except
    on LError: Exception do
    begin
      WriteLn('FAIL / ', LError.Message);
      {$ifndef PAS2JS}DumpExceptionBackTrace(StdErr);{$endif}
      ExitCode := 1;
    end;
  end;
end.
