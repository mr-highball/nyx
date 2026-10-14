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


program nyx_flow_routes_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.bytes, nyx.model, nyx.state,
  nyx.flow.routes, nyx.flow.route.model, nyx.flow.declarations, nyx.flow.types, nyx.flow,
  nyx.logic, nyx.schema, nyx.codec, nyx.codegen, nyx.source, nyx.studio.projects,
  nyx.studio.session, nyx.studio.edits, nyx.events, nyx.behavior, nyx.scheduler, nyx.controls,
  nyx.callbacks, nyx.test.flow.routes, nyx.test.logic.fixture
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  TObserver = class(TNyxEventObserver)
  public
    Mark: TNyxText;
    Expected: Integer;
    AddDuringDispatch: Boolean;
    procedure Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer); override;
  end;
  TCallback = class(TNyxEventCallback)
  public
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;
  GOrder: TNyxText;
  GObservation: INyxEventObservation;
  GAdded: INyxEventSubscription;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Flow routes: ' + AMessage);
  end;
  Inc(GChecks);
end;

procedure TObserver.Observe(const AEvent: TNyxEventInfo; ACallbackCount: Integer);
var
  LObserver: TObserver;
begin
  Check(ACallbackCount = Expected, 'observer sees exact ordinary callback count');
  GOrder := GOrder + Mark;

  if AddDuringDispatch then
  begin
    AddDuringDispatch := False;
    LObserver := TObserver.Create;
    LObserver.Mark := 'N';
    LObserver.Expected := 1;
    GAdded := GObservation.Observe(NyxControlEvents('button'), ntClick,
      LObserver, nopAfterDispatch, NyxEvent(''));
  end;
end;

procedure TCallback.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
begin
  GOrder := GOrder + 'C';
end;

procedure EventObservers;
var
  LEvents: INyxEvents;
  LBefore: TObserver;
  LAfter: TObserver;
  LFirst: INyxEventSubscription;
  LSecond: INyxEventSubscription;
  LCallback: INyxEventSubscription;
  LInfo: TNyxEventInfo;
  LRuns: TNyxExecutions;
begin
  LEvents := NewNyxEvents;
  Check(Supports(LEvents, INyxEventObservation, GObservation), 'optional observation capability');
  LBefore := TObserver.Create;
  LBefore.Mark := 'B';
  LBefore.Expected := 1;
  LBefore.AddDuringDispatch := True;
  LAfter := TObserver.Create;
  LAfter.Mark := 'A';
  LAfter.Expected := 1;
  LFirst := GObservation.Observe(NyxControlEvents('button'), ntClick,
    LBefore, nopBeforeCallbacks, NyxEvent(''));
  LSecond := GObservation.Observe(NyxControlEvents('button'), ntClick,
    LAfter, nopAfterDispatch, NyxEvent(''));
  LCallback := LEvents.On(NyxControlEvents('button'), ntClick).Subscribe(TCallback.Create);
  LInfo := Default(TNyxEventInfo);
  LInfo.Value := NyxNull;
  LInfo.Details := NyxNull;
  LInfo.Trigger := ntClick;
  LInfo.Name := NyxEvent('click');
  LInfo.OriginID := 'button';
  LInfo.SourceID := 'button';
  LInfo.TargetID := 'button';
  GOrder := '';
  LRuns := LEvents.Dispatch(LInfo, 'button', 'button');
  Check((GOrder = 'BCA') and (Length(LRuns) = 3),
    'all phases snapshot before callbacks; new observer waits for next dispatch');
  GOrder := '';
  LEvents.Dispatch(LInfo, 'button', 'button');
  Check(GOrder = 'BCAN', 'new observer joins next dispatch in stable phase order');
  LFirst.Cancel;
  LSecond.Cancel;
  GAdded.Cancel;
  GOrder := '';
  LEvents.Dispatch(LInfo, 'button', 'button');
  Check(GOrder = 'C', 'cancelled observers never enter later dispatch');
  LCallback.Cancel;
  Check(not LEvents.HasSubscribers(ntClick), 'cancelled observation groups prune completely');
  LEvents.Close;
  GObservation := nil;
  GAdded := nil;
end;

{$ifndef PAS2JS}
procedure WriteExact(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure SavedRoutes;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LRoutes: INyxFlowRoutes;
  LOther: INyxFlowRoutes;
  LSource: TNyxText;
  LBefore: TNyxText;
  LContinuation: TNyxText;
  LContinuationDesign: TNyxText;
  LHandwritten: TNyxText;
  LHelper: TNyxText;
  LPosition: Integer;
  LSession: TNyxStudioSession;
  LPair: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LFailed: Boolean;
begin
  LDocument := RoutedLogicDocument;
  LCopy := nil;
  LParsed := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    LDocument.Flows.Define(NyxFlowRef('Constants'), NewNyxFlowGraph.WithStep(
      NyxFlowStep('typed'), NewNyxLogicDeclaration(
        NyxLogicDescriptor(NyxLogicKind('constants'), 'Typed constants',
          'Qualification of every constant family without executable authority.', nlgData)
          .Input(NyxFlowPort('text'), nftText, 'Text')
          .Input(NyxFlowPort('boolean'), nftBoolean, 'Boolean')
          .Input(NyxFlowPort('integer'), nftInteger, 'Integer')
          .Input(NyxFlowPort('number'), nftNumber, 'Exact decimal')
          .Input(NyxFlowPort('json'), nftJSON, 'JSON')
          .Input(NyxFlowPort('bytes'), nftBytes, 'Bytes')
          .Input(NyxFlowPort('signal'), nftSignal, 'Signal'), NyxObject([]))).Build);
    LDocument.Pages[0].Add(NewNyxButton('constants-button').WithText('Typed constants'));
    SetNyxFlowRoutes(LDocument.Find('constants-button'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('All families'), NyxFlowRef('Constants'), ntClick, nfcmExclusive)
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('text')), NyxFlowText('Exact 🐾' + TNyxText(#0)))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('boolean')), NyxFlowBoolean(True))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('integer')), NyxFlowInteger(-17))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('number')), NyxFlowNumber(NyxDecimal('9007199254740993.125')))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('json')), NyxFlowJSON(NyxObject([NyxField('exact', NyxData('🐾'))])))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('bytes')), NyxFlowBytes(NyxDecodeBase64('AAH/')))
        .InputConstant(NyxFlowEndpoint(NyxFlowStep('typed'), NyxFlowPort('signal')), NyxFlowSignal)
        .Using(neAsynchronous).Concurrent(nfrEvery).Enabled(False).MaximumRuns(4)));
    ValidateNyxDocumentProperties(LDocument);
    LRoutes := NyxControlFlowRoutes(LDocument.Find('run-button'));
    LOther := LRoutes.WithRoute(ProfileRoute.Enabled(False).MaximumRuns(3));
    Check(LRoutes.Route(0).IsEnabled and not LOther.Route(0).IsEnabled,
      'immutable route replacement preserves earlier set');
    Check((LOther.Count = 1) and (LOther.Route(0).RunLimit = 3), 'replace preserves identity/order');
    Check(LOther.WithoutRoute(NyxFlowRouteID('Format profile')).Count = 0, 'exact route removal');
    LBefore := TNyxCodec.Encode(LDocument);
    LCopy := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LCopy) = LBefore, 'route persistence is exact');
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('SetNyxFlowRoutes(', LSource) > 0) and
      (Pos('.InputEventValue(', LSource) > 0) and (Pos('nfcmExclusive', LSource) > 0) and
      (Pos('NyxFlowText(', LSource) > 0), 'crafted source retains typed event routes and constants');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'typed source rebuilds exact complete routes');
    Check(LWorkspace.Render(LParsed) = LSource, 'no-op route source retains exact bytes');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LPair := NyxProjectPair(LBefore, LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('run-button');
    LSession.SetProperty('text', 'Format this profile');
    LAfter := LSession.ProjectSnapshot;
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair), 'route pair Undo exact');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter), 'route pair Redo exact');
    LSession.ApplyPatch(NyxFlowRoutesPatch(NyxControl('run-button'), LOther));
    Check(not NyxControlFlowRoutes(LSession.Document.Find('run-button')).Route(0).IsEnabled,
      'typed route patch shares ordinary paired source publication');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'route patch Undo restores exact complete pair');
    LSession.SetSourceDraft(LAfter.Source + #10 + '{ unfinished route draft }');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailed := False;
    try
      LSession.ApplyPatch(NyxFlowRoutesPatch(NyxControl('run-button'), LOther));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LBefore),
      'route draft refusal preserves accepted pair, unfinished buffer and baseline');
    LBefore := TNyxCodec.Encode(LDocument);

    FreeAndNil(LCopy);
    LCopy := LDocument.Clone;
    SetNyxFlowRoutes(LCopy.Find('run-button'), LOther);
    LContinuation := TNyxCodegen.CustomizationRoutine(LCopy,
      TNyxCodegen.CustomizationChanges(LDocument, LCopy));
    Check((Pos('SetNyxFlowRoutes(', LContinuation) > 0) and
      (Pos('.Enabled(False)', LContinuation) > 0),
      'targeted handwritten continuation expresses route changes without replacing helpers');
    LHelper := 'function SavedRouteCapacity: Integer;' + #10 +
      'begin' + #10 + '  Result := 8;' + #10 + 'end;' + #10;
    LPosition := Pos('implementation' + #10, LSource);
    LHandwritten := Copy(LSource, 1, LPosition + Length('implementation' + #10) - 1) +
      #10 + LHelper + Copy(LSource, LPosition + Length('implementation' + #10), MaxInt);
    LPosition := Pos('.MaximumRuns(8)', LHandwritten);
    LHandwritten := Copy(LHandwritten, 1, LPosition - 1) + '.MaximumRuns(SavedRouteCapacity)' +
      Copy(LHandwritten, LPosition + Length('.MaximumRuns(8)'), MaxInt);
    LContinuation := CustomizeNyxExecutedSource(LHandwritten, LDocument, LCopy);
    LContinuationDesign := TNyxCodec.Encode(LCopy);
    Check((Pos(LHelper, LContinuation) > 0) and
      (Pos('.MaximumRuns(SavedRouteCapacity)', LContinuation) > 0),
      'handwritten route helper remains exact behind targeted continuation');
    LCopy.Flows.Remove(NyxFlowRef('Profile'));
    LFailed := False;
    try
      LCopy.Validate;
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'referenced graph removal refuses even for a disabled route');
    FreeAndNil(LCopy);
    LCopy := LDocument.Clone;
    NyxCallbacks(LCopy.Find('run-button')).OnClick.Add(NyxHandler('TExisting'), NyxCallbackID('existing'));
    LFailed := False;
    try
      ValidateNyxDocumentProperties(LCopy);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'exclusive authored callback conflict refuses before mount');
    SetNyxFlowRoutes(LCopy.Find('run-button'),
      NewNyxFlowRoutes.WithRoute(ProfileRoute(nfcmAfterDispatch)));
    ValidateNyxDocumentProperties(LCopy);
    Check(True, 'explicit mixed order admits existing callbacks');
    SetNyxFlowRoutes(LCopy.Find('pretty-code'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('unsupported'), NyxFlowRef('Profile'), ntChange, nfcmExclusive)
        .InputState(LogicEndpoint('parse', nlpText), NyxTextState('source'))
        .InputConstant(LogicEndpoint('summary', nlpLeft), NyxFlowText('Profile'))));
    LFailed := False;
    try
      ValidateNyxDocumentProperties(LCopy);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'unsupported physical event refuses instead of inventing a producer');
    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      ForceDirectories(ParamStr(1));
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', LSource);
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'design.nyx', LBefore);
      ForceDirectories(IncludeTrailingPathDelimiter(ParamStr(1)) + 'continuation');
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'continuation/nyx.generated.view.pas', LContinuation);
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'continuation/design.nyx', LContinuationDesign);
    end;
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LCopy.Free;
    LDocument.Free;
  end;
end;

begin
  try
    EventObservers;
    SavedRoutes;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-flow-routes', 'passed');
    document.body.setAttribute('data-flow-routes-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' saved route/observer/source checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-flow-routes', 'failed');
      document.body.setAttribute('data-flow-routes-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
