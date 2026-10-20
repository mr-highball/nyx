{ Nyx shared source-controller navigation qualification. MIT license, see LICENSE.
  Controlled physical/service ports exercise production controller/event paths;
  this console fixture creates no GUI, network host or authored execution. }
program nyx_studio_definition_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.contract, nyx.model, nyx.events,
  nyx.behavior, nyx.editing, nyx.editing.history, nyx.editing.commands,
  nyx.text.projection, nyx.pascal.names, nyx.pascal.drafts,
  nyx.pascal.completion, nyx.pascal.definition,
  nyx.studio.completion, nyx.studio.session, nyx.studio.projects,
  nyx.studio.sourceinput {$ifdef PAS2JS}, Web{$endif};

type
  { Detached deterministic service lease. Delivery occurs on a separate explicit
    turn, matching the real transport's lifetime without hosting a service. }
  TContext = class(TInterfacedObject, INyxStudioCompletionContext, INyxStudioDefinitionContext)
  public
    Query: INyxPascalCompletionRequest;
    Reply: INyxStudioCompletionReply;
    Enabled: Boolean;
    DefinitionMode: Boolean;
    DefinitionTargets: Integer;
    function Request(const AQuery: INyxPascalCompletionRequest;
      const AReply: INyxStudioCompletionReply): TNyxStudioCompletionReadiness;
    function RequestDefinition(const AQuery: INyxPascalDefinitionRequest;
      const AReply: INyxStudioCompletionReply): TNyxStudioCompletionReadiness;
    function Current(const AQuery: INyxPascalCompletionRequest): Boolean;
    procedure Cancel;
    procedure Detach;
    procedure Deliver;
  end;
  { Owns a real session/controller/events and copied physical editing snapshot.
    No renderer lifetime is borrowed; the host methods model one live mount. }
  THost = class
  public
    Session: TNyxStudioSession;
    Input: TNyxStudioSourceInput;
    Events: INyxEvents;
    Context: TContext;
    ContextLease: INyxStudioCompletionContext;
    OriginalContextLease: INyxStudioCompletionContext;
    Physical: TNyxEditingSnapshot;
    Writable: Boolean;
    Writes: Integer;
    ChangedWrites: Integer;
    Notices: Integer;
    UseExplicitOwner: Boolean;
    ExplicitOwner: TNyxPascalSourceRef;
    constructor Create;
    destructor Destroy; override;
    function ReadInput(out ASession: TNyxStudioSession): TNyxEditingSnapshot;
    procedure WriteInput(const AFrame: TNyxTextEditorFrame; AChanged: Boolean);
    function ReadContext: INyxStudioCompletionContext;
    function ReadOwner: TNyxPascalSourceRef;
    procedure Changed;
    procedure Key(AKey: TNyxKey; AModifiers: TNyxKeyModifiers = []);
    procedure PresentAndQueue;
  end;

const
  CPrefix = '// 🚀 𐐷' + #13#10 + 'procedure ';
  CSecondPrefix = CPrefix + 'WithText;' + #13#10 + 'procedure ';
  CBuffer: TNyxText = CSecondPrefix + 'WithTheme;' + #13#10 + 'LBadge.Wi';

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
end;

function TContext.Request(const AQuery: INyxPascalCompletionRequest;
  const AReply: INyxStudioCompletionReply): TNyxStudioCompletionReadiness;
begin
  DefinitionMode := False;
  Query := AQuery;
  Reply := AReply;
  Result := scrSubmitted;
end;

function TContext.RequestDefinition(const AQuery: INyxPascalDefinitionRequest;
  const AReply: INyxStudioCompletionReply): TNyxStudioCompletionReadiness;
begin
  Query := NyxPascalDefinitionQuery(AQuery);
  Reply := AReply;
  DefinitionMode := True;
  Result := scrSubmitted;
end;

function TContext.Current(const AQuery: INyxPascalCompletionRequest): Boolean;
begin
  Result := Enabled and (AQuery <> nil) and (Query <> nil) and
    AQuery.Matches(Query.Sources);
end;

procedure TContext.Cancel;
begin
  Query := nil;
  Reply := nil;
end;

procedure TContext.Detach;
begin
  Enabled := False;
  Cancel;
end;

procedure TContext.Deliver;
var
  LReply: INyxStudioCompletionReply;
  LResult: INyxPascalCompletionResult;
  LText: TNyxText;
  LFirst: Integer;
  LSecond: Integer;
  LTargets: array of TNyxPascalDefinitionTarget;
begin
  LReply := Reply;
  Reply := nil;
  LText := Query.Sources.Text(TNyxPascalSourceRef.Main);
  LFirst := NyxTextScalarCount(Copy(LText, 1, Pos('WithText', LText) - 1));
  LSecond := NyxTextScalarCount(Copy(LText, 1, Pos('WithTheme', LText) - 1));

  if DefinitionMode then
  begin
    SetLength(LTargets, DefinitionTargets);
    LTargets[0] := NyxPascalDefinitionTarget('WithText', 'Controlled declaration', pckMethod,
      NyxPascalCompletionLocation(TNyxPascalSourceRef.Main, LFirst, 8, 'WithText'));

    if DefinitionTargets = 2 then
    begin
      LTargets[1] := NyxPascalDefinitionTarget('WithTheme', 'Second declaration', pckMethod,
        NyxPascalCompletionLocation(TNyxPascalSourceRef.Main, LSecond, 9, 'WithTheme'));
    end;
    LResult := NyxPascalDefinitionPage(NewNyxPascalDefinitionResult(
      NyxPascalDefinitionRequest(Query), pdsReady, '', LTargets, Length(LTargets)));
    LReply.Receive(Query, LResult);
    Exit;
  end;
  LResult := NewNyxPascalCompletionResult(Query, pcsReady, '', Query.Caret - 2,
    'Wi', [NyxPascalCompletion('WithText', 'WithText', 'Controlled declaration', pckMethod),
      NyxPascalCompletion('WithTheme', 'WithTheme', 'Second controlled declaration', pckMethod)], 2);
  LResult := NewNyxPascalCompletionDefinitions(LResult, [NyxPascalCompletionLocation(
    TNyxPascalSourceRef.Main, LFirst, 8, 'WithText'),
    NyxPascalCompletionLocation(TNyxPascalSourceRef.Main,
      LSecond, 9, 'WithTheme')]);
  LReply.Receive(Query, LResult);
end;

constructor THost.Create;
var
  LText: TNyxText;
begin
  inherited Create;
  Session := TNyxStudioSession.Create;
  Session.SetSourceDraft(CBuffer);
  LText := NyxPresentText(CBuffer, nteLF);
  Physical := NyxEditingSnapshot(nepSelectionChange, neiUnknown, LText, '', False,
    NyxTextSelection(LText, NyxTextScalarCount(LText), NyxTextScalarCount(LText)), False, False);
  Writable := True;
  Context := TContext.Create;
  Context.Enabled := True;
  Context.DefinitionTargets := 1;
  ContextLease := Context;
  OriginalContextLease := Context;
  Events := NewNyxEvents;
  Input := TNyxStudioSourceInput.Create(Events, ReadInput, WriteInput, nteLF);
  Input.ConfigureCompletion(ReadContext, ReadOwner, Changed);
  Input.Bind;
end;

destructor THost.Destroy;
begin
  Input.Free;
  Events.Close;
  Context.Detach;
  ContextLease := nil;
  OriginalContextLease := nil;
  Session.Free;
  inherited Destroy;
end;

function THost.ReadInput(out ASession: TNyxStudioSession): TNyxEditingSnapshot;
begin
  ASession := Session;
  Result := Default(TNyxEditingSnapshot);

  if Writable then
  begin
    Result := Physical;
  end;
end;

procedure THost.WriteInput(const AFrame: TNyxTextEditorFrame; AChanged: Boolean);
begin
  Inc(Writes);

  if AChanged then
  begin
    Inc(ChangedWrites);
  end;
  Physical := NyxEditingSnapshot(nepSelectionChange, neiUnknown, AFrame.Text, '', False,
    AFrame.Selection, False, False);
end;

function THost.ReadContext: INyxStudioCompletionContext;
begin
  Result := ContextLease;
end;

function THost.ReadOwner: TNyxPascalSourceRef;
begin
  Result := Session.ActiveSourceFile;

  if UseExplicitOwner then Result := ExplicitOwner;
end;

procedure THost.Changed;
begin
  Inc(Notices);
end;

procedure THost.Key(AKey: TNyxKey; AModifiers: TNyxKeyModifiers);
var
  LEvent: TNyxEventInfo;
  LConsumed: Boolean;
begin
  LEvent := Default(TNyxEventInfo);
  LEvent.Trigger := ntKeyDown;
  LEvent.Name := NyxEvent(NyxTriggerName(ntKeyDown));
  LEvent.Value := NyxNull;
  LEvent.Details := NyxNull;
  LEvent.SourceID := 'studio-code';
  LEvent.OriginID := 'studio-code';
  LEvent.TargetID := 'studio-code';
  LEvent.HasKeyboard := True;
  LEvent.Keyboard := NyxKeyStroke(AKey, AModifiers);
  Events.DispatchInput(LEvent, 'studio-code', 'studio-code', LConsumed);
  Check(LConsumed, 'Completion shortcuts consume only their synchronous input window');
end;

procedure THost.PresentAndQueue;
var
  LDocument: TNyxDocument;
  LPage: TNyxNode;
begin
  Key(nkSpaceKey, [nmControl]);
  Check(Context.Reply <> nil, 'Real input subscription submits a completion request');
  Context.Deliver;
  LDocument := Input.BuildCompletion;
  try
    Check(LDocument <> nil, 'The shared controller presents a definition page');
    LPage := LDocument.Pages[0];
    Check(Input.RouteCompletion(LPage, LPage.Find('studio-completion-row-0-definition'), ntClick),
      'Current Nyx definition button queues navigation');
  finally
    LDocument.Free;
  end;
end;

procedure Run;
var
  LHost: THost;
  LBefore: TNyxProjectPair;
  LLine, LColumn, LAt: Integer;
  LText: TNyxText;
  LOther: INyxStudioCompletionContext;
  LDocument: TNyxDocument;
  LPage: TNyxNode;
  LSecondOwner: TNyxPascalSourceRef;
begin
  LHost := THost.Create;
  try
    LBefore := LHost.Session.ProjectSnapshot;
    LHost.PresentAndQueue;
    Check((LHost.Writes = 0) and (LHost.ChangedWrites = 0), 'Popup dispatch changes no physical buffer');
    Check(LHost.Input.TakeDefinitionNavigation(LLine, LColumn) and (LLine = 2) and (LColumn = 11),
      'Queued navigation maps authored CRLF/Unicode to the expected source line');
    Check((LHost.Session.DraftSource = CBuffer) and
      (EncodeNyxProject(LHost.Session.ProjectSnapshot) = EncodeNyxProject(LBefore)) and
      not LHost.Input.Commands.CanExecute(nhcUndo) and (LHost.ChangedWrites = 0),
      'Navigation preserves the whole pending project and adds no draft/history publication');
    Check(not LHost.Input.TakeDefinitionNavigation(LLine, LColumn), 'A queued navigation consumes once');
    LHost.PresentAndQueue;
    LText := LHost.Physical.Text;
    LHost.Physical := NyxEditingSnapshot(nepSelectionChange, neiUnknown, LText, '', False,
      NyxTextSelection(LText, 0, 0), False, False);
    Check(not LHost.Input.TakeDefinitionNavigation(LLine, LColumn),
      'Caret movement between click and queued host turn revokes navigation');
  finally
    LHost.Free;
  end;
  for LAt := 0 to 1 do
  begin
    LHost := THost.Create;
    try
      LText := TNyxText('// 🚀 𐐷') + #13 + 'procedure WithText;' + #13#10 +
        'procedure WithTheme;' + #10 + 'LBadge.Wi';

      if LAt = 0 then
      begin
        LText := NyxPresentText(CBuffer, nteCR);
      end;
      LHost.Session.SetSourceDraft(LText);
      LBefore := LHost.Session.ProjectSnapshot;
      LText := NyxPresentText(LText, nteLF);
      LHost.Physical := NyxEditingSnapshot(nepSelectionChange, neiUnknown, LText, '', False,
        NyxTextSelection(LText, NyxTextScalarCount(LText), NyxTextScalarCount(LText)), False, False);
      LHost.Input.Bind;
      LHost.PresentAndQueue;
      Check(LHost.Input.TakeDefinitionNavigation(LLine, LColumn) and
        (LLine = 2) and (LColumn = 11),
        'CR-only/mixed authored lines navigate the same displayed declaration ' + IntToStr(LAt));
      Check((EncodeNyxProject(LHost.Session.ProjectSnapshot) = EncodeNyxProject(LBefore)) and
        not LHost.Input.Commands.CanExecute(nhcUndo),
        'Logical line mapping preserves original authored endings and history');
    finally
      LHost.Free;
    end;
  end;
  for LAt := 0 to 5 do
  begin
    LHost := THost.Create;
    try
      LHost.PresentAndQueue;
      case LAt of
        0: LHost.Context.Enabled := False;
        1: LHost.Session.SetSourceDraft(CBuffer + ' ');
        2: LHost.Writable := False;
        3:
          begin
            LHost.Session.LoadProject(LHost.Session.ProjectSnapshot);
          end;
        4:
          begin
            LOther := TContext.Create;
            LHost.ContextLease := LOther;
          end;
        5: LHost.Key(nkDownKey);
      end;
      Check(not LHost.Input.TakeDefinitionNavigation(LLine, LColumn),
        'Changed service/draft/mount/project/context/row refuses queued navigation ' + IntToStr(LAt));
    finally
      LHost.Free;
      LOther := nil;
    end;
  end;
  for LAt := 0 to 3 do
  begin
    LHost := THost.Create;
    try
      LText := CSecondPrefix + 'WithTheme;' + #13#10 + 'LBadge.WithText';
      LHost.Session.SetSourceDraft(LText);
      LBefore := LHost.Session.ProjectSnapshot;
      LText := NyxPresentText(LText, nteLF);
      LHost.Physical := NyxEditingSnapshot(nepSelectionChange, neiUnknown, LText, '', False,
        NyxTextSelection(LText, NyxTextScalarCount(LText), NyxTextScalarCount(LText)), False, False);
      LHost.Input.Bind;

      if LAt = 0 then LHost.Key(nkF12Key)
      else LHost.Input.RequestDefinition;
      Check(LHost.Context.DefinitionMode and (LHost.Context.Reply <> nil),
        'F12 and visible-action entry use the specialized definition service');

      if LAt = 3 then LHost.Context.DefinitionTargets := 2;
      LHost.Context.Deliver;

      if LAt = 1 then
      begin
        LHost.Physical := NyxEditingSnapshot(nepSelectionChange, neiUnknown, LText, '', False,
          NyxTextSelection(LText, 0, 0), False, False);
        Check(not LHost.Input.TakeDefinitionNavigation(LLine, LColumn),
          'A caret change revokes an automatically queued single definition');
      end
      else if LAt = 2 then
      begin
        LHost.Context.Enabled := False;
        Check(not LHost.Input.TakeDefinitionNavigation(LLine, LColumn),
          'Retired service revokes an automatically queued single definition');
      end
      else if LAt = 3 then
      begin
        Check(not LHost.Input.TakeDefinitionNavigation(LLine, LColumn),
          'Multiple declared alternatives never select the first target automatically');
        LDocument := LHost.Input.BuildCompletion;
        try
          Check(LDocument <> nil, 'Alternatives use the shared Nyx-built popup');
          LPage := LDocument.Pages[0];
          Check(LHost.Input.RouteCompletion(LPage,
            LPage.Find('studio-completion-row-1-choose'), ntClick),
            'A visible declaration choice queues its selected target');
        finally
          LDocument.Free;
        end;
        Check(LHost.Input.TakeDefinitionNavigation(LLine, LColumn) and
          (LLine = 3) and (LColumn = 11), 'Chosen alternative navigates its own declaration');
      end
      else
      begin
        Check(LHost.Input.TakeDefinitionNavigation(LLine, LColumn) and
          (LLine = 2) and (LColumn = 11), 'A single exact target navigates on the next host turn');
      end;
      Check((EncodeNyxProject(LHost.Session.ProjectSnapshot) = EncodeNyxProject(LBefore)) and
        not LHost.Input.Commands.CanExecute(nhcUndo) and (LHost.ChangedWrites = 0),
        'Direct definition inspection preserves all draft bytes and text Undo');
    finally
      LHost.Free;
    end;
  end;
  for LAt := 0 to 1 do
  begin
    LHost := THost.Create;
    try
      LSecondOwner := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('SecondOwner'));
      { Model a retired outline owner's selector without inventing supporting
        file construction. Real auxiliary project admission requires its actual
        compiled receipt; this controlled port proves live-owner revocation only. }
      LHost.UseExplicitOwner := True;
      LHost.ExplicitOwner := TNyxPascalSourceRef.Main;
      LHost.Input.Bind;
      LBefore := LHost.Session.ProjectSnapshot;
      LHost.Input.RequestDefinition;
      Check(LHost.Context.Query.FileReference.SameFile(TNyxPascalSourceRef.Main) and
        LHost.Context.Query.Owner.SameFile(TNyxPascalSourceRef.Main),
        'The shared input query captures the exact current owner selector');

      if LAt = 0 then LHost.ExplicitOwner := LSecondOwner;
      LHost.Context.Deliver;

      if LAt = 1 then LHost.ExplicitOwner := LSecondOwner;
      Check(not LHost.Input.TakeDefinitionNavigation(LLine, LColumn),
        'Owner-only change revokes lookup before reply/queued jump ' + IntToStr(LAt));
      Check(LHost.Session.ActiveSourceFile.SameFile(TNyxPascalSourceRef.Main) and
        (EncodeNyxProject(LHost.Session.ProjectSnapshot) = EncodeNyxProject(LBefore)) and
        not LHost.Input.Commands.CanExecute(nhcUndo),
        'Owner recheck changes no source selector, buffers or text history');
    finally
      LHost.Free;
    end;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' Studio definition checks';
    {$else}
    WriteLn('PASS ', GChecks, ' Studio definition checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, 'FAIL ', E.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
