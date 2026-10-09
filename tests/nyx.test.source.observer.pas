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

unit nyx.test.source.observer;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Ordinary browser/Win32 Studios observe one independent semantic session through
  the existing suspended exchange. Qualifies real controller/source/canvas/history
  consumption; it does not qualify sockets, authentication or installed rollout. }
procedure RunNyxUnitObserver;

implementation

uses
  SysUtils, Classes, nyx.text, nyx.data, nyx.studio.agents, nyx.studio.projects,
  nyx.studio.exchange, nyx.studio.workspaces, nyx.test.editor.exchange
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser
  {$else}, Forms, StdCtrls, nyx.studio.lcl, nyx.test.capture.lcl{$endif};

const
  COriginal: TNyxText = 'Your next idea';
  CChanged: TNyxText = 'A thoughtful workspace';
  CNoteLine: TNyxText = '// An agent-authored application note 🌿';
  CNote: TNyxText = '// An agent-authored application note 🌿' + #10;

type
  TTestStudio = class({$ifdef PAS2JS}TNyxStudio{$else}TNyxNativeStudio{$endif})
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;

  TJourney = class
  private
    FCore: TNyxAgentSession;
    FStudio: TTestStudio;
    FBefore: TNyxText;
    FAfter: TNyxText;
    FStage: Integer;
    FChecks: Integer;
    FFinished: Boolean;
    FStarted: {$ifdef PAS2JS}Double{$else}QWord{$endif};
    {$ifndef PAS2JS}
    FWindow: TForm;
    {$endif}
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Click(const AID: TNyxText);
    function Source: TNyxText;
    function Heading: TNyxText;
    function Pair: TNyxText;
    procedure Author;
    procedure Finish;
  public
    destructor Destroy; override;
    procedure Start;
    procedure Next;
    property Finished: Boolean read FFinished;
  end;

var
  GCore: TNyxAgentSession; { weak factory context; controller owns its transport }
  GExchange: TNyxTestEditorExchange; { borrowed until controller destruction }
  GJourney: TJourney;

function TTestStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  GExchange := TNyxTestEditorExchange.Create(GCore);
  Result := GExchange;
end;

destructor TJourney.Destroy;
begin
  FStudio.Free;
  GExchange := nil;
  GCore := nil;
  FCore.Free;
  {$ifndef PAS2JS}FWindow.Free;{$endif}
  inherited Destroy;
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Ordinary semantic source observer: ' + AReason);
  end;
  Inc(FChecks);
  {$ifndef PAS2JS}
  WriteLn('Source observer / ', FChecks, ' / ', AReason);
  Flush(Output);
  {$endif}
end;

procedure TJourney.Click(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LControl: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LControl := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if LControl = nil then
  begin
    raise Exception.Create('Missing ordinary control: ' + AID);
  end;
  LControl.click;
  {$else}
  TButton(FStudio.ShellView.ControlFor(AID)).Click;
  {$endif}
end;

function TJourney.Source: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLTextAreaElement(document.querySelector('[data-node="studio-code"]')).value;
  {$else}
  Result := TNyxText(RawByteString(TCustomMemo(FStudio.CodeView.InputFor('studio-code')).Text));
  {$endif}
end;

function TJourney.Heading: TNyxText;
begin
  {$ifdef PAS2JS}
  Result := TJSHTMLElement(document.querySelector('[data-node="form-title"]')).textContent;
  {$else}
  Result := TNyxText(TCustomLabel(FStudio.CanvasView.ControlFor('form-title')).Caption);
  {$endif}
end;

function TJourney.Pair: TNyxText;
begin
  Result := FCore.Exchange(NyxObject([NyxField('op', NyxData('observe')),
    NyxField('after', NyxData(0))])).Field('project').AsText;
end;

procedure TJourney.Author;
var
  LText: TNyxText;
  LWindow: TNyxDataValue;
  LOffset: Integer;
  LRevision: Integer;
  LCursor: Integer;
  LPosition: Integer;
  LScalar: Integer;
begin
  { All authoring uses semantic tools. Only focused accepted source windows are
    read, pinned to one revision; no DOM edit or whole-document mutation is used. }
  LRevision := FCore.Call('nyx_session', 'Source workshop', NyxObject([]),
    'owned-observer-agent').Field('revision').AsInteger;
  LText := '';
  LOffset := 0;
  repeat
    LWindow := FCore.Call('nyx_pascal', 'Source workshop', NyxObject([
      NyxField('mode', NyxData('unit')), NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('offset', NyxData(LOffset)), NyxField('count', NyxData(4096))]),
      'owned-observer-agent');
    LText := LText + LWindow.Field('text').AsText;
    LOffset := LWindow.Field('nextOffset').AsInteger;
  until LOffset = LWindow.Field('total').AsInteger;
  LPosition := Pos(COriginal, LText);
  Check(LPosition > 0, 'Bounded semantic source contains the exact heading anchor');
  LCursor := 1;
  LOffset := 0;
  while LCursor < LPosition do
  begin
    NyxNextScalar(LText, LCursor, LScalar);
    Inc(LOffset);
  end;
  FCore.Call('nyx_pascal', 'Source workshop', NyxObject([
    NyxField('mode', NyxData('edit-unit')), NyxField('expectedRevision', NyxData(LRevision)),
    NyxField('operationId', NyxData('observed-source-group')),
    NyxField('changes', NyxArray([
      NyxObject([NyxField('offset', NyxData(0)), NyxField('expected', NyxData('')),
        NyxField('replacement', NyxData(CNote))]),
      NyxObject([NyxField('offset', NyxData(LOffset)), NyxField('expected', NyxData(COriginal)),
        NyxField('replacement', NyxData(CChanged))])]))]), 'owned-observer-agent');
  FAfter := Pair;
  Check(FAfter <> FBefore, 'One semantic group changes the owned accepted pair');
  GExchange.FireTick;
  GExchange.Deliver;
end;

procedure TJourney.Start;
begin
  {$ifdef PAS2JS}FStarted := TJSDate.now;{$else}FStarted := GetTickCount64;{$endif}
  FCore := TNyxAgentSession.Create;
  GCore := FCore;
  {$ifdef PAS2JS}
  FStudio := TTestStudio.Create;
  FStudio.Run(False);
  FStudio.ConnectAgents;
  {$else}
  FWindow := TForm.CreateNew(nil);
  FWindow.SetBounds(40, 40, 1240, 820);
  FWindow.Show;
  FStudio := TTestStudio.Create(FWindow, '');
  FStudio.Run;
  FStudio.ConnectService('', NyxPrimaryWorkspace);
  {$endif}
  GExchange.Deliver;
  FStage := 1;
  {$ifdef PAS2JS}window.setTimeout(@Next, 20);{$endif}
end;

procedure TJourney.Finish;
begin
  FFinished := True;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-nyx-unit-studio', 'passed');
  document.body.setAttribute('data-test-count', IntToStr(FChecks));
  {$else}
  WriteLn('PASS ', FChecks, ' ordinary semantic source observer checks');
  {$endif}
end;

procedure TJourney.Next;
var
  LNow: {$ifdef PAS2JS}Double{$else}QWord{$endif};
begin
  {$ifdef PAS2JS}
  try
  {$endif}
    {$ifdef PAS2JS}LNow := TJSDate.now;{$else}LNow := GetTickCount64;{$endif}

    if LNow - FStarted > 120000 then
    begin
      raise Exception.Create('Ordinary source observer exceeded its deadline at stage ' +
        IntToStr(FStage) + ', presentation=' + BoolToStr(FStudio.PresentationPending, True) +
        ', source=' + BoolToStr(FStudio.SourceBusy, True));
    end;

    if not FStudio.PresentationPending and not FStudio.SourceBusy then
    begin
      case FStage of
        1:
          begin
            FBefore := Pair;
            Check(Heading = COriginal, 'Ordinary canvas shows the initial accepted heading');
            {$ifdef PAS2JS}

            if TJSHTMLElement(document.querySelector('[data-node="action-code"]'))
              .getBoundingClientRect.height = 0 then
            begin
              Click('action-actions');
              FStage := 10;
            end
            else
            {$endif}
            begin
              Click('action-code');
              FStage := 20;
            end;
          end;
        {$ifdef PAS2JS}
        10:
          begin
            Click('studio-menu-view');
            FStage := 11;
          end;
        11:
          begin
            Click('studio-menu-code');
            FStage := 20;
          end;
        {$endif}
        20:
          begin
            Check(Pos(COriginal, Source) > 0, 'Ordinary source pane shows accepted Pascal');
            Author;
            FStage := 30;
          end;
        30:
          begin
            Check(Heading = CChanged, 'Observing actual canvas receives semantic source change');
            Check((Pos(CChanged, Source) > 0) and (Pos(CNoteLine, Source) = 1),
              'Observing actual source pane receives exact helper note and changed view');
            Check(Pair = FAfter, 'Observing painting retains the exact accepted pair');
            {$ifndef PAS2JS}
            Check(EncodeNyxProject(FStudio.Session.ProjectSnapshot) = FAfter,
              'Ordinary native session mirrors the exact source/design pair');

            if ParamCount = 1 then
            begin
              SaveNyxNativeCapture(FWindow, ParamStr(1), ncmPrint);
            end;
            {$endif}
            FStage := 40;
            {$ifdef PAS2JS}
            document.body.setAttribute('data-capture-checkpoint', 'unit-source-observed');
            {$endif}
          end;
        40:
          begin
            {$ifdef PAS2JS}

            if document.body.getAttribute('data-capture-observed') <> 'unit-source-observed' then
            begin
              window.setTimeout(@Next, 20);
              Exit;
            end;
            {$endif}
            FCore.Call('nyx_history', 'Source workshop', NyxObject([
              NyxField('expectedRevision', NyxData(FCore.Revision)),
              NyxField('operationId', NyxData('observed-source-undo')),
              NyxField('direction', NyxData('undo'))]), 'owned-observer-agent');
            GExchange.FireTick;
            GExchange.Deliver;
            FStage := 50;
          end;
        50:
          begin
            Check(Pair = FBefore, 'One semantic Undo restores the exact original pair');
            Check(Heading = COriginal, 'Actual observing canvas receives paired Undo');
            Check((Pos(COriginal, Source) > 0) and (Pos(CNoteLine, Source) = 0),
              'Actual observing source pane receives paired Undo');
            Finish;
          end;
      end;
    end;
    {$ifdef PAS2JS}

    if FFinished then
    begin
      GJourney := nil;
      Free;
    end
    else
    begin
      window.setTimeout(@Next, 20);
    end;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-unit-studio', 'failed');
      document.body.setAttribute('data-test-failure', LException.Message);
      GJourney := nil;
      Free;
    end;
  end;
    {$endif}
end;

procedure RunNyxUnitObserver;
begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GJourney := TJourney.Create;
  {$ifdef PAS2JS}
  try
    GJourney.Start;
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-unit-studio', 'failed');
      document.body.setAttribute('data-test-failure', LException.Message);
      FreeAndNil(GJourney);
    end;
  end;
  {$else}
  try
    GJourney.Start;
    repeat
      { Native source preparation returns through the ordinary UI-thread queue.
        Pump it explicitly in this embedded host, as Application.Run would. }
      CheckSynchronize(0);
      Application.ProcessMessages;
      GJourney.Next;
      Sleep(1);
    until GJourney.Finished;
  finally
    FreeAndNil(GJourney);
  end;
  {$endif}
end;

end.
