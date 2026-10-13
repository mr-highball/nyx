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



program nyx_query_studio_browser;
{$mode delphi}{$H+}{$codepage utf8}
uses
  SysUtils, JS, Web, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.codegen,
  nyx.state, nyx.collections, nyx.collections.query, nyx.collections.query.editor,
  nyx.generated.view, nyx.studio.projects, nyx.studio.agents, nyx.studio.browser,
  nyx.studio.exchange, nyx.studio.collectionedits, nyx.test.editor.exchange;

type
  { Ordinary controller with the maintained asynchronous semantic exchange.
    Only transport is supplied; shell, public Nyx controls and handlers are stock. }
  TPreviewStudio = class(TNyxStudio)
  protected
    function CreateEditorExchange: TNyxStudioEditorExchange; override;
  end;

const
  CEditor = 'inspector-collection-query';

var
  GCore: TNyxAgentSession;
  GStudio: TPreviewStudio;
  GExchange: TNyxTestEditorExchange; { borrowed until Studio destruction }
  GBefore: TNyxText;
  GRedo: Integer;
  GUndo: Integer;
  GChecks: Integer;
  GStage: Integer;
  GStarted: Double;
  GFinished: Boolean;
  GSelection: Integer;

function TPreviewStudio.CreateEditorExchange: TNyxStudioEditorExchange;
begin
  GExchange := TNyxTestEditorExchange.Create(GCore);
  Result := GExchange;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Ordinary query Studio: ' + AReason);
  end;
  Inc(GChecks);
end;

function Pair: TNyxText;
begin
  Result := EncodeNyxProject(GCore.RecoveryFrame.Session.Pair);
end;

function Face(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
end;

function Value(const AID: TNyxText): TNyxText;
var
  LFace: TJSHTMLElement;
begin
  LFace := Face(AID);
  Check(LFace <> nil, 'mounted ' + AID);

  if (LFace.tagName <> 'INPUT') and (LFace.tagName <> 'TEXTAREA') and
    (LFace.tagName <> 'SELECT') then
  begin
    LFace := TJSHTMLElement(LFace.querySelector('input,textarea,select'));
    Check(LFace <> nil, 'mounted input for ' + AID);
  end;
  Result := TJSHTMLTextAreaElement(LFace).value;
end;

{ Polling readiness does not add assertions: presentation timing may require
  several turns on one host. The admitted stage below still checks its value. }
function HasValue(const AID, AExpected: TNyxText): Boolean;
var
  LFace: TJSHTMLElement;
begin
  Result := False;
  LFace := Face(AID);

  if LFace = nil then
  begin
    Exit;
  end;

  if (LFace.tagName <> 'INPUT') and (LFace.tagName <> 'TEXTAREA') and
    (LFace.tagName <> 'SELECT') then
  begin
    LFace := TJSHTMLElement(LFace.querySelector('input,textarea,select'));
  end;

  if LFace <> nil then
  begin
    Result := TJSHTMLTextAreaElement(LFace).value = AExpected;
  end;
end;

procedure Preserved;
var
  LFrame: TNyxAgentRecoveryFrame;
begin
  LFrame := GCore.RecoveryFrame;
  Check(EncodeNyxProject(LFrame.Session.Pair) = GBefore,
    'accepted design/Pascal and unfinished source/base remain exact');
  Check((Length(LFrame.Session.Redo) = GRedo) and (Length(LFrame.Session.Undo) = GUndo),
    'query exploration retains existing Undo/Redo');
end;

procedure Select(const AID: TNyxText; AActivate: Boolean = False);
begin
  { Selection/navigation use the semantic contract, never canvas automation. }
  Inc(GSelection);
  GCore.Call('nyx_select', 'Query inspector review', NyxObject([
    NyxField('expectedRevision', NyxData(GCore.Revision)),
    NyxField('operationId', NyxData('select-' + IntToStr(GSelection) + '-' + AID)),
    NyxField('id', NyxData(AID)), NyxField('activate', NyxData(AActivate))]), 'query-studio-owner');
end;

procedure Stage(const AName: TNyxText);
begin
  document.body.setAttribute('data-query-studio-stage', AName);
  document.body.removeAttribute('data-query-studio-action');
end;

procedure Retire;
begin
  GFinished := True;
  FreeAndNil(GStudio);
  GExchange := nil;
  FreeAndNil(GCore);
end;

procedure Drive;
var
  LAction: TNyxText;
  LText: TNyxText;
  LDocument: TNyxDocument;
begin
  try

    if GFinished then
    begin
      Exit;
    end;

    if TJSDate.now - GStarted > 180000 then
    begin
      raise Exception.Create('Timed out at ordinary query stage ' + IntToStr(GStage));
    end;

    if GExchange <> nil then
    begin

      if GExchange.RequestPending then
      begin
        GExchange.Deliver;
      end
      else if GExchange.TickPending then
      begin
        GExchange.FireTick;
      end;
    end;

    if GStudio.SourceBusy or GStudio.PresentationPending or
      not GStudio.SharedSourceSynchronized then
    begin
      window.setTimeout(@Drive, 20);
      Exit;
    end;
    LAction := document.body.getAttribute('data-query-studio-action');
    case GStage of
      0:
        begin
          Check(Face('measured-table') <> nil, 'ordinary canvas receives exact semantic companion');
          GBefore := Pair;
          GRedo := Length(GCore.RecoveryFrame.Session.Redo);
          GUndo := Length(GCore.RecoveryFrame.Session.Undo);
          Check(GCore.RecoveryFrame.Session.Pair.Pending and (GRedo > 0),
            'fixture retains a real source draft and Redo entry');
          Stage('open');
          GStage := 1;
        end;
      1:
        begin

          if LAction = 'opened' then
          begin
            Check(Value(NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue)) = 'Explore',
              'stock inspector exposes the saved typed text predicate');
            Stage('text');
            GStage := 2;
          end;
        end;
      2:
        begin

          if LAction = 'text-preview' then
          begin
            LText := Value(NyxQueryEditorPreviewOutputID(CEditor));
            Check((Pos('1 of 3 rows', LText) > 0) and (Pos('Plan the next chapter', LText) > 0),
              'actual input previews the unapplied text query');
            Check(Pos('.Contains(''Plan'')', LText) > 0, 'stock report shows crafted typed Pascal');
            Check(TJSHTMLTextAreaElement(Face(NyxQueryEditorPreviewOutputID(CEditor))).readOnly,
              'query report is a read-only Nyx code editor');
            Preserved;
            LDocument := TNyxCodec.Decode(GCore.RecoveryFrame.Session.Pair.Design);
            try
              Check(LDocument.Find('measured-table').CollectionView.QueryPolicy.Filter.Expected.TextValue =
                'Explore', 'preview never installs the candidate policy');
            finally
              LDocument.Free;
            end;
            Stage('park');
            GStage := 3;
          end;
        end;
      3:
        begin

          if LAction = 'parked' then
          begin
            Check(Pos('1 of 3 rows', Value(NyxQueryEditorPreviewOutputID(CEditor))) > 0,
              'ordinary inspector tab parking restores the copied report');
            Check(Value(NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue)) = 'Plan',
              'unapplied form survives unrelated section refresh');
            Preserved;
            Stage('text-capture');
            GStage := 4;
          end;
        end;
      4:
        begin

          if LAction = 'captured-text' then
          begin
            Select('measured-list');
            GStage := 5;
          end;
        end;
      5:
        begin

          if (Face(NyxQueryEditorPreviewOutputID(CEditor)) <> nil) and
            HasValue(NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue), '0') then
          begin
            Check(Pos('1 of 3 rows', Value(NyxQueryEditorPreviewOutputID(CEditor))) = 0,
              'selection retires the previous owner report');
            Check(Value(NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue)) = '0',
              'next owner exposes its numeric predicate');
            Stage('number');
            GStage := 6;
          end;
        end;
      6:
        begin

          if LAction = 'malformed' then
          begin
            Check(Pos('Preview refused /', Value(NyxQueryEditorPreviewOutputID(CEditor))) > 0,
              'malformed numeric input gives visible refusal');
            Check(Value(NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue)) = '-',
              'refusal retains incomplete user text');
            Preserved;
            Stage('correct');
            GStage := 7;
          end;
        end;
      7:
        begin

          if LAction = 'corrected' then
          begin
            Check(Pos('3 of 3 rows', Value(NyxQueryEditorPreviewOutputID(CEditor))) > 0,
              'corrected numeric draft previews all matching defaults');
            Preserved;
            Stage('number-capture');
            GStage := 8;
          end;
        end;
      8:
        begin

          if LAction = 'captured-number' then
          begin
            Select('next-view', True);
            GStage := 9;
          end;
        end;
      9:
        begin

          if Face('next-title') <> nil then
          begin
            Check(Face(NyxQueryEditorPreviewOutputID(CEditor)) = nil,
              'navigation unmounts the old query inspector');
            Preserved;
            Select('measured-view', True);
            Select('measured-table');
            Stage('return');
            GStage := 10;
          end;
        end;
      10:
        begin

          if (Face(NyxQueryEditorPreviewOutputID(CEditor)) <> nil) and
            HasValue(NyxQueryEditorFieldID(NyxQueryEditorPredicateID(CEditor), nqfValue), 'Explore') then
          begin
            Check(Pos('3 of 3 rows', Value(NyxQueryEditorPreviewOutputID(CEditor))) = 0,
              'returning to a page cannot resurrect another owner report');
            Preserved;
            Stage('done');
            Retire;
            document.body.setAttribute('data-query-studio', 'passed');
            document.body.setAttribute('data-query-studio-disposed', 'true');
            document.body.setAttribute('data-query-studio-checks', IntToStr(GChecks));
            Exit;
          end;
        end;
    end;
    window.setTimeout(@Drive, 20);
  except
    on LException: Exception do
    begin
      Retire;
      document.body.setAttribute('data-query-studio', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
    end;
  end;
end;

procedure Start;
var
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LFrame: TNyxAgentRecoveryFrame;
begin
  try
    GStarted := TJSDate.now;
    LDocument := BuildNyxDocument;
    try
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    finally
      LDocument.Free;
    end;
    GCore := TNyxAgentSession.Create(LPair);
    Select('measured-view', True);
    Select('measured-table');
    GCore.Call('nyx_collections', 'Query inspector review', NyxObject([
      NyxField('mode', NyxData('apply')), NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('history-seed')), NyxField('changes', NyxCollectionPatch([
        NyxUpdateCollectionRow(NyxCollectionItem(NyxItem(NyxCollection('measured'), 'row-0'))
          .WithValue(NyxIntegerField('priority'), 1))]).ToData)]), 'query-studio-owner');
    GCore.Call('nyx_history', 'Query inspector review', NyxObject([
      NyxField('expectedRevision', NyxData(GCore.Revision)),
      NyxField('operationId', NyxData('history-undo')), NyxField('direction', NyxData('undo'))]),
      'query-studio-owner');
    LFrame := GCore.RecoveryFrame;
    LFrame.Session.Pair.Pending := True;
    LFrame.Session.Pair.DraftBase := LFrame.Session.Pair.Source;
    LFrame.Session.Pair.Draft := LFrame.Session.Pair.Source + TNyxText(#10 + '// Unfinished preview draft');
    FreeAndNil(GCore);
    GCore := TNyxAgentSession.CreateRecovered(LFrame);
    GStudio := TPreviewStudio.Create;
    GStudio.Run(False);
    GStudio.ConnectAgents;
    Drive;
  except
    on LException: Exception do
    begin
      Retire;
      document.body.setAttribute('data-query-studio', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
    end;
  end;
end;

begin
  window.setTimeout(@Start, 0);
end.
