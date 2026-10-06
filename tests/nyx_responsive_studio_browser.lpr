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
program nyx_responsive_studio_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, JS, Web, nyx.text, nyx.studio.browser, nyx.studio.inspector;

const
  CResult = 'data-nyx-responsive-studio';
  CChecks = 'data-nyx-responsive-studio-checks';
  CError = 'data-nyx-responsive-studio-error';
  CDraft = 'Retain this independent English draft.';

var
  GStudio: TNyxStudio;
  GFrame: TJSHTMLIFrameElement;
  GEditor: TJSHTMLTextAreaElement;
  GInput: TJSHTMLTextAreaElement;
  GBefore: TNyxText;
  GAfter: TNyxText;
  GStage: Integer;
  GPolls: Integer;
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
end;

function Required(const AID: TNyxText): TJSHTMLElement;
begin
  Result := Find(AID);

  if Result = nil then
  begin
    raise Exception.Create('Missing ordinary Studio control: ' + AID);
  end;
end;

function Field(const AID: TNyxText): TJSHTMLElement;
begin
  Result := Required(AID);

  if not ((Result is TJSHTMLTextAreaElement) or (Result is TJSHTMLInputElement) or
    (Result is TJSHTMLSelectElement)) then
  begin
    Result := TJSHTMLElement(Result.querySelector('input,textarea,select'));
  end;

  if Result = nil then
  begin
    raise Exception.Create('Missing actual Studio input: ' + AID);
  end;
end;

procedure Change(const AID, AValue: TNyxText);
var
  LField: TJSHTMLElement;
begin
  LField := Field(AID);
  TJSHTMLInputElement(LField).value := AValue;
  LField.dispatchEvent(TJSEvent.new('change'));
end;

procedure Failed(const AReason: TNyxText);
begin
  document.body.setAttribute(CResult, 'failed');
  document.body.setAttribute(CError, AReason + ' / stage ' + IntToStr(GStage) +
    ' / checks ' + IntToStr(GChecks));
end;

procedure RetainedInput;
begin
  Check(Field('notes-editor') = GInput, 'Panel changes and admission retain the same canvas input');
  Check(GInput.value = CDraft, 'The independent uncommitted English draft survives');
  Check((GInput.selectionStart = 5) and (GInput.selectionEnd = 9),
    'The independent input keeps its exact selection range');
  Check(Field('studio-code') = GEditor, 'The adjacent source editor keeps its identity');
end;

procedure Poll;
var
  LStatus: TJSHTMLElement;
  LActual: TNyxText;
  LDifference: Integer;
begin
  try
    Inc(GPolls);
    { Expose bounded progress for the observing Pascal host when a real
      worker/history journey fails to finish. No source or design is exported. }
    document.body.setAttribute('data-nyx-responsive-studio-stage', IntToStr(GStage));
    document.body.setAttribute(CChecks, IntToStr(GChecks));

    if GPolls > 1000 then
    begin
      raise Exception.Create('Ordinary responsive Studio journey timed out at stage ' + IntToStr(GStage));
    end;
    case GStage of
      0:
        begin
          LStatus := Find('studio-agents-status');

          if (LStatus <> nil) and (Pos('Agents edit', LStatus.textContent) > 0) and
            (Find('workspace') <> nil) then
          begin
            Check(Pos('Room for ideas', document.title) > 0,
              'Ordinary agent connection loads the explicitly addressed MCP project');
            Check((window.getComputedStyle(Required('studio-agents-endpoint'))
              .getPropertyValue('display') = 'none') = (window.innerWidth < 640),
              'Typed visibility removes redundant agent transport information only at compact widths');
            Required('action-agents').click;
            Required('action-code').click;
            GEditor := TJSHTMLTextAreaElement(Field('studio-code'));
            Check(Pos('Messages (', Required('action-messages-tab').textContent) > 0,
              'The constrained source pane uses its typed compact caption');
            GBefore := GEditor.value;
            Check((Pos('INyxMemo', GBefore) > 0) and
              {$ifdef NYX_PRESENTATION_CONSUMER}
              (Pos('.WhenPresentation(NyxPresentation(''compact''))', GBefore) > 0),
              {$else}
              (Pos('TNyxViewportWidth.Below(640)', GBefore) > 0),
              {$endif}
              'Observed accepted source retains specialized typed MCP authoring');
            Required('workspace').click;
            GInput := TJSHTMLTextAreaElement(Field('notes-editor'));
            { Deliberately do not dispatch a change: this draft belongs to the
              live target input, independently of accepted design/source/history. }
            GInput.value := CDraft;
            GInput.selectionStart := 5;
            GInput.selectionEnd := 9;

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-inspector').click;
            end;
            Check(Pos('workspace', Required('selected-label').textContent) > 0,
              'Real canvas selection exposes its ordinary responsive Inspector');
            Change(NyxStudioViewportMinimumID, '0');
            Change(NyxStudioViewportMaximumID, '0');
            Change(NyxStudioViewportHeightMaximumID, '300');
            Change(NyxStudioViewportOrientationID, 'landscape');
            Change(NyxStudioViewportLayoutID, 'row');
            Check(GEditor.value = GBefore, 'Rule form drafts do not change accepted Pascal');
            Required(NyxStudioViewportApplyID).click;
            Check(Pos('Preparing', Required('studio-status').textContent) = 1,
              'The real Inspector button starts independent worker preparation');
            GStage := 1;
          end;
        end;
      1:
        begin

          if Pos('Design / Pascal updated', Required('studio-status').textContent) = 1 then
          begin

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-design').click;
            end;
            GAfter := TJSHTMLTextAreaElement(Field('studio-code')).value;
            Check((GAfter <> GBefore) and
              (Pos('TNyxViewportCondition.Any.HeightBelow(300).Orientation(nvoLandscape)', GAfter) > 0),
              'Actual browser worker admission publishes crafted responsive Pascal');
            RetainedInput;
            Required('action-undo').click;
            GStage := 2;
          end;
        end;
      2:
        begin

          if TJSHTMLTextAreaElement(Field('studio-code')).value = GBefore then
          begin
            Check(True, 'One ordinary editor Undo restores exact accepted Pascal');
            RetainedInput;
            Required('action-redo').click;
            GStage := 3;
          end;
        end;
      3:
        begin

          if TJSHTMLTextAreaElement(Field('studio-code')).value = GAfter then
          begin
            Check(True, 'One ordinary editor Redo restores exact admitted Pascal');
            RetainedInput;
            {$ifdef NYX_PRESENTATION_CONSUMER}
            Required('action-undo').click;
            GStage := 4;
            {$else}
            document.body.setAttribute(CResult, 'passed');
            document.body.setAttribute(CChecks, IntToStr(GChecks));
            { Keep the actual Studio mounted for the observing driver capture.
              Its owned browser profile tears down the transport after capture. }
            Exit;
            {$endif}
          end;
        end;
      {$ifdef NYX_PRESENTATION_CONSUMER}
      4:
        begin

          if TJSHTMLTextAreaElement(Field('studio-code')).value = GBefore then
          begin

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-inspector').click;
            end;
            Change(NyxStudioPresentationNameID, 'compact');
            Change(NyxStudioViewportMinimumID, '0');
            Change(NyxStudioViewportMaximumID, '900');
            Change(NyxStudioViewportHeightMinimumID, '0');
            Change(NyxStudioViewportHeightMaximumID, '0');
            Change(NyxStudioViewportOrientationID, 'any');
            Required(NyxStudioPresentationDefineID).click;
            GStage := 5;
          end;
        end;
      5:
        begin

          if Pos('Design / Pascal updated', Required('studio-status').textContent) = 1 then
          begin
            if window.innerWidth <= 960 then
            begin
              Required('action-panel-design').click;
            end;
            GAfter := TJSHTMLTextAreaElement(Field('studio-code')).value;
            Check(Pos('TNyxViewportCondition.Any.WidthBelow(900)', GAfter) > 0,
              'Real browser Inspector/worker updates the shared definition');
            RetainedInput;
            Required('action-undo').click;
            GStage := 6;
          end;
        end;
      6:
        begin

          if TJSHTMLTextAreaElement(Field('studio-code')).value = GBefore then
          begin
            Check(True, 'One synchronized Undo restores the exact named definition source');
            RetainedInput;
            Required('action-redo').click;
            GStage := 7;
          end;
        end;
      7:
        begin

          if TJSHTMLTextAreaElement(Field('studio-code')).value = GAfter then
          begin
            Check(True, 'One synchronized Redo restores the exact shared definition source');

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-inspector').click;
            end;
            Change(NyxStudioPresentationChoiceID, 'compact');
            Change(NyxStudioPresentationAttributeID, 'visible');
            Required(NyxStudioPresentationUseID).click;
            GStage := 8;
          end;
        end;
      8:
        begin

          if Pos('Design / Pascal updated', Required('studio-status').textContent) = 1 then
          begin

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-design').click;
            end;
            Check(Pos('.Visible(True)', TJSHTMLTextAreaElement(Field('studio-code')).value) > 0,
              'Real browser Inspector adds a strongly typed presentation override');

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-inspector').click;
            end;
            Change(NyxStudioPresentationChoiceID, 'compact');
            Change(NyxStudioPresentationAttributeID, 'visible');
            Required(NyxStudioPresentationResetID).click;
            GStage := 9;
          end;
        end;
      9:
        begin

          if Pos('Design / Pascal updated', Required('studio-status').textContent) = 1 then
          begin

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-design').click;
            end;

            if TJSHTMLTextAreaElement(Field('studio-code')).value = GAfter then
            begin
              Check(True, 'Reset removes only its exact named override and retains the shared definition');
              RetainedInput;
              {$ifdef NYX_MANUAL_CONSUMER}
              Change(NyxStudioPresentationPreviewID, 'Manual / wide workspace');
              Check(window.getComputedStyle(Required('workspace')).getPropertyValue('flex-direction') = 'row',
                'Actual browser preview chooser selects the manual row');
              Check(TJSHTMLTextAreaElement(Field('studio-code')).value = GAfter,
                'Manual preview changes neither accepted Pascal nor source editor text');
              RetainedInput;
              Change(NyxStudioPresentationPreviewID, 'Manual / focused');
              Check(window.getComputedStyle(Required('workspace')).getPropertyValue('flex-direction') = 'column',
                'Another browser manual choice replaces the layout');
              Check(window.getComputedStyle(Required('workspace')).getPropertyValue('gap') = '4px',
                'Manual browser preview applies its exact scoped configuration');
              RetainedInput;
              Change(NyxStudioPresentationPreviewID, 'Automatic / defaults');
              Check(window.getComputedStyle(Required('workspace')).getPropertyValue('gap') = '8px',
                'Automatic preview restores the shared host presentation');

              if window.innerWidth <= 960 then
              begin
                Required('action-panel-inspector').click;
              end;
              Change(NyxStudioPresentationNameID, 'reading');
              Change(NyxStudioPresentationActivationID, 'manual');
              Required(NyxStudioPresentationDefineID).click;
              Check(Pos('Preparing', Required('studio-status').textContent) = 1,
                'Manual definition starts the ordinary worker: ' + Required('studio-status').textContent);
              GStage := 10;
              {$else}
              document.body.setAttribute(CResult, 'passed');
              document.body.setAttribute(CChecks, IntToStr(GChecks));
              Exit;
              {$endif}
            end;
          end;
        end;
      {$endif}
      {$ifdef NYX_MANUAL_CONSUMER}
      10:
        begin

          if Pos('Design / Pascal updated', Required('studio-status').textContent) = 1 then
          begin

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-design').click;
            end;
            Check(Pos('NyxPresentation(''reading'')', TJSHTMLTextAreaElement(Field('studio-code')).value) > 0,
              'Actual browser Inspector/worker adds the exact manual definition');
            Check(Pos('TNyxPresentationCondition.Manual', TJSHTMLTextAreaElement(Field('studio-code')).value) > 0,
              'Ordinary browser manual authoring produces the dedicated Pascal construct');
            RetainedInput;
            Required('action-undo').click;
            GStage := 11;
          end;
        end;
      11:
        begin
          LActual := TJSHTMLTextAreaElement(Field('studio-code')).value;
          LDifference := 1;
          while (LDifference <= Length(LActual)) and (LDifference <= Length(GAfter)) and
            (LActual[LDifference] = GAfter[LDifference]) do
          begin
            Inc(LDifference);
          end;
          document.body.setAttribute('data-nyx-responsive-studio-comparison',
            IntToStr(Length(LActual)) + '/' + IntToStr(Length(GAfter)) +
            '/offset=' + IntToStr(LDifference) + '/actual=' + Copy(LActual, LDifference, 60) +
            '/expected=' + Copy(GAfter, LDifference, 60));

          if TJSHTMLTextAreaElement(Field('studio-code')).value = GAfter then
          begin
            Check(True, 'One synchronized browser Undo restores the exact pair before manual definition editing');
            RetainedInput;

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-inspector').click;
            end;
            Change(NyxStudioPresentationNameID, 'compact');
            Change(NyxStudioPresentationActivationID, 'automatic');
            Change(NyxStudioPresentationContainerID, 'workspace space');
            Required(NyxStudioPresentationDefineID).click;
            GStage := 12;
          end;
        end;
      12:
        begin

          if Pos('Design / Pascal updated', Required('studio-status').textContent) = 1 then
          begin

            if window.innerWidth <= 960 then
            begin
              Required('action-panel-design').click;
            end;
            Check(Pos('TNyxPresentationCondition.Within(NyxContainer(''workspace space'')',
              TJSHTMLTextAreaElement(Field('studio-code')).value) > 0,
              'Actual browser Inspector/worker generates the exact fluent container condition');
            RetainedInput;
            Required('action-undo').click;
            GStage := 13;
          end;
        end;
      13:
        begin

          if TJSHTMLTextAreaElement(Field('studio-code')).value = GAfter then
          begin
            Check(True, 'One synchronized browser Undo restores the exact source before container editing');
            RetainedInput;
            document.body.setAttribute(CResult, 'passed');
            document.body.setAttribute(CChecks, IntToStr(GChecks));
            Exit;
          end;
        end;
      {$endif}
    end;
    window.setTimeout(@Poll, 25);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

procedure ObserveCompact;
var
  LBody: TJSHTMLElement;
  LResult: TNyxText;
begin
  Inc(GPolls);

  if GFrame.contentDocument <> nil then
  begin
    LBody := TJSHTMLElement(GFrame.contentDocument.body);
    LResult := LBody.getAttribute(CResult);

    if LResult = 'passed' then
    begin
      document.body.setAttribute(CResult, 'passed');
      document.body.setAttribute(CChecks, LBody.getAttribute(CChecks));
      Exit;
    end;

    if LResult = 'failed' then
    begin
      Failed(LBody.getAttribute(CError));
      Exit;
    end;
  end;

  if GPolls > 600 then
  begin
    Failed('Exact-390 Studio journey did not finish');
    Exit;
  end;
  window.setTimeout(@ObserveCompact, 100);
end;

begin
  { The project must be prepared semantically. The fixture only drives input
    behavior that the document API cannot establish; it never imports or authors
    its own replacement document. Synthetic clicks are not trusted hardware. }

  if Pos('workspace=', window.location.search) = 0 then
  begin
    Failed('Supply an explicit MCP-owned responsive workspace in the URL');
  end
  else if Pos('host=1&', window.location.search) > 0 then
  begin
    GFrame := TJSHTMLIFrameElement(document.createElement('iframe'));
    GFrame.style.cssText := 'width:390px;height:900px;border:0;display:block;';
    TJSHTMLElement(document.body).style.setProperty('margin', '0');
    GFrame.src := window.location.pathname +
      StringReplace(window.location.search, 'host=1&', '', []);
    document.body.appendChild(GFrame);
    window.setTimeout(@ObserveCompact, 100);
  end
  else
  begin
    GStudio := TNyxStudio.Create;
    GStudio.Run(False);
    GStudio.ConnectAgents;
    window.setTimeout(@Poll, 25);
  end;
end.
