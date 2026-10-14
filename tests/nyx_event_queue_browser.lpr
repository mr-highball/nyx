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

program nyx_event_queue_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Web, nyx.text, nyx.types, nyx.callbacks, nyx.scheduler,
  nyx.studio.inspector, nyx.studio.browser, nyx.event.discovery, nyx.studio.projects;

type
  TCallbackStage = (csStart, csOpenSource, csSelectNotes, csOpenInspector, csInspect, csFiltered,
    csReturnEvents, csAdded, csWarning, csReview, csConfirmed, csRequestAgain, csConfirmAgain,
    csRemoved, csUndone, csWriteDraft, csAddWithDraft, csDraftRefused, csRetire);

const
  CCaptureQuery = '?capture=1';
  CCheckpoint = 'data-capture-checkpoint';
  CObserved = 'data-capture-observed';

var
  GStudio: TNyxStudio;
  GStep: TCallbackStage;
  GChecks: Integer;
  GPolls: Integer;
  GBefore: TNyxText;
  GWithCallbacks: TNyxText;
  GKey: TNyxText;
  GCapture: Boolean;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Browser callback queue: ' + AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if (Result = nil) and (window.innerWidth < 900) then
  begin

    if (Pos('event-', AID) = 1) or (Pos('inspector-', AID) = 1) or
      (Pos(NyxStudioEventFilterID, AID) = 1) then
    begin
      TJSHTMLButtonElement(document.querySelector('[data-node=action-panel-inspector]')).click;
    end
    else
    begin
      TJSHTMLButtonElement(document.querySelector('[data-node=action-panel-design]')).click;
    end;
    Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));
  end;
  Check(Result <> nil, 'Actual Nyx control exists / ' + AID);
end;

function Field(const AID: TNyxText): TJSHTMLElement;
begin
  Result := Find(AID);

  if not (Result is TJSHTMLTextAreaElement) and not (Result is TJSHTMLSelectElement) and
    not (Result is TJSHTMLInputElement) then
  begin
    Result := TJSHTMLElement(Result.querySelector('textarea,select,input'));
  end;
  Check(Result <> nil, 'Actual Nyx input exists / ' + AID);
end;

procedure Click(const AID: TNyxText);
begin
  Find(AID).click;
end;

procedure Change(const AID, AValue: TNyxText);
var
  LField: TJSHTMLElement;
begin
  LField := Field(AID);
  LField.focus;

  if LField is TJSHTMLSelectElement then
  begin
    TJSHTMLSelectElement(LField).value := AValue;
  end
  else if LField is TJSHTMLInputElement then
  begin

    if LField.getAttribute('type') = 'checkbox' then
    begin
      TJSHTMLInputElement(LField).checked := AValue = 'true';
    end
    else
    begin
      TJSHTMLInputElement(LField).value := AValue;
    end;
  end
  else
  begin
    TJSHTMLTextAreaElement(LField).value := AValue;
  end;
  LField.dispatchEvent(TJSEvent.new('change'));
end;

function Source: TNyxText;
var
  LPair: TNyxProjectPair;
begin
  { Observe the actual controller even while compact navigation parks its editor.
    Source input itself is still edited through the real field below. }
  LPair := GStudio.ProjectSnapshot;
  Result := LPair.Source;

  if LPair.Pending then
  begin
    Result := LPair.Draft;
  end;
end;

procedure Step;
begin
  try
    { Only the maintained real-clock capture opts in to this fixture handshake.
      Product workers continue normally; the test waits before issuing its next
      command, so evidence observes actual stable controls without injected code. }

    if GCapture and (document.body.getAttribute(CCheckpoint) <> '') and
      (document.body.getAttribute(CObserved) <> document.body.getAttribute(CCheckpoint)) then
    begin
      window.setTimeout(@Step, 50);
      Exit;
    end;
    if (GStudio <> nil) and (GStudio.SourceBusy or GStudio.PresentationPending) then
    begin
      Inc(GPolls);

      if GPolls >= 3000 then
      begin
        raise Exception.Create('Compiled callback worker exceeded the bounded journey');
      end;
      window.setTimeout(@Step, 10);
      Exit;
    end;
    GPolls := 0;
    case GStep of
      csStart:
        begin
          { No recovery or service connection touches the observing user's pair. }
          GStudio := TNyxStudio.Create;
          GStudio.Run(False);
          document.body.setAttribute('data-event-control-width', IntToStr(window.innerWidth));
          Click('action-code');
        end;
      csOpenSource:
        begin
          Click('project-description');
        end;
      csSelectNotes:
        begin

          if window.innerWidth < 900 then
          begin
            Click('action-panel-inspector');
          end;
        end;
      csOpenInspector:
        begin
          Click(NyxInspectorEventsID);
        end;
      csInspect:
        begin
          GKey := 'event-' + NyxTriggerName(ntBeforeKeyPress);
          GBefore := Source;
          Change(NyxEventFilterPartID(NyxStudioEventFilterID, nefpQuery), 'KEY press');
          Check((Find(GKey).offsetHeight > 0) and
            (Find('event-click').offsetHeight = 0), 'Actual search combines case-insensitive terms');
          Change(NyxEventFilterPartID(NyxStudioEventFilterID, nefpCategory),
            NyxEventCategoryCaption(necFocus));
          Check(Find(GKey).offsetHeight = 0, 'Family and query combine');
          Change(NyxEventFilterPartID(NyxStudioEventFilterID, nefpCategory),
            NyxEventCategoryCaption(necKeyboard));
          Find(NyxStudioEventFilterID).scrollIntoView(True);

          if GCapture then
          begin
            document.body.setAttribute(CCheckpoint, 'filtered');
          end;
        end;
      csFiltered:
        begin
          Click(NyxEventFilterPartID(NyxStudioEventFilterID, nefpClear));
          Check((Find(GKey).offsetHeight > 0) and (Source = GBefore),
            'Clear restores the complete list without changing Pascal');
          Click(GKey + '-add');
          Check(Source = GBefore, 'Add keeps accepted Pascal during isolated preparation');
        end;
      csReturnEvents:
        begin

          if window.innerWidth < 900 then
          begin
            Click('action-panel-inspector');
          end;
        end;
      csAdded:
        begin
          Check((Pos('TODO', Source) > 0) and (Find(GKey + '-count').textContent = '1 registrations'),
            'Admitted callback owns a TODO implementation and visible registration');
          Change(NyxEventFilterPartID(NyxStudioEventFilterID, nefpRegistered), 'true');
          Check((Find(GKey).offsetHeight > 0) and
            (Find('event-after-key-press').offsetHeight = 0),
            'Actual Boolean filter selects registered events');
          Click(NyxEventFilterPartID(NyxStudioEventFilterID, nefpClear));
          Change(GKey + '-policy', NyxPolicyName(neAsynchronous));
          Change(GKey + '-policy', NyxPolicyName(neSequential));
          Change(GKey + '-policy', NyxPolicyName(neUIQueue));
          Check(TJSHTMLSelectElement(Field(GKey + '-policy')).value = NyxPolicyName(neUIQueue),
            'Latest waiting policy remains visible');
        end;
      csWarning:
        begin
          Check(TJSHTMLSelectElement(Field(GKey + '-policy')).value = NyxPolicyName(neUIQueue),
            'Worker publication retains latest event policy');
          GWithCallbacks := Source;
          Click(GKey + '-callback-0-remove');
        end;
      csReview:
        begin
          Check((Find('event-removal-warning') <> nil) and (Source = GWithCallbacks),
            'Removal request only exposes its warning');
          Check(Find('event-removal-warning').parentElement.getAttribute('data-node') =
            GKey + '-callback-0', 'Warning belongs to the exact callback row');
          Change(NyxEventFilterPartID(NyxStudioEventFilterID, nefpQuery), 'no matching callback');
          Check((Find(GKey).offsetHeight > 0) and (Pos('pending review shown',
            Find(NyxEventFilterPartID(NyxStudioEventFilterID, nefpStatus)).textContent) > 0) and
            (Source = GWithCallbacks), 'Filtering cannot conceal a pending removal review');
          Click(NyxEventFilterPartID(NyxStudioEventFilterID, nefpClear));
          Find('event-removal-warning').scrollIntoView(False);

          if GCapture then
          begin
            document.body.setAttribute(CCheckpoint, 'warning');
          end;
        end;
      csConfirmed:
        begin
          Click('event-removal-cancel');
          Check(Source = GWithCallbacks, 'Keep registration retains source');
        end;
      csRequestAgain:
        begin
          Click(GKey + '-callback-0-remove');
        end;
      csConfirmAgain:
        begin
          Click('event-removal-confirm');
          Check((Find('event-removal-warning') <> nil) and (Source = GWithCallbacks),
            'Confirmation awaits admission without early pair publication');
        end;
      csRemoved:
        begin
          Check((Find(GKey + '-count').textContent = '0 registrations') and
            (document.querySelector('[data-node=event-removal-warning]') = nil) and
            (Pos('TODO', Source) > 0), 'Exact removal clears warning and retains implementation');
          Click('action-undo');
        end;
      csUndone:
        begin
          Check(Source = GWithCallbacks, 'One Undo restores exact callback source');

          if window.innerWidth < 900 then
          begin
            Click('action-panel-design');
          end;
        end;
      csWriteDraft:
        begin
          GBefore := Source + #10 + '{ Independent application draft }';
          Change('studio-code', GBefore);

          if window.innerWidth < 900 then
          begin
            Click('action-panel-inspector');
          end;
        end;
      csAddWithDraft:
        begin
          Click('event-' + NyxTriggerName(ntAfterKeyPress) + '-add');
        end;
      csDraftRefused:
        begin
          Check(Source = GBefore, 'Rejected addition retains independent Pascal draft');
          Change(NyxEventFilterPartID(NyxStudioEventFilterID, nefpQuery), 'key press');
          Check(Source = GBefore, 'Discovery retains pending Pascal exactly');
          Click(NyxEventFilterPartID(NyxStudioEventFilterID, nefpClear));
          Check(Find('event-' + NyxTriggerName(ntAfterKeyPress) + '-count').textContent =
            '0 registrations', 'Rejected addition creates no registration');

          if GCapture then
          begin
            document.body.setAttribute(CCheckpoint, 'retained-draft');
          end;
        end;
      csRetire:
        begin
          FreeAndNil(GStudio);
          Check(document.querySelector('[data-node=studio-code]') = nil,
            'Studio retirement unmounts its actual source control');
          document.body.setAttribute('data-event-controls', 'passed');
          document.body.setAttribute('data-event-control-checks', IntToStr(GChecks));
          Exit;
        end;
    end;
    GStep := Succ(GStep);
    window.setTimeout(@Step, 10);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-event-controls', 'failed');
      document.body.setAttribute('data-event-error', LException.Message);
    end;
  end;
end;

begin
  GStep := csStart;
  GCapture := window.location.search = CCaptureQuery;
  window.setTimeout(@Step, 0);
end.
