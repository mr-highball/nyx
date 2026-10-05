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
  nyx.studio.inspector, nyx.studio.browser;

var
  GStudio: TNyxStudio;
  GStep: Integer;
  GChecks: Integer;
  GPolls: Integer;
  GBefore: TNyxText;
  GWithCallbacks: TNyxText;
  GKey: TNyxText;

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

    if (Pos('event-', AID) = 1) or (Pos('inspector-', AID) = 1) then
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

  if not (Result is TJSHTMLTextAreaElement) and not (Result is TJSHTMLSelectElement) then
  begin
    Result := TJSHTMLElement(Result.querySelector('textarea,select'));
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
  else
  begin
    TJSHTMLTextAreaElement(LField).value := AValue;
  end;
  LField.dispatchEvent(TJSEvent.new('change'));
end;

function Source: TNyxText;
begin
  Result := TJSHTMLTextAreaElement(Field('studio-code')).value;
end;

procedure Step;
var
  LStatus: TJSHTMLElement;
begin
  try
    LStatus := TJSHTMLElement(document.querySelector('[data-node=studio-source-status]'));

    if (LStatus <> nil) and (Pos('Preparing', LStatus.textContent) = 1) then
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
      0:
        begin
          { No recovery or service connection touches the observing user's pair. }
          GStudio := TNyxStudio.Create;
          GStudio.Run(False);
          Click('action-code');
          Click('project-description');
          Click(NyxInspectorEventsID);
          GKey := 'event-' + NyxTriggerName(ntBeforeKeyPress);
          GBefore := Source;
          Click(GKey + '-add');
          Check(Source = GBefore, 'Add keeps accepted Pascal during isolated preparation');
        end;
      1:
        begin
          Check((Pos('TODO', Source) > 0) and (Find(GKey + '-count').textContent = '1 registrations'),
            'Admitted callback owns a TODO implementation and visible registration');
          Change(GKey + '-policy', NyxPolicyName(neAsynchronous));
          Change(GKey + '-policy', NyxPolicyName(neSequential));
          Change(GKey + '-policy', NyxPolicyName(neUIQueue));
          Check(TJSHTMLSelectElement(Field(GKey + '-policy')).value = NyxPolicyName(neUIQueue),
            'Latest waiting policy remains visible');
        end;
      2:
        begin
          Check(TJSHTMLSelectElement(Field(GKey + '-policy')).value = NyxPolicyName(neUIQueue),
            'Worker publication retains latest event policy');
          GWithCallbacks := Source;
          Click(GKey + '-callback-0-remove');
          Check((Find('event-removal-warning') <> nil) and (Source = GWithCallbacks),
            'Removal request only exposes its warning');
          Click('event-removal-cancel');
          Check(Source = GWithCallbacks, 'Keep registration retains source');
          Click(GKey + '-callback-0-remove');
          Click('event-removal-confirm');
          Check((Find('event-removal-warning') <> nil) and (Source = GWithCallbacks),
            'Confirmation awaits admission without early pair publication');
        end;
      3:
        begin
          Check((Find(GKey + '-count').textContent = '0 registrations') and
            (document.querySelector('[data-node=event-removal-warning]') = nil) and
            (Pos('TODO', Source) > 0), 'Exact removal clears warning and retains implementation');
          Click('action-undo');
          Check(Source = GWithCallbacks, 'One Undo restores exact callback source');
          GBefore := Source + #10 + '{ Independent application draft }';
          Change('studio-code', GBefore);
          Click('event-' + NyxTriggerName(ntAfterKeyPress) + '-add');
        end;
      4:
        begin
          Check(Source = GBefore, 'Rejected addition retains independent Pascal draft');
          Check(Find('event-' + NyxTriggerName(ntAfterKeyPress) + '-count').textContent =
            '0 registrations', 'Rejected addition creates no registration');
          FreeAndNil(GStudio);
          document.body.setAttribute('data-event-controls', 'passed');
          document.body.setAttribute('data-event-control-checks', IntToStr(GChecks));
          Exit;
        end;
    else
      begin
        raise Exception.Create('Unknown callback control journey step');
      end;
    end;
    Inc(GStep);
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
  window.setTimeout(@Step, 0);
end.
