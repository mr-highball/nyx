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



program nyx_state_source_browser;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Web, nyx.text, nyx.binding.types, nyx.studio.authoring,
  nyx.studio.browser;

var
  GStudio: TNyxStudio;
  GStep: Integer;
  GChecks: Integer;
  GPolls: Integer;
  GBefore: TNyxText;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Browser state controls: ' + AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="'+ AID +'"]'));

  if (Result = nil) and (window.innerWidth < 900) then
  begin

    if (Pos('state-', AID) = 1) or (AID = NyxStudioStateToggleID) or
      (AID = NyxStudioAddStateID) then
    begin
      TJSHTMLButtonElement(document.querySelector('[data-node=action-panel-project]')).click;
    end
    else if (Pos('binding-', AID) = 1) or (AID = NyxStudioBindingsToggleID) then
    begin
      TJSHTMLButtonElement(document.querySelector('[data-node=action-panel-inspector]')).click;
    end
    else
    begin
      TJSHTMLButtonElement(document.querySelector('[data-node=action-panel-design]')).click;
    end;
    Result := TJSHTMLElement(document.querySelector('[data-node="'+ AID +'"]'));
  end;
  Check(Result <> nil, 'Mounted Nyx control exists / ' + AID);
end;

function Field(const AID: TNyxText): TJSHTMLElement;
begin
  Result := Find(AID);

  if not (Result is TJSHTMLInputElement) and not (Result is TJSHTMLTextAreaElement) and
    not (Result is TJSHTMLSelectElement) then
  begin
    Result := TJSHTMLElement(Result.querySelector('input,textarea,select'));
  end;
  Check(Result <> nil, 'Actual field exists / ' + AID);
end;

procedure Change(const AID, AValue: TNyxText);
var
  LField: TJSHTMLElement;
begin
  LField := Field(AID);
  LField.focus;
  TJSHTMLInputElement(LField).value := AValue;
  LField.dispatchEvent(TJSEvent.new('change'));
end;

procedure Click(const AID: TNyxText);
begin
  Find(AID).click;
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
        raise Exception.Create('Actual compiled worker exceeded the bounded journey');
      end;
      window.setTimeout(@Step, 10);
      Exit;
    end;
    GPolls := 0;
    case GStep of
      0:
        begin
          GStudio := TNyxStudio.Create;
          GStudio.Run(False);
          Click('action-code');
          Click(NyxStudioStateToggleID);
          Change(NyxStudioNewStateNameID, 'reply');
          Change(NyxStudioNewStateValueID, 'Ready to compose.');
          Click(NyxStudioAddStateID);
          Click(NyxStudioAddStateID);
        end;
      1:
        begin
          Check((Pos('NyxTextState(''reply'')', Source) > 0) and
            (TJSHTMLInputElement(Field(NyxStudioNewStateNameID)).value = ''),
            'Queued creation publishes once and clears its exact form');
          GBefore := Source;
          Change('state-name-0', 'r');
          Change('state-name-0', 'res');
          Change('state-name-0', 'response');
          Check(Source = GBefore, 'Rapid name drafts do not mutate Pascal');
          Click(NyxStudioStateToggleID);
          Click(NyxStudioStateToggleID);
          Check(TJSHTMLInputElement(Field('state-name-0')).value = 'response',
            'Panel navigation preserves the complete name draft');
          Click('state-rename-0');
        end;
      2:
        begin
          Check(Pos('NyxTextState(''response'')', Source) > 0,
            'Explicit Rename migrates generated references');
          Change('state-default-0', 'First reply');
          Change('state-default-0', 'Second reply');
          Change('state-default-0', 'Latest reply');
        end;
      3:
        begin
          Check((Pos('Latest reply', Source) > 0) and
            (TJSHTMLInputElement(Field('state-default-0')).value = 'Latest reply') and
            (document.activeElement = Field('state-default-0')),
            'Latest queued text and focused physical editor survive earlier completion');
          Change(NyxStudioNewStateNameID, 'ratio');
          Change(NyxStudioNewStateInputID, NyxStudioStateInputName(ssiNumber));
          Change(NyxStudioNewStateValueID, '0.125');
          Click(NyxStudioAddStateID);
        end;
      4:
        begin
          GBefore := Source;
          Change('state-default-1', '-');
        end;
      5:
        begin
          Check((Source = GBefore) and
            (TJSHTMLInputElement(Field('state-default-1')).value = '0.125') and
            (document.activeElement = Field('state-default-1')),
            'Rejected partial Number restores accepted text on the focused editor');
          Change('state-default-1', '-');
          Change('state-default-1', '0.875');
        end;
      6:
        begin
          Check((Pos('0.875', Source) > 0) and
            (TJSHTMLInputElement(Field('state-default-1')).value = '0.875'),
            'Rejected earlier input cannot overwrite newer numeric text');
          Click('project-description');
          Click(NyxStudioBindingsToggleID);
          Click('binding-state-0');
        end;
      7:
        begin
          Check(Pos('.Value(LResponseTextState)', Source) > 0,
            'Actual binding action emits the specialized typed reference');
          Change(NyxStudioBindingFlowID, NyxStudioBindingDirectionTitle(bdFromState));
          Change(NyxStudioBindingFlowID, NyxStudioBindingDirectionTitle(bdTwoWay));
          Change(NyxStudioBindingFlowID, NyxStudioBindingDirectionTitle(bdFromState));
        end;
      8:
        begin
          Check(TJSHTMLSelectElement(Field(NyxStudioBindingFlowID)).value =
            NyxStudioBindingDirectionTitle(bdFromState),
            'Rapid flow changes preserve the latest pending descriptor');
          Click('binding-clear');
        end;
      9:
        begin
          Check(Pos('.Clear(bpValue)', Source) > 0, 'Actual Unbind emits a typed clear');
          if window.innerWidth < 900 then
          begin
            Click('action-panel-project');
          end;
          Change('state-name-0', 'ratio');
          GBefore := Source;
          Click('state-rename-0');
        end;
      10:
        begin
          Check((Source = GBefore) and
            (TJSHTMLInputElement(Field('state-name-0')).value = 'ratio'),
            'Colliding rename retains the exact companion and editable name draft');
          document.body.setAttribute('data-state-controls', 'passed');
          document.body.setAttribute('data-state-control-checks', IntToStr(GChecks));
          Exit;
        end;
    else
      begin
        raise Exception.Create('Unknown state-control journey step');
      end;
    end;
    Inc(GStep);
    window.setTimeout(@Step, 10);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-state-controls', 'failed');
      document.body.setAttribute('data-state-error', LException.Message);
    end;
  end;
end;

begin
  window.setTimeout(@Step, 0);
end.
