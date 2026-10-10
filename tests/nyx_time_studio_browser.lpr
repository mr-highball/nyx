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

program nyx_time_studio_browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils, JS, Web, nyx.text, nyx.model, nyx.codec, nyx.studio.projects,
  nyx.studio.browser, nyx.studio.inspector, nyx.times.editor, nyx.generated.time;

type
  { The ordinary picker still reads real File objects with FileReader. This
    bridge supplies only owned fixture bytes, as in the existing project test. }
  TProjectTransfer = class external name 'DataTransfer'(TJSDataTransfer)
  public
    constructor new;
  end;
  TClockStudioEvent = class external name 'Event'(TJSEvent)
    constructor new(const AType: String; const AOptions: TJSObject); reintroduce;
  end;

var
  GStudio: TNyxStudio;
  GRequest: TJSXMLHttpRequest;
  GSeed: TNyxProjectPair;
  GTitle: TNyxText;
  GExport: TNyxText;
  GBefore: TNyxText;
  GAfter: TNyxText;
  GChoices: TNyxText;
  GCode: TJSHTMLTextAreaElement;
  GStage: Integer;
  GChecks: Integer;
  GStarted: Double;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Ordinary clock Studio: ' + AReason);
  end;
  Inc(GChecks);
end;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise Exception.Create('Missing ordinary Studio control: ' + AID);
  end;
end;

procedure Click(const AID: TNyxText);
var
  LFace: TJSHTMLElement;
begin
  LFace := Find(AID);
  Check(LFace.getBoundingClientRect.height > 0, 'visible command ' + AID);
  LFace.scrollIntoView;
  LFace.click;
end;

{ Compact header actions use the same public Nyx menu as an operator. Hidden
  actions are never invoked as a shortcut to the controller's private methods. }
procedure Action(const AID, ABranch, ACommand: TNyxText);
begin

  if Find(AID).getBoundingClientRect.height > 0 then
  begin
    Click(AID);
    Exit;
  end;
  Click('action-actions');
  Click('studio-menu-' + ABranch);
  Click('studio-menu-' + ACommand);
end;

procedure Panel(const AName: TNyxText);
var
  LFace: TJSHTMLElement;
begin
  LFace := TJSHTMLElement(document.querySelector('[data-node="action-panel-' + AName + '"]'));

  if (LFace <> nil) and (LFace.getBoundingClientRect.height > 0) then
  begin
    Click('action-panel-' + AName);
  end;
end;

function Input(AField: TNyxTimeDomainEditorField): TJSHTMLElement;
begin
  Result := Find(NyxTimeDomainEditorFieldID('inspector-time-domain', AField));

  if not ((Result is TJSHTMLInputElement) or (Result is TJSHTMLTextAreaElement) or
    (Result is TJSHTMLSelectElement)) then
  begin
    Result := TJSHTMLElement(Result.querySelector('input,textarea,select'));
  end;
  Check(Result <> nil, 'public typed clock field is mounted');
end;

function Value(AField: TNyxTimeDomainEditorField): TNyxText;
begin
  Result := TJSHTMLInputElement(Input(AField)).value;
end;

procedure Change(AField: TNyxTimeDomainEditorField; const AText: TNyxText);
var
  LInput: TJSHTMLElement;
  LOptions: TJSObject;
begin
  LInput := Input(AField);
  LInput.scrollIntoView;
  LInput.focus;
  TJSHTMLInputElement(LInput).value := AText;
  LOptions := TJSObject.new;
  LOptions['bubbles'] := True;
  LInput.dispatchEvent(TClockStudioEvent.new('input', LOptions));
  LInput.dispatchEvent(TClockStudioEvent.new('change', LOptions));
end;

procedure FieldsRetained(const AReason: TNyxText);
begin
  Check((Value(ntfMinimum) = '23:00:00.000') and
    (Value(ntfMaximum) = '01:00:00.000') and
    (Value(ntfStepMode) = NyxTimeDomainEditorStepName(ntsMilliseconds)) and
    (Value(ntfMilliseconds) = '1.5') and (Value(ntfChoices) = GChoices), AReason);
end;

procedure SelectClock(const AID: TNyxText);
var
  LFace: TJSHTMLElement;
begin
  Panel('design');
  LFace := TJSHTMLElement(Find('studio-canvas').querySelector('[data-node="' + AID + '"]'));
  Check((LFace <> nil) and (LFace.getBoundingClientRect.height > 0),
    'the imported clock is present on the real canvas');
  LFace.scrollIntoView;
  LFace.click;
  Panel('inspector');
end;

{ Observe the ordinary exported backup rather than exposing controller/session
  internals. Fixture downloads never reach the user's Downloads directory; only
  the explicitly requested backup supplies this test's paired observation. }
function CaptureExport(AEvent: TJSEvent): Boolean;
var
  LAnchor: TJSHTMLAnchorElement;
  LComma: Integer;
begin
  Result := True;

  if not (AEvent.target is TJSHTMLAnchorElement) then
  begin
    Exit;
  end;
  LAnchor := TJSHTMLAnchorElement(AEvent.target);

  if LAnchor.download = '' then
  begin
    Exit;
  end;
  AEvent.preventDefault;

  if LAnchor.download <> 'project.nyxproject' then
  begin
    Exit;
  end;
  LComma := Pos(',', LAnchor.href);
  GExport := decodeURIComponent(Copy(LAnchor.href, LComma + 1, Length(LAnchor.href)));
end;

procedure Files;
begin
  Panel('project');

  if document.querySelector('[data-node="studio-project-files"]') = nil then
  begin
    Action('action-import', 'project', 'open');
  end;
end;

function Snapshot: TNyxText;
begin
  Files;
  GExport := '';
  Click('action-project-export');
  Check(GExport <> '', 'the ordinary backup command exports its current paired state');
  Result := EncodeNyxProject(DecodeNyxProject(GExport));
  Panel('inspector');
end;

procedure PickSeed;
var
  LInput: TJSHTMLInputElement;
  LTransfer: TProjectTransfer;
  LDescriptor: TJSObject;
begin
  Files;
  Click('action-project-import');
  LInput := TJSHTMLInputElement(document.querySelector('[data-nyx-text-files]'));
  Check(LInput <> nil, 'the ordinary paired-file picker is mounted');
  LTransfer := TProjectTransfer.new;
  LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(GSeed.Source), 'nyx.generated.time.pas'));
  LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(GSeed.Design), 'design.nyx'));
  LDescriptor := TJSObject.new;
  LDescriptor['value'] := LTransfer.files;
  TJSObject.defineProperty(LInput, 'files', LDescriptor);
  LInput.dispatchEvent(TJSEvent.new('change'));
end;

procedure Failed(const AReason: TNyxText);
begin
  document.body.setAttribute('data-time-studio', 'failed');
  document.body.setAttribute('data-event-error', AReason);
end;

{ Each phase yields to the real FileReader/source worker and normal repaint.
  Readiness is based on actual exported pairs, source work and mounted controls;
  visible status wording is diagnostic only. Captures precede explicit disposal. }
procedure Step;
var
  LPair: TNyxProjectPair;
begin
  try
    document.body.setAttribute('data-time-studio-phase', IntToStr(GStage));

    if window.performance.now - GStarted > 120000 then
    begin
      raise Exception.Create('Ordinary clock workflow timed out at ' + IntToStr(GStage));
    end;

    if GStudio.SourceBusy then
    begin
      window.setTimeout(@Step, 50);
      Exit;
    end;
    case GStage of
      0:
        begin
          PickSeed;
          GStage := 1;
        end;
      1:
        begin

          { Compact Project parks the canvas. Await the imported project field,
            then open Design before requiring a physically mounted clock. }

          if TJSHTMLInputElement(Find('project-title').querySelector('input')).value <> GTitle then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          SelectClock('start-time');
          GBefore := Snapshot;
          Check(GBefore = EncodeNyxProject(GSeed), 'FileReader imports both exact accepted files atomically');
          Change(ntfMinimum, '23:00:00.000');
          Change(ntfMaximum, '01:00:00.000');
          Change(ntfStepMode, NyxTimeDomainEditorStepName(ntsMilliseconds));
          Change(ntfMilliseconds, '1.5');
          GChoices := '23:00:00.000' + #10 + '00:30:00.000' + #10 + '(empty)';
          Change(ntfChoices, GChoices);
          Action('action-code', 'view', 'code');
          Panel('design');
          GCode := TJSHTMLTextAreaElement(Find('studio-code'));
          Check(GCode.value = GSeed.Source, 'ordinary source pane shows the exact imported Pascal');
          Check((window.getComputedStyle(GCode).getPropertyValue('background-color') =
            'rgb(23, 27, 41)') and (window.getComputedStyle(GCode).getPropertyValue('color') =
            'rgb(203, 213, 237)'),
            'ordinary browser source consumes its independent typed readable palette');
          Panel('inspector');
          FieldsRetained('opening source retains all five unsubmitted policy fields');
          Click(NyxInspectorEventsID);
          Click(NyxInspectorPropertiesID);
          FieldsRetained('Properties/Events navigation retains the clock proposals');
          Check(Snapshot = GBefore, 'unsubmitted policy fields retain the exact accepted pair');
          Click(NyxTimeDomainEditorFieldID('inspector-time-domain', ntfApply));
          GStage := 2;
        end;
      2:
        begin
          FieldsRetained('refused Apply retains the exact invalid proposal for correction');
          Check(Snapshot = GBefore, 'refused policy Apply preserves the entire accepted pair');
          Change(ntfMilliseconds, '500');
          Click(NyxTimeDomainEditorFieldID('inspector-time-domain', ntfApply));
          GStage := 3;
        end;
      3:
        begin
          GAfter := Snapshot;
          LPair := DecodeNyxProject(GAfter);
          Check((GAfter <> GBefore) and not LPair.Pending and
            (Pos('.StepMilliseconds(500)', LPair.Source) > 0),
            'corrected Apply admits a paired typed policy through the real source worker');
          Panel('design');
          Check((Find('studio-code') = GCode) and (GCode.value = LPair.Source),
            'the retained source editor displays the exact newly admitted companion');
          Click('action-undo');
          Check(Snapshot = GBefore, 'one ordinary Undo restores the complete original pair');
          Click('action-redo');
          Check(Snapshot = GAfter, 'one ordinary Redo restores the complete corrected pair');
          Change(ntfMilliseconds, '2.5');
          SelectClock('earliest-time');
          Check(Value(ntfMinimum) = '08:30', 'selection changes never receive another owner draft');
          SelectClock('start-time');
          Check(Value(ntfMilliseconds) = '500', 'returning selection restores the accepted policy');
          Change(ntfMilliseconds, '3.5');
          PickSeed;
          GStage := 4;
        end;
      4:
        begin

          if Snapshot <> GBefore then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          SelectClock('start-time');
          Check(Value(ntfMilliseconds) = '1500', 'project replacement retires a same-named owner draft');
          Panel('design');
          document.body.setAttribute('data-capture-checkpoint', 'clock-studio');
          GStage := 5;
        end;
      5:
        begin

          if document.body.getAttribute('data-capture-observed') = 'clock-studio' then
          begin
            document.removeEventListener('click', @CaptureExport);
            FreeAndNil(GStudio);
            Check(document.querySelector('[data-node="studio-shell"]') = nil,
              'explicit ordinary controller retirement removes its owned views');
            document.body.setAttribute('data-time-studio', 'passed');
            document.body.setAttribute('data-time-studio-checks', IntToStr(GChecks));
            Exit;
          end;
        end;
    end;
    window.setTimeout(@Step, 50);
  except
    on LException: Exception do
    begin
      Failed(LException.Message);
    end;
  end;
end;

begin
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'seed.pas.txt', True);
  GRequest.onload := function(AEvent: TJSProgressEvent): Boolean
    var
      LDocument: TNyxDocument;
    begin
      Result := True;
      try
        Check(GRequest.status = 200, 'the exact accepted clock source loads over HTTP');
        LDocument := BuildNyxDocument;
        try
          GSeed := NyxProjectPair(TNyxCodec.Encode(LDocument), GRequest.responseText);
          GTitle := LDocument.Title;
        finally
          LDocument.Free;
        end;
        document.addEventListener('click', @CaptureExport);
        GStudio := TNyxStudio.Create;
        GStudio.Run(False);
        GStarted := window.performance.now;
        window.setTimeout(@Step, 250);
      except
        on LException: Exception do
        begin
          Failed(LException.Message);
        end;
      end;
    end;
  GRequest.send;
end.
