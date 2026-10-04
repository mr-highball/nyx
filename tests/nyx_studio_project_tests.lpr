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


program nyx_studio_project_tests;

{$mode delphi}{$H+}
{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils,
  JS,
  Web,
  nyx.text,
  nyx.model,
  nyx.codec,
  nyx.data,
  nyx.studio.projects,
  nyx.studio.browser,
  nyx.test.source;

type
  { Standard browser API constructor, solely to inject actual File objects into
    the test picker. Product import still uses its ordinary FileReader callbacks. }
  TProjectTransfer = class external name 'DataTransfer' (TJSDataTransfer)
  public
    constructor new;
  end;

var
  LStudio: TNyxStudio;
  LDocument: TNyxDocument;
  LPair: TNyxProjectPair;
  LSource: TNyxText;
  LName: TNyxText;
  LCount: Integer;
  LPhase: Integer;
  LWaits: Integer;
  LOldRecovery: TNyxText;
  LOldTarget: TNyxText;
  LRequest: TJSXMLHttpRequest;
  LRevision: TNyxText;
  LFrame: TJSHTMLIframeElement;
  LHostWaits: Integer;

function Find(const AID: TNyxText): TJSHTMLElement;
begin
  Result := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if Result = nil then
  begin
    raise Exception.Create('Missing project control: ' + AID);
  end;
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL project UI: ' + AReason);
  end;
  Inc(LCount);
end;

procedure Field(const AID, AValue: TNyxText);
var
  LInput: TJSHTMLInputElement;
  LField: TJSHTMLElement;
begin
  LField := Find(AID);
  LInput := TJSHTMLInputElement(LField);

  if not ((LField is TJSHTMLInputElement) or (LField is TJSHTMLTextAreaElement)) then
  begin
    LInput := TJSHTMLInputElement(Find(AID).querySelector('input,textarea'));
  end;
  LInput.value := AValue;
  LInput.dispatchEvent(TJSEvent.new('input'));
  LInput.dispatchEvent(TJSEvent.new('change'));
end;

procedure Panel(ADesign: Boolean);
var
  LButton: TJSHTMLElement;
begin
  { Compact journeys follow the same visible tabs a phone user must use. Wide
    Studio has both panels available and therefore needs no navigation action. }

  if ADesign then
  begin
    LButton := TJSHTMLElement(document.querySelector('[data-node=action-panel-design]'));
  end
  else
  begin
    LButton := TJSHTMLElement(document.querySelector('[data-node=action-panel-project]'));
  end;

  if LButton <> nil then
  begin
    LButton.click;
  end;
end;

procedure Pick(const APair: TNyxProjectPair; ABundle: Boolean);
var
  LInput: TJSHTMLInputElement;
  LTransfer: TProjectTransfer;
  LDescriptor: TJSObject;
begin
  Find('action-project-import').click;
  LInput := TJSHTMLInputElement(document.querySelector('[data-nyx-project-picker]'));
  Check(LInput <> nil, 'ordinary file-picker host exists');
  LTransfer := TProjectTransfer.new;

  if ABundle then
  begin
    LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(EncodeNyxProject(APair)), 'craft.nyxproject'));
  end
  else
  begin
    { Reverse order exercises pairing by file type rather than list order. }
    LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(APair.Source), 'nyx.edited.view.pas'));
    LTransfer.items.add(TJSHTMLFile.new(TJSArray.new(APair.Design), 'design.nyx'));
  end;
  LDescriptor := TJSObject.new;
  LDescriptor['value'] := LTransfer.files;
  TJSObject.defineProperty(LInput, 'files', LDescriptor);
  LInput.dispatchEvent(TJSEvent.new('change'));
end;

procedure RestoreStorage;
begin

  if isString(LOldRecovery) then
  begin
    window.localStorage.setItem('nyx-studio-project-v2', LOldRecovery);
  end
  else
  begin
    window.localStorage.removeItem('nyx-studio-project-v2');
  end;

  if isString(LOldTarget) then
  begin
    window.localStorage.setItem('nyx-studio-output-target-v1', LOldTarget);
  end
  else
  begin
    window.localStorage.removeItem('nyx-studio-output-target-v1');
  end;
end;

procedure Step;
var
  LReply: TNyxDataValue;
  LRecovery: TNyxDataValue;
  LBad: TNyxProjectPair;
  LInput: TJSHTMLInputElement;
begin
  try
    Inc(LWaits);
    document.body.setAttribute('data-nyx-project-ui-phase', IntToStr(LPhase));

    if LWaits > 120 then
    begin
      raise Exception.Create('Project UI timed out in phase ' + IntToStr(LPhase) +
        ': ' + Find('studio-status').textContent);
    end;
    case LPhase of
      0:
        begin
          Find('action-import').click;
          Check(Find('studio-project-files') <> nil, 'Nyx-built file panel opens without output choice');
          Check(document.querySelector('[data-node=studio-outputs]') = nil,
            'files open without target setup');
          Pick(LPair, False);
          LPhase := 1;
        end;
      1:
        begin

          LInput := TJSHTMLInputElement(Find('project-title').querySelector('input'));

          if LInput.value <> LDocument.Title then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          Check(document.querySelector('[data-node=project-import-warning]') = nil,
            'matching paired files need no conflict choice');
          Panel(True);
          Check(Find('studio-canvas').querySelector('[data-node=eyebrow]').textContent =
            'Crafted status / 🌙', 'paired import remounts real canvas bindings even for reused page IDs');
          Find('action-code').click;
          Check(TJSHTMLTextAreaElement(Find('studio-code')).value = LSource,
            'real paired import preserves crafted source in the Nyx editor');
          Panel(False);
          Field('project-file-name', LName);
          Find('action-project-save').click;
          LPhase := 2;
        end;
      2:
        begin

          if TJSHTMLButtonElement(Find('action-project-save')).disabled then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;

          if Pos('saved', Find('studio-status').textContent) = 0 then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          Check(Pos('Paired project files saved', Find('studio-status').textContent) > 0,
            'actual Save completes through the HTTP Pascal repository');
          Panel(True);
          Field('studio-code', 'rejected draft / 🌙');
          Panel(False);
          Find('action-project-save').click;
          LPhase := 3;
        end;
      3:
        begin

          if Pos('Paired project files saved', Find('studio-status').textContent) = 0 then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          LRequest := TJSXMLHttpRequest.new;
          LRequest.open('GET', '/api/project?name=' + LName, True);
          LRequest.send;
          LPhase := 4;
        end;
      4:
        begin

          if LRequest.readyState <> TJSXMLHttpRequest.DONE then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          Check(LRequest.Status = 200, 'saved browser project reopens over HTTP');
          LReply := TNyxDataValue.ParseJSON(LRequest.responseText);
          LBad := DecodeNyxProject(LReply.Field('project').AsText);
          LRevision := LReply.Field('revision').AsText;
          Check(LBad.Pending and (LBad.Source = LSource) and
            (LBad.Draft = 'rejected draft / 🌙') and (LBad.DraftBase = LSource),
            'real disk project retains rejected draft beside accepted source');
          { Another client changes only recovery metadata. This must conflict just
            as a design/source change would; no timestamp race is involved. }
          LBad.Draft := 'other client / 🌙';
          LRequest := TJSXMLHttpRequest.new;
          LRequest.open('POST', '/api/project?name=' + LName, True);
          LRequest.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
          LRequest.send(NyxObject([
            NyxField('version', NyxData(1)),
            NyxField('expected', NyxData(LRevision)),
            NyxField('project', NyxData(EncodeNyxProject(LBad)))
          ]).ToJSON);
          LPhase := 5;
        end;
      5:
        begin

          if LRequest.readyState <> TJSXMLHttpRequest.DONE then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          Check(LRequest.Status = 200, 'second client publishes a new project revision');
          Find('action-project-save').click;
          LPhase := 6;
        end;
      6:
        begin

          if document.querySelector('[data-node=project-conflict-warning]') = nil then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          Panel(True);
          Check(TJSHTMLTextAreaElement(Find('studio-code')).value =
            'rejected draft / 🌙', 'conflict leaves the current editable buffer intact');
          Panel(False);
          Check(Find('action-project-use-remote') <> nil, 'conflict exposes explicit backup/open choice');
          Find('action-project-use-remote').click;
          Panel(True);
          Check(TJSHTMLTextAreaElement(Find('studio-code')).value =
            'other client / 🌙', 'explicit remote choice opens its recovered draft');
          Panel(False);
          LBad := LPair;
          LBad.Source := 'unsupported Pascal kept in full / 🌙';
          Pick(LBad, True);
          LPhase := 7;
        end;
      7:
        begin

          if document.querySelector('[data-node=project-import-warning]') = nil then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          Panel(True);
          Check(TJSHTMLTextAreaElement(Find('studio-code')).value =
            'other client / 🌙', 'unsupported import leaves the accepted project and draft intact');
          Panel(False);
          Check(Find('action-project-input-backup') <> nil, 'unresolved input remains exportable');
          Find('action-project-use-design').click;
          Panel(True);
          Check(TJSHTMLTextAreaElement(Find('studio-code')).value =
            'unsupported Pascal kept in full / 🌙', 'design choice retains unsupported Pascal as a draft');
          { A real recovery round trip uses the normal production Run path, then
            restores the user's storage values even if a test assertion fails. }
          LBad := LPair;
          LBad.Source := 'unsupported Pascal kept in full / 🌙';
          LRecovery := NyxObject([
            NyxField('version', NyxData(2)),
            NyxField('project', NyxData(EncodeNyxProject(LBad))),
            NyxField('name', NyxData('')),
            NyxField('boundName', NyxData('')),
            NyxField('revision', NyxData('')),
            NyxField('import', NyxData('')),
            NyxField('importName', NyxData('')),
            NyxField('importRevision', NyxData(''))
          ]);
          window.localStorage.setItem('nyx-studio-project-v2', LRecovery.ToJSON);
          LStudio.Free;
          LStudio := TNyxStudio.Create;
          LStudio.Run(True);
          Check(document.querySelector('[data-node=project-import-warning]') <> nil,
            'mismatched recovery is staged rather than half-loaded');
          Find('action-project-use-design').click;
          Find('action-code').click;
          LRecovery := TNyxDataValue.ParseJSON(window.localStorage.getItem('nyx-studio-project-v2'));
          LBad := DecodeNyxProject(LRecovery.Field('project').AsText);
          Check(LBad.Pending and (LBad.Draft = 'unsupported Pascal kept in full / 🌙'),
            'single recovery write contains complete accepted pair and retained draft');
          LStudio.Free;
          LStudio := TNyxStudio.Create;
          LStudio.Run(True);
          Find('action-code').click;
          Check(TJSHTMLTextAreaElement(Find('studio-code')).value =
            'unsupported Pascal kept in full / 🌙', 'normal recovery restores a rejected draft exactly');
          Check(document.documentElement.scrollWidth <= window.innerWidth + 1,
            'file/conflict controls preserve viewport bounds');
          LStudio.Free;
          LStudio := nil;
          RestoreStorage;
          document.body.setAttribute('data-nyx-project-ui', 'passed');
          document.body.setAttribute('data-nyx-project-ui-checks', IntToStr(LCount));
          Exit;
        end;
    end;
    window.setTimeout(@Step, 50);
  except
    on LException: Exception do
    begin
      document.body.setAttribute('data-nyx-project-ui', 'failed');
      document.body.setAttribute('data-nyx-project-ui-error', LException.Message);
      LStudio.Free;
      LStudio := nil;
      RestoreStorage;
    end;
  end;
end;

procedure HostStep;
var
  LBody: TJSHTMLElement;
  LResult: TNyxText;
begin
  Inc(LHostWaits);

  if (LFrame.contentDocument = nil) or (LFrame.contentDocument.body = nil) then
  begin
    window.setTimeout(@HostStep, 50);
    Exit;
  end;
  LBody := TJSHTMLElement(LFrame.contentDocument.body);
  LResult := LBody.getAttribute('data-nyx-project-ui');

  if (LResult = 'passed') or (LResult = 'failed') then
  begin
    document.body.setAttribute('data-nyx-project-ui', LResult);
    document.body.setAttribute('data-nyx-project-ui-checks',
      LBody.getAttribute('data-nyx-project-ui-checks'));
    document.body.setAttribute('data-nyx-project-ui-error',
      LBody.getAttribute('data-nyx-project-ui-error'));
    document.body.setAttribute('data-nyx-project-ui-width', IntToStr(LFrame.contentWindow.innerWidth));

    if LFrame.contentWindow.innerWidth <> 390 then
    begin
      document.body.setAttribute('data-nyx-project-ui', 'failed');
      document.body.setAttribute('data-nyx-project-ui-error', 'Phone host is not exactly 390 pixels');
    end;
  end
  else if LHostWaits < 400 then
  begin
    window.setTimeout(@HostStep, 50);
  end
  else
  begin
    document.body.setAttribute('data-nyx-project-ui', 'failed');
    document.body.setAttribute('data-nyx-project-ui-error', 'Phone project journey timed out');
  end;
end;

begin

  if window.location.search = '?host=1' then
  begin
    LFrame := TJSHTMLIframeElement(document.createElement('iframe'));
    LFrame.setAttribute('style', 'width:390px;height:1000px;border:0');
    LFrame.src := 'studio-projects.html';
    document.body.appendChild(LFrame);
    window.setTimeout(@HostStep, 50);
  end
  else
  begin
    LOldRecovery := window.localStorage.getItem('nyx-studio-project-v2');
    LOldTarget := window.localStorage.getItem('nyx-studio-output-target-v1');
    LName := 'browser-project-' + FormatFloat('0', TJSDate.now);
    LDocument := CreateNyxEditedFixture(LSource);
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
    LStudio := TNyxStudio.Create;
    LStudio.Run(False);
    window.setTimeout(@Step, 10);
  end;
end.
