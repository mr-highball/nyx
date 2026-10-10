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
  nyx.studio.menu,
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
  LExpectedCaption: TNyxText;
  LCaptionSelector: TNyxText;

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

function Pause: TJSPromise;
begin
  Result := TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(procedure
        begin
          AResolve(True);
        end, 20);
    end);
end;

procedure WaitReady; async;
var
  LStarted: Double;
begin
  LStarted := window.performance.now;
  repeat
    await(Pause);

    if window.performance.now - LStarted > 6000 then
    begin
      raise Exception.Create('Ordinary project presentation did not retire');
    end;
  until not LStudio.PresentationPending and not LStudio.SourceBusy;
end;

procedure Click(const AID: TNyxText); async;
var
  LFace: TJSHTMLElement;
begin
  { Borrowed input callbacks queue section replacement. Wait through the public
    readiness observations, never force presentation or bypass the real action. }
  LFace := Find(AID);

  if LFace.getBoundingClientRect.height <= 0 then
  begin
    raise Exception.Create('Project action is not in the visible presentation: ' + AID);
  end;
  LFace.click;
  await(WaitReady);
end;

procedure WorkspaceAction(AAction: TNyxStudioMenuAction); async;
var
  LTarget: TNyxText;
  LBranch: TNyxText;
  LItem: TNyxText;
  LAnchor: TJSHTMLElement;
  LTrace: TNyxText;
  LMenuFace: TJSHTMLElement;
begin
  LTarget := NyxStudioMenuActionTarget(AAction);

  if Find(LTarget).getBoundingClientRect.height > 0 then
  begin
    await(Click(LTarget));
    Exit;
  end;
  { Compact Studio exposes hidden header commands through its public Nyx menu.
    Exercise the visible submenu callback instead of clicking a hidden face. }
  case AAction of
    smaOpen:
      begin
        LBranch := 'studio-menu-project';
        LItem := 'studio-menu-open';
      end;
    smaCode:
      begin
        LBranch := 'studio-menu-view';
        LItem := 'studio-menu-code';
      end;
  else
    begin
      raise Exception.Create('This project journey has no other compact workspace action');
    end;
  end;
  { Read-only lifecycle evidence: distinguish disposal of an unchanged menu
    anchor from actual shell replacement during ordinary startup/recovery. }
  LAnchor := Find(NyxStudioActionMenuID);
  LAnchor.click;
  LTrace := document.body.getAttribute('data-project-menu-trace');

  if not isString(LTrace) then
  begin
    LTrace := '';
  end;
  LTrace := LTrace + ' phase=' + IntToStr(LPhase) + ' action=' + IntToStr(Ord(AAction)) +
    ' immediate=' + BoolToStr(document.querySelector('[data-node=studio-menu-view]') <> nil, True);
  await(WaitReady);
  LTrace := LTrace + ' sameAnchor=' + BoolToStr(Find(NyxStudioActionMenuID) = LAnchor, True) +
    ' mounted=' + BoolToStr(document.querySelector('[data-node=studio-menu-view]') <> nil, True);
  document.body.setAttribute('data-project-menu-trace', LTrace);
  LMenuFace := Find(LBranch);
  LMenuFace.click;
  LTrace := LTrace + ' childImmediate=' + BoolToStr(
    document.querySelector('[data-node="' + LItem + '"]') <> nil, True);
  await(WaitReady);
  LTrace := LTrace + ' sameMenu=' + BoolToStr(
    document.querySelector('[data-node="' + LBranch + '"]') = LMenuFace, True) +
    ' sameAnchorAfterChild=' + BoolToStr(Find(NyxStudioActionMenuID) = LAnchor, True) +
    ' childMounted=' + BoolToStr(document.querySelector('[data-node="' + LItem + '"]') <> nil, True);
  document.body.setAttribute('data-project-menu-trace', LTrace);
  await(Click(LItem));
end;

procedure Panel(ADesign: Boolean); async;
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
    await(WaitReady);
  end;
end;

procedure Pick(const APair: TNyxProjectPair; ABundle: Boolean);
var
  LInput: TJSHTMLInputElement;
  LTransfer: TProjectTransfer;
  LDescriptor: TJSObject;
begin
  Find('action-project-import').click;
  LInput := TJSHTMLInputElement(document.querySelector('[data-nyx-text-files]'));
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

procedure Step; async;
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
      -1:
        begin

          if LRequest.readyState <> TJSXMLHttpRequest.DONE then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          Check(LRequest.Status = 200, 'Owned MCP-authored seed arrives over HTTP');
          LPair := DecodeNyxProject(LRequest.responseText);
          LDocument := TNyxCodec.Decode(LPair.Design);
          LSource := LPair.Source;
          LExpectedCaption := LDocument.Find('note-heading').Prop('text');
          LCaptionSelector := 'h2';
          Check((LDocument.Count = 2) and (LDocument.ComponentCount = 1) and
            (Pos('function NotebookHint', LSource) > 0),
            'The ordinary journey uses the exact semantic multipage/reusable companion');
          LStudio := TNyxStudio.Create;
          LStudio.Run(False);
          LPhase := 0;
        end;
      0:
        begin
          await(WorkspaceAction(smaOpen));
          LPhase := -2;
        end;
      -2:
        begin
          { Section publication may finish after the current host callback.
            Observe the mounted public panel, without directly refreshing or
            changing the controller to make the fixture proceed. }

          if document.querySelector('[data-node=studio-project-files]') = nil then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
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
          await(Panel(True));
          Check(Find('studio-canvas').querySelector(LCaptionSelector).textContent = LExpectedCaption,
            'paired import remounts real canvas bindings even for reused page IDs');
          await(WorkspaceAction(smaCode));
          Check(TJSHTMLTextAreaElement(Find('studio-code')).value = LSource,
            'real paired import preserves crafted source in the Nyx editor');

          if Pos('semantic=1', window.location.search) > 0 then
          begin
            await(Panel(False));
            await(Click('view-page-1'));
            await(Panel(False));
            await(Click('view-component-0'));
            await(Panel(True));
            Check(Find('studio-canvas').querySelector('[data-node=note-card]') <> nil,
              'The mounted reusable canvas belongs to the exact imported component');
            Check(Find('studio-canvas').querySelector('h2').textContent = LExpectedCaption,
              'Imported reusable root has its own real design canvas');
            await(Panel(False));
            await(Click('view-page-0'));
            await(Panel(True));
            Check(TJSHTMLTextAreaElement(Find('studio-code')).value = LSource,
              'Page/component navigation retains the exact crafted companion');
            document.body.setAttribute('data-capture-checkpoint', 'files-imported-source');
            LPhase := 10;
            window.setTimeout(@Step, 50);
            Exit;
          end;
          await(Panel(False));
          Field('project-file-name', LName);
          await(Click('action-project-save'));
          LPhase := 2;
        end;
      10:
        begin

          if document.body.getAttribute('data-capture-observed') <> 'files-imported-source' then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          await(Panel(False));
          Field('project-file-name', LName);
          await(Click('action-project-save'));
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
          await(Panel(True));
          Field('studio-code', 'rejected draft / 🌙');
          await(Panel(False));
          await(Click('action-project-save'));
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
          await(Click('action-project-save'));
          LPhase := 6;
        end;
      6:
        begin

          if document.querySelector('[data-node=project-conflict-warning]') = nil then
          begin
            window.setTimeout(@Step, 50);
            Exit;
          end;
          await(Panel(True));
          Check(TJSHTMLTextAreaElement(Find('studio-code')).value =
            'rejected draft / 🌙', 'conflict leaves the current editable buffer intact');
          await(Panel(False));
          Check(Find('action-project-use-remote') <> nil, 'conflict exposes explicit backup/open choice');
          await(Click('action-project-use-remote'));
          await(Panel(True));
          Check(TJSHTMLTextAreaElement(Find('studio-code')).value =
            'other client / 🌙', 'explicit remote choice opens its recovered draft');
          await(Panel(False));
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
          await(Panel(True));
          Check(TJSHTMLTextAreaElement(Find('studio-code')).value =
            'other client / 🌙', 'unsupported import leaves the accepted project and draft intact');
          await(Panel(False));
          Check(Find('action-project-input-backup') <> nil, 'unresolved input remains exportable');
          await(Click('action-project-use-design'));
          await(Panel(True));
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
          await(Click('action-project-use-design'));
          await(WorkspaceAction(smaCode));
          LRecovery := TNyxDataValue.ParseJSON(window.localStorage.getItem('nyx-studio-project-v2'));
          LBad := DecodeNyxProject(LRecovery.Field('project').AsText);
          Check(LBad.Pending and (LBad.Draft = 'unsupported Pascal kept in full / 🌙'),
            'single recovery write contains complete accepted pair and retained draft');
          LStudio.Free;
          LStudio := TNyxStudio.Create;
          LStudio.Run(True);
          await(WorkspaceAction(smaCode));
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

      if document.querySelector('[data-node=studio-status]') <> nil then
      begin
        document.body.setAttribute('data-nyx-project-ui-status',
          Find('studio-status').textContent);
      end;
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
  LCheckpoint: TNyxText;
  LObserved: TNyxText;
begin
  Inc(LHostWaits);

  if (LFrame.contentDocument = nil) or (LFrame.contentDocument.body = nil) then
  begin
    window.setTimeout(@HostStep, 50);
    Exit;
  end;
  LBody := TJSHTMLElement(LFrame.contentDocument.body);
  LResult := LBody.getAttribute('data-nyx-project-ui');
  { Forward only the read-only capture handshake. This observes the same child
    journey; it never changes a design, clicks a control or forces presentation. }

  LCheckpoint := LBody.getAttribute('data-capture-checkpoint');
  LObserved := document.body.getAttribute('data-capture-observed');

  if isString(LCheckpoint) and (LCheckpoint <> '') then
  begin
    document.body.setAttribute('data-capture-checkpoint', LCheckpoint);
  end;

  if isString(LObserved) and (LObserved <> '') then
  begin
    LBody.setAttribute('data-capture-observed', LObserved);
  end;

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

  if Pos('host=1', window.location.search) > 0 then
  begin
    LFrame := TJSHTMLIframeElement(document.createElement('iframe'));
    LFrame.setAttribute('style', 'width:390px;height:1000px;border:0');
    LFrame.src := 'studio-projects.html';

    if Pos('semantic=1', window.location.search) > 0 then
    begin
      LFrame.src := LFrame.src + '?semantic=1';
    end;
    document.body.appendChild(LFrame);
    window.setTimeout(@HostStep, 50);
  end
  else
  begin
    LOldRecovery := window.localStorage.getItem('nyx-studio-project-v2');
    LOldTarget := window.localStorage.getItem('nyx-studio-output-target-v1');
    LName := 'browser-project-' + FormatFloat('0', TJSDate.now);

    if Pos('semantic=1', window.location.search) > 0 then
    begin
      LRequest := TJSXMLHttpRequest.new;
      LRequest.open('GET', 'project-file-seed.nyxproject', True);
      LRequest.send;
      LPhase := -1;
    end
    else
    begin
      LDocument := CreateNyxEditedFixture(LSource);
      LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
      LExpectedCaption := 'Crafted status / 🌙';
      LCaptionSelector := '[data-node=eyebrow]';
      LStudio := TNyxStudio.Create;
      LStudio.Run(False);
    end;
    window.setTimeout(@Step, 10);
  end;
end.
