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
program nyx_tree_disclosure_controls;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

uses
  SysUtils, JS, Web, nyx.text, nyx.types, nyx.behavior, nyx.model,
  nyx.codec, nyx.application.browser, nyx.events, nyx.scheduler,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.collections.mount, nyx.collections.selection,
  nyx.collections.tree.inspection, nyx.process.tree;

type
  { Browser KeyboardEvent construction only. The fixture owns each event and
    sends F2/Escape through the mounted editor's normal input handlers. }
  TTreeKeyEvent = class external name 'KeyboardEvent'(TJSKeyboardEvent)
    constructor new(const AType: String; AOptions: TJSObject); reintroduce;
  end;

  { A click can withdraw its original authority without leaving the final
    enabled flag false. A new source and terminal retirement are separate cases. }
  TClickEffect = (ceEnableCycle, ceUpdateSource, ceRetire);

  { Each explicitly cancelled subscription owns this callback and its mount
    lease only until the current case finishes. No application or document is
    retained. Calls distinguishes a real event delivery from an inert hook. }
  TClickProbe = class(TNyxEventCallback)
  private
    FMount: INyxCollectionMount;
    FEffect: TClickEffect;
  public
    Calls: Integer;
    constructor Create(const AMount: INyxCollectionMount; AEffect: TClickEffect);
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

constructor TClickProbe.Create(const AMount: INyxCollectionMount; AEffect: TClickEffect);
begin
  inherited Create;
  FMount := AMount;
  FEffect := AEffect;
end;

procedure TClickProbe.Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution);
var
  LView: INyxCollectionView;
begin
  Inc(Calls);
  case FEffect of
    ceEnableCycle:
      begin
        FMount.SetInteraction(False, False);
        FMount.SetInteraction(True, False);
      end;
    ceUpdateSource:
      begin
        LView := FMount.View;
        LView.Store.Update(NyxCollectionItem(NyxItem(LView.Spec.Key, 'archive'))
          .WithValue(NyxTextField('name'), 'Archive updated during click'));
      end;
    ceRetire:
      begin
        FMount.Disconnect;
      end;
  end;
end;

function Settle: JSValue; async;
begin
  Result := Undefined;
  await(TJSPromise.resolve(TJSPromise.new(procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(
        procedure
        begin
          AResolve(Undefined);
        end, 50);
    end)));
end;

{ Native DOM cancellation precedes the row's bubble listener. This is distinct
  from a Nyx Click callback, whose ordinary notification has no consume port. }
function CancelClick(AEvent: TJSMouseEvent): Boolean;
begin
  AEvent.preventDefault;
  Result := False;
end;

function Journey(AWidth: Integer): JSValue; async;
var
  LDocument: TNyxDocument;
  LApplication: TNyxBrowserApplication;
  LHost: TJSHTMLElement;
  LFace: TJSHTMLElement;
  LInput: TJSHTMLElement;
  LMount: INyxCollectionMount;
  LView: INyxCollectionView;
  LTree: INyxTreeHierarchy;
  LBefore: INyxCollectionSnapshot;
  LSaved: TNyxText;
  LCallback: INyxEventCallback;
  LSubscription: INyxEventSubscription;
  LProbe: TClickProbe;
  LEffect: TClickEffect;

  function Branch: TJSHTMLElement;
  begin
    Result := TJSHTMLElement(LApplication.View.ElementFor(ProcessTreeID)
      .querySelector('[data-nyx-item="workers"] > summary'));
    Check(Result <> nil, 'the real branch summary is realized');
  end;

  procedure Key(AElement: TJSHTMLElement; const AKey: TNyxText);
  var
    LOptions: TJSObject;
  begin
    LOptions := TJSObject.new;
    LOptions['key'] := AKey;
    LOptions['code'] := AKey;
    LOptions['bubbles'] := True;
    LOptions['cancelable'] := True;
    AElement.dispatchEvent(TTreeKeyEvent.new('keydown', LOptions));
  end;

begin
  Result := Undefined;
  { Reuse the public demo's ordinary bound Tree without installing its search
    controller. These cases qualify disclosure, not the separately tested query
    coordinator or its application-context lifetime. }
  LDocument := BuildProcessTreeDocument;
  LDocument.Title := 'Tree disclosure checks';
  LDocument.Find(ProcessTreeID).SetCollectionView(
    NyxCollectionView(NyxCollection(ProcessCollectionName))
      .Column(NyxTextField('name'), 'Process', cmEditable)
    .Parent(NyxTextField('parent')).Selection(nsmMultiple));
  LSaved := TNyxCodec.Encode(LDocument);
  LApplication := nil;
  LHost := TJSHTMLElement(document.createElement('div'));
  LHost.style.setProperty('width', IntToStr(AWidth) + 'px');
  LHost.style.setProperty('height', '700px');
  document.body.appendChild(LHost);
  try
    LApplication := TNyxBrowserApplication.Create;
    LApplication.Run(LDocument, LHost);
    LMount := LApplication.View.CollectionMount(ProcessTreeID);
    LView := LMount.View;
    LTree := NyxTreeHierarchy(LView);
    LBefore := LView.Store.Snapshot;
    LTree.ExpandAll;
    LTree.SetExpanded(NyxItem(LView.Spec.Key, 'workers'), False);
    LView.Select(NyxItem(LView.Spec.Key, 'workers'));
    RevealNyxTreeItem(LView, NyxItem(LView.Spec.Key, 'workers'));
    await(Settle);
    Branch.click;
    LMount.Refresh;
    await(Settle);
    Check(LTree.IsExpanded(NyxItem(LView.Spec.Key, 'workers')),
      'same-row expansion survives a refresh before the queued native toggle');
    Branch.click;
    LMount.Refresh;
    await(Settle);
    Check(not LTree.IsExpanded(NyxItem(LView.Spec.Key, 'workers')),
      'the second summary click collapses exactly once through refresh');
    LMount.SetInteraction(True, True);
    Branch.click;
    LMount.Refresh;
    await(Settle);
    Check(LTree.IsExpanded(NyxItem(LView.Spec.Key, 'workers')),
      'read-only controls still permit disclosure');
    LTree.SetExpanded(NyxItem(LView.Spec.Key, 'workers'), False);
    LMount.SetInteraction(False, True);
    Branch.click;
    await(Settle);
    Check(not LTree.IsExpanded(NyxItem(LView.Spec.Key, 'workers')),
      'disabled controls refuse the native disclosure default');
    LMount.SetInteraction(True, False);
    LFace := Branch;
    LFace.onclick := @CancelClick;
    try
      LFace.click;
      await(Settle);
      Check(not LTree.IsExpanded(NyxItem(LView.Spec.Key, 'workers')),
        'an already cancelled native click cannot disclose a branch');
    finally
      LFace.onclick := nil;
    end;
    LFace := Branch;
    TJSHTMLElement(LFace.parentElement).focus;
    Key(TJSHTMLElement(LFace.parentElement), 'F2');
    await(Settle);
    LInput := TJSHTMLElement(Branch.querySelector('input'));
    Check((LInput <> nil) and (document.activeElement = LInput),
      'F2 enters the actual bound branch editor');
    LInput.click;
    LMount.Refresh;
    await(Settle);
    Check(not LTree.IsExpanded(NyxItem(LView.Spec.Key, 'workers')) and
      (document.activeElement = LInput), 'editor clicks preserve caret ownership without disclosure');
    Key(LInput, 'Escape');
    await(Settle);
    Check(LView.Store.Snapshot = LBefore, 'ordinary disclosure never edits source records');
    for LEffect := Low(TClickEffect) to High(TClickEffect) do
    begin
      LProbe := TClickProbe.Create(LMount, LEffect);
      LCallback := LProbe;
      LSubscription := LApplication.View.Events.On(NyxControlEvents(ProcessTreeID), ntClick)
        .Policy(neSequential).Subscribe(LCallback);
      try
        Branch.click;
        await(Settle);
        Check((LProbe.Calls = 1) and not LTree.IsExpanded(NyxItem(LView.Spec.Key, 'workers')),
          'a click cannot revive disclosure after its epoch/source/mount is withdrawn');

        if LEffect <> ceRetire then
        begin
          Check(LMount.Connected, 'epoch and source refusal are exercised on the same live mount');
        end;
      finally
        LSubscription.Cancel;
        LSubscription := nil;
        LCallback := nil;
      end;
    end;
    Check(not LMount.Connected, 'retirement inside the original click is terminal');
    Check(LView.Store.Snapshot.Revision = LBefore.Revision + 1,
      'only the deliberate callback source update changes record revision');
    Check(TNyxCodec.Encode(LDocument) = LSaved, 'runtime disclosure leaves authored defaults intact');
  finally
    LApplication.Free;
    LDocument.Free;
    LHost.remove;
  end;
end;

function Run: JSValue; async;
begin
  Result := Undefined;
  try
    await(Journey(1280));
    await(Journey(390));
    document.body.setAttribute('data-tree-disclosure-result', 'passed');
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-tree-disclosure-result', 'failed');
      document.body.setAttribute('data-tree-disclosure-error', E.Message);
    end;
  end;
  document.body.setAttribute('data-tree-disclosure-checks', IntToStr(GChecks));
end;

begin
  Run;
end.
