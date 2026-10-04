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

unit nyx.test.events;

{$mode delphi}{$H+}
{$codepage utf8}

interface

{ Routing, atomic admission and retained scalar payloads run without target types.
  The separate DOM/LCL journeys prove that physical controls use this contract. }
function RunNyxEventTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.state,
  nyx.model,
  nyx.behavior,
  nyx.binding,
  nyx.catalog,
  nyx.composition,
  nyx.codec,
  nyx.codegen,
  nyx.test.binding;

function RunNyxEventTests: Integer;
const
  CReplyChanged: TNyxText = 'reply/changed/🌙/漢字';
var
  LAction: TNyxAction;
  LDecodedAction: TNyxAction;
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LDispatch: TNyxDispatch;
  LRetained: TNyxEventInfo;
  LChanged: TNyxEventInfo;
  LDesign: TNyxEventInfo;
  LRevision: Integer;
  LRejected: Boolean;
  LCatalog: TNyxCatalog;
  LOuter: TNyxNode;
  LStepper: TNyxNode;
  LBefore: TNyxText;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Event fixture: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  for LAction := Low(TNyxAction) to High(TNyxAction) do
  begin
    Check(TryNyxAction(NyxActionName(LAction), LDecodedAction) and
      (LDecodedAction = LAction), 'closed action wire mapping');
  end;
  Check(not TryNyxAction('Increment', LDecodedAction),
    'unknown action is refused rather than approximated');
  Check((NyxTriggerName(ntClick) = 'click') and
    (NyxTriggerName(ntChange) = 'change') and
    (NyxTriggerName(ntDesignSelect) = 'select') and
    (NyxTriggerName(ntDesignValue) = 'edit-value'), 'explicit physical/design vocabulary');

  LDocument := CreateNyxBindingFixture;
  LRoot := nil;
  LStore := nil;
  LLive := nil;
  try
    LDocument.Find('reply-memo').Configure.OnChange(NyxEvent(CReplyChanged)).Done;
    LDocument.Pages[0].Add(TNyxNode.Create(nkButton, 'publish'));
    LDocument.Pages[0].Add(TNyxNode.Create(nkSpin, 'unbound-quantity')
      .Configure.Value(7).Done);
    LDocument.Pages[0].Add(TNyxNode.Create(nkCheckbox, 'unbound-enabled')
      .Configure.Value(True).Done);
    LCopy := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    try
      Check(LCopy.Find('reply-memo').Prop(NyxAttributeName(atEmitChange)) = CReplyChanged,
        'open event reference survives versioned persistence');
      Check(Pos('.OnChange(NyxEvent(''reply/changed/🌙/漢字''))',
        TNyxCodegen.Generate(LCopy)) > 0, 'generated events use the typed authoring contract');
    finally
      LCopy.Free;
    end;
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LLive.Activate;

    LDispatch := LLive.Edit(LRoot.Find('reply-memo'), 'Crafted / 🌙' + #10 + '漢字');
    Check((LDispatch.Info.Trigger = ntChange) and
      LDispatch.Info.IsNamed(NyxEvent(CReplyChanged)), 'trigger and application name are distinct');
    Check((LDispatch.Source = LRoot.Find('reply-memo')) and
      (LDispatch.Target = LDispatch.Source) and
      (LDispatch.Info.SourceID = 'reply-memo') and
      (LDispatch.Info.OriginID = 'reply-memo') and
      (LDispatch.Info.TargetID = 'reply-memo'), 'primitive routing snapshots retain exact IDs');
    Check(LDispatch.Info.HasValue and (LDispatch.Info.ValueKind = nskText) and
      (LDispatch.Info.Value.AsText = TNyxText('Crafted / 🌙' + #10 + '漢字')) and
      LDispatch.Info.Changed, 'accepted text payload is exact');
    LRetained := LDispatch.Info.Copy;
    LChanged := LRetained.Copy;
    LChanged.Name := NyxEvent('local edit');
    LChanged.Value := NyxData('local data');
    Check(LRetained.IsNamed(NyxEvent(CReplyChanged)) and
      (LRetained.Value.AsText = TNyxText('Crafted / 🌙' + #10 + '漢字')),
      'caller replacement cannot mutate a retained event');

    LDispatch := LLive.Edit(LRoot.Find('remember-checkbox'), 'true');
    Check((LDispatch.Info.ValueKind = nskBoolean) and LDispatch.Info.Value.AsBoolean,
      'Boolean binding produces a Boolean event value');
    LDispatch := LLive.Edit(LRoot.Find('ratio-input'), '0.125');
    Check((LDispatch.Info.ValueKind = nskNumber) and
      (LDispatch.Info.Value.AsNumber = 0.125), 'finite number keeps number meaning');
    LDispatch := LLive.Dispatch(LRoot.Find('quantity-stepper').Part('increment'), ntClick);
    Check((LDispatch.Info.Trigger = ntClick) and
      (LDispatch.Source = LRoot.Find('quantity-stepper')) and
      (LDispatch.Info.SourceID = LDispatch.Source.ID) and
      (LDispatch.Info.OriginID = LRoot.Find('quantity-stepper').Part('increment').ID) and
      (LDispatch.Info.TargetID = LRoot.Find('quantity-stepper').Part('value').ID),
      'compound source, physical origin and mutated target are independent');
    Check(LDispatch.Info.HasValue and (LDispatch.Info.ValueKind = nskInteger) and
      (LDispatch.Info.Value.AsInteger = 3), 'compound step captures admitted integer target');
    LDispatch := LLive.Dispatch(LRoot.Find('search').Part('clear'), ntClick);
    Check((LDispatch.Info.SourceID = 'search') and
      (LDispatch.Info.TargetID = LRoot.Find('search').Part('query').ID) and
      LDispatch.Info.HasValue and (LDispatch.Info.Value.AsText = ''),
      'clear preserves an explicitly empty value');
    LDispatch := LLive.Dispatch(LRoot.Find('publish'), ntClick);
    Check(not LDispatch.Info.HasValue and (LDispatch.Info.Value.Kind = ndNull),
      'ordinary command distinguishes absent value from empty text');
    LDispatch := LLive.Edit(LRoot.Find('unbound-quantity'), '8');
    Check((LDispatch.Info.ValueKind = nskInteger) and
      (LDispatch.Info.Value.AsInteger = 8), 'unbound primitive uses its control schema');
    LDispatch := LLive.Edit(LRoot.Find('unbound-enabled'), 'false');
    Check((LDispatch.Info.ValueKind = nskBoolean) and
      not LDispatch.Info.Value.AsBoolean, 'unbound Boolean primitive keeps its schema');

    LRevision := LStore.Revision;
    LRejected := False;
    try
      LLive.Edit(LRoot.Find('ratio-input'), '0.125tail');
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.Revision = LRevision) and
      (LRoot.Find('ratio-input').Prop(NyxAttributeName(atValue)) = '0.125'),
      'invalid payload is refused before publication');
    Check(LRetained.Value.AsText = TNyxText('Crafted / 🌙' + #10 + '漢字'),
      'later accepted/rejected commands preserve retained payloads');

    LDesign := NyxDesignEvent(LRoot.Find('reply-memo'), ntDesignValue);
    Check((LDesign.Trigger = ntDesignValue) and
      (LDesign.SourceID = 'reply-memo') and LDesign.HasValue and
      (LDesign.ValueKind = nskText) and (LDesign.Value.AsText = ''),
      'design events carry a text draft without executing application actions');
    LRejected := False;
    try
      LLive.Dispatch(LRoot.Find('publish'), ntDesignSelect);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.Revision = LRevision),
      'design triggers cannot enter the application command interpreter');
  finally
    LLive.Free;
    LStore.Free;
    LRoot.Free;
    LDocument.Free;
  end;
  Check((LRetained.SourceID = 'reply-memo') and
    LRetained.IsNamed(NyxEvent(CReplyChanged)) and
    (LRetained.Value.AsText = TNyxText('Crafted / 🌙' + #10 + '漢字')),
    'owned event data survives destruction of its document and view');

  LCatalog := TNyxCatalog.Create;
  LOuter := TNyxNode.Create(nkColumn, 'outer');
  try
    LOuter.Configure.Compound(True).Done;
    LStepper := LCatalog.NewNode(nkNumberStepper, 'inner');
    LOuter.Add(LStepper);
    LOuter.Configure.Enabled(False).Done;
    LDispatch := DispatchNyxBehavior(LStepper.Part('increment'), ntClick);
    Check((LDispatch.Source = LStepper) and (LDispatch.EventName = '') and
      not LDispatch.Changed and (LStepper.Part('value').Prop('value') = '1'),
      'nearest compound routing still checks all ancestor permissions');
    LOuter.Configure.Enabled(True).Done;
    LStepper.Part('value').Configure.ReadOnly(True).Done;
    LRejected := False;
    try
      DispatchNyxBehavior(LStepper.Part('increment'), ntClick);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStepper.Part('value').Prop('value') = '1'),
      'read-only action target is refused before mutation');
    LStepper.Part('value').Configure.ReadOnly(False).Done;
    { Raw malformed data is confined to the explicit interpreter admission test. }
    LStepper.Part('value').SetProp(NyxAttributeName(atValue), 'unfinished');
    LBefore := LStepper.Part('value').Prop(NyxAttributeName(atValue));
    LRejected := False;
    try
      DispatchNyxBehavior(LStepper.Part('increment'), ntClick);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStepper.Part('value').Prop(NyxAttributeName(atValue)) = LBefore),
      'malformed step value never silently becomes zero');
    LStepper.Part('increment').SetProp(NyxAttributeName(atAction), 'unknown');
    LRejected := False;
    try
      DispatchNyxBehavior(LStepper.Part('increment'), ntClick);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStepper.Part('value').Prop(NyxAttributeName(atValue)) = LBefore),
      'unknown action cannot silently emit an accepted command');
  finally
    LOuter.Free;
    LCatalog.Free;
  end;
end;

end.

