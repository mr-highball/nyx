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

unit nyx.test.binding.lcl;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxNativeBindingJourney: Integer;

implementation

uses
  nyx.types,
  nyx.behavior,
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  Spin,
  ExtCtrls,
  nyx.text,
  nyx.state,
  nyx.binding.types,
  nyx.model,
  nyx.widgets.lcl,
  nyx.application.lcl,
  nyx.render.lcl,
  nyx.test.binding;

var
  GRejectUpdate: Boolean = False;

function CaptionFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
var
  LPanel: TPanel;
  LCaption: TLabel;
  LDecoration: TLabel;
begin
  LPanel := TPanel.Create(AOwner);
  LPanel.BevelOuter := bvNone;
  LCaption := TLabel.Create(AOwner);
  LCaption.Parent := LPanel;
  LCaption.SetBounds(0, 0, 240, 32);
  LDecoration := TLabel.Create(AOwner);
  LDecoration.Parent := LPanel;
  LDecoration.Caption := ' •';
  Result := LPanel;
end;

procedure UpdateCaption(ANode: TNyxNode; AControl: TControl);
begin

  if GRejectUpdate then
  begin
    raise ENyxState.Create('Admitted custom notification failed');
  end;
  TLabel(TPanel(AControl).Controls[0]).Caption := ANode.Prop('text');
end;

procedure RejectCaptionUpdate(ANode: TNyxNode; AControl: TControl);
begin
  raise ENyxState.Create('Custom candidate updater rejected');
end;

function RunCustomJourney: Integer;
var
  LDocument: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LCaption: TPanel;
  LRoot: TNyxNode;
  LRejected: Boolean;
  LMemo: TMemo;
  LStore: TNyxState;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxState.Create('Custom native binding: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LDocument := CreateNyxBindingFixture;
  LRenderer := TNyxLCLRenderer.Create;
  LHost := TForm.CreateNew(nil);
  try
    LHost.ClientWidth := 1000;
    LHost.ClientHeight := 800;
    LRenderer.RegisterFactory('label', CaptionFactory, UpdateCaption);
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LCaption := TPanel(LRenderer.ControlFor('reply-caption'));
    LRoot := LRenderer.Root;
    Check((TNyxText(TLabel(LCaption.Controls[0]).Caption) = TNyxText('Café / 🌙 / 漢字')) and
      (LCaption.ControlCount = 2), 'custom native caption retains children');
    LRenderer.State.SetValue(NyxTextState('🌙/reply'), 'Updated custom caption');
    Check((LRenderer.ControlFor('reply-caption') = LCaption) and
      (TNyxText(TLabel(LCaption.Controls[0]).Caption) = 'Updated custom caption') and
      (LCaption.ControlCount = 2), 'custom updater retains native control and children');
    LMemo := TMemo(LRenderer.InputFor('reply-memo'));
    GRejectUpdate := True;
    LMemo.Text := 'Committed despite repaint failure';
    Check((LRenderer.State.GetValue(NyxTextState('🌙/reply')) = TNyxText(LMemo.Text)) and
      (LRenderer.Root.Find('reply-memo').Prop('value') = TNyxText(LMemo.Text)) and
      (LRenderer.LastBindingFailure = nbfNotificationFailed),
      'native notification reports committed state without retry or rollback');
    GRejectUpdate := False;
    LRenderer.State.SetValue(NyxTextState('🌙/reply'), 'Recovered custom view');
    Check(TNyxText(TLabel(LCaption.Controls[0]).Caption) = 'Recovered custom view',
      'native custom view recovers through its existing subscription');
    LMemo.Text := 'Accepted after recovery';
    Check((LRenderer.LastBindingError = '') and (LRenderer.LastBindingFailure = nbfNone),
      'accepted native command clears notification diagnostic');
    LRenderer.RegisterFactory('label', CaptionFactory, RejectCaptionUpdate);
    LRejected := False;
    try
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LRenderer.Root = LRoot) and
      (LRenderer.ControlFor('reply-caption') = LCaption), 'failed updater retains native accepted view');
    LRenderer.State.SetValue(NyxTextState('🌙/reply'), 'Accepted updater remains attached');
    Check(TNyxText(TLabel(LCaption.Controls[0]).Caption) = 'Accepted updater remains attached',
      'registry changes preserve mounted native updater ownership');
    LRenderer.RegisterFactory('label', CaptionFactory);
    LRejected := False;
    try
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LRenderer.Root = LRoot) and
      (LRenderer.ControlFor('reply-caption') = LCaption), 'missing updater preserves native accepted view');
    LRenderer.RegisterFactory('label', CaptionFactory, UpdateCaption);
    LStore := LRenderer.State;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost, LStore);
    Check((LRenderer.State = LStore) and
      (LRenderer.State.GetValue(NyxTextState('🌙/reply')) = 'Accepted updater remains attached'),
      'owned native store survives explicit full remount');
    LRenderer.State.SetValue(NyxTextState('🌙/reply'), 'After owned-store remount');
    Check(TNyxText(TLabel(TPanel(LRenderer.ControlFor('reply-caption')).Controls[0]).Caption) =
      'After owned-store remount', 'remounted native store retains active coordinator');
  finally
    GRejectUpdate := False;
    LRenderer.Free;
    LHost.Free;
    LDocument.Free;
  end;
end;

type
  TNativeBindingProbe = class
  public
    Calls: Integer;
    Reject: Boolean;
    LastInfo: TNyxEventInfo;
    LastSourceID: TNyxText;
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
    procedure Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
  end;

procedure TNativeBindingProbe.Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
begin
  LastInfo := AEvent.Copy;
  LastSourceID := ANode.ID;
  Inc(Calls);
end;

procedure TNativeBindingProbe.Validate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
begin

  if Reject then
  begin
    raise ENyxState.Create('Domain command rejected');
  end;
end;

function RunNyxNativeBindingJourney: Integer;
var
  LDocument: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LProbe: TNativeBindingProbe;
  LToken: TNyxStateSubscription;
  LMemo: TMemo;
  LMirror: TMemo;
  LNumber: TEdit;
  LCheckbox: TCheckBox;
  LNode: TNyxNode;
  LRevision: Integer;
  LCalls: Integer;
  LRejected: Boolean;
  LChange: TNotifyEvent;
  LRetained: TNyxEventInfo;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxState.Create('Native binding: ' + AReason);
    end;
    Inc(Result);
  end;

  function MemoText(AControl: TMemo): TNyxText;
  begin
    Result := StringReplace(TNyxText(AControl.Text), #13#10, #10, [rfReplaceAll]);
  end;

begin
  Result := RunCustomJourney;
  LDocument := CreateNyxBindingFixture;
  LApplication := TNyxLCLApplication.Create;
  LProbe := TNativeBindingProbe.Create;
  LToken := nil;
  try
    LApplication.View.OnEvent := LProbe.Event;
    LApplication.Mount(LDocument);
    LToken := LApplication.State.Subscribe(nil, LProbe.Validate);
    LMemo := TMemo(LApplication.View.InputFor('reply-memo'));
    LMirror := TMemo(LApplication.View.InputFor('reply-mirror'));
    LNumber := TEdit(LApplication.View.InputFor('ratio-input'));
    LCheckbox := TCheckBox(LApplication.View.InputFor('remember-checkbox'));
    LNode := LApplication.View.Root.Find('reply-memo');
    Check((MemoText(LMemo) = TNyxText('Café / 🌙 / 漢字')) and
      (TNyxText(LNumber.Text) = '0.1'), 'typed defaults reach actual native controls');
    LMemo.Text := 'Crafted / 🌙' + #10 + 'Second line';
    LMemo.SelStart := 3;
    LMemo.SelLength := 4;
    Check((LApplication.State.GetValue(NyxTextState('🌙/reply')) = MemoText(LMemo)) and
      (MemoText(LMirror) = MemoText(LMemo)) and
      (TNyxText(TLabel(LApplication.View.ControlFor('reply-caption')).Caption) = MemoText(LMemo)),
      'physical memo edit synchronizes native caption and sibling');
    Check((LApplication.View.InputFor('reply-memo') = LMemo) and
      (LApplication.View.Root.Find('reply-memo') = LNode),
      'accepted edit retains native control and node identity');
    Check(LProbe.Calls = 1, 'one physical edit emits one semantic event');
    Check((LProbe.LastInfo.Trigger = ntChange) and
      LProbe.LastInfo.IsNamed(NyxEvent('change')) and
      (LProbe.LastInfo.SourceID = 'reply-memo') and
      (LProbe.LastSourceID = LProbe.LastInfo.SourceID) and
      (LProbe.LastInfo.OriginID = 'reply-memo') and
      (LProbe.LastInfo.TargetID = 'reply-memo'), 'native callback carries typed routing');
    Check(LProbe.LastInfo.HasValue and (LProbe.LastInfo.ValueKind = nskText) and
      (LProbe.LastInfo.Value.AsText = MemoText(LMemo)), 'native callback owns exact text');
    LRetained := LProbe.LastInfo.Copy;
    LCalls := LProbe.Calls;
    LApplication.State.SetValue(NyxBooleanState('checked'), True);
    Check(LCheckbox.Checked and (LProbe.Calls = LCalls),
      'programmatic native Boolean update causes no feedback event');
    LCheckbox.Checked := False;
    Check(not LApplication.State.GetValue(NyxBooleanState('checked')) and
      (LProbe.Calls = LCalls + 1), 'physical checkbox writes Boolean state: state=' +
      BoolToStr(LApplication.State.GetValue(NyxBooleanState('checked')), True) +
      ', callback delta=' + IntToStr(LProbe.Calls - LCalls));
    Check((LProbe.LastInfo.ValueKind = nskBoolean) and
      not LProbe.LastInfo.Value.AsBoolean, 'native callback keeps Boolean value type');
    LCalls := LProbe.Calls;
    LNumber.Text := '0.';
    Check((TNyxText(LNumber.Text) = '0.') and
      (LApplication.State.GetValue(NyxNumberState('ratio')) =
        LDocument.State.GetValue(NyxNumberState('ratio'))) and (LProbe.Calls = LCalls),
      'unfinished numeric draft remains editable before commit');
    LNumber.Text := '0.125';
    Check((LApplication.State.GetValue(NyxNumberState('ratio')) =
      LDocument.State.GetValue(NyxNumberState('ratio'))) and (LProbe.Calls = LCalls),
      'completed native numeric draft remains pending until editing completes');
    LNumber.OnEditingDone(LNumber);
    Check(LApplication.State.GetValue(NyxNumberState('ratio')) = 0.125,
      'physical native numeric edit writes Double state');
    Check((LProbe.LastInfo.ValueKind = nskNumber) and
      (LProbe.LastInfo.Value.AsNumber = 0.125), 'native callback keeps finite number type');
    LRevision := LApplication.State.Revision;
    LCalls := LProbe.Calls;
    LNumber.Text := 'invalid';
    LNumber.OnEditingDone(LNumber);
    Check((LApplication.State.Revision = LRevision) and (TNyxText(LNumber.Text) = '0.125') and
      (LProbe.Calls = LCalls) and (LApplication.View.LastBindingError <> ''),
      'invalid native number restores accepted value without semantic event: revision=' +
      IntToStr(LApplication.State.Revision) + '/' + IntToStr(LRevision) + ', text=' +
      TNyxText(LNumber.Text) + ', events=' + IntToStr(LProbe.Calls) + '/' +
      IntToStr(LCalls) + ', error=' + LApplication.View.LastBindingError);
    Check((LProbe.LastInfo.TargetID = 'ratio-input') and
      (LProbe.LastInfo.Value.AsNumber = 0.125), 'rejected edit cannot replace last native event');
    LProbe.Reject := True;
    LMemo.Text := 'Rejected draft';
    Check((LApplication.State.Revision = LRevision) and
      (MemoText(LMemo) = LApplication.State.GetValue(NyxTextState('🌙/reply'))),
      'domain rejection restores the same native memo');
    LProbe.Reject := False;
    TNyxLCLButton(LApplication.View.ControlFor(
      LApplication.View.Root.Find('quantity-stepper').Part('increment').ID)).Click;
    Check((LApplication.State.GetValue(NyxIntegerState('quantity')) = 3) and
      (TNyxText(TLabel(LApplication.View.ControlFor('quantity-caption')).Caption) = '3') and
      (LApplication.View.LastBindingError = ''), 'compound native increment synchronizes state');
    Check((LProbe.LastInfo.Trigger = ntClick) and LProbe.LastInfo.IsNamed(NyxEvent('increment')) and
      (LProbe.LastInfo.SourceID = 'quantity-stepper') and
      (LProbe.LastSourceID = 'quantity-stepper') and
      (LProbe.LastInfo.OriginID = LApplication.View.Root.Find('quantity-stepper').Part('increment').ID) and
      (LProbe.LastInfo.TargetID = LApplication.View.Root.Find('quantity-stepper').Part('value').ID),
      'native compound callback separates source, origin and target');
    Check((LProbe.LastInfo.ValueKind = nskInteger) and
      (LProbe.LastInfo.Value.AsInteger = 3), 'native compound callback owns admitted integer');
    LCalls := LProbe.Calls;
    { Simulate an unfinished widget draft before its change event is delivered.
      The bridge is reconnected before the unrelated state update being tested. }
    LChange := LMemo.OnChange;
    LMemo.OnChange := nil;
    LMemo.Text := 'Unfinished focused draft';
    LMemo.SelStart := 4;
    LMemo.SelLength := 5;
    LMemo.OnChange := LChange;
    LApplication.State.SetValue(NyxIntegerState('quantity'), 4);
    Check((MemoText(LMemo) = 'Unfinished focused draft') and (LMemo.SelStart = 4) and
      (LMemo.SelLength = 5) and (LProbe.Calls = LCalls),
      'unrelated native update preserves draft and selection');
    LMemo.OnChange(LMemo);
    LApplication.State.SetValue(NyxBooleanState('readonly'), True);
    Check(LMemo.ReadOnly, 'read-only state reaches native memo');
    LApplication.State.SetValue(NyxBooleanState('readonly'), False);
    LApplication.State.SetValue(NyxBooleanState('enabled'), False);
    Check(not LMemo.Enabled and not LCheckbox.Enabled,
      'ancestor disabled state reaches native controls');
    LApplication.State.SetValue(NyxBooleanState('enabled'), True);
    Check(LMemo.Enabled and LCheckbox.Enabled, 'native enabled state recovers in place');
    LApplication.State.SetValue(NyxBooleanState('visible'), False);
    Check(not LApplication.View.ControlFor('reply-caption').Visible,
      'visibility false hides native caption');
    LApplication.State.SetValue(NyxBooleanState('visible'), True);
    Check(LApplication.View.ControlFor('reply-caption').Visible,
      'visibility true restores native caption');
    LApplication.State.SetValue(NyxIntegerState('width'), 420);
    Check(LApplication.View.ControlFor('reply-memo').Width = 420,
      'layout state updates native field geometry');
    LApplication.ShowPage('review');
    Check(TNyxText(TLabel(LApplication.View.ControlFor('review-caption')).Caption) =
      'Unfinished focused draft', 'native navigation keeps application state');
    Check((LRetained.SourceID = 'reply-memo') and
      (LRetained.Value.AsText = TNyxText('Crafted / 🌙' + #10 + 'Second line')),
      'native retained event survives navigation and control destruction');
    LRevision := LApplication.State.Revision;
    LRejected := False;
    try
      LApplication.State.SetValue(NyxIntegerState('quantity'), 1000);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LApplication.State.Revision = LRevision),
      'unmounted native page constraints reject invalid updates');
    LApplication.ShowPage('editor');
    LMemo := TMemo(LApplication.View.InputFor('reply-memo'));
    Check((MemoText(LMemo) = 'Unfinished focused draft') and
      (LDocument.State.GetValue(NyxTextState('🌙/reply')) = TNyxText('Café / 🌙 / 漢字')),
      'returning native page reconstructs runtime values without changing defaults');
    TNyxLCLButton(LApplication.View.ControlFor(
      LApplication.View.Root.Find('search').Part('clear').ID)).Click;
    Check((MemoText(LMemo) = '') and (LApplication.State.GetValue(NyxTextState('🌙/reply')) = ''),
      'compound native clear synchronizes state and memo');
    Check(LProbe.LastInfo.HasValue and (LProbe.LastInfo.ValueKind = nskText) and
      (LProbe.LastInfo.Value.AsText = '') and (LProbe.LastInfo.SourceID = 'search'),
      'native clear callback distinguishes empty text from absent data');
    LApplication.State.SetValue(NyxTextState('🌙/reply'), 'Declared query / 🌙');
    TNyxLCLButton(LApplication.View.ControlFor(
      LApplication.View.Root.Find('search').Part('search').ID)).Click;
    Check(LProbe.LastInfo.HasValue and
      (LProbe.LastInfo.Value.AsText = TNyxText('Declared query / 🌙')) and
      (LProbe.LastInfo.ValueID = LApplication.View.Root.Find('search').Part('query').ID) and
      (LProbe.LastInfo.TargetID = LApplication.View.Root.Find('search').Part('search').ID),
      'native search declares a payload separate from its command target');
    TNyxLCLButton(LApplication.View.ControlFor('fixture-ratio-commit')).Click;
    Check((LProbe.LastInfo.ValueKind = nskNumber) and
      (LProbe.LastInfo.Value.AsNumber = 0.125) and (LProbe.LastInfo.ValueID = 'fixture-ratio-input'),
      'native named field payload is numeric');
    LNumber := TEdit(LApplication.View.InputFor('fixture-ratio-input'));
    LNumber.Text := '0.5';
    LNumber.OnEditingDone(LNumber);
    Check((LApplication.View.Root.Find('fixture-ratio-input').Prop('value') = '0.5') and
      (LProbe.LastInfo.ValueKind = nskNumber), 'native field admits a declared choice');
    LCalls := LProbe.Calls;
    LNumber.Text := '0.2';
    LNumber.OnEditingDone(LNumber);
    Check((TNyxText(LNumber.Text) = '0.5') and (LProbe.Calls = LCalls) and
      (LApplication.View.LastBindingError <> ''), 'native rejects an undeclared choice atomically');
    TNyxLCLButton(LApplication.View.ControlFor(
      LApplication.View.Root.Find('fixture-rating').Part('star-5').ID)).Click;
    Check((LProbe.LastInfo.ValueKind = nskInteger) and (LProbe.LastInfo.Value.AsInteger = 5),
      'native rating selection emits an integer');
    LNumber := TEdit(LApplication.View.InputFor('fixture-integer-input'));
    LNumber.Text := '2147483646';
    LNumber.OnEditingDone(LNumber);
    Check((LProbe.LastInfo.ValueKind = nskInteger) and
      (LProbe.LastInfo.Value.AsInteger = 2147483646),
      'native numeric input retains the full declared integer family');
    LCalls := LProbe.Calls;
    LNumber.Text := '1.5';
    LNumber.OnEditingDone(LNumber);
    Check((TNyxText(LNumber.Text) = '2147483646') and (LProbe.Calls = LCalls),
      'native integer domain rejects fractions without narrowing or notification');
  finally
    LToken.Free;
    LApplication.Free;
    LProbe.Free;
    LDocument.Free;
  end;
  Check(LRetained.Value.AsText = TNyxText('Crafted / 🌙' + #10 + 'Second line'),
    'native retained event outlives its application');
end;

end.
