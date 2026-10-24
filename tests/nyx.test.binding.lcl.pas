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
  nyx.codec,
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

type
  { The four existing public-renderer routes exercise distinct retirement
    boundaries without replacing the original shared native binding fixture. }
  TNativeRetirementAction = (nraNone, nraProgrammaticUnmount,
    nraExternalRemount, nraOwnedTransfer, nraCommittedFailure);
  { Borrowed forward link from the tracked native face to its test owner. }
  TNativeRetirementProbe = class;

  { This real custom control records destruction of the exact accepted face.
    Its probe outlives renderer retirement and the ordinary async frame drain. }
  TNativeRetirementCaption = class(TPanel)
  public
    Probe: TNativeRetirementProbe;
    BorrowedNode: TNyxNode;
    destructor Destroy; override;
  end;

  { The test owns renderer, document and host. An external Store is retained
    until after every callback. For nraOwnedTransfer only, Store is a borrowed
    observation of the renderer's OWN state: never free it from this probe.
    The old entered generation and exact successor must share that real owner. }
  TNativeRetirementProbe = class
  public
    Renderer: TNyxLCLRenderer;
    Document: TNyxDocument;
    Host: TForm;
    Store: TNyxState;
    WatchedControl: TNativeRetirementCaption;
    Action: TNativeRetirementAction;
    Armed: Boolean;
    Updates: Integer;
    Destroyed: Integer;
    OldEvents: Integer;
    SuccessorEvents: Integer;
    SawBusyStore: Boolean;
    SawExactStateTransfer: Boolean;
    SawLiveControl: Boolean;
    SawLiveNode: Boolean;
    SawDestructorNode: Boolean;
    SawOwnedStateAfterSuccessor: Boolean;
    RaisedNotification: Boolean;
    RevisionAtEntry: Integer;
    procedure Update(ANode: TNyxNode; AControl: TControl);
    procedure Event(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

var
  { Existing factory callbacks are plain procedures. This temporary borrowed
    receiver is installed only while its independent journey owns every face. }
  GNativeRetirementProbe: TNativeRetirementProbe;

destructor TNativeRetirementCaption.Destroy;
begin

  if (Probe <> nil) and (Probe.WatchedControl = Self) then
  begin
    { The real deferred control destructor still owns its original borrowed
      node. The panel lease must outlive every later-created custom control. }
    Probe.SawDestructorNode := (BorrowedNode <> nil) and
      (BorrowedNode.ID = 'retirement-caption');
    Inc(Probe.Destroyed);
  end;
  inherited Destroy;
end;

function NativeRetirementFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
var
  LCaption: TNativeRetirementCaption;
begin
  LCaption := TNativeRetirementCaption.Create(AOwner);
  LCaption.Probe := GNativeRetirementProbe;
  LCaption.BorrowedNode := ANode;
  LCaption.BevelOuter := bvNone;
  Result := LCaption;
end;

procedure NativeRetirementUpdate(ANode: TNyxNode; AControl: TControl);
begin
  GNativeRetirementProbe.Update(ANode, AControl);
end;

procedure TNativeRetirementProbe.Update(ANode: TNyxNode; AControl: TControl);
var
  LText: TNyxText;
  LAction: TNativeRetirementAction;
begin
  LText := ANode.Prop('text');
  TPanel(AControl).Caption := LText;

  if not Armed or (ANode.ID <> 'retirement-caption') then
  begin
    Exit;
  end;
  Armed := False;
  Inc(Updates);
  LAction := Action;
  RevisionAtEntry := Store.Revision;
  SawBusyStore := Store.Busy;
  case LAction of
    nraNone:
      raise ENyxState.Create('Native retirement requires an explicit scenario');
    nraProgrammaticUnmount:
      Renderer.Unmount;
    nraExternalRemount:
      begin
        Renderer.Unmount;
        Renderer.Render(Document, Document.Pages[1], Host, Store);
        { Only the successor's shared binding remains. An old subscriber would
          reject removing its still-bound old-only key immediately here. }
        Store.Remove(NyxTextState('retirement-only'));
        Store.SetValue(NyxTextState('retirement-shared'), 'Successor accepted');
      end;
    nraOwnedTransfer:
      begin
        { Pass the exact currently owned state through FULL remount, without
          first unmounting or manufacturing ownership from pointer equality. }
        Renderer.Render(Document, Document.Pages[1], Host, Store);
        SawExactStateTransfer := Renderer.State = Store;
        Renderer.Unmount;
        { Destroy the accepted successor BEFORE the old command returns. Its
          shared state owner must still be leased by the old entered call. }
        SawOwnedStateAfterSuccessor :=
          (Store.GetValue(NyxTextState('retirement-only')) = 'Old accepted') and
          (Store.Revision = RevisionAtEntry);
      end;
    nraCommittedFailure:
      begin
        { State notification is already Busy/committed. Use a separate owned
          successor state rather than subscribing to that busy external store. }
        Renderer.Render(Document, Document.Pages[1], Host);
      end;
  end;
  { Pumping existing queued retirement while still entered must NOT destroy
    this borrowed physical face or its node. No synthetic sleep/wait is added. }
  Application.ProcessMessages;
  SawLiveControl := (Destroyed = 0) and
    (TNyxText(TPanel(AControl).Caption) = LText);
  TPanel(AControl).Caption := LText + ' / entered callback still owns this face';
  SawLiveNode := ANode.Prop('text') = LText;

  if LAction = nraCommittedFailure then
  begin
    RaisedNotification := True;
    raise ENyxState.Create('Intentional retired native notification failure');
  end;
end;

procedure TNativeRetirementProbe.Event(ANode: TNyxNode;
  const AEvent: TNyxEventInfo);
begin

  if (ANode.ID = 'retirement-click') or (ANode.ID = 'retirement-entry') then
  begin
    Inc(OldEvents);
  end;

  if ANode.ID = 'successor-click' then
  begin
    Inc(SuccessorEvents);
  end;
end;

{ The seed is independent of CreateNyxBindingFixture and all old control
  assertions. Its old-only field makes stale validation observable; its labeled
  no-op Click enters real Command/Sync without assigning any state. }
function CreateNativeRetirementFixture: TNyxDocument;
var
  LPage, LNode: TNyxNode;
begin
  Result := TNyxDocument.Create;
  try
    Result.State.SetValue(NyxTextState('retirement-only'), 'Old accepted')
      .SetValue(NyxTextState('retirement-shared'), 'Shared accepted');
    LPage := TNyxNode.Create(nkPage, 'retirement-page');
    Result.AddPage(LPage);
    LPage.Configure.Layout(nlColumn).Done;
    LNode := TNyxNode.Create(nkLabel, 'retirement-caption');
    LPage.Add(LNode);
    LNode.Binds.Text(NyxTextState('retirement-shared')).Done;
    LNode := TNyxNode.Create(nkMemo, 'retirement-entry');
    LPage.Add(LNode);
    LNode.Binds.Value(NyxTextState('retirement-only')).Done;
    LNode := TNyxNode.Create(nkButton, 'retirement-click');
    LPage.Add(LNode);
    LNode.Configure.Text('Retirement command').OnClick(NyxEvent('retirement-click')).Done;
    LPage := TNyxNode.Create(nkPage, 'retirement-successor');
    Result.AddPage(LPage);
    LPage.Configure.Layout(nlColumn).Done;
    LNode := TNyxNode.Create(nkLabel, 'successor-caption');
    LPage.Add(LNode);
    LNode.Binds.Text(NyxTextState('retirement-shared')).Done;
    LNode := TNyxNode.Create(nkButton, 'successor-click');
    LPage.Add(LNode);
    LNode.Configure.Text('Successor command').OnClick(NyxEvent('successor-click')).Done;
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function RunNativeRetirementCases: Integer;
var
  LProbe: TNativeRetirementProbe;
  LExternalStore: TNyxState;
  LAction: TNativeRetirementAction;
  LRevision: Integer;
  LMemo: TMemo;
  LBefore: TNyxText;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxState.Create('Native retirement: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  for LAction := nraProgrammaticUnmount to nraCommittedFailure do
  begin
    LProbe := TNativeRetirementProbe.Create;
    LExternalStore := nil;
    try
      GNativeRetirementProbe := LProbe;
      LProbe.Document := CreateNativeRetirementFixture;
      LBefore := TNyxCodec.Encode(LProbe.Document);
      LProbe.Renderer := TNyxLCLRenderer.Create;
      LProbe.Host := TForm.CreateNew(nil);
      LProbe.Host.ClientWidth := 1000;
      LProbe.Host.ClientHeight := 800;
      LProbe.Renderer.RegisterFactory('label', NativeRetirementFactory,
        NativeRetirementUpdate);
      LProbe.Renderer.OnEvent := LProbe.Event;

      if LAction = nraOwnedTransfer then
      begin
        LProbe.Renderer.Render(LProbe.Document, LProbe.Document.Pages[0], LProbe.Host);
      end
      else
      begin
        LExternalStore := LProbe.Document.State.Clone;
        LProbe.Renderer.Render(LProbe.Document, LProbe.Document.Pages[0],
          LProbe.Host, LExternalStore);
      end;
      LProbe.Store := LProbe.Renderer.State;
      LProbe.WatchedControl := TNativeRetirementCaption(
        LProbe.Renderer.ControlFor('retirement-caption'));
      LRevision := LProbe.Store.Revision;
      LProbe.Action := LAction;
      LProbe.Armed := True;
      case LAction of
        nraNone:
          raise ENyxState.Create('Native retirement requires an explicit scenario');
        nraProgrammaticUnmount:
          LExternalStore.SetValue(NyxTextState('retirement-shared'), 'Programmatic accepted');
        nraExternalRemount, nraOwnedTransfer:
          TNyxLCLButton(LProbe.Renderer.ControlFor('retirement-click')).Click;
        nraCommittedFailure:
          begin
            LMemo := TMemo(LProbe.Renderer.InputFor('retirement-entry'));
            LMemo.Text := 'Committed before retired notification failure';
          end;
      end;
      Check((LProbe.Updates = 1) and LProbe.SawLiveControl and LProbe.SawLiveNode,
        'entered native updater retains its exact borrowed control and node through retirement');
      Application.ProcessMessages;
      Check(LProbe.SawDestructorNode,
        'actual retired control destructor retains the exact borrowed node');
      Check(LProbe.Destroyed = 1,
        'retired native physical face is destroyed exactly once after the entered callback unwinds');
      Check(LProbe.OldEvents = 0,
        'retired command cannot emit its stale old semantic Click');
      case LAction of
        nraNone:
          raise ENyxState.Create('Native retirement requires an explicit scenario');
        nraProgrammaticUnmount:
          begin
            Check(LProbe.SawBusyStore and (LProbe.Renderer.Root = nil) and
              (LExternalStore.Revision = LRevision + 1) and
              (LExternalStore.GetValue(NyxTextState('retirement-shared')) = 'Programmatic accepted'),
              'programmatic Sync retirement preserves the already committed external state');
            LExternalStore.SetValue(NyxTextState('retirement-shared'), 'After programmatic unmount');
            Check(LProbe.Updates = 1,
              'programmatic unmount removes its old native subscriber immediately');
          end;
        nraExternalRemount:
          begin
            Check(not LProbe.SawBusyStore and (LProbe.RevisionAtEntry = LRevision) and
              (LProbe.Renderer.State = LExternalStore) and
              not LExternalStore.Has('retirement-only') and
              (LExternalStore.Revision = LRevision + 2) and
              (TNyxText(TPanel(LProbe.Renderer.ControlFor('successor-caption')).Caption) =
                'Successor accepted'),
              'no-assignment Click remounts the same external store and removes the old-only validator');
            TNyxLCLButton(LProbe.Renderer.ControlFor('successor-click')).Click;
            Check((LProbe.SuccessorEvents = 1) and (LProbe.OldEvents = 0) and
              (LProbe.Renderer.LastBindingFailure = nbfNone),
              'successor Click remains usable without stale old semantic or diagnostic tails');
          end;
        nraOwnedTransfer:
          begin
            { Store is no longer ours to read after the old command returns. }
            LProbe.Store := nil;
            Check(not LProbe.SawBusyStore and LProbe.SawExactStateTransfer and
              LProbe.SawOwnedStateAfterSuccessor and (LProbe.Renderer.Root = nil),
              'exact renderer-owned state survives successor destruction until its old entered command returns');
            Check(LProbe.Renderer.LastBindingFailure = nbfNone,
              'owned-state successor retirement leaves no stale old command error');
          end;
        nraCommittedFailure:
          begin
            Check(LProbe.SawBusyStore and LProbe.RaisedNotification and
              (LExternalStore.Revision = LRevision + 1) and
              (LExternalStore.GetValue(NyxTextState('retirement-only')) =
                'Committed before retired notification failure'),
              'retired notification failure preserves the external command commitment without rollback');
            Check((LProbe.Renderer.State <> LExternalStore) and
              (LProbe.Renderer.State.GetValue(NyxTextState('retirement-only')) = 'Old accepted') and
              (LProbe.Renderer.LastBindingError = '') and
              (LProbe.Renderer.LastBindingFailure = nbfNone),
              'old committed failure cannot overwrite independent successor state or error diagnostics: ' +
              'same external=' + BoolToStr(LProbe.Renderer.State = LExternalStore, True) +
              ', successor value=' + LProbe.Renderer.State.GetValue(NyxTextState('retirement-only')) +
              ', error=' + LProbe.Renderer.LastBindingError +
              ', failure=' + IntToStr(Ord(LProbe.Renderer.LastBindingFailure)));
            TNyxLCLButton(LProbe.Renderer.ControlFor('successor-click')).Click;
            Check((LProbe.SuccessorEvents = 1) and (LProbe.OldEvents = 0),
              'independent successor remains interactive after an old notification failure');
          end;
      end;
      Check(TNyxCodec.Encode(LProbe.Document) = LBefore,
        'native retirement never mutates the independent authored defaults');
    finally
      LProbe.Armed := False;
      try
        FreeAndNil(LProbe.Renderer);
      finally
        try
          Application.ProcessMessages;
        finally
          LExternalStore.Free;
          LProbe.Host.Free;
          LProbe.Document.Free;
          GNativeRetirementProbe := nil;
          LProbe.Free;
        end;
      end;
    end;
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
  Inc(Result, RunNativeRetirementCases);
  LDocument := CreateNyxBindingFixture;
  LApplication := TNyxLCLApplication.Create;
  LProbe := TNativeBindingProbe.Create;
  LToken := nil;
  try
    LApplication.View.OnEvent := LProbe.Event;
    LApplication.Mount(LDocument);
    { Native alignment is deferred until the mounted window is realized. Width
      bindings must be checked in an actual admitted viewport, not against the
      temporary 150-pixel default of an unrealized child panel. }
    LApplication.Window.Show;
    Application.ProcessMessages;
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
      'layout state updates native field geometry: actual=' +
      IntToStr(LApplication.View.ControlFor('reply-memo').Width) + ', model=' +
      LApplication.View.Root.Find('reply-memo').Prop('width') + ', parent=' +
      IntToStr(LApplication.View.ControlFor('reply-memo').Parent.ClientWidth));
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
