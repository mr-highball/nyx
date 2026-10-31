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
program nyx_collection_cell_context_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.collections,
  nyx.collections.arrangement, nyx.collections.cell, nyx.behavior
  , nyx.events, nyx.scheduler
  {$ifdef PAS2JS}, JS, Web{$else}, Classes{$endif};

type
  { A registration owns only its copied received event. It borrows no store,
    router, scheduler, view or callback owner, including after delayed delivery. }
  TCellReceiver = class(TNyxEventCallback)
  public
    Calls: Integer;
    Last: TNyxEventInfo;
    procedure Invoke(const AEvent: TNyxEventInfo;
      const AExecution: INyxExecution); override;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure TCellReceiver.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(Calls);
  Last := AEvent.Copy;
end;

{$ifdef PAS2JS}
function Turn: TJSPromise;
begin
  Result := TJSPromise.new(
    procedure(AResolve, AReject: TJSPromiseResolver)
    begin
      window.setTimeout(
        procedure
        begin
          AResolve(True);
        end, 1);
    end);
end;
{$endif}

procedure Run;
const
  CText: TNyxText = 'Original 🌙' + #0 + ' value' + #13#10 + 'Next';
var
  LKey: TNyxCollectionRef;
  LItem: TNyxItemRef;
  LCell: TNyxCollectionCellSnapshot;
  LOther: TNyxCollectionCellSnapshot;
  LValue: TNyxStateValue;
  LField: TNyxColumnIdentity;
  LEvent: TNyxEventInfo;
  LRetained: TNyxEventInfo;
  LKind: TNyxStateKind;
  LFailed: Boolean;
begin
  LKey := NyxCollection('services 🌙');
  LItem := NyxItem(LKey, 'gateway 🌙');
  LCell := Default(TNyxCollectionCellSnapshot);
  Check(not LCell.Defined and not LCell.Copy.Defined, 'Absent snapshots remain absent through copies');
  LFailed := False;
  try
    LValue := LCell.Value;
  except
    on ENyxCollection do LFailed := True;
  end;
  Check(LFailed, 'Absent snapshots cannot invent a text scalar');
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin
    case LKind of
      nskText:
        begin
          LField := NyxColumnIdentity(NyxTextField('detail 🌙'));
          LValue := TNyxStateValue.FromText(CText);
        end;
      nskBoolean:
        begin
          LField := NyxColumnIdentity(NyxBooleanField('enabled'));
          LValue := TNyxStateValue.FromBoolean(True);
        end;
      nskInteger:
        begin
          LField := NyxColumnIdentity(NyxIntegerField('pid'));
          LValue := TNyxStateValue.FromInteger(Low(Integer));
        end;
      nskNumber:
        begin
          LField := NyxColumnIdentity(NyxNumberField('measurement'));
          LValue := TNyxStateValue.FromNumber(1.25e100);
        end;
    end;
    LCell := NyxCollectionCellSnapshot(LItem, LField, LValue, 37);
    LOther := LCell.Copy;
    Check(LCell.Defined and LOther.Defined and
      (LOther.Item.ID = LItem.ID) and (LOther.Item.Collection.Name = LKey.Name),
      'A copied accepted cell owns exact scoped Unicode item identity');
    Check(LOther.Field.Same(LField) and (LOther.Field.Kind = LKind) and
      LOther.Value.SameValue(LValue) and (LOther.Revision = 37),
      'Each scalar family retains its exact field, raw value and source revision');
    LEvent := Default(TNyxEventInfo);
    LEvent.Trigger := ntContextMenu;
    LEvent.Name := NyxEvent(NyxTriggerName(ntContextMenu));
    LEvent.SourceID := 'table';
    LEvent.OriginID := 'table';
    LEvent.TargetID := 'table';
    LEvent.Value := NyxNull;
    LEvent.Details := NyxNull;
    LEvent.HasCollectionCell := True;
    LEvent.CollectionCell := LCell;
    LRetained := LEvent.Copy;
    LEvent.CollectionCell := NyxCollectionCellSnapshot(NyxItem(LKey, 'replacement'),
      NyxColumnIdentity(NyxTextField('other')), TNyxStateValue.FromText('New value'), 38);
    LCell := Default(TNyxCollectionCellSnapshot);
    Check(LRetained.HasCollectionCell and LRetained.CollectionCell.Defined and
      (LRetained.CollectionCell.Item.ID = LItem.ID) and
      LRetained.CollectionCell.Field.Same(LField) and
      LRetained.CollectionCell.Value.SameValue(LValue) and
      (LRetained.CollectionCell.Revision = 37),
      'Event copies cannot borrow a replacement cell, field family or newer revision');
    LValue := LRetained.CollectionCell.Value;
    LValue := TNyxStateValue.FromText('Caller replacement');
    Check(LRetained.CollectionCell.Value.SameValue(LOther.Value),
      'Replacing a returned scalar does not mutate retained event data');
    LRetained.HasCollectionCell := False;
    LEvent := LRetained.Copy;
    Check(not LEvent.HasCollectionCell and not LEvent.CollectionCell.Defined,
      'Masked event context copies stay explicitly absent');
  end;
  LFailed := False;
  try
    LCell := NyxCollectionCellSnapshot(LItem, NyxColumnIdentity(NyxIntegerField('pid')),
      TNyxStateValue.FromText('42'), 37);
  except
    on ENyxCollection do LFailed := True;
  end;
  Check(LFailed, 'Text cannot silently become the typed integer cell');
  LFailed := False;
  try
    LCell := NyxCollectionCellSnapshot(LItem, NyxColumnIdentity(NyxTextField('name')),
      TNyxStateValue.FromText(CText), -1);
  except
    on ENyxCollection do LFailed := True;
  end;
  Check(LFailed, 'Negative source revisions refuse before snapshot publication');
  LFailed := False;
  try
    LCell := NyxCollectionCellSnapshot(Default(TNyxItemRef),
      NyxColumnIdentity(NyxTextField('name')), TNyxStateValue.FromText(CText), 0);
  except
    on ENyxCollection do LFailed := True;
  end;
  Check(LFailed, 'Absent item identity cannot be presented as an accepted cell');
  LFailed := False;
  try
    LCell := NyxCollectionCellSnapshot(LItem, Default(TNyxColumnIdentity),
      TNyxStateValue.FromText(CText), 0);
  except
    on ENyxCollection do LFailed := True;
  end;
  Check(LFailed, 'Absent field identity cannot borrow another scalar family');
end;

procedure Scheduled; {$ifdef PAS2JS}async;{$endif}
const
  COriginal: TNyxText = 'Original 🌙' + #0 + ' service';
var
  LKey: TNyxCollectionRef;
  LItem: TNyxItemRef;
  LField: TNyxTextFieldRef;
  LStore: INyxCollection;
  LRevision: Integer;
  LEvents: INyxEvents;
  LStream: INyxEventStream;
  LFirst: TCellReceiver;
  LSecond: TCellReceiver;
  LFirstLease: INyxEventCallback;
  LSecondLease: INyxEventCallback;
  LFirstToken: INyxEventSubscription;
  LSecondToken: INyxEventSubscription;
  LTickets: TNyxExecutions;
  LEvent: TNyxEventInfo;
  LTry: Integer;
begin
  LKey := NyxCollection('scheduled services');
  LItem := NyxItem(LKey, 'gateway');
  LField := NyxTextField('name');
  LStore := NewNyxCollection(LKey, NyxCollectionSchema.Text(LField, ''), [
    NyxCollectionItem(LItem).WithValue(LField, COriginal)]);
  LRevision := LStore.Snapshot.Revision;
  LEvent := Default(TNyxEventInfo);
  LEvent.Trigger := ntContextMenu;
  LEvent.Name := NyxEvent(NyxTriggerName(ntContextMenu));
  LEvent.SourceID := 'table';
  LEvent.OriginID := 'table';
  LEvent.TargetID := 'table';
  LEvent.Value := NyxNull;
  LEvent.Details := NyxNull;
  LEvent.HasCollectionCell := True;
  LEvent.CollectionCell := NyxCollectionCellSnapshot(LItem,
    NyxColumnIdentity(LField), TNyxStateValue.FromText(COriginal), LRevision);
  LEvents := NewNyxEvents;
  LFirst := TCellReceiver.Create;
  LFirstLease := LFirst;
  LSecond := TCellReceiver.Create;
  LSecondLease := LSecond;
  try
    LStream := LEvents.OnContextMenu(NyxControlEvents('table')).Policy(neUIQueue);
    LFirstToken := LStream.Subscribe(LFirstLease);
    LSecondToken := LStream.Subscribe(LSecondLease);
    LTickets := LEvents.Dispatch(LEvent, 'table', 'table');
    Check((Length(LTickets) = 2) and (LFirst.Calls = 0) and (LSecond.Calls = 0),
      'Two ordinary queued registrations capture input before delayed callbacks');
    LStore.Apply([NyxUpdate(NyxCollectionItem(LItem).WithValue(LField, 'Later service'))]);
    LEvent.CollectionCell := NyxCollectionCellSnapshot(NyxItem(LKey, 'replacement'),
      NyxColumnIdentity(LField), TNyxStateValue.FromText('Replacement'),
      LStore.Snapshot.Revision);
    LStore := nil;
    LEvent := Default(TNyxEventInfo);
    for LTry := 1 to 100 do
    begin
      {$ifdef PAS2JS}await(Turn);{$else}CheckSynchronize(10);{$endif}

      if (LFirst.Calls = 1) and (LSecond.Calls = 1) then
      begin
        Break;
      end;
    end;
    Check((LFirst.Calls = 1) and (LSecond.Calls = 1) and
      (LTickets[0].Status = nesSucceeded) and (LTickets[1].Status = nesSucceeded),
      'Ordinary scheduling delivers each original context once');
    Check(LFirst.Last.HasCollectionCell and LSecond.Last.HasCollectionCell and
      (LFirst.Last.CollectionCell.Item.ID = 'gateway') and
      (LSecond.Last.CollectionCell.Item.ID = 'gateway') and
      (LFirst.Last.CollectionCell.Value.TextValue = COriginal) and
      (LSecond.Last.CollectionCell.Value.TextValue = COriginal) and
      (LFirst.Last.CollectionCell.Revision = LRevision) and
      (LSecond.Last.CollectionCell.Revision = LRevision),
      'Delayed siblings retain exact original cells after publication, source release and event replacement');
    LEvents.Close;
    Check((LFirst.Last.Copy.CollectionCell.Value.TextValue = COriginal) and
      (LSecond.Last.Copy.CollectionCell.Value.TextValue = COriginal),
      'Retained callback values outlive router and subscription retirement');
  finally
    LEvents.Close;
    LFirstToken := nil;
    LSecondToken := nil;
    LStream := nil;
    LTickets := nil;
    LEvents := nil;
    LFirstLease := nil;
    LSecondLease := nil;
  end;
end;

procedure Main; {$ifdef PAS2JS}async;{$endif}
begin
  try
    Run;
    {$ifdef PAS2JS}await(Scheduled);{$else}Scheduled;{$endif}
    {$ifdef PAS2JS}
    document.body.setAttribute('data-cell-context', 'passed');
    document.body.setAttribute('data-cell-context-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' owned typed cell/context copy checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-cell-context', 'failed');
      document.body.setAttribute('data-cell-context-checks', IntToStr(GChecks));
      document.body.setAttribute('data-cell-context-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end;

begin
  Main;
end.
