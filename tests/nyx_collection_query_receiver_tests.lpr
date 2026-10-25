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
program nyx_collection_query_receiver_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.collections, nyx.scheduler, nyx.linq,
  nyx.linq.execution, nyx.linq.publication
  {$ifdef PAS2JS}, Web{$endif};

type
  { A validator can revoke delivery without vetoing the candidate itself. The
    notify failure instead happens after commit and must retain that result. }
  TReceiverProbeMode = (rpmObserve, rpmClose, rpmCancel, rpmReject, rpmNotifyFail);

  { Borrowed callbacks are disconnected before this probe dies. A retained port
    outlives its receiver lease; neither test nor implementation owns a widget. }
  TProbe = class
  public
    Receiver: INyxCollectionQueryReceiver;
    Scope: INyxCancellationScope;
    Mode: TReceiverProbeMode;
    Notifications: Integer;
    procedure Validate(const AData: INyxCollectionSnapshot;
      const AChanges: INyxCollectionChanges);
    procedure Changed(const AStore: INyxCollection; const AChanges: INyxCollectionChanges);
  end;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
  begin
    raise Exception.Create('Query receiver: ' + AReason);
  end;
end;

procedure TProbe.Validate(const AData: INyxCollectionSnapshot;
  const AChanges: INyxCollectionChanges);
begin
  case Mode of
    rpmClose:
      begin
        Receiver.Close;
        Receiver := nil;
      end;
    rpmCancel:
      Scope.Cancel;
    rpmReject:
      raise ENyxCollection.Create('Expected source veto');
    rpmObserve, rpmNotifyFail:
      begin
        { Ordinary admission is unchanged. }
      end;
  end;
end;

procedure TProbe.Changed(const AStore: INyxCollection;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Notifications);

  if Mode = rpmNotifyFail then
  begin
    raise ENyxCollection.Create('Expected committed notification failure');
  end;
end;

procedure Run;
const
  CExact: TNyxText = 'New 🌙' + #0 + #13#10 + 'value';
var
  LScheduler: INyxScheduler;
  LStore: INyxCollection;
  LBefore: INyxCollectionSnapshot;
  LProbe: TProbe;
  LToken: INyxCollectionSubscription;
  LPort: INyxQueryResultPort<TNyxCollectionItem>;
  LValues: TNyxSequenceArray<TNyxCollectionItem>;
  LItem: TNyxItemRef;
  LRefused: Boolean;
begin
  LProbe := TProbe.Create;
  try
    LScheduler := NewNyxScheduler;
    LItem := NyxItem(NyxCollection('receiver'), 'record');
    LStore := NewNyxCollection(LItem.Collection, NyxCollectionSchema.Text(NyxTextField('name'), ''),
      [NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Old')]);
    LToken := LStore.Subscribe(LProbe.Changed, LProbe.Validate);
    SetLength(LValues, 1);
    LValues[0] := NyxCollectionItem(LItem).WithValue(NyxTextField('name'), CExact);
    LProbe.Scope := NewNyxCancellationScope;
    LProbe.Receiver := NyxCollectionQueryReceiver(LScheduler, LStore, LProbe.Scope);
    LPort := LProbe.Receiver.Port;
    LProbe.Mode := rpmClose;
    LBefore := LStore.Snapshot;
    Check(not LPort.Publish(LValues) and (LStore.Snapshot = LBefore) and
      (LProbe.Notifications = 0), 'closure inside validation discards before source installation');
    Check(not LPort.Publish(LValues), 'a retained closed port cannot revive its destination');
    LProbe.Scope := NewNyxCancellationScope;
    LProbe.Receiver := NyxCollectionQueryReceiver(LScheduler, LStore, LProbe.Scope);
    LPort := LProbe.Receiver.Port;
    LProbe.Mode := rpmCancel;
    Check(not LPort.Publish(LValues) and (LStore.Snapshot = LBefore) and
      (LProbe.Notifications = 0), 'generation cancelled by a validator refuses final installation');
    LProbe.Scope := NewNyxCancellationScope;
    LProbe.Receiver := NyxCollectionQueryReceiver(LScheduler, LStore, LProbe.Scope);
    LPort := LProbe.Receiver.Port;
    LProbe.Mode := rpmReject;
    LRefused := False;
    try
      LPort.Publish(LValues);
    except
      on LException: ENyxCollection do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Snapshot = LBefore) and (LProbe.Notifications = 0),
      'ordinary validator veto retains its original atomic refusal');
    LProbe.Mode := rpmObserve;
    Check(LPort.Publish(LValues) and (LStore.Snapshot.Revision = LBefore.Revision + 1) and
      (LStore.Snapshot.Item(LItem).GetValue(NyxTextField('name')) = CExact) and
      (LProbe.Notifications = 1), 'successful delivery publishes exact scalar text once');
    LRefused := False;
    try
      LPort.Publish(LValues);
    except
      on LException: ENyxQuery do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LProbe.Notifications = 1), 'the captured receiver revision cannot overwrite a newer source');
    LProbe.Receiver := NyxCollectionQueryReceiver(LScheduler, LStore, LProbe.Scope);
    LPort := LProbe.Receiver.Port;
    LValues[0] := NyxCollectionItem(LItem).WithValue(NyxTextField('name'), 'Committed');
    LProbe.Mode := rpmNotifyFail;
    LRefused := False;
    try
      LPort.Publish(LValues);
    except
      on LException: ENyxCollectionNotification do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Snapshot.Item(LItem).GetValue(NyxTextField('name')) = 'Committed') and
      (LProbe.Notifications = 2), 'notification failure reports committed data rather than pretending rollback');
  finally
    LToken := nil;
    LPort := nil;
    LProbe.Receiver := nil;
    LProbe.Free;

    if LScheduler <> nil then
    begin
      LScheduler.Shutdown;
    end;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-query-receiver-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' collection query receiver checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-query-receiver-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
