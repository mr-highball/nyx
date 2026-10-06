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
unit nyx.authority.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data;

type
  { Borrowed synchronous qualification driver. Its owner keeps the selected
    independent project/review and router alive throughout the journey. Call
    supplies distinct trusted connection owners with an identical display actor;
    Snapshot is trusted editor observation, never an agent whole-document query. }
  TNyxAuthorityDriver = class abstract
  public
    function Call(const ATool, AOwner: TNyxText;
      const AArguments: TNyxDataValue): TNyxDataValue; virtual; abstract;
    function Snapshot: TNyxDataValue; virtual; abstract;
  end;

{ Exercise actual semantic edits, callback/root removal tickets and paired history
  across two same-named connections. The driver remains borrowed; CheckCount gains
  only completed assertions. Refusal must preserve exact pair/navigation/history. }
procedure VerifyNyxAgentAuthority(ADriver: TNyxAuthorityDriver; var ACheckCount: Integer);

implementation

uses
  SysUtils;

procedure VerifyNyxAgentAuthority(ADriver: TNyxAuthorityDriver; var ACheckCount: Integer);
var
  LFirstRequest: TNyxDataValue;
  LFirstReceipt: TNyxDataValue;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LAdds: TNyxDataValue;
  LChanges: TNyxDataValue;
  LFirstReview: TNyxDataValue;
  LSecondReview: TNyxDataValue;
  LCallbackRequest: TNyxDataValue;
  LCallbackReceipt: TNyxDataValue;
  LRoots: TNyxDataValue;
  LRootRequest: TNyxDataValue;
  LRootReceipt: TNyxDataValue;
  LBeforeRemoval: TNyxDataValue;

  procedure Check(AValue: Boolean; const AReason: TNyxText);
  begin

    if not AValue then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(ACheckCount);
  end;

  function Revision: TNyxDataValue;
  begin
    Result := ADriver.Call('nyx_session', 'connection-one', NyxObject([])).Field('revision');
  end;

  function Transaction(const AOperation: TNyxText;
    const AOperations: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('expectedRevision', Revision),
      NyxField('operationId', NyxData(AOperation)), NyxField('operations', AOperations)]);
  end;

  function Title(const AText: TNyxText): TNyxDataValue;
  begin
    Result := Transaction('same-operation', NyxArray([NyxObject([
      NyxField('op', NyxData('title')), NyxField('value', NyxData(AText))])]));
  end;

  procedure Refuses(const ATool, AOwner: TNyxText; const AArguments: TNyxDataValue);
  const
    CFields: array[0..5] of TNyxText =
      ('revision', 'selection', 'view', 'pendingDraft', 'canUndo', 'canRedo');
  var
    LBefore: TNyxDataValue;
    LAfter: TNyxDataValue;
    LRefused: Boolean;
    LIndex: Integer;
  begin
    LBefore := ADriver.Snapshot;
    LRefused := False;
    try
      ADriver.Call(ATool, AOwner, AArguments);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, ATool + ' refuses foreign authority or a stale request');
    LAfter := ADriver.Snapshot;
    Check(LBefore.Field('project').AsText = LAfter.Field('project').AsText,
      'Authority refusal retains both exact accepted files and draft/base');
    for LIndex := 0 to High(CFields) do
    begin
      Check(LBefore.Field('session').Field(CFields[LIndex]).ToJSON =
        LAfter.Field('session').Field(CFields[LIndex]).ToJSON,
        'Authority refusal retains ' + CFields[LIndex]);
    end;
  end;

  function CallbackChange(const AOperation, ARegistration: TNyxText): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
  begin
    SetLength(LFields, 3);
    LFields[0] := NyxField('op', NyxData(AOperation));
    LFields[1] := NyxField('id', NyxData('authority-button'));
    LFields[2] := NyxField('event', NyxObject([NyxField('trigger', NyxData('click'))]));

    if ARegistration <> '' then
    begin
      SetLength(LFields, 4);
      LFields[3] := NyxField('registration', NyxData(ARegistration));
    end;
    Result := NyxObject(LFields);
  end;

  function Removal(const AKey, AMode, AOperation: TNyxText;
    const AChanges: TNyxDataValue; const AReview: TNyxDataValue): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
  begin
    SetLength(LFields, 3);
    LFields[0] := NyxField('mode', NyxData(AMode));
    LFields[1] := NyxField('expectedRevision', Revision);
    LFields[2] := NyxField(AKey, AChanges);

    if AMode = 'apply' then
    begin
      SetLength(LFields, 5);
      LFields[3] := NyxField('operationId', NyxData(AOperation));
      LFields[4] := NyxField('reviewID', AReview.Field('reviewID'));
    end;
    Result := NyxObject(LFields);
  end;

begin
  LFirstRequest := Title('First connection idea 😀');
  LFirstReceipt := ADriver.Call('nyx_transaction', 'connection-one', LFirstRequest);
  Check(ADriver.Call('nyx_transaction', 'connection-one', LFirstRequest).ToJSON =
    LFirstReceipt.ToJSON, 'Only the original connection receives its exact retry receipt');
  Refuses('nyx_transaction', 'connection-two', LFirstRequest);
  LRequest := Title('Second connection idea 😀');
  LReceipt := ADriver.Call('nyx_transaction', 'connection-two', LRequest);
  Check(LReceipt.Field('title').AsText = TNyxText('Second connection idea 😀'),
    'Same operation ID with a current revision is independent for the other connection');
  Check(ADriver.Call('nyx_transaction', 'connection-one', LFirstRequest).ToJSON =
    LFirstReceipt.ToJSON, 'A later foreign edit cannot replace the first connection receipt');

  ADriver.Call('nyx_transaction', 'connection-one', Transaction('create-owned-controls',
    NyxArray([NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('page')),
      NyxField('root', NyxData('page')), NyxField('id', NyxData('authority-page'))]),
      NyxObject([NyxField('op', NyxData('create')), NyxField('kind', NyxData('button')),
      NyxField('parent', NyxData('authority-page')),
      NyxField('id', NyxData('authority-button'))])])));
  LAdds := ADriver.Call('nyx_callbacks', 'connection-one', NyxObject([
    NyxField('mode', NyxData('apply')), NyxField('expectedRevision', Revision),
    NyxField('operationId', NyxData('add-callbacks')), NyxField('changes', NyxArray([
      CallbackChange('add', ''), CallbackChange('add', '')]))]));
  LChanges := NyxArray([CallbackChange('remove',
    LAdds.Field('callbacks').Item(0).Field('registration').AsText)]);
  LFirstReview := ADriver.Call('nyx_callbacks', 'connection-one',
    Removal('changes', 'review', '', LChanges, NyxNull));
  LCallbackRequest := Removal('changes', 'apply', 'same-callback-operation', LChanges, LFirstReview);
  Refuses('nyx_callbacks', 'connection-two', LCallbackRequest);
  LSecondReview := ADriver.Call('nyx_callbacks', 'connection-two',
    Removal('changes', 'review', '', LChanges, NyxNull));
  Check(LFirstReview.Field('reviewID').AsText <> LSecondReview.Field('reviewID').AsText,
    'Same-named connections own distinct callback removal tickets');
  LCallbackReceipt := ADriver.Call('nyx_callbacks', 'connection-one', LCallbackRequest);
  Check(ADriver.Call('nyx_callbacks', 'connection-one', LCallbackRequest).ToJSON =
    LCallbackReceipt.ToJSON, 'Consumed callback ticket permits the original exact retry');
  Refuses('nyx_callbacks', 'connection-two', LCallbackRequest);
  LChanges := NyxArray([CallbackChange('remove',
    LAdds.Field('callbacks').Item(1).Field('registration').AsText)]);
  LSecondReview := ADriver.Call('nyx_callbacks', 'connection-two',
    Removal('changes', 'review', '', LChanges, NyxNull));
  ADriver.Call('nyx_callbacks', 'connection-two',
    Removal('changes', 'apply', 'same-callback-operation', LChanges, LSecondReview));
  Check(ADriver.Call('nyx_callbacks', 'connection-one', LCallbackRequest).ToJSON =
    LCallbackReceipt.ToJSON, 'Independent foreign callback receipt does not overwrite the original');

  LRoots := NyxArray([NyxObject([NyxField('root', NyxData('page')),
    NyxField('id', NyxData('authority-page'))])]);
  LFirstReview := ADriver.Call('nyx_roots', 'connection-one',
    Removal('roots', 'review', '', LRoots, NyxNull));
  LRootRequest := Removal('roots', 'apply', 'same-root-operation', LRoots, LFirstReview);
  Refuses('nyx_roots', 'connection-two', LRootRequest);
  LBeforeRemoval := ADriver.Snapshot;
  LRootReceipt := ADriver.Call('nyx_roots', 'connection-one', LRootRequest);
  Check(ADriver.Call('nyx_roots', 'connection-one', LRootRequest).ToJSON = LRootReceipt.ToJSON,
    'Root removal receipt remains owned after consuming its ticket');
  Refuses('nyx_roots', 'connection-two', LRootRequest);
  LRequest := NyxObject([NyxField('expectedRevision', Revision),
    NyxField('operationId', NyxData('same-history-operation')),
    NyxField('direction', NyxData('undo'))]);
  LReceipt := ADriver.Call('nyx_history', 'connection-two', LRequest);
  Check(ADriver.Snapshot.Field('project').AsText = LBeforeRemoval.Field('project').AsText,
    'One paired Undo restores the exact source/design after reviewed root removal');
  Refuses('nyx_history', 'connection-one', LRequest);
  ADriver.Call('nyx_history', 'connection-one', NyxObject([
    NyxField('expectedRevision', Revision),
    NyxField('operationId', NyxData('same-history-operation')),
    NyxField('direction', NyxData('redo'))]));
  Check(ADriver.Call('nyx_history', 'connection-two', LRequest).ToJSON = LReceipt.ToJSON,
    'Independent history receipts retain their exact original result');
  Refuses('nyx_session', '', NyxObject([]));
  Refuses('nyx_session', TNyxText(StringOfChar('x', 121)), NyxObject([]));
end;

end.
