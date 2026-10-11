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

{ Portable ordinary/semantic ordering qualification, included by the existing
  transaction consumer on checked FPC and actual HTTP pas2js. These independent
  literal owners confer no synthetic executed-source or transport authority. }
procedure RootOrderParity;
var
  LSource: TNyxDocument;
  LExpected: TNyxDocument;
  LActual: TNyxDocument;
  LSession: TNyxStudioSession;
  LSemantic: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LArguments: TNyxDataValue;
  LPatch: INyxDesignPatch;
  LChanges: array of TNyxRootMoveChange;
  LIndex: Integer;
  LRejected: Boolean;
  LInitial: TNyxText;
  LOrdered: TNyxText;

  function Frame: TNyxDataValue;
  begin
    Result := LSemantic.Exchange(NyxObject([NyxField('op', NyxData('observe'))]));
  end;

  function RootArguments(const AID: TNyxText;
    const AOperations: TNyxDataValue): TNyxDataValue;
  begin
    Result := NyxObject([NyxField('expectedRevision', NyxData(LSemantic.Revision)),
      NyxField('operationId', NyxData(AID)), NyxField('operations', AOperations)]);
  end;

  procedure Refuse(const AID: TNyxText; const AOperations: TNyxDataValue);
  var
    LPrior: TNyxDataValue;
    LCurrent: TNyxDataValue;
    LRefused: Boolean;
  begin
    LPrior := Frame;
    LRefused := False;
    try
      LSemantic.Call('nyx_transaction', 'Root ordering', RootArguments(AID, AOperations));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    LCurrent := Frame;
    Check(LRefused and (LCurrent.Field('project').ToJSON = LPrior.Field('project').ToJSON) and
      (LCurrent.Field('session').Field('revision').ToJSON =
        LPrior.Field('session').Field('revision').ToJSON) and
      (LCurrent.Field('session').Field('selection').ToJSON =
        LPrior.Field('session').Field('selection').ToJSON) and
      (LCurrent.Field('session').Field('view').ToJSON =
        LPrior.Field('session').Field('view').ToJSON) and
      (LCurrent.Field('session').Field('canUndo').ToJSON =
        LPrior.Field('session').Field('canUndo').ToJSON) and
      (LCurrent.Field('session').Field('canRedo').ToJSON =
        LPrior.Field('session').Field('canRedo').ToJSON),
      'refused root movement retains the complete pair/navigation/history: ' + AID);
  end;

begin
  LSource := TNyxDocument.Create;
  LExpected := nil;
  LActual := nil;
  LSession := nil;
  LSemantic := nil;
  try
    LSource.AddPage(NewNyxPage('first'));
    LSource.AddPage(NewNyxPage('middle').Add(NewNyxLabel('middle-caption')
      .Configure.Text('An exact rocket 🚀 and letter 𐐷.').Done));
    LSource.AddPage(NewNyxPage('last'));
    LSource.AddComponent(NewNyxCard('plain-card'));
    LSource.AddComponent(NewNyxColumn('stack-card'));
    LSource.Find('middle').Add(NewNyxComponent('card-instance')
      .Configure.Component(NyxComponent('plain-card')).Done);
    LSource.State.SetValue(NyxTextState('note'), 'Retained default');
    LPair := NyxProjectPair(TNyxCodec.Encode(LSource), TNyxCodegen.Generate(LSource));
    LInitial := EncodeNyxProject(LPair);
    SetLength(LChanges, 3);
    LChanges[0] := NyxMoveRoot(NyxPageRoot('last'), 0);
    LChanges[1] := NyxMoveRoot(NyxPageRoot('first'), 2);
    LChanges[2] := NyxMoveRoot(NyxReusableRoot('stack-card'), 0);
    LPatch := NyxRootOrderPatch(LChanges);
    LChanges[0] := NyxMoveRoot(NyxPageRoot('first'), 0);
    LExpected := LSource.Clone;
    LExpected.MoveRoot(NyxPageRoot('last'), 0)
      .MoveRoot(NyxPageRoot('first'), 2).MoveRoot(NyxReusableRoot('stack-card'), 0);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Activate('middle');
    LSession.Select('middle-caption');
    LSession.ApplyPatch(LPatch);
    Check(TNyxCodec.Encode(LSession.Document) = TNyxCodec.Encode(LExpected),
      'typed copied movements reproduce both exact partitions and all content/references');
    Check((LSession.SelectedID = 'middle-caption') and (LSession.ActiveViewID = 'middle'),
      'root ordering retains the selected descendant and active view by identity');
    LOrdered := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LInitial,
      'one ordinary paired Undo restores the exact original root/source order');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LOrdered,
      'one ordinary paired Redo restores both partitions and exact Pascal');
    LSession.Activate('middle');
    LSession.MoveSelected(-1);
    Check(LSession.Document.Pages[0].ID = 'middle', 'ordinary Move up reorders a selected page');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LOrdered,
      'ordinary page movement uses the same complete paired history');
    LSession.Activate('stack-card');
    LSession.MoveSelected(1);
    Check(LSession.Document.Components[1].ID = 'stack-card',
      'ordinary Move down recognizes a column as a reusable root by membership');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LOrdered,
      'ordinary reusable movement uses the same complete paired history');
    LSession.Activate('last');
    LSession.MoveSelected(-1);
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LOrdered,
      'an ordinary out-of-range root move is an exact no-op');
    LSession.SetSourceDraft(LSession.Source + #10 + '// unfinished work');
    LRejected := False;
    try
      LSession.MoveSelected(1);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.DraftSource <> LSession.Source),
      'ordinary root movement refuses a pending draft without consuming it');

    LSemantic := TNyxAgentSession.Create(LPair);
    LSemantic.Call('nyx_select', 'Root ordering', NyxObject([
      NyxField('id', NyxData('middle-caption')),
      NyxField('operationId', NyxData('select-root-caption')),
      NyxField('expectedRevision', NyxData(LSemantic.Revision))]));
    LBefore := Frame;
    LArguments := RootArguments('order-both', NyxDesignChanges(LPatch).ToData);
    LReceipt := LSemantic.Call('nyx_transaction', 'Root ordering', LArguments);
    LAfter := Frame;
    LActual := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
    Check(TNyxCodec.Encode(LActual) = TNyxCodec.Encode(LExpected),
      'semantic grouped movement uses the identical public candidate');
    Check(LSemantic.Call('nyx_transaction', 'Root ordering', LArguments).ToJSON =
      LReceipt.ToJSON, 'exact grouped root retry returns its original receipt');
    LSemantic.Call('nyx_history', 'Root ordering', NyxObject([
      NyxField('expectedRevision', NyxData(LSemantic.Revision)),
      NyxField('operationId', NyxData('order-undo')), NyxField('direction', NyxData('undo'))]));
    Check(Frame.Field('project').ToJSON = LBefore.Field('project').ToJSON,
      'one semantic Undo restores the exact accepted pair');
    LSemantic.Call('nyx_history', 'Root ordering', NyxObject([
      NyxField('expectedRevision', NyxData(LSemantic.Revision)),
      NyxField('operationId', NyxData('order-redo')), NyxField('direction', NyxData('redo'))]));
    Check(Frame.Field('project').ToJSON = LAfter.Field('project').ToJSON,
      'one semantic Redo restores the complete ordered pair');
    Refuse('late-missing', NyxArray([NyxMoveRoot(NyxPageRoot('middle'), 0).ToData,
      NyxMoveRoot(NyxPageRoot('absent'), 1).ToData]));
    Refuse('wrong-partition', NyxArray([NyxMoveRoot(NyxReusableRoot('middle'), 0).ToData]));
    Refuse('descendant', NyxArray([NyxMoveRoot(NyxPageRoot('middle-caption'), 0).ToData]));
    Refuse('range', NyxArray([NyxMoveRoot(NyxPageRoot('first'), 3).ToData]));
    Refuse('negative', TNyxDataValue.ParseJSON(
      '[{"op":"root-move","root":{"kind":"page","name":"middle"},"index":-1}]'));
    Refuse('string-index', TNyxDataValue.ParseJSON(
      '[{"op":"root-move","root":{"kind":"page","name":"middle"},"index":"0"}]'));
    Refuse('foreign-owner', TNyxDataValue.ParseJSON(
      '[{"op":"root-move","root":{"kind":"page","name":"middle","parent":"first"},"index":0}]'));
    Refuse('missing-index', TNyxDataValue.ParseJSON(
      '[{"op":"root-move","root":{"kind":"page","name":"middle"}}]'));
    LSemantic.Exchange(NyxObject([NyxField('op', NyxData('configure')),
      NyxField('permission', NyxData('readOnly'))]));
    Refuse('readonly-root', NyxArray([NyxMoveRoot(NyxPageRoot('middle'), 0).ToData]));
    for LIndex := 0 to 1 do
    begin
      LRejected := False;
      try

        if LIndex = 0 then
        begin
          NyxMoveRoot(Default(TNyxRootRef), 0);
        end
        else
        begin
          NyxRootOrderPatch([Default(TNyxRootMoveChange)]);
        end;
      except
        on ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'an unconstructed root or movement refuses at the typed boundary');
    end;
  finally
    LSemantic.Free;
    LSession.Free;
    LActual.Free;
    LExpected.Free;
    LSource.Free;
  end;
end;
