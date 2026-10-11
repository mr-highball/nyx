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

{ Production semantic/compiler consumer: derive and customize exact independent
  definitions, then promote a source control without changing its public identity.
  The backend owns source preparation, real execution and paired publication. }
procedure SemanticReusables;
var
  LInitial: TNyxDataValue;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LArguments: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LDocument: TNyxDocument;
  LCounter: Integer;

  procedure Apply(const AOperations: TNyxDataValue);
  begin
    Inc(LCounter);
    LBefore := Observe;
    LArguments := Group(Revision, 'handwritten-reuse-' + IntToStr(LCounter), AOperations);
    Marker(GGate, True);
    try
      LReceipt := Submit(LArguments);
      Check(LReceipt.Field('publication').Field('state').AsText = 'pending',
        'reusable changes queue execution without publishing their proposed source');
      Check(Observe.Field('project').AsText = LBefore.Field('project').AsText,
        'the current pair remains exact during reusable construction');
      Check(Submit(LArguments).ToJSON = LReceipt.ToJSON,
        'queued reusable retries retain the original bounded receipt');
    finally
      Marker(GGate, False);
    end;
    LStatus := Await(LReceipt.Field('job').AsText);
    Check((LStatus.Field('publication').Field('state').AsText = 'committed') and
      (Revision = LBefore.Field('session').Field('revision').AsInteger + 1),
      'actual reusable construction publishes one exact next revision');
    LAfter := Observe;
    Check(Submit(LArguments).ToJSON = LReceipt.ToJSON,
      'completed reusable retries retain their original receipt');
    Check(Pos('class function TNotebookCards.Caption',
      DecodeNyxProject(LAfter.Field('project').AsText).Source) > 0,
      'reusable authoring retains the handwritten helper');
    Check(History('undo').Field('project').AsText = LBefore.Field('project').AsText,
      'one Undo restores the whole pre-reusable pair');
    Check(History('redo').Field('project').AsText = LAfter.Field('project').AsText,
      'one Redo restores the whole constructed reusable pair');
  end;

begin
  LInitial := Observe;
  LCounter := 0;
  Apply(TNyxDataValue.ParseJSON(
    '[{"op":"derive","source":"reusable-note","id":"featured-note",' +
    '"identities":{"note-caption":"featured-caption"}},' +
    '{"op":"instance","component":"featured-note","id":"featured-1","parent":"notebook-1"},' +
    '{"op":"override","instance":"featured-1","id":"featured-title",' +
    '"path":"caption","mode":"properties"},' +
    '{"op":"update","id":"featured-title","properties":{"text":"Featured notebook"}}]'));
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    Check((LDocument.FindComponent('featured-note') <> nil) and
      (LDocument.Find('featured-caption').Prop('text') =
        LDocument.Find('note-caption').Prop('text')) and
      (LDocument.Find('featured-title').Prop('text') = 'Featured notebook'),
      'derived parts and a local override preserve the independent definition');
  finally
    LDocument.Free;
  end;
  Apply(TNyxDataValue.ParseJSON(
    '[{"op":"inherit","instance":"featured-1","id":"featured-title","path":"caption"}]'));
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    Check((LDocument.Find('featured-title') = nil) and
      (LDocument.Find('featured-1') <> nil) and
      (LDocument.FindComponent('featured-note') <> nil),
      'restored inheritance removes only the local descriptor and keeps its definition');
  finally
    LDocument.Free;
  end;
  { A leaf promotion is the minimal regression: the authored identity stays
    heading-1 while its specialized type becomes INyxComponent. }
  Apply(TNyxDataValue.ParseJSON(
    '[{"op":"derive","source":"heading-1","id":"page-heading","identities":{}},' +
    '{"op":"delete","id":"heading-1"},' +
    '{"op":"instance","component":"page-heading","id":"heading-1",' +
    '"parent":"notebook-1","index":0}]'));
  LDocument := TNyxCodec.Decode(DecodeNyxProject(LAfter.Field('project').AsText).Design);
  try
    Check((LDocument.Find('heading-1').Kind = NyxKindName(nkComponent)) and
      (LDocument.FindComponent('page-heading').Kind = NyxKindName(nkHeading)),
      'extraction keeps the original identity with a specialized reusable instance');
  finally
    LDocument.Free;
  end;
  { Three deliberate source groups, independent from their tested Undo/Redo,
    return the owned fixture to its original complete pair. }
  History('undo');
  History('undo');
  Check(History('undo').Field('project').AsText = LInitial.Field('project').AsText,
    'reusable workflow leaves its original fixture restored');
end;
