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

{ Included by the maintained native semantic consumer. These are actual owning
  service calls and compiler children; no listener, current project replacement
  or synthetic producer is used. Shared candidate/history values qualify in
  nyx_project_import_tests on both compilers. Application execution is separate. }
procedure SemanticProjectImports(const APortableSource, ACheckpoint: TNyxText);
var
  LSerial: Integer;
  LInput: TNyxProjectPair;
  LInvalid: TNyxProjectPair;
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LRequest: TNyxDataValue;
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LImport: TNyxText;
  LTicket: TNyxText;
  LSource: TNyxText;
  LOutput: TNyxText;
  LCheckpoint: TNyxBytes;
  LDocument: TNyxDocument;
  LRejected: Boolean;
  LTarget: Integer;
  LStarted: QWord;
  LActivity: TNyxStudioBuildActivity;
const
  CTargets: array[0..1] of TNyxText = ('lcl', 'browser');
  CImportMarker: TNyxText = 'implementation' + #10 +
    '{ Crafted import / 🚀 𐐷 / keep the helpers and spacing. }';
  CImportedDraft: TNyxText = #10 + '{ Independent unfinished import 🚀';

  function ImportArguments(const AMode: TNyxText;
    const AFields: array of TNyxDataField; AMutation: Boolean = True): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LBase: Integer;
    LIndex: Integer;
  begin
    LBase := 2;

    if AMutation then
    begin
      Inc(LBase);
    end;
    SetLength(LFields, LBase + Length(AFields));
    LFields[0] := NyxField('mode', NyxData(AMode));
    LFields[1] := NyxField('expectedRevision', NyxData(Revision));

    if AMutation then
    begin
      Inc(LSerial);
      LFields[2] := NyxField('operationId', NyxData('compiler-import-' + IntToStr(LSerial)));
    end;
    for LIndex := 0 to High(AFields) do
    begin
      LFields[LBase + LIndex] := AFields[LIndex];
    end;
    Result := NyxObject(LFields);
  end;

  function ImportCall(const AArguments: TNyxDataValue): TNyxDataValue;
  begin
    Result := GEngine.InvokeTool('nyx_project', COwner, CActor, AArguments);
  end;

  { Exact first original-source replacement keeps every operand in TNyxText.
    SysUtils.StringReplace is an ANSI RTL boundary on native Windows and can
    retag UTF-8 bytes; it must not define this Unicode preservation oracle. }
  function ReplaceSource(const ASource, AExpected, AReplacement: TNyxText): TNyxText;
  var
    LPosition: Integer;
  begin
    LPosition := Pos(AExpected, ASource);

    if LPosition = 0 then
    begin
      raise Exception.Create('Expected exact original import fixture source');
    end;
    Result := Copy(ASource, 1, LPosition - 1) + AReplacement +
      Copy(ASource, LPosition + Length(AExpected), MaxInt);
  end;

  function Upload(const APair: TNyxProjectPair): TNyxText;
  var
    LPacket: TNyxText;
    LOffset: Integer;
    LTotal: Integer;
    LWindow: TNyxText;
    LPosition: Integer;
    LStart: Integer;
    LCount: Integer;
    LScalar: Integer;
    LReply: TNyxDataValue;
  begin
    LPacket := EncodeNyxProject(APair);
    LReply := ImportCall(ImportArguments('begin-import', [
      NyxField('bytes', NyxData(NyxUTF8ByteCount(LPacket)))]));
    Result := LReply.Field('projectImport').Field('import').AsText;
    LTotal := NyxTextScalarCount(LPacket);
    LOffset := 0;
    LPosition := 1;
    repeat
      LStart := LPosition;
      LCount := 0;
      while (LPosition <= Length(LPacket)) and (LCount < 4096) do
      begin

        if not NyxNextScalar(LPacket, LPosition, LScalar) then
        begin
          raise Exception.Create('Malformed qualification upload text');
        end;
        Inc(LCount);
      end;
      LWindow := Copy(LPacket, LStart, LPosition - LStart);
      LReply := ImportCall(ImportArguments('append-import', [
        NyxField('import', NyxData(Result)), NyxField('offset', NyxData(LOffset)),
        NyxField('text', NyxData(LWindow))]));
      LOffset := LReply.Field('projectImport').Field('nextOffset').AsInteger;
    until LOffset = LTotal;
    Check(LReply.Field('projectImport').Field('complete').AsBoolean,
      'exact uploaded project byte reservation is complete');
  end;

  function Review(const AImport, AResolution: TNyxText): TNyxDataValue;
  begin
    LRequest := ImportArguments('review-import', [NyxField('import', NyxData(AImport)),
      NyxField('resolution', NyxData(AResolution)), NyxField('admission', NyxData('compiler'))]);
    Result := ImportCall(LRequest);
    Check(ImportCall(LRequest).ToJSON = Result.ToJSON,
      'exact compiler review retry retains one immutable job');
  end;

  procedure Retained(const AReason: TNyxText);
  begin
    LAfter := Observe;
    Check((LAfter.Field('project').AsText = LBefore.Field('project').AsText) and
      SameSession(LAfter.Field('session'), LBefore.Field('session')), AReason);
  end;

begin
  LSerial := 0;
  LBefore := Observe;
  LSource := ReplaceSource(APortableSource, 'implementation', CImportMarker);
  LInput := NyxProjectPair(ExpectedNyxProjectionDesign, LSource);
  LInput.Pending := True;
  LInput.Draft := LSource + CImportedDraft;
  LInput.DraftBase := APortableSource + #10 + '{ independent accepted baseline }';
  LImport := Upload(LInput);
  Refuse('nyx_project', ImportArguments('review-import', [NyxField('import', NyxData(LImport)),
    NyxField('resolution', NyxData('match'))]), 'default literal review never executes handwritten factories');
  LCheckpoint := ReadBytes(ACheckpoint);
  LReceipt := Review(LImport, 'match');
  LStatus := Await(LReceipt.Field('job').AsText);
  Check((LStatus.Field('state').AsText = 'succeeded') and
    (LStatus.Field('publication').Field('state').AsText = 'reviewed') and
    (LStatus.Field('publication').Field('revision').AsInteger = Revision),
    'actual uploaded constructor prepares a review at the unchanged revision');
  Retained('review leaves the complete current pair, draft and history unchanged');
  Check(SameBytes(LCheckpoint, ReadBytes(ACheckpoint)), 'review does not persist document changes');
  LActivity := ReadNyxStudioBuildActivity(LStatus);
  Check((LActivity.Kind = sbakProjectImport) and (LActivity.Publication = sbpReviewed) and
    (LActivity.PublishedRevision = 0) and (Pos('Review ready', LActivity.Caption) > 0),
    'ordinary build presentation distinguishes ready review from applied changes');
  LTicket := LStatus.Field('projectImport').Field('reviewID').AsText;
  Check((LTicket <> '') and
    LStatus.Field('projectImport').Field('candidate').Field('pendingDraft').AsBoolean and
    (LStatus.Field('projectImport').Field('candidate').Field('pages').AsInteger = 2) and
    (LStatus.Field('projectImport').Field('candidate').Field('components').AsInteger = 1),
    'bounded review describes the whole independently expected uploaded design');
  LRejected := False;
  try
    GEngine.InvokeTool('nyx_project', 'foreign-transport', CActor,
      ImportArguments('apply', [NyxField('import', NyxData(LImport)),
        NyxField('reviewID', NyxData(LTicket))]));
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'foreign transport cannot adopt this ready review');
  Refuse('nyx_project', ImportArguments('apply', [NyxField('import', NyxData(LImport)),
    NyxField('reviewID', NyxData('wrong-ticket'))]), 'wrong ticket leaves ready candidate intact');
  LRequest := ImportArguments('apply', [NyxField('import', NyxData(LImport)),
    NyxField('reviewID', NyxData(LTicket))]);
  LReceipt := ImportCall(LRequest);
  Check(ImportCall(LRequest).ToJSON = LReceipt.ToJSON, 'adoption retry has one paired command');
  Check(Observe.Field('project').AsText = EncodeNyxProject(LInput),
    'ticketed adoption retains exact accepted Pascal, design and incoming draft/base');
  Check(History('undo').Field('project').AsText = LBefore.Field('project').AsText,
    'one Undo restores the complete original project');
  Check(History('redo').Field('project').AsText = EncodeNyxProject(LInput),
    'one Redo restores the imported accepted and unfinished pair');
  History('undo');

  LInvalid := LInput;
  LInvalid.Source := ReplaceSource(LSource, 'implementation',
    'implementation' + #10 + 'this is deliberately invalid Pascal;');
  LInvalid.Pending := False;
  LInvalid.Draft := '';
  LInvalid.DraftBase := '';
  LBefore := Observe;
  LImport := Upload(LInvalid);
  LReceipt := Review(LImport, 'match');
  LStatus := Await(LReceipt.Field('job').AsText);
  Check((LStatus.Field('state').AsText = 'failed') and
    (LStatus.Field('publication').Field('state').AsText = 'refused'),
    'real Pascal compiler failure produces no usable review');
  Retained('compiler failure retains complete current accepted/draft/history');
  ImportCall(ImportArguments('cancel-import', [NyxField('import', NyxData(LImport))]));

  LInvalid.Source := ReplaceSource(LSource, '    LDocument.Title :=',
    '    raise Exception.Create(''Imported constructor refusal 🚀 𐐷.'');' + #10 +
    '    LDocument.Title :=');
  LImport := Upload(LInvalid);
  LReceipt := Review(LImport, 'match');
  LStatus := Await(LReceipt.Field('job').AsText);
  Check((LStatus.Field('state').AsText = 'failed') and
    (LStatus.Field('publication').Field('state').AsText = 'refused'),
    'actual throwing constructor cannot supply an imported candidate');
  Retained('execution failure retains current files/history');
  ImportCall(ImportArguments('cancel-import', [NyxField('import', NyxData(LImport))]));

  LInvalid := LInput;
  LInvalid.Pending := False;
  LInvalid.Draft := '';
  LInvalid.DraftBase := '';
  LDocument := TNyxCodec.Decode(LInvalid.Design);
  try
    LDocument.Title := 'A deliberate saved-design conflict';
    LInvalid.Design := TNyxCodec.Encode(LDocument);
  finally
    LDocument.Free;
  end;
  LImport := Upload(LInvalid);
  LReceipt := Review(LImport, 'match');
  LStatus := Await(LReceipt.Field('job').AsText);
  Check((LStatus.Field('state').AsText = 'failed') and
    (LStatus.Field('publication').Field('state').AsText = 'refused'),
    'executed constructor still refuses a divergent match resolution');
  Retained('meaning conflict retains current work and the uploaded file');
  LReceipt := Review(LImport, 'pascal');
  LStatus := Await(LReceipt.Field('job').AsText);
  Check((LStatus.Field('publication').Field('state').AsText = 'reviewed') and
    LStatus.Field('projectImport').Field('candidate').Field('designChanged').AsBoolean and
    not LStatus.Field('projectImport').Field('candidate').Field('sourceChanged').AsBoolean,
    'explicit Pascal resolution constructs meaning while keeping exact uploaded source');
  ImportCall(ImportArguments('apply', [NyxField('import', NyxData(LImport)),
    NyxField('reviewID', LStatus.Field('projectImport').Field('reviewID'))]));
  Check(Observe.Field('project').AsText = EncodeNyxProject(
    NyxProjectPair(ExpectedNyxProjectionDesign, LSource)),
    'Pascal resolution adopts the independent complete constructor oracle');

  LOutput := GEngine.InvokeBuild(COwner, CActor, NyxObject([
    NyxField('mode', NyxData('outputs'))])).Field('outputID').AsText;
  for LTarget := 0 to 1 do
  begin
    Inc(LSerial);
    LReceipt := GEngine.InvokeBuild(COwner, CActor, NyxObject([
      NyxField('mode', NyxData('request')), NyxField('expectedRevision', NyxData(Revision)),
      NyxField('operationId', NyxData('imported-app-' + IntToStr(LSerial))),
      NyxField('outputID', NyxData(LOutput)), NyxField('target', NyxData(CTargets[LTarget])),
      NyxField('scope', NyxData('application'))]));
    LStarted := GetTickCount64;
    repeat
      LStatus := GEngine.InvokeBuild(COwner, CActor, NyxObject([
        NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
        NyxField('limit', NyxData(3))]));

      if NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText)) then
      begin
        Break;
      end;

      if GetTickCount64 - LStarted > 90000 then
      begin
        raise Exception.Create('Exact imported application exceeded its observation budget');
      end;
      Sleep(5);
    until False;
    Check((LStatus.Field('state').AsText = 'succeeded') and
      (LStatus.Field('sourceFingerprint').AsText = NyxBuildFingerprint(LSource)) and
      LStatus.Field('currentSource').AsBoolean,
      'ordinary ' + CTargets[LTarget] + ' compiler consumes exact imported accepted Pascal');
  end;
  History('undo');
  Check(Observe.Field('project').AsText = LBefore.Field('project').AsText,
    'successful import/application qualification restores the original owned pair');

  LBefore := Observe;
  LImport := Upload(LInput);
  LReceipt := Review(LImport, 'match');
  Draft(DecodeNyxProject(LBefore.Field('project').AsText).Source + CNewDraft);
  LAfter := Observe;
  LStatus := Await(LReceipt.Field('job').AsText);
  Check((LStatus.Field('publication').Field('state').AsText = 'refused') and
    (Observe.Field('project').AsText = LAfter.Field('project').AsText),
    'late review after ordinary typing retains the exact newer unfinished pair');
  ClearDraft;

  LBefore := Observe;
  LImport := Upload(LInput);
  LReceipt := Review(LImport, 'match');
  GEngine.InvokeBuild(COwner, CActor, NyxObject([NyxField('mode', NyxData('cancel')),
    NyxField('job', LReceipt.Field('job')), NyxField('expectedRevision', NyxData(Revision)),
    NyxField('operationId', NyxData('cancel-imported-constructor'))]));
  LStatus := Await(LReceipt.Field('job').AsText);
  Check((LStatus.Field('state').AsText = 'cancelled') and
    (LStatus.Field('publication').Field('state').AsText = 'refused'),
    'owning cancellation joins the constructor without a ready ticket');
  Retained('cancelled review retains current files/drafts/history');
  ImportCall(ImportArguments('cancel-import', [NyxField('import', NyxData(LImport))]));
end;
