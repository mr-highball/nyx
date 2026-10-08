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
program nyx_mcp_catalog_focus;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.studio.builds, nyx.text, nyx.data, nyx.types, nyx.model, nyx.schema, nyx.catalog,
  nyx.test.mcp.client;

type
  { Command-line choices are parsed once. Review mode keeps all authoring/build
    operations inside a transport-owned copy of the accepted primary project. }
  TCatalogAuthorMode = (camCompose, camInspect, camProperties, camReviewProperties);

var
  GClient: TNyxMCPTestClient;
  GRevision: Integer;
  GDirectory: TNyxText;
  GMode: TCatalogAuthorMode;
  GReview: TNyxText;
  GExactSource: TNyxText;

{ TNyxText is UTF-8 on this native boundary. Write its bytes directly, avoiding
  ANSI TStringList conversion of the generated companion or catalog metadata. }
procedure Save(const AName: TNyxText; const AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(GDirectory + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;

{ Requests are explicit protocol data. One persistent initialized client authors
  the explicitly supplied disposable service in legacy modes. Review mode uses
  an independent owned review on a shared service; it never authors the primary.
  This program never claims/replaces a project through the operator API or
  discovers credentials from a remote URL. }
function Call(const ATool: TNyxText; const AArguments: TNyxDataValue): TNyxDataValue;
var
  LResponse: TNyxDataValue;
  LArguments: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  LArguments := AArguments;

  if (GReview <> '') and (ATool <> 'nyx_reviews') then
  begin
    { Append exact context to every bounded query, mutation and build. Only the
      lifecycle tool is unscoped; its explicit review argument stays caller-owned. }
    SetLength(LFields, AArguments.Count + 1);
    for LIndex := 0 to AArguments.Count - 1 do
    begin
      LFields[LIndex] := NyxField(AArguments.Key(LIndex),
        AArguments.Field(AArguments.Key(LIndex)));
    end;
    LFields[High(LFields)] := NyxField('review', NyxData(GReview));
    LArguments := NyxObject(LFields);
  end;
  LResponse := GClient.Tool(ATool, LArguments);

  if LResponse.Field('isError').AsBoolean then
  begin
    raise Exception.Create('Semantic catalog request refused: ' + LResponse.ToJSON);
  end;
  Result := LResponse.Field('structuredContent');
  for LIndex := 0 to Result.Count - 1 do
  begin

    if Result.Key(LIndex) = 'revision' then
    begin
      GRevision := Result.Field('revision').AsInteger;
    end;
  end;
end;

function CreateControl(const AKind, AID, AParent: TNyxText;
  const AProperties: TNyxDataValue; APage: Boolean = False): TNyxDataValue;
begin

  if APage then
  begin
    Exit(NyxObject([NyxField('op', NyxData('create')),
      NyxField('kind', NyxData(AKind)), NyxField('id', NyxData(AID)),
      NyxField('root', NyxData('page')), NyxField('properties', AProperties)]));
  end;
  Result := NyxObject([NyxField('op', NyxData('create')),
    NyxField('kind', NyxData(AKind)), NyxField('id', NyxData(AID)),
    NyxField('parent', NyxData(AParent)), NyxField('properties', AProperties)]);
end;

procedure CreatePropertyReview;
begin
  { The property consumer uses the same semantic path as catalog composition.
    Geometry is numeric protocol data, never a behavioral string workaround.
    This adds one complete review as one undoable revision-aware operation. }
  Call('nyx_transaction', NyxObject([
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('catalog-property-review')),
    NyxField('operations', NyxArray([
      CreateControl('page', 'property-review', '', NyxObject([
        NyxField('padding', NyxData(24)), NyxField('gap', NyxData(16))]), True),
      CreateControl('row', 'property-layout', 'property-review', NyxObject([
        NyxField('width', NyxData(320)), NyxField('height', NyxData(140)),
        NyxField('padding', NyxData(16)), NyxField('gap', NyxData(12))])),
      CreateControl('label', 'property-left', 'property-layout', NyxObject([
        NyxField('text', NyxData('Left')), NyxField('width', NyxData(100)),
        NyxField('height', NyxData(28))])),
      CreateControl('label', 'property-right', 'property-layout', NyxObject([
        NyxField('text', NyxData('Right')), NyxField('width', NyxData(100)),
        NyxField('height', NyxData(28))])),
      CreateControl('heading', 'property-title', 'property-review', NyxObject([
        NyxField('text', NyxData('Everything stays in sync.'))])),
      CreateControl('code', 'property-code', 'property-review', NyxObject([
        NyxField('text', NyxData('procedure CreateSomething;'))])),
      CreateControl('progress', 'property-progress', 'property-review', NyxObject([
        NyxField('min', NyxData(20)), NyxField('max', NyxData(80)),
        NyxField('value', NyxData(50))])),
      CreateControl('input', 'property-numeric', 'property-review', NyxObject([
        NyxField('text', NyxData('A format can change')),
        NyxField('input-type', NyxData('number')), NyxField('value', NyxData(12))]))]))]));
end;

{ Export only bounded accepted-source windows at one revision. The consumer
  compiles these unchanged bytes; it does not reconstruct the design locally. }
procedure ExportSource;
var
  LLines: TNyxStrings;
  LValue: TNyxDataValue;
  LLine: Integer;
  LIndex: Integer;
  LRevision: Integer;
begin
  LLines := TNyxStrings.Create;
  try
    LLine := 1;
    LRevision := GRevision;
    repeat
      LValue := Call('nyx_source', NyxObject([
        NyxField('line', NyxData(LLine)), NyxField('count', NyxData(80))]));

      if (GRevision <> LRevision) or (LValue.Field('lines').Count = 0) then
      begin
        raise Exception.Create('Catalog companion changed during bounded export');
      end;
      for LIndex := 0 to LValue.Field('lines').Count - 1 do
      begin
        LLines.Add(LValue.Field('lines').Item(LIndex).AsText);
      end;
      Inc(LLine, LValue.Field('lines').Count);
    until LLine > LValue.Field('totalLines').AsInteger;
    GExactSource := LLines.Text;
    Save('nyx.generated.view.pas', GExactSource);
  finally
    LLines.Free;
  end;
end;

procedure Compile(const ATarget, AOutput: TNyxText);
var
  LReceipt: TNyxDataValue;
  LStatus: TNyxDataValue;
  LStarted: QWord;
begin
  LReceipt := Call('nyx_build', NyxObject([
    NyxField('mode', NyxData('request')),
    NyxField('expectedRevision', NyxData(GRevision)),
    NyxField('operationId', NyxData('catalog-focus-build-' + ATarget)),
    NyxField('outputID', NyxData(AOutput)), NyxField('target', NyxData(ATarget)),
    NyxField('scope', NyxData('application'))]));
  { Admission is distinct from completion. Retain the exact job handle before
    polling, including when a transport observation fails or exceeds its wait. }
  Save(ATarget + '-receipt.json', LReceipt.ToJSON);
  LStarted := GetTickCount64;
  repeat
    LStatus := Call('nyx_build', NyxObject([
      NyxField('mode', NyxData('status')), NyxField('job', LReceipt.Field('job')),
      NyxField('limit', NyxData(2)), NyxField('severity', NyxData('error'))]));

    if NyxBuildJobTerminal(ParseNyxBuildJobState(LStatus.Field('state').AsText)) then
    begin
      Break;
    end;

    if GetTickCount64 - LStarted > 180000 then
    begin
      { An observation window is not a compiler deadline. Preserve this exact
        admitted job, review and transport while the service still reports live
        work; only a terminal state or actual transport failure ends the wait. }
      Save(ATarget + '-waiting.json', LStatus.ToJSON);
      WriteLn('WAIT actual MCP ', ATarget, ' compiler remains ',
        LStatus.Field('state').AsText, ' / same admitted job');
      LStarted := GetTickCount64;
    end;
    Sleep(100);
  until False;
  Save(ATarget + '-build.json', LStatus.ToJSON);

  if (LStatus.Field('state').AsText <> 'succeeded') or
    not LStatus.Field('currentSource').AsBoolean then
  begin
    raise Exception.Create('Catalog compiler did not qualify the current pair: ' +
      LStatus.ToJSON);
  end;
  WriteLn('PASS actual MCP ', ATarget, ' application compiler');
end;

{ Compare bounded live-service publication with the catalog recipe independently
  compiled into this consumer. Compound metadata is the union of its parts;
  the target harness later checks each of those real physical faces separately. }
procedure CheckPublished(const AKind: TNyxText; const AMetadata: TNyxDataValue;
  ACatalog: TNyxCatalog);
var
  LNode: TNyxNode;
  LExpected: TNyxEventSchemas;
  LTrigger: TNyxTrigger;
  LIndex: Integer;
  LExpectedPresent: Boolean;
  LPublished: set of TNyxTrigger;
  LActualTrigger: TNyxTrigger;
  LEvents: TNyxDataValue;
  LEvent: TNyxDataValue;
begin
  LNode := ACatalog.NewNode(AKind, 'metadata-sample');
  try
    LExpected := NyxEventsMetadata(LNode);
    LPublished := [];
    LEvents := AMetadata.Field('events');
    { Data values own copies. Read the bounded array once, rather than copying
      the entire response inside every trigger/member comparison. }
    for LIndex := 0 to LEvents.Count - 1 do
    begin
      LEvent := LEvents.Item(LIndex);

      if TryNyxTrigger(LEvent.Field('trigger').AsText, LActualTrigger) and
        (NyxIsKeyboardTrigger(LActualTrigger) or
        (LActualTrigger in [ntAfterEnter, ntAfterExit])) then
      begin
        Include(LPublished, LActualTrigger);

        if (LEvent.Field('browser').AsText = 'Unavailable') or
          (LEvent.Field('native').AsText = 'Unavailable') then
        begin
          raise Exception.Create('Published focus/key bridge is unavailable: ' + AKind);
        end;
      end;
    end;
    for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
    begin

      if not NyxIsKeyboardTrigger(LTrigger) and
        not (LTrigger in [ntAfterEnter, ntAfterExit]) then
      begin
        Continue;
      end;
      LExpectedPresent := False;
      for LIndex := 0 to High(LExpected) do
      begin
        LExpectedPresent := LExpectedPresent or (LExpected[LIndex].Trigger = LTrigger);
      end;

      if LExpectedPresent <> (LTrigger in LPublished) then
      begin
        raise Exception.Create('Live metadata differs from the compiled catalog: ' +
          AKind + ' / ' + NyxTriggerName(LTrigger));
      end;
    end;
  finally
    LNode.Free;
  end;
end;

var
  LCatalog: TNyxCatalog;
  LEntries: array of TNyxDataValue;
  LOps: array of TNyxDataValue;
  LMetadata: array of TNyxDataValue;
  LValue: TNyxDataValue;
  LProperties: TNyxDataValue;
  LKind: TNyxText;
  LPage: TNyxText;
  LOffset: Integer;
  LTotal: Integer;
  LIndex: Integer;
  LBatch: Integer;
  LCount: Integer;
  LBeforeSource: TNyxText;
  LPages: Integer;
  LUndoRevision: Integer;
begin
  GClient := nil;
  LCatalog := nil;
  SetLength(LEntries, 0);
  SetLength(LOps, 0);
  try

    if (ParamCount <> 2) and (ParamCount <> 3) then
    begin
      raise Exception.Create('Supply MCP config, owned output directory and optional inspect/properties/review-properties');
    end;
    GMode := camCompose;

    if ParamCount = 3 then
    begin

      if ParamStr(3) = 'inspect' then
      begin
        GMode := camInspect;
      end
      else if ParamStr(3) = 'properties' then
      begin
        GMode := camProperties;
      end
      else if ParamStr(3) = 'review-properties' then
      begin
        GMode := camReviewProperties;
      end
      else
      begin
        raise Exception.Create('Unknown catalog author mode');
      end;
    end;
    GDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(2)));

    if (GMode = camReviewProperties) and DirectoryExists(GDirectory) then
    begin
      raise Exception.Create('Owned review export requires a fresh output directory');
    end;
    ForceDirectories(GDirectory);
    GClient := TNyxMCPTestClient.Create(ParamStr(1), 'Scooty catalog focus qualification');
    LValue := Call('nyx_session', NyxObject([]));

    if GMode = camReviewProperties then
    begin
      { Creation and every subsequent call share this authenticated transport.
        An accepted copy retains reusable definitions but excludes pending drafts. }
      LValue := Call('nyx_reviews', NyxObject([
        NyxField('mode', NyxData('create')), NyxField('base', NyxData('accepted')),
        NyxField('label', NyxData('Full property qualification')),
        NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData('catalog-properties-owned-review'))]));
      GReview := LValue.Field('review').AsText;

      if GReview = '' then
      begin
        raise Exception.Create('Owned review creation returned no context');
      end;
      Save('review-created.json', LValue.ToJSON);
      Call('nyx_session', NyxObject([]));
    end;
    LCatalog := TNyxCatalog.Create;
    LOffset := 0;
    repeat
      LValue := Call('nyx_components', NyxObject([
        NyxField('offset', NyxData(LOffset)), NyxField('limit', NyxData(50))]));
      LTotal := LValue.Field('total').AsInteger;
      for LIndex := 0 to LValue.Field('items').Count - 1 do
      begin
        SetLength(LEntries, Length(LEntries) + 1);
        LEntries[High(LEntries)] := LValue.Field('items').Item(LIndex);
      end;
      Inc(LOffset, LValue.Field('items').Count);
    until LOffset >= LTotal;

    if Length(LEntries) <> LCatalog.Count then
    begin
      raise Exception.Create('Service and qualification catalog scopes differ');
    end;
    LBatch := 0;
    { Inspect is read-only and never retries composition on an existing service. }

    if GMode <> camInspect then
    begin
      for LIndex := 0 to High(LEntries) do
      begin
        LKind := LEntries[LIndex].Field('kind').AsText;
        LPage := 'catalog-' + LKind;
        LProperties := NyxObject([]);

        if LKind = 'component' then
        begin
          LProperties := NyxObject([NyxField('component', NyxData('welcome-card'))]);
        end;
        LCount := Length(LOps);
        SetLength(LOps, LCount + 4);
        LOps[LCount] := CreateControl('page', LPage, '',
          NyxObject([NyxField('padding', NyxData(24))]), True);
        LOps[LCount + 1] := CreateControl('button', LPage + '-before', LPage,
          NyxObject([NyxField('text', NyxData('Before the sample'))]));
        LOps[LCount + 2] := CreateControl(LKind, LPage + '-sample', LPage, LProperties);
        LOps[LCount + 3] := CreateControl('button', LPage + '-after', LPage,
          NyxObject([NyxField('text', NyxData('After the sample'))]));

        { Shared-review source reconciliation is independently bounded. Keep
          each sample/page/neighbor family atomic instead of assuming that 64
          protocol leaves also fit the source edit-distance budget. Legacy
          disposable composition keeps its original batching contract. }

        if ((GMode = camReviewProperties) and (Length(LOps) = 4)) or
          (Length(LOps) = 64) or (LIndex = High(LEntries)) then
        begin
          Inc(LBatch);
          Call('nyx_transaction', NyxObject([
            NyxField('expectedRevision', NyxData(GRevision)),
            NyxField('operationId', NyxData('catalog-focus-compose-' + IntToStr(LBatch))),
            NyxField('operations', NyxArray(LOps))]));
          SetLength(LOps, 0);
        end;
      end;
      { A separate semantic page qualifies radio peer entry without pretending
        that every member of a group owns an independent Tab stop. The ordinary
        catalog cases still inspect each radio's complete callback family. }
      Call('nyx_transaction', NyxObject([
        NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData('catalog-focus-radio-peers')),
        NyxField('operations', NyxArray([
          CreateControl('page', 'catalog-radio-peers', '', NyxObject([]), True),
          CreateControl('button', 'radio-before', 'catalog-radio-peers', NyxObject([])),
          CreateControl('column', 'radio-peers', 'catalog-radio-peers', NyxObject([])),
          CreateControl('radio', 'radio-first', 'radio-peers', NyxObject([])),
          CreateControl('radio', 'radio-middle', 'radio-peers', NyxObject([])),
          CreateControl('radio', 'radio-last', 'radio-peers', NyxObject([])),
          CreateControl('button', 'radio-after', 'catalog-radio-peers', NyxObject([]))]))]));
      Inc(LBatch);

      if GMode in [camProperties, camReviewProperties] then
      begin
        CreatePropertyReview;
        Inc(LBatch);
      end;
    end;
    SetLength(LMetadata, Length(LEntries));
    for LIndex := 0 to High(LEntries) do
    begin
      LKind := LEntries[LIndex].Field('kind').AsText;
      LMetadata[LIndex] := Call('nyx_node', NyxObject([
        NyxField('id', NyxData('catalog-' + LKind + '-sample')),
        NyxField('keys', NyxArray([NyxData('enabled')])),
        NyxField('limit', NyxData(1)), NyxField('events', NyxData(True)),
        NyxField('eventLimit', NyxData(50))]));
      CheckPublished(LKind, LMetadata[LIndex], LCatalog);
    end;
    Save('catalog.json', NyxArray(LEntries).ToJSON);
    Save('metadata.json', NyxArray(LMetadata).ToJSON);
    ExportSource;

    if GMode = camReviewProperties then
    begin
      { Only the complete property-review group is undone. Its redo must restore
        the exact accepted builder; no local reconstruction can satisfy this check. }
      LBeforeSource := GExactSource;
      LPages := Call('nyx_session', NyxObject([])).Field('pages').AsInteger;
      Call('nyx_history', NyxObject([
        NyxField('direction', NyxData('undo')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData('catalog-properties-owned-undo'))]));
      LUndoRevision := GRevision;
      LValue := Call('nyx_session', NyxObject([]));

      if (LValue.Field('pages').AsInteger <> LPages - 1) or
        not LValue.Field('canRedo').AsBoolean then
      begin
        raise Exception.Create('Owned property Undo did not remove its one complete root group');
      end;
      Call('nyx_history', NyxObject([
        NyxField('direction', NyxData('redo')), NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData('catalog-properties-owned-redo'))]));
      ExportSource;

      if GExactSource <> LBeforeSource then
      begin
        raise Exception.Create('Owned property Redo did not restore exact accepted Pascal');
      end;
      Save('history-receipt.json', NyxObject([
        NyxField('review', NyxData(GReview)), NyxField('undoRevision', NyxData(LUndoRevision)),
        NyxField('redoRevision', NyxData(GRevision)), NyxField('exactSource', NyxData(True))]).ToJSON);
      WriteLn('PASS owned semantic property Undo/Redo with exact source');
    end;

    if GMode <> camInspect then
    begin
      LValue := Call('nyx_build', NyxObject([NyxField('mode', NyxData('outputs'))]));
      Compile('browser', LValue.Field('outputID').AsText);
      Compile('lcl', LValue.Field('outputID').AsText);
    end;
    if GMode <> camInspect then
    begin
      WriteLn('PASS semantic composition, bounded metadata/source and both compilers / ',
        Length(LEntries), ' catalog kinds / ', LBatch, ' paired transactions');
    end
    else
    begin
      WriteLn('PASS read-only live metadata concordance and exact source export / ',
        Length(LEntries), ' catalog kinds / revision ', GRevision);
    end;
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
  { Cleanup still runs after a refused query or failed build. Never retire a
    foreign review or silently fall back to the primary. Close the transport
    after explicit disposal so shared Studio can observe the review lifetime. }
  try

    if GReview <> '' then
    begin
      LValue := Call('nyx_reviews', NyxObject([
        NyxField('mode', NyxData('discard')), NyxField('review', NyxData(GReview)),
        NyxField('expectedRevision', NyxData(GRevision)),
        NyxField('operationId', NyxData('catalog-properties-owned-dispose'))]));
      Save('review-disposed.json', LValue.ToJSON);
      GReview := '';
      WriteLn('PASS explicit owned review disposal');
    end;

  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'FAIL semantic cleanup: ', LException.Message);
      ExitCode := 1;
    end;
  end;
  { A disposal refusal must not skip transport retirement. Server-side session
    retirement also owns any remaining review leases; no context falls back. }
  try

    if GClient <> nil then
    begin
      GClient.Close;
    end;
  except
    on LException: Exception do
    begin
      WriteLn(StdErr, 'FAIL transport cleanup: ', LException.Message);
      ExitCode := 1;
    end;
  end;
  GClient.Free;
  LCatalog.Free;
end.
