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

program nyx_http_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  Classes,
  SysUtils,
  fphttpclient,
  fpjson,
  jsonparser,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.controls,
  nyx.state,
  nyx.codec,
  nyx.codegen,
  nyx.catalog,
  nyx.test.core,
  nyx.test.identity,
  nyx.test.source,
  nyx.test.source.structural,
  nyx.test.collections.registry,
  nyx.collections,
  nyx.collections.view.types,
  nyx.test.callbacks,
  nyx.callbacks,
  nyx.scheduler,
  nyx.source,
  nyx.composition,
  nyx.studio.builds,
  nyx.studio.compiler,
  nyx.test.compiler,
  nyx.test.interactions,
  nyx.test.split,
  Process,
  nyx.studio.outputs,
  nyx.studio.projects,
  nyx.data;

var
  LBaseURL: TNyxText;
  LDocument: TNyxDocument;
  LResponse: TNyxText;
  LResult: TJSONData;
  LIndex: Integer;
  LCount: Integer;
  LExpected: TNyxText;
  LDifference: Integer;
  LConfiguration: TNyxText;
  LOutputs: TNyxOutputConfiguration;
  LBrowserOnly: TNyxOutputConfiguration;
  LCatalog: TNyxCatalog;
  LTemplate: TNyxNode;

function JoinText(const AParts: array of TNyxText): TNyxText;
var
  LParts: TNyxStrings;
  LPart: TNyxText;
begin
  LParts := TNyxStrings.Create;
  try
    for LPart in AParts do
    begin
      LParts.Add(LPart);
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function ReplaceSource(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(ABefore, ASource);

  if LPosition = 0 then
  begin
    raise Exception.Create('Companion fixture replacement is missing');
  end;
  Result := JoinText([Copy(ASource, 1, LPosition - 1), AAfter,
    Copy(ASource, LPosition + Length(ABefore), MaxInt)]);
end;

function Exchange(const AMethod, APath, ABody: TNyxText;
  AStatus: Integer = 200; const AOrigin: TNyxText = ''): TNyxText;
var
  LClient: TFPHTTPClient;
  LRequest: TMemoryStream;
  LReply: TMemoryStream;
begin
  { Exercise network bytes through native Pascal, including the old HTTP RTL's
    ANSI defaults. Neither body takes a TStringStream/String conversion path. }
  LClient := TFPHTTPClient.Create(nil);
  LRequest := TMemoryStream.Create;
  LReply := TMemoryStream.Create;
  try
    LClient.IOTimeout := 65000;
    LClient.AddHeader('Content-Type', 'application/json; charset=utf-8');

    if AOrigin = '' then
    begin
      LClient.AddHeader('Origin', LBaseURL);
    end
    else
    begin
      LClient.AddHeader('Origin', AOrigin);
    end;

    if ABody <> '' then
    begin
      LRequest.WriteBuffer(ABody[1], Length(ABody));
      LRequest.Position := 0;
      LClient.RequestBody := LRequest;
    end;
    LClient.HTTPMethod(AMethod, LBaseURL + APath, LReply, [AStatus, 400, 500]);
    SetLength(Result, LReply.Size);
    { FPC 3.2.0 SetLength may allocate a system-codepage buffer even for a typed
      UTF8String result. Bytes read from this UTF-8 protocol must carry UTF-8
      metadata too, otherwise String comparisons can convert identical bytes. }
    SetCodePage(RawByteString(Result), CP_UTF8, False);
    LReply.Position := 0;

    if LReply.Size > 0 then
    begin
      LReply.ReadBuffer(Result[1], LReply.Size);
    end;

    if LClient.ResponseStatusCode <> AStatus then
    begin
      raise Exception.Create('HTTP status ' + IntToStr(LClient.ResponseStatusCode) +
        ' for ' + APath + ': ' + Result);
    end;
  finally
    LClient.RequestBody := nil;
    LClient.Free;
    LRequest.Free;
    LReply.Free;
  end;
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL HTTP: ' + AMessage);
  end;
  Inc(LCount);
end;

function QueryID(const AID: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  { Encode each UTF-8 byte, including slash/tilde and long supplementary names.
    The service must decode the query without converting its ANSI-tagged bytes. }
  Result := '';
  for LIndex := 1 to Length(AID) do
  begin
    Result := Result + '%' + IntToHex(Ord(AID[LIndex]), 2);
  end;
end;

function BuildQuery(AIndex: Integer): TNyxText;
begin
  case AIndex of
    0: Result := 'target=browser&scope=view&page=home';
    1: Result := 'target=browser&scope=view&page=welcome-card';
    2: Result := 'target=browser&scope=application&page=home';
    3: Result := 'target=lcl&scope=application&page=home';
    4: Result := 'target=browser&scope=view&page=custom-action';
    5: Result := 'target=lcl&scope=view&page=custom-action';
    6: Result := 'target=browser&scope=view&page=' + QueryID('identity/page~🌙');
    7: Result := 'target=browser&scope=view&page=' + QueryID(NyxIdentityDefinitionID);
    8: Result := 'target=lcl&scope=view&page=' + QueryID(NyxIdentityInstanceID);
    9: Result := 'target=lcl&scope=view&page=' + QueryID('identity/page~🌙');
  else
    raise Exception.Create('Unknown HTTP fixture');
  end;
end;

{ Compile the new portable authoring contracts through the real delegated
  service, including a complete authored callback class and every event family.
  Actual compiled control execution has its own both-target consumer fixture. }
procedure InteractionAndSplitBuilds;
var
  LDesign: TNyxDocument;
  LSource: TNyxText;
  LReply: TNyxText;
  LTarget: TNyxText;
  LPage: TNyxText;
  LData: TJSONData;
  LIndex: Integer;
  LTrigger: TNyxTrigger;
  LPageControl: INyxPage;
  LTransferButton: INyxButton;
begin
  for LIndex := 0 to 5 do
  begin
    LTarget := 'browser';

    if Odd(LIndex) then
    begin
      LTarget := 'lcl';
    end;

    if LIndex < 2 then
    begin
      LDesign := CreateNyxSplitFixture;
      LPage := 'workspace';
    end
    else if LIndex < 4 then
    begin
      LDesign := CreateNyxInteractionFixture;
      LPage := 'interactions';
    end
    else
    begin
      { Exercise typed gesture configuration and every new authored callback
        through the actual delegated compiler, including isolated companions. }
      LDesign := TNyxDocument.Create;
      LPage := 'gestures';
      LPageControl := NewNyxPage(LPage);
      LTransferButton := NewNyxButton('transfer-button').WithText('Transfer / 🌙');
      LTransferButton.Configure.DragSource(True).DropTarget(True).TouchBehavior(ntbNone);
      for LTrigger := ntPointerCancel to ntDragEnd do
      begin
        NyxCallbacks(LTransferButton.Node).On(LTrigger).Add(NyxHandler('TInteractionProbe'),
          NyxCallbackID('transfer.' + NyxTriggerName(LTrigger)));
      end;
      LPageControl.Add(LTransferButton);
      LDesign.AddPage(LPageControl.Node);
    end;
    try
      LSource := TNyxCodegen.Generate(LDesign);

      if LIndex >= 2 then
      begin
        LSource := Copy(LSource, 1, Length(LSource) - Length('end.' + #10)) +
          'type' + #10 +
          '  TInteractionProbe = class(TNyxEventCallback)' + #10 +
          '  public' + #10 +
          '    procedure Invoke(const AEvent: TNyxEventInfo;' + #10 +
          '      const AExecution: INyxExecution); override;' + #10 +
          '  end;' + #10 + #10 +
          'procedure TInteractionProbe.Invoke(const AEvent: TNyxEventInfo;' + #10 +
          '  const AExecution: INyxExecution);' + #10 +
          'begin' + #10 +
          '  // TODO: implement application interaction.' + #10 +
          'end;' + #10 + #10 +
          'initialization' + #10 +
          '  RegisterNyxCallback(NyxHandler(''TInteractionProbe''), TInteractionProbe);' + #10 +
          'end.' + #10;
      end;
      LReply := Exchange('POST', '/api/build?source=companion&target=' + LTarget +
        '&scope=view&page=' + LPage, EncodeNyxBuildRequest(LDesign, LSource));
      LData := GetJSON(LReply);
      try
        Check(TJSONObject(LData).Booleans['ok'],
          'new split/interaction companion compiles through ' + LTarget + ' service');
        LReply := Exchange('GET', '/' + TJSONObject(LData).Strings['source'], '');
        Check((Pos('INyxSplitView', LReply) > 0) or
          ((Pos('.OnBeforeKeyPress', LReply) > 0) and
          (Pos('.OnAfterTextInput', LReply) > 0) and
          (Pos('procedure TInteractionProbe.Invoke', LReply) > 0)) or
          ((Pos('.OnPointerCancel', LReply) > 0) and
          (Pos('.OnDragStart', LReply) > 0) and (Pos('.OnDrop', LReply) > 0) and
          (Pos('.DragSource(True)', LReply) > 0) and (Pos('.DropTarget(True)', LReply) > 0) and
          (Pos('.TouchBehavior(ntbNone)', LReply) > 0) and
          (Pos('procedure TInteractionProbe.Invoke', LReply) > 0)),
          'delegated isolation retains typed platform/event source and authored helper');
      finally
        LData.Free;
      end;
    finally
      LDesign.Free;
    end;
  end;
end;

procedure CompanionBuilds;
const
  CMarker = 'A crafted application / 🌙 漢字';
var
  LDesign: TNyxDocument;
  LIsolated: TNyxDocument;
  LSource: TNyxText;
  LBody: TNyxText;
  LReply: TNyxText;
  LExpectedSource: TNyxText;
  LData: TJSONData;
  LJob: TJSONObject;
  LTestIndex: Integer;
  LTarget: TNyxText;
  LScope: TNyxText;
  LPage: TNyxText;
  LProcess: TProcess;
  LOutput: TNyxText;
  LBytes: RawByteString;
  LStream: TMemoryStream;
  LFile: TFileStream;
  LPosition: Integer;
begin
  LDesign := CreateNyxEditedFixture(LSource);
  try
    { Execute the actual preserved helper when the compiled output loads.
      Browser initialization exposes its result on the real host; native test
      startup verifies the builder/helper without entering a blocking GUI loop. }
    LPosition := Pos('end.' + #10, LSource);
    Check(LPosition > 0, 'companion fixture has a final unit terminator');
    LSource := JoinText([Copy(LSource, 1, LPosition - 1),
      '{$IFDEF PAS2JS}' + #10 +
      'initialization' + #10 +
      '  document.body.setAttribute(''data-nyx-companion'', AppCaption);' + #10 +
      '{$ELSE}' + #10 +
      'procedure VerifyCompanion;' + #10 +
      'var LDesign: TNyxDocument; LFile: TFileStream; LText: TNyxText;' + #10 +
      'begin' + #10 +
      '  LDesign := BuildNyxDocument;' + #10 +
      '  try' + #10 +
      '    LFile := TFileStream.Create(ChangeFileExt(ParamStr(0), ''.verification''), fmCreate);' + #10 +
      '    try' + #10 +
      '      LText := AppCaption;' + #10 +
      '      LFile.WriteBuffer(LText[1], Length(LText));' + #10 +
      #10 + '    if LDesign.Find(''eyebrow'') <> nil then' + #10 +
      '    begin' + #10 +
      '      LText := LDesign.Find(''eyebrow'').Prop(''text'');' + #10 +
      '      LFile.WriteBuffer(LText[1], Length(LText));' + #10 +
      '    end;' + #10 +
      '    finally' + #10 +
      '      LFile.Free;' + #10 +
      '    end;' + #10 +
      '  finally' + #10 +
      '    LDesign.Free;' + #10 +
      '  end;' + #10 +
      'end;' + #10 +
      'initialization' + #10 +
      '  if ParamStr(1) = ''--nyx-verify'' then' + #10 +
      '  begin' + #10 +
      '    VerifyCompanion;' + #10 +
      '    Halt(0);' + #10 +
      '  end;' + #10 +
      '{$ENDIF}' + #10 + 'end.' + #10]);
    LPosition := Pos('implementation' + #10, LSource);
    LSource := JoinText([Copy(LSource, 1, LPosition - 1),
      'implementation' + #10 + #10 +
      '{$IFDEF PAS2JS}uses Web;{$ELSE}uses Classes, SysUtils;{$ENDIF}' + #10,
      Copy(LSource, LPosition + Length('implementation' + #10), MaxInt)]);
    LBody := EncodeNyxBuildRequest(LDesign, LSource);
    LReply := Exchange('POST',
      '/api/build?source=companion&target=browser&scope=application',
      JoinText(['{"version":1,"design":', TNyxCodec.Encode(LDesign),
        ',"source":""}']), 400);
    Check(Pos('accepted Pascal', LReply) > 0, 'empty companion rejected before job creation');
    LReply := Exchange('POST',
      '/api/build?source=companion&target=browser&scope=application',
      EncodeNyxBuildRequest(LDesign, ReplaceSource(LSource,
        '.Text(''CRAFTED / 🌙'')', '.Text(''Unapplied caption'')')), 400);
    Check((Pos('Apply Pascal', LReply) > 0) and (Pos('"line"', LReply) > 0),
      'design/source mismatch carries a source position without compiling: ' + LReply);
    LReply := Exchange('POST',
      '/api/build?source=companion&target=browser&scope=application',
      EncodeNyxBuildRequest(LDesign, ReplaceSource(LSource,
        'unit nyx.edited.view;', 'unit ../outside;')), 400);
    Check(Pos('namespace', LReply) > 0, 'companion namespace cannot escape its job');
    LReply := Exchange('POST',
      '/api/build?source=companion&target=browser&scope=application',
      EncodeNyxBuildRequest(LDesign, ReplaceSource(LSource,
        'implementation' + #10, 'implementation' + #10 + '{$I ../outside.inc}' + #10)), 400);
    Check(Pos('External-file', LReply) > 0, 'external-file directive rejected before compiling');

    for LTestIndex := 0 to 5 do
    begin
      LTarget := 'browser';

      if LTestIndex >= 3 then
      begin
        LTarget := 'lcl';
      end;
      LScope := 'view';
      LPage := 'home';
      case LTestIndex mod 3 of
        0: LScope := 'application';
        2: LPage := 'welcome-card';
      end;
      LReply := Exchange('POST', '/api/build?source=companion&target=' + LTarget +
        '&scope=' + LScope + '&page=' + LPage, LBody);
      LData := GetJSON(LReply);
      try
        LJob := TJSONObject(LData);
        Check(LJob.Booleans['ok'] and LJob.Booleans['companion'],
          'accepted companion compiles for ' + LTarget + '/' + LScope + '/' + LPage);
        LReply := Exchange('GET', '/' + LJob.Strings['source'], '');
        LExpectedSource := LSource;

        if LScope = 'view' then
        begin
          LIsolated := CloneNyxViewDocument(LDesign, LDesign.Find(LPage));
          try
            LExpectedSource := PrepareNyxCompanion(LDesign, LIsolated, LSource, True);
          finally
            LIsolated.Free;
          end;
        end;
        Check(LReply = LExpectedSource,
          'job source retains the exact companion/frame and admitted view meaning');
        WriteLn('Companion artifact ', LTestIndex, ': ', LJob.Strings['artifact']);

        if LTarget = 'lcl' then
        begin
          { Execute a compiler-produced native artifact through Pascal Process,
            with a fixed verification argument, never through a shell. }
          LProcess := TProcess.Create(nil);
          LStream := TMemoryStream.Create;
          try
            LProcess.Executable := ExpandFileName('build/studio/jobs/' +
              LJob.Strings['build'] + '/nyx_native.exe');
            LProcess.Parameters.Add('--nyx-verify');
            LProcess.Options := [poUsePipes, poWaitOnExit, poNoConsole];
            LProcess.Execute;
            LFile := TFileStream.Create(ChangeFileExt(LProcess.Executable,
              '.verification'), fmOpenRead);
            try
              LStream.CopyFrom(LFile, 0);
            finally
              LFile.Free;
            end;
            SetLength(LBytes, LStream.Size);
            LStream.Position := 0;

            if LStream.Size > 0 then
            begin
              LStream.ReadBuffer(LBytes[1], LStream.Size);
            end;
            SetCodePage(LBytes, CP_UTF8, False);
            LOutput := LBytes;
            Check((LProcess.ExitStatus = 0) and (Pos(TNyxText(CMarker), LOutput) > 0),
              'native compiled artifact executes the handwritten Unicode helper');

            if LPage <> 'welcome-card' then
            begin
              Check(Pos(TNyxText('CRAFTED / 🌙'), LOutput) > 0,
                'native compiled companion executes the accepted typed builder');
            end;
          finally
            LStream.Free;
            LProcess.Free;
          end;
        end;
      finally
        LData.Free;
      end;
    end;
    { A syntactically admitted helper still belongs to the compiler: errors must
      return a real job/log, without substituting generated design-only source. }
    LReply := Exchange('POST',
      '/api/build?source=companion&target=browser&scope=application',
      EncodeNyxBuildRequest(LDesign, ReplaceSource(LSource,
        'Result := ''A crafted application / 🌙 漢字'';',
        'Result := MissingApplicationFunction;')));
    LData := GetJSON(LReply);
    try
      Check(not TJSONObject(LData).Booleans['ok'] and
        (Pos('MissingApplicationFunction', TJSONObject(LData).Strings['log']) > 0),
        'handwritten compiler failures retain actionable companion diagnostics');
    finally
      LData.Free;
    end;
  finally
    LDesign.Free;
  end;
end;

{ Exercise executable authored registrations through the same HTTP envelope as
  Studio. Local real-control fixtures separately prove adapter focus bridges;
  these jobs prove the service preserves and executes the handwritten classes. }

procedure CompilerLocations;
var
  LDocument: TNyxDocument;
  LSource: TNyxText;
  LReply: TNyxText;
  LTarget: TNyxText;
  LScope: TNyxText;
  LData: TJSONData;
  LReport: INyxCompilerReport;
  LItem: TNyxCompilerDiagnostic;
  LExpected: ENyxSource;
  LTargetIndex: Integer;
  LScopeIndex: Integer;
  LIndex: Integer;
  LFound: Boolean;
begin
  LDocument := CreateNyxCompilerFixture(LSource);
  LExpected := ENyxSource.CreateAt('expected', LSource,
    Pos('MissingApplicationFunction;', LSource));
  try
    for LTargetIndex := 0 to 1 do
    begin
      LTarget := 'browser';

      if LTargetIndex = 1 then
      begin
        LTarget := 'lcl';
      end;
      for LScopeIndex := 0 to 1 do
      begin
        LScope := 'application';

        if LScopeIndex = 1 then
        begin
          LScope := 'view';
        end;
        LReply := Exchange('POST', '/api/build?source=companion&target=' + LTarget +
          '&scope=' + LScope + '&page=home', EncodeNyxBuildRequest(LDocument, LSource));
        LData := GetJSON(LReply);
        try
          Check(not TJSONObject(LData).Booleans['ok'] and
            (TJSONObject(LData).Find('diagnostics').JSONType = jtObject),
            'real compiler failure returns structured diagnostics / ' + LTarget + '/' + LScope);
          LReport := DecodeNyxCompilerReport(TJSONObject(LData).Find('diagnostics').AsJSON);
          Check(LReport.Source = LSource, 'compiler report retains its exact submitted companion');
          LFound := False;
          for LIndex := 0 to LReport.Count - 1 do
          begin
            LItem := LReport.Item(LIndex);

            if (LItem.Severity = csError) and
              (Pos('MissingApplicationFunction', LItem.Message) > 0) then
            begin
              Check(LItem.Navigable and (LItem.Column = 18) and
                (LItem.SourceLine = LExpected.Line) and (LItem.SourceColumn = 11) and
                (LItem.FileName = NyxCompanionUnitName(LSource) + '.pas'),
                'real compiler UTF-8 location maps to the exact Unicode helper site');

              if LScope = 'view' then
              begin
                Check(LItem.Line <> LItem.SourceLine,
                  'isolated compiler line changes while submitted helper coordinates stay stable');
              end;
              LFound := True;
              Break;
            end;
          end;
          Check(LFound, 'real compiler helper error is retained in the typed report');
        finally
          LData.Free;
        end;
      end;
    end;
  finally
    LExpected.Free;
    LDocument.Free;
  end;
end;

procedure PairedProjectFiles;
var
  LDesign: TNyxDocument;
  LPair: TNyxProjectPair;
  LRejected: TNyxProjectPair;
  LPascal: TNyxText;
  LPath: TNyxText;
  LReply: TNyxDataValue;
  LRevision: TNyxText;
  LSaved: TNyxText;

  function SaveRequest(const AExpected: TNyxText; const APair: TNyxProjectPair): TNyxText;
  begin
    Result := NyxObject([
      NyxField('version', NyxData(1)),
      NyxField('expected', NyxData(AExpected)),
      NyxField('project', NyxData(EncodeNyxProject(APair)))
    ]).ToJSON;
  end;

begin
  LDesign := CreateNyxEditedFixture(LPascal);
  try
    LPath := '/api/project?name=http-project-' + FormatDateTime('yyyymmddhhnnsszzz', Now);
    LPair := NyxProjectPair(TNyxCodec.Encode(LDesign), LPascal);
    LPair.Pending := True;
    LPair.Draft := 'rejected / 🌙' + NyxScalarText(0);
    LPair.DraftBase := 'original older baseline / 漢字';
    LReply := TNyxDataValue.ParseJSON(Exchange('GET', LPath, '', 404));
    Check((LReply.Field('project').AsText = '') and
      (LReply.Field('revision').AsText = ''), 'missing saved project has no revision');
    LReply := TNyxDataValue.ParseJSON(Exchange('POST', LPath, SaveRequest('', LPair)));
    LRevision := LReply.Field('revision').AsText;
    LSaved := LReply.Field('project').AsText;
    Check(LRevision <> '', 'paired project save supplies its server revision');
    Check(DecodeNyxProject(LSaved).Source = LPascal, 'project HTTP preserves exact crafted companion');
    Check((DecodeNyxProject(LSaved).Draft = LPair.Draft) and
      (DecodeNyxProject(LSaved).DraftBase = LPair.DraftBase),
      'project HTTP preserves NUL/Unicode rejected draft and stale baseline');
    LReply := TNyxDataValue.ParseJSON(Exchange('GET', LPath, ''));
    Check((LReply.Field('project').AsText = LSaved) and
      (LReply.Field('revision').AsText = LRevision), 'HTTP reopen reads the same paired disk version');
    LReply := TNyxDataValue.ParseJSON(Exchange('POST', LPath, SaveRequest('', LPair), 409));
    Check((LReply.Field('project').AsText = LSaved) and
      (LReply.Field('revision').AsText = LRevision), 'HTTP create conflict returns the saved pair');
    LRejected := LPair;
    LRejected.Source := ReplaceSource(LPascal, '.Text(''CRAFTED / 🌙'')', '.Text(''Divergent'')');
    Check(Pos('different values', Exchange('POST', LPath, SaveRequest(LRevision, LRejected), 400)) > 0,
      'HTTP refuses mismatched pair before replacement');
    LReply := TNyxDataValue.ParseJSON(Exchange('GET', LPath, ''));
    Check(LReply.Field('project').AsText = LSaved, 'rejected HTTP pair preserves accepted disk files');
    LRejected := LPair;
    LRejected.Source := 'unsupported Pascal';
    Check(Pos('unit', Exchange('POST', LPath, SaveRequest(LRevision, LRejected), 400)) > 0,
      'HTTP rejects unsupported accepted source');
    LPair.Draft := 'newer rejected draft';
    LReply := TNyxDataValue.ParseJSON(Exchange('POST', LPath, SaveRequest(LRevision, LPair)));
    Check(LReply.Field('revision').AsText <> LRevision, 'draft-only HTTP save changes revision');
    LSaved := LReply.Field('project').AsText;
    LReply := TNyxDataValue.ParseJSON(Exchange('POST', LPath, SaveRequest(LRevision, LPair), 409));
    Check(LReply.Field('project').AsText = LSaved, 'stale HTTP writer retains the newer project');
    Check(Pos('unsupported character', Exchange('GET', '/api/project?name=..%2Fescape', '', 400)) > 0,
      'project paths cannot escape service storage');
    Check(Pos('reserved', Exchange('GET', '/api/project?name=CON', '', 400)) > 0,
      'reserved native directory names are rejected');
    Check(Pos('Origin', Exchange('POST', LPath, SaveRequest('', LPair), 403,
      'http://different.example')) > 0, 'cross-origin project writes are rejected');
    Check(Pos('project packet', Exchange('POST', LPath,
      '{"version":1,"expected":"","project":"{}"}', 400)) > 0,
      'HTTP refuses malformed project packets');
  finally
    LDesign.Free;
  end;
end;


procedure StructuralBuilds;
var
  LDesign: TNyxDocument;
  LIsolated: TNyxDocument;
  LSource: TNyxText;
  LExpected: TNyxText;
  LReply: TNyxText;
  LTarget: TNyxText;
  LScope: TNyxText;
  LPage: TNyxText;
  LIndex: Integer;
  LData: TJSONData;
  LJob: TJSONObject;
begin
  LDesign := CreateNyxStructuralSourceFixture(LSource);
  try
    for LIndex := 0 to 5 do
    begin
      LTarget := 'browser';

      if LIndex >= 3 then
      begin
        LTarget := 'lcl';
      end;
      LScope := 'view';
      LPage := 'code-notes';
      case LIndex mod 3 of
        0: LScope := 'application';
        2: LPage := 'code-notice';
      end;
      LReply := Exchange('POST', '/api/build?source=companion&target=' + LTarget +
        '&scope=' + LScope + '&page=' + LPage, EncodeNyxBuildRequest(LDesign, LSource));
      LData := GetJSON(LReply);
      try
        LJob := TJSONObject(LData);
        Check(LJob.Booleans['ok'] and LJob.Booleans['companion'],
          'source-created structure compiles through HTTP for ' + LTarget + '/' + LScope + '/' + LPage);
        LExpected := LSource;

        if LScope = 'view' then
        begin
          LIsolated := CloneNyxViewDocument(LDesign, LDesign.Find(LPage));
          try
            LExpected := PrepareNyxCompanion(LDesign, LIsolated, LSource, True);
          finally
            LIsolated.Free;
          end;
        end;
        LReply := Exchange('GET', '/' + LJob.Strings['source'], '');
        Check(LReply = LExpected,
          'delegated compiler receives the exact admitted structural companion');
        WriteLn('Structural artifact ', LIndex, ': ', LJob.Strings['artifact']);
      finally
        LData.Free;
      end;
    end;
  finally
    LDesign.Free;
  end;
end;

procedure CollectionBuilds;
var
  LDesign: TNyxDocument;
  LIsolated: TNyxDocument;
  LSource: TNyxText;
  LExpected: TNyxText;
  LReply: TNyxText;
  LTarget: TNyxText;
  LScope: TNyxText;
  LPage: TNyxText;
  LIndex: Integer;
  LData: TJSONData;
  LJob: TJSONObject;
begin
  LDesign := CreateNyxCollectionFixture;
  try
    LDesign.Find('home').Add(TNyxNode.Create(nkTable, 'tasks-data').Binds.Collection(
      NyxCollectionView(LDesign.Collections.Key(0))
        .Column(NyxTextField(LDesign.Collections.Snapshot(LDesign.Collections.Key(0)).Schema.FieldAt(0).Name), 'Task', cmEditable)
        .Column(NyxIntegerField('priority'), 'Priority', cmEditable)).Done);
    LDesign.FindComponent('task-card').Add(TNyxNode.Create(nkList, 'task-card-list')
      .Binds.Collection(NyxCollectionView(LDesign.Collections.Key(0))
        .Column(NyxTextField(LDesign.Collections.Snapshot(LDesign.Collections.Key(0)).Schema.FieldAt(0).Name), 'Task').Scoped(csInstance)).Done);
    LSource := TNyxCodegen.Generate(LDesign, 'nyx.fixture.collections.http');
    for LIndex := 0 to 5 do
    begin
      LTarget := 'browser';

      if LIndex >= 3 then
      begin
        LTarget := 'lcl';
      end;
      LScope := 'view';
      LPage := 'home';
      case LIndex mod 3 of
        0: LScope := 'application';
        2: LPage := 'task-card';
      end;
      LReply := Exchange('POST', '/api/build?source=companion&target=' + LTarget +
        '&scope=' + LScope + '&page=' + LPage, EncodeNyxBuildRequest(LDesign, LSource));
      LData := GetJSON(LReply);
      try
        LJob := TJSONObject(LData);
        Check(LJob.Booleans['ok'] and LJob.Booleans['companion'],
          'typed collection companion compiles for ' + LTarget + '/' + LScope + '/' + LPage);
        LExpected := LSource;

        if LScope = 'view' then
        begin
          LIsolated := CloneNyxViewDocument(LDesign, LDesign.Find(LPage));
          try
            LExpected := PrepareNyxCompanion(LDesign, LIsolated, LSource, True);
          finally
            LIsolated.Free;
          end;
        end;
        LReply := Exchange('GET', '/' + LJob.Strings['source'], '');
        Check(LReply = LExpected, 'delegated compiler receives exact collection schema/rows/source');
        Check((Pos('.Collection(', LReply) > 0) and (Pos('csInstance', LReply) > 0),
          'v3 compiler source retains automatic typed application/instance bindings');
        LReply := Exchange('GET', '/builds/' + LJob.Strings['build'] + '/design.nyx', '');
        LIsolated := TNyxCodec.Decode(LReply);
        try
          Check((LIsolated.Collections.Count = LDesign.Collections.Count) and
            (LIsolated.Collections.Key(0).Name = LDesign.Collections.Key(0).Name) and
            LIsolated.HasCollectionViews, 'every saved job design retains Unicode defaults and typed bindings');
        finally
          LIsolated.Free;
        end;
        WriteLn('Collection artifact ', LIndex, ': ', LJob.Strings['artifact']);
      finally
        LData.Free;
      end;
    end;
  finally
    LDesign.Free;
  end;
end;

procedure AuthoredCallbackBuilds;
var
  LDesign: TNyxDocument;
  LIsolated: TNyxDocument;
  LThreaded: TNyxDocument;
  LSource: TNyxText;
  LExpectedSource: TNyxText;
  LReply: TNyxText;
  LTarget: TNyxText;
  LScope: TNyxText;
  LPage: TNyxText;
  LData: TJSONData;
  LJob: TJSONObject;
  LTestIndex: Integer;
  LProcess: TProcess;
  LFile: TFileStream;
  LBytes: RawByteString;
  LExpectedCount: TNyxText;
begin
  LDesign := CreateNyxCallbackFixture(LSource);
  try
    LSource := ReplaceSource(LSource, 'implementation' + #10,
      'implementation' + #10 + #10 +
      '{$IFDEF PAS2JS}uses Web, SysUtils;{$ELSE}uses Classes, SysUtils, nyx.composition;{$ENDIF}' + #10);
    LSource := ReplaceSource(LSource, 'initialization' + #10,
      '{$IFDEF PAS2JS}' + #10 +
      'procedure VerifyAuthoredCallback;' + #10 +
      'var LInput: TJSHTMLElement;' + #10 +
      'begin' + #10 +
      '  LInput := TJSHTMLElement(document.querySelector(''textarea''));' + #10 +
      #10 + '  if LInput <> nil then' + #10 +
      '  begin' + #10 + '    LInput.focus;' + #10 + '  end;' + #10 +
      '  document.body.setAttribute(''data-nyx-authored-callbacks'', IntToStr(CallbackInvocations));' + #10 +
      'end;' + #10 + '{$ELSE}' + #10 +
      'procedure VerifyAuthoredCallback;' + #10 +
      'var' + #10 +
      '  LDesign: TNyxDocument; LRoot, LControl: TNyxNode;' + #10 +
      '  LEvents: INyxEvents; LInfo: TNyxEventInfo; LFile: TFileStream; LText: TNyxText;' + #10 +
      'begin' + #10 +
      '  LDesign := BuildNyxDocument;' + #10 +
      '  LRoot := nil;' + #10 +
      '  try' + #10 +
      '    LEvents := NewNyxEvents;' + #10 +
      '    BindNyxCallbacks(LDesign, LEvents);' + #10 +
      '    LRoot := RealizeNyxView(LDesign, LDesign.Pages[0]);' + #10 +
      '    LControl := LRoot.Find(''reply-memo'');' + #10 +
      #10 + '    if LControl = nil then' + #10 +
      '    begin' + #10 + '      LControl := LRoot.Find(''template-reply'');' + #10 + '    end;' + #10 +
      #10 + '    if LControl = nil then' + #10 +
      '    begin' + #10 + '      raise Exception.Create(''Missing authored callback control'');' + #10 + '    end;' + #10 +
      '    LInfo.Trigger := ntAfterEnter;' + #10 +
      '    LInfo.Name := NyxEvent(''after-enter'');' + #10 +
      '    LInfo.SourceID := LControl.ID;' + #10 +
      '    LInfo.OriginID := LControl.ID;' + #10 +
      '    LInfo.TargetID := LControl.ID;' + #10 +
      '    LInfo.ValueID := LControl.ID;' + #10 +
      '    LInfo.Value := NyxData(LControl.Prop(''value''));' + #10 +
      '    LInfo.HasValue := True;' + #10 +
      '    LInfo.ValueKind := nskText;' + #10 +
      '    LInfo.Changed := False;' + #10 +
      '    LEvents.Dispatch(LInfo, LControl.DesignID, LControl.DesignID);' + #10 +
      '    LText := IntToStr(CallbackInvocations);' + #10 +
      '    LFile := TFileStream.Create(ChangeFileExt(ParamStr(0), ''.verification''), fmCreate);' + #10 +
      '    try' + #10 +
      '      LFile.WriteBuffer(LText[1], Length(LText));' + #10 +
      '    finally' + #10 + '      LFile.Free;' + #10 + '    end;' + #10 +
      '  finally' + #10 +
      '    LEvents := nil;' + #10 + '    ReleaseNyxNode(LRoot);' + #10 + '    LDesign.Free;' + #10 +
      '  end;' + #10 + 'end;' + #10 + '{$ENDIF}' + #10 + #10 + 'initialization' + #10);
    LSource := ReplaceSource(LSource, 'end.' + #10,
      '{$IFDEF PAS2JS}' + #10 +
      '  window.setTimeout(@VerifyAuthoredCallback, 0);' + #10 +
      '{$ELSE}' + #10 +
      #10 + '  if ParamStr(1) = ''--nyx-verify'' then' + #10 +
      '  begin' + #10 + '    VerifyAuthoredCallback;' + #10 + '    Halt(0);' + #10 + '  end;' + #10 +
      '{$ENDIF}' + #10 + 'end.' + #10);
    for LTestIndex := 0 to 5 do
    begin
      LTarget := 'browser';

      if LTestIndex >= 3 then
      begin
        LTarget := 'lcl';
      end;
      LScope := 'view';
      LPage := 'home';
      LExpectedCount := '11';
      case LTestIndex mod 3 of
        0: LScope := 'application';
        2:
          begin
            LPage := 'reply-template';
            LExpectedCount := '100';
          end;
      end;
      LReply := Exchange('POST', '/api/build?source=companion&target=' + LTarget +
        '&scope=' + LScope + '&page=' + LPage, EncodeNyxBuildRequest(LDesign, LSource));
      LData := GetJSON(LReply);
      try
        LJob := TJSONObject(LData);
        Check(LJob.Booleans['ok'], 'authored callback companion compiles for ' +
          LTarget + '/' + LScope + '/' + LPage + ': ' + LJob.Strings['log']);
        LReply := Exchange('GET', '/' + LJob.Strings['source'], '');
        LExpectedSource := LSource;

        if LScope = 'view' then
        begin
          LIsolated := CloneNyxViewDocument(LDesign, LDesign.Find(LPage));
          try
            LExpectedSource := PrepareNyxCompanion(LDesign, LIsolated, LSource, True);
          finally
            LIsolated.Free;
          end;
        end;
        Check(LReply = LExpectedSource, 'callback job preserves exact source frames and typed descriptors');
        WriteLn('Authored callback artifact ', LTestIndex, ': ', LJob.Strings['artifact']);

        if LTarget = 'lcl' then
        begin
          LProcess := TProcess.Create(nil);
          try
            LProcess.Executable := ExpandFileName('build/studio/jobs/' +
              LJob.Strings['build'] + '/nyx_native.exe');
            LProcess.Parameters.Add('--nyx-verify');
            LProcess.Options := [poWaitOnExit, poNoConsole];
            LProcess.Execute;
            LFile := TFileStream.Create(ChangeFileExt(LProcess.Executable,
              '.verification'), fmOpenRead);
            try
              SetLength(LBytes, LFile.Size);
              LFile.ReadBuffer(LBytes[1], Length(LBytes));
            finally
              LFile.Free;
            end;
            Check((LProcess.ExitStatus = 0) and (LBytes = LExpectedCount),
              'delegated native output executes the compiled authored callbacks');
          finally
            LProcess.Free;
          end;
        end;
      finally
        LData.Free;
      end;
    end;
    LThreaded := LDesign.Clone;
    try
      NyxCallbacks(LThreaded.Find('reply-memo')).OnAfterEnter.Policy(neThreaded);
      LReply := Exchange('POST', '/api/build?source=companion&target=browser&scope=application',
        EncodeNyxBuildRequest(LThreaded, PrepareNyxCompanion(LDesign, LThreaded, LSource, True)), 400);
      Check(Pos('threaded', LowerCase(LReply)) > 0,
        'browser build rejects worker-only policy before allocating a compiler job');
    finally
      LThreaded.Free;
    end;
  finally
    LDesign.Free;
  end;
end;

begin
  LBaseURL := 'http://127.0.0.1:8088';

  if ParamCount > 0 then
  begin
    LBaseURL := ParamStr(1);
  end;
  LDocument := CreateNyxPersistenceFixture;
  try
    { Studio's client and compiler service share crafted state-name rules too.
      These type-bearing keys exercise the boundary beyond a store-only fixture. }
    LDocument.State.SetValue(NyxTextState('replyText'), 'HTTP authored default / 🌙')
      .SetValue(NyxBooleanState('canReplyBoolean'), True);
    { The same fixture can be run against loopback or the LAN address. Admission
      must follow that destination origin, while rejecting other browser sites. }
    Check(Pos('"ok":true', Exchange('GET', '/api/health', '')) > 0,
      'same-origin health is available at the selected Studio address');
    LResponse := Exchange('POST', '/api/generate', TNyxCodec.Encode(LDocument),
      403, 'http://example.invalid');
    Check(Pos('Origin does not match', LResponse) > 0,
      'cross-origin generation is rejected at loopback and LAN addresses');
    LResponse := Exchange('POST', '/api/generate',
      '{"version":1,"title":"","pages":[],"components":[],"state":{"x":{"type":"integer","value":1.5}}}', 400);
    Check(Pos('signed 32-bit', LResponse) > 0,
      'HTTP design admission rejects untyped/fractional state integers');
    LResponse := Exchange('POST', '/api/generate',
      '{"version":1,"title":"","pages":[],"components":[],"state":{"x":{"type":"integer","value":1.0000000000000000001}}}', 400);
    Check(Pos('signed 32-bit', LResponse) > 0,
      'HTTP integer admission cannot round away fractional decimal digits');
    LResponse := Exchange('POST', '/api/generate',
      '{"version":1.0000000000000000001,"title":"","pages":[],"components":[]}', 400);
    Check(Pos('Unsupported design version', LResponse) > 0,
      'HTTP version admission requires the exact supported integer tag');
    LResponse := Exchange('POST', '/api/generate',
      '{"version":1,"title":"","pages":[],"components":[],"state":{"x":{"type":"number","value":"1e-324"}}}', 400);
    Check(Pos('finite decimal', LResponse) > 0,
      'HTTP design admission refuses nonzero numeric underflow');
    { A derived primitive must compile from persisted design alone: the service's
      generated application has no handle to this construction-time registry. }
    LCatalog := TNyxCatalog.Create;
    try
      LTemplate := LCatalog.NewNode('button', 'custom-action-template')
        .SetProp('text', 'Derived / 🌙').SetProp('emit', 'accept');
      try
        LCatalog.RegisterRecipe('derived-action', 'Derived action', 'Custom', LTemplate);
      finally
        LTemplate.Free;
      end;
      LDocument.AddPage(LCatalog.NewNode('derived-action', 'custom-action'));
    finally
      LCatalog.Free;
    end;
    { Configuration changes are reversible test fixtures. Restore the exact
      accepted machine profile even if a diagnostic or late build check fails. }
    LConfiguration := Exchange('GET', '/api/configuration', '');
    LOutputs := TNyxOutputConfiguration.Decode(LConfiguration);
    try
      try
        LResponse := Exchange('POST', '/api/configuration', '{"version":1,"fields":{}}');
        LBrowserOnly := TNyxOutputConfiguration.Decode(LResponse);
        try
          Check((LBrowserOnly.Field('pas2js') = '') and (LBrowserOnly.Field('fpc') = ''),
            'empty output profiles are admitted while Studio runs');
          LResponse := Exchange('POST', '/api/generate', TNyxCodec.Encode(LDocument));
          Check(LResponse = TNyxCodegen.Generate(LDocument),
            'project source generation works without any output tools');
          LResponse := Exchange('POST', '/api/build?target=browser&scope=view&page=home',
            TNyxCodec.Encode(LDocument), 400);
          Check(Pos('configure the pas2js compiler', LResponse) > 0,
            'missing tools produce an actionable chosen-output diagnostic');
          Exchange('POST', '/api/configuration',
            '{"version":1,"fields":{"arguments":"anything"}}', 400);
          Check(Exchange('GET', '/api/configuration', '') = LBrowserOnly.Encode,
            'invalid profile preserves the accepted empty configuration');
          LBrowserOnly.SetField('pas2js', LOutputs.Field('pas2js'));
          LBrowserOnly.SetField('runtime', LOutputs.Field('runtime'));
          Exchange('POST', '/api/configuration', LBrowserOnly.Encode);
          LResponse := Exchange('POST', '/api/build?target=browser&scope=view&page=home',
            TNyxCodec.Encode(LDocument));
          LResult := GetJSON(LResponse);
          try
            Check(TJSONObject(LResult).Booleans['ok'],
              'late browser configuration builds without a native profile or restart');
          finally
            LResult.Free;
          end;
        finally
          LBrowserOnly.Free;
        end;
      finally
        Exchange('POST', '/api/configuration', LConfiguration);
      end;
    finally
      LOutputs.Free;
    end;
    LResponse := Exchange('POST', '/api/generate', TNyxCodec.Encode(LDocument));
    LExpected := TNyxCodegen.Generate(LDocument);

    if LResponse <> LExpected then
    begin
      LDifference := 1;
      while (LDifference <= Length(LResponse)) and
        (LDifference <= Length(LExpected)) and
        (LResponse[LDifference] = LExpected[LDifference]) do
      begin
        Inc(LDifference);
      end;
      WriteLn('First source difference at byte ', LDifference);
      WriteLn('Lengths expected=', Length(LExpected), ' received=', Length(LResponse));

      if LDifference <= Length(LResponse) then
      begin
        WriteLn('Received byte=', Ord(LResponse[LDifference]));
      end;
      WriteLn('Expected: ', Copy(LExpected, LDifference, 120));
      WriteLn('Received: ', Copy(LResponse, LDifference, 120));
    end;
    Check(LResponse = LExpected,
      'Unicode network request generates byte-identical Pascal');
    Check((Pos('LCanReplyBooleanState: TNyxBooleanStateRef;', LResponse) > 0) and
      (Pos('TextTextState', LResponse) = 0),
      'client/service generation retains crafted type-bearing state names');
    Check((Pos('NyxExtension(''studio.assets'')', LResponse) > 0) and
      (Pos('NyxDecimal(''9007199254740993'')', LResponse) > 0) and
      (Pos('NyxDecimal(''1.234567890123456789'')', LResponse) > 0),
      'generation preserves structured project data and exact decimal spelling');
    LTemplate := LDocument.Find('custom-action');
    LTemplate.Named('custom-🌙漢字').SetProp('width', '120000');
    LResponse := Exchange('POST', '/api/build?target=browser&scope=view&page=custom-action',
      TNyxCodec.Encode(LDocument), 400);
    LResult := GetJSON(LResponse);
    try
      Check(Pos(TNyxText('Invalid width on custom-🌙漢字'),
        TJSONObject(LResult).Strings['error']) > 0,
        'HTTP typed diagnostics preserve Unicode source identity before compiling');
    finally
      LResult.Free;
    end;
    LTemplate.Named('custom-action').SetProp('width', '');
    LTemplate := LDocument.Find('fixture-title-override');
    LTemplate.SetProp('path', 'missing-part');
    LResponse := Exchange('POST', '/api/build?target=browser&scope=view&page=home',
      TNyxCodec.Encode(LDocument), 400);
    Check(Pos('Invalid part override', LResponse) > 0,
      'HTTP rejects missing reusable part paths before compiling');
    LTemplate.SetProp('path', 'title');
    LResponse := Exchange('POST', '/api/generate',
      StringReplace(TNyxCodec.Encode(LDocument), NyxIdentityInstanceID,
        NyxIdentityInstanceID + TNyxText('界'), [rfReplaceAll]), 400);
    Check(Pos('128 Unicode scalar', LResponse) > 0,
      'HTTP rejects overflowing Unicode identity before generating source');
    for LIndex := 0 to 9 do
    begin
      LResponse := Exchange('POST', '/api/build?' + BuildQuery(LIndex),
        TNyxCodec.Encode(LDocument));
      LResult := GetJSON(LResponse);
      try
        Check((LResult is TJSONObject) and TJSONObject(LResult).Booleans['ok'],
          'compiler accepted ' + BuildQuery(LIndex));
        LResponse := Exchange('GET', '/' + TJSONObject(LResult).Strings['source'], '');
        { Definition-only and derived-button builds omit unrelated instances.
          Each isolated view retains just its own payloads and required recipes. }
        Check((Pos(TNyxText('🌙'), LResponse) > 0) and
          ((not (LIndex in [0, 2, 3])) or
          ((Pos('NewNyxSlotOverride', LResponse) > 0) and
          (Pos('INyxSlotOverride', LResponse) > 0))) and
          ((not (LIndex in [6, 8, 9])) or
            (Pos(TNyxText('identity/caption~🌙'), LResponse) > 0)) and
          ((LIndex <> 7) or (Pos(NyxIdentityDefinitionID, LResponse) > 0)),
          'compiled job source preserves Unicode and scope-owned part overrides');
        Check((Pos('NyxTextState(''🌙/reply'')', LResponse) > 0) and
          (Pos('NyxNumberState(''whole'')', LResponse) > 0) and
          (Pos('.SetValue(LWholeNumberState, 10000000000000000.0)', LResponse) > 0) and
          (Pos('NyxScalarText(0)', LResponse) > 0),
          'every browser/native application/view build retains typed exact defaults');
        Check((Pos('NyxExtension(''studio.assets'')', LResponse) > 0) and
          (Pos('NyxDecimal(''9007199254740993'')', LResponse) > 0) and
          (Pos('NyxDecimal(''1.234567890123456789'')', LResponse) > 0),
          'every browser/native application/view build retains exact project extension data');
        WriteLn('Artifact ', LIndex, ': ', TJSONObject(LResult).Strings['artifact']);
      finally
        LResult.Free;
      end;
    end;
    CompanionBuilds;
    CompilerLocations;
    StructuralBuilds;
    CollectionBuilds;
    AuthoredCallbackBuilds;
    InteractionAndSplitBuilds;
    PairedProjectFiles;
    WriteLn('PASS ', LCount, ' HTTP generation/build/Unicode checks');
  finally
    LDocument.Free;
  end;
end.
