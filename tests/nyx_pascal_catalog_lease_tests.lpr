{ Immutable input ownership must remove repeated preparation without admitting
  mutable/alternate providers as source authority. No host file or SDK is read. }
program nyx_pascal_catalog_lease_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, {$ifndef PAS2JS}Classes,{$endif} nyx.text, nyx.bytes,
  nyx.pascal.files, nyx.pascal.encoding, nyx.pascal.context,
  nyx.pascal.translation, nyx.pascal.syntax, nyx.pascal.syntax.translated;

type
  { Mutable external provider, serialized by this test while capture runs.
    Count/search callbacks can refuse; the resulting capture must retain no
    provider borrow or alias to a later replacement of Current. }
  TTextCatalog = class(TInterfacedObject, INyxPascalFileCatalog)
  public
    Current: INyxPascalFileCatalog;
    Reads, FailAt, CountOverride, DirectoryOverride: Integer;
    DuplicateName: Boolean;
    constructor Create(const ACurrent: INyxPascalFileCatalog);
    procedure Read;
    function CasePolicy: TNyxPascalFileCase;
    function Count: Integer;
    function FileAt(AIndex: Integer): TNyxPascalPhysicalFile;
    function TextAt(AIndex: Integer): TNyxText;
    function ByteCount: Integer;
    function Find(const AFile: TNyxPascalPhysicalFile): Integer;
    function WithFile(const AFile: TNyxPascalPhysicalFile; const AText: TNyxText): INyxPascalFileCatalog;
    function WithIncludeDirectory(const ADirectory: TNyxText): INyxPascalFileCatalog;
    function DirectoryCount: Integer;
    function DirectoryAt(AIndex: Integer): TNyxText;
    function ResolveInclude(const AFrom: TNyxPascalPhysicalFile; const AName: TNyxText): Integer;
    function SameFiles(const AOther: INyxPascalFileCatalog): Boolean;
  end;
  { Optional alternate byte provider preserves original packets and opaque
    entries. Its mutable wrapper still grants no immutable catalog authority. }
  TByteCatalog = class(TTextCatalog, INyxPascalByteFileCatalog)
    function SourceBytesAt(AIndex: Integer): INyxPascalSourceBytes;
    function SourceViewAt(AIndex: Integer): INyxPascalSourceView;
    function OriginalByteCount: Integer;
    function WithByteSource(const AFile: TNyxPascalPhysicalFile;
      const ABytes: INyxPascalSourceBytes; const AView: INyxPascalSourceView = nil): INyxPascalFileCatalog;
  end;
  { Deliberate ABI-compatible counterfeit of the private owner receipt. Returning
    a real catalog object is insufficient: its ordinary interface must also be
    the requesting object. This negative fixture never supplies an invalid pointer. }
  IClaimedOwner = interface(IInterface)
    ['{84F6E6F3-CCAF-45F2-A18D-9153CF573653}']
    function CatalogOwner: TObject;
  end;
  { Keeps its own mutable data getters while attempting to borrow another
    object's owner identity. The production gate must refuse the whole request. }
  TImpostor = class(TTextCatalog, IClaimedOwner)
    function CatalogOwner: TObject;
  end;
  {$ifndef PAS2JS}
  { Readers share one immutable owned input. Captures/results remain local;
    main joins before reading counters or retiring any thread lease. }
  TCatalogReader = class(TThread)
  protected
    procedure Execute; override;
  public
    Source: INyxPascalFileCatalog;
    Iterations: Integer;
    ErrorText: String;
  end;
  {$endif}

var
  GChecks: Integer;
  GMeasureOnly: Boolean;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

constructor TTextCatalog.Create(const ACurrent: INyxPascalFileCatalog);
begin
  inherited Create;
  Current := ACurrent;
  CountOverride := -1;
  DirectoryOverride := -1;
end;

procedure TTextCatalog.Read;
begin
  Inc(Reads);

  if (FailAt > 0) and (Reads >= FailAt) then raise ENyxPascalContext.Create('External provider refused this read');
end;

function TTextCatalog.CasePolicy: TNyxPascalFileCase;
begin Read; Result := Current.CasePolicy; end;
function TTextCatalog.Count: Integer;
begin
  Read;
  Result := CountOverride;

  if Result = -1 then Result := Current.Count;
end;
function TTextCatalog.FileAt(AIndex: Integer): TNyxPascalPhysicalFile;
begin
  Read;

  if DuplicateName then AIndex := 0;
  Result := Current.FileAt(AIndex);
end;
function TTextCatalog.TextAt(AIndex: Integer): TNyxText;
begin Read; Result := Current.TextAt(AIndex); end;
function TTextCatalog.ByteCount: Integer;
begin Read; Result := Current.ByteCount; end;
function TTextCatalog.Find(const AFile: TNyxPascalPhysicalFile): Integer;
begin Read; Result := Current.Find(AFile); end;
function TTextCatalog.WithFile(const AFile: TNyxPascalPhysicalFile; const AText: TNyxText): INyxPascalFileCatalog;
begin Read; Result := Current.WithFile(AFile, AText); end;
function TTextCatalog.WithIncludeDirectory(const ADirectory: TNyxText): INyxPascalFileCatalog;
begin Read; Result := Current.WithIncludeDirectory(ADirectory); end;
function TTextCatalog.DirectoryCount: Integer;
begin
  Read;
  Result := DirectoryOverride;

  if Result = -1 then Result := Current.DirectoryCount;
end;
function TTextCatalog.DirectoryAt(AIndex: Integer): TNyxText;
begin Read; Result := Current.DirectoryAt(AIndex); end;
function TTextCatalog.ResolveInclude(const AFrom: TNyxPascalPhysicalFile; const AName: TNyxText): Integer;
begin Read; Result := Current.ResolveInclude(AFrom, AName); end;
function TTextCatalog.SameFiles(const AOther: INyxPascalFileCatalog): Boolean;
begin Read; Result := Current.SameFiles(AOther); end;

function TByteCatalog.SourceBytesAt(AIndex: Integer): INyxPascalSourceBytes;
begin Read; Result := NyxPascalSourceBytesAt(Current, AIndex); end;
function TByteCatalog.SourceViewAt(AIndex: Integer): INyxPascalSourceView;
begin Read; Result := NyxPascalSourceViewAt(Current, AIndex); end;
function TByteCatalog.OriginalByteCount: Integer;
var
  LBytes: INyxPascalByteFileCatalog;
begin Read; Supports(Current, INyxPascalByteFileCatalog, LBytes); Result := LBytes.OriginalByteCount; end;
function TByteCatalog.WithByteSource(const AFile: TNyxPascalPhysicalFile;
  const ABytes: INyxPascalSourceBytes; const AView: INyxPascalSourceView): INyxPascalFileCatalog;
var
  LBytes: INyxPascalByteFileCatalog;
begin Read; Supports(Current, INyxPascalByteFileCatalog, LBytes); Result := LBytes.WithByteSource(AFile, ABytes, AView); end;
function TImpostor.CatalogOwner: TObject;
begin Result := Current as TInterfacedObject; end;

function Fixture(ACount: Integer = 8): INyxPascalFileCatalog;
var
  LAt: Integer;
  LName: TNyxText;
begin
  Result := NewNyxPascalFileCatalog(pfcASCIIInsensitive);
  for LAt := 1 to ACount do
  begin
    LName := TNyxText(IntToStr(LAt));
    Result := Result.WithFile(NyxPascalPhysicalFile('units/U' + LName + '.pas'),
      'unit U' + LName + '; interface const Value=' + LName + '; implementation end.');
  end;
  Result := Result.WithIncludeDirectory('units').WithIncludeDirectory('shared');
end;

procedure OwnershipAndLifetime;
var
  LFiles, LCaptured, LChanged: INyxPascalFileCatalog;
  LIdentity: TNyxText;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact);
  Check((CaptureNyxPascalFiles(LFiles) = LFiles) and (LFiles.Count = 0),
    'An empty validated immutable catalog is already frozen too');
  LFiles := Fixture;
  LIdentity := NyxPascalFileCatalogIdentity(LFiles);
  LCaptured := CaptureNyxPascalFiles(LFiles);
  Check(LCaptured = LFiles, 'An exact owned immutable catalog needs no duplicate capture or reconstruction');
  Check(NyxPascalFileCatalogIdentity(LCaptured) = LIdentity, 'The exact input owner retains its original process-local identity');
  LChanged := LFiles.WithFile(NyxPascalPhysicalFile('units/U1.pas'), 'unit U1; interface const Changed=99; implementation end.');
  Check((NyxPascalFileCatalogIdentity(LChanged) <> LIdentity) and
    (LCaptured.TextAt(0) = LFiles.TextAt(0)) and (LChanged.TextAt(0) <> LCaptured.TextAt(0)),
    'Fluent edits obtain a distinct immutable input identity and preserve previous text');
  LFiles := nil;
  Check((LCaptured.Count = 8) and (LCaptured.DirectoryAt(0) = TNyxText('units')) and
    (LCaptured.DirectoryAt(1) = TNyxText('shared')), 'A captured lease outlives the caller and retains exact search order');
  LCaptured := CaptureNyxPascalFiles(LCaptured);
  Check(NyxPascalFileCatalogIdentity(LCaptured) = LIdentity, 'In-place repeated capture keeps the input lease alive');
  LChanged := CaptureNyxPascalFiles(LChanged);
  Check(LChanged.Find(NyxPascalPhysicalFile('UNITS/u1.PAS')) = 0, 'Case policy and normalized lookup keep their captured meaning');
end;

procedure AlternateInputs;
var
  LProvider: TTextCatalog;
  LByteProvider: TByteCatalog;
  LExternal, LCaptured, LNew: INyxPascalFileCatalog;
  LBytes: INyxPascalByteFileCatalog;
  LRaw: INyxPascalSourceBytes;
  LView: INyxPascalSourceView;
  LData: TNyxBytes;
  LIdentity: TNyxText;
begin
  LProvider := TTextCatalog.Create(Fixture);
  LExternal := LProvider;
  LCaptured := CaptureNyxPascalFiles(LExternal);
  Check((LProvider.Reads > 8) and (LCaptured <> LExternal) and LCaptured.SameFiles(LProvider.Current),
    'Alternate text providers still undergo complete validated capture');
  LIdentity := NyxPascalFileCatalogIdentity(LCaptured);
  LProvider.Current := LProvider.Current.WithFile(NyxPascalPhysicalFile('units/U1.pas'), 'changed');
  Check(LCaptured.TextAt(0) <> LProvider.Current.TextAt(0), 'Later external mutation cannot change frozen source text');
  LNew := CaptureNyxPascalFiles(LExternal);
  Check(NyxPascalFileCatalogIdentity(LNew) <> LIdentity, 'A fresh alternate capture never claims a previous input identity');
  LProvider.FailAt := LProvider.Reads + 1;
  Check((LCaptured.Count = 8) and (LCaptured.Find(NyxPascalPhysicalFile('units/U8.pas')) = 7),
    'Published captured queries call no external provider');

  LData := NyxEncodeUTF8(TNyxText('Hello 😀') + #13#10);
  LRaw := NewNyxPascalSourceBytes(LData);
  LView := DecodeNyxPascalSource(LRaw, NyxPascalUTF8Codec);
  Supports(Fixture, INyxPascalByteFileCatalog, LBytes);
  LNew := LBytes.WithByteSource(NyxPascalPhysicalFile('shared/greeting.txt'), LRaw, LView);
  SetLength(LData, 2); LData[0] := $FF; LData[1] := $FE;
  Supports(LNew, INyxPascalByteFileCatalog, LBytes);
  LNew := LBytes.WithByteSource(NyxPascalPhysicalFile('shared/opaque.bin'), NewNyxPascalSourceBytes(LData));
  LByteProvider := TByteCatalog.Create(LNew);
  LExternal := LByteProvider;
  LCaptured := CaptureNyxPascalFiles(LExternal);
  Check(LCaptured.SameFiles(LNew) and (LCaptured <> LExternal), 'Alternate byte providers preserve all original packets and decoded views');
  Check(NyxPascalSourceBytesAt(LCaptured, 8).SameBytes(LRaw) and
    (NyxPascalSourceViewAt(LCaptured, 8).Text = LView.Text) and
    (NyxPascalSourceViewAt(LCaptured, 9) = nil), 'Unicode/CRLF and intentionally opaque bytes retain exact ownership');
  LNew := nil;
  LExternal := nil;
  Check((NyxPascalSourceBytesAt(LCaptured, 9).ByteAt(0) = $FF) and
    (NyxPascalSourceBytesAt(LCaptured, 9).ByteAt(1) = $FE), 'Opaque bytes survive alternate-provider retirement');
end;

procedure Refusals;
var
  LProvider: TTextCatalog;
  LExternal, LCandidate, LOriginal: INyxPascalFileCatalog;
  LClaim: IClaimedOwner;
  LMode: Integer;
  LRejected: Boolean;
begin
  for LMode := 0 to 5 do
  begin

    if LMode = 5 then LProvider := TImpostor.Create(Fixture)
    else LProvider := TTextCatalog.Create(Fixture);
    LExternal := LProvider;

    if LMode = 5 then
    begin
      Supports(LExternal, IClaimedOwner, LClaim);
      Check(LClaim.CatalogOwner <> nil, 'The counterfeit really presents another owned object rather than a missing receipt');
    end;
    case LMode of
      0: LProvider.CountOverride := 4097;
      1: LProvider.CountOverride := -2;
      2: LProvider.DirectoryOverride := 257;
      3: LProvider.DuplicateName := True;
      4: LProvider.FailAt := 12;
    end;
    LCandidate := nil;
    LRejected := False;
    try
      LCandidate := CaptureNyxPascalFiles(LExternal);
    except
      on ENyxPascalContext do LRejected := True;
    end;
    Check(LRejected and (LCandidate = nil) and (LProvider.Current.Count = 8),
      'Invalid bounds/names, callback refusal and counterfeit ownership publish no partial input');

    if LMode = 5 then
      Check(LProvider.Reads = 0, 'A counterfeit owner is refused before invoking its alternate data callbacks');
  end;
  LCandidate := nil;
  LRejected := False;
  try
    LCandidate := CaptureNyxPascalFiles(nil);
  except
    on ENyxPascalContext do LRejected := True;
  end;
  Check(LRejected and (LCandidate = nil), 'A missing input cannot acquire owned capture authority');
  LProvider := TTextCatalog.Create(Fixture);
  LExternal := LProvider;
  LOriginal := LExternal;
  LProvider.FailAt := 12;
  LRejected := False;
  try
    LExternal := CaptureNyxPascalFiles(LExternal);
  except
    on ENyxPascalContext do LRejected := True;
  end;
  Check(LRejected and (LExternal = LOriginal), 'An in-place alternate failure preserves the original managed input reference');
  LProvider.FailAt := 0;
  LOriginal := nil;
  LExternal := CaptureNyxPascalFiles(LExternal);
  Check((LExternal.Count = 8) and (LExternal.Find(NyxPascalPhysicalFile('units/U8.pas')) = 7),
    'An in-place successful alternate capture retires its provider only after publishing the complete result');
end;

procedure TranslationUsesCurrentInputs;
var
  LFiles, LChanged: INyxPascalFileCatalog;
  LContext: INyxPascalCompilerContext;
  LBefore, LAfter: INyxPascalTranslation;
  LView: INyxPascalTranslatedSyntax;
begin
  LFiles := NewNyxPascalFileCatalog(pfcExact)
    .WithFile(NyxPascalPhysicalFile('main.pas'), 'unit Main; interface {$i values.inc} implementation end.')
    .WithFile(NyxPascalPhysicalFile('values.inc'), 'const BeforeEdit=1;');
  LContext := NewNyxPascalCompilerContext(ppcFPC, TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi);
  LBefore := NewNyxPascalTranslation(LFiles, NyxPascalPhysicalFile('main.pas'), LContext, TNyxPascalTranslationOptions.Defaults);
  Check(LBefore.Files = LFiles, 'Translation retains the verified immutable catalog rather than reconstructing it');
  LChanged := LFiles.WithFile(NyxPascalPhysicalFile('values.inc'), 'const AfterEdit=2;');
  LAfter := NewNyxPascalTranslation(LChanged, NyxPascalPhysicalFile('main.pas'), LContext, TNyxPascalTranslationOptions.Defaults);
  Check((LAfter.Files = LChanged) and (LBefore.Files.TextAt(1) <> LAfter.Files.TextAt(1)),
    'An edited include still receives full translation from its distinct current input set');
  LView := NewNyxPascalTranslatedSyntax(LAfter, TNyxPascalSyntaxOptions.Defaults);
  Check((LView.Syntax.Named(0, 'AfterEdit') >= 0) and (LView.Syntax.Named(0, 'BeforeEdit') < 0),
    'Catalog leasing grants no old conditional/token/declaration meaning');
end;

{$ifndef PAS2JS}
procedure Measurement;
var
  LFiles: INyxPascalFileCatalog;
  LCopies: array of INyxPascalFileCatalog;
  LBefore, LAfter: PtrUInt;
  LAt, LSame: Integer;
begin
  LFiles := Fixture(128);
  SetLength(LCopies, 64);
  LBefore := GetFPCHeapStatus.CurrHeapUsed;
  for LAt := 0 to High(LCopies) do LCopies[LAt] := CaptureNyxPascalFiles(LFiles);
  LAfter := GetFPCHeapStatus.CurrHeapUsed;
  LSame := 0;
  for LAt := 0 to High(LCopies) do
  begin

    if LCopies[LAt] = LFiles then Inc(LSame);
  end;
  WriteLn('owned catalog captures / files=128 retained=64 same-owner=', LSame,
    ' heap-delta=', Int64(LAfter) - Int64(LBefore));
end;

procedure TCatalogReader.Execute;
var
  LAt: Integer;
  LCaptured: INyxPascalFileCatalog;
begin
  try
    for LAt := 1 to 256 do
    begin
      LCaptured := CaptureNyxPascalFiles(Source);

      if (LCaptured <> Source) or (LCaptured.TextAt(0) <> Source.TextAt(0)) then
        raise Exception.Create('Concurrent capture lost its immutable input owner');
      Inc(Iterations);
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure ConcurrentReaders;
var
  LFiles: INyxPascalFileCatalog;
  LReaders: array of TCatalogReader;
  LAt: Integer;
begin
  LFiles := Fixture(32);
  SetLength(LReaders, 4);
  try
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt] := TCatalogReader.Create(True);
      LReaders[LAt].FreeOnTerminate := False;
      LReaders[LAt].Source := LFiles;
      LReaders[LAt].Start;
    end;
    for LAt := 0 to High(LReaders) do
    begin
      LReaders[LAt].WaitFor;
      Check((LReaders[LAt].Iterations = 256) and (LReaders[LAt].ErrorText = ''),
        'Concurrent immutable catalog captures complete independently: ' + LReaders[LAt].ErrorText);
    end;
  finally
    for LAt := 0 to High(LReaders) do
    begin

      if LReaders[LAt] <> nil then
      begin
        LReaders[LAt].WaitFor;
        LReaders[LAt].Free;
      end;
    end;
  end;
  WriteLn('native concurrent catalog captures / 1024 joined');
end;
{$endif}

begin
  try
    GMeasureOnly := False;
    {$ifndef PAS2JS}
    Measurement;

    if ParamCount = 1 then GMeasureOnly := ParamStr(1) = '--measure';
    {$endif}

    if not GMeasureOnly then
    begin
      OwnershipAndLifetime;
      AlternateInputs;
      Refusals;
      TranslationUsesCurrentInputs;
      {$ifndef PAS2JS}ConcurrentReaders;{$endif}
      WriteLn('PASS ', GChecks, ' immutable input catalog lease checks');
    end;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
