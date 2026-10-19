{ Current project input preparation must remain byte/source exact while one
  unpublished overlay replaces repeated persistent catalog construction. }
program nyx_pascal_project_overlay_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses SysUtils, {$ifndef PAS2JS}Classes,{$endif} nyx.text, nyx.bytes, nyx.scheduler,
  nyx.pascal.work, nyx.pascal.lexing, nyx.pascal.encoding, nyx.pascal.files, nyx.pascal.context,
  nyx.pascal.files.project, nyx.pascal.project, nyx.pascal.sources,
  nyx.pascal.drafts, nyx.pascal.names;

type
  { Mutable external packet fields acquire owned byte/view data only on capture.
    A getter may fail late, after earlier packets have reached the candidate. }
  TPacketSpy = class(TInterfacedObject, INyxPascalFilePacket)
  public
    Name: TNyxPascalPhysicalFile;
    Bytes: INyxPascalSourceBytes;
    View: INyxPascalSourceView;
    Fail: Boolean;
    function FileReference: TNyxPascalPhysicalFile;
    function Original: INyxPascalSourceBytes;
    function Decoded: INyxPascalSourceView;
  end;
  { Caller-owned cancellation is detached from every published source result. }
  TStop = class(TInterfacedObject, INyxCancellationScope)
  public
    Stopped: Boolean;
    DropInputs: Boolean;
    procedure Cancel;
    function GetCancelled: Boolean;
  end;
  { Class-owned dynamic interface vector avoids managed interfaces in records
    and static arrays. Count/getters can deliberately fail or cancel. Generated
    names reuse one immutable byte value for exact count/byte-limit checks. }
  TPackets = class(TInterfacedObject, INyxPascalFilePackets)
  public
    Items: array of INyxPascalFilePacket;
    Reads, ReportedCount, FailAt, CancelAt: Integer;
    Generated: Boolean;
    Shared: INyxPascalSourceBytes;
    Cancellation: INyxCancellationScope;
    constructor Create;
    function Count: Integer;
    function Packet(AIndex: Integer): INyxPascalFilePacket;
  end;
  {$ifndef PAS2JS}
  { Each reader constructs its own catalog and scope from shared immutable seed
    and project snapshots. Main joins before releasing leases or reading errors. }
  TOverlayReader = class(TThread)
  protected
    procedure Execute; override;
  public
    Seed: INyxPascalFileCatalog;
    Sources: INyxPascalSourceSnapshot;
    ErrorText: String;
  end;
  {$endif}

var
  GChecks: Integer;
  GBaseInput: INyxPascalFileCatalog;
  GPacketsInput: INyxPascalFilePackets;
  GScopeInput: INyxCancellationScope;
  GBytesInput: INyxPascalSourceBytes;
  GViewInput: INyxPascalSourceView;
  GFileInput: TNyxPascalPhysicalFile;

procedure Check(AValue: Boolean; const AReason: String);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AReason);
end;

function TPacketSpy.FileReference: TNyxPascalPhysicalFile;
begin Result := Name; end;
function TPacketSpy.Original: INyxPascalSourceBytes;
begin

  if Fail then raise Exception.Create('Deliberate packet callback failure');
  Result := Bytes;
end;
function TPacketSpy.Decoded: INyxPascalSourceView;
begin Result := View; end;
procedure TStop.Cancel;
begin Stopped := True; end;
function TStop.GetCancelled: Boolean;
begin

  if DropInputs then
  begin
    DropInputs := False;
    GBaseInput := nil;
    GPacketsInput := nil;
    GScopeInput := nil;
    GBytesInput := nil;
    GViewInput := nil;
    GFileInput := NyxPascalPhysicalFile('changed-by-callback.inc');
  end;
  Result := Stopped;
end;

constructor TPackets.Create;
begin
  inherited Create;
  ReportedCount := -1;
  FailAt := -1;
  CancelAt := -1;
end;

function TPackets.Count: Integer;
begin

  if ReportedCount <> -1 then Result := ReportedCount else Result := Length(Items);
end;

function TPackets.Packet(AIndex: Integer): INyxPascalFilePacket;
begin
  Inc(Reads);

  if AIndex = FailAt then raise Exception.Create('Deliberate packet producer failure');

  if (AIndex = CancelAt) and (Cancellation <> nil) then Cancellation.Cancel;

  if Generated then
    Result := NewNyxPascalFilePacket(NyxPascalPhysicalFile('file' + IntToStr(AIndex) + '.inc'), Shared)
  else
    Result := Items[AIndex];
end;

function TextPacket(const AFile, AText: TNyxText): INyxPascalFilePacket;
var
  LView: INyxPascalSourceView;
begin
  LView := NyxPascalSourceViewFromLexicalInput(NewNyxPascalLexicalInput(AText));
  Result := NewNyxPascalFilePacket(NyxPascalPhysicalFile(AFile), LView.Original, LView);
end;

function Seed(ACase: TNyxPascalFileCase = pfcExact): INyxPascalFileCatalog;
begin
  Result := NewNyxPascalFileCatalog(ACase).WithIncludeDirectory('sdk')
    .WithIncludeDirectory('').WithFile(NyxPascalPhysicalFile('sdk/types.pas'), 'unit Types; interface implementation end.')
    .WithFile(NyxPascalPhysicalFile('main.pas'), 'old main')
    .WithFile(NyxPascalPhysicalFile('first.inc'), 'old first');
end;

procedure OverlayAndOwnership;
var
  LSeed, LCurrent, LExpected, LAlias: INyxPascalFileCatalog;
  LReader: TPackets;
  LPackets: INyxPascalFilePackets;
  LByteFiles: INyxPascalByteFileCatalog;
  LSpy: TPacketSpy;
  LInput: INyxPascalFilePacket;
  LAt: Integer;
begin
  LSeed := Seed;
  LReader := TPackets.Create;
  LPackets := LReader;
  SetLength(LReader.Items, 3);
  LReader.Items[0] := TextPacket('main.pas', 'current main');
  LReader.Items[1] := TextPacket('first.inc', 'current first');
  LReader.Items[2] := TextPacket('next.inc', 'Memo ''😀'';');
  LExpected := LSeed;
  for LAt := 0 to 2 do
  begin
    Supports(LExpected, INyxPascalByteFileCatalog, LByteFiles);
    LInput := LReader.Items[LAt];
    LExpected := LByteFiles.WithByteSource(LInput.FileReference, LInput.Original, LInput.Decoded);
  end;
  LCurrent := OverlayNyxPascalFilePackets(LSeed, LPackets);
  Check(LCurrent.SameFiles(LExpected), 'Single-clone overlay equals independent sequential byte-source updates');
  Check((LCurrent.Count = 4) and (LCurrent.DirectoryCount = 2) and (LCurrent.DirectoryAt(0) = TNyxText('sdk')),
    'Unused reserved rows never escape and include search order remains exact');
  Check((LSeed.TextAt(1) = TNyxText('old main')) and
    (NyxPascalFileCatalogIdentity(LCurrent) <> NyxPascalFileCatalogIdentity(LSeed)), 'Seed stays immutable and a changed catalog has its own identity');
  Check(NyxPascalSourceLexicalInput(NyxPascalSourceViewAt(LCurrent, 3)) =
    NyxPascalSourceLexicalInput(LReader.Items[2].Decoded), 'Exact immutable per-buffer text ownership survives the overlay');
  LAlias := LSeed;
  LAlias := OverlayNyxPascalFilePackets(LAlias, LPackets);
  Check(LAlias.SameFiles(LCurrent), 'Aliased input/output retains the seed throughout construction');
  LSpy := TPacketSpy.Create;
  LInput := LSpy;
  LSpy.Name := NyxPascalPhysicalFile('external.inc');
  LSpy.View := LReader.Items[2].Decoded;
  LSpy.Bytes := LSpy.View.Original;
  SetLength(LReader.Items, 1);
  LReader.Items[0] := LInput;
  LCurrent := OverlayNyxPascalFilePackets(LSeed, LPackets);
  LSpy.Name := NyxPascalPhysicalFile('changed.inc');
  LSpy.Bytes := nil;
  LSpy.View := nil;
  LPackets := nil;
  LInput := nil;
  Check((LCurrent.FileAt(3).Name = TNyxText('external.inc')) and
    (LCurrent.TextAt(3) = TNyxText('Memo ''😀'';')), 'Later alternate packet mutation and producer retirement cannot change captured data');
  LReader := TPackets.Create;
  LPackets := LReader;
  Check(OverlayNyxPascalFilePackets(LSeed, LPackets) = LSeed, 'Empty overlay retains the exact already immutable seed');
end;

procedure FailuresAndWork;
var
  LSeed, LAttempt, LRetained: INyxPascalFileCatalog;
  LReader: TPackets;
  LPackets: INyxPascalFilePackets;
  LBudget, LParent: INyxPascalAnalysisBudget;
  LScope: INyxCancellationScope;
  LStop: TStop;
  LAt, LFit: Integer;
  LRefused: Boolean;
begin
  LSeed := Seed;
  LReader := TPackets.Create;
  LPackets := LReader;
  SetLength(LReader.Items, 3);
  LReader.Items[0] := TextPacket('main.pas', 'new main');
  LReader.Items[1] := TextPacket('first.inc', 'new first');
  LReader.Items[2] := TextPacket('next.inc', 'new next');
  LFit := -1;
  for LAt := 1 to 100 do
  begin
    LAttempt := nil;
    LBudget := NewNyxPascalAnalysisBudget(LAt);
    try
      try
        LAttempt := OverlayNyxPascalFilePackets(LSeed, LPackets, LBudget);
        Check(LAttempt.Count = 4, 'First sufficient preparation allowance publishes exactly the complete catalog');
        LRetained := LAttempt;
        LFit := LAt;
      except
        on ENyxPascalAnalysisWorkLimit do
          Check((LAttempt = nil) and (LBudget.Receipt.Total <= LAt), 'Every smaller preparation allowance refuses without a partial result');
      end;
    finally
      LBudget.Retire;
    end;

    if LFit > 0 then Break;
  end;
  Check(LFit = 9, 'Actual work charges one header, three copied files, two directories and three packets');
  WriteLn('project overlay work / first fit=', LFit, ' every smaller allowance refused');
  LParent := NewNyxPascalAnalysisBudget(2);
  LBudget := NewNyxPascalAnalysisBudget(100, LParent);
  LAttempt := nil;
  LRefused := False;
  try
    try
      LAttempt := OverlayNyxPascalFilePackets(LSeed, LPackets, LBudget);
    except
      on ENyxPascalAnalysisWorkLimit do LRefused := True;
    end;
    Check(LRefused and (LAttempt = nil) and (LParent.Receipt.Total <= 2), 'A child budget cannot bypass its parent preparation allowance');
  finally
    LBudget.Retire;
    LParent.Retire;
  end;
  for LAt := 0 to 2 do
  begin
    LReader.FailAt := LAt;
    LAttempt := LSeed;
    LRefused := False;
    try
      LAttempt := OverlayNyxPascalFilePackets(LAttempt, LPackets);
    except
      on E: Exception do LRefused := E.Message = 'Deliberate packet producer failure';
    end;
    Check(LRefused and (LAttempt = LSeed), 'Each late producer failure preserves an aliased caller result');
  end;
  LReader.FailAt := -1;
  LStop := TStop.Create;
  LScope := LStop;
  LReader.Cancellation := LScope;
  for LAt := 0 to 2 do
  begin
    LStop.Stopped := False;
    LReader.CancelAt := LAt;
    LAttempt := nil;
    LRefused := False;
    try
      LAttempt := OverlayNyxPascalFilePackets(LSeed, LPackets, LScope);
    except
      on ENyxPascalAnalysisCancelled do LRefused := True;
    end;
    Check(LRefused and (LAttempt = nil), 'Cancellation within every packet callback, including the last, refuses atomically');
  end;
  Check(LRetained.TextAt(1) = TNyxText('new main'), 'Retiring budgets/cancellation cannot retire a completed immutable source');
end;

procedure LimitsAndAliases;
var
  LReader: TPackets;
  LPackets: INyxPascalFilePackets;
  LSeed, LAttempt: INyxPascalFileCatalog;
  LBytes: TNyxBytes;
  LAt: Integer;
  LRefused: Boolean;
begin
  LSeed := Seed(pfcASCIIInsensitive);
  LReader := TPackets.Create;
  LPackets := LReader;
  SetLength(LReader.Items, 2);
  LReader.Items[0] := TextPacket('main.pas', 'valid replacement');
  LReader.Items[1] := TextPacket('MAIN.PAS', 'case alias');
  LAttempt := nil;
  LRefused := False;
  try
    LAttempt := OverlayNyxPascalFilePackets(LSeed, LPackets);
  except
    on ENyxPascalContext do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil), 'Case-folded duplicate packet names cannot publish an earlier replacement');
  for LAt := 0 to 1 do
  begin
    LReader.ReportedCount := -2;

    if LAt = 1 then LReader.ReportedCount := 4097;
    LReader.Reads := 0;
    LRefused := False;
    try
      LAttempt := OverlayNyxPascalFilePackets(LSeed, LPackets);
    except
      on ENyxPascalContext do LRefused := True;
    end;
    Check(LRefused and (LReader.Reads = 0), 'Invalid packet counts refuse before source callbacks');
  end;
  LReader.Generated := True;
  SetLength(LBytes, 1);
  LBytes[0] := 255;
  LReader.Shared := NewNyxPascalSourceBytes(LBytes, pboCapturedBytes);
  LReader.ReportedCount := 4096;
  LSeed := OverlayNyxPascalFilePackets(NewNyxPascalFileCatalog(pfcExact), LPackets);
  Check(LSeed.Count = 4096, 'The unchanged 4096-file ceiling is fully usable');
  LReader.Generated := False;
  LReader.ReportedCount := 1;
  LReader.Items[0] := TextPacket('file0.inc', 'replacement at capacity');
  LAttempt := OverlayNyxPascalFilePackets(LSeed, LPackets);
  Check((LAttempt.Count = 4096) and (LAttempt.TextAt(0) = TNyxText('replacement at capacity')),
    'Replacement at capacity does not confuse reserved slots with published membership');
  LReader.Items[0] := TextPacket('extra.inc', 'one too many');
  LRefused := False;
  LAttempt := nil;
  try
    LAttempt := OverlayNyxPascalFilePackets(LSeed, LPackets);
  except
    on ENyxPascalContext do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil), 'Appending above the original file limit refuses atomically');
  SetLength(LBytes, 4 * 1024 * 1024);
  LReader.Shared := NewNyxPascalSourceBytes(LBytes, pboCapturedBytes);
  LReader.Generated := True;
  LReader.ReportedCount := 64;
  LSeed := OverlayNyxPascalFilePackets(NewNyxPascalFileCatalog(pfcExact), LPackets);
  Check(LSeed.ByteCount = 256 * 1024 * 1024, 'Shared immutable bytes still charge every logical file at the exact 256-MiB ceiling');
  LReader.ReportedCount := 65;
  LRefused := False;
  LAttempt := nil;
  try
    LAttempt := OverlayNyxPascalFilePackets(NewNyxPascalFileCatalog(pfcExact), LPackets);
  except
    on ENyxPascalContext do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil), 'Shared physical storage cannot bypass aggregate byte accounting');
end;

procedure CallbackAndPacketRefusal;
var
  LReader: TPackets;
  LPackets: INyxPascalFilePackets;
  LStop: TStop;
  LPacket, LBad: INyxPascalFilePacket;
  LSpy: TPacketSpy;
  LSeed, LAttempt: INyxPascalFileCatalog;
  LRefused: Boolean;
  LBytes: TNyxBytes;
begin
  LPacket := TextPacket('held.inc', 'unchanged text');
  GFileInput := LPacket.FileReference;
  GBytesInput := LPacket.Original;
  GViewInput := LPacket.Decoded;
  LStop := TStop.Create;
  GScopeInput := LStop;
  LStop.DropInputs := True;
  LBad := NewNyxPascalFilePacket(GFileInput, GBytesInput, GViewInput, GScopeInput);
  Check((LBad.FileReference.Name = TNyxText('held.inc')) and
    (LBad.Decoded.Text = TNyxText('unchanged text')) and (GScopeInput = nil),
    'Packet construction leases record/byte/view/scope inputs before a cancellation callback can retire caller variables');
  LReader := TPackets.Create;
  LPackets := LReader;
  SetLength(LReader.Items, 1);
  LReader.Items[0] := LPacket;
  GBaseInput := Seed;
  GPacketsInput := LPackets;
  LStop := TStop.Create;
  GScopeInput := LStop;
  LStop.DropInputs := True;
  GBaseInput := OverlayNyxPascalFilePackets(GBaseInput, GPacketsInput, GScopeInput);
  Check((GBaseInput.Count = 4) and (GBaseInput.TextAt(3) = TNyxText('unchanged text')),
    'An aliased overlay survives reentrant retirement of every caller interface input');
  GBaseInput := nil;
  LSpy := TPacketSpy.Create;
  LReader.Items[0] := TextPacket('new.inc', 'earlier admitted update');
  SetLength(LReader.Items, 2);
  LReader.Items[1] := LSpy;
  LSpy.Name := NyxPascalPhysicalFile('late.inc');
  LSpy.Fail := True;
  LSeed := Seed;
  LAttempt := LSeed;
  LRefused := False;
  try
    LAttempt := OverlayNyxPascalFilePackets(LAttempt, LPackets);
  except
    on E: Exception do LRefused := E.Message = 'Deliberate packet callback failure';
  end;
  Check(LRefused and (LAttempt = LSeed), 'A late alternate packet getter cannot expose an earlier partial catalog');
  LBad := nil;
  LRefused := False;
  try
    LBad := NewNyxPascalFilePacket(NyxPascalPhysicalFile('mismatch.inc'),
      TextPacket('other.inc', 'different bytes').Original, LPacket.Decoded);
  except
    on ENyxPascalContext do LRefused := True;
  end;
  Check(LRefused and (LBad = nil), 'A byte/view mismatch cannot create a sealed packet');
  LReader.Cancellation := NewNyxCancellationScope;
  LReader.Cancellation.Cancel;
  LReader.Reads := 0;
  LAttempt := nil;
  LRefused := False;
  try
    LAttempt := OverlayNyxPascalFilePackets(LSeed, LPackets, LReader.Cancellation);
  except
    on ENyxPascalAnalysisCancelled do LRefused := True;
  end;
  Check(LRefused and (LAttempt = nil) and (LReader.Reads = 0), 'Entry cancellation invokes no packet callbacks');
  SetLength(LBytes, 4 * 1024 * 1024 + 1);
  LBad := nil;
  LRefused := False;
  try
    LBad := NewNyxPascalFilePacket(NyxPascalPhysicalFile('too-large.inc'),
      NewNyxPascalSourceBytes(LBytes, pboCapturedBytes));
  except
    on ENyxPascalContext do LRefused := True;
    on ENyxPascalSourceDecodeLimit do LRefused := True;
  end;
  Check(LRefused and (LBad = nil), 'Byte/packet capture cannot weaken the original four-MiB physical-source limit');
end;

function ProjectSources: INyxPascalSourceSnapshot;
begin
  Result := CaptureNyxPascalSources(NewNyxPascalProject(NyxPascalUnit('Main'),
    'unit Main; interface {$i first.inc} implementation end.')
    .WithInclude(NyxPascalInclude('first.inc'), 'const Value=42;')
    .WithUnit(NyxPascalUnit('Other'), 'unit Other; interface implementation end.'));
end;

procedure ProjectPath;
var
  LSources, LChanged, LAlias: INyxPascalSourceSnapshot;
  LSeed, LCurrent, LExpected: INyxPascalFileCatalog;
  LAt: Integer;
  LFile: TNyxPascalSourceRef;
begin
  LSources := ProjectSources;
  Check(CaptureNyxPascalSources(LSources) = LSources, 'Exactly owned immutable source snapshots lease without recopying or revalidation');
  LAlias := LSources;
  LAlias := CaptureNyxPascalSources(LAlias);
  Check(LAlias = LSources, 'Aliased immutable snapshot capture retains the original owner');
  LChanged := LSources.WithBuffer(TNyxPascalSourceRef.Main, 'unfinished main');
  Check((LChanged <> LSources) and (LSources.Text(TNyxPascalSourceRef.Main) <> TNyxText('unfinished main')),
    'Editing retains its own new owner and leaves earlier captured source unchanged');
  LSeed := Seed;
  LExpected := LSeed;
  for LAt := 0 to LSources.Count - 1 do
  begin
    LFile := LSources.FileReference(LAt);
    LExpected := LExpected.WithFile(NyxPascalProjectPhysicalFile(LSources, LFile), LSources.Text(LFile));
  end;
  LCurrent := WithNyxPascalProjectFiles(LSeed, LSources);
  Check(LCurrent.SameFiles(LExpected), 'Actual project overlay equals its independent sequential authoring-UTF8 construction');
  for LAt := 0 to LSources.Count - 1 do
  begin
    LFile := LSources.FileReference(LAt);
    Check(NyxPascalSourceLexicalInput(NyxPascalSourceViewAt(LCurrent,
      LCurrent.Find(NyxPascalProjectPhysicalFile(LSources, LFile)))) = NyxPascalSourcesLexicalInput(LSources, LFile),
      'Every project packet retains its own exact immutable buffer owner');
  end;
end;

{$ifndef PAS2JS}
procedure TOverlayReader.Execute;
var
  LAt: Integer;
  LCurrent: INyxPascalFileCatalog;
begin
  try
    for LAt := 1 to 16 do
    begin
      LCurrent := WithNyxPascalProjectFiles(Seed, Sources);

      if LCurrent.TextAt(LCurrent.Find(NyxPascalPhysicalFile('Main.pas'))) <> Sources.Text(TNyxPascalSourceRef.Main) then
        raise Exception.Create('Concurrent project text changed');
    end;
  except
    on E: Exception do ErrorText := E.Message;
  end;
end;

procedure ConcurrentReaders;
var
  LReaders: array[0..3] of TOverlayReader;
  LSeed: INyxPascalFileCatalog;
  LSources: INyxPascalSourceSnapshot;
  LAt: Integer;
begin
  LSeed := Seed;
  LSources := ProjectSources;
  for LAt := 0 to High(LReaders) do
  begin
    LReaders[LAt] := TOverlayReader.Create(True);
    LReaders[LAt].Seed := LSeed;
    LReaders[LAt].Sources := LSources;
  end;
  try
    for LAt := 0 to High(LReaders) do LReaders[LAt].Start;
    for LAt := 0 to High(LReaders) do LReaders[LAt].WaitFor;
    for LAt := 0 to High(LReaders) do Check(LReaders[LAt].ErrorText = '', 'Joined overlay reader: ' + LReaders[LAt].ErrorText);
  finally
    for LAt := 0 to High(LReaders) do LReaders[LAt].Free;
  end;
  WriteLn('native concurrent project overlays / 64 joined');
end;
{$endif}

begin
  try
    OverlayAndOwnership;
    FailuresAndWork;
    LimitsAndAliases;
    CallbackAndPacketRefusal;
    ProjectPath;
    {$ifndef PAS2JS}ConcurrentReaders;{$endif}
    WriteLn('PASS ', GChecks, ' atomic project preparation checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', GChecks, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
