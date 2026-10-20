{ Intrinsic/alias metadata shapes; synthetic graphs only. MIT, see LICENSE. }
program nyx_documentation_shapes_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.scheduler, nyx.schema, nyx.catalog,
  nyx.callbacks, nyx.documentation, nyx.documentation.registration,
  nyx.documentation.registry {$ifdef PAS2JS}, Web{$endif};

type
  { Mutable alternate metadata tests detached capture. Every fluent method
    refuses: an inspector must copy facts instead of calling producer builders. }
  TAlternateType = class(TInterfacedObject, INyxDocumentationType)
  public
    Value: INyxDocumentationType;
    destructor Destroy; override;
    function Identity: TNyxDocumentationRef;
    function Kind: TNyxDocumentationTypeKind;
    function Summary: TNyxText;
    function Evidence: TNyxDocumentationEvidence;
    function AliasTarget: TNyxDocumentationRef; virtual;
    function BaseCount: Integer;
    function BaseAt(AIndex: Integer): TNyxDocumentationRef;
    function MemberCount: Integer;
    function MemberAt(AIndex: Integer): INyxDocumentationMember;
    function ExampleCount: Integer;
    function ExampleAt(AIndex: Integer): TNyxDocumentationExample;
    function RelatedCount: Integer;
    function RelatedAt(AIndex: Integer): TNyxDocumentationRef;
    function WithAlias(const ATarget: TNyxDocumentationRef): INyxDocumentationType;
    function WithBase(const ABase: TNyxDocumentationRef): INyxDocumentationType;
    function WithMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
    function WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
    function WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
  end;
  { A family supplier can change after capture without changing the owned fact. }
  TAlternateIntrinsic = class(TAlternateType, INyxDocumentationIntrinsic)
  public
    CurrentFamily: TNyxDocumentationIntrinsicFamily;
    function Family: TNyxDocumentationIntrinsicFamily;
  end;
  { Branch getters may cancel or overclaim before any row allocation/read.
    HasTarget/TargetFor deliberately refuse; capture reconstructs those queries
    from admitted rows instead of trusting alternate derived answers. }
  TAlternateAlias = class(TAlternateType, INyxDocumentationAliasTargets)
  public
    Branches: TNyxDocumentationAliasTargetVector;
    ClaimedCount, Reads: Integer;
    ClaimedCommon: TNyxDocumentationRef;
    Cancellation: INyxCancellationScope;
    function AliasTarget: TNyxDocumentationRef; override;
    function TargetCount: Integer;
    function TargetAt(AIndex: Integer): TNyxDocumentationAliasTarget;
    function HasTarget(APlatform: TNyxPlatform): Boolean;
    function TargetFor(APlatform: TNyxPlatform): TNyxDocumentationRef;
  end;
  { Immutable synthetic catalog exercises shape integration in the existing
    index, without manufacturing descriptors for any actual System/core type or
    replacing the real extension closure tested by the separate registry suite. }
  TShapeCatalog = class(TInterfacedObject, INyxCatalogSnapshot, INyxCatalogDocumentation)
  public
    Registrations: TNyxDocumentationRegistrations;
    CapturedIdentity: Integer;
    function Identity: Integer;
    function Revision: Integer;
    function Count: Integer;
    function ItemAt(AIndex: Integer): TNyxComponentInfo;
    function DocumentationAt(AIndex: Integer): INyxDocumentationRegistration;
  end;

var
  GChecks, GProducersDestroyed, GCatalogIdentity: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AMessage);
end;

function ShapeRef(const AName: TNyxText): TNyxDocumentationRef;
begin
  Result := NyxDocumentationType('documentation.shapes', AName);
end;

destructor TAlternateType.Destroy;
begin
  Inc(GProducersDestroyed);
  inherited Destroy;
end;

function TAlternateType.Identity: TNyxDocumentationRef;
begin
  Result := Value.Identity;
end;

function TAlternateType.Kind: TNyxDocumentationTypeKind;
begin
  Result := Value.Kind;
end;

function TAlternateType.Summary: TNyxText;
begin
  Result := Value.Summary;
end;

function TAlternateType.Evidence: TNyxDocumentationEvidence;
begin
  Result := Value.Evidence;
end;

function TAlternateType.AliasTarget: TNyxDocumentationRef;
begin
  Result := Value.AliasTarget;
end;

function TAlternateType.BaseCount: Integer;
begin
  Result := Value.BaseCount;
end;

function TAlternateType.BaseAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Result := Value.BaseAt(AIndex);
end;

function TAlternateType.MemberCount: Integer;
begin
  Result := Value.MemberCount;
end;

function TAlternateType.MemberAt(AIndex: Integer): INyxDocumentationMember;
begin
  Result := Value.MemberAt(AIndex);
end;

function TAlternateType.ExampleCount: Integer;
begin
  Result := Value.ExampleCount;
end;

function TAlternateType.ExampleAt(AIndex: Integer): TNyxDocumentationExample;
begin
  Result := Value.ExampleAt(AIndex);
end;

function TAlternateType.RelatedCount: Integer;
begin
  Result := Value.RelatedCount;
end;

function TAlternateType.RelatedAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Result := Value.RelatedAt(AIndex);
end;

function TAlternateType.WithAlias(const ATarget: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Capture must not call producer WithAlias');
end;

function TAlternateType.WithBase(const ABase: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Capture must not call producer WithBase');
end;

function TAlternateType.WithMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Capture must not call producer WithMember');
end;

function TAlternateType.WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Capture must not call producer WithExample');
end;

function TAlternateType.WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Capture must not call producer WithRelated');
end;

function TAlternateIntrinsic.Family: TNyxDocumentationIntrinsicFamily;
begin
  Result := CurrentFamily;
end;

function TAlternateAlias.AliasTarget: TNyxDocumentationRef;
begin
  Result := ClaimedCommon;
end;

function TAlternateAlias.TargetCount: Integer;
begin
  Result := ClaimedCount;
end;

function TAlternateAlias.TargetAt(AIndex: Integer): TNyxDocumentationAliasTarget;
begin
  Inc(Reads);

  if Cancellation <> nil then
    Cancellation.Cancel;
  Result := Branches[AIndex];
end;

function TAlternateAlias.HasTarget(APlatform: TNyxPlatform): Boolean;
begin
  Result := False;
  raise Exception.Create('Capture must derive HasTarget from admitted branch rows');
end;

function TAlternateAlias.TargetFor(APlatform: TNyxPlatform): TNyxDocumentationRef;
begin
  Result := Default(TNyxDocumentationRef);
  raise Exception.Create('Capture must derive TargetFor from admitted branch rows');
end;

function TShapeCatalog.Identity: Integer;
begin
  Result := CapturedIdentity;
end;

function TShapeCatalog.Revision: Integer;
begin
  Result := 1;
end;

function TShapeCatalog.Count: Integer;
begin
  Result := Length(Registrations);
end;

function TShapeCatalog.ItemAt(AIndex: Integer): TNyxComponentInfo;
begin

  if (AIndex < 0) or (AIndex >= Count) then
    raise Exception.Create('Shape catalog ordinal');
  Result := Default(TNyxComponentInfo);
  Result.Kind := 'documentation-shapes-' + IntToStr(AIndex);
  Result.Title := 'Synthetic metadata shape';
  Result.Category := 'Custom';
end;

function TShapeCatalog.DocumentationAt(AIndex: Integer): INyxDocumentationRegistration;
begin
  ItemAt(AIndex);
  Result := Registrations[AIndex];
end;

function ShapeCatalog(const AFirst: INyxDocumentationRegistration;
  const ASecond: INyxDocumentationRegistration = nil): INyxCatalogSnapshot;
var
  LOwner: TShapeCatalog;
  LLease: INyxCatalogSnapshot;
begin
  LOwner := TShapeCatalog.Create;
  LLease := LOwner;
  Inc(GCatalogIdentity);
  LOwner.CapturedIdentity := GCatalogIdentity;
  SetLength(LOwner.Registrations, 1 + Ord(ASecond <> nil));
  LOwner.Registrations[0] := AFirst;

  if ASecond <> nil then
    LOwner.Registrations[1] := ASecond;
  Result := LLease;
end;

{ Returning across this lexical boundary retires all compiler-held supplier
  temporaries, so the following lifetime assertion checks the actual owner. }
function FreezeIntrinsicSupplier: INyxDocumentationType;
var
  LOwner: TAlternateIntrinsic;
  LLease: INyxDocumentationType;
begin
  LOwner := TAlternateIntrinsic.Create;
  LLease := LOwner;
  LOwner.Value := NewNyxDocumentationIntrinsic(ShapeRef('Captured'), ndifString, 'Captured family.');
  LOwner.CurrentFamily := ndifString;
  Result := CaptureNyxDocumentationType(LLease);
  LOwner.CurrentFamily := ndifInteger;
end;

{ Synthetic definitions prove graph algorithms, not the identity/encoding of a
  compiler's System declarations. A union of BranchA/BranchB edges is cyclic,
  but each actual target has an acyclic path to a string-family leaf. }
function ShapeBundle(AChange: Integer = 0): INyxDocumentationRegistration;
var
  LDeclarations: TNyxDocumentationDeclarations;
  LUnit: TNyxDocumentationRef;
  LFamily: TNyxDocumentationIntrinsicFamily;
begin
  LUnit := NyxDocumentationUnit('documentation.shapes');
  SetLength(LDeclarations, 7);
  LFamily := ndifString;

  if AChange = 2 then
    LFamily := ndifInteger;
  LDeclarations[0] := NewNyxDocumentationIntrinsic(ShapeRef('NativeText'), LFamily, 'Synthetic native family.');
  LDeclarations[1] := NewNyxDocumentationIntrinsic(ShapeRef('BrowserText'), ndifString, 'Synthetic browser family.');
  LDeclarations[2] := NewNyxDocumentationTargetAlias(ShapeRef('Text'), 'Keep both actual targets.', [
    NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('NativeText')),
    NyxDocumentationAliasTarget(npfBrowser, ShapeRef('BrowserText'))]);

  if AChange = 3 then
    LDeclarations[2] := NewNyxDocumentationTargetAlias(ShapeRef('Text'), 'Keep both actual targets.', [
      NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('BrowserText')),
      NyxDocumentationAliasTarget(npfBrowser, ShapeRef('NativeText'))]);

  if AChange = 4 then
    LDeclarations[2] := NewNyxDocumentationTargetAlias(ShapeRef('Text'), 'Keep both actual targets.', [
      NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('Missing')),
      NyxDocumentationAliasTarget(npfBrowser, ShapeRef('BrowserText'))]);
  LDeclarations[3] := NewNyxDocumentationTargetAlias(ShapeRef('SharedText'), 'One actual common target.', [
    NyxDocumentationAliasTarget(npfAny, ShapeRef('NativeText'))]);
  LDeclarations[4] := NewNyxDocumentationTargetAlias(ShapeRef('BranchA'), 'Different edges per target.', [
    NyxDocumentationAliasTarget(npfBrowser, ShapeRef('BranchB')),
    NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('NativeText'))]);
  LDeclarations[5] := NewNyxDocumentationTargetAlias(ShapeRef('BranchB'), 'Different edges per target.', [
    NyxDocumentationAliasTarget(npfBrowser, ShapeRef('BrowserText')),
    NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('BranchA'))]);

  if AChange = 1 then
    LDeclarations[5] := NewNyxDocumentationTargetAlias(ShapeRef('BranchB'), 'Different edges per target.', [
      NyxDocumentationAliasTarget(npfBrowser, ShapeRef('BranchA')),
      NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('BranchA'))]);
  LDeclarations[6] := NewNyxDocumentationType(LUnit, ndtUnit, 'Synthetic metadata shape owner.')
    .WithRelated(ShapeRef('Text'));
  Result := NewNyxDocumentationRegistration(LUnit, ShapeRef('Text'), LDeclarations,
    [ndplatformNative, ndplatformBrowser]);
end;

function PartialBundle(APlatforms: TNyxDocumentationPlatforms): INyxDocumentationRegistration;
var
  LDeclarations: TNyxDocumentationDeclarations;
begin
  SetLength(LDeclarations, 2);
  LDeclarations[0] := NewNyxDocumentationIntrinsic(ShapeRef('NativeText'), ndifString, 'Native family.');
  LDeclarations[1] := NewNyxDocumentationTargetAlias(ShapeRef('Text'), 'One declared branch.', [
    NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('NativeText'))]);
  Result := NewNyxDocumentationRegistration(NyxDocumentationUnit('documentation.shapes'),
    ShapeRef('Text'), LDeclarations, APlatforms);
end;

{ An inherited alias must have a compatible base on every explicitly available
  projection. The same declarations can truthfully serve a native-only class;
  a browser interface must never be guessed into a native class base. }
function InheritanceBundle(APlatforms: TNyxDocumentationPlatforms): INyxDocumentationRegistration;
var
  LDeclarations: TNyxDocumentationDeclarations;
  LBranches: TNyxDocumentationAliasTargetVector;
begin
  SetLength(LDeclarations, 4);
  LDeclarations[0] := NewNyxDocumentationType(ShapeRef('NativeBase'), ndtClass, 'Synthetic native class.');
  LDeclarations[1] := NewNyxDocumentationType(ShapeRef('BrowserBase'), ndtInterface, 'Synthetic browser interface.');
  SetLength(LBranches, 2);
  LBranches[0] := NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('NativeBase'));
  LBranches[1] := NyxDocumentationAliasTarget(npfBrowser, ShapeRef('BrowserBase'));
  LDeclarations[2] := NewNyxDocumentationTargetAlias(ShapeRef('Base'), 'Actual base by projection.', LBranches);
  LDeclarations[3] := NewNyxDocumentationType(ShapeRef('Derived'), ndtClass, 'Native inherited API.')
    .WithBase(ShapeRef('Base'));
  Result := NewNyxDocumentationRegistration(NyxDocumentationUnit('documentation.shapes'),
    ShapeRef('Derived'), LDeclarations, APlatforms);
end;

procedure RefuseIndex(ARegistry: TNyxDocumentationRegistry;
  const ASchemas: INyxSchemaSnapshot; const ACallbacks: INyxCallbackSnapshot;
  const ACatalog: INyxCatalogSnapshot; const AFragment: TNyxText;
  const ACancellation: INyxCancellationScope = nil);
var
  LBefore: INyxDocumentationIndex;
  LVersion: Integer;
  LRefused: Boolean;
begin
  LBefore := ARegistry.Current;
  LVersion := ARegistry.Version;
  LRefused := False;
  try
    ARegistry.Refresh(ASchemas, ACallbacks, ACatalog, LVersion, ACancellation);
  except
    on E: ENyxDocumentation do
      LRefused := (AFragment = '') or (Pos(AFragment, E.Message) > 0);
  end;
  Check(LRefused and (ARegistry.Current = LBefore) and (ARegistry.Version = LVersion),
    'Index refusal retains the exact prior owner/version: ' + AFragment);
end;

procedure Run;
var
  LFamily: TNyxDocumentationIntrinsicFamily;
  LType, LOld, LFrozen, LChanged, LAlias, LPlain: INyxDocumentationType;
  LIntrinsic: INyxDocumentationIntrinsic;
  LTargets: INyxDocumentationAliasTargets;
  LAlternate: TAlternateAlias;
  LAlternateLease: INyxDocumentationType;
  LWrongIntrinsic: TAlternateIntrinsic;
  LWrongLease: INyxDocumentationType;
  LMissing: TAlternateType;
  LMissingLease: INyxDocumentationType;
  LRegistry: TNyxDocumentationRegistry;
  LSchemas: INyxSchemaSnapshot;
  LCallbacks: INyxCallbackSnapshot;
  LIndex, LNext: INyxDocumentationIndex;
  LBundle, LSame: INyxDocumentationRegistration;
  LScope: INyxCancellationScope;
  LAt, LBefore: Integer;
  LRefused: Boolean;
begin
  Check((Ord(ndtUnit) = 6) and (Ord(ndtIntrinsic) = 7), 'Intrinsic appends without changing old type ordinals');
  for LFamily := Low(TNyxDocumentationIntrinsicFamily) to High(TNyxDocumentationIntrinsicFamily) do
  begin
    LType := NewNyxDocumentationIntrinsic(ShapeRef('Family' + IntToStr(Ord(LFamily))), LFamily,
      'A declared family, with no guessed width or encoding.');
    Check(Supports(LType, INyxDocumentationIntrinsic, LIntrinsic) and (LIntrinsic.Family = LFamily),
      'Each explicit intrinsic family has its specialized typed capability');
    LOld := LType;
    LType := LType.WithExample(NyxDocumentationExample('Inert content', '// <script>' + #0#13#10 + 'Ω'))
      .WithRelated(ShapeRef('Related'));
    Check(Supports(LType, INyxDocumentationIntrinsic, LIntrinsic) and (LIntrinsic.Family = LFamily) and
      (LOld.ExampleCount = 0) and (LOld.RelatedCount = 0),
      'Fluent content copies preserve intrinsic specialization and previous readers');
    LFrozen := CaptureNyxDocumentationType(LType);
    Check(Supports(LFrozen, INyxDocumentationIntrinsic, LIntrinsic) and (LIntrinsic.Family = LFamily) and
      (LFrozen.ExampleAt(0).Pascal = LType.ExampleAt(0).Pascal),
      'Capture retains the family, inert Unicode/NUL/newlines and existing aggregate content');
  end;
  LPlain := NewNyxDocumentationType(ShapeRef('Record'), ndtRecord, 'An ordinary declaration.');
  Check(not Supports(LPlain, INyxDocumentationIntrinsic, LIntrinsic) and
    not Supports(LPlain, INyxDocumentationAliasTargets, LTargets),
    'Ordinary declarations expose neither an invented family nor target branches');
  LRefused := False;
  try
    NewNyxDocumentationType(ShapeRef('MissingFamily'), ndtIntrinsic, 'No family supplied.');
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'An intrinsic kind cannot bypass its required family factory');
  LChanged := LType;
  for LAt := LChanged.ExampleCount to NyxDocumentationMaximumExamples - 1 do
    LChanged := LChanged.WithExample(NyxDocumentationExample('Bounded example ' + IntToStr(LAt), '// inert'));
  LRefused := False;
  try
    LChanged.WithExample(NyxDocumentationExample('Excess example', '// inert'));
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused and (LChanged.ExampleCount = NyxDocumentationMaximumExamples) and
    Supports(LChanged, INyxDocumentationIntrinsic, LIntrinsic) and (LType.ExampleCount = 1),
    'Specialized fluent copies preserve the existing example allowance and earlier immutable owner');
  for LAt := 0 to 3 do
  begin
    LRefused := False;
    try
      case LAt of
        0: LType.WithBase(ShapeRef('Other'));
        1: LType.WithAlias(ShapeRef('Other'));
        2: LType.WithMember(NewNyxDocumentationMember(
          NyxDocumentationMember(LType.Identity, 'Invented'), ndmMethod, 'No intrinsic member proof.'));
        3: NewNyxDocumentationIntrinsic(NyxDocumentationUnit('documentation.shapes'), ndifInteger, 'A unit is not intrinsic.');
      end;
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'Intrinsic role combinations refuse ' + IntToStr(LAt));
  end;
  LBefore := GProducersDestroyed;
  LFrozen := FreezeIntrinsicSupplier;
  Check((GProducersDestroyed = LBefore + 1) and
    Supports(LFrozen, INyxDocumentationIntrinsic, LIntrinsic) and (LIntrinsic.Family = ndifString),
    'Frozen intrinsic owns its fact after the mutable producer and all lexical temporaries retire');

  LAlias := NewNyxDocumentationTargetAlias(ShapeRef('Text'), 'Actual branch identities.', [
    NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('NativeText')),
    NyxDocumentationAliasTarget(npfBrowser, ShapeRef('BrowserText'))]);
  Check(Supports(LAlias, INyxDocumentationAliasTargets, LTargets) and
    (LTargets.TargetAt(0).Platform = npfBrowser) and not LAlias.AliasTarget.Defined and
    not LTargets.HasTarget(npfAny), 'Case order is deterministic and differing branches invent no common alias');
  Check(LTargets.TargetFor(npfBrowser).Same(ShapeRef('BrowserText')) and
    LTargets.TargetFor(npfNativeLCL).Same(ShapeRef('NativeText')), 'Each actual projection resolves its own exact target');
  LOld := LAlias;
  LAlias := LAlias.WithExample(NyxDocumentationExample('Inert alias', '// alias example'))
    .WithRelated(ShapeRef('Related'));
  Check(Supports(LAlias, INyxDocumentationAliasTargets, LTargets) and (LTargets.TargetCount = 2) and
    LTargets.TargetFor(npfBrowser).Same(ShapeRef('BrowserText')) and (LOld.ExampleCount = 0),
    'Fluent alias copies retain branches and previous owners');
  LChanged := LAlias.WithAlias(ShapeRef('Shared'));
  Check(Supports(LChanged, INyxDocumentationAliasTargets, LTargets) and (LTargets.TargetCount = 1) and
    (LTargets.TargetAt(0).Platform = npfAny) and LChanged.AliasTarget.Same(ShapeRef('Shared')) and
    not LAlias.AliasTarget.Defined, 'Explicit WithAlias replaces all branches with one actual shared target');
  LChanged := NewNyxDocumentationTargetAlias(ShapeRef('Same'), 'Two explicitly equal branches.', [
    NyxDocumentationAliasTarget(npfBrowser, ShapeRef('Shared')),
    NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('Shared'))]);
  Check(LChanged.AliasTarget.Same(ShapeRef('Shared')) and
    Supports(LChanged, INyxDocumentationAliasTargets, LTargets) and (LTargets.TargetCount = 2),
    'A common target is known when both explicit branches agree without discarding their declarations');
  LChanged := NewNyxDocumentationTargetAlias(ShapeRef('OnlyNative'), 'A genuinely platform-only alias.', [
    NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('NativeText'))]);
  Check(Supports(LChanged, INyxDocumentationAliasTargets, LTargets) and
    LTargets.HasTarget(npfNativeLCL) and not LTargets.HasTarget(npfBrowser) and
    not LTargets.HasTarget(npfAny) and not LChanged.AliasTarget.Defined,
    'One specific branch remains useful without pretending to know another or a universal target');
  LRefused := False;
  try
    LTargets.TargetFor(npfBrowser);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'Missing target selection refuses explicitly');
  for LAt := 0 to 6 do
  begin
    LRefused := False;
    try
      case LAt of
        0: NewNyxDocumentationTargetAlias(ShapeRef('Bad'), 'No branches.', []);
        1: NewNyxDocumentationTargetAlias(ShapeRef('Bad'), 'Repeated branch.', [
          NyxDocumentationAliasTarget(npfBrowser, ShapeRef('NativeText')),
          NyxDocumentationAliasTarget(npfBrowser, ShapeRef('BrowserText'))]);
        2: NewNyxDocumentationTargetAlias(ShapeRef('Bad'), 'Overlapping common branch.', [
          NyxDocumentationAliasTarget(npfAny, ShapeRef('NativeText')),
          NyxDocumentationAliasTarget(npfBrowser, ShapeRef('BrowserText'))]);
        3: NyxDocumentationAliasTarget(npfAny, NyxDocumentationUnit('documentation.shapes'));
        4: NyxDocumentationAliasTarget(npfAny, NyxDocumentationMember(ShapeRef('Record'), 'Value'));
        5: NewNyxDocumentationTargetAlias(ShapeRef('Self'), 'Self alias.', [
          NyxDocumentationAliasTarget(npfBrowser, ShapeRef('Self'))]);
        6: LTargets.TargetAt(-1);
      end;
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'Invalid target shape refuses ' + IntToStr(LAt));
  end;

  LAlternate := TAlternateAlias.Create;
  LAlternateLease := LAlternate;
  LAlternate.Value := LAlias;
  SetLength(LAlternate.Branches, 2);
  LAlternate.Branches[0] := NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('NativeText'));
  LAlternate.Branches[1] := NyxDocumentationAliasTarget(npfBrowser, ShapeRef('BrowserText'));
  LAlternate.ClaimedCount := 2;
  LFrozen := CaptureNyxDocumentationType(LAlternateLease);
  LAlternate.Branches[0] := NyxDocumentationAliasTarget(npfNativeLCL, ShapeRef('Changed'));
  Check(Supports(LFrozen, INyxDocumentationAliasTargets, LTargets) and
    LTargets.TargetFor(npfNativeLCL).Same(ShapeRef('NativeText')) and (LFrozen.ExampleCount = 1),
    'Alternate capture owns branch values and prose without invoking derived/producer fluent methods');
  LAlternate.ClaimedCount := 3;
  LAlternate.Reads := 0;
  LFrozen := nil;
  LRefused := False;
  try
    LFrozen := CaptureNyxDocumentationType(LAlternateLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused and (LFrozen = nil) and (LAlternate.Reads = 0),
    'Overclaimed branch count refuses atomically before any row read');
  LAlternate.ClaimedCount := 2;
  LAlternate.ClaimedCommon := ShapeRef('InventedCommon');
  LRefused := False;
  try
    CaptureNyxDocumentationType(LAlternateLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'A producer cannot claim a common alias contradicted by its branch data');
  LAlternate.ClaimedCommon := Default(TNyxDocumentationRef);
  LAlternate.Cancellation := NewNyxCancellationScope;
  LFrozen := nil;
  LRefused := False;
  try
    LFrozen := CaptureNyxDocumentationType(LAlternateLease, LAlternate.Cancellation);
  except
    on ENyxDocumentationCancelled do
      LRefused := True;
  end;
  Check(LRefused and (LFrozen = nil), 'Mid-branch cancellation publishes no partial shape');
  LAlternate.Cancellation := nil;
  LWrongIntrinsic := TAlternateIntrinsic.Create;
  LWrongLease := LWrongIntrinsic;
  LWrongIntrinsic.Value := LPlain;
  LWrongIntrinsic.CurrentFamily := ndifString;
  LRefused := False;
  try
    CaptureNyxDocumentationType(LWrongLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'An ordinary record cannot attach a contradictory intrinsic capability');
  LMissing := TAlternateType.Create;
  LMissingLease := LMissing;
  LMissing.Value := LType;
  LRefused := False;
  try
    CaptureNyxDocumentationType(LMissingLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused, 'An intrinsic-kind producer must actually expose its specialized family');

  LRegistry := TNyxDocumentationRegistry.Create;
  try
    LSchemas := CaptureNyxSchemas;
    LCallbacks := CaptureNyxCallbacks;
    LBundle := ShapeBundle;
    LIndex := LRegistry.Refresh(LSchemas, LCallbacks, ShapeCatalog(LBundle), 0);
    Check(LIndex.DeclarationCount = 7,
      'The index accepts exact per-platform graphs whose combined edge union would be falsely cyclic');
    LType := LIndex.FindDeclaration(ShapeRef('NativeText'));
    Check(Supports(LType, INyxDocumentationIntrinsic, LIntrinsic) and (LIntrinsic.Family = ndifString),
      'Registration/index capture retains intrinsic specialization');
    LAlias := LIndex.FindDeclaration(ShapeRef('Text'));
    Check(Supports(LAlias, INyxDocumentationAliasTargets, LTargets) and
      LTargets.TargetFor(npfBrowser).Same(ShapeRef('BrowserText')),
      'Registration/index capture retains all exact alias branches');
    LRefused := False;
    try
      LIndex.Members(ShapeRef('Text'));
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LIndex.Members(LTargets.TargetFor(npfBrowser)).Count = 0),
      'Target-neutral inherited lookup refuses a guess; explicit target lookup uses its actual owner');
    LSame := ShapeBundle;
    Check(LBundle.DeclarationContentAt(0) = LSame.DeclarationContentAt(0),
      'Equivalent intrinsic shapes retain deterministic framed equality');
    Check(LBundle.DeclarationContentAt(0) <> ShapeBundle(2).DeclarationContentAt(0),
      'Intrinsic family is included in framed content instead of silently deduplicated');
    Check(LBundle.DeclarationContentAt(2) <> ShapeBundle(3).DeclarationContentAt(2),
      'Target-specific identities are included even when both common AliasTarget getters are absent');
    LNext := LRegistry.Refresh(LSchemas, LCallbacks, ShapeCatalog(LBundle, LSame), LRegistry.Version);
    Check((LNext.DeclarationCount = 7) and (LIndex.DeclarationCount = 7),
      'Equivalent specialized declarations deduplicate without changing an older reader');
    RefuseIndex(LRegistry, LSchemas, LCallbacks, ShapeCatalog(LBundle, ShapeBundle(2)), 'Conflicting documentation');
    RefuseIndex(LRegistry, LSchemas, LCallbacks, ShapeCatalog(LBundle, ShapeBundle(3)), 'Conflicting documentation');
    RefuseIndex(LRegistry, LSchemas, LCallbacks, ShapeCatalog(ShapeBundle(1)), 'Cyclic');
    RefuseIndex(LRegistry, LSchemas, LCallbacks, ShapeCatalog(ShapeBundle(4)), 'Unresolved documentation');
    RefuseIndex(LRegistry, LSchemas, LCallbacks,
      ShapeCatalog(PartialBundle([ndplatformNative, ndplatformBrowser])), 'declared platform availability');
    RefuseIndex(LRegistry, LSchemas, LCallbacks,
      ShapeCatalog(InheritanceBundle([ndplatformNative, ndplatformBrowser])), 'Incompatible declared inheritance kinds');
    LNext := LRegistry.Refresh(LSchemas, LCallbacks,
      ShapeCatalog(InheritanceBundle([ndplatformNative])), LRegistry.Version);
    Check(LNext.DeclarationCount = 4,
      'Native-only inheritance validates its actual class target without using the unrelated browser interface branch');
    LNext := LRegistry.Refresh(LSchemas, LCallbacks,
      ShapeCatalog(PartialBundle([ndplatformNative])), LRegistry.Version);
    Check((LNext.DeclarationCount = 2) and not LNext.FindDeclaration(ShapeRef('Text')).AliasTarget.Defined,
      'Platform-only availability accepts its one exact branch without a manufactured common target');
    LNext := LRegistry.Refresh(LSchemas, LCallbacks,
      ShapeCatalog(PartialBundle([])), LRegistry.Version);
    Check(LNext.DeclarationCount = 2, 'Unspecified availability does not promise a missing alias branch');
    LScope := NewNyxCancellationScope;
    LScope.Cancel;
    RefuseIndex(LRegistry, LSchemas, LCallbacks, ShapeCatalog(LBundle), '', LScope);
    Check(LIndex.FindDeclaration(ShapeRef('Text')).Kind = ndtAlias,
      'Later shape refusals and publications preserve earlier specialized indices');
  finally
    LRegistry.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' documentation shape checks';
    {$else}
    WriteLn('PASS ', GChecks, ' documentation shape checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
