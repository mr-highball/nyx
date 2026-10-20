{ Declared property metadata; native source witness is mandatory. MIT, see LICENSE. }
program nyx_documentation_properties_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.documentation,
  nyx.documentation.registration, nyx.documentation.declarations,
  nyx.documentation.registry, nyx.catalog, nyx.schema, nyx.callbacks,
  nyx.pascal.syntax, nyx.test.documentation
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Mutable alternate metadata exercises role/count/copy/final-getter guards.
    Its fluent methods refuse: capture must construct its own immutable owners. }
  TAlternateProperty = class(TInterfacedObject, INyxDocumentationMember, INyxDocumentationProperty)
  public
    Value: INyxDocumentationMember;
    Parameters: array of TNyxDocumentationParameter;
    CountClaim, Reads, ChangeAtRead, CancelAtRead: Integer;
    KindClaim: TNyxDocumentationMemberKind;
    Access: TNyxDocumentationPropertyAccesses;
    IsDefault: Boolean;
    Scope: INyxCancellationScope;
    destructor Destroy; override;
    function Identity: TNyxDocumentationRef;
    function Kind: TNyxDocumentationMemberKind;
    function Summary: TNyxText;
    function Evidence: TNyxDocumentationEvidence;
    function Signature: INyxDocumentationSignature;
    function ValueType: TNyxDocumentationRef;
    function ExampleCount: Integer;
    function ExampleAt(AIndex: Integer): TNyxDocumentationExample;
    function RelatedCount: Integer;
    function RelatedAt(AIndex: Integer): TNyxDocumentationRef;
    function WithSignature(const AValue: INyxDocumentationSignature): INyxDocumentationMember;
    function OfType(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
    function WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationMember;
    function WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
    function ParameterCount: Integer;
    function ParameterAt(AIndex: Integer): TNyxDocumentationParameter;
    function DeclaredAccess: TNyxDocumentationPropertyAccesses;
    function DefaultProperty: Boolean;
  end;
  { Capture-local field admission can refuse the first property extension field;
    no partial framed result or declaration may escape that refusal. }
  TRejectingAdmission = class(TInterfacedObject, INyxDocumentationAdmission)
  public
    Fields, RejectAt: Integer;
    procedure AdmitField(ABytes: Integer);
  end;

var
  GChecks, GAlternateDestroyed: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

function Origin: TNyxDocumentationRef;
begin
  Result := NyxDocumentationUnit('mechanics.properties');
end;

function Ref(const AName: TNyxText): TNyxDocumentationRef;
begin
  Result := NyxDocumentationType('mechanics.properties', AName);
end;

function Items(AAccess: TNyxDocumentationPropertyAccesses = [ndpaRead, ndpaWrite];
  ADefault: Boolean = True): INyxDocumentationMember;
begin
  Result := NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TBag'), 'Items'),
    'Read or replace a synthetic indexed value.', [
      NyxDocumentationParameter('Index', Ref('Index')),
      NyxDocumentationParameter('Key', Ref('Key'), ndpConst).WithDefault('''line'#0#13#10'Ω''')],
    AAccess, ADefault).OfType(Ref('Value'));
end;

function Declarations(AAccess: TNyxDocumentationPropertyAccesses = [ndpaRead, ndpaWrite];
  AMissing: Boolean = False): TNyxDocumentationDeclarations;
var
  LProperty: INyxDocumentationMember;
begin
  Result := nil;
  SetLength(Result, 5);
  Result[0] := NewNyxDocumentationIntrinsic(Ref('Index'), ndifInteger, 'Synthetic index family.');
  Result[1] := NewNyxDocumentationIntrinsic(Ref('Key'), ndifString, 'Synthetic key family.');
  Result[2] := NewNyxDocumentationType(Ref('Value'), ndtInterface, 'Synthetic element contract.');
  LProperty := Items(AAccess);

  if AMissing then
    LProperty := NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TBag'), 'Items'),
      'An unresolved index remains a real link.', [
        NyxDocumentationParameter('Index', Ref('MissingIndex'))], AAccess).OfType(Ref('Value'));
  Result[3] := NewNyxDocumentationType(Ref('TBag'), ndtClass, 'Synthetic property owner.').WithMember(LProperty);
  Result[4] := NewNyxDocumentationType(Ref('TChild'), ndtClass, 'Synthetic inherited owner.').WithBase(Ref('TBag'));
end;

destructor TAlternateProperty.Destroy;
begin
  Inc(GAlternateDestroyed);
  inherited Destroy;
end;

function TAlternateProperty.Identity: TNyxDocumentationRef;
begin
  Result := Value.Identity;
end;

function TAlternateProperty.Kind: TNyxDocumentationMemberKind;
begin
  Result := KindClaim;
end;

function TAlternateProperty.Summary: TNyxText;
begin
  Result := Value.Summary;
end;

function TAlternateProperty.Evidence: TNyxDocumentationEvidence;
begin
  Result := Value.Evidence;
end;

function TAlternateProperty.Signature: INyxDocumentationSignature;
begin
  Result := Value.Signature;
end;

function TAlternateProperty.ValueType: TNyxDocumentationRef;
begin
  Result := Value.ValueType;
end;

function TAlternateProperty.ExampleCount: Integer;
begin
  Result := Value.ExampleCount;
end;

function TAlternateProperty.ExampleAt(AIndex: Integer): TNyxDocumentationExample;
begin
  Result := Value.ExampleAt(AIndex);
end;

function TAlternateProperty.RelatedCount: Integer;
begin
  Result := Value.RelatedCount;
end;

function TAlternateProperty.RelatedAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Result := Value.RelatedAt(AIndex);
end;

function TAlternateProperty.WithSignature(const AValue: INyxDocumentationSignature): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not call producer fluent methods');
end;

function TAlternateProperty.OfType(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not call producer fluent methods');
end;

function TAlternateProperty.WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not call producer fluent methods');
end;

function TAlternateProperty.WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
begin
  Result := nil;
  raise Exception.Create('Capture must not call producer fluent methods');
end;

function TAlternateProperty.ParameterCount: Integer;
begin
  Result := CountClaim;
end;

function TAlternateProperty.ParameterAt(AIndex: Integer): TNyxDocumentationParameter;
begin
  Inc(Reads);
  Result := Parameters[AIndex];

  if (ChangeAtRead > 0) and (Reads = ChangeAtRead) then
    Inc(CountClaim);

  if (Scope <> nil) and (Reads = CancelAtRead) then
    Scope.Cancel;
end;

function TAlternateProperty.DeclaredAccess: TNyxDocumentationPropertyAccesses;
begin
  Result := Access;
end;

function TAlternateProperty.DefaultProperty: Boolean;
begin
  Result := IsDefault;
end;

procedure TRejectingAdmission.AdmitField(ABytes: Integer);
begin
  Inc(Fields);

  if Fields = RejectAt then
    raise ENyxDocumentation.Create('Controlled property field refusal');
end;

procedure PrepareAlternate(AOwner: TAlternateProperty);
var
  LProperty: INyxDocumentationProperty;
  LAt: Integer;
begin
  AOwner.Value := Items;
  Supports(AOwner.Value, INyxDocumentationProperty, LProperty);
  AOwner.KindClaim := ndmProperty;
  AOwner.CountClaim := LProperty.ParameterCount;
  AOwner.Access := LProperty.DeclaredAccess;
  AOwner.IsDefault := LProperty.DefaultProperty;
  SetLength(AOwner.Parameters, AOwner.CountClaim);
  for LAt := 0 to AOwner.CountClaim - 1 do
    AOwner.Parameters[LAt] := LProperty.ParameterAt(LAt);
end;

{ Returning through a helper retires compiler-held interface temporaries before
  observing supplier destruction, independently of either target's lowering. }
function DetachedAlternate: INyxDocumentationMember;
var
  LOwner: TAlternateProperty;
  LLease: INyxDocumentationMember;
begin
  LOwner := TAlternateProperty.Create;
  LLease := LOwner;
  PrepareAlternate(LOwner);
  Result := CaptureNyxDocumentationMember(LLease);
  LOwner.Parameters[0] := NyxDocumentationParameter('Changed', Ref('Key'));
end;

function Frame(const AValue: TNyxText): TNyxText;
begin
  Result := IntToStr(NyxUTF8ByteCount(AValue)) + ':' + AValue;
end;

procedure CoreCases;
var
  LOld, LNew, LFrozen, LLegacy: INyxDocumentationMember;
  LProperty: INyxDocumentationProperty;
  LType: INyxDocumentationType;
  LOwner: TAlternateProperty;
  LLease: INyxDocumentationMember;
  LParameters: array of TNyxDocumentationParameter;
  LAt, LBefore, LBytes: Integer;
  LScope: INyxCancellationScope;
  LRefused: Boolean;
  LContent, LExpected: TNyxText;
  LDeclarations: TNyxDocumentationDeclarations;
  LAdmission: TRejectingAdmission;
  LAdmissionLease: INyxDocumentationAdmission;
begin
  LOld := Items;
  Check((LOld.Kind = ndmProperty) and (LOld.Signature = nil) and
    Supports(LOld, INyxDocumentationProperty, LProperty), 'Indexing retains a specialized property, not a call signature');
  Check((LProperty.ParameterCount = 2) and (LProperty.ParameterAt(0).Name = 'Index') and
    (LProperty.ParameterAt(1).Passing = ndpConst) and LProperty.DefaultProperty and
    (LProperty.DeclaredAccess = [ndpaRead, ndpaWrite]), 'Ordered parameters, modes, declared clauses and default marker survive');
  Check(LProperty.ParameterAt(1).DefaultPascal = TNyxText('''line'#0#13#10'Ω'''),
    'Parameter default text keeps exact NUL, Unicode and CRLF');
  LNew := LOld.OfType(Ref('TChild')).WithExample(NyxDocumentationExample('Inspect a value', 'Value := Bag.Items[1, ''a''];'))
    .WithRelated(Ref('Value'));
  Check(Supports(LNew, INyxDocumentationProperty, LProperty) and (LProperty.ParameterCount = 2) and
    LNew.ValueType.Same(Ref('TChild')) and LOld.ValueType.Same(Ref('Value')) and
    (LOld.ExampleCount = 0) and (LOld.RelatedCount = 0), 'Every fluent copy retains property shape and earlier immutable readers');
  LType := NewNyxDocumentationType(Ref('TBag'), ndtClass, 'Synthetic owner.').WithMember(LOld);
  LType := CaptureNyxDocumentationType(LType);
  Check(Supports(LType.MemberAt(0), INyxDocumentationProperty, LProperty) and
    (LProperty.ParameterCount = 2), 'Containing type construction and capture preserve specialized property data');
  LBefore := GAlternateDestroyed;
  LFrozen := DetachedAlternate;
  Check((GAlternateDestroyed = LBefore + 1) and Supports(LFrozen, INyxDocumentationProperty, LProperty) and
    (LProperty.ParameterAt(0).Name = 'Index'), 'Alternate producer retires and later mutation cannot change captured parameters');
  LNew := NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TBag'), 'Inherited'),
    'No new clauses are authored.', [], []);
  Check(Supports(LNew, INyxDocumentationProperty, LProperty) and (LProperty.ParameterCount = 0) and
    (LProperty.DeclaredAccess = []) and not LProperty.DefaultProperty and not LNew.ValueType.Defined,
    'Omitted inherited clauses and value type stay declared absence, without guessed effective access');
  LNew := NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TBag'), 'ReadOnly'),
    'An ordinary readable property.', [], [ndpaRead]).OfType(Ref('Value'));
  Check(Supports(LNew, INyxDocumentationProperty, LProperty) and (LProperty.ParameterCount = 0) and
    (LProperty.DeclaredAccess = [ndpaRead]) and not LProperty.DefaultProperty,
    'An ordinary declared read clause remains separate from indexed/default-member syntax');
  LNew := NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TBag'), 'WriteOnly'),
    'An ordinary writable property.', [], [ndpaWrite]).OfType(Ref('Value'));
  Check(Supports(LNew, INyxDocumentationProperty, LProperty) and
    (LProperty.DeclaredAccess = [ndpaWrite]), 'A declared write clause implies no read clause');
  LLegacy := NewNyxDocumentationMember(NyxDocumentationMember(Ref('TBag'), 'Count'), ndmProperty, 'Legacy').OfType(Ref('Index'));
  Check(not Supports(LLegacy, INyxDocumentationProperty, LProperty), 'Legacy property absence remains unknown');
  for LAt := 0 to 4 do
  begin
    LRefused := False;
    try
      case LAt of
        0: LOld.WithSignature(NewNyxDocumentationSignature([], Ref('Value')));
        1: LOld.OfType(Origin);
        2: NewNyxDocumentationProperty(NyxDocumentationMember(Origin, 'GlobalProperty'), '', [], []);
        3: begin
          Supports(LOld, INyxDocumentationProperty, LProperty);
          LProperty.ParameterAt(-1);
        end;
        4: NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TBag'), 'Duplicate'), '', [
             NyxDocumentationParameter('Index', Ref('Index')), NyxDocumentationParameter('INDEX', Ref('Index'))], []);
      end;
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused, 'Invalid property role/type/ordinal/parameter identity refuses ' + IntToStr(LAt));
  end;
  SetLength(LParameters, NyxDocumentationMaximumParameters + 1);
  for LAt := 0 to High(LParameters) do
    LParameters[LAt] := NyxDocumentationParameter('P' + IntToStr(LAt), Ref('Index'));
  LRefused := False;
  try
    NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TBag'), 'Large'), '', LParameters, []);
  except
    on ENyxDocumentation do LRefused := True;
  end;
  Check(LRefused, 'Ordinary property creation retains the existing64 parameter ceiling');
  SetLength(LParameters, NyxDocumentationMaximumParameters);
  LNew := NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TBag'), 'Largest'), '', LParameters, [ndpaRead]);
  Check(Supports(LNew, INyxDocumentationProperty, LProperty) and
    (LProperty.ParameterCount = NyxDocumentationMaximumParameters), 'Exactly64 index parameters remain supported');
  LOwner := TAlternateProperty.Create;
  LLease := LOwner;
  PrepareAlternate(LOwner);
  for LAt := 0 to 4 do
  begin
    LOwner.Reads := 0;
    LOwner.CountClaim := 2;
    LOwner.KindClaim := ndmProperty;
    LOwner.ChangeAtRead := 0;
    LOwner.Scope := nil;
    case LAt of
      0: LOwner.CountClaim := 65;
      1: LOwner.CountClaim := -1;
      2: LOwner.KindClaim := ndmMethod;
      3: LOwner.ChangeAtRead := 4;
      4: begin
        LOwner.Scope := NewNyxCancellationScope;
        LOwner.CancelAtRead := 4;
      end;
    end;
    LFrozen := nil;
    LRefused := False;
    try
      LFrozen := CaptureNyxDocumentationMember(LLease, LOwner.Scope);
    except
      on ENyxDocumentation do LRefused := True;
    end;
    Check(LRefused and (LFrozen = nil), 'Alternate property admission refuses atomically ' + IntToStr(LAt));

    if LAt <= 2 then
      Check(LOwner.Reads = 0, 'Invalid alternate envelope refuses before parameter getters ' + IntToStr(LAt));
  end;
  LScope := NewNyxCancellationScope;
  LScope.Cancel;
  LFrozen := nil;
  LRefused := False;
  try
    LFrozen := CaptureNyxDocumentationMember(LOld, LScope);
  except
    on ENyxDocumentationCancelled do LRefused := True;
  end;
  Check(LRefused and (LFrozen = nil), 'Cancellation refuses canonical property capture before publication');
  LType := NewNyxDocumentationType(Ref('TBag'), ndtClass, 'S').WithMember(LLegacy);
  LBytes := 0;
  CaptureNyxDocumentationDeclaration(LType, Origin, LContent, LBytes);
  LExpected := Frame(Ref('TBag').Key) + Frame('0') + Frame('S') + Frame('0') + Frame('') +
    Frame('0') + Frame('0') + Frame('0') + Frame('1') + Frame(LLegacy.Identity.Key) + Frame('4') +
    Frame('Legacy') + Frame('0') + Frame(Ref('Index').Key) + Frame('0') + Frame('0') + Frame('0');
  Check(LContent = LExpected, 'Absent property shape preserves independently framed legacy content byte for byte');
  LType := NewNyxDocumentationType(Ref('TBag'), ndtClass, 'S').WithMember(LOld);
  LBytes := 0;
  CaptureNyxDocumentationDeclaration(LType, Origin, LContent, LBytes);
  Check(Pos('property-shape', LContent) > 0, 'Present property syntax participates in retained exact framing');
  LAdmission := TRejectingAdmission.Create;
  LAdmissionLease := LAdmission;
  { The independently framed legacy header above contains seventeen fields;
    the next field is the first optional property fact. }
  LAdmission.RejectAt := 18;
  LDeclarations := Declarations;
  LContent := 'Unpublished';
  LBytes := 0;
  LRefused := False;
  LType := nil;
  try
    LType := CaptureNyxDocumentationDeclaration(LDeclarations[3], Origin, LContent, LBytes, nil, LAdmissionLease);
  except
    on ENyxDocumentation do LRefused := True;
  end;
  Check(LRefused and (LType = nil) and (LContent = '') and (LAdmission.Fields = 18),
    'Property extension field admission refuses without publishing a partial declaration/content');
end;

procedure AggregateMemberCase;
var
  LParameters: array of TNyxDocumentationParameter;
  LMember, LBefore: INyxDocumentationMember;
  LProperty: INyxDocumentationProperty;
  LLongType: TNyxDocumentationRef;
  LAt: Integer;
  LName, LUnit: TNyxText;
  LRefused: Boolean;
begin
  { Each value is within its existing individual limit. Their combination must
    still consume the shared member allowance, including copied index facts. }
  LUnit := 'mechanics.' + StringOfChar('u', 246);
  LLongType := NyxDocumentationType(LUnit, 'T' + StringOfChar('x', 255));
  SetLength(LParameters, NyxDocumentationMaximumParameters);
  for LAt := 0 to High(LParameters) do
  begin
    LName := 'P' + IntToStr(LAt);
    LName := LName + StringOfChar('p', 256 - Length(LName));
    LParameters[LAt] := NyxDocumentationParameter(LName, LLongType)
      .WithDefault(StringOfChar('d', 1024));
  end;
  LMember := NewNyxDocumentationProperty(NyxDocumentationMember(Ref('TBag'), 'Bounded'),
    StringOfChar('s', 2048), LParameters, [ndpaRead]);
  for LAt := 0 to NyxDocumentationMaximumExamples - 1 do
    LMember := LMember.WithExample(NyxDocumentationExample('Example' + IntToStr(LAt), StringOfChar('e', 16384)));
  LBefore := LMember;
  LRefused := False;
  try
    for LAt := 0 to NyxDocumentationMaximumLinks - 1 do
    begin
      LName := 'T' + IntToStr(LAt);
      LName := LName + StringOfChar('r', 256 - Length(LName));
      LMember := LMember.WithRelated(NyxDocumentationType(LUnit, LName));
    end;
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused and Supports(LBefore, INyxDocumentationProperty, LProperty) and
    (LProperty.ParameterCount = NyxDocumentationMaximumParameters) and (LBefore.RelatedCount = 0),
    'Index facts consume the unchanged aggregate member byte ceiling while earlier readers remain immutable');
end;

procedure RegistryCases;
var
  LRegistry: TNyxDocumentationRegistry;
  LCatalog, LBadCatalog: TNyxCatalog;
  LIndex, LBefore: INyxDocumentationIndex;
  LProperty: INyxDocumentationProperty;
  LMembers: INyxDocumentationMembers;
  LChunks: TNyxDocumentationDeclarationChunks;
  LPackage: INyxDocumentationPackage;
  LVersion, LAt: Integer;
  LRefused: Boolean;
  LLimits: TNyxDocumentationIndexLimits;
begin
  LRegistry := TNyxDocumentationRegistry.Create;
  LCatalog := TNyxCatalog.Create;
  LBadCatalog := TNyxCatalog.Create;
  try
    SetLength(LChunks, 1);
    LChunks[0] := NewNyxDocumentationDeclarationChunk(Origin, Declarations);
    LPackage := NewNyxDocumentationPackage(LChunks, TNyxDocumentationContributionLimits.Defaults);
    LCatalog.RegisterKind('property-mechanics', 'Property mechanics', 'Custom', False,
      NewNyxDocumentationBinding(LPackage, Ref('TBag')));
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, 0);
    Check((LIndex.DeclarationCount = 5) and Supports(LIndex.FindMember(Items.Identity),
      INyxDocumentationProperty, LProperty) and (LProperty.ParameterCount = 2),
      'Actual catalog contribution discovery retains property syntax and all index-type links');
    LMembers := LIndex.Members(Ref('TChild'));
    Check((LMembers.Count = 1) and LMembers.ItemAt(0).Identity.Owner.Same(Ref('TBag')),
      'Inherited member lookup retains the original property owner');
    LBefore := LIndex;
    LVersion := LRegistry.Version;
    for LAt := 0 to 3 do
    begin
      LBadCatalog.Free;
      LBadCatalog := TNyxCatalog.Create;
      LLimits := TNyxDocumentationIndexLimits.Defaults;
      case LAt of
        0: begin
          LBadCatalog.RegisterKind('first', 'First', 'Custom', False,
            NewNyxDocumentationRegistration(Origin, Ref('TBag'), Declarations));
          LBadCatalog.RegisterKind('second', 'Second', 'Custom', False,
            NewNyxDocumentationRegistration(Origin, Ref('TBag'), Declarations([ndpaRead])));
        end;
        1: LBadCatalog.RegisterKind('missing', 'Missing', 'Custom', False,
          NewNyxDocumentationRegistration(Origin, Ref('TBag'), Declarations([ndpaRead], True)));
        2: LLimits := LLimits.Work(1);
        3: LLimits := LLimits.Bytes(1);
      end;
      LIndex := nil;
      LRefused := False;
      try

        if LAt <= 1 then
          LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LBadCatalog.Capture, LVersion, LLimits)
        else
          LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, LVersion, LLimits);
      except
        on ENyxDocumentation do LRefused := True;
      end;
      Check(LRefused and (LIndex = nil) and (LRegistry.Current = LBefore) and (LRegistry.Version = LVersion),
        'Conflicting property shape, unresolved index type or aggregate exhaustion preserves publication ' + IntToStr(LAt));
    end;
  finally
    LBadCatalog.Free;
    LCatalog.Free;
    LRegistry.Free;
  end;
end;

{$ifndef PAS2JS}
function RepositoryArgument: String;
begin

  if ParamCount <> 1 then
    raise EArgumentException.Create('Usage: nyx_documentation_properties_tests <repository-root>');
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));

  if not DirectoryExists(Result) or not FileExists(Result + 'PROJECT.md') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.controls.pas') then
    raise EArgumentException.Create('The repository root must contain PROJECT.md and src/nyx.controls.pas');
end;

procedure ActualChildren(const ARepository: String);
var
  LStream: TMemoryStream;
  LBytes: TNyxBytes;
  LSyntax: INyxPascalSyntaxSnapshot;
  LOptions: TNyxPascalSyntaxOptions;
  LOwner, LProperty, LParameter, LAt: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
begin
  LOptions := TNyxPascalSyntaxOptions.Defaults;
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(ARepository + 'src' + PathDelim + 'nyx.controls.pas');

    if LStream.Size > LOptions.LexicalOptions.MaximumSourceBytes then
      raise Exception.Create('Actual controls source exceeds the existing lexical allowance');
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
      Move(LStream.Memory^, LBytes[0], Length(LBytes));
    LSyntax := NewNyxPascalSyntaxSnapshot(NyxDecodeUTF8(LBytes), LOptions);
  finally
    LStream.Free;
  end;
  LOwner := -1;
  LAt := LSyntax.Named(0, 'INyxControl');
  while LAt >= 0 do
  begin
    LDeclaration := LSyntax.Declaration(LAt);

    if (LDeclaration.Kind = psdkType) and not (psdfForward in LDeclaration.Flags) then
      LOwner := LAt;
    LAt := LDeclaration.NextNamed;
  end;
  Check(LOwner >= 0, 'Mandatory actual controls source contains its full INyxControl declaration');
  LProperty := LSyntax.Named(LSyntax.Declaration(LOwner).NestedScope, 'Children');
  Check(LProperty >= 0, 'Actual source exposes Children as a declared member');
  LDeclaration := LSyntax.Declaration(LProperty);
  Check((LDeclaration.Kind = psdkProperty) and (LDeclaration.PropertyParameters.Length > 0) and
    (LDeclaration.PropertyAccess = [pspaRead]) and not LDeclaration.DefaultProperty and
    ((LDeclaration.Flags * [psdfIncomplete, psdfUnsupported, psdfConditional]) = []) and
    (Trim(LSyntax.Lexical.Slice(LDeclaration.TypeSpan)) = TNyxText('INyxControl')),
    'Actual Children retains indexed/read/default facts and exact unbound value type spelling');
  Check(LSyntax.Scope(LDeclaration.NestedScope).Complete and
    (LSyntax.Scope(LDeclaration.NestedScope).Kind = psskPropertyParameters),
    'Actual property index parameters keep their complete dedicated syntax owner');
  LParameter := LSyntax.Named(LDeclaration.NestedScope, 'AIndex');
  Check((LParameter >= 0) and (LSyntax.Scope(LDeclaration.NestedScope).DeclarationCount = 1),
    'Actual Children has exactly its one named index parameter');
  LDeclaration := LSyntax.Declaration(LParameter);
  Check((LDeclaration.ParameterMode = pspmValue) and
    (Trim(LSyntax.Lexical.Slice(LDeclaration.TypeSpan)) = TNyxText('Integer')),
    'Actual parameter mode/type spelling is read without inventing a qualified System/core binding');
  { Includes remain outside this structural source read; it is neither a full
    translated unit nor registered core metadata. No getter or control runs. }
end;
{$endif}

{$ifndef PAS2JS}
var
  LRepository: String;
{$endif}
begin
  try
    {$ifndef PAS2JS}
    LRepository := RepositoryArgument;
    {$endif}
    CoreCases;
    AggregateMemberCase;
    RegistryCases;
    {$ifndef PAS2JS}
    ActualChildren(LRepository);
    {$endif}
    Check((DocumentationControlsConstructed = 0) and (DocumentationCallbacksConstructed = 0) and
      (DocumentationCallbacksInvoked = 0) and (DocumentationGlobalFactoryCalls = 0) and
      (DocumentationGlobalRoutineCalls = 0), 'Property inspection executes no documented control, callback, factory or routine');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' portable property documentation checks';
    {$else}
    WriteLn('PASS ', GChecks, ' property documentation checks / actual controls source inspected');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.ClassName, ': ', E.Message, ' at check count ', GChecks);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
