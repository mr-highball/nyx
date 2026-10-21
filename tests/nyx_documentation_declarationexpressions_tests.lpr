{ Declared compound types preserve source syntax and nominal owners. MIT. }
program nyx_documentation_declarationexpressions_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.types, nyx.pascal.work, nyx.pascal.context,
  nyx.pascal.files, nyx.pascal.translation, nyx.pascal.syntax.translated,
  nyx.pascal.binding, nyx.documentation.source, nyx.documentation.declarations,
  nyx.documentation.typeexpressions, nyx.documentation,
  nyx.documentation.registration, nyx.documentation.registry,
  nyx.catalog, nyx.schema, nyx.callbacks, nyx.test.documentation
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { An alternate type forwards old inert facts, but its fluent methods refuse.
    Counters distinguish envelope admission from borrowing an invalid row. }
  TAlternateType = class(TInterfacedObject, INyxDocumentationType)
  public
    Value: INyxDocumentationType;
    OverrideKind: Boolean;
    AdvertisedKind: TNyxDocumentationTypeKind;
    Bases, Members, BaseReads, MemberReads: Integer;
    ChangedSummary: Boolean;
    constructor Create(const AValue: INyxDocumentationType);
    destructor Destroy; override;
    function Identity: TNyxDocumentationRef;
    function Kind: TNyxDocumentationTypeKind;
    function Summary: TNyxText;
    function Evidence: TNyxDocumentationEvidence;
    function AliasTarget: TNyxDocumentationRef;
    function BaseCount: Integer;
    function BaseAt(AIndex: Integer): TNyxDocumentationRef;
    function MemberCount: Integer;
    function MemberAt(AIndex: Integer): INyxDocumentationMember;
    function ExampleCount: Integer;
    function ExampleAt(AIndex: Integer): TNyxDocumentationExample;
    function RelatedCount: Integer;
    function RelatedAt(AIndex: Integer): TNyxDocumentationRef;
    function WithAlias(const AValue: TNyxDocumentationRef): INyxDocumentationType;
    function WithBase(const AValue: TNyxDocumentationRef): INyxDocumentationType;
    function WithMember(const AValue: INyxDocumentationMember): INyxDocumentationType;
    function WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
    function WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
  end;
  { The new optional capability can change/cancel on its final observation.
    Captures must freeze it and recheck the old envelope before publication. }
  TAlternateDeclared = class(TAlternateType, INyxDocumentationDeclaredTypeExpression)
  public
    Use, ChangedUse: INyxDocumentationTypeUse;
    AdvertisedFamily: TNyxPascalTypeKind;
    DefinitionReads, FamilyReads: Integer;
    MutateSummary: Boolean;
    Scope: INyxCancellationScope;
    function Definition: INyxDocumentationTypeUse;
    function Family: TNyxPascalTypeKind;
  end;
  { A bounded binding supplier exposes read counters and deliberate last-read
    mutation, without retaining a builder or executing a source declaration. }
  TAlternateUse = class(TInterfacedObject, INyxDocumentationTypeUse)
  public
    Value: INyxDocumentationTypeUse;
    Count, Reads, MutateAt: Integer;
    constructor Create(const AValue: INyxDocumentationTypeUse);
    function Reference: TNyxDocumentationRef;
    function Expression: INyxDocumentationTypeExpression;
    function BindingCount: Integer;
    function BindingAt(AIndex: Integer): TNyxDocumentationRef;
    function WithBinding(AIndex: Integer; const AValue: TNyxDocumentationRef): INyxDocumentationTypeUse;
  end;
  { Construction leases point outward to this probe. A published declaration
    must keep neither cancellation nor admission alive after lexical retirement. }
  TLeaseProbe = class(TInterfacedObject, INyxCancellationScope, INyxDocumentationTypeAdmission)
  public
    CancelledValue: Boolean;
    destructor Destroy; override;
    procedure Cancel;
    function GetCancelled: Boolean;
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;
  { Alternate roots expose final-read mutation/cancellation without supplying
    compiler execution authority. Their context is an explicitly authored value. }
  TAlternateRoot = class(TInterfacedObject, INyxDocumentationCompilerRoot)
  public
    Value: INyxDocumentationCompilerRoot;
    NameReads, MutateAt: Integer;
    Scope: INyxCancellationScope;
    function Name: TNyxPascalSizeReference;
    function Family: TNyxDocumentationIntrinsicFamily;
    function Facts: INyxPascalCompilerContext;
  end;
  { Optional root topology remains separate from the forwarded legacy envelope. }
  TAlternateRootUse = class(TAlternateUse, INyxDocumentationCompilerRootBindings)
  public
    Root: INyxDocumentationCompilerRoot;
    RootReads, ClearAt, PublishAt: Integer;
    function RootAt(AIndex: Integer): INyxDocumentationCompilerRoot;
  end;
  { The alias capability can disappear semantically by returning nil late. }
  TAlternateRootAlias = class(TAlternateType, INyxDocumentationAliasDefinition)
  public
    Use: INyxDocumentationTypeUse;
    Reads, ClearAt: Integer;
    function Definition: INyxDocumentationTypeUse;
  end;
  { Reentrant admission replaces caller slots after entry. Local leases must
    retain the already admitted record/text/context and construction values. }
  TRootInputAdmission = class(TInterfacedObject, INyxDocumentationTypeAdmission)
  public
    AliasInput, RootInput, Fired: Boolean;
    destructor Destroy; override;
    procedure AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
    function RemainingTypeBytes: Integer;
  end;

var
  GChecks, GDisposed, GLeasesDisposed: Integer;
  GRootInputDisposed: Integer;
  GAliasIdentity: TNyxDocumentationRef;
  GAliasSummary: TNyxText;
  GRootInputFacts: INyxPascalCompilerContext;
  GRootInputScope: INyxDocumentationTypeConstruction;
  {$ifndef PAS2JS}
  GActualDefinitions: Integer;
  {$endif}

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not AValue then
    raise Exception.Create(AReason);
end;

function Ref(const AName: TNyxText): TNyxDocumentationRef;
begin
  Result := NyxDocumentationType('mechanics.declarations', AName);
end;

function TAlternateRoot.Name: TNyxPascalSizeReference;
begin
  Inc(NameReads);
  Result := Value.Name;

  if (MutateAt > 0) and (NameReads >= MutateAt) then
    Result := NyxPascalSizeReference('Byte');
end;

function TAlternateRoot.Family: TNyxDocumentationIntrinsicFamily;
begin
  Result := Value.Family;
end;

function TAlternateRoot.Facts: INyxPascalCompilerContext;
begin
  Result := Value.Facts;

  if Scope <> nil then
    Scope.Cancel;
end;

function TAlternateRootUse.RootAt(AIndex: Integer): INyxDocumentationCompilerRoot;
begin
  Value.BindingAt(AIndex);
  Inc(RootReads);
  Result := Root;

  if (PublishAt > 0) and (RootReads < PublishAt) then
    Result := nil;

  if (ClearAt > 0) and (RootReads >= ClearAt) then
    Result := nil;
end;

function TAlternateRootAlias.Definition: INyxDocumentationTypeUse;
begin
  Inc(Reads);
  Result := Use;

  if (ClearAt > 0) and (Reads >= ClearAt) then
    Result := nil;
end;

destructor TRootInputAdmission.Destroy;
begin
  Inc(GRootInputDisposed);
  inherited Destroy;
end;

procedure TRootInputAdmission.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) or (AUnits < 1) then
    raise Exception.Create('Invalid root input admission');

  if Fired then
    Exit;
  Fired := True;

  if AliasInput then
  begin
    GAliasIdentity := Ref('TReplacedCallerSlot');
    GAliasSummary := 'Replaced after admission.';
  end;

  if RootInput then
  begin
    GRootInputFacts := nil;
    GRootInputScope := nil;
  end;
end;

function TRootInputAdmission.RemainingTypeBytes: Integer;
begin
  Result := NyxDocumentationMaximumBytes;
end;

function Construction: INyxDocumentationTypeConstruction;
begin
  Result := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(NyxDocumentationMaximumBytes));
end;

function TypeUse(const AText: TNyxText; const AScope: INyxDocumentationTypeConstruction;
  ABound: Boolean = True; ARole: TNyxDocumentationTypeOccurrence = ndtoDeclaration): INyxDocumentationTypeUse;
var
  LAt: Integer;
begin
  Result := NewNyxDocumentationExpressionTypeUse(
    NewNyxDocumentationTypeExpression(AText, ARole, AScope), AScope);

  if ABound then
    for LAt := 0 to Result.BindingCount - 1 do
      Result := Result.WithBinding(LAt, Ref('TItem'));
end;

constructor TAlternateType.Create(const AValue: INyxDocumentationType);
begin
  inherited Create;
  Value := AValue;
  Bases := -1;
  Members := -1;
end;

destructor TAlternateType.Destroy;
begin
  Inc(GDisposed);
  inherited Destroy;
end;

function TAlternateType.Identity: TNyxDocumentationRef;
begin
  Result := Value.Identity;
end;

function TAlternateType.Kind: TNyxDocumentationTypeKind;
begin
  Result := Value.Kind;

  if OverrideKind then
    Result := AdvertisedKind;
end;

function TAlternateType.Summary: TNyxText;
begin
  Result := Value.Summary;

  if ChangedSummary then
    Result := Result + ' changed';
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

  if Bases >= 0 then
    Result := Bases;
end;

function TAlternateType.BaseAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Inc(BaseReads);
  Result := Value.BaseAt(AIndex);
end;

function TAlternateType.MemberCount: Integer;
begin
  Result := Value.MemberCount;

  if Members >= 0 then
    Result := Members;
end;

function TAlternateType.MemberAt(AIndex: Integer): INyxDocumentationMember;
begin
  Inc(MemberReads);
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

function TAlternateType.WithAlias(const AValue: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Inspection cannot invoke a supplier fluent method');
end;

function TAlternateType.WithBase(const AValue: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Inspection cannot invoke a supplier fluent method');
end;

function TAlternateType.WithMember(const AValue: INyxDocumentationMember): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Inspection cannot invoke a supplier fluent method');
end;

function TAlternateType.WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Inspection cannot invoke a supplier fluent method');
end;

function TAlternateType.WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
begin
  Result := nil;
  raise Exception.Create('Inspection cannot invoke a supplier fluent method');
end;

function TAlternateDeclared.Definition: INyxDocumentationTypeUse;
begin
  Inc(DefinitionReads);
  Result := Use;

  if DefinitionReads = 2 then
  begin

    if ChangedUse <> nil then
      Result := ChangedUse;

    if Scope <> nil then
      Scope.Cancel;
  end;
end;

function TAlternateDeclared.Family: TNyxPascalTypeKind;
begin
  Inc(FamilyReads);
  Result := AdvertisedFamily;

  if MutateSummary and (FamilyReads = 2) then
    ChangedSummary := True;
end;

constructor TAlternateUse.Create(const AValue: INyxDocumentationTypeUse);
begin
  inherited Create;
  Value := AValue;
  Count := -1;
end;

function TAlternateUse.Reference: TNyxDocumentationRef;
begin
  Result := Value.Reference;
end;

function TAlternateUse.Expression: INyxDocumentationTypeExpression;
begin
  Result := Value.Expression;
end;

function TAlternateUse.BindingCount: Integer;
begin
  Result := Value.BindingCount;

  if Count >= 0 then
    Result := Count;
end;

function TAlternateUse.BindingAt(AIndex: Integer): TNyxDocumentationRef;
begin
  Inc(Reads);
  Result := Value.BindingAt(AIndex);

  if Reads = MutateAt then
    Result := Ref('Changed');
end;

function TAlternateUse.WithBinding(AIndex: Integer; const AValue: TNyxDocumentationRef): INyxDocumentationTypeUse;
begin
  Result := nil;
  raise Exception.Create('Inspection cannot invoke a supplier fluent method');
end;

destructor TLeaseProbe.Destroy;
begin
  Inc(GLeasesDisposed);
  inherited Destroy;
end;

procedure TLeaseProbe.Cancel;
begin
  CancelledValue := True;
end;

function TLeaseProbe.GetCancelled: Boolean;
begin
  Result := CancelledValue;
end;

procedure TLeaseProbe.AdmitTypeWork(APhase: TNyxPascalAnalysisPhase; AUnits: Integer);
begin

  if (Ord(APhase) < Ord(Low(TNyxPascalAnalysisPhase))) or
    (Ord(APhase) > Ord(High(TNyxPascalAnalysisPhase))) or (AUnits < 1) then
    raise Exception.Create('Invalid outward admission');
end;

function TLeaseProbe.RemainingTypeBytes: Integer;
begin
  Result := NyxDocumentationMaximumBytes;
end;

procedure ShapeCases;
const
  CText: array[0..7] of TNyxText = ('array of TItem', 'set of TItem',
    'procedure(const A:TItem) of object', 'function(A:TItem):TItem; cdecl', 'type TItem',
    '^TItem', 'class of TItem', 'AnsiString(CP_UTF8)');
  CFamily: array[0..7] of TNyxPascalTypeKind = (ptxArray, ptxSet, ptxProcedure,
    ptxFunction, ptxDistinct, ptxPointer, ptxClassOf, ptxEncodedString);
var
  LScope: INyxDocumentationTypeConstruction;
  LValue, LCopy, LFrozen: INyxDocumentationType;
  LShape, LCopied: INyxDocumentationDeclaredTypeExpression;
  LAt, LAction: Integer;
  LRefused: Boolean;
begin
  Check((Ord(ndtClass) = 0) and (Ord(ndtIntrinsic) = 7) and (Ord(ndtExpression) = 8),
    'New declaration kind appends without renumbering old kinds');
  LScope := Construction;
  try
    for LAt := 0 to High(CText) do
    begin
      LValue := NewNyxDocumentationDeclaredType(Ref('TShape'), 'Declared shape.', TypeUse(CText[LAt], LScope), LScope);
      Check((LValue.Kind = ndtExpression) and Supports(LValue, INyxDocumentationDeclaredTypeExpression, LShape) and
        (LShape.Family = CFamily[LAt]) and (LShape.Definition.Expression.Text = CText[LAt]) and
        not LValue.AliasTarget.Defined and (LValue.BaseCount = 0) and (LValue.MemberCount = 0),
        'Compound definition preserves exact grammar and its nominal declaration ' + IntToStr(LAt));
      LCopy := LValue.WithExample(NyxDocumentationExample('Declaration', 'type TShape = ' + CText[LAt] + ';'))
        .WithRelated(Ref('TItem'));
      Check(Supports(LCopy, INyxDocumentationDeclaredTypeExpression, LCopied) and
        (LCopied.Definition = LShape.Definition) and (LValue.ExampleCount = 0),
        'Ordinary fluent copies retain immutable definition and prior readers ' + IntToStr(LAt));
      LFrozen := CaptureNyxDocumentationType(LCopy);
      Check(Supports(LFrozen, INyxDocumentationDeclaredTypeExpression, LCopied) and
        (LCopied.Family = CFamily[LAt]) and (LFrozen.ExampleCount = 1),
        'Capture preserves declared specialization ' + IntToStr(LAt));
    end;
    for LAction := 0 to 2 do
    begin
      LRefused := False;
      try
        case LAction of
          0: LValue.WithAlias(Ref('TItem'));
          1: LValue.WithBase(Ref('TItem'));
          2: LValue.WithMember(NewNyxDocumentationMember(NyxDocumentationMember(LValue.Identity, 'Invoke'), ndmMethod, ''));
        end;
      except
        on ENyxDocumentation do
          LRefused := True;
      end;
      Check(LRefused, 'Compound definitions cannot acquire invented alias/base/member meaning ' + IntToStr(LAction));
    end;
  finally
    LScope.Retire;
  end;
  Check(LShape.Definition.Expression.Text <> '', 'Definition remains inspectable after construction retirement');
end;

procedure RefusalCases;
const
  CWrong: array[0..5] of TNyxText = ('TItem', 'Some.TItem', 'TList<TItem>', 'file of TItem',
    'record Value:TItem; end', 'array of');
var
  LScope: INyxDocumentationTypeConstruction;
  LValue: INyxDocumentationType;
  LAt: Integer;
  LRefused: Boolean;
begin
  for LAt := 0 to High(CWrong) do
  begin
    LScope := Construction;
    LValue := nil;
    LRefused := False;
    try
      try
        LValue := NewNyxDocumentationDeclaredType(Ref('TWrong'), '', TypeUse(CWrong[LAt], LScope, False), LScope);
      except
        on ENyxDocumentation do
          LRefused := True;
        on ENyxDocumentationTypeExpression do
          LRefused := True;
      end;
      Check(LRefused and (LValue = nil), 'Unsupported or alias/body substitute refuses atomically ' + IntToStr(LAt));
    finally
      LScope.Retire;
    end;
  end;
  LScope := Construction;
  try
    for LAt := 0 to 3 do
    begin
      LValue := nil;
      LRefused := False;
      try
        case LAt of
          0: LValue := NewNyxDocumentationType(Ref('TWrong'), ndtExpression, '');
          1: LValue := NewNyxDocumentationDeclaredType(Ref('TWrong'), '', nil, LScope);
          2: LValue := NewNyxDocumentationDeclaredType(Ref('TWrong'), '', TypeUse('array of TItem', LScope, False, ndtoParameter), LScope);
          3: LValue := NewNyxDocumentationDeclaredType(NyxDocumentationUnit('mechanics.declarations'), '', TypeUse('array of TItem', LScope), LScope);
        end;
      except
        on ENyxDocumentation do
          LRefused := True;
      end;
      Check(LRefused and (LValue = nil), 'Missing shape, wrong role or unit identity refuses ' + IntToStr(LAt));
    end;
  finally
    LScope.Retire;
  end;
end;

function FreezeAlternate(const AValue: INyxDocumentationType): INyxDocumentationType;
var
  LOwner: TAlternateDeclared;
  LLease: INyxDocumentationType;
  LShape: INyxDocumentationDeclaredTypeExpression;
begin
  Supports(AValue, INyxDocumentationDeclaredTypeExpression, LShape);
  LOwner := TAlternateDeclared.Create(AValue);
  LLease := LOwner;
  LOwner.Use := LShape.Definition;
  LOwner.AdvertisedFamily := LShape.Family;
  Result := CaptureNyxDocumentationType(LLease);
end;

function RetiredLeaseValue: INyxDocumentationType;
var
  LOwner: TLeaseProbe;
  LCancel: INyxCancellationScope;
  LAdmission: INyxDocumentationTypeAdmission;
  LScope: INyxDocumentationTypeConstruction;
begin
  LOwner := TLeaseProbe.Create;
  LCancel := LOwner;
  LAdmission := LOwner;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults, LCancel, LAdmission);
  LCancel := nil;
  LAdmission := nil;
  try
    Result := NewNyxDocumentationDeclaredType(Ref('TRetained'), '', TypeUse('array of TItem', LScope), LScope);
  finally
    LScope.Retire;
  end;
end;

procedure CaptureCases;
var
  LBuild, LScope: INyxDocumentationTypeConstruction;
  LSource, LFrozen, LLease: INyxDocumentationType;
  LShape: INyxDocumentationDeclaredTypeExpression;
  LOwner: TAlternateDeclared;
  LPlain: TAlternateType;
  LAlternate: TAlternateUse;
  LUseLease: INyxDocumentationTypeUse;
  LCancel: INyxCancellationScope;
  LBefore, LAt: Integer;
  LRefused: Boolean;
begin
  LBuild := Construction;
  try
    LSource := NewNyxDocumentationDeclaredType(Ref('TSource'), 'Original.', TypeUse('function(A:TItem):TItem', LBuild), LBuild);
    Supports(LSource, INyxDocumentationDeclaredTypeExpression, LShape);
  finally
    LBuild.Retire;
  end;
  LBefore := GDisposed;
  LFrozen := FreezeAlternate(LSource);
  Check((GDisposed = LBefore + 1) and (LFrozen.Kind = ndtExpression), 'Captured declaration does not retain its alternate producer');
  LBefore := GLeasesDisposed;
  LFrozen := RetiredLeaseValue;
  Check((GLeasesDisposed = LBefore + 1) and (LFrozen.Kind = ndtExpression), 'Published definition retains no cancellation or admission lease');
  LPlain := TAlternateType.Create(LSource);
  LLease := LPlain;
  LFrozen := nil;
  LRefused := False;
  try
    LFrozen := CaptureNyxDocumentationType(LLease);
  except
    on ENyxDocumentation do
      LRefused := True;
  end;
  Check(LRefused and (LFrozen = nil), 'Expression kind without its capability refuses');
  for LAt := 0 to 6 do
  begin
    LOwner := TAlternateDeclared.Create(LSource);
    LLease := LOwner;
    LOwner.Use := LShape.Definition;
    LOwner.AdvertisedFamily := LShape.Family;
    case LAt of
      0: LOwner.AdvertisedFamily := ptxArray;
      1: begin LOwner.OverrideKind := True; LOwner.AdvertisedKind := ndtRecord; end;
      2: LOwner.Bases := 1;
      3: LOwner.Members := 129;
      4: begin
        LScope := Construction;
        try
          LOwner.ChangedUse := TypeUse('array of TItem', LScope);
        finally
          LScope.Retire;
        end;
      end;
      5: LOwner.MutateSummary := True;
      6: LOwner.Use := nil;
    end;
    LFrozen := nil;
    LRefused := False;
    try
      LFrozen := CaptureNyxDocumentationType(LLease);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LFrozen = nil) and (LOwner.BaseReads = 0) and (LOwner.MemberReads = 0),
      'Contradiction/stale envelope refuses before invalid row getters ' + IntToStr(LAt));
  end;
  LAlternate := TAlternateUse.Create(LShape.Definition);
  LUseLease := LAlternate;
  LAlternate.Count := 4097;
  LScope := Construction;
  LFrozen := nil;
  LRefused := False;
  try
    try
      LFrozen := NewNyxDocumentationDeclaredType(Ref('TInvalid'), '', LUseLease, LScope);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LFrozen = nil) and (LAlternate.Reads = 0), 'Binding count refuses before allocation or BindingAt');
  finally
    LScope.Retire;
  end;
  LAlternate.Count := -1;
  LAlternate.MutateAt := 2 * LShape.Definition.BindingCount;
  LScope := Construction;
  LFrozen := nil;
  LRefused := False;
  try
    try
      LFrozen := NewNyxDocumentationDeclaredType(Ref('TInvalid'), '', LUseLease, LScope);
    except
      on ENyxDocumentation do
        LRefused := True;
    end;
    Check(LRefused and (LFrozen = nil), 'Last binding mutation refuses the complete declared type');
  finally
    LScope.Retire;
  end;
  LOwner := TAlternateDeclared.Create(LSource);
  LLease := LOwner;
  LOwner.Use := LShape.Definition;
  LOwner.AdvertisedFamily := LShape.Family;
  LCancel := NewNyxCancellationScope;
  LOwner.Scope := LCancel;
  LFrozen := nil;
  LRefused := False;
  try
    LFrozen := CaptureNyxDocumentationType(LLease, LCancel);
  except
    on ENyxDocumentationCancelled do
      LRefused := True;
  end;
  Check(LRefused and (LFrozen = nil), 'Last definition observation cancellation uses the existing core exception class');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LOwner.DefinitionReads := 0;
  LOwner.FamilyReads := 0;
  LRefused := False;
  try
    LFrozen := CaptureNyxDocumentationType(LLease, LCancel);
  except
    on ENyxDocumentationCancelled do
      LRefused := True;
  end;
  Check(LRefused and (LFrozen = nil) and (LOwner.DefinitionReads = 0) and (LOwner.FamilyReads = 0),
    'Pre-cancelled core capture refuses before optional capability getters');
  LOwner.Scope := nil;
  LOwner.DefinitionReads := 0;
  LOwner.FamilyReads := 0;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Work(1));
  LFrozen := nil;
  LRefused := False;
  try
    try
      LFrozen := CaptureNyxDocumentationType(LLease, LScope);
    except
      on ENyxDocumentationTypeExpression do
        LRefused := True;
    end;
    Check(LRefused and (LFrozen = nil) and (LOwner.DefinitionReads = 1) and (LOwner.FamilyReads = 0),
      'Shared work refuses before later source capability getters');
  finally
    LScope.Retire;
  end;
end;

procedure SharedStorageCases;
var
  LBuild, LScope: INyxDocumentationTypeConstruction;
  LUse, LIndependent: INyxDocumentationTypeUse;
  LValue: INyxDocumentationType;
  LRefused: Boolean;
  LBytes: Integer;
  LAlternate: TAlternateUse;
  LLease: INyxDocumentationTypeUse;
begin
  LBuild := Construction;
  try
    LUse := TypeUse('array of TItem', LBuild);
    LIndependent := TypeUse('array of TItem', LBuild);
  finally
    LBuild.Retire;
  end;
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Nodes(2));
  try
    LValue := NewNyxDocumentationDeclaredType(Ref('TFirst'), '', LUse, LScope);
    LValue := NewNyxDocumentationDeclaredType(Ref('TSecond'), '', LUse, LScope);
    Check(LValue.Kind = ndtExpression, 'Exact expression owner reuses captured storage under the same scope');
    LValue := nil;
    LRefused := False;
    try
      LValue := NewNyxDocumentationDeclaredType(Ref('TThird'), '', LIndependent, LScope);
    except
      on ENyxDocumentationTypeExpression do
        LRefused := True;
    end;
    Check(LRefused and (LValue = nil), 'Independent equal expression consumes its full retained node allowance');
  finally
    LScope.Retire;
  end;
  { This is the exact newly copied field sum, not a measured first-fit search.
    The second declaration must consume this same remaining byte allowance. }
  LBytes := NyxUTF8ByteCount(Ref('TFirst').Key) + NyxUTF8ByteCount('array of TItem') + NyxUTF8ByteCount(Ref('TItem').Key);
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(LBytes));
  LAlternate := TAlternateUse.Create(LUse);
  LLease := LAlternate;
  try
    LValue := NewNyxDocumentationDeclaredType(Ref('TFirst'), '', LUse, LScope);
    Check(LValue.Kind = ndtExpression, 'First declaration fits its exact copied metadata allowance');
    LValue := nil;
    LRefused := False;
    try
      LValue := NewNyxDocumentationDeclaredType(Ref('TSecond'), '', LLease, LScope);
    except
      on ENyxDocumentationTypeExpression do
        LRefused := True;
    end;
    Check(LRefused and (LValue = nil) and (LAlternate.Reads = 0),
      'Shared byte capacity refuses before later declaration binding reads');
  finally
    LScope.Retire;
  end;
end;

function Frame(const AValue: TNyxText): TNyxText;
begin
  Result := IntToStr(NyxUTF8ByteCount(AValue)) + ':' + AValue;
end;

function Bundle(ABound: Boolean; const AText: TNyxText): INyxDocumentationRegistration;
var
  LScope: INyxDocumentationTypeConstruction;
  LDeclarations: TNyxDocumentationDeclarations;
begin
  LScope := Construction;
  try
    SetLength(LDeclarations, 2);
    LDeclarations[0] := NewNyxDocumentationType(Ref('TItem'), ndtInterface, 'Element.');
    LDeclarations[1] := NewNyxDocumentationDeclaredType(Ref('TValues'), 'Values.', TypeUse(AText, LScope, ABound), LScope);
    Result := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.declarations'), Ref('TValues'), LDeclarations);
  finally
    LScope.Retire;
  end;
end;

{ These complete controlled Pascal units exercise source binding and registration,
  using explicitly authored facts. They are not archived SDK input or evidence
  that the real native package's missing inputs/facts have been obtained. }
procedure CompilerRootSourceCases;
const
  CSource: TNyxText = 'unit RootSlots; interface uses System; type TFlag = Boolean; ' +
    'TKeyword = type String(1252); TArray = array of String(1252); TShort = ShortString; ' +
    'function Echo(const A:TFlag):Boolean; implementation ' +
    'function Echo(const A:TFlag):Boolean; begin Result:=A; end; end.';
var
  LRoot, LContext: INyxPascalCompilerContext;
  LFiles: INyxPascalFileCatalog;
  LWorld: INyxPascalSemanticWorld;
  LScopes: TNyxDocumentationSourceScopes;
  LCreators: TNyxDocumentationSourceCreators;
  LScope: INyxDocumentationTypeConstruction;
  LPackage, LPrevious: INyxDocumentationPackage;
  LAlias, LUnit: INyxDocumentationType;
  LDefinition: INyxDocumentationAliasDefinition;
  LRootMap, LResultMap: INyxDocumentationCompilerRootBindings;
  LSignature: INyxDocumentationMemberSignature;
  LShape: INyxDocumentationDeclaredTypeExpression;
  LBinding: INyxDocumentationRegistration;
  LRegistry: TNyxDocumentationRegistry;
  LCatalog: TNyxCatalog;
  LIndex: INyxDocumentationIndex;
  LFailed: Boolean;
  LAt: Integer;
  procedure Add(const AName, AText: TNyxText; AMode: TNyxPascalLanguageMode);
  var
    LView: INyxPascalTranslatedSyntax;
  begin
    LContext := NyxPascalWithBootstrapContext(NewNyxPascalCompilerContext(ppcFPC,
      TNyxPascalCompilerVersion.Create(3, 3, 1), AMode).WithOption('H', ppkFalse), LRoot);
    LFiles := NewNyxPascalFileCatalog(pfcExact).WithFile(NyxPascalPhysicalFile(AName + '.pas'), AText);
    LView := NewNyxPascalTranslatedSyntax(NewNyxPascalTranslation(LFiles,
      NyxPascalPhysicalFile(AName + '.pas'), LContext, TNyxPascalTranslationOptions.Defaults),
      TNyxPascalSyntaxOptions.Defaults);
    LWorld := LWorld.WithModule(LView);
  end;
  procedure MakeWorld(AKnown, AAnsiKnown: Boolean);
  begin
    LRoot := NewNyxPascalCompilerContext(ppcFPC,
      TNyxPascalCompilerVersion.Create(3, 3, 1), ppmFPC, pcsoSystemBootstrap).WithOption('H', ppkFalse);

    if AKnown then
    begin
      LRoot := NyxPascalWithCapturedSize(LRoot, NyxPascalSizeReference('Boolean'),
        NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 1));
      LRoot := NyxPascalWithCapturedSize(LRoot, NyxPascalSizeReference('ShortString'),
        NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 256));
    end;

    if AAnsiKnown then
      LRoot := NyxPascalWithCapturedSize(LRoot, NyxPascalSizeReference('AnsiString'),
        NyxPascalSizeFact(pssKnown, ppmFPC, ppkFalse, 4));
    LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits));
    Add('System', 'unit System; interface implementation end.', ppmFPC);
    Add('ObjPas', 'unit ObjPas; interface implementation end.', ppmObjFPC);
    Add('RootSlots', CSource, ppmDelphi);
  end;
begin
  LCreators := nil;
  MakeWorld(True, True);
  SetLength(LScopes, 5);
  LScopes[0].ModuleIndex := 2;
  LScopes[0].DeclarationIndex := -1;
  LScopes[1].ModuleIndex := 2;
  LScopes[1].DeclarationIndex := LWorld.ModuleAt(2).Syntax.Named(0, 'TFlag');
  LScopes[2].ModuleIndex := 2;
  LScopes[2].DeclarationIndex := LWorld.ModuleAt(2).Syntax.Named(0, 'TKeyword');
  LScopes[3].ModuleIndex := 2;
  LScopes[3].DeclarationIndex := LWorld.ModuleAt(2).Syntax.Named(0, 'TArray');
  LScopes[4].ModuleIndex := 2;
  LScopes[4].DeclarationIndex := LWorld.ModuleAt(2).Syntax.Named(0, 'TShort');
  LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
  try
    LPackage := CaptureNyxDocumentationSourcePackage(LWorld, LScopes, LCreators, [], LScope);
  finally
    LScope.Retire;
  end;
  LAlias := LPackage.FindDeclaration(NyxDocumentationType('RootSlots', 'TFlag'));
  LUnit := LPackage.FindDeclaration(NyxDocumentationUnit('RootSlots'));
  Check((LPackage.DeclarationCount = 5) and (LUnit.MemberCount = 1) and
    Supports(LAlias, INyxDocumentationAliasDefinition, LDefinition) and
    Supports(LDefinition.Definition, INyxDocumentationCompilerRootBindings, LRootMap) and
    (LRootMap.RootAt(0) <> nil), 'Owned controlled source captures complete globals and a genuine root-terminal alias');
  Check(Supports(LUnit.MemberAt(0), INyxDocumentationMemberSignature, LSignature) and
    Supports(LSignature.DeclaredSignature.ResultUse, INyxDocumentationCompilerRootBindings, LResultMap) and
    (LRootMap.RootAt(0) = LResultMap.RootAt(0)), 'Repeated source root uses share one exact context/name owner within the operation');
  Check((LRootMap.RootAt(0).Facts.SameFacts(LRoot)) and (LUnit.MemberAt(0).Signature = nil),
    'Root provenance remains exact while the old named-only signature stays explicitly absent');
  Check(Supports(LPackage.FindDeclaration(NyxDocumentationType('RootSlots', 'TKeyword')),
    INyxDocumentationDeclaredTypeExpression, LShape) and (LShape.Family = ptxDistinct) and
    Supports(LShape.Definition, INyxDocumentationCompilerRootBindings, LResultMap) and
    (LResultMap.RootAt(0).Name.Name = 'ansistring'),
    'Distinct String(codepage) retains its actual encoded parent and ANSI base under H-');
  Check(Supports(LPackage.FindDeclaration(NyxDocumentationType('RootSlots', 'TArray')),
    INyxDocumentationDeclaredTypeExpression, LShape) and (LShape.Family = ptxArray) and
    Supports(LShape.Definition, INyxDocumentationCompilerRootBindings, LResultMap) and
    (LResultMap.RootAt(0).Name.Name = 'ansistring'),
    'Array String(codepage) uses the local encoded-string parent rather than whole-expression kind');
  Check(Supports(LPackage.FindDeclaration(NyxDocumentationType('RootSlots', 'TShort')),
    INyxDocumentationAliasDefinition, LDefinition) and
    Supports(LDefinition.Definition, INyxDocumentationCompilerRootBindings, LResultMap) and
    (LResultMap.RootAt(0).Name.Name = 'shortstring'),
    'Explicit ShortString remains a separate root from the encoded ANSI base under the same H- context');
  LBinding := NewNyxDocumentationBinding(LPackage, LAlias.Identity);
  LCatalog := TNyxCatalog.Create;
  LRegistry := TNyxDocumentationRegistry.Create;
  try
    LCatalog.RegisterKind('controlled-source-root', 'Controlled root', 'Mechanics', False, LBinding);
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, 0);
    Check((LIndex.DeclarationCount = 5) and (LIndex.FindDeclaration(LAlias.Identity) <> nil),
      'Real registration/index consumes the controlled source package without a fabricated System declaration');
  finally
    LRegistry.Free;
    LCatalog.Free;
  end;
  LPrevious := LPackage;
  for LAt := 0 to 2 do
  begin
    MakeWorld(LAt = 2, False);

    if LAt = 1 then
      LWorld := NewNyxPascalSemanticWorld(TNyxPascalBindingOptions.Defaults.WithImplicitScopes(pispCompilerUnits))
        .WithModule(LWorld.ModuleAt(2));
    LScopes[0].ModuleIndex := LWorld.Count - 1;
    LScopes[1].ModuleIndex := LScopes[0].ModuleIndex;
    LScopes[2].ModuleIndex := LScopes[0].ModuleIndex;
    LScopes[3].ModuleIndex := LScopes[0].ModuleIndex;
    LScopes[4].ModuleIndex := LScopes[0].ModuleIndex;
    LPackage := nil;
    LFailed := False;
    LScope := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults);
    try
      try
        LPackage := CaptureNyxDocumentationSourcePackage(LWorld, LScopes, LCreators, [], LScope);
      except
        on ENyxDocumentationSource do LFailed := True;
      end;
    finally
      LScope.Retire;
    end;
    Check(LFailed and (LPackage = nil) and (LPrevious.DeclarationCount = 5),
      'Unknown Boolean/ANSI root or absent imported source refuses complete capture, retaining prior package ' + IntToStr(LAt));
  end;
end;

{ Native hidden interface-result temporaries retire at this lexical boundary;
  the caller verifies actual policy destruction only after this procedure. }
procedure CaptureReleasedRootInputs(const AFacts: INyxPascalCompilerContext;
  const AName: TNyxPascalSizeReference; AFamily: TNyxDocumentationIntrinsicFamily;
  out ARoot: INyxDocumentationCompilerRoot);
var
  LPolicy: TRootInputAdmission;
  LAdmission: INyxDocumentationTypeAdmission;
begin
  LPolicy := TRootInputAdmission.Create;
  LAdmission := LPolicy;
  LPolicy.RootInput := True;
  GRootInputFacts := AFacts;
  GRootInputScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LAdmission);
  ARoot := NewNyxDocumentationCompilerRoot(AName, AFamily,
    GRootInputFacts, GRootInputScope);
  Check((GRootInputFacts = nil) and (GRootInputScope = nil) and (ARoot.Facts = AFacts),
    'Last caller context/construction slots can retire during admission without invalidating local root leases');
end;

procedure CompilerRootCases;
var
  LScope, LSmall: INyxDocumentationTypeConstruction;
  LContext, LChangedContext: INyxPascalCompilerContext;
  LRoot, LOther, LRootSupplier: INyxDocumentationCompilerRoot;
  LUse, LClear, LNominal, LChangedUse, LAlternateUse: INyxDocumentationTypeUse;
  LPlainArrayUse, LClearArrayUse: INyxDocumentationTypeUse;
  LRoots: INyxDocumentationCompilerRootBindings;
  LFormalMap: INyxDocumentationFormalBindings;
  LGenerics: INyxDocumentationGenericParameters;
  LNames: TNyxDocumentationGenericNames;
  LAlias, LCopy, LCaptured, LReplacement: INyxDocumentationType;
  LPlainArray, LClearArray: INyxDocumentationType;
  LAliasDefinition: INyxDocumentationAliasDefinition;
  LAlternate: TAlternateRoot;
  LAlternateMap: TAlternateRootUse;
  LAlternateAlias: TAlternateRootAlias;
  LDeclarations: TNyxDocumentationDeclarations;
  LInterfaces: array of INyxDocumentationTypeUse;
  LRegistration, LOldRegistration: INyxDocumentationRegistration;
  LRegistry: TNyxDocumentationRegistry;
  LCatalog, LBad: TNyxCatalog;
  LIndex, LPrevious: INyxDocumentationIndex;
  LFailure: Boolean;
  LBytes, LVersion, LLegacyWork: Integer;
  LInputPolicy: TRootInputAdmission;
  LInputAdmission: INyxDocumentationTypeAdmission;
  LDisposedBefore: Integer;
  LKnownHelperContent: TNyxText;
begin
  { Controlled values prove metadata mechanics only. They are not measured
    compiler facts and never replace the native source-package prerequisites. }
  LContext := NewNyxPascalCompilerContext(ppcFPC,
    TNyxPascalCompilerVersion.Create(3, 3, 1), ppmDelphi, pcsoSystemBootstrap);
  LScope := Construction;
  LRoot := NewNyxDocumentationCompilerRoot(NyxPascalSizeReference('Boolean'), ndifBoolean, LContext, LScope);
  Check((LRoot.Name.Name = NyxPascalSizeReference('Boolean').Name) and
    (LRoot.Family = ndifBoolean) and (LRoot.Facts = LContext) and
    (NyxDocumentationCompilerRootContent(LRoot) <> ''), 'Root metadata keeps exact supplied context and explicit family');
  LChangedContext := NyxPascalWithCompilerPolicies(LContext,
    NyxPascalCompilerPolicies(LContext).WithMultipleHelpers(ppkFalse));
  LOther := NewNyxDocumentationCompilerRoot(LRoot.Name, LRoot.Family, LChangedContext, LScope);
  LKnownHelperContent := NyxDocumentationCompilerRootContent(LOther);
  Check(LKnownHelperContent <> NyxDocumentationCompilerRootContent(LRoot),
    'Declared known-off multiple-helper provenance participates in detached root context framing');
  LChangedContext := NyxPascalWithCompilerPolicies(LContext,
    NyxPascalCompilerPolicies(LContext).WithMultipleHelpers(ppkTrue));
  LOther := NewNyxDocumentationCompilerRoot(LRoot.Name, LRoot.Family, LChangedContext, LScope);
  Check((NyxDocumentationCompilerRootContent(LOther) <> LKnownHelperContent) and
    (NyxDocumentationCompilerRootContent(LOther) <> NyxDocumentationCompilerRootContent(LRoot)),
    'Known-on, known-off and absent helper policy retain three distinct documentation-root contents');
  LBytes := NyxUTF8ByteCount(LRoot.Name.Name) + 2 * NyxUTF8ByteCount(NyxDocumentationCompilerRootContent(LRoot));
  LSmall := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(LBytes));
  LOther := NewNyxDocumentationCompilerRoot(LRoot.Name, LRoot.Family, LContext, LSmall);
  Check(NyxDocumentationCompilerRootContent(LOther) = NyxDocumentationCompilerRootContent(LRoot),
    'Exact capacity admits every temporary and final context-stream byte before formatting');
  LFailure := False;
  try
    NewNyxDocumentationCompilerRoot(LRoot.Name, LRoot.Family, LContext, LSmall);
  except
    on ENyxDocumentationTypeExpression do LFailure := True;
  end;
  Check(LFailure and (LOther.Facts = LContext), 'Second independent context capture cannot reset the shared byte allowance');
  LSmall.Retire;
  LSmall := NewNyxDocumentationTypeConstruction(TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(LBytes - 1));
  LFailure := False;
  try
    NewNyxDocumentationCompilerRoot(LRoot.Name, LRoot.Family, LContext, LSmall);
  except
    on ENyxDocumentationTypeExpression do LFailure := True;
  end;
  Check(LFailure, 'One byte below exact context capacity refuses before final formatting/join');
  LSmall.Retire;
  LUse := WithNyxDocumentationCompilerRootBinding(TypeUse('Boolean', LScope, False), 0, LRoot, LScope);
  Check(Supports(LUse, INyxDocumentationCompilerRootBindings, LRoots) and (LRoots.RootAt(0) = LRoot) and
    not LUse.Reference.Defined and not LUse.BindingAt(0).Defined,
    'Explicit root does not manufacture an old nominal declaration');
  LClear := WithNyxDocumentationCompilerRootBinding(LUse, 0, nil, LScope);
  Check(Supports(LClear, INyxDocumentationCompilerRootBindings, LRoots) and (LRoots.RootAt(0) = nil),
    'Root clear restores the unbound slot while old immutable use stays readable');
  LPlainArrayUse := TypeUse('array of Boolean', LScope, False);
  LClearArrayUse := WithNyxDocumentationCompilerRootBinding(LPlainArrayUse, 0, LRoot, LScope);
  LClearArrayUse := WithNyxDocumentationCompilerRootBinding(LClearArrayUse, 0, nil, LScope);
  LPlainArray := NewNyxDocumentationDeclaredType(Ref('TClearArray'), '', LPlainArrayUse, LScope);
  LClearArray := NewNyxDocumentationDeclaredType(Ref('TClearArray'), '', LClearArrayUse, LScope);
  LSmall := Construction;
  LCaptured := CaptureNyxDocumentationType(LPlainArray, LSmall);
  LLegacyWork := LSmall.Receipt.Total;
  LSmall.Retire;
  LSmall := Construction;
  LCaptured := CaptureNyxDocumentationType(LClearArray, LSmall);
  Check(LSmall.Receipt.Total = LLegacyWork, 'Cleared all-nil root vector restores the exact absent capture work receipt');
  LSmall.Retire;
  SetLength(LDeclarations, 1);
  LDeclarations[0] := LPlainArray;
  LRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.declarations'), Ref('TClearArray'), LDeclarations);
  LDeclarations[0] := LClearArray;
  LOldRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.declarations'), Ref('TClearArray'), LDeclarations);
  Check(LRegistration.DeclarationContentAt(0) = LOldRegistration.DeclarationContentAt(0),
    'Cleared root metadata restores byte-identical old optional-absence framing');
  LNominal := LUse.WithBinding(0, Ref('TItem'));
  Check(Supports(LNominal, INyxDocumentationCompilerRootBindings, LRoots) and (LRoots.RootAt(0) = nil) and
    LNominal.BindingAt(0).Same(Ref('TItem')), 'Nominal replacement clears a root without losing exact syntax');
  SetLength(LNames, 1);
  LNames[0] := 'T';
  LGenerics := NewNyxDocumentationGenericParameters(NyxDocumentationType('mechanics.declarations', 'TBox', 1), LNames, LScope);
  LChangedUse := WithNyxDocumentationFormalBinding(TypeUse('T', LScope, False), 0, LGenerics.ParameterAt(0), LScope);
  LChangedUse := WithNyxDocumentationCompilerRootBinding(LChangedUse, 0, LRoot, LScope);
  Check(Supports(LChangedUse, INyxDocumentationFormalBindings, LFormalMap) and (LFormalMap.FormalAt(0) = nil),
    'A root replacement clears a scoped formal without inferring a spelling identity');
  LChangedUse := WithNyxDocumentationFormalBinding(LChangedUse, 0, LGenerics.ParameterAt(0), LScope);
  Check(Supports(LChangedUse, INyxDocumentationCompilerRootBindings, LRoots) and (LRoots.RootAt(0) = nil),
    'Formal replacement clears root provenance');
  LFailure := False;
  try
    WithNyxDocumentationCompilerRootBinding(TypeUse('TBox<T>', LScope, False), 0, LRoot, LScope);
  except
    on ENyxDocumentation do LFailure := True;
  end;
  Check(LFailure, 'Generic designator cannot be relabeled as a compiler-root leaf');
  LAlias := NewNyxDocumentationAliasDefinition(Ref('TRootAlias'), 'Explicit supplied root.', LUse, LScope);
  Check((LAlias.Kind = ndtAlias) and not LAlias.AliasTarget.Defined and
    Supports(LAlias, INyxDocumentationAliasDefinition, LAliasDefinition) and
    (LAliasDefinition.Definition.Expression.Text = 'Boolean'), 'Root alias preserves declared alias kind and exact source expression');
  LCopy := LAlias.WithExample(NyxDocumentationExample('Read metadata', 'Root.Facts;')).WithRelated(Ref('TItem'));
  Check(Supports(LCopy, INyxDocumentationAliasDefinition, LAliasDefinition) and
    (LAliasDefinition.Definition <> nil), 'Fluent prose copying retains specialized root alias');
  LCaptured := CaptureNyxDocumentationType(LAlias, LScope);
  Check(Supports(LCaptured, INyxDocumentationAliasDefinition, LAliasDefinition) and
    (LAliasDefinition.Definition <> nil), 'Type capture preserves the optional exact alias definition');
  LReplacement := LAlias.WithAlias(Ref('TItem'));
  Check(Supports(LReplacement, INyxDocumentationAliasDefinition, LAliasDefinition) and
    (LAliasDefinition.Definition = nil) and LReplacement.AliasTarget.Same(Ref('TItem')),
    'Explicit alias replacement clears stale root metadata');
  LAlternate := TAlternateRoot.Create;
  LRootSupplier := LAlternate;
  LAlternate.Value := LRoot;
  LAlternate.MutateAt := 2;
  LFailure := False;
  try
    WithNyxDocumentationCompilerRootBinding(LClear, 0, LRootSupplier, LScope);
  except
    on ENyxDocumentation do LFailure := True;
  end;
  Check(LFailure and (LAlternate.NameReads >= 2), 'Late alternate root-name mutation refuses immutable publication');
  LAlternateMap := TAlternateRootUse.Create(LUse);
  LAlternateUse := LAlternateMap;
  LAlternateMap.Root := LRoot;
  LAlternateMap.ClearAt := 2;
  LFailure := False;
  try
    NewNyxDocumentationAliasDefinition(Ref('TLate'), '', LAlternateUse, LScope);
  except
    on ENyxDocumentation do LFailure := True;
  end;
  Check(LFailure and (LAlternateMap.RootReads >= 2), 'Final root value-to-nil topology change refuses capture');
  LAlternateMap := TAlternateRootUse.Create(LClear);
  LAlternateUse := LAlternateMap;
  LAlternateMap.Root := LRoot;
  LAlternateMap.PublishAt := 2;
  LFailure := False;
  try
    NewNyxDocumentationAliasDefinition(Ref('TLateAppearance'), '', LAlternateUse, LScope);
  except
    on ENyxDocumentation do LFailure := True;
  end;
  Check(LFailure and (LAlternateMap.RootReads >= 2), 'Final nil-to-root topology change cannot acquire late authority');
  LAlternateAlias := TAlternateRootAlias.Create(LAlias);
  LCopy := LAlternateAlias;
  LAlternateAlias.Use := LUse;
  LAlternateAlias.ClearAt := 2;
  LFailure := False;
  try
    CaptureNyxDocumentationType(LCopy, LScope);
  except
    on ENyxDocumentation do LFailure := True;
  end;
  Check(LFailure, 'Late alias definition disappearance refuses capture');
  LChangedContext := LContext.WithSymbol(NyxPascalDefine('CONFIGURED_DIFFERENCE'), ppdInteger, 7);
  LOther := NewNyxDocumentationCompilerRoot(LRoot.Name, LRoot.Family, LChangedContext, LScope);
  Check(NyxDocumentationCompilerRootContent(LOther) <> NyxDocumentationCompilerRootContent(LRoot),
    'Same family/version/name with a different exact context remains different equality data');
  SetLength(LDeclarations, 1);
  LDeclarations[0] := LAlias;
  LRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.declarations'), LAlias.Identity, LDeclarations, [], LScope);
  Check(Pos('compiler-root-binding-v1', LRegistration.DeclarationContentAt(0)) > 0,
    'Root context and alias shape participate in bounded registration equality');
  LDeclarations[0] := NewNyxDocumentationDeclaredType(Ref('TUnbound'), '', TypeUse('array of Boolean', LScope, False), LScope);
  LOldRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.declarations'), Ref('TUnbound'), LDeclarations, [], LScope);
  LRegistry := TNyxDocumentationRegistry.Create;
  LCatalog := TNyxCatalog.Create;
  LBad := TNyxCatalog.Create;
  try
    LCatalog.RegisterKind('explicit-root', 'Root', 'Mechanics', False, LRegistration);
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, 0);
    Check((LIndex.DeclarationCount = 1) and (LIndex.Members(LAlias.Identity).Count = 0),
      'Real immutable index accepts a terminal root alias without a fabricated System leaf');
    LPrevious := LIndex;
    LVersion := LRegistry.Version;
    LBad.RegisterKind('unbound-root', 'Unbound', 'Mechanics', False, LOldRegistration);
    LFailure := False;
    try
      LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LBad.Capture, LVersion);
    except
      on ENyxDocumentation do LFailure := True;
    end;
    Check(LFailure and (LRegistry.Current = LPrevious) and (LRegistry.Version = LVersion),
      'Adding a root channel never weakens missing ordinary named closure or atomic prior-index refusal');
    LBad.Free;
    LBad := TNyxCatalog.Create;
    SetLength(LDeclarations, 2);
    LDeclarations[0] := LAlias;
    LDeclarations[1] := NewNyxDocumentationType(Ref('TWrongBase'), ndtClass, '').WithBase(LAlias.Identity);
    LBad.RegisterKind('root-base', 'Bad base', 'Mechanics', False,
      NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.declarations'),
        Ref('TWrongBase'), LDeclarations));
    LFailure := False;
    try
      LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LBad.Capture, LVersion);
    except
      on ENyxDocumentation do LFailure := True;
    end;
    Check(LFailure and (LRegistry.Current = LPrevious) and (LRegistry.Version = LVersion),
      'Terminal compiler-root alias cannot become a class/interface base');
    LBad.Free;
    LBad := TNyxCatalog.Create;
    SetLength(LInterfaces, 1);
    LInterfaces[0] := WithNyxDocumentationCompilerRootBinding(
      TypeUse('Boolean', LScope, False, ndtoImplementedInterface), 0, LRoot, LScope);
    SetLength(LDeclarations, 1);
    LDeclarations[0] := WithNyxDocumentationImplementedInterfaces(
      NewNyxDocumentationType(Ref('TWrongInterface'), ndtClass, ''),
      NewNyxDocumentationInterfaceImplementations(LInterfaces, LScope), LScope);
    LBad.RegisterKind('root-interface', 'Bad interface', 'Mechanics', False,
      NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.declarations'),
        Ref('TWrongInterface'), LDeclarations));
    LFailure := False;
    try
      LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LBad.Capture, LVersion);
    except
      on ENyxDocumentation do LFailure := True;
    end;
    Check(LFailure and (LRegistry.Current = LPrevious) and (LRegistry.Version = LVersion),
      'Compiler-root occurrence cannot be the nominal implemented-interface root');
  finally
    LBad.Free;
    LCatalog.Free;
    LRegistry.Free;
  end;
  LSmall := Construction;
  LAlternate := TAlternateRoot.Create;
  LRootSupplier := LAlternate;
  LAlternate.Value := LRoot;
  LAlternate.Scope := LSmall;
  LFailure := False;
  try
    WithNyxDocumentationCompilerRootBinding(LClear, 0, LRootSupplier, LSmall);
  except
    on ENyxDocumentationCancelled do LFailure := True;
  end;
  Check(LFailure and (LRoot.Facts = LContext), 'Alternate root getter cancellation publishes no value and retains prior detached facts');
  LSmall.Retire;
  LScope.Retire;
  Check(LRoot.Facts = LContext, 'Published root retains no retired construction dependency');
  LDisposedBefore := GRootInputDisposed;
  CaptureReleasedRootInputs(LContext, LRoot.Name, LRoot.Family, LOther);
  Check(GRootInputDisposed = LDisposedBefore + 1, 'Published root retains no admission object after caller-slot release');
  LInputPolicy := TRootInputAdmission.Create;
  LInputAdmission := LInputPolicy;
  LInputPolicy.AliasInput := True;
  LSmall := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults, nil, LInputAdmission);
  GAliasIdentity := Ref('TCapturedInput');
  GAliasSummary := 'Originally admitted.';
  LCopy := NewNyxDocumentationAliasDefinition(GAliasIdentity, GAliasSummary, LUse, LSmall);
  Check(LCopy.Identity.Same(Ref('TCapturedInput')) and (LCopy.Summary = 'Originally admitted.') and
    GAliasIdentity.Same(Ref('TReplacedCallerSlot')), 'Alias input record/text slots are leased before the first outward callback');
  LSmall.Retire;
  LSmall := nil;
  LInputAdmission := nil;
  GAliasIdentity := Default(TNyxDocumentationRef);
  GAliasSummary := '';
end;

procedure IndexCases;
var
  LRegistry: TNyxDocumentationRegistry;
  LCatalog, LBad: TNyxCatalog;
  LIndex, LBefore: INyxDocumentationIndex;
  LShape: INyxDocumentationDeclaredTypeExpression;
  LRegistration: INyxDocumentationRegistration;
  LDeclarations: TNyxDocumentationDeclarations;
  LExpected: TNyxText;
  LVersion, LAt: Integer;
  LRefused: Boolean;
begin
  SetLength(LDeclarations, 1);
  LDeclarations[0] := NewNyxDocumentationType(Ref('TPlain'), ndtRecord, 'Plain.');
  LRegistration := NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.declarations'), Ref('TPlain'), LDeclarations);
  LExpected := Frame(Ref('TPlain').Key) + Frame('2') + Frame('Plain.') + Frame('0') + Frame('') +
    Frame('0') + Frame('0') + Frame('0') + Frame('0');
  Check(LRegistration.DeclarationContentAt(0) = LExpected, 'Absent capability keeps exact legacy declaration framing');
  LRegistration := Bundle(True, 'array of TItem');
  Check(Pos('declared-type-expression', LRegistration.DeclarationContentAt(1)) > 0, 'Present definition participates in framing');
  LRegistry := TNyxDocumentationRegistry.Create;
  LCatalog := TNyxCatalog.Create;
  LBad := TNyxCatalog.Create;
  try
    LCatalog.RegisterKind('declared-values', 'Values', 'Custom', False, LRegistration);
    LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LCatalog.Capture, 0);
    Check((LIndex.DeclarationCount = 2) and Supports(LIndex.FindDeclaration(Ref('TValues')),
      INyxDocumentationDeclaredTypeExpression, LShape) and (LShape.Family = ptxArray),
      'Real registration discovers the complete supplied expression closure');
    LBefore := LIndex;
    LVersion := LRegistry.Version;
    for LAt := 0 to 2 do
    begin
      LBad.Free;
      LBad := TNyxCatalog.Create;
      case LAt of
        0: LBad.RegisterKind('unbound', 'Unbound', 'Custom', False, Bundle(False, 'array of TItem'));
        1: begin
          LBad.RegisterKind('first', 'First', 'Custom', False, LRegistration);
          LBad.RegisterKind('second', 'Second', 'Custom', False, Bundle(True, 'set of TItem'));
        end;
        2: begin
          LDeclarations[0] := NewNyxDocumentationType(Ref('TAlias'), ndtAlias, 'No target.');
          LBad.RegisterKind('alias', 'Alias', 'Custom', False,
            NewNyxDocumentationRegistration(NyxDocumentationUnit('mechanics.declarations'), Ref('TAlias'), LDeclarations));
        end;
      end;
      LIndex := nil;
      LRefused := False;
      try
        LIndex := LRegistry.Refresh(CaptureNyxSchemas, CaptureNyxCallbacks, LBad.Capture, LVersion);
      except
        on ENyxDocumentation do
          LRefused := True;
      end;
      Check(LRefused and (LIndex = nil) and (LRegistry.Current = LBefore) and (LRegistry.Version = LVersion),
        'Unbound/conflicting/targetless alias refusal preserves prior index/version ' + IntToStr(LAt));
    end;
  finally
    LBad.Free;
    LCatalog.Free;
    LRegistry.Free;
  end;
end;

{$ifndef PAS2JS}
function RepositoryArgument: String;
begin

  if ParamCount <> 1 then
    raise EArgumentException.Create('Usage: nyx_documentation_declarationexpressions_tests <repository-root>');
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));

  if not DirectoryExists(Result) or not FileExists(Result + 'PROJECT.md') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.events.pas') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.behavior.pas') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.state.pas') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.binding.types.pas') or
    not FileExists(Result + 'src' + PathDelim + 'nyx.types.pas') then
    raise EArgumentException.Create('Repository root must contain PROJECT.md and all five required core source units');
end;

function SourceSyntax(const APath: String): INyxPascalSyntaxSnapshot;
var
  LStream: TMemoryStream;
  LBytes: TNyxBytes;
  LOptions: TNyxPascalSyntaxOptions;
begin
  Result := nil;
  LOptions := TNyxPascalSyntaxOptions.Defaults;
  LStream := TMemoryStream.Create;
  try
    LStream.LoadFromFile(APath);

    if LStream.Size > LOptions.LexicalOptions.MaximumSourceBytes then
      raise Exception.Create('Required source exceeds the existing source allowance');
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) > 0 then
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  Result := NewNyxPascalSyntaxSnapshot(NyxDecodeUTF8(LBytes), LOptions);
end;

procedure ActualDeclaration(const ASource: INyxPascalSyntaxSnapshot;
  const AUnitName, AName: TNyxText; AFamily: TNyxPascalTypeKind; AMethod: Boolean = False);
var
  LAt, LLink: Integer;
  LDeclaration: TNyxPascalSyntaxDeclaration;
  LScope: INyxDocumentationTypeConstruction;
  LText: TNyxText;
  LValue: INyxDocumentationType;
  LShape: INyxDocumentationDeclaredTypeExpression;
  LUse: INyxDocumentationTypeUse;
  LDetached: INyxPascalDeclaredTypeSyntax;
begin
  Check(ASource.Scope(0).Name = AUnitName, 'Actual physical module retains its declared unit name');
  LAt := ASource.Named(0, AName);
  Check(LAt >= 0, 'Required actual compound type exists: ' + AName);
  LDeclaration := ASource.Declaration(LAt);
  Check((LDeclaration.Kind = psdkType) and
    ((LDeclaration.Flags * [psdfIncomplete, psdfIncompleteHeader, psdfUnsupported, psdfConditional, psdfForward]) = []),
    'Required actual type has a complete checked declaration window: ' + AName);
  LScope := Construction;
  try
    LText := CopyNyxDocumentationTypeTextWindow(ASource.Lexical.Source, LDeclaration.TypeSpan, LScope);
    LUse := TypeUse(LText, LScope, False);
    LValue := NewNyxDocumentationDeclaredType(NyxDocumentationType(AUnitName, AName), '', LUse, LScope);
  finally
    LScope.Retire;
  end;
  Check(Supports(LValue, INyxDocumentationDeclaredTypeExpression, LShape) and
    (LShape.Family = AFamily) and (LShape.Definition.Expression.Text = LText) and
    (LShape.Definition.Expression.Occurrence = ndtoDeclaration), 'Actual declaration retains exact source family/window: ' + AName);
  Check((LShape.Definition.BindingCount > 0) and Supports(LShape.Definition.Expression.Syntax,
    INyxPascalDeclaredTypeSyntax, LDetached) and (LDetached.Declarations = nil),
    'Actual definition is detached and retains every unbound named occurrence: ' + AName);
  for LLink := 0 to LShape.Definition.BindingCount - 1 do
    Check(not LShape.Definition.BindingAt(LLink).Defined, 'Source spelling manufactures no qualified core/System binding');

  if AMethod then
    Check(ptfOfObject in LShape.Definition.Expression.Syntax.Node(LShape.Definition.Expression.Syntax.Root).Flags,
      'Actual procedural declaration retains of-object intent');
  Inc(GActualDefinitions);
end;

procedure ActualSources(const ARepository: String);
var
  LSource: INyxPascalSyntaxSnapshot;
begin
  LSource := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.events.pas');
  ActualDeclaration(LSource, 'nyx.events', 'TNyxExecutions', ptxArray);
  ActualDeclaration(LSource, 'nyx.events', 'TNyxSignalSnapshot', ptxFunction, True);
  ActualDeclaration(LSource, 'nyx.events', 'TNyxRegistrationID', ptxDistinct);
  LSource := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.behavior.pas');
  ActualDeclaration(LSource, 'nyx.behavior', 'TNyxEventHandler', ptxProcedure, True);
  LSource := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.state.pas');
  ActualDeclaration(LSource, 'nyx.state', 'TNyxStateValidator', ptxProcedure, True);
  ActualDeclaration(LSource, 'nyx.state', 'TNyxStateObserver', ptxProcedure, True);
  LSource := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.binding.types.pas');
  ActualDeclaration(LSource, 'nyx.binding.types', 'TNyxStateKinds', ptxSet);
  ActualDeclaration(LSource, 'nyx.binding.types', 'TNyxBindingTargetInfos', ptxArray);
  LSource := SourceSyntax(ARepository + 'src' + PathDelim + 'nyx.types.pas');
  ActualDeclaration(LSource, 'nyx.types', 'TNyxKeyModifiers', ptxSet);
  ActualDeclaration(LSource, 'nyx.types', 'TNyxPointerButtons', ptxSet);
  Check(GActualDefinitions = 10, 'All ten required actual definitions are inspected without a source skip');
end;

var
  LRepository: String;
{$endif}
begin
  try
    {$ifndef PAS2JS}
    LRepository := RepositoryArgument;
    {$endif}
    ShapeCases;
    RefusalCases;
    CaptureCases;
    SharedStorageCases;
    CompilerRootCases;
    CompilerRootSourceCases;
    IndexCases;
    {$ifndef PAS2JS}
    ActualSources(LRepository);
    {$endif}
    Check((DocumentationControlsConstructed = 0) and (DocumentationCallbacksConstructed = 0) and
      (DocumentationCallbacksInvoked = 0) and (DocumentationGlobalFactoryCalls = 0) and
      (DocumentationGlobalRoutineCalls = 0), 'Metadata inspection executes no source control/callback/factory/example');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' portable declared-expression checks';
    {$else}
    WriteLn('PASS ', GChecks, ' declared-expression checks / all', GActualDefinitions, ' actual definitions inspected');
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
