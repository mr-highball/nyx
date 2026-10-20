{ Declared compound types preserve source syntax and nominal owners. MIT. }
program nyx_documentation_declarationexpressions_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.scheduler, nyx.pascal.lexing,
  nyx.pascal.syntax, nyx.pascal.types, nyx.pascal.work,
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

var
  GChecks, GDisposed, GLeasesDisposed: Integer;
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
  CText: array[0..4] of TNyxText = ('array of TItem', 'set of TItem',
    'procedure(const A:TItem) of object', 'function(A:TItem):TItem; cdecl', 'type TItem');
  CFamily: array[0..4] of TNyxPascalTypeKind = (ptxArray, ptxSet, ptxProcedure, ptxFunction, ptxDistinct);
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
  CWrong: array[0..5] of TNyxText = ('TItem', 'Some.TItem', 'TList<TItem>', '^TItem',
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
