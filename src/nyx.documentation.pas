{ Nyx portable API documentation. MIT license, see LICENSE. }
unit nyx.documentation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.scheduler;

type
  { Invalid declarations fail before an immutable documentation value escapes.
    These are metadata errors, never compiler diagnostics or execution rights. }
  ENyxDocumentation = class(EArgumentException);
  { Cooperative metadata cancellation is distinct from a malformed declaration;
    no partial descriptor is returned to a cancelled inspection. }
  ENyxDocumentationCancelled = class(ENyxDocumentation);
  { Qualified authored identity. A unit root has no type name or generic arity;
    type roots retain both. A member keeps its exact unit/type owner and stable
    overload key. Pascal comparison folds ASCII without host locale rules. }
  TNyxDocumentationRef = record
  private
    FUnit, FType, FMember, FOverload: TNyxText;
    FArity: Integer;
  public
    function Defined: Boolean;
    function IsUnit: Boolean;
    function IsMember: Boolean;
    function Owner: TNyxDocumentationRef;
    function Key: TNyxText;
    function Same(const AOther: TNyxDocumentationRef): Boolean;
    procedure Validate;
    property UnitName: TNyxText read FUnit;
    property TypeName: TNyxText read FType;
    property MemberName: TNyxText read FMember;
    property OverloadKey: TNyxText read FOverload;
    property GenericArity: Integer read FArity;
  end;
  { Declared API shape only. A unit groups its actual global declarations and
    is never an invented containing class. Kinds prove neither implementation
    nor reflection; an alias retains its own qualified identity. }
  TNyxDocumentationTypeKind = (ndtClass, ndtInterface, ndtRecord, ndtEnum,
    ndtAlias, ndtCallback, ndtUnit, ndtIntrinsic);
  { A declared intrinsic family retains its original qualified identity. It
    supplies no inferred size, encoding, conversion rank or runtime capability;
    distinct integer/string declarations never become one nominal type. }
  TNyxDocumentationIntrinsicFamily = (ndifBoolean, ndifInteger, ndifReal,
    ndifCharacter, ndifString, ndifPointer);
  { Public member families distinguish fluent calls from properties and event
    declarations. Routine means a unit procedure/function, not a class method;
    existing kinds retain their ordinals when a new family is appended. }
  TNyxDocumentationMemberKind = (ndmConstructor, ndmFactory, ndmMethod,
    ndmFluentMethod, ndmProperty, ndmField, ndmEvent, ndmEnumValue, ndmRoutine);
  { Parameter intent remains declared Pascal metadata. Availability of a given
    spelling on a compiler is a separate qualification requirement. }
  TNyxDocumentationParameterMode = (ndpValue, ndpConst, ndpVar, ndpOut, ndpConstRef);
  { Documentation prose and schema declarations have independent authorities.
    Neither value is an observed runtime capability or compiler RTTI receipt. }
  TNyxDocumentationEvidence = (ndeAuthoredDeclaration, ndeSchemaDeclaration,
    ndeCatalogDeclaration);
  { One exact alias branch, with a type reference rather than executable code.
    Any is an authored universal target; a lone specific branch provides no
    information about the other projection. This record contains no interface. }
  TNyxDocumentationAliasTarget = record
  private
    FPlatform: TNyxPlatform;
    FTarget: TNyxDocumentationRef;
  public
    procedure Validate;
    property Platform: TNyxPlatform read FPlatform;
    property Target: TNyxDocumentationRef read FTarget;
  end;
  { Only two concrete projections exist; copied vectors keep branches detached. }
  TNyxDocumentationAliasTargetVector = array of TNyxDocumentationAliasTarget;
  { Optional intrinsic shape is implemented only by admitted intrinsic owners.
    Absence is never interpreted as an unknown/default Boolean family. }
  INyxDocumentationIntrinsic = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D101}']
    function Family: TNyxDocumentationIntrinsicFamily;
  end;
  { Optional target-specific alias data preserves every declared branch.
    TargetFor refuses absence; Any requires an actually universal/common target.
    The old AliasTarget getter is undefined when no common target is known. }
  INyxDocumentationAliasTargets = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D102}']
    function TargetCount: Integer;
    function TargetAt(AIndex: Integer): TNyxDocumentationAliasTarget;
    function HasTarget(APlatform: TNyxPlatform): Boolean;
    function TargetFor(APlatform: TNyxPlatform): TNyxDocumentationRef;
  end;
  { One copied parameter/type reference. Optional defaults retain their exact
    Pascal text without parsing, evaluating or running a callback. }
  TNyxDocumentationParameter = record
  private
    FName: TNyxText;
    FType: TNyxDocumentationRef;
    FMode: TNyxDocumentationParameterMode;
    FHasDefault: Boolean;
    FDefault: TNyxText;
  public
    function WithDefault(const APascal: TNyxText): TNyxDocumentationParameter;
    property Name: TNyxText read FName;
    property TypeReference: TNyxDocumentationRef read FType;
    property Passing: TNyxDocumentationParameterMode read FMode;
    property HasDefault: Boolean read FHasDefault;
    property DefaultPascal: TNyxText read FDefault;
  end;
  { Inert English-labelled Pascal example. Reading this copied value never
    compiles or executes it; later verification has its own explicit owner. }
  TNyxDocumentationExample = record
  private
    FTitle, FPascal: TNyxText;
  public
    property Title: TNyxText read FTitle;
    property Pascal: TNyxText read FPascal;
  end;
  { Detached declared signature. An absent result means no declared function
    result; constructor ownership remains in its member kind. An absent member
    signature means unknown, rather than an invented zero-argument call. }
  INyxDocumentationSignature = interface(IInterface)
    ['{A42D72BB-00BB-4A1D-BE81-855B4491EAE1}']
    function ParameterCount: Integer;
    function ParameterAt(AIndex: Integer): TNyxDocumentationParameter;
    function ResultType: TNyxDocumentationRef;
    function Evidence: TNyxDocumentationEvidence;
  end;
  { Immutable member decoration. Types/signatures are explicitly authored facts;
    schema adapters separately supply existing defaults, bounds and event facts.
    Fluent setters return new owners and never mutate an earlier descriptor. }
  INyxDocumentationMember = interface(IInterface)
    ['{1A569AD9-21CF-471F-8C9D-E91AF106602F}']
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
  end;
  { One declared unit/class/interface/record/enum/alias/callback contract. A unit
    owns globals without being a type. Bases retain their original owners;
    inherited members belong to a later complete index, never a copied owner. }
  INyxDocumentationType = interface(IInterface)
    ['{52146AAD-D232-4D6E-A20A-650541D7D26D}']
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
    function WithAlias(const ATarget: TNyxDocumentationRef): INyxDocumentationType;
    function WithBase(const ABase: TNyxDocumentationRef): INyxDocumentationType;
    function WithMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
    function WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
    function WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
  end;
  { Optional metadata capability for an existing declaration/provider. Registry
    inspection consumes registered descriptors; it must never construct a
    control/callback merely to query this interface. }
  INyxDocumentationSource = interface(IInterface)
    ['{E00694CB-5DD5-49CD-B25D-A1AA51EE420A}']
    function Documentation: INyxDocumentationType;
  end;

const
  NyxDocumentationMaximumMembers = 128;
  NyxDocumentationMaximumParameters = 64;
  NyxDocumentationMaximumLinks = 32;
  NyxDocumentationMaximumExamples = 8;
  NyxDocumentationMaximumBytes = 256 * 1024;

{ A unit identity can own global members and related links, but cannot occupy
  a type-required parameter/result/property/base/alias position. }
function NyxDocumentationUnit(const AUnit: TNyxText): TNyxDocumentationRef;
function NyxDocumentationType(const AUnit, AType: TNyxText;
  AGenericArity: Integer = 0): TNyxDocumentationRef;
function NyxDocumentationMember(const AOwner: TNyxDocumentationRef;
  const AName: TNyxText; const AOverload: TNyxText = ''): TNyxDocumentationRef;
function NyxDocumentationParameter(const AName: TNyxText;
  const AType: TNyxDocumentationRef;
  AMode: TNyxDocumentationParameterMode = ndpValue): TNyxDocumentationParameter;
function NyxDocumentationExample(const ATitle, APascal: TNyxText): TNyxDocumentationExample;
function NewNyxDocumentationSignature(const AParameters: array of TNyxDocumentationParameter;
  const AResult: TNyxDocumentationRef): INyxDocumentationSignature;
function NewNyxDocumentationMember(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationMemberKind; const ASummary: TNyxText): INyxDocumentationMember;
function NewNyxDocumentationType(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationTypeKind; const ASummary: TNyxText): INyxDocumentationType;
{ Intrinsic and target-alias facts have dedicated immutable owners. They are
  ordinary declaration data; these factories create no System/core inventory. }
function NewNyxDocumentationIntrinsic(const AIdentity: TNyxDocumentationRef;
  AFamily: TNyxDocumentationIntrinsicFamily; const ASummary: TNyxText): INyxDocumentationType;
function NyxDocumentationAliasTarget(APlatform: TNyxPlatform;
  const ATarget: TNyxDocumentationRef): TNyxDocumentationAliasTarget;
function NewNyxDocumentationTargetAlias(const AIdentity: TNyxDocumentationRef;
  const ASummary: TNyxText;
  const ATargets: array of TNyxDocumentationAliasTarget): INyxDocumentationType;
{ Freeze alternate metadata producers before retaining them. Counts and text
  are bounded; publication is atomic, with no retained producer or callback. }
function CaptureNyxDocumentationSignature(const AValue: INyxDocumentationSignature;
  const ACancellation: INyxCancellationScope = nil): INyxDocumentationSignature;
function CaptureNyxDocumentationMember(const AValue: INyxDocumentationMember;
  const ACancellation: INyxCancellationScope = nil): INyxDocumentationMember;
function CaptureNyxDocumentationType(const AValue: INyxDocumentationType;
  const ACancellation: INyxCancellationScope = nil): INyxDocumentationType;

implementation

uses nyx.bytes;

type
  { Shared immutable prose storage has value-only vectors and no document,
    renderer, registry, callback implementation or mutable provider reference. }
  TDocumentationContent = class(TInterfacedObject)
  protected
    FSummary: TNyxText;
    FExamples: array of TNyxDocumentationExample;
    FRelated: array of TNyxDocumentationRef;
    procedure CopyContent(ATarget: TDocumentationContent);
    procedure AddExample(const AValue: TNyxDocumentationExample);
    procedure AddRelated(const AValue: TNyxDocumentationRef);
  public
    function Summary: TNyxText;
    function Evidence: TNyxDocumentationEvidence;
    function ExampleCount: Integer;
    function ExampleAt(AIndex: Integer): TNyxDocumentationExample;
    function RelatedCount: Integer;
    function RelatedAt(AIndex: Integer): TNyxDocumentationRef;
  end;

procedure CheckCancellation(const ACancellation: INyxCancellationScope);
begin

  if (ACancellation <> nil) and ACancellation.Cancelled then
    raise ENyxDocumentationCancelled.Create('Documentation inspection cancelled');
end;

procedure AdmitBytes(AValue: Integer);
begin

  if AValue > NyxDocumentationMaximumBytes then
    raise ENyxDocumentation.Create('Documentation exceeds its aggregate UTF-8 allowance');
end;

function RefBytes(const AValue: TNyxDocumentationRef): Integer;
begin
  Result := 0;

  if AValue.Defined then
    Result := NyxUTF8ByteCount(AValue.Key);
end;

function SignatureBytes(const AValue: INyxDocumentationSignature): Integer;
var
  LAt: Integer;
  LParameter: TNyxDocumentationParameter;
begin
  Result := 0;

  if AValue = nil then
    Exit;
  Result := RefBytes(AValue.ResultType);
  for LAt := 0 to AValue.ParameterCount - 1 do
  begin
    LParameter := AValue.ParameterAt(LAt);
    Inc(Result, NyxUTF8ByteCount(LParameter.Name) + RefBytes(LParameter.TypeReference) +
      NyxUTF8ByteCount(LParameter.DefaultPascal));
    AdmitBytes(Result);
  end;
end;

function MemberBytes(const AValue: INyxDocumentationMember): Integer;
var
  LAt: Integer;
  LExample: TNyxDocumentationExample;
begin
  Result := RefBytes(AValue.Identity) + NyxUTF8ByteCount(AValue.Summary) +
    RefBytes(AValue.ValueType) + SignatureBytes(AValue.Signature);
  for LAt := 0 to AValue.ExampleCount - 1 do
  begin
    LExample := AValue.ExampleAt(LAt);
    Inc(Result, NyxUTF8ByteCount(LExample.Title) + NyxUTF8ByteCount(LExample.Pascal));
    AdmitBytes(Result);
  end;
  for LAt := 0 to AValue.RelatedCount - 1 do
  begin
    Inc(Result, RefBytes(AValue.RelatedAt(LAt)));
    AdmitBytes(Result);
  end;
  AdmitBytes(Result);
end;

function TypeBytes(const AValue: INyxDocumentationType): Integer;
var
  LAt: Integer;
  LExample: TNyxDocumentationExample;
  LIntrinsic: INyxDocumentationIntrinsic;
  LTargets: INyxDocumentationAliasTargets;
begin
  Result := RefBytes(AValue.Identity) + NyxUTF8ByteCount(AValue.Summary) + RefBytes(AValue.AliasTarget);

  if Supports(AValue, INyxDocumentationIntrinsic, LIntrinsic) then
    Inc(Result, Length(IntToStr(Ord(LIntrinsic.Family))));

  if Supports(AValue, INyxDocumentationAliasTargets, LTargets) then
    for LAt := 0 to LTargets.TargetCount - 1 do
    begin
      Inc(Result, RefBytes(LTargets.TargetAt(LAt).Target) +
        Length(IntToStr(Ord(LTargets.TargetAt(LAt).Platform))));
      AdmitBytes(Result);
    end;
  for LAt := 0 to AValue.BaseCount - 1 do
  begin
    Inc(Result, RefBytes(AValue.BaseAt(LAt)));
    AdmitBytes(Result);
  end;
  for LAt := 0 to AValue.MemberCount - 1 do
  begin
    Inc(Result, MemberBytes(AValue.MemberAt(LAt)));
    AdmitBytes(Result);
  end;
  for LAt := 0 to AValue.ExampleCount - 1 do
  begin
    LExample := AValue.ExampleAt(LAt);
    Inc(Result, NyxUTF8ByteCount(LExample.Title) + NyxUTF8ByteCount(LExample.Pascal));
    AdmitBytes(Result);
  end;
  for LAt := 0 to AValue.RelatedCount - 1 do
  begin
    Inc(Result, RefBytes(AValue.RelatedAt(LAt)));
    AdmitBytes(Result);
  end;
  AdmitBytes(Result);
end;

type
  { An immutable signature owns a detached parameter vector. Its records contain
    only names, scalar flags and qualified references, including on pas2js. }
  TDocumentationSignature = class(TInterfacedObject, INyxDocumentationSignature)
  public
    Parameters: array of TNyxDocumentationParameter;
    ReturnType: TNyxDocumentationRef;
    function ParameterCount: Integer;
    function ParameterAt(AIndex: Integer): TNyxDocumentationParameter;
    function ResultType: TNyxDocumentationRef;
    function Evidence: TNyxDocumentationEvidence;
  end;
  { A fluent member owns its signature interface in this managed class, never
    inside a copied record. Clone publication preserves earlier readers. }
  TDocumentationMember = class(TDocumentationContent, INyxDocumentationMember)
  private
    FIdentity: TNyxDocumentationRef;
    FKind: TNyxDocumentationMemberKind;
    FSignature: INyxDocumentationSignature;
    FValueType: TNyxDocumentationRef;
    function CopyValue: TDocumentationMember;
  public
    function Identity: TNyxDocumentationRef;
    function Kind: TNyxDocumentationMemberKind;
    function Signature: INyxDocumentationSignature;
    function ValueType: TNyxDocumentationRef;
    function WithSignature(const AValue: INyxDocumentationSignature): INyxDocumentationMember;
    function OfType(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
    function WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationMember;
    function WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
  end;
  { Member interfaces live in a dynamic managed vector. Base/alias links remain
    data references, preventing an inherited metadata graph from owning cycles. }
  TDocumentationType = class(TDocumentationContent, INyxDocumentationType)
  private
    FIdentity: TNyxDocumentationRef;
    FKind: TNyxDocumentationTypeKind;
    FAlias: TNyxDocumentationRef;
    FBases: array of TNyxDocumentationRef;
    FMembers: array of INyxDocumentationMember;
  protected
    procedure CopyType(ATarget: TDocumentationType);
    function CopyValue: TDocumentationType; virtual;
    procedure AssignAlias(const ATarget: TNyxDocumentationRef); virtual;
  public
    function Identity: TNyxDocumentationRef;
    function Kind: TNyxDocumentationTypeKind;
    function AliasTarget: TNyxDocumentationRef;
    function BaseCount: Integer;
    function BaseAt(AIndex: Integer): TNyxDocumentationRef;
    function MemberCount: Integer;
    function MemberAt(AIndex: Integer): INyxDocumentationMember;
    function WithAlias(const ATarget: TNyxDocumentationRef): INyxDocumentationType;
    function WithBase(const ABase: TNyxDocumentationRef): INyxDocumentationType;
    function WithMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
    function WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
    function WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
  end;
  { Only an intrinsic declaration exposes its family. Virtual internal copying
    preserves the specialized capability through every ordinary fluent method. }
  TDocumentationIntrinsic = class(TDocumentationType, INyxDocumentationIntrinsic)
  private
    FFamily: TNyxDocumentationIntrinsicFamily;
  protected
    function CopyValue: TDocumentationType; override;
  public
    function Family: TNyxDocumentationIntrinsicFamily;
  end;
  { Detached value branches retain their original type owners. Replacing the
    alias explicitly replaces the complete branch set with one universal case;
    ordinary prose/member copying leaves every original branch untouched. }
  TDocumentationTargetAlias = class(TDocumentationType, INyxDocumentationAliasTargets)
  private
    FTargets: TNyxDocumentationAliasTargetVector;
  protected
    function CopyValue: TDocumentationType; override;
    procedure AssignAlias(const ATarget: TNyxDocumentationRef); override;
  public
    function TargetCount: Integer;
    function TargetAt(AIndex: Integer): TNyxDocumentationAliasTarget;
    function HasTarget(APlatform: TNyxPlatform): Boolean;
    function TargetFor(APlatform: TNyxPlatform): TNyxDocumentationRef;
  end;

procedure TextBound(const AValue: TNyxText; AMaximum: Integer);
var
  LAt, LScalar, LCount: Integer;
begin
  LAt := 1;
  LCount := 0;
  while LAt <= Length(AValue) do
  begin

    if not NyxNextScalar(AValue, LAt, LScalar) then
      raise ENyxDocumentation.Create('Documentation text has malformed Unicode');
    Inc(LCount);

    if LCount > AMaximum then
      raise ENyxDocumentation.Create('Documentation text exceeds its scalar allowance');
  end;
end;

procedure NameBound(const AValue: TNyxText; ADotted: Boolean);
var
  LAt: Integer;
  LInitial: Boolean;
  LChar: Char;
begin
  TextBound(AValue, 256);
  LInitial := True;
  for LAt := 1 to Length(AValue) do
  begin
    LChar := AValue[LAt];

    if ADotted and (LChar = '.') and not LInitial then
      LInitial := True
    else
    begin

      if not ((LChar in ['A'..'Z', 'a'..'z', '_']) or
        (not LInitial and (LChar in ['0'..'9']))) then
        raise ENyxDocumentation.Create('Documentation identity requires qualified Pascal names');
      LInitial := False;
    end;
  end;

  if LInitial then
    raise ENyxDocumentation.Create('Documentation identity cannot be empty or end with a dot');
end;

function Fold(const AValue: TNyxText): TNyxText;
var
  LAt: Integer;
begin
  Result := AValue;
  for LAt := 1 to Length(Result) do
  begin

    if Result[LAt] in ['A'..'Z'] then
      Result[LAt] := Chr(Ord(Result[LAt]) + 32);
  end;
end;

function TNyxDocumentationRef.Defined: Boolean;
begin
  Result := FUnit <> '';
end;

function TNyxDocumentationRef.IsUnit: Boolean;
begin
  Result := Defined and (FType = '') and not IsMember;
end;

function TNyxDocumentationRef.IsMember: Boolean;
begin
  Result := FMember <> '';
end;

function TNyxDocumentationRef.Owner: TNyxDocumentationRef;
begin
  Result := Self;
  Result.FMember := '';
  Result.FOverload := '';
end;

procedure TNyxDocumentationRef.Validate;
begin
  NameBound(FUnit, True);

  if FType <> '' then
    NameBound(FType, True)
  else if FArity <> 0 then
    raise ENyxDocumentation.Create('A documentation unit has no type generic arity');

  if (FArity < 0) or (FArity > 32) then
    raise ENyxDocumentation.Create('Documentation generic arity is outside its allowance');

  if IsMember then
    NameBound(FMember, False)
  else if FOverload <> '' then
    raise ENyxDocumentation.Create('An overload identity requires its declaring member');

  if FOverload <> '' then
    NameBound(FOverload, True);
end;

function TNyxDocumentationRef.Key: TNyxText;
begin
  Validate;
  Result := Fold(FUnit) + '|' + Fold(FType) + '|' + IntToStr(FArity) +
    '|' + Fold(FMember) + '|' + Fold(FOverload);
end;

function TNyxDocumentationRef.Same(const AOther: TNyxDocumentationRef): Boolean;
begin
  Result := Defined and AOther.Defined;

  if Result then
    Result := Key = AOther.Key;
end;

function NyxDocumentationUnit(const AUnit: TNyxText): TNyxDocumentationRef;
begin
  Result := Default(TNyxDocumentationRef);
  Result.FUnit := AUnit;
  Result.Validate;
end;

function NyxDocumentationType(const AUnit, AType: TNyxText;
  AGenericArity: Integer): TNyxDocumentationRef;
begin

  if AType = '' then
    raise ENyxDocumentation.Create('A documentation type requires its Pascal type name');
  Result := Default(TNyxDocumentationRef);
  Result.FUnit := AUnit;
  Result.FType := AType;
  Result.FArity := AGenericArity;
  Result.Validate;
end;

function NyxDocumentationMember(const AOwner: TNyxDocumentationRef;
  const AName, AOverload: TNyxText): TNyxDocumentationRef;
begin
  AOwner.Validate;

  if AOwner.IsMember then
    raise ENyxDocumentation.Create('A member identity requires its original declaring unit or type');
  Result := AOwner;
  Result.FMember := AName;
  Result.FOverload := AOverload;
  Result.Validate;

  if not Result.IsMember then
    raise ENyxDocumentation.Create('A member identity requires its Pascal name');
end;

function NyxDocumentationParameter(const AName: TNyxText;
  const AType: TNyxDocumentationRef;
  AMode: TNyxDocumentationParameterMode): TNyxDocumentationParameter;
begin
  NameBound(AName, False);
  AType.Validate;

  if AType.IsUnit or AType.IsMember or (Ord(AMode) < 0) or
    (Ord(AMode) > Ord(High(TNyxDocumentationParameterMode))) then
    raise ENyxDocumentation.Create('A parameter requires its declared type and passing mode');
  Result := Default(TNyxDocumentationParameter);
  Result.FName := AName;
  Result.FType := AType;
  Result.FMode := AMode;
end;

function TNyxDocumentationParameter.WithDefault(const APascal: TNyxText): TNyxDocumentationParameter;
begin
  TextBound(APascal, 1024);

  if (APascal = '') or (FMode in [ndpVar, ndpOut]) then
    raise ENyxDocumentation.Create('This parameter cannot declare an empty or writable default');
  Result := NyxDocumentationParameter(FName, FType, FMode);
  Result.FHasDefault := True;
  Result.FDefault := APascal;
end;

function NyxDocumentationExample(const ATitle, APascal: TNyxText): TNyxDocumentationExample;
begin
  TextBound(ATitle, 256);
  TextBound(APascal, 16384);

  if (ATitle = '') or (APascal = '') then
    raise ENyxDocumentation.Create('A documentation example requires its title and Pascal text');
  Result.FTitle := ATitle;
  Result.FPascal := APascal;
end;

function TDocumentationSignature.ParameterCount: Integer;
begin
  Result := Length(Parameters);
end;

function TDocumentationSignature.ParameterAt(AIndex: Integer): TNyxDocumentationParameter;
begin

  if (AIndex < 0) or (AIndex >= ParameterCount) then
    raise ENyxDocumentation.Create('Documentation parameter ordinal is out of range');
  Result := Parameters[AIndex];
end;

function TDocumentationSignature.ResultType: TNyxDocumentationRef;
begin
  Result := ReturnType;
end;

function TDocumentationSignature.Evidence: TNyxDocumentationEvidence;
begin
  Result := ndeAuthoredDeclaration;
end;

function NewNyxDocumentationSignature(const AParameters: array of TNyxDocumentationParameter;
  const AResult: TNyxDocumentationRef): INyxDocumentationSignature;
var
  LValue: TDocumentationSignature;
  LHold: INyxDocumentationSignature;
  LAt, LOther: Integer;
begin
  Result := nil;

  if Length(AParameters) > NyxDocumentationMaximumParameters then
    raise ENyxDocumentation.Create('Documentation signature exceeds its parameter allowance');

  if AResult.Defined then
  begin
    AResult.Validate;

    if AResult.IsUnit or AResult.IsMember then
      raise ENyxDocumentation.Create('A signature result requires a type identity');
  end;
  LValue := TDocumentationSignature.Create;
  LHold := LValue;
  LValue.ReturnType := Default(TNyxDocumentationRef);

  if AResult.Defined then
    LValue.ReturnType := AResult;
  SetLength(LValue.Parameters, Length(AParameters));
  for LAt := 0 to High(AParameters) do
  begin
    LValue.Parameters[LAt] := NyxDocumentationParameter(AParameters[LAt].Name,
      AParameters[LAt].TypeReference, AParameters[LAt].Passing);

    if AParameters[LAt].HasDefault then
      LValue.Parameters[LAt] := LValue.Parameters[LAt].WithDefault(AParameters[LAt].DefaultPascal);
    for LOther := 0 to LAt - 1 do
    begin

      if Fold(AParameters[LOther].Name) = Fold(AParameters[LAt].Name) then
        raise ENyxDocumentation.Create('A documentation signature repeats a parameter name');
    end;
  end;
  SignatureBytes(LHold);
  Result := LHold;
end;

function CaptureNyxDocumentationSignature(const AValue: INyxDocumentationSignature;
  const ACancellation: INyxCancellationScope): INyxDocumentationSignature;
var
  LSource: INyxDocumentationSignature;
  LFrozen: INyxDocumentationSignature;
  LCount, LAt: Integer;
  LParameters: array of TNyxDocumentationParameter;
begin
  LSource := AValue;
  Result := nil;
  CheckCancellation(ACancellation);

  if LSource = nil then
    Exit;
  LCount := LSource.ParameterCount;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumParameters) then
    raise ENyxDocumentation.Create('Captured documentation signature exceeds its parameter allowance');
  SetLength(LParameters, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LParameters[LAt] := LSource.ParameterAt(LAt);
  end;
  LFrozen := NewNyxDocumentationSignature(LParameters, LSource.ResultType);
  CheckCancellation(ACancellation);
  Result := LFrozen;
end;

function TDocumentationContent.Summary: TNyxText;
begin
  Result := FSummary;
end;

function TDocumentationContent.Evidence: TNyxDocumentationEvidence;
begin
  Result := ndeAuthoredDeclaration;
end;

function TDocumentationContent.ExampleCount: Integer;
begin
  Result := Length(FExamples);
end;

function TDocumentationContent.ExampleAt(AIndex: Integer): TNyxDocumentationExample;
begin

  if (AIndex < 0) or (AIndex >= ExampleCount) then
    raise ENyxDocumentation.Create('Documentation example ordinal is out of range');
  Result := FExamples[AIndex];
end;

function TDocumentationContent.RelatedCount: Integer;
begin
  Result := Length(FRelated);
end;

function TDocumentationContent.RelatedAt(AIndex: Integer): TNyxDocumentationRef;
begin

  if (AIndex < 0) or (AIndex >= RelatedCount) then
    raise ENyxDocumentation.Create('Documentation related-link ordinal is out of range');
  Result := FRelated[AIndex];
end;

procedure TDocumentationContent.CopyContent(ATarget: TDocumentationContent);
var
  LAt: Integer;
begin
  ATarget.FSummary := FSummary;
  SetLength(ATarget.FExamples, ExampleCount);
  for LAt := 0 to ExampleCount - 1 do
    ATarget.FExamples[LAt] := FExamples[LAt];
  SetLength(ATarget.FRelated, RelatedCount);
  for LAt := 0 to RelatedCount - 1 do
    ATarget.FRelated[LAt] := FRelated[LAt];
end;

procedure TDocumentationContent.AddExample(const AValue: TNyxDocumentationExample);
begin

  if ExampleCount >= NyxDocumentationMaximumExamples then
    raise ENyxDocumentation.Create('Documentation example allowance is exhausted');
  SetLength(FExamples, ExampleCount + 1);
  FExamples[High(FExamples)] := NyxDocumentationExample(AValue.Title, AValue.Pascal);
end;

procedure TDocumentationContent.AddRelated(const AValue: TNyxDocumentationRef);
var
  LAt: Integer;
begin
  AValue.Validate;

  if RelatedCount >= NyxDocumentationMaximumLinks then
    raise ENyxDocumentation.Create('Documentation link allowance is exhausted');
  for LAt := 0 to RelatedCount - 1 do
  begin

    if FRelated[LAt].Same(AValue) then
      raise ENyxDocumentation.Create('Documentation repeats a related reference');
  end;
  SetLength(FRelated, RelatedCount + 1);
  FRelated[High(FRelated)] := AValue;
end;

function NewNyxDocumentationMember(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationMemberKind; const ASummary: TNyxText): INyxDocumentationMember;
var
  LValue: TDocumentationMember;
begin
  Result := nil;
  AIdentity.Validate;
  TextBound(ASummary, 2048);

  if not AIdentity.IsMember or (Ord(AKind) < 0) or
    (Ord(AKind) > Ord(High(TNyxDocumentationMemberKind))) then
    raise ENyxDocumentation.Create('A documentation member requires its identity and family');

  if AIdentity.Owner.IsUnit then
  begin

    if not (AKind in [ndmFactory, ndmRoutine, ndmField]) then
      raise ENyxDocumentation.Create('A unit owns factories, routines and global fields, not class members');
  end
  else if AKind = ndmRoutine then
    raise ENyxDocumentation.Create('A global routine requires its actual declaring unit');
  LValue := TDocumentationMember.Create;
  LValue.FIdentity := AIdentity;
  LValue.FKind := AKind;
  LValue.FSummary := ASummary;
  Result := LValue;
end;

function TDocumentationMember.CopyValue: TDocumentationMember;
begin
  Result := TDocumentationMember.Create;
  CopyContent(Result);
  Result.FIdentity := FIdentity;
  Result.FKind := FKind;
  Result.FSignature := FSignature;
  Result.FValueType := FValueType;
end;

function TDocumentationMember.Identity: TNyxDocumentationRef;
begin
  Result := FIdentity;
end;

function TDocumentationMember.Kind: TNyxDocumentationMemberKind;
begin
  Result := FKind;
end;

function TDocumentationMember.Signature: INyxDocumentationSignature;
begin
  Result := FSignature;
end;

function TDocumentationMember.ValueType: TNyxDocumentationRef;
begin
  Result := FValueType;
end;

function TDocumentationMember.WithSignature(const AValue: INyxDocumentationSignature): INyxDocumentationMember;
var
  LNext: TDocumentationMember;
  LHold: INyxDocumentationMember;
  LFrozen: INyxDocumentationSignature;
begin
  LFrozen := CaptureNyxDocumentationSignature(AValue);

  if not (FKind in [ndmConstructor, ndmFactory, ndmMethod, ndmFluentMethod, ndmRoutine]) then
    raise ENyxDocumentation.Create('This documentation member has no call signature');
  LNext := CopyValue;
  LHold := LNext;
  LNext.FSignature := LFrozen;
  MemberBytes(LHold);
  Result := LHold;
end;

function TDocumentationMember.OfType(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
var
  LNext: TDocumentationMember;
  LHold: INyxDocumentationMember;
begin
  AValue.Validate;

  if AValue.IsUnit or AValue.IsMember or
    not (FKind in [ndmProperty, ndmField, ndmEvent, ndmEnumValue]) then
    raise ENyxDocumentation.Create('This documentation member cannot declare a value type');
  LNext := CopyValue;
  LHold := LNext;
  LNext.FValueType := AValue;
  MemberBytes(LHold);
  Result := LHold;
end;

function TDocumentationMember.WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationMember;
var
  LNext: TDocumentationMember;
  LHold: INyxDocumentationMember;
begin
  LNext := CopyValue;
  LHold := LNext;
  LNext.AddExample(AValue);
  MemberBytes(LHold);
  Result := LHold;
end;

function TDocumentationMember.WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
var
  LNext: TDocumentationMember;
  LHold: INyxDocumentationMember;
begin
  LNext := CopyValue;
  LHold := LNext;
  LNext.AddRelated(AValue);
  MemberBytes(LHold);
  Result := LHold;
end;

function CaptureNyxDocumentationMember(const AValue: INyxDocumentationMember;
  const ACancellation: INyxCancellationScope): INyxDocumentationMember;
var
  LSource, LFrozen: INyxDocumentationMember;
  LAt, LCount: Integer;
  LType: TNyxDocumentationRef;
  LSignature: INyxDocumentationSignature;
begin
  LSource := AValue;
  Result := nil;
  CheckCancellation(ACancellation);

  if LSource = nil then
    raise ENyxDocumentation.Create('A documentation member is required');
  LFrozen := NewNyxDocumentationMember(LSource.Identity, LSource.Kind, LSource.Summary);

  LSignature := CaptureNyxDocumentationSignature(LSource.Signature, ACancellation);

  if LSignature <> nil then
    LFrozen := LFrozen.WithSignature(LSignature);
  LType := LSource.ValueType;

  if LType.Defined then
    LFrozen := LFrozen.OfType(LType);
  LCount := LSource.ExampleCount;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumExamples) then
    raise ENyxDocumentation.Create('Captured documentation exceeds its example allowance');
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LFrozen := LFrozen.WithExample(LSource.ExampleAt(LAt));
  end;
  LCount := LSource.RelatedCount;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumLinks) then
    raise ENyxDocumentation.Create('Captured documentation exceeds its related-link allowance');
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LFrozen := LFrozen.WithRelated(LSource.RelatedAt(LAt));
  end;
  MemberBytes(LFrozen);
  CheckCancellation(ACancellation);
  Result := LFrozen;
end;

procedure CheckTypeIdentity(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationTypeKind; const ASummary: TNyxText);
begin
  AIdentity.Validate;
  TextBound(ASummary, 2048);

  if AIdentity.IsMember or (Ord(AKind) < 0) or
    (Ord(AKind) > Ord(High(TNyxDocumentationTypeKind))) then
    raise ENyxDocumentation.Create('A documentation type requires its own qualified identity');

  if AIdentity.IsUnit <> (AKind = ndtUnit) then
    raise ENyxDocumentation.Create('A unit identity requires a unit declaration; a type requires its type name');
end;

function NewNyxDocumentationType(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationTypeKind; const ASummary: TNyxText): INyxDocumentationType;
var
  LValue: TDocumentationType;
begin
  Result := nil;
  CheckTypeIdentity(AIdentity, AKind, ASummary);

  if AKind = ndtIntrinsic then
    raise ENyxDocumentation.Create('An intrinsic declaration requires its explicit family factory');
  LValue := TDocumentationType.Create;
  LValue.FIdentity := AIdentity;
  LValue.FKind := AKind;
  LValue.FSummary := ASummary;
  Result := LValue;
end;

procedure CheckAliasPlatform(APlatform: TNyxPlatform);
begin

  if (Ord(APlatform) < Ord(Low(TNyxPlatform))) or (Ord(APlatform) > Ord(High(TNyxPlatform))) then
    raise ENyxDocumentation.Create('Unknown documentation alias platform');
end;

procedure TNyxDocumentationAliasTarget.Validate;
begin
  CheckAliasPlatform(FPlatform);
  FTarget.Validate;

  if not FTarget.Defined or FTarget.IsUnit or FTarget.IsMember then
    raise ENyxDocumentation.Create('An alias branch requires an actual type reference');
end;

function NyxDocumentationAliasTarget(APlatform: TNyxPlatform;
  const ATarget: TNyxDocumentationRef): TNyxDocumentationAliasTarget;
begin
  Result.FPlatform := APlatform;
  Result.FTarget := ATarget;
  Result.Validate;
end;

function NewNyxDocumentationIntrinsic(const AIdentity: TNyxDocumentationRef;
  AFamily: TNyxDocumentationIntrinsicFamily; const ASummary: TNyxText): INyxDocumentationType;
var
  LOwner: TDocumentationIntrinsic;
  LLease: INyxDocumentationType;
begin
  Result := nil;
  CheckTypeIdentity(AIdentity, ndtIntrinsic, ASummary);

  if (Ord(AFamily) < Ord(Low(TNyxDocumentationIntrinsicFamily))) or
    (Ord(AFamily) > Ord(High(TNyxDocumentationIntrinsicFamily))) then
    raise ENyxDocumentation.Create('Unknown documentation intrinsic family');
  LOwner := TDocumentationIntrinsic.Create;
  LLease := LOwner;
  LOwner.FIdentity := AIdentity;
  LOwner.FKind := ndtIntrinsic;
  LOwner.FSummary := ASummary;
  LOwner.FFamily := AFamily;
  TypeBytes(LLease);
  Result := LLease;
end;

function NewNyxDocumentationTargetAlias(const AIdentity: TNyxDocumentationRef;
  const ASummary: TNyxText;
  const ATargets: array of TNyxDocumentationAliasTarget): INyxDocumentationType;
var
  LOwner: TDocumentationTargetAlias;
  LLease: INyxDocumentationType;
  LAt: Integer;
  LSwap: TNyxDocumentationAliasTarget;
begin
  Result := nil;
  CheckTypeIdentity(AIdentity, ndtAlias, ASummary);

  if (Length(ATargets) < 1) or (Length(ATargets) > 2) then
    raise ENyxDocumentation.Create('A target alias requires one or two nonoverlapping branches');
  for LAt := 0 to High(ATargets) do
  begin
    ATargets[LAt].Validate;

    if ATargets[LAt].Target.Same(AIdentity) then
      raise ENyxDocumentation.Create('An alias branch cannot target itself');
  end;

  if (Length(ATargets) = 2) and
    ((ATargets[0].Platform = ATargets[1].Platform) or
    (ATargets[0].Platform = npfAny) or (ATargets[1].Platform = npfAny)) then
    raise ENyxDocumentation.Create('Documentation alias branches overlap');
  LOwner := TDocumentationTargetAlias.Create;
  LLease := LOwner;
  LOwner.FIdentity := AIdentity;
  LOwner.FKind := ndtAlias;
  LOwner.FSummary := ASummary;
  SetLength(LOwner.FTargets, Length(ATargets));
  for LAt := 0 to High(ATargets) do
    LOwner.FTargets[LAt] := ATargets[LAt];

  if (Length(ATargets) = 2) and (Ord(LOwner.FTargets[0].Platform) > Ord(LOwner.FTargets[1].Platform)) then
  begin
    LSwap := LOwner.FTargets[0];
    LOwner.FTargets[0] := LOwner.FTargets[1];
    LOwner.FTargets[1] := LSwap;
  end;

  if (LOwner.FTargets[0].Platform = npfAny) or
    ((Length(ATargets) = 2) and ATargets[0].Target.Same(ATargets[1].Target)) then
    LOwner.FAlias := LOwner.FTargets[0].Target;
  TypeBytes(LLease);
  Result := LLease;
end;

procedure TDocumentationType.CopyType(ATarget: TDocumentationType);
var
  LAt: Integer;
begin
  CopyContent(ATarget);
  ATarget.FIdentity := FIdentity;
  ATarget.FKind := FKind;
  ATarget.FAlias := FAlias;
  SetLength(ATarget.FBases, BaseCount);
  for LAt := 0 to BaseCount - 1 do
    ATarget.FBases[LAt] := FBases[LAt];
  SetLength(ATarget.FMembers, MemberCount);
  for LAt := 0 to MemberCount - 1 do
    ATarget.FMembers[LAt] := FMembers[LAt];
end;

function TDocumentationType.CopyValue: TDocumentationType;
begin
  Result := TDocumentationType.Create;
  try
    CopyType(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TDocumentationType.AssignAlias(const ATarget: TNyxDocumentationRef);
begin
  FAlias := ATarget;
end;

function TDocumentationIntrinsic.CopyValue: TDocumentationType;
var
  LCopy: TDocumentationIntrinsic;
begin
  LCopy := TDocumentationIntrinsic.Create;
  try
    CopyType(LCopy);
    LCopy.FFamily := FFamily;
  except
    LCopy.Free;
    raise;
  end;
  Result := LCopy;
end;

function TDocumentationIntrinsic.Family: TNyxDocumentationIntrinsicFamily;
begin
  Result := FFamily;
end;

function TDocumentationTargetAlias.CopyValue: TDocumentationType;
var
  LCopy: TDocumentationTargetAlias;
  LAt: Integer;
begin
  LCopy := TDocumentationTargetAlias.Create;
  try
    CopyType(LCopy);
    SetLength(LCopy.FTargets, TargetCount);
    for LAt := 0 to TargetCount - 1 do
      LCopy.FTargets[LAt] := FTargets[LAt];
  except
    LCopy.Free;
    raise;
  end;
  Result := LCopy;
end;

procedure TDocumentationTargetAlias.AssignAlias(const ATarget: TNyxDocumentationRef);
begin
  inherited AssignAlias(ATarget);
  SetLength(FTargets, 1);
  FTargets[0] := NyxDocumentationAliasTarget(npfAny, ATarget);
end;

function TDocumentationTargetAlias.TargetCount: Integer;
begin
  Result := Length(FTargets);
end;

function TDocumentationTargetAlias.TargetAt(AIndex: Integer): TNyxDocumentationAliasTarget;
begin

  if (AIndex < 0) or (AIndex >= TargetCount) then
    raise ENyxDocumentation.Create('Documentation alias branch ordinal is out of range');
  Result := FTargets[AIndex];
end;

function TDocumentationTargetAlias.HasTarget(APlatform: TNyxPlatform): Boolean;
var
  LAt: Integer;
begin
  CheckAliasPlatform(APlatform);
  Result := False;

  if (APlatform = npfAny) and FAlias.Defined then
    Exit(True);
  for LAt := 0 to TargetCount - 1 do
  begin

    if (FTargets[LAt].Platform = npfAny) or (FTargets[LAt].Platform = APlatform) then
      Exit(True);
  end;
end;

function TDocumentationTargetAlias.TargetFor(APlatform: TNyxPlatform): TNyxDocumentationRef;
var
  LAt: Integer;
begin

  if not HasTarget(APlatform) then
    raise ENyxDocumentation.Create('No alias target is declared for the requested platform');

  if APlatform = npfAny then
    Exit(FAlias);
  for LAt := 0 to TargetCount - 1 do
  begin

    if (FTargets[LAt].Platform = npfAny) or (FTargets[LAt].Platform = APlatform) then
      Exit(FTargets[LAt].Target);
  end;
  raise ENyxDocumentation.Create('No alias target is declared for the requested platform');
end;

function TDocumentationType.Identity: TNyxDocumentationRef;
begin
  Result := FIdentity;
end;

function TDocumentationType.Kind: TNyxDocumentationTypeKind;
begin
  Result := FKind;
end;

function TDocumentationType.AliasTarget: TNyxDocumentationRef;
begin
  Result := FAlias;
end;

function TDocumentationType.BaseCount: Integer;
begin
  Result := Length(FBases);
end;

function TDocumentationType.BaseAt(AIndex: Integer): TNyxDocumentationRef;
begin

  if (AIndex < 0) or (AIndex >= BaseCount) then
    raise ENyxDocumentation.Create('Documentation base ordinal is out of range');
  Result := FBases[AIndex];
end;

function TDocumentationType.MemberCount: Integer;
begin
  Result := Length(FMembers);
end;

function TDocumentationType.MemberAt(AIndex: Integer): INyxDocumentationMember;
begin

  if (AIndex < 0) or (AIndex >= MemberCount) then
    raise ENyxDocumentation.Create('Documentation member ordinal is out of range');
  Result := FMembers[AIndex];
end;

function TDocumentationType.WithAlias(const ATarget: TNyxDocumentationRef): INyxDocumentationType;
var
  LNext: TDocumentationType;
  LHold: INyxDocumentationType;
begin
  ATarget.Validate;

  if (FKind <> ndtAlias) or ATarget.IsUnit or ATarget.IsMember or ATarget.Same(FIdentity) then
    raise ENyxDocumentation.Create('An alias requires another original type identity');
  LNext := CopyValue;
  LHold := LNext;
  LNext.AssignAlias(ATarget);
  TypeBytes(LHold);
  Result := LHold;
end;

function TDocumentationType.WithBase(const ABase: TNyxDocumentationRef): INyxDocumentationType;
var
  LNext: TDocumentationType;
  LHold: INyxDocumentationType;
  LAt: Integer;
begin
  ABase.Validate;

  if not (FKind in [ndtClass, ndtInterface, ndtCallback]) or
    ABase.IsUnit or ABase.IsMember or
    ABase.Same(FIdentity) or (BaseCount >= NyxDocumentationMaximumLinks) then
    raise ENyxDocumentation.Create('A base requires a distinct type and bounded class/interface/callback owner');
  for LAt := 0 to BaseCount - 1 do
  begin

    if FBases[LAt].Same(ABase) then
      raise ENyxDocumentation.Create('Documentation repeats a base type');
  end;
  LNext := CopyValue;
  LHold := LNext;
  SetLength(LNext.FBases, BaseCount + 1);
  LNext.FBases[High(LNext.FBases)] := ABase;
  TypeBytes(LHold);
  Result := LHold;
end;

function TDocumentationType.WithMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
var
  LFrozen: INyxDocumentationMember;
  LNext: TDocumentationType;
  LHold: INyxDocumentationType;
  LAt: Integer;
begin
  { Intrinsic families have no declared instance members in this contract;
    helpers and compiler intrinsics require their own truthful future shapes. }

  if FKind = ndtIntrinsic then
    raise ENyxDocumentation.Create('An intrinsic declaration cannot invent members');
  LFrozen := CaptureNyxDocumentationMember(AMember);

  if not LFrozen.Identity.Owner.Same(FIdentity) or
    (MemberCount >= NyxDocumentationMaximumMembers) then
    raise ENyxDocumentation.Create('A documentation member must retain its original bounded owner');
  for LAt := 0 to MemberCount - 1 do
  begin

    if FMembers[LAt].Identity.Same(LFrozen.Identity) then
      raise ENyxDocumentation.Create('Documentation repeats a member/overload identity');
  end;
  LNext := CopyValue;
  LHold := LNext;
  SetLength(LNext.FMembers, MemberCount + 1);
  LNext.FMembers[High(LNext.FMembers)] := LFrozen;
  TypeBytes(LHold);
  Result := LHold;
end;

function TDocumentationType.WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationType;
var
  LNext: TDocumentationType;
  LHold: INyxDocumentationType;
begin
  LNext := CopyValue;
  LHold := LNext;
  LNext.AddExample(AValue);
  TypeBytes(LHold);
  Result := LHold;
end;

function TDocumentationType.WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationType;
var
  LNext: TDocumentationType;
  LHold: INyxDocumentationType;
begin
  LNext := CopyValue;
  LHold := LNext;
  LNext.AddRelated(AValue);
  TypeBytes(LHold);
  Result := LHold;
end;

function CaptureNyxDocumentationType(const AValue: INyxDocumentationType;
  const ACancellation: INyxCancellationScope): INyxDocumentationType;
var
  LSource, LFrozen: INyxDocumentationType;
  LAt, LCount: Integer;
  LAlias: TNyxDocumentationRef;
  LKind: TNyxDocumentationTypeKind;
  LIntrinsic: INyxDocumentationIntrinsic;
  LTargets: INyxDocumentationAliasTargets;
  LBranches: TNyxDocumentationAliasTargetVector;
  LHasIntrinsic, LHasTargets: Boolean;
begin
  LSource := AValue;
  Result := nil;
  CheckCancellation(ACancellation);

  if LSource = nil then
    raise ENyxDocumentation.Create('A documentation type is required');
  LKind := LSource.Kind;
  LHasIntrinsic := Supports(LSource, INyxDocumentationIntrinsic, LIntrinsic);
  LHasTargets := Supports(LSource, INyxDocumentationAliasTargets, LTargets);

  if (LHasIntrinsic <> (LKind = ndtIntrinsic)) or (LHasTargets and (LKind <> ndtAlias)) then
    raise ENyxDocumentation.Create('Documentation shape capability contradicts its declared kind');

  if LHasIntrinsic then
    LFrozen := NewNyxDocumentationIntrinsic(LSource.Identity, LIntrinsic.Family, LSource.Summary)
  else if LHasTargets then
  begin
    LCount := LTargets.TargetCount;

    if (LCount < 1) or (LCount > 2) then
      raise ENyxDocumentation.Create('Captured alias exceeds its one-or-two branch allowance');
    SetLength(LBranches, LCount);
    for LAt := 0 to LCount - 1 do
    begin
      CheckCancellation(ACancellation);
      LBranches[LAt] := LTargets.TargetAt(LAt);
    end;
    LFrozen := NewNyxDocumentationTargetAlias(LSource.Identity, LSource.Summary, LBranches);
  end
  else
    LFrozen := NewNyxDocumentationType(LSource.Identity, LKind, LSource.Summary);
  LAlias := LSource.AliasTarget;

  if LAlias.Defined then
  begin

    if LHasTargets then
    begin

      if not LAlias.Same(LFrozen.AliasTarget) then
        raise ENyxDocumentation.Create('Common alias target contradicts its declared platform branches');
    end
    else
      LFrozen := LFrozen.WithAlias(LAlias);
  end;
  LCount := LSource.BaseCount;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumLinks) then
    raise ENyxDocumentation.Create('Captured documentation exceeds its base allowance');
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LFrozen := LFrozen.WithBase(LSource.BaseAt(LAt));
  end;
  LCount := LSource.MemberCount;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumMembers) then
    raise ENyxDocumentation.Create('Captured documentation exceeds its member allowance');
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LFrozen := LFrozen.WithMember(CaptureNyxDocumentationMember(LSource.MemberAt(LAt), ACancellation));
  end;
  LCount := LSource.ExampleCount;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumExamples) then
    raise ENyxDocumentation.Create('Captured documentation exceeds its example allowance');
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LFrozen := LFrozen.WithExample(LSource.ExampleAt(LAt));
  end;
  LCount := LSource.RelatedCount;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumLinks) then
    raise ENyxDocumentation.Create('Captured documentation exceeds its related-link allowance');
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LFrozen := LFrozen.WithRelated(LSource.RelatedAt(LAt));
  end;
  TypeBytes(LFrozen);
  CheckCancellation(ACancellation);
  Result := LFrozen;
end;

end.
