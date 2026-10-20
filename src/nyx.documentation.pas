{ Nyx portable API documentation. MIT license, see LICENSE. }
unit nyx.documentation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.scheduler, nyx.pascal.types,
  nyx.pascal.syntax,
  nyx.documentation.typeexpressions;

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
    nor reflection; an alias retains its own qualified identity. Expression is
    a named compound construction, distinct from an alias or containing class. }
  TNyxDocumentationTypeKind = (ndtClass, ndtInterface, ndtRecord, ndtEnum,
    ndtAlias, ndtCallback, ndtUnit, ndtIntrinsic, ndtExpression);
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
  { Authored property clauses only. Inherited redeclarations may omit both;
    these values never prove effective access or permit a getter/setter call. }
  TNyxDocumentationPropertyAccess = (ndpaRead, ndpaWrite);
  { A copied clause set keeps declared read/write independent of target support. }
  TNyxDocumentationPropertyAccesses = set of TNyxDocumentationPropertyAccess;
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
  { Optional immutable property syntax. Ordered index parameters retain their
    declared types/modes/defaults. DefaultProperty means the separate member
    directive, never a scalar streaming default or an effective inherited rule.
    Absence of this capability leaves property details unknown. }
  INyxDocumentationProperty = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D103}']
    function ParameterCount: Integer;
    function ParameterAt(AIndex: Integer): TNyxDocumentationParameter;
    function DeclaredAccess: TNyxDocumentationPropertyAccesses;
    function DefaultProperty: Boolean;
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
  { Authored member modifiers are separate from containing-class ownership and
    effective compiler policy. Unspecified visibility and an absent convention
    remain those exact requests, without an inferred public/default ABI. }
  INyxDocumentationMemberIntent = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D116}']
    function DeclaredClass: Boolean;
    function DeclaredStatic: Boolean;
    function Visibility: TNyxPascalSyntaxVisibility;
    function CallingConvention: TNyxPascalRoutineConvention;
  end;
  { Optional managed intent composes with every existing member shape. A nil
    result is unknown, not a fabricated instance/public/default declaration. }
  INyxDocumentationMemberDeclaration = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D117}']
    function DeclarationIntent: INyxDocumentationMemberIntent;
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
  { A type use is a named declaration OR exact compound syntax. Expression name
    bindings are explicit ordered values; an undefined binding remains unbound,
    never a fabricated element alias or a compiler-resolved type. }
  INyxDocumentationTypeUse = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D110}']
    function Reference: TNyxDocumentationRef;
    function Expression: INyxDocumentationTypeExpression;
    function BindingCount: Integer;
    function BindingAt(AIndex: Integer): TNyxDocumentationRef;
    function WithBinding(AIndex: Integer; const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
  end;
  { Ordered authored interface applications retain exact syntax and explicit
    bindings. They grant neither compiler specialization nor member mappings. }
  INyxDocumentationInterfaceImplementations = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D119}']
    function Count: Integer;
    function ItemAt(AIndex: Integer): INyxDocumentationTypeUse;
  end;
  { Nil is unknown; a captured empty vector explicitly declares no interfaces.
    This relationship is separate from class bases and inherited member lookup. }
  INyxDocumentationDeclaredInterfaces = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D11A}']
    function ImplementedInterfaces: INyxDocumentationInterfaceImplementations;
  end;
  { Managed parameter metadata carries compound syntax outside the unchanged
    copied named-parameter record. Defaults are inert, exact Pascal text. }
  INyxDocumentationParameterDeclaration = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D111}']
    function Name: TNyxText;
    function TypeUse: INyxDocumentationTypeUse;
    function Passing: TNyxDocumentationParameterMode;
    function HasDefault: Boolean;
    function DefaultPascal: TNyxText;
    function WithDefault(const APascal: TNyxText): INyxDocumentationParameterDeclaration;
  end;
  { Dynamic managed storage keeps interfaces out of copied parameter records. }
  TNyxDocumentationParameterDeclarations = array of INyxDocumentationParameterDeclaration;
  { An immutable callable declaration can mix named and complex parameter/result
    uses. An absent result means no declared function result, as in the old API. }
  INyxDocumentationRichSignature = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D112}']
    function ParameterCount: Integer;
    function ParameterAt(AIndex: Integer): INyxDocumentationParameterDeclaration;
    function ResultUse: INyxDocumentationTypeUse;
  end;
  { Optional richer callable metadata. The old Signature is absent when the
    declaration cannot be projected truthfully into named-parameter records. }
  INyxDocumentationMemberSignature = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D113}']
    function DeclaredSignature: INyxDocumentationRichSignature;
  end;
  { Complex indices/value uses have a distinct capability. They never masquerade
    as simple named property records. Clauses/default remain declared syntax. }
  INyxDocumentationRichProperty = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D114}']
    function ParameterCount: Integer;
    function ParameterAt(AIndex: Integer): INyxDocumentationParameterDeclaration;
    function ValueUse: INyxDocumentationTypeUse;
    function DeclaredAccess: TNyxDocumentationPropertyAccesses;
    function DefaultProperty: Boolean;
  end;
  { A named compound declaration keeps its own nominal identity and exact
    declaration-position definition. Family belongs to the existing type grammar;
    it supplies no alias, inheritance, compiler layout or executable member. }
  INyxDocumentationDeclaredTypeExpression = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D115}']
    function Definition: INyxDocumentationTypeUse;
    function Family: TNyxPascalTypeKind;
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
{ Intent and attachment share the caller's construction. Nil attachment clears
  the fact to unknown; neither function infers source or compiler authority. }
function NewNyxDocumentationMemberIntent(ADeclaredClass, ADeclaredStatic: Boolean;
  AVisibility: TNyxPascalSyntaxVisibility;
  AConvention: TNyxPascalRoutineConvention;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationMemberIntent;
function WithNyxDocumentationMemberIntent(const AMember: INyxDocumentationMember;
  const AIntent: INyxDocumentationMemberIntent;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationMember;
{ Retain property declaration syntax without treating indexing as a call.
  The ordinary OfType setter supplies its value type; an omitted type remains
  explicitly unknown, including on an inherited property redeclaration. }
function NewNyxDocumentationProperty(const AIdentity: TNyxDocumentationRef;
  const ASummary: TNyxText; const AParameters: array of TNyxDocumentationParameter;
  ADeclaredAccess: TNyxDocumentationPropertyAccesses;
  ADefaultProperty: Boolean = False): INyxDocumentationMember;
function NewNyxDocumentationNamedTypeUse(const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
function NewNyxDocumentationExpressionTypeUse(const AExpression: INyxDocumentationTypeExpression;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTypeUse;
{ Capture ordered interface uses under one construction; no supplier or scope is
  retained. Nil attachment clears to unknown, unlike an explicit empty vector. }
function NewNyxDocumentationInterfaceImplementations(
  const AInterfaces: array of INyxDocumentationTypeUse;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationInterfaceImplementations;
function WithNyxDocumentationImplementedInterfaces(const AType: INyxDocumentationType;
  const AInterfaces: INyxDocumentationInterfaceImplementations;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationType;
function NewNyxDocumentationParameterDeclaration(const AName: TNyxText;
  const AType: INyxDocumentationTypeUse;
  AMode: TNyxDocumentationParameterMode = ndpValue): INyxDocumentationParameterDeclaration;
function NewNyxDocumentationRichSignature(const AParameters: TNyxDocumentationParameterDeclarations;
  const AResult: INyxDocumentationTypeUse;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationRichSignature;
function NewNyxDocumentationRichMember(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationMemberKind; const ASummary: TNyxText;
  const ASignature: INyxDocumentationRichSignature;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationMember;
function NewNyxDocumentationRichProperty(const AIdentity: TNyxDocumentationRef;
  const ASummary: TNyxText; const AParameters: TNyxDocumentationParameterDeclarations;
  const AValue: INyxDocumentationTypeUse; AAccess: TNyxDocumentationPropertyAccesses;
  ADefault: Boolean; const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationMember;
function NewNyxDocumentationType(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationTypeKind; const ASummary: TNyxText): INyxDocumentationType;
{ Array/set/procedural/distinct definitions use the caller's shared construction;
  named aliases and declaration bodies requiring source context refuse. }
function NewNyxDocumentationDeclaredType(const AIdentity: TNyxDocumentationRef;
  const ASummary: TNyxText; const ADefinition: INyxDocumentationTypeUse;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationType;
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

{ Rich fields retain separate syntax/qualified bindings; old byte paths do not
  call this growing traversal when neither optional capability is present. }
function RichMemberBytes(const AValue: INyxDocumentationMember): Integer; forward;
{ Individual rich owners share the old aggregate bound before publication. }
function TypeUseBytes(const AValue: INyxDocumentationTypeUse): Integer; forward;
{ A parameter includes its exact name, full type occurrence and inert default. }
function RichParameterBytes(const AValue: INyxDocumentationParameterDeclaration): Integer; forward;

function MemberBytes(const AValue: INyxDocumentationMember): Integer;
var
  LAt: Integer;
  LExample: TNyxDocumentationExample;
  LProperty: INyxDocumentationProperty;
  LParameter: TNyxDocumentationParameter;
  LDeclaration: INyxDocumentationMemberDeclaration;
  LIntent: INyxDocumentationMemberIntent;
begin
  Result := RefBytes(AValue.Identity) + NyxUTF8ByteCount(AValue.Summary) +
    RefBytes(AValue.ValueType) + SignatureBytes(AValue.Signature);
  Inc(Result, RichMemberBytes(AValue));

  if Supports(AValue, INyxDocumentationMemberDeclaration, LDeclaration) then
  begin
    LIntent := LDeclaration.DeclarationIntent;

    if LIntent <> nil then
      Inc(Result, 2 + Length(IntToStr(Ord(LIntent.Visibility))) +
        Length(IntToStr(Ord(LIntent.CallingConvention))));
  end;

  if Supports(AValue, INyxDocumentationProperty, LProperty) then
  begin
    Inc(Result, 3); { Two declared clause flags and one default-member flag. }
    for LAt := 0 to LProperty.ParameterCount - 1 do
    begin
      LParameter := LProperty.ParameterAt(LAt);
      Inc(Result, NyxUTF8ByteCount(LParameter.Name) + RefBytes(LParameter.TypeReference) +
        NyxUTF8ByteCount(LParameter.DefaultPascal));
      AdmitBytes(Result);
    end;
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

function TypeBytes(const AValue: INyxDocumentationType): Integer;
var
  LAt: Integer;
  LExample: TNyxDocumentationExample;
  LIntrinsic: INyxDocumentationIntrinsic;
  LTargets: INyxDocumentationAliasTargets;
  LDeclared: INyxDocumentationDeclaredTypeExpression;
  LDeclaration: INyxDocumentationDeclaredInterfaces;
  LInterfaces: INyxDocumentationInterfaceImplementations;
begin
  Result := RefBytes(AValue.Identity) + NyxUTF8ByteCount(AValue.Summary) + RefBytes(AValue.AliasTarget);

  if Supports(AValue, INyxDocumentationDeclaredInterfaces, LDeclaration) then
  begin
    LInterfaces := LDeclaration.ImplementedInterfaces;

    if LInterfaces <> nil then
    begin
      Inc(Result, Length(IntToStr(LInterfaces.Count)));
      for LAt := 0 to LInterfaces.Count - 1 do
      begin
        Inc(Result, TypeUseBytes(LInterfaces.ItemAt(LAt)));
        AdmitBytes(Result);
      end;
    end;
  end;

  if Supports(AValue, INyxDocumentationIntrinsic, LIntrinsic) then
    Inc(Result, Length(IntToStr(Ord(LIntrinsic.Family))));

  if Supports(AValue, INyxDocumentationDeclaredTypeExpression, LDeclared) then
    Inc(Result, TypeUseBytes(LDeclared.Definition) + Length(IntToStr(Ord(LDeclared.Family))));

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
  { Operation-local managed rows preserve the exact borrowed item topology;
    published vectors never retain these supplier observations. }
  TDocumentationTypeUseVector = array of INyxDocumentationTypeUse;
  { An attachment carries its inner capture's final observations only until
    publication, preventing an outer getter from concealing a nil/topology swap. }
  TImplementationObservation = class
  public
    Capability: INyxDocumentationDeclaredInterfaces;
    Source: INyxDocumentationInterfaceImplementations;
    Items: TDocumentationTypeUseVector;
    Frozen: INyxDocumentationInterfaceImplementations;
  end;
  { Construction-private storage publishes only captured uses. No mutable
    source, admission or cancellation survives its factory/capture operation. }
  TDocumentationInterfaceImplementations = class(TInterfacedObject,
    INyxDocumentationInterfaceImplementations)
  public
    Items: array of INyxDocumentationTypeUse;
    function Count: Integer;
    function ItemAt(AIndex: Integer): INyxDocumentationTypeUse;
  end;
  { Forward captured member class for exact private immutable-owner admission. }
  TDocumentationMember = class;
  { A private borrow is valid only with the owner's exact public interface.
    Alternate producers cannot borrow another member's no-copy authority. }
  IDocumentationMemberOwner = interface(IInterface)
    ['{014254DA-C69D-48CD-B14E-6718EB13D118}']
    function MemberOwner: TDocumentationMember;
  end;
  { Four scalar facts retain no source, cancellation, admission or executable
    supplier. Capturing an alternate always produces this private value owner. }
  TDocumentationMemberIntent = class(TInterfacedObject, INyxDocumentationMemberIntent)
  public
    ClassModifier, StaticModifier: Boolean;
    DeclaredVisibility: TNyxPascalSyntaxVisibility;
    Convention: TNyxPascalRoutineConvention;
    function DeclaredClass: Boolean;
    function DeclaredStatic: Boolean;
    function Visibility: TNyxPascalSyntaxVisibility;
    function CallingConvention: TNyxPascalRoutineConvention;
  end;
  { The private type owner permits a captured member to be appended without a
    second reconstruction. Only this unit creates or borrows that owner. }
  TDocumentationType = class;
  { This private capability carries no public API slots or producer authority;
    capture verifies its exact canonical type interface before borrowing it. }
  IDocumentationTypeOwner = interface(IInterface)
    ['{DA3CF3CB-5263-49B0-A338-9B92575FF8CA}']
    function TypeOwner: TDocumentationType;
  end;
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
  TDocumentationMember = class(TDocumentationContent, INyxDocumentationMember,
    INyxDocumentationMemberDeclaration, IDocumentationMemberOwner)
  private
    FIdentity: TNyxDocumentationRef;
    FKind: TNyxDocumentationMemberKind;
    FSignature: INyxDocumentationSignature;
    FValueType: TNyxDocumentationRef;
    FIntent: INyxDocumentationMemberIntent;
  protected
    procedure CopyMember(ATarget: TDocumentationMember);
    function CopyValue: TDocumentationMember; virtual;
    procedure AssignSignature(const AValue: INyxDocumentationSignature); virtual;
    procedure AssignValueType(const AValue: TNyxDocumentationRef); virtual;
  public
    function MemberOwner: TDocumentationMember;
    function DeclarationIntent: INyxDocumentationMemberIntent;
    function Identity: TNyxDocumentationRef;
    function Kind: TNyxDocumentationMemberKind;
    function Signature: INyxDocumentationSignature;
    function ValueType: TNyxDocumentationRef;
    function WithSignature(const AValue: INyxDocumentationSignature): INyxDocumentationMember;
    function OfType(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
    function WithExample(const AValue: TNyxDocumentationExample): INyxDocumentationMember;
    function WithRelated(const AValue: TNyxDocumentationRef): INyxDocumentationMember;
  end;
  { Index parameters reuse the admitted immutable parameter owner internally;
    its absent result is never exposed as a callable property Signature.
    Specialized virtual copying retains shape through ordinary fluent methods. }
  TDocumentationProperty = class(TDocumentationMember, INyxDocumentationProperty)
  private
    FParameters: INyxDocumentationSignature;
    FAccess: TNyxDocumentationPropertyAccesses;
    FDefault: Boolean;
  protected
    function CopyValue: TDocumentationMember; override;
  public
    function ParameterCount: Integer;
    function ParameterAt(AIndex: Integer): TNyxDocumentationParameter;
    function DeclaredAccess: TNyxDocumentationPropertyAccesses;
    function DefaultProperty: Boolean;
  end;
  { Frozen compound use owns its exact expression and copied binding vector.
    Named uses retain a real reference and no expression or binding rows. }
  TDocumentationTypeUse = class(TInterfacedObject, INyxDocumentationTypeUse)
  public
    Named: TNyxDocumentationRef;
    Compound: INyxDocumentationTypeExpression;
    Bindings: array of TNyxDocumentationRef;
    function Reference: TNyxDocumentationRef;
    function Expression: INyxDocumentationTypeExpression;
    function BindingCount: Integer;
    function BindingAt(AIndex: Integer): TNyxDocumentationRef;
    function WithBinding(AIndex: Integer; const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
  end;
  { A managed immutable parameter is deliberately separate from the old named
    record, whose strict constructor and capture semantics stay unchanged. }
  TDocumentationParameterDeclaration = class(TInterfacedObject, INyxDocumentationParameterDeclaration)
  public
    ParameterName: TNyxText;
    ParameterType: INyxDocumentationTypeUse;
    Mode: TNyxDocumentationParameterMode;
    DefaultPresent: Boolean;
    DefaultText: TNyxText;
    function Name: TNyxText;
    function TypeUse: INyxDocumentationTypeUse;
    function Passing: TNyxDocumentationParameterMode;
    function HasDefault: Boolean;
    function DefaultPascal: TNyxText;
    function WithDefault(const APascal: TNyxText): INyxDocumentationParameterDeclaration;
  end;
  { Rich callable owns only captured parameter/type uses, with no supplier or
    construction scope retained by its dynamic interface vector. }
  TDocumentationRichSignature = class(TInterfacedObject, INyxDocumentationRichSignature)
  public
    Parameters: TNyxDocumentationParameterDeclarations;
    ReturnUse: INyxDocumentationTypeUse;
    function ParameterCount: Integer;
    function ParameterAt(AIndex: Integer): INyxDocumentationParameterDeclaration;
    function ResultUse: INyxDocumentationTypeUse;
  end;
  { Ordinary fluent copies retain rich callable data. Explicit legacy signature
    replacement clears it through a virtual assignment hook. }
  TDocumentationRichMember = class(TDocumentationMember, INyxDocumentationMemberSignature)
  public
    RichSignature: INyxDocumentationRichSignature;
    function DeclaredSignature: INyxDocumentationRichSignature;
  protected
    function CopyValue: TDocumentationMember; override;
    procedure AssignSignature(const AValue: INyxDocumentationSignature); override;
  end;
  { Complex property facts stay specialized through fluent copies. OfType
    replaces its value use only, preserving every captured index expression. }
  TDocumentationRichProperty = class(TDocumentationMember, INyxDocumentationRichProperty)
  public
    Parameters: TNyxDocumentationParameterDeclarations;
    PropertyValue: INyxDocumentationTypeUse;
    Access: TNyxDocumentationPropertyAccesses;
    IsDefault: Boolean;
    function ParameterCount: Integer;
    function ParameterAt(AIndex: Integer): INyxDocumentationParameterDeclaration;
    function ValueUse: INyxDocumentationTypeUse;
    function DeclaredAccess: TNyxDocumentationPropertyAccesses;
    function DefaultProperty: Boolean;
  protected
    function CopyValue: TDocumentationMember; override;
    procedure AssignValueType(const AValue: TNyxDocumentationRef); override;
  end;
  { Member interfaces live in a dynamic managed vector. Base/alias links remain
    data references, preventing an inherited metadata graph from owning cycles. }
  TDocumentationType = class(TDocumentationContent, INyxDocumentationType,
    IDocumentationTypeOwner, INyxDocumentationDeclaredInterfaces)
  private
    FIdentity: TNyxDocumentationRef;
    FKind: TNyxDocumentationTypeKind;
    FAlias: TNyxDocumentationRef;
    FBases: array of TNyxDocumentationRef;
    FMembers: array of INyxDocumentationMember;
    FImplemented: INyxDocumentationInterfaceImplementations;
    function AppendCapturedMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
  protected
    procedure CopyType(ATarget: TDocumentationType);
    function CopyValue: TDocumentationType; virtual;
    procedure AssignAlias(const ATarget: TNyxDocumentationRef); virtual;
  public
    function TypeOwner: TDocumentationType;
    function ImplementedInterfaces: INyxDocumentationInterfaceImplementations;
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
  { A captured definition owns only immutable expression/binding data. Virtual
    copying keeps this capability through prose/link edits without retaining the
    source producer, construction allowance or containing source snapshot. }
  TDocumentationDeclaredType = class(TDocumentationType, INyxDocumentationDeclaredTypeExpression)
  private
    FDefinition: INyxDocumentationTypeUse;
    FFamily: TNyxPascalTypeKind;
  protected
    function CopyValue: TDocumentationType; override;
  public
    function Definition: INyxDocumentationTypeUse;
    function Family: TNyxPascalTypeKind;
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

procedure CheckMemberIdentity(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationMemberKind; const ASummary: TNyxText);
begin
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
end;

function NewNyxDocumentationMember(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationMemberKind; const ASummary: TNyxText): INyxDocumentationMember;
var
  LValue: TDocumentationMember;
begin
  Result := nil;
  CheckMemberIdentity(AIdentity, AKind, ASummary);
  LValue := TDocumentationMember.Create;
  LValue.FIdentity := AIdentity;
  LValue.FKind := AKind;
  LValue.FSummary := ASummary;
  Result := LValue;
end;

function TDocumentationMemberIntent.DeclaredClass: Boolean;
begin
  Result := ClassModifier;
end;

function TDocumentationMemberIntent.DeclaredStatic: Boolean;
begin
  Result := StaticModifier;
end;

function TDocumentationMemberIntent.Visibility: TNyxPascalSyntaxVisibility;
begin
  Result := DeclaredVisibility;
end;

function TDocumentationMemberIntent.CallingConvention: TNyxPascalRoutineConvention;
begin
  Result := Convention;
end;

function NewNyxDocumentationMemberIntent(ADeclaredClass, ADeclaredStatic: Boolean;
  AVisibility: TNyxPascalSyntaxVisibility;
  AConvention: TNyxPascalRoutineConvention;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationMemberIntent;
var
  LOwner: TDocumentationMemberIntent;
  LLease: INyxDocumentationMemberIntent;
begin
  Result := nil;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  AConstruction.Spend;

  if (Ord(AVisibility) < Ord(Low(TNyxPascalSyntaxVisibility))) or
    (Ord(AVisibility) > Ord(High(TNyxPascalSyntaxVisibility))) or
    (Ord(AConvention) < Ord(Low(TNyxPascalRoutineConvention))) or
    (Ord(AConvention) > Ord(High(TNyxPascalRoutineConvention))) then
    raise ENyxDocumentation.Create('Unknown authored member visibility or calling convention');
  { Scalar text costs are bounded before allocating the immutable owner. These
    are logical metadata bytes; final registration framing has its own owner. }
  AConstruction.AdmitText(IntToStr(Ord(ADeclaredClass)));
  AConstruction.AdmitText(IntToStr(Ord(ADeclaredStatic)));
  AConstruction.AdmitText(IntToStr(Ord(AVisibility)));
  AConstruction.AdmitText(IntToStr(Ord(AConvention)));
  AConstruction.Spend;
  CheckCancellation(AConstruction);
  LOwner := TDocumentationMemberIntent.Create;
  LLease := LOwner;
  LOwner.ClassModifier := ADeclaredClass;
  LOwner.StaticModifier := ADeclaredStatic;
  LOwner.DeclaredVisibility := AVisibility;
  LOwner.Convention := AConvention;
  CheckCancellation(AConstruction);
  Result := LLease;
end;

{ Check each borrowed scalar under shared admission. An outward admission or
  getter can cancel; no later intent getter runs after that revocation. }
function SameMemberIntent(const AValue, AFrozen: INyxDocumentationMemberIntent;
  const AScope: INyxDocumentationTypeConstruction): Boolean;
var
  LClass, LStatic: Boolean;
  LVisibility: TNyxPascalSyntaxVisibility;
  LConvention: TNyxPascalRoutineConvention;
begin
  Result := False;
  AScope.Spend;
  CheckCancellation(AScope);
  LClass := AValue.DeclaredClass;
  AScope.Spend;
  CheckCancellation(AScope);
  LStatic := AValue.DeclaredStatic;
  AScope.Spend;
  CheckCancellation(AScope);
  LVisibility := AValue.Visibility;
  AScope.Spend;
  CheckCancellation(AScope);
  LConvention := AValue.CallingConvention;
  CheckCancellation(AScope);
  Result := (LClass = AFrozen.DeclaredClass) and (LStatic = AFrozen.DeclaredStatic) and
    (LVisibility = AFrozen.Visibility) and (LConvention = AFrozen.CallingConvention);
end;

{ Fixed scalar observation is always admitted before a borrowed getter. Final
  agreement includes all four facts, followed by cancellation before publication. }
function CaptureMemberIntent(const AValue: INyxDocumentationMemberIntent;
  const AScope: INyxDocumentationTypeConstruction): INyxDocumentationMemberIntent;
var
  LClass, LStatic: Boolean;
  LVisibility: TNyxPascalSyntaxVisibility;
  LConvention: TNyxPascalRoutineConvention;
  LFrozen: INyxDocumentationMemberIntent;
begin
  Result := nil;

  if AValue = nil then
    Exit;
  AScope.Spend;
  CheckCancellation(AScope);
  LClass := AValue.DeclaredClass;
  AScope.Spend;
  CheckCancellation(AScope);
  LStatic := AValue.DeclaredStatic;
  AScope.Spend;
  CheckCancellation(AScope);
  LVisibility := AValue.Visibility;
  AScope.Spend;
  CheckCancellation(AScope);
  LConvention := AValue.CallingConvention;
  CheckCancellation(AScope);
  LFrozen := NewNyxDocumentationMemberIntent(LClass, LStatic, LVisibility, LConvention, AScope);

  if not SameMemberIntent(AValue, LFrozen, AScope) then
    raise ENyxDocumentation.Create('Authored member intent changed during capture');
  CheckCancellation(AScope);
  Result := LFrozen;
end;

{ Only an exact captured interface permits private copying or the legacy nil
  fast path. Mutable alternate implementations receive no ownership receipt. }
function CapturedMemberOwner(const AValue: INyxDocumentationMember): TDocumentationMember;
var
  LPrivate: IDocumentationMemberOwner;
  LCanonical: INyxDocumentationMember;
begin
  Result := nil;

  if not Supports(AValue, IDocumentationMemberOwner, LPrivate) then
    Exit;
  Result := LPrivate.MemberOwner;

  if Result = nil then
    raise ENyxDocumentation.Create('Missing private captured member owner');
  LCanonical := Result;

  if LCanonical <> AValue then
    raise ENyxDocumentation.Create('An alternate member cannot borrow another captured owner');
end;

procedure CheckMemberIntent(const AMember: INyxDocumentationMember;
  const AIntent: INyxDocumentationMemberIntent);
begin

  if AIntent = nil then
    Exit;

  if AMember.Identity.Owner.IsUnit and (AIntent.DeclaredClass or AIntent.DeclaredStatic) then
    raise ENyxDocumentation.Create('A unit member cannot invent an authored class or static modifier');

  if not (AMember.Kind in [ndmConstructor, ndmFactory, ndmMethod,
    ndmFluentMethod, ndmRoutine]) and (AIntent.CallingConvention <> prcDefault) then
    raise ENyxDocumentation.Create('Only callable members own an authored calling convention');

  if (AMember.Kind = ndmEnumValue) and (AIntent.DeclaredClass or AIntent.DeclaredStatic) then
    raise ENyxDocumentation.Create('An enum value cannot declare a class or static modifier');
end;

function TDocumentationMember.MemberOwner: TDocumentationMember;
begin
  Result := Self;
end;

function TDocumentationMember.DeclarationIntent: INyxDocumentationMemberIntent;
begin
  Result := FIntent;
end;

function NewNyxDocumentationProperty(const AIdentity: TNyxDocumentationRef;
  const ASummary: TNyxText; const AParameters: array of TNyxDocumentationParameter;
  ADeclaredAccess: TNyxDocumentationPropertyAccesses;
  ADefaultProperty: Boolean): INyxDocumentationMember;
var
  LParameters: INyxDocumentationSignature;
  LOwner: TDocumentationProperty;
  LLease: INyxDocumentationMember;
begin
  Result := nil;
  CheckMemberIdentity(AIdentity, ndmProperty, ASummary);
  LParameters := NewNyxDocumentationSignature(AParameters, Default(TNyxDocumentationRef));
  LOwner := TDocumentationProperty.Create;
  LLease := LOwner;
  LOwner.FIdentity := AIdentity;
  LOwner.FKind := ndmProperty;
  LOwner.FSummary := ASummary;
  LOwner.FParameters := LParameters;
  LOwner.FAccess := ADeclaredAccess;
  LOwner.FDefault := ADefaultProperty;
  MemberBytes(LLease);
  Result := LLease;
end;

procedure TDocumentationMember.CopyMember(ATarget: TDocumentationMember);
begin
  CopyContent(ATarget);
  ATarget.FIdentity := FIdentity;
  ATarget.FKind := FKind;
  ATarget.FSignature := FSignature;
  ATarget.FValueType := FValueType;
  ATarget.FIntent := FIntent;
end;

function TDocumentationMember.CopyValue: TDocumentationMember;
begin
  Result := TDocumentationMember.Create;
  CopyMember(Result);
end;

function TDocumentationProperty.CopyValue: TDocumentationMember;
var
  LCopy: TDocumentationProperty;
begin
  LCopy := TDocumentationProperty.Create;
  CopyMember(LCopy);
  LCopy.FParameters := FParameters;
  LCopy.FAccess := FAccess;
  LCopy.FDefault := FDefault;
  Result := LCopy;
end;

function TDocumentationProperty.ParameterCount: Integer;
begin
  Result := FParameters.ParameterCount;
end;

function TDocumentationProperty.ParameterAt(AIndex: Integer): TNyxDocumentationParameter;
begin
  Result := FParameters.ParameterAt(AIndex);
end;

function TDocumentationProperty.DeclaredAccess: TNyxDocumentationPropertyAccesses;
begin
  Result := FAccess;
end;

function TDocumentationProperty.DefaultProperty: Boolean;
begin
  Result := FDefault;
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

procedure TDocumentationMember.AssignSignature(const AValue: INyxDocumentationSignature);
begin
  FSignature := AValue;
end;

procedure TDocumentationMember.AssignValueType(const AValue: TNyxDocumentationRef);
begin
  FValueType := AValue;
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
  LNext.AssignSignature(LFrozen);
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
  LNext.AssignValueType(AValue);
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

function DocumentationConstruction(const ACancellation: INyxCancellationScope;
  out AOwned: Boolean): INyxDocumentationTypeConstruction;
var
  LOuter: INyxDocumentationTypeAdmission;
begin
  AOwned := not Supports(ACancellation, INyxDocumentationTypeConstruction, Result);

  if AOwned then
  begin
    Supports(ACancellation, INyxDocumentationTypeAdmission, LOuter);
    try
      Result := NewNyxDocumentationTypeConstruction(
        TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(NyxDocumentationMaximumBytes), ACancellation, LOuter);
    except
      on E: ENyxDocumentationTypeExpressionCancelled do
        raise ENyxDocumentationCancelled.Create(E.Message);
    end;
  end
  else
    CheckNyxDocumentationTypeConstruction(Result);
end;

procedure RequireTypeReference(const AValue: TNyxDocumentationRef);
begin
  AValue.Validate;

  if AValue.IsUnit or AValue.IsMember then
    raise ENyxDocumentation.Create('A declared type use requires its actual named type');
end;

function TDocumentationTypeUse.Reference: TNyxDocumentationRef;
begin
  Result := Named;
end;

function TDocumentationTypeUse.Expression: INyxDocumentationTypeExpression;
begin
  Result := Compound;
end;

function TDocumentationTypeUse.BindingCount: Integer;
begin
  Result := Length(Bindings);
end;

function TDocumentationTypeUse.BindingAt(AIndex: Integer): TNyxDocumentationRef;
begin

  if (AIndex < 0) or (AIndex >= Length(Bindings)) then
    raise ENyxDocumentation.Create('Type-expression binding ordinal out of range');
  Result := Bindings[AIndex];
end;

procedure CheckTypeBinding(const AExpression: INyxDocumentationTypeExpression;
  AIndex: Integer; const AReference: TNyxDocumentationRef);
begin
  RequireTypeReference(AReference);

  if (AExpression = nil) or (AReference.GenericArity <> AExpression.NamedUseArity(AIndex)) then
    raise ENyxDocumentation.Create('Binding must retain the exact named-use generic arity');
end;

function TDocumentationTypeUse.WithBinding(AIndex: Integer;
  const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
var
  LNext: TDocumentationTypeUse;
  LLease: INyxDocumentationTypeUse;
  LAt, LBytes: Integer;
begin
  BindingAt(AIndex);
  CheckTypeBinding(Compound, AIndex, AReference);
  LBytes := TypeUseBytes(Self) - RefBytes(Bindings[AIndex]) + RefBytes(AReference);
  AdmitBytes(LBytes);
  LNext := TDocumentationTypeUse.Create;
  LLease := LNext;
  LNext.Compound := Compound;
  SetLength(LNext.Bindings, Length(Bindings));
  for LAt := 0 to High(Bindings) do
    LNext.Bindings[LAt] := Bindings[LAt];
  LNext.Bindings[AIndex] := AReference;
  Result := LLease;
end;

function NewNyxDocumentationNamedTypeUse(const AReference: TNyxDocumentationRef): INyxDocumentationTypeUse;
var
  LOwner: TDocumentationTypeUse;
begin
  Result := nil;
  RequireTypeReference(AReference);
  LOwner := TDocumentationTypeUse.Create;
  LOwner.Named := AReference;
  Result := LOwner;
end;

function NewNyxDocumentationExpressionTypeUse(const AExpression: INyxDocumentationTypeExpression;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationTypeUse;
var
  LOwner: TDocumentationTypeUse;
  LLease: INyxDocumentationTypeUse;
  LFrozen: INyxDocumentationTypeExpression;
begin
  Result := nil;
  LFrozen := CaptureNyxDocumentationTypeExpression(AExpression, AConstruction);
  LOwner := TDocumentationTypeUse.Create;
  LLease := LOwner;
  LOwner.Compound := LFrozen;
  AConstruction.Spend(LFrozen.NamedUseCount + 1);
  SetLength(LOwner.Bindings, LFrozen.NamedUseCount);
  AConstruction.Spend;
  Result := LLease;
end;

function CaptureTypeUse(const AValue: INyxDocumentationTypeUse;
  const AScope: INyxDocumentationTypeConstruction): INyxDocumentationTypeUse;
var
  LReference, LBound: TNyxDocumentationRef;
  LExpression: INyxDocumentationTypeExpression;
  LFrozen: INyxDocumentationTypeUse;
  LCount, LAt: Integer;
  LOwner: TDocumentationTypeUse;
  LBytes: Integer;
begin
  Result := nil;
  AScope.Spend;

  if AValue = nil then
    raise ENyxDocumentation.Create('A managed parameter needs its named type or exact expression');
  LReference := AValue.Reference;
  AScope.Spend;
  LExpression := AValue.Expression;
  AScope.Spend;
  LCount := AValue.BindingCount;

  if LReference.Defined then
  begin

    if (LExpression <> nil) or (LCount <> 0) then
      raise ENyxDocumentation.Create('A type use cannot claim both a named type and compound syntax');
    AScope.AdmitText(LReference.Key);
    LFrozen := NewNyxDocumentationNamedTypeUse(LReference);
  end
  else
  begin

    if (LExpression = nil) or (LCount < 0) or (LCount > AScope.Limits.Types.MaximumNodes) then
      raise ENyxDocumentation.Create('Invalid compound type-use binding count');
    LOwner := TDocumentationTypeUse.Create;
    LFrozen := LOwner;
    LOwner.Compound := CaptureNyxDocumentationTypeExpression(LExpression, AScope);

    if LOwner.Compound.NamedUseCount <> LCount then
      raise ENyxDocumentation.Create('Every named expression occurrence needs its explicit binding slot');
    LBytes := NyxUTF8ByteCount(LOwner.Compound.Text) + 1;
    AdmitBytes(LBytes);
    AScope.Spend(LCount + 1);
    SetLength(LOwner.Bindings, LCount);
    for LAt := 0 to LCount - 1 do
    begin
      AScope.Spend;
      LBound := AValue.BindingAt(LAt);

      if LBound.Defined then
      begin
        CheckTypeBinding(LOwner.Compound, LAt, LBound);
        Inc(LBytes, RefBytes(LBound));
        AdmitBytes(LBytes);
        AScope.AdmitText(LBound.Key);
        LOwner.Bindings[LAt] := LBound;
      end;
    end;
    for LAt := 0 to LCount - 1 do
    begin
      AScope.Spend;
      LBound := AValue.BindingAt(LAt);

      if (LBound.Defined <> LFrozen.BindingAt(LAt).Defined) or
        (LBound.Defined and not LBound.Same(LFrozen.BindingAt(LAt))) then
        raise ENyxDocumentation.Create('Expression binding changed during capture');
    end;
  end;
  AScope.Spend;

  if (AValue.BindingCount <> LCount) or (AValue.Expression <> LExpression) or
    (AValue.Reference.Defined <> LReference.Defined) or
    (LReference.Defined and not AValue.Reference.Same(LReference)) then
    raise ENyxDocumentation.Create('Type-use envelope changed before publication');
  CheckCancellation(AScope);
  Result := LFrozen;
end;

procedure InterfaceWork(const AScope: INyxDocumentationTypeConstruction;
  AUnits: Integer = 1);
begin
  CheckCancellation(AScope);
  AScope.Spend(AUnits);
  CheckCancellation(AScope);
end;

function TDocumentationInterfaceImplementations.Count: Integer;
begin
  Result := Length(Items);
end;

function TDocumentationInterfaceImplementations.ItemAt(AIndex: Integer): INyxDocumentationTypeUse;
begin

  if (AIndex < 0) or (AIndex >= Count) then
    raise ENyxDocumentation.Create('Implemented-interface ordinal is out of range');
  Result := Items[AIndex];
end;

{ Root is a checked designator, never an arbitrary first binding or a guessed
  interface for an element/pointee. Named generics need their actual arguments. }
function ImplementedRootSlot(const AUse: INyxDocumentationTypeUse;
  const AScope: INyxDocumentationTypeConstruction): Integer;
var
  LExpression: INyxDocumentationTypeExpression;
  LSyntax: INyxPascalTypeSyntax;
  LRoot, LAt: Integer;
begin
  InterfaceWork(AScope);

  if AUse.Reference.Defined then
  begin

    if AUse.Reference.GenericArity <> 0 then
      raise ENyxDocumentation.Create('A generic interface needs its explicit application syntax');
    Exit(-1);
  end;
  LExpression := AUse.Expression;

  if (LExpression = nil) or (LExpression.Occurrence <> ndtoImplementedInterface) then
    raise ENyxDocumentation.Create('An implemented interface requires its declaration occurrence');
  LSyntax := LExpression.Syntax;
  LRoot := LSyntax.Root;

  if not (LSyntax.Node(LRoot).Kind in [ptxName, ptxQualified, ptxGeneric]) then
    raise ENyxDocumentation.Create('An implemented interface must be a named or generic designator');

  if LSyntax.Node(LRoot).Kind = ptxGeneric then
    LRoot := LSyntax.Child(LRoot, ptrBase);

  if (LRoot < 0) or not (LSyntax.Node(LRoot).Kind in [ptxName, ptxQualified]) then
    raise ENyxDocumentation.Create('A generic interface requires its checked root designator');
  for LAt := 0 to LExpression.NamedUseCount - 1 do
  begin
    InterfaceWork(AScope);

    if LExpression.NamedUseNode(LAt) = LRoot then
      Exit(LAt);
  end;
  raise ENyxDocumentation.Create('Missing implemented-interface root occurrence');
end;

function SameImplementedUse(const ALeft, ARight: INyxDocumentationTypeUse;
  const AScope: INyxDocumentationTypeConstruction): Boolean;
var
  LAt: Integer;
  LLeft, LRight: TNyxDocumentationRef;
begin
  InterfaceWork(AScope);
  Result := False;

  if ALeft.Reference.Defined <> ARight.Reference.Defined then
    Exit;

  if ALeft.Reference.Defined then
    Exit(ALeft.Reference.Same(ARight.Reference));
  InterfaceWork(AScope, Length(ALeft.Expression.Text) + Length(ARight.Expression.Text) + 1);

  if (ALeft.Expression.Text <> ARight.Expression.Text) or
    (ALeft.BindingCount <> ARight.BindingCount) then
    Exit;
  for LAt := 0 to ALeft.BindingCount - 1 do
  begin
    InterfaceWork(AScope);
    LLeft := ALeft.BindingAt(LAt);
    LRight := ARight.BindingAt(LAt);

    if (LLeft.Defined <> LRight.Defined) or (LLeft.Defined and not LLeft.Same(LRight)) then
      Exit;
  end;
  Result := True;
end;

{ Final borrowed observation performs no reconstruction or fresh allowance.
  It checks every used field/map, stopping before the next getter if cancelled. }
function SameImplementedObservation(const ASource, AFrozen: INyxDocumentationTypeUse;
  const AScope: INyxDocumentationTypeConstruction): Boolean;
var
  LReference, LBound, LExpected: TNyxDocumentationRef;
  LExpression, LFinalExpression: INyxDocumentationTypeExpression;
  LCount, LAt, LNode, LArity: Integer;
  LText: TNyxText;
  LRole: TNyxDocumentationTypeOccurrence;
begin
  Result := False;
  InterfaceWork(AScope);
  LReference := ASource.Reference;
  InterfaceWork(AScope);
  LExpression := ASource.Expression;
  InterfaceWork(AScope);
  LCount := ASource.BindingCount;
  CheckCancellation(AScope);

  if (LReference.Defined <> AFrozen.Reference.Defined) or
    (LReference.Defined and not LReference.Same(AFrozen.Reference)) or
    ((LExpression = nil) <> (AFrozen.Expression = nil)) or (LCount <> AFrozen.BindingCount) then
    Exit;

  if LExpression <> nil then
  begin
    InterfaceWork(AScope);
    LText := LExpression.Text;
    CheckCancellation(AScope);

    if Length(LText) > AScope.Limits.Lexical.MaximumSourceBytes then
      Exit;
    InterfaceWork(AScope, Length(LText) + 1);

    if LText <> AFrozen.Expression.Text then
      Exit;
    InterfaceWork(AScope);
    LRole := LExpression.Occurrence;
    InterfaceWork(AScope);
    LAt := LExpression.NamedUseCount;
    CheckCancellation(AScope);

    if (LRole <> ndtoImplementedInterface) or (LAt <> AFrozen.Expression.NamedUseCount) then
      Exit;
    for LAt := 0 to LCount - 1 do
    begin
      InterfaceWork(AScope);
      LNode := LExpression.NamedUseNode(LAt);
      InterfaceWork(AScope);
      LArity := LExpression.NamedUseArity(LAt);
      CheckCancellation(AScope);

      if (LNode <> AFrozen.Expression.NamedUseNode(LAt)) or
        (LArity <> AFrozen.Expression.NamedUseArity(LAt)) then
        Exit;
    end;
  end;
  for LAt := 0 to LCount - 1 do
  begin
    InterfaceWork(AScope);
    LBound := ASource.BindingAt(LAt);
    CheckCancellation(AScope);
    LExpected := AFrozen.BindingAt(LAt);

    if (LBound.Defined <> LExpected.Defined) or (LBound.Defined and not LBound.Same(LExpected)) then
      Exit;
  end;
  InterfaceWork(AScope);
  LBound := ASource.Reference;
  InterfaceWork(AScope);
  LFinalExpression := ASource.Expression;
  InterfaceWork(AScope);
  LAt := ASource.BindingCount;
  CheckCancellation(AScope);
  Result := (LBound.Defined = LReference.Defined) and
    (not LReference.Defined or LReference.Same(LBound)) and
    (LFinalExpression = LExpression) and (LAt = LCount);

  if Result and (LExpression <> nil) then
  begin
    InterfaceWork(AScope);
    LText := LExpression.Text;
    CheckCancellation(AScope);

    if Length(LText) > AScope.Limits.Lexical.MaximumSourceBytes then
      Exit(False);
    InterfaceWork(AScope, Length(LText) + 1);

    if LText <> AFrozen.Expression.Text then
      Exit(False);
    InterfaceWork(AScope);
    LRole := LExpression.Occurrence;
    InterfaceWork(AScope);
    LAt := LExpression.NamedUseCount;
    CheckCancellation(AScope);
    Result := (LRole = ndtoImplementedInterface) and (LAt = LCount);
  end;
end;

function CaptureInterfaceImplementations(const AValue: INyxDocumentationInterfaceImplementations;
  const AScope: INyxDocumentationTypeConstruction;
  out AObserved: TDocumentationTypeUseVector): INyxDocumentationInterfaceImplementations;
var
  LOwner: TDocumentationInterfaceImplementations;
  LLease: INyxDocumentationInterfaceImplementations;
  LSources: array of INyxDocumentationTypeUse;
  LCurrent: INyxDocumentationTypeUse;
  LCount, LAt, LOther, LBytes, LFinalCount: Integer;
begin
  Result := nil;
  AObserved := nil;

  if AValue = nil then
    Exit;
  InterfaceWork(AScope);
  LCount := AValue.Count;
  CheckCancellation(AScope);

  if (LCount < 0) or (LCount > NyxDocumentationMaximumLinks) then
    raise ENyxDocumentation.Create('Implemented-interface vector allowance exhausted');
  AScope.AdmitText(IntToStr(LCount));
  InterfaceWork(AScope, 2 * LCount + 1);
  LOwner := TDocumentationInterfaceImplementations.Create;
  LLease := LOwner;
  SetLength(LOwner.Items, LCount);
  SetLength(LSources, LCount);
  LBytes := Length(IntToStr(LCount));
  for LAt := 0 to LCount - 1 do
  begin
    InterfaceWork(AScope);
    LSources[LAt] := AValue.ItemAt(LAt);
    CheckCancellation(AScope);
    LOwner.Items[LAt] := CaptureTypeUse(LSources[LAt], AScope);
    ImplementedRootSlot(LOwner.Items[LAt], AScope);
    Inc(LBytes, TypeUseBytes(LOwner.Items[LAt]));
    AdmitBytes(LBytes);
    for LOther := 0 to LAt - 1 do
    begin

      if SameImplementedUse(LOwner.Items[LAt], LOwner.Items[LOther], AScope) then
        raise ENyxDocumentation.Create('An implemented-interface vector repeats a declaration use');
    end;
  end;
  for LAt := 0 to LCount - 1 do
  begin
    InterfaceWork(AScope);
    LCurrent := AValue.ItemAt(LAt);
    CheckCancellation(AScope);

    if (LCurrent <> LSources[LAt]) or
      not SameImplementedObservation(LCurrent, LOwner.Items[LAt], AScope) then
      raise ENyxDocumentation.Create('Implemented-interface topology or use changed before publication');
  end;
  InterfaceWork(AScope);
  LFinalCount := AValue.Count;
  CheckCancellation(AScope);

  if LFinalCount <> LCount then
    raise ENyxDocumentation.Create('Implemented-interface count changed before publication');
  AObserved := LSources;
  Result := LLease;
end;

function NewNyxDocumentationInterfaceImplementations(
  const AInterfaces: array of INyxDocumentationTypeUse;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationInterfaceImplementations;
var
  LOwner: TDocumentationInterfaceImplementations;
  LSource, LFrozen: INyxDocumentationInterfaceImplementations;
  LObserved: TDocumentationTypeUseVector;
  LAt: Integer;
begin
  Result := nil;
  CheckNyxDocumentationTypeConstruction(AConstruction);

  if Length(AInterfaces) > NyxDocumentationMaximumLinks then
    raise ENyxDocumentation.Create('Implemented-interface vector allowance exhausted');
  InterfaceWork(AConstruction, Length(AInterfaces) + 1);
  LOwner := TDocumentationInterfaceImplementations.Create;
  LSource := LOwner;
  SetLength(LOwner.Items, Length(AInterfaces));
  for LAt := 0 to High(AInterfaces) do
    LOwner.Items[LAt] := AInterfaces[LAt];
  LFrozen := CaptureInterfaceImplementations(LSource, AConstruction, LObserved);
  for LAt := 0 to High(AInterfaces) do
  begin
    InterfaceWork(AConstruction);

    if AInterfaces[LAt] <> LOwner.Items[LAt] then
      raise ENyxDocumentation.Create('Input interface use changed during capture');
  end;
  CheckCancellation(AConstruction);
  Result := LFrozen;
end;

function TDocumentationParameterDeclaration.Name: TNyxText;
begin
  Result := ParameterName;
end;

function TDocumentationParameterDeclaration.TypeUse: INyxDocumentationTypeUse;
begin
  Result := ParameterType;
end;

function TDocumentationParameterDeclaration.Passing: TNyxDocumentationParameterMode;
begin
  Result := Mode;
end;

function TDocumentationParameterDeclaration.HasDefault: Boolean;
begin
  Result := DefaultPresent;
end;

function TDocumentationParameterDeclaration.DefaultPascal: TNyxText;
begin
  Result := DefaultText;
end;

function TDocumentationParameterDeclaration.WithDefault(const APascal: TNyxText): INyxDocumentationParameterDeclaration;
var
  LNext: TDocumentationParameterDeclaration;
begin
  Result := nil;
  TextBound(APascal, 1024);

  if (APascal = '') or (Mode in [ndpVar, ndpOut]) then
    raise ENyxDocumentation.Create('This parameter cannot declare an empty or writable default');
  AdmitBytes(NyxUTF8ByteCount(ParameterName) + TypeUseBytes(ParameterType) + NyxUTF8ByteCount(APascal));
  LNext := TDocumentationParameterDeclaration.Create;
  LNext.ParameterName := ParameterName;
  LNext.ParameterType := ParameterType;
  LNext.Mode := Mode;
  LNext.DefaultPresent := True;
  LNext.DefaultText := APascal;
  Result := LNext;
end;

function CaptureParameter(const AValue: INyxDocumentationParameterDeclaration;
  AOccurrence: TNyxDocumentationTypeOccurrence;
  const AScope: INyxDocumentationTypeConstruction): INyxDocumentationParameterDeclaration;
var
  LOwner: TDocumentationParameterDeclaration;
  LLease: INyxDocumentationParameterDeclaration;
  LOriginalType: INyxDocumentationTypeUse;
  LName, LDefault: TNyxText;
begin
  Result := nil;
  AScope.Spend;

  if AValue = nil then
    raise ENyxDocumentation.Create('A declared parameter is required');
  LOwner := TDocumentationParameterDeclaration.Create;
  LLease := LOwner;
  LName := AValue.Name;
  NameBound(LName, False);
  AScope.AdmitText(LName);
  LOwner.ParameterName := LName;
  AScope.Spend;
  LOwner.Mode := AValue.Passing;

  if (Ord(LOwner.Mode) < 0) or (Ord(LOwner.Mode) > Ord(High(TNyxDocumentationParameterMode))) then
    raise ENyxDocumentation.Create('Unknown declared parameter passing mode');
  AScope.Spend;
  LOriginalType := AValue.TypeUse;
  LOwner.ParameterType := CaptureTypeUse(LOriginalType, AScope);

  if (LOwner.ParameterType.Expression <> nil) and
    (LOwner.ParameterType.Expression.Occurrence <> AOccurrence) then
    raise ENyxDocumentation.Create('A parameter expression must retain its actual declaration position');
  AScope.Spend;
  LOwner.DefaultPresent := AValue.HasDefault;
  AScope.Spend;
  LDefault := AValue.DefaultPascal;
  TextBound(LDefault, 1024);
  AScope.AdmitText(LDefault);
  LOwner.DefaultText := LDefault;

  if LOwner.DefaultPresent then
    LLease := LLease.WithDefault(LOwner.DefaultText)
  else if LOwner.DefaultText <> '' then
    raise ENyxDocumentation.Create('An absent parameter default cannot retain hidden text');
  AScope.Spend;

  if (AValue.Name <> LLease.Name) or (AValue.Passing <> LLease.Passing) or
    (AValue.TypeUse <> LOriginalType) or (AValue.HasDefault <> LLease.HasDefault) or
    (AValue.DefaultPascal <> LLease.DefaultPascal) then
    raise ENyxDocumentation.Create('Parameter changed before publication');
  CheckCancellation(AScope);
  RichParameterBytes(LLease);
  Result := LLease;
end;

function NewNyxDocumentationParameterDeclaration(const AName: TNyxText;
  const AType: INyxDocumentationTypeUse;
  AMode: TNyxDocumentationParameterMode): INyxDocumentationParameterDeclaration;
var
  LOwner: TDocumentationParameterDeclaration;
  LLease: INyxDocumentationParameterDeclaration;
  LScope: INyxDocumentationTypeConstruction;
begin
  Result := nil;
  NameBound(AName, False);

  if (Ord(AMode) < 0) or (Ord(AMode) > Ord(High(TNyxDocumentationParameterMode))) then
    raise ENyxDocumentation.Create('Unknown declared parameter passing mode');
  LScope := NewNyxDocumentationTypeConstruction(
    TNyxDocumentationTypeExpressionLimits.Defaults.Bytes(NyxDocumentationMaximumBytes));
  try
    LOwner := TDocumentationParameterDeclaration.Create;
    LLease := LOwner;
    LScope.AdmitText(AName);
    LOwner.ParameterName := AName;
    LOwner.Mode := AMode;
    LOwner.ParameterType := CaptureTypeUse(AType, LScope);
  finally
    LScope.Retire;
  end;
  RichParameterBytes(LLease);
  Result := LLease;
end;

function TDocumentationRichSignature.ParameterCount: Integer;
begin
  Result := Length(Parameters);
end;

function TDocumentationRichSignature.ParameterAt(AIndex: Integer): INyxDocumentationParameterDeclaration;
begin

  if (AIndex < 0) or (AIndex >= Length(Parameters)) then
    raise ENyxDocumentation.Create('Rich signature parameter ordinal out of range');
  Result := Parameters[AIndex];
end;

function TDocumentationRichSignature.ResultUse: INyxDocumentationTypeUse;
begin
  Result := ReturnUse;
end;

function NewNyxDocumentationRichSignature(const AParameters: TNyxDocumentationParameterDeclarations;
  const AResult: INyxDocumentationTypeUse;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationRichSignature;
var
  LOwner: TDocumentationRichSignature;
  LLease: INyxDocumentationRichSignature;
  LAt, LOther, LBytes: Integer;
  LParameter: INyxDocumentationParameterDeclaration;
begin
  Result := nil;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  AConstruction.Spend;

  if Length(AParameters) > NyxDocumentationMaximumParameters then
    raise ENyxDocumentation.Create('Rich signature exceeds its parameter allowance');
  LOwner := TDocumentationRichSignature.Create;
  LLease := LOwner;
  AConstruction.Spend(Length(AParameters) + 1);
  SetLength(LOwner.Parameters, Length(AParameters));
  LBytes := 0;
  for LAt := 0 to High(AParameters) do
  begin
    LParameter := CaptureParameter(AParameters[LAt], ndtoParameter, AConstruction);
    Inc(LBytes, RichParameterBytes(LParameter));
    AdmitBytes(LBytes);
    LOwner.Parameters[LAt] := LParameter;
    for LOther := 0 to LAt - 1 do
    begin
      AConstruction.Spend;

      if Fold(LOwner.Parameters[LOther].Name) = Fold(LOwner.Parameters[LAt].Name) then
        raise ENyxDocumentation.Create('A rich signature repeats a parameter name');
    end;
  end;

  if AResult <> nil then
  begin
    LOwner.ReturnUse := CaptureTypeUse(AResult, AConstruction);
    Inc(LBytes, TypeUseBytes(LOwner.ReturnUse));
    AdmitBytes(LBytes);

    if (LOwner.ReturnUse.Expression <> nil) and (LOwner.ReturnUse.Expression.Occurrence <> ndtoResult) then
      raise ENyxDocumentation.Create('A function result must retain its declared result position');
  end;
  CheckCancellation(AConstruction);
  Result := LLease;
end;

function CaptureRichSignature(const AValue: INyxDocumentationRichSignature;
  const AScope: INyxDocumentationTypeConstruction): INyxDocumentationRichSignature;
var
  LSources: TNyxDocumentationParameterDeclarations;
  LResult: INyxDocumentationTypeUse;
  LFrozen: INyxDocumentationRichSignature;
  LCount, LAt: Integer;
begin
  Result := nil;
  AScope.Spend;

  if AValue = nil then
    raise ENyxDocumentation.Create('A richer callable declaration needs its signature');
  LCount := AValue.ParameterCount;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumParameters) then
    raise ENyxDocumentation.Create('Captured rich parameter allowance exhausted');
  AScope.Spend(LCount + 1);
  SetLength(LSources, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    AScope.Spend;
    LSources[LAt] := AValue.ParameterAt(LAt);
  end;
  AScope.Spend;
  LResult := AValue.ResultUse;
  LFrozen := NewNyxDocumentationRichSignature(LSources, LResult, AScope);
  for LAt := 0 to LCount - 1 do
  begin
    AScope.Spend;

    if AValue.ParameterAt(LAt) <> LSources[LAt] then
      raise ENyxDocumentation.Create('Rich signature parameter topology changed');
  end;
  AScope.Spend;

  if (AValue.ParameterCount <> LCount) or (AValue.ResultUse <> LResult) then
    raise ENyxDocumentation.Create('Rich signature changed before publication');
  CheckCancellation(AScope);
  Result := LFrozen;
end;

function LegacySignature(const AValue: INyxDocumentationRichSignature;
  const AScope: INyxDocumentationTypeConstruction): INyxDocumentationSignature;
var
  LParameters: array of TNyxDocumentationParameter;
  LAt: Integer;
  LParameter: INyxDocumentationParameterDeclaration;
  LResult: TNyxDocumentationRef;
begin
  Result := nil;
  LResult := Default(TNyxDocumentationRef);
  AScope.Spend;

  if AValue.ResultUse <> nil then
  begin

    if AValue.ResultUse.Expression <> nil then
      Exit;
    LResult := AValue.ResultUse.Reference;
  end;
  { Prove representability before allocating a named vector. Mixed signatures
    retain only their rich shape rather than constructing a partial projection. }
  for LAt := 0 to AValue.ParameterCount - 1 do
  begin
    AScope.Spend;

    if AValue.ParameterAt(LAt).TypeUse.Expression <> nil then
      Exit;
  end;
  AScope.Spend(AValue.ParameterCount + 1);
  SetLength(LParameters, AValue.ParameterCount);
  for LAt := 0 to High(LParameters) do
  begin
    AScope.Spend;
    LParameter := AValue.ParameterAt(LAt);
    AScope.AdmitText(LParameter.Name);
    AScope.AdmitText(LParameter.TypeUse.Reference.Key);
    AScope.AdmitText(LParameter.DefaultPascal);
    LParameters[LAt] := NyxDocumentationParameter(LParameter.Name, LParameter.TypeUse.Reference, LParameter.Passing);

    if LParameter.HasDefault then
      LParameters[LAt] := LParameters[LAt].WithDefault(LParameter.DefaultPascal);
  end;

  if LResult.Defined then
    AScope.AdmitText(LResult.Key);
  Result := NewNyxDocumentationSignature(LParameters, LResult);
end;

function TDocumentationRichMember.DeclaredSignature: INyxDocumentationRichSignature;
begin
  Result := RichSignature;
end;

function TDocumentationRichMember.CopyValue: TDocumentationMember;
var
  LNext: TDocumentationRichMember;
begin
  LNext := TDocumentationRichMember.Create;
  CopyMember(LNext);
  LNext.RichSignature := RichSignature;
  Result := LNext;
end;

procedure TDocumentationRichMember.AssignSignature(const AValue: INyxDocumentationSignature);
begin
  inherited AssignSignature(AValue);
  RichSignature := nil;
end;

function NewNyxDocumentationRichMember(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationMemberKind; const ASummary: TNyxText;
  const ASignature: INyxDocumentationRichSignature;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationMember;
var
  LOwner: TDocumentationRichMember;
  LLease: INyxDocumentationMember;
  LFrozen: INyxDocumentationRichSignature;
begin
  Result := nil;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  CheckMemberIdentity(AIdentity, AKind, ASummary);

  if not (AKind in [ndmConstructor, ndmFactory, ndmMethod, ndmFluentMethod, ndmRoutine]) then
    raise ENyxDocumentation.Create('This member cannot own a richer callable signature');
  AConstruction.AdmitText(AIdentity.Key);
  AConstruction.AdmitText(ASummary);
  LFrozen := CaptureRichSignature(ASignature, AConstruction);
  LOwner := TDocumentationRichMember.Create;
  LLease := LOwner;
  LOwner.FIdentity := AIdentity;
  LOwner.FKind := AKind;
  LOwner.FSummary := ASummary;
  LOwner.RichSignature := LFrozen;
  LOwner.FSignature := LegacySignature(LFrozen, AConstruction);
  MemberBytes(LLease);
  CheckCancellation(AConstruction);
  Result := LLease;
end;

function TDocumentationRichProperty.ParameterCount: Integer;
begin
  Result := Length(Parameters);
end;

function TDocumentationRichProperty.ParameterAt(AIndex: Integer): INyxDocumentationParameterDeclaration;
begin

  if (AIndex < 0) or (AIndex >= Length(Parameters)) then
    raise ENyxDocumentation.Create('Rich property parameter ordinal out of range');
  Result := Parameters[AIndex];
end;

function TDocumentationRichProperty.ValueUse: INyxDocumentationTypeUse;
begin
  Result := PropertyValue;
end;

function TDocumentationRichProperty.DeclaredAccess: TNyxDocumentationPropertyAccesses;
begin
  Result := Access;
end;

function TDocumentationRichProperty.DefaultProperty: Boolean;
begin
  Result := IsDefault;
end;

function TDocumentationRichProperty.CopyValue: TDocumentationMember;
var
  LNext: TDocumentationRichProperty;
  LAt: Integer;
begin
  LNext := TDocumentationRichProperty.Create;
  CopyMember(LNext);
  LNext.PropertyValue := PropertyValue;
  LNext.Access := Access;
  LNext.IsDefault := IsDefault;
  SetLength(LNext.Parameters, Length(Parameters));
  for LAt := 0 to High(Parameters) do
    LNext.Parameters[LAt] := Parameters[LAt];
  Result := LNext;
end;

procedure TDocumentationRichProperty.AssignValueType(const AValue: TNyxDocumentationRef);
begin
  inherited AssignValueType(AValue);
  PropertyValue := NewNyxDocumentationNamedTypeUse(AValue);
end;

function NewNyxDocumentationRichProperty(const AIdentity: TNyxDocumentationRef;
  const ASummary: TNyxText; const AParameters: TNyxDocumentationParameterDeclarations;
  const AValue: INyxDocumentationTypeUse; AAccess: TNyxDocumentationPropertyAccesses;
  ADefault: Boolean; const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationMember;
var
  LOwner: TDocumentationRichProperty;
  LLease: INyxDocumentationMember;
  LAt, LOther, LBytes: Integer;
  LParameter: INyxDocumentationParameterDeclaration;
begin
  Result := nil;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  CheckMemberIdentity(AIdentity, ndmProperty, ASummary);
  AConstruction.Spend;

  if Length(AParameters) > NyxDocumentationMaximumParameters then
    raise ENyxDocumentation.Create('Rich property index allowance exhausted');
  AConstruction.AdmitText(AIdentity.Key);
  AConstruction.AdmitText(ASummary);
  LOwner := TDocumentationRichProperty.Create;
  LLease := LOwner;
  LOwner.FIdentity := AIdentity;
  LOwner.FKind := ndmProperty;
  LOwner.FSummary := ASummary;
  LOwner.Access := AAccess;
  LOwner.IsDefault := ADefault;
  AConstruction.Spend(Length(AParameters) + 1);
  SetLength(LOwner.Parameters, Length(AParameters));
  LBytes := RefBytes(AIdentity) + NyxUTF8ByteCount(ASummary) + 3;
  for LAt := 0 to High(AParameters) do
  begin
    LParameter := CaptureParameter(AParameters[LAt], ndtoPropertyIndex, AConstruction);
    Inc(LBytes, RichParameterBytes(LParameter));
    AdmitBytes(LBytes);
    LOwner.Parameters[LAt] := LParameter;
    for LOther := 0 to LAt - 1 do
    begin
      AConstruction.Spend;

      if Fold(LOwner.Parameters[LOther].Name) = Fold(LOwner.Parameters[LAt].Name) then
        raise ENyxDocumentation.Create('Rich property repeats an index name');
    end;
  end;

  if AValue <> nil then
  begin
    LOwner.PropertyValue := CaptureTypeUse(AValue, AConstruction);

    if (LOwner.PropertyValue.Expression <> nil) and
      (LOwner.PropertyValue.Expression.Occurrence <> ndtoPropertyValue) then
      raise ENyxDocumentation.Create('A property value must retain its declared value position');
    LOwner.FValueType := LOwner.PropertyValue.Reference;
  end;
  MemberBytes(LLease);
  CheckCancellation(AConstruction);
  Result := LLease;
end;

function TypeUseBytes(const AValue: INyxDocumentationTypeUse): Integer;
var
  LAt: Integer;
begin
  Result := 0;

  if AValue = nil then
    Exit;
  Result := RefBytes(AValue.Reference);

  if AValue.Expression <> nil then
  begin
    Inc(Result, NyxUTF8ByteCount(AValue.Expression.Text) + 1);
    for LAt := 0 to AValue.BindingCount - 1 do
    begin
      Inc(Result, RefBytes(AValue.BindingAt(LAt)));
      AdmitBytes(Result);
    end;
  end;
  AdmitBytes(Result);
end;

function RichParameterBytes(const AValue: INyxDocumentationParameterDeclaration): Integer;
begin
  Result := NyxUTF8ByteCount(AValue.Name) + TypeUseBytes(AValue.TypeUse) + NyxUTF8ByteCount(AValue.DefaultPascal);
  AdmitBytes(Result);
end;

function RichMemberBytes(const AValue: INyxDocumentationMember): Integer;
var
  LMember: INyxDocumentationMemberSignature;
  LProperty: INyxDocumentationRichProperty;
  LSignature: INyxDocumentationRichSignature;
  LAt: Integer;
begin
  Result := 0;

  if Supports(AValue, INyxDocumentationMemberSignature, LMember) then
  begin
    LSignature := LMember.DeclaredSignature;

    if LSignature <> nil then
    begin
      Result := TypeUseBytes(LSignature.ResultUse);
      for LAt := 0 to LSignature.ParameterCount - 1 do
      begin
        Inc(Result, RichParameterBytes(LSignature.ParameterAt(LAt)));
        AdmitBytes(Result);
      end;
    end;
  end;

  if Supports(AValue, INyxDocumentationRichProperty, LProperty) then
  begin
    Inc(Result, TypeUseBytes(LProperty.ValueUse) + 3);
    for LAt := 0 to LProperty.ParameterCount - 1 do
    begin
      Inc(Result, RichParameterBytes(LProperty.ParameterAt(LAt)));
      AdmitBytes(Result);
    end;
  end;
end;

function CaptureDocumentationMember(const AValue: INyxDocumentationMember;
  const ACancellation: INyxCancellationScope): INyxDocumentationMember;
var
  LSource, LFrozen: INyxDocumentationMember;
  LAt, LCount: Integer;
  LType, LCurrentType: TNyxDocumentationRef;
  LSignature: INyxDocumentationSignature;
  LProperty: INyxDocumentationProperty;
  LPropertyCount: Integer;
  LParameters: array of TNyxDocumentationParameter;
  LAccess: TNyxDocumentationPropertyAccesses;
  LDefault: Boolean;
  LParameter, LOriginal: TNyxDocumentationParameter;
  LScope: INyxDocumentationTypeConstruction;
  LRich: INyxDocumentationMemberSignature;
  LRichSignature: INyxDocumentationRichSignature;
  LRichProperty: INyxDocumentationRichProperty;
  LRichParameters: TNyxDocumentationParameterDeclarations;
  LRichValue: INyxDocumentationTypeUse;
  LHandled: Boolean;
  LOriginalSignature: INyxDocumentationSignature;
  LDeclaration, LFinalDeclaration: INyxDocumentationMemberDeclaration;
  LIntent, LCapturedIntent, LFinalIntent: INyxDocumentationMemberIntent;
  LHasDeclaration, LFinalHasDeclaration, LLegacyCaptured: Boolean;
  LMemberOwner: TDocumentationMember;
  LCurrentIdentity: TNyxDocumentationRef;
  LCurrentKind: TNyxDocumentationMemberKind;
  LCurrentSummary: TNyxText;
  LCurrentSignature: INyxDocumentationSignature;
  LCurrentExamples, LCurrentRelated: Integer;
  LCurrentEvidence: TNyxDocumentationEvidence;
begin
  LSource := AValue;
  Result := nil;
  CheckCancellation(ACancellation);

  if LSource = nil then
    raise ENyxDocumentation.Create('A documentation member is required');

  LHandled := False;
  Supports(ACancellation, INyxDocumentationTypeConstruction, LScope);
  LMemberOwner := CapturedMemberOwner(LSource);
  LLegacyCaptured := (LMemberOwner <> nil) and (LMemberOwner.FIntent = nil);
  LHasDeclaration := Supports(LSource, INyxDocumentationMemberDeclaration, LDeclaration);

  if LHasDeclaration and not LLegacyCaptured then
  begin
    LScope.Spend;
    CheckCancellation(LScope);
    LIntent := LDeclaration.DeclarationIntent;
    CheckCancellation(LScope);
    LCapturedIntent := CaptureMemberIntent(LIntent, LScope);
  end;
  Supports(LSource, INyxDocumentationMemberSignature, LRich);

  if LRich <> nil then
  begin
    LScope.Spend;
    LRichSignature := LRich.DeclaredSignature;
  end;

  if (LRichSignature <> nil) and Supports(LSource, INyxDocumentationProperty, LProperty) then
    raise ENyxDocumentation.Create('A callable cannot also advertise simple property syntax');

  if Supports(LSource, INyxDocumentationRichProperty, LRichProperty) then
  begin
    LScope.Spend;

    if (LRichSignature <> nil) or Supports(LSource, INyxDocumentationProperty, LProperty) or
      (LSource.Kind <> ndmProperty) then
      raise ENyxDocumentation.Create('A property cannot publish conflicting signature/index capabilities');
    LPropertyCount := LRichProperty.ParameterCount;

    if (LPropertyCount < 0) or (LPropertyCount > NyxDocumentationMaximumParameters) then
      raise ENyxDocumentation.Create('Captured rich property index allowance exhausted');
    LScope.Spend(LPropertyCount + 1);
    SetLength(LRichParameters, LPropertyCount);
    for LAt := 0 to LPropertyCount - 1 do
    begin
      LScope.Spend;
      LRichParameters[LAt] := LRichProperty.ParameterAt(LAt);
    end;
    LScope.Spend;
    LRichValue := LRichProperty.ValueUse;
    LAccess := LRichProperty.DeclaredAccess;
    LDefault := LRichProperty.DefaultProperty;
    LFrozen := NewNyxDocumentationRichProperty(LSource.Identity, LSource.Summary,
      LRichParameters, LRichValue, LAccess, LDefault, LScope);
    LHandled := True;
  end
  else if LRichSignature <> nil then
  begin
    LFrozen := NewNyxDocumentationRichMember(LSource.Identity, LSource.Kind,
      LSource.Summary, LRichSignature, LScope);
    LHandled := True;
  end
  else if Supports(LSource, INyxDocumentationProperty, LProperty) then
  begin

    if LSource.Kind <> ndmProperty then
      raise ENyxDocumentation.Create('Property syntax requires a property member');
    LPropertyCount := LProperty.ParameterCount;

    if (LPropertyCount < 0) or (LPropertyCount > NyxDocumentationMaximumParameters) then
      raise ENyxDocumentation.Create('Captured property exceeds its index parameter allowance');
    LAccess := LProperty.DeclaredAccess;
    LDefault := LProperty.DefaultProperty;
    SetLength(LParameters, LPropertyCount);
    for LAt := 0 to LPropertyCount - 1 do
    begin
      CheckCancellation(ACancellation);
      LParameters[LAt] := LProperty.ParameterAt(LAt);
    end;
    LFrozen := NewNyxDocumentationProperty(LSource.Identity, LSource.Summary,
      LParameters, LAccess, LDefault);
  end
  else
    LFrozen := NewNyxDocumentationMember(LSource.Identity, LSource.Kind, LSource.Summary);

  LOriginalSignature := LSource.Signature;
  LSignature := CaptureNyxDocumentationSignature(LOriginalSignature, ACancellation);

  if LSignature <> nil then
  begin

    if LHandled then
    begin

      if (LFrozen.Signature = nil) or (LSignature.ParameterCount <> LFrozen.Signature.ParameterCount) or
        (LSignature.ResultType.Defined <> LFrozen.Signature.ResultType.Defined) or
        (LSignature.ResultType.Defined and not LSignature.ResultType.Same(LFrozen.Signature.ResultType)) then
        raise ENyxDocumentation.Create('Legacy and rich callable signatures conflict');
      for LAt := 0 to LSignature.ParameterCount - 1 do
      begin
        LScope.Spend;
        LParameter := LSignature.ParameterAt(LAt);
        LOriginal := LFrozen.Signature.ParameterAt(LAt);

        if (LParameter.Name <> LOriginal.Name) or not LParameter.TypeReference.Same(LOriginal.TypeReference) or
          (LParameter.Passing <> LOriginal.Passing) or (LParameter.HasDefault <> LOriginal.HasDefault) or
          (LParameter.DefaultPascal <> LOriginal.DefaultPascal) then
          raise ENyxDocumentation.Create('Legacy and rich parameter declarations conflict');
      end;
    end
    else
      LFrozen := LFrozen.WithSignature(LSignature);
  end;
  LType := LSource.ValueType;

  if LType.Defined then
  begin

    if LHandled and ((not LFrozen.ValueType.Defined) or not LFrozen.ValueType.Same(LType)) then
      raise ENyxDocumentation.Create('Legacy and rich property value types conflict');

    if not LHandled then
      LFrozen := LFrozen.OfType(LType);
  end;

  if LCapturedIntent <> nil then
  begin
    CheckMemberIntent(LFrozen, LCapturedIntent);
    LMemberOwner := CapturedMemberOwner(LFrozen);
    LMemberOwner.FIntent := LCapturedIntent;
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
  MemberBytes(LFrozen);

  if LProperty <> nil then
  begin

    if (LProperty.ParameterCount <> LPropertyCount) or
      (LProperty.DeclaredAccess <> LAccess) or (LProperty.DefaultProperty <> LDefault) or
      (LSource.Kind <> ndmProperty) or not LFrozen.Identity.Same(LSource.Identity) then
      raise ENyxDocumentation.Create('Property declaration changed during capture');
    for LAt := 0 to LPropertyCount - 1 do
    begin
      CheckCancellation(ACancellation);
      LParameter := LProperty.ParameterAt(LAt);
      LOriginal := LParameters[LAt];

      if (LParameter.Name <> LOriginal.Name) or
        not LParameter.TypeReference.Same(LOriginal.TypeReference) or
        (LParameter.Passing <> LOriginal.Passing) or
        (LParameter.HasDefault <> LOriginal.HasDefault) or
        (LParameter.DefaultPascal <> LOriginal.DefaultPascal) then
        raise ENyxDocumentation.Create('Property index parameters changed during capture');
    end;
    LCurrentType := LSource.ValueType;

    if (LProperty.ParameterCount <> LPropertyCount) or
      (LProperty.DeclaredAccess <> LAccess) or (LProperty.DefaultProperty <> LDefault) or
      (LSource.Kind <> ndmProperty) or not LFrozen.Identity.Same(LSource.Identity) or
      (LSource.Signature <> nil) or (LCurrentType.Defined <> LType.Defined) or
      (LType.Defined and not LType.Same(LCurrentType)) then
      raise ENyxDocumentation.Create('Property declaration changed before publication');
  end;

  if LRichSignature <> nil then
  begin
    LScope.Spend;

    if (LRich.DeclaredSignature <> LRichSignature) or
      (LSource.Kind <> LFrozen.Kind) or not LSource.Identity.Same(LFrozen.Identity) then
      raise ENyxDocumentation.Create('Rich callable changed before publication');
  end;

  if LRichProperty <> nil then
  begin
    for LAt := 0 to LPropertyCount - 1 do
    begin
      LScope.Spend;

      if LRichProperty.ParameterAt(LAt) <> LRichParameters[LAt] then
        raise ENyxDocumentation.Create('Rich property parameter topology changed');
    end;
    LScope.Spend;

    if (LRichProperty.ParameterCount <> LPropertyCount) or (LRichProperty.ValueUse <> LRichValue) or
      (LRichProperty.DeclaredAccess <> LAccess) or (LRichProperty.DefaultProperty <> LDefault) or
      (LSource.Signature <> nil) or (LSource.Kind <> ndmProperty) or
      not LSource.Identity.Same(LFrozen.Identity) then
      raise ENyxDocumentation.Create('Rich property changed before publication');
  end;

  if LHandled then
  begin
    LScope.Spend;
    LCurrentType := LSource.ValueType;

    if (LSource.Signature <> LOriginalSignature) or
      (LCurrentType.Defined <> LType.Defined) or
      (LType.Defined and not LType.Same(LCurrentType)) then
      raise ENyxDocumentation.Create('Legacy rich-member envelope changed before publication');
  end;
  LFinalHasDeclaration := Supports(LSource, INyxDocumentationMemberDeclaration, LFinalDeclaration);

  if (LFinalHasDeclaration <> LHasDeclaration) or
    (LHasDeclaration and (LFinalDeclaration <> LDeclaration)) then
    raise ENyxDocumentation.Create('Member declaration capability changed during capture');

  if LHasDeclaration and not LLegacyCaptured then
  begin
    LScope.Spend(10);
    CheckCancellation(LScope);
    LFinalIntent := LDeclaration.DeclarationIntent;
    CheckCancellation(LScope);
    LCurrentIdentity := LSource.Identity;
    CheckCancellation(LScope);
    LCurrentKind := LSource.Kind;
    CheckCancellation(LScope);
    LCurrentSummary := LSource.Summary;
    CheckCancellation(LScope);
    LCurrentSignature := LSource.Signature;
    CheckCancellation(LScope);
    LCurrentType := LSource.ValueType;
    CheckCancellation(LScope);
    LCurrentExamples := LSource.ExampleCount;
    CheckCancellation(LScope);
    LCurrentRelated := LSource.RelatedCount;
    CheckCancellation(LScope);

    if (LFinalIntent <> LIntent) or
      (LCurrentKind <> LFrozen.Kind) or not LCurrentIdentity.Same(LFrozen.Identity) or
      (LCurrentSummary <> LFrozen.Summary) or (LCurrentSignature <> LOriginalSignature) or
      (LCurrentType.Defined <> LType.Defined) or
      (LType.Defined and not LCurrentType.Same(LType)) or
      (LCurrentExamples <> LFrozen.ExampleCount) or
      (LCurrentRelated <> LFrozen.RelatedCount) then
      raise ENyxDocumentation.Create('Member intent or legacy envelope changed before publication');

    if LIntent <> nil then
    begin
      LCurrentEvidence := LSource.Evidence;
      CheckCancellation(LScope);

      if not SameMemberIntent(LIntent, LCapturedIntent, LScope) or
        (LCurrentEvidence <> ndeAuthoredDeclaration) then
        raise ENyxDocumentation.Create('Authored member scalar intent changed before publication');
    end;
  end;
  CheckCancellation(ACancellation);
  Result := LFrozen;
end;

function CaptureNyxDocumentationMember(const AValue: INyxDocumentationMember;
  const ACancellation: INyxCancellationScope): INyxDocumentationMember;
var
  LScope: INyxDocumentationTypeConstruction;
  LOwned: Boolean;
  LFrozen: INyxDocumentationMember;
begin
  Result := nil;
  CheckCancellation(ACancellation);
  LScope := DocumentationConstruction(ACancellation, LOwned);
  try
    try
      LFrozen := CaptureDocumentationMember(AValue, LScope);
    except
      on E: ENyxDocumentationTypeExpressionCancelled do
        raise ENyxDocumentationCancelled.Create(E.Message);
    end;
  finally

    if LOwned then
      LScope.Retire;
  end;
  CheckCancellation(ACancellation);
  Result := LFrozen;
end;

function WithNyxDocumentationMemberIntent(const AMember: INyxDocumentationMember;
  const AIntent: INyxDocumentationMemberIntent;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationMember;
var
  LFrozen, LLease: INyxDocumentationMember;
  LCaptured: INyxDocumentationMemberIntent;
  LOwner, LCopy: TDocumentationMember;
  LSignature: INyxDocumentationSignature;
  LOriginalIntent, LCurrentIntent: INyxDocumentationMemberIntent;
  LDeclaration, LCurrentDeclaration: INyxDocumentationMemberDeclaration;
  LHasDeclaration, LHasCurrent: Boolean;
  LCurrentIdentity, LCurrentType: TNyxDocumentationRef;
  LCurrentKind: TNyxDocumentationMemberKind;
  LCurrentSummary: TNyxText;
  LCurrentSignature: INyxDocumentationSignature;
  LCurrentExamples, LCurrentRelated: Integer;
begin
  Result := nil;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  AConstruction.Spend;

  if AMember = nil then
    raise ENyxDocumentation.Create('An intent attachment requires its actual member');
  AConstruction.Spend;
  CheckCancellation(AConstruction);
  LSignature := AMember.Signature;
  CheckCancellation(AConstruction);
  LHasDeclaration := Supports(AMember, INyxDocumentationMemberDeclaration, LDeclaration);
  CheckCancellation(AConstruction);

  if LHasDeclaration then
  begin
    AConstruction.Spend;
    CheckCancellation(AConstruction);
    LOriginalIntent := LDeclaration.DeclarationIntent;
    CheckCancellation(AConstruction);
  end;
  LFrozen := CaptureDocumentationMember(AMember, AConstruction);
  LCaptured := CaptureMemberIntent(AIntent, AConstruction);
  CheckMemberIntent(LFrozen, LCaptured);
  LOwner := CapturedMemberOwner(LFrozen);

  if LOwner = nil then
    raise ENyxDocumentation.Create('Missing captured member for intent attachment');
  { Virtual copying retains simple/rich/property specialization. Only captured
    immutable vectors are copied here; no alternate fluent method is invoked. }
  AConstruction.Spend(LFrozen.ExampleCount + LFrozen.RelatedCount + 1);
  CheckCancellation(AConstruction);
  LCopy := LOwner.CopyValue;
  LLease := LCopy;
  LCopy.FIntent := LCaptured;
  MemberBytes(LLease);
  AConstruction.Spend(10);
  CheckCancellation(AConstruction);
  LHasCurrent := Supports(AMember, INyxDocumentationMemberDeclaration, LCurrentDeclaration);
  CheckCancellation(AConstruction);

  if LHasCurrent then
    LCurrentIntent := LCurrentDeclaration.DeclarationIntent;
  CheckCancellation(AConstruction);
  { Observe each borrowed envelope field once. The fixed work reservation above
    covers this bounded vector; a cancelling getter revokes the next read. }
  LCurrentIdentity := AMember.Identity;
  CheckCancellation(AConstruction);
  LCurrentKind := AMember.Kind;
  CheckCancellation(AConstruction);
  LCurrentSummary := AMember.Summary;
  CheckCancellation(AConstruction);
  LCurrentSignature := AMember.Signature;
  CheckCancellation(AConstruction);
  LCurrentType := AMember.ValueType;
  CheckCancellation(AConstruction);
  LCurrentExamples := AMember.ExampleCount;
  CheckCancellation(AConstruction);
  LCurrentRelated := AMember.RelatedCount;
  CheckCancellation(AConstruction);

  if (LHasCurrent <> LHasDeclaration) or
    (LHasDeclaration and ((LCurrentDeclaration <> LDeclaration) or (LCurrentIntent <> LOriginalIntent))) or
    not LCurrentIdentity.Same(LFrozen.Identity) or (LCurrentKind <> LFrozen.Kind) or
    (LCurrentSummary <> LFrozen.Summary) or (LCurrentSignature <> LSignature) or
    (LCurrentType.Defined <> LFrozen.ValueType.Defined) or
    (LFrozen.ValueType.Defined and not LCurrentType.Same(LFrozen.ValueType)) or
    (LCurrentExamples <> LFrozen.ExampleCount) or (LCurrentRelated <> LFrozen.RelatedCount) then
    raise ENyxDocumentation.Create('Member envelope changed during intent attachment');

  if (LOriginalIntent = nil) <> (LOwner.FIntent = nil) then
    raise ENyxDocumentation.Create('Member intent presence changed before inner capture');

  if (LOriginalIntent <> nil) and
    not SameMemberIntent(LOriginalIntent, LOwner.FIntent, AConstruction) then
    raise ENyxDocumentation.Create('Original member intent changed during replacement');

  if (AIntent <> nil) and not SameMemberIntent(AIntent, LCaptured, AConstruction) then
    raise ENyxDocumentation.Create('Member intent changed before attachment publication');
  CheckCancellation(AConstruction);
  Result := LLease;
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

  if AKind = ndtExpression then
    raise ENyxDocumentation.Create('A compound declaration requires its explicit definition factory');
  LValue := TDocumentationType.Create;
  LValue.FIdentity := AIdentity;
  LValue.FKind := AKind;
  LValue.FSummary := ASummary;
  Result := LValue;
end;

function NewNyxDocumentationDeclaredType(const AIdentity: TNyxDocumentationRef;
  const ASummary: TNyxText; const ADefinition: INyxDocumentationTypeUse;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationType;
var
  LOwner: TDocumentationDeclaredType;
  LLease: INyxDocumentationType;
  LFrozen: INyxDocumentationTypeUse;
  LExpression: INyxDocumentationTypeExpression;
  LFamily: TNyxPascalTypeKind;
begin
  Result := nil;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  CheckTypeIdentity(AIdentity, ndtExpression, ASummary);
  AConstruction.AdmitText(AIdentity.Key);
  AConstruction.AdmitText(ASummary);
  LFrozen := CaptureTypeUse(ADefinition, AConstruction);
  LExpression := LFrozen.Expression;

  if LFrozen.Reference.Defined or (LExpression = nil) or
    (LExpression.Occurrence <> ndtoDeclaration) then
    raise ENyxDocumentation.Create('A declared compound type requires its exact declaration-position expression');
  AConstruction.Spend;
  LFamily := LExpression.Syntax.Node(LExpression.Syntax.Root).Kind;

  if not (LFamily in [ptxArray, ptxSet, ptxProcedure, ptxFunction, ptxDistinct]) then
    raise ENyxDocumentation.Create('This declared compound family is not represented by this contract');
  LOwner := TDocumentationDeclaredType.Create;
  LLease := LOwner;
  LOwner.FIdentity := AIdentity;
  LOwner.FKind := ndtExpression;
  LOwner.FSummary := ASummary;
  LOwner.FDefinition := LFrozen;
  LOwner.FFamily := LFamily;
  TypeBytes(LLease);
  CheckCancellation(AConstruction);
  Result := LLease;
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
  ATarget.FImplemented := FImplemented;
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

function TDocumentationDeclaredType.CopyValue: TDocumentationType;
var
  LCopy: TDocumentationDeclaredType;
begin
  LCopy := TDocumentationDeclaredType.Create;
  try
    CopyType(LCopy);
    LCopy.FDefinition := FDefinition;
    LCopy.FFamily := FFamily;
  except
    LCopy.Free;
    raise;
  end;
  Result := LCopy;
end;

function TDocumentationDeclaredType.Definition: INyxDocumentationTypeUse;
begin
  Result := FDefinition;
end;

function TDocumentationDeclaredType.Family: TNyxPascalTypeKind;
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
begin
  { The public method continues to capture alternate producers before append. }

  if FKind = ndtIntrinsic then
    raise ENyxDocumentation.Create('An intrinsic declaration cannot invent members');

  if FKind = ndtExpression then
    raise ENyxDocumentation.Create('A compound declaration cannot invent members');
  LFrozen := CaptureNyxDocumentationMember(AMember);
  Result := AppendCapturedMember(LFrozen);
end;

function TDocumentationType.TypeOwner: TDocumentationType;
begin
  Result := Self;
end;

function TDocumentationType.ImplementedInterfaces: INyxDocumentationInterfaceImplementations;
begin
  Result := FImplemented;
end;

function TDocumentationType.AppendCapturedMember(const AMember: INyxDocumentationMember): INyxDocumentationType;
var
  LNext: TDocumentationType;
  LHold: INyxDocumentationType;
  LAt: Integer;
begin
  { Intrinsic families have no declared instance members in this contract;
    helpers and compiler intrinsics require their own truthful future shapes. }

  if FKind = ndtIntrinsic then
    raise ENyxDocumentation.Create('An intrinsic declaration cannot invent members');

  if FKind = ndtExpression then
    raise ENyxDocumentation.Create('A compound declaration cannot invent members');

  if not AMember.Identity.Owner.Same(FIdentity) or
    (MemberCount >= NyxDocumentationMaximumMembers) then
    raise ENyxDocumentation.Create('A documentation member must retain its original bounded owner');
  for LAt := 0 to MemberCount - 1 do
  begin

    if FMembers[LAt].Identity.Same(AMember.Identity) then
      raise ENyxDocumentation.Create('Documentation repeats a member/overload identity');
  end;
  LNext := CopyValue;
  LHold := LNext;
  SetLength(LNext.FMembers, MemberCount + 1);
  LNext.FMembers[High(LNext.FMembers)] := AMember;
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

function SameImplementationVector(const ASource, AFrozen: INyxDocumentationInterfaceImplementations;
  const AObserved: TDocumentationTypeUseVector;
  const AScope: INyxDocumentationTypeConstruction): Boolean;
var
  LAt, LCount: Integer;
  LItem: INyxDocumentationTypeUse;
begin
  Result := False;

  if (ASource = nil) or (AFrozen = nil) then
    Exit((ASource = nil) and (AFrozen = nil));
  InterfaceWork(AScope);
  LCount := ASource.Count;
  CheckCancellation(AScope);

  if (LCount <> AFrozen.Count) or (LCount <> Length(AObserved)) then
    Exit;
  for LAt := 0 to LCount - 1 do
  begin
    InterfaceWork(AScope);
    LItem := ASource.ItemAt(LAt);
    CheckCancellation(AScope);

    if (LItem <> AObserved[LAt]) or
      not SameImplementedObservation(LItem, AFrozen.ItemAt(LAt), AScope) then
      Exit;
  end;
  InterfaceWork(AScope);
  LCount := ASource.Count;
  CheckCancellation(AScope);
  Result := LCount = AFrozen.Count;
end;

procedure CheckImplementedOwner(const AIdentity: TNyxDocumentationRef;
  AKind: TNyxDocumentationTypeKind;
  const AValue: INyxDocumentationInterfaceImplementations;
  const AScope: INyxDocumentationTypeConstruction);
var
  LAt, LRoot: Integer;
  LUse: INyxDocumentationTypeUse;
  LReference: TNyxDocumentationRef;
begin

  if AValue = nil then
    Exit;

  if not (AKind in [ndtClass, ndtCallback]) then
    raise ENyxDocumentation.Create('Only a class declaration can own implemented interfaces');
  for LAt := 0 to AValue.Count - 1 do
  begin
    InterfaceWork(AScope);
    LUse := AValue.ItemAt(LAt);
    LRoot := ImplementedRootSlot(LUse, AScope);
    LReference := LUse.Reference;

    if LRoot >= 0 then
      LReference := LUse.BindingAt(LRoot);

    if LReference.Defined and LReference.Same(AIdentity) then
      raise ENyxDocumentation.Create('A class cannot name itself as an implemented interface');
  end;
end;

function SameImplementationEnvelope(const ASource, AFrozen: INyxDocumentationType;
  const AScope: INyxDocumentationTypeConstruction): Boolean;
var
  LIdentity, LAlias, LBase: TNyxDocumentationRef;
  LKind: TNyxDocumentationTypeKind;
  LSummary: TNyxText;
  LEvidence: TNyxDocumentationEvidence;
  LAt, LBases, LMembers, LExamples, LRelated: Integer;
begin
  Result := False;
  InterfaceWork(AScope);
  LIdentity := ASource.Identity;
  InterfaceWork(AScope);
  LKind := ASource.Kind;
  InterfaceWork(AScope);
  LSummary := ASource.Summary;
  CheckCancellation(AScope);

  if Length(LSummary) > NyxDocumentationMaximumBytes then
    Exit;
  InterfaceWork(AScope, Length(LSummary) + 1);
  InterfaceWork(AScope);
  LEvidence := ASource.Evidence;
  InterfaceWork(AScope);
  LAlias := ASource.AliasTarget;
  InterfaceWork(AScope);
  LBases := ASource.BaseCount;
  InterfaceWork(AScope);
  LMembers := ASource.MemberCount;
  InterfaceWork(AScope);
  LExamples := ASource.ExampleCount;
  InterfaceWork(AScope);
  LRelated := ASource.RelatedCount;
  CheckCancellation(AScope);

  if not LIdentity.Same(AFrozen.Identity) or (LKind <> AFrozen.Kind) or
    (LSummary <> AFrozen.Summary) or (LEvidence <> AFrozen.Evidence) or
    (LAlias.Defined <> AFrozen.AliasTarget.Defined) or
    (LAlias.Defined and not LAlias.Same(AFrozen.AliasTarget)) or
    (LBases <> AFrozen.BaseCount) or (LMembers <> AFrozen.MemberCount) or
    (LExamples <> AFrozen.ExampleCount) or (LRelated <> AFrozen.RelatedCount) then
    Exit;
  for LAt := 0 to LBases - 1 do
  begin
    InterfaceWork(AScope);
    LBase := ASource.BaseAt(LAt);
    CheckCancellation(AScope);

    if not LBase.Same(AFrozen.BaseAt(LAt)) then
      Exit;
  end;
  Result := True;
end;

function CaptureDocumentationType(const AValue: INyxDocumentationType;
  const ACancellation: INyxCancellationScope;
  AObservation: TImplementationObservation = nil): INyxDocumentationType;
var
  LSource, LFrozen: INyxDocumentationType;
  LAt, LCount: Integer;
  LAlias: TNyxDocumentationRef;
  LKind: TNyxDocumentationTypeKind;
  LIntrinsic: INyxDocumentationIntrinsic;
  LTargets: INyxDocumentationAliasTargets;
  LDeclared, LCapturedDeclared, LFinalDeclared: INyxDocumentationDeclaredTypeExpression;
  LDefinition: INyxDocumentationTypeUse;
  LFamily: TNyxPascalTypeKind;
  LScope: INyxDocumentationTypeConstruction;
  LBranches: TNyxDocumentationAliasTargetVector;
  LHasIntrinsic, LHasTargets, LHasDeclared: Boolean;
  LOwner: IDocumentationTypeOwner;
  LBuilder: TDocumentationType;
  LCanonical: INyxDocumentationType;
  LMember: INyxDocumentationMember;
  LInterfaces, LFinalInterfaces: INyxDocumentationDeclaredInterfaces;
  LImplementationSource, LImplementationFrozen, LImplementationFinal: INyxDocumentationInterfaceImplementations;
  LImplementationItems: TDocumentationTypeUseVector;
  LHasInterfaces, LFinalHasInterfaces, LLegacyInterfaces: Boolean;
  LReadIdentity, LReadReference: TNyxDocumentationRef;
  LReadSummary: TNyxText;
  LReadExample: TNyxDocumentationExample;
  LReadIntrinsic: TNyxDocumentationIntrinsicFamily;
  procedure CheckSourceRead;
  begin

    if LHasInterfaces and not LLegacyInterfaces then
      CheckCancellation(ACancellation);
  end;
begin
  LSource := AValue;
  Result := nil;
  CheckCancellation(ACancellation);

  if LSource = nil then
    raise ENyxDocumentation.Create('A documentation type is required');
  LKind := LSource.Kind;
  Supports(ACancellation, INyxDocumentationTypeConstruction, LScope);
  LLegacyInterfaces := False;

  if Supports(LSource, IDocumentationTypeOwner, LOwner) then
  begin
    LBuilder := LOwner.TypeOwner;

    if LBuilder = nil then
      raise ENyxDocumentation.Create('Missing private type owner');
    LCanonical := LBuilder;

    if LCanonical <> LSource then
      raise ENyxDocumentation.Create('A source cannot borrow another captured type owner');
    LLegacyInterfaces := LBuilder.FImplemented = nil;
  end;
  LHasInterfaces := Supports(LSource, INyxDocumentationDeclaredInterfaces, LInterfaces);

  if LHasInterfaces and not LLegacyInterfaces then
  begin
    InterfaceWork(LScope);
    LImplementationSource := LInterfaces.ImplementedInterfaces;
    CheckCancellation(LScope);

    if (LImplementationSource <> nil) and not (LKind in [ndtClass, ndtCallback]) then
      raise ENyxDocumentation.Create('Only a class declaration can own implemented interfaces');
    LImplementationFrozen := CaptureInterfaceImplementations(LImplementationSource, LScope, LImplementationItems);
  end;
  CheckSourceRead;
  LReadIdentity := LSource.Identity;
  CheckSourceRead;
  LReadSummary := LSource.Summary;
  CheckSourceRead;
  CheckImplementedOwner(LReadIdentity, LKind, LImplementationFrozen, LScope);
  LHasIntrinsic := Supports(LSource, INyxDocumentationIntrinsic, LIntrinsic);
  CheckSourceRead;
  LHasTargets := Supports(LSource, INyxDocumentationAliasTargets, LTargets);
  CheckSourceRead;
  LHasDeclared := Supports(LSource, INyxDocumentationDeclaredTypeExpression, LDeclared);
  CheckSourceRead;

  if (LHasIntrinsic <> (LKind = ndtIntrinsic)) or (LHasTargets and (LKind <> ndtAlias)) or
    (LHasDeclared <> (LKind = ndtExpression)) then
    raise ENyxDocumentation.Create('Documentation shape capability contradicts its declared kind');

  if LHasDeclared then
  begin

    if not Supports(ACancellation, INyxDocumentationTypeConstruction, LScope) then
      raise ENyxDocumentation.Create('A declared definition needs the shared capture construction');
    LScope.Spend;
    LDefinition := LDeclared.Definition;
    CheckSourceRead;
    LScope.Spend;
    LFamily := LDeclared.Family;
    CheckSourceRead;
    LFrozen := NewNyxDocumentationDeclaredType(LReadIdentity, LReadSummary, LDefinition, LScope);
    Supports(LFrozen, INyxDocumentationDeclaredTypeExpression, LCapturedDeclared);

    if LFamily <> LCapturedDeclared.Family then
      raise ENyxDocumentation.Create('Declared family disagrees with its exact type expression');
  end
  else if LHasIntrinsic then
  begin
    LReadIntrinsic := LIntrinsic.Family;
    CheckSourceRead;
    LFrozen := NewNyxDocumentationIntrinsic(LReadIdentity, LReadIntrinsic, LReadSummary);
  end
  else if LHasTargets then
  begin
    LCount := LTargets.TargetCount;
    CheckSourceRead;

    if (LCount < 1) or (LCount > 2) then
      raise ENyxDocumentation.Create('Captured alias exceeds its one-or-two branch allowance');
    SetLength(LBranches, LCount);
    for LAt := 0 to LCount - 1 do
    begin
      CheckCancellation(ACancellation);
      LBranches[LAt] := LTargets.TargetAt(LAt);
      CheckSourceRead;
    end;
    LFrozen := NewNyxDocumentationTargetAlias(LReadIdentity, LReadSummary, LBranches);
  end
  else
    LFrozen := NewNyxDocumentationType(LReadIdentity, LKind, LReadSummary);

  if LImplementationFrozen <> nil then
  begin
    Supports(LFrozen, IDocumentationTypeOwner, LOwner);
    LBuilder := LOwner.TypeOwner;
    LBuilder.FImplemented := LImplementationFrozen;
  end;
  LAlias := LSource.AliasTarget;
  CheckSourceRead;

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
  CheckSourceRead;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumLinks) then
    raise ENyxDocumentation.Create('Captured documentation exceeds its base allowance');

  if LHasDeclared and (LCount <> 0) then
    raise ENyxDocumentation.Create('A compound definition cannot invent declared class ancestry');
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LReadReference := LSource.BaseAt(LAt);
    CheckSourceRead;
    LFrozen := LFrozen.WithBase(LReadReference);
  end;
  LCount := LSource.MemberCount;
  CheckSourceRead;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumMembers) then
    raise ENyxDocumentation.Create('Captured documentation exceeds its member allowance');

  if LHasDeclared and (LCount <> 0) then
    raise ENyxDocumentation.Create('A compound definition cannot invent executable members');
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LMember := LSource.MemberAt(LAt);
    CheckSourceRead;
    LMember := CaptureNyxDocumentationMember(LMember, ACancellation);

    if not Supports(LFrozen, IDocumentationTypeOwner, LOwner) then
      raise ENyxDocumentation.Create('Missing private captured type owner');
    LBuilder := LOwner.TypeOwner;

    if LBuilder = nil then
      raise ENyxDocumentation.Create('Missing private captured type');
    LCanonical := LBuilder;

    if LCanonical <> LFrozen then
      raise ENyxDocumentation.Create('A captured type cannot borrow another owner');
    LFrozen := LBuilder.AppendCapturedMember(LMember);
  end;
  LCount := LSource.ExampleCount;
  CheckSourceRead;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumExamples) then
    raise ENyxDocumentation.Create('Captured documentation exceeds its example allowance');
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LReadExample := LSource.ExampleAt(LAt);
    CheckSourceRead;
    LFrozen := LFrozen.WithExample(LReadExample);
  end;
  LCount := LSource.RelatedCount;
  CheckSourceRead;

  if (LCount < 0) or (LCount > NyxDocumentationMaximumLinks) then
    raise ENyxDocumentation.Create('Captured documentation exceeds its related-link allowance');
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancellation(ACancellation);
    LReadReference := LSource.RelatedAt(LAt);
    CheckSourceRead;
    LFrozen := LFrozen.WithRelated(LReadReference);
  end;
  TypeBytes(LFrozen);
  LFinalHasInterfaces := Supports(LSource, INyxDocumentationDeclaredInterfaces, LFinalInterfaces);
  CheckCancellation(ACancellation);

  if (LFinalHasInterfaces <> LHasInterfaces) or
    (LHasInterfaces and (LInterfaces <> LFinalInterfaces)) then
    raise ENyxDocumentation.Create('Implemented-interface capability changed during capture');

  if LHasInterfaces and not LLegacyInterfaces then
  begin
    InterfaceWork(LScope);
    LImplementationFinal := LInterfaces.ImplementedInterfaces;
    CheckCancellation(LScope);

    if (LImplementationFinal <> LImplementationSource) or
      not SameImplementationVector(LImplementationFinal, LImplementationFrozen, LImplementationItems, LScope) or
      not SameImplementationEnvelope(LSource, LFrozen, LScope) then
      raise ENyxDocumentation.Create('Implemented-interface declaration changed before publication');
  end;

  if LHasDeclared then
  begin
    LScope.Spend;

    if not Supports(LSource, INyxDocumentationDeclaredTypeExpression, LFinalDeclared) or
      (LFinalDeclared <> LDeclared) or (LDeclared.Definition <> LDefinition) or
      (LDeclared.Family <> LFamily) or (LSource.Kind <> ndtExpression) or
      not LSource.Identity.Same(LFrozen.Identity) or (LSource.Summary <> LFrozen.Summary) or
      LSource.AliasTarget.Defined or (LSource.BaseCount <> 0) or (LSource.MemberCount <> 0) or
      (LSource.ExampleCount <> LFrozen.ExampleCount) or (LSource.RelatedCount <> LFrozen.RelatedCount) then
      raise ENyxDocumentation.Create('Declared type envelope changed before publication');
  end;
  CheckCancellation(ACancellation);

  if AObservation <> nil then
  begin
    AObservation.Capability := LInterfaces;
    AObservation.Source := LImplementationSource;
    AObservation.Frozen := LImplementationFrozen;
    AObservation.Items := LImplementationItems;
  end;
  Result := LFrozen;
end;

function CaptureNyxDocumentationType(const AValue: INyxDocumentationType;
  const ACancellation: INyxCancellationScope): INyxDocumentationType;
var
  LScope: INyxDocumentationTypeConstruction;
  LOwned: Boolean;
  LFrozen: INyxDocumentationType;
begin
  Result := nil;
  CheckCancellation(ACancellation);
  LScope := DocumentationConstruction(ACancellation, LOwned);
  try
    try
      LFrozen := CaptureDocumentationType(AValue, LScope);
    except
      on E: ENyxDocumentationTypeExpressionCancelled do
        raise ENyxDocumentationCancelled.Create(E.Message);
    end;
  finally

    if LOwned then
      LScope.Retire;
  end;
  CheckCancellation(ACancellation);
  Result := LFrozen;
end;

function WithNyxDocumentationImplementedInterfaces(const AType: INyxDocumentationType;
  const AInterfaces: INyxDocumentationInterfaceImplementations;
  const AConstruction: INyxDocumentationTypeConstruction): INyxDocumentationType;
var
  LFrozen, LLease, LCanonical: INyxDocumentationType;
  LOwner: IDocumentationTypeOwner;
  LBuilder, LCopy: TDocumentationType;
  LInitialCapability, LFinalCapability: INyxDocumentationDeclaredInterfaces;
  LInitial, LFinal, LCaptured: INyxDocumentationInterfaceImplementations;
  LObserved: TDocumentationTypeUseVector;
  LObservation: TImplementationObservation;
  LHasInitial, LHasFinal: Boolean;
begin
  Result := nil;
  CheckNyxDocumentationTypeConstruction(AConstruction);
  InterfaceWork(AConstruction);

  if AType = nil then
    raise ENyxDocumentation.Create('Implemented interfaces require their actual class declaration');
  LObservation := TImplementationObservation.Create;
  try
    LHasInitial := Supports(AType, INyxDocumentationDeclaredInterfaces, LInitialCapability);
    CheckCancellation(AConstruction);

    if LHasInitial then
    begin
      InterfaceWork(AConstruction);
      LInitial := LInitialCapability.ImplementedInterfaces;
      CheckCancellation(AConstruction);
    end;
    LFrozen := CaptureDocumentationType(AType, AConstruction, LObservation);
    LCaptured := CaptureInterfaceImplementations(AInterfaces, AConstruction, LObserved);

    CheckImplementedOwner(LFrozen.Identity, LFrozen.Kind, LCaptured, AConstruction);

    if not Supports(LFrozen, IDocumentationTypeOwner, LOwner) then
      raise ENyxDocumentation.Create('Missing captured class declaration');
    LBuilder := LOwner.TypeOwner;

    if LBuilder = nil then
      raise ENyxDocumentation.Create('Missing captured class owner');
    LCanonical := LBuilder;

    if LCanonical <> LFrozen then
      raise ENyxDocumentation.Create('A class attachment cannot borrow another owner');
    InterfaceWork(AConstruction, LFrozen.BaseCount + LFrozen.MemberCount +
      LFrozen.ExampleCount + LFrozen.RelatedCount + 1);
    LCopy := LBuilder.CopyValue;
    LLease := LCopy;
    LCopy.FImplemented := LCaptured;
    TypeBytes(LLease);
    InterfaceWork(AConstruction);
    LHasFinal := Supports(AType, INyxDocumentationDeclaredInterfaces, LFinalCapability);
    CheckCancellation(AConstruction);

    if LHasFinal then
    begin
      InterfaceWork(AConstruction);
      LFinal := LFinalCapability.ImplementedInterfaces;
      CheckCancellation(AConstruction);
    end;

    if (LHasFinal <> LHasInitial) or
      (LHasInitial and ((LFinalCapability <> LInitialCapability) or
      (LInitialCapability <> LObservation.Capability) or (LFinal <> LInitial))) or
      (LInitial <> LObservation.Source) or
      not SameImplementationVector(LFinal, LObservation.Frozen, LObservation.Items, AConstruction) or
      not SameImplementationEnvelope(AType, LFrozen, AConstruction) or
      not SameImplementationVector(AInterfaces, LCaptured, LObserved, AConstruction) then
      raise ENyxDocumentation.Create('Class/interface attachment changed before publication');
  finally
    LObservation.Free;
  end;
  LObserved := nil;
  CheckCancellation(AConstruction);
  Result := LLease;
end;

end.
