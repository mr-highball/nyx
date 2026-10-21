{ Declaration-owned registration metadata. MIT license, see LICENSE. }
unit nyx.documentation.registration;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.scheduler, nyx.documentation,
  nyx.documentation.typeexpressions;

type
  { Authored availability intent, never an observed adapter capability. An
    empty set means unspecified; captures preserve that absence explicitly. }
  TNyxDocumentationPlatform = (ndplatformNative, ndplatformBrowser);
  { A declaration can intentionally target either or both supported projections. }
  TNyxDocumentationPlatforms = set of TNyxDocumentationPlatform;
  { Declaration interfaces live in managed dynamic vectors. Callers initialize
    these explicitly; no static interface array or copied interface record is
    needed at the portable registration boundary. }
  TNyxDocumentationDeclarations = array of INyxDocumentationType;
  { Query-local aggregate admission. Each framed metadata field is charged
    before appending it to a capture. The supplier is borrowed synchronously,
    never retained in a bundle or called after publication. }
  INyxDocumentationAdmission = interface(IInterface)
    ['{D13EF28B-6AF0-4F8B-87D2-8F434B20D903}']
    procedure AdmitField(ABytes: Integer);
  end;
  { Immutable metadata attached to a real creator/callback/catalog registration.
    Origin owns every declaration; Primary names its public type or unit member.
    No implementation class, factory, instance, producer or executable is kept.
    ContentAt is exact length-framed inert equality data, not a wire format or
    hash. It preserves authored prose/signatures while normalizing identities. }
  INyxDocumentationRegistration = interface(IInterface)
    ['{D13EF28B-6AF0-4F8B-87D2-8F434B20D902}']
    function OriginUnit: TNyxDocumentationRef;
    function Primary: TNyxDocumentationRef;
    function Platforms: TNyxDocumentationPlatforms;
    function DeclarationCount: Integer;
    function DeclarationAt(AIndex: Integer): INyxDocumentationType;
    function DeclarationContentAt(AIndex: Integer): TNyxText;
    function ContentBytes: Integer;
    function ForPrimary(const APrimary: TNyxDocumentationRef): INyxDocumentationRegistration;
  end;
  { Dynamic interface storage is owned by managed registry/snapshot classes;
    copied schema/catalog/callback records never contain these interfaces. }
  TNyxDocumentationRegistrations = array of INyxDocumentationRegistration;

const
  NyxDocumentationRegistrationMaximumDeclarations = 128;
  NyxDocumentationRegistrationMaximumBytes = 4 * 1024 * 1024;

{ Complete bundles admit one unit's declarations. External links remain exact
  references for the complete index to resolve; no dependency is invented here.
  Existing per-declaration allowances apply before the additional bundle bound. }
function NewNyxDocumentationRegistration(const AOrigin, APrimary: TNyxDocumentationRef;
  const ADeclarations: TNyxDocumentationDeclarations;
  APlatforms: TNyxDocumentationPlatforms = [];
  const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationAdmission = nil): INyxDocumentationRegistration;
{ Alternate producers are frozen, including all declarations. Claimed byte and
  content values are never trusted; failure publishes no provisional result. }
function CaptureNyxDocumentationRegistration(const AValue: INyxDocumentationRegistration;
  const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationAdmission = nil): INyxDocumentationRegistration;
{ Shared bounded capture/framing for a declaration with its actual unit owner.
  Alternate suppliers are frozen before framing; output content is published
  only on success. Consumed admission/bytes remain consumed on refusal. }
function CaptureNyxDocumentationDeclaration(const AValue: INyxDocumentationType;
  const AOrigin: TNyxDocumentationRef; out AContent: TNyxText;
  var ABytes: Integer; const ACancellation: INyxCancellationScope = nil;
  const AAdmission: INyxDocumentationAdmission = nil): INyxDocumentationType;

implementation

uses nyx.bytes, nyx.documentation.declarations;

type
  { All executable suppliers die at capture. Declaration/content vectors are
    private, immutable and bounded; changing a primary creates another owner. }
  TDocumentationRegistration = class(TInterfacedObject, INyxDocumentationRegistration)
  private
    FOrigin, FPrimary: TNyxDocumentationRef;
    FPlatforms: TNyxDocumentationPlatforms;
    FDeclarations: TNyxDocumentationDeclarations;
    FContents: array of TNyxText;
    FBytes: Integer;
  public
    function OriginUnit: TNyxDocumentationRef;
    function Primary: TNyxDocumentationRef;
    function Platforms: TNyxDocumentationPlatforms;
    function DeclarationCount: Integer;
    function DeclarationAt(AIndex: Integer): INyxDocumentationType;
    function DeclarationContentAt(AIndex: Integer): TNyxText;
    function ContentBytes: Integer;
    function ForPrimary(const APrimary: TNyxDocumentationRef): INyxDocumentationRegistration;
  end;

procedure CheckCancelled(const AValue: INyxCancellationScope);
begin

  if (AValue <> nil) and AValue.Cancelled then
    raise ENyxDocumentationCancelled.Create('Documentation registration cancelled');
end;

function ReferenceContent(const AValue: TNyxDocumentationRef): TNyxText;
begin
  Result := '';

  if AValue.Defined then
    Result := AValue.Key;
end;

{ Each append checks the remaining bundle bytes before allocating its framed
  result. Fields have already passed the existing individual declaration cap. }
procedure AppendContent(var AContent: TNyxText; var ABytes: Integer;
  const AValue: TNyxText; const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission);
var
  LPrefix: TNyxText;
  LBytes, LValueBytes: Integer;
begin
  CheckCancelled(ACancellation);
  LValueBytes := NyxUTF8ByteCount(AValue);
  LPrefix := IntToStr(LValueBytes) + ':';
  LBytes := Length(LPrefix) + LValueBytes;

  if LBytes > NyxDocumentationRegistrationMaximumBytes - ABytes then
    raise ENyxDocumentation.Create('Documentation registration exceeds aggregate bytes');

  if AAdmission <> nil then
    AAdmission.AdmitField(LBytes);
  Inc(ABytes, LBytes);
  AContent := AContent + LPrefix + AValue;
end;

function DeclarationContent(const AValue: INyxDocumentationType; var ABytes: Integer;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission): TNyxText;
var
  LIndex, LAt: Integer;
  LMember: INyxDocumentationMember;
  LSignature: INyxDocumentationSignature;
  LParameter: TNyxDocumentationParameter;
  LExample: TNyxDocumentationExample;
  LIntrinsic: INyxDocumentationIntrinsic;
  LTargets: INyxDocumentationAliasTargets;
  LProperty: INyxDocumentationProperty;
  LRichMember: INyxDocumentationMemberSignature;
  LMemberValue: INyxDocumentationMemberValue;
  LStorage: INyxDocumentationStorageDeclaration;
  LRichSignature: INyxDocumentationRichSignature;
  LRichProperty: INyxDocumentationRichProperty;
  LDeclared: INyxDocumentationDeclaredTypeExpression;
  LAliasDefinition: INyxDocumentationAliasDefinition;
  LMemberDeclaration: INyxDocumentationMemberDeclaration;
  LMemberIntent: INyxDocumentationMemberIntent;
  LInterfaceDeclaration: INyxDocumentationDeclaredInterfaces;
  LImplementations: INyxDocumentationInterfaceImplementations;
  procedure Add(const AText: TNyxText);
  begin
    AppendContent(Result, ABytes, AText, ACancellation, AAdmission);
  end;
  procedure Number(AValue: Integer);
  begin
    Add(IntToStr(AValue));
  end;
  procedure Generics(const ADeclaration: IInterface);
  var
    LCapability: INyxDocumentationGenericDeclaration;
    LParameters: INyxDocumentationGenericParameters;
    LFormal: INyxDocumentationFormalParameter;
    LParameter: Integer;
  begin

    if not Supports(ADeclaration, INyxDocumentationGenericDeclaration, LCapability) then
      Exit;
    LParameters := LCapability.GenericParameters;

    if LParameters = nil then
      Exit;
    Add('generic-parameters');
    Number(LParameters.Count);
    for LParameter := 0 to LParameters.Count - 1 do
    begin
      LFormal := LParameters.ParameterAt(LParameter);
      Add(LFormal.Owner.Key);
      Number(LFormal.ParameterIndex);
      Add(LFormal.Name);
    end;
  end;
  procedure TypeUse(const AUse: INyxDocumentationTypeUse);
  var
    LUse: Integer;
    LExpression: INyxDocumentationTypeExpression;
    LCapability: INyxDocumentationFormalBindings;
    LFormal: INyxDocumentationFormalParameter;
    LRoots: INyxDocumentationCompilerRootBindings;
    LRoot: INyxDocumentationCompilerRoot;
  begin
    Number(Ord(AUse <> nil));

    if AUse = nil then
      Exit;
    Add(ReferenceContent(AUse.Reference));
    LExpression := AUse.Expression;
    Number(Ord(LExpression <> nil));

    if LExpression <> nil then
    begin
      Add(LExpression.Text);
      Number(Ord(LExpression.Occurrence));
      Number(AUse.BindingCount);
      for LUse := 0 to AUse.BindingCount - 1 do
        Add(ReferenceContent(AUse.BindingAt(LUse)));

      if Supports(AUse, INyxDocumentationFormalBindings, LCapability) then
        for LUse := 0 to AUse.BindingCount - 1 do
        begin
          LFormal := LCapability.FormalAt(LUse);

          if LFormal <> nil then
          begin
            Add('formal-binding');
            Number(LUse);
            Add(LFormal.Owner.Key);
            Number(LFormal.ParameterIndex);
            Add(LFormal.Name);
          end;
        end;

      if Supports(AUse, INyxDocumentationCompilerRootBindings, LRoots) then
        for LUse := 0 to AUse.BindingCount - 1 do
        begin
          LRoot := LRoots.RootAt(LUse);

          if LRoot <> nil then
          begin
            Add('compiler-root-binding-v1');
            Number(LUse);
            Add(LRoot.Name.Name);
            Number(Ord(LRoot.Family));
            Add(NyxDocumentationCompilerRootContent(LRoot));
          end;
        end;
    end;
  end;
  procedure RichParameter(const AParameter: INyxDocumentationParameterDeclaration);
  var
    LUntyped: INyxDocumentationUntypedParameter;
  begin
    Add(AParameter.Name);
    TypeUse(AParameter.TypeUse);
    Number(Ord(AParameter.Passing));
    Number(Ord(AParameter.HasDefault));
    Add(AParameter.DefaultPascal);

    if Supports(AParameter, INyxDocumentationUntypedParameter, LUntyped) then
    begin
      Add('untyped-parameter');
      Number(Ord(LUntyped.DeclaredUntyped));
    end;
  end;
begin
  Result := '';
  Add(AValue.Identity.Key);
  Number(Ord(AValue.Kind));
  Add(AValue.Summary);
  Number(Ord(AValue.Evidence));
  Add(ReferenceContent(AValue.AliasTarget));
  Number(AValue.BaseCount);
  for LIndex := 0 to AValue.BaseCount - 1 do
    Add(AValue.BaseAt(LIndex).Key);
  Number(AValue.RelatedCount);
  for LIndex := 0 to AValue.RelatedCount - 1 do
    Add(AValue.RelatedAt(LIndex).Key);
  Number(AValue.ExampleCount);
  for LIndex := 0 to AValue.ExampleCount - 1 do
  begin
    LExample := AValue.ExampleAt(LIndex);
    Add(LExample.Title);
    Add(LExample.Pascal);
  end;
  Number(AValue.MemberCount);
  for LIndex := 0 to AValue.MemberCount - 1 do
  begin
    LMember := AValue.MemberAt(LIndex);
    Add(LMember.Identity.Key);
    Number(Ord(LMember.Kind));
    Add(LMember.Summary);
    Number(Ord(LMember.Evidence));
    Add(ReferenceContent(LMember.ValueType));
    Number(LMember.RelatedCount);
    for LAt := 0 to LMember.RelatedCount - 1 do
      Add(LMember.RelatedAt(LAt).Key);
    Number(LMember.ExampleCount);
    for LAt := 0 to LMember.ExampleCount - 1 do
    begin
      LExample := LMember.ExampleAt(LAt);
      Add(LExample.Title);
      Add(LExample.Pascal);
    end;
    LSignature := LMember.Signature;
    Number(Ord(LSignature <> nil));

    if LSignature <> nil then
    begin
      Number(Ord(LSignature.Evidence));
      Add(ReferenceContent(LSignature.ResultType));
      Number(LSignature.ParameterCount);
      for LAt := 0 to LSignature.ParameterCount - 1 do
      begin
        LParameter := LSignature.ParameterAt(LAt);
        Add(LParameter.Name);
        Add(LParameter.TypeReference.Key);
        Number(Ord(LParameter.Passing));
        Number(Ord(LParameter.HasDefault));
        Add(LParameter.DefaultPascal);
      end;
    end;

    if Supports(LMember, INyxDocumentationProperty, LProperty) then
    begin
      Add('property-shape');
      Number(Ord(ndpaRead in LProperty.DeclaredAccess));
      Number(Ord(ndpaWrite in LProperty.DeclaredAccess));
      Number(Ord(LProperty.DefaultProperty));
      Number(LProperty.ParameterCount);
      for LAt := 0 to LProperty.ParameterCount - 1 do
      begin
        LParameter := LProperty.ParameterAt(LAt);
        Add(LParameter.Name);
        Add(LParameter.TypeReference.Key);
        Number(Ord(LParameter.Passing));
        Number(Ord(LParameter.HasDefault));
        Add(LParameter.DefaultPascal);
      end;
    end;

    if Supports(LMember, INyxDocumentationMemberSignature, LRichMember) then
    begin
      LRichSignature := LRichMember.DeclaredSignature;

      if LRichSignature <> nil then
      begin
        Add('rich-signature');
        TypeUse(LRichSignature.ResultUse);
        Number(LRichSignature.ParameterCount);
        for LAt := 0 to LRichSignature.ParameterCount - 1 do
          RichParameter(LRichSignature.ParameterAt(LAt));
      end;
    end;

    if Supports(LMember, INyxDocumentationRichProperty, LRichProperty) then
    begin
      Add('rich-property');
      TypeUse(LRichProperty.ValueUse);
      Number(Ord(ndpaRead in LRichProperty.DeclaredAccess));
      Number(Ord(ndpaWrite in LRichProperty.DeclaredAccess));
      Number(Ord(LRichProperty.DefaultProperty));
      Number(LRichProperty.ParameterCount);
      for LAt := 0 to LRichProperty.ParameterCount - 1 do
        RichParameter(LRichProperty.ParameterAt(LAt));
    end;

    if Supports(LMember, INyxDocumentationMemberValue, LMemberValue) and
      (LMemberValue.DeclaredValueType <> nil) then
    begin

      if LMember.Kind = ndmField then
        Add('declared-field-type')
      else
        Add('declared-storage-type');
      TypeUse(LMemberValue.DeclaredValueType);
    end;

    if Supports(LMember, INyxDocumentationStorageDeclaration, LStorage) then
    begin
      Add('storage-declaration');
      Number(Ord(LStorage.HasDeclaredType));
      Number(Ord(LStorage.HasInitializer));
      Add(LStorage.InitializerPascal);
    end;

    if Supports(LMember, INyxDocumentationMemberDeclaration, LMemberDeclaration) then
    begin
      LMemberIntent := LMemberDeclaration.DeclarationIntent;

      if LMemberIntent <> nil then
      begin
        Add('member-declaration-intent');
        Number(Ord(LMemberIntent.DeclaredClass));
        Number(Ord(LMemberIntent.DeclaredStatic));
        Number(Ord(LMemberIntent.Visibility));
        Number(Ord(LMemberIntent.CallingConvention));
      end;
    end;
    Generics(LMember);
  end;
  { Shape extensions follow the complete legacy content. Absence leaves the
    earlier declaration framing byte-identical; present facts cannot disappear
    during duplicate comparison or aggregate accounting. }

  if Supports(AValue, INyxDocumentationIntrinsic, LIntrinsic) then
  begin
    Add('intrinsic-family');
    Number(Ord(LIntrinsic.Family));
  end;

  if Supports(AValue, INyxDocumentationAliasTargets, LTargets) then
  begin
    Add('alias-targets');
    Number(LTargets.TargetCount);
    for LIndex := 0 to LTargets.TargetCount - 1 do
    begin
      Number(Ord(LTargets.TargetAt(LIndex).Platform));
      Add(LTargets.TargetAt(LIndex).Target.Key);
    end;
  end;

  if Supports(AValue, INyxDocumentationDeclaredTypeExpression, LDeclared) then
  begin
    Add('declared-type-expression');
    Number(Ord(LDeclared.Family));
    TypeUse(LDeclared.Definition);
  end;

  if Supports(AValue, INyxDocumentationAliasDefinition, LAliasDefinition) then

    if LAliasDefinition.Definition <> nil then
    begin
      Add('direct-alias-definition-v1');
      TypeUse(LAliasDefinition.Definition);
    end;

  if Supports(AValue, INyxDocumentationDeclaredInterfaces, LInterfaceDeclaration) then
  begin
    LImplementations := LInterfaceDeclaration.ImplementedInterfaces;

    if LImplementations <> nil then
    begin
      Add('implemented-interfaces');
      Number(LImplementations.Count);
      for LIndex := 0 to LImplementations.Count - 1 do
        TypeUse(LImplementations.ItemAt(LIndex));
    end;
  end;
  Generics(AValue);
end;

function CaptureNyxDocumentationDeclaration(const AValue: INyxDocumentationType;
  const AOrigin: TNyxDocumentationRef; out AContent: TNyxText;
  var ABytes: Integer; const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission): INyxDocumentationType;
var
  LFrozen: INyxDocumentationType;
  LContent: TNyxText;
begin
  Result := nil;
  AContent := '';
  AOrigin.Validate;

  if not AOrigin.IsUnit or (ABytes < 0) or
    (ABytes > NyxDocumentationRegistrationMaximumBytes) then
    raise ENyxDocumentation.Create('A declaration capture needs its unit and remaining bundle allowance');
  LFrozen := CaptureNyxDocumentationType(AValue, ACancellation);

  if not NyxDocumentationUnit(LFrozen.Identity.UnitName).Same(AOrigin) then
    raise ENyxDocumentation.Create('A declaration capture cannot claim another unit');
  LContent := DeclarationContent(LFrozen, ABytes, ACancellation, AAdmission);
  CheckCancelled(ACancellation);
  AContent := LContent;
  Result := LFrozen;
end;

function TDocumentationRegistration.OriginUnit: TNyxDocumentationRef;
begin
  Result := FOrigin;
end;

function TDocumentationRegistration.Primary: TNyxDocumentationRef;
begin
  Result := FPrimary;
end;

function TDocumentationRegistration.Platforms: TNyxDocumentationPlatforms;
begin
  Result := FPlatforms;
end;

function TDocumentationRegistration.DeclarationCount: Integer;
begin
  Result := Length(FDeclarations);
end;

function TDocumentationRegistration.DeclarationAt(AIndex: Integer): INyxDocumentationType;
begin

  if (AIndex < 0) or (AIndex >= DeclarationCount) then
    raise ENyxDocumentation.Create('Documentation registration declaration index out of range');
  Result := FDeclarations[AIndex];
end;

function TDocumentationRegistration.DeclarationContentAt(AIndex: Integer): TNyxText;
begin
  DeclarationAt(AIndex);
  Result := FContents[AIndex];
end;

function TDocumentationRegistration.ContentBytes: Integer;
begin
  Result := FBytes;
end;

function TDocumentationRegistration.ForPrimary(const APrimary: TNyxDocumentationRef): INyxDocumentationRegistration;
begin
  Result := NewNyxDocumentationRegistration(FOrigin, APrimary, FDeclarations, FPlatforms);
end;

function NewNyxDocumentationRegistration(const AOrigin, APrimary: TNyxDocumentationRef;
  const ADeclarations: TNyxDocumentationDeclarations;
  APlatforms: TNyxDocumentationPlatforms;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission): INyxDocumentationRegistration;
var
  LOwner: TDocumentationRegistration;
  LLease: INyxDocumentationRegistration;
  LAt, LOther, LMember: Integer;
  LFound: Boolean;
  LContent: TNyxText;
begin
  CheckCancelled(ACancellation);
  AOrigin.Validate;
  APrimary.Validate;

  if not AOrigin.IsUnit or not APrimary.Defined or APrimary.IsUnit or
    not NyxDocumentationUnit(APrimary.UnitName).Same(AOrigin) then
    raise ENyxDocumentation.Create('A registration needs its unit and exact public type or member');

  if (Length(ADeclarations) = 0) or
    (Length(ADeclarations) > NyxDocumentationRegistrationMaximumDeclarations) then
    raise ENyxDocumentation.Create('Documentation registration declaration count exceeds allowance');
  LOwner := TDocumentationRegistration.Create;
  LLease := LOwner;
  LOwner.FOrigin := AOrigin;
  LOwner.FPrimary := APrimary;
  LOwner.FPlatforms := APlatforms;
  AppendContent(LContent, LOwner.FBytes, AOrigin.Key, ACancellation, AAdmission);
  AppendContent(LContent, LOwner.FBytes, APrimary.Key, ACancellation, AAdmission);
  SetLength(LOwner.FDeclarations, Length(ADeclarations));
  SetLength(LOwner.FContents, Length(ADeclarations));
  LFound := False;
  for LAt := 0 to High(ADeclarations) do
  begin
    CheckCancelled(ACancellation);
    LOwner.FDeclarations[LAt] := CaptureNyxDocumentationType(ADeclarations[LAt], ACancellation);

    if not NyxDocumentationUnit(LOwner.FDeclarations[LAt].Identity.UnitName).Same(AOrigin) then
      raise ENyxDocumentation.Create('A registration cannot claim another unit declaration');
    for LOther := 0 to LAt - 1 do
    begin

      if LOwner.FDeclarations[LOther].Identity.Same(LOwner.FDeclarations[LAt].Identity) then
        raise ENyxDocumentation.Create('Duplicate declaration in one registration');
    end;
    LOwner.FContents[LAt] := DeclarationContent(LOwner.FDeclarations[LAt], LOwner.FBytes,
      ACancellation, AAdmission);

    if APrimary.Same(LOwner.FDeclarations[LAt].Identity) then
      LFound := True;
    for LMember := 0 to LOwner.FDeclarations[LAt].MemberCount - 1 do
    begin

      if APrimary.Same(LOwner.FDeclarations[LAt].MemberAt(LMember).Identity) then
      begin

        if not APrimary.Owner.IsUnit or
          (LOwner.FDeclarations[LAt].MemberAt(LMember).Kind <> ndmFactory) then
          raise ENyxDocumentation.Create('A member registration primary must be an actual unit factory');
        LFound := True;
      end;
    end;
  end;

  if not LFound then
    raise ENyxDocumentation.Create('Registration primary is absent from its declared unit');
  CheckCancelled(ACancellation);
  Result := LLease;
end;

function CaptureNyxDocumentationRegistration(const AValue: INyxDocumentationRegistration;
  const ACancellation: INyxCancellationScope;
  const AAdmission: INyxDocumentationAdmission): INyxDocumentationRegistration;
var
  LInput: INyxDocumentationRegistration;
  LDeclarations: TNyxDocumentationDeclarations;
  LOrigin, LPrimary: TNyxDocumentationRef;
  LPlatforms: TNyxDocumentationPlatforms;
  LAt, LCount: Integer;
begin
  LInput := AValue;
  Result := nil;
  CheckCancelled(ACancellation);

  if LInput = nil then
    Exit;

  if TryCaptureNyxDocumentationContributionBinding(LInput, Result, ACancellation, AAdmission) then
    Exit;
  LCount := LInput.DeclarationCount;

  if (LCount < 1) or (LCount > NyxDocumentationRegistrationMaximumDeclarations) then
    raise ENyxDocumentation.Create('Documentation registration declaration count exceeds allowance');
  LOrigin := LInput.OriginUnit;
  LPrimary := LInput.Primary;
  LPlatforms := LInput.Platforms;
  SetLength(LDeclarations, LCount);
  for LAt := 0 to LCount - 1 do
  begin
    CheckCancelled(ACancellation);
    LDeclarations[LAt] := LInput.DeclarationAt(LAt);
  end;

  if (LCount <> LInput.DeclarationCount) or not LOrigin.Same(LInput.OriginUnit) or
    not LPrimary.Same(LInput.Primary) or (LPlatforms <> LInput.Platforms) then
    raise ENyxDocumentation.Create('Documentation registration changed during capture');
  Result := NewNyxDocumentationRegistration(LOrigin, LPrimary, LDeclarations, LPlatforms,
    ACancellation, AAdmission);
end;

end.
