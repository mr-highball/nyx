{ Public declaration/decorator example. MIT license, see LICENSE. }
unit nyx.test.documentation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.model, nyx.controls, nyx.events, nyx.behavior,
  nyx.scheduler, nyx.catalog, nyx.documentation, nyx.documentation.declarations;

type
  { A specialized extension retains the ordinary badge contract. Documentation
    is an optional capability, never a replacement for its public control type. }
  IDocumentationBadge = interface(INyxBadge)
    ['{44E7F656-823E-4DAA-99D2-613565DCBB8A}']
    function GetGreeting: TNyxText;
    function WithGreeting(const AName: TNyxText): IDocumentationBadge;
    property Greeting: TNyxText read GetGreeting;
  end;
  { Declaration-owned example metadata has a static entry for registration;
    inspection never needs this control's constructor or descriptor node. }
  TDocumentationBadge = class(TNyxBadge, IDocumentationBadge, INyxDocumentationSource)
  public
    constructor Create(const AID: TNyxText); reintroduce;
    destructor Destroy; override;
    function GetGreeting: TNyxText;
    function WithGreeting(const AName: TNyxText): IDocumentationBadge;
    function Documentation: INyxDocumentationType;
    class function Reference: INyxDocumentationType; static;
    class function InterfaceReference: INyxDocumentationType; static;
  end;
  { Callback registration stores this class without constructing or invoking it.
    Its declared signature reuses the real event/execution API type identities. }
  TDocumentationCallback = class(TNyxEventCallback)
  public
    constructor Create;
    procedure Invoke(const AEvent: TNyxEventInfo; const AExecution: INyxExecution); override;
    class function Reference: INyxDocumentationType; static;
  end;

const
  DocumentationFixtureKind = 'documentation-badge-fixture';
  DocumentationFixtureSummary = 'Display a friendly greeting in a reusable badge.';
  DocumentationGreetingDefault = 'Hello';

var
  DocumentationControlsConstructed: Integer;
  DocumentationControlsDestroyed: Integer;
  DocumentationCallbacksConstructed: Integer;
  DocumentationCallbacksInvoked: Integer;
  DocumentationGlobalFactoryCalls: Integer;
  DocumentationGlobalRoutineCalls: Integer;

{ A genuine unit-level factory returns the specialized public interface. Its
  descriptor names this unit directly, without a synthetic containing class. }
function NewDocumentationBadge(const AID: TNyxText): IDocumentationBadge;
{ Ordinary overloaded functions retain independent declared signatures and
  stable overload identities. Documentation inspection invokes neither body. }
function DocumentationGreeting: TNyxText; overload;
function DocumentationGreeting(const AName: TNyxText): TNyxText; overload;
{ The unit's own declaration metadata lives beside those actual globals. No
  registry, generator, reflective enumeration or control construction runs. }
function DocumentationUnitReference: INyxDocumentationType;

{ Registration publishes the same extension-owned prose/defaults to the existing
  catalog/schema/callback authorities. No second help registry or page list is
  populated. Declaration attachments travel with those same registration calls. }
procedure RegisterDocumentationFixture(ACatalog: TNyxCatalog); overload;
{ The source-adoption path receives one already complete declaration package.
  Its exact captured owner lives with the real schema/callback registrations and
  is reused by later catalogs. Different owners and prior legacy registration
  refuse; this operation never silently replaces global registry entries.
  Individual registries retain their own publication authority. }
procedure RegisterDocumentationFixture(ACatalog: TNyxCatalog;
  const APackage: INyxDocumentationPackage); overload;

implementation

uses SysUtils, nyx.schema, nyx.catalog.labels, nyx.callbacks, nyx.contract, nyx.event.payload,
  nyx.documentation.registration, nyx.documentation.typeexpressions,
  nyx.pascal.types;

var
  GRegistered: Boolean;
  { Only explicit source registration installs these immutable leases. Legacy
    bundles keep their original independent capture/accounting behavior. }
  GSourcePackage: INyxDocumentationPackage;
  GSourceDocumentation: INyxDocumentationRegistration;

function TextType: TNyxDocumentationRef;
begin
  Result := NyxDocumentationType('nyx.text', 'TNyxText');
end;

function NewDocumentationBadge(const AID: TNyxText): IDocumentationBadge;
begin
  Inc(DocumentationGlobalFactoryCalls);
  Result := TDocumentationBadge.Create(AID);
end;

function DocumentationGreeting: TNyxText;
begin
  Inc(DocumentationGlobalRoutineCalls);
  Result := DocumentationGreetingDefault;
end;

function DocumentationGreeting(const AName: TNyxText): TNyxText;
begin
  Inc(DocumentationGlobalRoutineCalls);
  Result := TNyxText(DocumentationGreetingDefault) + ' ' + AName;
end;

function DocumentationUnitReference: INyxDocumentationType;
var
  LUnit: TNyxDocumentationRef;
begin
  LUnit := NyxDocumentationUnit('nyx.test.documentation');

  if GSourcePackage <> nil then
    Exit(GSourcePackage.FindDeclaration(LUnit));
  Result := NewNyxDocumentationType(LUnit, ndtUnit,
    'Create friendly badges and format their greetings.')
    .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(LUnit, 'NewDocumentationBadge'),
      ndmFactory, 'Create a badge with its stable component identifier.')
      .WithSignature(NewNyxDocumentationSignature([
        NyxDocumentationParameter('AID', TextType, ndpConst)],
        NyxDocumentationType('nyx.test.documentation', 'IDocumentationBadge'))))
    .WithMember(NewNyxDocumentationMember(
      NyxDocumentationMember(LUnit, 'DocumentationGreeting', 'Default'),
      ndmRoutine, 'Read the default English greeting.')
      .WithSignature(NewNyxDocumentationSignature([], TextType)))
    .WithMember(NewNyxDocumentationMember(
      NyxDocumentationMember(LUnit, 'DocumentationGreeting', 'Named'),
      ndmRoutine, 'Format an English greeting for a name.')
      .WithSignature(NewNyxDocumentationSignature([
        NyxDocumentationParameter('AName', TextType, ndpConst)], TextType)))
    .WithRelated(NyxDocumentationType('nyx.test.documentation', 'IDocumentationBadge'))
    .WithExample(NyxDocumentationExample('Create a named greeting',
      'Badge := NewDocumentationBadge(''welcome'').WithGreeting(DocumentationGreeting(''Ada''));'));
end;

function GreetingMember(const AOwner: TNyxDocumentationRef): INyxDocumentationMember;
begin
  Result := NewNyxDocumentationMember(NyxDocumentationMember(AOwner, 'WithGreeting'),
    ndmFluentMethod, 'Set the greeting and retain this specialized badge.')
    .WithSignature(NewNyxDocumentationSignature([
      NyxDocumentationParameter('AName', TextType, ndpConst)],
      NyxDocumentationType('nyx.test.documentation', 'IDocumentationBadge')));
end;

class function TDocumentationBadge.Reference: INyxDocumentationType;
var
  LID: TNyxDocumentationRef;
begin
  LID := NyxDocumentationType('nyx.test.documentation', 'TDocumentationBadge');

  if GSourcePackage <> nil then
    Exit(GSourcePackage.FindDeclaration(LID));
  Result := NewNyxDocumentationType(LID, ndtClass, DocumentationFixtureSummary)
    .WithBase(NyxDocumentationType('nyx.controls', 'TNyxBadge'))
    .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(LID, 'Create'),
      ndmConstructor, 'Create a badge with its stable component identifier.')
      .WithSignature(NewNyxDocumentationSignature([
        NyxDocumentationParameter('AID', TextType, ndpConst)],
        Default(TNyxDocumentationRef))))
    .WithMember(GreetingMember(LID))
    .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(LID, 'Greeting'),
      ndmProperty, 'Read the stored greeting.').OfType(TextType))
    .WithRelated(NyxDocumentationType('nyx.test.documentation', 'IDocumentationBadge'))
    .WithExample(NyxDocumentationExample('Create a friendly badge',
      'Badge := TDocumentationBadge.Create(''welcome'').WithGreeting(''Ada'');'));
end;

class function TDocumentationBadge.InterfaceReference: INyxDocumentationType;
var
  LID: TNyxDocumentationRef;
begin
  LID := NyxDocumentationType('nyx.test.documentation', 'IDocumentationBadge');

  if GSourcePackage <> nil then
    Exit(GSourcePackage.FindDeclaration(LID));
  Result := NewNyxDocumentationType(LID, ndtInterface, DocumentationFixtureSummary)
    .WithBase(NyxDocumentationType('nyx.controls', 'INyxBadge'))
    .WithMember(GreetingMember(LID));
end;

constructor TDocumentationBadge.Create(const AID: TNyxText);
var
  LNode: TNyxNode;
begin
  Inc(DocumentationControlsConstructed);
  LNode := TNyxNode.Create(DocumentationFixtureKind, AID);
  inherited CreateFromNode(LNode);
  LNode.ReleaseOwnership;
end;

function TDocumentationBadge.GetGreeting: TNyxText;
begin
  Result := Node.Prop('greeting', DocumentationGreetingDefault);
end;

destructor TDocumentationBadge.Destroy;
begin
  Inc(DocumentationControlsDestroyed);
  inherited Destroy;
end;

function TDocumentationBadge.WithGreeting(const AName: TNyxText): IDocumentationBadge;
begin
  Node.SetProp('greeting', AName);
  Result := Self as IDocumentationBadge;
end;

function TDocumentationBadge.Documentation: INyxDocumentationType;
begin
  Result := Reference;
end;

constructor TDocumentationCallback.Create;
begin
  inherited Create;
  Inc(DocumentationCallbacksConstructed);
end;

procedure TDocumentationCallback.Invoke(const AEvent: TNyxEventInfo;
  const AExecution: INyxExecution);
begin
  Inc(DocumentationCallbacksInvoked);
end;

class function TDocumentationCallback.Reference: INyxDocumentationType;
var
  LID: TNyxDocumentationRef;
begin
  LID := NyxDocumentationType('nyx.test.documentation', 'TDocumentationCallback');

  if GSourcePackage <> nil then
    Exit(GSourcePackage.FindDeclaration(LID));
  Result := NewNyxDocumentationType(LID, ndtCallback, 'Observe a friendly greeting event.')
    .WithBase(NyxDocumentationType('nyx.events', 'TNyxEventCallback'))
    .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(LID, 'Invoke'),
      ndmMethod, 'Receive an event and its execution scope.')
      .WithSignature(NewNyxDocumentationSignature([
        NyxDocumentationParameter('AEvent', NyxDocumentationType('nyx.behavior', 'TNyxEventInfo'), ndpConst),
        NyxDocumentationParameter('AExecution', NyxDocumentationType('nyx.scheduler', 'INyxExecution'), ndpConst)],
        Default(TNyxDocumentationRef))));
end;

{ Share the real registry calls without adding a cross-registry transaction.
  All supplied metadata has been prepared before this publication boundary. }
procedure PublishDocumentationFixture(const ADocumentation,
  ACallbackDocumentation: INyxDocumentationRegistration);
var
  LProperty: TNyxPropertyInfo;
begin

  if GRegistered then
    Exit;
  LProperty := Default(TNyxPropertyInfo);
  LProperty.Key := 'greeting';
  LProperty.Title := 'Greeting';
  LProperty.ValueType := npText;
  LProperty.DefaultValue := DocumentationGreetingDefault;
  LProperty.Support := NyxPropertySupport(npmCustom, ncCustom, ncCustom,
    'Stored greeting supplied by this extension.');
  RegisterNyxSchema(NyxCustomKind(DocumentationFixtureKind), [LProperty], [
    NyxNamedEventSchema(NyxEvent('GreetingChanged'), 'Greeting changed',
      'A declared text payload; producer execution remains separate.',
      ncCustom, ncCustom, NyxScalarPayload(NyxTextDomain))], ADocumentation);
  RegisterNyxCallback(NyxHandler('TDocumentationCallback'), TDocumentationCallback,
    ACallbackDocumentation);
  GRegistered := True;
end;

procedure AddDocumentationFixture(ACatalog: TNyxCatalog;
  const ADocumentation: INyxDocumentationRegistration);
begin

  if (ACatalog <> nil) and (ACatalog.IndexOf(DocumentationFixtureKind) < 0) then
  begin
    ACatalog.RegisterKind(NyxCustomKind(DocumentationFixtureKind), 'Friendly badge', 'Custom',
      False, ADocumentation);
    ACatalog.Describe(NyxCustomKind(DocumentationFixtureKind), pgText, [clText],
      'greeting, hello', DocumentationFixtureSummary);
  end;
end;

procedure RegisterDocumentationFixture(ACatalog: TNyxCatalog);
var
  LDocumentation, LCallbackDocumentation: INyxDocumentationRegistration;
  LDeclarations: TNyxDocumentationDeclarations;
  LUnit: TNyxDocumentationRef;
begin

  if GSourcePackage <> nil then
  begin
    RegisterDocumentationFixture(ACatalog, GSourcePackage);
    Exit;
  end;
  { This bundle owns only this actual unit's declarations. Its external core
    links remain complete and must be resolved by the eventual core owners. }
  LUnit := NyxDocumentationUnit('nyx.test.documentation');
  SetLength(LDeclarations, 4);
  LDeclarations[0] := DocumentationUnitReference;
  LDeclarations[1] := TDocumentationBadge.Reference;
  LDeclarations[2] := TDocumentationBadge.InterfaceReference;
  LDeclarations[3] := TDocumentationCallback.Reference;
  LDocumentation := NewNyxDocumentationRegistration(LUnit,
    NyxDocumentationMember(LUnit, 'NewDocumentationBadge'), LDeclarations,
    [ndplatformNative, ndplatformBrowser]);

  LCallbackDocumentation := LDocumentation.ForPrimary(
    NyxDocumentationType('nyx.test.documentation', 'TDocumentationCallback'));
  PublishDocumentationFixture(LDocumentation, LCallbackDocumentation);
  AddDocumentationFixture(ACatalog, LDocumentation);
end;

{ An exact bare declared use keeps its nominal source binding. A similarly
  spelled alias, unresolved expression or procedural type is not this API. }
function FixtureNamedUse(const AUse: INyxDocumentationTypeUse;
  const AExpected: TNyxDocumentationRef): Boolean;
var
  LExpression: INyxDocumentationTypeExpression;
begin
  Result := False;

  if AUse = nil then
    Exit;
  LExpression := AUse.Expression;

  if LExpression = nil then
    Exit(AUse.Reference.Same(AExpected));

  if not LExpression.Syntax.Complete or (LExpression.Syntax.Root < 0) or
    (LExpression.Syntax.Node(LExpression.Syntax.Root).Kind <> ptxName) or
    (LExpression.NamedUseCount <> 1) or
    (LExpression.NamedUseNode(0) <> LExpression.Syntax.Root) or
    (LExpression.NamedUseArity(0) <> 0) or (AUse.BindingCount <> 1) then
    Exit;
  Result := AUse.BindingAt(0).Same(AExpected);
end;

function FixtureFactory(const AUnit: INyxDocumentationType): TNyxDocumentationRef;
var
  LAt, LMatches: Integer;
  LMember: INyxDocumentationMember;
  LCapability: INyxDocumentationMemberSignature;
  LRich: INyxDocumentationRichSignature;
  LParameter: INyxDocumentationParameterDeclaration;
  LLegacy: INyxDocumentationSignature;
  LOldParameter: TNyxDocumentationParameter;
  LShape: Boolean;
  LResultType: TNyxDocumentationRef;
begin
  Result := Default(TNyxDocumentationRef);
  LMatches := 0;
  LResultType := NyxDocumentationType('nyx.test.documentation', 'IDocumentationBadge');
  for LAt := 0 to AUnit.MemberCount - 1 do
  begin
    LMember := AUnit.MemberAt(LAt);

    if (LMember.Kind <> ndmFactory) or
      (LMember.Identity.MemberName <> 'NewDocumentationBadge') then
      Continue;
    LRich := nil;

    if Supports(LMember, INyxDocumentationMemberSignature, LCapability) then
      LRich := LCapability.DeclaredSignature;
    LShape := False;

    if LRich <> nil then
    begin

      if (LRich.ParameterCount = 1) and FixtureNamedUse(LRich.ResultUse, LResultType) then
      begin
        LParameter := LRich.ParameterAt(0);
        LShape := (LParameter.Name = 'AID') and (LParameter.Passing = ndpConst) and
          not LParameter.HasDefault and FixtureNamedUse(LParameter.TypeUse, TextType);
      end;
    end
    else
    begin
      LLegacy := LMember.Signature;

      if (LLegacy <> nil) and (LLegacy.ParameterCount = 1) and
        LLegacy.ResultType.Same(LResultType) then
      begin
        LOldParameter := LLegacy.ParameterAt(0);
        LShape := (LOldParameter.Name = 'AID') and (LOldParameter.Passing = ndpConst) and
          not LOldParameter.HasDefault and LOldParameter.TypeReference.Same(TextType);
      end;
    end;

    if not LShape then
      Continue;
    Inc(LMatches);
    Result := LMember.Identity;
  end;

  if LMatches <> 1 then
    raise ENyxDocumentation.Create('The fixture requires its unique actual declared factory signature');
end;

procedure RegisterDocumentationFixture(ACatalog: TNyxCatalog;
  const APackage: INyxDocumentationPackage);
var
  LPackage: INyxDocumentationPackage;
  LUnit, LClass, LInterface, LCallback: INyxDocumentationType;
  LDocumentation, LCallbackDocumentation: INyxDocumentationRegistration;
  LSnapshot: INyxCatalogSnapshot;
  LDocuments: INyxCatalogDocumentation;
  LExisting: INyxDocumentationRegistration;
  LBinding: INyxDocumentationContributionBinding;
  LIndex: Integer;
begin
  { Capture may call an alternate producer. Recheck global ownership only after
    it returns, before any real registry or catalog publication. }
  LPackage := CaptureNyxDocumentationPackage(APackage);

  if LPackage = nil then
    raise ENyxDocumentation.Create('Source fixture registration requires its explicit declaration package');

  if GRegistered and (GSourcePackage = nil) then
    raise ENyxDocumentation.Create('The fixture is already registered with legacy metadata');

  if (GSourcePackage <> nil) and not SameNyxDocumentationPackage(GSourcePackage, LPackage) then
    raise ENyxDocumentation.Create('The fixture is already registered with a different source package');
  LUnit := LPackage.FindDeclaration(NyxDocumentationUnit('nyx.test.documentation'));
  LClass := LPackage.FindDeclaration(NyxDocumentationType('nyx.test.documentation', 'TDocumentationBadge'));
  LInterface := LPackage.FindDeclaration(NyxDocumentationType('nyx.test.documentation', 'IDocumentationBadge'));
  LCallback := LPackage.FindDeclaration(NyxDocumentationType('nyx.test.documentation', 'TDocumentationCallback'));

  if (LUnit = nil) or (LClass = nil) or (LInterface = nil) or (LCallback = nil) then
    raise ENyxDocumentation.Create('The source package omits a declared fixture registration owner');

  if (LUnit.Kind <> ndtUnit) or (LClass.Kind <> ndtClass) or
    (LInterface.Kind <> ndtInterface) or not (LCallback.Kind in [ndtClass, ndtCallback]) then
    raise ENyxDocumentation.Create('Source fixture declaration roles disagree with their real registration');

  if GSourceDocumentation = nil then
    LDocumentation := NewNyxDocumentationBinding(LPackage, FixtureFactory(LUnit))
  else
    LDocumentation := GSourceDocumentation;
  { Prepare the actual callback binding before any global effect. ForPrimary
    retains the exact package, with this class's source-provided identity. }
  LCallbackDocumentation := LDocumentation.ForPrimary(LCallback.Identity);

  if ACatalog <> nil then
  begin
    LIndex := ACatalog.IndexOf(DocumentationFixtureKind);

    if LIndex >= 0 then
    begin
      LSnapshot := ACatalog.Capture;

      if not Supports(LSnapshot, INyxCatalogDocumentation, LDocuments) then
        raise ENyxDocumentation.Create('The existing catalog fixture has no declaration attachment');
      LExisting := LDocuments.DocumentationAt(LIndex);

      if (LExisting = nil) or not LExisting.Primary.Same(LDocumentation.Primary) or
        not Supports(LExisting, INyxDocumentationContributionBinding, LBinding) then
        raise ENyxDocumentation.Create('The catalog fixture has a different declaration attachment');

      if not SameNyxDocumentationPackage(LPackage, LBinding.PackageOwner) then
        raise ENyxDocumentation.Create('The catalog fixture belongs to a different source package');
    end;
  end;
  PublishDocumentationFixture(LDocumentation, LCallbackDocumentation);
  { The same immutable owner now follows the actual global registration lifetime.
    No parsed world, source document, producer, control or callback is retained. }
  GSourcePackage := LPackage;
  GSourceDocumentation := LDocumentation;
  AddDocumentationFixture(ACatalog, LDocumentation);
end;

end.
