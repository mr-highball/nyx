{ Public declaration/decorator example. MIT license, see LICENSE. }
unit nyx.test.documentation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.model, nyx.controls, nyx.events, nyx.behavior,
  nyx.scheduler, nyx.catalog, nyx.documentation;

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
procedure RegisterDocumentationFixture(ACatalog: TNyxCatalog);

implementation

uses nyx.schema, nyx.catalog.labels, nyx.callbacks, nyx.contract, nyx.event.payload,
  nyx.documentation.registration;

var
  GRegistered: Boolean;

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
  Result := NewNyxDocumentationType(LID, ndtCallback, 'Observe a friendly greeting event.')
    .WithBase(NyxDocumentationType('nyx.events', 'TNyxEventCallback'))
    .WithMember(NewNyxDocumentationMember(NyxDocumentationMember(LID, 'Invoke'),
      ndmMethod, 'Receive an event and its execution scope.')
      .WithSignature(NewNyxDocumentationSignature([
        NyxDocumentationParameter('AEvent', NyxDocumentationType('nyx.behavior', 'TNyxEventInfo'), ndpConst),
        NyxDocumentationParameter('AExecution', NyxDocumentationType('nyx.scheduler', 'INyxExecution'), ndpConst)],
        Default(TNyxDocumentationRef))));
end;

procedure RegisterDocumentationFixture(ACatalog: TNyxCatalog);
var
  LProperty: TNyxPropertyInfo;
  LDocumentation: INyxDocumentationRegistration;
  LDeclarations: TNyxDocumentationDeclarations;
  LUnit: TNyxDocumentationRef;
begin
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

  if not GRegistered then
  begin
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
        ncCustom, ncCustom, NyxScalarPayload(NyxTextDomain))], LDocumentation);
    RegisterNyxCallback(NyxHandler('TDocumentationCallback'), TDocumentationCallback,
      LDocumentation.ForPrimary(NyxDocumentationType('nyx.test.documentation', 'TDocumentationCallback')));
    GRegistered := True;
  end;

  if (ACatalog <> nil) and (ACatalog.IndexOf(DocumentationFixtureKind) < 0) then
  begin
    ACatalog.RegisterKind(NyxCustomKind(DocumentationFixtureKind), 'Friendly badge', 'Custom',
      False, LDocumentation);
    ACatalog.Describe(NyxCustomKind(DocumentationFixtureKind), pgText, [clText],
      'greeting, hello', DocumentationFixtureSummary);
  end;
end;

end.
