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

{ Registration publishes the same extension-owned prose/defaults to the existing
  catalog/schema/callback authorities. No second help registry or page list is
  populated. Reference02 will consume declaration-bound registration metadata. }
procedure RegisterDocumentationFixture(ACatalog: TNyxCatalog);

implementation

uses nyx.schema, nyx.catalog.labels, nyx.callbacks, nyx.contract, nyx.event.payload;

var
  GRegistered: Boolean;

function TextType: TNyxDocumentationRef;
begin
  Result := NyxDocumentationType('nyx.text', 'TNyxText');
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
begin

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
        ncCustom, ncCustom, NyxScalarPayload(NyxTextDomain))]);
    RegisterNyxCallback(NyxHandler('TDocumentationCallback'), TDocumentationCallback);
    GRegistered := True;
  end;

  if (ACatalog <> nil) and (ACatalog.IndexOf(DocumentationFixtureKind) < 0) then
  begin
    ACatalog.RegisterKind(NyxCustomKind(DocumentationFixtureKind), 'Friendly badge', 'Custom', False);
    ACatalog.Describe(NyxCustomKind(DocumentationFixtureKind), pgText, [clText],
      'greeting, hello', DocumentationFixtureSummary);
  end;
end;

end.
