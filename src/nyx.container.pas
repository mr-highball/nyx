unit nyx.container;

{$mode delphi}

interface

uses
  nyx.types,
  nyx.element;

type

  { TNyxContainerBaseImpl }
  (*
    base implementation class for all INyxContainer
  *)
  TNyxContainerBaseImpl = class(TNyxElementBaseImpl, INyxContainer)
  strict private
    FElements : INyxElements;
    FUI : INyxUI;
  protected
    function GetElements: INyxElements;
    function GetUI: INyxUI;
    procedure SetUI(const AValue: INyxUI);
  strict protected
    (*
      override container get since we are a container
    *)
    function DoGetContainer: INyxContainer; override;

    (*
      called when an element has been added to the container
      and is for "parenting" purposes
    *)
    procedure DoUpdateElementParent(const AElement : INyxElement); virtual;
  public
    property Elements : INyxElements read GetElements;
    property UI : INyxUI read GetUI write SetUI;

    function Add(const AItem : INyxElement; out Index : Integer) : INyxContainer; overload;
    function Add(const AItem : INyxElement) : INyxContainer; overload;

    constructor Create; override;
    destructor Destroy; override;
  end;

  (*
    metaclass for nyx containers
  *)
  TNyxContainerClass = class of TNyxContainerBaseImpl;

implementation

{ TNyxContainerBaseImpl }

function TNyxContainerBaseImpl.GetElements: INyxElements;
begin
  Result := FElements;
end;

function TNyxContainerBaseImpl.GetUI: INyxUI;
begin
  Result := FUI;
end;

procedure TNyxContainerBaseImpl.SetUI(const AValue: INyxUI);
begin
  FUI := nil;
  FUI := AValue;
end;

function TNyxContainerBaseImpl.DoGetContainer: INyxContainer;
begin
  Result := Self as INyxContainer;
end;

procedure TNyxContainerBaseImpl.DoUpdateElementParent(
  const AElement: INyxElement);
begin
  AElement.Container := Self as INyxContainer;
end;

function TNyxContainerBaseImpl.Add(const AItem: INyxElement; out Index: Integer): INyxContainer;
begin
  Result := Self as INyxContainer;
  DoUpdateElementParent(AItem);
  Index := FElements.Add(AItem);
end;

function TNyxContainerBaseImpl.Add(const AItem: INyxElement): INyxContainer;
var
  I : Integer;
begin
  Result := Add(AItem, I);
end;

constructor TNyxContainerBaseImpl.Create;
begin
  inherited Create;
  FElements := NewNyxElements;
  FUI := nil
end;

destructor TNyxContainerBaseImpl.Destroy;
begin
  FUI := nil;
  inherited Destroy;
end;

end.

