{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

unit nyx.application.navigation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.behavior, nyx.model, nyx.controls, nyx.selection;

type
  { Owned portable navigation recipe. Document and Root are borrowed/retained
    presentation seams; a renderer must unmount before the last navigation lease
    is released. Targets and captions are copied at construction, independent of
    later source page order, captions or source retirement. No source document,
    widget or callback is retained. Callers may customize named page-N parts
    before mounting. Resolve returns an intent, never performs navigation. }
  INyxPageNavigation = interface(IInterface)
    ['{E51B5C80-705A-45A3-8962-BD9EE336E4A1}']
    function GetDocument: TNyxDocument;
    function GetRoot: INyxToolbar;
    { Only unconsumed clicks with an exact source in this tree and an origin
      within a known page button resolve. False returns an empty control ref. }
    function Resolve(const AEvent: TNyxEventInfo; out APage: TNyxControlRef): Boolean;
    property Document: TNyxDocument read GetDocument;
    property Root: INyxToolbar read GetRoot;
  end;

  { Optional current-page capability. Keeping this separate preserves the original
    navigation interface's method table. UI-thread operations retain no source
    document or target view. Selection is confirmed after a page mounts, never
    merely because Resolve returned an intent. Unknown identities refuse before
    changing any button. Styling is an immutable copy; authored mode preserves
    each part's Variant. This capability never writes toggle Pressed semantics. }
  INyxPageNavigationState = interface(IInterface)
    ['{39887D47-26B0-44B8-BF11-131026000004}']
    function GetCurrentPage: TNyxControlRef;
    function GetOptions: TNyxSelectionOptions;
    procedure Configure(const AOptions: TNyxSelectionOptions);
    procedure SelectPage(const APage: TNyxControlRef);
    { Copy only the current variant policy into a caller-owned realized navigation
      tree. Exact captured button identities/kinds/ownership are admitted before
      mutation. Extra toolbar parts and button descendants remain creator-owned.
      No tree is retained; adapters synchronize existing controls. }
    procedure Project(ARoot: TNyxNode);
    function IsCurrent(const AButton: TNyxControlRef): Boolean;
    property CurrentPage: TNyxControlRef read GetCurrentPage;
    property Options: TNyxSelectionOptions read GetOptions;
  end;

{ Borrow the caption contract for one read. Exact nonempty user text supplies
  navigation/accessibility text; absent/empty text falls back to the stable ID.
  Whitespace and supplementary Unicode are retained without normalization.
  Raw legacy descriptors use a temporary retained caption facade; nil refuses. }
function NyxPageCaption(APage: TNyxNode): TNyxText;
{ Stable independent navigation control identity; negative indices refuse. }
function NyxPageNavigationButtonID(AIndex: Integer): TNyxText;
{ Copy page identities/captions and theme into public Nyx toolbar/button parts.
  Nil/empty sources refuse. The returned interface owns its independent document;
  neither source ownership nor page identity is changed. Composition failure
  retires every candidate before raising. }
function NewNyxPageNavigation(ASource: TNyxDocument): INyxPageNavigation;
{ Acquire the optional owned selection capability. Nil/foreign recipes refuse;
  the result may outlive the original navigation reference. }
function NyxPageNavigationState(const ANavigation: INyxPageNavigation): INyxPageNavigationState;

implementation

uses
  nyx.design.tokens;

type
  TPageNavigation = class(TInterfacedObject, INyxPageNavigation, INyxPageNavigationState)
  private
    FDocument: TNyxDocument;
    FRoot: INyxToolbar;
    FTargets: array of TNyxControlRef;
    FButtons: array of INyxButton;
    FCurrent: TNyxControlRef;
    FOptions: TNyxSelectionOptions;
    procedure ApplySelection;
  public
    constructor Create(ASource: TNyxDocument);
    destructor Destroy; override;
    function GetDocument: TNyxDocument;
    function GetRoot: INyxToolbar;
    function Resolve(const AEvent: TNyxEventInfo; out APage: TNyxControlRef): Boolean;
    function GetCurrentPage: TNyxControlRef;
    function GetOptions: TNyxSelectionOptions;
    procedure Configure(const AOptions: TNyxSelectionOptions);
    procedure SelectPage(const APage: TNyxControlRef);
    procedure Project(ARoot: TNyxNode);
    function IsCurrent(const AButton: TNyxControlRef): Boolean;
  end;

function NyxPageCaption(APage: TNyxNode): TNyxText;
var
  LCaption: INyxCaptionControl;
begin

  if APage = nil then
  begin
    raise ENyxModel.Create('Page caption requires a descriptor');
  end;

  if not Supports(APage.ComponentReference, INyxCaptionControl, LCaption) then
  begin
    LCaption := TNyxCaptionControl.CreateFromNode(APage);
  end;
  Result := LCaption.Text;

  if Result = '' then
  begin
    Result := APage.ID;
  end;
end;

function NyxPageNavigationButtonID(AIndex: Integer): TNyxText;
begin

  if AIndex < 0 then
  begin
    raise ENyxModel.Create('Page navigation index must be nonnegative');
  end;
  Result := TNyxText('nav-') + TNyxText(IntToStr(AIndex));
end;

constructor TPageNavigation.Create(ASource: TNyxDocument);
var
  LIndex: Integer;
  LPrevious: Integer;
  LButton: INyxButton;
begin
  inherited Create;
  FOptions := TNyxSelectionOptions.Defaults;

  if (ASource = nil) or (ASource.Count = 0) then
  begin
    raise ENyxModel.Create('Page navigation needs at least one source page');
  end;
  SetLength(FTargets, ASource.Count);
  SetLength(FButtons, ASource.Count);
  FDocument := TNyxDocument.Create;
  SetNyxThemeTokens(FDocument, NyxDeclaredThemeTokens(ASource));
  FRoot := NewNyxToolbar('app-navigation');
  FRoot.Configure.Layout(nlRow).Padding(12).Done;
  FDocument.AddPage(FRoot);
  for LIndex := 0 to ASource.Count - 1 do
  begin
    FTargets[LIndex] := NyxControl(ASource.Pages[LIndex].ID);
    for LPrevious := 0 to LIndex - 1 do
    begin

      if FTargets[LPrevious].ID = FTargets[LIndex].ID then
      begin
        raise ENyxModel.Create('Page navigation requires distinct source identities');
      end;
    end;
    LButton := NewNyxButton(NyxPageNavigationButtonID(LIndex));
    LButton.Configure.Text(NyxPageCaption(ASource.Pages[LIndex]))
      .PartName(NyxPart(TNyxText('page-') + TNyxText(IntToStr(LIndex)))).Done;
    FRoot.Add(LButton);
    FButtons[LIndex] := LButton;
  end;
end;

destructor TPageNavigation.Destroy;
begin
  FButtons := nil;
  FRoot := nil;
  FDocument.Free;
  inherited Destroy;
end;

function TPageNavigation.GetDocument: TNyxDocument;
begin
  Result := FDocument;
end;

function TPageNavigation.GetRoot: INyxToolbar;
begin
  Result := FRoot;
end;

function TPageNavigation.GetCurrentPage: TNyxControlRef;
begin
  Result := FCurrent;
end;

function TPageNavigation.GetOptions: TNyxSelectionOptions;
begin
  Result := FOptions;
end;

procedure TPageNavigation.ApplySelection;
var
  LIndex: Integer;
begin

  if FOptions.StylingMode = nssAuthored then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FButtons) do
  begin

    if FTargets[LIndex].ID = FCurrent.ID then
    begin
      FButtons[LIndex].Configure.Variant(FOptions.SelectedVariant).Done;
    end
    else
    begin
      FButtons[LIndex].Configure.Variant(FOptions.UnselectedVariant).Done;
    end;
  end;
end;

procedure TPageNavigation.Configure(const AOptions: TNyxSelectionOptions);
begin
  AOptions.Validate;
  FOptions := AOptions;
  ApplySelection;
end;

procedure TPageNavigation.SelectPage(const APage: TNyxControlRef);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FTargets) do
  begin

    if FTargets[LIndex].ID = APage.ID then
    begin
      FCurrent := APage;
      ApplySelection;
      Exit;
    end;
  end;
  raise ENyxModel.Create('Navigation page is not a captured target: ' + APage.ID);
end;

function TPageNavigation.IsCurrent(const AButton: TNyxControlRef): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to High(FButtons) do
  begin

    if FButtons[LIndex].Node.ID = AButton.ID then
    begin
      Exit(FTargets[LIndex].ID = FCurrent.ID);
    end;
  end;
end;

procedure TPageNavigation.Project(ARoot: TNyxNode);
var
  LIndex: Integer;
  LButton: TNyxNode;
begin

  if (ARoot = nil) or (ARoot.ID <> FRoot.Node.ID) then
  begin
    raise ENyxModel.Create('Current navigation requires its exact realized root');
  end;
  { Admit the complete group before copying any presentation values. }
  for LIndex := 0 to High(FButtons) do
  begin
    LButton := ARoot.Find(FButtons[LIndex].Node.ID);

    if (LButton = nil) or (LButton.Kind <> NyxKindName(nkButton)) or
      (LButton.Parent <> ARoot) then
    begin
      raise ENyxModel.Create('Current navigation requires its exact realized buttons');
    end;
  end;

  if FOptions.StylingMode = nssAuthored then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FButtons) do
  begin
    LButton := ARoot.Find(FButtons[LIndex].Node.ID);

    if FTargets[LIndex].ID = FCurrent.ID then
    begin
      LButton.Configure.Variant(FOptions.SelectedVariant).Done;
    end
    else
    begin
      LButton.Configure.Variant(FOptions.UnselectedVariant).Done;
    end;
  end;
end;

function TPageNavigation.Resolve(const AEvent: TNyxEventInfo; out APage: TNyxControlRef): Boolean;
var
  LSource: TNyxNode;
  LButton: TNyxNode;
  LIndex: Integer;
begin
  APage := Default(TNyxControlRef);
  Result := False;

  if (AEvent.Trigger <> ntClick) or AEvent.DefaultPrevented then
  begin
    Exit;
  end;
  LSource := FRoot.Node.Find(AEvent.SourceID);

  if LSource = nil then
  begin
    Exit;
  end;
  for LIndex := 0 to High(FTargets) do
  begin
    LButton := FButtons[LIndex].Node;

    if (LButton <> nil) and (LButton.Find(AEvent.OriginID) <> nil) and
      ((LSource.Find(LButton.ID) <> nil) or (LButton.Find(LSource.ID) <> nil)) and
      (LButton.Parent <> nil) then
    begin
      APage := FTargets[LIndex];
      Exit(True);
    end;
  end;
end;

function NewNyxPageNavigation(ASource: TNyxDocument): INyxPageNavigation;
begin
  Result := TPageNavigation.Create(ASource);
end;

function NyxPageNavigationState(const ANavigation: INyxPageNavigation): INyxPageNavigationState;
begin

  if not Supports(ANavigation, INyxPageNavigationState, Result) then
  begin
    raise ENyxModel.Create('Navigation recipe has no current-page capability');
  end;
end;

end.
