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
  SysUtils, nyx.text, nyx.types, nyx.behavior, nyx.model, nyx.controls;

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

implementation

uses
  nyx.design.tokens;

type
  TPageNavigation = class(TInterfacedObject, INyxPageNavigation)
  private
    FDocument: TNyxDocument;
    FRoot: INyxToolbar;
    FTargets: array of TNyxControlRef;
    FButtons: array of INyxButton;
  public
    constructor Create(ASource: TNyxDocument);
    destructor Destroy; override;
    function GetDocument: TNyxDocument;
    function GetRoot: INyxToolbar;
    function Resolve(const AEvent: TNyxEventInfo; out APage: TNyxControlRef): Boolean;
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

end.
