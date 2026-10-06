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
unit nyx.content;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.responsive, nyx.presentations, nyx.data;

const
  NyxMaximumContentRules = 64;
  NyxContentRulesWireField = 'contentRules';

type
  ENyxContent = class(Exception);
  TNyxContentScope = (ncsDefault, ncsViewport, ncsPresentation);

  { Copied recipe reference and closed scope. Recipes are reusable definitions
    owned by the document, never borrowed node/UI pointers. Each rule replaces
    the entire instance recipe; ordinary configuration and part overrides are
    subsequently applied to that independent selected recipe. }
  TNyxContentRule = record
  private
    FScope: TNyxContentScope;
    FPlatform: TNyxPlatform;
    FViewport: TNyxViewportCondition;
    FPresentation: TNyxPresentationRef;
    FComponent: TNyxComponentRef;
  public
    function SameScope(const AOther: TNyxContentRule): Boolean;
    { Strict structured editor/codec boundary. Application authoring uses the
      fluent registry, not these closed wire spellings. }
    function ToData: TNyxDataValue;
    property Scope: TNyxContentScope read FScope;
    property Platform: TNyxPlatform read FPlatform;
    property Viewport: TNyxViewportCondition read FViewport;
    property Presentation: TNyxPresentationRef read FPresentation;
    property Component: TNyxComponentRef read FComponent;
  end;

  { Managed recipe authoring. Each scope owns its registry reference, but the
    registry owns only copied values and never retains scopes, nodes or its
    document. Retained scopes remain safe after a control/document is released.
    Use replaces an exact scope in place, preserving deterministic rule order.
    All candidates are validated before mutation. Clear removes only this scope;
    Done returns ordinary defaults. Clone owns independent rule storage. }
  INyxContent = interface(IInterface)
    ['{13FD1BA5-EEAC-4F89-B482-72C6D66B5A01}']
    function GetCount: Integer;
    function Rule(AIndex: Integer): TNyxContentRule;
    function Use(const AComponent: TNyxComponentRef): INyxContent;
    function Clear: INyxContent;
    function WhenViewport(const ACondition: TNyxViewportCondition): INyxContent; overload;
    function WhenViewport(const AWidth: TNyxViewportWidth): INyxContent; overload;
    function WhenPresentation(const AReference: TNyxPresentationRef): INyxContent;
    function ForPlatform(APlatform: TNyxPlatform): INyxContent;
    function Done: INyxContent;
    function Clone: INyxContent;
    function ToData: TNyxDataValue;
    property Count: Integer read GetCount;
  end;

function NewNyxContent: INyxContent;
{ Version-one ordered rules. Duplicate scopes, missing/unknown fields, invalid
  intervals/enums/references and excess rules refuse the complete candidate.
  Document admission separately checks every referenced recipe/presentation,
  including currently inactive branches and recursive recipe dependencies. }
function NyxContentFromData(const AData: TNyxDataValue): INyxContent;

implementation

type
  INyxContentBook = interface(IInterface)
    ['{13FD1BA5-EEAC-4F89-B482-72C6D66B5A02}']
    function GetCount: Integer;
    function Rule(AIndex: Integer): TNyxContentRule;
    procedure Put(const ARule: TNyxContentRule);
    procedure Remove(const AScope: TNyxContentRule);
  end;

  TNyxContentBook = class(TInterfacedObject, INyxContentBook)
  private
    FRules: array of TNyxContentRule;
    function IndexOf(const AScope: TNyxContentRule): Integer;
  public
    function GetCount: Integer;
    function Rule(AIndex: Integer): TNyxContentRule;
    procedure Put(const ARule: TNyxContentRule);
    procedure Remove(const AScope: TNyxContentRule);
  end;

  TNyxContent = class(TInterfacedObject, INyxContent)
  private
    FBook: INyxContentBook;
    FScope: TNyxContentRule;
  public
    constructor Create(const ABook: INyxContentBook; const AScope: TNyxContentRule);
    function GetCount: Integer;
    function Rule(AIndex: Integer): TNyxContentRule;
    function Use(const AComponent: TNyxComponentRef): INyxContent;
    function Clear: INyxContent;
    function WhenViewport(const ACondition: TNyxViewportCondition): INyxContent; overload;
    function WhenViewport(const AWidth: TNyxViewportWidth): INyxContent; overload;
    function WhenPresentation(const AReference: TNyxPresentationRef): INyxContent;
    function ForPlatform(APlatform: TNyxPlatform): INyxContent;
    function Done: INyxContent;
    function Clone: INyxContent;
    function ToData: TNyxDataValue;
  end;

function TNyxContentRule.SameScope(const AOther: TNyxContentRule): Boolean;
begin
  Result := (FScope = AOther.FScope) and (FPlatform = AOther.FPlatform);

  if Result and (FScope = ncsViewport) then
  begin
    Result := FViewport.Same(AOther.FViewport);
  end;

  if Result and (FScope = ncsPresentation) then
  begin
    Result := FPresentation.Name = AOther.FPresentation.Name;
  end;
end;

function TNyxContentRule.ToData: TNyxDataValue;
var
  LWhen: TNyxDataValue;
  LScope: TNyxText;
begin
  LScope := 'default';
  LWhen := NyxNull;

  if FScope = ncsViewport then
  begin
    LScope := 'viewport';
    LWhen := NyxObject([
      NyxField('widthMinimum', NyxData(FViewport.WidthMinimum)),
      NyxField('widthMaximum', NyxData(FViewport.WidthMaximum)),
      NyxField('heightMinimum', NyxData(FViewport.HeightMinimum)),
      NyxField('heightMaximum', NyxData(FViewport.HeightMaximum)),
      NyxField('orientation', NyxData(NyxViewportOrientationName(FViewport.OrientationValue)))]);
  end
  else if FScope = ncsPresentation then
  begin
    LScope := 'presentation';
    LWhen := NyxData(FPresentation.Name);
  end;
  Result := NyxObject([
    NyxField('scope', NyxData(LScope)),
    NyxField('platform', NyxData(NyxPlatformName(FPlatform))),
    NyxField('component', NyxData(FComponent.Name)),
    NyxField('when', LWhen)]);
end;

function TNyxContentBook.GetCount: Integer;
begin
  Result := Length(FRules);
end;

function TNyxContentBook.Rule(AIndex: Integer): TNyxContentRule;
begin

  if (AIndex < 0) or (AIndex >= GetCount) then
  begin
    raise ENyxContent.Create('Content rule index is outside its registry');
  end;
  Result := FRules[AIndex];
end;

function TNyxContentBook.IndexOf(const AScope: TNyxContentRule): Integer;
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(FRules) do
  begin

    if FRules[LIndex].SameScope(AScope) then
    begin
      Exit(LIndex);
    end;
  end;
  Result := -1;
end;

procedure ValidateComponent(const AComponent: TNyxComponentRef);
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
  LMeaningful: Boolean;
begin
  LIndex := 1;
  LCount := 0;
  LMeaningful := False;
  while LIndex <= Length(AComponent.Name) do
  begin

    if not NyxNextScalar(AComponent.Name, LIndex, LScalar) or (LScalar < 32) or
      ((LScalar >= 127) and (LScalar <= 159)) then
    begin
      raise ENyxContent.Create('Recipe references require printable Unicode');
    end;
    Inc(LCount);
    LMeaningful := LMeaningful or not NyxScalarWhitespace(LScalar);
  end;

  if not LMeaningful or (LCount > 128) then
  begin
    raise ENyxContent.Create('Recipe references require 1..128 meaningful Unicode scalars');
  end;
end;

procedure TNyxContentBook.Put(const ARule: TNyxContentRule);
var
  LIndex: Integer;
begin
  { Re-admit the distinct reference before touching storage. The default record
    is deliberately not a usable component reference. }
  ValidateComponent(ARule.Component);
  NyxPlatformName(ARule.Platform);
  LIndex := IndexOf(ARule);

  if LIndex < 0 then
  begin

    if GetCount >= NyxMaximumContentRules then
    begin
      raise ENyxContent.Create('Instance exceeds its content recipe budget');
    end;
    LIndex := GetCount;
    SetLength(FRules, LIndex + 1);
  end;
  FRules[LIndex] := ARule;
end;

procedure TNyxContentBook.Remove(const AScope: TNyxContentRule);
var
  LIndex: Integer;
  LNext: Integer;
begin
  LIndex := IndexOf(AScope);

  if LIndex >= 0 then
  begin
    for LNext := LIndex to High(FRules) - 1 do
    begin
      FRules[LNext] := FRules[LNext + 1];
    end;
    SetLength(FRules, Length(FRules) - 1);
  end;
end;

constructor TNyxContent.Create(const ABook: INyxContentBook; const AScope: TNyxContentRule);
begin
  inherited Create;
  FBook := ABook;
  FScope := AScope;
end;

function TNyxContent.GetCount: Integer;
begin
  Result := FBook.GetCount;
end;

function TNyxContent.Rule(AIndex: Integer): TNyxContentRule;
begin
  Result := FBook.Rule(AIndex);
end;

function TNyxContent.Use(const AComponent: TNyxComponentRef): INyxContent;
var
  LRule: TNyxContentRule;
begin
  LRule := FScope;
  LRule.FComponent := NyxComponent(AComponent.Name);
  FBook.Put(LRule);
  Result := Self;
end;

function TNyxContent.Clear: INyxContent;
begin
  FBook.Remove(FScope);
  Result := Self;
end;

function TNyxContent.WhenViewport(const ACondition: TNyxViewportCondition): INyxContent;
var
  LScope: TNyxContentRule;
begin
  ACondition.Matches(0, 0);
  LScope := FScope;
  LScope.FPresentation := Default(TNyxPresentationRef);
  LScope.FViewport := ACondition;
  LScope.FScope := ncsViewport;

  if ACondition.IsAny then
  begin
    LScope.FScope := ncsDefault;
  end;
  Result := TNyxContent.Create(FBook, LScope);
end;

function TNyxContent.WhenViewport(const AWidth: TNyxViewportWidth): INyxContent;
begin
  Result := WhenViewport(TNyxViewportCondition.FromWidth(AWidth));
end;

function TNyxContent.WhenPresentation(const AReference: TNyxPresentationRef): INyxContent;
var
  LScope: TNyxContentRule;
begin
  LScope := FScope;
  LScope.FPresentation := NyxPresentation(AReference.Name);
  LScope.FViewport := TNyxViewportCondition.Any;
  LScope.FScope := ncsPresentation;
  Result := TNyxContent.Create(FBook, LScope);
end;

function TNyxContent.ForPlatform(APlatform: TNyxPlatform): INyxContent;
var
  LScope: TNyxContentRule;
begin
  NyxPlatformName(APlatform);
  LScope := FScope;
  LScope.FPlatform := APlatform;
  Result := TNyxContent.Create(FBook, LScope);
end;

function TNyxContent.Done: INyxContent;
begin
  Result := TNyxContent.Create(FBook, Default(TNyxContentRule));
end;

function TNyxContent.Clone: INyxContent;
var
  LBook: INyxContentBook;
  LIndex: Integer;
begin
  LBook := TNyxContentBook.Create;
  for LIndex := 0 to GetCount - 1 do
  begin
    LBook.Put(Rule(LIndex));
  end;
  Result := TNyxContent.Create(LBook, FScope);
end;

function TNyxContent.ToData: TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LItems, GetCount);
  for LIndex := 0 to High(LItems) do
  begin
    LItems[LIndex] := Rule(LIndex).ToData;
  end;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('rules', NyxArray(LItems))]);
end;

function NewNyxContent: INyxContent;
begin
  Result := TNyxContent.Create(TNyxContentBook.Create, Default(TNyxContentRule));
end;

function NyxContentFromData(const AData: TNyxDataValue): INyxContent;
var
  LItems: TNyxDataValue;
  LItem: TNyxDataValue;
  LWhen: TNyxDataValue;
  LScope: INyxContent;
  LIndex: Integer;
  LMinimum: Integer;
  LMaximum: Integer;
  LBefore: Integer;
  LPlatform: TNyxPlatform;
  LOrientation: TNyxViewportOrientation;
  LCondition: TNyxViewportCondition;
  LFound: Boolean;
  LName: TNyxText;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 2) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxContent.Create('Content recipes require version/rules');
  end;
  LItems := AData.Field('rules');

  if (LItems.Kind <> ndArray) or (LItems.Count > NyxMaximumContentRules) then
  begin
    raise ENyxContent.Create('Content recipes require a bounded rule array');
  end;
  Result := NewNyxContent;
  for LIndex := 0 to LItems.Count - 1 do
  begin
    LItem := LItems.Item(LIndex);

    if (LItem.Kind <> ndObject) or (LItem.Count <> 4) then
    begin
      raise ENyxContent.Create('Content rule requires scope/platform/component/when');
    end;
    LFound := False;
    for LPlatform := npfAny to npfNativeLCL do
    begin

      if NyxPlatformName(LPlatform) = LItem.Field('platform').AsText then
      begin
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      raise ENyxContent.Create('Unknown content platform');
    end;
    LScope := Result.ForPlatform(LPlatform);
    LWhen := LItem.Field('when');
    LName := LItem.Field('scope').AsText;

    if LName = 'viewport' then
    begin

      if (LWhen.Kind <> ndObject) or (LWhen.Count <> 5) then
      begin
        raise ENyxContent.Create('Content viewport requires both intervals and orientation');
      end;
      LCondition := TNyxViewportCondition.Any;
      LMinimum := LWhen.Field('widthMinimum').AsInteger;
      LMaximum := LWhen.Field('widthMaximum').AsInteger;

      if LMaximum = 0 then
      begin
        LCondition := LCondition.WidthAtLeast(LMinimum);
      end
      else
      begin
        LCondition := LCondition.WidthBetween(LMinimum, LMaximum);
      end;
      LMinimum := LWhen.Field('heightMinimum').AsInteger;
      LMaximum := LWhen.Field('heightMaximum').AsInteger;

      if LMaximum = 0 then
      begin
        LCondition := LCondition.HeightAtLeast(LMinimum);
      end
      else
      begin
        LCondition := LCondition.HeightBetween(LMinimum, LMaximum);
      end;
      LFound := False;
      for LOrientation := nvoAny to nvoSquare do
      begin

        if NyxViewportOrientationName(LOrientation) = LWhen.Field('orientation').AsText then
        begin
          LFound := True;
          Break;
        end;
      end;

      if not LFound or LCondition.Orientation(LOrientation).IsAny then
      begin
        raise ENyxContent.Create('Content viewport requires a nonempty valid condition');
      end;
      LScope := LScope.WhenViewport(LCondition.Orientation(LOrientation));
    end
    else if LName = 'presentation' then
    begin
      LScope := LScope.WhenPresentation(NyxPresentation(LWhen.AsText));
    end
    else if (LName <> 'default') or (LWhen.Kind <> ndNull) then
    begin
      raise ENyxContent.Create('Unknown content scope or invalid default predicate');
    end;
    LBefore := Result.Count;
    LScope.Use(NyxComponent(LItem.Field('component').AsText));

    if Result.Count <> LBefore + 1 then
    begin
      raise ENyxContent.Create('Duplicate content scope');
    end;
  end;
end;

end.
