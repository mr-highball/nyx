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

unit nyx.disclosure;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls;

type
  { Immutable named-part policy. The header must be a button outside the retained
    body container. Defaults use header/body; derived recipes may select their
    own nested parts without exposing DOM or LCL types. }
  TNyxDisclosureOptions = record
  private
    FHeader: TNyxPartRef;
    FBody: TNyxPartRef;
  public
    class function Defaults: TNyxDisclosureOptions; static;
    function Header(const APart: TNyxPartRef): TNyxDisclosureOptions;
    function Body(const APart: TNyxPartRef): TNyxDisclosureOptions;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AValue: TNyxDataValue): TNyxDisclosureOptions; static;
    property HeaderPart: TNyxPartRef read FHeader;
    property BodyPart: TNyxPartRef read FBody;
  end;

const
  NyxDisclosureKey = 'nyx.disclosure';

{ Known extension detection; custom derived recipes retain this same contract. }
function NyxIsDisclosure(ANode: TNyxNode): Boolean;
{ Copy typed policy at its explicit persistence boundary. It validates names;
  complete document admission checks parts, structure and Boolean value domain. }
procedure SetNyxDisclosureOptions(const AControl: INyxControl;
  const AOptions: TNyxDisclosureOptions);
function NyxDisclosureOptions(ANode: TNyxNode): TNyxDisclosureOptions;
{ Borrow exact parts only during this call. Invalid shapes/domains/value refuse.
  Header/body must be independent owned branches; no reference is retained. }
procedure ValidateNyxDisclosure(ANode: TNyxNode);
{ Desired expansion is a Boolean scalar, separate from authored visibility. }
function NyxDisclosureExpanded(ANode: TNyxNode): Boolean;
{ Local body gate. Ordinary interaction policy combines it with all authored and
  ancestor restrictions; opening cannot reveal a caller-hidden body/descendant. }
function NyxDisclosureBodyVisible(ANode: TNyxNode): Boolean;
{ An exact header recognition returns borrowed root/body plus copied caption.
  Empty header Text inherits the root caption; explicit header text wins. }
function NyxDisclosureHeader(ANode: TNyxNode; out ARoot, ABody: TNyxNode;
  out ACaption: TNyxText): Boolean;
{ Convenience recognition for pre-admission commands with no physical edit. }
function NyxIsDisclosureHeader(ANode: TNyxNode): Boolean;

implementation

uses
  SysUtils, nyx.contract, nyx.schema, nyx.state;

class function TNyxDisclosureOptions.Defaults: TNyxDisclosureOptions;
begin
  Result.FHeader := NyxPart('header');
  Result.FBody := NyxPart('body');
end;

function TNyxDisclosureOptions.Header(const APart: TNyxPartRef): TNyxDisclosureOptions;
begin
  Result := Self;
  Result.FHeader := NyxPart(APart.Name);
  Result.Validate;
end;

function TNyxDisclosureOptions.Body(const APart: TNyxPartRef): TNyxDisclosureOptions;
begin
  Result := Self;
  Result.FBody := NyxPart(APart.Name);
  Result.Validate;
end;

procedure TNyxDisclosureOptions.Validate;
begin

  if (FHeader.Name = '') or (FBody.Name = '') or (FHeader.Name = FBody.Name) then
  begin
    raise ENyxModel.Create('Disclosure requires distinct nonempty header/body parts');
  end;
end;

function TNyxDisclosureOptions.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('header', NyxData(FHeader.Name)), NyxField('body', NyxData(FBody.Name))]);
end;

class function TNyxDisclosureOptions.FromData(const AValue: TNyxDataValue): TNyxDisclosureOptions;
begin

  if (AValue.Kind <> ndObject) or (AValue.Count <> 3) or
    (AValue.Field('version').AsInteger <> 1) then
  begin
    raise ENyxModel.Create('Disclosure policy requires its exact version-one fields');
  end;
  Result.FHeader := NyxPart(AValue.Field('header').AsText);
  Result.FBody := NyxPart(AValue.Field('body').AsText);
  Result.Validate;
end;

function NyxIsDisclosure(ANode: TNyxNode): Boolean;
begin
  Result := (ANode <> nil) and ANode.Extensions.Has(NyxExtension(NyxDisclosureKey));
end;

procedure SetNyxDisclosureOptions(const AControl: INyxControl;
  const AOptions: TNyxDisclosureOptions);
begin

  if AControl = nil then
  begin
    raise ENyxModel.Create('Disclosure policy requires its control');
  end;
  AControl.Extensions.SetValue(NyxExtension(NyxDisclosureKey), AOptions.ToData);
end;

function NyxDisclosureOptions(ANode: TNyxNode): TNyxDisclosureOptions;
begin

  if not NyxIsDisclosure(ANode) then
  begin
    raise ENyxModel.Create('Disclosure policy requires its declared control');
  end;
  Result := TNyxDisclosureOptions.FromData(ANode.Extensions.Value(NyxExtension(NyxDisclosureKey)));
end;

function NyxDisclosureExpanded(ANode: TNyxNode): Boolean;
var
  LDomain: TNyxValueDomain;
begin

  if not ANode.Contract.FindValue(LDomain) or (LDomain.Kind <> nskBoolean) then
  begin
    raise ENyxModel.Create('Disclosure requires a Boolean value domain');
  end;
  Result := LDomain.ReadWire(ANode.Prop(NyxAttributeName(atValue), 'false')).AsBoolean;
end;

procedure ValidateNyxDisclosure(ANode: TNyxNode);
var
  LOptions: TNyxDisclosureOptions;
  LHeader: TNyxNode;
  LBody: TNyxNode;
  LCursor: TNyxNode;
  LPrimitive: TNyxPrimitiveInfo;
begin
  LOptions := NyxDisclosureOptions(ANode);
  LHeader := ANode.Part(LOptions.HeaderPart);
  LBody := ANode.Part(LOptions.BodyPart);

  if (LHeader.ProjectionKind <> NyxKindName(nkButton)) or
    not FindNyxPrimitive(LBody.ProjectionKind, LPrimitive) or not LPrimitive.Container then
  begin
    raise ENyxModel.Create('Disclosure requires a button header and a body container');
  end;
  LCursor := LHeader;
  while LCursor <> nil do
  begin

    if LCursor = LBody then
    begin
      raise ENyxModel.Create('A disclosure header cannot be inside its hidden body');
    end;
    LCursor := LCursor.Parent;
  end;
  NyxDisclosureExpanded(ANode);
end;

function NyxDisclosureBodyVisible(ANode: TNyxNode): Boolean;
var
  LOwner: TNyxNode;
  LOptions: TNyxDisclosureOptions;
begin
  Result := True;
  LOwner := ANode.Parent;
  while LOwner <> nil do
  begin

    if NyxIsDisclosure(LOwner) then
    begin
      LOptions := NyxDisclosureOptions(LOwner);

      if LOwner.Part(LOptions.BodyPart) = ANode then
      begin
        Result := Result and NyxDisclosureExpanded(LOwner);
      end;
    end;
    LOwner := LOwner.Parent;
  end;
end;

function NyxDisclosureHeader(ANode: TNyxNode; out ARoot, ABody: TNyxNode;
  out ACaption: TNyxText): Boolean;
var
  LOwner: TNyxNode;
  LOptions: TNyxDisclosureOptions;
begin
  Result := False;
  ARoot := nil;
  ABody := nil;
  ACaption := '';
  LOwner := ANode.Parent;
  while LOwner <> nil do
  begin

    if NyxIsDisclosure(LOwner) then
    begin
      LOptions := NyxDisclosureOptions(LOwner);

      if LOwner.Part(LOptions.HeaderPart) = ANode then
      begin
        ARoot := LOwner;
        ABody := LOwner.Part(LOptions.BodyPart);
        ACaption := ANode.Prop(NyxAttributeName(atText));

        if ACaption = '' then
        begin
          ACaption := LOwner.Prop(NyxAttributeName(atText), 'Details');
        end;
        Exit(True);
      end;
    end;
    LOwner := LOwner.Parent;
  end;
end;

function NyxIsDisclosureHeader(ANode: TNyxNode): Boolean;
var
  LRoot: TNyxNode;
  LBody: TNyxNode;
  LCaption: TNyxText;
begin
  Result := NyxDisclosureHeader(ANode, LRoot, LBody, LCaption);
end;

end.
