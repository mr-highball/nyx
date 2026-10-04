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
unit nyx.studio.rootedits;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.root.types, nyx.studio.projects;

const
  NyxMaximumRootRemovals = 16;

type
  { Immutable reviewed command. Owns copied roots and accepted paired text,
    never a document, widget or supplier session. Inspection describes the exact
    deletion group; references from retained roots block it. Candidate checks
    the current pair/draft again and returns independently owned paired text.
    The caller publishes through ordinary Studio history after response admission.
    Pascal imports/helpers/handler classes and document defaults remain retained. }
  INyxRootRemoval = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001004003002}']
    function Inspect: TNyxDataValue;
    function Candidate(const ACurrent: TNyxProjectPair): TNyxProjectPair;
  end;

function ReviewNyxRootRemoval(const APair: TNyxProjectPair;
  const ARoots: array of TNyxRootRef): INyxRootRemoval;
{ Closed explicit wire boundary: each entry contains exactly root/id, with
  root = page/component. No descendant, wildcard or cascading removal exists. }
function ReadNyxRootRemoval(const APair: TNyxProjectPair;
  const ARoots: TNyxDataValue): INyxRootRemoval;

implementation

uses
  SysUtils, nyx.model, nyx.codec, nyx.source, nyx.schema, nyx.callbacks;

type
  TNyxRootRemoval = class(TInterfacedObject, INyxRootRemoval)
  private
    FRoots: array of TNyxRootRef;
    FBase: TNyxProjectPair;
    FInspection: TNyxDataValue;
    FReferences: Integer;
    function Includes(ANode: TNyxNode): Boolean;
    function CountReferences(ADocument: TNyxDocument;
      const ARoot: TNyxRootRef): Integer;
  public
    constructor Create(const APair: TNyxProjectPair;
      const ARoots: array of TNyxRootRef);
    function Inspect: TNyxDataValue;
    function Candidate(const ACurrent: TNyxProjectPair): TNyxProjectPair;
  end;

function TNyxRootRemoval.Includes(ANode: TNyxNode): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to High(FRoots) do
  begin

    if ANode.ID = FRoots[LIndex].Name then
    begin
      Exit(True);
    end;
  end;
end;

function TNyxRootRemoval.CountReferences(ADocument: TNyxDocument;
  const ARoot: TNyxRootRef): Integer;
var
  LIndex: Integer;

  procedure Visit(ANode: TNyxNode);
  var
    LChild: Integer;
  begin

    if ((ANode.Kind = 'component') or (ANode.ProjectionKind = 'component')) and
      (ANode.Prop('component') = ARoot.Name) then
    begin
      Inc(Result);
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LChild]);
    end;
  end;

begin
  Result := 0;

  if ARoot.Kind <> nrReusable then
  begin
    Exit;
  end;
  for LIndex := 0 to ADocument.Count - 1 do
  begin

    if not Includes(ADocument.Pages[LIndex]) then
    begin
      Visit(ADocument.Pages[LIndex]);
    end;
  end;
  for LIndex := 0 to ADocument.ComponentCount - 1 do
  begin

    if not Includes(ADocument.Components[LIndex]) then
    begin
      Visit(ADocument.Components[LIndex]);
    end;
  end;
end;

constructor TNyxRootRemoval.Create(const APair: TNyxProjectPair;
  const ARoots: array of TNyxRootRef);
var
  LDocument: TNyxDocument;
  LNode: TNyxNode;
  LIndex: Integer;
  LPrior: Integer;
  LNodes: Integer;
  LCallbacks: Integer;
  LReferences: Integer;
  LTotalNodes: Integer;
  LTotalCallbacks: Integer;
  LItems: array of TNyxDataValue;

  procedure CountTree(ANode: TNyxNode);
  var
    LChild: Integer;
    LEvent: Integer;
    LEvents: TNyxAuthoredEventInfos;
  begin
    Inc(LNodes);
    LEvents := NyxAuthoredEvents(ANode);
    for LEvent := 0 to High(LEvents) do
    begin
      Inc(LCallbacks, Length(LEvents[LEvent].Callbacks));
    end;
    for LChild := 0 to ANode.Count - 1 do
    begin
      CountTree(ANode.Children[LChild]);
    end;
  end;

begin
  inherited Create;

  if APair.Pending then
  begin
    raise ENyxModel.Create('Resolve the pending Pascal draft before reviewing root removal');
  end;

  if (Length(ARoots) < 1) or (Length(ARoots) > NyxMaximumRootRemovals) then
  begin
    raise ENyxModel.Create('Root removal requires 1..16 exact root references');
  end;
  FBase := NyxProjectPair(APair.Design, APair.Source);
  SetLength(FRoots, Length(ARoots));
  for LIndex := 0 to High(ARoots) do
  begin
    FRoots[LIndex] := NyxRoot(ARoots[LIndex].Kind, ARoots[LIndex].Name);
    for LPrior := 0 to LIndex - 1 do
    begin

      if FRoots[LPrior].Name = FRoots[LIndex].Name then
      begin
        raise ENyxModel.Create('List each document root once per removal group');
      end;
    end;
  end;
  LDocument := TNyxCodec.Decode(FBase.Design);
  try
    LDocument.Validate;
    ValidateNyxDocumentProperties(LDocument);
    LTotalNodes := 0;
    LTotalCallbacks := 0;
    SetLength(LItems, Length(FRoots));
    for LIndex := 0 to High(FRoots) do
    begin
      LNode := LDocument.FindRoot(FRoots[LIndex]);

      if LNode = nil then
      begin
        raise ENyxModel.Create('The specified page/reusable root is missing');
      end;
      LNodes := 0;
      LCallbacks := 0;
      CountTree(LNode);
      LReferences := CountReferences(LDocument, FRoots[LIndex]);
      Inc(FReferences, LReferences);
      Inc(LTotalNodes, LNodes);
      Inc(LTotalCallbacks, LCallbacks);
      LItems[LIndex] := NyxObject([
        NyxField('root', NyxData(NyxRootKindName(FRoots[LIndex].Kind))),
        NyxField('id', NyxData(FRoots[LIndex].Name)), NyxField('nodes', NyxData(LNodes)),
        NyxField('registrations', NyxData(LCallbacks)),
        NyxField('retainedReferences', NyxData(LReferences))]);
    end;
    FInspection := NyxObject([
      NyxField('roots', NyxArray(LItems)), NyxField('nodes', NyxData(LTotalNodes)),
      NyxField('registrations', NyxData(LTotalCallbacks)),
      NyxField('retainedReferences', NyxData(FReferences)),
      NyxField('ready', NyxData(FReferences = 0)),
      NyxField('pascalRetained', NyxData(True)),
      NyxField('warning', NyxData('Remove these roots and all their authored descendants as one Undo step. ' +
        'Pascal imports, helpers, handler classes and document state defaults remain. ' +
        'Retained reusable references block removal. Compile afterward to check application code that mentions removed IDs.'))]);
  finally
    LDocument.Free;
  end;
end;

function TNyxRootRemoval.Inspect: TNyxDataValue;
begin
  Result := FInspection.Copy;
end;

function TNyxRootRemoval.Candidate(const ACurrent: TNyxProjectPair): TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LIndex: Integer;
  LSource: TNyxText;
begin

  if ACurrent.Pending or (ACurrent.Design <> FBase.Design) or
    (ACurrent.Source <> FBase.Source) then
  begin
    raise ENyxModel.Create('The reviewed project changed; review root removal again');
  end;

  if FReferences <> 0 then
  begin
    raise ENyxModel.Create('Retained roots still reference these reusable definitions; remove the references or include their owning roots');
  end;
  LDocument := TNyxCodec.Decode(FBase.Design);
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LWorkspace.Accept(LDocument, FBase.Source);
    for LIndex := 0 to High(FRoots) do
    begin
      LDocument.RemoveRoot(FRoots[LIndex]);
    end;
    LDocument.Validate;
    ValidateNyxDocumentProperties(LDocument);
    LSource := LWorkspace.Render(LDocument);
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

function ReviewNyxRootRemoval(const APair: TNyxProjectPair;
  const ARoots: array of TNyxRootRef): INyxRootRemoval;
begin
  Result := TNyxRootRemoval.Create(APair, ARoots);
end;

function ReadNyxRootRemoval(const APair: TNyxProjectPair;
  const ARoots: TNyxDataValue): INyxRootRemoval;
var
  LRoots: array of TNyxRootRef;
  LIndex: Integer;
  LField: Integer;
  LValue: TNyxDataValue;
  LKind: TNyxText;
begin

  if (ARoots.Kind <> ndArray) or (ARoots.Count < 1) or
    (ARoots.Count > NyxMaximumRootRemovals) then
  begin
    raise ENyxModel.Create('Root removal requires an array of 1..16 roots');
  end;
  SetLength(LRoots, ARoots.Count);
  for LIndex := 0 to High(LRoots) do
  begin
    LValue := ARoots.Item(LIndex);

    if LValue.Kind <> ndObject then
    begin
      raise ENyxModel.Create('Each removal root requires root/id fields');
    end;
    for LField := 0 to LValue.Count - 1 do
    begin

      if (LValue.Key(LField) <> 'root') and (LValue.Key(LField) <> 'id') then
      begin
        raise ENyxModel.Create('Unknown root removal field');
      end;
    end;
    LKind := LValue.Field('root').AsText;

    if LKind = 'page' then
    begin
      LRoots[LIndex] := NyxPageRoot(LValue.Field('id').AsText);
    end
    else if LKind = 'component' then
    begin
      LRoots[LIndex] := NyxReusableRoot(LValue.Field('id').AsText);
    end
    else
    begin
      raise ENyxModel.Create('A removal root must be page/component');
    end;
  end;
  Result := ReviewNyxRootRemoval(APair, LRoots);
end;

end.
