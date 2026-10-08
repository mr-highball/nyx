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


unit nyx.studio.resources;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.resources.editor, nyx.studio.edits;

{ Typed candidate-only resource proposal. Both ordinary source processors use
  this public boundary; future semantic resource tools must retain their own
  permission/revision/actor guards around it. Catalog, optional binding and
  crafted Pascal are accepted together by the session's ordinary paired gate. }
function NewNyxStudioResourcePatch(
  const AChange: TNyxResourceEditorChange): INyxDesignPatch;

implementation

uses SysUtils, nyx.types, nyx.model, nyx.catalog,
  nyx.resources, nyx.binding, nyx.composition, nyx.studio.resourceedits,
  nyx.resources.rows, nyx.collections, nyx.collections.codec;

type
  TResourcePatch = class(TInterfacedObject, INyxDesignPatch)
  private
    FChange: TNyxResourceEditorChange;
  public
    constructor Create(const AChange: TNyxResourceEditorChange);
    function Candidate(ADocument: TNyxDocument; ACatalog: TNyxCatalog): TNyxDocument;
  end;

constructor TResourcePatch.Create(const AChange: TNyxResourceEditorChange);
begin
  inherited Create;
  FChange := TNyxResourceEditorChange.FromData(AChange.ToData);
end;

function Projection(ADocument: TNyxDocument; AOwner: TNyxNode): TNyxNode;
var
  LRuntime: TNyxNode;
begin
  Result := nil;

  if AOwner = nil then
  begin
    raise ENyxResource.Create('The captured resource binding control is missing');
  end;

  if AOwner.Kind = NyxKindName(nkSlotOverride) then
  begin

    if AOwner.Prop('mode') = 'remove' then
    begin
      raise ENyxResource.Create('A removed reusable part cannot receive a resource binding');
    end;
    LRuntime := RealizeNyxView(ADocument, AOwner.Parent);
    try
      ApplyNyxBindings(LRuntime, ADocument.State);
      Result := LRuntime.Part(AOwner.Prop('path')).Clone;
    finally
      LRuntime.Free;
    end;
  end
  else
  begin
    Result := RealizeNyxView(ADocument, AOwner);
    try
      ApplyNyxBindings(Result, ADocument.State);
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  end;
end;

function TResourcePatch.Candidate(ADocument: TNyxDocument;
  ACatalog: TNyxCatalog): TNyxDocument;
var
  LCandidate: TNyxDocument;
  LOwner: TNyxNode;
  LProjection: TNyxNode;
  LChanges: array of TNyxResourceChange;
begin

  if (ADocument = nil) or
    (ADocument.Resources.ToData.ToJSON <> FChange.CatalogBaseline) then
  begin
    raise ENyxResource.Create('Resources changed; review the current files before applying');
  end;

  if FChange.Operation in [reoRows, reoDetachRows] then
  begin

    if EncodeNyxCollectionDefaults(ADocument.Collections) <> FChange.CollectionBaseline then
    begin
      raise ENyxResource.Create('Collections changed; review the resource relationship before applying');
    end;

    if FChange.Operation = reoRows then
    begin
      Exit(NyxResourcePatch([NyxDefineResourceRows(NyxCollection(FChange.CollectionName),
        TNyxResourceRows.FromData(FChange.RowsData), FChange.ReplaceStatic)]).Candidate(ADocument, ACatalog));
    end;
    Exit(NyxResourcePatch([NyxDetachResourceRows(NyxCollection(FChange.CollectionName))])
      .Candidate(ADocument, ACatalog));
  end;

  if FChange.Bind then
  begin
    LOwner := ADocument.Find(FChange.Owner);
    LProjection := Projection(ADocument, LOwner);
    try

      if NyxResourceEditorOwnerBaseline(LOwner, LProjection) <> FChange.OwnerBaseline then
      begin
        raise ENyxResource.Create('The selected control binding changed; review before applying');
      end;

      if not (FChange.Binding.ValueKind in NyxBindingKinds(LProjection, FChange.Binding.Target)) then
      begin
        raise ENyxResource.Create('This resource value is unsupported on the selected property');
      end;
    finally
      LProjection.Free;
    end;
  end;
  SetLength(LChanges, 1);
  case FChange.Operation of
    reoDefine:
      LChanges[0] := NyxDefineResource(FChange.Selection.Reference, FChange.Selection.Locale,
        NyxResourceFromData(FChange.DefinitionData));
    reoRemove:
      LChanges[0] := NyxRemoveResource(FChange.Selection.Reference, FChange.Selection.Locale);
    reoRows, reoDetachRows:
      begin
        raise ENyxResource.Create('Saved row proposal must use its guarded collection branch');
      end;
  end;

  if FChange.Bind then
  begin
    SetLength(LChanges, 2);
    LChanges[1] := NyxBindResource(NyxControl(FChange.Owner), FChange.Binding.Target,
      FChange.Binding.ResourceValue);
  end;
  { Editor exact catalog/control guards stay above; both entry points now share
    the same final-consumer admission, ownership and source candidate contract. }
  LCandidate := NyxResourcePatch(LChanges).Candidate(ADocument, ACatalog);
  Result := LCandidate;
end;

function NewNyxStudioResourcePatch(
  const AChange: TNyxResourceEditorChange): INyxDesignPatch;
begin
  Result := TResourcePatch.Create(AChange);
end;

end.
