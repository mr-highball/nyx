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

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.codec, nyx.catalog,
  nyx.resources, nyx.schema, nyx.binding, nyx.composition;

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
begin

  if (ADocument = nil) or
    (ADocument.Resources.ToData.ToJSON <> FChange.CatalogBaseline) then
  begin
    raise ENyxResource.Create('Resources changed; review the current files before applying');
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
  LCandidate := TNyxCodec.Decode(TNyxCodec.Encode(ADocument));
  try
    case FChange.Operation of
      reoDefine:
        begin
          LCandidate.Resources.Define(FChange.Selection.Reference, FChange.Selection.Locale,
            NyxResourceFromData(FChange.DefinitionData));
        end;
      reoRemove:
        begin
          LCandidate.Resources.Remove(FChange.Selection.Reference, FChange.Selection.Locale);
        end;
    end;

    if FChange.Bind then
    begin
      LCandidate.Find(FChange.Owner).SetBinding(FChange.Binding);
    end;
    { Recheck every retained consumer, including reusable defaults, before the
      session sees the candidate. Missing/wrong-kind paths block replacement or
      removal atomically; the source processor still owns reconciliation/history. }
    LCandidate.Validate;
    ValidateNyxDocumentProperties(LCandidate);
    Result := LCandidate;
    LCandidate := nil;
  finally
    LCandidate.Free;
  end;
end;

function NewNyxStudioResourcePatch(
  const AChange: TNyxResourceEditorChange): INyxDesignPatch;
begin
  Result := TResourcePatch.Create(AChange);
end;

end.
