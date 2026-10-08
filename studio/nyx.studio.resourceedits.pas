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


unit nyx.studio.resourceedits;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.types, nyx.model, nyx.catalog, nyx.resources,
  nyx.resources.rows, nyx.collections, nyx.binding.types, nyx.studio.edits;

const
  NyxMaximumResourceChanges = 32;

type
  { A closed candidate operation. Definitions/selectors are copied immutable
    values; no document, renderer, path on disk or transport authority is owned.
    Construct every change through a typed factory below. }
  TNyxResourceChangeKind = (rckDefine, rckRemove, rckBind, rckClear, rckInherit,
    rckRows, rckDetachRows);
  TNyxResourceChange = record
  private
    FDefined: Boolean;
    FKind: TNyxResourceChangeKind;
    FReference: TNyxResourceRef;
    FLocale: TNyxLocaleRef;
    FDefinition: TNyxDataValue;
    FOwner: TNyxControlRef;
    FTarget: TNyxBindingProperty;
    FValue: TNyxResourceValueRef;
    FCollection: TNyxCollectionRef;
    FRows: TNyxResourceRows;
    FReplaceStatic: Boolean;
  end;

  { Candidate-only extension of the existing design contract; its original GUID
    stays unchanged. All changes run on one independent document and its FINAL
    retained consumers must admit. A resource replacement and many selector
    changes can therefore repair each other in one paired Undo checkpoint.
    Permissions/revisions/drafts/publication belong to the calling session. }
  INyxResourcePatch = interface(INyxDesignPatch)
    ['{CE7A8853-BD97-4E24-BD1C-A733C01D7698}']
    function ToData: TNyxDataValue;
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

function NyxDefineResource(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef; const ADefinition: INyxResourceDefinition): TNyxResourceChange;
function NyxRemoveResource(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): TNyxResourceChange;
function NyxBindResource(const AOwner: TNyxControlRef; ATarget: TNyxBindingProperty;
  const AValue: TNyxResourceValueRef): TNyxResourceChange;
{ Clear masks inheritance; inherit removes a local descriptor. These use the
  same property choices as ordinary Studio binding commands. }
function NyxClearResourceBinding(const AOwner: TNyxControlRef;
  ATarget: TNyxBindingProperty): TNyxResourceChange;
function NyxInheritResourceBinding(const AOwner: TNyxControlRef;
  ATarget: TNyxBindingProperty): TNyxResourceChange;
{ Saved relationships share the final resource candidate boundary. A static
  dataset requires explicit conversion consent; existing source recipes may be
  replaced. Detach materializes authored default-locale/fallback rows, retaining
  schema, key and control bindings. It never fetches a URL or copies live edits. }
function NyxDefineResourceRows(const ACollection: TNyxCollectionRef;
  const ARows: TNyxResourceRows; AReplaceStatic: Boolean = False): TNyxResourceChange;
function NyxDetachResourceRows(const ACollection: TNyxCollectionRef): TNyxResourceChange;
function NyxResourcePatch(const AChanges: array of TNyxResourceChange): INyxResourcePatch;
{ Strict MCP/persistence boundary: exact shapes, canonical resource definitions,
  structural selector arrays, and no Boolean/numeric string coercion. }
function ReadNyxResourcePatch(const AChanges: TNyxDataValue): INyxResourcePatch;
function NyxResourceAgentSchema: TNyxDataValue;

implementation

uses nyx.codec, nyx.schema, nyx.bytes, nyx.composition, nyx.binding,
  nyx.collections.registry;

type
  TResourceChanges = class(TInterfacedObject, INyxDesignPatch, INyxResourcePatch)
  private
    FChanges: array of TNyxResourceChange;
  public
    constructor Create(const AChanges: array of TNyxResourceChange);
    function Candidate(ADocument: TNyxDocument; ACatalog: TNyxCatalog): TNyxDocument;
    function ToData: TNyxDataValue;
    function GetCount: Integer;
  end;

function NyxDefineResource(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef; const ADefinition: INyxResourceDefinition): TNyxResourceChange;
begin

  if ADefinition = nil then
  begin
    raise ENyxResource.Create('Construct a resource definition before authoring it');
  end;
  Result := Default(TNyxResourceChange);
  Result.FReference := NyxResourceRef(AReference.Name);
  Result.FLocale := ALocale;
  Result.FDefinition := NyxResourceFromData(ADefinition.ToData).ToData;
  Result.FKind := rckDefine;
  Result.FDefined := True;
end;

function NyxRemoveResource(const AReference: TNyxResourceRef;
  const ALocale: TNyxLocaleRef): TNyxResourceChange;
begin
  Result := Default(TNyxResourceChange);
  Result.FReference := NyxResourceRef(AReference.Name);
  Result.FLocale := ALocale;
  Result.FKind := rckRemove;
  Result.FDefined := True;
end;

function BindingChange(const AOwner: TNyxControlRef; ATarget: TNyxBindingProperty;
  AKind: TNyxResourceChangeKind): TNyxResourceChange;
begin

  if AOwner.ID = '' then
  begin
    raise ENyxResource.Create('An exact authored resource binding owner is required');
  end;
  NyxUTF8ByteCount(AOwner.ID);
  TNyxBindingSpec.Clear(ATarget).Validate;
  Result := Default(TNyxResourceChange);
  Result.FOwner := AOwner;
  Result.FTarget := ATarget;
  Result.FKind := AKind;
  Result.FDefined := True;
end;

function NyxBindResource(const AOwner: TNyxControlRef; ATarget: TNyxBindingProperty;
  const AValue: TNyxResourceValueRef): TNyxResourceChange;
begin
  Result := BindingChange(AOwner, ATarget, rckBind);
  Result.FValue := TNyxResourceValueRef.FromData(AValue.ToData);
  TNyxBindingSpec.Resource(ATarget, Result.FValue).Validate;
end;

function NyxClearResourceBinding(const AOwner: TNyxControlRef;
  ATarget: TNyxBindingProperty): TNyxResourceChange;
begin
  Result := BindingChange(AOwner, ATarget, rckClear);
end;

function NyxInheritResourceBinding(const AOwner: TNyxControlRef;
  ATarget: TNyxBindingProperty): TNyxResourceChange;
begin
  Result := BindingChange(AOwner, ATarget, rckInherit);
end;

function NyxDefineResourceRows(const ACollection: TNyxCollectionRef;
  const ARows: TNyxResourceRows; AReplaceStatic: Boolean): TNyxResourceChange;
begin
  Result := Default(TNyxResourceChange);
  Result.FCollection := NyxCollection(ACollection.Name);
  Result.FRows := TNyxResourceRows.FromData(ARows.ToData);
  Result.FReplaceStatic := AReplaceStatic;
  Result.FKind := rckRows;
  Result.FDefined := True;
end;

function NyxDetachResourceRows(const ACollection: TNyxCollectionRef): TNyxResourceChange;
begin
  Result := Default(TNyxResourceChange);
  Result.FCollection := NyxCollection(ACollection.Name);
  Result.FKind := rckDetachRows;
  Result.FDefined := True;
end;

constructor TResourceChanges.Create(const AChanges: array of TNyxResourceChange);
var
  LIndex: Integer;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > NyxMaximumResourceChanges) then
  begin
    raise ENyxResource.Create('A resource group requires 1..32 changes');
  end;
  SetLength(FChanges, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin

    if not AChanges[LIndex].FDefined then
    begin
      raise ENyxResource.Create('Construct every resource change before applying');
    end;
    FChanges[LIndex] := AChanges[LIndex];

    if AChanges[LIndex].FKind = rckDefine then
    begin
      FChanges[LIndex].FDefinition := AChanges[LIndex].FDefinition.Copy;
    end;

    if AChanges[LIndex].FKind = rckBind then
    begin
      FChanges[LIndex].FValue := AChanges[LIndex].FValue.Copy;
    end;

    if AChanges[LIndex].FKind = rckRows then
    begin
      FChanges[LIndex].FRows := AChanges[LIndex].FRows.Copy;
    end;
  end;
end;

function TResourceChanges.GetCount: Integer;
begin
  Result := Length(FChanges);
end;

function TResourceChanges.Candidate(ADocument: TNyxDocument;
  ACatalog: TNyxCatalog): TNyxDocument;
var
  LCandidate: TNyxDocument;
  LOwner: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LBinding: TNyxBindingSpec;
  LChange: TNyxResourceChange;
  LRows: TNyxResourceRows;
  LIndex: Integer;
begin

  if (ADocument = nil) or (ACatalog = nil) then
  begin
    raise ENyxResource.Create('A resource group requires a document and catalog');
  end;
  LCandidate := TNyxCodec.Decode(TNyxCodec.Encode(ADocument));
  try
    for LIndex := 0 to High(FChanges) do
    begin
      LChange := FChanges[LIndex];
      case LChange.FKind of
        rckDefine:
          LCandidate.Resources.Define(LChange.FReference, LChange.FLocale,
            NyxResourceFromData(LChange.FDefinition));
        rckRemove:
          LCandidate.Resources.Remove(LChange.FReference, LChange.FLocale);
        rckRows:
          begin

            if LCandidate.Collections.Has(LChange.FCollection) and
              not LCandidate.ResourceCollections.HasSource(LChange.FCollection) and
              not LChange.FReplaceStatic then
            begin
              raise ENyxResource.Create('Replacing static rows requires explicit consent');
            end;
            LCandidate.ResourceCollections.Define(LChange.FCollection, LChange.FRows);
          end;
        rckDetachRows:
          begin

            if not NyxCollectionResourceSource(LCandidate.Collections,
              LChange.FCollection, LRows) then
            begin
              raise ENyxResource.Create('Detach requires an existing saved resource relationship');
            end;
            LCandidate.Collections.Define(LRows.Read(LCandidate.Resources,
              LChange.FCollection, NyxDefaultLocale, NyxDefaultLocale));
          end;
        rckBind, rckClear, rckInherit:
          begin
            LOwner := LCandidate.Find(LChange.FOwner.ID);

            if LOwner = nil then
            begin
              raise ENyxResource.Create('The exact authored resource binding owner is missing');
            end;
            case LChange.FKind of
              rckBind:
                LOwner.SetBinding(TNyxBindingSpec.Resource(LChange.FTarget, LChange.FValue));
              rckClear:
                LOwner.SetBinding(TNyxBindingSpec.Clear(LChange.FTarget));
              rckInherit:
                LOwner.RemoveBinding(LChange.FTarget);
            else
              raise ENyxResource.Create('Invalid resource binding operation');
            end;
          end;
      end;
    end;
    { Validate all pages and reusable defaults at the final boundary, including
      scalar families, supported target properties and inherited consumers.
      Failure releases the candidate; the borrowed document is never modified. }
    for LIndex := 0 to High(FChanges) do
    begin
      LChange := FChanges[LIndex];

      if LChange.FKind <> rckBind then
      begin
        Continue;
      end;
      LOwner := LCandidate.Find(LChange.FOwner.ID);
      LContext := RealizeNyxContext(LCandidate, LOwner, LProjection);
      try

        if LProjection = nil then
        begin
          raise ENyxResource.Create('The resource binding has no realized control');
        end;

        if not LProjection.FindBinding(LChange.FTarget, LBinding) then
        begin
          Continue;
        end;

        if not (LBinding.ValueKind in NyxBindingKinds(LProjection, LChange.FTarget)) then
        begin
          raise ENyxResource.Create('The final resource scalar is unsupported on this control property');
        end;
      finally
        LContext.Free;
      end;
    end;
    LCandidate.Validate;
    ValidateNyxDocumentProperties(LCandidate);
    Result := LCandidate;
    LCandidate := nil;
  finally
    LCandidate.Free;
  end;
end;

function TResourceChanges.ToData: TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LChange: TNyxResourceChange;
  LIndex: Integer;
  LOp: TNyxText;
begin
  SetLength(LValues, Length(FChanges));
  for LIndex := 0 to High(FChanges) do
  begin
    LChange := FChanges[LIndex];
    case LChange.FKind of
      rckDefine:
        LValues[LIndex] := NyxObject([NyxField('op', NyxData('define')),
          NyxField('name', NyxData(LChange.FReference.Name)),
          NyxField('locale', NyxData(LChange.FLocale.Name)),
          NyxField('definition', LChange.FDefinition)]);
      rckRemove:
        LValues[LIndex] := NyxObject([NyxField('op', NyxData('remove')),
          NyxField('name', NyxData(LChange.FReference.Name)),
          NyxField('locale', NyxData(LChange.FLocale.Name))]);
      rckRows:
        LValues[LIndex] := NyxObject([NyxField('op', NyxData('define-rows')),
          NyxField('collection', NyxData(LChange.FCollection.Name)),
          NyxField('source', LChange.FRows.ToData),
          NyxField('replaceStatic', NyxData(LChange.FReplaceStatic))]);
      rckDetachRows:
        LValues[LIndex] := NyxObject([NyxField('op', NyxData('detach-rows')),
          NyxField('collection', NyxData(LChange.FCollection.Name))]);
      rckBind:
        LValues[LIndex] := NyxObject([NyxField('op', NyxData('bind')),
          NyxField('owner', NyxData(LChange.FOwner.ID)),
          NyxField('target', NyxData(NyxBindingPropertyName(LChange.FTarget))),
          NyxField('value', LChange.FValue.ToData)]);
      rckClear, rckInherit:
        begin
          LOp := 'clear-binding';

          if LChange.FKind = rckInherit then
          begin
            LOp := 'inherit-binding';
          end;
          LValues[LIndex] := NyxObject([NyxField('op', NyxData(LOp)),
            NyxField('owner', NyxData(LChange.FOwner.ID)),
            NyxField('target', NyxData(NyxBindingPropertyName(LChange.FTarget)))]);
        end;
    end;
  end;
  Result := NyxArray(LValues);
end;

function NyxResourcePatch(const AChanges: array of TNyxResourceChange): INyxResourcePatch;
begin
  Result := TResourceChanges.Create(AChanges);
end;

function ReadNyxResourcePatch(const AChanges: TNyxDataValue): INyxResourcePatch;
var
  LChanges: array of TNyxResourceChange;
  LData: TNyxDataValue;
  LOp: TNyxText;
  LName: TNyxText;
  LLocale: TNyxLocaleRef;
  LTarget: TNyxBindingProperty;
  LIndex: Integer;

  procedure Fields(const ANames: TNyxText; ACount: Integer);
  var
    LField: Integer;
  begin

    if (LData.Kind <> ndObject) or (LData.Count <> ACount) then
    begin
      raise ENyxResource.Create('A resource change requires its exact declared members');
    end;
    for LField := 0 to LData.Count - 1 do
    begin

      if Pos('|' + LData.Key(LField) + '|', ANames) = 0 then
      begin
        raise ENyxResource.Create('Unknown resource change member');
      end;
    end;
  end;

begin

  if (AChanges.Kind <> ndArray) or (AChanges.Count < 1) or
    (AChanges.Count > NyxMaximumResourceChanges) then
  begin
    raise ENyxResource.Create('A resource group requires 1..32 changes');
  end;
  SetLength(LChanges, AChanges.Count);
  for LIndex := 0 to High(LChanges) do
  begin
    LData := AChanges.Item(LIndex);
    LOp := LData.Field('op').AsText;

    if (LOp = 'define') or (LOp = 'remove') then
    begin

      if LOp = 'define' then
      begin
        Fields('|op|name|locale|definition|', 4);
      end
      else
      begin
        Fields('|op|name|locale|', 3);
      end;
      LName := LData.Field('locale').AsText;
      LLocale := NyxDefaultLocale;

      if LName <> '' then
      begin
        LLocale := NyxLocale(LName);
      end;

      if LOp = 'define' then
      begin
        LChanges[LIndex] := NyxDefineResource(NyxResourceRef(LData.Field('name').AsText),
          LLocale, NyxResourceFromData(LData.Field('definition')));
      end
      else
      begin
        LChanges[LIndex] := NyxRemoveResource(NyxResourceRef(LData.Field('name').AsText), LLocale);
      end;
    end
    else if LOp = 'define-rows' then
    begin
      Fields('|op|collection|source|replaceStatic|', 4);
      LChanges[LIndex] := NyxDefineResourceRows(
        NyxCollection(LData.Field('collection').AsText),
        TNyxResourceRows.FromData(LData.Field('source')), LData.Field('replaceStatic').AsBoolean);
    end
    else if LOp = 'detach-rows' then
    begin
      Fields('|op|collection|', 2);
      LChanges[LIndex] := NyxDetachResourceRows(NyxCollection(LData.Field('collection').AsText));
    end
    else if (LOp = 'bind') or (LOp = 'clear-binding') or (LOp = 'inherit-binding') then
    begin

      if LOp = 'bind' then
      begin
        Fields('|op|owner|target|value|', 4);
      end
      else
      begin
        Fields('|op|owner|target|', 3);
      end;

      if not TryNyxBindingProperty(LData.Field('target').AsText, LTarget) then
      begin
        raise ENyxResource.Create('Unknown resource binding property');
      end;
      LName := LData.Field('owner').AsText;

      if LOp = 'bind' then
      begin
        LChanges[LIndex] := NyxBindResource(NyxControl(LName), LTarget,
          TNyxResourceValueRef.FromData(LData.Field('value')));
      end
      else if LOp = 'clear-binding' then
      begin
        LChanges[LIndex] := NyxClearResourceBinding(NyxControl(LName), LTarget);
      end
      else
      begin
        LChanges[LIndex] := NyxInheritResourceBinding(NyxControl(LName), LTarget);
      end;
    end
    else
    begin
      raise ENyxResource.Create('Unknown resource change operation');
    end;
  end;
  Result := NyxResourcePatch(LChanges);
end;

function NyxResourceAgentSchema: TNyxDataValue;
var
  LTarget: TNyxBindingProperty;
  LTargets: array of TNyxDataValue;
  LEmbedded: TNyxDataValue;
  LDefinition: TNyxDataValue;
  LChanges: TNyxDataValue;
  LPath: TNyxDataValue;
  LSelector: TNyxDataValue;
  LRows: TNyxDataValue;
  LName: TNyxDataValue;
  LText: TNyxDataValue;
  LResult: TNyxText;
begin
  SetLength(LTargets, Ord(High(TNyxBindingProperty)) + 1);
  for LTarget := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
  begin
    LTargets[Ord(LTarget)] := NyxData(NyxBindingPropertyName(LTarget));
  end;
  LName := TNyxDataValue.ParseJSON('{"type":"string","minLength":1}');
  LText := TNyxDataValue.ParseJSON('{"type":"string"}');
  LPath := TNyxDataValue.ParseJSON('{"type":"array","maxItems":32,"items":{"oneOf":[{"type":"string"},{"type":"integer","minimum":0,"maximum":2147483647}]}}');
  LEmbedded := TNyxDataValue.ParseJSON('{"type":"object","properties":{"version":{"const":1},"kind":{"enum":["image","json","text","binary"]},"content":{"type":"string","maxLength":1398104},"title":{"type":"string","maxLength":512},"description":{"type":"string","maxLength":4096}},"required":["version","kind","content","title","description"],"additionalProperties":false}');
  LDefinition := TNyxDataValue.ParseJSON('{"oneOf":[' + LEmbedded.ToJSON +
    ',{"type":"object","properties":{"version":{"const":2},"kind":{"enum":["image","json","text","binary"]},"title":{"type":"string","maxLength":512},"description":{"type":"string","maxLength":4096},"source":{"type":"object","properties":{"version":{"const":1},"url":{"type":"string","minLength":1,"maxLength":8192},"cache":{"type":"object","properties":{"version":{"const":1},"mode":{"enum":[0,1,2]},"fresh":{"type":"integer","minimum":0,"maximum":31536000},"stale":{"type":"integer","minimum":0,"maximum":31536000},"bytes":{"type":"integer","minimum":1,"maximum":1048576},"server":{"enum":[0,1]}},"required":["version","mode","fresh","stale","bytes","server"],"additionalProperties":false}},"required":["version","url","cache"],"additionalProperties":false},"fallback":{"oneOf":[{"type":"null"},' +
    LEmbedded.ToJSON + ']}},"required":["version","kind","title","description","source","fallback"],"additionalProperties":false}]}');
  LSelector := TNyxDataValue.ParseJSON('{"type":"object","properties":{"resource":' +
    LName.ToJSON + ',"path":' + LPath.ToJSON +
    ',"locale":{"type":"string"},"fallback":{"type":"string"},"type":{"enum":["text","boolean","integer","number"]}},"required":["resource","path","locale","fallback","type"],"additionalProperties":false}');
  LRows := TNyxDataValue.ParseJSON('{"type":"object","properties":{"version":{"const":1},"resource":' +
    LName.ToJSON + ',"path":' + LPath.ToJSON + ',"identity":' + LPath.ToJSON +
    ',"fields":{"type":"array","minItems":1,"maxItems":64,"items":{"type":"object","properties":{"name":' +
    LName.ToJSON + ',"type":{"enum":["text","boolean","integer","number"]},"path":' + LPath.ToJSON +
    '},"required":["name","type","path"],"additionalProperties":false}}},"required":["version","resource","path","identity","fields"],"additionalProperties":false}');
  LChanges := TNyxDataValue.ParseJSON('{"type":"array","minItems":1,"maxItems":32,"items":{"oneOf":[' +
    '{"type":"object","properties":{"op":{"const":"define"},"name":' + LName.ToJSON +
    ',"locale":' + LText.ToJSON + ',"definition":' + LDefinition.ToJSON +
    '},"required":["op","name","locale","definition"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"remove"},"name":' + LName.ToJSON +
    ',"locale":' + LText.ToJSON + '},"required":["op","name","locale"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"define-rows"},"collection":' + LName.ToJSON +
    ',"source":' + LRows.ToJSON + ',"replaceStatic":{"type":"boolean"}},"required":["op","collection","source","replaceStatic"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"detach-rows"},"collection":' + LName.ToJSON +
    '},"required":["op","collection"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"const":"bind"},"owner":' + LName.ToJSON +
    ',"target":{"enum":' + NyxArray(LTargets).ToJSON + '},"value":' + LSelector.ToJSON +
    '},"required":["op","owner","target","value"],"additionalProperties":false},' +
    '{"type":"object","properties":{"op":{"enum":["clear-binding","inherit-binding"]},"owner":' +
    LName.ToJSON + ',"target":{"enum":' + NyxArray(LTargets).ToJSON +
    '}},"required":["op","owner","target"],"additionalProperties":false}]}}');
  LResult := '{"type":"object","oneOf":[' +
    '{"properties":{"mode":{"const":"list"},"offset":{"type":"integer","minimum":0,"maximum":128},"limit":{"type":"integer","minimum":1,"maximum":16},"filter":{"type":"string"}},"required":["mode"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"details"},"name":' + LName.ToJSON + ',"locale":{"type":"string"},"offset":{"type":"integer","minimum":0,"maximum":1048576},"count":{"type":"integer","minimum":1,"maximum":1024}},"required":["mode","name","locale"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"content"},"name":' + LName.ToJSON + ',"locale":{"type":"string"},"offset":{"type":"integer","minimum":0,"maximum":1048576},"count":{"type":"integer","minimum":1,"maximum":4096}},"required":["mode","name","locale"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"json"},"name":' + LName.ToJSON + ',"locale":{"type":"string"},"path":' + LPath.ToJSON + ',"offset":{"type":"integer","minimum":0,"maximum":1048576},"limit":{"type":"integer","minimum":1,"maximum":16},"textOffset":{"type":"integer","minimum":0,"maximum":1048576},"textCount":{"type":"integer","minimum":1,"maximum":4096}},"required":["mode","name","locale","path"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"bindings"},"owner":' + LName.ToJSON + ',"offset":{"type":"integer","minimum":0,"maximum":19},"limit":{"type":"integer","minimum":1,"maximum":20}},"required":["mode","owner"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"apply"},"expectedRevision":{"type":"integer","minimum":1,"maximum":2147483647},"operationId":{"type":"string","minLength":1,"maxLength":120},"changes":' +
    LChanges.ToJSON + '},"required":["mode","expectedRevision","operationId","changes"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"sources"},"offset":{"type":"integer","minimum":0,"maximum":64},"limit":{"type":"integer","minimum":1,"maximum":16},"filter":{"type":"string"}},"required":["mode"],"additionalProperties":false},' +
    '{"properties":{"mode":{"const":"rows"},"collection":' + LName.ToJSON + ',"offset":{"type":"integer","minimum":0,"maximum":64},"limit":{"type":"integer","minimum":1,"maximum":16}},"required":["mode","collection"],"additionalProperties":false}]}';
  Result := TNyxDataValue.ParseJSON(LResult);
end;

end.
