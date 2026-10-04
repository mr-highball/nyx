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

program nyx_catalog_reference;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  nyx.types,
  Classes,
  SysUtils,
  nyx.text,
  nyx.model,
  nyx.contract,
  nyx.state,
  nyx.schema,
  nyx.catalog,
  nyx.catalog.labels;

{ Generate the default catalog reference from the exact public API Studio uses.
  No renderer/toolchain profiles or private paths enter this document. Output is
  an explicitly named UTF-8 artifact; orchestration may copy an accepted result
  into docs. Keeping the logic in Pascal avoids a second metadata implementation. }

function Cell(const AValue: TNyxText): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 1 to Length(AValue) do
  begin
    case AValue[LIndex] of
      '|':
        begin
          Result := Result + '\|';
        end;
      #10:
        begin
          Result := Result + '<br>';
        end;
      #13:
        begin
          { LF already represents a line break in the portable defaults. }
        end;
      '`':
        begin
          Result := Result + '&#96;';
        end;
      else
        begin
          { Copy preserves native UTF-8 bytes; appending an ANSI Char here would
            corrupt a future extension caption containing supplementary text. }
          Result := Result + Copy(AValue, LIndex, 1);
        end;
    end;
  end;
end;

function TypeName(AType: TNyxPropertyType): TNyxText;
begin
  case AType of
    npText:
      begin
        Result := 'Text';
      end;
    npLines:
      begin
        Result := 'Lines';
      end;
    npBoolean:
      begin
        Result := 'Boolean';
      end;
    npInteger:
      begin
        Result := 'Integer';
      end;
    npNumber:
      begin
        Result := 'Number';
      end;
    npChoice:
      begin
        Result := 'Choice';
      end;
    npReference:
      begin
        Result := 'Reference';
      end;
  end;
end;

procedure AddParts(ALines: TNyxStrings; ANode: TNyxNode; const APath: TNyxText);
var
  LIndex: Integer;
  LChild: TNyxNode;
  LPath: TNyxText;
begin
  for LIndex := 0 to ANode.Count - 1 do
  begin
    LChild := ANode.Children[LIndex];

    if LChild.Prop('part') <> '' then
    begin
      LPath := LChild.Prop('part');

      if APath <> '' then
      begin
        LPath := APath + '/' + LPath;
      end;
      ALines.Add('| ' + Cell(LPath) + ' | ' + Cell(LChild.Kind) + ' | ' +
        Cell(LChild.Prop('text', LChild.Prop('value'))) + ' | ' +
        Cell(LChild.Prop('emit')) + ' |');
      AddParts(ALines, LChild, LPath);
    end;
  end;
end;

procedure AddComponent(ALines: TNyxStrings; ACatalog: TNyxCatalog; AIndex: Integer);
var
  LInfo: TNyxComponentInfo;
  LPrimitive: TNyxPrimitiveInfo;
  LNode: TNyxNode;
  LProperties: TNyxPropertyInfos;
  LEvents: TNyxEventSchemas;
  LIndex: Integer;
  LConstraint: TNyxText;
  LDomain: TNyxValueDomain;
  LField: TNyxFieldContract;
  LRouteIndex: Integer;
  LRoute: TNyxEventRoute;
begin
  LInfo := ACatalog[AIndex];
  LNode := ACatalog.NewNode(LInfo.Kind, 'reference');
  try
    ALines.Add('## ' + LInfo.Kind + TNyxText(' — ') + LInfo.Title);
    ALines.Add('');
    ALines.Add('Family: ' + LInfo.Category + '. Base projection: `' + LNode.ProjectionKind + '`.');
    ALines.Add('');
    ALines.Add(LInfo.Discovery.Description);
    ALines.Add('');
    ALines.Add('Palette group: ' + NyxPaletteGroupName(LInfo.Discovery.Group) + '.');

    if LInfo.Discovery.Labels <> [] then
    begin
      ALines.Add('Search labels: ' + NyxComponentLabelNames(LInfo.Discovery.Labels) + '.');
    end;
    ALines.Add('');

    if FindNyxPrimitive(LNode.ProjectionKind, LPrimitive) then
    begin
      ALines.Add(TNyxText('Root projection — Browser: ') + NyxCapabilityText(LPrimitive.Browser) +
        '. LCL: ' + NyxCapabilityText(LPrimitive.Native) + '.');
      ALines.Add('');
    end;
    ALines.Add('| Property | Type | Factory default | Constraint | Meaning | Browser | LCL | Effect/help |');
    ALines.Add('| --- | --- | --- | --- | --- | --- | --- | --- |');
    LProperties := NyxProperties(LNode);
    for LIndex := 0 to Length(LProperties) - 1 do
    begin
      LConstraint := LProperties[LIndex].Choices;

      if LProperties[LIndex].ValueType = npInteger then
      begin
        LConstraint := IntToStr(LProperties[LIndex].Minimum) + '..' +
          IntToStr(LProperties[LIndex].Maximum);
      end;
      ALines.Add('| ' + Cell(LProperties[LIndex].Key) + ' | ' +
        TypeName(LProperties[LIndex].ValueType) + ' | ' +
        Cell(LNode.Prop(LProperties[LIndex].Key, LProperties[LIndex].DefaultValue)) +
        ' | ' + Cell(LConstraint) + ' | ' +
        NyxPropertyMeaningText(LProperties[LIndex].Support.Meaning) + ' | ' +
        NyxCapabilityText(LProperties[LIndex].Support.Browser) + ' | ' +
        NyxCapabilityText(LProperties[LIndex].Support.Native) + ' | ' +
        Cell(LProperties[LIndex].Support.Description) + ' |');
    end;
    ALines.Add('');
    ALines.Add('| Event | Browser | LCL | Meaning |');
    ALines.Add('| --- | --- | --- | --- |');
    LEvents := NyxEventsMetadata(LNode);
    for LIndex := 0 to High(LEvents) do
    begin
      ALines.Add('| ' + Cell(LEvents[LIndex].Title) + ' | ' +
        NyxCapabilityText(LEvents[LIndex].Browser) + ' | ' +
        NyxCapabilityText(LEvents[LIndex].Native) + ' | ' +
        Cell(LEvents[LIndex].Description) + ' |');
    end;
    ALines.Add('');

    for LIndex := 0 to High(LEvents) do
    begin

      if Length(LEvents[LIndex].Routes) = 0 then
      begin
        Continue;
      end;
      ALines.Add('Semantic action `' + Cell(LEvents[LIndex].Name.Name) + '`:');
      ALines.Add('');
      ALines.Add('| Source control | Trigger | Value control | Payload | Optional |');
      ALines.Add('| --- | --- | --- | --- | --- |');
      for LRouteIndex := 0 to High(LEvents[LIndex].Routes) do
      begin
        LRoute := LEvents[LIndex].Routes[LRouteIndex];
        ALines.Add('| ' + Cell(LRoute.OriginID) + ' | ' + NyxTriggerName(LRoute.Trigger) +
          ' | ' + Cell(LRoute.ValueID) + ' | ' + Cell(LRoute.Payload.Description) +
          ' | ' + BoolToStr(LRoute.PayloadOptional, True) + ' |');
      end;
      ALines.Add('');
    end;

    if LNode.Contract.FindValue(LDomain) then
    begin
      LConstraint := 'No self value';

      if LDomain.Defined then
      begin
        LConstraint := LDomain.ToData.ToJSON;
      end;
      ALines.Add('Self value contract: ' + Cell(LConstraint) + '.');
      ALines.Add('');
    end;

    if LNode.Contract.FieldCount > 0 then
    begin
      ALines.Add('| Typed field path | Scalar domain |');
      ALines.Add('| --- | --- |');
      for LIndex := 0 to LNode.Contract.FieldCount - 1 do
      begin
        LField := LNode.Contract.FieldAt(LIndex);
        ALines.Add('| ' + Cell(LField.Part.Name) + ' | ' +
          Cell(LField.Domain.ToData.ToJSON) + ' |');
      end;
      ALines.Add('');
    end;

    if LNode.Count > 0 then
    begin
      ALines.Add('| Named part path | Kind | Caption/value | Click event |');
      ALines.Add('| --- | --- | --- | --- |');
      AddParts(ALines, LNode, '');
      ALines.Add('');
    end;
  finally
    LNode.Free;
  end;
end;

var
  LCatalog: TNyxCatalog;
  LLines: TNyxStrings;
  LStream: TFileStream;
  LText: TNyxText;
  LIndex: Integer;
begin

  if ParamCount <> 1 then
  begin
    WriteLn('Usage: nyx_catalog_reference OUTPUT.md');
    Halt(1);
  end;
  LCatalog := TNyxCatalog.Create;
  LLines := TNyxStrings.Create;
  try
    LLines.Add('# Default catalog reference');
    LLines.Add('');
    LLines.Add('[Composition guide](components.md) · [Shared schema](../src/nyx.schema.pas)');
    LLines.Add('');
    LLines.Add('Generated by `tools/nyx_catalog_reference.lpr` from the public catalog and schema.');
    LLines.Add('Empty defaults clear optional fields; unknown extension properties are preserved.');
    LLines.Add('Named paths work with `Part` and reusable `OverridePart`; `.` addresses the root.');
    LLines.Add('');
    LLines.Add('Capabilities describe current projections, not complete production behavior.');
    LLines.Add('Property Meaning separates shared contract metadata from presentation and interaction.');
    LLines.Add('Unavailable means the standard projection does not enact that property; portable data is retained.');
    LLines.Add('Custom requires a supplied implementation. Studio hints and semantic MCP queries use the same support.');
    LLines.Add('Basic support has family gaps; Text fallback uses a text field. Recipe behavior');
    LLines.Add('and broader accessibility/parity limits are recorded in the composition guide.');
    LLines.Add('');
    LLines.Add('Page/definition roots are ordinary nodes. Reusable references own explicit');
    LLines.Add('override descriptors, with definition-provided defaults and independent payloads.');
    LLines.Add('');
    for LIndex := 0 to LCatalog.Count - 1 do
    begin
      AddComponent(LLines, LCatalog, LIndex);
    end;
    LText := LLines.Text;
    LStream := TFileStream.Create(ParamStr(1), fmCreate);
    try

      if Length(LText) > 0 then
      begin
        LStream.WriteBuffer(LText[1], Length(LText));
      end;
    finally
      LStream.Free;
    end;
    WriteLn('Generated reference for ', LCatalog.Count, ' kinds');
  finally
    LLines.Free;
    LCatalog.Free;
  end;
end.
