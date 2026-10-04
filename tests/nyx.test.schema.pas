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

unit nyx.test.schema;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.types,
  SysUtils,
  nyx.text,
  nyx.model,
  nyx.schema,
  nyx.catalog,
  nyx.codec,
  nyx.codegen,
  nyx.composition,
  nyx.behavior;

function RunNyxSchemaTests: Integer;

implementation

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL schema: ' + AMessage);
  end;
  Inc(ACount);
end;

procedure RejectProperty(ANode: TNyxNode; const AKey, AValue: TNyxText;
  var ACount: Integer);
var
  LCandidate: TNyxNode;
  LRejected: Boolean;
begin
  { Use independent candidates so rejection tests cannot taint the admitted
    fixture. The diagnostic must identify the bad property and source identity. }
  LCandidate := ANode.Clone;
  try
    LCandidate.SetProp(AKey, AValue);
    LRejected := False;
    try
      ValidateNyxProperties(LCandidate);
    except
      on LException: ENyxModel do
      begin
        LRejected := (Pos(AKey, LException.Message) > 0) and
          (Pos(ANode.ID, LException.Message) > 0);
      end;
    end;
    Check(LRejected, 'reject ' + AKey + '=' + AValue, ACount);
  finally
    LCandidate.Free;
  end;
end;

function RunNyxSchemaTests: Integer;
var
  LCatalog: TNyxCatalog;
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LTemplate: TNyxNode;
  LButton: TNyxNode;
  LOther: TNyxNode;
  LInstance: TNyxNode;
  LRealized: TNyxNode;
  LInfo: TNyxPrimitiveInfo;
  LProperties: TNyxPropertyInfos;
  LFound: Boolean;
  LRejected: Boolean;
  LIndex: Integer;
  LCount: Integer;
begin
  Result := 0;
  LCatalog := TNyxCatalog.Create;
  LDocument := TNyxDocument.Create;
  try
    LInfo := NyxPrimitiveInfo(0);
    LInfo.Title := 'Caller-owned metadata';
    Check(NyxPrimitiveInfo(0).Title = 'Page', 'metadata record isolation', Result);
    FindNyxPrimitive('date', LInfo);
    Check((LInfo.Browser = ncAvailable) and (LInfo.Native = ncText),
      'picker capability exposes the native text fallback', Result);
    LTemplate := TNyxNode.Create('row', 'layout-override');
    try
      Check(NyxLayout(LTemplate) = 'row', 'raw row has its declared natural layout', Result);
      LTemplate.SetProp('layout', 'column');
      Check(NyxLayout(LTemplate) = 'column', 'explicit layout overrides a primitive default', Result);
    finally
      LTemplate.Free;
    end;
    LTemplate := LCatalog.NewNode('button', 'prototype')
      .SetProp('text', 'Accept / 🌙').SetProp('emit', 'accept');
    try
      LCatalog.RegisterRecipe('accept-button', 'Accept', 'Custom', LTemplate);
    finally
      LTemplate.Free;
    end;
    LTemplate := LCatalog.NewNode('accept-button', 'derived-prototype');
    try
      LCatalog.RegisterRecipe('compact-accept', 'Compact accept', 'Custom', LTemplate);
    finally
      LTemplate.Free;
    end;
    LButton := LCatalog.NewNode('compact-accept', 'custom-🌙漢字');
    LDocument.AddPage(LButton);
    LOther := LCatalog.NewNode('compact-accept', 'other-button');
    LDocument.AddPage(LOther);
    Check((LButton.Kind = 'compact-accept') and (LButton.ProjectionKind = 'button') and
      not LCatalog[LCatalog.IndexOf('compact-accept')].Container,
      'successive primitive derivation keeps base behavior and child admission', Result);
    LButton.SetProp('text', 'Customized').SetProp('variant', 'my-theme')
      .SetProp('extension.owner', '🌙 漢字');
    ValidateNyxProperties(LButton);
    Check(LOther.Prop('text') = TNyxText('Accept / 🌙'), 'derived leaf instance isolation', Result);
    Check(DispatchNyxBehavior(LButton, ntClick).EventName = 'accept',
      'derived primitive keeps semantic event', Result);
    LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    try
      Check((LDecoded.Pages[0].ProjectionKind = 'button') and
        (LDecoded.Pages[0].Prop('extension.owner') = TNyxText('🌙 漢字')),
        'base projection and unknown extension properties survive persistence', Result);
      Check(Pos('.ProjectAs(nkButton)', TNyxCodegen.Generate(LDecoded)) > 0,
        'generated Pascal carries the base contract without registry handles', Result);
    finally
      LDecoded.Free;
    end;
    RejectProperty(LButton, 'width', '100001', Result);
    RejectProperty(LButton, 'height', '-1', Result);
    RejectProperty(LButton, 'width', '$10', Result);
    RejectProperty(LButton, 'enabled', 'yes', Result);
    LProperties := NyxProperties(LButton);
    LFound := False;
    for LIndex := 0 to Length(LProperties) - 1 do
    begin

      if LProperties[LIndex].Key = 'enabled' then
      begin
        LFound := LProperties[LIndex].ValueType = npBoolean;
      end;
    end;
    Check(LFound, 'derived metadata exposes typed primitive fields', Result);
    LTemplate := LCatalog.NewNode('spin', 'numeric-definition')
      .SetProp('min', '10').SetProp('max', '20').SetProp('value', '12');
    LDocument.AddComponent(LTemplate);
    LInstance := TNyxNode.Create('component', 'numeric-instance')
      .SetProp('component', 'numeric-definition');
    LDocument.AddPage(LInstance);
    LProperties := NyxProperties(LInstance, LDocument);
    LFound := False;
    for LIndex := 0 to Length(LProperties) - 1 do
    begin

      if LProperties[LIndex].Key = 'value' then
      begin
        LFound := (LProperties[LIndex].ValueType = npInteger) and
          (LProperties[LIndex].DefaultValue = '12');
      end;
    end;
    Check(LFound, 'reusable inspector inherits definition types and defaults', Result);
    LInstance.SetProp('max', '5');
    LRejected := False;
    try
      ValidateNyxDocumentProperties(LDocument);
    except
      on LException: ENyxModel do
      begin
        LRejected := Pos('numeric-instance', LException.Message) > 0;
      end;
    end;
    Check(LRejected, 'instance range admission includes inherited minimum', Result);
    LInstance.SetProp('max', '25');
    LTemplate := LCatalog.NewNode('component', 'reference-template')
      .SetProp('component', 'numeric-definition');
    try
      LCatalog.RegisterRecipe('numeric-reference', 'Reusable numeric', 'Custom', LTemplate);
    finally
      LTemplate.Free;
    end;
    LOther := LCatalog.NewNode('numeric-reference', 'derived-reference').SetProp('value', '17');
    LDocument.AddPage(LOther);
    ValidateNyxDocumentProperties(LDocument);
    LRealized := RealizeNyxView(LDocument, LOther);
    try
      Check((LRealized.Kind = 'numeric-reference') and (LRealized.ProjectionKind = 'spin') and
        (LRealized.Prop('value') = '17'),
        'derived reusable reference expands and preserves definition projection', Result);
    finally
      LRealized.Free;
    end;
    LTemplate := LCatalog.NewNode('button', 'invalid-template')
      .Add(TNyxNode.Create('label', 'invalid-child'));
    try
      LCount := LCatalog.Count;
      LRejected := False;
      try
        LCatalog.RegisterRecipe('invalid-host', 'Invalid', 'Custom', LTemplate);
      except
        on LException: ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LCatalog.Count = LCount),
        'failed leaf-host registration leaves registry accepted', Result);
    finally
      LTemplate.Free;
    end;
    LInstance.Add(TNyxNode.Create('label', 'ignored-child'));
    LRejected := False;
    try
      LDocument.Validate;
    except
      on LException: ENyxModel do
      begin
        LRejected := Pos('numeric-instance', LException.Message) > 0;
      end;
    end;
    Check(LRejected, 'reusable instances cannot silently discard owned children', Result);
  finally
    LDocument.Free;
    LCatalog.Free;
  end;
end;

end.
