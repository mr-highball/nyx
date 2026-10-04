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

unit nyx.test.core;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  Classes,
  SysUtils,
  nyx.model,
  nyx.codec,
  nyx.codegen,
  nyx.catalog,
  nyx.sample;

function RunNyxCoreTests: Integer;
{ Shared fixture for compiled source reconstruction and service build evidence.
  Include supplementary characters and escaped data, not only ASCII captions. }
function CreateNyxPersistenceFixture: TNyxDocument;

implementation

uses
  nyx.test.identity,
  nyx.types,
  nyx.state,
  nyx.binding.types,
  nyx.test.state,
  nyx.test.contract,
  nyx.test.data;

function CreateNyxPersistenceFixture: TNyxDocument;
begin
  Result := CreateNyxSample;
  Result.Title := 'Café / 🌙 / 漢字';
  Result.Find('project-name').SetProp('value', '🌙 漢字')
    .SetProp('unicode', '🌙 漢字')
    .SetProp('extension.data', '''quotes'' "slashes" \ ' + #9 + #10 + #1);
  Result.Find('welcome-instance').OverridePart('title').Named('fixture-title-override')
    .SetProp('text', 'My welcome / 🌙 漢字');
  Result.Find('welcome-instance').OverridePart('.', 'append').Named('fixture-content-override')
    .Add(TNyxNode.Create('button', 'fixture-added-action')
      .SetProp('text', 'Added / 🌙').SetProp('emit', 'add'));
  AddNyxIdentityFixture(Result);
  AddNyxStateFixture(Result);
  { Compiled reconstruction also covers typed binding metadata and a deliberate
    unbinding override, without replacing the authored caption/value literals. }
  Result.Find('project-name').Binds.Value(NyxTextState('empty'))
    .Enabled(NyxBooleanState('enabled')).Done;
  Result.Find('project-name').Configure.OnChange(NyxEvent('project/name/changed/🌙')).Done;
  Result.Find('fixture-title-override').Binds.Text(NyxNumberState('ratio'))
    .Clear(bpHint).Done;
  AddNyxDataFixture(Result);
  AddNyxContractFixture(Result);
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
    raise Exception.Create('FAIL: ' + AMessage);
  Inc(ACount);
end;

procedure RejectJSON(const ASource: TNyxText; var ACount: Integer);
var
  LDocument: TNyxDocument;
  LRejected: Boolean;
begin
  LRejected := False;
  LDocument := nil;
  try
    try
      LDocument := TNyxCodec.Decode(ASource);
    except
      on LException: Exception do
        LRejected := True;
    end;
  finally
    LDocument.Free;
  end;
  Check(LRejected, 'reject invalid design', ACount);
end;

function RunNyxCoreTests: Integer;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LDecoded: TNyxDocument;
  LParent: TNyxNode;
  LChild: TNyxNode;
  LDetached: TNyxNode;
  LCatalog: TNyxCatalog;
  LSource: TNyxText;
  LJSON: TNyxText;
  LRejected: Boolean;
  LIndex: Integer;
begin
  Result := 0;
  LParent := TNyxNode.Create('column', 'parent');
  try
    LChild := TNyxNode.Create('button', 'child');
    LParent.Add(LChild);
    Check(LChild.Parent = LParent, 'non-owning parent', Result);
    Check(LParent.Find('child') = LChild, 'tree search', Result);
    LRejected := False;
    try
      LChild.Add(LParent);
    except
      on LException: ENyxModel do
        LRejected := True;
    end;
    Check(LRejected, 'reject ownership cycle', Result);
    LRejected := False;
    try
      LParent.Add(LChild);
    except
      on LException: ENyxModel do
        LRejected := True;
    end;
    Check(LRejected, 'reject duplicate ownership', Result);
    LDetached := LParent.Extract(0);
    Check((LParent.Count = 0) and (LDetached.Parent = nil), 'extract detaches', Result);
    LParent.Insert(0, LDetached);
    LParent.Remove(LDetached);
    Check(LParent.Count = 0, 'remove releases owned node', Result);
  finally
    LParent.Free;
  end;
  LDocument := CreateNyxSample;
  try
    LDocument.Find('project-name').SetProp('value', 'Café / Nyx')
      .SetProp('extension.data', '''quotes'' "slashes" \ ' + #9 + #10 + #1);
    LDocument.Title := 'Café / 🌙 / 漢字';
    LDocument.Find('project-name').SetProp('unicode', '🌙 漢字');
    LJSON := TNyxCodec.Encode(LDocument);
    LDecoded := TNyxCodec.Decode(LJSON);
    try
      Check(TNyxCodec.Encode(LDecoded) = LJSON, 'deterministic JSON round trip', Result);
      Check(LDecoded.Find('project-name').Prop('extension.data') =
        LDocument.Find('project-name').Prop('extension.data'), 'extension/control chars survive', Result);
      Check(LDecoded.ComponentCount = 1, 'reusable definitions survive', Result);
      Check(LDecoded.Title = TNyxText('Café / 🌙 / 漢字'),
        'supplementary Unicode title survives', Result);
      Check(LDecoded.Find('project-name').Prop('unicode') = TNyxText('🌙 漢字'),
        'supplementary Unicode property survives', Result);
    finally
      LDecoded.Free;
    end;
    LCopy := LDocument.Clone;
    try
      LCopy.Find('project-name').SetProp('value', 'Independent');
      Check(LDocument.Find('project-name').Prop('value') = TNyxText('Café / Nyx'),
        'clone mutation preserves baseline', Result);
      LCopy.AddPage(TNyxNode.Create('page', 'second-page'));
      Check((LCopy.Count = 2) and (LDocument.Count = 1), 'multi-page clone independence', Result);
    finally
      LCopy.Free;
    end;
    LSource := TNyxCodegen.Generate(LDocument);
    Check(LSource = TNyxCodegen.Generate(LDocument), 'stable Pascal generation', Result);
    Check(Pos('BuildNyxDocument', LSource) > 0, 'generated entry point', Result);
    Check(Pos('#9', LSource) > 0, 'Pascal control characters escaped', Result);
    Check(Pos('🌙', LSource) > 0, 'Unicode survives Pascal generation', Result);
    LRejected := False;
    try
      TNyxCodegen.Generate(LDocument, 'bad; begin');
    except
      on LException: ENyxModel do
        LRejected := True;
    end;
    Check(LRejected, 'reject source injection in unit name', Result);
    LDocument.AddPage(TNyxNode.Create('page', 'home'));
    LRejected := False;
    try
      LDocument.Validate;
    except
      on LException: ENyxModel do
        LRejected := True;
    end;
    Check(LRejected, 'reject duplicate document identity', Result);
  finally
    LDocument.Free;
  end;
  LCatalog := TNyxCatalog.Create;
  try
    Check(LCatalog.Count >= 30, 'initial catalog breadth', Result);
    LCatalog.RegisterKind('custom-list', 'My list', 'Custom', True);
    LParent := LCatalog.NewNode('custom-list', 'custom');
    try
      Check(LParent.Kind = 'custom-list', 'custom kind registration', Result);
    finally
      LParent.Free;
    end;
    for LIndex := 0 to LCatalog.Count - 1 do
    begin
      LParent := LCatalog.NewNode(LCatalog[LIndex].Kind, 'catalog-' + IntToStr(LIndex));
      LParent.Free;
    end;
    Check(True, 'all catalog factories construct', Result);
  finally
    LCatalog.Free;
  end;
  RejectJSON('null', Result);
  RejectJSON('{"version":2,"title":"","pages":[],"components":[]}', Result);
  RejectJSON('{"version":1.0000000000000000001,"title":"","pages":[],"components":[]}', Result);
  RejectJSON('{"version":1,"title":"","pages":[],"components":[],"state":{"x":{"type":"integer","value":1.0000000000000000001}}}', Result);
  RejectJSON('{"version":1,"title":"","pages":[false],"components":[]}', Result);
  RejectJSON('{"version":1,"title":"","pages":[{"kind":"page","id":"p","props":{"x":3},"children":[]}],"components":[]}', Result);
  RejectJSON('{"version":1,"title":"","pages":[{"kind":"component","id":"p","props":{"component":"missing"},"children":[]}],"components":[]}', Result);
  LDocument := TNyxDocument.Create;
  try
    LDocument.AddComponent(TNyxNode.Create('column', 'recursive')
      .Add(TNyxNode.Create('component', 'loop').SetProp('component', 'recursive')));
    LRejected := False;
    try
      LDocument.Validate;
    except
      on LException: ENyxModel do
        LRejected := True;
    end;
    Check(LRejected, 'reject recursive components', Result);
  finally
    LDocument.Free;
  end;
end;

end.
