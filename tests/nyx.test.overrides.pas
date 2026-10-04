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

unit nyx.test.overrides;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxOverrideTests: Integer;

implementation

uses
  nyx.types,
  SysUtils,
  nyx.text,
  nyx.model,
  nyx.catalog,
  nyx.schema,
  nyx.codec,
  nyx.codegen,
  nyx.composition,
  nyx.behavior,
  nyx.studio.session;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('FAIL part overrides: ' + AMessage);
  end;
  Inc(ACount);
end;

procedure Reject(ADocument: TNyxDocument; const AMessage: TNyxText; var ACount: Integer);
var
  LRejected: Boolean;
begin
  LRejected := False;
  try
    ValidateNyxDocumentProperties(ADocument);
  except
    on LException: ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, AMessage, ACount);
end;

function RunNyxOverrideTests: Integer;
var
  LCatalog: TNyxCatalog;
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LDefinition: TNyxNode;
  LInstance: TNyxNode;
  LOther: TNyxNode;
  LDerived: TNyxNode;
  LNested: TNyxNode;
  LRule: TNyxNode;
  LRuntime: TNyxNode;
  LSession: TNyxStudioSession;
  LBaseline: TNyxText;
  LInstanceID: TNyxText;
  LRuleID: TNyxText;
  LRejected: Boolean;
  LProperties: TNyxPropertyInfos;
  LIndex: Integer;
  LFound: Boolean;
begin
  Result := 0;
  LCatalog := TNyxCatalog.Create;
  LDocument := TNyxDocument.Create;
  try
    LDefinition := LCatalog.NewNode('list-card', 'activity-definition');
    LDocument.AddComponent(LDefinition);
    LInstance := TNyxNode.Create('component', 'activity-instance')
      .SetProp('component', LDefinition.ID);
    LDocument.AddPage(LInstance);
    LOther := TNyxNode.Create('component', 'other-instance')
      .SetProp('component', LDefinition.ID);
    LDocument.AddPage(LOther);
    LRule := LInstance.OverridePart('title').Named('custom-title')
      .SetProp('text', 'My activity / 🌙 漢字');
    Check(LInstance.OverridePart('title') = LRule,
      'a path has one independently owned customization', Result);
    LInstance.OverridePart('actions', 'append').Named('custom-actions')
      .Add(LCatalog.NewNode('button', 'floating-add').SetProp('part', 'floating')
        .SetProp('text', '+ Add').SetProp('emit', 'add'));
    Check(LInstance.OverridePart('actions').Prop('mode') = 'append',
      'borrowing an existing override keeps its payload operation', Result);
    ValidateNyxDocumentProperties(LDocument);
    LRuntime := RealizeNyxView(LDocument, LInstance);
    try
      Check(LRuntime.Part('title').Prop('text') = TNyxText('My activity / 🌙 漢字'),
        'custom caption reaches the realized named part', Result);
      Check((LRuntime.Part('actions/floating').Prop('design-id') = 'floating-add') and
        (DispatchNyxBehavior(LRuntime.Part('actions/floating'), ntClick).EventName = 'add'),
        'added payload retains editable identity and semantic behavior', Result);
    finally
      LRuntime.Free;
    end;
    LRuntime := RealizeNyxView(LDocument, LOther);
    try
      Check((LRuntime.Part('title').Prop('text') = LDefinition.Part('title').Prop('text')) and
        (LRuntime.Part('actions').Count = LDefinition.Part('actions').Count),
        'customization leaves the definition and second instance intact', Result);
    finally
      LRuntime.Free;
    end;
    LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    try
      LRuntime := RealizeNyxView(LDecoded, LDecoded.Pages[0]);
      try
        Check(LRuntime.Part('actions/floating').Prop('text') = '+ Add',
          'override payload survives persistence and realization', Result);
      finally
        LRuntime.Free;
      end;
      Check(Pos('NewNyxSlotOverride', TNyxCodegen.Generate(LDecoded)) > 0,
        'generated Pascal contains the portable instance customization', Result);
    finally
      LDecoded.Free;
    end;
    LRule.SetProp('width', '120000');
    Reject(LDocument, 'effective target properties are admitted', Result);
    LRule.SetProp('width', '');
    LRule.SetProp('path', 'missing/part');
    Reject(LDocument, 'missing part paths are diagnosed before a build', Result);
    LRule.SetProp('path', 'title');
    LRule.SetProp('mode', 'append').Add(LCatalog.NewNode('label', 'leaf-payload'));
    Reject(LDocument, 'leaf parts cannot silently consume appended children', Result);
    LRule.Remove(LRule.Children[0]);
    LRule.SetProp('mode', 'properties');
    LRule := LOther.OverridePart('actions', 'prepend').Named('prepended-actions')
      .Add(LCatalog.NewNode('button', 'first-action').SetProp('text', 'First'));
    LOther.OverridePart('title', 'replace').Named('replaced-title')
      .Add(LCatalog.NewNode('input', 'editable-title').SetProp('value', 'Custom title'));
    LOther.OverridePart('list', 'remove').Named('removed-list');
    ValidateNyxDocumentProperties(LDocument);
    LRuntime := RealizeNyxView(LDocument, LOther);
    try
      Check((LRuntime.Part('actions').Children[0].Prop('text') = 'First') and
        (LRuntime.Part('title').ProjectionKind = 'input'),
        'prepend keeps order and replacement keeps the named part contract', Result);
      Check(LRuntime.Find('other-instance/' + LDefinition.Part('list').ID) = nil,
        'removed part has no runtime projection', Result);
    finally
      LRuntime.Free;
    end;
    LInstance.OverridePart('.', 'append').Named('root-content')
      .Add(LCatalog.NewNode('label', 'root-extra').SetProp('text', 'Root content'));
    ValidateNyxDocumentProperties(LDocument);
    LRuntime := RealizeNyxView(LDocument, LInstance);
    try
      Check(LRuntime.Children[LRuntime.Count - 1].Prop('text') = 'Root content',
        'root content is customizable without an artificial wrapper slot', Result);
    finally
      LRuntime.Free;
    end;
    LCatalog.RegisterRecipe('custom-activity', 'Custom activity', 'Custom', LInstance);
    LDerived := LCatalog.NewNode('custom-activity', 'derived-activity');
    LDocument.AddPage(LDerived);
    ValidateNyxDocumentProperties(LDocument);
    LRuntime := RealizeNyxView(LDocument, LDerived);
    try
      Check((LRuntime.Kind = 'custom-activity') and
        (LRuntime.Part('title').Prop('text') = TNyxText('My activity / 🌙 漢字')) and
        (LRuntime.Part('actions/floating').Prop('text') = '+ Add'),
        'a derived reusable recipe preserves independently copied overrides', Result);
    finally
      LRuntime.Free;
    end;
    LNested := TNyxNode.Create('column', 'nested-definition')
      .Add(TNyxNode.Create('component', 'nested-activity')
        .SetProp('part', 'activity').SetProp('component', LDefinition.ID));
    LDocument.AddComponent(LNested);
    LNested := TNyxNode.Create('component', 'nested-instance')
      .SetProp('component', LNested.ID);
    LDocument.AddPage(LNested);
    LNested.OverridePart('activity/title').Named('nested-title-override')
      .SetProp('text', 'Nested customization');
    ValidateNyxDocumentProperties(LDocument);
    LRuntime := RealizeNyxView(LDocument, LNested);
    try
      Check(LRuntime.Part('activity/title').Prop('text') = 'Nested customization',
        'paths reach parts inside an expanded nested reusable instance', Result);
    finally
      LRuntime.Free;
    end;
    LNested.OverridePart('activity/title').SetProp('enabled', 'maybe');
    Reject(LDocument, 'nested target schemas reject invalid booleans', Result);
    LNested.OverridePart('activity/title').SetProp('enabled', 'true');
    LRule.SetProp('part', 'invalid-metadata');
    Reject(LDocument, 'descriptor identity metadata cannot corrupt part addressing', Result);
  finally
    LDocument.Free;
    LCatalog.Free;
  end;
  { Exercise the actual designer command boundary, including redo-preserving
    rejection and palette insertion through a selected layout-part descriptor. }
  LSession := TNyxStudioSession.Create;
  try
    LSession.Select('welcome-instance');
    LInstanceID := LSession.SelectedID;
    LSession.CustomizePart('title');
    LRuleID := LSession.SelectedID;
    LProperties := NyxProperties(LSession.Selected, LSession.Document);
    LFound := False;
    for LIndex := 0 to Length(LProperties) - 1 do
    begin

      if LProperties[LIndex].Key = 'text' then
      begin
        LFound := LProperties[LIndex].DefaultValue = 'Make something wonderful.';
      end;
    end;
    Check(LFound, 'inspector inherits the customized part schema and defaults', Result);
    LSession.SetProperty('text', 'My welcome / 🌙');
    LBaseline := LSession.Save;
    LSession.SetProperty('text', 'Later caption');
    LSession.Undo;
    LSession.Select(LRuleID);
    LRejected := False;
    try
      LSession.SetProperty('path', 'unknown-part');
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBaseline),
      'bad instance customization preserves accepted project and history', Result);
    LSession.Redo;
    LSession.Select(LRuleID);
    Check(LSession.Selected.Prop('text') = 'Later caption',
      'rejection keeps the preceding redo command available', Result);
    LSession.Select(LInstanceID);
    LSession.CustomizePart('.');
    LSession.AddKind('button');
    Check((LSession.Selected.Parent.Kind = 'slot-override') and
      (LSession.Selected.Parent.Prop('mode') = 'append'),
      'palette adds ordinary Nyx content to a selected instance layout part', Result);
    LSession.DeleteSelected;
    Check(LSession.Selected.Prop('mode') = 'properties',
      'deleting the final payload restores a valid property-only customization', Result);
    LSession.AddComponentInstance('welcome-card');
    LInstanceID := LSession.SelectedID;
    Check((LSession.Selected.Parent.Kind = 'slot-override') and
      (LSession.Selected.Prop('component') = 'welcome-card'),
      'reusable views use the same instance-slot insertion as palette content', Result);
    LBaseline := LSession.Save;
    LSession.Undo;
    Check(LSession.Document.Find(LInstanceID) = nil,
      'undo removes nested reusable slot content independently', Result);
    LSession.Redo;
    Check(LSession.Save = LBaseline, 'redo restores nested reusable slot content', Result);
    LSession.Select('welcome-instance');
    LSession.CustomizePart('title');
    LBaseline := LSession.Save;
    LRejected := False;
    try
      LSession.AddKind('button');
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBaseline),
      'failed leaf insertion releases its unadmitted candidate without editing the project', Result);
    LSession.Select('welcome-card');
    LRejected := False;
    try
      LSession.AddComponentInstance('welcome-card');
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBaseline),
      'failed cyclic insertion rolls back and releases its admitted candidate once', Result);
  finally
    LSession.Free;
  end;
end;

end.
