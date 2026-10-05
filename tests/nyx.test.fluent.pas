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

unit nyx.test.fluent;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxFluentTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.catalog,
  nyx.codec,
  nyx.codegen,
  nyx.composition,
  nyx.studio.session;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL fluent: ' + AMessage);
  end;
  Inc(ACount);
end;

function RunNyxFluentTests: Integer;
var
  LDocument: TNyxDocument;
  LNode: TNyxNode;
  LCopy: TNyxNode;
  LSource: TNyxText;
  LBaseline: TNyxText;
  LCatalog: TNyxCatalog;
  LSession: TNyxStudioSession;
  LRuntime: TNyxNode;
  LDefinition: TNyxNode;
  LKind: TNyxKind;
  LParsedKind: TNyxKind;
  LIndex: Integer;
  LRejected: Boolean;
  LOverride: TNyxNode;
begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  try
    LNode := TNyxNode.Create(nkColumn, 'typed-page');
    LDocument.AddPage(LNode);
    LNode.Configure.Layout(nlColumn).Gap(10).Surface(True).Padding(20)
      .ProjectAs(nkColumn).Compound(True).Done;
    LNode.Add(TNyxNode.Create(nkMemo, 'reply').Configure
      .Text('Reply / 🌙').Value('Draft').ReadOnly(False).PartName(NyxPart('reply')).Done);
    LNode.Add(TNyxNode.Create(nkButton, 'send').Configure
      .Text('Send').Variant(nvPrimary).OnClick(NyxEvent('reply.submitted')).Done);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('LReplyMemo', LSource) > 0) and (Pos('LSendButton', LSource) > 0) and
      (Pos('LNode', LSource) = 0), 'locals describe authored purpose and control type', Result);
    Check((Pos('.Layout(nlColumn)', LSource) > 0) and
      (Pos('.Gap(10)', LSource) > 0) and (Pos('.Surface(True)', LSource) > 0) and
      (Pos('.ProjectAs(nkColumn)', LSource) > 0) and (Pos('.Compound(True)', LSource) > 0),
      'screenshot options generate enum, integer and Boolean arguments', Result);
    Check((Pos('.Configure' + #10, LSource) > 0) and (Pos('.SetProp(', LSource) = 0),
      'configuration is a fluent block without low-level setter statements', Result);
    Check(Pos('.OnClick(NyxEvent(''reply.submitted''))', LSource) > 0,
      'open event names have a distinct typed reference', Result);
    LNode.Insert(0, TNyxNode.Create(nkLabel, 'before'));
    LNode.Insert(0, TNyxNode.Create(nkMemo, 'another-reply'));
    Check(Pos('LReplyMemo :=', TNyxCodegen.Generate(LDocument)) > 0,
      'inserting controls of any kind retains existing meaningful names', Result);
    LCopy := LNode.Clone;
    try
      LCopy.Configure.Gap(30).Done;
      Check((LCopy.Configure <> LNode.Configure) and (LNode.Prop('gap') = '10'),
        'clone configuration belongs to the clone, never the template', Result);
    finally
      LCopy.Free;
    end;
    LRejected := False;
    try
      LNode.Configure.Gap(-1);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LNode.Prop('gap') = '10'),
      'out-of-bounds typed edits preserve the accepted field', Result);
    LRejected := False;
    try
      LNode.Configure.Extension('gap', 'wrong');
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LNode.Prop('gap') = '10'),
      'extension boundary cannot bypass a known typed property', Result);
    LNode.Configure.Extension('extension.note', '🌙').Done;
    Check(Pos('.Extension(''extension.note'', ''🌙'')', TNyxCodegen.Generate(LDocument)) > 0,
      'explicit extension data survives source generation', Result);

    { Admit complete variable identifiers, including punctuation/case aliases and
      digit-suffix collisions. Arbitrary custom kinds stay outside the built-ins. }
    for LIndex := 1 to 11 do
    begin
      LNode.Add(TNyxNode.Create('foo', 'foo-' + IntToStr(LIndex)));
    end;
    LNode.Add(TNyxNode.Create('foo1', 'digit-kind'));
    LNode.Add(TNyxNode.Create('foo-bar', 'alias-name'));
    LNode.Add(TNyxNode.Create('foo_bar', 'alias_name'));
    LNode.Add(TNyxNode.Create('FooBar', 'AliasName'));
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('LFoo11 :=', LSource) > 0) and (Pos('LDigitKindFoo1 :=', LSource) > 0),
      'type-bearing identities avoid repetition and retain numeric kinds', Result);
    Check((Pos('LAliasNameFooBar :=', LSource) > 0) and
      (Pos('LAliasNameFooBar2 :=', LSource) > 0) and
      (Pos('LAliasNameFooBar3 :=', LSource) > 0),
      'case/punctuation aliases receive unique purposeful variables', Result);
    Check(LSource = TNyxCodegen.Generate(LDocument), 'crafted source remains deterministic', Result);
  finally
    LDocument.Free;
  end;
  LCatalog := TNyxCatalog.Create;
  try
    LNode := LCatalog.NewNode(nkListCard, 'activity-template');
    try
      LNode.Part(NyxPart('title')).Configure.Text('Crafted activity').Done;
      LCatalog.RegisterRecipe(NyxCustomKind('activity-card'), 'Activity', 'Custom', LNode);
      LNode.Part(NyxPart('title')).Configure.Text('Changed construction tree').Done;
      LCopy := LCatalog.NewNode(NyxCustomKind('activity-card'), 'activity');
      try
        Check(LCopy.Part(NyxPart('title')).Prop('text') = 'Crafted activity',
          'typed recipe registration owns its template independently', Result);
      finally
        LCopy.Free;
      end;
    finally
      LNode.Free;
    end;
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin
      Check(TryNyxKind(NyxKindName(LKind), LParsedKind) and (LParsedKind = LKind),
        'built-in kind codec is exact', Result);

      if LKind <> nkSlotOverride then
      begin
        LNode := LCatalog.NewNode(LKind, 'typed-instance');
        LNode.Free;
      end;
    end;
  finally
    LCatalog.Free;
  end;
  LSession := TNyxStudioSession.Create;
  try
    LRuntime := RealizeNyxView(LSession.Document, LSession.ActiveView);
    try
      LRuntime.Find('project-description').Configure.Value('Edited memo / 🌙').Done;
      LSession.SetCanvasValue(LRuntime.Find('project-description'));
    finally
      LRuntime.Free;
    end;
    Check((LSession.Document.Find('project-description').Prop('value') = TNyxText('Edited memo / 🌙')) and
      (Pos('Edited memo / 🌙', LSession.Source) > 0),
      'canvas value commits through persistence and generated source', Result);
    LSession.Undo;
    Check(LSession.Document.Find('project-description').Prop('value') = '',
      'canvas value is undoable', Result);
    LSession.Redo;
    Check(LSession.Document.Find('project-description').Prop('value') = TNyxText('Edited memo / 🌙'),
      'canvas value is redoable', Result);

    { Editing an inherited memo produces one instance descriptor, never a change
      to the shared definition or its sibling. Undo restores the exact snapshot. }
    LDefinition := TNyxNode.Create(nkColumn, 'reply-template')
      .Add(TNyxNode.Create(nkMemo, 'template-memo').Configure
        .PartName(NyxPart('reply')).Value('Template draft').Done);
    LSession.Document.AddComponent(LDefinition);
    LSession.ActiveView.Add(TNyxNode.Create(nkComponent, 'reply-a').Configure
      .Component(NyxComponent('reply-template')).Done);
    LSession.ActiveView.Add(TNyxNode.Create(nkComponent, 'reply-b').Configure
      .Component(NyxComponent('reply-template')).Done);
    LOverride := LSession.Document.Find('reply-a').OverridePart(NyxPart('.'), noAppend);
    Check((LSession.Document.Find('reply-a').OverridePart(NyxPart('.')) = LOverride) and
      (LOverride.Prop('mode') = 'append'), 'typed omitted mode preserves existing operation', Result);
    LSession.Document.Find('reply-a').Remove(LOverride);
    LBaseline := LSession.Save;
    LRuntime := RealizeNyxView(LSession.Document, LSession.Document.Find('reply-a'));
    try
      LRuntime.Part('reply').Configure.Value('Instance draft / 🌙').Done;
      LSession.SetCanvasValue(LRuntime.Part('reply'));
    finally
      LRuntime.Free;
    end;
    LRuntime := RealizeNyxView(LSession.Document, LSession.Document.Find('reply-a'));
    try
      Check(LRuntime.Part('reply').Prop('value') = TNyxText('Instance draft / 🌙'),
        'reusable canvas field persists an independent named-part override', Result);
    finally
      LRuntime.Free;
    end;
    LRuntime := RealizeNyxView(LSession.Document, LSession.Document.Find('reply-b'));
    try
      Check((LRuntime.Part('reply').Prop('value') = 'Template draft') and
        (LDefinition.Part('reply').Prop('value') = 'Template draft'),
        'editing one reusable field preserves its template and sibling', Result);
    finally
      LRuntime.Free;
    end;
    LSession.Undo;
    Check(LSession.Save = LBaseline, 'undo removes only the canvas customization command', Result);
  finally
    LSession.Free;
  end;
end;

end.
