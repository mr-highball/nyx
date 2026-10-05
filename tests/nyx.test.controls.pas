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

unit nyx.test.controls;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model,
  nyx.controls;

function RunNyxControlTests: Integer;
{ Shared actual-control fixture. The badge uses an unrelated implementation
  class; returned interfaces retain editable descriptors beyond document life. }
function CreateNyxManagedFixture(out ABadge: INyxBadge;
  out AMemo: INyxMemo): TNyxDocument;
{ Recover an accepted raw-node companion, edit it, then visually regenerate
  specialized source while retaining its namespace/imports/handwritten helper. }
function CreateNyxLegacyControlFixture(out ASource: TNyxText): TNyxDocument;

implementation

uses
  SysUtils,
  nyx.types,
  nyx.state,
  nyx.contract,
  nyx.data,
  nyx.codec,
  nyx.codegen,
  nyx.source;

type
  { Destruction counters observe actual descriptor/implementation lifetime,
    rather than assuming garbage collection or a successful compile proves it. }
  TTrackedNode = class(TNyxNode)
  public
    destructor Destroy; override;
  end;
  TTrackedBadge = class(TNyxBadge)
  public
    destructor Destroy; override;
  end;

  { An unrelated implementation base implements the specialized public contract.
    Its identity is this object, while adapters consume its retained descriptor.
    No renderer, document or generator performs a concrete-control class cast. }
  TBadgeDecorator = class(TInterfacedObject, INyxBadge, INyxCaptionControl, INyxControl, INyxNode)
  private
    FInner: INyxBadge;
  public
    constructor Create(const AInner: INyxBadge);
    function GetNode: TNyxNode;
    function GetID: TNyxText;
    function GetKind: TNyxText;
    function GetCount: Integer;
    function GetChild(AIndex: Integer): INyxControl;
    function GetConfigure: INyxConfiguration;
    function GetBinds: INyxBindings;
    function GetContract: INyxControlContract;
    function GetExtensions: INyxControlExtensions;
    function Named(const AID: TNyxText): INyxControl;
    function Add(const AChild: INyxNode): INyxControl;
    procedure Insert(AIndex: Integer; const AChild: INyxNode);
    function Extract(AIndex: Integer): INyxControl;
    procedure Remove(const AChild: INyxNode);
    function Part(const APath: TNyxPartRef): INyxControl;
    function OverridePart(const APath: TNyxPartRef; AMode: TNyxOverrideMode): INyxControl;
    function Clone: INyxControl;
    function GetText: TNyxText;
    procedure SetText(const AText: TNyxText);
    function WithText(const AText: TNyxText): INyxBadge;
  end;
var
  GNodesDestroyed: Integer;
  GControlsDestroyed: Integer;

destructor TTrackedNode.Destroy;
begin
  Inc(GNodesDestroyed);
  inherited Destroy;
end;

destructor TTrackedBadge.Destroy;
begin
  Inc(GControlsDestroyed);
  inherited Destroy;
end;

constructor TBadgeDecorator.Create(const AInner: INyxBadge);
begin
  inherited Create;
  FInner := AInner;
end;

function TBadgeDecorator.GetNode: TNyxNode;
begin
  Result := FInner.Node;
end;

function TBadgeDecorator.GetID: TNyxText;
begin
  Result := FInner.ID;
end;

function TBadgeDecorator.GetKind: TNyxText;
begin
  Result := FInner.Kind;
end;

function TBadgeDecorator.GetCount: Integer;
begin
  Result := FInner.Count;
end;

function TBadgeDecorator.GetChild(AIndex: Integer): INyxControl;
begin
  Result := FInner.Children[AIndex];
end;

function TBadgeDecorator.GetConfigure: INyxConfiguration;
begin
  Result := TNyxConfiguration.Create(Self as INyxControl);
end;

function TBadgeDecorator.GetBinds: INyxBindings;
begin
  Result := TNyxBindings.Create(Self as INyxControl);
end;

function TBadgeDecorator.GetContract: INyxControlContract;
begin
  Result := TNyxControlContract.Create(Self as INyxControl);
end;

function TBadgeDecorator.GetExtensions: INyxControlExtensions;
begin
  Result := TNyxControlExtensions.Create(Self as INyxControl);
end;

function TBadgeDecorator.Named(const AID: TNyxText): INyxControl;
begin
  FInner.Named(AID);
  Result := Self as INyxControl;
end;

function TBadgeDecorator.Add(const AChild: INyxNode): INyxControl;
begin
  FInner.Add(AChild);
  Result := Self as INyxControl;
end;

procedure TBadgeDecorator.Insert(AIndex: Integer; const AChild: INyxNode);
begin
  FInner.Insert(AIndex, AChild);
end;

function TBadgeDecorator.Extract(AIndex: Integer): INyxControl;
begin
  Result := FInner.Extract(AIndex);
end;

procedure TBadgeDecorator.Remove(const AChild: INyxNode);
begin
  FInner.Remove(AChild);
end;

function TBadgeDecorator.Part(const APath: TNyxPartRef): INyxControl;
begin
  Result := FInner.Part(APath);
end;

function TBadgeDecorator.OverridePart(const APath: TNyxPartRef;
  AMode: TNyxOverrideMode): INyxControl;
begin
  Result := FInner.OverridePart(APath, AMode);
end;

function TBadgeDecorator.Clone: INyxControl;
begin
  Result := FInner.Clone;
end;

function TBadgeDecorator.GetText: TNyxText;
begin
  Result := FInner.Text;
end;

procedure TBadgeDecorator.SetText(const AText: TNyxText);
begin
  FInner.Text := AText;
end;

function TBadgeDecorator.WithText(const AText: TNyxText): INyxBadge;
begin
  SetText(AText);
  Result := Self as INyxBadge;
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('FAIL managed controls: ' + AMessage);
  end;
  Inc(ACount);
end;

function CreateNyxManagedFixture(out ABadge: INyxBadge;
  out AMemo: INyxMemo): TNyxDocument;
var
  LPage: INyxColumn;
  LDiscussion: INyxCommentThread;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxColumn('managed-controls');
    LPage.Configure.Layout(nlColumn).Padding(20).Gap(12).Width(600).Done;
    ABadge := TBadgeDecorator.Create(NewNyxBadge('managed-status').WithText('Ready / 🌙'));
    AMemo := NewNyxMemo('managed-reply').WithText('Write a reply');
    AMemo.Value := 'Specialized editable content / 🌙';
    LDiscussion := NewNyxCommentThread('managed-discussion');
    LDiscussion.ReplyMemo.Value := 'A typed compound reply / 🌙';
    LPage.Add(ABadge).Add(AMemo).Add(LDiscussion);
    Result.AddPage(LPage);
  except
    Result.Free;
    raise;
  end;
end;

function CreateNyxLegacyControlFixture(out ASource: TNyxText): TNyxDocument;
var
  LWorkspace: TNyxSourceWorkspace;
  LPage: TNyxNode;
  LCandidate: TNyxDocument;
begin
  Result := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    try
      Result.Title := 'Legacy application';
      LPage := TNyxNode.Create(nkPage, 'home');
      Result.AddPage(LPage);
      LPage.Add(TNyxNode.Create(nkMemo, 'reply').Configure.Text('Old caption').Value('Old value').Done);
      ASource := TNyxText('unit nyx.legacy.controls;') + #10 +
        '{$mode delphi}{$H+}' + #10 + '{$codepage utf8}' + #10 +
        'interface' + #10 + 'uses nyx.text, nyx.types, nyx.model;' + #10 +
        'function BuildNyxDocument: TNyxDocument;' + #10 +
        'function LegacyNote: TNyxText;' + #10 + 'implementation' + #10 +
        NyxViewsBegin + #10 + 'function BuildNyxDocument: TNyxDocument;' + #10 +
        'var' + #10 + '  LHomePage: TNyxNode;' + #10 + '  LReplyMemo: TNyxNode;' + #10 +
        'begin' + #10 + '  Result := TNyxDocument.Create;' + #10 + '  try' + #10 +
        '    Result.Title := ''Legacy application'';' + #10 +
        '    LHomePage := TNyxNode.Create(nkPage, ''home'');' + #10 +
        '    Result.AddPage(LHomePage);' + #10 +
        '    LReplyMemo := TNyxNode.Create(nkMemo, ''reply'');' + #10 +
        '    LHomePage.Add(LReplyMemo);' + #10 +
        '    LReplyMemo.Configure.Text(''Old caption'').Value(''Old value'').Done;' + #10 +
        '  except' + #10 + '    Result.Free;' + #10 + '    raise;' + #10 +
        '  end;' + #10 + 'end;' + #10 + NyxViewsEnd + #10 +
        '// Handwritten helper retained from the accepted companion.' + #10 +
        'function LegacyNote: TNyxText;' + #10 + 'begin' + #10 +
        TNyxText('  Result := ''A retained legacy helper / 🌙'';') + #10 + 'end;' + #10 + 'end.' + #10;

      if Pos(TNyxText('🌙'), ASource) = 0 then
      begin
        raise ENyxModel.Create('Legacy fixture construction lost its UTF-8 helper');
      end;
      { This frame was accepted by the previous version. Recovery retains its
        exact body; the reader must still admit supported edits to that body. }
      LWorkspace.Accept(Result, ASource);
      LWorkspace.Restore(LWorkspace.Snapshot);
      ASource := StringReplace(ASource, '''Old caption''', '''Recovered caption''', []);

      if Pos(TNyxText('🌙'), ASource) = 0 then
      begin
        raise ENyxModel.Create('Legacy fixture replacement lost its UTF-8 helper');
      end;
      LCandidate := LWorkspace.Candidate(Result, ASource);
      Result.Free;
      Result := LCandidate;
      LWorkspace.Accept(Result, ASource);
      Result.Find('reply').Configure.Value('Visual edit after recovery').Done;
      { New focus signals must survive recovery, visual regeneration and actual
        compilation of the preserved legacy companion on both targets. }
      Result.Find('reply').Contract.Signal(ntAfterEnter).Signal(ntAfterExit);
      ASource := LWorkspace.Render(Result);
    except
      Result.Free;
      raise;
    end;
  finally
    LWorkspace.Free;
  end;
end;

function RunNyxControlTests: Integer;
var
  LDocument: TNyxDocument;
  LRoot: INyxColumn;
  LBadge: INyxBadge;
  LOther: INyxControl;
  LConfig: INyxConfiguration;
  LBindings: INyxBindings;
  LContract: INyxControlContract;
  LExtensions: INyxControlExtensions;
  LNode: TNyxNode;
  LMemo: INyxMemo;
  LCheck: INyxCheckbox;
  LSlider: INyxSlider;
  LImage: INyxImage;
  LChoice: INyxSelect;
  LReference: INyxComponent;
  LThread: INyxCommentThread;
  LThread2: INyxCommentThread;
  LKind: TNyxKind;
  LSource: TNyxText;
  LRejected: Boolean;
  LWorkspace: TNyxSourceWorkspace;
  LCandidate: TNyxDocument;

  procedure ObservedConfiguration(out AConfig: INyxConfiguration);
  var
    LOwnedNode: TNyxNode;
    LOwnedBadge: INyxBadge;
  begin
    { Compiler-generated interface temporaries can live until routine exit.
      This helper gives those temporaries a precise, observable scope. }
    LOwnedNode := TTrackedNode.Create(nkBadge, 'retained');
    LOwnedBadge := TTrackedBadge.CreateFromNode(LOwnedNode) as INyxBadge;
    LOwnedNode.ReleaseOwnership;
    AConfig := LOwnedBadge.Configure;
    AConfig.Text('Ready / 🌙');
  end;

  function ConfiguredBadgeText(const AConfig: INyxConfiguration): TNyxText;
  var
    LConfigured: INyxBadge;
  begin
    LConfigured := AConfig.Done as INyxBadge;
    Result := LConfigured.Text;
  end;

  procedure DisposedDocumentBadge(out ABadge: INyxBadge);
  var
    LOwnedDocument: TNyxDocument;
    LOwnedRoot: INyxColumn;
  begin
    { Both compilers may retain fluent-result temporaries to routine exit.
      Exiting this scope releases every ancestor interface and its document. }
    LOwnedDocument := TNyxDocument.Create;
    try
      LOwnedRoot := NewNyxColumn('main');
      ABadge := NewNyxBadge('status').WithText('Retained badge');
      LOwnedRoot.Add(ABadge);
      LOwnedDocument.AddPage(LOwnedRoot);
    finally
      LOwnedDocument.Free;
    end;
  end;
begin
  Result := 0;
  GNodesDestroyed := 0;
  GControlsDestroyed := 0;
  ObservedConfiguration(LConfig);
  Check((GNodesDestroyed = 0) and (GControlsDestroyed = 0),
    'retained configuration owns the implementation and descriptor', Result);
  Check(ConfiguredBadgeText(LConfig) = TNyxText('Ready / 🌙'),
    'Done preserves specialized QueryInterface', Result);
  LConfig := nil;
  Check((GNodesDestroyed = 1) and (GControlsDestroyed = 1),
    'final configuration release destroys its unowned control exactly once (' +
    IntToStr(GNodesDestroyed) + '/' + IntToStr(GControlsDestroyed) + ')', Result);

  DisposedDocumentBadge(LBadge);
  Check((LBadge.Text = 'Retained badge') and (LBadge.Node.Parent = nil),
    'retained descendant survives document and ancestor disposal', Result);
  LBadge.Text := 'Still editable';
  Check(LBadge.Text = 'Still editable', 'surviving component remains fully usable', Result);

  LRoot := NewNyxColumn('new-owner');
  LRoot.Add(LBadge);
  LOther := LRoot.Extract(0);
  Check((LOther.Node = LBadge.Node) and (LOther.Node.Parent = nil) and
    (LRoot.Count = 0), 'managed extraction transfers lifetime without copying', Result);
  LRoot.Add(LOther);
  LRoot.Remove(LBadge);
  Check((LRoot.Count = 0) and (LBadge.Text = 'Still editable') and
    (LBadge.Node.Parent = nil), 'removal releases tree ownership but preserves interfaces', Result);
  LOther := nil;

  LRoot.Add(LBadge);
  LRejected := False;
  try
    NewNyxColumn('other').Add(LBadge);
  except
    on LException: ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LBadge.Node.Parent = LRoot.Node),
    'failed adoption preserves the existing owner and references', Result);
  LRejected := False;
  try
    LBadge.Add(LRoot);
  except
    on LException: ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (LRoot.Node.Parent = nil), 'cycles are refused before mutation', Result);
  LNode := LRoot.Node.Extract(0);
  LNode.ReleaseOwnership;
  Check(LBadge.Node.Parent = nil, 'raw extraction interoperates with an explicit managed transfer', Result);
  LRoot := nil;

  LBindings := LBadge.Binds.Text(NyxTextState('status'));
  LContract := LBadge.Contract.Signal(ntClick);
  LExtensions := LBadge.Extensions.SetValue(NyxExtension('application.note'), NyxData('🌙'));
  LBadge := nil;
  Check((LBindings.Done.Node.BindingCount = 1) and
    (LContract.EventCount = 1) and LExtensions.Has(NyxExtension('application.note')),
    'bindings/contracts/extensions independently retain the same live control', Result);
  LBindings := nil;
  LContract := nil;
  Check(LExtensions.Value(NyxExtension('application.note')).AsText = TNyxText('🌙'),
    'the last retained authoring facade remains valid', Result);
  LExtensions := nil;

  LMemo := NewNyxMemo('reply').WithText('Write a reply');
  LMemo.Value := 'A crafted reply / 🌙';
  LMemo.Placeholder := 'Your words';
  LMemo.ReadOnly := True;
  Check((LMemo.Text = 'Write a reply') and (LMemo.Value = TNyxText('A crafted reply / 🌙')) and
    (LMemo.Placeholder = 'Your words') and LMemo.ReadOnly,
    'specialized memo keeps caption, text value and typed field properties', Result);
  LCheck := NewNyxCheckbox('agree');
  LCheck.Checked := True;
  Check(LCheck.Checked, 'Boolean control exposes a Boolean checked property', Result);
  LSlider := NewNyxSlider('volume');
  LSlider.Minimum := 0;
  LSlider.Maximum := 20;
  LSlider.Value := 7;
  Check((LSlider.Value = 7) and (LSlider.Maximum = 20), 'integer control keeps integer properties', Result);
  LImage := NewNyxImage('portrait');
  LImage.Source := 'portrait.png';
  LImage.AlternativeText := 'A portrait';
  Check((LImage.Source = 'portrait.png') and (LImage.AlternativeText = 'A portrait'),
    'image exposes its specialized source and alternative text', Result);
  LChoice := NewNyxSelect('language');
  LChoice.Items := 'Pascal' + #10 + 'More Pascal';
  LChoice.Value := 'Pascal';
  Check(LChoice.Value = 'Pascal', 'choice control retains text choice semantics', Result);
  LReference := NewNyxComponent('instance');
  LReference.Reference := NyxComponent('definition');
  Check(LReference.Reference.Name = 'definition', 'reusable reference keeps its distinct type', Result);
  LMemo := nil;
  LCheck := nil;
  LSlider := nil;
  LImage := nil;
  LChoice := nil;
  LReference := nil;

  LThread := NewNyxCommentThread('discussion');
  LThread2 := NewNyxCommentThread('other-discussion');
  Check((LThread.Count > 0) and (LThread2.Count = LThread.Count),
    'compound factories include independent ready recipe parts', Result);
  LThread.ReplyMemo.Value := 'Only this discussion';
  Check(LThread2.ReplyMemo.Value <> 'Only this discussion',
    'compound customization never shares sibling recipe payloads', Result);
  LOther := NewNyxCommentThread('empty', ncoDescriptor);
  Check(LOther.Count = 0, 'descriptor construction does not duplicate recipe children', Result);
  LOther := nil;
  LThread := nil;
  LThread2 := nil;

  LDocument := TNyxDocument.Create;
  try
    LRoot := NewNyxColumn('decorated-page');
    LBadge := TBadgeDecorator.Create(NewNyxBadge('decorated').WithText('Different base'));
    LRoot.Add(LBadge);
    LOther := LRoot.Children[0];
    Check((LOther as INyxBadge) = LBadge,
      'an owner retains and returns the actual alternative implementation', Result);
    LOther := nil;
    LDocument.AddPage(LRoot);
    LSource := TNyxCodegen.Generate(LDocument);
    Check((LDocument.Find('decorated') = LBadge.Node) and
      (Pos('LDecoratedBadge: INyxBadge;', LSource) > 0) and
      (Pos('NewNyxBadge(''decorated'')', LSource) > 0),
      'unrelated implementation composes and generates through its public interface', Result);
    LWorkspace := TNyxSourceWorkspace.Create;
    try
      LCandidate := LWorkspace.Candidate(LDocument,
        StringReplace(LSource, '''Different base''', '''Edited base''', []));
      try
        Check(LCandidate.Find('decorated').Prop('text') = 'Edited base',
          'specialized generated source still admits typed configuration edits', Result);
      finally
        LCandidate.Free;
      end;
    finally
      LWorkspace.Free;
    end;
    LRoot := nil;
  finally
    LDocument.Free;
  end;
  Check(LBadge.Text = 'Different base', 'alternative implementation survives document release', Result);
  LBadge := nil;

  { Exhaustive dispatch constructs every default class and every descriptor class.
    The source fixture contains each public kind. Native-generated compilation
    reconstructs the catalog on FPC and pas2js in the generated-target harness. }
  LDocument := TNyxDocument.Create;
  try
    LRoot := NewNyxColumn('complete-catalog');
    LDocument.AddPage(LRoot);
    LDocument.AddComponent(NewNyxColumn('definition'));
    for LKind := Low(TNyxKind) to High(TNyxKind) do
    begin
      LOther := NewNyxBuiltinControl(LKind, NyxKindName(LKind) + '-default');
      Check(LOther.Kind = NyxKindName(LKind), 'default factory covers ' + LOther.Kind, Result);
      LOther := NewNyxBuiltinControl(LKind, NyxKindName(LKind) + '-descriptor', ncoDescriptor);
      Check(LOther.Count = 0, 'descriptor factory covers ' + LOther.Kind, Result);

      if LKind = nkComponent then
      begin
        LOther.Configure.Component(NyxComponent('definition')).Done;
      end;

      if LKind <> nkSlotOverride then
      begin
        LRoot.Add(LOther);
      end;
    end;
    LOther := nil;
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos(': TNyxNode;', LSource) = 0) and (Pos('TNyxNode.Create(', LSource) = 0),
      'complete generated catalog uses specialized managed controls', Result);
    Check(LSource = TNyxCodegen.Generate(LDocument), 'managed source remains deterministic', Result);
    LRoot := nil;
  finally
    LDocument.Free;
  end;

  { Raw clients keep their original token until explicit transfer. Releasing a
    temporary interface must not dispose a still caller-owned raw descriptor. }
  LNode := TTrackedNode.Create(nkBadge, 'raw-client');
  LOther := RetainNyxControl(LNode);
  LOther := nil;
  Check(LNode.ID = 'raw-client', 'retaining raw construction preserves caller ownership', Result);
  LNode.Free;

  LDocument := CreateNyxLegacyControlFixture(LSource);
  try
    Check((LDocument.Find('reply').Prop('text') = 'Recovered caption') and
      (LDocument.Find('reply').Prop('value') = 'Visual edit after recovery'),
      'accepted raw-node recovery remains editable before specialized regeneration', Result);
    Check(Pos('LReplyMemo: INyxMemo;', LSource) > 0,
      'recovered source regenerates specialized declarations', Result);
    Check((Pos('nyx.controls;', LSource) > 0) or (Pos('nyx.controls,', LSource) > 0),
      'recovered companion gains the required interface import', Result);
    Check(Pos(TNyxText('A retained legacy helper / 🌙'), LSource) > 0,
      'regeneration retains the exact handwritten helper', Result);
  finally
    LDocument.Free;
  end;
end;

end.
