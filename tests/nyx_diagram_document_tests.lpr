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
program nyx_diagram_document_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.codegen, nyx.source,
  nyx.controls, nyx.diagram, nyx.diagram.declarations, nyx.diagram.editor,
  nyx.canvas.geometry, nyx.composition, nyx.design.tokens, nyx.design.tokens.core,
  nyx.studio.projects, nyx.studio.session, nyx.test.diagram.document
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  { Alternate immutable graph implementation for foreign-capture qualification.
    Public operations delegate to a private plan. Only ToData may deliberately
    reenter membership or fail; the registry must capture once before adoption.
    Registry is test-owned during the call and retains no reference to this object.
    Native object-query forwarding models aggregation without granting trust to
    this facade; the destructor counter observes its actual fixture retirement. }
  TForeignDiagram = class(TInterfacedObject, INyxDiagram)
  public
    Base: INyxDiagram;
    Registry: INyxDiagramDeclarations;
    Calls: Integer;
    Reenter: Boolean;
    FailCapture: Boolean;
    ForwardObject: Boolean;
    ObjectQueries: Integer;
    destructor Destroy; override;
    {$ifndef PAS2JS}
    function QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
      AIID: TGUID; out AObject): LongInt;
      {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif}; reintroduce;
    {$endif}
    function GetVertexCount: Integer;
    function GetEdgeCount: Integer;
    function Vertex(AIndex: Integer): TNyxDiagramVertex;
    function Edge(AIndex: Integer): TNyxDiagramEdge;
    function FindVertex(const AReference: TNyxDiagramVertexRef): TNyxDiagramVertex;
    function FindEdge(const AReference: TNyxDiagramEdgeRef): TNyxDiagramEdge;
    function WithVertex(const AVertex: TNyxDiagramVertex): INyxDiagram;
    function WithEdge(const AEdge: TNyxDiagramEdge): INyxDiagram;
    function WithoutVertex(const AReference: TNyxDiagramVertexRef;
      APolicy: TNyxDiagramRemoval): INyxDiagram;
    function WithoutEdge(const AReference: TNyxDiagramEdgeRef): INyxDiagram;
    function Route(const AReference: TNyxDiagramEdgeRef): TNyxDiagramRoute;
    function PickVertex(const APoint: TNyxCanvasPoint): TNyxDiagramVertexRef;
    function PickEdge(const APoint: TNyxCanvasPoint; ATolerance: Double): TNyxDiagramEdgeRef;
    function ToData: TNyxDataValue;
  end;

var
  GChecks: Integer;
  GForeignDestroyed: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Diagram document: ' + AReason);
  end;
  Inc(GChecks);
end;

function ReplaceOnce(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LAt: Integer;
begin
  LAt := Pos(ABefore, ASource);

  if (LAt = 0) or (Pos(ABefore, Copy(ASource, LAt + Length(ABefore), MaxInt)) > 0) then
  begin
    raise Exception.Create('Expected one exact source marker');
  end;
  Result := Copy(ASource, 1, LAt - 1) + AAfter + Copy(ASource, LAt + Length(ABefore), MaxInt);
end;

function Replacing(const AObject: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AObject.Count);
  for LIndex := 0 to AObject.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));

    if AObject.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
    end;
  end;
  Result := NyxObject(LFields);
end;

procedure RefuseWire(const ASource: TNyxText);
var
  LDocument: TNyxDocument;
  LFailed: Boolean;
begin
  LDocument := nil;
  LFailed := False;
  try
    LDocument := TNyxCodec.Decode(ASource);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  LDocument.Free;
  Check(LFailed, 'malformed/promoted persistence refuses');
end;

procedure RefuseSource(const ASource: TNyxText);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LFailed: Boolean;
begin
  LDocument := nil;
  LWorkspace := nil;
  LFailed := False;
  try
    LDocument := TNyxSourceWorkspace.PrepareDraft(ASource, LWorkspace);
  except
    on E: Exception do
    begin
      LFailed := True;
    end;
  end;
  LDocument.Free;
  LWorkspace.Free;
  Check(LFailed, 'wrong typed source refuses without an admitted workspace');
end;

{$ifndef PAS2JS}
procedure SaveText(const AName, AText: TNyxText);
var
  LFile: TFileStream;
begin
  LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + AName, fmCreate);
  try

    if AText <> '' then
    begin
      LFile.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LFile.Free;
  end;
end;
{$endif}


{$ifndef PAS2JS}
function TForeignDiagram.QueryInterface({$ifdef FPC_HAS_CONSTREF}constref{$else}const{$endif}
  AIID: TGUID; out AObject): LongInt;
  {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};
begin

  if ForwardObject and IsEqualGUID(AIID, IObjectInstance) then
  begin
    Inc(ObjectQueries);
    Result := Base.QueryInterface(AIID, AObject);
  end
  else
  begin
    Result := inherited QueryInterface(AIID, AObject);
  end;
end;
{$endif}

destructor TForeignDiagram.Destroy;
begin
  Inc(GForeignDestroyed);
  inherited Destroy;
end;

function TForeignDiagram.GetVertexCount: Integer;
begin
  Result := Base.VertexCount;
end;

function TForeignDiagram.GetEdgeCount: Integer;
begin
  Result := Base.EdgeCount;
end;

function TForeignDiagram.Vertex(AIndex: Integer): TNyxDiagramVertex;
begin
  Result := Base.Vertex(AIndex);
end;

function TForeignDiagram.Edge(AIndex: Integer): TNyxDiagramEdge;
begin
  Result := Base.Edge(AIndex);
end;

function TForeignDiagram.FindVertex(const AReference: TNyxDiagramVertexRef): TNyxDiagramVertex;
begin
  Result := Base.FindVertex(AReference);
end;

function TForeignDiagram.FindEdge(const AReference: TNyxDiagramEdgeRef): TNyxDiagramEdge;
begin
  Result := Base.FindEdge(AReference);
end;

function TForeignDiagram.WithVertex(const AVertex: TNyxDiagramVertex): INyxDiagram;
begin
  Result := Base.WithVertex(AVertex);
end;

function TForeignDiagram.WithEdge(const AEdge: TNyxDiagramEdge): INyxDiagram;
begin
  Result := Base.WithEdge(AEdge);
end;

function TForeignDiagram.WithoutVertex(const AReference: TNyxDiagramVertexRef;
  APolicy: TNyxDiagramRemoval): INyxDiagram;
begin
  Result := Base.WithoutVertex(AReference, APolicy);
end;

function TForeignDiagram.WithoutEdge(const AReference: TNyxDiagramEdgeRef): INyxDiagram;
begin
  Result := Base.WithoutEdge(AReference);
end;

function TForeignDiagram.Route(const AReference: TNyxDiagramEdgeRef): TNyxDiagramRoute;
begin
  Result := Base.Route(AReference);
end;

function TForeignDiagram.PickVertex(const APoint: TNyxCanvasPoint): TNyxDiagramVertexRef;
begin
  Result := Base.PickVertex(APoint);
end;

function TForeignDiagram.PickEdge(const APoint: TNyxCanvasPoint;
  ATolerance: Double): TNyxDiagramEdgeRef;
begin
  Result := Base.PickEdge(APoint, ATolerance);
end;

function TForeignDiagram.ToData: TNyxDataValue;
begin
  Inc(Calls);

  if Reenter then
  begin
    Registry.Remove(NyxDiagramRef('Captured'));
  end;

  if FailCapture then
  begin
    raise Exception.Create('Deliberate foreign capture failure');
  end;
  Result := Base.ToData;
end;

procedure Run;
const
  COpaqueFields: array[0..7] of TNyxText =
    ('diagrams', 'programs', 'flows', 'canvases', 'resources', 'menus',
     'presentations', 'collections');
  COpaqueNodeFields: array[0..5] of TNyxText =
    ('menuBar', 'menu', 'contentRules', 'presentationRules', 'collectionView', 'canvasScene');
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LRead: TNyxDocument;
  LOld: TNyxDocument;
  LFocused: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LRegistry: INyxDiagramDeclarations;
  LClone: INyxDiagramDeclarations;
  LPlan: INyxDiagram;
  LEditor: INyxDiagramEditor;
  LSession: TNyxStudioSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LPending: TNyxProjectPair;
  LSource: TNyxText;
  LChanged: TNyxText;
  LWire: TNyxText;
  LOldWire: TNyxText;
  LRetained: TNyxText;
  LCrafted: TNyxText;
  LContinued: TNyxText;
  LRemoved: TNyxText;
  LRecreated: TNyxText;
  LData: TNyxDataValue;
  LEntry: TNyxDataValue;
  LEntries: array of TNyxDataValue;
  LIndex: Integer;
  LFailed: Boolean;
  LTokens: nyx.design.tokens.TNyxThemeTokens;
  LCoreTokens: nyx.design.tokens.core.TNyxThemeTokens;
  LForeign: TForeignDiagram;
  LForeignLease: INyxDiagram;
  LCaptured: INyxDiagram;
  LCapturedWire: TNyxText;
  LDestroyed: Integer;
begin
  LDocument := DiagramDocumentFixture;
  LCopy := nil;
  LRead := nil;
  LOld := nil;
  LFocused := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    LWire := TNyxCodec.Encode(LDocument);
    LData := TNyxDataValue.ParseJSON(LWire);
    Check(LData.Field('version').AsInteger = 16, 'saved visual intent selects version sixteen');
    LCopy := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LCopy) = LWire, 'complete diagram document is byte-exact after persistence');
    LPlan := LDocument.Diagrams.Definition(NyxDiagramRef('Publishing workflow'));
    Check((LPlan.VertexCount = 3) and (LPlan.EdgeCount = 4), 'saved intent retains editable topology');
    LCaptured := CaptureNyxDiagram(LPlan);
    Check(LCaptured = LPlan, 'admitted built-in graph capture retains immutable identity');
    LFailed := False;
    try
      LCaptured := CaptureNyxDiagram(nil);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LCaptured = LPlan), 'missing graph capture preserves the accepted output');
    LForeign := TForeignDiagram.Create;
    LForeignLease := LForeign;
    LForeign.Base := LPlan;
    LForeign.ForwardObject := True;
    LCaptured := CaptureNyxDiagram(LForeignLease);
    {$ifndef PAS2JS}
    Check(LForeign.ObjectQueries > 0, 'native graph aggregation probe forwards an actual object query');
    {$endif}
    LCapturedWire := LCaptured.ToData.ToJSON;
    Check((LForeign.Calls = 1) and (LCaptured <> LForeignLease) and
      (LCapturedWire = LPlan.ToData.ToJSON), 'alternate graph implementation captures exact complete intent once');
    LForeign.Base := LPlan.WithVertex(LPlan.Vertex(0).Captioned('A later creator revision'));
    Check(LCaptured.ToData.ToJSON = LCapturedWire, 'later creator edits leave its admitted snapshot independent');
    LForeign.FailCapture := True;
    LFailed := False;
    try
      LCaptured := CaptureNyxDiagram(LForeignLease);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LForeign.Calls = 2) and (LCaptured.ToData.ToJSON = LCapturedWire),
      'creator capture failure cannot replace earlier complete graph intent');
    LDestroyed := GForeignDestroyed;
    LForeign := nil;
    LForeignLease := nil;
    Check((GForeignDestroyed = LDestroyed + 1) and (LCaptured.ToData.ToJSON = LCapturedWire),
      'captured graph retains no creator facade and remains valid after its retirement');
    Check((LPlan.Vertex(0).Bounds.Left = -120.125) and
      (LPlan.Vertex(0).Caption = TNyxText('Start / 🌙 / 漢字 / O''Brien')),
      'fractional geometry and supplementary text remain exact');

    LTokens := nyx.design.tokens.NyxThemePreset(nyx.design.tokens.ntpDark);
    LCoreTokens := LTokens;
    Check(LCoreTokens.ToData.ToJSON =
      nyx.design.tokens.core.NyxThemePreset(nyx.design.tokens.core.ntpDark).ToData.ToJSON,
      'existing theme authoring aliases have identical identity and values');
    Check(Ord(nyx.design.tokens.ntcWarningText) = Ord(nyx.design.tokens.core.ntcWarningText),
      'existing semantic color ordinals remain compatible');

    LRegistry := LDocument.Diagrams;
    LClone := LRegistry.Clone;
    LRetained := LClone.ToData.ToJSON;
    LRegistry.Define(NyxDiagramRef('Publishing workflow'),
      LPlan.WithVertex(LPlan.Vertex(0).Captioned('Changed locally')));
    Check(LRegistry.Reference(0).Name = 'Publishing workflow', 'upsert preserves authoring order');
    Check(LClone.ToData.ToJSON = LRetained, 'cloned membership retains original immutable values');
    LRegistry.Remove(NyxDiagramRef('Publishing workflow'));
    Check((LRegistry.Count = 1) and (LRegistry.Reference(0).Name = 'Empty board'),
      'removal preserves remaining authoring order');
    LFailed := False;
    try
      LRegistry.Remove(NyxDiagramRef('Missing'));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LRegistry.Count = 1), 'missing removal refuses without membership changes');
    LFailed := False;
    try
      LRegistry.Define(NyxDiagramRef('Invalid'), nil);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LRegistry.Count = 1), 'nil foreign definition refuses atomically');
    LRegistry := LClone;
    LForeign := TForeignDiagram.Create;
    LForeignLease := LForeign;
    LForeign.Base := LPlan;
    LForeign.Registry := NewNyxDiagramDeclarations;
    LForeign.Registry.Define(NyxDiagramRef('Captured'), LForeignLease);
    Check((LForeign.Calls = 1) and
      (LForeign.Registry.Definition(NyxDiagramRef('Captured')).ToData.ToJSON = LPlan.ToData.ToJSON),
      'foreign graph capture occurs once into a normalized private value');
    LRetained := LForeign.Registry.ToData.ToJSON;
    LForeign.Reenter := True;
    LFailed := False;
    try
      LForeign.Registry.Define(NyxDiagramRef('Captured'), LForeignLease);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LForeign.Registry.ToData.ToJSON = LRetained),
      'reentrant foreign capture cannot retire or replace accepted membership');
    LForeign.Reenter := False;
    LForeign.FailCapture := True;
    LFailed := False;
    try
      LForeign.Registry.Define(NyxDiagramRef('Captured'), LForeignLease);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LForeign.Registry.ToData.ToJSON = LRetained),
      'failed foreign capture preserves every accepted byte');
    LForeign.FailCapture := False;
    LForeign.Registry.Define(NyxDiagramRef('Captured'), LForeignLease);
    Check(LForeign.Calls = 4, 'failed capture releases the mutation reservation');
    LForeignLease := nil;
    LDocument.Free;
    LDocument := LCopy.Clone;
    Check(TNyxCodec.Encode(LDocument) = LWire, 'document clone includes the entire visual family');
    LFocused := CloneNyxViewDocument(LDocument, LDocument.Pages[0]);
    Check(LFocused.Diagrams.ToData.ToJSON = LDocument.Diagrams.ToData.ToJSON,
      'focused page context retains independent editable declarations');
    LFocused.Diagrams.Remove(NyxDiagramRef('Publishing workflow'));
    Check(LDocument.Diagrams.Count = 2, 'focused membership changes cannot alter the application');

    LEditor := NewNyxDiagramEditor('saved-editor', LCopy, NyxDiagramRef('Publishing workflow'));
    LCopy.Diagrams.Remove(NyxDiagramRef('Publishing workflow'));
    Check(LEditor.InitialDiagram.ToData.ToJSON = LPlan.ToData.ToJSON,
      'reopened editor retains exact intent independently of later document edits');
    LFailed := False;
    try
      NewNyxDiagramEditor('missing-editor', LCopy, NyxDiagramRef('Publishing workflow'));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'reopening a missing declaration refuses');
    FreeAndNil(LCopy);
    Check(LEditor.InitialDiagram.EdgeCount = 4, 'reopened descriptor outlives its borrowed source document');

    LEntries := nil;
    LEntry := LData.Field('diagrams').Item(0);
    SetLength(LEntries, 2);
    LEntries[0] := LEntry;
    LEntries[1] := LEntry;
    RefuseWire(Replacing(LData, 'diagrams', NyxArray(LEntries)).ToJSON);
    RefuseWire(Replacing(LData, 'diagrams', NyxObject([])).ToJSON);
    RefuseWire(Replacing(LData, 'diagrams', NyxArray([NyxObject([
      NyxField('name', NyxData(123)), NyxField('diagram', LEntry.Field('diagram'))])])).ToJSON);
    RefuseWire(Replacing(LData, 'diagrams', NyxArray([NyxObject([
      NyxField('name', NyxData('Extra')), NyxField('diagram', LEntry.Field('diagram')),
      NyxField('permission', NyxData(True))])])).ToJSON);
    RefuseWire(Replacing(LData, 'version', NyxData(16.5)).ToJSON);
    SetLength(LEntries, MaximumNyxDiagramDefinitions + 1);
    for LIndex := 0 to High(LEntries) do
    begin
      LEntries[LIndex] := NyxObject([NyxField('name', NyxData('Board ' + IntToStr(LIndex))),
        NyxField('diagram', NewNyxDiagram.ToData)]);
    end;
    RefuseWire(Replacing(LData, 'diagrams', NyxArray(LEntries)).ToJSON);
    LRegistry := NewNyxDiagramDeclarations;
    for LIndex := 0 to MaximumNyxDiagramDefinitions - 1 do
    begin
      LRegistry.Define(NyxDiagramRef('Board ' + IntToStr(LIndex)), NewNyxDiagram);
    end;
    LRetained := LRegistry.ToData.ToJSON;
    LFailed := False;
    try
      LRegistry.Define(NyxDiagramRef('Overflow'), NewNyxDiagram);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LRegistry.ToData.ToJSON = LRetained), 'family capacity refuses before publication');

    LPlan := NewNyxDiagram;
    for LIndex := 0 to MaximumNyxDiagramVertices - 1 do
    begin
      LPlan := LPlan.WithVertex(NyxDiagramVertex(NyxDiagramVertexRef('Shape ' + IntToStr(LIndex)),
        ndsRectangle, NyxCanvasBounds(LIndex, 0, 80, 40)));
    end;
    LRegistry := NewNyxDiagramDeclarations;
    for LIndex := 0 to (MaximumNyxDiagramFamilyVertices div MaximumNyxDiagramVertices) - 1 do
    begin
      LRegistry.Define(NyxDiagramRef('Complete board ' + IntToStr(LIndex)), LPlan);
    end;
    LRetained := LRegistry.ToData.ToJSON;
    LFailed := False;
    try
      LRegistry.Define(NyxDiagramRef('Geometry overflow'), LPlan);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LRegistry.ToData.ToJSON = LRetained),
      'aggregate geometry budget refuses independently of per-graph and membership budgets');

    LOld := TNyxDocument.Create;
    LOld.Title := 'Legacy';
    LOld.AddPage(NewNyxPage('home'));
    LOldWire := TNyxCodec.Encode(LOld);
    Check(TNyxDataValue.ParseJSON(LOldWire).Field('version').AsInteger = 1,
      'empty new registry preserves the original document version');
    LOld.Diagrams.Define(NyxDiagramRef('Temporary'), NewNyxDiagram).Remove(NyxDiagramRef('Temporary'));
    Check(TNyxCodec.Encode(LOld) = LOldWire, 'remove last definition restores exact original bytes');
    for LIndex := Low(COpaqueFields) to High(COpaqueFields) do
    begin
      LOld.Extensions.SetValue(NyxExtension(COpaqueFields[LIndex]), NyxData('Retained opaque'));
      LRetained := TNyxCodec.Encode(LOld);
      LRead := TNyxCodec.Decode(LRetained);
      Check(TNyxCodec.Encode(LRead) = LRetained, 'old opaque data stays exact without promotion');
      FreeAndNil(LRead);
      LOld.Diagrams.Define(NyxDiagramRef('Promoted'), NewNyxDiagram);
      LFailed := False;
      try
        TNyxCodec.Encode(LOld);
      except
        on E: Exception do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed, 'new version refuses collision with every promoted opaque family');
      LOld.Diagrams.Remove(NyxDiagramRef('Promoted'));
      LOld.Extensions.Remove(NyxExtension(COpaqueFields[LIndex]));
      Check(TNyxCodec.Encode(LOld) = LOldWire, 'refused promotion leaves exact legacy bytes');
    end;

    for LIndex := Low(COpaqueNodeFields) to High(COpaqueNodeFields) do
    begin
      LOld.Pages[0].Extensions.SetValue(NyxExtension(COpaqueNodeFields[LIndex]),
        NyxData('Retained node field'));
      LRetained := TNyxCodec.Encode(LOld);
      LRead := TNyxCodec.Decode(LRetained);
      Check(TNyxCodec.Encode(LRead) = LRetained, 'opaque node meaning stays exact at its old version');
      FreeAndNil(LRead);
      LOld.Diagrams.Define(NyxDiagramRef('Promoted'), NewNyxDiagram);
      LFailed := False;
      try
        TNyxCodec.Encode(LOld);
      except
        on E: Exception do
        begin
          LFailed := True;
        end;
      end;
      Check(LFailed, 'new diagram version refuses promoted opaque node fields');
      LOld.Diagrams.Remove(NyxDiagramRef('Promoted'));
      LOld.Pages[0].Extensions.Remove(NyxExtension(COpaqueNodeFields[LIndex]));
    end;
    LDocument.Pages[0].Extensions.SetValue(NyxExtension('diagrams'), NyxData('Node-local extension'));
    LRead := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    Check(LRead.Pages[0].Extensions.Value(NyxExtension('diagrams')).AsText = 'Node-local extension',
      'document diagram spelling does not capture an unrelated node extension');
    FreeAndNil(LRead);
    LDocument.Pages[0].Extensions.Remove(NyxExtension('diagrams'));

    LSource := TNyxCodegen.Generate(LDocument);
    Check(LSource = TNyxCodegen.Generate(LDocument), 'complete source generation is deterministic');
    Check((Pos('Result.Diagrams.Define(NyxDiagramRef(', LSource) > 0) and
      (Pos('.WithVertex(NyxDiagramVertex(', LSource) > 0) and
      (Pos('ndsDiamond', LSource) > 0) and (Pos('NyxThemePaint(ntcAccentText)', LSource) > 0),
      'crafted public Pascal exposes specialized visual intent without a serialized blob');
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LWire, 'literal source reconstructs complete exact document meaning');
    FreeAndNil(LRead);
    LCopy := LDocument.Clone;
    LPlan := LCopy.Diagrams.Definition(NyxDiagramRef('Publishing workflow'));
    LCopy.Diagrams.Define(NyxDiagramRef('Publishing workflow'),
      LPlan.WithVertex(LPlan.Vertex(1).Captioned('Visual proposal')));
    LChanged := LWorkspace.Render(LCopy);
    FreeAndNil(LWorkspace);
    LRead := TNyxSourceWorkspace.PrepareDraft(LChanged, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = TNyxCodec.Encode(LCopy),
      'existing source reconciliation carries a changed diagram definition exactly');
    FreeAndNil(LRead);
    FreeAndNil(LCopy);
    RefuseSource(ReplaceOnce(LSource, 'ndsDiamond', 'ndaOpen'));
    RefuseSource(ReplaceOnce(LSource, 'NyxDiagramRef(''Empty board'')', 'NyxDiagramVertexRef(''Empty board'')'));
    RefuseSource(ReplaceOnce(LSource, '.Arrows(ndaOpen, ndaNone)', '.Arrows(ndsDiamond, ndaNone)'));
    RefuseSource(ReplaceOnce(LSource, 'NewNyxDiagram' + #10,
      'NewNyxDiagram.UnknownMethod' + #10));

    { Compiler-owned helpers are retained, while chronological continuations
      express only deliberate changed/removed/recreated declarations. Exact
      compilation of exported units is qualified by the companion consumer. }
    LPlan := LDocument.Diagrams.Definition(NyxDiagramRef('Publishing workflow'));
    LCrafted := ReplaceOnce(LSource, TNyxCodegen.DiagramDefinition(LPlan), 'InitialWorkflow');
    LCrafted := ReplaceOnce(LCrafted, 'implementation', 'implementation' + #10 +
      TNyxText('{ Handwritten workflow helper / 🌙 }') + #10 +
      'function InitialWorkflow: INyxDiagram;' + #10 +
      'begin' + #10 + '  Result := ' + TNyxCodegen.DiagramDefinition(LPlan) + ';' + #10 +
      'end;' + #10);
    Check(CustomizeNyxExecutedSource(LCrafted, LDocument, LDocument) = LCrafted,
      'unchanged helper-defined diagrams require no source customization');
    LCopy := LDocument.Clone;
    LCopy.Diagrams.Define(NyxDiagramRef('Publishing workflow'),
      LPlan.WithVertex(LPlan.Vertex(1).Captioned('Visual proposal')));
    LContinued := CustomizeNyxExecutedSource(LCrafted, LDocument, LCopy);
    Check((Pos('Result := InitialWorkflow', LContinued) = 0) and
      (Pos('NyxDiagramRef(''Publishing workflow''),' + #10 + '      InitialWorkflow', LContinued) > 0),
      'the original constructor still calls its handwritten graph helper');
    Check(Pos(TNyxText('{ Handwritten workflow helper / 🌙 }'), LContinued) > 0,
      'continuation preserves exact supplementary helper comments');
    LRead := LCopy.Clone;
    LRead.Diagrams.Remove(NyxDiagramRef('Publishing workflow'));
    LRemoved := CustomizeNyxExecutedSource(LContinued, LCopy, LRead);
    FreeAndNil(LCopy);
    LCopy := LRead.Clone;
    LCopy.Diagrams.Define(NyxDiagramRef('Publishing workflow'), LPlan);
    LRecreated := CustomizeNyxExecutedSource(LRemoved, LRead, LCopy);
    Check((LCopy.Diagrams.Reference(0).Name = 'Empty board') and
      (Pos('ADocument.Diagrams.Remove(', LRecreated) > 0) and
      (Pos('ADocument.Diagrams.Define(', LRecreated) > 0),
      'chronological source retains removal before recreation and final authoring order');
    LRead.Diagrams.Define(NyxDiagramRef('Publishing workflow'), LPlan);
    LRead.Diagrams.Remove(NyxDiagramRef('Empty board'));
    LRead.Diagrams.Define(NyxDiagramRef('Empty board'), NewNyxDiagram);
    LFailed := False;
    try
      CustomizeNyxExecutedSource(LRecreated, LCopy, LRead);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'unexpressed registry reordering refuses instead of changing helper meaning');
    FreeAndNil(LRead);
    FreeAndNil(LCopy);

    LBefore := NyxProjectPair(LWire, LSource);
    LSession := TNyxStudioSession.Create(LBefore);
    LChanged := ReplaceOnce(LSource, '''Ready?''', '''Review before publishing''');
    LSession.SetSourceDraft(LChanged);
    LSession.ApplySourceDraft;
    LAfter := LSession.ProjectSnapshot;
    Check((LAfter.Source = LChanged) and
      (LSession.Document.Diagrams.Definition(NyxDiagramRef('Publishing workflow'))
        .FindVertex(NyxDiagramVertexRef('decision')).Caption = 'Review before publishing'),
      'ordinary source Apply publishes saved diagram meaning with its exact source');
    Check(LSession.CanUndo, 'accepted source/diagram edit owns ordinary history');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'source Apply consumes its buffer; one Undo restores the exact original clean pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter),
      'one Redo restores the full accepted source/design/draft pair');
    LSession.SetSourceDraft(LChanged + #10 + '// unfinished follow-up');
    LPending := LSession.ProjectSnapshot;
    LSession.AdoptProject(LBefore);
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPending),
      'ordinary paired adoption/Undo retains exact pending text and original base');

    {$ifndef PAS2JS}
    if ParamCount = 1 then
    begin
      SaveText('nyx.generated.view.pas', LSource);
      SaveText('expected.nyx', LWire);
      SaveText('edited.nyx', LAfter.Design);
      SaveText('edited.pas', LAfter.Source);
      SaveText('handwritten.pas', LCrafted);
      SaveText('continued.pas', LContinued);
      SaveText('removed.pas', LRemoved);
      SaveText('recreated.pas', LRecreated);
    end;
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LFocused.Free;
    LOld.Free;
    LRead.Free;
    LCopy.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-diagram-document', 'passed');
    document.body.setAttribute('data-diagram-document-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' diagram document/source/history checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-diagram-document', 'failed');
      document.body.setAttribute('data-diagram-document-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
