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
program nyx_logic_document_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.model, nyx.codec, nyx.codegen,
  nyx.source, nyx.controls, nyx.flow.types, nyx.flow, nyx.logic,
  nyx.flow.declarations, nyx.studio.projects, nyx.studio.session,
  nyx.canvas.declarations, nyx.canvas.types, nyx.canvas.geometry, nyx.root.types,
  nyx.resources, nyx.expressions, nyx.composition,
  nyx.test.logic.fixture, nyx.test.logic.document
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

type
  TReentrantDefinition = class(TInterfacedObject, INyxFlowDefinition)
  public
    Registry: INyxFlowDeclarations;
    Original: INyxFlowDefinition;
    Target: TNyxFlowRef;
    function GetStepCount: Integer;
    function GetWireCount: Integer;
    function Step(AIndex: Integer): TNyxFlowStepRef;
    function Descriptor(AIndex: Integer): TNyxLogicDescriptor;
    function Configuration(AIndex: Integer): TNyxDataValue;
    function ToData: TNyxDataValue;
    function Build(const AResolver: INyxLogicResolver): INyxFlowGraph;
  end;

var
  GChecks: Integer;

function TReentrantDefinition.GetStepCount: Integer;
begin
  Result := Original.StepCount;
end;

function TReentrantDefinition.GetWireCount: Integer;
begin
  Result := Original.WireCount;
end;

function TReentrantDefinition.Step(AIndex: Integer): TNyxFlowStepRef;
begin
  Result := Original.Step(AIndex);
end;

function TReentrantDefinition.Descriptor(AIndex: Integer): TNyxLogicDescriptor;
begin
  Result := Original.Descriptor(AIndex);
end;

function TReentrantDefinition.Configuration(AIndex: Integer): TNyxDataValue;
begin
  Result := Original.Configuration(AIndex);
end;

function TReentrantDefinition.ToData: TNyxDataValue;
begin
  Registry.Remove(Target);
  Result := Original.ToData;
end;

function TReentrantDefinition.Build(const AResolver: INyxLogicResolver): INyxFlowGraph;
begin
  Result := Original.Build(AResolver);
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Saved Logic: ' + AReason);
  end;
  Inc(GChecks);
end;

function ReplaceFirst(const AText, AExpected, AReplacement: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(AExpected, AText);

  if LPosition = 0 then
  begin
    raise Exception.Create('Expected source fragment missing');
  end;
  Result := Copy(AText, 1, LPosition - 1) + AReplacement +
    Copy(AText, LPosition + Length(AExpected), MaxInt);
end;

{$ifndef PAS2JS}
procedure WriteExact(const APath: String; const AText: TNyxText);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(AText) > 0 then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure Run;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LParsed: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LReference: TNyxFlowRef;
  LRetained: INyxFlowDefinition;
  LRegistry: INyxFlowDeclarations;
  LBefore: TNyxText;
  LSource: TNyxText;
  LChanged: TNyxText;
  LData: TNyxDataValue;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LFailed: Boolean;
  LPair: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LApplied: TNyxProjectPair;
  LDraft: TNyxProjectPair;
  LResult: INyxFlowResult;
  LDescriptor: TNyxLogicDescriptor;
  LReentrant: TReentrantDefinition;
  LAlternative: INyxFlowDefinition;
  LRegistryBefore: TNyxText;
  LHandwritten: TNyxText;
  LContinuation: TNyxText;
  LHelper: TNyxText;
begin
  LDocument := LogicDocument;
  LCopy := nil;
  LParsed := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    LReference := NyxFlowRef('Profile 🐾');
    LDocument.Resources.Define(NyxResourceRef('saved-copy'), NyxTextResource('Stored copy 🐾'));
    LDocument.AddComponent(NewNyxLabel('canvas-copy').WithText('Canvas content'));
    LDocument.Canvases.Define(NyxCanvasRef('saved-workspace'), NewNyxCanvasDefinition
      .WithLayer(NyxCanvasLayer('Content'))
      .WithItem(NyxCanvasItem('copy'), NyxReusableRoot('canvas-copy'),
        NyxCanvasPlacement(NyxCanvasLayer('Content'), NyxCanvasBounds(-50, -25, 200, 80))));
    LDocument.Pages[0].Add(NewNyxLabel('computed-copy'));
    LDocument.Find('computed-copy').Binds.Text(
      NyxTextExpression('Input: ').Append(NyxTextExpression(NyxTextState('source')))).Done;
    LDocument.Flows.Define(LReference, LogicFixture);
    LDocument.Flows.Define(NyxFlowRef('Custom'), NewNyxFlowGraph
      .WithStep(NyxFlowStep('custom'), NewNyxLogicDeclaration(
        NyxLogicDescriptor(NyxLogicKind('application-tool'), 'Application tool',
          'Creator-defined operation without implicit runtime authority.', nlgData)
          .Input(NyxFlowPort('value'), nftJSON, 'Owned JSON input')
          .Output(NyxFlowPort('result'), nftJSON, 'Owned JSON output').Labelled('creator'),
        NyxObject([NyxField('caption', NyxData('Exact 🐾' + TNyxText(#0)))])))
      .Build);
    LBefore := TNyxCodec.Encode(LDocument);
    LRegistryBefore := LDocument.Flows.ToData.ToJSON;
    LReentrant := TReentrantDefinition.Create;
    LAlternative := LReentrant;
    LReentrant.Registry := LDocument.Flows;
    LReentrant.Original := LDocument.Flows.Definition(LReference);
    LReentrant.Target := LReference;
    LFailed := False;
    try
      LDocument.Flows.Define(LReference, LAlternative);
    except
      on E: ENyxFlow do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LDocument.Flows.ToData.ToJSON = LRegistryBefore),
      'reentrant definition getter cannot mutate accepted registry membership');
    LAlternative := nil;
    LDocument.Flows.Define(LReference, LogicFixture);
    Check(LDocument.Flows.ToData.ToJSON = LRegistryBefore, 'registry admission guard retires after refusal');
    Check(TNyxDataValue.ParseJSON(LBefore).Field('version').AsInteger = 14, 'graphs promote typed project version');
    LCopy := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LCopy) = LBefore, 'full codec pair retains graphs and creator data');
    Check((LCopy.Canvases.Count = 1) and (LCopy.Resources.Count = 1) and
      LCopy.HasComputedBindings, 'version 14 retains preceding canvas/resource/expression families');
    LRetained := LCopy.Flows.Definition(LReference);
    FreeAndNil(LCopy);
    LCopy := CloneNyxViewDocument(LDocument, LDocument.Pages[0]);
    Check(LCopy.Flows.ToData.ToJSON = LDocument.Flows.ToData.ToJSON,
      'focused page retains complete saved graphs without executable authority');
    FreeAndNil(LCopy);
    LCopy := CloneNyxViewDocument(LDocument, LDocument.Components[0]);
    Check(LCopy.Flows.ToData.ToJSON = LDocument.Flows.ToData.ToJSON,
      'focused reusable retains complete saved graphs without executable authority');
    LRegistry := LCopy.Flows.Clone;
    LCopy.Flows.Remove(LReference);
    Check((LRegistry.Count = 2) and (LDocument.Flows.Count = 2), 'cloned membership is independent');
    FreeAndNil(LCopy);
    Check(LRetained.StepCount = 5, 'retained definition survives document retirement');
    LResult := LRetained.Build.Evaluate([
      NyxFlowInput(LogicEndpoint('parse', nlpText), NyxFlowText(LogicExample)),
      NyxFlowInput(LogicEndpoint('summary', nlpLeft), NyxFlowText('Profile'))]);
    Check(Pos('English demo', LResult.Value(LogicEndpoint('summary', nlpText)).AsText) > 0,
      'saved known graph resolves and executes only when requested');
    LFailed := False;
    try
      LDocument.Flows.Definition(NyxFlowRef('Custom')).Build;
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'saved unknown creator requires explicit runtime resolver');
    LDescriptor := LDocument.Flows.Definition(NyxFlowRef('Custom')).Descriptor(0);
    Check((LDescriptor.Kind.Name = 'application-tool') and (LDescriptor.LabelAt(0) = 'creator'),
      'creator schema/help/labels remain inspectable without executable objects');
    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('Result.Flows.Define(NyxFlowRef(', LSource) > 0) and
      (Pos('.WithStep(NyxFlowStep(', LSource) > 0) and
      (Pos('NyxLogicPort(nlpJSON)', LSource) > 0) and
      (Pos('NewNyxJSONFormat(njlIndented, 2)', LSource) > 0),
      'generated source uses typed factories and graph construction');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'literal source reconstructs complete graph/creator document');
    Check(LWorkspace.Render(LParsed) = LSource, 'no-op source preserves exact bytes');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);
    LPair := NyxProjectPair(LBefore, LSource);
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('run-button');
    LSession.SetProperty('text', 'Format profile');
    LAfter := LSession.ProjectSnapshot;
    Check(LAfter.Source <> LSource, 'ordinary paired control edit retains graph source');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair), 'paired Undo retains exact graph design/source');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter), 'paired Redo retains exact graph design/source');
    LChanged := ReplaceFirst(LAfter.Source, 'NewNyxJSONFormat(njlIndented, 2)',
      'NewNyxJSONFormat(njlIndented, 4)');
    LSession.SetSourceDraft(LChanged);
    LDraft := LSession.ProjectSnapshot;
    LFailed := False;
    try
      LSession.PrepareSourceEditRequest(LChanged + #10, 1);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LDraft)),
      'pending draft guard retains complete graph pair/buffer/base');
    LSession.ApplySourceDraft;
    LApplied := LSession.ProjectSnapshot;
    Check(LSession.Document.Flows.Definition(LReference).Configuration(2).Field('indent').AsInteger = 4,
      'ordinary source application updates typed operation configuration');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LAfter), 'source Undo restores exact accepted graph pair');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LApplied), 'source Redo restores exact applied graph pair');
    LHelper := 'function SavedIndent: Integer;' + #10 +
      'begin' + #10 + '  Result := 2;' + #10 + 'end;' + #10;
    LHandwritten := ReplaceFirst(LSource, 'implementation' + #10,
      'implementation' + #10 + #10 + LHelper);
    LHandwritten := ReplaceFirst(LHandwritten, 'NewNyxJSONFormat(njlIndented, 2)',
      'NewNyxJSONFormat(njlIndented, SavedIndent)');
    LContinuation := CustomizeNyxExecutedSource(LHandwritten, LDocument, LSession.Document);
    Check(Pos(LHelper, LContinuation) > 0, 'targeted graph continuation retains exact handwritten helper');
    Check(Pos('ADocument.Flows.Define(NyxFlowRef(', LContinuation) > 0,
      'changed graph uses an explicit typed continuation rather than replacing the original builder');
    LData := TNyxDataValue.ParseJSON(LBefore);
    SetLength(LFields, LData.Count);
    for LIndex := 0 to LData.Count - 1 do
    begin
      LFields[LIndex] := NyxField(LData.Key(LIndex), LData.Field(LData.Key(LIndex)));

      if LData.Key(LIndex) = 'version' then
      begin
        LFields[LIndex] := NyxField('version', NyxData(13));
      end;
    end;
    LCopy := TNyxCodec.Decode(NyxObject(LFields).ToJSON);
    Check((LCopy.Flows.Count = 0) and LCopy.Extensions.Has(NyxExtension(NyxFlowsWireField)),
      'older flow-named extensions retain opaque meaning');
    LCopy.Flows.Define(LReference, LogicFixture);
    LFailed := False;
    try
      TNyxCodec.Encode(LCopy);
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'typed promotion refuses an older opaque field collision');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      ForceDirectories(ParamStr(1));
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', LSource);
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'design.nyx', LBefore);
      ForceDirectories(IncludeTrailingPathDelimiter(ParamStr(1)) + 'continuation');
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'continuation/nyx.generated.view.pas', LContinuation);
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'continuation/design.nyx',
        TNyxCodec.Encode(LSession.Document));
    end;
    {$endif}
  finally
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LCopy.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-logic-document', 'passed');
    document.body.setAttribute('data-logic-document-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' saved Logic/source/history checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-logic-document', 'failed');
      document.body.setAttribute('data-logic-document-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
