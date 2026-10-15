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

program nyx_program_authoring_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.bytes, nyx.data, nyx.model, nyx.controls,
  nyx.types, nyx.codec, nyx.codegen, nyx.source, nyx.programs.types,
  nyx.programs.contract, nyx.programs, nyx.programs.declarations, nyx.programs.canvas,
  nyx.programs.editing, nyx.programs.editor, nyx.studio.session, nyx.studio.edits, nyx.studio.projects,
  nyx.studio.agents, nyx.studio.presentation
  {$ifndef PAS2JS}, nyx.studio.mcp{$endif}
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

const
  CGreeting: TNyxText =
    '{ Exact source: 😀 and mixed line endings stay authored data. }' + #13#10 +
    'function Greet(const Name: String): String;' + #10 +
    'begin' + #10 +
    '  Result := ''Hello, '' + Trim(Name) + ''!'' + #$1F600 + #0;' + #10 +
    'end;' + #10 +
    'begin Caption := Greet(Name) end.' + #10;

type
  { Independent creator definition proves normalization uses exact data. Optional
    reentry tries to remove a live declaration during admission and must refuse
    before any registry replacement. This fixture owns no document or runtime. }
  TForeignDefinition = class(TInterfacedObject, INyxProgramDefinition)
  public
    Original: INyxProgramDefinition;
    Registry: INyxProgramDeclarations;
    Target: TNyxProgramRef;
    Reenter: Boolean;
    RetireInputs: Boolean;
    Data: TNyxDataValue;
    destructor Destroy; override;
    function GetSource: TNyxText;
    function GetContract: TNyxProgramContract;
    function GetLimits: TNyxProgramLimits;
    function Compile: INyxProgram;
    function ToData: TNyxDataValue;
  end;

var
  GChecks: Integer;
  GRetiringDefinition: INyxProgramDefinition;
  GRetiringRegistry: INyxProgramDeclarations;
  GRetiringReference: TNyxProgramRef;
  GDefinitionsDisposed: Integer;

destructor TForeignDefinition.Destroy;
begin
  Inc(GDefinitionsDisposed);
  inherited Destroy;
end;

function TForeignDefinition.GetSource: TNyxText;
begin
  Result := Original.Source;
end;

function TForeignDefinition.GetContract: TNyxProgramContract;
begin

  if RetireInputs then
  begin
    GRetiringDefinition := nil;
    GRetiringRegistry := nil;
    GRetiringReference := NyxProgramRef('Changed by creator callback');
  end;
  Result := Original.Contract;
end;

function TForeignDefinition.GetLimits: TNyxProgramLimits;
begin
  Result := Original.Limits;
end;

function TForeignDefinition.Compile: INyxProgram;
begin
  Result := Original.Compile;
end;

function TForeignDefinition.ToData: TNyxDataValue;
begin

  if RetireInputs then
  begin
    GRetiringDefinition := nil;
    GRetiringRegistry := nil;
    GRetiringReference := NyxProgramRef('Changed by creator callback');
  end;

  if Reenter then
  begin
    Registry.Remove(Target);
  end;
  Result := Data;
end;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Program authoring: ' + AMessage);
  end;
  Inc(GChecks);
end;

function GreetingContract: TNyxProgramContract;
begin
  Result := NyxProgramContract.Input(NyxProgramSymbol('Name'), NyxProgramType(npkText))
    .Output(NyxProgramSymbol('Caption'), NyxProgramType(npkText));
end;

{$ifndef PAS2JS}
procedure WriteExact(const APath, AText: TNyxText);
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[0], Length(LBytes));
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
  LDefinition: INyxProgramDefinition;
  LForeign: TForeignDefinition;
  LForeignLease: INyxProgramDefinition;
  LRegistry: INyxProgramDeclarations;
  LReference: TNyxProgramRef;
  LBefore: TNyxText;
  LSource: TNyxText;
  LLegacy: TNyxText;
  LMetadata: TNyxDataValue;
  LProgram: INyxProgram;
  LFailed: Boolean;
  LIndex: Integer;
  LSession: TNyxStudioSession;
  LAcceptedPair: TNyxText;
  LChangedPair: TNyxText;
  LPendingPair: TNyxText;
  LChange: TNyxProgramChange;
begin
  LDocument := TNyxDocument.Create;
  LCopy := nil;
  LParsed := nil;
  LWorkspace := nil;
  LSession := nil;
  try
    LDocument.Title := 'Pascal behavior project';
    LDocument.AddPage(NewNyxPage('home').Add(NewNyxHeading('title').WithText('Create something useful')));
    LLegacy := TNyxCodec.Encode(LDocument);
    LReference := NyxProgramRef('Greeting 😀');
    LDefinition := NewNyxProgramDefinition(CGreeting, GreetingContract,
      NyxProgramLimits.Steps(20000).Nesting(32));
    LDocument.Programs.Define(LReference, LDefinition);
    LDocument.Programs.Define(NyxProgramRef('Canvas layout'),
      NewNyxProgramDefinition('begin MoveItem(SceneItem(0), 24, 12) end.',
        NyxCanvasProgramContract));
    LDocument.Programs.Define(NyxProgramRef('Not executed on opening'),
      NewNyxProgramDefinition('begin while True do begin end end.', NyxProgramContract,
        NyxProgramLimits.Steps(16)));
    LDocument.Validate;
    LBefore := TNyxCodec.Encode(LDocument);
    Check(TNyxDataValue.ParseJSON(LBefore).Field('version').AsInteger = 15,
      'Programs select version 15 with the complete existing document fields');
    LCopy := TNyxCodec.Decode(LBefore);
    Check(TNyxCodec.Encode(LCopy) = LBefore, 'Portable design round trip retains every exact program');
    Check(LCopy.Programs.Definition(LReference).Source = CGreeting,
      'Supplementary data, mixed line endings and literal NUL escapes retain exact source bytes');
    LCopy.Programs.Remove(LReference);
    Check((LDocument.Programs.Count = 3) and (LCopy.Programs.Count = 2),
      'Document registry membership clones independently');
    FreeAndNil(LCopy);
    LRegistry := LDocument.Programs.Clone;
    LProgram := LRegistry.Definition(LReference).Compile;
    Check(LProgram.Run(NyxProgramValues.WithValue(NyxProgramSymbol('Name'),
      NyxProgramValue('Ada'))).Outputs.Value(NyxProgramSymbol('Caption')).AsText =
      TNyxText('Hello, Ada!😀') + #0, 'Explicit execution uses independent typed inputs and exact Unicode outputs');
    LFailed := False;
    try
      LRegistry.Definition(NyxProgramRef('Not executed on opening')).Compile.Run(NyxProgramValues);
    except
      on E: ENyxProgram do
      begin
        LFailed := E.Diagnostic.Kind = npdLimit;
      end;
    end;
    Check(LFailed, 'Opening/clone/source admission did not execute an infinite program; explicit invocation is bounded');
    LFailed := False;
    try
      LRegistry.Definition(NyxProgramRef('Canvas layout')).Compile.Run(NyxProgramValues);
    except
      on E: ENyxProgram do
      begin
        LFailed := E.Diagnostic.Kind = npdCapability;
      end;
    end;
    Check(LFailed, 'Saved host declarations never grant runtime authority');

    LSource := TNyxCodegen.Generate(LDocument);
    Check((Pos('Result.Programs.Define(NyxProgramRef(', LSource) > 0) and
      (Pos('NewNyxProgramDefinition(', LSource) > 0) and
      (Pos('NyxCanvasProgramContract', LSource) > 0) and
      (Pos('.Steps(20000)', LSource) > 0) and
      (Pos('.Input(NyxProgramSymbol(', LSource) > 0),
      'Crafted source uses typed declarations, specialized canvas vocabulary and caller limits');
    Check(Pos('NyxProgramDefinitionFromData(', LSource) = 0,
      'Default authoring does not serialize program behavior as a JSON configuration dump');
    LParsed := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LParsed) = LBefore, 'The literal reader reconstructs the complete program document');
    Check(LWorkspace.Render(LParsed) = LSource, 'No-op program source reconstruction preserves exact bytes');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);

    LSession := TNyxStudioSession.Create(NyxProjectPair(LBefore, LSource));
    LAcceptedPair := EncodeNyxProject(LSession.ProjectSnapshot);
    LSession.ApplyPatch(NyxProgramPatch([
      NyxDefineProgram(LReference, NewNyxProgramDefinition(
        'begin Caption := ''Welcome, '' + Name end.', GreetingContract)),
      NyxRemoveProgram(NyxProgramRef('Not executed on opening'))]));
    LChangedPair := EncodeNyxProject(LSession.ProjectSnapshot);
    Check((LChangedPair <> LAcceptedPair) and (LSession.Document.Programs.Count = 2),
      'One grouped program command publishes the complete design/Pascal pair');
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LAcceptedPair,
      'One Undo restores the exact pair before grouped program replacement/removal');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LChangedPair,
      'One Redo restores the exact changed program pair');
    LSession.SetSourceDraft(LSession.Source + #10 + '// unfinished companion');
    LPendingPair := EncodeNyxProject(LSession.ProjectSnapshot);
    LFailed := False;
    try
      LSession.ApplyPatch(NyxProgramPatch([NyxRemoveProgram(LReference)]));
    except
      on E: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (EncodeNyxProject(LSession.ProjectSnapshot) = LPendingPair) and
      LSession.SourceDraftPending, 'Pending companion draft refuses program mutation and preserves accepted history');
    FreeAndNil(LSession);
    LFailed := False;
    try
      LParsed := TNyxSourceWorkspace.PrepareDraft(StringReplace(LSource,
        'NyxProgramType(npkText)', 'NyxProgramType(''text'')', []), LWorkspace);
    except
      on E: ENyxSource do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'Raw strings cannot replace a typed program kind or mutate the accepted document');
    FreeAndNil(LParsed);
    FreeAndNil(LWorkspace);

    LForeign := TForeignDefinition.Create;
    LForeignLease := LForeign;
    LForeign.Original := LDefinition;
    LForeign.Registry := LRegistry;
    LForeign.Target := LReference;
    LForeign.Data := LDefinition.ToData;
    LForeign.Reenter := True;
    LMetadata := LRegistry.ToData;
    LFailed := False;
    try
      LRegistry.Define(LReference, LForeignLease);
    except
      on E: ENyxProgram do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LRegistry.ToData.ToJSON = LMetadata.ToJSON),
      'A foreign metadata callback cannot reenter registry mutation');
    LForeign.Reenter := False;
    LForeign.Data := NyxObject([
      NyxField('version', NyxData(NyxProgramLanguageVersion)),
      NyxField('source', NyxData('begin invalid := True end.')),
      NyxField('contract', GreetingContract.ToData),
      NyxField('limits', NyxProgramLimits.ToData)]);
    LFailed := False;
    try
      LRegistry.Define(LReference, LForeignLease);
    except
      on E: ENyxProgram do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (LRegistry.ToData.ToJSON = LMetadata.ToJSON),
      'Malformed foreign program data refuses before replacing accepted membership');

    LForeign := TForeignDefinition.Create;
    GRetiringDefinition := LForeign;
    LForeign.Original := LDefinition;
    LForeign.Data := LDefinition.ToData;
    LForeign.RetireInputs := True;
    GRetiringReference := NyxProgramRef('Captured before creator');
    GRetiringRegistry := NewNyxProgramDeclarations;
    GDefinitionsDisposed := 0;
    LRegistry := GRetiringRegistry.Define(GRetiringReference, GRetiringDefinition);
    Check((GRetiringDefinition = nil) and (GRetiringRegistry = nil) and
      (GDefinitionsDisposed = 1) and
      LRegistry.Contains(NyxProgramRef('Captured before creator')) and
      not LRegistry.Contains(GRetiringReference),
      'Admission retains both interfaces and the exact original name through caller retirement');
    LForeign := TForeignDefinition.Create;
    GRetiringDefinition := LForeign;
    LForeign.Original := LDefinition;
    LForeign.Data := LDefinition.ToData;
    LForeign.RetireInputs := True;
    GRetiringReference := NyxProgramRef('Captured edit name');
    GDefinitionsDisposed := 0;
    LChange := NyxDefineProgram(GRetiringReference, GRetiringDefinition);
    Check((GRetiringDefinition = nil) and (GDefinitionsDisposed = 1) and
      (LChange.Reference.Name = 'Captured edit name') and
      (LChange.ToData.Field('definition').ToJSON = LDefinition.ToData.ToJSON),
      'Typed edit capture retains the creator and copied name through callback retirement');
    LForeign := TForeignDefinition.Create;
    GRetiringDefinition := LForeign;
    LForeign.Original := LDefinition;
    LForeign.Data := LDefinition.ToData;
    LForeign.RetireInputs := True;
    GDefinitionsDisposed := 0;
    LChangedPair := TNyxCodegen.ProgramDefinition(GRetiringDefinition);
    Check((GRetiringDefinition = nil) and (GDefinitionsDisposed = 1) and
      (LChangedPair = TNyxCodegen.ProgramDefinition(LDefinition)),
      'Typed source formatting retains a creator across every getter');

    LRegistry := NewNyxProgramDeclarations;
    for LIndex := 1 to NyxMaximumProgramDefinitions do
    begin
      LRegistry.Define(NyxProgramRef('Program ' + TNyxText(IntToStr(LIndex))), LDefinition);
    end;
    LMetadata := LRegistry.ToData;
    LFailed := False;
    try
      LRegistry.Define(NyxProgramRef('One too many'), LDefinition);
    except
      on E: ENyxProgram do
      begin
        LFailed := E.Diagnostic.Kind = npdLimit;
      end;
    end;
    Check(LFailed and (LRegistry.ToData.ToJSON = LMetadata.ToJSON),
      'Membership limits preserve the complete registry on refusal');
    Check(NyxProgramDeclarationsFromData(LMetadata).ToData.ToJSON = LMetadata.ToJSON,
      'Every admitted exported registry can be imported with the same budgets');
    LFailed := False;
    try
      NyxProgramDeclarationsFromData(NyxArray([LMetadata.Item(0), LMetadata.Item(0)]));
    except
      on E: ENyxProgram do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Duplicate names refuse on the persistence boundary');

    {$ifndef PAS2JS}

    if ParamCount > 0 then
    begin
      ForceDirectories(ParamStr(1));
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas', LSource);
      WriteExact(IncludeTrailingPathDelimiter(ParamStr(1)) + 'design.nyx', LBefore);
    end;
    {$endif}
    LDocument.Programs.Remove(LReference);
    LDocument.Programs.Remove(NyxProgramRef('Canvas layout'));
    LDocument.Programs.Remove(NyxProgramRef('Not executed on opening'));
    Check(TNyxCodec.Encode(LDocument) = LLegacy,
      'Removing all programs restores the exact prior legacy design encoding');
  finally
    LSession.Free;
    LWorkspace.Free;
    LParsed.Free;
    LCopy.Free;
    LDocument.Free;
  end;
  Check(LDefinition.Source = CGreeting, 'Independent definitions outlive source/document retirement');
end;

procedure RunEditor;
var
  LDocument: TNyxDocument;
  LForm: INyxCard;
  LOther: INyxCard;
  LDraft: TNyxProgramEditorDraft;
  LCapture: TNyxProgramEditorCapture;
  LStaleCapture: TNyxProgramEditorCapture;
  LReference: TNyxProgramRef;
  LPresentation: TNyxStudioPresentation;
  LPacket: TNyxText;
  LRefused: Boolean;
begin
  LDocument := TNyxDocument.Create;
  try
    LReference := NyxProgramRef('Greeting 😀');
    LDocument.Programs.Define(LReference, NewNyxProgramDefinition(CGreeting, GreetingContract,
      NyxProgramLimits.Nesting(23).Proposals(19)));
    LForm := NewNyxProgramEditor('program', LDocument, LReference);
    LPacket := TNyxCodec.Encode(LDocument);
    Check(LForm.Node.Find('program-source').Prop('value') = CGreeting,
      'Public editor copies exact accepted source, including mixed line endings');
    LForm.Node.Find('program-source').Configure.Value('begin broken end.').Done;
    Check(HandleNyxProgramEditor(LForm.Node.Find('program-check'), LForm.Node) and
      (Pos('Line ', LForm.Node.Find('program-feedback').Prop('text')) = 1) and
      (TNyxCodec.Encode(LDocument) = LPacket),
      'Local Check exposes structured source diagnostics without publishing or executing');
    LForm.Node.Find('program-depth').Configure.Value('unfinished budget').Done;
    LDraft.Capture('program', LForm.Node);
    LDraft := TNyxProgramEditorDraft.FromData(LDraft.ToData);
    LOther := NewNyxProgramEditor('program', LDocument, LReference);
    Check(LDraft.Restore('program', LOther.Node) and
      (LOther.Node.Find('program-depth').Prop('value') = 'unfinished budget') and
      (LOther.Node.Find('program-source').Prop('value') = 'begin broken end.'),
      'Invalid source and numeric proposals round trip independently from accepted definitions');
    LRefused := False;
    try
      OpenNyxProgramEditorChoice(LOther.Node.Find('program-new'), LOther.Node, LReference);
    except
      on E: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Dirty proposals refuse navigation instead of silently losing work');
    HandleNyxProgramEditor(LOther.Node.Find('program-restore'), LOther.Node);
    Check(LOther.Node.Find('program-source').Prop('value') = CGreeting,
      'Explicit Restore resets only the local proposal');
    LOther.Node.Find('program-port-name').Configure.Value('Enabled').Done;
    LOther.Node.Find('program-port-kind').Configure.Value('Boolean').Done;
    HandleNyxProgramEditor(LOther.Node.Find('program-add-port'), LOther.Node);
    LOther.Node.Find('program-source').Configure.Value(
      'begin Caption := Name; if Enabled then Caption := Caption + ''!'' end.').Done;
    CaptureNyxProgramEditor(LOther.Node.Find('program-apply'), LOther.Node, LCapture, LDocument);
    LCapture.Change.Apply(LDocument);
    Check((LCapture.Action = npeaDefine) and
      (LDocument.Programs.Definition(LCapture.Reference).Contract.PortCount = 3) and
      (LDocument.Programs.Definition(LCapture.Reference).Limits.MaximumNesting = 23) and
      (LDocument.Programs.Definition(LCapture.Reference).Limits.MaximumProposals = 19),
      'Typed parameter proposals publish with all unexposed limits retained');
    LRefused := False;
    try
      CaptureNyxProgramEditor(LOther.Node.Find('program-apply'), LOther.Node, LStaleCapture, LDocument);
    except
      on E: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'An older form refuses replacement after the exact accepted definition changes');
    Check(not LDraft.Restore('program', NewNyxProgramEditor('program', LDocument,
      LCapture.Reference).Node), 'A changed accepted definition refuses an obsolete local draft');
    LOther := NewNyxProgramEditor('program', LDocument, LCapture.Reference);
    LRefused := False;
    try
      CaptureNyxProgramEditor(LOther.Node.Find('program-remove'), LOther.Node, LCapture);
    except
      on E: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LDocument.Programs.Count = 1),
      'Removal needs explicit confirmation and cannot silently retire a program');
    LOther.Node.Find('program-confirm').Configure.Value(True).Done;
    CaptureNyxProgramEditor(LOther.Node.Find('program-remove'), LOther.Node, LCapture);
    Check(LCapture.Action = npeaRemove, 'Confirmed removal captures a typed command only');
    LDocument.Programs.Define(NyxProgramRef('Program'),
      NewNyxProgramDefinition('begin end.', NyxProgramContract));
    LForm := NewNyxProgramEditor('fresh-program', LDocument, Default(TNyxProgramRef));
    Check(LForm.Node.Find('fresh-program-name').Prop('value') = 'Program 2',
      'New program forms choose an unused default name');
    LForm.Node.Find('fresh-program-name').Configure.Value('Program').Done;
    LPacket := TNyxCodec.Encode(LDocument);
    LRefused := False;
    try
      CaptureNyxProgramEditor(LForm.Node.Find('fresh-program-apply'), LForm.Node,
        LStaleCapture, LDocument);
    except
      on E: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LPacket),
      'A new/renamed proposal cannot silently overwrite a different saved program');
    LPresentation := DefaultNyxStudioPresentation;
    LPacket := EncodeNyxStudioPresentation(LPresentation);
    Check((TNyxDataValue.ParseJSON(LPacket).Field('version').AsInteger = 16) and
      (EncodeNyxStudioPresentation(DecodeNyxStudioPresentation(LPacket)) = LPacket),
      'Unused program authoring preserves existing version-16 presentation bytes');
    LPresentation.ProgramsVisible := True;
    LPresentation.ProgramReference := LCapture.Reference;
    LPresentation.ProgramDraft := LDraft;
    LPacket := EncodeNyxStudioPresentation(LPresentation);
    LPresentation := DecodeNyxStudioPresentation(LPacket);
    Check((TNyxDataValue.ParseJSON(LPacket).Field('version').AsInteger = 18) and
      LPresentation.ProgramsVisible and LPresentation.ProgramDraft.Defined and
      (EncodeNyxStudioPresentation(LPresentation) = LPacket),
      'Optional version-18 presentation retains the exact independent program draft');
  finally
    LOther := nil;
    LForm := nil;
    LDocument.Free;
  end;
end;

procedure RunSemantic;
var
  LDocument: TNyxDocument;
  LAgent: TNyxAgentSession;
  LPair: TNyxProjectPair;
  LArguments: TNyxDataValue;
  LResponse: TNyxDataValue;
  LDefinition: INyxProgramDefinition;
  LRevision: Integer;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LRefused: Boolean;
begin
  LDocument := TNyxDocument.Create;
  LAgent := nil;
  try
    LDocument.AddPage(NewNyxPage('home'));
    LDefinition := NewNyxProgramDefinition(CGreeting, GreetingContract);
    LDocument.Programs.Define(NyxProgramRef('Greeting 😀'), LDefinition);
    LDocument.Programs.Define(NyxProgramRef('Infinite but not invoked'),
      NewNyxProgramDefinition('begin while True do begin end end.', NyxProgramContract,
        NyxProgramLimits.Steps(4)));
    LDocument.Programs.Define(NyxProgramRef('Canvas layout'), NewNyxProgramDefinition(
      'begin MoveItem(SceneItem(0), 24, 12) end.', NyxCanvasProgramContract));
    LPair := NyxProjectPair(TNyxCodec.Encode(LDocument), TNyxCodegen.Generate(LDocument));
    LAgent := TNyxAgentSession.Create(LPair);
    LAgent.InheritPermission(apEdit);
    LRevision := LAgent.Revision;
    LResponse := LAgent.Call('nyx_logic', 'Program review', NyxObject([
      NyxField('scope', NyxData('programs')), NyxField('limit', NyxData(1)),
      NyxField('expectedRevision', NyxData(LRevision))]));
    Check((LResponse.Field('total').AsInteger = 3) and (LResponse.Field('items').Count = 1) and
      (Pos('source', LResponse.ToJSON) = 0), 'Semantic discovery pages program facts without source dumps or execution');
    LResponse := LAgent.Call('nyx_logic', 'Program review', NyxObject([
      NyxField('scope', NyxData('program-source')), NyxField('program', NyxData('Greeting 😀')),
      NyxField('textLimit', NyxData(30)), NyxField('expectedRevision', NyxData(LRevision))]));
    Check(LResponse.Field('truncated').AsBoolean and
      (Pos('😀', LResponse.Field('text').AsText) > 0), 'Semantic source windows retain supplementary Unicode scalars');
    LResponse := LAgent.Call('nyx_logic', 'Program review', NyxObject([
      NyxField('scope', NyxData('program-ports')), NyxField('program', NyxData('Greeting 😀')),
      NyxField('limit', NyxData(1))]));
    Check((LResponse.Field('items').Count = 1) and (LResponse.Field('total').AsInteger = 2) and
      (LResponse.Field('items').Item(0).Field('direction').AsText = 'input'),
      'Semantic contract ports use bounded typed direction/type facts');
    LResponse := LAgent.Call('nyx_logic', 'Program review', NyxObject([
      NyxField('scope', NyxData('program-routine-arguments')), NyxField('program', NyxData('Canvas layout')),
      NyxField('routine', NyxData('MoveItem')), NyxField('limit', NyxData(1))]));
    Check((LResponse.Field('items').Count = 1) and (LResponse.Field('total').AsInteger = 3),
      'Host routine arguments page separately from signatures');
    LResponse := LAgent.Call('nyx_logic', 'Program review', NyxObject([
      NyxField('scope', NyxData('program-capabilities')), NyxField('program', NyxData('Canvas layout'))]));
    Check(LResponse.Field('total').AsInteger = 2,
      'Required capabilities remain declarative inspection without grants');
    LResponse := LAgent.Call('nyx_logic', 'Program review', NyxObject([
      NyxField('scope', NyxData('program-check')), NyxField('definition', NyxObject([
        NyxField('version', NyxData(1)), NyxField('source', NyxData('begin Missing := 1 end.')),
        NyxField('contract', GreetingContract.ToData), NyxField('limits', NyxProgramLimits.ToData)]))]));
    Check(not LResponse.Field('succeeded').AsBoolean and
      (LResponse.Field('items').Item(0).Field('line').AsInteger = 1) and
      (LAgent.Revision = LRevision), 'Proposed program diagnostics have exact spans and never publish');
    LBefore := EncodeNyxProject(LAgent.PreviewPair(LRevision, 'home'));
    LArguments := NyxObject([NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('program-group')),
      NyxField('operations', NyxArray([
        NyxDefineProgram(NyxProgramRef('Copy'), LDefinition).ToData,
        NyxRemoveProgram(NyxProgramRef('Infinite but not invoked')).ToData]))]);
    LAgent.Call('nyx_transaction', 'Program review', LArguments);
    Check(LAgent.Revision = LRevision + 1, 'Grouped semantic program changes publish one revision');
    LAfter := EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home'));
    LAgent.Call('nyx_transaction', 'Program review', LArguments);
    Check((LAgent.Revision = LRevision + 1) and
      (EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = LAfter),
      'Exact semantic retry duplicates neither program definitions nor history');
    LRefused := False;
    try
      LAgent.Call('nyx_logic', 'Program review', NyxObject([
        NyxField('scope', NyxData('programs')), NyxField('expectedRevision', NyxData(LRevision))]));
    except
      on E: Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Paged queries refuse an obsolete expected revision');
    LAgent.Call('nyx_history', 'Program review', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('program-undo')),
      NyxField('direction', NyxData('undo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = LBefore,
      'Semantic Undo restores exact source and program metadata as one pair');
    LAgent.Call('nyx_history', 'Program review', NyxObject([
      NyxField('expectedRevision', NyxData(LAgent.Revision)), NyxField('operationId', NyxData('program-redo')),
      NyxField('direction', NyxData('redo'))]));
    Check(EncodeNyxProject(LAgent.PreviewPair(LAgent.Revision, 'home')) = LAfter,
      'Semantic Redo restores exact source and program metadata together');
    {$ifndef PAS2JS}
    Check((Pos('program-check', NyxStudioMCPTools.ToJSON) > 0) and
      (Pos('program-define', NyxStudioMCPTools.ToJSON) > 0),
      'Current MCP schemas advertise the bounded query and paired mutation contract');
    {$endif}
  finally
    LAgent.Free;
    LDocument.Free;
  end;
end;

begin
  GChecks := 0;
  try
    Run;
    RunEditor;
    RunSemantic;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-program-authoring', 'passed');
    document.body.setAttribute('data-program-authoring-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' program declaration/source checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-program-authoring', 'failed');
      document.body.setAttribute('data-program-authoring-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
