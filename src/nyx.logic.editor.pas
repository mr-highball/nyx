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
unit nyx.logic.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls,
  nyx.flow.types, nyx.flow, nyx.flow.declarations, nyx.flow.editing,
  nyx.logic, nyx.logic.catalog;

type
  { Open is presentation only. Change contains an exact guarded proposal; the
    owner publishes it through its ordinary revision-aware paired command. }
  TNyxLogicEditorCapture = record
    Open: Boolean;
    Reference: TNyxFlowRef;
    Change: TNyxFlowChange;
  end;
  { Independent unfinished controls, including malformed creator JSON/numbers.
    Restoration requires the exact selected graph baseline. No tree is retained. }
  TNyxLogicEditorDraft = record
  private
    FContext: TNyxText;
    FValues: TNyxDataValue;
    FPath: TNyxDataValue;
    FDescriptor: TNyxDataValue;
    function GetDefined: Boolean;
  public
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode);
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    procedure Clear;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxLogicEditorDraft; static;
    property Defined: Boolean read GetDefined;
  end;

{ Public compound Nyx form: discovery/help, typed built-in configuration, saved
  steps/ports and keyboard-operable wire choices. No document/operation is retained.
  An optional immutable creator catalog supplies discovery/default configuration;
  creator JSON is explicitly a configuration-data extension boundary. }
function NewNyxLogicEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxFlowRef; const ACatalog: INyxLogicCatalog = nil): INyxCard;
{ Handles local search/grouping, configuration loading and typed path construction.
  True requests ordinary renderer Sync, never document history or a remount. }
function HandleNyxLogicEditor(ANode, ARoot: TNyxNode): Boolean;
{ Capture belongs to the exact mounted tree. Removal needs explicit warning
  acknowledgement. Invalid configuration raises and leaves every input intact. }
function CaptureNyxLogicEditor(ANode, ARoot: TNyxNode;
  out ACapture: TNyxLogicEditorCapture): Boolean;

implementation

const
  CEditor = 'nyx.logic-editor';
  CPath = 'nyx.logic-path';
  CDescriptor = 'nyx.logic-active-descriptor';
  CFields: array[0..16] of TNyxText =
    ('graph', 'graph-name', 'step-name', 'operation', 'step', 'source', 'target',
     'layout', 'indent', 'separator', 'path-part', 'path-kind', 'creator-config',
     'search', 'group', 'grouped', 'confirm');

function FormFor(ANode, ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if (ANode = nil) or (ARoot = nil) or (ARoot.Find(ANode.ID) <> ANode) then
  begin
    Exit;
  end;
  Result := ANode;
  while (Result <> nil) and not Result.Extensions.Has(NyxExtension(CEditor)) do
  begin
    Result := Result.Parent;
  end;
end;

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxFlow.Create('Logic editor field is unavailable');
  end;
end;

function Value(AForm: TNyxNode; const AName: TNyxText): TNyxText;
begin
  Result := Field(AForm, AName).Prop('value');
end;

procedure SetValue(AForm: TNyxNode; const AName, AValue: TNyxText);
begin
  Field(AForm, AName).Configure.Value(AValue).Done;
end;

{ Built-in configuration presents only its typed controls. Creator JSON is an
  explicit extension-data editor, never the default way to configure a builtin. }
procedure ShowConfiguration(AForm: TNyxNode; const AKind: TNyxText);
begin
  Field(AForm, 'layout').Configure.Visible(AKind = 'json-format').Done;
  Field(AForm, 'indent').Configure.Visible(AKind = 'json-format').Done;
  Field(AForm, 'separator').Configure.Visible(AKind = 'text-join').Done;
  Field(AForm, 'path-kind').Configure.Visible(AKind = 'json-select').Done;
  Field(AForm, 'path-part').Configure.Visible(AKind = 'json-select').Done;
  Field(AForm, 'path-add').Configure.Visible(AKind = 'json-select').Done;
  Field(AForm, 'path-clear').Configure.Visible(AKind = 'json-select').Done;
  Field(AForm, 'path-preview').Configure.Visible(AKind = 'json-select').Done;
  Field(AForm, 'creator-config').Configure.Visible((AKind <> 'json-parse') and
    (AKind <> 'json-select') and (AKind <> 'json-format') and (AKind <> 'text-join')).Done;
end;

function OperationData(const ADescriptor: TNyxLogicDescriptor;
  const AConfiguration: TNyxDataValue): TNyxDataValue;
begin
  Result := NyxObject([NyxField('descriptor', ADescriptor.ToData),
    NyxField('configuration', AConfiguration)]);
end;

function Entry(AForm: TNyxNode; const AKind: TNyxText): TNyxDataValue;
var
  LCatalog: TNyxDataValue;
  LIndex: Integer;
begin
  LCatalog := AForm.Extensions.Value(NyxExtension(CEditor)).Field('catalog');
  for LIndex := 0 to LCatalog.Count - 1 do
  begin
    Result := LCatalog.Item(LIndex);

    if Result.Field('descriptor').Field('kind').AsText = AKind then
    begin
      Exit;
    end;
  end;
  raise ENyxFlow.Create('Choose an operation from this Logic collection');
end;

procedure LoadConfiguration(AForm: TNyxNode; const AOperation: TNyxDataValue);
var
  LKind: TNyxText;
  LConfiguration: TNyxDataValue;
begin
  LKind := AOperation.Field('descriptor').Field('kind').AsText;
  LConfiguration := AOperation.Field('configuration');
  AForm.Extensions.SetValue(NyxExtension(CDescriptor), AOperation.Field('descriptor'));
  SetValue(AForm, 'operation', LKind);

  if LKind = 'json-format' then
  begin
    SetValue(AForm, 'layout', 'Indented');

    if LConfiguration.Field('layout').AsInteger = Ord(njlCompact) then
    begin
      SetValue(AForm, 'layout', 'Compact');
    end;
    SetValue(AForm, 'indent', LConfiguration.Field('indent').ToJSON);
  end
  else if LKind = 'text-join' then
  begin
    SetValue(AForm, 'separator', LConfiguration.Field('separator').AsText);
  end
  else if LKind = 'json-select' then
  begin
    AForm.Extensions.SetValue(NyxExtension(CPath), LConfiguration.Field('path'));
    Field(AForm, 'path-preview').Configure.Text(LConfiguration.Field('path').ToJSON).Done;
  end;
  SetValue(AForm, 'creator-config', LConfiguration.ToJSON);
  ShowConfiguration(AForm, LKind);
end;

function ConfiguredOperation(AForm: TNyxNode): INyxLogicOperation;
var
  LKind: TNyxText;
  LEntry: TNyxDataValue;
  LIndent: Integer;
  LLayout: TNyxJSONLayout;
  LDescriptor: TNyxDataValue;
begin
  LKind := Value(AForm, 'operation');
  LEntry := Entry(AForm, LKind);

  if LKind = 'json-parse' then
  begin
    Result := NewNyxJSONParse;
  end
  else if LKind = 'json-format' then
  begin
    LLayout := njlIndented;

    if Value(AForm, 'layout') = 'Compact' then
    begin
      LLayout := njlCompact;
    end
    else if Value(AForm, 'layout') <> 'Indented' then
    begin
      raise ENyxFlow.Create('Choose a typed JSON layout');
    end;

    if not TryStrToInt(Value(AForm, 'indent'), LIndent) then
    begin
      raise ENyxFlow.Create('Indent must be an integer from 1 to 8');
    end;
    Result := NewNyxJSONFormat(LLayout, LIndent);
  end
  else if LKind = 'json-select' then
  begin
    Result := NewNyxJSONSelect(TNyxDataPath.FromData(
      AForm.Extensions.Value(NyxExtension(CPath))));
  end
  else if LKind = 'text-join' then
  begin
    Result := NewNyxTextJoin(Value(AForm, 'separator'));
  end
  else
  begin
    LDescriptor := AForm.Extensions.Value(NyxExtension(CDescriptor));

    if (LDescriptor.Kind <> ndObject) or (LDescriptor.Field('kind').AsText <> LKind) then
    begin
      LDescriptor := LEntry.Field('descriptor');
    end;
    Result := NewNyxLogicDeclaration(
      TNyxLogicDescriptor.FromData(LDescriptor),
      TNyxDataValue.ParseJSON(Value(AForm, 'creator-config')));
  end;
end;

function NewNyxLogicEditor(const AID: TNyxText; ADocument: TNyxDocument;
  const AReference: TNyxFlowRef; const ACatalog: INyxLogicCatalog): INyxCard;
var
  LCatalog: INyxLogicCatalog;
  LEntries: array of TNyxDataValue;
  LRegistry: INyxFlowDeclarations;
  LNames: array of TNyxDataValue;
  LEndpoints: array of TNyxDataValue;
  LDefinition: INyxFlowDefinition;
  LDescriptor: TNyxLogicDescriptor;
  LPort: TNyxFlowPort;
  LBaseline: TNyxDataValue;
  LIndex: Integer;
  LPart: Integer;
  LCount: Integer;
  LGraphNames: TNyxText;
  LKinds: TNyxText;
  LStepNames: TNyxText;
  LSources: TNyxText;
  LTargets: TNyxText;
  LLabel: TNyxText;
  LGroup: TNyxLogicGroup;
  LColumn: INyxColumn;
  LCard: INyxCard;
  LGroupColumn: INyxColumn;

  procedure Section(const AName, ACaption: TNyxText; const ANames: array of TNyxText);
  var
    LSection: INyxDisclosure;
    LNode: TNyxNode;
    LPartIndex: Integer;
    LChildIndex: Integer;
  begin
    LSection := NewNyxDisclosure(AID + '-' + AName).WithText(ACaption);
    LSection.Configure.Value(True).Surface(False).Padding(0).Gap(10).Done;
    Result.Add(LSection);
    for LPartIndex := 0 to High(ANames) do
    begin
      LNode := Result.Node.Find(AID + '-' + ANames[LPartIndex]);

      if (LNode = nil) or (LNode.Parent <> Result.Node) then
      begin
        raise ENyxFlow.Create('Logic section must adopt its exact independently owned controls');
      end;
      for LChildIndex := 0 to Result.Node.Count - 1 do
      begin

        if Result.Node.Children[LChildIndex] = LNode then
        begin
          Result.Node.Extract(LChildIndex);
          Break;
        end;
      end;
      LSection.BodyColumn.Node.Add(LNode);
    end;
  end;

  procedure Input(const AName, ACaption, AValue: TNyxText);
  begin
    Result.Add(NewNyxInput(AID + '-' + AName).Configure.Text(ACaption).Value(AValue).Done);
  end;

  procedure Choice(const AName, ACaption, AItems, AValue: TNyxText);
  begin
    Result.Add(NewNyxSelect(AID + '-' + AName).Configure.Text(ACaption)
      .Items(AItems).Value(AValue).Done);
  end;

  procedure Button(const AName, ACaption: TNyxText);
  begin
    Result.Add(NewNyxButton(AID + '-' + AName).WithText(ACaption));
  end;

begin

  if ADocument = nil then
  begin
    raise ENyxFlow.Create('Logic authoring requires a document');
  end;
  { Snapshot document input before calling an alternate creator catalog. Its
    getters may retire the caller's document; the form needs only owned values. }
  LRegistry := ADocument.Flows.Clone;
  LCatalog := ACatalog;

  if LCatalog = nil then
  begin
    LCatalog := NewNyxLogicCatalog;
  end;
  LDefinition := nil;
  LBaseline := NyxObject([NyxField('exists', NyxData(False)), NyxField('definition', NyxNull)]);

  if AReference.Defined then
  begin
    LDefinition := LRegistry.Definition(AReference);
    LBaseline := NyxFlowBaseline(LRegistry, AReference);
    { Imported creator declarations remain discoverable without manufacturing
      executable implementations. Retain the host's catalog entry when present. }
    for LIndex := 0 to LDefinition.StepCount - 1 do
    begin
      LDescriptor := LDefinition.Descriptor(LIndex);
      LPart := 0;
      while (LPart < LCatalog.Count) and
        (LCatalog.Descriptor(LPart).Kind.Name <> LDescriptor.Kind.Name) do
      begin
        Inc(LPart);
      end;

      if LPart = LCatalog.Count then
      begin
        LCatalog := LCatalog.WithOperation(NewNyxLogicDeclaration(LDescriptor,
          LDefinition.Configuration(LIndex)));
      end;
    end;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(16).Gap(12).Done;
  LGraphNames := '';
  SetLength(LNames, LRegistry.Count);
  for LIndex := 0 to LRegistry.Count - 1 do
  begin
    LNames[LIndex] := NyxData(LRegistry.Reference(LIndex).Name);

    if LIndex > 0 then
    begin
      LGraphNames := LGraphNames + #10;
    end;
    LGraphNames := LGraphNames + LRegistry.Reference(LIndex).Name;
  end;
  Result.Add(NewNyxHeading(AID + '-title').WithText('Logic'));
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Compose reusable operations with typed connections. Each applied change is undoable.'));
  Choice('graph', 'Saved graph', LGraphNames, AReference.Name);
  Button('open', 'Open graph');
  Input('graph-name', 'New graph name', 'My tool');
  Button('create', 'Create graph with JSON parse');
  Input('search', 'Find an operation', '');
  Choice('group', 'Intent', 'All groups' + #10 + 'JSON' + #10 + 'Text' + #10 +
    'Files' + #10 + 'Collections' + #10 + 'Network' + #10 + 'Data' + #10 + 'Flow', 'All groups');
  Choice('grouped', 'Toolbox view', 'List' + #10 + 'Grouped', 'List');
  SetLength(LEntries, LCatalog.Count);
  LKinds := '';
  for LIndex := 0 to LCatalog.Count - 1 do
  begin
    LDescriptor := LCatalog.Descriptor(LIndex);
    LEntries[LIndex] := OperationData(LDescriptor, LCatalog.Configuration(LIndex));

    if LIndex > 0 then
    begin
      LKinds := LKinds + #10;
    end;
    LKinds := LKinds + LDescriptor.Kind.Name;
  end;
  LColumn := NewNyxColumn(AID + '-collection');
  LColumn.Configure.Gap(10).Done;
  Result.Add(LColumn);
  for LGroup := Low(TNyxLogicGroup) to High(TNyxLogicGroup) do
  begin
    LGroupColumn := NewNyxColumn(AID + '-group-' + TNyxText(IntToStr(Ord(LGroup))));
    LGroupColumn.Configure.Gap(10).Done;
    LGroupColumn.Add(NewNyxHeading(LGroupColumn.Node.ID + '-heading')
      .Configure.Text(NyxLogicGroupName(LGroup)).Visible(False).Done);
    for LIndex := 0 to LCatalog.Count - 1 do
    begin
      LDescriptor := LCatalog.Descriptor(LIndex);

      if LDescriptor.Group <> LGroup then
      begin
        Continue;
      end;
      LCard := NewNyxCard(AID + '-tool-' + TNyxText(IntToStr(LIndex)));
      LCard.Configure.Padding(12).Gap(8).Done;
      LCard.Node.Extensions.SetValue(NyxExtension('nyx.logic-kind'), NyxData(LDescriptor.Kind.Name));
      LCard.Add(NewNyxLabel(LCard.Node.ID + '-title').WithText(LDescriptor.Title));
      LCard.Add(NewNyxLabel(LCard.Node.ID + '-help').WithText(LDescriptor.Description));
      LCard.Add(NewNyxButton(LCard.Node.ID + '-use').WithText('Configure'));
      LGroupColumn.Add(LCard);
    end;
    LGroupColumn.Configure.Visible(LGroupColumn.Node.Count > 1).Done;
    LColumn.Add(LGroupColumn);
  end;
  Input('step-name', 'New step name', 'step');
  Choice('operation', 'Operation', LKinds, 'json-parse');
  Button('defaults', 'Load operation defaults');
  Choice('layout', 'JSON layout', 'Indented' + #10 + 'Compact', 'Indented');
  Input('indent', 'Indent width', '2');
  Input('separator', 'Join separator', '');
  Choice('path-kind', 'Path part', 'Field' + #10 + 'Item', 'Field');
  Input('path-part', 'Field name or zero-based item index', '');
  Button('path-add', 'Append path part');
  Button('path-clear', 'Use JSON root');
  Result.Add(NewNyxCode(AID + '-path-preview').WithText('[]'));
  Result.Add(NewNyxMemo(AID + '-creator-config').Configure
    .Text('Creator configuration JSON (extension data)').Value('{}').Done);
  Button('add', 'Add step');
  LStepNames := '';
  LSources := '';
  LTargets := '';
  LCount := 0;
  LEndpoints := nil;

  if LDefinition <> nil then
  begin
    for LIndex := 0 to LDefinition.StepCount - 1 do
    begin

      if LIndex > 0 then
      begin
        LStepNames := LStepNames + #10;
      end;
      LStepNames := LStepNames + LDefinition.Step(LIndex).Name;
      LDescriptor := LDefinition.Descriptor(LIndex);
      LCard := NewNyxCard(AID + '-step-card-' + TNyxText(IntToStr(LIndex)));
      LCard.Configure.Padding(12).Gap(8).Done;
      LCard.Add(NewNyxLabel(LCard.Node.ID + '-title').WithText(
        LDefinition.Step(LIndex).Name + ' / ' + LDescriptor.Title));
      LCard.Add(NewNyxLabel(LCard.Node.ID + '-help').WithText(LDescriptor.Description));
      for LPart := 0 to LDescriptor.PortCount - 1 do
      begin
        LPort := LDescriptor.Port(LPart);
        LLabel := TNyxText(IntToStr(LCount + 1)) + ' / ' +
          LDefinition.Step(LIndex).Name + ' / ' + LPort.Reference.Name +
          ' (' + NyxFlowTypeName(LPort.ValueType) + ')';
        SetLength(LEndpoints, LCount + 1);
        LEndpoints[LCount] := NyxObject([NyxField('label', NyxData(LLabel)),
          NyxField('direction', NyxData(Ord(LPort.Direction))),
          NyxField('step', NyxData(LDefinition.Step(LIndex).Name)),
          NyxField('port', NyxData(LPort.Reference.Name))]);
        Inc(LCount);
        LCard.Add(NewNyxLabel(LCard.Node.ID + '-port-' + TNyxText(IntToStr(LPart)))
          .WithText(LLabel + ' / ' + LPort.Description));

        if LPort.Direction = nfdOutput then
        begin
          LSources := LSources + LLabel + #10;
        end
        else
        begin
          LTargets := LTargets + LLabel + #10;
        end;
      end;
      Result.Add(LCard);
    end;
    for LIndex := 0 to LDefinition.WireCount - 1 do
    begin
      LLabel := LDefinition.ToData.Field('wires').Item(LIndex).Field('source').Field('step').AsText +
        ' / ' + LDefinition.ToData.Field('wires').Item(LIndex).Field('source').Field('port').AsText +
        ' -> ' + LDefinition.ToData.Field('wires').Item(LIndex).Field('target').Field('step').AsText +
        ' / ' + LDefinition.ToData.Field('wires').Item(LIndex).Field('target').Field('port').AsText;
      Result.Add(NewNyxLabel(AID + '-wire-' + TNyxText(IntToStr(LIndex))).WithText(LLabel));
    end;
  end;
  Choice('step', 'Selected step', LStepNames, '');
  Button('load', 'Load selected step');
  Button('set', 'Apply configuration to step');
  Choice('source', 'Output port', LSources, '');
  Choice('target', 'Input port', LTargets, '');
  Button('connect', 'Connect ports');
  Result.Add(NewNyxLabel(AID + '-warning').WithText(
    'Removal deletes the chosen graph, step or exact connection. Step removal also detaches its wires.'));
  Choice('confirm', 'Removal warning', 'Keep' + #10 + 'Confirm removal', 'Keep');
  Button('disconnect', 'Remove connection');
  Button('remove-step', 'Remove step and its wires');
  Button('remove-graph', 'Remove graph');
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), NyxObject([
    NyxField('flow', NyxData(AReference.Name)), NyxField('baseline', LBaseline),
    NyxField('names', NyxArray(LNames)), NyxField('catalog', NyxArray(LEntries)),
    NyxField('endpoints', NyxArray(LEndpoints))]));
  Result.Node.Extensions.SetValue(NyxExtension(CPath), NyxArray([]));
  Result.Node.Extensions.SetValue(NyxExtension(CDescriptor), NyxNull);
  Section('toolbox', 'Logic collection', ['search', 'group', 'grouped', 'collection']);
  Section('configuration', 'Configure operation', ['step-name', 'operation', 'defaults',
    'layout', 'indent', 'separator', 'path-kind', 'path-part', 'path-add', 'path-clear',
    'path-preview', 'creator-config', 'add', 'step', 'load', 'set']);
  Section('connections', 'Connect ports', ['source', 'target', 'connect']);
  Section('removal', 'Remove with confirmation', ['warning', 'confirm', 'disconnect',
    'remove-step', 'remove-graph']);
  ShowConfiguration(Result.Node, 'json-parse');
  Field(Result.Node, 'open').Configure.Enabled(LRegistry.Count > 0).Done;
  Field(Result.Node, 'add').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'set').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'load').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'connect').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'disconnect').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'remove-step').Configure.Enabled(AReference.Defined).Done;
  Field(Result.Node, 'remove-graph').Configure.Enabled(AReference.Defined).Done;
end;

function HandleNyxLogicEditor(ANode, ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LContext: TNyxDataValue;
  LEntry: TNyxDataValue;
  LPath: TNyxDataValue;
  LParts: array of TNyxDataValue;
  LDescriptor: TNyxLogicDescriptor;
  LCatalog: INyxLogicCatalog;
  LIndex: Integer;
  LPart: Integer;
  LItem: Integer;
  LGroup: TNyxNode;
  LCard: TNyxNode;
  LAny: Boolean;
  LMatch: Boolean;
begin
  Result := False;
  LForm := FormFor(ANode, ARoot);

  if LForm = nil then
  begin
    Exit;
  end;
  LContext := LForm.Extensions.Value(NyxExtension(CEditor));

  if (ANode.ID = LForm.ID + '-search') or (ANode.ID = LForm.ID + '-group') or
    (ANode.ID = LForm.ID + '-grouped') then
  begin
    LCatalog := NewNyxLogicCatalog;
    for LIndex := 0 to LContext.Field('catalog').Count - 1 do
    begin
      LEntry := LContext.Field('catalog').Item(LIndex);
      LCatalog := LCatalog.WithOperation(NewNyxLogicDeclaration(
        TNyxLogicDescriptor.FromData(LEntry.Field('descriptor')), LEntry.Field('configuration')));
    end;
    for LIndex := 0 to Field(LForm, 'collection').Count - 1 do
    begin
      LGroup := Field(LForm, 'collection').Children[LIndex];
      LAny := False;
      LGroup.Children[0].Configure.Visible(Value(LForm, 'grouped') = 'Grouped').Done;
      for LPart := 1 to LGroup.Count - 1 do
      begin
        LCard := LGroup.Children[LPart];
        LEntry := Entry(LForm, LCard.Extensions.Value(NyxExtension('nyx.logic-kind')).AsText);
        LDescriptor := TNyxLogicDescriptor.FromData(LEntry.Field('descriptor'));
        LMatch := False;
        for LItem := 0 to LCatalog.Count - 1 do
        begin

          if LCatalog.Descriptor(LItem).Kind.Name = LDescriptor.Kind.Name then
          begin
            LMatch := LCatalog.Matches(LItem, Value(LForm, 'search'));
            Break;
          end;
        end;
        LMatch := LMatch and ((Value(LForm, 'group') = 'All groups') or
          (Value(LForm, 'group') = NyxLogicGroupName(LDescriptor.Group)));
        LCard.Configure.Visible(LMatch).Done;
        LAny := LAny or LMatch;
      end;
      LGroup.Configure.Visible(LAny).Done;
    end;
    Exit(True);
  end;

  if (ANode.Parent <> nil) and ANode.Parent.Extensions.Has(NyxExtension('nyx.logic-kind')) then
  begin
    LoadConfiguration(LForm, Entry(LForm,
      ANode.Parent.Extensions.Value(NyxExtension('nyx.logic-kind')).AsText));
    Exit(True);
  end;

  if (ANode.ID = LForm.ID + '-defaults') or (ANode.ID = LForm.ID + '-operation') then
  begin
    LoadConfiguration(LForm, Entry(LForm, Value(LForm, 'operation')));
    Exit(True);
  end;

  if ANode.ID = LForm.ID + '-load' then
  begin
    LEntry := LContext.Field('baseline').Field('definition').Field('steps');
    for LIndex := 0 to LEntry.Count - 1 do
    begin

      if LEntry.Item(LIndex).Field('id').AsText = Value(LForm, 'step') then
      begin
        LoadConfiguration(LForm, LEntry.Item(LIndex));
        Exit(True);
      end;
    end;
    raise ENyxFlow.Create('Choose a saved step to load');
  end;

  if (ANode.ID = LForm.ID + '-path-add') or (ANode.ID = LForm.ID + '-path-clear') then
  begin
    LParts := nil;

    if ANode.ID = LForm.ID + '-path-add' then
    begin
      LPath := LForm.Extensions.Value(NyxExtension(CPath));
      SetLength(LParts, LPath.Count + 1);
      for LIndex := 0 to LPath.Count - 1 do
      begin
        LParts[LIndex] := LPath.Item(LIndex);
      end;

      if Value(LForm, 'path-kind') = 'Field' then
      begin
        LParts[High(LParts)] := NyxData(Value(LForm, 'path-part'));
      end
      else if (Value(LForm, 'path-kind') = 'Item') and
        TryStrToInt(Value(LForm, 'path-part'), LItem) and (LItem >= 0) then
      begin
        LParts[High(LParts)] := NyxData(LItem);
      end
      else
      begin
        raise ENyxFlow.Create('Choose Field or a nonnegative Item index');
      end;
    end;
    LPath := TNyxDataPath.FromData(NyxArray(LParts)).ToData;
    LForm.Extensions.SetValue(NyxExtension(CPath), LPath);
    Field(LForm, 'path-preview').Configure.Text(LPath.ToJSON).Done;
    Exit(True);
  end;
end;

function CaptureNyxLogicEditor(ANode, ARoot: TNyxNode;
  out ACapture: TNyxLogicEditorCapture): Boolean;
var
  LForm: TNyxNode;
  LContext: TNyxDataValue;
  LNames: TNyxDataValue;
  LIndex: Integer;
  LSource: TNyxFlowEndpoint;
  LTarget: TNyxFlowEndpoint;

  function Endpoint(const AField: TNyxText; ADirection: TNyxFlowDirection): TNyxFlowEndpoint;
  var
    LItems: TNyxDataValue;
    LPart: Integer;
    LValue: TNyxDataValue;
  begin
    LItems := LContext.Field('endpoints');
    for LPart := 0 to LItems.Count - 1 do
    begin
      LValue := LItems.Item(LPart);

      if (LValue.Field('label').AsText = Value(LForm, AField)) and
        (LValue.Field('direction').AsInteger = Ord(ADirection)) then
      begin
        Exit(NyxFlowEndpoint(NyxFlowStep(LValue.Field('step').AsText),
          NyxFlowPort(LValue.Field('port').AsText)));
      end;
    end;
    raise ENyxFlow.Create('Choose an exact typed port from this graph');
  end;

begin
  ACapture := Default(TNyxLogicEditorCapture);
  Result := False;
  LForm := FormFor(ANode, ARoot);

  if LForm = nil then
  begin
    Exit;
  end;
  LContext := LForm.Extensions.Value(NyxExtension(CEditor));

  if ANode.ID = LForm.ID + '-open' then
  begin
    ACapture.Open := True;
    ACapture.Reference := NyxFlowRef(Value(LForm, 'graph'));
    Exit(True);
  end;

  if ANode.ID = LForm.ID + '-create' then
  begin
    ACapture.Reference := NyxFlowRef(Value(LForm, 'graph-name'));
    LNames := LContext.Field('names');
    for LIndex := 0 to LNames.Count - 1 do
    begin

      if LNames.Item(LIndex).AsText = ACapture.Reference.Name then
      begin
        raise ENyxFlow.Create('Choose a new graph name or open the existing graph');
      end;
    end;
    ACapture.Change := NyxDefineFlow(ACapture.Reference, NyxFlowDefinitionFromData(
      NewNyxFlowGraph.WithStep(NyxFlowStep('parse'), NewNyxJSONParse).Build.ToData))
      .Expect(NyxObject([NyxField('exists', NyxData(False)), NyxField('definition', NyxNull)]));
    Exit(True);
  end;

  if LContext.Field('flow').AsText = '' then
  begin
    Exit;
  end;
  ACapture.Reference := NyxFlowRef(LContext.Field('flow').AsText);

  if ANode.ID = LForm.ID + '-add' then
  begin
    ACapture.Change := NyxAddFlowStep(ACapture.Reference,
      NyxFlowStep(Value(LForm, 'step-name')), ConfiguredOperation(LForm));
  end
  else if ANode.ID = LForm.ID + '-set' then
  begin
    ACapture.Change := NyxReplaceFlowStep(ACapture.Reference,
      NyxFlowStep(Value(LForm, 'step')), ConfiguredOperation(LForm));
  end
  else if (ANode.ID = LForm.ID + '-connect') or (ANode.ID = LForm.ID + '-disconnect') then
  begin
    LSource := Endpoint('source', nfdOutput);
    LTarget := Endpoint('target', nfdInput);
    ACapture.Change := NyxConnectFlow(ACapture.Reference, LSource, LTarget);

    if ANode.ID = LForm.ID + '-disconnect' then
    begin
      ACapture.Change := NyxDisconnectFlow(ACapture.Reference, LSource, LTarget);
    end;
  end
  else if ANode.ID = LForm.ID + '-remove-step' then
  begin
    ACapture.Change := NyxRemoveFlowStep(ACapture.Reference, NyxFlowStep(Value(LForm, 'step')), True);
  end
  else if ANode.ID = LForm.ID + '-remove-graph' then
  begin
    ACapture.Change := NyxRemoveFlow(ACapture.Reference);
  end
  else
  begin
    Exit;
  end;

  if (ACapture.Change.Kind in [nfcRemove, nfcRemoveStep, nfcDisconnect]) and
    (Value(LForm, 'confirm') <> 'Confirm removal') then
  begin
    raise ENyxFlow.Create('Review and confirm the removal warning first');
  end;
  ACapture.Change := ACapture.Change.Expect(LContext.Field('baseline'));
  Result := True;
end;

procedure TNyxLogicEditorDraft.Clear;
begin
  Self := Default(TNyxLogicEditorDraft);
end;

function TNyxLogicEditorDraft.GetDefined: Boolean;
begin
  Result := FContext <> '';
end;

function TNyxLogicEditorDraft.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if Defined then
  begin
    Result := NyxObject([NyxField('version', NyxData(1)),
      NyxField('context', TNyxDataValue.ParseJSON(FContext)),
      NyxField('values', FValues), NyxField('path', FPath), NyxField('descriptor', FDescriptor)]);
  end;
end;

class function TNyxLogicEditorDraft.FromData(const AData: TNyxDataValue): TNyxLogicEditorDraft;
var
  LContext: TNyxDataValue;
  LIndex: Integer;
begin
  Result := Default(TNyxLogicEditorDraft);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  ValidateNyxFlowFields(AData, ['version', 'context', 'values', 'path', 'descriptor']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxFlow.Create('Unknown Logic editor draft version');
  end;
  LContext := AData.Field('context');
  ValidateNyxFlowFields(LContext, ['flow', 'baseline', 'names', 'catalog', 'endpoints']);
  NyxRemoveFlow(NyxFlowRef('draft')).Expect(LContext.Field('baseline'));
  ValidateNyxFlowFields(AData.Field('values'), CFields);
  for LIndex := 0 to High(CFields) do
  begin
    AData.Field('values').Field(CFields[LIndex]).AsText;
  end;
  Result.FContext := LContext.ToJSON;
  Result.FValues := AData.Field('values');
  Result.FPath := TNyxDataPath.FromData(AData.Field('path')).ToData;
  Result.FDescriptor := AData.Field('descriptor');

  if Result.FDescriptor.Kind <> ndNull then
  begin
    Result.FDescriptor := TNyxLogicDescriptor.FromData(Result.FDescriptor).ToData;
  end;
end;

procedure TNyxLogicEditorDraft.Capture(const AID: TNyxText; ARoot: TNyxNode);
var
  LForm: TNyxNode;
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin

  if ARoot = nil then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AID);

  if LForm = nil then
  begin
    Exit;
  end;
  SetLength(LFields, Length(CFields));
  for LIndex := 0 to High(CFields) do
  begin
    LFields[LIndex] := NyxField(CFields[LIndex], NyxData(Value(LForm, CFields[LIndex])));
  end;
  FContext := LForm.Extensions.Value(NyxExtension(CEditor)).ToJSON;
  FValues := NyxObject(LFields);
  FPath := LForm.Extensions.Value(NyxExtension(CPath));
  FDescriptor := LForm.Extensions.Value(NyxExtension(CDescriptor));
end;

function TNyxLogicEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LIndex: Integer;
begin
  Result := False;

  if (ARoot = nil) or (FContext = '') then
  begin
    Exit;
  end;
  LForm := ARoot.Find(AID);

  if (LForm = nil) or (LForm.Extensions.Value(NyxExtension(CEditor)).ToJSON <> FContext) then
  begin
    Exit;
  end;
  for LIndex := 0 to High(CFields) do
  begin
    SetValue(LForm, CFields[LIndex], FValues.Field(CFields[LIndex]).AsText);
  end;
  LForm.Extensions.SetValue(NyxExtension(CPath), FPath);
  LForm.Extensions.SetValue(NyxExtension(CDescriptor), FDescriptor);
  Field(LForm, 'path-preview').Configure.Text(FPath.ToJSON).Done;
  ShowConfiguration(LForm, Value(LForm, 'operation'));
  HandleNyxLogicEditor(Field(LForm, 'search'), ARoot);
  Result := True;
end;

end.
