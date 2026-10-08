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
unit nyx.test.colors;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.model;

{ Reuse the exact MCP-exported English seed, then independently enrich it with
  current typed domains/state. The frozen service cannot publish new policies.
  Caller owns the returned document; no active editor is replaced. }
function CreateNyxColorFixture: TNyxDocument;
{ Shared checked value/domain/source/history journey; returns assertion count.
  All temporary documents, candidate frames and retained interfaces are released. }
function RunNyxColorChecks: Integer;

implementation

uses
  SysUtils, nyx.colors, nyx.types, nyx.controls, nyx.contract, nyx.schema,
  nyx.data, nyx.state, nyx.binding.types, nyx.codec, nyx.codegen, nyx.source,
  nyx.studio.session, nyx.studio.agents, nyx.studio.projects, nyx.studio.edits,
  {$ifndef PAS2JS}nyx.studio.mcp,{$endif}
  nyx.generated.view;

function CreateNyxColorFixture: TNyxDocument;
var
  LColor: INyxColor;
  LDefinition: INyxColumn;
  LInstance: INyxComponent;
begin
  Result := nyx.generated.view.BuildNyxDocument;
  try
    LColor := RetainNyxControl(Result.Find('accent-color')) as INyxColor;
    LColor.WithColor(NyxRGB(115, 87, 232));
    LColor.Contract.Value(NyxRGBDomain.Choices([
      NyxRGB(115, 87, 232), NyxRGB(18, 52, 86), NyxNoColor]));
    Result.State.SetValue(NyxTextState('accent'), LColor.ColorValue.ToText);
    LColor.Binds.Value(NyxTextState('accent'), bdTwoWay).Done;
    LDefinition := NewNyxColumn('swatch-template');
    Result.AddComponent(LDefinition);
    LColor := NewNyxColor('template-color').WithColor(NyxRGB(10, 20, 30));
    LColor.Configure.PartName(NyxPart('ink')).Done;
    LDefinition.Add(LColor);
    LInstance := NewNyxComponent('first-swatch');
    LInstance.Configure.Component(NyxComponent('swatch-template')).Done;
    LInstance.OverridePart(NyxPart('ink'), noProperties).Named('first-ink')
      .Configure.Value(NyxRGB(40, 50, 60)).Done;
    Result.Pages[0].Add(LInstance);
    LInstance := NewNyxComponent('second-swatch');
    LInstance.Configure.Component(NyxComponent('swatch-template')).Done;
    Result.Pages[0].Add(LInstance);
    LColor := nil;
  except
    Result.Free;
    raise;
  end;
end;

function RunNyxColorChecks: Integer;
const
  CValid: array[0..5] of TNyxText =
    ('', '#000000', '#ffffff', '#AbCdEf', '#012345', '#FEcd98');
  CInvalid: array[0..14] of TNyxText =
    ('black', '#fff', '#ffffffff', '#gg0000', ' #012345', '#012345 ',
     '#12345', '#1234567', '123456', '#１２３４５６', '#abcdef' + #0,
     'rgb(1,2,3)', 'transparent', 'color(display-p3 1 0 0)', '#12🙂');
  CBadChannels: array[0..3] of Integer = (-1, 256, Low(Integer), High(Integer));
var
  LChecks: Integer;
  LIndex: Integer;
  LChannel: Integer;
  LColor: TNyxRGBColor;
  LBefore: TNyxRGBColor;
  LRefused: Boolean;
  LDomain: TNyxTextDomain;
  LCopy: TNyxTextDomain;
  LDoc: TNyxDocument;
  LDecoded: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LWire: TNyxText;
  LPair: TNyxText;
  LAfter: TNyxText;
  LDraft: TNyxText;
  LManaged: INyxColor;
  LAgent: TNyxAgentSession;
  LReply: TNyxDataValue;
  {$ifndef PAS2JS}
  LTools: TNyxDataValue;
  LSchema: TNyxDataValue;
  LBranches: TNyxDataValue;
  LToolIndex: Integer;
  LBranchIndex: Integer;
  LFoundRGB: Boolean;
  {$endif}

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LColor := NyxNoColor;
  Check(not LColor.Defined and (LColor.ToText = ''), 'Absent color is explicit');
  Check(not LColor.SameColor(NyxRGB(0, 0, 0)), 'Absence is distinct from defined black');
  LRefused := False;
  try
    LChannel := LColor.Red;
  except
    on ENyxColorValue do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Absent channel access refuses');
  for LIndex := Low(CValid) to High(CValid) do
  begin
    Check(TryNyxRGB(CValid[LIndex], LColor), 'Valid exact RGB parses');
    Check(LColor.ToText = CValid[LIndex], 'Imported spelling remains exact');

    if LColor.Defined then
    begin
      Check(LColor.SameColor(NyxRGB(LColor.Red, LColor.Green, LColor.Blue)),
        'Numeric channels retain imported color meaning');
    end;
  end;
  for LIndex := Low(CInvalid) to High(CInvalid) do
  begin
    LColor := NyxRGB(1, 2, 3);
    Check(not TryNyxRGB(CInvalid[LIndex], LColor) and not LColor.Defined,
      'Malformed format refuses without partial/prior output');
    LRefused := False;
    try
      LColor := TNyxRGBColor.FromText(CInvalid[LIndex]);
    except
      on ENyxColorValue do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Checked RGB parsing refuses unsupported formats');
  end;
  for LIndex := Low(CBadChannels) to High(CBadChannels) do
  begin
    for LChannel := 0 to 2 do
    begin
      LRefused := False;
      try
        case LChannel of
          0: LColor := NyxRGB(CBadChannels[LIndex], 0, 0);
          1: LColor := NyxRGB(0, CBadChannels[LIndex], 0);
          2: LColor := NyxRGB(0, 0, CBadChannels[LIndex]);
        end;
      except
        on ENyxColorValue do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused, 'Every signed channel validates before narrowing');
    end;
  end;
  LColor := NyxRGB(255, 128, 0);
  LBefore := LColor;
  LColor := TNyxRGBColor.FromText('#ABCDEF');
  Check(LBefore.ToText = '#ff8000', 'Copied values remain independent');

  LDomain := NyxRGBDomain;
  Check(LDomain.Definition.RGBColor and
    (LDomain.Definition.ReadWire('').AsText = ''), 'Optional RGB domain is exact text');
  LCopy := LDomain.Choices([NyxNoColor, NyxRGB(171, 205, 239)]);
  Check((LCopy.Definition.ReadWire('#AbCdEf').AsText = '#AbCdEf') and
    (LDomain.Definition.ReadWire('#123456').AsText = '#123456'),
    'Choice membership uses channels while exact text/copies remain independent');
  LRefused := False;
  try
    LCopy := LDomain.Choices([NyxRGB(171, 205, 239), TNyxRGBColor.FromText('#ABCDEF')]);
  except
    on ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Color-equivalent duplicate choices refuse');
  LRefused := False;
  try
    LDomain.Definition.ReadWire('red');
  except
    on ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Domain refuses unsupported text');
  LRefused := False;
  try
    LCopy := NyxRGBDomain(NyxIntegerDomain.Definition);
  except
    on ENyxContract do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'Nontext domain cannot become RGB');

  LDoc := CreateNyxColorFixture;
  LWorkspace := TNyxSourceWorkspace.Create;
  LDecoded := nil;
  LCandidate := nil;
  LSession := nil;
  LAgent := nil;
  try
    LWire := TNyxCodec.Encode(LDoc);
    LDecoded := TNyxCodec.Decode(LWire);
    Check(TNyxCodec.Encode(LDecoded) = LWire, 'RGB persistence is exact');
    LSource := TNyxCodegen.Generate(LDoc);
    Check((Pos('.Value(NyxRGB(115, 87, 232))', LSource) > 0) and
      (Pos('.Value(NyxNoColor)', LSource) > 0) and
      (Pos('TNyxRGBColor.FromText(''#AbCdEf'')', LSource) > 0) and
      (Pos('LAccentColor: INyxColor;', LSource) > 0),
      'Crafted source uses specialized interfaces and typed RGB/absence');
    Check((Pos('.Value(NyxRGB(40, 50, 60))', LSource) > 0) and
      (Pos('.Value(NyxRGB(10, 20, 30))', LSource) > 0),
      'Independent reusable definition and named override stay typed');
    Check(Pos('.Value(NyxRGBDomain.Choices([NyxRGB(115, 87, 232)', LSource) > 0,
      'Ordinary RGB choices generate fluent typed contracts instead of raw metadata');
    LWorkspace.Render(LDoc);
    LCandidate := LWorkspace.Candidate(LDoc, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LWire, 'Typed source reconstructs the exact RGB document');
    FreeAndNil(LCandidate);
    { Exercise the actual logical semantic surface, independently of HTTP and
      the frozen service. Bounded context must advertise the same closed value
      domain that the shared candidate and generator already use. }
    LAgent := TNyxAgentSession.Create(NyxProjectPair(LWire, LSource));
    LReply := LAgent.Call('nyx_node', 'Scooty', NyxObject([
      NyxField('id', NyxData('accent-color')), NyxField('limit', NyxData(1)),
      NyxField('valueDomain', NyxData(True)), NyxField('domainLimit', NyxData(1))]));
    Check((LReply.Field('valueDomain').Field('format').AsText = 'rgb') and
      (LReply.Field('valueDomain').Field('choices').Count = 1),
      'Logical semantic context is bounded and exposes RGB format');
    LPair := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText;
    LReply := LAgent.Call('nyx_transaction', 'Color workshop', NyxObject([
      NyxField('operationId', NyxData('rgb-choice-pair')),
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operations', NyxArray([
        NyxSetValueDomain(NyxControl('optional-color'),
          NyxRGBDomain.Choices([NyxNoColor, NyxRGB(0, 0, 0)])).ToData,
        NyxSetValueDomain(NyxControl('imported-color'),
          NyxRGBDomain.Choices([TNyxRGBColor.FromText('#AbCdEf'),
            NyxRGB(18, 52, 86)])).ToData]))]));
    LAfter := LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
      .Field('project').AsText;
    Check(not NyxAgentHas(LReply, 'code') and (LAfter <> LPair),
      'Typed RGB policies publish together through the actual semantic dispatcher');
    LReply := LAgent.Call('nyx_history', 'Color workshop', NyxObject([
      NyxField('direction', NyxData('undo')),
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('undo-rgb-choice-pair'))]));
    Check(not NyxAgentHas(LReply, 'code') and
      (LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
        .Field('project').AsText = LPair),
      'One semantic Undo restores both exact color policies and companion source');
    LReply := LAgent.Call('nyx_history', 'Color workshop', NyxObject([
      NyxField('direction', NyxData('redo')),
      NyxField('expectedRevision', NyxData(LAgent.Revision)),
      NyxField('operationId', NyxData('redo-rgb-choice-pair'))]));
    Check(not NyxAgentHas(LReply, 'code') and
      (LAgent.Exchange(NyxObject([NyxField('op', NyxData('observe'))]))
        .Field('project').AsText = LAfter),
      'Semantic Redo retains exact imported RGB choices and the paired source');
    {$ifndef PAS2JS}
    { Discovery is hosted by the native backend. Keep this real server schema
      check out of the portable/browser fixture's dependency closure. }
    LTools := NyxStudioMCPTools.Field('tools');
    LFoundRGB := False;
    for LToolIndex := 0 to LTools.Count - 1 do
    begin

      if LTools.Item(LToolIndex).Field('name').AsText = 'nyx_transaction' then
      begin
        LSchema := LTools.Item(LToolIndex).Field('inputSchema').Field('properties')
          .Field('operations').Field('items').Field('oneOf');
        for LBranchIndex := 0 to LSchema.Count - 1 do
        begin

          if NyxAgentHas(LSchema.Item(LBranchIndex).Field('properties').Field('op'), 'const') and
            (LSchema.Item(LBranchIndex).Field('properties').Field('op')
            .Field('const').AsText = 'value-domain-set') then
          begin
            LBranches := LSchema.Item(LBranchIndex).Field('properties')
              .Field('domain').Field('oneOf');
            LFoundRGB := (LBranches.Count = 7) and
              (LBranches.Item(6).Field('properties').Field('format').Field('const').AsText = 'rgb');
          end;
        end;
      end;
    end;
    Check(LFoundRGB, 'Actual MCP discovery advertises the closed RGB domain');
    {$endif}
    FreeAndNil(LAgent);
    LManaged := NewNyxColor('retained-color').WithColor(NyxRGB(18, 52, 86));
    Check(LManaged.ColorValue.ToText = '#123456', 'Specialized managed Color retains typed value');
    LManaged.Configure.Value(NyxNoColor).Done;
    Check(not LManaged.ColorValue.Defined, 'Managed configuration preserves optional absence');
    LManaged := nil;

    LSession := TNyxStudioSession.Create;
    LSession.Load(LWire);
    LPair := LSession.Save;
    LDraft := StringReplace(LSession.Source, '.Value(NyxRGB(115, 87, 232))',
      '.Value(NyxRGB(18, 52, 86))', [rfReplaceAll]);
    LSession.SetSourceDraft(LDraft);
    LSession.ApplySourceDraft;
    LAfter := LSession.Save;
    Check(LSession.Document.Find('accent-color').Prop('value') = '#123456',
      'Typed source admission updates accepted color');
    LSession.Undo;
    Check(LSession.Save = LPair, 'One paired Undo restores the complete RGB source/design');
    LSession.Redo;
    Check((LSession.Save = LAfter) and (LSession.Source = LDraft),
      'Paired Redo restores exact source spelling');
    LDraft := StringReplace(LSession.Source, '.Value(NyxRGB(18, 52, 86))',
      '.Value(NyxRGB(''18'', 52, 86))', [rfReplaceAll]);
    LSession.SetSourceDraft(LDraft);
    LRefused := False;
    try
      LSession.ApplySourceDraft;
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Save = LAfter) and (LSession.DraftSource = LDraft),
      'Wrong typed source refuses without replacing the accepted pair or pending draft');
  finally
    LManaged := nil;
    LAgent.Free;
    LSession.Free;
    LCandidate.Free;
    LDecoded.Free;
    LWorkspace.Free;
    LDoc.Free;
  end;
  Result := LChecks;
end;

end.
