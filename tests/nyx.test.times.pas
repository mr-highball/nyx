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
unit nyx.test.times;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.model;

{ Owned English application exercising public managed authoring. It contains
  exact precision, optional values, one-sided/overnight domains and a named
  compound field/event contract. A declaration fixture is not a physical picker. }
function CreateNyxTimeFixture: TNyxDocument;
{ Admission, persistence and paired history run on both compilers. The caller
  receives a count; failed admission raises with the original accepted pair intact. }
function RunNyxTimeAuthoringTests: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.times, nyx.types, nyx.controls, nyx.data,
  nyx.contract, nyx.schema, nyx.codec, nyx.codegen, nyx.source,
  nyx.state, nyx.binding.types, nyx.binding, nyx.composition, nyx.studio.session;

function CreateNyxTimeFixture: TNyxDocument;
var
  LPage: INyxPage;
  LAppointment: INyxColumn;
  LStart: INyxTime;
  LTime: INyxTime;
  LDefinition: INyxColumn;
  LInstance: INyxComponent;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Meeting planner';
    LPage := NewNyxPage('home');
    Result.AddPage(LPage);
    LAppointment := NewNyxColumn('appointment');
    LPage.Add(LAppointment);
    LAppointment.Configure.Compound(True).Done;
    LStart := NewNyxTime('start-time');
    LAppointment.Add(LStart);
    LStart.Configure.Text('Starts at').PartName(NyxPart('start'))
      .Value(NyxTime(23, 0).WithPrecision(ntpMillisecond)).Done;
    LAppointment.Contract
      .Field(NyxPart('start'), NyxTimeDomain.Range(NyxTime(22, 0), NyxTime(2, 0))
        .StepMilliseconds(1500))
      .On(ntChange, NyxPartValue(NyxPart('start')), NyxTimeDomain.AnyStep);
    Result.State.SetValue(NyxTextState('reminder'),
      NyxTime(0, 30).WithPrecision(ntpMillisecond).ToText);
    LStart.Binds.Value(NyxTextState('reminder'), bdTwoWay).Done;

    LTime := NewNyxTime('earliest-time');
    LPage.Add(LTime);
    LTime.Contract.Value(NyxTimeDomain.Minimum(NyxTime(8, 30)).StepMilliseconds(125));
    LTime.WithTime(NyxTime(8, 30, 0, 250).WithPrecision(ntpHundredth));
    LTime := NewNyxTime('latest-time');
    LPage.Add(LTime);
    LTime.Contract.Value(NyxTimeDomain.Maximum(NyxTime(10, 0)).AnyStep);
    LTime.WithTime(NyxTime(10, 0).WithPrecision(ntpSecond));
    LTime := NewNyxTime('choice-time');
    LPage.Add(LTime);
    LTime.Contract.Value(NyxTimeDomain.Choices([
      NyxNoTime, NyxTime(9, 0, 0, 100).WithPrecision(ntpTenth)]));
    LTime.WithTime(NyxTime(9, 0, 0, 100).WithPrecision(ntpTenth));
    LTime := NewNyxTime('optional-time');
    LPage.Add(LTime);
    LTime.WithTime(NyxNoTime);
    LTime := NewNyxTime('midnight-time');
    LPage.Add(LTime);
    LTime.WithTime(NyxTime(0, 0));
    LTime := NewNyxTime('last-millisecond-time');
    LPage.Add(LTime);
    LTime.WithTime(NyxTime(23, 59, 59, 999));
    { An imported noncanonical descriptor keeps its complete exact member order
      through the explicit Metadata boundary, rather than being rewritten as
      a merely equivalent ordinary fluent declaration. }
    LTime := NewNyxTime('imported-time');
    LPage.Add(LTime);
    LTime.WithTime(NyxTime(9, 0));
    LTime.Contract.Metadata(NyxObject([
      NyxField('version', NyxData(1)),
      NyxField('value', NyxObject([
        NyxField('format', NyxData('time')), NyxField('type', NyxData('text')),
        NyxField('max', NyxData(NyxTime(10, 0).ToText)),
        NyxField('min', NyxData(NyxTime(8, 0).ToText)),
        NyxField('step', NyxData(1000))]))]));
    { Valid imported constraints may have an order no fluent chain can replay:
      the choice aligns with the final minimum, but not the transient midnight
      base when step appears first. Generation must preserve the whole domain
      atomically instead of emitting a constructor that fails at runtime. }
    LTime := NewNyxTime('imported-step-base-time');
    LPage.Add(LTime);
    LTime.WithTime(NyxTime(8, 30, 0, 125));
    LTime.Contract.Metadata(NyxObject([
      NyxField('version', NyxData(1)),
      NyxField('value', NyxObject([
        NyxField('type', NyxData('text')), NyxField('format', NyxData('time')),
        NyxField('choices', NyxArray([NyxData(NyxTime(8, 30, 0, 125).ToText)])),
        NyxField('step', NyxData(250)),
        NyxField('min', NyxData(NyxTime(8, 30, 0, 125).ToText)),
        NyxField('max', NyxData(NyxTime(9, 0, 0, 125).ToText))]))]));
    LDefinition := NewNyxColumn('appointment-template');
    Result.AddComponent(LDefinition);
    LTime := NewNyxTime('template-clock');
    LDefinition.Add(LTime);
    LTime.Configure.PartName(NyxPart('time')).Value(NyxTime(8, 0)).Done;
    LInstance := NewNyxComponent('appointment-instance');
    LPage.Add(LInstance);
    LInstance.Configure.Component(NyxComponent('appointment-template')).Done;
    LInstance.OverridePart(NyxPart('time'), noProperties).Named('instance-time').Configure
      .Value(NyxTime(9, 0).WithPrecision(ntpSecond)).Done;
  except
    Result.Free;
    raise;
  end;
end;

function ReplaceFirst(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(ABefore, ASource);

  if LPosition = 0 then
  begin
    raise Exception.Create('Clock source fixture cannot find its actual edit');
  end;
  Result := Copy(ASource, 1, LPosition - 1) + AAfter +
    Copy(ASource, LPosition + Length(ABefore), MaxInt);
end;

function RunNyxTimeAuthoringTests: Integer;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LControl: INyxTime;
  LConfiguration: INyxConfiguration;
  LLive: TNyxLiveBindings;
  LStore: TNyxState;
  LRoot: TNyxNode;
  LRevision: Integer;
  LDomain: TNyxValueDomain;
  LBaseline: TNyxText;
  LSource: TNyxText;
  LDraft: TNyxText;
  LAccepted: TNyxText;
  LPair: TNyxText;
  LRefused: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Clock authoring: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure Reject(const ABefore, AAfter: TNyxText);
  var
    LCandidate: TNyxDocument;
  begin
    LDraft := ReplaceFirst(LSource, ABefore, AAfter);
    LCandidate := nil;
    LRefused := False;
    try
      try
        LCandidate := LWorkspace.Candidate(LDocument, LDraft);
      except
        on LException: Exception do
        begin
          LRefused := LException.Message <> '';
        end;
      end;
    finally
      LCandidate.Free;
    end;
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBaseline) and
      (LWorkspace.Render(LDocument) = LSource), 'Invalid clock source refuses atomically');
  end;

begin
  Result := 0;
  LDocument := CreateNyxTimeFixture;
  LCopy := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  LSession := nil;
  LLive := nil;
  LStore := nil;
  LRoot := nil;
  try
    LControl := NewNyxTime('independent-clock');
    LConfiguration := LControl.Configure;
    LConfiguration.Value(NyxTime(12, 3, 4, 500).WithPrecision(ntpTenth));
    Check(LControl.TimeValue.ToText = '12:03:04.5', 'Managed specialized clock exposes typed precision');
    LControl := nil;
    LConfiguration.Value(NyxNoTime);
    Check(not (LConfiguration.Done as INyxTime).TimeValue.Defined,
      'An acyclic retained facade keeps its control alive without inventing now');
    LConfiguration := nil;

    LBaseline := TNyxCodec.Encode(LDocument);
    { This is the portable binding coordinator, with no physical control. It
      establishes complete candidate/store admission and subscription lifetime;
      a native/browser field journey must separately establish actual input. }
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LLive.Activate;
    Check(LRoot.Find('start-time').Prop('value') = '00:30:00.000',
      'The independent application store projects its exact declared clock default');
    LLive.Edit(LRoot.Find('start-time'), '01:00:00.000');
    Check(LStore.GetValue(NyxTextState('reminder')) = '01:00:00.000',
      'Clock wire admission preserves exact precision in the independent runtime store');
    LRevision := LStore.Revision;
    LRefused := False;
    try
      LStore.Apply([
        NyxStateValue(NyxTextState('reminder'), '01:00:00.001'),
        NyxStateValue(NyxTextState('unaccepted-note'), 'This whole group must refuse.')]);
    except
      on LException: ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.Revision = LRevision) and not LStore.Has('unaccepted-note') and
      (LStore.GetValue(NyxTextState('reminder')) = '01:00:00.000') and
      (LRoot.Find('start-time').Prop('value') = '01:00:00.000'),
      'An off-step clock refuses the complete state group without partial publication');
    Check(TNyxCodec.Encode(LDocument) = LBaseline, 'Runtime clock changes never mutate authored defaults');
    FreeAndNil(LLive);
    LStore.SetValue(NyxTextState('reminder'), 'Detached store data');
    Check(LRoot.Find('start-time').Prop('value') = '01:00:00.000',
      'Coordinator retirement disconnects its validator/observer from independent state');
    FreeAndNil(LStore);
    FreeAndNil(LRoot);
    LCopy := TNyxCodec.Decode(LBaseline);
    Check(TNyxCodec.Encode(LCopy) = LBaseline, 'Exact clock/domain/default persistence');
    Check(NyxNodeValueDomain(LCopy.Find('optional-time')).ClockTime,
      'An ordinary time field intrinsically admits clock values');
    LDomain := NyxNodeValueDomain(LCopy.Find('start-time'));
    Check(LDomain.ClockTime and (LDomain.ReadWire('00:30:00.000').AsText = '00:30:00.000'),
      'Named compound field retains overnight range and exact bound default');
    LCopy.Find('earliest-time').Configure.Value(NyxTime(9, 0)).Done;
    Check(TNyxCodec.Encode(LDocument) = LBaseline, 'Decoded clock owners are independent');
    FreeAndNil(LCopy);
    LSource := LWorkspace.Render(LDocument);
    Check(Pos('LStartTime: INyxTime;', LSource) > 0, 'Generated references use the specialized interface');
    Check(Pos('.Value(NyxNoTime)', LSource) > 0, 'Optional clocks generate typed absence');
    Check(Pos('.Value(NyxTime(0, 0))', LSource) > 0, 'Midnight generates a defined typed clock');
    Check(Pos('.WithPrecision(ntpMillisecond)', LSource) > 0, 'Explicit zero fraction generates typed precision');
    Check(Pos('NyxTimeDomain.Minimum(NyxTime(8, 30)).StepMilliseconds(125)', LSource) > 0,
      'A one-sided range and exact step generate fluent typed source');
    Check(Pos('NyxTimeDomain.Maximum(NyxTime(10, 0)).AnyStep', LSource) > 0,
      'An upper-only bound and explicit any-step generate typed source');
    Check(Pos('.Choices([NyxNoTime, NyxTime(9, 0, 0, 100).WithPrecision(ntpTenth)])', LSource) > 0,
      'Typed choices retain optional membership and exact fractional spelling');
    Check(Pos('.Metadata(NyxObject([', LSource) > 0,
      'Noncanonical imported clock metadata retains its explicit complete source boundary');
    Check(Pos('.Value(NyxTime(9, 0).WithPrecision(ntpSecond))', LSource) > 0,
      'Reusable named-part overrides resolve their clock kind for typed generation');
    LCopy := LWorkspace.Candidate(LDocument, LSource);
    Check(TNyxCodec.Encode(LCopy) = LBaseline, 'Managed source reconstruction retains the complete exact clock pair');
    Check(TNyxCodegen.Generate(LCopy) = TNyxCodegen.Generate(LDocument), 'Clock generation is stable across reconstruction');
    FreeAndNil(LCopy);
    Reject('NyxTime(8, 30)', 'NyxTime(''8'', 30)');
    Reject('NyxTime(8, 30)', 'NyxTime(8.5, 30)');
    Reject('NyxTime(8, 30)', 'NyxTime(24, 30)');
    Reject('Minimum(NyxTime(8, 30))', 'Minimum(NyxNoTime)');
    Reject('Minimum(NyxTime(8, 30))', 'Minimum(''08:30'')');
    Reject('Minimum(NyxTime(8, 30))', 'Minimum(NyxDate(2026, 10, 7))');
    Reject('StepMilliseconds(125)', 'StepMilliseconds(0)');
    Reject('StepMilliseconds(125)', 'StepMilliseconds(125.0)');
    Reject('StepMilliseconds(125)', 'StepMilliseconds(''125'')');
    Reject('WithPrecision(ntpTenth)', 'WithPrecision(ntpMinute)');
    Reject('WithPrecision(ntpTenth)', 'WithPrecision(nvPrimary)');
    Reject('WithPrecision(ntpTenth)', 'WithPrecision(''tenths'')');
    Reject('Choices([NyxNoTime,', 'Choices([False,');
    Reject('.Value(NyxNoTime)', '.Option(NyxNoTime)');
    Reject('NyxTimeDomain.AnyStep', 'NyxTimeDomain(NyxTextDomain)');
    Reject('NyxTimeDomain.AnyStep', 'NyxTimeDomain(NyxDateDomain.Definition)');
    Reject('SetValue(LReminderTextState, ''00:30:00.000'')',
      'SetValue(LReminderTextState, ''24:00'')');
    Reject('SetValue(LReminderTextState, ''00:30:00.000'')',
      'SetValue(LReminderTextState, ''03:00'')');
    Reject('NyxField(''step'', NyxData(1000))', 'NyxField(''unsupported'', NyxData(1000))');

    { The explicit Definition overload takes a value domain, never an implicit
      builder conversion accepted only by the reader. Checked compilers receive
      this exact accepted expression again in the generated reconstruction. }
    LDraft := ReplaceFirst(LSource, 'NyxTimeDomain.AnyStep',
      'NyxTimeDomain(NyxTextDomain.Definition).AnyStep');
    LCopy := LWorkspace.Candidate(LDocument, LDraft);
    Check(TNyxCodec.Encode(LCopy) = LBaseline, 'Explicit text-domain enrichment matches the public overload');
    FreeAndNil(LCopy);
    LDraft := ReplaceFirst(LSource, 'StepMilliseconds(1500)', 'StepSeconds(3)');
    LCopy := LWorkspace.Candidate(LDocument, LDraft);
    Check(NyxNodeValueDomain(LCopy.Find('start-time')).TimeStepMilliseconds = 3000,
      'Typed seconds authoring admits an exact integer millisecond domain');
    FreeAndNil(LCopy);

    LSession := TNyxStudioSession.Create;
    LSession.Load(LBaseline);
    LAccepted := LSession.Source;
    LDraft := ReplaceFirst(LAccepted, 'StepMilliseconds(125)', 'StepMilliseconds(250)');
    LSession.SetSourceDraft(LDraft);
    LSession.ApplySourceDraft;
    LPair := LSession.Save;
    Check(NyxNodeValueDomain(LSession.Document.Find('earliest-time')).TimeStepMilliseconds = 250,
      'Studio admits a clock domain through its ordinary paired source boundary');
    LSession.Undo;
    Check((LSession.Save = LBaseline) and (LSession.Source = LAccepted), 'One Undo restores the exact clock/source pair');
    LSession.Redo;
    Check((LSession.Save = LPair) and (LSession.Source = LDraft), 'Redo restores the exact admitted clock pair');
    LAccepted := LSession.Source;
    LDraft := ReplaceFirst(LAccepted, 'StepMilliseconds(250)', 'StepMilliseconds(-1)');
    LSession.SetSourceDraft(LDraft);
    LRefused := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: Exception do
      begin
        LRefused := LException.Message <> '';
      end;
    end;
    Check(LRefused and (LSession.Save = LPair) and (LSession.DraftSource = LDraft),
      'Failed clock admission preserves accepted data and the exact pending draft');
    LSession.DiscardSourceDraft;
    Check(LSession.Source = LAccepted, 'Discard restores accepted crafted clock source');
  finally
    LConfiguration := nil;
    LControl := nil;
    LLive.Free;
    LStore.Free;
    LRoot.Free;
    LSession.Free;
    LCopy.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

end.
