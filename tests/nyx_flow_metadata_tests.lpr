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

program nyx_flow_metadata_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.json, nyx.data, nyx.flow, nyx.flow.types,
  nyx.flow.declarations, nyx.logic, nyx.model, nyx.test.flow.conditionfixture
  {$ifdef PAS2JS}, Web{$endif};

type
  { Foreign data-only implementation exercises the public normalization seam.
    It owns an immutable source definition and optionally retains a registry
    through a managed lease. Admission must not retain this implementation or
    call Build; attempting a registry mutation while ToData runs must refuse. }
  TForeignDefinition = class(TInterfacedObject, INyxFlowDefinition)
  public
    Source: INyxFlowDefinition;
    Registry: INyxFlowDeclarations;
    Reads: Integer;
    Builds: Integer;
    ReentryRefused: Boolean;
    Malformed: Boolean;
    function GetStepCount: Integer;
    function GetWireCount: Integer;
    function Step(AIndex: Integer): TNyxFlowStepRef;
    function Descriptor(AIndex: Integer): TNyxLogicDescriptor;
    function Configuration(AIndex: Integer): TNyxDataValue;
    function ToData: TNyxDataValue;
    function Build(const AResolver: INyxLogicResolver = nil): INyxFlowGraph;
  end;

var
  GChecks: Integer;

function TForeignDefinition.GetStepCount: Integer;
begin
  Result := Source.StepCount;
end;

function TForeignDefinition.GetWireCount: Integer;
begin
  Result := Source.WireCount;
end;

function TForeignDefinition.Step(AIndex: Integer): TNyxFlowStepRef;
begin
  Result := Source.Step(AIndex);
end;

function TForeignDefinition.Descriptor(AIndex: Integer): TNyxLogicDescriptor;
begin
  Result := Source.Descriptor(AIndex);
end;

function TForeignDefinition.Configuration(AIndex: Integer): TNyxDataValue;
begin
  Result := Source.Configuration(AIndex);
end;

function TForeignDefinition.ToData: TNyxDataValue;
begin
  Inc(Reads);

  if Registry <> nil then
  begin
    try
      Registry.Remove(NyxFlowRef('Read report'));
    except
      on ENyxFlow do
      begin
        ReentryRefused := True;
      end;
    end;
  end;

  if Malformed then
  begin
    Exit(NyxNull);
  end;
  Result := Source.ToData;
end;

function TForeignDefinition.Build(const AResolver: INyxLogicResolver): INyxFlowGraph;
begin
  Result := nil;
  Inc(Builds);
  raise Exception.Create('Foreign executable construction is not admission authority');
end;

procedure Check(AValue: Boolean; const AMessage: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LDoc: TNyxDocument;
  LDefinition: INyxFlowDefinition;
  LClone: INyxFlowDeclarations;
  LForeign: TForeignDefinition;
  LForeignLease: INyxFlowDefinition;
  LDescriptor: TNyxLogicDescriptor;
  LDerived: TNyxLogicDescriptor;
  LPort: TNyxFlowPort;
  LData: TNyxDataValue;
  LBefore: TNyxText;
  LMetadata: TNyxText;
  LIndex: Integer;
  LRead: Integer;
  LRefused: Boolean;
begin
  LDoc := ConditionalFileDocument;
  try
    LDefinition := LDoc.Flows.Definition(NyxFlowRef('Read report'));
    LBefore := LDefinition.ToData.ToJSON;
    LData := LDefinition.ToData;
    Check((LDefinition.StepCount = LData.Field('steps').Count) and
      (LDefinition.WireCount = LData.Field('wires').Count), 'typed count snapshots match complete admitted wire data');
    for LIndex := 0 to LDefinition.StepCount - 1 do
    begin
      LData := LDefinition.ToData.Field('steps').Item(LIndex);
      LDescriptor := LDefinition.Descriptor(LIndex);
      LMetadata := LDescriptor.ToData.ToJSON;
      Check((LDefinition.Step(LIndex).Name = LData.Field('id').AsText) and
        (LDefinition.Configuration(LIndex).ToJSON = LData.Field('configuration').ToJSON) and
        (LMetadata = LData.Field('descriptor').ToJSON),
        'every typed snapshot preserves complete exact admitted step data');
      LDerived := LDescriptor.Copy.Input(NyxFlowPort('extra'), nftText, 'Independent input')
        .Labelled('derived');
      LPort := LDerived.Port(0);
      LPort.Description := 'Changed caller copy';
      LPort.Reference := NyxFlowPort('caller-only');
      Check((LDerived.PortCount = LDescriptor.PortCount + 1) and
        (LDescriptor.ToData.ToJSON = LMetadata) and
        (LDefinition.Descriptor(LIndex).ToData.ToJSON = LMetadata),
        'caller metadata derivation cannot mutate the admitted cached descriptor');
    end;
    for LIndex := 0 to 2 do
    begin
      for LRead := 0 to 1 do
      begin
        LRefused := False;
        try
          case LIndex of
            0:
              begin
                LDefinition.Step(LRead * (LDefinition.StepCount + 1) - 1);
              end;
            1:
              begin
                LDefinition.Descriptor(LRead * (LDefinition.StepCount + 1) - 1);
              end;
            2:
              begin
                LDefinition.Configuration(LRead * (LDefinition.StepCount + 1) - 1);
              end;
          end;
        except
          on ENyxJSON do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'negative/count index preserves the original data-boundary failure family');
      end;
    end;
    LForeign := TForeignDefinition.Create;
    LForeignLease := LForeign;
    LForeign.Source := LDefinition;
    LForeign.Registry := LDoc.Flows;
    LDoc.Flows.Define(NyxFlowRef('Independent report'), LForeignLease);
    Check(LForeign.ReentryRefused and LDoc.Flows.Contains(NyxFlowRef('Read report')),
      'public normalization still guards its complete registry admission against reentry');
    Check((LForeign.Reads = 1) and (LForeign.Builds = 0),
      'foreign admission reads one inert snapshot and never grants execution authority');
    LMetadata := LDoc.Flows.ToData.ToJSON;
    LForeign.Malformed := True;
    LRefused := False;
    try
      LDoc.Flows.Define(NyxFlowRef('Independent report'), LForeignLease);
    except
      LRefused := True;
    end;
    Check(LRefused and (LDoc.Flows.ToData.ToJSON = LMetadata),
      'invalid foreign replacement retains the complete accepted registry');
    LClone := LDoc.Flows.Clone;
    LForeign.Registry := nil;
    LForeignLease := nil;
    LDoc.Flows.Remove(NyxFlowRef('Read report'));
    Check((LClone.Definition(NyxFlowRef('Read report')).ToData.ToJSON = LBefore) and
      (LDefinition.ToData.ToJSON = LBefore), 'clone and independently retained definition outlive original membership');
  finally
    LForeignLease := nil;
    LClone := nil;
    LDefinition := nil;
    LDoc.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-flow-metadata', 'passed');
    document.body.setAttribute('data-flow-metadata-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' admitted metadata ownership/admission checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-flow-metadata', 'failed');
      document.body.setAttribute('data-event-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
