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

program nyx_logic_document_compiled;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.generated.view,
  nyx.flow, nyx.flow.types, nyx.flow.declarations, nyx.logic, nyx.scheduler
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};


type
  { Qualification host explicitly reconstructs one creator implementation. It
    owns only copied schema/configuration, never the saved document or a view. }
  TCreator = class(TInterfacedObject, INyxLogicOperation)
  public
    Schema: TNyxLogicDescriptor;
    Settings: TNyxDataValue;
    function GetDescriptor: TNyxLogicDescriptor;
    function GetConfiguration: TNyxDataValue;
    function Invoke(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues;
  end;
  TResolver = class(TInterfacedObject, INyxLogicResolver)
  public
    Schema: TNyxLogicDescriptor;
    Settings: TNyxDataValue;
    function Resolve(const AKind: TNyxLogicKindRef;
      const AConfiguration: TNyxDataValue): INyxLogicOperation;
  end;

function TCreator.GetDescriptor: TNyxLogicDescriptor;
begin
  Result := Schema;
end;

function TCreator.GetConfiguration: TNyxDataValue;
begin
  Result := Settings;
end;

function TCreator.Invoke(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxFlowPort('result'), AInputs.Value(NyxFlowPort('value')));
end;

function TResolver.Resolve(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
var
  LCreator: TCreator;
begin

  if (AKind.Name <> Schema.Kind.Name) or (AConfiguration.ToJSON <> Settings.ToJSON) then
  begin
    raise ENyxFlow.Create('Explicit creator authority does not match this declaration');
  end;
  LCreator := TCreator.Create;
  Result := LCreator;
  LCreator.Schema := Schema;
  LCreator.Settings := Settings;
end;

{$ifdef PAS2JS}
var
  GRequest: TJSXMLHttpRequest;
{$endif}

{ Execute the exact compiled companion and compare its complete admitted model.
  Fetch/file success alone never supplies a passing constructor result. }
procedure VerifyDesign(const AExpected: TNyxText);
var
  LDocument: TNyxDocument;
  LResult: INyxFlowResult;
  LDefinition: INyxFlowDefinition;
  LResolver: TResolver;
  LAuthority: INyxLogicResolver;
  LValue: TNyxDataValue;
  LRefused: Boolean;
begin
  LDocument := nil;
  try
    try
      LDocument := BuildNyxDocument;

      if TNyxCodec.Encode(LDocument) <> AExpected then
      begin
        raise ENyxModel.Create('Compiled design companion differs from its admitted pair');
      end;
      LDefinition := LDocument.Flows.Definition(NyxFlowRef('Profile 🐾'));
      LResult := LDefinition.Build.Evaluate([
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('parse'), NyxFlowPort('text')),
          NyxFlowText('{"profile":{"name":"Compiled graph","number":9007199254740993}}')),
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('summary'), NyxFlowPort('left')),
          NyxFlowText('Profile'))]);

      if Pos('9007199254740993', LResult.Value(
        NyxFlowEndpoint(NyxFlowStep('pretty'), NyxFlowPort('text'))).AsText) = 0 then
      begin
        raise ENyxFlow.Create('Exact compiled graph output was not executed');
      end;
      LDefinition := LDocument.Flows.Definition(NyxFlowRef('Custom'));
      LRefused := False;
      try
        LDefinition.Build;
      except
        on E: ENyxFlow do
        begin
          LRefused := True;
        end;
      end;

      if not LRefused then
      begin
        raise ENyxFlow.Create('Creator declaration gained implicit runtime authority');
      end;
      LResolver := TResolver.Create;
      LAuthority := LResolver;
      LResolver.Schema := LDefinition.Descriptor(0);
      LResolver.Settings := LDefinition.Configuration(0);
      LValue := NyxObject([NyxField('caption', NyxData(TNyxText('Exact 🐾') + TNyxText(#0)))]);
      LResult := LDefinition.Build(LAuthority).Evaluate([
        NyxFlowInput(NyxFlowEndpoint(NyxFlowStep('custom'), NyxFlowPort('value')),
          NyxFlowJSON(LValue))]);

      if LResult.Value(NyxFlowEndpoint(NyxFlowStep('custom'),
        NyxFlowPort('result'))).AsJSON.ToJSON <> LValue.ToJSON then
      begin
        raise ENyxFlow.Create('Explicit compiled creator did not produce exact owned output');
      end;
      {$ifdef PAS2JS}
      document.body.setAttribute('data-design-source', 'passed');
      {$else}
      WriteLn('PASS exact compiled design, graph execution and explicit creator authority');
      {$endif}
    except
      on LException: Exception do
      begin
        {$ifdef PAS2JS}
        document.body.setAttribute('data-design-source', 'failed');
        document.body.setAttribute('data-design-error', LException.Message);
        {$else}
        WriteLn('FAIL ', LException.Message);
        DumpExceptionBackTrace(Output);
        ExitCode := 1;
        {$endif}
      end;
    end;
  finally
    LDocument.Free;
  end;
end;

{$ifdef PAS2JS}
function Loaded(AEvent: TJSProgressEvent): Boolean;
begin
  Result := True;

  if (GRequest.status <> 200) or (Length(GRequest.responseText) > 16777216) then
  begin
    document.body.setAttribute('data-design-source', 'failed');
    document.body.setAttribute('data-design-error', 'Exact admitted design unavailable or oversized');
    Exit;
  end;
  VerifyDesign(GRequest.responseText);
  GRequest := nil;
end;
{$else}
var
  LStream: TFileStream;
  LExpected: TNyxText;
{$endif}

begin
  {$ifdef PAS2JS}
  GRequest := TJSXMLHttpRequest.new;
  GRequest.open('GET', 'design.nyx');
  GRequest.onload := @Loaded;
  GRequest.send;
  {$else}
  try

    if ParamCount <> 1 then
    begin
      raise ENyxModel.Create('Supply the exact expected design artifact');
    end;
    { Native file bytes are portable UTF-8, never ANSI line collections. }
    LStream := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
    try

      if LStream.Size > 16 * 1024 * 1024 then
      begin
        raise ENyxModel.Create('Expected design exceeds the fixture boundary');
      end;
      SetLength(LExpected, LStream.Size);

      if Length(LExpected) > 0 then
      begin
        LStream.ReadBuffer(LExpected[1], Length(LExpected));
      end;
    finally
      LStream.Free;
    end;
    VerifyDesign(LExpected);
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
  {$endif}
end.
