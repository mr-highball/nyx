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

program nyx_slider_companion;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data, nyx.model, nyx.source, nyx.codegen,
  nyx.state, nyx.binding.types, nyx.studio.edits, nyx.studio.stateedits, nyx.studio.transactions,
  nyx.test.mcp.client, nyx.test.sliders;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{ The explicit wire decoder below is the persistence boundary for one typed
  grouped edit. Application authoring/policy augmentation remains Pascal. }
function Composition: TNyxDataValue;
var
  LLayout: INyxDesignPatch;
  LState: INyxStateBindingPatch;
begin
  LLayout := ReadNyxDesignPatch(TNyxDataValue.ParseJSON(
    '[{"op":"create","kind":"page","id":"slider-review","root":"page","properties":{"gap":12,"padding":20}},' +
    '{"op":"create","kind":"heading","id":"slider-title","parent":"slider-review","properties":{"text":"Mixing desk"}},' +
    '{"op":"create","kind":"label","id":"slider-help","parent":"slider-review","properties":{"text":"Adjust gain smoothly or choose an exact preset."}},' +
    '{"op":"create","kind":"slider","id":"fractional-slider","parent":"slider-review"},' +
    '{"op":"create","kind":"slider","id":"choice-slider","parent":"slider-review"},' +
    '{"op":"create","kind":"slider","id":"number-choice-slider","parent":"slider-review"}]'));
  LState := NyxStateBindingPatch([
    NyxCreateDefault(NyxStateValue(NyxIntegerState('level'), 0)),
    NyxBindControl(NyxBindingOwner('choice-slider'), bpValue, NyxIntegerState('level'), bdTwoWay)]);
  Result := NyxProjectTransaction([NyxDesignStep(LLayout), NyxStateStep(LState)]).ToData;
end;

procedure WriteBytes(const AFile: String; const AText: TNyxText);
var
  LBytes: UTF8String;
  LFile: TFileStream;
begin
  LBytes := UTF8String(AText);
  LFile := TFileStream.Create(AFile, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[1], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
end;

{ Use one authenticated transport for a temporary review. This accepts an
  existing configuration, never creates a listener or edits enrollment/profiles.
  Public typed Pascal builds the grouped wire payload. Bounded source windows
  and paired Undo/Redo establish the exact semantic compiler companion. }
procedure Semantic(const AConfig, AOutput: String);
var
  LClient: TNyxMCPTestClient;
  LReview: TNyxText;
  LRevision: Integer;
  LPrimary: Integer;
  LInitial, LSource: TNyxText;
  LReply: TNyxDataValue;

  function Call(const AName: TNyxText; const AFields: array of TNyxDataField;
    AContext: Boolean = True): TNyxDataValue;
  var
    LFields: array of TNyxDataField;
    LIndex: Integer;
    LPacket: TNyxDataValue;
  begin
    SetLength(LFields, Length(AFields) + Ord(AContext));
    for LIndex := 0 to High(AFields) do
    begin
      LFields[LIndex] := AFields[LIndex];
    end;

    if AContext then
    begin
      LFields[High(LFields)] := NyxField('review', NyxData(LReview));
    end;
    LPacket := LClient.Tool(AName, NyxObject(LFields));
    Check(not LPacket.Field('isError').AsBoolean, AName + ' refused: ' + LPacket.ToJSON);
    Result := LPacket.Field('structuredContent');
  end;

  function Source: TNyxText;
  var
    LReply, LLines: TNyxDataValue;
    LLine, LIndex: Integer;
  begin
    Result := '';
    LLine := 1;
    repeat
      LReply := Call('nyx_source', [NyxField('line', NyxData(LLine)), NyxField('count', NyxData(80))]);
      Check(LReply.Field('revision').AsInteger = LRevision, 'bounded source stays at one revision');
      LLines := LReply.Field('lines');
      Check(LLines.Count > 0, 'source window makes progress');
      for LIndex := 0 to LLines.Count - 1 do
      begin
        Result := Result + LLines.Item(LIndex).AsText + TNyxText(#10);
      end;
      Inc(LLine, LLines.Count);
    until LLine > LReply.Field('totalLines').AsInteger;
  end;

  procedure History(const AMode: TNyxText);
  begin
    Call('nyx_history', [NyxField('direction', NyxData(AMode)),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('slider-' + AMode))]);
    LRevision := Call('nyx_session', []).Field('revision').AsInteger;
  end;

begin

  if DirectoryExists(AOutput) then
  begin
    raise Exception.Create('Semantic export destination must be new');
  end;
  LClient := TNyxMCPTestClient.Create(AConfig, 'Scooty slider companion');
  LReview := '';
  try
    LPrimary := Call('nyx_session', [], False).Field('revision').AsInteger;
    LReply := Call('nyx_reviews', [NyxField('mode', NyxData('create')),
      NyxField('base', NyxData('empty')), NyxField('label', NyxData('Numeric slider review')),
      NyxField('expectedRevision', NyxData(LPrimary)),
      NyxField('operationId', NyxData('slider-review-create'))], False);
    LReview := LReply.Field('review').AsText;
    LRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LInitial := Source;
    Call('nyx_transaction', [NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('slider-compose')), NyxField('operations', Composition)]);
    LRevision := Call('nyx_session', []).Field('revision').AsInteger;
    LSource := Source;
    Check((Pos('INyxSlider', LSource) > 0) and (Pos('NyxIntegerState', LSource) > 0),
      'specialized semantic slider companion');
    Call('nyx_node', [NyxField('id', NyxData('choice-slider')), NyxField('limit', NyxData(1))]);
    History('undo');
    Check(Source = LInitial, 'one paired Undo removes whole composition');
    Check(not Call('nyx_session', []).Field('canUndo').AsBoolean, 'composition has no hidden intermediate history');
    History('redo');
    Check(Source = LSource, 'one paired Redo restores exact source');
    ForceDirectories(AOutput);
    WriteBytes(IncludeTrailingPathDelimiter(AOutput) + 'nyx.generated.view.pas', LSource);
    Call('nyx_reviews', [NyxField('mode', NyxData('discard')), NyxField('review', NyxData(LReview)),
      NyxField('expectedRevision', NyxData(LRevision)),
      NyxField('operationId', NyxData('slider-review-discard'))], False);
    LReview := '';
    Check(Call('nyx_session', [], False).Field('revision').AsInteger = LPrimary,
      'primary project remains untouched');
    WriteLn('PASS ', GChecks, ' semantic slider companion checks');
  finally
    { Even a refused grouped composition leaves no review or agent presence. }
    try

      if LReview <> '' then
      begin
        LRevision := Call('nyx_session', []).Field('revision').AsInteger;
        Call('nyx_reviews', [NyxField('mode', NyxData('discard')), NyxField('review', NyxData(LReview)),
          NyxField('expectedRevision', NyxData(LRevision)),
          NyxField('operationId', NyxData('slider-review-cleanup'))], False);
      end;
    finally
      try
        LClient.Close;
      finally
        LClient.Free;
      end;
    end;
  end;
end;


{ This separate local pipeline is explicitly not an authenticated admission of
  the new policy by the frozen service. It reconstructs the exact exported seed,
  applies public typed contracts and emits the candidate both hosts compile. }
procedure EmitTypedCandidate(const AOutput: String);
var
  LFile: TFileStream;
  LBytes: UTF8String;
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
begin
  LFile := TFileStream.Create(IncludeTrailingPathDelimiter(AOutput) +
    'nyx.generated.view.pas', fmOpenRead or fmShareDenyWrite);
  LDocument := nil;
  LWorkspace := nil;
  try
    SetLength(LBytes, LFile.Size);

    if Length(LBytes) > 0 then
    begin
      LFile.ReadBuffer(LBytes[1], Length(LBytes));
    end;
    LDocument := TNyxSourceWorkspace.PrepareDraft(TNyxText(LBytes), LWorkspace);
    ConfigureNyxSliderCompanion(LDocument);
    WriteBytes(IncludeTrailingPathDelimiter(AOutput) + 'nyx.generated.slider.pas',
      TNyxCodegen.Generate(LDocument, 'nyx.generated.slider'));
  finally
    LWorkspace.Free;
    LDocument.Free;
    LFile.Free;
  end;
end;

begin
  try

    if ParamCount <> 2 then
    begin
      raise Exception.Create('Supply existing MCP configuration and a NEW export directory');
    end;
    { A retained successful export can be enriched after a local refusal without
      repeating authenticated editor mutations. The exact seed stays intact. }

    if ParamStr(1) <> '--enrich' then
    begin
      Semantic(ParamStr(1), ParamStr(2));
    end;
    EmitTypedCandidate(ParamStr(2));
    if GChecks > 0 then
    begin
      WriteLn('PASS ', GChecks, ' authenticated semantic seed checks; typed candidate emitted locally');
    end
    else
    begin
      WriteLn('PASS local typed enrichment of retained authenticated seed');
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
