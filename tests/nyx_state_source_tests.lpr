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


program nyx_state_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.state, nyx.binding.types,
  nyx.schema, nyx.source.preparation, nyx.studio.authoring, nyx.studio.projects,
  nyx.studio.session, nyx.test.agent.state
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxState.Create('State source intent: ' + AReason);
  end;
  Inc(GChecks);
end;

{ Replace one immutable wire field without sharing mutable JSON owners. }
function ReplaceField(const AObject: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AObject.Count);
  for LIndex := 0 to AObject.Count - 1 do
  begin
    LFields[LIndex] := NyxField(AObject.Key(LIndex), AObject.Field(AObject.Key(LIndex)));

    if LFields[LIndex].Name = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
    end;
  end;
  Result := NyxObject(LFields);
end;

procedure Run;
const
  CExactNumber: Double = 0.123456789012345;
var
  LSession: TNyxStudioSession;
  LOther: TNyxStudioSession;
  LSeed: TNyxProjectPair;
  LBefore: TNyxProjectPair;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LRead: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LReceived: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LWire: TNyxDataValue;
  LData: TNyxDataValue;
  LSpec: TNyxBindingSpec;
  LFailed: Boolean;

  procedure Intent(AAction: TNyxStudioDesignAction; const AName, AValue: TNyxText;
    AInput: TNyxStudioStateInput = ssiText);
  begin
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := AAction;
    LEdit.Selection := LSession.SelectedID;
    LEdit.View := LSession.ActiveViewID;
    LEdit.Name := AName;
    LEdit.Value := AValue;
    LEdit.StateInput := AInput;
  end;

  procedure Prepare;
  begin
    LBefore := LSession.ProjectSnapshot;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LWire := LRequest.ToData;
    LRead := ReadNyxStudioDesignRequest(LWire);
    Check(LRequest.SameRequest(LRead), 'Private version-three ticket keeps exact typed intent');
    LPrepared := PrepareNyxStudioDesign(LRead, LSchemas);
    LReceived := ReceiveNyxPreparedDesign(LPrepared.ToData, LRequest, LSchemas);
  end;

  procedure Admit;
  begin
    Prepare;
    Check((LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied) and
      (Pos('function StateWorkshopNote', LSession.Source) > 0),
      'Independent scalar/binding command publishes the pair and retains its handwritten helper');
    LReceived := nil;
    LPrepared := nil;
  end;

  procedure UndoExact;
  begin
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore),
      'One Undo restores the exact design/source pair');
    LSession.Redo;
  end;

  procedure Refuse;
  begin
    Prepare;
    Check((LSession.CompleteDesignRequest(LRequest, LReceived) = nscRejected) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LBefore)),
      'Rejected intent retains exact accepted pair, draft and history');
    LReceived := nil;
    LPrepared := nil;
  end;

  procedure RefuseWire(const AData: TNyxDataValue);
  begin
    LFailed := False;
    try
      LRead := ReadNyxStudioDesignRequest(AData);
    except
      on LException: Exception do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed, 'Malformed typed processor ticket refuses before preparation');
  end;

begin
  LSeed := CreateNyxAgentStateSeed;
  LSession := TNyxStudioSession.Create(LSeed);
  LOther := TNyxStudioSession.Create(LSeed);
  try
    LSchemas := CaptureNyxSchemas;
    Intent(sdaCreateStateDefault, 'exact-text', '"Hello 🌙\u0000world"', ssiEscapedText);
    Admit;
    Check(LSession.Document.State.Value('exact-text').TextValue = TNyxText('Hello 🌙' + #0 + 'world'),
      'Escaped editor notation retains supplementary text and embedded NUL');
    UndoExact;

    Intent(sdaSetStateDefault, 'checked', 'true', ssiBoolean);
    Admit;
    Check(LSession.Document.State.Value('checked').BooleanValue, 'Boolean keeps its declared family');
    Intent(sdaSetStateDefault, 'quantity', '-2147483648', ssiInteger);
    Admit;
    Check(LSession.Document.State.Value('quantity').IntegerValue = Low(Integer),
      'Signed lower bound remains exact');
    Intent(sdaSetStateDefault, 'ratio', '0.123456789012345', ssiNumber);
    Admit;
    Check(LSession.Document.State.Value('ratio').NumberValue = CExactNumber,
      'Number retains its complete admitted precision');
    UndoExact;

    Intent(sdaSetStateDefault, 'ratio', '-', ssiNumber);
    Refuse;
    Intent(sdaSetStateDefault, 'quantity', '1.5', ssiInteger);
    Refuse;
    Intent(sdaSetStateDefault, 'checked', '1', ssiBoolean);
    Refuse;
    Intent(sdaSetStateDefault, 'reply', '5', ssiInteger);
    Refuse;

    Intent(sdaRenameStateDefault, 'reply', 'response');
    Admit;
    Check(LSession.Document.State.Has('response') and
      (LSession.Document.Find('definition-editor').Bindings[0].StateName = 'response') and
      (LSession.Document.Find('review-label').Bindings[0].StateName = 'response'),
      'Rename migrates reusable and other-page references atomically');
    UndoExact;
    Intent(sdaRenameStateDefault, 'response', 'checked');
    Refuse;
    Intent(sdaRemoveStateDefault, 'response', '');
    Refuse;
    Intent(sdaRemoveStateDefault, 'exact-text', '');
    Admit;
    Check(not LSession.Document.State.Has('exact-text'), 'Unused scalar removal updates the pair');
    UndoExact;

    LSession.Select('first-editor');
    Intent(sdaSetBinding, '', '');
    LEdit.Binding := TNyxBindingSpec.Clear(bpValue);
    Admit;
    Check(not LSession.Selected.FindBinding(bpValue, LSpec),
      'Clear creates a descriptor on the captured authored override only');
    UndoExact;
    Intent(sdaInheritBinding, '', '');
    LEdit.Binding := TNyxBindingSpec.Clear(bpValue);
    Admit;
    Check(LSession.Selected.BindingCount = 0, 'Inherit removes only the local descriptor');
    UndoExact;

    Intent(sdaSetBinding, '', '');
    LEdit.Binding := TNyxBindingSpec.Bound(bpValue, 'response', nskText, bdFromState);
    Prepare;
    LSession.Select('reply-memo');
    Check((LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied) and
      (LSession.SelectedID = 'reply-memo') and
      (LSession.Document.Find('first-editor').Bindings[0].Direction = bdFromState) and
      (LSession.Document.Find('reply-memo').BindingCount = 0),
      'Later selection cannot retarget captured binding intent');
    LReceived := nil;
    LPrepared := nil;

    Intent(sdaSetBinding, '', '');
    LEdit.Binding := TNyxBindingSpec.Bound(bpValue, 'checked', nskBoolean, bdTwoWay);
    Refuse;
    LData := LWire.Field('edit');
    RefuseWire(ReplaceField(LWire, 'version', NyxData(2)));
    RefuseWire(ReplaceField(LWire, 'edit', ReplaceField(LData, 'stateInput', NyxData(999))));
    RefuseWire(ReplaceField(LWire, 'edit', ReplaceField(LData, 'binding', NyxNull)));
    RefuseWire(ReplaceField(LWire, 'edit', ReplaceField(LData, 'binding',
      ReplaceField(LData.Field('binding'), 'direction', NyxData(999)))));
    RefuseWire(ReplaceField(LWire, 'edit', ReplaceField(LData, 'stateInput', NyxData(Ord(ssiNumber)))));

    Intent(sdaSetStateDefault, 'response', 'Unadmitted response');
    Prepare;
    Check(LOther.CompleteDesignRequest(LRequest, LReceived) = nscStale,
      'Another owner refuses the identical processor ticket');
    LSession.SetSourceDraft(LSession.Source + #10 + '{ Unfinished application draft }');
    Check((LSession.CompleteDesignRequest(LRequest, LReceived) = nscStale) and
      (LSession.Document.State.Value('response').TextValue = 'Ready to compose.'),
      'A later Pascal draft prevents scalar publication');
    LReceived := nil;
    LPrepared := nil;
    LSession.DiscardSourceDraft;

    Intent(sdaSetStateDefault, 'response', 'Retired response');
    Prepare;
    LSession.LoadProject(LSession.ProjectSnapshot);
    Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscStale,
      'A later load retires captured scalar intent despite matching names');
  finally
    LReceived := nil;
    LPrepared := nil;
    LOther.Free;
    LSession.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-state-source', 'passed');
    document.body.setAttribute('data-state-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' typed state/binding source checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-state-source', 'failed');
      document.body.setAttribute('data-state-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
