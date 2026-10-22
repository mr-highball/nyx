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

unit nyx.studio.callbackedits;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.schema,
  nyx.callbacks, nyx.scheduler, nyx.studio.session, nyx.studio.projects,
  nyx.studio.callbackintent;

const
  NyxMaximumCallbackChanges = 32;

type
  { Authoring commands use closed Pascal choices. Open owner and registration
    identities remain data; JSON spellings are confined to Read/Encode below. }
  TNyxCallbackChangeKind = (nccAdd, nccPolicy, nccMove, nccRemove);
  TNyxCallbackEvent = record
  private
    FTrigger: TNyxTrigger;
    FName: TNyxEventRef;
  public
    property Trigger: TNyxTrigger read FTrigger;
    property Name: TNyxEventRef read FName;
  end;
  TNyxCallbackChange = record
  private
    FKind: TNyxCallbackChangeKind;
    FOwner: TNyxText;
    FEvent: TNyxCallbackEvent;
    FRegistration: TNyxCallbackRef;
    FPolicy: TNyxExecutionPolicy;
    FPosition: Integer;
  public
    property Kind: TNyxCallbackChangeKind read FKind;
    property Owner: TNyxText read FOwner;
    property Event: TNyxCallbackEvent read FEvent;
    property Registration: TNyxCallbackRef read FRegistration;
    property Policy: TNyxExecutionPolicy read FPolicy;
    property Position: Integer read FPosition;
  end;
  { Independent inspection values. Replacing a returned array member cannot
    change a captured batch or another reader; no patch/session is retained. }
  TNyxCallbackChanges = array of TNyxCallbackChange;
  TNyxCallbackEditResult = record
    Kind: TNyxCallbackChangeKind;
    Owner: TNyxText;
    Event: TNyxCallbackEvent;
    Registration: TNyxCallbackInfo;
    Policy: TNyxExecutionPolicy;
    Position: Integer;
    { One-based location of a local Invoke implementation, or zero for an
      external callback. Removed registrations retain their implementation. }
    SourceLine: Integer;
    Warning: TNyxText;
  end;
  TNyxCallbackEditResults = array of TNyxCallbackEditResult;

  { An immutable owned patch prepares a complete independent project through
    the inspector's existing commands. Candidate never changes its supplier.
    The caller admits response size/removal confirmation before publishing the
    resulting pair once through AdoptProject, yielding one content undo step. }
  INyxCallbackPatch = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001004002701}']
    function Candidate(ASession: TNyxStudioSession;
      out AResults: TNyxCallbackEditResults): TNyxProjectPair;
  end;

  { Optional planning capability preserves the original patch GUID/layout.
    Sources reserve names only; they never grant executed admission. Planning
    accepts an already literal detached session, retaining all typed edit guards. }
  INyxCallbackPlan = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001004002702}']
    function ToData: TNyxDataValue;
    function CandidateWithReservedSource(ASession: TNyxStudioSession;
      const AReservedSource: TNyxText; out AResults: TNyxCallbackEditResults): TNyxProjectPair;
  end;

function NyxCallbackEvent(ATrigger: TNyxTrigger): TNyxCallbackEvent; overload;
function NyxCallbackEvent(const AName: TNyxEventRef): TNyxCallbackEvent; overload;
function NyxAddCallback(const AOwner: TNyxText;
  const AEvent: TNyxCallbackEvent): TNyxCallbackChange;
function NyxSetCallbackPolicy(const AOwner: TNyxText;
  const AEvent: TNyxCallbackEvent; APolicy: TNyxExecutionPolicy): TNyxCallbackChange;
function NyxMoveCallback(const AOwner: TNyxText; const AEvent: TNyxCallbackEvent;
  const ARegistration: TNyxCallbackRef; APosition: Integer): TNyxCallbackChange;
function NyxRemoveCallback(const AOwner: TNyxText; const AEvent: TNyxCallbackEvent;
  const ARegistration: TNyxCallbackRef): TNyxCallbackChange;
function NyxCallbackPatch(const AChanges: array of TNyxCallbackChange): INyxCallbackPatch;
{ Capture built-in or substitutable snapshot-capable patches without retaining
  their interface in a queued record. Unknown implementations refuse. }
function CaptureNyxCallbackBatch(const APatch: INyxCallbackPatch): TNyxCallbackBatchSnapshot;
{ Inspect the existing normalized snapshot through the same strict decoder as
  ReadNyxCallbackPatch. Undefined or malformed input refuses without a session,
  candidate or source effect; the returned ordered values are independently owned. }
function ReadNyxCallbackChanges(
  const ABatch: TNyxCallbackBatchSnapshot): TNyxCallbackChanges;
{ Recognize only [Policy, Add] for one exact owner/trigger/name. Other valid
  batches return False with cleared outputs. Shape is not policy permission or
  proof of an absent stream; the current candidate still owns those checks. }
function TryNyxCallbackPolicyAddShape(const ABatch: TNyxCallbackBatchSnapshot;
  out AOwner: TNyxText; out AEvent: TNyxCallbackEvent;
  out APolicy: TNyxExecutionPolicy): Boolean;
{ Strict wire admission and bounded-result encoding are explicit persistence/
  transport boundaries. Unknown fields, wrong scalar types and ambiguous event
  identities reject before a temporary session or active project is edited. }
function ReadNyxCallbackPatch(const AChanges: TNyxDataValue): INyxCallbackPatch;
function EncodeNyxCallbackResults(const AResults: TNyxCallbackEditResults): TNyxDataValue;
{ Shared inspector/agent warning: definitions and local inherited overrides have
  different reach. Handler code is retained in both cases, never blindly deleted. }
function NyxCallbackRemovalWarning(ADocument: TNyxDocument; const AOwner: TNyxText;
  const AHandler: TNyxHandlerRef): TNyxText;

implementation

uses
  nyx.source;

type
  TNyxCallbackPatch = class(TInterfacedObject, INyxCallbackPatch, INyxCallbackPlan)
  private
    FChanges: array of TNyxCallbackChange;
  public
    constructor Create(const AChanges: array of TNyxCallbackChange);
    function Candidate(ASession: TNyxStudioSession;
      out AResults: TNyxCallbackEditResults): TNyxProjectPair;
    function ToData: TNyxDataValue;
    function CandidateWithReservedSource(ASession: TNyxStudioSession;
      const AReservedSource: TNyxText; out AResults: TNyxCallbackEditResults): TNyxProjectPair;
  end;

const
  CNames: array[TNyxCallbackChangeKind] of TNyxText = ('add', 'policy', 'move', 'remove');

function NyxCallbackEvent(ATrigger: TNyxTrigger): TNyxCallbackEvent;
begin

  if not NyxIsRuntimeTrigger(ATrigger) then
  begin
    raise ENyxModel.Create('A named callback requires an exact event reference');
  end;
  Result := Default(TNyxCallbackEvent);
  Result.FTrigger := ATrigger;
end;

function NyxCallbackEvent(const AName: TNyxEventRef): TNyxCallbackEvent;
begin
  Result := Default(TNyxCallbackEvent);
  Result.FTrigger := ntNamed;
  Result.FName := NyxEvent(AName.Name);
end;

function Change(AKind: TNyxCallbackChangeKind; const AOwner: TNyxText;
  const AEvent: TNyxCallbackEvent): TNyxCallbackChange;
begin
  Result := Default(TNyxCallbackChange);
  Result.FKind := AKind;
  Result.FOwner := NyxCallbackID(AOwner).Name;
  Result.FEvent := AEvent;

  if AEvent.Trigger = ntNamed then
  begin
    Result.FEvent := NyxCallbackEvent(AEvent.Name);
  end
  else
  begin
    Result.FEvent := NyxCallbackEvent(AEvent.Trigger);
  end;
end;

function NyxAddCallback(const AOwner: TNyxText;
  const AEvent: TNyxCallbackEvent): TNyxCallbackChange;
begin
  Result := Change(nccAdd, AOwner, AEvent);
end;

function NyxSetCallbackPolicy(const AOwner: TNyxText;
  const AEvent: TNyxCallbackEvent; APolicy: TNyxExecutionPolicy): TNyxCallbackChange;
begin
  Result := Change(nccPolicy, AOwner, AEvent);
  Result.FPolicy := APolicy;
end;

function NyxMoveCallback(const AOwner: TNyxText; const AEvent: TNyxCallbackEvent;
  const ARegistration: TNyxCallbackRef; APosition: Integer): TNyxCallbackChange;
begin
  Result := Change(nccMove, AOwner, AEvent);
  Result.FRegistration := NyxCallbackID(ARegistration.Name);
  Result.FPosition := APosition;
end;

function NyxRemoveCallback(const AOwner: TNyxText; const AEvent: TNyxCallbackEvent;
  const ARegistration: TNyxCallbackRef): TNyxCallbackChange;
begin
  Result := Change(nccRemove, AOwner, AEvent);
  Result.FRegistration := NyxCallbackID(ARegistration.Name);
end;

constructor TNyxCallbackPatch.Create(const AChanges: array of TNyxCallbackChange);
var
  LIndex: Integer;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > NyxMaximumCallbackChanges) then
  begin
    raise ENyxModel.Create('A callback batch contains 1..32 changes');
  end;
  SetLength(FChanges, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    { Recheck default/uninitialized references even for typed callers. }
    FChanges[LIndex] := Change(AChanges[LIndex].Kind,
      AChanges[LIndex].Owner, AChanges[LIndex].Event);
    FChanges[LIndex].FPolicy := AChanges[LIndex].Policy;

    if AChanges[LIndex].Kind in [nccMove, nccRemove] then
    begin
      FChanges[LIndex].FRegistration := NyxCallbackID(AChanges[LIndex].Registration.Name);
    end;
    FChanges[LIndex].FPosition := AChanges[LIndex].Position;
  end;
end;

function NyxCallbackPatch(const AChanges: array of TNyxCallbackChange): INyxCallbackPatch;
begin
  Result := TNyxCallbackPatch.Create(AChanges);
end;

function NyxCallbackRemovalWarning(ADocument: TNyxDocument; const AOwner: TNyxText;
  const AHandler: TNyxHandlerRef): TNyxText;
var
  LNode: TNyxNode;
  LIndex: Integer;
begin
  Result := AHandler.Name + ' will stop receiving this event for ' + AOwner +
    '. Its Pascal implementation is retained. Inherited changes create a local override.';
  LNode := ADocument.Find(AOwner);

  if LNode = nil then
  begin
    Exit;
  end;
  while LNode.Parent <> nil do
  begin
    LNode := LNode.Parent;
  end;
  for LIndex := 0 to ADocument.ComponentCount - 1 do
  begin

    if ADocument.Components[LIndex] = LNode then
    begin
      Result := AHandler.Name + ' will stop receiving this event in reusable definition ' +
        LNode.ID + '. Inheriting instances are affected. Its Pascal implementation is retained.';
      Exit;
    end;
  end;
end;

procedure EventInfo(ASession: TNyxStudioSession; const AEvent: TNyxCallbackEvent;
  const ARegistration: TNyxCallbackRef; out AInfo: TNyxCallbackInfo;
  out APolicy: TNyxExecutionPolicy; out APosition: Integer);
var
  LProjection: TNyxNode;
  LEvents: TNyxAuthoredEventInfos;
  LEventIndex: Integer;
  LIndex: Integer;
begin
  AInfo := Default(TNyxCallbackInfo);
  APolicy := neSequential;
  APosition := -1;
  LProjection := ASession.SelectedProjection;
  try
    LEvents := NyxAuthoredEvents(LProjection);
    for LEventIndex := 0 to High(LEvents) do
    begin

      if (LEvents[LEventIndex].Trigger = AEvent.Trigger) and
        (LEvents[LEventIndex].Name.Name = AEvent.Name.Name) then
      begin
        APolicy := LEvents[LEventIndex].Policy;
        for LIndex := 0 to High(LEvents[LEventIndex].Callbacks) do
        begin

          if LEvents[LEventIndex].Callbacks[LIndex].ID.Name = ARegistration.Name then
          begin
            AInfo := LEvents[LEventIndex].Callbacks[LIndex];
            APosition := LIndex;
            Exit;
          end;
        end;
        Exit;
      end;
    end;
  finally
    LProjection.Free;
  end;
end;

function TNyxCallbackPatch.Candidate(ASession: TNyxStudioSession;
  out AResults: TNyxCallbackEditResults): TNyxProjectPair;
begin
  Result := CandidateWithReservedSource(ASession, '', AResults);
end;

function TNyxCallbackPatch.CandidateWithReservedSource(ASession: TNyxStudioSession;
  const AReservedSource: TNyxText; out AResults: TNyxCallbackEditResults): TNyxProjectPair;
var
  LCandidate: TNyxStudioSession;
  LIndex: Integer;
  LMetadataIndex: Integer;
  LChange: TNyxCallbackChange;
  LNode: TNyxNode;
  LMetadata: TNyxEventSchemas;
  LSupported: Boolean;
  LLine: Integer;
  LRegistration: TNyxCallbackRef;
begin
  AResults := nil;

  if (ASession = nil) or (ASession.DraftSource <> ASession.Source) then
  begin
    raise ENyxModel.Create('Resolve the pending Pascal draft before editing callbacks');
  end;
  LCandidate := TNyxStudioSession.ForCallbackPlanning(ASession.ProjectSnapshot, AReservedSource);
  try
    SetLength(AResults, Length(FChanges));
    for LIndex := 0 to High(FChanges) do
    begin
      LChange := FChanges[LIndex];
      LNode := LCandidate.Document.Find(LChange.Owner);

      if LNode = nil then
      begin
        raise ENyxModel.Create('Callback owner is missing: ' + LChange.Owner);
      end;
      while LNode.Parent <> nil do
      begin
        LNode := LNode.Parent;
      end;
      LCandidate.Activate(LNode.ID);
      LCandidate.Select(LChange.Owner);
      LMetadata := NyxEventsMetadata(LCandidate.Selected, LCandidate.Document);
      LSupported := False;
      for LMetadataIndex := 0 to High(LMetadata) do
      begin
        LSupported := LSupported or
          ((LMetadata[LMetadataIndex].Trigger = LChange.Event.Trigger) and
           (LMetadata[LMetadataIndex].Name.Name = LChange.Event.Name.Name));
      end;

      if not LSupported then
      begin
        raise ENyxModel.Create('The callback owner does not publish this exact event');
      end;
      AResults[LIndex].Kind := LChange.Kind;
      AResults[LIndex].Owner := LChange.Owner;
      AResults[LIndex].Event := LChange.Event;
      EventInfo(LCandidate, LChange.Event, LChange.Registration,
        AResults[LIndex].Registration, AResults[LIndex].Policy, AResults[LIndex].Position);

      if (LChange.Kind in [nccMove, nccRemove]) and (AResults[LIndex].Position < 0) then
      begin
        raise ENyxModel.Create('The exact event registration is missing');
      end;
      case LChange.Kind of
        nccAdd:
          begin

            if LChange.Event.Trigger = ntNamed then
            begin
              AResults[LIndex].Registration.Handler := LCandidate.AddCallback(LChange.Event.Name, LLine);
            end
            else
            begin
              AResults[LIndex].Registration.Handler := LCandidate.AddCallback(LChange.Event.Trigger, LLine);
            end;
            AResults[LIndex].Registration.ID := NyxCallbackID(AResults[LIndex].Registration.Handler.Name);
          end;
        nccPolicy:
          begin

            if LChange.Event.Trigger = ntNamed then
            begin
              LCandidate.SetCallbackPolicy(LChange.Event.Name, LChange.Policy);
            end
            else
            begin
              LCandidate.SetCallbackPolicy(LChange.Event.Trigger, LChange.Policy);
            end;
          end;
        nccMove:
          begin

            if LChange.Event.Trigger = ntNamed then
            begin
              LCandidate.MoveCallback(LChange.Event.Name, LChange.Registration, LChange.Position);
            end
            else
            begin
              LCandidate.MoveCallback(LChange.Event.Trigger, LChange.Registration, LChange.Position);
            end;
          end;
        nccRemove:
          begin
            AResults[LIndex].Warning := NyxCallbackRemovalWarning(LCandidate.Document,
              LChange.Owner, AResults[LIndex].Registration.Handler);

            if LChange.Event.Trigger = ntNamed then
            begin
              LCandidate.RemoveCallback(LChange.Event.Name, LChange.Registration);
            end
            else
            begin
              LCandidate.RemoveCallback(LChange.Event.Trigger, LChange.Registration);
            end;
          end;
      end;

      if LChange.Kind <> nccRemove then
      begin
        { Copy the identity before passing the containing record as out. Native
          out initialization must not erase an aliased const reference. Results
          describe each operation's immediate outcome; lines use final source. }
        LRegistration := AResults[LIndex].Registration.ID;
        EventInfo(LCandidate, LChange.Event, LRegistration,
          AResults[LIndex].Registration, AResults[LIndex].Policy, AResults[LIndex].Position);
      end;
    end;
    Result := LCandidate.ProjectSnapshot;
    { Compute navigation against the final grouped source, after later edits
      may have shifted earlier handlers. External implementations have no local
      location; missing local code never authorizes deleting an implementation. }
    for LIndex := 0 to High(AResults) do
    begin

      if AResults[LIndex].Registration.Handler.Name <> '' then
      begin
        try
          AResults[LIndex].SourceLine := NyxHandlerSourceLine(Result.Source,
            AResults[LIndex].Registration.Handler);
        except
          on ENyxSource do
          begin
            AResults[LIndex].SourceLine := 0;
          end;
        end;
      end;
    end;
  finally
    LCandidate.Free;
  end;
end;

function TNyxCallbackPatch.ToData: TNyxDataValue;
var
  LChanges: array of TNyxDataValue;
  LFields: array of TNyxDataField;
  LEvent: TNyxDataValue;
  LIndex: Integer;
begin
  SetLength(LChanges, Length(FChanges));
  for LIndex := 0 to High(FChanges) do
  begin

    if FChanges[LIndex].Event.Trigger = ntNamed then
    begin
      LEvent := NyxObject([NyxField('name', NyxData(FChanges[LIndex].Event.Name.Name))]);
    end
    else
    begin
      LEvent := NyxObject([NyxField('trigger', NyxData(NyxTriggerName(FChanges[LIndex].Event.Trigger)))]);
    end;
    SetLength(LFields, 3);
    LFields[0] := NyxField('op', NyxData(CNames[FChanges[LIndex].Kind]));
    LFields[1] := NyxField('id', NyxData(FChanges[LIndex].Owner));
    LFields[2] := NyxField('event', LEvent);

    if FChanges[LIndex].Kind = nccPolicy then
    begin
      SetLength(LFields, 4);
      LFields[3] := NyxField('policy', NyxData(NyxPolicyName(FChanges[LIndex].Policy)));
    end;

    if FChanges[LIndex].Kind in [nccMove, nccRemove] then
    begin
      SetLength(LFields, 4);
      LFields[3] := NyxField('registration', NyxData(FChanges[LIndex].Registration.Name));
    end;

    if FChanges[LIndex].Kind = nccMove then
    begin
      SetLength(LFields, 5);
      LFields[4] := NyxField('index', NyxData(FChanges[LIndex].Position));
    end;
    LChanges[LIndex] := NyxObject(LFields);
  end;
  Result := NyxArray(LChanges);
end;

function CaptureNyxCallbackBatch(const APatch: INyxCallbackPatch): TNyxCallbackBatchSnapshot;
var
  LPlan: INyxCallbackPlan;
begin

  if (APatch = nil) or not Supports(APatch, INyxCallbackPlan, LPlan) then
  begin
    raise ENyxModel.Create('Callback intent requires a snapshot-capable typed patch');
  end;
  Result := ReadNyxCallbackBatchSnapshot(LPlan.ToData);
end;

procedure Fields(const AValue: TNyxDataValue; const AAllowed: TNyxText);
var
  LIndex: Integer;
begin

  if AValue.Kind <> ndObject then
  begin
    raise ENyxModel.Create('A callback change/event must be an object');
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    if Pos('|' + AValue.Key(LIndex) + '|', AAllowed) = 0 then
    begin
      raise ENyxModel.Create('Unknown callback field: ' + AValue.Key(LIndex));
    end;
  end;
end;

function Has(const AValue: TNyxDataValue; const AKey: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AValue.Count - 1 do
  begin

    if AValue.Key(LIndex) = AKey then
    begin
      Exit(True);
    end;
  end;
end;

{ Decode once for both executable patch construction and inert inspection.
  Keeping every closed-field/reference/enum check here prevents presentation or
  receipt inspection from accepting a weaker interpretation of the same batch. }
function DecodeNyxCallbackChanges(const AChanges: TNyxDataValue): TNyxCallbackChanges;
var
  LChanges: TNyxCallbackChanges;
  LIndex: Integer;
  LValue: TNyxDataValue;
  LEventData: TNyxDataValue;
  LEvent: TNyxCallbackEvent;
  LOwner: TNyxText;
  LAction: TNyxText;
  LTrigger: TNyxTrigger;
  LPolicy: TNyxExecutionPolicy;
begin

  if (AChanges.Kind <> ndArray) or (AChanges.Count < 1) or
    (AChanges.Count > NyxMaximumCallbackChanges) then
  begin
    raise ENyxModel.Create('A callback batch contains 1..32 changes');
  end;
  SetLength(LChanges, AChanges.Count);
  for LIndex := 0 to AChanges.Count - 1 do
  begin
    LValue := AChanges.Item(LIndex);
    Fields(LValue, '|op|id|event|policy|registration|index|');
    LAction := LValue.Field('op').AsText;
    LOwner := LValue.Field('id').AsText;
    LEventData := LValue.Field('event');
    Fields(LEventData, '|trigger|name|');

    if Has(LEventData, 'trigger') = Has(LEventData, 'name') then
    begin
      raise ENyxModel.Create('A callback event has one exact trigger or semantic name');
    end;

    if Has(LEventData, 'name') then
    begin
      LEvent := NyxCallbackEvent(NyxEvent(LEventData.Field('name').AsText));
    end
    else
    begin

      if not TryNyxTrigger(LEventData.Field('trigger').AsText, LTrigger) then
      begin
        raise ENyxModel.Create('Unknown callback trigger');
      end;
      LEvent := NyxCallbackEvent(LTrigger);
    end;

    if LAction = 'add' then
    begin
      Fields(LValue, '|op|id|event|');
      LChanges[LIndex] := NyxAddCallback(LOwner, LEvent);
    end
    else if LAction = 'policy' then
    begin
      Fields(LValue, '|op|id|event|policy|');

      if not TryNyxPolicy(LValue.Field('policy').AsText, LPolicy) then
      begin
        raise ENyxModel.Create('Unknown callback execution policy');
      end;
      LChanges[LIndex] := NyxSetCallbackPolicy(LOwner, LEvent, LPolicy);
    end
    else if LAction = 'move' then
    begin
      Fields(LValue, '|op|id|event|registration|index|');
      LChanges[LIndex] := NyxMoveCallback(LOwner, LEvent,
        NyxCallbackID(LValue.Field('registration').AsText), LValue.Field('index').AsInteger);
    end
    else if LAction = 'remove' then
    begin
      Fields(LValue, '|op|id|event|registration|');
      LChanges[LIndex] := NyxRemoveCallback(LOwner, LEvent,
        NyxCallbackID(LValue.Field('registration').AsText));
    end
    else
    begin
      raise ENyxModel.Create('Unknown callback operation');
    end;
  end;
  Result := LChanges;
end;

function ReadNyxCallbackPatch(const AChanges: TNyxDataValue): INyxCallbackPatch;
begin
  Result := NyxCallbackPatch(DecodeNyxCallbackChanges(AChanges));
end;

function ReadNyxCallbackChanges(
  const ABatch: TNyxCallbackBatchSnapshot): TNyxCallbackChanges;
begin
  Result := DecodeNyxCallbackChanges(ABatch.ToData);
end;

function TryNyxCallbackPolicyAddShape(const ABatch: TNyxCallbackBatchSnapshot;
  out AOwner: TNyxText; out AEvent: TNyxCallbackEvent;
  out APolicy: TNyxExecutionPolicy): Boolean;
var
  LChanges: TNyxCallbackChanges;
begin
  AOwner := '';
  AEvent := Default(TNyxCallbackEvent);
  APolicy := neSequential;
  Result := False;
  LChanges := ReadNyxCallbackChanges(ABatch);

  if Length(LChanges) <> 2 then
  begin
    Exit;
  end;

  if (LChanges[0].Kind <> nccPolicy) or (LChanges[1].Kind <> nccAdd) or
    (LChanges[0].Owner <> LChanges[1].Owner) or
    (LChanges[0].Event.Trigger <> LChanges[1].Event.Trigger) or
    (LChanges[0].Event.Name.Name <> LChanges[1].Event.Name.Name) then
  begin
    Exit;
  end;
  AOwner := LChanges[0].Owner;
  AEvent := LChanges[0].Event;
  APolicy := LChanges[0].Policy;
  Result := True;
end;

function EncodeNyxCallbackResults(const AResults: TNyxCallbackEditResults): TNyxDataValue;
var
  LItems: array of TNyxDataValue;
  LIndex: Integer;
  LEvent: TNyxDataValue;
begin
  SetLength(LItems, Length(AResults));
  for LIndex := 0 to High(AResults) do
  begin
    LEvent := NyxObject([NyxField('trigger', NyxData(NyxTriggerName(AResults[LIndex].Event.Trigger)))]);

    if AResults[LIndex].Event.Trigger = ntNamed then
    begin
      LEvent := NyxObject([NyxField('name', NyxData(AResults[LIndex].Event.Name.Name))]);
    end;
    LItems[LIndex] := NyxObject([
      NyxField('op', NyxData(CNames[AResults[LIndex].Kind])),
      NyxField('id', NyxData(AResults[LIndex].Owner)), NyxField('event', LEvent),
      NyxField('registration', NyxData(AResults[LIndex].Registration.ID.Name)),
      NyxField('handler', NyxData(AResults[LIndex].Registration.Handler.Name)),
      NyxField('policy', NyxData(NyxPolicyName(AResults[LIndex].Policy))),
      NyxField('index', NyxData(AResults[LIndex].Position)),
      NyxField('line', NyxData(AResults[LIndex].SourceLine)),
      NyxField('warning', NyxData(AResults[LIndex].Warning))]);
  end;
  Result := NyxArray(LItems);
end;

end.
