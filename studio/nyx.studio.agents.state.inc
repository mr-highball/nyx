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

{ Included by nyx.studio.agents. Readonly queries borrow accepted defaults only
  during this call. Candidate application is detached and response-bounded before
  the sole publication; ordinary agent guards own permission/revision/receipts. }

type
  TNyxAgentStateMode = (asmDefaults, asmValue, asmBindings, asmApply);

const
  CStateModes: array[TNyxAgentStateMode] of TNyxText =
    ('defaults', 'value', 'bindings', 'apply');

function BindingData(const ASpec: TNyxBindingSpec): TNyxDataValue;
begin

  if ASpec.Cleared then
  begin
    Exit(NyxObject([
      NyxField('target', NyxData(NyxBindingPropertyName(ASpec.Target))),
      NyxField('cleared', NyxData(True))]));
  end;
  if ASpec.Source = bsResource then
  begin
    Exit(NyxObject([
      NyxField('target', NyxData(NyxBindingPropertyName(ASpec.Target))),
      NyxField('cleared', NyxData(False)), NyxField('source', NyxData('resource')),
      NyxField('value', ASpec.ResourceValue.ToData)]));
  end;
  Result := NyxObject([
    NyxField('target', NyxData(NyxBindingPropertyName(ASpec.Target))),
    NyxField('cleared', NyxData(False)), NyxField('name', NyxData(ASpec.StateName)),
    NyxField('kind', NyxData(NyxStateKindName(ASpec.ValueKind))),
    NyxField('direction', NyxData(NyxBindingDirectionName(ASpec.Direction)))]);
end;

{ FindBinding intentionally reports usable bindings only. Authoring discovery
  must also report an explicit cleared descriptor, which masks inheritance. }
function LocalBinding(ANode: TNyxNode; ATarget: TNyxBindingProperty;
  out ASpec: TNyxBindingSpec): Boolean;
var
  LIndex: Integer;
begin
  for LIndex := 0 to ANode.BindingCount - 1 do
  begin

    if ANode.Bindings[LIndex].Target = ATarget then
    begin
      ASpec := ANode.Bindings[LIndex].Copy;
      Exit(True);
    end;
  end;
  Result := False;
end;

function TNyxAgentSession.StateBindings(const AArguments: TNyxDataValue): TNyxDataValue;
var
  LMode: TNyxAgentStateMode;
  LFound: Boolean;
  LPatch: INyxStateBindingPatch;
  LPair: TNyxProjectPair;
  LValues: array of TNyxDataValue;
  LKinds: array of TNyxDataValue;
  LValue: TNyxStateValue;
  LKey: TNyxText;
  LFilter: TNyxText;
  LText: TNyxText;
  LOffset: Integer;
  LLimit: Integer;
  LTotal: Integer;
  LIndex: Integer;
  LCount: Integer;
  LKind: TNyxStateKind;
  LTarget: TNyxBindingProperty;
  LAllowed: TNyxStateKinds;
  LNode: TNyxNode;
  LRoot: TNyxNode;
  LProjection: TNyxNode;
  LLocal: TNyxBindingSpec;
  LEffective: TNyxBindingSpec;
  LHasLocal: Boolean;
  LHasEffective: Boolean;
  LLocalData: TNyxDataValue;
  LEffectiveData: TNyxDataValue;
begin
  LFound := False;
  for LMode := Low(TNyxAgentStateMode) to High(TNyxAgentStateMode) do
  begin

    if CStateModes[LMode] = TextArgument(AArguments, 'mode') then
    begin
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxState.Create('State mode must be defaults, value, bindings or apply');
  end;
  case LMode of
    asmApply:
      begin
        NyxAgentFields(AArguments, '|mode|expectedRevision|operationId|changes|');
        RequireRevision(AArguments);
        LPatch := ReadNyxStateBindingPatch(AArguments.Field('changes'));
        LPair := LPatch.Candidate(FSession.ProjectSnapshot);
        Result := NyxObject([NyxField('changes', NyxData(LPatch.Count))]);
        { Bound the complete eventual response before touching active files.
          Draft refusal, fresh domain/source admission and all command effects
          already ran on the independently owned candidate session. }
        BoundContext(WithResults(Summary, Result, 'stateBindings', True));
        FSession.AdoptProject(LPair);
      end;
    asmDefaults:
      begin
        NyxAgentFields(AArguments, '|mode|offset|limit|filter|');
        LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumStateEntries);
        LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 50);
        LFilter := TextArgument(AArguments, 'filter');
        LTotal := 0;
        LValues := nil;
        for LIndex := 0 to FSession.Document.State.Count - 1 do
        begin
          LKey := FSession.Document.State.Key(LIndex);

          if (LFilter <> '') and (Pos(LFilter, LKey) = 0) then
          begin
            Continue;
          end;

          if (LTotal >= LOffset) and (Length(LValues) < LLimit) then
          begin
            LValue := FSession.Document.State.Value(LKey);
            LCount := Length(LValues);
            SetLength(LValues, LCount + 1);

            if LValue.Kind = nskText then
            begin
              LText := TextSpan(LValue.TextValue, 0, 80, LCount);
              LValues[High(LValues)] := NyxObject([
                NyxField('name', NyxData(LKey)), NyxField('kind', NyxData('text')),
                NyxField('preview', NyxData(LText)),
                NyxField('characters', NyxData(LCount)),
                NyxField('truncated', NyxData(LCount > 80))]);
            end
            else
            begin
              LValues[High(LValues)] := NyxObject([
                NyxField('name', NyxData(LKey)),
                NyxField('kind', NyxData(NyxStateKindName(LValue.Kind))),
                NyxField('value', NyxStateValueData(LValue))]);
            end;
          end;
          Inc(LTotal);
        end;
        Result := NyxObject([
          NyxField('revision', NyxData(FRevision)), NyxField('offset', NyxData(LOffset)),
          NyxField('nextOffset', NyxData(Min(LOffset + Length(LValues), LTotal))),
          NyxField('total', NyxData(LTotal)), NyxField('defaults', NyxArray(LValues))]);
      end;
    asmValue:
      begin
        NyxAgentFields(AArguments, '|mode|name|offset|count|');
        LKey := AArguments.Field('name').AsText;
        LValue := FSession.Document.State.Value(LKey);

        if LValue.Kind = nskText then
        begin
          LOffset := IntegerArgument(AArguments, 'offset', 0, 0, NyxMaximumStateBytes);
          LLimit := IntegerArgument(AArguments, 'count', 2048, 1, 4096);
          LText := TextSpan(LValue.TextValue, LOffset, LLimit, LTotal);

          if LOffset > LTotal then
          begin
            raise ENyxState.Create('State text offset is beyond the accepted default');
          end;
          Result := NyxObject([
            NyxField('revision', NyxData(FRevision)), NyxField('name', NyxData(LKey)),
            NyxField('kind', NyxData('text')), NyxField('offset', NyxData(LOffset)),
            NyxField('nextOffset', NyxData(Min(LOffset + LLimit, LTotal))),
            NyxField('characters', NyxData(LTotal)), NyxField('text', NyxData(LText))]);
        end
        else
        begin

          if NyxAgentHas(AArguments, 'offset') or NyxAgentHas(AArguments, 'count') then
          begin
            raise ENyxState.Create('Only text defaults have scalar windows');
          end;
          Result := NyxObject([
            NyxField('revision', NyxData(FRevision)), NyxField('name', NyxData(LKey)),
            NyxField('kind', NyxData(NyxStateKindName(LValue.Kind))),
            NyxField('value', NyxStateValueData(LValue))]);
        end;
      end;
    asmBindings:
      begin
        NyxAgentFields(AArguments, '|mode|owner|offset|limit|');
        LKey := AArguments.Field('owner').AsText;
        LNode := FSession.Document.Find(LKey);

        if LNode = nil then
        begin
          raise ENyxState.Create('The exact authored binding owner is missing');
        end;
        LOffset := IntegerArgument(AArguments, 'offset', 0, 0, Ord(High(TNyxBindingProperty)) + 1);
        LLimit := IntegerArgument(AArguments, 'limit', 20, 1, 20);
        LRoot := RealizeNyxContext(FSession.Document, LNode, LProjection);
        try
          LTotal := 0;
          LValues := nil;
          for LTarget := Low(TNyxBindingProperty) to High(TNyxBindingProperty) do
          begin
            LAllowed := [];
            LHasEffective := False;

            if LProjection <> nil then
            begin
              LAllowed := NyxBindingKinds(LProjection, LTarget);
              LHasEffective := LProjection.FindBinding(LTarget, LEffective);
            end;
            LHasLocal := LocalBinding(LNode, LTarget, LLocal);

            if (LAllowed = []) and not LHasLocal and not LHasEffective then
            begin
              Continue;
            end;

            if (LTotal >= LOffset) and (Length(LValues) < LLimit) then
            begin
              LKinds := nil;
              for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
              begin

                if LKind in LAllowed then
                begin
                  SetLength(LKinds, Length(LKinds) + 1);
                  LKinds[High(LKinds)] := NyxData(NyxStateKindName(LKind));
                end;
              end;
              LLocalData := NyxNull;
              LEffectiveData := NyxNull;

              if LHasLocal then
              begin
                LLocalData := BindingData(LLocal);
              end;

              if LHasEffective then
              begin
                LEffectiveData := BindingData(LEffective);
              end;
              SetLength(LValues, Length(LValues) + 1);
              LValues[High(LValues)] := NyxObject([
                NyxField('target', NyxData(NyxBindingPropertyName(LTarget))),
                NyxField('kinds', NyxArray(LKinds)), NyxField('local', LLocalData),
                NyxField('effective', LEffectiveData),
                NyxField('inherited', NyxData(not LHasLocal and LHasEffective))]);
            end;
            Inc(LTotal);
          end;
          Result := NyxObject([
            NyxField('revision', NyxData(FRevision)), NyxField('owner', NyxData(LKey)),
            NyxField('offset', NyxData(LOffset)), NyxField('total', NyxData(LTotal)),
            NyxField('nextOffset', NyxData(Min(LOffset + Length(LValues), LTotal))),
            NyxField('projectionAvailable', NyxData(LProjection <> nil)),
            NyxField('bindings', NyxArray(LValues))]);
        finally
          LRoot.Free;
        end;
      end;
  end;
end;
