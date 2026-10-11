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

{ Shared typed callback expressions. Open handler/registration names are quoted
  distinct references; trigger and scheduler choices remain Pascal contracts. }
function PascalCallbackPolicy(AValue: TNyxExecutionPolicy): TNyxText;
const
  CPolicies: array[TNyxExecutionPolicy] of TNyxText =
    ('neSequential', 'neAsynchronous', 'neUIQueue', 'neThreaded');
begin
  Result := CPolicies[AValue];
end;

function PascalCallbackEvent(const AOwner: TNyxText;
  const AInfo: TNyxAuthoredEventInfo): TNyxText;
begin
  Result := 'NyxCallbacks(' + AOwner + ').';

  if AInfo.Trigger = ntNamed then
  begin
    Result := Result + 'OnNamed(' + PascalEventReference(AInfo.Name.Name) + ')';
  end
  else
  begin
    Result := Result + NyxTriggerTitle(AInfo.Trigger);
  end;
end;

procedure EmitAuthoredCallbacks(const AInfos: TNyxAuthoredEventInfos;
  const AOwner: TNyxText; ALines: TNyxStrings; const AIndent: TNyxText);
var
  LEvent: Integer;
  LCallback: Integer;
begin

  if Length(AInfos) = 0 then
  begin
    ALines.Add(AIndent + 'NyxCallbacks(' + AOwner + ').Clear;');
  end;
  for LEvent := 0 to High(AInfos) do
  begin
    ALines.Add(AIndent + PascalCallbackEvent(AOwner, AInfos[LEvent]));
    ALines.Add(AIndent + '  .Policy(' + PascalCallbackPolicy(AInfos[LEvent].Policy) + ')');
    for LCallback := 0 to High(AInfos[LEvent].Callbacks) do
    begin
      ALines.Add(AIndent + '  .Add(NyxHandler(' +
        PascalString(AInfos[LEvent].Callbacks[LCallback].Handler.Name) +
        '), NyxCallbackID(' + PascalString(AInfos[LEvent].Callbacks[LCallback].ID.Name) + '))');
    end;
    ALines[ALines.Count - 1] := ALines[ALines.Count - 1] + ';';
  end;
end;

{ Simulate exactly the emitted operations on the independent candidate. Do not
  assign the final descriptor: that would hide unexpressed event reordering,
  selective event deletion or opaque extension fields from the whole-codec gate.
  Untouched policies/registrations still come from the handwritten builder on
  every execution; no callback implementation is copied, removed or rewritten. }
function CallbackCustomization(ABefore, AAfter, ACopy: TNyxNode;
  const AOwner: TNyxText): TNyxText;
var
  LBefore: TNyxAuthoredEventInfos;
  LAfter: TNyxAuthoredEventInfos;
  LCurrent: TNyxAuthoredEventInfos;
  LLines: TNyxStrings;
  LEvent: Integer;
  LPrevious: Integer;
  LCallback: Integer;
  LFound: Integer;
  LStream: INyxAuthoredEvent;
  LHead: TNyxText;

  function EventIndex(const AInfos: TNyxAuthoredEventInfos;
    const AInfo: TNyxAuthoredEventInfo): Integer;
  var
    LIndex: Integer;
  begin
    for LIndex := 0 to High(AInfos) do
    begin

      if (AInfos[LIndex].Trigger = AInfo.Trigger) and
        (AInfos[LIndex].Name.Name = AInfo.Name.Name) then
      begin
        Exit(LIndex);
      end;
    end;
    Result := -1;
  end;

  function CallbackIndex(const AInfo: TNyxAuthoredEventInfo;
    const AID: TNyxCallbackRef): Integer;
  var
    LIndex: Integer;
  begin
    for LIndex := 0 to High(AInfo.Callbacks) do
    begin

      if AInfo.Callbacks[LIndex].ID.Name = AID.Name then
      begin
        Exit(LIndex);
      end;
    end;
    Result := -1;
  end;

  procedure Statement(const ACall: TNyxText);
  begin
    LLines.Add('  ' + LHead);
    LLines.Add('    .' + ACall + ';');
  end;
begin
  Result := '';

  if ABefore.Extensions.Has(NyxExtension(NyxCallbacksKey)) =
    AAfter.Extensions.Has(NyxExtension(NyxCallbacksKey)) then
  begin

    if not AAfter.Extensions.Has(NyxExtension(NyxCallbacksKey)) or
      (NyxCallbacks(ABefore).Snapshot.ToJSON = NyxCallbacks(AAfter).Snapshot.ToJSON) then
    begin
      Exit;
    end;
  end;

  if not AAfter.Extensions.Has(NyxExtension(NyxCallbacksKey)) then
  begin
    NyxCallbacks(ACopy).Inherit;
    Exit('  NyxCallbacks(' + AOwner + ').Inherit;' + #10);
  end;
  LBefore := NyxAuthoredEvents(ABefore);
  LAfter := NyxAuthoredEvents(AAfter);

  if Length(LAfter) = 0 then
  begin
    NyxCallbacks(ACopy).Clear;
    Exit('  NyxCallbacks(' + AOwner + ').Clear;' + #10);
  end;
  LLines := TNyxStrings.Create;
  try
    for LEvent := 0 to High(LAfter) do
    begin
      LHead := PascalCallbackEvent(AOwner, LAfter[LEvent]);

      if LAfter[LEvent].Trigger = ntNamed then
      begin
        LStream := NyxCallbacks(ACopy).OnNamed(LAfter[LEvent].Name);
      end
      else
      begin
        LStream := NyxCallbacks(ACopy).On(LAfter[LEvent].Trigger);
      end;
      LPrevious := EventIndex(LBefore, LAfter[LEvent]);

      if (LPrevious < 0) or (LBefore[LPrevious].Policy <> LAfter[LEvent].Policy) then
      begin
        LStream.Policy(LAfter[LEvent].Policy);
        Statement('Policy(' + PascalCallbackPolicy(LAfter[LEvent].Policy) + ')');
      end;

      if LPrevious >= 0 then
      begin
        for LCallback := 0 to High(LBefore[LPrevious].Callbacks) do
        begin
          LFound := CallbackIndex(LAfter[LEvent], LBefore[LPrevious].Callbacks[LCallback].ID);

          if (LFound < 0) or
            (LAfter[LEvent].Callbacks[LFound].Handler.Name <>
              LBefore[LPrevious].Callbacks[LCallback].Handler.Name) then
          begin
            LStream.Remove(LBefore[LPrevious].Callbacks[LCallback].ID);
            Statement('Remove(NyxCallbackID(' +
              PascalString(LBefore[LPrevious].Callbacks[LCallback].ID.Name) + '))');
          end;
        end;
      end;
      for LCallback := 0 to High(LAfter[LEvent].Callbacks) do
      begin
        LCurrent := NyxAuthoredEvents(ACopy);
        LFound := EventIndex(LCurrent, LAfter[LEvent]);
        LFound := CallbackIndex(LCurrent[LFound], LAfter[LEvent].Callbacks[LCallback].ID);

        if LFound < 0 then
        begin
          LStream.Add(LAfter[LEvent].Callbacks[LCallback].Handler,
            LAfter[LEvent].Callbacks[LCallback].ID);
          Statement('Add(NyxHandler(' +
            PascalString(LAfter[LEvent].Callbacks[LCallback].Handler.Name) +
            '), NyxCallbackID(' + PascalString(LAfter[LEvent].Callbacks[LCallback].ID.Name) + '))');
        end;
        LCurrent := NyxAuthoredEvents(ACopy);
        LFound := EventIndex(LCurrent, LAfter[LEvent]);
        LFound := CallbackIndex(LCurrent[LFound], LAfter[LEvent].Callbacks[LCallback].ID);

        if LFound <> LCallback then
        begin
          LStream.Move(LAfter[LEvent].Callbacks[LCallback].ID, LCallback);
          Statement('Move(NyxCallbackID(' +
            PascalString(LAfter[LEvent].Callbacks[LCallback].ID.Name) + '), ' +
            PascalInteger(LCallback) + ')');
        end;
      end;
    end;
    Result := LLines.Text;
  finally
    LStream := nil;
    LLines.Free;
  end;
end;
