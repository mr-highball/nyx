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

{ Shared expansion/lexer supplies physical provenance. This pass records declared
  structure only; it neither evaluates expressions nor resolves inherited names. }

function TNyxPascalSymbolSite.QualifiedName: TNyxText;
begin
  Result := FName;

  if FScope <> '' then
  begin
    Result := FScope + '.' + FName;
  end;
end;

function NyxPascalSymbolKindName(AKind: TNyxPascalSymbolKind): TNyxText;
const
  CNames: array[TNyxPascalSymbolKind] of TNyxText = ('type', 'constant',
    'variable', 'field', 'property', 'routine', 'enum-member', 'resource-string');
begin

  if (Ord(AKind) < Ord(Low(TNyxPascalSymbolKind))) or
    (Ord(AKind) > Ord(High(TNyxPascalSymbolKind))) then
  begin
    raise ENyxModel.Create('Unknown declaration kind');
  end;
  Result := CNames[AKind];
end;

function NyxPascalSymbolVisibilityName(AVisibility: TNyxPascalSymbolVisibility): TNyxText;
const
  CNames: array[TNyxPascalSymbolVisibility] of TNyxText = ('interface',
    'implementation', 'private', 'protected', 'public', 'published', 'automated', 'unknown');
begin

  if (Ord(AVisibility) < Ord(Low(TNyxPascalSymbolVisibility))) or
    (Ord(AVisibility) > Ord(High(TNyxPascalSymbolVisibility))) then
  begin
    raise ENyxModel.Create('Unknown declaration visibility');
  end;
  Result := CNames[AVisibility];
end;

function TPascalRoutineIndex.SymbolCount: Integer;
begin
  Result := Length(FSymbols);
end;

function TPascalRoutineIndex.SymbolAt(AIndex: Integer): TNyxPascalSymbolSite;
begin

  if (AIndex < 0) or (AIndex >= SymbolCount) then
  begin
    raise ENyxModel.Create('Declaration ordinal is outside its captured source');
  end;
  Result := FSymbols[AIndex];
end;

procedure TPascalRoutineIndex.BuildSymbols;
type
  { Parser state belongs to one lexical scope. Member fields never become unit
    variables, and procedural implementations are skipped as complete blocks. }
  TSection = (ssNone, ssTypes, ssConstants, ssVariables, ssResources);
var
  LAll: TTokens;
  LTokens: TTokens;
  LProblem: TNyxSourceDiagnostic;
  LStop: Integer;
  LIndex: Integer;
  LCount: Integer;
  LDirective: TNyxText;

  procedure Problem(const AMessage: TNyxText; APosition: Integer);
  begin

    if FProblemText = '' then
    begin
      FProblemText := AMessage;
      FProblemLocation := Point(APosition);
    end;
  end;

  procedure Add(AName: Integer; const AScope: TNyxText;
    AKind: TNyxPascalSymbolKind; AVisibility: TNyxPascalSymbolVisibility);
  var
    LSite: TNyxPascalSymbolSite;
    LAt: Integer;
  begin

    if (AName < 0) or (AName >= Length(LTokens)) or
      (LTokens[AName].Kind <> tkWord) then
    begin
      HandlerFail(FText, 'A declaration needs its own identifier', LTokens, AName);
    end;

    if Length(FSymbols) = 4096 then
    begin
      HandlerLimitFail(FText, 'Declaration discovery exceeds 4096 entries', LTokens, AName);
    end;
    LSite := Default(TNyxPascalSymbolSite);
    LSite.FName := LTokens[AName].Text;
    LSite.FScope := AScope;
    LSite.FKind := AKind;
    LSite.FVisibility := AVisibility;
    LSite.FLocation := Point(LTokens[AName].Position);
    LAt := Length(FSymbols);
    SetLength(FSymbols, LAt + 1);
    FSymbols[LAt] := LSite;
  end;

  { Semicolons inside grouped constants, indexers and structured types do not
    end the outer declaration. A missing closing boundary grants no later scope. }
  function Finish(AStart, AEnd: Integer; AAllowEnd: Boolean = False): Integer;
  var
    LAt: Integer;
    LGroups: Integer;
    LBlocks: Integer;
    LAngles: Integer;
    LBeforeDefinition: Boolean;
  begin
    LGroups := 0;
    LBlocks := 0;
    LAngles := 0;
    LBeforeDefinition := True;
    for LAt := AStart to AEnd - 1 do
    begin

      if LBeforeDefinition and (LGroups = 0) and (LBlocks = 0) then
      begin

        if HandlerSymbol(LTokens, LAt, '<') then
        begin
          Inc(LAngles);
        end
        else if (LAngles > 0) and HandlerSymbol(LTokens, LAt, '>') then
        begin
          Dec(LAngles);
        end
        else if (LAngles = 0) and HandlerSymbol(LTokens, LAt, '=') then
        begin
          LBeforeDefinition := False;
        end;
      end;

      if LAngles > 0 then
      begin
        Continue;
      end;

      if (LGroups = 0) and (LBlocks > 0) and
        (HandlerWord(LTokens, LAt, 'implementation') or
        HandlerWord(LTokens, LAt, 'initialization') or
        HandlerWord(LTokens, LAt, 'finalization') or
        HandlerWord(LTokens, LAt, 'begin') or HandlerWord(LTokens, LAt, 'asm')) then
      begin
        { An unfinished include type cannot borrow a later routine/unit end as
          its closing token and relabel unrelated declarations as members. }
        HandlerFail(FText, 'Finish the structured type before its containing source continues',
          LTokens, AStart);
      end;

      if HandlerSymbol(LTokens, LAt, '(') or HandlerSymbol(LTokens, LAt, '[') then
      begin
        Inc(LGroups);
      end
      else if HandlerSymbol(LTokens, LAt, ')') or HandlerSymbol(LTokens, LAt, ']') then
      begin
        Dec(LGroups);
      end
      else if (LGroups = 0) and HandlerTypeBlock(LTokens, LAt) and
        not HandlerSymbol(LTokens, LAt + 1, ',') and
        not HandlerSymbol(LTokens, LAt + 1, '>') then
      begin
        Inc(LBlocks);
      end
      else if (LGroups = 0) and HandlerWord(LTokens, LAt, 'end') then
      begin
        Dec(LBlocks);
      end
      else if (LGroups = 0) and (LBlocks = 0) and HandlerSymbol(LTokens, LAt, ';') then
      begin
        Exit(LAt);
      end;

      if (LGroups < 0) or (LBlocks < 0) then
      begin
        HandlerFail(FText, 'Declaration groups have no unambiguous boundary', LTokens, LAt);
      end;

      if (LGroups > 256) or (LBlocks > 64) then
      begin
        HandlerLimitFail(FText, 'Declaration nesting exceeds its inspection budget', LTokens, LAt);
      end;
    end;
    if AAllowEnd and (LGroups = 0) and (LBlocks = 0) then
    begin
      Exit(AEnd);
    end;
    HandlerFail(FText, 'Finish the declaration before navigating its members', LTokens, AStart);
    Result := -1;
  end;

  function Identifier(AAt: Integer): Integer;
  begin
    Result := AAt;

    if HandlerSymbol(LTokens, Result, '&') then
    begin
      Inc(Result);
    end;
  end;

  procedure Scan(AStart, AEnd: Integer; const AScope: TNyxText;
    AVisibility: TNyxPascalSymbolVisibility; AMembers: Boolean; ADepth: Integer;
    AVariant: Boolean = False);
  var
    LAt: Integer;
    LName: Integer;
    LLast: Integer;
    LEqual: Integer;
    LType: Integer;
    LMember: Integer;
    LGroup: Integer;
    LHeader: Integer;
    LBodyEnd: Integer;
    LSection: TSection;
    LVisibility: TNyxPascalSymbolVisibility;
    LKind: TNyxPascalSymbolKind;
    LScope: TNyxText;
    LMemberVisibility: TNyxPascalSymbolVisibility;
    LNames: array of Integer;
    LNameCount: Integer;
    LWalk: Integer;
  begin

    if ADepth > 64 then
    begin
      HandlerLimitFail(FText, 'Declaration scope exceeds its inspection budget', LTokens, AStart);
    end;
    LAt := AStart;
    LSection := ssNone;
    LVisibility := AVisibility;

    if AMembers then
    begin
      LSection := ssVariables;
    end;
    while LAt < AEnd do
    begin

      if HandlerWord(LTokens, LAt, 'implementation') then
      begin
        LVisibility := psvImplementation;
        LSection := ssNone;
        Inc(LAt);
        Continue;
      end;

      if HandlerWord(LTokens, LAt, 'initialization') or
        HandlerWord(LTokens, LAt, 'finalization') or
        HandlerWord(LTokens, LAt, 'begin') or HandlerWord(LTokens, LAt, 'end') then
      begin
        Exit;
      end;

      if HandlerWord(LTokens, LAt, 'uses') or HandlerWord(LTokens, LAt, 'unit') or
        HandlerWord(LTokens, LAt, 'program') or HandlerWord(LTokens, LAt, 'library') then
      begin
        LAt := Finish(LAt + 1, AEnd) + 1;
        Continue;
      end;

      if HandlerWord(LTokens, LAt, 'interface') then
      begin
        LVisibility := psvInterface;
        LSection := ssNone;
        Inc(LAt);
        Continue;
      end;

      if AMembers and (HandlerWord(LTokens, LAt, 'private') or
        HandlerWord(LTokens, LAt, 'protected') or HandlerWord(LTokens, LAt, 'public') or
        HandlerWord(LTokens, LAt, 'published') or HandlerWord(LTokens, LAt, 'automated')) then
      begin

        if HandlerWord(LTokens, LAt, 'private') then
        begin
          LVisibility := psvPrivate;
        end
        else if HandlerWord(LTokens, LAt, 'protected') then
        begin
          LVisibility := psvProtected;
        end
        else if HandlerWord(LTokens, LAt, 'public') then
        begin
          LVisibility := psvPublic;
        end
        else if HandlerWord(LTokens, LAt, 'published') then
        begin
          LVisibility := psvPublished;
        end
        else
        begin
          LVisibility := psvAutomated;
        end;
        LSection := ssVariables;
        Inc(LAt);
        Continue;
      end;

      if HandlerWord(LTokens, LAt, 'strict') or
        HandlerWord(LTokens, LAt, 'generic') or
        (HandlerWord(LTokens, LAt, 'class') and not HandlerTypeBlock(LTokens, LAt)) then
      begin
        Inc(LAt);
        Continue;
      end;

      if HandlerWord(LTokens, LAt, 'virtual') or HandlerWord(LTokens, LAt, 'override') or
        HandlerWord(LTokens, LAt, 'abstract') or HandlerWord(LTokens, LAt, 'dynamic') or
        HandlerWord(LTokens, LAt, 'overload') or HandlerWord(LTokens, LAt, 'reintroduce') or
        HandlerWord(LTokens, LAt, 'inline') or HandlerWord(LTokens, LAt, 'cdecl') or
        HandlerWord(LTokens, LAt, 'stdcall') or HandlerWord(LTokens, LAt, 'safecall') or
        HandlerWord(LTokens, LAt, 'register') or HandlerWord(LTokens, LAt, 'static') or
        HandlerWord(LTokens, LAt, 'default') or HandlerWord(LTokens, LAt, 'message') or
        HandlerWord(LTokens, LAt, 'dispid') or HandlerWord(LTokens, LAt, 'deprecated') or
        HandlerWord(LTokens, LAt, 'platform') or HandlerWord(LTokens, LAt, 'experimental') or
        HandlerWord(LTokens, LAt, 'forward') or HandlerWord(LTokens, LAt, 'external') then
      begin
        LAt := Finish(LAt, AEnd) + 1;
        Continue;
      end;

      if HandlerWord(LTokens, LAt, 'type') or HandlerWord(LTokens, LAt, 'const') or
        HandlerWord(LTokens, LAt, 'var') or HandlerWord(LTokens, LAt, 'threadvar') or
        HandlerWord(LTokens, LAt, 'resourcestring') then
      begin

        if HandlerWord(LTokens, LAt, 'type') then
        begin
          LSection := ssTypes;
        end
        else if HandlerWord(LTokens, LAt, 'const') then
        begin
          LSection := ssConstants;
        end
        else if HandlerWord(LTokens, LAt, 'resourcestring') then
        begin
          LSection := ssResources;
        end
        else
        begin
          LSection := ssVariables;
        end;
        Inc(LAt);
        Continue;
      end;

      if RoutineOpening(LTokens, LAt) or HandlerWord(LTokens, LAt, 'operator') then
      begin
        LName := Identifier(LAt + 1);
        LHeader := Finish(LAt + 1, AEnd);
        LBodyEnd := LHeader;

        if not AMembers and (LVisibility = psvImplementation) then
        begin
          LBodyEnd := HandlerRoutineEnd(FText, LTokens, LAt, 1, LHeader);
        end;
        { Qualified implementations are represented by their exact authored
          scope; prototype rows remain distinct declaration-order entries. }
        LScope := AScope;
        LWalk := LName;
        while HandlerSymbol(LTokens, LWalk + 1, '.') and (LWalk + 2 < LHeader) do
        begin

          if LScope <> '' then
          begin
            LScope := LScope + '.';
          end;
          LScope := LScope + LTokens[LWalk].Text;
          LWalk := Identifier(LWalk + 2);
        end;

        if LTokens[LWalk].Kind = tkWord then
        begin
          Add(LWalk, LScope, pskRoutine, LVisibility);
        end;
        LAt := LBodyEnd + 1;
        Continue;
      end;

      if AMembers and HandlerWord(LTokens, LAt, 'property') then
      begin
        LName := Identifier(LAt + 1);
        LLast := Finish(LAt + 1, AEnd);
        Add(LName, AScope, pskProperty, LVisibility);
        LAt := LLast + 1;
        Continue;
      end;

      if AMembers and HandlerWord(LTokens, LAt, 'case') then
      begin
        { Variant arms retain their record scope. Labels are expressions, not
          declarations; only an explicit tag and parenthesized fields are rows. }
        LName := Identifier(LAt + 1);

        if HandlerSymbol(LTokens, LName + 1, ':') then
        begin
          Add(LName, AScope, pskField, LVisibility);
        end;
        while (LAt < AEnd) and not HandlerWord(LTokens, LAt, 'of') do
        begin
          Inc(LAt);
        end;
        Inc(LAt);
        while LAt < AEnd do
        begin
          while (LAt < AEnd) and not HandlerSymbol(LTokens, LAt, '(') do
          begin
            Inc(LAt);
          end;

          if LAt = AEnd then
          begin
            Break;
          end;
          LMember := LAt + 1;
          LGroup := 1;
          Inc(LAt);
          while (LAt < AEnd) and (LGroup > 0) do
          begin

            if HandlerSymbol(LTokens, LAt, '(') then
            begin
              Inc(LGroup);
            end
            else if HandlerSymbol(LTokens, LAt, ')') then
            begin
              Dec(LGroup);
            end;
            Inc(LAt);
          end;

          if LGroup <> 0 then
          begin
            HandlerFail(FText, 'Variant fields have an unfinished group', LTokens, LMember);
          end;
          Scan(LMember, LAt - 1, AScope, LVisibility, True, ADepth + 1, True);
        end;
        Exit;
      end;

      LName := Identifier(LAt);

      if (LName < AEnd) and (LTokens[LName].Kind = tkWord) and (LSection <> ssNone) then
      begin
        LLast := Finish(LAt, AEnd, AVariant);
        LEqual := LName + 1;
        LGroup := 0;
        while LEqual < LLast do
        begin

          if HandlerSymbol(LTokens, LEqual, '<') then
          begin
            Inc(LGroup);
          end
          else if HandlerSymbol(LTokens, LEqual, '>') then
          begin
            Dec(LGroup);
          end
          else if HandlerSymbol(LTokens, LEqual, '=') and (LGroup = 0) then
          begin
            Break;
          end;
          Inc(LEqual);
        end;

        if LSection = ssTypes then
        begin

          if LEqual = LLast then
          begin
            HandlerFail(FText, 'A type declaration needs its exact definition', LTokens, LAt);
          end;
          Add(LName, AScope, pskType, LVisibility);
          LScope := FSymbols[High(FSymbols)].QualifiedName;
          LType := LEqual + 1;

          if HandlerWord(LTokens, LType, 'packed') then
          begin
            Inc(LType);
          end;

          if HandlerTypeBlock(LTokens, LType) then
          begin
            LMember := LType + 1;
            LMemberVisibility := psvPublic;

            if HandlerWord(LTokens, LType, 'class') then
            begin
              LMemberVisibility := psvUnknown;
            end;
            { Skip ancestor/GUID groups and helper target headers only. The
              following tokens belong to the type's authored member scopes. }
            while HandlerWord(LTokens, LMember, 'abstract') or
              HandlerWord(LTokens, LMember, 'sealed') do
            begin
              Inc(LMember);
            end;

            if HandlerWord(LTokens, LMember, 'helper') then
            begin
              Inc(LMember);
            end;
            while HandlerSymbol(LTokens, LMember, '(') or HandlerSymbol(LTokens, LMember, '[') do
            begin
              LGroup := 1;
              Inc(LMember);
              while (LMember < LLast) and (LGroup > 0) do
              begin

                if HandlerSymbol(LTokens, LMember, '(') or HandlerSymbol(LTokens, LMember, '[') then
                begin
                  Inc(LGroup);
                end
                else if HandlerSymbol(LTokens, LMember, ')') or HandlerSymbol(LTokens, LMember, ']') then
                begin
                  Dec(LGroup);
                end;
                Inc(LMember);
              end;
            end;

            if HandlerWord(LTokens, LMember, 'for') then
            begin
              LMember := Identifier(LMember + 1) + 1;
              while HandlerSymbol(LTokens, LMember, '.') do
              begin
                LMember := Identifier(LMember + 1) + 1;
              end;
            end;
            Scan(LMember, LLast - 1, LScope, LMemberVisibility, True, ADepth + 1);
          end
          else if HandlerSymbol(LTokens, LType, '(') then
          begin
            LWalk := LType + 1;
            while LWalk < LLast do
            begin
              LWalk := Identifier(LWalk);
              Add(LWalk, LScope, pskEnumMember, LVisibility);
              Inc(LWalk);
              LGroup := 0;
              while LWalk < LLast do
              begin

                if HandlerSymbol(LTokens, LWalk, '(') then
                begin
                  Inc(LGroup);
                end
                else if HandlerSymbol(LTokens, LWalk, ')') then
                begin

                  if LGroup = 0 then
                  begin
                    Break;
                  end;
                  Dec(LGroup);
                end
                else if (LGroup = 0) and HandlerSymbol(LTokens, LWalk, ',') then
                begin
                  Break;
                end;
                Inc(LWalk);
              end;

              if HandlerSymbol(LTokens, LWalk, ')') then
              begin
                Break;
              end;
              Inc(LWalk);
            end;
          end;
        end
        else
        begin
          LKind := pskVariable;

          if LSection = ssConstants then
          begin
            LKind := pskConstant;
          end
          else if LSection = ssResources then
          begin
            LKind := pskResourceString;
          end
          else if AMembers then
          begin
            LKind := pskField;
          end;
          LWalk := LAt;
          LNameCount := 0;
          LNames := nil;
          repeat
            LName := Identifier(LWalk);
            SetLength(LNames, LNameCount + 1);
            LNames[LNameCount] := LName;
            Inc(LNameCount);
            LWalk := LName + 1;

            if not HandlerSymbol(LTokens, LWalk, ',') then
            begin
              Break;
            end;
            Inc(LWalk);
          until LWalk >= LLast;

          if (LSection = ssVariables) and not HandlerSymbol(LTokens, LWalk, ':') then
          begin
            HandlerFail(FText, 'A field or variable needs its own type boundary', LTokens, LAt);
          end;
          for LWalk := 0 to High(LNames) do
          begin
            Add(LNames[LWalk], AScope, LKind, LVisibility);
          end;
        end;
        LAt := LLast + 1;
        Continue;
      end;
      Inc(LAt);
    end;
  end;

begin
  LAll := LexSourceRead(FText, FSources.Kind = pssEditing, LProblem, LStop);
  SetLength(LTokens, Length(LAll));
  LCount := 0;
  for LIndex := 0 to High(LAll) do
  begin

    if LAll[LIndex].Kind = tkComment then
    begin
      LDirective := ImportDirective(LAll[LIndex]);

      if (LDirective = 'if') or (LDirective = 'ifdef') or (LDirective = 'ifndef') or
        (LDirective = 'ifopt') or (LDirective = 'else') or (LDirective = 'elseif') or
        (LDirective = 'endif') then
      begin
        Problem('Conditional declarations need compiler-aware scope', LAll[LIndex].Position);
        Break;
      end;
    end
    else
    begin
      LTokens[LCount] := LAll[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(LTokens, LCount);
  try
    Scan(0, Length(LTokens), '', psvImplementation, False, 0);
  except
    on LException: ENyxSourceLimit do
    begin
      raise;
    end;
    on LException: ENyxSource do
    begin

      if FSources.Kind <> pssEditing then
      begin
        raise;
      end;
      Problem(LException.Detail, NyxTextPosition(FText, LException.Line, LException.Column));
    end;
  end;

  if LProblem.Defined then
  begin
    Problem(LProblem.Message, NyxTextPosition(FText, LProblem.Line, LProblem.Column));
  end;
end;

function CaptureNyxPascalSymbolIndex(const ASources: INyxPascalSourceSnapshot;
  const AOwner: TNyxPascalSourceRef;
  const ALimits: TNyxPascalRoutineLimits): INyxPascalSymbolIndex;
begin
  Result := TPascalRoutineIndex.Create(ASources, AOwner, ALimits, True);
end;

function CaptureNyxPascalSymbolIndex(const ASources: INyxPascalSourceSnapshot;
  const AOwner: TNyxPascalSourceRef): INyxPascalSymbolIndex;
begin
  Result := CaptureNyxPascalSymbolIndex(ASources, AOwner, TNyxPascalRoutineLimits.Defaults);
end;
