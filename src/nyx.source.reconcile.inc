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

{ Private implementation of nyx.source. It uses that unit's admitted lexer and
  typed expression reader rather than a second Pascal grammar. All positions are
  exact storage offsets: UTF-8 bytes natively and UTF-16 units in pas2js. }

type
  TSourceSymbolKind = (ssControl, ssState);
  TSourceSymbol = record
    Kind: TSourceSymbolKind;
    Name: TNyxText;
    Identity: TNyxText;
    StateKind: TNyxStateKind;
  end;
  TSourceSymbols = array of TSourceSymbol;
  TSourceAtom = record
    Key: TNyxText;
    Position: Integer;
    Finish: Integer;
  end;
  TSourceAtoms = array of TSourceAtom;
  TSourceIntegers = array of Integer;
  TSourceTrace = array of TSourceIntegers;
  TSourceChange = record
    BeforeStart: Integer;
    BeforeFinish: Integer;
    AfterStart: Integer;
    AfterFinish: Integer;
  end;
  TSourceChanges = array of TSourceChange;

  { One immutable source snapshot owns its lexical facts. Arrays returned by
    these private getters are read-only: consumers must not modify their records
    or resize shared arrays. Mutable expression readers are always independent.
    No document, renderer or reader is retained, and no cache survives the merge. }
  TBuilderContext = class
  private
    FBody: TNyxText;
    FAllTokens: TTokens;
    FTokens: TTokens;
    FSymbols: TSourceSymbols;
    FLocalFacts: TControlLocals;
    FStateFacts: TStateLocals;
    FHasTokens: Boolean;
    FHasSymbols: Boolean;
    procedure ReadFacts;
  public
    constructor Create(const ABody: TNyxText);
    function AllTokens: TTokens;
    function Tokens: TTokens;
    function Symbols: TSourceSymbols;
    { Caller owns the returned lexical reader and must release it. Its cursor,
      state-reference assignments and declaration arrays are never shared. }
    function NewReader: TConfigurationReader;
  end;

  { Operation-scoped pool owns every context, including failed lazy reads.
    Exact source equality is the cache key; identities/names are not substitutes
    for source snapshots. The small ordered pool never determines output order. }
  TBuilderContexts = class
  private
    FItems: array of TBuilderContext;
  public
    destructor Destroy; override;
    { Returned context is borrowed until this reconciliation's pool is freed. }
    function ForSource(const ABody: TNyxText): TBuilderContext;
  end;

function FilterBuilderTokens(const AAllTokens: TTokens): TTokens;
var
  LIndex: Integer;
  LCount: Integer;
begin
  SetLength(Result, Length(AAllTokens));
  LCount := 0;
  for LIndex := 0 to Length(AAllTokens) - 1 do
  begin

    if AAllTokens[LIndex].Kind <> tkComment then
    begin
      Result[LCount] := AAllTokens[LIndex];
      Inc(LCount);
    end;
  end;
  SetLength(Result, LCount);
end;

procedure TBuilderContext.ReadFacts;
var
  LReader: TConfigurationReader;
  LControls: TControlLocals;
  LIndex: Integer;
  LCount: Integer;
  LLocal: Integer;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  {$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  LReader := TConfigurationReader.Create(FBody, Tokens, nil, nil, False);
  try
    LControls := ControlLocals(FBody, Tokens, LReader);
    for LIndex := 0 to Length(LControls) - 1 do
    begin
      LLocal := LReader.LocalIndex(LControls[LIndex].Name);

      if LLocal < 0 then
      begin
        raise ENyxSource.CreateAt('Declare every constructed control local', FBody, 1);
      end;
      LReader.FLocals[LLocal].ID := LControls[LIndex].ID;
    end;
    { Preserve the former lexical constructor's exact identity admission. Names
      were already checked by its declaration reader; unused empty IDs are not
      constructed controls and therefore do not reserve an application identity. }
    for LIndex := 0 to Length(LReader.FLocals) - 1 do
    begin

      if LReader.FLocals[LIndex].ID <> '' then
      begin

        if LReader.FControlIDs.IndexOf(LReader.FLocals[LIndex].ID) >= 0 then
        begin
          raise ENyxSource.CreateAt('Control locals require unique names and identities', FBody, 1);
        end;
        LReader.FControlIDs.AddFirst(LReader.FLocals[LIndex].ID, LIndex);
      end;
    end;
    SetLength(FSymbols, Length(LReader.FLocals) + Length(LReader.FStates));
    for LIndex := 0 to Length(LReader.FLocals) - 1 do
    begin
      FSymbols[LIndex].Kind := ssControl;
      FSymbols[LIndex].Name := LReader.FLocals[LIndex].Name;
      FSymbols[LIndex].Identity := LReader.FLocals[LIndex].ID;
      FSymbols[LIndex].StateKind := nskText;
    end;
    for LIndex := 0 to Length(LReader.FStates) - 1 do
    begin
      LCount := Length(LReader.FLocals) + LIndex;
      FSymbols[LCount].Kind := ssState;
      FSymbols[LCount].Name := LReader.FStates[LIndex].Name;
      FSymbols[LCount].Identity := LReader.FStates[LIndex].Key;
      FSymbols[LCount].StateKind := LReader.FStates[LIndex].Kind;
    end;
    { These arrays become immutable facts; every mutable reader clones records.
      The source/arrays own no controls, document, callbacks or global registry. }
    FLocalFacts := LReader.FLocals;
    FStateFacts := LReader.FStates;
    FHasSymbols := True;
    {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spSymbolRead, LStarted);{$endif}
  finally
    LReader.Free;
  end;
end;

constructor TBuilderContext.Create(const ABody: TNyxText);
begin
  inherited Create;
  FBody := ABody;
end;

function TBuilderContext.AllTokens: TTokens;
begin
  { Tokens initializes both arrays from one lexer pass, preserving original
    comment offsets while exposing a separate filtered structural sequence. }
  Tokens;
  Result := FAllTokens;
end;

function TBuilderContext.Tokens: TTokens;
{$ifdef NYX_SOURCE_PROFILE}
var
  LStarted: Double;
{$endif}
begin

  if not FHasTokens then
  begin
    {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
    FAllTokens := Lex(FBody);
    FTokens := FilterBuilderTokens(FAllTokens);
    FHasTokens := True;
    {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spLex, LStarted);{$endif}
  end;
  Result := FTokens;
end;

function TBuilderContext.Symbols: TSourceSymbols;
begin

  if not FHasSymbols then
  begin
    ReadFacts;
  end;
  Result := FSymbols;
end;

function TBuilderContext.NewReader: TConfigurationReader;
var
  LIndex: Integer;
begin

  if not FHasSymbols then
  begin
    ReadFacts;
  end;
  { The reconstruction flag initializes an empty reader without reparsing its
    declarations. This private lexical clone never admits a candidate document;
    complete candidate reconstruction always parses and verifies source fresh. }
  Result := TConfigurationReader.Create(FBody, Tokens, nil, nil, False, True);
  try
    SetLength(Result.FLocals, Length(FLocalFacts));
    SetLength(Result.FControls, Length(FLocalFacts));
    for LIndex := 0 to Length(FLocalFacts) - 1 do
    begin
      Result.FLocals[LIndex] := FLocalFacts[LIndex];
      Result.FLocalNames.AddFirst(LowerCase(FLocalFacts[LIndex].Name), LIndex);

      if FLocalFacts[LIndex].ID <> '' then
      begin
        Result.FControlIDs.AddFirst(FLocalFacts[LIndex].ID, LIndex);
      end;
    end;
    SetLength(Result.FStates, Length(FStateFacts));
    for LIndex := 0 to Length(FStateFacts) - 1 do
    begin
      Result.FStates[LIndex] := FStateFacts[LIndex];
      Result.FStateNames.AddFirst(LowerCase(FStateFacts[LIndex].Name), LIndex);
    end;
  except
    Result.Free;
    raise;
  end;
end;

destructor TBuilderContexts.Destroy;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FItems) - 1 do
  begin
    FItems[LIndex].Free;
  end;
  inherited Destroy;
end;

function TBuilderContexts.ForSource(const ABody: TNyxText): TBuilderContext;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FItems) - 1 do
  begin

    if FItems[LIndex].FBody = ABody then
    begin
      Exit(FItems[LIndex]);
    end;
  end;
  Result := TBuilderContext.Create(ABody);
  try
    LIndex := Length(FItems);
    SetLength(FItems, LIndex + 1);
    FItems[LIndex] := Result;
  except
    Result.Free;
    raise;
  end;
end;

function SymbolNames(const ASymbols: TSourceSymbols): TSourceIndex;
var
  LIndex: Integer;
begin
  Result := TSourceIndex.Create;
  try
    for LIndex := 0 to Length(ASymbols) - 1 do
    begin
      Result.AddFirst(LowerCase(ASymbols[LIndex].Name), LIndex);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function SymbolIdentity(AKind: TSourceSymbolKind; AStateKind: TNyxStateKind;
  const AIdentity: TNyxText): TNyxText;
begin

  if AKind = ssControl then
  begin
    Exit('control:' + AIdentity);
  end;
  Result := 'state:' + IntToStr(Ord(AStateKind)) + ':' + AIdentity;
end;

function PreserveBuilderNames(AContexts: TBuilderContexts; const AGenerated: TNyxText;
  const AAuthored: TSourceSymbols;
  const ARenames: array of TNyxSourceStateRename): TNyxText;
var
  LSymbols: TSourceSymbols;
  LNames: array of TNyxText;
  LTokens: TTokens;
  LIndex: Integer;
  LOther: Integer;
  LRename: Integer;
  LSymbol: Integer;
  LSerial: Integer;
  LCursor: Integer;
  LIdentity: TNyxText;
  LStem: TNyxText;
  LReserved: TSourceIndex;
  LAuthoredIDs: TSourceIndex;
  LSymbolNames: TSourceIndex;
  LParts: TNyxStrings;
begin
  LSymbols := AContexts.ForSource(AGenerated).Symbols;
  SetLength(LNames, Length(LSymbols));
  LReserved := TSourceIndex.Create;
  LAuthoredIDs := nil;
  LSymbolNames := nil;
  LParts := TNyxStrings.Create;
  try
    LAuthoredIDs := TSourceIndex.Create;
    LSymbolNames := SymbolNames(LSymbols);
    { Reserve deliberate names before choosing new defaults. A new auto-named
      control cannot steal an authored local, even when traversal order changes. }
    for LIndex := 0 to Length(AAuthored) - 1 do
    begin
      LReserved.AddFirst(LowerCase(AAuthored[LIndex].Name), LIndex);
      LAuthoredIDs.AddFirst(SymbolIdentity(AAuthored[LIndex].Kind,
        AAuthored[LIndex].StateKind, AAuthored[LIndex].Identity), LIndex);
    end;
    for LIndex := 0 to Length(LSymbols) - 1 do
    begin
      LIdentity := LSymbols[LIndex].Identity;

      if LSymbols[LIndex].Kind = ssState then
      begin
        for LRename := 0 to High(ARenames) do
        begin

          if ARenames[LRename].NewName = LIdentity then
          begin
            LIdentity := ARenames[LRename].OldName;
            Break;
          end;
        end;
      end;
      LOther := LAuthoredIDs.IndexOf(SymbolIdentity(LSymbols[LIndex].Kind,
        LSymbols[LIndex].StateKind, LIdentity));

      if LOther >= 0 then
      begin
        LNames[LIndex] := AAuthored[LOther].Name;
      end;

      if LNames[LIndex] = '' then
      begin
        LStem := LSymbols[LIndex].Name;
        LNames[LIndex] := LStem;
        LSerial := 1;
        while LReserved.IndexOf(LowerCase(LNames[LIndex])) >= 0 do
        begin
          Inc(LSerial);
          LNames[LIndex] := LStem + IntToStr(LSerial);
        end;
        LReserved.AddFirst(LowerCase(LNames[LIndex]), LIndex);
      end;
    end;
    LTokens := AContexts.ForSource(AGenerated).Tokens;
    LCursor := 1;
    for LIndex := 0 to Length(LTokens) - 1 do
    begin

      if (LTokens[LIndex].Kind = tkWord) and
        ((LIndex = 0) or (LTokens[LIndex - 1].Text <> '.')) then
      begin
        LSymbol := LSymbolNames.IndexOf(LowerCase(LTokens[LIndex].Text));

        if (LSymbol >= 0) and (LNames[LSymbol] <> LTokens[LIndex].Text) then
        begin
          LParts.Add(Copy(AGenerated, LCursor, LTokens[LIndex].Position - LCursor));
          LParts.Add(LNames[LSymbol]);
          LCursor := LTokens[LIndex].Finish;
        end;
      end;
    end;
    LParts.Add(Copy(AGenerated, LCursor, MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
    LSymbolNames.Free;
    LAuthoredIDs.Free;
    LReserved.Free;
  end;
end;

function ScaffoldBuilderSections(AContexts: TBuilderContexts;
  const AGenerated, AAuthored: TNyxText): TNyxText;
type
  TSectionSites = record
    OwnerEnd: Integer;
    ConfigureEnd: Integer;
    BindingEnd: Integer;
  end;
  TSectionSitesArray = array of TSectionSites;
var
  LGenerated: TTokens;
  LAuthored: TTokens;
  LSymbols: TSourceSymbols;
  LNames: TSourceIndex;
  LGeneratedSites: TSectionSitesArray;
  LAuthoredSites: TSectionSitesArray;
  LInsertions: array of TNyxText;
  LSymbol: Integer;
  LIndex: Integer;
  LOwnerEnd: Integer;
  LConfigureEnd: Integer;
  LCursor: Integer;
  LParts: TNyxStrings;

  function StatementEnd(const ATokens: TTokens; AStart: Integer): Integer;
  begin
    Result := AStart;
    while (Result < Length(ATokens)) and
      ((ATokens[Result].Kind <> tkSymbol) or (ATokens[Result].Text <> ';')) do
    begin
      Inc(Result);
    end;
  end;

  procedure ReadSites(const ATokens: TTokens; out ASites: TSectionSitesArray);
  var
    LToken: Integer;
    LOwner: Integer;
    LEnd: Integer;
  begin
    SetLength(ASites, Length(LSymbols));
    for LOwner := 0 to Length(ASites) - 1 do
    begin
      ASites[LOwner].OwnerEnd := -1;
      ASites[LOwner].ConfigureEnd := -1;
      ASites[LOwner].BindingEnd := -1;
    end;
    { One pass locates each owner's first construction/configuration/binding
      statement. The previous per-owner full token scans scaled quadratically,
      even when every authored fluent section was unchanged. }
    for LToken := 0 to Length(ATokens) - 3 do
    begin

      if (ATokens[LToken].Kind <> tkWord) or
        ((ATokens[LToken + 1].Text <> ':=') and
        ((ATokens[LToken + 1].Text <> '.') or
        (not SameText(ATokens[LToken + 2].Text, 'Configure') and
        not SameText(ATokens[LToken + 2].Text, 'Binds')))) then
      begin
        Continue;
      end;
      LOwner := LNames.IndexOf(LowerCase(ATokens[LToken].Text));

      if LOwner < 0 then
      begin
        Continue;
      end;

      if ATokens[LToken + 1].Text = ':=' then
      begin

        if ASites[LOwner].OwnerEnd < 0 then
        begin
          LEnd := StatementEnd(ATokens, LToken + 2);
          { Retain the original construction-and-adoption anchor: the first
            following statement is where generated ownership is completed. }
          ASites[LOwner].OwnerEnd := StatementEnd(ATokens, LEnd + 1);
        end;
      end
      else if SameText(ATokens[LToken + 2].Text, 'Configure') then
      begin

        if ASites[LOwner].ConfigureEnd < 0 then
        begin
          ASites[LOwner].ConfigureEnd := StatementEnd(ATokens, LToken + 3);
        end;
      end
      else if ASites[LOwner].BindingEnd < 0 then
      begin
        ASites[LOwner].BindingEnd := StatementEnd(ATokens, LToken + 3);
      end;
    end;
  end;

begin
  { Clear/Inherit can leave an authored block whose resulting descriptor is
    empty. Give it stable anchors in both private generated comparison frames,
    while retaining exact authored statements and their original source order. }
  LGenerated := AContexts.ForSource(AGenerated).Tokens;
  LAuthored := AContexts.ForSource(AAuthored).Tokens;
  LSymbols := AContexts.ForSource(AGenerated).Symbols;
  LNames := SymbolNames(LSymbols);
  LParts := nil;
  try
    ReadSites(LGenerated, LGeneratedSites);
    ReadSites(LAuthored, LAuthoredSites);
    SetLength(LInsertions, Length(LGenerated));
    for LSymbol := 0 to Length(LSymbols) - 1 do
    begin

      if LSymbols[LSymbol].Kind <> ssControl then
      begin
        Continue;
      end;
      LOwnerEnd := LGeneratedSites[LSymbol].OwnerEnd;

      if (LOwnerEnd < 0) or (LOwnerEnd >= Length(LGenerated)) then
      begin
        Continue;
      end;
      LConfigureEnd := LGeneratedSites[LSymbol].ConfigureEnd;

      if (LConfigureEnd < 0) and (LAuthoredSites[LSymbol].ConfigureEnd >= 0) then
      begin
        LInsertions[LOwnerEnd] := JoinSourceFrame([LInsertions[LOwnerEnd], #10,
          '    ', LSymbols[LSymbol].Name, '.Configure', #10, '      .Done;']);
      end;

      if (LGeneratedSites[LSymbol].BindingEnd < 0) and
        (LAuthoredSites[LSymbol].BindingEnd >= 0) then
      begin

        if LConfigureEnd >= 0 then
        begin
          LOwnerEnd := LConfigureEnd;
        end;
        LInsertions[LOwnerEnd] := JoinSourceFrame([LInsertions[LOwnerEnd], #10,
          '    ', LSymbols[LSymbol].Name, '.Binds', #10, '      .Done;']);
      end;
    end;
    LParts := TNyxStrings.Create;
    LCursor := 1;
    for LIndex := 0 to Length(LGenerated) - 1 do
    begin

      if LInsertions[LIndex] <> '' then
      begin
        LParts.Add(Copy(AGenerated, LCursor, LGenerated[LIndex].Finish - LCursor));
        LParts.Add(LInsertions[LIndex]);
        LCursor := LGenerated[LIndex].Finish;
      end;
    end;
    LParts.Add(Copy(AGenerated, LCursor, MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
    LNames.Free;
  end;
end;

function SourceValueKey(const AValue: TValue; AReader: TConfigurationReader): TNyxText;
var
  LPayload: TNyxText;
  LDomain: TNyxValueDomain;
  LInteger: Integer;
begin
  LPayload := AValue.Text;
  case AValue.Kind of
    vkInteger:
      begin
        TryNyxStateInteger(AValue.Text, LInteger);
        LPayload := IntToStr(LInteger);
      end;
    vkNumber: LPayload := NyxStateNumberText(AReader.Numeric(AValue));
    vkBoolean, vkKind, vkAttribute, vkLayout, vkVariant, vkAction, vkOverride,
      vkInput, vkBindingProperty, vkBindingDirection, vkTrigger, vkPolicy, vkConstruction,
      vkCollectionScope, vkCollectionCellMode, vkPlatform, vkSplitOrientation, vkTouchBehavior:
      LPayload := IntToStr(AValue.Ordinal);
    vkStateRef: LPayload := IntToStr(AValue.Ordinal) + ':' + AValue.Text;
    vkCollectionField: LPayload := IntToStr(AValue.Ordinal) + ':' + AValue.Text;
    vkCollectionView: LPayload := AValue.CollectionView.ToData.ToJSON;
    vkData: LPayload := AValue.Data.ToJSON;
    vkDataField: LPayload := JoinSourceFrame([AValue.Text, ':', AValue.Data.ToJSON]);
    vkDecimal: LPayload := AValue.Text;
    vkEventSource: LPayload := IntToStr(Ord(AValue.EventSource.Source)) + ':' + AValue.EventSource.Part;
    vkDomain:
      begin
        case TNyxStateKind(AValue.Ordinal) of
          nskText: LDomain := AValue.TextDomain.Definition;
          nskBoolean: LDomain := AValue.BooleanDomain.Definition;
          nskInteger: LDomain := AValue.IntegerDomain.Definition;
          nskNumber: LDomain := AValue.NumberDomain.Definition;
        end;
        LPayload := LDomain.ToData.ToJSON;
      end;
  end;
  Result := JoinSourceFrame(['value:', IntToStr(Ord(AValue.Kind)), ':',
    IntToStr(Length(LPayload)), ':', LPayload]);
end;

function BuilderAtoms(AContexts: TBuilderContexts; const ABody: TNyxText): TSourceAtoms;
var
  LTokens: TTokens;
  LReader: TConfigurationReader;
  LIndex: Integer;
  LFinish: Integer;
  LCount: Integer;
  LLocal: Integer;
  LValue: TValue;
  LKey: TNyxText;
  LExpression: Boolean;
begin
  LTokens := AContexts.ForSource(ABody).Tokens;
  LReader := AContexts.ForSource(ABody).NewReader;
  try
    { Only already-admitted source reaches reconciliation. Resolve named values
      for comparison; actual admission still replays assignment order separately. }
    for LIndex := 0 to Length(LReader.FStates) - 1 do
    begin
      LReader.FStates[LIndex].Assigned := True;
    end;
    SetLength(Result, Length(LTokens));
    LCount := 0;
    LIndex := 0;
    while LIndex < Length(LTokens) do
    begin
      LFinish := LIndex + 1;
      LKey := LTokens[LIndex].Text;
      LExpression := (LIndex > 0) and
        ((LTokens[LIndex - 1].Text = '(') or (LTokens[LIndex - 1].Text = ',') or
        (LTokens[LIndex - 1].Text = ':='));

      if LExpression and (LTokens[LIndex].Kind = tkWord) then
      begin
        { Already-admitted ownership references and control factories are
          structural atoms. Recognize them before scalar probing: an expected
          exception here used to compute a complete-file diagnostic for every
          control, despite the caller immediately discarding that diagnostic.
          Real candidate admission still uses the full typed reader and errors. }
        LKey := LowerCase(LTokens[LIndex].Text);
        LExpression := (LReader.LocalIndex(LKey) < 0) and (LKey <> 'result') and
          (LKey <> 'nil') and (Copy(LKey, 1, 6) <> 'newnyx') and
          not ((Copy(LKey, 1, 4) = 'tnyx') and (LIndex + 2 < Length(LTokens)) and
            (LTokens[LIndex + 1].Text = '.') and SameText(LTokens[LIndex + 2].Text, 'Create'));
      end;

      if LExpression then
      begin
        LReader.FCursor := LIndex;
        try
          LValue := LReader.Expression;
          { Composite construction remains a sequence of atoms. Changing one
            field must not replace a whole object/domain and erase unrelated
            authored expressions or notes in its other values. }
          LExpression := not (LValue.Kind in [vkDataField, vkDomain, vkCollectionView]) and
            not ((LValue.Kind = vkData) and (LValue.Data.Kind in [ndObject, ndArray]));

          if LExpression then
          begin
            LKey := SourceValueKey(LValue, LReader);
            LFinish := LReader.FCursor;
          end;
        except
          on LException: ENyxSource do
          begin
            { Ownership calls and control factories are structural tokens,
              not scalar expression atoms. They retain their exact grammar. }
            LExpression := False;
          end;
        end;
      end;

      if not LExpression then
      begin

        if LTokens[LIndex].Kind = tkWord then
        begin
          LKey := LowerCase(LKey);

          if (LIndex = 0) or (LTokens[LIndex - 1].Text <> '.') then
          begin
            LLocal := LReader.LocalIndex(LKey);

            if LLocal >= 0 then
            begin
              LKey := JoinSourceFrame(['control:', LReader.FLocals[LLocal].ID]);
            end;
          end;
        end;
        LKey := JoinSourceFrame(['token:', IntToStr(Ord(LTokens[LIndex].Kind)), ':', LKey]);
      end;
      Result[LCount].Key := LKey;
      Result[LCount].Position := LTokens[LIndex].Position;
      Result[LCount].Finish := LTokens[LFinish - 1].Finish;
      Inc(LCount);
      LIndex := LFinish;
    end;
    SetLength(Result, LCount);
  finally
    LReader.Free;
  end;
end;

procedure SourceDifferences(const ABefore, AAfter: TSourceAtoms;
  out AChanges: TSourceChanges; out AMatches: TSourceIntegers);
const
  CMaximumDistance = 2048;
  CMaximumTraceCells = 2 * 1024 * 1024;
var
  LFront: TSourceIntegers;
  LTrace: TSourceTrace;
  LDistance: Integer;
  LMaximum: Integer;
  LOffset: Integer;
  LDiagonal: Integer;
  LPrevious: Integer;
  LX: Integer;
  LY: Integer;
  LPreviousX: Integer;
  LPreviousY: Integer;
  LIndex: Integer;
  LCells: Integer;
  LBefore: Integer;
  LAfter: Integer;
  LCount: Integer;
  LFound: Boolean;
begin
  { Myers' shortest edit path needs O(distance squared) trace storage, rather
    than a whole-source quadratic matrix. Typical visual edits have a small
    distance even in large documents. The explicit cap fails before publication. }
  SetLength(AMatches, Length(ABefore));
  for LIndex := 0 to Length(AMatches) - 1 do
  begin
    AMatches[LIndex] := -1;
  end;
  AChanges := nil;
  LMaximum := Length(ABefore) + Length(AAfter);

  if LMaximum > CMaximumDistance then
  begin
    LMaximum := CMaximumDistance;
  end;
  LOffset := LMaximum + 1;
  SetLength(LFront, 2 * LMaximum + 3);
  SetLength(LTrace, LMaximum + 1);
  LFront[LOffset + 1] := 0;
  LCells := 0;
  LFound := False;
  LDistance := 0;
  while LDistance <= LMaximum do
  begin
    Inc(LCells, 2 * LDistance + 1);

    if LCells > CMaximumTraceCells then
    begin
      Break;
    end;
    SetLength(LTrace[LDistance], 2 * LDistance + 1);
    LDiagonal := -LDistance;
    while LDiagonal <= LDistance do
    begin

      if (LDiagonal = -LDistance) or ((LDiagonal <> LDistance) and
        (LFront[LOffset + LDiagonal - 1] < LFront[LOffset + LDiagonal + 1])) then
      begin
        LX := LFront[LOffset + LDiagonal + 1];
      end
      else
      begin
        LX := LFront[LOffset + LDiagonal - 1] + 1;
      end;
      LY := LX - LDiagonal;
      while (LX < Length(ABefore)) and (LY < Length(AAfter)) and
        (ABefore[LX].Key = AAfter[LY].Key) do
      begin
        Inc(LX);
        Inc(LY);
      end;
      LFront[LOffset + LDiagonal] := LX;
      LTrace[LDistance][LDiagonal + LDistance] := LX;

      if (LX >= Length(ABefore)) and (LY >= Length(AAfter)) then
      begin
        LFound := True;
        Break;
      end;
      Inc(LDiagonal, 2);
    end;

    if LFound then
    begin
      Break;
    end;
    Inc(LDistance);
  end;

  if not LFound then
  begin
    raise ENyxSource.CreateAt('Source reconciliation exceeds its edit budget; keep the accepted pair and split this visual change', '', 1);
  end;
  LX := Length(ABefore);
  LY := Length(AAfter);
  while LDistance > 0 do
  begin
    LDiagonal := LX - LY;
    LPrevious := LDiagonal - 1;

    if (LDiagonal = -LDistance) or ((LDiagonal <> LDistance) and
      (LTrace[LDistance - 1][LDiagonal - 1 + LDistance - 1] <
      LTrace[LDistance - 1][LDiagonal + 1 + LDistance - 1])) then
    begin
      LPrevious := LDiagonal + 1;
    end;
    LPreviousX := LTrace[LDistance - 1][LPrevious + LDistance - 1];
    LPreviousY := LPreviousX - LPrevious;
    while (LX > LPreviousX) and (LY > LPreviousY) do
    begin
      Dec(LX);
      Dec(LY);
      AMatches[LX] := LY;
    end;
    LX := LPreviousX;
    LY := LPreviousY;
    Dec(LDistance);
  end;
  while (LX > 0) and (LY > 0) do
  begin
    Dec(LX);
    Dec(LY);
    AMatches[LX] := LY;
  end;
  LBefore := 0;
  LAfter := 0;
  LCount := 0;
  for LIndex := 0 to Length(ABefore) do
  begin

    if (LIndex = Length(ABefore)) or (AMatches[LIndex] >= 0) then
    begin
      LY := Length(AAfter);

      if LIndex < Length(ABefore) then
      begin
        LY := AMatches[LIndex];
      end;

      if (LIndex > LBefore) or (LY > LAfter) then
      begin
        SetLength(AChanges, LCount + 1);
        AChanges[LCount].BeforeStart := LBefore;
        AChanges[LCount].BeforeFinish := LIndex;
        AChanges[LCount].AfterStart := LAfter;
        AChanges[LCount].AfterFinish := LY;
        Inc(LCount);
      end;
      LBefore := LIndex + 1;
      LAfter := LY + 1;
    end;
  end;
end;

function CommentsWithin(AContexts: TBuilderContexts;
  const ASource: TNyxText; AStart, AFinish: Integer): TNyxText;
var
  LTokens: TTokens;
  LIndex: Integer;
  LParts: TNyxStrings;
  LComment: TNyxText;
begin
  LTokens := AContexts.ForSource(ASource).AllTokens;
  LParts := TNyxStrings.Create;
  try
    for LIndex := 0 to Length(LTokens) - 1 do
    begin

      if (LTokens[LIndex].Kind = tkComment) and
        (LTokens[LIndex].Position >= AStart) and (LTokens[LIndex].Finish <= AFinish) then
      begin
        LComment := Copy(ASource, LTokens[LIndex].Position,
          LTokens[LIndex].Finish - LTokens[LIndex].Position);

        if (Copy(LComment, 1, 2) = '//') or (Pos(#10, LComment) > 0) or
          (Pos(#13, LComment) > 0) then
        begin
          LParts.Add(#10);
          LParts.Add(LComment);
          LParts.Add(#10);
        end
        else
        begin
          LParts.Add(LComment);
          LParts.Add(' ');
        end;
      end;
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function ReconcileBuilderText(AContexts: TBuilderContexts;
  const ABefore, AAuthored, AAfter: TNyxText): TNyxText;
var
  LBefore: TSourceAtoms;
  LAuthored: TSourceAtoms;
  LAfter: TSourceAtoms;
  LChanges: TSourceChanges;
  LAuthoredChanges: TSourceChanges;
  LMatches: TSourceIntegers;
  LAuthoredMatches: TSourceIntegers;
  LIndex: Integer;
  LOldIndex: Integer;
  LStart: Integer;
  LFinish: Integer;
  LCursor: Integer;
  LNewStart: Integer;
  LNewFinish: Integer;
  LNewText: TNyxText;
  LNotes: TNyxText;
  LParts: TNyxStrings;
begin
  { Preserve exact authored bytes without lexical work when the generated
    section has no delta. A pristine section can likewise take the generated
    replacement directly. The whole-pair verifier still admits the final body. }

  if ABefore = AAfter then
  begin
    Exit(AAuthored);
  end;

  if ABefore = AAuthored then
  begin
    Exit(AAfter);
  end;
  LBefore := BuilderAtoms(AContexts, ABefore);
  LAuthored := BuilderAtoms(AContexts, AAuthored);
  LAfter := BuilderAtoms(AContexts, AAfter);
  SourceDifferences(LBefore, LAfter, LChanges, LMatches);

  if Length(LChanges) = 0 then
  begin
    Exit(AAuthored);
  end;
  SourceDifferences(LBefore, LAuthored, LAuthoredChanges, LAuthoredMatches);
  LParts := TNyxStrings.Create;
  try
    LCursor := 1;
    for LIndex := 0 to Length(LChanges) - 1 do
    begin
      LOldIndex := LChanges[LIndex].BeforeStart;
      LStart := 1;
      while LOldIndex > 0 do
      begin
        Dec(LOldIndex);

        if LAuthoredMatches[LOldIndex] >= 0 then
        begin
          LStart := LAuthored[LAuthoredMatches[LOldIndex]].Finish;
          Break;
        end;
      end;
      LOldIndex := LChanges[LIndex].BeforeFinish;
      LFinish := Length(AAuthored) + 1;
      while LOldIndex < Length(LBefore) do
      begin

        if LAuthoredMatches[LOldIndex] >= 0 then
        begin
          LFinish := LAuthored[LAuthoredMatches[LOldIndex]].Position;
          Break;
        end;
        Inc(LOldIndex);
      end;

      if LStart < LCursor then
      begin
        raise ENyxSource.CreateAt('Overlapping authored edits require a source merge before this visual change', AAuthored, LStart);
      end;
      LNewStart := 1;

      if LChanges[LIndex].AfterStart > 0 then
      begin
        LNewStart := LAfter[LChanges[LIndex].AfterStart - 1].Finish;
      end;
      LNewFinish := Length(AAfter) + 1;

      if LChanges[LIndex].AfterFinish < Length(LAfter) then
      begin
        LNewFinish := LAfter[LChanges[LIndex].AfterFinish].Position;
      end;
      LNewText := Copy(AAfter, LNewStart, LNewFinish - LNewStart);
      LParts.Add(Copy(AAuthored, LCursor, LStart - LCursor));

      if LChanges[LIndex].BeforeStart = LChanges[LIndex].BeforeFinish then
      begin
        { Both sides may insert fluent calls at an otherwise empty gap. Retain
          the admitted authored calls first; the visual insertion then supplies
          the new final value. The verifier checks the complete execution result. }
        LNotes := Copy(AAuthored, LStart, LFinish - LStart);

        if Trim(LNotes) <> '' then
        begin
          LParts.Add(LNotes);
        end;
        LNotes := '';
      end
      else
      begin
        LNotes := CommentsWithin(AContexts, AAuthored, LStart, LFinish);
      end;
      LParts.Add(LNotes);
      LParts.Add(LNewText);
      LCursor := LFinish;
    end;
    LParts.Add(Copy(AAuthored, LCursor, MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function UpgradeLegacyBuilder(AContexts: TBuilderContexts;
  const ABody, AGenerated: TNyxText): TNyxText;
var
  LTokens: TTokens;
  LGenerated: TTokens;
  LReader: TConfigurationReader;
  LGeneratedReader: TConfigurationReader;
  LParts: TNyxStrings;
  LIndex: Integer;
  LOther: Integer;
  LFinish: Integer;
  LGeneratedFinish: Integer;
  LCursor: Integer;
  LArgument: Integer;
  LIDStart: Integer;
  LIDFinish: Integer;
  LValue: TValue;
  LID: TNyxText;
  LReplacement: TNyxText;
  LLegacy: Boolean;
begin
  { Recovered base-class builders migrate to the specialized interface contract.
    Restrict replacement to actual control declarations/constructors, preserving
    authored identity expressions, all scalar configuration and every comment. }
  LTokens := AContexts.ForSource(ABody).Tokens;
  LLegacy := False;
  for LIndex := 2 to Length(LTokens) - 1 do
  begin

    if SameText(LTokens[LIndex].Text, 'TNyxNode') and
      ((LTokens[LIndex - 1].Text = ':') or (LTokens[LIndex - 1].Text = ':=')) then
    begin
      LLegacy := True;
      Break;
    end;
  end;

  if not LLegacy then
  begin
    { Specialized builders need no migration readers or generated lookup. }
    Exit(ABody);
  end;
  LGenerated := AContexts.ForSource(AGenerated).Tokens;
  LReader := AContexts.ForSource(ABody).NewReader;
  LGeneratedReader := nil;
  LParts := TNyxStrings.Create;
  try
    LGeneratedReader := AContexts.ForSource(AGenerated).NewReader;
    LCursor := 1;
    LIndex := 2;
    while LIndex < Length(LTokens) do
    begin
      LReplacement := '';
      LFinish := LIndex + 1;

      if SameText(LTokens[LIndex].Text, 'TNyxNode') and
        ((LTokens[LIndex - 1].Text = ':') or (LTokens[LIndex - 1].Text = ':=')) then
      begin
        LOther := 0;
        while LOther + 2 < Length(LGenerated) do
        begin

          if SameText(LGenerated[LOther].Text, LTokens[LIndex - 2].Text) and
            (LGenerated[LOther + 1].Text = LTokens[LIndex - 1].Text) then
          begin

            if LTokens[LIndex - 1].Text = ':' then
            begin
              LReplacement := LGenerated[LOther + 2].Text;
            end
            else
            begin
              LFinish := LIndex;
              while (LFinish < Length(LTokens)) and
                ((LTokens[LFinish].Kind <> tkSymbol) or (LTokens[LFinish].Text <> ';')) do
              begin
                Inc(LFinish);
              end;
              LGeneratedFinish := LOther + 2;
              while (LGeneratedFinish < Length(LGenerated)) and
                ((LGenerated[LGeneratedFinish].Kind <> tkSymbol) or
                (LGenerated[LGeneratedFinish].Text <> ';')) do
              begin
                Inc(LGeneratedFinish);
              end;
              LReader.FCursor := LIndex + 4;
              LValue := LReader.Expression;
              LReader.Expect(',');
              LArgument := LReader.FCursor;
              LValue := LReader.Expression;
              LID := Copy(ABody, LTokens[LArgument].Position,
                LTokens[LReader.FCursor - 1].Finish - LTokens[LArgument].Position);
              LGeneratedReader.FCursor := LOther + 4;

              if SameText(LGenerated[LOther + 2].Text, 'NewNyxControl') then
              begin
                LValue := LGeneratedReader.Expression;
                LGeneratedReader.Expect(',');
              end;
              LIDStart := LGenerated[LGeneratedReader.FCursor].Position;
              LValue := LGeneratedReader.Expression;
              LIDFinish := LGenerated[LGeneratedReader.FCursor - 1].Finish;
              LReplacement := JoinSourceFrame([
                Copy(AGenerated, LGenerated[LOther + 2].Position,
                  LIDStart - LGenerated[LOther + 2].Position), LID,
                Copy(AGenerated, LIDFinish, LGenerated[LGeneratedFinish].Position - LIDFinish)]);
              { Notes embedded in the removed kind argument are retained as
                comments beside the new factory; identity notes stay inline. }
              LReplacement := JoinSourceFrame([LReplacement,
                CommentsWithin(AContexts, ABody, LTokens[LIndex].Position, LTokens[LArgument].Position)]);
            end;
            Break;
          end;
          Inc(LOther);
        end;
      end;

      if LReplacement <> '' then
      begin
        LParts.Add(Copy(ABody, LCursor, LTokens[LIndex].Position - LCursor));
        LParts.Add(LReplacement);
        LCursor := LTokens[LFinish - 1].Finish;
      end;
      LIndex := LFinish;
    end;
    LParts.Add(Copy(ABody, LCursor, MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
    LGeneratedReader.Free;
    LReader.Free;
  end;
end;

type
  TBuilderMetadataGroup = record
    Identity: TNyxText;
    Text: TNyxText;
    FirstSlot: Integer;
  end;
  TBuilderMetadataGroups = array of TBuilderMetadataGroup;
  TBuilderMetadataSlot = record
    Group: Integer;
    Text: TNyxText;
  end;
  TBuilderMetadataSlots = array of TBuilderMetadataSlot;

function MetadataGroupsIndex(const AGroups: TBuilderMetadataGroups): TSourceIndex;
var
  LIndex: Integer;
begin
  Result := TSourceIndex.Create;
  try
    for LIndex := 0 to Length(AGroups) - 1 do
    begin
      Result.AddFirst(AGroups[LIndex].Identity, LIndex);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function MetadataMarker(const APrefix: TNyxText; ASlot: Integer): TNyxText;
begin
  Result := JoinSourceFrame([APrefix, IntToStr(ASlot), '> }']);
end;

procedure SplitBuilderMetadata(AContexts: TBuilderContexts; const ASource, AMarkerPrefix: TNyxText;
  out AFrame: TNyxText; out AGroups: TBuilderMetadataGroups;
  out ASlots: TBuilderMetadataSlots);
var
  LTokens: TTokens;
  LSymbols: TSourceSymbols;
  LNames: TSourceIndex;
  LGroups: TSourceIndex;
  LParts: TNyxStrings;
  LIndex: Integer;
  LFinish: Integer;
  LOwner: Integer;
  LGroup: Integer;
  LSlot: Integer;
  LCursor: Integer;
  LName: TNyxText;
  LSection: TNyxText;
  LIdentity: TNyxText;
  LStatement: TNyxText;
  LStart: Integer;
  LEnd: Integer;
begin
  { Metadata can be authored anywhere after owner admission. Partition it by
    stable owner identity before diffing the structural frame. A moved contract
    or callback block must never become an anchor for control construction.
    Inline notes remain part of their statement; surrounding notes stay at their
    exact source sites. Unchanged groups are reinserted into every original slot. }
  AGroups := nil;
  ASlots := nil;
  LTokens := AContexts.ForSource(ASource).Tokens;
  LSymbols := AContexts.ForSource(ASource).Symbols;
  LParts := TNyxStrings.Create;
  LNames := nil;
  LGroups := nil;
  try
    LNames := SymbolNames(LSymbols);
    LGroups := TSourceIndex.Create;
    LCursor := 1;
    LIndex := 0;
    while LIndex + 4 < Length(LTokens) do
    begin
      LName := '';
      LSection := '';

      if SameText(LTokens[LIndex].Text, 'NyxCallbacks') and
        (LTokens[LIndex + 1].Text = '(') then
      begin
        LName := LTokens[LIndex + 2].Text;
      end
      else if (LTokens[LIndex + 1].Text = '.') and
        (SameText(LTokens[LIndex + 2].Text, 'Contract') or
        SameText(LTokens[LIndex + 2].Text, 'Collections') or
        SameText(LTokens[LIndex + 2].Text, 'Extensions') or
        SameText(LTokens[LIndex + 2].Text, 'Configure') or
        SameText(LTokens[LIndex + 2].Text, 'Binds')) then
      begin
        LName := LTokens[LIndex].Text;

        if SameText(LTokens[LIndex + 2].Text, 'Configure') or
          SameText(LTokens[LIndex + 2].Text, 'Binds') then
        begin
          { Configuration and binding blocks have independent fluent grammars.
            A missing Configure can be supplied by WithText on a constructor;
            diffing it against Add/Binds would place new calls on the wrong API. }
          LSection := ':' + LowerCase(LTokens[LIndex + 2].Text);
        end;
      end;
      LIdentity := '';

      if SameText(LName, 'Result') then
      begin
        LIdentity := 'document';
      end
      else if LName <> '' then
      begin
        LOwner := LNames.IndexOf(LowerCase(LName));

        if (LOwner >= 0) and (LSymbols[LOwner].Kind = ssControl) then
        begin
          { IDs are open application text and may themselves contain colons or
            section names. Length framing prevents an owner from colliding with
            another owner's Configure/Binds group. }
          LIdentity := JoinSourceFrame(['control:',
            IntToStr(Length(LSymbols[LOwner].Identity)), ':',
            LSymbols[LOwner].Identity, LSection]);
        end;
      end;

      if LIdentity <> '' then
      begin
        LFinish := LIndex;
        while (LFinish < Length(LTokens)) and
          ((LTokens[LFinish].Kind <> tkSymbol) or (LTokens[LFinish].Text <> ';')) do
        begin
          Inc(LFinish);
        end;

        if LFinish >= Length(LTokens) then
        begin
          raise ENyxSource.CreateAt('Finish the admitted metadata statement', ASource,
            LTokens[LIndex].Position);
        end;
        LStatement := Copy(ASource, LTokens[LIndex].Position,
          LTokens[LFinish].Finish - LTokens[LIndex].Position);
        LGroup := LGroups.IndexOf(LIdentity);
        LSlot := Length(ASlots);

        if LGroup < 0 then
        begin
          LGroup := Length(AGroups);
          SetLength(AGroups, LGroup + 1);
          AGroups[LGroup].Identity := LIdentity;
          AGroups[LGroup].Text := '';
          AGroups[LGroup].FirstSlot := LSlot;
          LGroups.AddFirst(LIdentity, LGroup);
        end;
        AGroups[LGroup].Text := JoinSourceFrame([AGroups[LGroup].Text,
          '    ', LStatement, #10]);
        SetLength(ASlots, LSlot + 1);
        ASlots[LSlot].Group := LGroup;
        ASlots[LSlot].Text := LStatement;
        LStart := LTokens[LIndex].Position;
        LEnd := LTokens[LFinish].Finish;

        if AMarkerPrefix = '' then
        begin
          { Generated comparison frames drop whole metadata lines. Leaving
            their indentation behind would insert empty indented rows whenever
            the structural merge adds a nearby control. Authored sites retain
            their own exact surrounding whitespace through private slots. }
          while (LStart > LCursor) and (ASource[LStart - 1] in [' ', #9]) do
          begin
            Dec(LStart);
          end;
          while (LEnd <= Length(ASource)) and (ASource[LEnd] in [' ', #9, #13]) do
          begin
            Inc(LEnd);
          end;

          if (LEnd <= Length(ASource)) and (ASource[LEnd] = #10) then
          begin
            Inc(LEnd);
          end;
        end;
        LParts.Add(Copy(ASource, LCursor, LStart - LCursor));

        if AMarkerPrefix <> '' then
        begin
          LParts.Add(MetadataMarker(AMarkerPrefix, LSlot));
        end;
        LCursor := LEnd;
        LIndex := LFinish;
      end;
      Inc(LIndex);
    end;
    LParts.Add(Copy(ASource, LCursor, MaxInt));
    AFrame := LParts.Join;
  finally
    LGroups.Free;
    LNames.Free;
    LParts.Free;
  end;
end;

function ExpandBuilderMetadata(AContexts: TBuilderContexts; const AFrame, APrefix: TNyxText;
  const ASlots: TBuilderMetadataSlots; const AUpdates: TBuilderMetadataGroups;
  const AOriginals: TBuilderMetadataGroups): TNyxText;
var
  LTokens: TTokens;
  LParts: TNyxStrings;
  LIndex: Integer;
  LSlot: Integer;
  LGroup: Integer;
  LCursor: Integer;
  LReplacement: TNyxText;
begin
  LTokens := AContexts.ForSource(AFrame).AllTokens;
  LParts := TNyxStrings.Create;
  try
    LCursor := 1;
    for LIndex := 0 to Length(LTokens) - 1 do
    begin

      if (LTokens[LIndex].Kind <> tkComment) or
        (Copy(LTokens[LIndex].Text, 1, Length(APrefix)) <> APrefix) then
      begin
        Continue;
      end;

      if not TryStrToInt(Copy(LTokens[LIndex].Text, Length(APrefix) + 1,
        Length(LTokens[LIndex].Text) - Length(APrefix) - 3), LSlot) or
        (LSlot < 0) or (LSlot >= Length(ASlots)) then
      begin
        raise ENyxSource.CreateAt('Invalid private reconciliation slot', AFrame,
          LTokens[LIndex].Position);
      end;
      LGroup := ASlots[LSlot].Group;
      LReplacement := '';

      if AUpdates[LGroup].Text = AOriginals[LGroup].Text then
      begin
        LReplacement := ASlots[LSlot].Text;
      end
      else if LSlot = AOriginals[LGroup].FirstSlot then
      begin
        LReplacement := AUpdates[LGroup].Text;

        if Copy(LReplacement, 1, 4) = '    ' then
        begin
          LReplacement := Copy(LReplacement, 5, MaxInt);
        end;

        if Copy(LReplacement, Length(LReplacement), 1) = #10 then
        begin
          LReplacement := Copy(LReplacement, 1, Length(LReplacement) - 1);
        end;
      end;
      LParts.Add(Copy(AFrame, LCursor, LTokens[LIndex].Position - LCursor));
      LParts.Add(LReplacement);
      LCursor := LTokens[LIndex].Finish;
    end;
    LParts.Add(Copy(AFrame, LCursor, MaxInt));
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function AppendBuilderMetadata(AContexts: TBuilderContexts;
  const ABody, ADeferred: TNyxText): TNyxText;
var
  LTokens: TTokens;
  LIndex: Integer;
  LPosition: Integer;
begin
  Result := ABody;

  if ADeferred = '' then
  begin
    Exit;
  end;
  LTokens := AContexts.ForSource(ABody).Tokens;
  for LIndex := Length(LTokens) - 1 downto 0 do
  begin

    if (LTokens[LIndex].Kind = tkWord) and SameText(LTokens[LIndex].Text, 'except') then
    begin
      LPosition := LTokens[LIndex].Position;
      while (LPosition > 1) and (ABody[LPosition - 1] <> #10) do
      begin
        Dec(LPosition);
      end;
      Exit(JoinSourceFrame([Copy(ABody, 1, LPosition - 1), #10, ADeferred,
        Copy(ABody, LPosition, MaxInt)]));
    end;
  end;
  raise ENyxSource.CreateAt('The admitted builder requires its document cleanup boundary', ABody, 1);
end;

function ReconcilePartitionedBuilder(AContexts: TBuilderContexts;
  const ABefore, AAuthored, AAfter: TNyxText): TNyxText;
var
  LBeforeFrame: TNyxText;
  LAuthoredFrame: TNyxText;
  LAfterFrame: TNyxText;
  LPrefix: TNyxText;
  LOldText: TNyxText;
  LNewText: TNyxText;
  LBeforeGroups: TBuilderMetadataGroups;
  LAuthoredGroups: TBuilderMetadataGroups;
  LAfterGroups: TBuilderMetadataGroups;
  LUpdates: TBuilderMetadataGroups;
  LSlots: TBuilderMetadataSlots;
  LIgnoredSlots: TBuilderMetadataSlots;
  LDeferred: TNyxStrings;
  LBeforeIndex: TSourceIndex;
  LAuthoredIndex: TSourceIndex;
  LAfterIndex: TSourceIndex;
  LIndex: Integer;
  LGroup: Integer;
  LSerial: Integer;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  {$endif}
begin
  LBeforeIndex := nil;
  LAuthoredIndex := nil;
  LAfterIndex := nil;
  try
    LSerial := 0;
    repeat
      Inc(LSerial);
      LPrefix := JoinSourceFrame(['{ <nyx:source:', IntToStr(LSerial), ':slot:']);
    until (Pos(LPrefix, ABefore) = 0) and (Pos(LPrefix, AAuthored) = 0) and
      (Pos(LPrefix, AAfter) = 0);
    {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
    SplitBuilderMetadata(AContexts, ABefore, '', LBeforeFrame, LBeforeGroups, LIgnoredSlots);
    SplitBuilderMetadata(AContexts, AAuthored, LPrefix, LAuthoredFrame, LAuthoredGroups, LSlots);
    SplitBuilderMetadata(AContexts, AAfter, '', LAfterFrame, LAfterGroups, LIgnoredSlots);
    LBeforeIndex := MetadataGroupsIndex(LBeforeGroups);
    LAuthoredIndex := MetadataGroupsIndex(LAuthoredGroups);
    LAfterIndex := MetadataGroupsIndex(LAfterGroups);
    {$ifdef NYX_SOURCE_PROFILE}
    SourceProfileFinish(spMetadataSplit, LStarted);
    LStarted := SourceProfileStart;
    {$endif}
    SetLength(LUpdates, Length(LAuthoredGroups));
    for LIndex := 0 to Length(LAuthoredGroups) - 1 do
    begin
      LUpdates[LIndex].Identity := LAuthoredGroups[LIndex].Identity;
      LUpdates[LIndex].FirstSlot := LAuthoredGroups[LIndex].FirstSlot;
      LOldText := '';
      LNewText := '';
      LGroup := LBeforeIndex.IndexOf(LAuthoredGroups[LIndex].Identity);

      if LGroup >= 0 then
      begin
        LOldText := LBeforeGroups[LGroup].Text;
      end;
      LGroup := LAfterIndex.IndexOf(LAuthoredGroups[LIndex].Identity);

      if LGroup >= 0 then
      begin
        LNewText := LAfterGroups[LGroup].Text;
      end;
      LUpdates[LIndex].Text := ReconcileBuilderText(AContexts, LOldText, LAuthoredGroups[LIndex].Text, LNewText);
    end;
    {$ifdef NYX_SOURCE_PROFILE}
    SourceProfileFinish(spMetadataMerge, LStarted);
    LStarted := SourceProfileStart;
    {$endif}
    Result := ReconcileBuilderText(AContexts, LBeforeFrame, LAuthoredFrame, LAfterFrame);
    {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spFrameMerge, LStarted);{$endif}
    Result := ExpandBuilderMetadata(AContexts, Result, LPrefix, LSlots, LUpdates, LAuthoredGroups);
    LDeferred := TNyxStrings.Create;
    try
      for LIndex := 0 to Length(LAfterGroups) - 1 do
      begin

        if LAuthoredIndex.IndexOf(LAfterGroups[LIndex].Identity) < 0 then
        begin
          LGroup := LBeforeIndex.IndexOf(LAfterGroups[LIndex].Identity);

          if (LGroup < 0) or (LBeforeGroups[LGroup].Text <> LAfterGroups[LIndex].Text) then
          begin
            { An unchanged generated Configure may already be expressed by a
              factory's WithText/default recipe. Only changed/new meaning needs
              a new block when the authored builder had no corresponding slot. }
            LDeferred.Add(LAfterGroups[LIndex].Text);
          end;
        end;
      end;
      Result := AppendBuilderMetadata(AContexts, Result, LDeferred.Join);
    finally
      LDeferred.Free;
    end;
  finally
    LAfterIndex.Free;
    LAuthoredIndex.Free;
    LBeforeIndex.Free;
  end;
end;

function PruneRemovedBuilderControls(AContexts: TBuilderContexts; const ABody: TNyxText;
  const ARetained: TSourceSymbols): TNyxText;
var
  LTokens: TTokens;
  LSymbols: TSourceSymbols;
  LParts: TNyxStrings;
  LIndex: Integer;
  LFinish: Integer;
  LCursor: Integer;
  LSymbol: Integer;
  LChild: Integer;
  LRemoved: Boolean;
  LNames: TNyxStrings;
  LDeclaration: Boolean;
  LInVariables: Boolean;
  LStart: Integer;
  LEnd: Integer;
  LNotes: TNyxText;
  LHaveRemoved: Boolean;
  LRemovedSymbols: array of Boolean;
  LRetainedIDs: TSourceIndex;
  LSymbolNames: TSourceIndex;

  function Removed(AIndex: Integer): Boolean;
  begin
    Result := False;

    if (AIndex < 0) or (LSymbols[AIndex].Kind <> ssControl) or
      (LSymbols[AIndex].Identity = '') then
    begin
      Exit;
    end;
    Result := LRemovedSymbols[AIndex];
  end;

begin
  { A constructor may be staged long before its ownership statement. Removing
    a root through a token diff would then let unrelated factory punctuation
    match that staged construction. Remove complete statements by admitted
    identity first, in both private comparison and authored frames. Retained
    constructors, ownership expressions and their ordering stay byte-exact.

    Declarations can group locals; remove only excluded control names and keep
    unused authored declarations. Notes from removed statements stay as notes.
    No application code executes here. The full typed candidate verifier still
    checks the merged result before either member of the pair is published. }
  LSymbols := AContexts.ForSource(ABody).Symbols;
  LRetainedIDs := TSourceIndex.Create;
  LSymbolNames := nil;
  try
    LSymbolNames := SymbolNames(LSymbols);
    for LIndex := 0 to Length(ARetained) - 1 do
    begin

      if ARetained[LIndex].Kind = ssControl then
      begin
        LRetainedIDs.AddFirst(ARetained[LIndex].Identity, LIndex);
      end;
    end;
    SetLength(LRemovedSymbols, Length(LSymbols));
    for LIndex := 0 to Length(LSymbols) - 1 do
    begin
      LRemovedSymbols[LIndex] := LRetainedIDs.IndexOf(LSymbols[LIndex].Identity) < 0;
    end;
    LHaveRemoved := False;
    for LIndex := 0 to Length(LSymbols) - 1 do
    begin

      if Removed(LIndex) then
      begin
        LHaveRemoved := True;
        Break;
      end;
    end;

    if not LHaveRemoved then
    begin
      Exit(ABody);
    end;
    LTokens := AContexts.ForSource(ABody).Tokens;
    LParts := TNyxStrings.Create;
    LNames := TNyxStrings.Create;
    try
      LCursor := 1;
      LIndex := 0;
      LInVariables := False;
      while LIndex + 2 < Length(LTokens) do
      begin

        if (LTokens[LIndex].Kind = tkWord) and SameText(LTokens[LIndex].Text, 'var') then
        begin
          LInVariables := True;
          Inc(LIndex);
          Continue;
        end;

        if (LTokens[LIndex].Kind = tkWord) and SameText(LTokens[LIndex].Text, 'begin') then
        begin
          LInVariables := False;
        end;
        LDeclaration := LInVariables and (LTokens[LIndex].Kind = tkWord);
        LSymbol := LSymbolNames.IndexOf(LowerCase(LTokens[LIndex].Text));
        LRemoved := False;
        LFinish := LIndex;

        if LDeclaration then
        begin
          LNames.Clear;
          repeat

            if Removed(LSymbolNames.IndexOf(LowerCase(LTokens[LFinish].Text))) then
            begin
              LRemoved := True;
            end
            else
            begin
              LNames.Add(LTokens[LFinish].Text);
            end;
            Inc(LFinish);

            if LTokens[LFinish].Text <> ',' then
            begin
              Break;
            end;
            Inc(LFinish);
          until LFinish >= Length(LTokens);
          { The frame is already admitted: this is its declaration's colon. }
          LChild := LFinish;
        end
        else if (LTokens[LIndex].Kind = tkWord) and
          ((LTokens[LIndex + 1].Text = ':=') or (LTokens[LIndex + 1].Text = '.')) then
        begin
          LRemoved := Removed(LSymbol);
        end
        else if (LTokens[LIndex].Kind = tkWord) and
          SameText(LTokens[LIndex].Text, 'NyxCallbacks') then
        begin
          LRemoved := Removed(LSymbolNames.IndexOf(LowerCase(LTokens[LIndex + 2].Text)));
        end
        else
        begin
          Inc(LIndex);
          Continue;
        end;
        while (LFinish < Length(LTokens)) and
          ((LTokens[LFinish].Kind <> tkSymbol) or (LTokens[LFinish].Text <> ';')) do
        begin
          Inc(LFinish);
        end;

        if LFinish >= Length(LTokens) then
        begin
          Break;
        end;

        if not LDeclaration and (LTokens[LIndex + 1].Text = '.') and
          (SameText(LTokens[LIndex + 2].Text, 'Add') or
          SameText(LTokens[LIndex + 2].Text, 'Insert') or
          SameText(LTokens[LIndex + 2].Text, 'AddPage') or
          SameText(LTokens[LIndex + 2].Text, 'AddComponent')) then
        begin
          { The admitted grammar's final argument is always the child local,
            including Insert with an independently authored integer expression. }
          LRemoved := LRemoved or
            Removed(LSymbolNames.IndexOf(LowerCase(LTokens[LFinish - 2].Text)));
        end;

        if LRemoved then
        begin
          LStart := LTokens[LIndex].Position;
          LEnd := LTokens[LFinish].Finish;
          LNotes := CommentsWithin(AContexts, ABody, LStart, LEnd);

          if not LDeclaration or (LNames.Count = 0) then
          begin
            { Remove the line's whitespace with its statement, rather than leaving
              one empty indented row per excluded control. Inline surrounding
              statements and notes keep their original offsets/content. }
            while (LStart > LCursor) and (ABody[LStart - 1] in [' ', #9]) do
            begin
              Dec(LStart);
            end;
            while (LEnd <= Length(ABody)) and (ABody[LEnd] in [' ', #9, #13]) do
            begin
              Inc(LEnd);
            end;

            if (LEnd <= Length(ABody)) and (ABody[LEnd] = #10) then
            begin
              Inc(LEnd);
            end;
          end;
          LParts.Add(Copy(ABody, LCursor, LStart - LCursor));
          LParts.Add(LNotes);

          if LDeclaration and (LNames.Count > 0) then
          begin
            LParts.Add(LNames.Join(', '));
            LParts.Add(Copy(ABody, LTokens[LChild].Position,
              LTokens[LFinish].Finish - LTokens[LChild].Position));
          end;
          LCursor := LEnd;
        end;
        LIndex := LFinish + 1;
      end;
      LParts.Add(Copy(ABody, LCursor, MaxInt));
      Result := LParts.Join;
    finally
      LNames.Free;
      LParts.Free;
    end;
  finally
    LSymbolNames.Free;
    LRetainedIDs.Free;
  end;
end;

function ReconcileNyxBuilder(AOldDocument: TNyxDocument;
  const ABody, AGenerated: TNyxText;
  const ARenames: array of TNyxSourceStateRename;
  out AAuthored: Boolean): TNyxText;
var
  LBefore: TNyxText;
  LAfter: TNyxText;
  LAuthoredBody: TNyxText;
  LPrefix: TNyxText;
  LSuffix: TNyxText;
  LTokens: TTokens;
  LAuthored: TSourceSymbols;
  LRetained: TSourceSymbols;
  LContexts: TBuilderContexts;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  {$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  Split(TNyxCodegen.Generate(AOldDocument), LPrefix, LBefore, LSuffix, LTokens);
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spOldGeneration, LStarted);{$endif}
  AAuthored := (ABody <> LBefore) or (Length(ARenames) > 0);

  if not AAuthored then
  begin
    { Ordinary generated builders have no authored delta to merge. Keep the
      generator's linear path for the overwhelmingly common visual command. }
    Exit(AGenerated);
  end;
  LContexts := TBuilderContexts.Create;
  try
    {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
    LAuthored := LContexts.ForSource(ABody).Symbols;
    {$ifdef NYX_SOURCE_PROFILE}
    SourceProfileFinish(spSymbols, LStarted);
    LStarted := SourceProfileStart;
    {$endif}
    LBefore := PreserveBuilderNames(LContexts, LBefore, LAuthored, []);
    {$ifdef NYX_SOURCE_PROFILE}
    SourceProfileFinish(spNames, LStarted);
    LStarted := SourceProfileStart;
    {$endif}
    LAuthoredBody := UpgradeLegacyBuilder(LContexts, ABody, LBefore);
    {$ifdef NYX_SOURCE_PROFILE}
    SourceProfileFinish(spLegacy, LStarted);
    LStarted := SourceProfileStart;
    {$endif}
    LAfter := PreserveBuilderNames(LContexts, AGenerated, LAuthored, ARenames);
    {$ifdef NYX_SOURCE_PROFILE}
    SourceProfileFinish(spNames, LStarted);
    LStarted := SourceProfileStart;
    {$endif}
    LRetained := LContexts.ForSource(LAfter).Symbols;
    {$ifdef NYX_SOURCE_PROFILE}
    SourceProfileFinish(spSymbols, LStarted);
    LStarted := SourceProfileStart;
    {$endif}
    LAuthoredBody := PruneRemovedBuilderControls(LContexts, LAuthoredBody, LRetained);
    LBefore := PruneRemovedBuilderControls(LContexts, LBefore, LRetained);
    {$ifdef NYX_SOURCE_PROFILE}
    SourceProfileFinish(spPrune, LStarted);
    LStarted := SourceProfileStart;
    {$endif}
    LBefore := ScaffoldBuilderSections(LContexts, LBefore, LAuthoredBody);
    LAfter := ScaffoldBuilderSections(LContexts, LAfter, LAuthoredBody);
    {$ifdef NYX_SOURCE_PROFILE}
    SourceProfileFinish(spScaffold, LStarted);
    LStarted := SourceProfileStart;
    {$endif}
    Result := ReconcilePartitionedBuilder(LContexts, LBefore, LAuthoredBody, LAfter);
    {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spPartition, LStarted);{$endif}
  finally
    LContexts.Free;
  end;
end;
