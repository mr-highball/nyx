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
unit nyx.pascal.outline;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.model, nyx.controls, nyx.source,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.sources;

const
  NyxPascalOutlinePageSize = 12;

type
  { Closed physical counterparts. Undefined sites remain visible but disabled;
    no inferred main-file position substitutes for a split/include location. }
  TNyxPascalOutlinePart = (popDefinition, popDeclaration, popBody);

  { Routine counterparts and general declared structure share one source card;
    neither presentation choice requests compiler type/symbol completion. }
  TNyxPascalOutlineContent = (pocRoutines, pocDeclarations);

  { Presentation intent only. A navigation action carries a revision-local row
    ordinal, not a persistent routine identity or permission to edit source. }
  TNyxPascalOutlineAction = (poaNone, poaField, poaOwner, poaFind,
    poaPrevious, poaNext, poaNavigate, poaSnapshot, poaContent);

{ General declared structure uses the same owner/origin/search/page controls and
  physical navigation intent. Rows retain lexical scopes and remain read-only. }
function NewNyxPascalSymbolOutline(const AID: TNyxText; const AProject: INyxPascalProject;
  const AIndex: INyxPascalSymbolIndex; const AOwner: TNyxPascalSourceRef;
  const AQuery, AFilter: TNyxText; APage: Integer;
  AKind: TNyxPascalSourceSnapshotKind; const AMessage: TNyxText = ''): INyxCard;
{ Decode only this card's closed presentation captions. }
function NyxPascalOutlineContent(const ACaption: TNyxText): TNyxPascalOutlineContent;

{ Build an ordinary reusable Nyx card from a borrowed immutable project/index.
  Search matches routine names and signatures; at most twelve rows are mounted.
  Matching reuses Nyx's locale-independent full Unicode folding. Authored text
  and query fields remain exact; no normalization or accent removal occurs.
  Owner is explicit even for shared includes. The card retains no provider,
  session, widget or compiler; callers own snapshot validation before navigation.
  Query is applied on Find, preserving a live search field while the user types. }
function NewNyxPascalOutline(const AID: TNyxText; const AProject: INyxPascalProject;
  const AIndex: INyxPascalRoutineIndex; const AOwner: TNyxPascalSourceRef;
  const AQuery, AFilter: TNyxText; APage: Integer; const AMessage: TNyxText = ''): INyxCard; overload;
{ Snapshot origin is a closed presentation choice, never compiler authority.
  The embedding owner pairs the index with these exact accepted members and
  inspected buffers, validating that same snapshot before placing a caret. }
function NewNyxPascalOutline(const AID: TNyxText; const AProject: INyxPascalProject;
  const AIndex: INyxPascalRoutineIndex; const AOwner: TNyxPascalSourceRef;
  const AQuery, AFilter: TNyxText; APage: Integer;
  AKind: TNyxPascalSourceSnapshotKind; const AMessage: TNyxText = ''): INyxCard; overload;
{ Decode a closed source-selector caption at the UI boundary. Unknown values
  refuse, preserving the caller's current inspection/selection. }
function NyxPascalOutlineSnapshotKind(const ACaption: TNyxText): TNyxPascalSourceSnapshotKind;

{ Decode only the card's closed controls. The embedding owner must additionally
  require a currently mounted node and the exact source snapshot. }
function NyxPascalOutlineAction(const AID: TNyxText; ANode: TNyxNode;
  ATrigger: TNyxTrigger; out ARow: Integer;
  out APart: TNyxPascalOutlinePart): TNyxPascalOutlineAction;
{ Return the original physical counterpart, including its undefined state;
  the embedding editor must still validate its own snapshot and target buffer. }
function NyxPascalOutlineLocation(const ASite: TNyxPascalRoutineSite;
  APart: TNyxPascalOutlinePart): TNyxPascalRoutineLocation;

implementation

uses SysUtils, nyx.data, nyx.text.search;

const
  CRowKey = 'nyx.pascal.outline.row';
  CPartKey = 'nyx.pascal.outline.part';
  CSourceCaptions: array[TNyxPascalSourceSnapshotKind] of TNyxText =
    ('Accepted code', 'Editing buffers');
  CSourceSummaries: array[TNyxPascalSourceSnapshotKind] of TNyxText =
    ('accepted source', 'editing buffers');

function NyxPascalOutlineContent(const ACaption: TNyxText): TNyxPascalOutlineContent;
begin

  if ACaption = 'Routines' then
  begin
    Exit(pocRoutines);
  end;

  if ACaption = 'Declarations' then
  begin
    Exit(pocDeclarations);
  end;
  raise ENyxPascalProject.Create('Choose routines or declarations');
end;

function NyxPascalOutlineSnapshotKind(const ACaption: TNyxText): TNyxPascalSourceSnapshotKind;
var
  LKind: TNyxPascalSourceSnapshotKind;
begin
  for LKind := Low(TNyxPascalSourceSnapshotKind) to High(TNyxPascalSourceSnapshotKind) do
  begin

    if ACaption = CSourceCaptions[LKind] then
    begin
      Exit(LKind);
    end;
  end;
  raise ENyxPascalProject.Create('Choose accepted code or editing buffers');
end;

function NyxPascalOutlineLocation(const ASite: TNyxPascalRoutineSite;
  APart: TNyxPascalOutlinePart): TNyxPascalRoutineLocation;
begin
  case APart of
    popDefinition:
      begin
        Result := ASite.Definition;
      end;
    popDeclaration:
      begin
        Result := ASite.DeclarationLocation;
      end;
    popBody:
      begin
        Result := ASite.BodyLocation;
      end;
  end;
end;

function BuildPascalOutline(const AID: TNyxText; const AProject: INyxPascalProject;
  const AIndex: INyxPascalRoutineIndex; const ASymbols: INyxPascalSymbolIndex;
  AContent: TNyxPascalOutlineContent; const AOwner: TNyxPascalSourceRef;
  const AQuery, AFilter: TNyxText; APage: Integer;
  AKind: TNyxPascalSourceSnapshotKind; const AMessage: TNyxText): INyxCard;
var
  LItems: TNyxText;
  LOwner: TNyxText;
  LInput: Integer;
  LRow: Integer;
  LCount: Integer;
  LPage: Integer;
  LMatches: array of Integer;
  LSite: TNyxPascalRoutineSite;
  LSymbol: TNyxPascalSymbolSite;
  LTotal: Integer;
  LSearch: TNyxText;
  LRowHint: TNyxText;
  LNoun: TNyxText;
  LCard: INyxCard;
  LActions: INyxRow;
  LNavigation: INyxRow;
  LTitle: TNyxText;
  LFilter: TNyxText;

  procedure AddSite(APart: TNyxPascalOutlinePart; const ACaption: TNyxText);
  var
    LButton: INyxButton;
    LLocation: TNyxPascalRoutineLocation;
    LHint: TNyxText;
  begin

    if AContent = pocDeclarations then
    begin
      LLocation := LSymbol.Location;
    end
    else
    begin
      LLocation := NyxPascalOutlineLocation(LSite, APart);
    end;
    LHint := 'No unambiguous physical location';

    if LLocation.IsDefined then
    begin
      LHint := LLocation.FileReference.Name;

      if LLocation.FileReference.Role = npsMain then
      begin
        LHint := AProject.MainUnit.Name + '.pas';
      end;
      LHint := LHint + ' / ' + IntToStr(LLocation.Line) + ':' + IntToStr(LLocation.Column);
    end;
    LButton := NewNyxButton(AID + '-go-' + IntToStr(LMatches[LRow]) + '-' + IntToStr(Ord(APart)));
    LButton.Configure.Text(ACaption).Hint(LHint).Enabled(LLocation.IsDefined).Done;
    LButton.Node.Extensions.SetValue(NyxExtension(CRowKey), NyxData(LMatches[LRow]));
    LButton.Node.Extensions.SetValue(NyxExtension(CPartKey), NyxData(Ord(APart)));
    LNavigation.Add(LButton);
  end;

begin

  if AProject = nil then
  begin
    raise ENyxPascalProject.Create('A Pascal outline requires captured project inputs');
  end;
  AOwner.Validate;

  if (Ord(AKind) < Ord(Low(TNyxPascalSourceSnapshotKind))) or
    (Ord(AKind) > Ord(High(TNyxPascalSourceSnapshotKind))) then
  begin
    raise ENyxPascalProject.Create('Unknown outline snapshot origin');
  end;

  if (AOwner.Role = npsInclude) or
    ((AIndex <> nil) and not AIndex.Owner.SameFile(AOwner)) or
    ((ASymbols <> nil) and not ASymbols.Owner.SameFile(AOwner)) then
  begin
    raise ENyxPascalProject.Create('A Pascal outline requires one matching owning unit');
  end;
  LItems := AProject.MainUnit.Name + '.pas';
  LOwner := LItems;
  for LInput := 0 to AProject.Count - 1 do
  begin

    if AProject.Input(LInput).Kind = npiUnit then
    begin
      LItems := LItems + #10 + AProject.Input(LInput).Name;
    end;
  end;

  if AOwner.Role = npsUnit then
  begin
    LOwner := AOwner.Name;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Flex(1).Done;
  LTitle := 'Routines';
  LNoun := 'routine';

  if AContent = pocDeclarations then
  begin
    LTitle := 'Declarations';
    LNoun := 'declaration';
  end;
  Result.Add(NewNyxSelect(AID + '-content').Configure.Text('Show')
    .AccessibleName('Outline content').Items('Routines' + #10 + 'Declarations')
    .Value(LTitle).Hint('Declared structure is lexical; compiler type resolution stays separate').Done);
  Result.Add(NewNyxSelect(AID + '-snapshot').Configure.Text('Source snapshot')
    .AccessibleName('Outline source snapshot').Items(CSourceCaptions[pssAccepted] + #10 +
    CSourceCaptions[pssEditing]).Value(CSourceCaptions[AKind])
    .Hint('Editing buffers are uncompiled text; accepted code stays independent').Done);
  Result.Add(NewNyxSelect(AID + '-owner').Configure.Text('Owning unit')
    .AccessibleName('Outline owning unit').Items(LItems).Value(LOwner)
    .Hint('An include is interpreted within the unit that includes it').Done);
  Result.Add(NewNyxInput(AID + '-query').Configure.Text('Find a ' + LNoun)
    .AccessibleName('Find a ' + LNoun).Placeholder('Name, scope or kind').Value(AQuery).Done);
  Result.Add(NewNyxButton(AID + '-find').Configure.Text('Find').Done);
  Result.Add(NewNyxLabel(AID + '-message').Configure.Text(AMessage)
    .Visible(AMessage <> '').Done);
  LCount := 0;
  LTotal := 0;
  LFilter := NyxFoldText(AFilter);

  if AIndex <> nil then
  begin
    LTotal := AIndex.Count;
  end;

  if ASymbols <> nil then
  begin
    LTotal := ASymbols.SymbolCount;
  end;
  SetLength(LMatches, LTotal);
  for LInput := 0 to LTotal - 1 do
  begin

    if AContent = pocDeclarations then
    begin
      LSymbol := ASymbols.SymbolAt(LInput);
      LSearch := LSymbol.QualifiedName + #10 + NyxPascalSymbolKindName(LSymbol.Kind) +
        #10 + NyxPascalSymbolVisibilityName(LSymbol.Visibility);
    end
    else
    begin
      LSite := AIndex.Item(LInput);
      LSearch := LSite.Routine.Name + #10 + LSite.Signature;
    end;

    if (LFilter = '') or (Pos(LFilter, NyxFoldText(LSearch)) > 0) then
    begin
      LMatches[LCount] := LInput;
      Inc(LCount);
    end;
  end;
  LPage := APage;

  if LPage < 0 then
  begin
    LPage := 0;
  end;

  if LPage > (LCount - 1) div NyxPascalOutlinePageSize then
  begin
    LPage := (LCount - 1) div NyxPascalOutlinePageSize;
  end;
  LTitle := IntToStr(LCount) + ' ' + LNoun + 's / ' + CSourceSummaries[AKind];

  if LCount = 1 then
  begin
    LTitle := '1 ' + LNoun + ' / ' + CSourceSummaries[AKind];
  end;
  Result.Add(NewNyxLabel(AID + '-summary').Configure.Text(LTitle).Done);
  LActions := NewNyxRow(AID + '-pages');
  LActions.Configure.Gap(8).Done;
  Result.Add(LActions);
  LActions.Add(NewNyxButton(AID + '-previous').Configure.Text('Previous')
    .Enabled(LPage > 0).Done);
  LActions.Add(NewNyxLabel(AID + '-page').Configure.Text('Page ' + IntToStr(LPage + 1)).Done);
  LActions.Add(NewNyxButton(AID + '-next').Configure.Text('Next')
    .Enabled((LPage + 1) * NyxPascalOutlinePageSize < LCount).Done);
  for LRow := LPage * NyxPascalOutlinePageSize to LCount - 1 do
  begin

    if LRow >= (LPage + 1) * NyxPascalOutlinePageSize then
    begin
      Break;
    end;

    if AContent = pocDeclarations then
    begin
      LSymbol := ASymbols.SymbolAt(LMatches[LRow]);
      LTitle := LSymbol.QualifiedName;
      LSearch := NyxPascalSymbolKindName(LSymbol.Kind) + ' / ' +
        NyxPascalSymbolVisibilityName(LSymbol.Visibility);
      LRowHint := LSearch;
    end
    else
    begin
      LSite := AIndex.Item(LMatches[LRow]);
      LTitle := LSite.Routine.Name;
      LSearch := LSite.Reason;
      LRowHint := LSite.Signature;
    end;
    LCard := NewNyxCard(AID + '-row-' + IntToStr(LMatches[LRow]));
    LCard.Configure.Padding(10).Gap(6).Done;
    Result.Add(LCard);
    LCard.Add(NewNyxLabel(AID + '-name-' + IntToStr(LMatches[LRow])).Configure
      .Text(LTitle).Hint(LRowHint).Done);
    LCard.Add(NewNyxLabel(AID + '-reason-' + IntToStr(LMatches[LRow])).Configure
      .Text(LSearch).Visible((AContent = pocDeclarations) or not LSite.Editable).Done);
    LNavigation := NewNyxRow(AID + '-sites-' + IntToStr(LMatches[LRow]));
    LNavigation.Configure.Gap(6).Done;
    LCard.Add(LNavigation);

    if AContent = pocDeclarations then
    begin
      AddSite(popDefinition, 'Declaration');
    end
    else
    begin
      AddSite(popDefinition, 'Definition');
      AddSite(popDeclaration, 'Declaration');
      AddSite(popBody, 'Body');
    end;
  end;
end;

function NewNyxPascalOutline(const AID: TNyxText; const AProject: INyxPascalProject;
  const AIndex: INyxPascalRoutineIndex; const AOwner: TNyxPascalSourceRef;
  const AQuery, AFilter: TNyxText; APage: Integer;
  AKind: TNyxPascalSourceSnapshotKind; const AMessage: TNyxText): INyxCard;
begin
  Result := BuildPascalOutline(AID, AProject, AIndex, nil, pocRoutines, AOwner,
    AQuery, AFilter, APage, AKind, AMessage);
end;

function NewNyxPascalSymbolOutline(const AID: TNyxText; const AProject: INyxPascalProject;
  const AIndex: INyxPascalSymbolIndex; const AOwner: TNyxPascalSourceRef;
  const AQuery, AFilter: TNyxText; APage: Integer;
  AKind: TNyxPascalSourceSnapshotKind; const AMessage: TNyxText): INyxCard;
begin
  Result := BuildPascalOutline(AID, AProject, nil, AIndex, pocDeclarations, AOwner,
    AQuery, AFilter, APage, AKind, AMessage);
end;

function NewNyxPascalOutline(const AID: TNyxText; const AProject: INyxPascalProject;
  const AIndex: INyxPascalRoutineIndex; const AOwner: TNyxPascalSourceRef;
  const AQuery, AFilter: TNyxText; APage: Integer; const AMessage: TNyxText): INyxCard;
begin
  Result := NewNyxPascalOutline(AID, AProject, AIndex, AOwner,
    AQuery, AFilter, APage, pssAccepted, AMessage);
end;

function NyxPascalOutlineAction(const AID: TNyxText; ANode: TNyxNode;
  ATrigger: TNyxTrigger; out ARow: Integer;
  out APart: TNyxPascalOutlinePart): TNyxPascalOutlineAction;
var
  LPart: Integer;
begin
  Result := poaNone;
  ARow := -1;
  APart := popDefinition;

  if ANode = nil then
  begin
    Exit;
  end;

  if ATrigger = ntChange then
  begin

    if ANode.ID = AID + '-content' then
    begin
      Exit(poaContent);
    end;

    if ANode.ID = AID + '-query' then
    begin
      Exit(poaField);
    end;

    if ANode.ID = AID + '-owner' then
    begin
      Exit(poaOwner);
    end;

    if ANode.ID = AID + '-snapshot' then
    begin
      Exit(poaSnapshot);
    end;
  end;

  if ATrigger <> ntClick then
  begin
    Exit;
  end;

  if ANode.ID = AID + '-find' then
  begin
    Exit(poaFind);
  end;

  if ANode.ID = AID + '-previous' then
  begin
    Exit(poaPrevious);
  end;

  if ANode.ID = AID + '-next' then
  begin
    Exit(poaNext);
  end;

  if (Pos(AID + '-go-', ANode.ID) = 1) and
    ANode.Extensions.Has(NyxExtension(CRowKey)) and
    ANode.Extensions.Has(NyxExtension(CPartKey)) then
  begin
    ARow := ANode.Extensions.Value(NyxExtension(CRowKey)).AsInteger;
    LPart := ANode.Extensions.Value(NyxExtension(CPartKey)).AsInteger;

    if (ARow < 0) or (ARow >= 4096) or (LPart < Ord(Low(TNyxPascalOutlinePart))) or
      (LPart > Ord(High(TNyxPascalOutlinePart))) then
    begin
      raise ENyxPascalProject.Create('Invalid Pascal outline navigation');
    end;
    APart := TNyxPascalOutlinePart(LPart);
    Result := poaNavigate;
  end;
end;

end.
