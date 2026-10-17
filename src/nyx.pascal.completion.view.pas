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

unit nyx.pascal.completion.view;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.types, nyx.controls, nyx.model, nyx.editing.history,
  nyx.editing, nyx.pascal.sources, nyx.pascal.drafts, nyx.pascal.completion;

type
  { Closed presentation actions. Request/page actions delegate asynchronous work;
    Accept prepares a normal buffer edit, never accepted source publication. }
  TNyxPascalCompletionAction = (pcaNone, pcaRequest, pcaSelect, pcaAccept,
    pcaDismiss, pcaPrevious, pcaNext, pcaUndo, pcaRedo);

  { Runtime view state, deliberately independent of a provider's transport/job.
    A ready empty page is distinct from a missing or failed provider. }
  TNyxPascalCompletionViewState = (pcvClosed, pcvPending, pcvReady,
    pcvUnavailable, pcvFailed, pcvCancelled);

  { Opaque UI-lifetime reply ticket. Only this menu's current request may publish;
    equal source/caret queries cannot substitute an older out-of-order reply.
    No wire codec, pointer, session or source ownership is retained. }
  TNyxPascalCompletionTicket = record
  private
    FOwner: Integer;
    FSerial: Integer;
  end;

  { One UI-thread-owned completion view beside one physical buffer/history.
    It owns immutable query/result data and a managed history, never a widget,
    renderer, application or document. Fresh source arguments and history
    generation seal every presentation/accept action. Hosts still validate their
    physical mount and synchronize the returned proposed buffer normally.
    Build returns an ordinary Nyx card, embeddable in a popover or inline host. }
  INyxPascalCompletionMenu = interface(IInterface)
    ['{F45DC5F0-1679-4905-BBB9-FF012CD6A6C2}']
    function BeginQuery(const ASources: INyxPascalSourceSnapshot;
      const AFile, AOwner: TNyxPascalSourceRef;
      out ATicket: TNyxPascalCompletionTicket;
      AOffset: Integer = 0; ALimit: Integer = 12): INyxPascalCompletionRequest;
    { False ignores a foreign/retired ticket; a stale matching request closes.
      Provider data is independently frozen before any visible page changes. }
    function Present(const ATicket: TNyxPascalCompletionTicket;
      const AResult: INyxPascalCompletionResult;
      const ACurrentSources: INyxPascalSourceSnapshot): Boolean;
    procedure Dismiss;
    function State: TNyxPascalCompletionViewState;
    function Selected: Integer;
    function MessageText: TNyxText;
    function Build(const AID: TNyxText;
      const ACurrentSources: INyxPascalSourceSnapshot): INyxCard;
    function Key(const AKey: TNyxKeyStroke;
      const ACurrentSources: INyxPascalSourceSnapshot): TNyxPascalCompletionAction;
    { Check the node's exact presence in the supplied current mount and this
      view's row ticket; nodes from a retired tree never gain action authority. }
    function Route(const AID: TNyxText; ARoot, ANode: TNyxNode;
      ATrigger: TNyxTrigger;
      const ACurrentSources: INyxPascalSourceSnapshot): TNyxPascalCompletionAction;
    { Stage the complete source snapshot before changing history. One exact
      replacement is one normal text Undo entry; accepted source remains exact.
      The caller renders History.Current and sends its normal draft notification. }
    function Accept(const ACurrentSources: INyxPascalSourceSnapshot): INyxPascalSourceSnapshot;
  end;

function NewNyxPascalCompletionMenu(
  const AHistory: INyxTextEditHistory): INyxPascalCompletionMenu;

implementation

uses SysUtils, nyx.data, nyx.pascal.project, nyx.interaction;

type
  { No callback back-reference is kept. The menu's serial and history generation
    revoke detached rows/replies, while the history owns only text values. }
  TCompletionMenu = class(TInterfacedObject, INyxPascalCompletionMenu)
  private
    FIdentity: Integer;
    FSerial: Integer;
    FGeneration: Integer;
    FHistory: INyxTextEditHistory;
    FFrame: TNyxTextEditorFrame;
    FRequest: INyxPascalCompletionRequest;
    FResult: INyxPascalCompletionResult;
    FState: TNyxPascalCompletionViewState;
    FSelected: Integer;
    function Current(const ASources: INyxPascalSourceSnapshot): Boolean;
  public
    function BeginQuery(const ASources: INyxPascalSourceSnapshot;
      const AFile, AOwner: TNyxPascalSourceRef;
      out ATicket: TNyxPascalCompletionTicket;
      AOffset: Integer = 0; ALimit: Integer = 12): INyxPascalCompletionRequest;
    function Present(const ATicket: TNyxPascalCompletionTicket;
      const AResult: INyxPascalCompletionResult;
      const ACurrentSources: INyxPascalSourceSnapshot): Boolean;
    procedure Dismiss;
    function State: TNyxPascalCompletionViewState;
    function Selected: Integer;
    function MessageText: TNyxText;
    function Build(const AID: TNyxText;
      const ACurrentSources: INyxPascalSourceSnapshot): INyxCard;
    function Key(const AKey: TNyxKeyStroke;
      const ACurrentSources: INyxPascalSourceSnapshot): TNyxPascalCompletionAction;
    function Route(const AID: TNyxText; ARoot, ANode: TNyxNode;
      ATrigger: TNyxTrigger;
      const ACurrentSources: INyxPascalSourceSnapshot): TNyxPascalCompletionAction;
    function Accept(const ACurrentSources: INyxPascalSourceSnapshot): INyxPascalSourceSnapshot;
  end;

const
  CTicketKey = 'nyx.pascal.completion.ticket';
  CActionKey = 'nyx.pascal.completion.action';
  CRowKey = 'nyx.pascal.completion.row';
  CKindCaptions: array[TNyxPascalCompletionKind] of TNyxText =
    ('Symbol', 'Type', 'Method', 'Property', 'Field', 'Variable', 'Constant',
      'Parameter', 'Unit', 'Keyword');

var
  GNextIdentity: Integer;

function TCompletionMenu.Current(const ASources: INyxPascalSourceSnapshot): Boolean;
begin
  Result := (FRequest <> nil) and not FHistory.Composing and
    (FGeneration = FHistory.Generation);

  if Result then
  begin
    Result := FFrame.SameValue(FHistory.Current) and FRequest.Matches(ASources);
  end;
end;

function TCompletionMenu.BeginQuery(const ASources: INyxPascalSourceSnapshot;
  const AFile, AOwner: TNyxPascalSourceRef;
  out ATicket: TNyxPascalCompletionTicket; AOffset, ALimit: Integer): INyxPascalCompletionRequest;
var
  LFrame: TNyxTextEditorFrame;
  LRequest: INyxPascalCompletionRequest;
begin
  ATicket := Default(TNyxPascalCompletionTicket);
  LFrame := FHistory.Current;

  if FHistory.Composing or not LFrame.Selection.Defined or not LFrame.Selection.Collapsed then
  begin
    raise ENyxPascalProject.Create('Completion needs a known collapsed caret outside composition');
  end;
  LRequest := CaptureNyxPascalCompletionRequest(NewNyxPascalCompletionRequest(ASources)
    .At(AFile, LFrame.Selection.Start).From(AOwner).Skip(AOffset).Take(ALimit));

  if LRequest.Sources.Text(AFile) <> LFrame.Text then
  begin
    raise ENyxPascalProject.Create('Completion buffer does not match the current source snapshot');
  end;

  if FSerial = High(Integer) then
  begin
    raise ENyxPascalProject.Create('Completion view request generation exhausted');
  end;
  Inc(FSerial);
  FRequest := LRequest;
  FFrame := LFrame;
  FGeneration := FHistory.Generation;
  FResult := nil;
  FState := pcvPending;
  FSelected := -1;
  ATicket.FOwner := FIdentity;
  ATicket.FSerial := FSerial;
  Result := LRequest;
end;

procedure TCompletionMenu.Dismiss;
begin
  FRequest := nil;
  FResult := nil;
  FState := pcvClosed;
  FSelected := -1;
end;

function TCompletionMenu.State: TNyxPascalCompletionViewState;
begin
  Result := FState;
end;

function TCompletionMenu.Selected: Integer;
begin
  Result := FSelected;
end;

function TCompletionMenu.Present(const ATicket: TNyxPascalCompletionTicket;
  const AResult: INyxPascalCompletionResult;
  const ACurrentSources: INyxPascalSourceSnapshot): Boolean;
var
  LQuery: INyxPascalCompletionRequest;
  LRows: array of TNyxPascalCompletionItem;
  LChecked: INyxPascalCompletionResult;
  LAt: Integer;
  LCount: Integer;
begin
  Result := False;

  if (ATicket.FOwner <> FIdentity) or (ATicket.FSerial <> FSerial) or
    (FRequest = nil) or (FState <> pcvPending) then
  begin
    Exit;
  end;

  if not Current(ACurrentSources) then
  begin
    Dismiss;
    Exit;
  end;

  if AResult = nil then
  begin
    raise ENyxPascalProject.Create('Completion view needs a provider result');
  end;
  LQuery := CaptureNyxPascalCompletionRequest(AResult.Request);

  if not FRequest.Matches(LQuery.Sources) or
    not FRequest.FileReference.SameFile(LQuery.FileReference) or
    not FRequest.Owner.SameFile(LQuery.Owner) or
    (FRequest.Caret <> LQuery.Caret) or (FRequest.Offset <> LQuery.Offset) or
    (FRequest.Limit <> LQuery.Limit) then
  begin
    raise ENyxPascalProject.Create('Completion view received a different query');
  end;
  LCount := AResult.Count;

  if (LCount < 0) or (LCount > FRequest.Limit) then
  begin
    raise ENyxPascalProject.Create('Completion view exceeds its row budget');
  end;
  SetLength(LRows, LCount);
  for LAt := 0 to High(LRows) do
  begin
    LRows[LAt] := AResult.Item(LAt);
  end;
  LChecked := NewNyxPascalCompletionResult(FRequest, AResult.Status,
    AResult.MessageText, AResult.ReplaceStart, AResult.ExpectedText, LRows, AResult.Total);
  FResult := LChecked;
  case LChecked.Status of
    pcsReady: FState := pcvReady;
    pcsUnavailable: FState := pcvUnavailable;
    pcsFailed: FState := pcvFailed;
    pcsCancelled: FState := pcvCancelled;
  end;

  if LChecked.Count > 0 then
  begin
    FSelected := 0;
  end;
  Result := True;
end;

function TCompletionMenu.MessageText: TNyxText;
begin
  Result := '';

  if FState = pcvPending then
  begin
    Exit('Finding suggestions...');
  end;

  if FResult = nil then
  begin
    Exit;
  end;
  Result := FResult.MessageText;

  if Result <> '' then
  begin
    Exit;
  end;

  if FState = pcvReady then
  begin

    if FResult.Count = 0 then
    begin
      Exit('No suggestions in this context');
    end;
    Result := IntToStr(FResult.Count) + ' of ' + IntToStr(FResult.Total) + ' suggestions';
  end;
  case FState of
    pcvUnavailable: Result := 'Completion is not configured';
    pcvFailed: Result := 'Completion could not finish';
    pcvCancelled: Result := 'Completion cancelled';
  else
    { Closed/pending/ready states were handled above; retain that message. }
  end;
end;

function TCompletionMenu.Build(const AID: TNyxText;
  const ACurrentSources: INyxPascalSourceSnapshot): INyxCard;
var
  LHeader: INyxRow;
  LRows: INyxScroll;
  LRow: INyxColumn;
  LLine: INyxRow;
  LButton: INyxButton;
  LAt: Integer;
  LID: TNyxText;

  procedure Action(const AButton: INyxButton; AAction: TNyxPascalCompletionAction; ARow: Integer = -1);
  begin
    AButton.Node.Extensions.SetValue(NyxExtension(CTicketKey), NyxData(FSerial));
    AButton.Node.Extensions.SetValue(NyxExtension(CActionKey), NyxData(Ord(AAction)));
    AButton.Node.Extensions.SetValue(NyxExtension(CRowKey), NyxData(ARow));
  end;
begin

  if (AID = '') or (NyxTextScalarCount(AID) > 80) then
  begin
    raise ENyxPascalProject.Create('A completion view needs a bounded nonempty control identity');
  end;

  if (FState <> pcvClosed) and not Current(ACurrentSources) then
  begin
    Dismiss;
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Visible(FState <> pcvClosed).Padding(12).Gap(8)
    .AccessibleName('Pascal completion suggestions').Done;
  Result.Node.Extensions.SetValue(NyxExtension(CTicketKey), NyxData(FSerial));

  if FState = pcvClosed then
  begin
    Exit;
  end;
  LHeader := NewNyxRow(AID + '-header');
  LHeader.Configure.Gap(8).Done;
  LHeader.Add(NewNyxLabel(AID + '-title').WithText('Code completion').Configure.Flex(1).Done);
  LButton := NewNyxButton(AID + '-dismiss').WithText('Close');
  LButton.Configure.AccessibleName('Close completion suggestions').Done;
  Action(LButton, pcaDismiss);
  LHeader.Add(LButton);
  Result.Add(LHeader);
  Result.Add(NewNyxLabel(AID + '-status').WithText(MessageText));

  if (FResult = nil) or (FState <> pcvReady) then
  begin
    Exit;
  end;
  LRows := NewNyxScroll(AID + '-rows');
  LRows.Configure.Layout(nlColumn).Gap(6).MaximumHeight(320).Done;
  Result.Add(LRows);
  for LAt := 0 to FResult.Count - 1 do
  begin
    LID := AID + '-row-' + IntToStr(LAt);
    LRow := NewNyxColumn(LID);
    LRow.Configure.Gap(4).Padding(4).Done;
    LLine := NewNyxRow(LID + '-heading');
    LLine.Configure.Gap(6).Done;
    LButton := NewNyxButton(LID + '-choose').WithText(FResult.Item(LAt).Name);
    LButton.Configure.Flex(1).Pressed(LAt = FSelected).Hint(FResult.Item(LAt).Detail).Done;

    if LAt = FSelected then
    begin
      LButton.Configure.Variant(nvPrimary).Done;
    end;
    Action(LButton, pcaAccept, LAt);
    LLine.Add(LButton);
    LLine.Add(NewNyxBadge(LID + '-kind').WithText(CKindCaptions[FResult.Item(LAt).Kind]));
    LRow.Add(LLine);
    LRow.Add(NewNyxLabel(LID + '-detail').WithText(FResult.Item(LAt).Detail));
    LRows.Add(LRow);
  end;
  LHeader := NewNyxRow(AID + '-pages');
  LHeader.Configure.Gap(6).Done;
  LButton := NewNyxButton(AID + '-previous').WithText('Previous');
  LButton.Configure.Enabled(FRequest.Offset > 0).Done;
  Action(LButton, pcaPrevious);
  LHeader.Add(LButton);
  LButton := NewNyxButton(AID + '-next').WithText('More suggestions');
  LButton.Configure.Enabled(FRequest.Offset + FResult.Count < FResult.Total).Done;
  Action(LButton, pcaNext);
  LHeader.Add(LButton);
  Result.Add(LHeader);
end;

function TCompletionMenu.Key(const AKey: TNyxKeyStroke;
  const ACurrentSources: INyxPascalSourceSnapshot): TNyxPascalCompletionAction;
begin
  Result := pcaNone;

  if FHistory.Composing then
  begin
    Exit;
  end;

  if AKey.Matches(nkSpaceKey, [nmControl]) then
  begin
    Exit(pcaRequest);
  end;

  if AKey.Matches(nkZKey, [nmControl], True) or AKey.Matches(nkZKey, [nmMeta], True) then
  begin
    { The caller owns this buffer's timeline. Even an empty Undo command must
      consume the platform default, whose separate native stack may be stale. }
    Dismiss;
    Exit(pcaUndo);
  end;

  if AKey.Matches(nkZKey, [nmControl, nmShift], True) or
    AKey.Matches(nkZKey, [nmMeta, nmShift], True) or AKey.Matches(nkYKey, [nmControl], True) then
  begin
    Dismiss;
    Exit(pcaRedo);
  end;

  if not Current(ACurrentSources) then
  begin
    Dismiss;
    Exit;
  end;

  if AKey.Matches(nkEscapeKey) then
  begin
    Dismiss;
    Exit(pcaDismiss);
  end;

  if (FState <> pcvReady) or (FResult = nil) or (FResult.Count = 0) then
  begin
    Exit;
  end;

  if AKey.Matches(nkDownKey, [], True) then
  begin
    FSelected := (FSelected + 1) mod FResult.Count;
    Exit(pcaSelect);
  end;

  if AKey.Matches(nkUpKey, [], True) then
  begin
    FSelected := (FSelected + FResult.Count - 1) mod FResult.Count;
    Exit(pcaSelect);
  end;

  if AKey.Matches(nkEnterKey) or AKey.Matches(nkTabKey) then
  begin
    Result := pcaAccept;
  end;
end;

function TCompletionMenu.Route(const AID: TNyxText; ARoot, ANode: TNyxNode;
  ATrigger: TNyxTrigger;
  const ACurrentSources: INyxPascalSourceSnapshot): TNyxPascalCompletionAction;
var
  LCard: TNyxNode;
  LParent: TNyxNode;
  LAction: Integer;
  LRow: Integer;
begin
  Result := pcaNone;

  if (ARoot = nil) or (ANode = nil) or (ATrigger <> ntClick) or
    not NyxInteractionPolicy(ANode).CanIssueCommand or
    (ARoot.Find(ANode.ID) <> ANode) or not Current(ACurrentSources) then
  begin
    Exit;
  end;
  LCard := ARoot.Find(AID);
  LParent := ANode;
  while (LParent <> nil) and (LParent <> LCard) do
  begin
    LParent := LParent.Parent;
  end;

  if (LCard = nil) or (LParent <> LCard) or
    not ANode.Extensions.Has(NyxExtension(CTicketKey)) or
    not ANode.Extensions.Has(NyxExtension(CActionKey)) or
    not ANode.Extensions.Has(NyxExtension(CRowKey)) or
    (ANode.Extensions.Value(NyxExtension(CTicketKey)).AsInteger <> FSerial) then
  begin
    Exit;
  end;
  LAction := ANode.Extensions.Value(NyxExtension(CActionKey)).AsInteger;
  LRow := ANode.Extensions.Value(NyxExtension(CRowKey)).AsInteger;

  if not (LAction in [Ord(pcaAccept), Ord(pcaDismiss), Ord(pcaPrevious), Ord(pcaNext)]) then
  begin
    Exit;
  end;
  Result := TNyxPascalCompletionAction(LAction);

  if Result = pcaAccept then
  begin

    if not NyxInteractionPolicy(ANode).CanEditValue then
    begin
      Exit(pcaNone);
    end;

    if (FResult = nil) or (FState <> pcvReady) or (LRow < 0) or (LRow >= FResult.Count) then
    begin
      Exit(pcaNone);
    end;
    FSelected := LRow;
  end;

  if Result = pcaDismiss then
  begin
    Dismiss;
  end;
end;

function TCompletionMenu.Accept(
  const ACurrentSources: INyxPascalSourceSnapshot): INyxPascalSourceSnapshot;
var
  LCandidate: INyxPascalSourceSnapshot;
  LCaret: Integer;
begin

  if not Current(ACurrentSources) or (FState <> pcvReady) or
    (FResult = nil) or (FSelected < 0) then
  begin
    raise ENyxPascalProject.Create('Completion suggestion is no longer current');
  end;
  LCandidate := FResult.Preview(FSelected, ACurrentSources, LCaret);
  FHistory.Replace(FFrame, FResult.ReplaceStart, FResult.ExpectedText,
    FResult.Item(FSelected).InsertText);
  Result := LCandidate;
  Dismiss;
end;

function NewNyxPascalCompletionMenu(
  const AHistory: INyxTextEditHistory): INyxPascalCompletionMenu;
var
  LMenu: TCompletionMenu;
begin

  if (AHistory = nil) or (GNextIdentity = High(Integer)) then
  begin
    raise ENyxPascalProject.Create('Completion view needs an owned text history and available identity');
  end;
  AHistory.Current.Validate;
  Inc(GNextIdentity);
  LMenu := TCompletionMenu.Create;
  Result := LMenu;
  LMenu.FIdentity := GNextIdentity;
  LMenu.FHistory := AHistory;
  LMenu.FSelected := -1;
end;

end.
