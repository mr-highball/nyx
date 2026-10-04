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

unit nyx.studio.session;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  Classes,
  nyx.text,
  nyx.data,
  nyx.types,
  SysUtils,
  nyx.model,
  nyx.state,
  nyx.binding.types,
  nyx.collections,
  nyx.collections.view.types,
  nyx.codec,
  nyx.catalog,
  nyx.controls,
  nyx.codegen,
  nyx.source,
  nyx.studio.history,
  nyx.studio.projects,
  nyx.studio.edits,
  nyx.studio.rootedits,
  nyx.callbacks,
  nyx.scheduler,
  nyx.sample;

type
  { Closed command destination; extension names and values remain typed data. }
  TNyxStudioExtensionOwner = (seoDocument, seoSelection);

  { Portable designer state, independent of Studio's browser/native shell.
    Commands operate on the same owned document applications consume. Snapshots
    provide deterministic reversible history and never retain renderer handles.
    SelectedID and ActiveViewID are stable identities, not borrowed node pointers,
    because undo/load can replace the entire tree. }
  TNyxStudioSession = class
  private
    FDocument: TNyxDocument;
    FCatalog: TNyxCatalog;
    FUndo: TNyxStudioHistory;
    FRedo: TNyxStudioHistory;
    FSourceWorkspace: TNyxSourceWorkspace;
    FSourceDraft: TNyxText;
    FSourceDraftBase: TNyxText;
    FSourceDraftPending: Boolean;
    FSourceDiagnostic: TNyxSourceDiagnostic;
    FSourceDiagnosticSource: TNyxText;
    FSelectedID: TNyxText;
    FActiveViewID: TNyxText;
    FNextID: Integer;
    function GetSourceDiagnostic: TNyxSourceDiagnostic;
    procedure DoApplySourceDraft;
    function NewID(const AKind: TNyxText): TNyxText;
    procedure Checkpoint;
    procedure TrimUndoHistory;
    procedure Commit;
    procedure Rollback;
    procedure Restore(const ASource: TNyxText);
    { Decode/validate a complete checkpoint before swapping either accepted
      owner. Optional ARemember receives the freshly synchronized current pair
      after target admission and before publication; failures retain both owners
      and history. Rollback deliberately omits capture of an invalid candidate. }
    procedure RestorePair(const ACheckpoint: TNyxSourceCheckpoint;
      ARemember: TNyxStudioHistory = nil);
    procedure Reidentify(ANode: TNyxNode);
    { All content insertion uses one destination contract, including reusable
      payloads inside instance layout overrides. InsertNode consumes its newly
      constructed candidate on success or failure; admission/history own cleanup. }
    function InsertionParent: TNyxNode;
    procedure InsertNode(ANode: TNyxNode);
    { Admit/no-op-check a detached document before checkpoint/publication. On
      success ownership transfers and the argument becomes nil; otherwise the
      caller still owns it. Accepted nodes, selection and redo remain untouched. }
    procedure PublishCandidate(var ACandidate: TNyxDocument); overload;
    procedure PublishCandidate(var ACandidate: TNyxDocument;
      const ARenames: array of TNyxSourceStateRename); overload;
    { Transfers an already-admitted design/workspace pair with one checkpoint.
      Callers retain both owners if checkpointing fails; success clears them. }
    procedure PublishPair(var ACandidate: TNyxDocument;
      var AWorkspace: TNyxSourceWorkspace);
    function CallbackCandidate: TNyxDocument;
    function DoAddCallback(ATrigger: TNyxTrigger; const AName: TNyxEventRef;
      out ALine: Integer): TNyxHandlerRef;
    procedure DoSetCallbackPolicy(ATrigger: TNyxTrigger; const AName: TNyxEventRef;
      APolicy: TNyxExecutionPolicy);
    procedure DoRemoveCallback(ATrigger: TNyxTrigger; const AName: TNyxEventRef;
      const AID: TNyxCallbackRef);
  public
    constructor Create;
    destructor Destroy; override;
    function Selected: TNyxNode;
    function ActiveView: TNyxNode;
    procedure Select(const AID: TNyxText);
    procedure Activate(const AID: TNyxText);
    procedure SetProperty(const AKey, AValue: TNyxText);
    { Append an independent descriptor clone through the ordinary undoable
      insertion command. The reference-counted control is borrowed; the caller
      retains its implementation/lifetime and the session owns only the clone. }
    procedure AddControl(const AControl: INyxControl);
    { Persist a design-canvas field through one undoable command. The borrowed
      realized node must come from this document's current view. Ordinary fields
      edit their authored owner; inherited reusable fields create/update an
      instance-only named-part override, leaving the definition untouched. }
    procedure SetCanvasValue(ARuntimeNode: TNyxNode);
    { Project naming is an undoable document edit, independent of output choices
      and machine tooling. The title is ordinary user text in the portable file. }
    procedure SetTitle(const ATitle: TNyxText);
    { One undoable authored-default edit. Typed assignments and full document
      admission run on a detached clone before history/publication; rejection and
      no-ops preserve the accepted document, selection and redo stack. }
    procedure SetStateValues(const AValues: array of TNyxStateAssignment);
    { Undoable opaque project/control data edits. Scope protects standard fields;
      complete detached admission precedes history. No-ops and rejected edits
      retain document handles and redo. Selection denotes the authored owner. }
    procedure SetExtension(AOwner: TNyxStudioExtensionOwner;
      const AKey: TNyxExtensionRef; const AValue: TNyxDataValue);
    procedure RemoveExtension(AOwner: TNyxStudioExtensionOwner;
      const AKey: TNyxExtensionRef);
    { Open keys and tagged values are the explicit designer input boundary.
      Creation refuses duplicates; rename retains type/order and updates every
      authored binding reference in one command. Removal refuses used defaults. }
    procedure CreateState(const AName: TNyxText; const AValue: TNyxStateValue);
    procedure RenameState(const AOldName, ANewName: TNyxText);
    procedure RemoveState(const AName: TNyxText);
    { Binding metadata edits apply only to the selected authored owner/override.
      Inherit removes the local descriptor; Clear records deliberate unbinding.
      Unsupported targets/defaults fail on a detached complete document. }
    procedure SetBinding(const ASpec: TNyxBindingSpec);
    procedure InheritBinding(ATarget: TNyxBindingProperty);
    { Complete typed collection definitions and view edits are detached, admitted
      commands. Removing a used definition rejects; Clear and Inherit preserve
      their distinct reusable override meanings in paired source/history. }
    procedure DefineCollection(const AKey: TNyxCollectionRef;
      const ASchema: TNyxCollectionSchema; const AItems: array of TNyxCollectionItem);
    procedure RemoveCollection(const AKey: TNyxCollectionRef);
    procedure SetCollectionView(const ASpec: TNyxCollectionViewSpec);
    procedure InheritCollectionView;
    { Event edits preserve exact registration identity/order and inheritance on
      detached candidates. Adding a handler is one paired design/source history
      command and returns its TODO line. Removal retains application code. }
    function AddCallback(ATrigger: TNyxTrigger; out ALine: Integer): TNyxHandlerRef; overload;
    function AddCallback(const AName: TNyxEventRef; out ALine: Integer): TNyxHandlerRef; overload;
    procedure SetCallbackPolicy(ATrigger: TNyxTrigger; APolicy: TNyxExecutionPolicy); overload;
    procedure SetCallbackPolicy(const AName: TNyxEventRef; APolicy: TNyxExecutionPolicy); overload;
    procedure RemoveCallback(ATrigger: TNyxTrigger; const AID: TNyxCallbackRef); overload;
    procedure RemoveCallback(const AName: TNyxEventRef; const AID: TNyxCallbackRef); overload;
    { Reorder an exact selected-owner registration through the same detached,
      source-preserving command as inspector policy/removal. Positions are
      zero-based final order; inherited metadata is materialized independently. }
    procedure MoveCallback(ATrigger: TNyxTrigger;
      const AID: TNyxCallbackRef; AIndex: Integer); overload;
    procedure MoveCallback(const AName: TNyxEventRef;
      const AID: TNyxCallbackRef; AIndex: Integer); overload;
    function CallbackLine(const AHandler: TNyxHandlerRef): Integer;
    { Caller owns this independent realized selection (or customized part),
      projected against saved defaults. Removed part rules return nil. }
    function SelectedProjection: TNyxNode;
    procedure AddKind(const AKind: TNyxText);
    procedure DeleteSelected;
    procedure DuplicateSelected;
    procedure MoveSelected(ADirection: Integer);
    procedure AddPage;
    procedure CreateComponent;
    procedure AddComponentInstance(const ADefinitionID: TNyxText);
    { Select an independently owned customization of a named reusable part.
      The ordinary property inspector edits it. Adding through the palette then
      appends content when that part is a container, through the public model API. }
    procedure CustomizePart(const APath: TNyxText);
    procedure Undo;
    procedure Redo;
    procedure Load(const ASource: TNyxText);
    function Save: TNyxText;
    function Source: TNyxText;
    { Snapshot pairs accepted files and the exact pending draft/base. Load stages
      the whole pair before one publication and deliberately starts new history. }
    function ProjectSnapshot: TNyxProjectPair;
    procedure LoadProject(const APair: TNyxProjectPair;
      AResolution: TNyxProjectResolution = nprRequireMatch);
    { Shared semantic command boundary for MCP and other controllers. All
      related edits stage a complete candidate and publish once with one paired
      undo checkpoint. Pending source drafts block external design mutation. }
    procedure ApplyPatch(const APatch: INyxDesignPatch);
    { Apply an immutable reviewed root group through one paired Undo command.
      Stale reviews, dangling reusable references and pending drafts retain all
      owners/history. Imports/helpers and document state remain deliberate. }
    procedure RemoveRoots(const AReview: INyxRootRemoval);
    { Admit an editor's exact paired files through ordinary history. Used by the
      collaboration service; draft-only changes do not add content undo entries.
      Unlike LoadProject, this never resets an existing session's history. }
    procedure AdoptProject(const APair: TNyxProjectPair);
    function CanUndo: Boolean;
    function CanRedo: Boolean;
    { Source is accepted Pascal; DraftSource is the editable buffer. Applying a
      draft stages both model and companion source before one history entry.
      Unsupported/invalid edits retain the buffer, document and redo history. }
    function DraftSource: TNyxText;
    procedure SetSourceDraft(const ASource: TNyxText);
    { Recovery retains the draft's original accepted source, so reloading cannot
      turn a stale draft into an edit of a newer visual design. }
    procedure RestoreSourceDraft(const ASource, ABase: TNyxText);
    procedure ApplySourceDraft;
    procedure DiscardSourceDraft;
    { Companion source has independent local recovery. Import first loads the
      portable design, then applies its paired Pascal. .nyx export stays portable. }
    property Document: TNyxDocument read FDocument;
    property Catalog: TNyxCatalog read FCatalog;
    property SelectedID: TNyxText read FSelectedID;
    property ActiveViewID: TNyxText read FActiveViewID;
    property SourceDraftBase: TNyxText read FSourceDraftBase;
    { Read-only presentation of the most recent failed Apply. It is valid only
      for the exact current draft, is never saved as design/history data, and
      clears on editing/restoring/success. Exceptions still propagate normally. }
    property SourceDiagnostic: TNyxSourceDiagnostic read GetSourceDiagnostic;
  end;

implementation

uses
  nyx.schema,
  nyx.binding,
  nyx.studio.authoring,
  nyx.composition;

constructor TNyxStudioSession.Create;
begin
  inherited Create;
  FDocument := CreateNyxSample;
  FCatalog := TNyxCatalog.Create;
  FUndo := TNyxStudioHistory.Create;
  FRedo := TNyxStudioHistory.Create;
  FSourceWorkspace := TNyxSourceWorkspace.Create;
  FActiveViewID := FDocument.Pages[0].ID;
  FSelectedID := FActiveViewID;
end;

destructor TNyxStudioSession.Destroy;
begin
  FSourceWorkspace.Free;
  FRedo.Free;
  FUndo.Free;
  FCatalog.Free;
  FDocument.Free;
  inherited Destroy;
end;

function TNyxStudioSession.NewID(const AKind: TNyxText): TNyxText;
begin
  repeat
    Inc(FNextID);
    Result := AKind + '-' + IntToStr(FNextID);
  until FDocument.Find(Result) = nil;
end;

procedure TNyxStudioSession.Checkpoint;
  {$ifdef NYX_SOURCE_PROFILE}
var
  LStarted: Double;
  {$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  { Capture encodes the current public document once, synchronizes authored
    source, then stores the complete immutable pair in one history entry. }
  FUndo.Add(FSourceWorkspace.Capture(FDocument));
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spCheckpoint, LStarted);{$endif}
end;

procedure TNyxStudioSession.TrimUndoHistory;
begin
  { Keep the latest fifty commands within a 16 MiB retained-text budget. The
    immutable checkpoint carries its paired design once and accounting uses real
    target text bytes, without JSON escaping or a second design history list.
    Retain one oversized entry so the immediately previous command stays undoable. }
  while FUndo.Count > 50 do
  begin
    FUndo.Delete(0);
  end;
  while (FUndo.StorageBytes > 16 * 1024 * 1024) and (FUndo.Count > 1) do
  begin
    FUndo.Delete(0);
  end;
end;

procedure TNyxStudioSession.Rollback;
begin
  RestorePair(FUndo.Last);
  FUndo.Delete(FUndo.Count - 1);
end;

procedure TNyxStudioSession.Commit;
{$ifdef NYX_SOURCE_PROFILE}
var
  LStarted: Double;
{$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  { Admit the entire candidate after a command. Invalid references/cycles restore
    the accepted snapshot, including the old redo history, before surfacing the
    diagnostic. The next command always starts from a valid document. }
  try
    ValidateNyxDocumentProperties(FDocument);
    FSourceWorkspace.Render(FDocument);
  except
    Rollback;
    raise;
  end;
  FRedo.Clear;
  TrimUndoHistory;
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spCommit, LStarted);{$endif}
end;

procedure TNyxStudioSession.Restore(const ASource: TNyxText);
var
  LCandidate: TNyxDocument;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  {$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  LCandidate := TNyxCodec.Decode(ASource);
  try
    ValidateNyxDocumentProperties(LCandidate);
  except
    LCandidate.Free;
    raise;
  end;
  FDocument.Free;
  FDocument := LCandidate;

  if FDocument.Find(FActiveViewID) = nil then
  begin
    FActiveViewID := '';

    if FDocument.Count > 0 then
    begin
      FActiveViewID := FDocument.Pages[0].ID;
    end
    else if FDocument.ComponentCount > 0 then
    begin
      FActiveViewID := FDocument.Components[0].ID;
    end;
  end;

  if FDocument.Find(FSelectedID) = nil then
  begin
    FSelectedID := FActiveViewID;
  end;
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spRestoreDocument, LStarted);{$endif}
end;

function TNyxStudioSession.Selected: TNyxNode;
begin
  Result := FDocument.Find(FSelectedID);
end;

procedure TNyxStudioSession.RestorePair(const ACheckpoint: TNyxSourceCheckpoint;
  ARemember: TNyxStudioHistory);
var
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPrevious: TNyxDocument;
  LPreviousSource: TNyxSourceWorkspace;
  {$ifdef NYX_SOURCE_PROFILE}
  LStarted: Double;
  {$endif}
begin
  LCandidate := nil;
  LWorkspace := nil;
  try
    {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
    LCandidate := TNyxCodec.Decode(ACheckpoint.Design);
    ValidateNyxDocumentProperties(LCandidate);
    {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spRestoreDocument, LStarted);{$endif}
    LWorkspace := TNyxSourceWorkspace.Create;
    LWorkspace.Restore(ACheckpoint);

    if ARemember <> nil then
    begin
      { Current public nodes may have changed since the previous command. Never
        remember a stale frame; a failed reconciliation leaves the target pair
        unpublished and the history entry unrecorded. }
      ARemember.Add(FSourceWorkspace.Capture(FDocument));
    end;
    LPrevious := FDocument;
    LPreviousSource := FSourceWorkspace;
    FDocument := LCandidate;
    LCandidate := nil;
    FSourceWorkspace := LWorkspace;
    LWorkspace := nil;
    LPrevious.Free;
    LPreviousSource.Free;

    if FDocument.Find(FActiveViewID) = nil then
    begin
      FActiveViewID := '';

      if FDocument.Count > 0 then
      begin
        FActiveViewID := FDocument.Pages[0].ID;
      end
      else if FDocument.ComponentCount > 0 then
      begin
        FActiveViewID := FDocument.Components[0].ID;
      end;
    end;

    if FDocument.Find(FSelectedID) = nil then
    begin
      FSelectedID := FActiveViewID;
    end;
  finally
    LWorkspace.Free;
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.ActiveView: TNyxNode;
begin
  Result := FDocument.Find(FActiveViewID);
end;

procedure TNyxStudioSession.Select(const AID: TNyxText);
begin

  if FDocument.Find(AID) = nil then
  begin
    raise ENyxModel.Create('Selection is missing');
  end;
  FSelectedID := AID;
end;

procedure TNyxStudioSession.Activate(const AID: TNyxText);
begin

  if FDocument.Find(AID) = nil then
  begin
    raise ENyxModel.Create('View is missing');
  end;
  FActiveViewID := AID;
  FSelectedID := AID;
end;

procedure TNyxStudioSession.SetProperty(const AKey, AValue: TNyxText);
begin

  if Selected = nil then
  begin
    raise ENyxModel.Create('Select a component first');
  end;
  Checkpoint;
  try
    Selected.SetProp(AKey, AValue);
  except
    Rollback;
    raise;
  end;
  Commit;
end;

procedure TNyxStudioSession.SetCanvasValue(ARuntimeNode: TNyxNode);
var
  LBinding: TNyxBindingSpec;
  LStateValue: TNyxStateValue;
  LOwner: TNyxNode;
  LTarget: TNyxNode;
  LPart: TNyxNode;
  LPath: TNyxText;
  LValue: TNyxText;
  LCount: Integer;
  LIndex: Integer;
begin

  if (ARuntimeNode = nil) or not ARuntimeNode.IsRealized then
  begin
    raise ENyxModel.Create('Canvas value requires a realized field');
  end;
  LOwner := FDocument.Find(ARuntimeNode.DesignID);

  if LOwner = nil then
  begin
    raise ENyxModel.Create('Canvas field has no editable owner');
  end;
  LValue := ARuntimeNode.Prop('value');

  if ARuntimeNode.FindBinding(bpValue, LBinding) then
  begin
    { A bound canvas edit changes its authored default, never a hidden fallback
      property that would immediately be overwritten by the projection. }

    if LBinding.Direction <> bdTwoWay then
    begin
      raise ENyxState.Create('This value projects from state; edit its default in State');
    end;
    LStateValue := FDocument.State.Value(LBinding.StateName);
    LStateValue := ParseNyxStudioStateInput(NyxStudioStateInputFor(LStateValue), LValue);
    SetStateValues([NyxStateAssign(LBinding.StateName, LStateValue)]);
    Exit;
  end;
  LTarget := LOwner;
  LPath := '.';

  if LOwner.ProjectionKind = 'component' then
  begin
    { Root fields use ".". Descendants follow the public direct named-part path,
      including expanded nested references. An unnamed edge cannot be addressed
      unambiguously; refuse it rather than changing shared template defaults. }
    LPart := ARuntimeNode;
    LPath := '';
    while (LPart.Parent <> nil) and (LPart.Parent.DesignID = LOwner.ID) do
    begin

      if LPart.Prop('part') = '' then
      begin
        raise ENyxModel.Create('Reusable canvas field needs a named part to edit');
      end;

      if LPath = '' then
      begin
        LPath := LPart.Prop('part');
      end
      else
      begin
        LPath := LPart.Prop('part') + '/' + LPath;
      end;
      LPart := LPart.Parent;
    end;

    if LPath = '' then
    begin
      LPath := '.';
    end;
    LTarget := nil;
    for LIndex := 0 to LOwner.Count - 1 do
    begin

      if LOwner.Children[LIndex].Prop('path') = LPath then
      begin
        LTarget := LOwner.Children[LIndex];
      end;
    end;
  end;

  if (LTarget <> nil) and (LTarget.Prop('value') = LValue) then
  begin
    Exit;
  end;
  Checkpoint;
  try

    if LTarget = nil then
    begin
      LCount := LOwner.Count;
      { Empty mode preserves an existing append/replace descriptor if present. }
      LTarget := LOwner.OverridePart(LPath);

      if LOwner.Count > LCount then
      begin
        LTarget.Named(NewID('part'));
      end;
    end;
    LTarget.SetProp('value', LValue);
  except
    Rollback;
    raise;
  end;
  Commit;
end;

procedure TNyxStudioSession.SetTitle(const ATitle: TNyxText);
begin

  if FDocument.Title = ATitle then
  begin
    Exit;
  end;
  Checkpoint;
  FDocument.Title := ATitle;
  Commit;
end;

procedure TNyxStudioSession.PublishCandidate(var ACandidate: TNyxDocument);
begin
  PublishCandidate(ACandidate, []);
end;

procedure TNyxStudioSession.PublishCandidate(var ACandidate: TNyxDocument;
  const ARenames: array of TNyxSourceStateRename);
var
  LWorkspace: TNyxSourceWorkspace;
  LCheckpoint: TNyxSourceCheckpoint;
begin
  ValidateNyxDocumentProperties(ACandidate);
  LCheckpoint := FSourceWorkspace.Capture(FDocument);

  if TNyxCodec.Encode(ACandidate) = LCheckpoint.Design then
  begin
    Exit;
  end;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LWorkspace.Restore(LCheckpoint);
    LWorkspace.Render(ACandidate, ARenames);
    PublishPair(ACandidate, LWorkspace);
  finally
    LWorkspace.Free;
  end;
end;

procedure TNyxStudioSession.PublishPair(var ACandidate: TNyxDocument;
  var AWorkspace: TNyxSourceWorkspace);
var
  LPrevious: TNyxDocument;
  LPreviousSource: TNyxSourceWorkspace;
begin
  Checkpoint;
  { No fallible reconciliation follows publication. Source edits already have
    their exact admitted companion; regenerating that unused intermediate would
    reject valid authored arrangements and perform the same work twice. }
  LPrevious := FDocument;
  LPreviousSource := FSourceWorkspace;
  FDocument := ACandidate;
  ACandidate := nil;
  FSourceWorkspace := AWorkspace;
  AWorkspace := nil;
  LPrevious.Free;
  LPreviousSource.Free;
  FRedo.Clear;
  TrimUndoHistory;
end;

procedure TNyxStudioSession.SetExtension(AOwner: TNyxStudioExtensionOwner;
  const AKey: TNyxExtensionRef; const AValue: TNyxDataValue);
var
  LCandidate: TNyxDocument;
  LStore: TNyxExtensions;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    case AOwner of
      seoDocument:
        begin
          LStore := LCandidate.Extensions;
        end;
      seoSelection:
        begin
          LNode := LCandidate.Find(FSelectedID);

          if LNode = nil then
          begin
            raise ENyxModel.Create('Select an authored extension owner');
          end;
          LStore := LNode.Extensions;
        end;
      else
        begin
          raise ENyxModel.Create('Unknown extension command owner');
        end;
    end;
    LStore.SetValue(AKey, AValue);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.CallbackCandidate: TNyxDocument;
var
  LProjection: TNyxNode;
  LNode: TNyxNode;
begin

  if Selected = nil then
  begin
    raise ENyxModel.Create('Select a component before editing its events');
  end;
  Result := FDocument.Clone;
  try
    LNode := Result.Find(FSelectedID);
    LProjection := SelectedProjection;
    try
      { Materialize inherited registrations only on this edited instance/part.
        The definition and sibling instances keep their independent metadata. }

      if not LNode.Extensions.Has(NyxExtension(NyxCallbacksKey)) and
        (LProjection <> nil) and
        LProjection.Extensions.Has(NyxExtension(NyxCallbacksKey)) then
      begin
        NyxCallbacks(LNode).Metadata(
          LProjection.Extensions.Value(NyxExtension(NyxCallbacksKey)));
      end;
    finally
      LProjection.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TNyxStudioSession.DoAddCallback(ATrigger: TNyxTrigger;
  const AName: TNyxEventRef; out ALine: Integer): TNyxHandlerRef;
var
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LStem: TNyxText;
  LName: TNyxText;
  LIndex: Integer;
  LSerial: Integer;
  LUpper: Boolean;
  LChar: Char;
  LMetadata: TNyxEventSchemas;
  LSupported: Boolean;
begin

  if DraftSource <> Source then
  begin
    raise ENyxSource.CreateAt('Apply or restore the Pascal draft before adding a handler',
      DraftSource, 1);
  end;
  LMetadata := NyxEventsMetadata(Selected, Document);
  LSupported := False;
  for LIndex := 0 to High(LMetadata) do
  begin
    LSupported := LSupported or ((LMetadata[LIndex].Trigger = ATrigger) and
      (LMetadata[LIndex].Name.Name = AName.Name));
  end;

  if not LSupported then
  begin
    raise ENyxModel.Create('The selected component does not publish this callback');
  end;
  { Readable bounded Pascal class names describe authored purpose and event.
    Whole-source collision checking protects the user's existing helper types. }
  LStem := '';
  LUpper := True;
  LName := Selected.ID;

  if (LowerCase(Copy(LName, 1, Length(Selected.Kind))) <> Selected.Kind) and
    (LowerCase(Copy(LName, Length(LName) - Length(Selected.Kind) + 1, MaxInt)) <>
      Selected.Kind) then
  begin
    LName := LName + '-' + Selected.Kind;
  end;

  if ATrigger = ntNamed then
  begin
    LName := LName + '-' + AName.Name;
  end
  else
  begin
    LName := LName + '-' + NyxTriggerName(ATrigger);
  end;
  for LIndex := 1 to Length(LName) do
  begin
    LChar := LName[LIndex];

    if LChar in ['A'..'Z', 'a'..'z', '0'..'9'] then
    begin

      if LUpper then
      begin
        LChar := UpCase(LChar);
      end;
      LStem := LStem + LChar;
      LUpper := False;

      if Length(LStem) >= 90 then
      begin
        Break;
      end;
    end
    else
    begin
      LUpper := True;
    end;
  end;
  LStem := 'T' + LStem;
  LName := LStem;
  LSerial := 1;
  LSource := Source;
  while Pos(LowerCase(LName), LowerCase(LSource)) > 0 do
  begin
    Inc(LSerial);
    LName := LStem + IntToStr(LSerial);
  end;
  Result := NyxHandler(LName);
  LCandidate := CallbackCandidate;
  LWorkspace := TNyxSourceWorkspace.Create;
  try

    if ATrigger = ntNamed then
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).OnNamed(AName)
        .Add(Result, NyxCallbackID(LName));
    end
    else
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).On(ATrigger)
        .Add(Result, NyxCallbackID(LName));
    end;
    ValidateNyxDocumentProperties(LCandidate);
    LWorkspace.Restore(FSourceWorkspace.Capture);
    LSource := LWorkspace.Render(LCandidate);

    if ATrigger = ntNamed then
    begin
      LSource := AddNyxHandlerStub(LSource, Result, AName, ALine);
    end
    else
    begin
      LSource := AddNyxHandlerStub(LSource, Result, ATrigger, ALine);
    end;
    LWorkspace.Accept(LCandidate, LSource);
    PublishPair(LCandidate, LWorkspace);
    DiscardSourceDraft;
  finally
    LWorkspace.Free;
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.AddCallback(ATrigger: TNyxTrigger;
  out ALine: Integer): TNyxHandlerRef;
begin

  if not NyxIsRuntimeTrigger(ATrigger) then
  begin
    raise ENyxModel.Create('Use an exact event reference for a named callback');
  end;
  Result := DoAddCallback(ATrigger, Default(TNyxEventRef), ALine);
end;

function TNyxStudioSession.AddCallback(const AName: TNyxEventRef;
  out ALine: Integer): TNyxHandlerRef;
begin
  Result := DoAddCallback(ntNamed, AName, ALine);
end;

procedure TNyxStudioSession.DoSetCallbackPolicy(ATrigger: TNyxTrigger;
  const AName: TNyxEventRef; APolicy: TNyxExecutionPolicy);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := CallbackCandidate;
  try

    if ATrigger = ntNamed then
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).OnNamed(AName).Policy(APolicy);
    end
    else
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).On(ATrigger).Policy(APolicy);
    end;
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.SetCallbackPolicy(ATrigger: TNyxTrigger;
  APolicy: TNyxExecutionPolicy);
begin
  DoSetCallbackPolicy(ATrigger, Default(TNyxEventRef), APolicy);
end;

procedure TNyxStudioSession.SetCallbackPolicy(const AName: TNyxEventRef;
  APolicy: TNyxExecutionPolicy);
begin
  DoSetCallbackPolicy(ntNamed, AName, APolicy);
end;

procedure TNyxStudioSession.DoRemoveCallback(ATrigger: TNyxTrigger;
  const AName: TNyxEventRef; const AID: TNyxCallbackRef);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := CallbackCandidate;
  try

    if ATrigger = ntNamed then
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).OnNamed(AName).Remove(AID);
    end
    else
    begin
      NyxCallbacks(LCandidate.Find(FSelectedID)).On(ATrigger).Remove(AID);
    end;
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.RemoveCallback(ATrigger: TNyxTrigger;
  const AID: TNyxCallbackRef);
begin
  DoRemoveCallback(ATrigger, Default(TNyxEventRef), AID);
end;

procedure TNyxStudioSession.RemoveCallback(const AName: TNyxEventRef;
  const AID: TNyxCallbackRef);
begin
  DoRemoveCallback(ntNamed, AName, AID);
end;

procedure TNyxStudioSession.MoveCallback(ATrigger: TNyxTrigger;
  const AID: TNyxCallbackRef; AIndex: Integer);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := CallbackCandidate;
  try
    NyxCallbacks(LCandidate.Find(FSelectedID)).On(ATrigger).Move(AID, AIndex);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.MoveCallback(const AName: TNyxEventRef;
  const AID: TNyxCallbackRef; AIndex: Integer);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := CallbackCandidate;
  try
    NyxCallbacks(LCandidate.Find(FSelectedID)).OnNamed(AName).Move(AID, AIndex);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.CallbackLine(const AHandler: TNyxHandlerRef): Integer;
begin
  Result := NyxHandlerSourceLine(DraftSource, AHandler);
end;

procedure TNyxStudioSession.RemoveExtension(AOwner: TNyxStudioExtensionOwner;
  const AKey: TNyxExtensionRef);
var
  LCandidate: TNyxDocument;
  LStore: TNyxExtensions;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    case AOwner of
      seoDocument:
        begin
          LStore := LCandidate.Extensions;
        end;
      seoSelection:
        begin
          LNode := LCandidate.Find(FSelectedID);

          if LNode = nil then
          begin
            raise ENyxModel.Create('Select an authored extension owner');
          end;
          LStore := LNode.Extensions;
        end;
      else
        begin
          raise ENyxModel.Create('Unknown extension command owner');
        end;
    end;
    LStore.Remove(AKey);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.SetStateValues(const AValues: array of TNyxStateAssignment);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := FDocument.Clone;
  try
    LCandidate.State.Apply(AValues, FDocument.State.Revision);

    if LCandidate.State.Revision <> FDocument.State.Revision then
    begin
      PublishCandidate(LCandidate);
    end;
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.CreateState(const AName: TNyxText;
  const AValue: TNyxStateValue);
begin

  if FDocument.State.Has(AName) then
  begin
    raise ENyxState.Create('State key already exists: ' + AName);
  end;
  SetStateValues([NyxStateAssign(AName, AValue)]);
end;

procedure TNyxStudioSession.RemoveState(const AName: TNyxText);
begin
  SetStateValues([NyxStateRemove(AName)]);
end;

procedure TNyxStudioSession.RenameState(const AOldName, ANewName: TNyxText);
var
  LCandidate: TNyxDocument;
  LState: TNyxState;
  LIndex: Integer;
  LKey: TNyxText;

  procedure RenameBindings(ANode: TNyxNode);
  var
    LBindingIndex: Integer;
    LSpec: TNyxBindingSpec;
  begin
    for LBindingIndex := 0 to ANode.BindingCount - 1 do
    begin
      LSpec := ANode.Bindings[LBindingIndex];

      if not LSpec.Cleared and (LSpec.StateName = AOldName) then
      begin
        ANode.SetBinding(TNyxBindingSpec.Bound(LSpec.Target, ANewName,
          LSpec.ValueKind, LSpec.Direction));
      end;
    end;
    for LBindingIndex := 0 to ANode.Count - 1 do
    begin
      RenameBindings(ANode.Children[LBindingIndex]);
    end;
  end;

begin
  FDocument.State.Value(AOldName);

  if AOldName = ANewName then
  begin
    Exit;
  end;

  if FDocument.State.Has(ANewName) then
  begin
    raise ENyxState.Create('State key already exists: ' + ANewName);
  end;
  LCandidate := FDocument.Clone;
  LState := nil;
  try
    LState := TNyxState.Create;
    for LIndex := 0 to FDocument.State.Count - 1 do
    begin
      LKey := FDocument.State.Key(LIndex);

      if LKey = AOldName then
      begin
        LKey := ANewName;
      end;
      LState.Apply([NyxStateAssign(LKey,
        FDocument.State.Value(FDocument.State.Key(LIndex)))]);
    end;
    LCandidate.State.Assign(LState);
    for LIndex := 0 to LCandidate.Count - 1 do
    begin
      RenameBindings(LCandidate.Pages[LIndex]);
    end;
    for LIndex := 0 to LCandidate.ComponentCount - 1 do
    begin
      RenameBindings(LCandidate.Components[LIndex]);
    end;
    PublishCandidate(LCandidate, [NyxSourceStateRename(AOldName, ANewName)]);
  finally
    LState.Free;
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.SetBinding(const ASpec: TNyxBindingSpec);
var
  LCandidate: TNyxDocument;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    LNode := LCandidate.Find(FSelectedID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Select a control to bind');
    end;
    LNode.SetBinding(ASpec);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.InheritBinding(ATarget: TNyxBindingProperty);
var
  LCandidate: TNyxDocument;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    LNode := LCandidate.Find(FSelectedID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Select a control to restore its inherited binding');
    end;
    LNode.Binds.Inherit(ATarget).Done;
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.DefineCollection(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema; const AItems: array of TNyxCollectionItem);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := FDocument.Clone;
  try
    LCandidate.Collections.Define(AKey, ASchema, AItems);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.RemoveCollection(const AKey: TNyxCollectionRef);
var
  LCandidate: TNyxDocument;
begin
  LCandidate := FDocument.Clone;
  try
    LCandidate.Collections.Remove(AKey);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.SetCollectionView(const ASpec: TNyxCollectionViewSpec);
var
  LCandidate: TNyxDocument;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    LNode := LCandidate.Find(FSelectedID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Select a control for its collection binding');
    end;
    LNode.SetCollectionView(ASpec);
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.InheritCollectionView;
var
  LCandidate: TNyxDocument;
  LNode: TNyxNode;
begin
  LCandidate := FDocument.Clone;
  try
    LNode := LCandidate.Find(FSelectedID);

    if LNode = nil then
    begin
      raise ENyxModel.Create('Select a control to restore its inherited collection binding');
    end;
    LNode.RemoveCollectionView;
    PublishCandidate(LCandidate);
  finally
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.SelectedProjection: TNyxNode;
var
  LSelected: TNyxNode;
  LRuntime: TNyxNode;
begin
  Result := nil;
  LSelected := Selected;

  if LSelected = nil then
  begin
    Exit;
  end;

  if LSelected.Kind = 'slot-override' then
  begin

    if LSelected.Prop('mode') = 'remove' then
    begin
      Exit;
    end;
    LRuntime := RealizeNyxView(FDocument, LSelected.Parent);
    try
      ApplyNyxBindings(LRuntime, FDocument.State);
      Result := LRuntime.Part(LSelected.Prop('path')).Clone;
    finally
      LRuntime.Free;
    end;
  end
  else
  begin
    LRuntime := RealizeNyxView(FDocument, LSelected);
    try
      ApplyNyxBindings(LRuntime, FDocument.State);
      Result := LRuntime;
      LRuntime := nil;
    finally
      LRuntime.Free;
    end;
  end;
end;

function TNyxStudioSession.InsertionParent: TNyxNode;
var
  LParent: TNyxNode;
  LIndex: Integer;
  LRuntime: TNyxNode;
  LInfo: TNyxPrimitiveInfo;
begin
  LParent := Selected;

  if LParent = nil then
  begin
    raise ENyxModel.Create('Select a container first');
  end;
  LIndex := FCatalog.IndexOf(LParent.Kind);

  if (LParent.Kind <> 'slot-override') and
    ((LIndex < 0) or not FCatalog[LIndex].Container) then
  begin
    LParent := LParent.Parent;
  end;

  if LParent = nil then
  begin
    raise ENyxModel.Create('Selected view has no editable container');
  end;

  if LParent.Kind = 'slot-override' then
  begin

    if (LParent.Prop('mode') <> 'properties') and
      (LParent.Prop('mode') <> 'append') and (LParent.Prop('mode') <> 'prepend') then
    begin
      raise ENyxModel.Create('Select an appended part to add content');
    end;
    LRuntime := RealizeNyxView(FDocument, LParent.Parent);
    try

      if not FindNyxPrimitive(LRuntime.Part(LParent.Prop('path')).ProjectionKind, LInfo) or
        not LInfo.Container then
      begin
        raise ENyxModel.Create('This named part is a leaf; select a layout part to add content');
      end;
    finally
      LRuntime.Free;
    end;
  end;
  Result := LParent;
end;

procedure TNyxStudioSession.InsertNode(ANode: TNyxNode);
var
  LParent: TNyxNode;
  LOwned: Boolean;
begin
  LOwned := False;
  try
    LParent := InsertionParent;
    Checkpoint;
    try
      LParent.Add(ANode);
      LOwned := True;

      if (LParent.Kind = 'slot-override') and (LParent.Prop('mode') = 'properties') then
      begin
        LParent.SetProp('mode', 'append');
      end;
    except
      Rollback;
      raise;
    end;
    FSelectedID := ANode.ID;
    { Commit performs its own rollback if effective paths, values or references
      fail. The ownership flag prevents touching the freed candidate afterward. }
    Commit;
  finally

    if not LOwned then
    begin
      ANode.Free;
    end;
  end;
end;

procedure TNyxStudioSession.AddKind(const AKind: TNyxText);
begin
  InsertNode(FCatalog.NewNode(AKind, NewID(AKind)));
end;

procedure TNyxStudioSession.AddControl(const AControl: INyxControl);
var
  LNode: TNyxNode;
begin

  if AControl = nil then
  begin
    raise ENyxModel.Create('A control is required');
  end;
  LNode := AControl.Node;
  ValidateNyxPropertyTree(LNode);
  InsertNode(LNode.Clone);
end;

procedure TNyxStudioSession.CustomizePart(const APath: TNyxText);
var
  LReference: TNyxNode;
  LRule: TNyxNode;
  LCount: Integer;
  LMode: TNyxText;
  LIndex: Integer;
begin
  LReference := Selected;

  if (LReference = nil) or (LReference.ProjectionKind <> 'component') then
  begin
    raise ENyxModel.Create('Select a reusable instance to customize its parts');
  end;
  LCount := LReference.Count;
  LMode := 'properties';
  for LIndex := 0 to LReference.Count - 1 do
  begin

    if LReference.Children[LIndex].Prop('path') = APath then
    begin
      LMode := LReference.Children[LIndex].Prop('mode');
    end;
  end;
  Checkpoint;
  try
    LRule := LReference.OverridePart(APath, LMode);

    if LReference.Count > LCount then
    begin
      LRule.Named(NewID('part'));
    end;
  except
    Rollback;
    raise;
  end;
  Commit;
  FSelectedID := LRule.ID;
end;

procedure TNyxStudioSession.DeleteSelected;
var
  LNode: TNyxNode;
  LParent: TNyxNode;
begin
  LNode := Selected;

  if LNode = nil then
  begin
    Exit;
  end;
  LParent := LNode.Parent;

  if LParent = nil then
  begin
    raise ENyxModel.Create('Root views are retained; select a child to delete');
  end;
  Checkpoint;
  FSelectedID := LParent.ID;
  LParent.Remove(LNode);

  if (LParent.Kind = 'slot-override') and (LParent.Count = 0) then
  begin
    LParent.SetProp('mode', 'properties');
  end;
  Commit;
end;

procedure TNyxStudioSession.Reidentify(ANode: TNyxNode);
var
  LIndex: Integer;
begin
  ANode.Named(NewID(ANode.Kind));
  for LIndex := 0 to ANode.Count - 1 do
  begin
    Reidentify(ANode.Children[LIndex]);
  end;
end;

procedure TNyxStudioSession.DuplicateSelected;
var
  LNode: TNyxNode;
  LCopy: TNyxNode;
begin
  LNode := Selected;

  if (LNode = nil) or (LNode.Parent = nil) then
  begin
    raise ENyxModel.Create('Select a child to duplicate');
  end;
  LCopy := LNode.Clone;
  try
    Reidentify(LCopy);
    Checkpoint;
    LNode.Parent.Add(LCopy);
  except
    LCopy.Free;
    Rollback;
    raise;
  end;
  FSelectedID := LCopy.ID;
  Commit;
end;

procedure TNyxStudioSession.MoveSelected(ADirection: Integer);
var
  LNode: TNyxNode;
  LParent: TNyxNode;
  LIndex: Integer;
  LDestination: Integer;
begin
  LNode := Selected;

  if (LNode = nil) or (LNode.Parent = nil) then
  begin
    Exit;
  end;
  LParent := LNode.Parent;
  LIndex := 0;
  while LParent.Children[LIndex] <> LNode do
  begin
    Inc(LIndex);
  end;
  LDestination := LIndex + ADirection;

  if (LDestination < 0) or (LDestination >= LParent.Count) then
  begin
    Exit;
  end;
  Checkpoint;
  LNode := LParent.Extract(LIndex);
  LParent.Insert(LDestination, LNode);
  Commit;
end;

procedure TNyxStudioSession.AddPage;
var
  LNode: TNyxNode;
begin
  LNode := FCatalog.NewNode('page', NewID('page'));
  try
    Checkpoint;
    FDocument.AddPage(LNode);
  except
    LNode.Free;
    raise;
  end;
  Activate(LNode.ID);
  Commit;
end;

procedure TNyxStudioSession.CreateComponent;
var
  LCopy: TNyxNode;
begin

  if Selected = nil then
  begin
    raise ENyxModel.Create('Select a subtree to make reusable');
  end;
  LCopy := Selected.Clone;
  try
    Reidentify(LCopy);
    Checkpoint;
    FDocument.AddComponent(LCopy);
  except
    LCopy.Free;
    raise;
  end;
  Activate(LCopy.ID);
  Commit;
end;

procedure TNyxStudioSession.AddComponentInstance(const ADefinitionID: TNyxText);
begin

  if FDocument.FindComponent(ADefinitionID) = nil then
  begin
    raise ENyxModel.Create('Reusable definition is missing');
  end;
  InsertNode(TNyxNode.Create('component', NewID('component'))
    .SetProp('component', ADefinitionID));
end;

procedure TNyxStudioSession.Undo;
begin

  if FUndo.Count = 0 then
  begin
    Exit;
  end;
  RestorePair(FUndo.Last, FRedo);
  FUndo.Delete(FUndo.Count - 1);
end;

procedure TNyxStudioSession.Redo;
begin

  if FRedo.Count = 0 then
  begin
    Exit;
  end;
  RestorePair(FRedo.Last, FUndo);
  FRedo.Delete(FRedo.Count - 1);
end;

procedure TNyxStudioSession.Load(const ASource: TNyxText);
begin
  { Admission precedes releasing the baseline. A successful import deliberately
    starts a new history; an invalid import preserves both document and history. }
  Restore(ASource);
  FSourceWorkspace.Reset;
  DiscardSourceDraft;
  FUndo.Clear;
  FRedo.Clear;
end;

function TNyxStudioSession.Save: TNyxText;
{$ifdef NYX_SOURCE_PROFILE}
var
  LStarted: Double;
{$endif}
begin
  {$ifdef NYX_SOURCE_PROFILE}LStarted := SourceProfileStart;{$endif}
  Result := TNyxCodec.Encode(FDocument);
  {$ifdef NYX_SOURCE_PROFILE}SourceProfileFinish(spSave, LStarted);{$endif}
end;

function TNyxStudioSession.Source: TNyxText;
begin
  Result := FSourceWorkspace.Render(FDocument);
end;

function TNyxStudioSession.ProjectSnapshot: TNyxProjectPair;
begin
  Result := NyxProjectPair(Save, Source);
  Result.Pending := FSourceDraftPending;

  if Result.Pending then
  begin
    Result.Draft := FSourceDraft;
    Result.DraftBase := FSourceDraftBase;
  end;
end;

procedure TNyxStudioSession.LoadProject(const APair: TNyxProjectPair;
  AResolution: TNyxProjectResolution);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin
  AdmitNyxProject(APair, AResolution, LDocument, LWorkspace, LResolved);
  { Publication contains no parsing, filesystem calls or callback execution.
    Both old owners remain alive until the entire replacement has been admitted. }
  FDocument.Free;
  FSourceWorkspace.Free;
  FDocument := LDocument;
  FSourceWorkspace := LWorkspace;
  FActiveViewID := '';

  if FDocument.Count > 0 then
  begin
    FActiveViewID := FDocument.Pages[0].ID;
  end
  else if FDocument.ComponentCount > 0 then
  begin
    FActiveViewID := FDocument.Components[0].ID;
  end;
  FSelectedID := FActiveViewID;
  FNextID := 0;
  DiscardSourceDraft;

  if LResolved.Pending then
  begin
    FSourceDraft := LResolved.Draft;
    FSourceDraftBase := LResolved.DraftBase;
    FSourceDraftPending := True;
  end;
  FUndo.Clear;
  FRedo.Clear;
end;

function TNyxStudioSession.CanUndo: Boolean;
begin
  Result := FUndo.Count > 0;
end;

function TNyxStudioSession.CanRedo: Boolean;
begin
  Result := FRedo.Count > 0;
end;

procedure TNyxStudioSession.ApplyPatch(const APatch: INyxDesignPatch);
var
  LCandidate: TNyxDocument;
begin

  if (APatch = nil) or (DraftSource <> Source) then
  begin
    raise ENyxModel.Create('Resolve the pending Pascal draft before applying an agent transaction');
  end;
  LCandidate := APatch.Candidate(FDocument, FCatalog);
  try
    PublishCandidate(LCandidate);

    if FDocument.Find(FSelectedID) = nil then
    begin
      FSelectedID := FActiveViewID;
    end;
  finally
    LCandidate.Free;
  end;
end;

procedure TNyxStudioSession.RemoveRoots(const AReview: INyxRootRemoval);
var
  LPair: TNyxProjectPair;
begin

  if AReview = nil then
  begin
    raise ENyxModel.Create('Review the exact root group before removing it');
  end;
  LPair := AReview.Candidate(ProjectSnapshot);
  AdoptProject(LPair);
end;

procedure TNyxStudioSession.AdoptProject(const APair: TNyxProjectPair);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LResolved: TNyxProjectPair;
begin
  LDocument := nil;
  LWorkspace := nil;
  try
    AdmitNyxProject(APair, nprRequireMatch, LDocument, LWorkspace, LResolved);

    if (APair.Design <> Save) or (APair.Source <> Source) then
    begin
      PublishPair(LDocument, LWorkspace);
    end;
    DiscardSourceDraft;

    if LResolved.Pending then
    begin
      RestoreSourceDraft(LResolved.Draft, LResolved.DraftBase);
    end;

    if FDocument.Find(FActiveViewID) = nil then
    begin
      FActiveViewID := '';

      if FDocument.Count > 0 then
      begin
        FActiveViewID := FDocument.Pages[0].ID;
      end
      else if FDocument.ComponentCount > 0 then
      begin
        FActiveViewID := FDocument.Components[0].ID;
      end;
    end;

    if FDocument.Find(FSelectedID) = nil then
    begin
      FSelectedID := FActiveViewID;
    end;
  finally
    LDocument.Free;
    LWorkspace.Free;
  end;
end;

function TNyxStudioSession.DraftSource: TNyxText;
begin

  if FSourceDraftPending then
  begin
    Exit(FSourceDraft);
  end;
  Result := Source;
end;

procedure TNyxStudioSession.SetSourceDraft(const ASource: TNyxText);
var
  LAccepted: TNyxText;
begin
  LAccepted := Source;

  if ASource <> DraftSource then
  begin
    FSourceDiagnostic := Default(TNyxSourceDiagnostic);
    FSourceDiagnosticSource := '';
  end;

  if not FSourceDraftPending then
  begin
    FSourceDraftBase := LAccepted;
  end;
  FSourceDraft := ASource;
  FSourceDraftPending := ASource <> LAccepted;
end;

procedure TNyxStudioSession.DiscardSourceDraft;
begin
  FSourceDiagnostic := Default(TNyxSourceDiagnostic);
  FSourceDiagnosticSource := '';
  FSourceDraft := '';
  FSourceDraftBase := '';
  FSourceDraftPending := False;
end;

procedure TNyxStudioSession.RestoreSourceDraft(const ASource, ABase: TNyxText);
begin
  FSourceDiagnostic := Default(TNyxSourceDiagnostic);
  FSourceDiagnosticSource := '';
  FSourceDraft := ASource;
  FSourceDraftBase := ABase;
  FSourceDraftPending := ASource <> Source;
end;

procedure TNyxStudioSession.DoApplySourceDraft;
var
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
begin
  LSource := DraftSource;

  if LSource = Source then
  begin
    DiscardSourceDraft;
    Exit;
  end;

  if FSourceDraftBase <> Source then
  begin
    raise ENyxSource.CreateAt(
      'The design changed while this draft was open. Save the draft, restore ' +
      'accepted Pascal and merge your edits before applying', LSource, 1);
  end;
  LCandidate := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LCandidate := FSourceWorkspace.Candidate(FDocument, LSource);
    LWorkspace.Accept(LCandidate, LSource);
    { Comment/helper-only edits and changed designs publish their exact admitted
      pair through one source/design checkpoint. }
    PublishPair(LCandidate, LWorkspace);
    DiscardSourceDraft;
    TrimUndoHistory;

    if FDocument.Find(FActiveViewID) = nil then
    begin
      FActiveViewID := '';
    end;

    if FDocument.Find(FSelectedID) = nil then
    begin
      FSelectedID := FActiveViewID;
    end;
  finally
    LWorkspace.Free;
    LCandidate.Free;
  end;
end;

function TNyxStudioSession.GetSourceDiagnostic: TNyxSourceDiagnostic;
begin
  Result := Default(TNyxSourceDiagnostic);

  if FSourceDiagnostic.Defined and
    (FSourceDiagnosticSource = DraftSource) then
  begin
    Result := FSourceDiagnostic;
  end;
end;

procedure TNyxStudioSession.ApplySourceDraft;
var
  LSource: TNyxText;
begin
  LSource := DraftSource;
  FSourceDiagnostic := Default(TNyxSourceDiagnostic);
  FSourceDiagnosticSource := '';
  try
    DoApplySourceDraft;
  except
    on LException: Exception do
    begin
      FSourceDiagnostic.Defined := True;
      FSourceDiagnostic.Message := LException.Message;
      FSourceDiagnosticSource := LSource;

      if LException is ENyxSource then
      begin
        FSourceDiagnostic.Message := ENyxSource(LException).DiagnosticText;
        FSourceDiagnostic.Line := ENyxSource(LException).Line;
        FSourceDiagnostic.Column := ENyxSource(LException).Column;
      end;
      raise;
    end;
  end;
end;

end.
