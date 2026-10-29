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


unit nyx.studio.collectionedits;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.state, nyx.contract, nyx.collections,
  nyx.collections.registry,
  nyx.collections.view.types, nyx.collections.selection, nyx.collections.query,
  nyx.typeahead, nyx.collections.inspection, nyx.collections.recovery.policy,
  nyx.collections.query.policy, nyx.collections.arrangement, nyx.formatting,
  nyx.collections.workbench.policy, nyx.collections.presentation, nyx.collections.alignment,
  nyx.studio.authoring,
  nyx.studio.collectionintent, nyx.studio.stateedits, nyx.studio.projects;

const
  NyxMaximumCollectionChanges = 32;

type
  { One adjacent authored move, independent of rendered pin groups, hidden
    columns and runtime preferences. Field identity determines the source. }
  TNyxCollectionColumnMove = (nccmEarlier, nccmLater);

  { Append-only command families preserve existing private-wire ordinals while
    routing copied authored values through one isolated paired candidate. }
  TNyxCollectionChangeKind = (ccDefine, ccField, ccRemoveField, ccAppend,
    ccUpdateRow, ccMoveRow, ccIntent, ccBind, ccQuery, ccTypeAhead,
    ccTableQuery, ccColumnQuery, ccTableLayout, ccColumnLayout, ccColumnFormat,
    ccTableWorkbench, ccTablePresentation, ccColumnText, ccColumnAlignment, ccInspection,
    ccDraftRecovery, ccColumnMove);

  { Immutable typed proposals own copied values, never a document/store/control.
    A one-field schema supplies named field authoring with its typed default and
    domain. Scoped items keep row identity distinct from open collection names.
    The ordinary intent overload covers the editor's closed actions, including
    complete typed queries qualified by an exact schema/binding baseline. }
  TNyxCollectionChange = record
  private
    FKind: TNyxCollectionChangeKind;
    FKey: TNyxCollectionRef;
    FSchema: TNyxCollectionSchema;
    FItems: array of TNyxCollectionItem;
    FField: TNyxStudioCollectionFieldRef;
    FItem: TNyxCollectionItem;
    FIndex: Integer;
    FColumnMove: TNyxCollectionColumnMove;
    FOwner: TNyxStudioBindingOwner;
    FIntent: TNyxStudioCollectionIntent;
    FSpec: TNyxCollectionViewSpec;
    FProjection: TNyxCollectionProjection;
    FQuery: TNyxCollectionQuery;
    { Scalar-only authored choice. False means an explicit library-default
      reset at replay, never collection inheritance or a live search engine. }
    FTypeAheadDeclared: Boolean;
    FTypeAhead: TNyxTypeAheadOptions;
    { Raw presence differs from explicitly choosing the default policy. These
      values contain no paused snapshot, receiver, viewport or runtime state. }
    FInspectionDeclared: Boolean;
    FInspection: TNyxCollectionInspectionPolicy;
    FDraftRecoveryDeclared: Boolean;
    FDraftRecovery: TNyxCollectionDraftRecoveryPolicy;
    { Command policies own only scalar flags and an operator set. An explicit
      default object stays declared; only the separate reset factories clear
      presence. No baseline, projected node or runtime query owner is retained. }
    FQueryControlsDeclared: Boolean;
    FTableQuery: TNyxTableQueryPolicy;
    FColumnQuery: TNyxColumnQueryPolicy;
    { Defined defaults remain authored; a separate reset clears raw presence.
      These scalar policies retain no runtime preference or widget owner. }
    FLayoutDeclared: Boolean;
    FTableLayout: TNyxTableLayoutPolicy;
    FColumnLayout: TNyxColumnLayoutPolicy;
    FColumnFormat: TNyxValueFormat;
    { Workbench presence and immutable intent never retain clipboard hosts,
      input, selection or runtime result state inside an agent/history patch. }
    FWorkbenchDeclared: Boolean;
    FTableWorkbench: TNyxTableWorkbenchPolicy;
    { Text layout and row spacing are copied authored values. This declaration
      flag carries explicit defaults separately from resetting raw presence. }
    FPresentationDeclared: Boolean;
    FTablePresentation: TNyxTablePresentationPolicy;
    FColumnText: TNyxColumnTextPolicy;
    FColumnAlignment: TNyxColumnAlignmentPolicy;
  public
    property Kind: TNyxCollectionChangeKind read FKind;
  end;

  { Candidate replays ordinary Studio commands on one independent owned session.
    Each ordered intermediate must admit: clear dependencies before removing a
    field/collection. Failure discards the complete group and leaves APair intact.
    The caller publishes the final pair once, as one paired Undo step. Pending
    Pascal drafts refuse; queries may still inspect accepted document defaults.
    Runtime stores and user-owned navigation are outside this mutation contract. }
  INyxCollectionPatch = interface(IInterface)
    ['{2D83A76B-9649-4382-AB29-7E39C917EA32}']
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function ToData: TNyxDataValue;
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

{ New named definitions refuse existing keys; omitted rows cannot erase data. }
function NyxDefineCollection(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema; const AItems: array of TNyxCollectionItem):
  TNyxCollectionChange;
{ Add/update exactly one named field. Existing family must agree; unspecified
  fields/rows retain order and values. Changed domains revalidate every row/view. }
function NyxSetCollectionField(const AKey: TNyxCollectionRef;
  const ADefinition: TNyxCollectionSchema): TNyxCollectionChange;
{ Remove one exact family-qualified field and its row cells; dependent authored
  columns/parent mappings must be cleared earlier in the admitted group. }
function NyxRemoveCollectionField(const AKey: TNyxCollectionRef;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
{ Append materializes omitted schema defaults; update changes only supplied
  fields of an existing scoped row. Unknown/duplicate identities refuse. }
function NyxAppendCollectionRow(const AItem: TNyxCollectionItem): TNyxCollectionChange;
function NyxUpdateCollectionRow(const AItem: TNyxCollectionItem): TNyxCollectionChange;
{ Final index is in the sequence after removing this existing row (0..Count-1). }
function NyxMoveCollectionRow(const AItem: TNyxItemRef;
  AIndex: Integer): TNyxCollectionChange;
{ Data intents have no owner; view intents require an exact authored owner. }
function NyxCollectionIntentChange(const AIntent: TNyxStudioCollectionIntent):
  TNyxCollectionChange; overload;
function NyxCollectionIntentChange(const AOwner: TNyxStudioBindingOwner;
  const AIntent: TNyxStudioCollectionIntent): TNyxCollectionChange; overload;
{ Full fluent view specification, with explicit expected list/table/tree family.
  Clearing/inheritance remain distinct ordinary intents, rather than null binds. }
function NyxBindCollection(const AOwner: TNyxStudioBindingOwner;
  AProjection: TNyxCollectionProjection; const ASpec: TNyxCollectionViewSpec):
  TNyxCollectionChange;
{ Replace only the effective view's query, preserving its columns, scope, parent
  and selection. The exact owner/key/projection must still agree when replayed.
  A grouped agent caller additionally supplies the authoritative revision; no
  expanded schema or UI-form baseline needs to cross the protocol. An empty
  policy clears filtering/sorting without clearing the collection binding.
  An inherited view acquires an independent local specification. }
function NyxSetCollectionQuery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AQuery: TNyxCollectionQuery): TNyxCollectionChange;
{ Replace only a bound list/tree's authored search choice. Exact owner, key and
  projection must still agree during replay. Copies preserve columns, hierarchy,
  query, scope and selection. An inherited view gains an independent local spec;
  no runtime prefix, timer, receiver or selection enters the patch. Invalid
  options/projections refuse before a candidate or project can be published. }
function NyxSetCollectionTypeAhead(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxTypeAheadOptions): TNyxCollectionChange;
{ Explicit local library-default reset. It retains the complete effective
  binding; ordinary collection inheritance remains a separate editor operation.
  Tables, unbound owners and changed identities refuse rather than silently
  discard behavior. The caller's revision/Undo guards remain authoritative. }
function NyxUseDefaultCollectionTypeAhead(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection):
  TNyxCollectionChange;
{ Apply only saved follow/retention defaults to the exact effective binding.
  Ordered replay derives that binding after preceding group edits, so another
  policy in the same transaction cannot be overwritten. Tree supports pause
  and keep-position; follow-last is refused. Runtime inspection is untouched. }
function NyxSetCollectionInspection(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxCollectionInspectionPolicy): TNyxCollectionChange;
{ Clear only inspection's raw declaration, retaining an independent local
  effective binding. Restoring reusable inheritance is a separate operation. }
function NyxUseDefaultCollectionInspection(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection): TNyxCollectionChange;
{ Partial authored recovery edits replay against the current ordered candidate.
  They preserve inspection/layout choices and acquire no runtime draft authority. }
function NyxSetCollectionDraftRecovery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxCollectionDraftRecoveryPolicy): TNyxCollectionChange;
{ Clears raw recovery presence only, leaving the effective binding local. }
function NyxUseDefaultCollectionDraftRecovery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection): TNyxCollectionChange;
{ Replace only a table's authored filter/sort command policy. Each ordered
  candidate derives the current schema/binding baseline internally and reuses
  ordinary intent admission; agents need only the exact revision/owner/key.
  A defined default object remains an explicit saved choice. Non-table
  projections refuse, independently of their inspectable saved metadata. }
function NyxSetCollectionTableQuery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxTableQueryPolicy): TNyxCollectionChange;
{ Remove the table's declaration while retaining its independent local binding,
  columns, query, paging and sizing. This does not restore reusable inheritance. }
function NyxUseDefaultCollectionTableQuery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection):
  TNyxCollectionChange;
{ Replace one exact typed column's command policy. Its field must still occur
  in both the current schema and bound columns with the same scalar family.
  Policies are immutable scalar/set values; caller edits cannot retarget them. }
function NyxSetCollectionColumnQuery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef;
  const APolicy: TNyxColumnQueryPolicy): TNyxCollectionChange;
{ Reset only this column's declaration to library defaults. Undefined fields
  refuse; the remaining table/column policies and local ownership survive. }
function NyxUseDefaultCollectionColumnQuery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
{ Replace one complete authored table layout policy. The ordered candidate
  derives its current schema/binding baseline, not a live user's preferences. }
function NyxSetCollectionTableLayout(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxTableLayoutPolicy): TNyxCollectionChange;
{ Clear only this table policy's declaration, preserving the local binding and
  every column. Restoring reusable inheritance remains a separate operation. }
function NyxUseDefaultCollectionTableLayout(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection): TNyxCollectionChange;
{ Replace only one existing family-qualified column's authored layout defaults;
  an old spelling cannot retarget a different field family or added column. }
function NyxSetCollectionColumnLayout(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef;
  const APolicy: TNyxColumnLayoutPolicy): TNyxCollectionChange;
{ Remove just this column's raw policy. Explicit defaults and inherited binding
  ownership stay distinguishable through persistence and paired Undo/Redo. }
function NyxUseDefaultCollectionColumnLayout(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
{ Move one exact typed field across its immediate authored neighbour. The
  ordered candidate derives its current schema/binding baseline internally,
  preserving all policies and reusable independence. Edge moves refuse; no
  runtime arrangement or physical column position enters the saved change. }
function NyxMoveCollectionColumn(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; const AField: TNyxStudioCollectionFieldRef;
  ADirection: TNyxCollectionColumnMove): TNyxCollectionChange;
function NyxCollectionPatch(const AChanges: array of TNyxCollectionChange):
  INyxCollectionPatch;

{ Complete immutable display policy with an ordered candidate baseline. Reset
  removes just this declaration; reusable inheritance remains independent. }
function NyxSetCollectionColumnFormat(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef;
  const APolicy: TNyxValueFormat): TNyxCollectionChange;
function NyxUseDefaultCollectionColumnFormat(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;

{ Change the complete saved action policy of one exact bound table. Replay
  derives the current baseline after preceding grouped edits; an inherited
  binding becomes a local copy. No runtime service or data mutation is implied. }
function NyxSetCollectionTableWorkbench(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxTableWorkbenchPolicy): TNyxCollectionChange;
{ Clear only the policy's presence, retaining the effective local binding.
  Restoring reusable inheritance remains a separate explicit operation. }
function NyxUseDefaultCollectionTableWorkbench(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection):
  TNyxCollectionChange;

{ Replace only authored body-row spacing. Ordered replay derives the current
  binding baseline; runtime user preferences and text work limits stay separate. }
function NyxSetCollectionTablePresentation(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxTablePresentationPolicy): TNyxCollectionChange;
{ Remove row-spacing presence without resetting any column or local binding. }
function NyxUseDefaultCollectionTablePresentation(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection): TNyxCollectionChange;
{ Set one family-qualified column's display layout. Accepted values, formatters,
  editing text and independent arrangement policies are never rewritten. }
function NyxSetCollectionColumnText(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef;
  const APolicy: TNyxColumnTextPolicy): TNyxCollectionChange;
{ Clear only the exact column's text declaration; explicit Clip is distinct. }
function NyxUseDefaultCollectionColumnText(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
{ Complete logical axes share the current owner/schema baseline and undo group.
  An undefined setter refuses; reset removes only this authored declaration. }
function NyxSetCollectionColumnAlignment(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef;
  const APolicy: TNyxColumnAlignmentPolicy): TNyxCollectionChange;
function NyxUseDefaultCollectionColumnAlignment(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;

{ Explicit MCP/descriptor boundary. Friendly closed choices decode to the typed
  API above. Unknown/extra members, wrong primitives, duplicates and unused
  payload refuse. Numeric values retain their exact accepted Pascal family. }
function ReadNyxCollectionPatch(const AData: TNyxDataValue): INyxCollectionPatch;
function NyxCollectionAgentSchema: TNyxDataValue;

implementation

uses
  nyx.model, nyx.studio.session, nyx.collections.query.editor;

type
  TNyxCollectionPatch = class(TInterfacedObject, INyxCollectionPatch)
  private
    FChanges: array of TNyxCollectionChange;
  public
    constructor Create(const AChanges: array of TNyxCollectionChange);
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
    function ToData: TNyxDataValue;
    function GetCount: Integer;
  end;

const
  CCollectionActions: array[TNyxStudioCollectionAction] of TNyxText =
    ('create', 'remove', 'add-field', 'default', 'add-row', 'remove-row', 'cell',
     'bind', 'scope', 'column-title', 'column-mode', 'parent', 'remove-column',
     'add-column', 'clear', 'inherit', 'selection', 'query',
     'column-content', 'column-fixed', 'column-flex', 'column-minimum',
     'column-maximum', 'column-default', 'page-size', 'initial-page', 'default-paging',
     'table-query', 'column-query', 'default-table-query', 'default-column-query',
     'table-layout', 'column-layout', 'default-table-layout', 'default-column-layout',
     'column-format', 'default-column-format', 'table-workbench', 'default-table-workbench',
     'table-presentation', 'default-table-presentation', 'column-text', 'default-column-text',
     'column-alignment', 'default-column-alignment', 'inspection', 'default-inspection',
     'draft-recovery', 'default-draft-recovery', 'column-earlier', 'column-later');
  CCollectionProjections: array[TNyxCollectionProjection] of TNyxText =
    ('list', 'table', 'tree');

function PutValue(const AItem: TNyxCollectionItem; const AName: TNyxText;
  const AValue: TNyxStateValue): TNyxCollectionItem;
begin
  case Ord(AValue.Kind) of
    Ord(nskText):
      begin
        Result := AItem.WithValue(NyxTextField(AName), AValue.TextValue);
      end;
    Ord(nskBoolean):
      begin
        Result := AItem.WithValue(NyxBooleanField(AName), AValue.BooleanValue);
      end;
    Ord(nskInteger):
      begin
        Result := AItem.WithValue(NyxIntegerField(AName), AValue.IntegerValue);
      end;
    Ord(nskNumber):
      begin
        Result := AItem.WithValue(NyxNumberField(AName), AValue.NumberValue);
      end;
  else
    begin
      raise ENyxCollection.Create('Unknown collection scalar family');
    end;
  end;
end;

function NyxDefineCollection(const AKey: TNyxCollectionRef;
  const ASchema: TNyxCollectionSchema; const AItems: array of TNyxCollectionItem):
  TNyxCollectionChange;
var
  LIndex: Integer;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccDefine;
  Result.FKey := NyxCollection(AKey.Name);
  Result.FSchema := ASchema.Copy;
  Result.FSchema.Validate;

  if Length(AItems) > NyxMaximumCollectionItems then
  begin
    raise ENyxCollection.Create('Too many initial collection rows');
  end;
  SetLength(Result.FItems, Length(AItems));
  for LIndex := 0 to High(AItems) do
  begin
    Result.FItems[LIndex] := AItems[LIndex].Copy;

    if Result.FItems[LIndex].Ref.Collection.Name <> AKey.Name then
    begin
      raise ENyxCollection.Create('Initial row belongs to another collection');
    end;
  end;
end;

function NyxSetCollectionField(const AKey: TNyxCollectionRef;
  const ADefinition: TNyxCollectionSchema): TNyxCollectionChange;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccField;
  Result.FKey := NyxCollection(AKey.Name);
  Result.FSchema := ADefinition.Copy;
  Result.FSchema.Validate;

  if Result.FSchema.Count <> 1 then
  begin
    raise ENyxCollection.Create('Field authoring requires exactly one typed definition');
  end;
end;

function NyxRemoveCollectionField(const AKey: TNyxCollectionRef;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccRemoveField;
  Result.FKey := NyxCollection(AKey.Name);

  if not AField.Defined then
  begin
    raise ENyxCollection.Create('Field removal requires a typed field reference');
  end;
  Result.FField := AField;
end;

function RowChange(const AItem: TNyxCollectionItem;
  AKind: TNyxCollectionChangeKind): TNyxCollectionChange;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := AKind;
  Result.FKey := NyxCollection(AItem.Ref.Collection.Name);
  Result.FItem := AItem.Copy;
end;

function NyxAppendCollectionRow(const AItem: TNyxCollectionItem): TNyxCollectionChange;
begin
  Result := RowChange(AItem, ccAppend);
end;

function NyxUpdateCollectionRow(const AItem: TNyxCollectionItem): TNyxCollectionChange;
begin
  Result := RowChange(AItem, ccUpdateRow);
end;

function NyxMoveCollectionRow(const AItem: TNyxItemRef;
  AIndex: Integer): TNyxCollectionChange;
begin

  if (AIndex < 0) or (AIndex >= NyxMaximumCollectionItems) then
  begin
    raise ENyxCollection.Create('Collection row index is outside its budget');
  end;
  Result := RowChange(NyxCollectionItem(AItem), ccMoveRow);
  Result.FIndex := AIndex;
end;

function NyxCollectionIntentChange(const AIntent: TNyxStudioCollectionIntent):
  TNyxCollectionChange;
begin
  AIntent.Validate;

  if NyxStudioCollectionViewAction(AIntent.Action) then
  begin
    raise ENyxCollection.Create('View intent requires its exact authored owner');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccIntent;
  Result.FKey := NyxCollection(AIntent.Key.Name);
  Result.FIntent := AIntent;
end;

function NyxCollectionIntentChange(const AOwner: TNyxStudioBindingOwner;
  const AIntent: TNyxStudioCollectionIntent): TNyxCollectionChange;
begin
  AIntent.Validate;

  if not NyxStudioCollectionViewAction(AIntent.Action) then
  begin
    raise ENyxCollection.Create('Data intent cannot carry an authored view owner');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccIntent;
  Result.FKey := NyxCollection(AIntent.Key.Name);
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FIntent := AIntent;
end;

function NyxBindCollection(const AOwner: TNyxStudioBindingOwner;
  AProjection: TNyxCollectionProjection; const ASpec: TNyxCollectionViewSpec):
  TNyxCollectionChange;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccBind;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FSpec := ASpec.Copy;
  Result.FSpec.Validate;

  if not Result.FSpec.Defined or (Ord(AProjection) < Ord(Low(TNyxCollectionProjection))) or
    (Ord(AProjection) > Ord(High(TNyxCollectionProjection))) then
  begin
    raise ENyxCollection.Create('Bind requires a defined view and a valid projection');
  end;

  if Result.FSpec.HasTableWorkbenchPolicy and (AProjection <> cpTable) then
  begin
    { The named bind knows its presentation family at construction. Refuse
      unsupported action intent here, before a semantic patch can advertise
      a value its schema and eventual runtime mount would both reject. }
    raise ENyxCollection.Create('Authored workbench actions require a table binding');
  end;

  if Result.FSpec.HasPresentationPolicy and (AProjection <> cpTable) then
  begin
    { Text wrapping/density has the same known table-only authoring boundary;
      neutral reusable descriptors still defer actual projection validation. }
    raise ENyxCollection.Create('Table text and density policies require a table binding');
  end;
  Result.FKey := NyxCollection(ASpec.Key.Name);
  Result.FProjection := AProjection;
end;

function NyxSetCollectionQuery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AQuery: TNyxCollectionQuery): TNyxCollectionChange;
begin
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccQuery;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FKey := NyxCollection(AKey.Name);

  if (Ord(AProjection) < Ord(Low(TNyxCollectionProjection))) or
    (Ord(AProjection) > Ord(High(TNyxCollectionProjection))) then
  begin
    raise ENyxCollection.Create('Query edit requires a valid collection projection');
  end;
  Result.FProjection := AProjection;
  Result.FQuery := AQuery.Copy;
  Result.FQuery.ToData;
end;

function NyxUseDefaultCollectionTypeAhead(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection):
  TNyxCollectionChange;
begin

  if not (AProjection in [cpList, cpTree]) then
  begin
    raise ENyxCollection.Create('Typeahead edits require a list or tree projection');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccTypeAhead;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FKey := NyxCollection(AKey.Name);
  Result.FProjection := AProjection;
  Result.FTypeAheadDeclared := False;
end;

function NyxSetCollectionTypeAhead(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxTypeAheadOptions): TNyxCollectionChange;
begin
  APolicy.Validate;
  Result := NyxUseDefaultCollectionTypeAhead(AOwner, AKey, AProjection);
  Result.FTypeAheadDeclared := True;
  Result.FTypeAhead := APolicy;
end;

function NyxUseDefaultCollectionInspection(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection):
  TNyxCollectionChange;
begin

  if not (AProjection in [cpList, cpTable, cpTree]) then
  begin
    raise ENyxCollection.Create('Inspection requires a valid collection projection');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccInspection;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FKey := NyxCollection(AKey.Name);
  Result.FProjection := AProjection;
  Result.FInspectionDeclared := False;
end;

function NyxSetCollectionInspection(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxCollectionInspectionPolicy): TNyxCollectionChange;
begin
  APolicy.Validate;

  if (AProjection = cpTree) and (APolicy.DefaultFollow = ncfLastVisible) then
  begin
    raise ENyxCollection.Create('Tree inspection supports keep-position, not follow-last');
  end;
  Result := NyxUseDefaultCollectionInspection(AOwner, AKey, AProjection);
  Result.FInspectionDeclared := True;
  Result.FInspection := APolicy;
end;

function NyxUseDefaultCollectionDraftRecovery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection): TNyxCollectionChange;
begin

  if not (AProjection in [cpList, cpTable, cpTree]) then
  begin
    raise ENyxCollection.Create('Recovery requires a valid collection projection');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccDraftRecovery;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FKey := NyxCollection(AKey.Name);
  Result.FProjection := AProjection;
end;

function NyxSetCollectionDraftRecovery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxCollectionDraftRecoveryPolicy): TNyxCollectionChange;
begin
  APolicy.Validate;
  Result := NyxUseDefaultCollectionDraftRecovery(AOwner, AKey, AProjection);
  Result.FDraftRecoveryDeclared := True;
  Result.FDraftRecovery := APolicy;
end;

function NyxUseDefaultCollectionTableQuery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection):
  TNyxCollectionChange;
begin

  if AProjection <> cpTable then
  begin
    raise ENyxCollection.Create('Table query controls require the table projection');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccTableQuery;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FKey := NyxCollection(AKey.Name);
  Result.FProjection := AProjection;
end;

function NyxSetCollectionTableQuery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxTableQueryPolicy): TNyxCollectionChange;
begin
  APolicy.Validate;
  Result := NyxUseDefaultCollectionTableQuery(AOwner, AKey, AProjection);
  Result.FQueryControlsDeclared := True;
  Result.FTableQuery := APolicy;
end;

function NyxUseDefaultCollectionColumnQuery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
begin

  if not AField.Defined then
  begin
    raise ENyxCollection.Create('Column query controls require an exact typed field');
  end;
  Result := NyxUseDefaultCollectionTableQuery(AOwner, AKey, AProjection);
  Result.FKind := ccColumnQuery;
  Result.FField := AField;
end;

function NyxSetCollectionColumnQuery(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef;
  const APolicy: TNyxColumnQueryPolicy): TNyxCollectionChange;
begin
  APolicy.Validate(AField.Kind);
  Result := NyxUseDefaultCollectionColumnQuery(AOwner, AKey, AProjection, AField);
  Result.FQueryControlsDeclared := True;
  Result.FColumnQuery := APolicy;
end;

function NyxUseDefaultCollectionTableLayout(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection): TNyxCollectionChange;
begin

  if AProjection <> cpTable then
  begin
    raise ENyxCollection.Create('Authored layout controls require the table projection');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccTableLayout;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FKey := NyxCollection(AKey.Name);
  Result.FProjection := AProjection;
end;

function NyxSetCollectionTableLayout(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxTableLayoutPolicy): TNyxCollectionChange;
begin
  APolicy.Validate;
  Result := NyxUseDefaultCollectionTableLayout(AOwner, AKey, AProjection);
  Result.FLayoutDeclared := True;
  Result.FTableLayout := APolicy.Copy;
end;

function NyxUseDefaultCollectionColumnLayout(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
begin

  if not AField.Defined then
  begin
    raise ENyxCollection.Create('Column layout controls require an exact typed field');
  end;
  Result := NyxUseDefaultCollectionTableLayout(AOwner, AKey, AProjection);
  Result.FKind := ccColumnLayout;
  Result.FField := AField;
end;

function NyxSetCollectionColumnLayout(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef;
  const APolicy: TNyxColumnLayoutPolicy): TNyxCollectionChange;
begin
  APolicy.Validate;
  Result := NyxUseDefaultCollectionColumnLayout(AOwner, AKey, AProjection, AField);
  Result.FLayoutDeclared := True;
  Result.FColumnLayout := APolicy.Copy;
end;

function NyxUseDefaultCollectionColumnFormat(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
begin
  Result := NyxUseDefaultCollectionColumnLayout(AOwner, AKey, AProjection, AField);
  Result.FKind := ccColumnFormat;
end;

function NyxSetCollectionColumnFormat(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef;
  const APolicy: TNyxValueFormat): TNyxCollectionChange;
begin
  APolicy.ValidateKind(AField.Kind);
  Result := NyxUseDefaultCollectionColumnFormat(AOwner, AKey, AProjection, AField);
  Result.FLayoutDeclared := True;
  Result.FColumnFormat := APolicy.Copy;
end;

function NyxMoveCollectionColumn(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; const AField: TNyxStudioCollectionFieldRef;
  ADirection: TNyxCollectionColumnMove): TNyxCollectionChange;
begin

  if (Ord(ADirection) < Ord(Low(TNyxCollectionColumnMove))) or
    (Ord(ADirection) > Ord(High(TNyxCollectionColumnMove))) then
  begin
    raise ENyxCollection.Create('Column move requires a known direction');
  end;
  Result := NyxUseDefaultCollectionColumnLayout(AOwner, AKey, cpTable, AField);
  Result.FKind := ccColumnMove;
  Result.FColumnMove := ADirection;
end;

function NyxUseDefaultCollectionTableWorkbench(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection):
  TNyxCollectionChange;
begin

  if AProjection <> cpTable then
  begin
    raise ENyxCollection.Create('Authored workbench actions require the table projection');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccTableWorkbench;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FKey := NyxCollection(AKey.Name);
  Result.FProjection := AProjection;
end;

function NyxSetCollectionTableWorkbench(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxTableWorkbenchPolicy): TNyxCollectionChange;
begin
  APolicy.Validate;
  Result := NyxUseDefaultCollectionTableWorkbench(AOwner, AKey, AProjection);
  Result.FWorkbenchDeclared := True;
  Result.FTableWorkbench := APolicy.Copy;
end;

function NyxUseDefaultCollectionTablePresentation(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection):
  TNyxCollectionChange;
begin

  if AProjection <> cpTable then
  begin
    raise ENyxCollection.Create('Row spacing requires the table projection');
  end;
  Result := Default(TNyxCollectionChange);
  Result.FKind := ccTablePresentation;
  Result.FOwner := NyxBindingOwner(AOwner.ID);
  Result.FKey := NyxCollection(AKey.Name);
  Result.FProjection := AProjection;
end;

function NyxSetCollectionTablePresentation(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const APolicy: TNyxTablePresentationPolicy): TNyxCollectionChange;
begin
  APolicy.Validate;

  if not APolicy.Defined then
  begin
    raise ENyxCollection.Create('Set row spacing requires a defined policy; use the explicit default reset');
  end;
  Result := NyxUseDefaultCollectionTablePresentation(AOwner, AKey, AProjection);
  Result.FPresentationDeclared := True;
  Result.FTablePresentation := APolicy.Copy;
end;

function NyxUseDefaultCollectionColumnText(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
begin

  if not AField.Defined then
  begin
    raise ENyxCollection.Create('Column text layout requires an exact typed field');
  end;
  Result := NyxUseDefaultCollectionTablePresentation(AOwner, AKey, AProjection);
  Result.FKind := ccColumnText;
  Result.FField := AField;
end;

function NyxSetCollectionColumnText(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef;
  const APolicy: TNyxColumnTextPolicy): TNyxCollectionChange;
begin
  APolicy.Validate;

  if not APolicy.Defined then
  begin
    raise ENyxCollection.Create('Set column text requires a defined policy; use the explicit default reset');
  end;
  Result := NyxUseDefaultCollectionColumnText(AOwner, AKey, AProjection, AField);
  Result.FPresentationDeclared := True;
  Result.FColumnText := APolicy.Copy;
end;

function NyxUseDefaultCollectionColumnAlignment(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef): TNyxCollectionChange;
begin
  Result := NyxUseDefaultCollectionColumnText(AOwner, AKey, AProjection, AField);
  Result.FKind := ccColumnAlignment;
end;

function NyxSetCollectionColumnAlignment(const AOwner: TNyxStudioBindingOwner;
  const AKey: TNyxCollectionRef; AProjection: TNyxCollectionProjection;
  const AField: TNyxStudioCollectionFieldRef;
  const APolicy: TNyxColumnAlignmentPolicy): TNyxCollectionChange;
begin
  APolicy.Validate;

  if not APolicy.Defined then
  begin
    raise ENyxCollection.Create('Set column alignment requires explicit axes; use the default reset');
  end;
  Result := NyxUseDefaultCollectionColumnAlignment(AOwner, AKey, AProjection, AField);
  Result.FPresentationDeclared := True;
  Result.FColumnAlignment := APolicy.Copy;
end;

constructor TNyxCollectionPatch.Create(const AChanges: array of TNyxCollectionChange);
var
  LIndex: Integer;
  LRow: Integer;
begin
  inherited Create;

  if (Length(AChanges) < 1) or (Length(AChanges) > NyxMaximumCollectionChanges) then
  begin
    raise ENyxCollection.Create('Collection group requires 1..32 changes');
  end;
  SetLength(FChanges, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    FChanges[LIndex] := AChanges[LIndex];
    FChanges[LIndex].FQuery := AChanges[LIndex].FQuery.Copy;
    { Immutable public records are copied explicitly at the owned group boundary;
      caller array replacement cannot mutate a patch on either compiler. }
    FChanges[LIndex].FItems := nil;
    SetLength(FChanges[LIndex].FItems, Length(AChanges[LIndex].FItems));
    for LRow := 0 to High(AChanges[LIndex].FItems) do
    begin
      FChanges[LIndex].FItems[LRow] := AChanges[LIndex].FItems[LRow].Copy;
    end;
  end;
end;

function TNyxCollectionPatch.GetCount: Integer;
begin
  Result := Length(FChanges);
end;

procedure SelectCollectionOwner(ASession: TNyxStudioSession;
  const AOwner: TNyxStudioBindingOwner; AProjection: TNyxCollectionProjection);
var
  LNode: TNyxNode;
  LProjected: TNyxNode;
begin
  LNode := ASession.Document.Find(AOwner.ID);

  if LNode = nil then
  begin
    raise ENyxCollection.Create('The exact authored collection owner is missing');
  end;
  while LNode.Parent <> nil do
  begin
    LNode := LNode.Parent;
  end;
  ASession.Activate(LNode.ID);
  ASession.Select(AOwner.ID);
  LProjected := ASession.SelectedProjection;
  try

    if (LProjected = nil) or
      (LProjected.ProjectionKind <> CCollectionProjections[AProjection]) then
    begin
      raise ENyxCollection.Create('The authored collection projection changed');
    end;
  finally
    LProjected.Free;
  end;
end;

function TNyxCollectionPatch.Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
var
  LSession: TNyxStudioSession;
  LChange: TNyxCollectionChange;
  LData: INyxCollectionSnapshot;
  LSchema: TNyxCollectionSchema;
  LNextSchema: TNyxCollectionSchema;
  LField: TNyxCollectionField;
  LDefinition: TNyxCollectionField;
  LItems: array of TNyxCollectionItem;
  LRow: TNyxCollectionItem;
  LIndex: Integer;
  LColumn: Integer;
  LItemIndex: Integer;
  LOther: Integer;
  LFound: Boolean;
  LValue: TNyxStateValue;
  LProjected: TNyxNode;
  LIntent: TNyxStudioCollectionIntent;
  LSpec: TNyxCollectionViewSpec;
begin
  LSession := TNyxStudioSession.Create(APair);
  try

    if LSession.DraftSource <> LSession.Source then
    begin
      raise ENyxCollection.Create('Resolve the pending Pascal draft before collection edits');
    end;
    for LIndex := 0 to High(FChanges) do
    begin
      LChange := FChanges[LIndex];
      case LChange.FKind of
        ccIntent:
          begin

            if NyxStudioCollectionViewAction(LChange.FIntent.Action) then
            begin
              SelectCollectionOwner(LSession, LChange.FOwner, LChange.FIntent.Projection);
            end;
            LSession.ApplyCollectionIntent(LChange.FIntent);
          end;
        ccBind:
          begin
            SelectCollectionOwner(LSession, LChange.FOwner, LChange.FProjection);
            LSession.SetCollectionView(LChange.FSpec);
          end;
        ccTypeAhead:
          begin
            SelectCollectionOwner(LSession, LChange.FOwner, LChange.FProjection);
            LProjected := LSession.SelectedProjection;
            try

              if (LProjected = nil) or not LProjected.HasCollectionView or
                not LProjected.CollectionView.Defined or
                (LProjected.CollectionView.Key.Name <> LChange.FKey.Name) then
              begin
                raise ENyxCollection.Create('Typeahead edit requires the exact bound collection');
              end;
              LSpec := LProjected.CollectionView;

              if LChange.FTypeAheadDeclared then
              begin
                LSpec := LSpec.TypeAhead(LChange.FTypeAhead);
              end
              else
              begin
                LSpec := LSpec.UseDefaultTypeAhead;
              end;
              { Use ordinary Studio binding admission in this independent
                candidate. The agent layer owns the single final paired Undo. }
              LSession.SetCollectionView(LSpec);
            finally
              LProjected.Free;
            end;
          end;
        ccInspection, ccDraftRecovery:
          begin
            SelectCollectionOwner(LSession, LChange.FOwner, LChange.FProjection);
            LProjected := LSession.SelectedProjection;
            try

              if (LProjected = nil) or not LProjected.HasCollectionView or
                not LProjected.CollectionView.Defined or
                (LProjected.CollectionView.Key.Name <> LChange.FKey.Name) then
              begin
                raise ENyxCollection.Create('Policy edit requires the exact bound collection');
              end;
              LSpec := LProjected.CollectionView;

              if LChange.FKind = ccDraftRecovery then
              begin

                if LChange.FDraftRecoveryDeclared then
                begin
                  LSpec := LSpec.DraftRecovery(LChange.FDraftRecovery);
                end
                else
                begin
                  LSpec := LSpec.UseDefaultDraftRecovery;
                end;
              end
              else if LChange.FInspectionDeclared then
              begin
                LSpec := LSpec.Inspection(LChange.FInspection);
              end
              else
              begin
                LSpec := LSpec.UseDefaultInspection;
              end;
              { Use ordinary Studio binding admission in this independent
                candidate. The agent layer owns the single final paired Undo. }
              LSession.SetCollectionView(LSpec);
            finally
              LProjected.Free;
            end;
          end;
        ccQuery, ccTableQuery, ccColumnQuery, ccTableLayout, ccColumnLayout,
        ccColumnFormat, ccTableWorkbench, ccTablePresentation, ccColumnText, ccColumnAlignment,
        ccColumnMove:
          begin
            SelectCollectionOwner(LSession, LChange.FOwner, LChange.FProjection);
            LProjected := LSession.SelectedProjection;
            try

              if (LProjected = nil) or not LProjected.HasCollectionView or
                not LProjected.CollectionView.Defined or
                (LProjected.CollectionView.Key.Name <> LChange.FKey.Name) then
              begin
                raise ENyxCollection.Create('Policy edit requires the exact bound collection');
              end;
              LIntent := Default(TNyxStudioCollectionIntent);
              LIntent.Key := LChange.FKey;
              LIntent.Projection := LChange.FProjection;
              case LChange.FKind of
                ccQuery:
                  begin
                    LIntent.Action := scaQuery;
                    LIntent.Query := LChange.FQuery.Copy;
                  end;
                ccTableQuery:
                  begin
                    LIntent.Action := scaDefaultTableQuery;

                    if LChange.FQueryControlsDeclared then
                    begin
                      LIntent.Action := scaTableQuery;
                      LIntent.TableQuery := LChange.FTableQuery;
                    end;
                  end;
                ccColumnQuery:
                  begin
                    LIntent.Action := scaDefaultColumnQuery;
                    LIntent.Field := LChange.FField;

                    if LChange.FQueryControlsDeclared then
                    begin
                      LIntent.Action := scaColumnQuery;
                      LIntent.ColumnQuery := LChange.FColumnQuery;
                    end;
                  end;
                ccTableLayout:
                  begin
                    LIntent.Action := scaDefaultTableLayout;

                    if LChange.FLayoutDeclared then
                    begin
                      LIntent.Action := scaTableLayout;
                      LIntent.TableLayout := LChange.FTableLayout.Copy;
                    end;
                  end;
                ccColumnLayout:
                  begin
                    LIntent.Action := scaDefaultColumnLayout;
                    LIntent.Field := LChange.FField;

                    if LChange.FLayoutDeclared then
                    begin
                      LIntent.Action := scaColumnLayout;
                      LIntent.ColumnLayout := LChange.FColumnLayout.Copy;
                    end;
                  end;
                ccColumnFormat:
                  begin
                    LIntent.Action := scaDefaultColumnFormat;
                    LIntent.Field := LChange.FField;

                    if LChange.FLayoutDeclared then
                    begin
                      LIntent.Action := scaColumnFormat;
                      LIntent.ColumnFormat := LChange.FColumnFormat.Copy;
                    end;
                  end;
                ccTableWorkbench:
                  begin
                    LIntent.Action := scaDefaultTableWorkbench;

                    if LChange.FWorkbenchDeclared then
                    begin
                      LIntent.Action := scaTableWorkbench;
                      LIntent.TableWorkbench := LChange.FTableWorkbench.Copy;
                    end;
                  end;
                ccTablePresentation:
                  begin
                    LIntent.Action := scaDefaultTablePresentation;

                    if LChange.FPresentationDeclared then
                    begin
                      LIntent.Action := scaTablePresentation;
                      LIntent.TablePresentation := LChange.FTablePresentation.Copy;
                    end;
                  end;
                ccColumnText:
                  begin
                    LIntent.Action := scaDefaultColumnText;
                    LIntent.Field := LChange.FField;

                    if LChange.FPresentationDeclared then
                    begin
                      LIntent.Action := scaColumnText;
                      LIntent.ColumnText := LChange.FColumnText.Copy;
                    end;
                  end;
                ccColumnAlignment:
                  begin
                    LIntent.Action := scaDefaultColumnAlignment;
                    LIntent.Field := LChange.FField;

                    if LChange.FPresentationDeclared then
                    begin
                      LIntent.Action := scaColumnAlignment;
                      LIntent.ColumnAlignment := LChange.FColumnAlignment.Copy;
                    end;
                  end;
                ccColumnMove:
                  begin
                    LIntent.Field := LChange.FField;

                    if LChange.FColumnMove = nccmEarlier then
                    begin
                      LIntent.Action := scaColumnEarlier;
                    end
                    else
                    begin
                      LIntent.Action := scaColumnLater;
                    end;
                  end;
              else
                begin
                  raise ENyxCollection.Create('Unexpected baseline-qualified policy change');
                end;
              end;
              { The candidate owns the current binding/schema at this ordered
                step. Ordinary Studio admission validates the same complete
                replacement, without sending private form metadata to agents.
                Recompute after every prior edit, including another policy.
                ApplyCollectionIntent also preserves the pending-draft guard
                when accepted and draft bytes happen to be equal. }

              if LChange.FKind in [ccTableLayout, ccColumnLayout, ccColumnFormat,
                ccTableWorkbench, ccTablePresentation, ccColumnText, ccColumnAlignment,
                ccColumnMove] then
              begin
                LIntent.LayoutBaseline := NyxQueryEditorBaseline(
                  LSession.Document.Collections.Snapshot(LChange.FKey).Schema,
                  LProjected.CollectionView);
              end
              else
              begin
                LIntent.QueryBaseline := NyxQueryEditorBaseline(
                  LSession.Document.Collections.Snapshot(LChange.FKey).Schema,
                  LProjected.CollectionView);
              end;
              LSession.ApplyCollectionIntent(LIntent);
            finally
              LProjected.Free;
            end;
          end;
        ccDefine:
          begin

            if LSession.Document.Collections.Has(LChange.FKey) then
            begin
              raise ENyxCollection.Create('Named collection already exists');
            end;
            LSession.DefineCollection(LChange.FKey, LChange.FSchema, LChange.FItems);
          end;
      else
        begin
          { Editing materialized resource rows must not silently replace their
            saved recipe with a static seed. Source editing belongs to Resources
            authoring; runtime stores remain independently editable. }

          if NyxResourceCollections(LSession.Document.Collections).HasSource(LChange.FKey) then
          begin
            raise ENyxCollection.Create('Edit this collection through its resource row recipe');
          end;
          LData := LSession.Document.Collections.Snapshot(LChange.FKey);
          LSchema := LData.Schema;
          SetLength(LItems, LData.Count);
          for LOther := 0 to LData.Count - 1 do
          begin
            LItems[LOther] := LData.ItemAt(LOther).Copy;
          end;
          case LChange.FKind of
            ccField, ccRemoveField:
              begin
                LNextSchema := NyxCollectionSchema;
                LFound := False;

                if LChange.FKind = ccField then
                begin
                  LDefinition := LChange.FSchema.FieldAt(0);
                end;
                for LColumn := 0 to LSchema.Count - 1 do
                begin
                  LField := LSchema.FieldAt(LColumn);

                  if ((LChange.FKind = ccField) and (LField.Name = LDefinition.Name)) or
                    ((LChange.FKind = ccRemoveField) and
                     (LField.Name = LChange.FField.Name)) then
                  begin
                    LFound := True;

                    if LChange.FKind = ccField then
                    begin

                      if LField.Kind <> LDefinition.Kind then
                      begin
                        raise ENyxCollection.Create('Field update cannot change scalar family');
                      end;
                      LNextSchema := LNextSchema.Field(LDefinition.Name,
                        LDefinition.DefaultValue, LDefinition.Domain);
                    end
                    else if LField.Kind <> LChange.FField.Kind then
                    begin
                      raise ENyxCollection.Create('Field removal family changed');
                    end;
                  end
                  else
                  begin
                    LNextSchema := LNextSchema.Field(LField.Name,
                      LField.DefaultValue, LField.Domain);
                  end;
                end;

                if not LFound then
                begin

                  if LChange.FKind = ccRemoveField then
                  begin
                    raise ENyxCollection.Create('Removed field no longer exists');
                  end;
                  LNextSchema := LNextSchema.Field(LDefinition.Name,
                    LDefinition.DefaultValue, LDefinition.Domain);
                end;

                if LChange.FKind = ccRemoveField then
                begin
                  for LOther := 0 to High(LItems) do
                  begin
                    LRow := NyxCollectionItem(LItems[LOther].Ref);
                    for LColumn := 0 to LItems[LOther].Count - 1 do
                    begin

                      if LItems[LOther].FieldName(LColumn) <> LChange.FField.Name then
                      begin
                        LRow := PutValue(LRow, LItems[LOther].FieldName(LColumn),
                          LItems[LOther].FieldValue(LColumn));
                      end;
                    end;
                    LItems[LOther] := LRow;
                  end;
                end;
                LSchema := LNextSchema;
              end;
            ccAppend:
              begin

                if LData.IndexOf(LChange.FItem.Ref) >= 0 then
                begin
                  raise ENyxCollection.Create('Appended row already exists');
                end;
                SetLength(LItems, Length(LItems) + 1);
                LItems[High(LItems)] := LChange.FItem.Copy;
              end;
            ccUpdateRow, ccMoveRow:
              begin
                LItemIndex := LData.IndexOf(LChange.FItem.Ref);

                if LItemIndex < 0 then
                begin
                  raise ENyxCollection.Create('The exact scoped row is missing');
                end;

                if LChange.FKind = ccMoveRow then
                begin

                  if LChange.FIndex >= LData.Count then
                  begin
                    raise ENyxCollection.Create('Final row index exceeds this collection');
                  end;
                  LRow := LItems[LItemIndex];

                  if LItemIndex < LChange.FIndex then
                  begin
                    for LOther := LItemIndex to LChange.FIndex - 1 do
                    begin
                      LItems[LOther] := LItems[LOther + 1];
                    end;
                  end
                  else
                  begin
                    for LOther := LItemIndex downto LChange.FIndex + 1 do
                    begin
                      LItems[LOther] := LItems[LOther - 1];
                    end;
                  end;
                  LItems[LChange.FIndex] := LRow;
                end
                else
                begin
                  LRow := LItems[LItemIndex];
                  for LColumn := 0 to LChange.FItem.Count - 1 do
                  begin
                    LValue := LChange.FItem.FieldValue(LColumn);
                    LFound := False;
                    for LOther := 0 to LSchema.Count - 1 do
                    begin
                      LField := LSchema.FieldAt(LOther);

                      if LField.Name = LChange.FItem.FieldName(LColumn) then
                      begin

                        if LField.Kind <> LValue.Kind then
                        begin
                          raise ENyxCollection.Create('Updated cell family changed');
                        end;
                        LFound := True;
                        Break;
                      end;
                    end;

                    if not LFound then
                    begin
                      raise ENyxCollection.Create('Updated cell field does not exist');
                    end;
                    LRow := PutValue(LRow, LChange.FItem.FieldName(LColumn), LValue);
                  end;
                  LItems[LItemIndex] := LRow;
                end;
              end;
          else
            begin
              raise ENyxCollection.Create('Unknown collection data change');
            end;
          end;
          LSession.DefineCollection(LChange.FKey, LSchema, LItems);
        end;
      end;
    end;
    Result := LSession.ProjectSnapshot;
  finally
    LSession.Free;
  end;
end;

{ All raw member names and choices below are confined to this explicit protocol
  boundary. The ordinary authoring and candidate commands above use Pascal types. }
procedure Fields(const AData: TNyxDataValue; const AAllowed: TNyxText;
  ACount: Integer = -1);
var
  LIndex: Integer;
begin
  AData.Validate;

  if (AData.Kind <> ndObject) or ((ACount >= 0) and (AData.Count <> ACount)) then
  begin
    raise ENyxCollection.Create('Malformed collection operation object');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin

    if (Pos('|', AData.Key(LIndex)) > 0) or
      (Pos('|' + AData.Key(LIndex) + '|', AAllowed) = 0) then
    begin
      raise ENyxCollection.Create('Unknown collection operation member: ' + AData.Key(LIndex));
    end;
  end;
end;

function ReadKind(const AData: TNyxDataValue): TNyxStateKind;
var
  LKind: TNyxStateKind;
begin
  for LKind := Low(TNyxStateKind) to High(TNyxStateKind) do
  begin

    if AData.AsText = NyxStateKindName(LKind) then
    begin
      Exit(LKind);
    end;
  end;
  raise ENyxCollection.Create('Unknown collection scalar family');
end;

function ReadValue(AKind: TNyxStateKind; const AData: TNyxDataValue): TNyxStateValue;
begin
  case Ord(AKind) of
    Ord(nskText):
      begin
        Result := TNyxStateValue.FromText(AData.AsText);
      end;
    Ord(nskBoolean):
      begin
        Result := TNyxStateValue.FromBoolean(AData.AsBoolean);
      end;
    Ord(nskInteger):
      begin
        Result := TNyxStateValue.FromInteger(AData.AsInteger);
      end;
    Ord(nskNumber):
      begin
        Result := TNyxStateValue.FromNumber(AData.AsNumber);
      end;
  else
    begin
      raise ENyxCollection.Create('Unknown collection scalar family');
    end;
  end;
end;

function ReadField(const AName: TNyxText; AKind: TNyxStateKind):
  TNyxStudioCollectionFieldRef;
begin
  case Ord(AKind) of
    Ord(nskText):
      begin
        Result := TNyxStudioCollectionFieldRef.Text(NyxTextField(AName));
      end;
    Ord(nskBoolean):
      begin
        Result := TNyxStudioCollectionFieldRef.Boolean(NyxBooleanField(AName));
      end;
    Ord(nskInteger):
      begin
        Result := TNyxStudioCollectionFieldRef.Integer(NyxIntegerField(AName));
      end;
    Ord(nskNumber):
      begin
        Result := TNyxStudioCollectionFieldRef.Number(NyxNumberField(AName));
      end;
  else
    begin
      raise ENyxCollection.Create('Unknown collection scalar family');
    end;
  end;
end;

function ReadSchema(const AData: TNyxDataValue): TNyxCollectionSchema;
var
  LIndex: Integer;
  LField: TNyxDataValue;
  LDomain: TNyxValueDomain;
  LValue: TNyxStateValue;
  LMember: Integer;
begin

  if (AData.Kind <> ndArray) or (AData.Count < 1) or
    (AData.Count > NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Schema requires 1..64 named fields');
  end;
  Result := NyxCollectionSchema;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LField := AData.Item(LIndex);
    Fields(LField, '|name|kind|default|domain|');
    LValue := ReadValue(ReadKind(LField.Field('kind')), LField.Field('default'));
    LDomain := NyxNoDomain;

    for LMember := 0 to LField.Count - 1 do
    begin

      if LField.Key(LMember) = 'domain' then
      begin
        LDomain := TNyxValueDomain.FromData(LField.Field('domain'));
      end;
    end;
    Result := Result.Field(LField.Field('name').AsText, LValue, LDomain);
  end;
end;

function ReadRow(const AKey: TNyxCollectionRef; const AID: TNyxText;
  const AValues: TNyxDataValue): TNyxCollectionItem;
var
  LIndex: Integer;
  LOther: Integer;
  LCell: TNyxDataValue;
  LName: TNyxText;
begin

  if (AValues.Kind <> ndArray) or (AValues.Count > NyxMaximumCollectionFields) then
  begin
    raise ENyxCollection.Create('Row values require an array of at most 64 typed cells');
  end;
  Result := NyxCollectionItem(NyxItem(AKey, AID));
  for LIndex := 0 to AValues.Count - 1 do
  begin
    LCell := AValues.Item(LIndex);
    Fields(LCell, '|field|kind|value|', 3);
    LName := LCell.Field('field').AsText;
    for LOther := 0 to Result.Count - 1 do
    begin

      if Result.FieldName(LOther) = LName then
      begin
        raise ENyxCollection.Create('Duplicate row value field');
      end;
    end;
    Result := PutValue(Result, LName,
      ReadValue(ReadKind(LCell.Field('kind')), LCell.Field('value')));
  end;
end;

function ReadProjection(const AData: TNyxDataValue): TNyxCollectionProjection;
var
  LProjection: TNyxCollectionProjection;
begin
  for LProjection := Low(TNyxCollectionProjection) to High(TNyxCollectionProjection) do
  begin

    if AData.AsText = CCollectionProjections[LProjection] then
    begin
      Exit(LProjection);
    end;
  end;
  raise ENyxCollection.Create('Projection must be list, table or tree');
end;

function IntentData(const AChange: TNyxCollectionChange): TNyxDataValue;
var
  LIntent: TNyxStudioCollectionIntent;
  LFields: array of TNyxDataField;
  LIndex: Integer;
  LSizeValue: Integer;

  procedure Add(const AName: TNyxText; const AValue: TNyxDataValue);
  begin
    SetLength(LFields, Length(LFields) + 1);
    LFields[High(LFields)] := NyxField(AName, AValue);
  end;

begin
  LIntent := AChange.FIntent;
  LFields := nil;
  Add('op', NyxData('intent'));
  Add('action', NyxData(CCollectionActions[LIntent.Action]));
  Add('key', NyxData(LIntent.Key.Name));

  if LIntent.Action in [scaDraftRecovery, scaDefaultDraftRecovery] then
  begin
    Add('baseline', NyxData(LIntent.DraftRecoveryBaseline));

    if LIntent.Action = scaDraftRecovery then
    begin
      Add('draftRecovery', LIntent.DraftRecovery.ToData);
    end;
  end;

  if LIntent.Action in [scaInspection, scaDefaultInspection] then
  begin
    Add('baseline', NyxData(LIntent.InspectionBaseline));

    if LIntent.Action = scaInspection then
    begin
      Add('inspection', LIntent.Inspection.ToData);
    end;
  end;
  if LIntent.Action = scaQuery then
  begin
    Add('query', LIntent.Query.ToData);
  end;

  if LIntent.Action in [scaQuery, scaTableQuery, scaColumnQuery,
    scaDefaultTableQuery, scaDefaultColumnQuery] then
  begin
    Add('baseline', NyxData(LIntent.QueryBaseline));
  end;

  if LIntent.Action = scaTableQuery then
  begin
    Add('tableQuery', LIntent.TableQuery.ToData);
  end;

  if LIntent.Action = scaColumnQuery then
  begin
    Add('columnQuery', LIntent.ColumnQuery.ToData);
  end;

  if LIntent.Action in [scaTableLayout, scaColumnLayout,
    scaDefaultTableLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
    scaTableWorkbench, scaDefaultTableWorkbench,
    scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment, scaColumnEarlier, scaColumnLater] then
  begin
    Add('baseline', NyxData(LIntent.LayoutBaseline));
  end;

  if LIntent.Action = scaTableLayout then
  begin
    Add('tableLayout', LIntent.TableLayout.ToData);
  end;

  if LIntent.Action = scaColumnLayout then
  begin
    Add('columnLayout', LIntent.ColumnLayout.ToData);
  end;

  if LIntent.Action = scaColumnFormat then
  begin
    Add('columnFormat', LIntent.ColumnFormat.ToData);
  end;

  if LIntent.Action = scaTableWorkbench then
  begin
    Add('tableWorkbench', LIntent.TableWorkbench.ToData);
  end;

  if LIntent.Action = scaTablePresentation then
  begin
    Add('tablePresentation', LIntent.TablePresentation.ToData);
  end;

  if LIntent.Action = scaColumnText then
  begin
    Add('columnText', LIntent.ColumnText.ToData);
  end;

  if LIntent.Action = scaColumnAlignment then
  begin
    Add('columnAlignment', LIntent.ColumnAlignment.ToData);
  end;

  if NyxStudioCollectionViewAction(LIntent.Action) then
  begin
    Add('owner', NyxData(AChange.FOwner.ID));
    Add('projection', NyxData(CCollectionProjections[LIntent.Projection]));
  end;

  if LIntent.Action in [scaAddRow, scaRemoveRow, scaCell] then
  begin
    Add('item', NyxData(LIntent.Item.ID));
  end;

  if LIntent.Action in [scaDefault, scaCell, scaTitle, scaMode,
    scaRemoveColumn, scaAddColumn, scaColumnContent, scaColumnFixed,
    scaColumnFlex, scaColumnMinimum, scaColumnMaximum, scaColumnDefault,
    scaColumnQuery, scaDefaultColumnQuery, scaColumnLayout,
    scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
    scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment, scaColumnEarlier, scaColumnLater] then
  begin
    Add('field', NyxData(LIntent.Field.Name));
    Add('kind', NyxData(NyxStateKindName(LIntent.Field.Kind)));
  end;

  if LIntent.Action = scaAddField then
  begin
    Add('kind', NyxData(NyxStateKindName(LIntent.Kind)));
  end;

  if LIntent.Action in [scaDefault, scaCell] then
  begin
    Add('value', NyxStateValueData(ParseNyxStudioStateInput(LIntent.Input, LIntent.Value)));
  end;

  if LIntent.Action = scaTitle then
  begin
    Add('title', NyxData(LIntent.Value));
  end;

  if LIntent.Action in [scaColumnFixed, scaColumnFlex, scaColumnMinimum, scaColumnMaximum,
    scaPageSize, scaInitialPage] then
  begin

    if not TryNyxStateInteger(LIntent.Value, LSizeValue) then
    begin
      raise ENyxCollection.Create('Collection extent requires an exact Integer');
    end;
    Add('value', NyxData(LSizeValue));
  end;

  if LIntent.Action = scaMode then
  begin
    Add('editable', NyxData(LIntent.Mode = cmEditable));
  end;

  if LIntent.Action = scaParent then
  begin
    Add('parent', NyxNull);

    if LIntent.Field.Defined then
    begin
      LIndex := High(LFields);
      LFields[LIndex] := NyxField('parent', NyxData(LIntent.Field.Name));
    end;
  end;

  if LIntent.Action = scaScope then
  begin

    if LIntent.Scope = csInstance then
    begin
      Add('scope', NyxData('instance'));
    end
    else
    begin
      Add('scope', NyxData('application'));
    end;
  end;

  if LIntent.Action = scaSelection then
  begin

    if LIntent.SelectionMode = nsmMultiple then
    begin
      Add('selection', NyxData('multiple'));
    end
    else
    begin
      Add('selection', NyxData('single'));
    end;
  end;
  Result := NyxObject(LFields);
end;

function ReadIntent(const AData: TNyxDataValue): TNyxCollectionChange;
var
  LIntent: TNyxStudioCollectionIntent;
  LAction: TNyxStudioCollectionAction;
  LFound: Boolean;
  LAllowed: TNyxText;
  LRequired: Integer;
  LValue: TNyxStateValue;
begin
  LIntent := Default(TNyxStudioCollectionIntent);
  LFound := False;
  for LAction := Low(TNyxStudioCollectionAction) to High(TNyxStudioCollectionAction) do
  begin

    if AData.Field('action').AsText = CCollectionActions[LAction] then
    begin
      LIntent.Action := LAction;
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Unknown ordinary collection action');
  end;
  LIntent.Key := NyxCollection(AData.Field('key').AsText);
  LAllowed := '|op|action|key|';
  LRequired := 3;

  if LIntent.Action in [scaDraftRecovery, scaDefaultDraftRecovery] then
  begin
    LAllowed := LAllowed + 'baseline|';
    Inc(LRequired);
    LIntent.DraftRecoveryBaseline := AData.Field('baseline').AsText;

    if LIntent.Action = scaDraftRecovery then
    begin
      LAllowed := LAllowed + 'draftRecovery|';
      Inc(LRequired);
      LIntent.DraftRecovery := TNyxCollectionDraftRecoveryPolicy.FromData(AData.Field('draftRecovery'));
    end;
  end;

  if LIntent.Action in [scaInspection, scaDefaultInspection] then
  begin
    LAllowed := LAllowed + 'baseline|';
    Inc(LRequired);
    LIntent.InspectionBaseline := AData.Field('baseline').AsText;

    if LIntent.Action = scaInspection then
    begin
      LAllowed := LAllowed + 'inspection|';
      Inc(LRequired);
      LIntent.Inspection := TNyxCollectionInspectionPolicy.FromData(AData.Field('inspection'));
    end;
  end;
  if LIntent.Action = scaQuery then
  begin
    LAllowed := LAllowed + 'query|';
    Inc(LRequired);
    LIntent.Query := TNyxCollectionQuery.FromData(AData.Field('query'));
  end;

  if LIntent.Action in [scaQuery, scaTableQuery, scaColumnQuery,
    scaDefaultTableQuery, scaDefaultColumnQuery] then
  begin
    LAllowed := LAllowed + 'baseline|';
    Inc(LRequired);
    LIntent.QueryBaseline := AData.Field('baseline').AsText;
  end;

  if LIntent.Action = scaTableQuery then
  begin
    LAllowed := LAllowed + 'tableQuery|';
    Inc(LRequired);
    LIntent.TableQuery := TNyxTableQueryPolicy.FromData(AData.Field('tableQuery'));
  end;

  if LIntent.Action = scaColumnQuery then
  begin
    LAllowed := LAllowed + 'columnQuery|';
    Inc(LRequired);
    LIntent.ColumnQuery := TNyxColumnQueryPolicy.FromData(AData.Field('columnQuery'));
  end;

  if LIntent.Action in [scaTableLayout, scaColumnLayout,
    scaDefaultTableLayout, scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
    scaTableWorkbench, scaDefaultTableWorkbench,
    scaTablePresentation, scaDefaultTablePresentation, scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment, scaColumnEarlier, scaColumnLater] then
  begin
    LAllowed := LAllowed + 'baseline|';
    Inc(LRequired);
    LIntent.LayoutBaseline := AData.Field('baseline').AsText;
  end;

  if LIntent.Action = scaTableLayout then
  begin
    LAllowed := LAllowed + 'tableLayout|';
    Inc(LRequired);
    LIntent.TableLayout := TNyxTableLayoutPolicy.FromData(AData.Field('tableLayout'));
  end;

  if LIntent.Action = scaColumnLayout then
  begin
    LAllowed := LAllowed + 'columnLayout|';
    Inc(LRequired);
    LIntent.ColumnLayout := TNyxColumnLayoutPolicy.FromData(AData.Field('columnLayout'));
  end;

  if LIntent.Action = scaColumnFormat then
  begin
    LAllowed := LAllowed + 'columnFormat|';
    Inc(LRequired);
    LIntent.ColumnFormat := TNyxValueFormat.FromData(AData.Field('columnFormat'));
  end;

  if LIntent.Action = scaTableWorkbench then
  begin
    LAllowed := LAllowed + 'tableWorkbench|';
    Inc(LRequired);
    LIntent.TableWorkbench := TNyxTableWorkbenchPolicy.FromData(AData.Field('tableWorkbench'));
  end;

  if LIntent.Action = scaTablePresentation then
  begin
    LAllowed := LAllowed + 'tablePresentation|';
    Inc(LRequired);
    LIntent.TablePresentation := TNyxTablePresentationPolicy.FromData(AData.Field('tablePresentation'));
  end;

  if LIntent.Action = scaColumnText then
  begin
    LAllowed := LAllowed + 'columnText|';
    Inc(LRequired);
    LIntent.ColumnText := TNyxColumnTextPolicy.FromData(AData.Field('columnText'));
  end;

  if LIntent.Action = scaColumnAlignment then
  begin
    LAllowed := LAllowed + 'columnAlignment|';
    Inc(LRequired);
    LIntent.ColumnAlignment := TNyxColumnAlignmentPolicy.FromData(AData.Field('columnAlignment'));
  end;

  if NyxStudioCollectionViewAction(LIntent.Action) then
  begin
    LAllowed := LAllowed + 'owner|projection|';
    Inc(LRequired, 2);
    LIntent.Projection := ReadProjection(AData.Field('projection'));
  end;

  if LIntent.Action in [scaAddRow, scaRemoveRow, scaCell] then
  begin
    LAllowed := LAllowed + 'item|';
    Inc(LRequired);
    LIntent.Item := NyxItem(LIntent.Key, AData.Field('item').AsText);
  end;

  if LIntent.Action in [scaDefault, scaCell, scaTitle, scaMode,
    scaRemoveColumn, scaAddColumn, scaColumnContent, scaColumnFixed,
    scaColumnFlex, scaColumnMinimum, scaColumnMaximum, scaColumnDefault,
    scaColumnQuery, scaDefaultColumnQuery, scaColumnLayout,
    scaDefaultColumnLayout, scaColumnFormat, scaDefaultColumnFormat,
    scaColumnText, scaDefaultColumnText,
    scaColumnAlignment, scaDefaultColumnAlignment, scaColumnEarlier, scaColumnLater] then
  begin
    LAllowed := LAllowed + 'field|kind|';
    Inc(LRequired, 2);
    LIntent.Field := ReadField(AData.Field('field').AsText, ReadKind(AData.Field('kind')));
  end;

  if LIntent.Action = scaAddField then
  begin
    LAllowed := LAllowed + 'kind|';
    Inc(LRequired);
    LIntent.Kind := ReadKind(AData.Field('kind'));
  end;

  if LIntent.Action in [scaDefault, scaCell] then
  begin
    LAllowed := LAllowed + 'value|';
    Inc(LRequired);
    LValue := ReadValue(LIntent.Field.Kind, AData.Field('value'));
    LIntent.Input := NyxStudioStateInputFor(LValue);
    LIntent.Value := NyxStudioStateEditorText(LValue);
  end;

  if LIntent.Action = scaTitle then
  begin
    LAllowed := LAllowed + 'title|';
    Inc(LRequired);
    LIntent.Value := AData.Field('title').AsText;
  end;

  if LIntent.Action in [scaColumnFixed, scaColumnFlex, scaColumnMinimum, scaColumnMaximum,
    scaPageSize, scaInitialPage] then
  begin
    LAllowed := LAllowed + 'value|';
    Inc(LRequired);
    LValue := ReadValue(nskInteger, AData.Field('value'));
    LIntent.Value := NyxStudioStateEditorText(LValue);
  end;

  if LIntent.Action = scaMode then
  begin
    LAllowed := LAllowed + 'editable|';
    Inc(LRequired);

    if AData.Field('editable').AsBoolean then
    begin
      LIntent.Mode := cmEditable;
    end;
  end;

  if LIntent.Action = scaParent then
  begin
    LAllowed := LAllowed + 'parent|';
    Inc(LRequired);

    if AData.Field('parent').Kind <> ndNull then
    begin
      LIntent.Field := TNyxStudioCollectionFieldRef.Text(
        NyxTextField(AData.Field('parent').AsText));
    end;
  end;

  if LIntent.Action = scaScope then
  begin
    LAllowed := LAllowed + 'scope|';
    Inc(LRequired);

    if AData.Field('scope').AsText = 'instance' then
    begin
      LIntent.Scope := csInstance;
    end
    else if AData.Field('scope').AsText <> 'application' then
    begin
      raise ENyxCollection.Create('Unknown collection scope');
    end;
  end;

  if LIntent.Action = scaSelection then
  begin
    LAllowed := LAllowed + 'selection|';
    Inc(LRequired);

    if AData.Field('selection').AsText = 'multiple' then
    begin
      LIntent.SelectionMode := nsmMultiple;
    end
    else if AData.Field('selection').AsText <> 'single' then
    begin
      raise ENyxCollection.Create('Unknown collection selection mode');
    end;
  end;
  Fields(AData, LAllowed, LRequired);

  if NyxStudioCollectionViewAction(LIntent.Action) then
  begin
    Result := NyxCollectionIntentChange(NyxBindingOwner(AData.Field('owner').AsText), LIntent);
  end
  else
  begin
    Result := NyxCollectionIntentChange(LIntent);
  end;
end;

function RowData(const AItem: TNyxCollectionItem): TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LIndex: Integer;
  LValue: TNyxStateValue;
begin
  SetLength(LValues, AItem.Count);
  for LIndex := 0 to AItem.Count - 1 do
  begin
    LValue := AItem.FieldValue(LIndex);
    LValues[LIndex] := NyxObject([
      NyxField('field', NyxData(AItem.FieldName(LIndex))),
      NyxField('kind', NyxData(NyxStateKindName(LValue.Kind))),
      NyxField('value', NyxStateValueData(LValue))]);
  end;
  Result := NyxObject([NyxField('item', NyxData(AItem.Ref.ID)),
    NyxField('values', NyxArray(LValues))]);
end;

function SchemaData(const ASchema: TNyxCollectionSchema): TNyxDataValue;
var
  LFields: array of TNyxDataValue;
  LIndex: Integer;
  LField: TNyxCollectionField;
begin
  SetLength(LFields, ASchema.Count);
  for LIndex := 0 to ASchema.Count - 1 do
  begin
    LField := ASchema.FieldAt(LIndex);
    LFields[LIndex] := NyxObject([
      NyxField('name', NyxData(LField.Name)),
      NyxField('kind', NyxData(NyxStateKindName(LField.Kind))),
      NyxField('default', NyxStateValueData(LField.DefaultValue)),
      NyxField('domain', LField.Domain.ToData)]);
  end;
  Result := NyxArray(LFields);
end;

function TNyxCollectionPatch.ToData: TNyxDataValue;
var
  LValues: array of TNyxDataValue;
  LRows: array of TNyxDataValue;
  LChange: TNyxCollectionChange;
  LIndex: Integer;
  LRow: Integer;
  LOp: TNyxText;
  LRowData: TNyxDataValue;
  LPolicy: TNyxDataValue;
begin
  SetLength(LValues, GetCount);
  for LIndex := 0 to High(FChanges) do
  begin
    LChange := FChanges[LIndex];
    case LChange.FKind of
      ccDefine:
        begin
          SetLength(LRows, Length(LChange.FItems));
          for LRow := 0 to High(LChange.FItems) do
          begin
            LRows[LRow] := RowData(LChange.FItems[LRow]);
          end;
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('define')),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('fields', SchemaData(LChange.FSchema)),
            NyxField('rows', NyxArray(LRows))]);
        end;
      ccField:
        begin
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('field')),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('definition', SchemaData(LChange.FSchema).Item(0))]);
        end;
      ccRemoveField:
        begin
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('remove-field')),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('field', NyxData(LChange.FField.Name)),
            NyxField('kind', NyxData(NyxStateKindName(LChange.FField.Kind)))]);
        end;
      ccAppend, ccUpdateRow:
        begin
          LOp := 'append';

          if LChange.FKind = ccUpdateRow then
          begin
            LOp := 'update-row';
          end;
          LRowData := RowData(LChange.FItem);
          LValues[LIndex] := NyxObject([NyxField('op', NyxData(LOp)),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('item', LRowData.Field('item')),
            NyxField('values', LRowData.Field('values'))]);
        end;
      ccMoveRow:
        begin
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('move-row')),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('item', NyxData(LChange.FItem.Ref.ID)),
            NyxField('index', NyxData(LChange.FIndex))]);
        end;
      ccIntent:
        begin
          LValues[LIndex] := IntentData(LChange);
        end;
      ccBind:
        begin
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('bind')),
            NyxField('owner', NyxData(LChange.FOwner.ID)),
            NyxField('projection', NyxData(CCollectionProjections[LChange.FProjection])),
            NyxField('spec', LChange.FSpec.ToData)]);
        end;
      ccTypeAhead:
        begin
          LPolicy := NyxNull;

          if LChange.FTypeAheadDeclared then
          begin
            LPolicy := LChange.FTypeAhead.ToData;
          end;
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('typeahead')),
            NyxField('owner', NyxData(LChange.FOwner.ID)),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('projection', NyxData(CCollectionProjections[LChange.FProjection])),
            NyxField('policy', LPolicy)]);
        end;
      ccDraftRecovery:
        begin
          LPolicy := NyxNull;

          if LChange.FDraftRecoveryDeclared then
          begin
            LPolicy := LChange.FDraftRecovery.ToData;
          end;
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('draft-recovery')),
            NyxField('owner', NyxData(LChange.FOwner.ID)),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('projection', NyxData(CCollectionProjections[LChange.FProjection])),
            NyxField('policy', LPolicy)]);
        end;
      ccInspection:
        begin
          LPolicy := NyxNull;

          if LChange.FInspectionDeclared then
          begin
            LPolicy := LChange.FInspection.ToData;
          end;
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('inspection')),
            NyxField('owner', NyxData(LChange.FOwner.ID)),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('projection', NyxData(CCollectionProjections[LChange.FProjection])),
            NyxField('policy', LPolicy)]);
        end;
      ccQuery:
        begin
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('query')),
            NyxField('owner', NyxData(LChange.FOwner.ID)),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('projection', NyxData(CCollectionProjections[LChange.FProjection])),
            NyxField('query', LChange.FQuery.ToData)]);
        end;
      ccColumnMove:
        begin
          LOp := 'earlier';

          if LChange.FColumnMove = nccmLater then
          begin
            LOp := 'later';
          end;
          LValues[LIndex] := NyxObject([NyxField('op', NyxData('move-column')),
            NyxField('owner', NyxData(LChange.FOwner.ID)),
            NyxField('key', NyxData(LChange.FKey.Name)),
            NyxField('projection', NyxData('table')),
            NyxField('field', NyxData(LChange.FField.Name)),
            NyxField('kind', NyxData(NyxStateKindName(LChange.FField.Kind))),
            NyxField('direction', NyxData(LOp))]);
        end;
      ccTableQuery, ccColumnQuery, ccTableLayout, ccColumnLayout, ccColumnFormat,
      ccTableWorkbench, ccTablePresentation, ccColumnText, ccColumnAlignment:
        begin
          LOp := 'table-query';
          LPolicy := NyxNull;

          if LChange.FKind = ccColumnQuery then
          begin
            LOp := 'column-query';
          end;

          if LChange.FKind = ccTableLayout then
          begin
            LOp := 'table-layout';
          end;

          if LChange.FKind = ccColumnLayout then
          begin
            LOp := 'column-layout';
          end;

          if LChange.FKind = ccColumnFormat then
          begin
            LOp := 'column-format';
          end;

          if LChange.FKind = ccTableWorkbench then
          begin
            LOp := 'table-workbench';

            if LChange.FWorkbenchDeclared then
            begin
              LPolicy := LChange.FTableWorkbench.ToData;
            end;
          end;

          if LChange.FKind = ccTablePresentation then
          begin
            LOp := 'table-presentation';

            if LChange.FPresentationDeclared then
            begin
              LPolicy := LChange.FTablePresentation.ToData;
            end;
          end;

          if LChange.FKind = ccColumnText then
          begin
            LOp := 'column-text';

            if LChange.FPresentationDeclared then
            begin
              LPolicy := LChange.FColumnText.ToData;
            end;
          end;

          if LChange.FKind = ccColumnAlignment then
          begin
            LOp := 'column-alignment';

            if LChange.FPresentationDeclared then
            begin
              LPolicy := LChange.FColumnAlignment.ToData;
            end;
          end;

          if LChange.FQueryControlsDeclared then
          begin

            if LChange.FKind = ccTableQuery then
            begin
              LPolicy := LChange.FTableQuery.ToData;
            end
            else
            begin
              LPolicy := LChange.FColumnQuery.ToData;
            end;
          end;

          if LChange.FLayoutDeclared then
          begin

            if LChange.FKind = ccTableLayout then
            begin
              LPolicy := LChange.FTableLayout.ToData;
            end
            else if LChange.FKind = ccColumnLayout then
            begin
              LPolicy := LChange.FColumnLayout.ToData;
            end
            else
            begin
              LPolicy := LChange.FColumnFormat.ToData;
            end;
          end;

          if LChange.FKind in [ccTableQuery, ccTableLayout, ccTableWorkbench, ccTablePresentation] then
          begin
            LValues[LIndex] := NyxObject([NyxField('op', NyxData(LOp)),
              NyxField('owner', NyxData(LChange.FOwner.ID)),
              NyxField('key', NyxData(LChange.FKey.Name)),
              NyxField('projection', NyxData(CCollectionProjections[LChange.FProjection])),
              NyxField('policy', LPolicy)]);
          end
          else
          begin
            LValues[LIndex] := NyxObject([NyxField('op', NyxData(LOp)),
              NyxField('owner', NyxData(LChange.FOwner.ID)),
              NyxField('key', NyxData(LChange.FKey.Name)),
              NyxField('projection', NyxData(CCollectionProjections[LChange.FProjection])),
              NyxField('field', NyxData(LChange.FField.Name)),
              NyxField('kind', NyxData(NyxStateKindName(LChange.FField.Kind))),
              NyxField('policy', LPolicy)]);
          end;
        end;
    end;
  end;
  Result := NyxArray(LValues);
end;

function NyxCollectionPatch(const AChanges: array of TNyxCollectionChange): INyxCollectionPatch;
begin
  Result := TNyxCollectionPatch.Create(AChanges);
end;

function ReadNyxCollectionPatch(const AData: TNyxDataValue): INyxCollectionPatch;
var
  LChanges: array of TNyxCollectionChange;
  LRows: array of TNyxCollectionItem;
  LData: TNyxDataValue;
  LRow: TNyxDataValue;
  LKey: TNyxCollectionRef;
  LIndex: Integer;
  LRowIndex: Integer;
  LOp: TNyxText;
  LDirection: TNyxCollectionColumnMove;
begin
  AData.Validate;

  if (AData.Kind <> ndArray) or (AData.Count < 1) or
    (AData.Count > NyxMaximumCollectionChanges) then
  begin
    raise ENyxCollection.Create('Collection group requires 1..32 changes');
  end;
  SetLength(LChanges, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin
    LData := AData.Item(LIndex);
    LOp := LData.Field('op').AsText;

    if LOp = 'intent' then
    begin
      LChanges[LIndex] := ReadIntent(LData);
      Continue;
    end;

    if LOp = 'bind' then
    begin
      Fields(LData, '|op|owner|projection|spec|', 4);
      LChanges[LIndex] := NyxBindCollection(NyxBindingOwner(LData.Field('owner').AsText),
        ReadProjection(LData.Field('projection')),
        TNyxCollectionViewSpec.FromData(LData.Field('spec')));
      Continue;
    end;

    if LOp = 'move-column' then
    begin
      Fields(LData, '|op|owner|key|projection|field|kind|direction|', 7);

      if ReadProjection(LData.Field('projection')) <> cpTable then
      begin
        raise ENyxCollection.Create('Column moves require the table projection');
      end;

      if LData.Field('direction').AsText = 'earlier' then
      begin
        LDirection := nccmEarlier;
      end
      else if LData.Field('direction').AsText = 'later' then
      begin
        LDirection := nccmLater;
      end
      else
      begin
        raise ENyxCollection.Create('Column move direction must be earlier or later');
      end;
      LChanges[LIndex] := NyxMoveCollectionColumn(
        NyxBindingOwner(LData.Field('owner').AsText),
        NyxCollection(LData.Field('key').AsText),
        ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))), LDirection);
      Continue;
    end;

    if LOp = 'typeahead' then
    begin
      Fields(LData, '|op|owner|key|projection|policy|', 5);

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionTypeAhead(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionTypeAhead(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')),
          TNyxTypeAheadOptions.FromData(LData.Field('policy')));
      end;
      Continue;
    end;

    if LOp = 'query' then
    begin
      Fields(LData, '|op|owner|key|projection|query|', 5);
      LChanges[LIndex] := NyxSetCollectionQuery(
        NyxBindingOwner(LData.Field('owner').AsText),
        NyxCollection(LData.Field('key').AsText),
        ReadProjection(LData.Field('projection')),
        TNyxCollectionQuery.FromData(LData.Field('query')));
      Continue;
    end;

    if LOp = 'draft-recovery' then
    begin
      Fields(LData, '|op|owner|key|projection|policy|', 5);

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionDraftRecovery(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText), ReadProjection(LData.Field('projection')));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionDraftRecovery(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText), ReadProjection(LData.Field('projection')),
          TNyxCollectionDraftRecoveryPolicy.FromData(LData.Field('policy')));
      end;
      Continue;
    end;

    if LOp = 'inspection' then
    begin
      Fields(LData, '|op|owner|key|projection|policy|', 5);

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionInspection(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionInspection(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')),
          TNyxCollectionInspectionPolicy.FromData(LData.Field('policy')));
      end;
      Continue;
    end;

    if LOp = 'query' then
    begin
      Fields(LData, '|op|owner|key|projection|query|', 5);
      LChanges[LIndex] := NyxSetCollectionQuery(
        NyxBindingOwner(LData.Field('owner').AsText),
        NyxCollection(LData.Field('key').AsText),
        ReadProjection(LData.Field('projection')),
        TNyxCollectionQuery.FromData(LData.Field('query')));
      Continue;
    end;

    if LOp = 'table-query' then
    begin
      Fields(LData, '|op|owner|key|projection|policy|', 5);

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionTableQuery(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionTableQuery(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')),
          TNyxTableQueryPolicy.FromData(LData.Field('policy')));
      end;
      Continue;
    end;

    if LOp = 'column-query' then
    begin
      Fields(LData, '|op|owner|key|projection|field|kind|policy|', 7);

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionColumnQuery(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')),
          ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionColumnQuery(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')),
          ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))),
          TNyxColumnQueryPolicy.FromData(LData.Field('policy')));
      end;
      Continue;
    end;

    if LOp = 'table-presentation' then
    begin
      Fields(LData, '|op|owner|key|projection|policy|', 5);

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionTablePresentation(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText), ReadProjection(LData.Field('projection')));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionTablePresentation(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText), ReadProjection(LData.Field('projection')),
          TNyxTablePresentationPolicy.FromData(LData.Field('policy')));
      end;
      Continue;
    end;

    if LOp = 'column-alignment' then
    begin
      Fields(LData, '|op|owner|key|projection|field|kind|policy|', 7);

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionColumnAlignment(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText), ReadProjection(LData.Field('projection')),
          ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionColumnAlignment(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText), ReadProjection(LData.Field('projection')),
          ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))),
          TNyxColumnAlignmentPolicy.FromData(LData.Field('policy')));
      end;
      Continue;
    end;

    if LOp = 'column-text' then
    begin
      Fields(LData, '|op|owner|key|projection|field|kind|policy|', 7);

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionColumnText(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText), ReadProjection(LData.Field('projection')),
          ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionColumnText(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText), ReadProjection(LData.Field('projection')),
          ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))),
          TNyxColumnTextPolicy.FromData(LData.Field('policy')));
      end;
      Continue;
    end;

    if LOp = 'table-workbench' then
    begin
      Fields(LData, '|op|owner|key|projection|policy|', 5);

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionTableWorkbench(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionTableWorkbench(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')),
          TNyxTableWorkbenchPolicy.FromData(LData.Field('policy')));
      end;
      Continue;
    end;

    if LOp = 'table-layout' then
    begin
      Fields(LData, '|op|owner|key|projection|policy|', 5);

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionTableLayout(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionTableLayout(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')),
          TNyxTableLayoutPolicy.FromData(LData.Field('policy')));
      end;
      Continue;
    end;

    if (LOp = 'column-layout') or (LOp = 'column-format') then
    begin
      Fields(LData, '|op|owner|key|projection|field|kind|policy|', 7);

      if LOp = 'column-format' then
      begin

        if LData.Field('policy').Kind = ndNull then
        begin
          LChanges[LIndex] := NyxUseDefaultCollectionColumnFormat(
            NyxBindingOwner(LData.Field('owner').AsText),
            NyxCollection(LData.Field('key').AsText), ReadProjection(LData.Field('projection')),
            ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))));
        end
        else
        begin
          LChanges[LIndex] := NyxSetCollectionColumnFormat(
            NyxBindingOwner(LData.Field('owner').AsText),
            NyxCollection(LData.Field('key').AsText), ReadProjection(LData.Field('projection')),
            ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))),
            TNyxValueFormat.FromData(LData.Field('policy')));
        end;
        Continue;
      end;

      if LData.Field('policy').Kind = ndNull then
      begin
        LChanges[LIndex] := NyxUseDefaultCollectionColumnLayout(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')),
          ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))));
      end
      else
      begin
        LChanges[LIndex] := NyxSetCollectionColumnLayout(
          NyxBindingOwner(LData.Field('owner').AsText),
          NyxCollection(LData.Field('key').AsText),
          ReadProjection(LData.Field('projection')),
          ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))),
          TNyxColumnLayoutPolicy.FromData(LData.Field('policy')));
      end;
      Continue;
    end;
    LKey := NyxCollection(LData.Field('key').AsText);

    if LOp = 'define' then
    begin
      Fields(LData, '|op|key|fields|rows|', 4);
      LRow := LData.Field('rows');

      if (LRow.Kind <> ndArray) or (LRow.Count > NyxMaximumCollectionItems) then
      begin
        raise ENyxCollection.Create('Initial rows require a bounded array');
      end;
      SetLength(LRows, LRow.Count);
      for LRowIndex := 0 to LRow.Count - 1 do
      begin
        Fields(LRow.Item(LRowIndex), '|item|values|', 2);
        LRows[LRowIndex] := ReadRow(LKey, LRow.Item(LRowIndex).Field('item').AsText,
          LRow.Item(LRowIndex).Field('values'));
      end;
      LChanges[LIndex] := NyxDefineCollection(LKey, ReadSchema(LData.Field('fields')), LRows);
    end
    else if LOp = 'field' then
    begin
      Fields(LData, '|op|key|definition|', 3);
      LChanges[LIndex] := NyxSetCollectionField(LKey,
        ReadSchema(NyxArray([LData.Field('definition')])));
    end
    else if LOp = 'remove-field' then
    begin
      Fields(LData, '|op|key|field|kind|', 4);
      LChanges[LIndex] := NyxRemoveCollectionField(LKey,
        ReadField(LData.Field('field').AsText, ReadKind(LData.Field('kind'))));
    end
    else if (LOp = 'append') or (LOp = 'update-row') then
    begin
      Fields(LData, '|op|key|item|values|', 4);
      LRows := nil;
      SetLength(LRows, 1);
      LRows[0] := ReadRow(LKey, LData.Field('item').AsText, LData.Field('values'));

      if LOp = 'append' then
      begin
        LChanges[LIndex] := NyxAppendCollectionRow(LRows[0]);
      end
      else
      begin
        LChanges[LIndex] := NyxUpdateCollectionRow(LRows[0]);
      end;
    end
    else if LOp = 'move-row' then
    begin
      Fields(LData, '|op|key|item|index|', 4);
      LChanges[LIndex] := NyxMoveCollectionRow(NyxItem(LKey, LData.Field('item').AsText),
        LData.Field('index').AsInteger);
    end
    else
    begin
      raise ENyxCollection.Create('Unknown collection change');
    end;
  end;
  Result := NyxCollectionPatch(LChanges);
end;

{$I nyx.studio.collectionedits.schema.inc}

end.
