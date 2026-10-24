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
unit nyx.collections.draft;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.editing, nyx.state, nyx.contract, nyx.collections,
  nyx.collections.arrangement, nyx.collections.clipboard,
  nyx.collections.representation, nyx.scheduler;

type
  { One deliberate readonly capture of an actually focused cell editor. Value
    is explicitly draft text with copied accepted RAW provenance and domain;
    Selection uses Unicode scalar positions in those physical draft bytes.
    Only owner-free original input/open flags are retained by Readiness. No
    editor, tree, presenter, mount, view or store survives in this capture.
    Retaining Value after retirement remains useful; Readiness cannot authorize
    a replacement input incarnation or undo an entered clipboard effect. }
  INyxCollectionFocusedDraftCapture = interface(IInterface)
    ['{D5D30DCB-A643-4233-89DC-1E8D955A5C93}']
    function GetValue: INyxCollectionCellText;
    function GetSelection: TNyxTextSelection;
    function GetInputEpoch: Integer;
    function GetReadiness: INyxCancellationReadiness;
    property Value: INyxCollectionCellText read GetValue;
    property Selection: TNyxTextSelection read GetSelection;
    property InputEpoch: Integer read GetInputEpoch;
    property Readiness: INyxCancellationReadiness read GetReadiness;
  end;

  { Optional UI-thread target entry. It reads only the actual current focused
    editable text cell; nil means no eligible editor, composition, stale input
    or retirement. It requires configured managed copy authority and the target's
    observed text-edit lifetime; unobserved direct native mounts refuse.
    Capture must neither blur/hide/suspend nor validate/commit a
    physical draft. Dormant text and readonly inspection memos are not active
    editor authority. Ordinary active-editor Ctrl+C remains the host's route. }
  INyxCollectionFocusedDraftInput = interface(IInterface)
    ['{E8C94B69-31D5-4B87-979B-836BDA4798D8}']
    function CaptureFocusedDraft: INyxCollectionFocusedDraftCapture;
  end;

{ Explicitly construct a different representation from one admitted accepted
  RAW cell. ADomain validates the copied accepted value, never ADraftText: an
  unfinished numeric '-' is draft text, not a newly accepted scalar.
  This portable constructor alone makes no claim that an editor has focus. }
function NewNyxCollectionCellDraft(const ARaw: INyxCollectionClipboardText;
  const ADraftText: TNyxText; const ADomain: TNyxValueDomain;
  AMaximumUTF8Bytes: Integer = 4194304): INyxCollectionCellText;

{ Target implementations call this only after original physical/editor facts
  were captured. They must recheck the SAME facts after construction and before
  returning. The final receipt retains no target ownership or live getter. }
function NewNyxCollectionFocusedDraftCapture(const AValue: INyxCollectionCellText;
  AEpoch: Integer; const ASelection: TNyxTextSelection;
  const AReadiness: INyxCancellationReadiness): INyxCollectionFocusedDraftCapture;

{ Read only an already captured immutable schema. The caller retains/rechecks
  original target authority around acquiring that schema from a source. }
function NyxCollectionDraftDomain(const ASchema: TNyxCollectionSchema;
  const AField: TNyxColumnIdentity): TNyxValueDomain;
{ Normalize a foreign capture into copied scalar text/selection/epoch and its
  declared owner-free readiness facet. Callers recheck their entered authority
  after these getters. Exact matching includes accepted scalar and RAW bytes;
  equal draft text alone never identifies the original cell. }
function CopyNyxCollectionFocusedDraft(const ACapture: INyxCollectionFocusedDraftCapture):
  INyxCollectionFocusedDraftCapture;
function NyxCollectionDraftMatches(const ACapture: INyxCollectionFocusedDraftCapture;
  const ARaw: INyxCollectionClipboardText): Boolean;

implementation

uses nyx.bytes, nyx.data, nyx.formatting;

function CopyNyxCollectionFocusedDraft(const ACapture: INyxCollectionFocusedDraftCapture):
  INyxCollectionFocusedDraftCapture;
var
  LCapture: INyxCollectionFocusedDraftCapture;
  LValue: INyxCollectionCellText;
  LSelection: TNyxTextSelection;
  LEpoch: Integer;
  LReady: INyxCancellationReadiness;
begin
  Result := nil;
  LCapture := ACapture;

  if LCapture = nil then
  begin
    Exit;
  end;
  LValue := LCapture.Value;
  LSelection := LCapture.Selection;
  LEpoch := LCapture.InputEpoch;
  LReady := LCapture.Readiness;
  Result := NewNyxCollectionFocusedDraftCapture(LValue, LEpoch, LSelection, LReady);
end;

function NyxCollectionDraftMatches(const ACapture: INyxCollectionFocusedDraftCapture;
  const ARaw: INyxCollectionClipboardText): Boolean;
var
  LValue: INyxCollectionCellText;
  LItem: TNyxItemRef;
begin
  Result := False;

  if (ACapture = nil) or (ARaw = nil) or (ARaw.RowCount <> 1) or
    (ARaw.FieldCount <> 1) then
  begin
    Exit;
  end;
  LValue := ACapture.Value;
  LItem := ARaw.ItemAt(0);
  Result := (LValue.Representation = nccrExplicitDraft) and
    (LValue.Item.Collection.Name = LItem.Collection.Name) and
    (LValue.Item.ID = LItem.ID) and LValue.Field.Same(ARaw.FieldAt(0)) and
    (LValue.Revision = ARaw.Revision) and
    LValue.Value.SameValue(ARaw.CellValue(0, 0)) and (LValue.RawText = ARaw.Text);
end;

type
  { Every delegated accepted fact belongs to a newly normalized, owner-free
    RAW object. Draft text/domain are copied; no caller interface is retained. }
  TDraftValue = class(TInterfacedObject, INyxCollectionCellText)
  public
    Accepted: INyxCollectionCellText;
    Text: TNyxText;
    Bytes: Integer;
    Domain: TNyxValueDomain;
    function GetRepresentation: TNyxCollectionCellRepresentation;
    function GetText: TNyxText;
    function GetRawText: TNyxText;
    function GetByteCount: Integer;
    function GetItem: TNyxItemRef;
    function GetField: TNyxColumnIdentity;
    function GetRevision: Integer;
    function GetValue: TNyxStateValue;
    function GetHasFormatter: Boolean;
    function GetFormatter: TNyxValueFormat;
    function GetDomain: TNyxValueDomain;
  end;

  { Separate flags-only authority prevents a retained value from retaining its
    mount or borrowing a later editor. Selection is never a platform handle
    and does not change the clipboard's whole-cell text. }
  TFocusedDraft = class(TInterfacedObject, INyxCollectionFocusedDraftCapture)
  public
    Value: INyxCollectionCellText;
    Selection: TNyxTextSelection;
    Epoch: Integer;
    Ready: INyxCancellationReadiness;
    function GetValue: INyxCollectionCellText;
    function GetSelection: TNyxTextSelection;
    function GetInputEpoch: Integer;
    function GetReadiness: INyxCancellationReadiness;
  end;

function NyxCollectionDraftDomain(const ASchema: TNyxCollectionSchema;
  const AField: TNyxColumnIdentity): TNyxValueDomain;
var
  LDefinition: TNyxCollectionField;
begin
  AField.Validate;
  case AField.Kind of
    nskText: LDefinition := ASchema.Definition(NyxTextField(AField.Name));
    nskBoolean: LDefinition := ASchema.Definition(NyxBooleanField(AField.Name));
    nskInteger: LDefinition := ASchema.Definition(NyxIntegerField(AField.Name));
    nskNumber: LDefinition := ASchema.Definition(NyxNumberField(AField.Name));
  end;
  Result := LDefinition.Domain;
end;

function NewNyxCollectionCellDraft(const ARaw: INyxCollectionClipboardText;
  const ADraftText: TNyxText; const ADomain: TNyxValueDomain;
  AMaximumUTF8Bytes: Integer): INyxCollectionCellText;
var
  LValue: TDraftValue;
  LScalar: TNyxStateValue;
  LData: TNyxDataValue;
begin
  LValue := TDraftValue.Create;
  Result := LValue;
  LValue.Accepted := NewNyxCollectionCellText(ARaw, AMaximumUTF8Bytes);
  LValue.Bytes := NyxUTF8ByteCount(ADraftText);

  if LValue.Bytes > AMaximumUTF8Bytes then
  begin
    raise ENyxCollectionRepresentation.Create('Explicit draft exceeds its UTF-8 byte budget');
  end;
  LValue.Text := ADraftText;
  LValue.Domain := ADomain.Copy;
  LValue.Domain.Validate;
  LScalar := LValue.Accepted.Value;

  if LValue.Domain.Defined then
  begin

    if LValue.Domain.Kind <> LScalar.Kind then
    begin
      raise ENyxCollectionRepresentation.Create('Draft domain differs from its accepted typed field');
    end;
    case LScalar.Kind of
      nskText: LData := NyxData(LScalar.TextValue);
      nskBoolean: LData := NyxData(LScalar.BooleanValue);
      nskInteger: LData := NyxData(LScalar.IntegerValue);
      nskNumber: LData := NyxData(LScalar.NumberValue);
    end;
    LValue.Domain.Admit(LData);
  end;
end;

function NewNyxCollectionFocusedDraftCapture(const AValue: INyxCollectionCellText;
  AEpoch: Integer; const ASelection: TNyxTextSelection;
  const AReadiness: INyxCancellationReadiness): INyxCollectionFocusedDraftCapture;
var
  LCapture: TFocusedDraft;
  LValue: INyxCollectionCellText;
begin

  if (AValue = nil) or (AReadiness = nil) or
    (AEpoch <= 0) or (AEpoch >= High(Integer)) then
  begin
    raise ENyxCollectionRepresentation.Create('Focused draft requires original value/input/readiness');
  end;
  LValue := CopyNyxCollectionCellText(AValue);

  if LValue.Representation <> nccrExplicitDraft then
  begin
    raise ENyxCollectionRepresentation.Create('Focused draft cannot relabel RAW or display output');
  end;
  LCapture := TFocusedDraft.Create;
  Result := LCapture;
  LCapture.Value := LValue;
  LCapture.Epoch := AEpoch;
  LCapture.Ready := AReadiness;

  if ASelection.Defined then
  begin
    LCapture.Selection := NyxTextSelection(LValue.Text, ASelection.Start,
      ASelection.Finish, ASelection.Direction);
  end;
  AReadiness.CheckReady;
end;

function TDraftValue.GetRepresentation: TNyxCollectionCellRepresentation;
begin
  Result := nccrExplicitDraft;
end;

function TDraftValue.GetText: TNyxText;
begin
  Result := Text;
end;

function TDraftValue.GetRawText: TNyxText;
begin
  Result := Accepted.RawText;
end;

function TDraftValue.GetByteCount: Integer;
begin
  Result := Bytes;
end;

function TDraftValue.GetItem: TNyxItemRef;
begin
  Result := Accepted.Item;
end;

function TDraftValue.GetField: TNyxColumnIdentity;
begin
  Result := Accepted.Field;
end;

function TDraftValue.GetRevision: Integer;
begin
  Result := Accepted.Revision;
end;

function TDraftValue.GetValue: TNyxStateValue;
begin
  Result := Accepted.Value;
end;

function TDraftValue.GetHasFormatter: Boolean;
begin
  Result := False;
end;

function TDraftValue.GetFormatter: TNyxValueFormat;
begin
  Result := Default(TNyxValueFormat);
  raise ENyxCollectionRepresentation.Create('An explicit physical draft has no display formatter');
end;

function TDraftValue.GetDomain: TNyxValueDomain;
begin
  Result := Domain.Copy;
end;

function TFocusedDraft.GetValue: INyxCollectionCellText;
begin
  Result := Value;
end;

function TFocusedDraft.GetSelection: TNyxTextSelection;
begin
  Result := Selection;
end;

function TFocusedDraft.GetInputEpoch: Integer;
begin
  Result := Epoch;
end;

function TFocusedDraft.GetReadiness: INyxCancellationReadiness;
begin
  Result := Ready;
end;

end.
