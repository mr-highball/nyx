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

unit nyx.designer.input;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.behavior, nyx.data, nyx.gestures,
  nyx.designer.placement;

type
  { Explicit renderer-host input policy. Design mode normally exposes selection
    and editable values only. Opting into drops does not enable application
    callbacks, drag sources, pointer hooks or custom native drag handlers. }
  TNyxDesignerInput = record
  private
    FDrops: Boolean;
  public
    { Return a configured value; no renderer/model reference is retained. }
    function Drops(AEnabled: Boolean): TNyxDesignerInput;
    property DropEnabled: Boolean read FDrops;
  end;

  { Copied identity of a physical design target. Owner names the editable
    authored design control; Source names the projected primitive's source.
    They differ for inherited reusable content. Path is empty when an unnamed
    edge prevents exact part addressing; "." addresses the owner's root.
    Container describes the effective primitive, not a guessed catalog kind. }
  TNyxDesignerTarget = record
  private
    FOwner: TNyxControlRef;
    FSource: TNyxControlRef;
    FPath: TNyxPartRef;
    FContainer: Boolean;
    FParent: TNyxControlRef;
    FFrame: TNyxDropFrame;
  public
    { Adapters add only copied, positive physical/logical geometry. Default
      remains usable by identity-only consumers and refuses automatic placement. }
    function WithFrame(const AFrame: TNyxDropFrame): TNyxDesignerTarget;
    property Owner: TNyxControlRef read FOwner;
    property Source: TNyxControlRef read FSource;
    property Path: TNyxPartRef read FPath;
    property Container: Boolean read FContainer;
    { Exact runtime parent identity, absent only for a realized root. }
    property Parent: TNyxControlRef read FParent;
    property Frame: TNyxDropFrame read FFrame;
  end;

  { Synchronous borrowed receiver. The event/target contain owned values, never
    widget/model pointers. The decision is sealed by the adapter on return or
    failure; retained decisions cannot negotiate later. Hosts must clear this
    receiver before releasing its object. Only an opted-in design drop invokes
    it; ordinary application events retain their existing Nyx subscriptions. }
  TNyxDesignerGesture = procedure(const ATarget: TNyxDesignerTarget;
    const AEvent: TNyxEventInfo; const ADecision: INyxGestureDecision) of object;

{ Defaults to selection/value authoring, with designer drops disabled. }
function NyxDesignerInput: TNyxDesignerInput;
{ Borrow the realized origin for this call only. Copy exact owner/source IDs
  and direct named-part path; nil refuses and ambiguous unnamed edges stay
  explicitly unaddressable. Platform adapters share this identity contract. }
function NyxDesignerTarget(AOrigin: TNyxNode): TNyxDesignerTarget;
{ Borrow the realized origin for this call only. Only ordinary row/column flow
  has an automatic insertion axis; absolute/grid/custom allocation is explicit. }
function NyxDesignerParentAxis(AOrigin: TNyxNode): TNyxPlacementAxis;
{ Copy a designer-only physical notification without runtime binding admission.
  Design views deliberately have no live application store subscription. Only
  target drag phases are admitted; values/commands/application hooks stay absent. }
function NyxDesignerDragEvent(AOrigin: TNyxNode; ATrigger: TNyxTrigger): TNyxEventInfo;

implementation

uses
  nyx.schema;

function NyxDesignerInput: TNyxDesignerInput;
begin
  Result := Default(TNyxDesignerInput);
end;

function TNyxDesignerTarget.WithFrame(const AFrame: TNyxDropFrame): TNyxDesignerTarget;
begin

  if AFrame.Defined and ((AFrame.Container <> FContainer) or (AFrame.Parent.ID <> FParent.ID)) then
  begin
    raise ENyxModel.Create('Designer drop frame disagrees with its realized primitive');
  end;
  Result := Self;
  Result.FFrame := AFrame;
end;

function NyxDesignerParentAxis(AOrigin: TNyxNode): TNyxPlacementAxis;
var
  LLayout: TNyxText;
begin
  Result := npaUnknown;

  if (AOrigin = nil) or (AOrigin.Parent = nil) then
  begin
    Exit;
  end;
  LLayout := NyxLayout(AOrigin.Parent);

  if LLayout = NyxLayoutName(nlRow) then
  begin
    Result := npaHorizontal;
  end
  else if LLayout = NyxLayoutName(nlColumn) then
  begin
    Result := npaVertical;
  end;
end;

function TNyxDesignerInput.Drops(AEnabled: Boolean): TNyxDesignerInput;
begin
  Result := Self;
  Result.FDrops := AEnabled;
end;

function NyxDesignerTarget(AOrigin: TNyxNode): TNyxDesignerTarget;
var
  LPart: TNyxNode;
  LPath: TNyxText;
  LInfo: TNyxPrimitiveInfo;
begin

  if AOrigin = nil then
  begin
    raise ENyxModel.Create('Designer input requires an exact realized origin');
  end;
  Result := Default(TNyxDesignerTarget);
  Result.FOwner := NyxControl(AOrigin.DesignID);
  Result.FSource := NyxControl(AOrigin.SourceID);

  if AOrigin.Parent <> nil then
  begin
    Result.FParent := NyxControl(AOrigin.Parent.ID);
  end;
  Result.FContainer := FindNyxPrimitive(AOrigin.ProjectionKind, LInfo) and LInfo.Container;
  LPath := '.';
  LPart := AOrigin;

  if (LPart.Parent <> nil) and (LPart.Parent.DesignID = AOrigin.DesignID) then
  begin
    LPath := '';
    while (LPart.Parent <> nil) and (LPart.Parent.DesignID = AOrigin.DesignID) do
    begin

      if LPart.Prop('part') = '' then
      begin
        Exit;
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
  end;
  Result.FPath := NyxPart(LPath);
end;

function NyxDesignerDragEvent(AOrigin: TNyxNode; ATrigger: TNyxTrigger): TNyxEventInfo;
begin

  if (AOrigin = nil) or not (ATrigger in [ntDragEnter, ntDragOver, ntDragExit, ntDrop]) then
  begin
    raise ENyxModel.Create('Designer drag input requires a realized target notification');
  end;
  Result := Default(TNyxEventInfo);
  Result.Trigger := ATrigger;
  Result.Name := NyxEvent(NyxTriggerName(ATrigger));
  Result.OriginID := AOrigin.ID;
  Result.SourceID := AOrigin.ID;
  Result.TargetID := AOrigin.ID;
  Result.Value := NyxNull;
  Result.Details := NyxNull;
end;

end.
