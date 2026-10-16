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
unit nyx.canvas.adoption;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.types, nyx.data, nyx.model,
  nyx.canvas.types, nyx.canvas.route.types, nyx.canvas.workspace;

type
  { Immutable applied-route snapshot, containing copied names/data only. Source
    and candidate frames are exact portable design bytes, not compiler or runtime
    authority. No document, widget, callback or content-lifetime token is retained.
    Default is undefined; changes are returned as independent copied values. }
  TNyxCanvasRouteAdoptionSnapshot = record
  private
    FDefined: Boolean;
    FOwner: TNyxControlRef;
    FScene: TNyxCanvasRef;
    FBaseDesign: TNyxText;
    FCandidateDesign: TNyxText;
    FSceneRevision: Integer;
    FRouteRevision: Integer;
    FChanges: TNyxCanvasRouteChanges;
    function GetCount: Integer;
  public
    function Change(AIndex: Integer): TNyxCanvasRouteChange;
    function Changes: TNyxCanvasRouteChanges;
    property Defined: Boolean read FDefined;
    property Owner: TNyxControlRef read FOwner;
    property Scene: TNyxCanvasRef read FScene;
    property BaseDesign: TNyxText read FBaseDesign;
    property CandidateDesign: TNyxText read FCandidateDesign;
    property SceneRevision: Integer read FSceneRevision;
    property RouteRevision: Integer read FRouteRevision;
    property Count: Integer read GetCount;
  end;

  { UI-owned revocable request. The managed ticket owns an inert snapshot and a
    weak inspector receiver only. Submit acceptance is separate from eventual
    source publication. Accepted follows an actual durable publication; Refused
    resolves failure/cancellation without touching source or live routes.
    A disconnected or completed request returns False and never revives a view.
    Later live edits are intentionally outside this immutable saved snapshot.
    Invalid acknowledgement data refuses before admission; a presentation error
    after admission stays diagnostic feedback rather than reversing acceptance. }
  INyxCanvasRouteAdoptionRequest = interface(IInterface)
    ['{6CB3770D-2C16-4097-8CF7-490774468D40}']
    function GetSnapshot: TNyxCanvasRouteAdoptionSnapshot;
    function GetPending: Boolean;
    function Accepted(ADocument: TNyxDocument): Boolean;
    function Refused(const AMessage: TNyxText): Boolean;
    property Snapshot: TNyxCanvasRouteAdoptionSnapshot read GetSnapshot;
    property Pending: Boolean read GetPending;
  end;

  { Explicit application/project owner for ordinary Save connections. Submit
    borrows the request during this UI call; asynchronous hosts retain their own
    interface lease and eventually acknowledge actual publication or refusal.
    Hosts provide their own revocable lifetime and exact project/revision guards.
    Merely returning from Submit never means compilation or persistence passed. }
  INyxCanvasRouteAdoptionHost = interface(IInterface)
    ['{9E27605C-27CD-453A-A92B-B784B758426C}']
    procedure Submit(const ARequest: INyxCanvasRouteAdoptionRequest);
  end;

{ Capture applied runtime overrides for an exact authored canvas. The source is
  borrowed only during this call and copied before entering creator view APIs.
  Whole candidate validation retains shared roots and every unrelated mask.
  Missing/replaced authored content, changed live generations and unavailable
  editing refuse. A zero-count snapshot means all captured masks are already
  saved. Pan/zoom/placement and unfinished editor proposals are not adopted. }
function CaptureNyxCanvasRouteAdoption(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef; const AView: INyxCanvasWorkspaceView):
  TNyxCanvasRouteAdoptionSnapshot;

implementation

uses nyx.codec, nyx.composition, nyx.canvas.declarations, nyx.canvas.routes,
  nyx.flow.route.model;

function TNyxCanvasRouteAdoptionSnapshot.GetCount: Integer;
begin
  Result := Length(FChanges);
end;

function TNyxCanvasRouteAdoptionSnapshot.Change(AIndex: Integer): TNyxCanvasRouteChange;
begin

  if not FDefined or (AIndex < 0) or (AIndex >= Length(FChanges)) then
  begin
    raise ENyxCanvasScene.Create('Choose an existing captured connection change');
  end;
  Result := TNyxCanvasRouteChange.FromData(FChanges[AIndex].ToData);
end;

function TNyxCanvasRouteAdoptionSnapshot.Changes: TNyxCanvasRouteChanges;
var
  LIndex: Integer;
begin
  Result := nil;

  if not FDefined then
  begin
    raise ENyxCanvasScene.Create('Capture canvas connections before requesting adoption');
  end;
  SetLength(Result, Length(FChanges));
  for LIndex := 0 to High(FChanges) do
  begin
    Result[LIndex] := Change(LIndex);
  end;
end;

function CaptureNyxCanvasRouteAdoption(ADocument: TNyxDocument;
  const AOwner: TNyxControlRef; const AView: INyxCanvasWorkspaceView):
  TNyxCanvasRouteAdoptionSnapshot;
var
  LView: INyxCanvasWorkspaceView;
  LRuntime: INyxCanvasRuntimeView;
  LEditing: INyxCanvasRouteEditing;
  LSource: TNyxDocument;
  LCandidate: TNyxDocument;
  LOwner: TNyxControlRef;
  LNode: TNyxNode;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LExpectedRoot: TNyxNode;
  LActualRoot: TNyxNode;
  LContent: INyxNode;
  LDefinition: INyxCanvasDefinition;
  LBuilder: INyxCanvasConnections;
  LCaptured: TNyxCanvasRouteChanges;
  LSaved: TNyxCanvasRouteChanges;
  LItem: TNyxCanvasDeclarationItem;
  LIndex: Integer;
  LPosition: Integer;
  LPrior: Integer;
  LFound: Boolean;
  LCount: Integer;
  LCheckedItems: array of TNyxCanvasItemRef;
  LChecked: Boolean;
begin
  Result := Default(TNyxCanvasRouteAdoptionSnapshot);
  LView := AView;
  LOwner := NyxControl(AOwner.ID);

  if (ADocument = nil) or (LView = nil) then
  begin
    raise ENyxCanvasScene.Create('Saving connections requires a source and connected canvas');
  end;
  LSource := ADocument.Clone;
  LCandidate := nil;
  LContext := nil;
  LExpectedRoot := nil;
  LActualRoot := nil;
  LCheckedItems := nil;
  try

    if not LView.Connected or not Supports(LView, INyxCanvasRuntimeView, LRuntime) or
      not Supports(LView, INyxCanvasRouteEditing, LEditing) then
    begin
      raise ENyxCanvasScene.Create('This canvas does not expose applied connection capture');
    end;
    Result.FSceneRevision := LRuntime.Scene.Revision;
    Result.FRouteRevision := LEditing.RouteRevision;
    LNode := LSource.Find(LOwner.ID);

    if LNode = nil then
    begin
      raise ENyxCanvasScene.Create('The authored canvas owner is no longer available');
    end;
    LContext := RealizeNyxContext(LSource, LNode, LProjection);

    if (LProjection = nil) or (LProjection.ProjectionKind <> NyxKindName(nkCanvas)) or
      not LProjection.CanvasScene.Defined then
    begin
      raise ENyxCanvasScene.Create('Save this canvas scene in the project before saving connections');
    end;
    Result.FOwner := LOwner;
    Result.FScene := LProjection.CanvasScene;
    Result.FBaseDesign := TNyxCodec.Encode(LSource);
    LDefinition := LSource.Canvases.Definition(Result.FScene);
    LSaved := NyxCanvasConnectionChanges(LDefinition);
    LCaptured := LEditing.CaptureRoutes;
    LBuilder := NyxCanvasConnections(LDefinition);

    if Length(LCaptured) > 0 then
    begin
      LBuilder := LBuilder.Controls(LCaptured);
    end;
    SetLength(Result.FChanges, Length(LCaptured));
    LCount := 0;
    for LIndex := 0 to High(LCaptured) do
    begin
      LFound := False;
      for LPosition := 0 to LDefinition.ItemCount - 1 do
      begin
        LItem := LDefinition.Items[LPosition];

        if LItem.Item.Name = LCaptured[LIndex].Item.Name then
        begin
          LFound := True;
          Break;
        end;
      end;

      if not LFound then
      begin
        raise ENyxCanvasScene.Create('Save the new canvas item before adopting its connections');
      end;
      LChecked := False;
      for LPrior := 0 to High(LCheckedItems) do
      begin
        LChecked := LChecked or (LCheckedItems[LPrior].Name = LItem.Item.Name);
      end;

      if not LChecked then
      begin
        LContent := LRuntime.Scene.Control(LItem.Item);
        LExpectedRoot := LSource.FindRoot(LItem.Content).Clone;
        LActualRoot := LContent.Node.Clone;
        try
          for LPrior := 0 to High(LSaved) do
          begin

            if LSaved[LPrior].Item.Name = LItem.Item.Name then
            begin
              SetNyxFlowRoutes(LExpectedRoot.Find(LSaved[LPrior].Control.ID), LSaved[LPrior].Routes);
            end;
          end;
          { Runtime masks overlay raw item content. After an earlier save the
            retained raw tree still has its original route metadata. Ignore only
            those explicitly captured route fields on independent copies; every
            other authored property/type/child/contract must remain exact. }
          for LPrior := 0 to High(LCaptured) do
          begin

            if LCaptured[LPrior].Item.Name = LItem.Item.Name then
            begin
              InheritNyxFlowRoutes(LExpectedRoot.Find(LCaptured[LPrior].Control.ID));
              InheritNyxFlowRoutes(LActualRoot.Find(LCaptured[LPrior].Control.ID));
            end;
          end;

          if TNyxCodec.NodeSnapshot(LExpectedRoot).ToJSON <>
            TNyxCodec.NodeSnapshot(LActualRoot).ToJSON then
          begin
            raise ENyxCanvasScene.Create('The live item differs from its authored content; reconcile it before saving connections');
          end;
        finally
          FreeAndNil(LActualRoot);
          FreeAndNil(LExpectedRoot);
        end;
        SetLength(LCheckedItems, Length(LCheckedItems) + 1);
        LCheckedItems[High(LCheckedItems)] := LItem.Item;
      end;
      LFound := False;
      for LPrior := 0 to High(LSaved) do
      begin

        if LSaved[LPrior].ToData.ToJSON = LCaptured[LIndex].ToData.ToJSON then
        begin
          LFound := True;
          Break;
        end;
      end;

      if LFound then
      begin
        Continue;
      end;
      Result.FChanges[LCount] := TNyxCanvasRouteChange.FromData(LCaptured[LIndex].ToData);
      Inc(LCount);
    end;
    SetLength(Result.FChanges, LCount);
    LCandidate := LSource.Clone;
    LCandidate.Canvases.Define(Result.FScene, LBuilder.Done);
    LCandidate.Validate;
    Result.FCandidateDesign := TNyxCodec.Encode(LCandidate);

    if not LView.Connected or (LRuntime.Scene.Revision <> Result.FSceneRevision) or
      (LEditing.RouteRevision <> Result.FRouteRevision) then
    begin
      raise ENyxCanvasScene.Create('The canvas changed while its connections were captured');
    end;
    Result.FDefined := True;
  finally
    LActualRoot.Free;
    LExpectedRoot.Free;
    LContext.Free;
    LCandidate.Free;
    LSource.Free;
  end;
end;

end.
