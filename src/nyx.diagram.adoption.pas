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
unit nyx.diagram.adoption;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.model, nyx.diagram, nyx.diagram.declarations, nyx.diagram.editing;

type
  { Copied save destination and exact accepted definition, or explicit absence.
    No document, renderer or session authority is retained. A stale destination
    refuses through the ordinary candidate guard rather than overwriting work. }
  TNyxDiagramSaveTarget = record
  private
    FDefined: Boolean;
    FReference: TNyxDiagramRef;
    FBefore: TNyxDataValue;
  public
    property Defined: Boolean read FDefined;
    property Reference: TNyxDiagramRef read FReference;
    property Before: TNyxDataValue read FBefore;
  end;

  { Immutable applied model capture; unfinished property fields are deliberately
    outside it. Revision identifies the captured local editor generation only,
    never the server/project revision or compiler execution provenance. }
  TNyxDiagramSaveSnapshot = record
  private
    FDefined: Boolean;
    FChange: TNyxDiagramChange;
    FDiagram: TNyxDataValue;
    FRevision: Integer;
  public
    function Matches(ADocument: TNyxDocument): Boolean;
    property Defined: Boolean read FDefined;
    property Change: TNyxDiagramChange read FChange;
    property Diagram: TNyxDataValue read FDiagram;
    property Revision: Integer read FRevision;
  end;

  { UI-owned single-use reply. The request retains an inert snapshot and a
    revocable borrowed receiver, never the editor itself. Pending submission is
    distinct from actual durable paired publication. Late replies return False
    after retirement and cannot revive a closed editor or replace newer edits. }
  INyxDiagramSaveRequest = interface(IInterface)
    ['{08CB4FF5-BDD8-439C-AD7F-54CCB1016001}']
    function GetSnapshot: TNyxDiagramSaveSnapshot;
    function GetPending: Boolean;
    function Accepted(ADocument: TNyxDocument): Boolean;
    function Refused(const AMessage: TNyxText): Boolean;
    property Snapshot: TNyxDiagramSaveSnapshot read GetSnapshot;
    property Pending: Boolean read GetPending;
  end;

  { Explicit revocable project owner. Submit borrows the request during this
    call; asynchronous hosts retain their own lease and acknowledge publication
    or refusal. No view, source compiler or document authority is inferred from
    the model snapshot. Hosts enforce their owning project/load/revision guards. }
  INyxDiagramSaveHost = interface(IInterface)
    ['{08CB4FF5-BDD8-439C-AD7F-54CCB1016002}']
    procedure Submit(const ARequest: INyxDiagramSaveRequest);
  end;

  { Optional read-only accepted-definition access on a save host. The copied
    target grants no document pointer and does not fetch/compile/source-publish.
    An explicit Reload can inspect current intent after a stale save refusal. }
  INyxDiagramSaveInspection = interface(IInterface)
    ['{08CB4FF5-BDD8-439C-AD7F-54CCB1016005}']
    function Current(const AReference: TNyxDiagramRef): TNyxDiagramSaveTarget;
  end;

{ Borrow a document solely to capture the exact destination before editing. }
function NyxDiagramSaveTarget(ADocument: TNyxDocument;
  const AReference: TNyxDiagramRef): TNyxDiagramSaveTarget;
{ Capture a complete applied graph once. Caller owns UI/draft/revision fences. }
function CaptureNyxDiagramSave(const ATarget: TNyxDiagramSaveTarget;
  const ADiagram: INyxDiagram; ALocalRevision: Integer): TNyxDiagramSaveSnapshot;
{ Advance only the acknowledged baseline; the caller keeps newer model/input. }
function AcceptedNyxDiagramSaveTarget(const ASnapshot: TNyxDiagramSaveSnapshot;
  ADocument: TNyxDocument): TNyxDiagramSaveTarget;

implementation

function NyxDiagramSaveTarget(ADocument: TNyxDocument;
  const AReference: TNyxDiagramRef): TNyxDiagramSaveTarget;
begin

  if ADocument = nil then
  begin
    raise ENyxDiagram.Create('A diagram save destination requires its owning document');
  end;
  Result := Default(TNyxDiagramSaveTarget);
  Result.FReference := NyxDiagramRef(AReference.Name);
  Result.FBefore := NyxNull;

  if ADocument.Diagrams.Contains(Result.FReference) then
  begin
    Result.FBefore := ADocument.Diagrams.Definition(Result.FReference).ToData;
  end;
  Result.FDefined := True;
end;

function CaptureNyxDiagramSave(const ATarget: TNyxDiagramSaveTarget;
  const ADiagram: INyxDiagram; ALocalRevision: Integer): TNyxDiagramSaveSnapshot;
begin

  if not ATarget.Defined or (ALocalRevision < 0) then
  begin
    raise ENyxDiagram.Create('A diagram save requires a defined destination and local revision');
  end;
  Result := Default(TNyxDiagramSaveSnapshot);
  Result.FChange := NyxDefineDiagram(ATarget.Reference, ADiagram);
  Result.FDiagram := Result.FChange.ToData.Field('definition');

  if ATarget.Before.Kind = ndNull then
  begin
    Result.FChange := Result.FChange.ExpectAbsent;
  end
  else
  begin
    Result.FChange := Result.FChange.Expect(NyxDiagramFromData(ATarget.Before));
  end;
  Result.FRevision := ALocalRevision;
  Result.FDefined := True;
end;

function TNyxDiagramSaveSnapshot.Matches(ADocument: TNyxDocument): Boolean;
begin
  Result := FDefined and (ADocument <> nil) and
    ADocument.Diagrams.Contains(FChange.Reference) and
    (ADocument.Diagrams.Definition(FChange.Reference).ToData.ToJSON = FDiagram.ToJSON);
end;

function AcceptedNyxDiagramSaveTarget(const ASnapshot: TNyxDiagramSaveSnapshot;
  ADocument: TNyxDocument): TNyxDiagramSaveTarget;
begin

  if not ASnapshot.Matches(ADocument) then
  begin
    raise ENyxDiagram.Create('Diagram acknowledgement does not match the captured published intent');
  end;
  Result := NyxDiagramSaveTarget(ADocument, ASnapshot.Change.Reference);
end;

end.
