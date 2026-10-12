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
unit nyx.linq.publication;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.linq, nyx.linq.execution, nyx.scheduler, nyx.collections;

type
  { Capture fixes the exact target key/schema/revision on the UI thread. It owns
    that collection until Close/last-lease release, and owns no renderer/view.
    Close before the owning view navigates or retires. Queued ports then discard
    without touching the collection. Create a fresh lease for each new generation.
    A complete result is independently admitted and assigned as ONE collection
    publication. Duplicate identities, invalid fields, budgets and changed target
    revisions fail without partial replacement. Existing validators/observers
    retain their ordinary semantics, including errors AFTER committed notification.
    An unchanged result is a collection no-op, although the run succeeds.
    Optional AGeneration provides automatic navigation/disposal revocation, even
    for direct Port.Publish callers. It retains no UI owner. Pair it with the same
    generation on Submit to stop obsolete computation too. Close remains useful
    for a receiver that retires earlier than its view; nil preserves existing
    explicit receiver ownership for application-wide stores. }
  INyxCollectionQueryReceiver = interface
    ['{953BC1C5-59C0-4591-9F8B-BF49BE6DD86C}']
    procedure Close;
    function GetPort: INyxQueryResultPort<TNyxCollectionItem>;
    property Port: INyxQueryResultPort<TNyxCollectionItem> read GetPort;
  end;

function NyxCollectionQueryReceiver(const AScheduler: INyxScheduler;
  const ACollection: INyxCollection;
  const AGeneration: INyxCancellationScope = nil): INyxCollectionQueryReceiver;

implementation

type
  TCollectionPort = class(TInterfacedObject, INyxQueryResultPort<TNyxCollectionItem>)
  private
    FScheduler: INyxScheduler;
    FCollection: INyxCollection;
    FRevision: Integer;
    FGeneration: INyxCancellationScope;
  public
    constructor Create(const AScheduler: INyxScheduler; const ACollection: INyxCollection;
      const AGeneration: INyxCancellationScope);
    procedure Close;
    function Publish(const AValues: TNyxSequenceArray<TNyxCollectionItem>): Boolean;
  end;
  TCollectionLease = class(TInterfacedObject, INyxCollectionQueryReceiver)
  private
    FPort: TCollectionPort;
    FOwned: INyxQueryResultPort<TNyxCollectionItem>;
  public
    constructor Create(const AScheduler: INyxScheduler; const ACollection: INyxCollection;
      const AGeneration: INyxCancellationScope);
    destructor Destroy; override;
    procedure Close;
    function GetPort: INyxQueryResultPort<TNyxCollectionItem>;
  end;

constructor TCollectionPort.Create(const AScheduler: INyxScheduler;
  const ACollection: INyxCollection; const AGeneration: INyxCancellationScope);
begin
  inherited Create;

  if (AScheduler = nil) or (ACollection = nil) then
  begin
    raise ENyxQuery.Create('A collection query receiver requires a scheduler and collection');
  end;
  AScheduler.RequireUI;
  FScheduler := AScheduler;
  FCollection := ACollection;
  FRevision := ACollection.Snapshot.Revision;
  FGeneration := AGeneration;
end;

procedure TCollectionPort.Close;
begin
  FScheduler.RequireUI;
  FCollection := nil;
end;

function TCollectionPort.Publish(const AValues: TNyxSequenceArray<TNyxCollectionItem>): Boolean;
var
  LCollection, LCandidate: INyxCollection;
  LBefore: INyxCollectionSnapshot;
begin
  FScheduler.RequireUI;

  if (FGeneration <> nil) and FGeneration.Cancelled then
  begin
    Close;
  end;
  LCollection := FCollection;
  Result := LCollection <> nil;

  if not Result then
  begin
    Exit;
  end;
  LBefore := LCollection.Snapshot;

  if LBefore.Revision <> FRevision then
  begin
    raise ENyxQuery.Create('Collection changed before query result delivery');
  end;
  { Full candidate admission occurs on UI, away from the accepted store. Assign
    performs ordinary revision/validator checks before one atomic publication. }
  LCandidate := NewNyxCollection(LBefore.Key, LBefore.Schema, AValues);
  LCollection.Assign(LCandidate.Snapshot, FRevision);
end;

constructor TCollectionLease.Create(const AScheduler: INyxScheduler;
  const ACollection: INyxCollection; const AGeneration: INyxCancellationScope);
begin
  inherited Create;
  FPort := TCollectionPort.Create(AScheduler, ACollection, AGeneration);
  FOwned := FPort;
end;

destructor TCollectionLease.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TCollectionLease.Close;
begin

  if FPort <> nil then
  begin
    FPort.Close;
  end;
end;

function TCollectionLease.GetPort: INyxQueryResultPort<TNyxCollectionItem>;
begin
  Result := FOwned;
end;

function NyxCollectionQueryReceiver(const AScheduler: INyxScheduler;
  const ACollection: INyxCollection;
  const AGeneration: INyxCancellationScope): INyxCollectionQueryReceiver;
begin
  Result := TCollectionLease.Create(AScheduler, ACollection, AGeneration);
end;

end.
