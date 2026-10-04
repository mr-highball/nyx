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
unit nyx.test.collections.view;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.collections,
  nyx.collections.view.types;

function CreateNyxViewCollection: INyxCollection;
function NyxTestTableViewSpec: TNyxCollectionViewSpec;
function NyxTestTreeViewSpec: TNyxCollectionViewSpec;
function RunNyxCollectionViewTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.data,
  nyx.state,
  nyx.contract,
  nyx.collections.registry,
  nyx.collections.view,
  nyx.collections.mount;

type
  { A borrowed target callback invokes the attachment while its only external
    interface owner is released by an observer. Track destruction independently
    of the attachment so checking the result never reads freed memory. }
  TMountReleaseProbe = class
  public
    Owner: INyxCollectionMount;
    Destroyed: Boolean;
    DestroyedDuringCallback: Boolean;
    procedure ReleaseOwner(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;

  TTrackedMount = class(TNyxCollectionMountBase)
  private
    FProbe: TMountReleaseProbe;
  protected
    procedure RenderDataset; override;
    procedure DetachTarget; override;
  public
    constructor Create(const AView: INyxCollectionView; AProbe: TMountReleaseProbe);
    destructor Destroy; override;
  end;

  TViewProbe = class
  public
    Calls: Integer;
    SelectionCalls: Integer;
    Cancel: INyxCollectionViewSubscription;
    Fail: Boolean;
    EmptyFailure: Boolean;
    Reenter: Boolean;
    RejectedReentry: Boolean;
    OwnedView: INyxCollectionView;
    procedure Changed(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
    procedure DropOwner(const AView: INyxCollectionView;
      const AChanges: INyxCollectionChanges);
  end;

procedure TMountReleaseProbe.ReleaseOwner(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Owner := nil;
  DestroyedDuringCallback := Destroyed;
end;

constructor TTrackedMount.Create(const AView: INyxCollectionView;
  AProbe: TMountReleaseProbe);
begin
  inherited Create(AView);
  FProbe := AProbe;
end;

destructor TTrackedMount.Destroy;
begin
  FProbe.Destroyed := True;
  inherited Destroy;
end;

procedure TTrackedMount.RenderDataset;
begin
  { No platform control is needed to test the portable attachment call frame. }
end;

procedure TTrackedMount.DetachTarget;
begin
end;

function ReleaseLastMountOwner: Boolean;
var
  LProbe: TMountReleaseProbe;
  LView: INyxCollectionView;
  LToken: INyxCollectionViewSubscription;
  LRaw: TTrackedMount;

  procedure Prepare;
  begin
    { End all constructor/factory interface temporaries before the raw target
      call below. The probe then holds the only external attachment reference. }
    LView := NewNyxCollectionView(CreateNyxViewCollection, NyxTestTableViewSpec, cpTable);
    LRaw := TTrackedMount.Create(LView, LProbe);
    LProbe.Owner := LRaw;
    LRaw.Activate;
    LToken := LView.Subscribe(LProbe.ReleaseOwner);
  end;

begin
  LProbe := TMountReleaseProbe.Create;
  try
    Prepare;
    Result := LRaw.EditCell(LView.Snapshot.ItemAt(0).Ref, 2, '3');
    { LRaw is invalid after this call. Only inspect the independent probe/view. }
    Result := Result and LProbe.Destroyed and not LProbe.DestroyedDuringCallback and
      (LView.CellText(LView.Snapshot.ItemAt(0).Ref, 2) = '3');
  finally

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;
    LProbe.Owner := nil;
    LView := nil;
    LProbe.Free;
  end;
end;

procedure TViewProbe.Changed(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  Inc(Calls);

  if AChanges = nil then
  begin
    Inc(SelectionCalls);
  end;

  if Cancel <> nil then
  begin
    Cancel.Disconnect;
    Cancel := nil;
  end;

  if Reenter then
  begin
    try
      AView.ClearSelection;
    except
      on LException: ENyxCollection do
      begin
        RejectedReentry := True;
      end;
    end;
  end;

  if Fail then
  begin
    raise ENyxCollection.Create('Observer fixture failed');
  end;

  if EmptyFailure then
  begin
    raise ENyxCollection.Create('');
  end;
end;

procedure TViewProbe.DropOwner(const AView: INyxCollectionView;
  const AChanges: INyxCollectionChanges);
begin
  OwnedView := nil;
  Inc(Calls);
end;

function CreateNyxViewCollection: INyxCollection;
var
  LKey: TNyxCollectionRef;
begin
  LKey := NyxCollection('tasks / 🌙');
  Result := NewNyxCollection(LKey, NyxCollectionSchema
    .Text(NyxTextField('caption'), '')
    .Boolean(NyxBooleanField('done'), False)
    .Integer(NyxIntegerField('priority'), 1, NyxIntegerDomain.Range(1, 5))
    .Number(NyxNumberField('score'), 0.5, NyxNumberDomain.Range(0, 1))
    .Text(NyxTextField('parent'), ''), [
      NyxCollectionItem(NyxItem(LKey, 'child / 漢字'))
        .WithValue(NyxTextField('caption'), 'Child 🌙')
        .WithValue(NyxTextField('parent'), 'root'),
      NyxCollectionItem(NyxItem(LKey, 'root'))
        .WithValue(NyxTextField('caption'), 'Root 漢字'),
      NyxCollectionItem(NyxItem(LKey, 'leaf'))
        .WithValue(NyxTextField('caption'), 'Leaf')
        .WithValue(NyxTextField('parent'), 'child / 漢字')]);
end;

function NyxTestTableViewSpec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(NyxCollection('tasks / 🌙'))
    .Column(NyxTextField('caption'), 'Caption / 🌙', cmEditable)
    .Column(NyxBooleanField('done'), 'Complete', cmEditable)
    .Column(NyxIntegerField('priority'), 'Priority', cmEditable)
    .Column(NyxNumberField('score'), 'Score', cmEditable);
end;

function NyxTestTreeViewSpec: TNyxCollectionViewSpec;
begin
  Result := NyxCollectionView(NyxCollection('tasks / 🌙'))
    .Column(NyxTextField('caption'), 'Task', cmEditable)
    .Parent(NyxTextField('parent'));
end;

procedure ReleaseThroughCallback(AProbe: TViewProbe; const AItem: TNyxItemRef);
begin
  AProbe.OwnedView.Select(AItem);
end;

procedure PrepareReleasingView(AProbe: TViewProbe; const AStore: INyxCollection;
  const ASpec: TNyxCollectionViewSpec; out AToken: INyxCollectionViewSubscription);
begin
  { Keep constructor/receiver interface temporaries inside this scope. Otherwise
    FPC may legitimately retain a factory result until the entire test returns,
    preventing this fixture from testing the application's last real owner. }
  AProbe.OwnedView := NewNyxCollectionView(AStore, ASpec, cpTable);
  AToken := AProbe.OwnedView.Subscribe(AProbe.DropOwner);
end;

function RunNyxCollectionViewTests: Integer;
var
  LStore: INyxCollection;
  LView: INyxCollectionView;
  LTree: INyxCollectionView;
  LSpec: TNyxCollectionViewSpec;
  LCopy: TNyxCollectionViewSpec;
  LDecoded: TNyxCollectionViewSpec;
  LDefaults: INyxCollectionDefaults;
  LContext: INyxCollectionContext;
  LOne: INyxCollection;
  LTwo: INyxCollection;
  LKey: TNyxCollectionRef;
  LChild: TNyxItemRef;
  LToken: INyxCollectionViewSubscription;
  LSecondToken: INyxCollectionViewSubscription;
  LProbe: TViewProbe;
  LSecond: TViewProbe;
  LBefore: Integer;
  LRejected: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxCollection.Create('Collection views: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LProbe := TViewProbe.Create;
  LSecond := TViewProbe.Create;
  try
    LStore := CreateNyxViewCollection;
    LKey := LStore.Snapshot.Key;
    LChild := NyxItem(LKey, 'child / 漢字');
    LSpec := NyxTestTableViewSpec;
    LCopy := LSpec.Scoped(csInstance).Column(NyxTextField('parent'), 'Parent');
    Check((LSpec.Count = 4) and (LCopy.Count = 5) and (LSpec.Scope = csApplication),
      'fluent specifications keep independent columns/scope');
    LDecoded := TNyxCollectionViewSpec.FromData(LCopy.ToData);
    Check(LDecoded.ToData.ToJSON = LCopy.ToData.ToJSON,
      'versioned typed descriptor preserves exact Unicode');
    LDecoded := Default(TNyxCollectionViewSpec);
    Check(LDecoded.ToData.Kind = ndNull, 'absent descriptors use explicit null');
    LRejected := False;
    try
      LCopy := LSpec.Column(NyxTextField('caption'), 'Duplicate');
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSpec.Count = 4), 'duplicate columns preserve the original builder');
    LRejected := False;
    try
      LDecoded := TNyxCollectionViewSpec.FromData(
        TNyxDataValue.ParseJSON('{"version":1,"key":"tasks","scope":"random","parent":"","columns":[]}'));
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unknown scope rejects at the descriptor boundary');
    LView := NewNyxCollectionView(LStore, LSpec, cpTable);
    Check((LView.Snapshot.Count = 3) and (LView.Spec.Count = 4),
      'managed table view binds all four exact field families');
    Check((LView.CellText(LChild, 0) = TNyxText('Child 🌙')) and
      (LView.CellText(LChild, 1) = 'false') and (LView.CellText(LChild, 2) = '1') and
      (LView.CellText(LChild, 3) = '0.5'), 'cells project exact typed values');
    LRejected := False;
    try
      LTree := NewNyxCollectionView(LStore, NyxCollectionView(LKey)
        .Column(NyxTextField('priority'), 'Wrong type'), cpList);
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'a field name cannot bypass its distinct declared type');
    LTree := NewNyxCollectionView(LStore, NyxTestTreeViewSpec, cpTree);
    Check((LTree.ParentIndex(0) = 1) and (LTree.ParentIndex(1) = -1) and
      (LTree.ParentIndex(2) = 0), 'child-before-parent input admits a real hierarchy');
    LToken := LView.Subscribe(LProbe.Changed);
    LSecondToken := LView.Subscribe(LSecond.Changed);
    LView.Select(LChild);
    Check(LView.HasSelection and (LView.Selected.ID = LChild.ID) and
      (LProbe.SelectionCalls = 1) and (LSecond.Calls = 1),
      'selection publishes stable identity to multiple ordered observers');
    LView.Select(LChild);
    Check(LProbe.Calls = 1, 'unchanged selection is a no-op');
    LStore.Move(LChild, 2);
    Check((LView.Selected.ID = LChild.ID) and (LView.Snapshot.IndexOf(LChild) = 2),
      'move retains selected item identity');
    LView.EditWire(LChild, 2, '3');
    Check((LView.CellText(LChild, 2) = '3') and (LView.Selected.ID = LChild.ID),
      'wire edit publishes through exact typed store admission');
    LBefore := LStore.Snapshot.Revision;
    LRejected := False;
    try
      LView.EditWire(LChild, 2, '99');
    except
      on LException: ENyxContract do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.Snapshot.Revision = LBefore) and
      (LView.CellText(LChild, 2) = '3'), 'domain rejection preserves accepted cells/revision');
    LRejected := False;
    try
      LView.Edit(LChild, 2, TNyxStateValue.FromBoolean(True));
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'tagged adapter values cannot bypass column kind');
    LRejected := False;
    try
      LView.Select(NyxItem(NyxCollection('other'), LChild.ID));
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LView.Selected.ID = LChild.ID), 'foreign-scope selection preserves identity');
    LBefore := LStore.Snapshot.Revision;
    LRejected := False;
    try
      LStore.Update(NyxCollectionItem(NyxItem(LKey, 'root'))
        .WithValue(NyxTextField('parent'), 'leaf'));
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.Snapshot.Revision = LBefore),
      'tree cycles reject before publication');
    LRejected := False;
    try
      LStore.Remove(NyxItem(LKey, 'root'));
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LTree.Snapshot.Count = 3), 'orphaning a tree child rejects atomically');
    LProbe.Reenter := True;
    LView.ClearSelection;
    Check(LProbe.RejectedReentry and not LView.HasSelection,
      'reentrant view mutation rejects without undoing accepted selection');
    LProbe.Reenter := False;
    LProbe.Cancel := LSecondToken;
    LBefore := LSecond.Calls;
    LView.Select(LChild);
    Check(not LSecondToken.Connected and (LSecond.Calls = LBefore),
      'an earlier observer can cancel a later token safely');
    LProbe.Fail := True;
    LRejected := False;
    try
      LView.EditWire(LChild, 1, 'true');
    except
      on LException: ENyxCollectionNotification do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LView.CellText(LChild, 1) = 'true'),
      'observer failure reports an already-committed edit');
    LProbe.Fail := False;
    LSecondToken := LView.Subscribe(LSecond.Changed);
    LBefore := LSecond.Calls;
    LProbe.EmptyFailure := True;
    LRejected := False;
    try
      LView.EditWire(LChild, 2, '4');
    except
      on LException: ENyxCollectionNotification do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LView.CellText(LChild, 2) = '4') and
      (LSecond.Calls = LBefore + 1),
      'an empty observer diagnostic still reports commitment and continues later observers');
    LProbe.EmptyFailure := False;
    LStore.Apply([NyxRemove(NyxItem(LKey, 'root')), NyxRemove(LChild),
      NyxRemove(NyxItem(LKey, 'leaf'))]);
    Check((LTree.Snapshot.Count = 0) and not LView.HasSelection,
      'atomic whole-subtree removal clears selected identity');
    LToken.Disconnect;
    LView := nil;
    LTree := nil;
    Check(not LToken.Connected, 'explicit view disconnect is deterministic');

    LStore := CreateNyxViewCollection;
    LDefaults := NewNyxCollectionDefaults.Define(LStore.Snapshot);
    LContext := NewNyxCollectionContext(LDefaults);
    LCopy := NyxTestTableViewSpec.Scoped(csInstance);
    LOne := LContext.Resolve(LCopy, 'page/left / 🌙');
    LTwo := LContext.Resolve(LCopy, 'page/right / 🌙');
    LOne.Update(NyxCollectionItem(LChild).WithValue(NyxBooleanField('done'), True));
    Check(LOne.Snapshot.Item(LChild).GetValue(NyxBooleanField('done')) and
      not LTwo.Snapshot.Item(LChild).GetValue(NyxBooleanField('done')) and
      not LDefaults.Snapshot(LKey).Item(LChild).GetValue(NyxBooleanField('done')),
      'instance stores and saved defaults remain independent');
    Check(LContext.Resolve(LCopy, 'page/left / 🌙').Snapshot.Revision = 1,
      'resolving an existing owner preserves local runtime data');
    LContext.Collections.Collection(LKey).Update(
      NyxCollectionItem(LChild).WithValue(NyxBooleanField('done'), True));
    LStore := LContext.Resolve(LCopy, 'page/new');
    Check(not LStore.Snapshot.Item(LChild).GetValue(NyxBooleanField('done')),
      'new instances seed authored defaults rather than mutated application data');
    LStore := LContext.Resolve(LSpec, '');
    Check(LStore.Snapshot.Item(LChild).GetValue(NyxBooleanField('done')),
      'application scope resolves the shared runtime registry');
    LRejected := False;
    try
      LStore := LContext.Resolve(LCopy, '');
    except
      on LException: ENyxCollection do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'instance scope requires a qualified owner identity');
    LContext := nil;
    LDefaults := nil;
    Check(LOne.Snapshot.Revision = 1, 'retained instance store outlives its context');

    LStore := CreateNyxViewCollection;
    PrepareReleasingView(LProbe, LStore, LSpec, LToken);
    ReleaseThroughCallback(LProbe, LChild);
    Check((LProbe.OwnedView = nil) and not LToken.Connected,
      'last application view reference may be released during notification');
    LToken.Disconnect;
    Check(ReleaseLastMountOwner,
      'borrowed target edit retains its attachment until the last-owner callback returns');
  finally

    if LToken <> nil then
    begin
      LToken.Disconnect;
    end;

    if LSecondToken <> nil then
    begin
      LSecondToken.Disconnect;
    end;
    LView := nil;
    LTree := nil;
    LProbe.OwnedView := nil;
    LProbe.Free;
    LSecond.Free;
  end;
end;

end.
