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
program nyx_arrangement_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.projection.refresh
  {$ifdef PAS2JS}, Web{$endif};

type
  { An independent managed implementation proves that rearrangement retains
    actual owner anchors, not merely borrowed descriptors. The descriptor has
    no reference back to this implementation; the tree owns the interface. }
  TManagedPart = class(TInterfacedObject, INyxNode)
  private
    FNode: TNyxNode;
  public
    constructor Create(ANode: TNyxNode);
    destructor Destroy; override;
    function GetNode: TNyxNode;
  end;

var
  GChecks: Integer;
  GDisposed: Integer;

constructor TManagedPart.Create(ANode: TNyxNode);
begin
  inherited Create;
  FNode := ANode;
  FNode.AcquireReference;
  FNode.ReleaseOwnership;
end;

destructor TManagedPart.Destroy;
begin
  Inc(GDisposed);
  FNode.ReleaseReference;
  inherited Destroy;
end;

function TManagedPart.GetNode: TNyxNode;
begin
  Result := FNode;
end;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxModel.Create('Arrangement: ' + AReason);
  end;
  Inc(GChecks);
end;

function Realized(AKind: TNyxKind; const AID: TNyxText): TNyxNode;
begin
  Result := TNyxNode.CreateRealized(NyxKindName(AKind), AID, AID, AID);
  Result.SetInstanceScope('room');
end;

procedure Run;
var
  LRoot: TNyxNode;
  LOriginal: TNyxNode;
  LShape: TNyxNode;
  LPart: TNyxNode;
  LManaged: INyxNode;
  LRepeat: Integer;
begin
  LRoot := Realized(nkPage, 'room');
  LOriginal := nil;
  LShape := nil;
  try
    LRoot.Add(Realized(nkColumn, 'left'));
    LRoot.Add(Realized(nkColumn, 'right'));
    LPart := Realized(nkInput, 'notes-🌙');
    LPart.Configure.Value('Independent live draft / 🌙').Done;
    LManaged := TManagedPart.Create(LPart);
    LRoot.Find('left').Add(LManaged);
    LManaged := nil;
    LOriginal := LRoot.Clone;
    LShape := LRoot.Clone;
    LShape.Find('right').Add(LShape.Find('left').Extract(0));
    LShape.Find('right').Add(LShape.Extract(0));
    Check(not CanRefreshNyxProjection(LRoot, LShape),
      'Scalar guard stays strict for a changed hierarchy');
    Check(CanArrangeNyxProjection(LRoot, LShape),
      'Independent structural admission preserves unchanged control meaning');
    for LRepeat := 1 to 8 do
    begin
      Check(LRoot.ArrangeLike(LShape) and (LRoot.Find('notes-🌙') = LPart) and
        (LPart.Parent.ID = 'right') and (LRoot.Find('left').Parent.ID = 'right'),
        'Prepared ownership moves the exact managed part and host');
      LManaged := LPart.ComponentReference;
      Check((LManaged <> nil) and (GDisposed = 0) and
        (LPart.Prop('value') = TNyxText('Independent live draft / 🌙')),
        'New owner retains the original implementation and live value');
      LManaged := nil;
      Check(LRoot.ArrangeLike(LOriginal) and (LPart.Parent.ID = 'left') and
        (LRoot.Children[0].ID = 'left') and (GDisposed = 0),
        'Reversal retains original interfaces and child order');
    end;
    Check(not LRoot.ArrangeLike(LRoot) and (LPart.Parent.ID = 'left'),
      'A source tree cannot masquerade as an independent candidate');
    LShape.Add(Realized(nkBadge, 'added'));
    Check(not LRoot.ArrangeLike(LShape) and (LRoot.Count = 2) and
      (LPart.Parent.ID = 'left') and (GDisposed = 0),
      'Additional identity refuses before any owner is changed');
    LShape.Remove(LShape.Find('added'));
    LShape.Find('notes-🌙').SetInstanceScope('another instance');
    Check(not LRoot.ArrangeLike(LShape) and (LPart.Parent.ID = 'left'),
      'An identical local name cannot cross instance state ownership');
    FreeAndNil(LShape);
    LShape := LRoot.Clone;
    LShape.Add(Realized(nkColumn, 'left'));
    Check(not LRoot.ArrangeLike(LShape) and (LRoot.Count = 2),
      'Duplicate runtime keys refuse the whole candidate');
    FreeAndNil(LShape);

    LShape := Realized(nkPage, 'room');
    LShape.Add(Realized(nkColumn, 'left'));
    Check(not LRoot.ArrangeLike(LShape) and (LPart.Parent.ID = 'left'),
      'A missing identity cannot drop a retained implementation');
    FreeAndNil(LShape);

    LShape := LRoot.Clone;
    LShape.Find('right').Add(LShape.Find('left').Extract(0));
    LShape.Find('notes-🌙').Configure.InputType(niPassword).Done;
    Check(not CanArrangeNyxProjection(LRoot, LShape) and (LPart.Parent.ID = 'left'),
      'Rearrangement cannot bypass constructor-property admission');
    FreeAndNil(LShape);

    LShape := LRoot.Clone;
    LShape.Find('left').Add(Realized(nkSplitView, 'panes'));
    LShape.Find('panes').Add(Realized(nkColumn, 'first'));
    LShape.Find('panes').Add(Realized(nkColumn, 'second'));
    FreeAndNil(LOriginal);
    LOriginal := LShape.Clone;
    LOriginal.Find('right').Add(LOriginal.Find('panes').Extract(0));
    Check(not CanArrangeNyxProjection(LShape, LOriginal),
      'Special pane-parent changes stay outside retained admission');
  finally
    LManaged := nil;
    LShape.Free;
    LOriginal.Free;
    LRoot.Free;
  end;
end;

begin
  try
    Run;
    Check(GDisposed = 1, 'Teardown releases the one actual managed implementation exactly once');
    WriteLn('PASS ', GChecks, ' owned arrangement checks');
    {$ifdef PAS2JS}
    document.body.setAttribute('data-result', 'passed');
    document.body.setAttribute('data-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-result', 'failed');
      document.body.setAttribute('data-error', LException.Message);
      {$else}ExitCode := 1;{$endif}
    end;
  end;
end.
