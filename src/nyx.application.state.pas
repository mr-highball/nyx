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

unit nyx.application.state;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.state,
  nyx.collections.registry,
  nyx.collections.view,
  nyx.collections.bindings,
  nyx.model;

type
  { Owns an application's runtime scalar store and admitted page prototypes.
    The authored document is borrowed and must remain unchanged while mounted;
    its defaults and nodes are never mutated by application interaction.

    All pages, including unmounted pages, validate each proposed store snapshot.
    This prevents admitting an invalid range/key/kind that fails only on later
    navigation. Prototypes retain no target handles. Subscription disconnects
    before prototypes/store are freed. Renderers borrow State and must unmount
    before this owner is destroyed. }
  TNyxApplicationState = class
  private
    FState: TNyxState;
    FCollections: INyxCollections;
    FCollectionContext: INyxCollectionContext;
    FPageCollections: array of INyxCollectionBindings;
    FPages: array of TNyxNode;
    FSubscription: TNyxStateSubscription;
    procedure ValidateCandidate(ACandidate: TNyxState; AChanges: TNyxStateChanges);
  public
    constructor Create(ADocument: TNyxDocument);
    destructor Destroy; override;
    property State: TNyxState read FState;
    { Managed runtime stores are independent of saved defaults and sibling
      applications. Keep the same registry through navigation. A retained store
      contains no application/document/renderer reference and can outlive us. }
    property Collections: INyxCollections read FCollections;
    { Retained page bindings share one context and keep hidden-page validation,
      instance data and selection alive through navigation. Unknown IDs reject. }
    function PageCollections(const APageID: TNyxText): INyxCollectionBindings;
  end;

implementation

uses
  nyx.composition,
  nyx.binding;

constructor TNyxApplicationState.Create(ADocument: TNyxDocument);
var
  LIndex: Integer;
begin
  inherited Create;

  if (ADocument = nil) or (ADocument.Count = 0) then
  begin
    raise ENyxState.Create('An application requires at least one page');
  end;
  ADocument.Validate;
  FState := ADocument.State.Clone;
  FCollectionContext := NewNyxCollectionContext(ADocument.Collections);
  FCollections := FCollectionContext.Collections;
  SetLength(FPages, ADocument.Count);
  SetLength(FPageCollections, ADocument.Count);
  for LIndex := 0 to ADocument.Count - 1 do
  begin
    FPages[LIndex] := RealizeNyxView(ADocument, ADocument.Pages[LIndex]);
    ApplyNyxBindings(FPages[LIndex], FState);
    FPageCollections[LIndex] := NewNyxCollectionBindings(FPages[LIndex], FCollectionContext);
  end;
  FSubscription := FState.Subscribe(nil, ValidateCandidate);
end;

destructor TNyxApplicationState.Destroy;
var
  LIndex: Integer;
begin
  FSubscription.Free;
  FPageCollections := nil;
  for LIndex := 0 to Length(FPages) - 1 do
  begin
    FPages[LIndex].Free;
  end;
  FState.Free;
  FCollections := nil;
  FCollectionContext := nil;
  inherited Destroy;
end;

procedure TNyxApplicationState.ValidateCandidate(ACandidate: TNyxState;
  AChanges: TNyxStateChanges);
var
  LIndex: Integer;
  LPage: TNyxNode;
begin
  for LIndex := 0 to Length(FPages) - 1 do
  begin
    LPage := FPages[LIndex].Clone;
    try
      ApplyNyxBindings(LPage, ACandidate);
    finally
      LPage.Free;
    end;
  end;
end;

function TNyxApplicationState.PageCollections(const APageID: TNyxText): INyxCollectionBindings;
var
  LIndex: Integer;
begin
  for LIndex := 0 to Length(FPages) - 1 do
  begin

    if FPages[LIndex].ID = APageID then
    begin
      Exit(FPageCollections[LIndex]);
    end;
  end;
  raise ENyxState.Create('Unknown application page: ' + APageID);
end;

end.
