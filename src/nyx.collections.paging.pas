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
unit nyx.collections.paging;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.collections;

type
  { Immutable authored page defaults. Positive Integer values have no arbitrary
    toolkit limit. Every fluent call validates a copy; the original survives a
    refusal. Default records are absent, preserving unpaged legacy descriptors. }
  TNyxCollectionPaging = record
  private
    FDefined: Boolean;
    FPageSize: Integer;
    FInitialPage: Integer;
  public
    function PageSize(AItems: Integer): TNyxCollectionPaging;
    function InitialPage(APage: Integer): TNyxCollectionPaging;
    procedure Validate;
    { Version-one exact persistence. Null means absent; unknown/extra/missing
      members and non-Integer primitives refuse before publication. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCollectionPaging; static;
    property Defined: Boolean read FDefined;
    property ItemsPerPage: Integer read FPageSize;
    property FirstPage: Integer read FInitialPage;
  end;

  { Pure, copied runtime geometry after filtering and stable ordering. Pages
    are one-based; an empty dataset retains Page=1, Pages=0, First=0, Count=0.
    Requested pages clamp to actual membership, without Integer overflow. }
  TNyxCollectionPageWindow = record
  private
    FPage: Integer;
    FPages: Integer;
    FFirst: Integer;
    FCount: Integer;
    FTotal: Integer;
  public
    function CanPrevious: Boolean;
    function CanNext: Boolean;
    { Geometry is constructed as one admitted value. Callers cannot forge an
      interval whose page number disagrees with its membership. }
    property Page: Integer read FPage;
    property Pages: Integer read FPages;
    property First: Integer read FFirst;
    property Count: Integer read FCount;
    property Total: Integer read FTotal;
  end;

function NyxCollectionPaging: TNyxCollectionPaging;
function NyxCollectionPageWindow(ATotal, APageSize, ARequestedPage: Integer):
  TNyxCollectionPageWindow;

{ Retained read projection. Items/schema/revision remain owned by the source;
  no store, renderer, callback or document is created. Membership lookup maps
  only the admitted contiguous page, and DataBytes retains full source cost.
  Paging must follow query projection, never precede filtering/sorting. }
function NyxPageSnapshot(const ASource: INyxCollectionSnapshot;
  const AWindow: TNyxCollectionPageWindow): INyxCollectionSnapshot;

implementation

type
  TPageSnapshot = class(TInterfacedObject, INyxCollectionSnapshot)
  private
    FSource: INyxCollectionSnapshot;
    FWindow: TNyxCollectionPageWindow;
  public
    constructor Create(const ASource: INyxCollectionSnapshot;
      const AWindow: TNyxCollectionPageWindow);
    function GetKey: TNyxCollectionRef;
    function GetSchema: TNyxCollectionSchema;
    function GetCount: Integer;
    function GetRevision: Integer;
    function GetDataBytes: Integer;
    function ItemAt(AIndex: Integer): TNyxCollectionItem;
    function Item(const ARef: TNyxItemRef): TNyxCollectionItem;
    function IndexOf(const ARef: TNyxItemRef): Integer;
    function Has(const ARef: TNyxItemRef): Boolean;
  end;

function NyxCollectionPaging: TNyxCollectionPaging;
begin
  Result := Default(TNyxCollectionPaging);
  Result.FDefined := True;
  Result.FPageSize := 25;
  Result.FInitialPage := 1;
end;

procedure TNyxCollectionPaging.Validate;
begin

  if not FDefined or (FPageSize < 1) or (FInitialPage < 1) then
  begin
    raise ENyxCollection.Create('Create paging with a positive page size and initial page');
  end;
end;

function TNyxCollectionPaging.PageSize(AItems: Integer): TNyxCollectionPaging;
begin
  Result := Self;
  Result.FPageSize := AItems;
  Result.Validate;
end;

function TNyxCollectionPaging.InitialPage(APage: Integer): TNyxCollectionPaging;
begin
  Result := Self;
  Result.FInitialPage := APage;
  Result.Validate;
end;

function TNyxCollectionPaging.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('pageSize', NyxData(FPageSize)), NyxField('initialPage', NyxData(FInitialPage))]);
end;

class function TNyxCollectionPaging.FromData(const AData: TNyxDataValue): TNyxCollectionPaging;
var
  LIndex: Integer;
  LKey: TNyxText;
begin
  Result := Default(TNyxCollectionPaging);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 3) then
  begin
    raise ENyxCollection.Create('Paging requires its complete version-one object');
  end;
  for LIndex := 0 to AData.Count - 1 do
  begin
    LKey := AData.Key(LIndex);

    if (LKey <> 'version') and (LKey <> 'pageSize') and (LKey <> 'initialPage') then
    begin
      raise ENyxCollection.Create('Unknown paging member');
    end;
  end;

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxCollection.Create('Unknown paging version');
  end;
  Result := NyxCollectionPaging.PageSize(AData.Field('pageSize').AsInteger)
    .InitialPage(AData.Field('initialPage').AsInteger);
end;

function NyxCollectionPageWindow(ATotal, APageSize, ARequestedPage: Integer):
  TNyxCollectionPageWindow;
begin

  if (ATotal < 0) or (APageSize < 1) or (ARequestedPage < 1) then
  begin
    raise ENyxCollection.Create('Page geometry requires nonnegative membership and positive arguments');
  end;
  Result := Default(TNyxCollectionPageWindow);
  Result.FTotal := ATotal;
  Result.FPage := 1;

  if ATotal = 0 then
  begin
    Exit;
  end;
  { Subtract before ceiling division; adding PageSize-1 could overflow. The
    admitted clamped page ensures First never exceeds Total-1. }
  Result.FPages := ((ATotal - 1) div APageSize) + 1;
  Result.FPage := ARequestedPage;

  if Result.Page > Result.Pages then
  begin
    Result.FPage := Result.Pages;
  end;
  Result.FFirst := (Result.Page - 1) * APageSize;
  Result.FCount := ATotal - Result.First;

  if Result.Count > APageSize then
  begin
    Result.FCount := APageSize;
  end;
end;

function TNyxCollectionPageWindow.CanPrevious: Boolean;
begin
  Result := (Pages > 0) and (Page > 1);
end;

function TNyxCollectionPageWindow.CanNext: Boolean;
begin
  Result := (Pages > 0) and (Page < Pages);
end;

constructor TPageSnapshot.Create(const ASource: INyxCollectionSnapshot;
  const AWindow: TNyxCollectionPageWindow);
begin
  inherited Create;

  if (ASource = nil) or (AWindow.Page < 1) or
    (AWindow.Total <> ASource.Count) or (AWindow.First < 0) or
    (AWindow.Count < 0) or (AWindow.First > ASource.Count) or
    (AWindow.Count > ASource.Count - AWindow.First) then
  begin
    raise ENyxCollection.Create('Page projection does not match its retained source');
  end;
  FSource := ASource;
  FWindow := AWindow;
end;

function TPageSnapshot.GetKey: TNyxCollectionRef;
begin
  Result := FSource.Key;
end;

function TPageSnapshot.GetSchema: TNyxCollectionSchema;
begin
  Result := FSource.Schema;
end;

function TPageSnapshot.GetCount: Integer;
begin
  Result := FWindow.Count;
end;

function TPageSnapshot.GetRevision: Integer;
begin
  Result := FSource.Revision;
end;

function TPageSnapshot.GetDataBytes: Integer;
begin
  Result := FSource.DataBytes;
end;

function TPageSnapshot.ItemAt(AIndex: Integer): TNyxCollectionItem;
begin

  if (AIndex < 0) or (AIndex >= FWindow.Count) then
  begin
    raise ENyxCollection.Create('Page item index is outside visible membership');
  end;
  Result := FSource.ItemAt(FWindow.First + AIndex);
end;

function TPageSnapshot.IndexOf(const ARef: TNyxItemRef): Integer;
begin
  Result := FSource.IndexOf(ARef);

  if (Result < FWindow.First) or (Result - FWindow.First >= FWindow.Count) then
  begin
    Exit(-1);
  end;
  Dec(Result, FWindow.First);
end;

function TPageSnapshot.Item(const ARef: TNyxItemRef): TNyxCollectionItem;
var
  LIndex: Integer;
begin
  LIndex := IndexOf(ARef);

  if LIndex < 0 then
  begin
    raise ENyxCollection.Create('Item is outside this page');
  end;
  Result := ItemAt(LIndex);
end;

function TPageSnapshot.Has(const ARef: TNyxItemRef): Boolean;
begin
  Result := IndexOf(ARef) >= 0;
end;

function NyxPageSnapshot(const ASource: INyxCollectionSnapshot;
  const AWindow: TNyxCollectionPageWindow): INyxCollectionSnapshot;
begin
  Result := TPageSnapshot.Create(ASource, AWindow);
end;

end.
