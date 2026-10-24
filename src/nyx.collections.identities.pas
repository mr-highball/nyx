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
unit nyx.collections.identities;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.collections, nyx.collections.selection,
  nyx.collections.clipboard;

type
  { Identity-only text is distinct from typed column-cell/row output. It owns
    copied scoped references and exact TSV bytes, never a dataset/view/store,
    selection controller, document, formatter, editor or clipboard authority. }
  INyxCollectionIdentityText = interface(IInterface)
    ['{834C4504-CAC4-43B0-98D6-240580C442D2}']
    function GetText: TNyxText;
    function GetByteCount: Integer;
    function GetKey: TNyxCollectionRef;
    function GetRevision: Integer;
    function GetMembership: TNyxCollectionCopyMembership;
    function GetCount: Integer;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function GetIncludeHeader: Boolean;
    function GetLineEnding: TNyxCollectionCopyLineEnding;
    property Text: TNyxText read GetText;
    property ByteCount: Integer read GetByteCount;
    property Key: TNyxCollectionRef read GetKey;
    property Revision: Integer read GetRevision;
    property Membership: TNyxCollectionCopyMembership read GetMembership;
    property Count: Integer read GetCount;
    property IncludeHeader: Boolean read GetIncludeHeader;
    property LineEnding: TNyxCollectionCopyLineEnding read GetLineEnding;
  end;

{ Reuses exact bounded source/page selection admission. Source mode includes
  hidden/off-page selected references in source order; page mode intersects the
  explicit actual page in page order. Output has one ID column, optional literal
  ID header and no trailing terminator. This is identity metadata, never a schema
  field or CellValue. Existing scoped-reference validation refuses control/NUL/
  newline IDs before output; supplementary Unicode and quotes stay exact. }
function NewNyxCollectionIdentityText(const ASource: INyxCollectionSnapshot;
  const ASelection: TNyxCollectionSelectionSnapshot;
  AMembership: TNyxCollectionCopyMembership; const APage: INyxCollectionSnapshot;
  AHeader: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionIdentityText;

implementation

uses nyx.bytes;

type
  { Private builder publishes only after complete row/byte admission. Fields
    contain scalar/identity values; all getters are owner-free after publication. }
  TIdentityText = class(TInterfacedObject, INyxCollectionIdentityText)
  public
    Text: TNyxText;
    Bytes: Integer;
    Key: TNyxCollectionRef;
    Revision: Integer;
    Membership: TNyxCollectionCopyMembership;
    Items: TNyxItemRefs;
    Header: Boolean;
    Lines: TNyxCollectionCopyLineEnding;
    function GetText: TNyxText;
    function GetByteCount: Integer;
    function GetKey: TNyxCollectionRef;
    function GetRevision: Integer;
    function GetMembership: TNyxCollectionCopyMembership;
    function GetCount: Integer;
    function ItemAt(AIndex: Integer): TNyxItemRef;
    function GetIncludeHeader: Boolean;
    function GetLineEnding: TNyxCollectionCopyLineEnding;
  end;

function NewNyxCollectionIdentityText(const ASource: INyxCollectionSnapshot;
  const ASelection: TNyxCollectionSelectionSnapshot;
  AMembership: TNyxCollectionCopyMembership; const APage: INyxCollectionSnapshot;
  AHeader: Boolean; ALineEnding: TNyxCollectionCopyLineEnding;
  const ALimits: TNyxCollectionClipboardLimits): INyxCollectionIdentityText;
var
  LMembers: TNyxCollectionCopyMembers;
  LPayload: TIdentityText;
  LParts: TNyxStrings;
  LLine: TNyxText;
  LIndex: Integer;
  LLimits: TNyxCollectionClipboardLimits;

  procedure Append(const AText: TNyxText);
  var
    LBytes: Integer;
  begin
    LBytes := NyxUTF8ByteCount(AText);

    if LBytes > LLimits.MaximumUTF8Bytes - LPayload.Bytes then
    begin
      raise ENyxCollectionClipboard.Create('Selected identity output exceeds its UTF-8 budget');
    end;
    Inc(LPayload.Bytes, LBytes);
    LParts.Add(AText);
  end;

begin
  LLimits := ALimits;
  LLimits.Validate;

  if (Ord(ALineEnding) < Ord(Low(TNyxCollectionCopyLineEnding))) or
    (Ord(ALineEnding) > Ord(High(TNyxCollectionCopyLineEnding))) then
  begin
    raise ENyxCollectionClipboard.Create('Selected identity output requires a known line ending');
  end;
  LMembers := CaptureNyxCollectionCopyMembers(ASource, ASelection,
    AMembership, APage, 1, LLimits);
  LPayload := TIdentityText.Create;
  try
    LPayload.Key := LMembers.Key;
    LPayload.Revision := LMembers.Revision;
    LPayload.Membership := AMembership;
    LPayload.Header := AHeader;
    LPayload.Lines := ALineEnding;
    SetLength(LPayload.Items, LMembers.Count);
    LParts := TNyxStrings.Create;
    try
      LLine := #10;

      if ALineEnding = nccleCRLF then
      begin
        LLine := #13#10;
      end;

      if AHeader then
      begin
        Append('ID');
        Append(LLine);
      end;
      for LIndex := 0 to LMembers.Count - 1 do
      begin

        if LIndex > 0 then
        begin
          Append(LLine);
        end;
        LPayload.Items[LIndex] := NyxItem(LMembers.Key, LMembers.ItemAt(LIndex).Ref.ID);
        Append(NyxCollectionTSVCell(LPayload.Items[LIndex].ID,
          LLimits.MaximumUTF8Bytes - LPayload.Bytes));
      end;
      LPayload.Text := LParts.Join;
    finally
      LParts.Free;
    end;
    Result := LPayload;
  except
    LPayload.Free;
    raise;
  end;
end;

function TIdentityText.GetText: TNyxText;
begin
  Result := Text;
end;

function TIdentityText.GetByteCount: Integer;
begin
  Result := Bytes;
end;

function TIdentityText.GetKey: TNyxCollectionRef;
begin
  Result := Key;
end;

function TIdentityText.GetRevision: Integer;
begin
  Result := Revision;
end;

function TIdentityText.GetMembership: TNyxCollectionCopyMembership;
begin
  Result := Membership;
end;

function TIdentityText.GetCount: Integer;
begin
  Result := Length(Items);
end;

function TIdentityText.ItemAt(AIndex: Integer): TNyxItemRef;
begin

  if (AIndex < 0) or (AIndex >= Length(Items)) then
  begin
    raise ENyxCollectionClipboard.Create('Selected identity index is outside its copied membership');
  end;
  Result := NyxItem(Key, Items[AIndex].ID);
end;

function TIdentityText.GetIncludeHeader: Boolean;
begin
  Result := Header;
end;

function TIdentityText.GetLineEnding: TNyxCollectionCopyLineEnding;
begin
  Result := Lines;
end;

end.
