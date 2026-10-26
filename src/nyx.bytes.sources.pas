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
unit nyx.bytes.sources;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.bytes;

const
  NyxMaximumByteChunk = 1024 * 1024;

type
  { One private sequential cursor. Read returns owned bytes, at most the positive
    requested count (up to NyxMaximumByteChunk); empty means final EOF. Short
    nonempty reads are allowed. A reader has no shared mutable cursor. Read on
    its owning thread; handing immutable source data to another owner is explicit. }
  INyxByteReader = interface(IInterface)
    ['{5EE00911-3D68-445D-9E68-BB0A83D08DC6}']
    function Read(AMaximumBytes: Integer): TNyxBytes;
  end;

  { Immutable, exact known-length byte content. Open returns an independent
    cursor at offset zero; opening never consumes this source. Factories own
    their inputs and no stream/widget/application/callback. Custom sources must
    keep ByteCount and bytes stable for every cursor; consumers verify lengths. }
  INyxByteSource = interface(IInterface)
    ['{D2D74976-6278-493D-B6AC-7C9AA8361C68}']
    function GetByteCount: Integer;
    function Open: INyxByteReader;
    property ByteCount: Integer read GetByteCount;
  end;

{ Validates all Unicode before publication, retaining immutable text rather than
  another whole encoded array. Reads use one output chunk plus four scalar bytes.
  Chunks may split UTF-8 sequences; concatenating all chunks is exact UTF-8. }
function NyxUTF8ByteSource(const AText: TNyxText): INyxByteSource;

implementation

type
  { Retains one immutable text value and its validated length. Native string
    sharing and browser immutable strings avoid an additional encoded payload. }
  TTextSource = class(TInterfacedObject, INyxByteSource)
  private
    FText: TNyxText;
    FCount: Integer;
  public
    constructor Create(const AText: TNyxText);
    function GetByteCount: Integer;
    function Open: INyxByteReader;
  end;

  { Independent scalar encoder. Only the cursor and up to four pending bytes
    mutate; another opened reader never observes those changes. }
  TTextReader = class(TInterfacedObject, INyxByteReader)
  private
    FText: TNyxText;
    FIndex: Integer;
    FRemaining: Integer;
    FPending: array[0..3] of Byte;
    FOffset: Integer;
    FCount: Integer;
    procedure Next;
  public
    constructor Create(const AText: TNyxText; ACount: Integer);
    function Read(AMaximumBytes: Integer): TNyxBytes;
  end;

constructor TTextSource.Create(const AText: TNyxText);
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: TNyxTextBytes;
begin
  inherited Create;
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AText) do
  begin

    if Ord(AText[LIndex]) < $80 then
    begin
      Inc(LIndex);
      Inc(LCount);
    end
    else
    begin

      if not NyxNextScalar(AText, LIndex, LScalar) then
      begin
        raise ENyxBytes.Create('Byte source requires valid Unicode text');
      end;

      if LScalar < $800 then
      begin
        Inc(LCount, 2);
      end
      else if LScalar < $10000 then
      begin
        Inc(LCount, 3);
      end
      else
      begin
        Inc(LCount, 4);
      end;
    end;

    if LCount > High(Integer) then
    begin
      raise ENyxBytes.Create('Byte source exceeds portable signed length');
    end;
  end;
  FCount := Integer(LCount);
  FText := AText;
end;

function TTextSource.GetByteCount: Integer;
begin
  Result := FCount;
end;

function TTextSource.Open: INyxByteReader;
begin
  Result := TTextReader.Create(FText, FCount);
end;

constructor TTextReader.Create(const AText: TNyxText; ACount: Integer);
begin
  inherited Create;
  FText := AText;
  FRemaining := ACount;
  FIndex := 1;
end;

procedure TTextReader.Next;
var
  LScalar: Integer;
begin

  if (FIndex <= Length(FText)) and (Ord(FText[FIndex]) < $80) then
  begin
    LScalar := Ord(FText[FIndex]);
    Inc(FIndex);
  end
  else if not NyxNextScalar(FText, FIndex, LScalar) then
  begin
    raise ENyxBytes.Create('Immutable byte source ended before its declared length');
  end;
  FOffset := 0;
  FCount := 1;

  if LScalar < $80 then
  begin
    FPending[0] := LScalar;
  end
  else if LScalar < $800 then
  begin
    FCount := 2;
    FPending[0] := $C0 or (LScalar shr 6);
    FPending[1] := $80 or (LScalar and $3F);
  end
  else if LScalar < $10000 then
  begin
    FCount := 3;
    FPending[0] := $E0 or (LScalar shr 12);
    FPending[1] := $80 or ((LScalar shr 6) and $3F);
    FPending[2] := $80 or (LScalar and $3F);
  end
  else
  begin
    FCount := 4;
    FPending[0] := $F0 or (LScalar shr 18);
    FPending[1] := $80 or ((LScalar shr 12) and $3F);
    FPending[2] := $80 or ((LScalar shr 6) and $3F);
    FPending[3] := $80 or (LScalar and $3F);
  end;
end;

function TTextReader.Read(AMaximumBytes: Integer): TNyxBytes;
var
  LCount: Integer;
  LIndex: Integer;
begin
  Result := nil;

  if (AMaximumBytes <= 0) or (AMaximumBytes > NyxMaximumByteChunk) then
  begin
    raise ENyxBytes.Create('Byte reader requires a positive bounded chunk size');
  end;
  LCount := AMaximumBytes;

  if LCount > FRemaining then
  begin
    LCount := FRemaining;
  end;
  SetLength(Result, LCount);
  for LIndex := 0 to LCount - 1 do
  begin

    if FOffset = FCount then
    begin
      Next;
    end;
    Result[LIndex] := FPending[FOffset];
    Inc(FOffset);
  end;
  Dec(FRemaining, LCount);
end;

function NyxUTF8ByteSource(const AText: TNyxText): INyxByteSource;
begin
  Result := TTextSource.Create(AText);
end;

end.
