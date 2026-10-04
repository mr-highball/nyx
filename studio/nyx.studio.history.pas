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
unit nyx.studio.history;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.source;

type
  { One entry carries the complete immutable design/source checkpoint. This
    prevents two history lists from diverging if allocation fails halfway through
    recording a pair. Entries contain managed text only, never mutable documents,
    nodes, renderer handles or shared dynamic arrays. Sessions own each history;
    Add/Delete/Clear maintain its retained text-byte total without rescanning or
    encoding entries. The caller selects its count/byte retention policy. }
  TNyxStudioHistory = class
  private
    FEntries: array of TNyxSourceCheckpoint;
    FStorageBytes: Int64;
    function GetCount: Integer;
    function GetLast: TNyxSourceCheckpoint;
  public
    { Allocation precedes publication of the new entry and accounting. }
    procedure Add(const ACheckpoint: TNyxSourceCheckpoint);
    { Invalid indexes raise without changing entries or accounting. Managed text
      references are released when an entry leaves the history. }
    procedure Delete(AIndex: Integer);
    procedure Clear;
    property Count: Integer read GetCount;
    property Last: TNyxSourceCheckpoint read GetLast;
    property StorageBytes: Int64 read FStorageBytes;
  end;

implementation

uses
  SysUtils;

function TNyxStudioHistory.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TNyxStudioHistory.GetLast: TNyxSourceCheckpoint;
begin

  if Count = 0 then
  begin
    raise ERangeError.Create('History has no checkpoint');
  end;
  Result := FEntries[Count - 1];
end;

procedure TNyxStudioHistory.Add(const ACheckpoint: TNyxSourceCheckpoint);
var
  LIndex: Integer;
  LSize: Int64;
begin
  LIndex := Count;
  LSize := ACheckpoint.StorageBytes;
  SetLength(FEntries, LIndex + 1);
  FEntries[LIndex] := ACheckpoint;
  FStorageBytes := FStorageBytes + LSize;
end;

procedure TNyxStudioHistory.Delete(AIndex: Integer);
var
  LIndex: Integer;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ERangeError.Create('Invalid history checkpoint index');
  end;
  FStorageBytes := FStorageBytes - FEntries[AIndex].StorageBytes;
  for LIndex := AIndex to Count - 2 do
  begin
    FEntries[LIndex] := FEntries[LIndex + 1];
  end;
  { Explicitly release the trailing value on both compilers before truncation. }
  FEntries[Count - 1] := Default(TNyxSourceCheckpoint);
  SetLength(FEntries, Count - 1);
end;

procedure TNyxStudioHistory.Clear;
begin
  SetLength(FEntries, 0);
  FStorageBytes := 0;
end;

end.
