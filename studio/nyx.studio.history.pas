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
  nyx.source, nyx.text;

type
  { Immutable editor checkpoint. The admitted source frame remains independent
    of Studio, while this value adds the exact unfinished buffer and its original
    baseline. Pending distinguishes an empty draft from no draft. Values contain
    managed text only, with no document/session/control references. }
  TNyxStudioCheckpoint = record
  private
    FFrame: TNyxSourceCheckpoint;
    FDraft: TNyxText;
    FDraftBase: TNyxText;
    FPending: Boolean;
    function GetDesign: TNyxText;
    function GetSource: TNyxText;
    function GetStorageBytes: TNyxTextBytes;
  public
    property Frame: TNyxSourceCheckpoint read FFrame;
    property Design: TNyxText read GetDesign;
    property Source: TNyxText read GetSource;
    property Draft: TNyxText read FDraft;
    property DraftBase: TNyxText read FDraftBase;
    property Pending: Boolean read FPending;
    property StorageBytes: TNyxTextBytes read GetStorageBytes;
  end;

  { One entry carries immutable design/source and unfinished draft/base. This
    prevents two history lists from diverging if allocation fails halfway through
    recording a pair. Entries contain managed text only, never mutable documents,
    nodes, renderer handles or shared dynamic arrays. Sessions own each history;
    Add/Delete/Clear maintain its retained text-byte total without rescanning or
    encoding entries. The caller selects its count/byte retention policy. }
  TNyxStudioHistory = class
  private
    FEntries: array of TNyxStudioCheckpoint;
    FStorageBytes: TNyxTextBytes;
    function GetCount: Integer;
    function GetLast: TNyxSourceCheckpoint;
    function GetLastState: TNyxStudioCheckpoint;
  public
    { Allocation precedes publication of the new entry and accounting. }
    procedure Add(const ACheckpoint: TNyxSourceCheckpoint); overload;
    procedure Add(const ACheckpoint: TNyxStudioCheckpoint); overload;
    { Invalid indexes raise without changing entries or accounting. Managed text
      references are released when an entry leaves the history. }
    procedure Delete(AIndex: Integer);
    procedure Clear;
    { Borrow-free immutable value at the exact oldest-to-newest index. Invalid
      indexes refuse; callers never receive the mutable backing array. }
    function Entry(AIndex: Integer): TNyxSourceCheckpoint;
    { Complete editor value at the same index. The legacy Entry/Last frame-only
      accessors remain useful to source-workspace consumers. Studio always uses
      State/LastState so unfinished buffers cannot disappear during traversal. }
    function State(AIndex: Integer): TNyxStudioCheckpoint;
    property Count: Integer read GetCount;
    property Last: TNyxSourceCheckpoint read GetLast;
    property LastState: TNyxStudioCheckpoint read GetLastState;
    property StorageBytes: TNyxTextBytes read FStorageBytes;
  end;

{ Wrap an already admitted immutable frame. The complete overload validates
  portable Unicode but does not parse unfinished Pascal. A nonpending state
  retains no draft buffers; the caller keeps its own source diagnostic. }
function NyxStudioCheckpoint(const AFrame: TNyxSourceCheckpoint): TNyxStudioCheckpoint; overload;
function NyxStudioCheckpoint(const AFrame: TNyxSourceCheckpoint;
  const ADraft, ABase: TNyxText; APending: Boolean): TNyxStudioCheckpoint; overload;

implementation

uses
  SysUtils, nyx.editing;

function NyxStudioCheckpoint(const AFrame: TNyxSourceCheckpoint): TNyxStudioCheckpoint;
begin
  Result := Default(TNyxStudioCheckpoint);
  Result.FFrame := AFrame;
end;

function NyxStudioCheckpoint(const AFrame: TNyxSourceCheckpoint;
  const ADraft, ABase: TNyxText; APending: Boolean): TNyxStudioCheckpoint;
begin
  Result := NyxStudioCheckpoint(AFrame);

  if APending then
  begin
    NyxTextScalarCount(ADraft);
    NyxTextScalarCount(ABase);
    Result.FDraft := ADraft;
    Result.FDraftBase := ABase;
    Result.FPending := True;
  end;
end;

function TNyxStudioCheckpoint.GetDesign: TNyxText;
begin
  Result := FFrame.Design;
end;

function TNyxStudioCheckpoint.GetSource: TNyxText;
begin
  Result := FFrame.Source;
end;

function TNyxStudioCheckpoint.GetStorageBytes: TNyxTextBytes;
var
  LDraftBytes: TNyxTextBytes;
begin
  LDraftBytes := TNyxTextBytes(Length(FDraft)) + Length(FDraftBase);
  {$ifdef PAS2JS}
  LDraftBytes := LDraftBytes * 2;
  {$endif}
  Result := FFrame.StorageBytes + LDraftBytes;
end;

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
  Result := LastState.Frame;
end;

function TNyxStudioHistory.GetLastState: TNyxStudioCheckpoint;
begin

  if Count = 0 then
  begin
    raise ERangeError.Create('History has no checkpoint');
  end;
  Result := FEntries[Count - 1];
end;

function TNyxStudioHistory.Entry(AIndex: Integer): TNyxSourceCheckpoint;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ERangeError.Create('Invalid history checkpoint index');
  end;
  Result := State(AIndex).Frame;
end;

procedure TNyxStudioHistory.Add(const ACheckpoint: TNyxSourceCheckpoint);
begin
  Add(NyxStudioCheckpoint(ACheckpoint));
end;

function TNyxStudioHistory.State(AIndex: Integer): TNyxStudioCheckpoint;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ERangeError.Create('Invalid history checkpoint index');
  end;
  Result := FEntries[AIndex];
end;

procedure TNyxStudioHistory.Add(const ACheckpoint: TNyxStudioCheckpoint);
var
  LIndex: Integer;
  LSize: TNyxTextBytes;
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
  FEntries[Count - 1] := Default(TNyxStudioCheckpoint);
  SetLength(FEntries, Count - 1);
end;

procedure TNyxStudioHistory.Clear;
begin
  SetLength(FEntries, 0);
  FStorageBytes := 0;
end;

end.
