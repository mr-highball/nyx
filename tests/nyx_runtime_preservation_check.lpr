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
program nyx_runtime_preservation_check;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, md5, nyx.text, nyx.editing;

type
  { Read-only qualification receipt. No model, compiler, listener, enrollment or
    replacement authority is created. Complete session bytes include revision,
    permission, accepted pair, unfinished draft/base, navigation, next identity
    and every ordered Undo/Redo pair. Version-one history is normalized solely
    by supplying its implicit False draft flag. No authored field is discarded. }
  TCheckpointReceipt = record
    Identity: TNyxText;
    Serial: Integer;
    Entries: array of record
      ID: TNyxText;
      LabelText: TNyxText;
      Session: RawByteString;
    end;
  end;

procedure Require(ACondition: Boolean; const AReason: String);
begin

  if not ACondition then
  begin
    raise Exception.Create('Runtime preservation: ' + AReason);
  end;
end;

{ The private qualification input is limited to 4 MiB, much less than the
  product checkpoint maximum. This parser recognizes the existing versions and
  verifies the complete production digest before interpreting any field. It
  never executes source or rewrites the original input. }
function ReadReceipt(const APath: String): TCheckpointReceipt;
var
  LStream: TFileStream;
  LBytes: RawByteString;
  LPosition: Integer;
  LPayloadLength: Integer;
  LVersion: Integer;
  LCount: Integer;
  LIndex: Integer;

  function Number: Integer;
  var
    LValue: LongWord;
    LByte: Integer;
  begin
    Require(LPosition + 3 <= LPayloadLength, 'truncated integer');
    LValue := 0;
    for LByte := 0 to 3 do
    begin
      LValue := LValue or (LongWord(Byte(LBytes[LPosition + LByte])) shl (LByte * 8));
    end;
    Inc(LPosition, 4);
    Require(LValue <= LongWord(High(Integer)), 'integer exceeds native budget');
    Result := Integer(LValue);
  end;

  function Text: TNyxText;
  var
    LLength: Integer;
  begin
    LLength := Number;
    Require((LLength <= 4 * 1024 * 1024) and
      (LLength <= LPayloadLength - LPosition + 1), 'truncated or oversized text');
    Result := TNyxText(Copy(LBytes, LPosition, LLength));
    NyxTextScalarCount(Result);
    Inc(LPosition, LLength);
  end;

  function Session: RawByteString;
  var
    LStart: Integer;
    LUndo: Integer;
    LRedo: Integer;
    LHistory: Integer;
    LField: Integer;
    LPending: Integer;
  begin
    LStart := LPosition;
    Number;
    Require(Number <= 2, 'unknown permission');
    Require(Number <= 1, 'invalid claimed flag');
    for LField := 0 to 3 do
    begin
      Text;
    end;
    Require(Number <= 1, 'invalid pending flag');
    Text;
    Text;
    Number;
    LUndo := Number;
    LRedo := Number;
    Require((LUndo <= 50) and (LRedo <= 50) and (LUndo + LRedo <= 50),
      'history exceeds production budget');
    Result := Copy(LBytes, LStart, LPosition - LStart);
    for LHistory := 0 to LUndo + LRedo - 1 do
    begin
      LStart := LPosition;
      Text;
      Text;
      Result := Result + Copy(LBytes, LStart, LPosition - LStart);
      LPending := 0;

      if LVersion = 2 then
      begin
        LPending := Number;
        Require(LPending <= 1, 'invalid historical draft flag');
      end;
      Result := Result + AnsiChar(LPending) + #0#0#0;

      if LPending = 1 then
      begin
        LStart := LPosition;
        Text;
        Text;
        Result := Result + Copy(LBytes, LStart, LPosition - LStart);
      end;
    end;
  end;

begin
  Result := Default(TCheckpointReceipt);
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Require((LStream.Size > 32) and (LStream.Size <= 4 * 1024 * 1024),
      'private checkpoint exceeds qualification budget');
    SetLength(LBytes, LStream.Size);
    LStream.ReadBuffer(LBytes[1], Length(LBytes));
  finally
    LStream.Free;
  end;
  LPayloadLength := Length(LBytes) - 32;
  Require(MD5Print(MD5Buffer(LBytes[1], LPayloadLength)) =
    Copy(LBytes, LPayloadLength + 1, 32), 'complete checkpoint digest differs');
  LPosition := 1;
  Require(Text = 'NYX-STUDIO-RUNTIME', 'unknown checkpoint magic');
  LVersion := Number;
  Require(LVersion in [1, 2], 'unsupported checkpoint version');
  SetLength(Result.Entries, 1);
  Result.Entries[0].ID := '';
  Result.Entries[0].LabelText := 'Primary project';
  Result.Entries[0].Session := Session;
  Result.Identity := Text;
  Result.Serial := Number;
  LCount := Number;
  Require(LCount = 8, 'expected the complete nine-project registry');
  SetLength(Result.Entries, LCount + 1);
  for LIndex := 1 to LCount do
  begin
    Result.Entries[LIndex].ID := Text;
    Result.Entries[LIndex].LabelText := Text;
    Result.Entries[LIndex].Session := Session;
  end;
  Require(LPosition = LPayloadLength + 1, 'trailing checkpoint fields');
end;

var
  LBefore: TCheckpointReceipt;
  LAfter: TCheckpointReceipt;
  LIndex: Integer;
  LOwnedCount: Integer;
begin
  try
    Require(ParamCount = 3, 'supply before/current checkpoint and exact owned workspace');
    Require(ParamStr(3) <> '', 'primary cannot be excluded from preservation');
    LBefore := ReadReceipt(ParamStr(1));
    LAfter := ReadReceipt(ParamStr(2));
    Require((LBefore.Identity = LAfter.Identity) and
      (LBefore.Serial = LAfter.Serial), 'registry identity/serial changed');
    LOwnedCount := 0;
    for LIndex := 0 to High(LBefore.Entries) do
    begin
      Require((LBefore.Entries[LIndex].ID = LAfter.Entries[LIndex].ID) and
        (LBefore.Entries[LIndex].LabelText = LAfter.Entries[LIndex].LabelText),
        'registry order/identity/label changed');

      if LBefore.Entries[LIndex].ID = TNyxText(ParamStr(3)) then
      begin
        Inc(LOwnedCount);
      end
      else
      begin
        Require(LBefore.Entries[LIndex].Session = LAfter.Entries[LIndex].Session,
          'protected complete session changed');
      end;
    end;
    Require(LOwnedCount = 1, 'owned workspace must match exactly one registry entry');
    WriteLn('PASS eight protected complete session/history receipts and exact registry');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.

