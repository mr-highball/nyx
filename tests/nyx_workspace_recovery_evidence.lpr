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
program nyx_workspace_recovery_evidence;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Classes, SysUtils, nyx.text, nyx.data;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMeaning: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMeaning);
  end;
  Inc(GChecks);
end;

{ Reads the actual authenticated bounded workspace-list receipts. Exact durable
  pair/profile bytes and actual constructor recovery are separate host gates;
  these metadata files alone never establish recovered source or execution. }
function ReadReceipt(const APath: TNyxText): TNyxDataValue;
var
  LFile: TFileStream;
  LBytes: RawByteString;
begin
  LFile := TFileStream.Create(APath, fmOpenRead or fmShareDenyNone);
  try
    Check((LFile.Size > 0) and (LFile.Size <= 1024 * 1024),
      'Workspace receipt exceeds its private byte budget');
    SetLength(LBytes, LFile.Size);
    LFile.ReadBuffer(LBytes[1], Length(LBytes));
    { The stream contains UTF-8 bytes. A direct UTF8String cast labels those
      bytes without an ANSI conversion; the shared JSON reader validates text. }
    Result := TNyxDataValue.ParseJSON(UTF8String(LBytes));
  finally
    LFile.Free;
  end;
end;

procedure Run;
var
  LBefore: TNyxDataValue;
  LAfter: TNyxDataValue;
  LExpected: TNyxDataValue;
  LActual: TNyxDataValue;
  LFound: Integer;
  LIndex: Integer;
  LCandidate: Integer;
  LField: Integer;
  LName: TNyxText;
begin
  Check(ParamCount = 2, 'Supply actual before/after cold-recovery workspace receipts');
  LBefore := ReadReceipt(ParamStr(1));
  LAfter := ReadReceipt(ParamStr(2));
  Check((LBefore.Field('total').AsInteger >= 1) and
    (LBefore.Field('total').AsInteger <= 9) and
    (LBefore.Field('items').Count = LBefore.Field('total').AsInteger) and
    (LAfter.Field('items').Count = LBefore.Field('items').Count) and
    (LBefore.Field('total').ToJSON = LAfter.Field('total').ToJSON),
    'Cold recovery retains the complete bounded project registry');
  for LIndex := 0 to LBefore.Field('items').Count - 1 do
  begin
    LExpected := LBefore.Field('items').Item(LIndex);
    LFound := 0;
    LActual := Default(TNyxDataValue);
    for LCandidate := 0 to LAfter.Field('items').Count - 1 do
    begin

      if LAfter.Field('items').Item(LCandidate).Field('workspace').AsText =
        LExpected.Field('workspace').AsText then
      begin
        Inc(LFound);
        LActual := LAfter.Field('items').Item(LCandidate);
      end;
    end;
    Check((LFound = 1) and (LActual.Field('label').ToJSON = LExpected.Field('label').ToJSON) and
      (LActual.Field('lifetime').AsText = 'project'),
      'Cold recovery retains one exact handle, label and ordinary project lifetime');
    Check(LActual.Field('session').Count = LExpected.Field('session').Count,
      'Recovered session metadata retains its complete field set');
    for LField := 0 to LExpected.Field('session').Count - 1 do
    begin
      LName := LExpected.Field('session').Key(LField);

      if LName = 'activitySequence' then
      begin
        { Activities/presence belong to the live service, not authored history.
          Verify their deliberate reset instead of pretending they are durable. }
        Check(LActual.Field('session').Field(LName).AsInteger = 0,
          'A fresh service starts its transient activity journal at zero');
      end
      else
      begin
        Check(LActual.Field('session').Field(LName).ToJSON =
          LExpected.Field('session').Field(LName).ToJSON,
          'Cold recovery retains the exact durable session field ' + LName);
      end;
    end;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' exact recovered workspace metadata checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
