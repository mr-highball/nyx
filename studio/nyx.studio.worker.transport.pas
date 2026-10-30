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
unit nyx.studio.worker.transport;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, JS, nyx.text;

type
  { Transport shape/identity errors are separate from positioned Pascal errors.
    Neither can admit a design; the controller retains its accepted pair. }
  ENyxSourceWorkerTransport = class(Exception);

  { Positive controller-local request ordinal, never a document revision.
    Zero belongs only to the explicitly recognized legacy text transport. }
  TNyxSourceWorkerSequence = type Integer;

  { Structured-clone framing copies only an ordinal and text. It retains no
    MessageEvent/Worker, model or caller. Logical JSON remains byte-for-byte
    unchanged, with its existing decoder/depth/byte limits: wrapping it in a
    JSON object would add depth and change admission near those limits.
    The worker also accepts legacy text requests for older observing hosts. }
  TNyxSourceWorkerPacket = record
  private
    FHasSequence: Boolean;
    FSequence: TNyxSourceWorkerSequence;
    FText: TNyxText;
  public
    class function Tagged(ASequence: TNyxSourceWorkerSequence;
      const AText: TNyxText): TNyxSourceWorkerPacket; static;
    class function Read(const AValue: JSValue): TNyxSourceWorkerPacket; static;
    function WithText(const AText: TNyxText): TNyxSourceWorkerPacket;
    function ToValue: JSValue;
    property HasSequence: Boolean read FHasSequence;
    property Sequence: TNyxSourceWorkerSequence read FSequence;
    property Text: TNyxText read FText;
  end;

implementation

uses Math;

class function TNyxSourceWorkerPacket.Tagged(ASequence: TNyxSourceWorkerSequence;
  const AText: TNyxText): TNyxSourceWorkerPacket;
var
  LPacket: TNyxSourceWorkerPacket;
begin

  if ASequence <= 0 then
  begin
    raise ENyxSourceWorkerTransport.Create('Worker request sequence must be positive');
  end;
  LPacket := Default(TNyxSourceWorkerPacket);
  LPacket.FHasSequence := True;
  LPacket.FSequence := ASequence;
  LPacket.FText := AText;
  Result := LPacket;
end;

class function TNyxSourceWorkerPacket.Read(const AValue: JSValue): TNyxSourceWorkerPacket;
var
  LPacket: TNyxSourceWorkerPacket;
  LArray: TJSArray;
  LSequence: Double;
begin
  LPacket := Default(TNyxSourceWorkerPacket);

  if isString(AValue) then
  begin
    LPacket.FText := TNyxText(AValue);
    Exit(LPacket);
  end;

  if not TJSArray.isArray(AValue) then
  begin
    raise ENyxSourceWorkerTransport.Create('Worker transport requires tagged text');
  end;
  LArray := TJSArray(AValue);

  if (LArray.Length <> 2) or not isNumber(LArray[0]) or not isString(LArray[1]) then
  begin
    raise ENyxSourceWorkerTransport.Create('Worker transport requires an ordinal and text');
  end;
  LSequence := Double(LArray[0]);

  if IsNan(LSequence) or IsInfinite(LSequence) or (LSequence < 1) or
    (LSequence > High(Integer)) or (Trunc(LSequence) <> LSequence) then
  begin
    raise ENyxSourceWorkerTransport.Create('Worker request sequence is outside its integer domain');
  end;
  Result := Tagged(TNyxSourceWorkerSequence(Trunc(LSequence)), TNyxText(LArray[1]));
end;

function TNyxSourceWorkerPacket.WithText(const AText: TNyxText): TNyxSourceWorkerPacket;
var
  LPacket: TNyxSourceWorkerPacket;
begin
  LPacket := Self;
  LPacket.FText := AText;
  Result := LPacket;
end;

function TNyxSourceWorkerPacket.ToValue: JSValue;
var
  LArray: TJSArray;
begin

  if not FHasSequence then
  begin
    Exit(FText);
  end;
  LArray := TJSArray.new;
  LArray.push(Integer(FSequence));
  LArray.push(FText);
  Result := LArray;
end;

end.
