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
unit nyx.clipboard.unavailable;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.clipboard;

{ Explicit runtime absence keeps portable commands inspectable without granting
  platform access. Valid writes complete inline with Unavailable and zero bytes;
  cancelled receivers remain silent. Invalid payloads still refuse admission.
  The copied diagnostic is bounded like every other clipboard result. }
function NewNyxUnavailableClipboardWriteHost(
  const AReason: TNyxText = 'This application has not supplied a clipboard service'):
  INyxClipboardWriteHost;

implementation

uses nyx.bytes, nyx.scheduler;

type
  { Synchronous terminal request reuses ordinary text, reply and cancellation
    admission. It owns no platform object, application, widget or delayed work. }
  TUnavailableRequest = class(TNyxClipboardWriteRequest)
  public
    procedure Start(const AReason: TNyxText);
  end;

  { Immutable copied absence reason. An entered BeginWriteText retains this
    host while scheduler/cancellation getters or the reply release its caller. }
  TUnavailableHost = class(TInterfacedObject, INyxClipboardWriteHost)
  private
    FReason: TNyxText;
  public
    constructor Create(const AReason: TNyxText);
    function BeginWriteText(const AText: TNyxText;
      const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope;
      AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
  end;

procedure TUnavailableRequest.Start(const AReason: TNyxText);
begin
  Complete(ncwoUnavailable, 0, AReason);
end;

constructor TUnavailableHost.Create(const AReason: TNyxText);
var
  LBytes: Integer;
begin
  inherited Create;

  if Length(AReason) > NyxClipboardMaximumErrorBytes then
  begin
    raise ENyxClipboard.Create('Clipboard absence reason exceeds its UTF-8 byte budget');
  end;
  try
    LBytes := NyxUTF8ByteCount(AReason);
  except
    on ENyxBytes do
    begin
      raise ENyxClipboard.Create('Clipboard absence reason contains malformed Unicode');
    end;
  end;

  if LBytes > NyxClipboardMaximumErrorBytes then
  begin
    raise ENyxClipboard.Create('Clipboard absence reason exceeds its UTF-8 byte budget');
  end;
  FReason := AReason;
end;

function TUnavailableHost.BeginWriteText(const AText: TNyxText;
  const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope;
  AReply: TNyxClipboardWriteReply): INyxClipboardWriteRequest;
var
  LLease: INyxClipboardWriteHost;
  LRequest: TUnavailableRequest;
begin
  LLease := Self;
  LRequest := TUnavailableRequest.Create(AText, AScheduler, ACancellation, AReply);
  Result := LRequest;
  LRequest.Start(FReason);
end;

function NewNyxUnavailableClipboardWriteHost(const AReason: TNyxText):
  INyxClipboardWriteHost;
begin
  Result := TUnavailableHost.Create(AReason);
end;

end.
