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
unit nyx.files.save.unavailable;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.files.save;

{ Portable explicit refusal. Valid requests receive one Unavailable result on
  their supplied UI scheduler's current entry; cancelled requests stay silent.
  No picker, download, file system access or retained receiver is introduced. }
function NewNyxUnavailableFileSaveHost: INyxFileSaveHost;

implementation

uses nyx.bytes, nyx.scheduler;

type
  { The common request owns admission/cancellation and detaches the borrowed
    receiver before inline completion. No pending platform work is retained. }
  TUnavailableRequest = class(TNyxFileSaveRequest)
  public
    procedure Start;
  end;

  { Stateless portable host: the request copies values, checks original scope
    cancellation and reports absence of authority without inventing success. }
  TUnavailableHost = class(TInterfacedObject, INyxFileSaveHost)
  public
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

procedure TUnavailableRequest.Start;
begin
  Complete(nfsoUnavailable, FOptions.SuggestedName, 0,
    'This application has not supplied file-saving access');
end;

function TUnavailableHost.BeginSave(const AOptions: TNyxFileSaveOptions;
  const ABytes: TNyxBytes; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope;
  AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LRequest: TUnavailableRequest;
begin
  LRequest := TUnavailableRequest.Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  Result := LRequest;
  LRequest.Start;
end;

function NewNyxUnavailableFileSaveHost: INyxFileSaveHost;
begin
  Result := TUnavailableHost.Create;
end;

end.
