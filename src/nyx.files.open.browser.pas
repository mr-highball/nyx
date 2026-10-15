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

unit nyx.files.open.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.files.open;

{ User-activated single-file selection and asynchronous raw byte reading through
  the browser's real input/FileReader APIs. Temporary DOM objects remain private;
  no file name is used as a path and no browser capability leaves the adapter. }
function NewNyxBrowserFileOpenHost: INyxFileOpenHost;

implementation

uses SysUtils, JS, Web, nyx.text, nyx.bytes, nyx.files.save, nyx.scheduler;

type
  { UI-owned request with independent input/reader state. The pending base lease
    covers host callbacks. Cleanup disconnects listeners before removing the
    input/aborting reads, so late host events never reach a retired receiver. }
  TOpenRequest = class(TNyxFileOpenRequest)
  private
    FInput: TJSHTMLInputElement;
    FReader: TJSFileReader;
    FMetadata: TNyxFileMetadata;
    function Chosen(AEvent: TJSEvent): Boolean;
    function Read(AEvent: TJSEvent): Boolean;
    function Cancelled(AEvent: TJSEvent): Boolean;
  protected
    procedure EndHost; override;
  public
    procedure Start;
  end;

  { Stateless reference-counted runtime authority. Every BeginOpen owns a fresh
    request; parallel or reentrant callers never share an input, reader or reply. }
  TOpenHost = class(TInterfacedObject, INyxFileOpenHost)
    function BeginOpen(const AOptions: TNyxFileOpenOptions;
      const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
      AReply: TNyxFileOpenReply): INyxFileOpenRequest;
  end;

procedure TOpenRequest.EndHost;
begin

  if FReader <> nil then
  begin
    FReader.onload := nil;
    FReader.onerror := nil;
    FReader.onabort := nil;
    FReader.abort;
    FReader := nil;
  end;

  if FInput <> nil then
  begin
    FInput.onchange := nil;
    FInput.removeEventListener('cancel', @Cancelled);
    FInput.remove;
    FInput := nil;
  end;
end;

procedure TOpenRequest.Start;
var
  LLease: INyxFileOpenRequest;
  LActivation: JSValue;
  LAccept: String;
  LIndex: Integer;
begin
  FScheduler.RequireUI;
  LLease := Self;
  RetainPending;
  try

    if not Pending then
    begin
      Cancel;
      Exit;
    end;
    LActivation := TJSObject(window.navigator)['userActivation'];

    if not isObject(LActivation) then
    begin
      Complete(nfooUnavailable, nil, 'This browser does not expose current user activation');
      Exit;
    end;

    if not Boolean(TJSObject(LActivation)['isActive']) then
    begin
      Complete(nfooActivationRequired, nil, 'Choose Open from a current user action');
      Exit;
    end;
    { Admit retirement before presenting an external file chooser. }
    Watch;

    if not Pending then
    begin
      Exit;
    end;
    FInput := TJSHTMLInputElement(document.createElement('input'));
    FInput.setAttribute('type', 'file');
    FInput.multiple := False;
    FInput.style.setProperty('display', 'none');
    LAccept := '';
    for LIndex := 0 to FOptions.ExtensionCount - 1 do
    begin

      if LIndex > 0 then
      begin
        LAccept := LAccept + ',';
      end;
      LAccept := LAccept + '.' + FOptions.Extension(LIndex);
    end;
    FInput.accept := LAccept;
    FInput.onchange := @Chosen;
    FInput.addEventListener('cancel', @Cancelled);
    document.body.appendChild(FInput);
    FInput.click;
  except
    Complete(nfooFailed, nil, 'The browser file selector could not open');
  end;
end;

function TOpenRequest.Cancelled(AEvent: TJSEvent): Boolean;
begin
  Result := True;
  Complete(nfooCancelled);
end;

function TOpenRequest.Chosen(AEvent: TJSEvent): Boolean;
var
  LLease: INyxFileOpenRequest;
  LFile: TJSFile;
begin
  Result := True;
  LLease := Self;

  if not Pending then
  begin
    Cancel;
    Exit;
  end;

  if (FInput = nil) or (FInput.files.length <> 1) then
  begin
    Complete(nfooCancelled);
    Exit;
  end;
  try
    LFile := FInput.files[0];

    if (LFile.size < 0) or (LFile.size > FOptions.ByteLimit) then
    begin
      Complete(nfooFailed, nil, 'Selected file exceeds the byte budget');
      Exit;
    end;
    FMetadata := NyxFileMetadata(LFile.name, Trunc(LFile.size));

    if LFile.type_ <> '' then
    begin
      FMetadata := FMetadata.MediaType(NyxMediaType(LFile.type_));
    end;
    { File.lastModified is a host observation, not proof that a filesystem
      timestamp exists. Browser snapshots retain exactly the value it reports. }
    FMetadata := FMetadata.ModifiedMilliseconds(Double(TJSObject(LFile)['lastModified']));
    FReader := TJSFileReader.new;
    FReader.onload := @Read;
    FReader.onerror := @Read;
    FReader.onabort := @Read;
    FReader.readAsArrayBuffer(LFile);
  except
    Complete(nfooFailed, nil, 'The browser could not start the selected file read');
  end;
end;

function TOpenRequest.Read(AEvent: TJSEvent): Boolean;
var
  LLease: INyxFileOpenRequest;
  LArray: TJSUint8Array;
  LBytes: TNyxBytes;
  LIndex: Integer;
  LFile: INyxFileSnapshot;
begin
  Result := True;
  LLease := Self;

  if not Pending then
  begin
    Cancel;
    Exit;
  end;
  try

    if (FReader = nil) or (FReader.error <> nil) or not isObject(FReader.result) then
    begin
      Complete(nfooFailed, nil, 'The selected file could not be read completely');
      Exit;
    end;
    LArray := TJSUint8Array.new(TJSArrayBuffer(FReader.result));

    if (LArray.length <> FMetadata.ByteCount) or (LArray.length > FOptions.ByteLimit) then
    begin
      Complete(nfooFailed, nil, 'Selected file changed or exceeded its byte budget');
      Exit;
    end;
    SetLength(LBytes, LArray.length);
    for LIndex := 0 to High(LBytes) do
    begin
      LBytes[LIndex] := LArray[LIndex];
    end;
    LFile := NyxFileSnapshot(FMetadata, LBytes);
  except
    Complete(nfooFailed, nil, 'The browser could not admit the selected file bytes');
    Exit;
  end;
  Complete(nfooRead, LFile);
end;

function TOpenHost.BeginOpen(const AOptions: TNyxFileOpenOptions;
  const AScheduler: INyxScheduler; const AScope: INyxCancellationScope;
  AReply: TNyxFileOpenReply): INyxFileOpenRequest;
var
  LRequest: TOpenRequest;
begin
  LRequest := TOpenRequest.Create(AOptions, AScheduler, AScope, AReply);
  Result := LRequest;
  LRequest.Start;
end;

function NewNyxBrowserFileOpenHost: INyxFileOpenHost;
begin
  Result := TOpenHost.Create;
end;

end.
