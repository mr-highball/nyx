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

unit nyx.files.save.browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

interface

uses nyx.files.save;

{ Byte-only browser host. PreferPicker falls back only when the API is absent,
  never after user cancellation. Picker completion follows write + close.
  Downloads report a request, independently of any eventual disk outcome. }
function NewNyxBrowserFileSaveHost: INyxFileSaveHost;

implementation

uses SysUtils, JS, Web, WebOrWorker, nyx.text, nyx.bytes, nyx.bytes.sources,
  nyx.files, nyx.scheduler;

type
  { File System Access bridge to showSaveFilePicker on the browser's borrowed
    Window; declaring this type neither creates a window nor grants permission.
    Run checks API presence and current user activation before invoking it.
    Secure-context and file-access restrictions remain enforced by the browser. }
  TSaveWindow = class external name 'Window'(TJSWindow)
    function showSaveFilePicker(AOptions: TJSObject): TJSPromise;
  end;

  { File System Access bridge to the browser-owned FileSystemWritableFileStream
    returned by createWritable after picker selection; opening may still fail.
    Run retains it across promise turns: write stages bytes, close commits them,
    and abort discards uncommitted work when cancellation or failure permits.
    Releasing the Pascal reference does not close or abort the host stream. }
  TSaveWritable = class external name 'FileSystemWritableFileStream'(TJSObject)
    function write(AData: JSValue): TJSPromise;
    function close: TJSPromise;
    function abort: TJSPromise;
  end;

  { File System Access bridge to the browser-owned FileSystemFileHandle returned
    by the user's save selection. Its name is a leaf caption, not a path or a way
    to reconstruct permission; createWritable can reject even after selection.
    Run keeps the handle local while opening its stream. Neither the handle nor
    its authority enters portable options, results or graph persistence. }
  TSaveHandle = class external name 'FileSystemFileHandle'(TJSObject)
    name: String;
    function createWritable: TJSPromise;
  end;

  { UI-scheduled owner of one temporary Blob URL's revocation, retaining only
    that opaque URL and no request, application or caller receiver. Execute
    revokes and clears it; destruction provides the same cleanup when delayed
    work is skipped. Neither path reports eventual download completion. }
  TURLRetirement = class(TInterfacedObject, INyxWork)
  public
    URL: String;
    destructor Destroy; override;
    procedure Execute(const AExecution: INyxExecution);
  end;

  { One UI-owned browser save with copied options/bytes in the shared base.
    The private promise lease keeps this request alive until platform cleanup
    settles, even after Cancel revokes the borrowed reply and clears its payload.
    Late picker results check Pending before starting further file operations. }
  TSaveRequest = class(TNyxFileSaveRequest)
  private
    FPlatformLease: INyxFileSaveRequest;
    function Run: JSValue; async;
    function Download: JSValue; async;
  public
    procedure Start;
  end;

  { Reference-counted browser adapter; it stores no document or file handles.
    Each BeginSave creates an independent request, whose lifetime covers picker
    promises or download dispatch. Only a completed write/close reports Written;
    an anchor download reports a request, not eventual disk completion. }
  TSaveHost = class(TInterfacedObject, INyxFileSaveHost, INyxFileSaveStreamHost)
    function BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
      const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
      AReply: TNyxFileSaveReply): INyxFileSaveRequest;
    function BeginStream(const AOptions: TNyxFileSaveStreamOptions;
      const ASource: INyxByteSource; const AScheduler: INyxScheduler;
      const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
  end;

{ Promise rejection values are external platform data, not Pascal exceptions.
  Normalize both branches before await, including DOM AbortError cancellation. }
function Accepted(AValue: JSValue): JSValue;
var
  LPacket: TJSObject;
begin
  LPacket := TJSObject.new;
  LPacket['ok'] := True;
  LPacket['value'] := AValue;
  Result := LPacket;
end;

function Rejected(AValue: JSValue): JSValue;
var
  LPacket: TJSObject;
begin
  LPacket := TJSObject.new;
  LPacket['ok'] := False;
  LPacket['value'] := AValue;
  Result := LPacket;
end;

function Settled(const APromise: TJSPromise): TJSPromise;
begin
  Result := APromise._then(@Accepted, @Rejected);
end;

function ActiveGesture: Boolean;
var
  LActivation: JSValue;
begin
  LActivation := TJSObject(window.navigator)['userActivation'];
  Result := isObject(LActivation) and Boolean(TJSObject(LActivation)['isActive']);
end;

destructor TURLRetirement.Destroy;
begin

  if URL <> '' then
  begin
    TJSURL.revokeObjectURL(URL);
  end;
  inherited Destroy;
end;

procedure TURLRetirement.Execute(const AExecution: INyxExecution);
begin

  if URL <> '' then
  begin
    TJSURL.revokeObjectURL(URL);
    URL := '';
  end;
end;

{ A real host turn permits input/cancellation between encoded download chunks.
  This never fabricates or extends the browser's transient user activation. }
procedure DownloadTurn(AResolve, AReject: TJSPromiseResolver);
begin
  window.setTimeout(
    procedure
    begin
      AResolve(Undefined);
    end, 0);
end;

function TSaveRequest.Download: JSValue; async;
var
  LArray: TJSUint8Array;
  LBlobOptions: TJSBlobInit;
  LBlob: TJSBlob;
  LAnchor: TJSHTMLAnchorElement;
  LRetirement: TURLRetirement;
  LWork: INyxWork;
  LIndex: Integer;
  LCount: Integer;
  LChunk: TNyxBytes;
  LParts: array of JSValue;
begin
  Result := Undefined;

  if (FOptions.ActivationPolicy = nfdaRequired) and not ActiveGesture then
  begin
    Complete(nfsoActivationRequired, FOptions.SuggestedName, 0,
      'Choose Save again from a current user action');
    Exit;
  end;
  LCount := FExpectedBytes;

  if FStreamed then
  begin
    { Blob downloads inherently retain the entire output in browser storage.
      This explicit budget is independent of the genuinely incremental picker. }

    if LCount > FStreamOptions.DownloadLimit then
    begin
      Complete(nfsoUnavailable, FOptions.SuggestedName, 0,
        'The download exceeds its buffer budget; choose a picker or raise that explicit limit');
      Exit;
    end;
    LParts := nil;
    repeat
      LChunk := ReadPayloadChunk;

      if not Pending then
      begin
        Cancel;
        Exit;
      end;

      if Length(LChunk) = 0 then
      begin
        Break;
      end;
      LArray := TJSUint8Array.new(Length(LChunk));
      for LIndex := 0 to High(LChunk) do
      begin
        LArray[LIndex] := LChunk[LIndex];
      end;
      SetLength(LParts, Length(LParts) + 1);
      LParts[High(LParts)] := LArray.buffer;
      LChunk := nil;
      await(TJSPromise.resolve(TJSPromise.new(@DownloadTurn)));
    until False;
  end
  else
  begin
    LArray := TJSUint8Array.new(LCount);
    for LIndex := 0 to High(FBytes) do
    begin
      LArray[LIndex] := FBytes[LIndex];
    end;
    SetLength(LParts, 1);
    LParts[0] := LArray.buffer;
  end;

  if not Pending then
  begin
    Cancel;
    Exit;
  end;

  if (FOptions.ActivationPolicy = nfdaRequired) and not ActiveGesture then
  begin
    Complete(nfsoActivationRequired, FOptions.SuggestedName, 0,
      'The browser requires a current user action to request this download');
    Exit;
  end;
  LBlobOptions := TJSBlobInit.new;
  LBlobOptions.type_ := FOptions.ContentType.Name;
  LBlob := TJSBlob.new(LParts, LBlobOptions);
  LRetirement := TURLRetirement.Create;
  LWork := LRetirement;
  LRetirement.URL := TJSURL.createObjectURL(LBlob);
  LAnchor := TJSHTMLAnchorElement(document.createElement('a'));
  try
    LAnchor.href := LRetirement.URL;
    LAnchor.download := FOptions.SuggestedName;
    { Admit cleanup before the external effect. Failure cannot follow an already
      requested download merely because the host cannot queue URL retirement. }
    NyxSchedulerDelays(FScheduler).PostAfter(LWork, 1000);
    document.body.appendChild(LAnchor);
    LAnchor.click;
  finally
    LAnchor.remove;
  end;
  Complete(nfsoDownloadRequested, FOptions.SuggestedName, LCount);
end;

procedure TSaveRequest.Start;
begin
  FScheduler.RequireUI;
  RetainPending;
  FPlatformLease := Self;
  Run;
end;

function TSaveRequest.Run: JSValue; async;
var
  LLease: INyxFileSaveRequest;
  LPicker: Boolean;
  LOptions: TJSObject;
  LAccept: TJSObject;
  LType: TJSObject;
  LExtensions: TJSArray;
  LPacket: TJSObject;
  LError: JSValue;
  LHandle: TSaveHandle;
  LWritable: TSaveWritable;
  LArray: TJSUint8Array;
  LIndex: Integer;
  LCount: Integer;
  LName: TNyxText;
  LLeaf: INyxTextFile;
  LCommitted: Boolean;
  LChunk: TNyxBytes;
begin
  Result := Undefined;
  LLease := Self;
  LWritable := nil;
  LCommitted := False;
  try
    try

      if not Pending then
      begin
        Cancel;
        Exit;
      end;
      LPicker := isFunction(TJSObject(window)['showSaveFilePicker']);

      if (FOptions.SaveMode = nfsmDownload) or
        ((FOptions.SaveMode = nfsmPreferPicker) and not LPicker) then
      begin
        await(Download);
        Exit;
      end;

      if not LPicker then
      begin
        Complete(nfsoUnavailable, FOptions.SuggestedName, 0,
          'This browser context does not provide a save picker');
        Exit;
      end;

      if not ActiveGesture then
      begin
        Complete(nfsoActivationRequired, FOptions.SuggestedName, 0,
          'Choose Save again from a current user action');
        Exit;
      end;
      LOptions := TJSObject.new;
      LOptions['suggestedName'] := FOptions.SuggestedName;
      LOptions['excludeAcceptAllOption'] := not FOptions.IncludeAllTypes;

      if FOptions.ExtensionCount > 0 then
      begin
        LExtensions := TJSArray.new;
        for LIndex := 0 to FOptions.ExtensionCount - 1 do
        begin
          LExtensions.push('.' + FOptions.Extension(LIndex));
        end;
        LAccept := TJSObject.new;
        LAccept[FOptions.ContentType.Name] := LExtensions;
        LType := TJSObject.new;
        LType['description'] := FOptions.DialogTitle;
        LType['accept'] := LAccept;
        LOptions['types'] := TJSArray.new(LType);
      end;
      LPacket := TJSObject(await(Settled(TSaveWindow(window).showSaveFilePicker(LOptions))));

      if not Pending then
      begin
        Cancel;
        Exit;
      end;

      if not Boolean(LPacket['ok']) then
      begin
        LError := LPacket['value'];

        if isObject(LError) and (String(TJSObject(LError)['name']) = 'AbortError') then
        begin
          Complete(nfsoCancelled, FOptions.SuggestedName, 0);
        end
        else
        begin
          Complete(nfsoFailed, FOptions.SuggestedName, 0, 'The browser save picker refused the request');
        end;
        Exit;
      end;
      LHandle := TSaveHandle(LPacket['value']);
      LName := LHandle.name;
      LLeaf := NyxTextFile(LName, '');
      LPacket := TJSObject(await(Settled(LHandle.createWritable)));

      if not Boolean(LPacket['ok']) then
      begin
        Complete(nfsoFailed, FOptions.SuggestedName, 0, 'The selected file could not be opened for writing');
        Exit;
      end;
      LWritable := TSaveWritable(LPacket['value']);

      if not Pending then
      begin
        Cancel;
        Exit;
      end;
      LCount := FExpectedBytes;

      if FStreamed then
      begin
        repeat
          LChunk := ReadPayloadChunk;

          if not Pending then
          begin
            Cancel;
            Exit;
          end;

          if Length(LChunk) = 0 then
          begin
            Break;
          end;
          LArray := TJSUint8Array.new(Length(LChunk));
          for LIndex := 0 to High(LChunk) do
          begin
            LArray[LIndex] := LChunk[LIndex];
          end;
          LChunk := nil;
          LPacket := TJSObject(await(Settled(LWritable.write(LArray))));

          if not Pending then
          begin
            Cancel;
            Exit;
          end;

          if not Boolean(LPacket['ok']) then
          begin
            Complete(nfsoFailed, FOptions.SuggestedName, 0, 'The browser could not write a complete chunk');
            Exit;
          end;
        until False;
      end
      else
      begin
        LArray := TJSUint8Array.new(LCount);
        for LIndex := 0 to High(FBytes) do
        begin
          LArray[LIndex] := FBytes[LIndex];
        end;
        LPacket := TJSObject(await(Settled(LWritable.write(LArray))));
      end;

      if not Pending then
      begin
        Cancel;
        Exit;
      end;

      if not FStreamed and not Boolean(LPacket['ok']) then
      begin
        Complete(nfsoFailed, FOptions.SuggestedName, 0, 'The browser could not write the complete file');
        Exit;
      end;
      LPacket := TJSObject(await(Settled(LWritable.close)));
      LCommitted := Boolean(LPacket['ok']);

      if not Pending then
      begin
        Cancel;
        Exit;
      end;

      if not LCommitted then
      begin
        Complete(nfsoFailed, FOptions.SuggestedName, 0, 'The browser could not commit the selected file');
        Exit;
      end;
      Complete(nfsoWritten, LName, LCount);
    except
      Complete(nfsoFailed, FOptions.SuggestedName, 0, 'The browser file-save operation could not complete');
    end;
  finally
    { Cancellation never closes/commits an uncommitted stream. Its private
      promise lease outlives the revoked reply, so late results touch no caller. }

    if (LWritable <> nil) and not LCommitted then
    begin
      try
        await(Settled(LWritable.abort));
      except
        { Platform cleanup cannot revive a cancelled/completed receiver. }
      end;
    end;
    FPlatformLease := nil;
  end;
end;

function TSaveHost.BeginSave(const AOptions: TNyxFileSaveOptions; const ABytes: TNyxBytes;
  const AScheduler: INyxScheduler; const ACancellation: INyxCancellationScope;
  AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LRequest: TSaveRequest;
begin
  LRequest := TSaveRequest.Create(AOptions, ABytes, AScheduler, ACancellation, AReply);
  Result := LRequest;
  LRequest.Start;
end;

function NewNyxBrowserFileSaveHost: INyxFileSaveHost;
begin
  Result := TSaveHost.Create;
end;

function TSaveHost.BeginStream(const AOptions: TNyxFileSaveStreamOptions;
  const ASource: INyxByteSource; const AScheduler: INyxScheduler;
  const ACancellation: INyxCancellationScope; AReply: TNyxFileSaveReply): INyxFileSaveRequest;
var
  LRequest: TSaveRequest;
begin
  LRequest := TSaveRequest.CreateStream(AOptions, ASource, AScheduler, ACancellation, AReply);
  Result := LRequest;
  LRequest.Start;
end;

end.
