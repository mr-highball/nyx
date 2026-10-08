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


unit nyx.resources.import.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.resources.import;

{ Private user-activated file input/reader. Read raw ArrayBuffer bytes, not browser
  text decoding, so exact UTF-8/NUL/JSON numbers and arbitrary data survive. }
function NewNyxBrowserResourcePicker: INyxResourcePicker;

implementation

uses SysUtils, JS, Web, nyx.text, nyx.bytes, nyx.resources;

type
  TResourcePicker = class(TInterfacedObject, INyxResourcePicker)
  private
    FInput: TJSHTMLInputElement;
    FReader: TJSFileReader;
    FKind: TNyxResourceKind;
    FReply: TNyxResourcePickReply;
    function Chosen(AEvent: TJSEvent): Boolean;
    function Read(AEvent: TJSEvent): Boolean;
    function Cancelled(AEvent: TJSEvent): Boolean;
    procedure Complete(AStatus: TNyxResourcePickStatus;
      const ADefinition: INyxResourceDefinition; const AError: TNyxText);
  public
    destructor Destroy; override;
    procedure Pick(AKind: TNyxResourceKind; AReply: TNyxResourcePickReply);
    procedure Cancel;
  end;

procedure TResourcePicker.Cancel;
begin
  FReply := nil;

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

destructor TResourcePicker.Destroy;
begin
  Cancel;
  inherited Destroy;
end;

procedure TResourcePicker.Pick(AKind: TNyxResourceKind; AReply: TNyxResourcePickReply);
begin
  NyxResourceKindName(AKind);
  Cancel;
  FKind := AKind;
  FReply := AReply;
  FInput := TJSHTMLInputElement(document.createElement('input'));
  FInput.setAttribute('type', 'file');
  FInput.style.setProperty('display', 'none');
  FInput.onchange := @Chosen;
  FInput.addEventListener('cancel', @Cancelled);
  document.body.appendChild(FInput);
  FInput.click;
end;

procedure TResourcePicker.Complete(AStatus: TNyxResourcePickStatus;
  const ADefinition: INyxResourceDefinition; const AError: TNyxText);
var
  LReply: TNyxResourcePickReply;
  LLease: INyxResourcePicker;
begin
  LLease := Self;
  LReply := FReply;
  Cancel;

  if Assigned(LReply) then
  begin
    LReply(AStatus, ADefinition, AError);
  end;
  LLease := nil;
end;

function TResourcePicker.Cancelled(AEvent: TJSEvent): Boolean;
begin
  Result := True;
  Complete(rpsCancelled, nil, '');
end;

function TResourcePicker.Chosen(AEvent: TJSEvent): Boolean;
begin
  Result := True;

  if (FInput = nil) or (FInput.files.length <> 1) then
  begin
    Complete(rpsCancelled, nil, '');
    Exit;
  end;

  if FInput.files[0].size > NyxMaximumPackedBytes then
  begin
    Complete(rpsFailed, nil, 'Choose a resource file up to 1 MiB');
    Exit;
  end;
  try
    FReader := TJSFileReader.new;
    FReader.onload := @Read;
    FReader.onerror := @Read;
    FReader.onabort := @Read;
    FReader.readAsArrayBuffer(FInput.files[0]);
  except
    { Platform setup errors are ordinary JS exceptions, not Pascal exceptions. }
    Complete(rpsFailed, nil, 'The browser could not read the selected file');
  end;
end;

function TResourcePicker.Read(AEvent: TJSEvent): Boolean;
var
  LArray: TJSUint8Array;
  LBytes: TNyxBytes;
  LDefinition: INyxResourceDefinition;
  LError: TNyxText;
  LIndex: Integer;
begin
  Result := True;

  if FReader = nil then
  begin
    Exit;
  end;
  LDefinition := nil;
  LError := '';
  try

    if (FReader.error <> nil) or not isObject(FReader.result) then
    begin
      raise ENyxResource.Create('Cannot read the selected resource file');
    end;
    LArray := TJSUint8Array.new(TJSArrayBuffer(FReader.result));

    if LArray.length > NyxMaximumPackedBytes then
    begin
      raise ENyxResource.Create('Selected resource exceeds the byte budget');
    end;
    SetLength(LBytes, LArray.length);
    for LIndex := 0 to LArray.length - 1 do
    begin
      LBytes[LIndex] := LArray[LIndex];
    end;
    LDefinition := NyxResourceFromBytes(FKind, LBytes);
  except
    on LException: Exception do
    begin
      LError := LException.Message;
    end;
    else
    begin
      LError := 'The browser could not admit the selected resource file';
    end;
  end;

  if LError = '' then
  begin
    Complete(rpsSelected, LDefinition, '');
  end
  else
  begin
    Complete(rpsFailed, nil, LError);
  end;
end;

function NewNyxBrowserResourcePicker: INyxResourcePicker;
begin
  Result := TResourcePicker.Create;
end;

end.
