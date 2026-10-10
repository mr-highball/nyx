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

unit nyx.files.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.files;

{ User-activated selection and strict UTF-8 byte reading. Temporary DOM/reader
  owners remain inside this adapter; copied payloads contain no browser handles. }
function NewNyxBrowserTextFiles: INyxTextFileExchange;

implementation

uses SysUtils, JS, Web, nyx.text, nyx.bytes;

type
  TTextFiles = class(TInterfacedObject, INyxTextFileExchange)
  private
    FInput: TJSHTMLInputElement;
    FReader: TJSFileReader;
    FSelection: TNyxTextFileSelection;
    FReply: TNyxTextFileReply;
    FFiles: TNyxTextFiles;
    FIndex: Integer;
    function Chosen(AEvent: TJSEvent): Boolean;
    function Read(AEvent: TJSEvent): Boolean;
    function Cancelled(AEvent: TJSEvent): Boolean;
    procedure ReadNext;
    procedure Complete(AStatus: TNyxFilePickStatus; const AError: TNyxText);
  public
    destructor Destroy; override;
    procedure Pick(const ASelection: TNyxTextFileSelection; AReply: TNyxTextFileReply);
    function ExportFiles(const AFiles: TNyxTextFiles): Boolean;
    procedure Cancel;
  end;

procedure TTextFiles.Cancel;
begin
  FReply := nil;
  FFiles := nil;

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

destructor TTextFiles.Destroy;
begin
  Cancel;
  inherited Destroy;
end;

procedure TTextFiles.Pick(const ASelection: TNyxTextFileSelection;
  AReply: TNyxTextFileReply);
var
  LIndex: Integer;
  LAccept: TNyxText;
begin
  ASelection.Validate;
  Cancel;
  FSelection := ASelection;
  FReply := AReply;
  try
    FInput := TJSHTMLInputElement(document.createElement('input'));
    FInput.setAttribute('type', 'file');
    FInput.style.setProperty('display', 'none');
    FInput.setAttribute('data-nyx-text-files', 'true');
    FInput.multiple := ASelection.MaximumFiles > 1;
    LAccept := '';
    for LIndex := 0 to ASelection.ExtensionCount - 1 do
    begin

      if LAccept <> '' then
      begin
        LAccept := LAccept + ',';
      end;
      LAccept := LAccept + '.' + ASelection.Extension(LIndex);
    end;
    FInput.accept := LAccept;
    FInput.onchange := @Chosen;
    FInput.addEventListener('cancel', @Cancelled);
    document.body.appendChild(FInput);
    FInput.click;
  except
    Complete(fpsFailed, 'The browser could not open its text-file picker');
  end;
end;

procedure TTextFiles.Complete(AStatus: TNyxFilePickStatus; const AError: TNyxText);
var
  LReply: TNyxTextFileReply;
  LFiles: TNyxTextFiles;
  LLease: INyxTextFileExchange;
begin
  LLease := Self;
  LReply := FReply;
  LFiles := nil;

  if AStatus = fpsSelected then
  begin
    LFiles := FFiles;
  end;
  Cancel;

  if Assigned(LReply) then
  begin
    LReply(AStatus, LFiles, AError);
  end;
  LLease := nil;
end;

function TTextFiles.Cancelled(AEvent: TJSEvent): Boolean;
begin
  Result := True;
  Complete(fpsCancelled, '');
end;

procedure TTextFiles.ReadNext;
begin
  FReader := TJSFileReader.new;
  FReader.onload := @Read;
  FReader.onerror := @Read;
  FReader.onabort := @Read;
  FReader.readAsArrayBuffer(FInput.files[FIndex]);
end;

function TTextFiles.Chosen(AEvent: TJSEvent): Boolean;
var
  LIndex: Integer;
  LTotal: Integer;
begin
  Result := True;

  if (FInput = nil) or (FInput.files.length = 0) then
  begin
    Complete(fpsCancelled, '');
    Exit;
  end;
  try

    if FInput.files.length > FSelection.MaximumFiles then
    begin
      raise ENyxFile.Create('Too many text files were selected');
    end;
    LTotal := 0;
    for LIndex := 0 to FInput.files.length - 1 do
    begin

      if not FSelection.Accepts(FInput.files[LIndex].name) or
        (FInput.files[LIndex].size > FSelection.MaximumBytes) then
      begin
        raise ENyxFile.Create('A selected text file is filtered or over budget');
      end;
      Inc(LTotal, Integer(FInput.files[LIndex].size));

      if LTotal > NyxTextFileMaximumSelectionBytes then
      begin
        raise ENyxFile.Create('Selected text files exceed the complete byte budget');
      end;
    end;
    SetLength(FFiles, FInput.files.length);
    FIndex := 0;
    ReadNext;
  except
    on LException: Exception do
    begin
      Complete(fpsFailed, LException.Message);
    end;
    else
    begin
      Complete(fpsFailed, 'The browser could not read the selected text files');
    end;
  end;
end;

function TTextFiles.Read(AEvent: TJSEvent): Boolean;
var
  LArray: TJSUint8Array;
  LBytes: TNyxBytes;
  LIndex: Integer;
  LError: TNyxText;
begin
  Result := True;

  if FReader = nil then
  begin
    Exit;
  end;
  LError := '';
  try

    if (FReader.error <> nil) or not isObject(FReader.result) then
    begin
      raise ENyxFile.Create('Cannot read the selected text file');
    end;
    LArray := TJSUint8Array.new(TJSArrayBuffer(FReader.result));

    if LArray.length > FSelection.MaximumBytes then
    begin
      raise ENyxFile.Create('The read text file exceeds its byte budget');
    end;
    SetLength(LBytes, LArray.length);
    for LIndex := 0 to LArray.length - 1 do
    begin
      LBytes[LIndex] := LArray[LIndex];
    end;
    FFiles[FIndex] := NyxTextFileBytes(FInput.files[FIndex].name, LBytes);
    FReader.onload := nil;
    FReader.onerror := nil;
    FReader.onabort := nil;
    FReader := nil;
    Inc(FIndex);

    if FIndex < Length(FFiles) then
    begin
      ReadNext;
      Exit;
    end;
    ValidateNyxTextFiles(FFiles, FSelection);
  except
    on LException: Exception do
    begin
      LError := LException.Message;
    end;
    else
    begin
      LError := 'The browser could not admit the selected UTF-8 text files';
    end;
  end;

  if LError <> '' then
  begin
    Complete(fpsFailed, LError);
  end
  else
  begin
    Complete(fpsSelected, '');
  end;
end;

function TTextFiles.ExportFiles(const AFiles: TNyxTextFiles): Boolean;
var
  LIndex: Integer;
  LAnchor: TJSHTMLAnchorElement;
  LLease: INyxTextFileExchange;
begin
  ValidateNyxTextFiles(AFiles, NyxTextFileSelection.UpToFiles(NyxTextFileMaximumFiles));
  LLease := Self;
  for LIndex := 0 to High(AFiles) do
  begin
    LAnchor := TJSHTMLAnchorElement(document.createElement('a'));
    LAnchor.href := 'data:application/octet-stream;charset=utf-8,' +
      encodeURIComponent(AFiles[LIndex].Text);
    LAnchor.download := AFiles[LIndex].Name;
    document.body.appendChild(LAnchor);
    try
      LAnchor.click;
    finally
      { Host refusal still retires the temporary anchor. Saved bytes remain an
        explicit host/browser download outcome, independent of the design. }
      LAnchor.remove;
    end;
  end;
  Result := True;
  LLease := nil;
end;

function NewNyxBrowserTextFiles: INyxTextFileExchange;
begin
  Result := TTextFiles.Create;
end;

end.
