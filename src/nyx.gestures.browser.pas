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
unit nyx.gestures.browser;

{$mode delphi}{$H+}{$codepage utf8}
{$modeswitch externalclass}

interface

uses JS, Web, SysUtils, nyx.text, nyx.data, nyx.gestures;

type
  { The owned bridge supplements the installed RTL without changing it. Capture
    methods retain their host semantics, including refusal for inactive IDs. }
  TNyxGestureElement = class external name 'HTMLElement' (TJSHTMLElement)
  public
    function hasPointerCapture(APointerID: NativeInt): Boolean;
  end;

{ Hover capture never invokes getData or reads files. Readable drop capture owns
  every admitted value/file metadata before dispatch. Unsupported host MIME
  names and admission-budget violations refuse the snapshot atomically. }
function CaptureNyxBrowserTransfer(ATransfer: TJSDataTransfer;
  AReadable: Boolean): TNyxTransferSnapshot;
{ Only genuine drag-start response application writes the data store. Values
  are copied as inert transfer data; Nyx never inserts transferred HTML or
  follows transferred URIs automatically. }
procedure OfferNyxBrowserTransfer(ATransfer: TJSDataTransfer;
  const AResult: TNyxGestureResult);

implementation

function CaptureNyxBrowserTransfer(ATransfer: TJSDataTransfer;
  AReadable: Boolean): TNyxTransferSnapshot;
var
  LItems: array of TNyxDataValue;
  LFiles: TNyxTransferFiles;
  LIndex: Integer;
  LCount: Integer;
  LName: TNyxText;
  LHasFiles: Boolean;
  LFile: TJSHTMLFile;
begin

  if ATransfer = nil then
  begin
    raise ENyxGesture.Create('The browser did not supply a physical transfer store');
  end;
  LItems := nil;
  LHasFiles := False;
  for LIndex := 0 to High(ATransfer.types) do
  begin
    LName := ATransfer.types[LIndex];

    if LName = 'Files' then
    begin
      LHasFiles := True;
      Continue;
    end;
    LCount := Length(LItems);

    if LCount >= MaximumNyxTransferItems then
    begin
      raise ENyxGesture.Create('Browser transfer exceeds its format budget');
    end;
    SetLength(LItems, LCount + 1);

    if AReadable then
    begin
      LItems[LCount] := NyxObject([
        NyxField('format', NyxData(NyxTransferFormat(LName).Name)),
        NyxField('text', NyxData(ATransfer.getData(LName)))]);
    end
    else
    begin
      LItems[LCount] := NyxObject([
        NyxField('format', NyxData(NyxTransferFormat(LName).Name))]);
    end;
  end;
  Result := NyxTransferFromData(NyxArray(LItems), AReadable, LHasFiles);

  if AReadable then
  begin

    if ATransfer.files.length > MaximumNyxTransferFiles then
    begin
      raise ENyxGesture.Create('Browser transfer exceeds its file metadata budget');
    end;
    SetLength(LFiles, ATransfer.files.length);
    for LIndex := 0 to High(LFiles) do
    begin
      LFile := ATransfer.files[LIndex];
      LFiles[LIndex].Name := LFile.name;
      LFiles[LIndex].MediaType := LFile._type;
      LFiles[LIndex].Size := LFile.size;
      LFiles[LIndex].Modified := LFile.lastModified;
    end;
    Result := Result.WithFiles(LFiles);
  end;
end;

procedure OfferNyxBrowserTransfer(ATransfer: TJSDataTransfer;
  const AResult: TNyxGestureResult);
var
  LIndex: Integer;
  LFormat: TNyxTransferFormatRef;
begin

  if (ATransfer = nil) or not AResult.Offered then
  begin
    raise ENyxGesture.Create('A drag-start store and accepted offer are required');
  end;
  ATransfer.clearData;
  for LIndex := 0 to AResult.Transfer.Count - 1 do
  begin
    LFormat := AResult.Transfer.Formats[LIndex];
    ATransfer.setData(LFormat.Name, AResult.Transfer.TextFor(LFormat));
  end;
  ATransfer.effectAllowed := NyxDropOperationsName(AResult.Allowed);
end;

end.
