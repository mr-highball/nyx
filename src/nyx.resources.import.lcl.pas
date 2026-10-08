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


unit nyx.resources.import.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.resources, nyx.resources.import;

{ Borrow a UTF-8 machine filename for one bounded byte read. Empty text/binary
  files are admitted; JSON/images retain strict content validation. Native image
  decoding must succeed on a detached candidate. Failure publishes no result.
  No path/global codepage change or file handle survives this call. }
function ReadNyxResourceFile(const AFileName: TNyxText;
  AKind: TNyxResourceKind): INyxResourceDefinition;
function NewNyxLCLResourcePicker: INyxResourcePicker;

implementation

uses Classes, SysUtils, Dialogs, LazFileUtils, nyx.bytes, nyx.image.import.lcl;

type
  TResourcePicker = class(TInterfacedObject, INyxResourcePicker)
  private
    FReply: TNyxResourcePickReply;
    FGeneration: Integer;
    FPicking: Boolean;
  public
    procedure Pick(AKind: TNyxResourceKind; AReply: TNyxResourcePickReply);
    procedure Cancel;
  end;

function ReadNyxResourceFile(const AFileName: TNyxText;
  AKind: TNyxResourceKind): INyxResourceDefinition;
var
  LHandle: THandle;
  LStream: THandleStream;
  LBytes: TNyxBytes;
  LDefinition: INyxResourceDefinition;
begin
  NyxResourceKindName(AKind);
  LHandle := FileOpenUTF8(AFileName, fmOpenRead or fmShareDenyWrite);

  if LHandle = THandle(-1) then
  begin
    raise ENyxResource.Create('Cannot open the selected resource file');
  end;
  LStream := nil;
  try
    LStream := THandleStream.Create(LHandle);

    if (LStream.Size < 0) or (LStream.Size > NyxMaximumPackedBytes) then
    begin
      raise ENyxResource.Create('Choose a resource file up to 1 MiB');
    end;
    SetLength(LBytes, Integer(LStream.Size));

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    LDefinition := NyxResourceFromBytes(AKind, LBytes);

    if AKind = nrkImage then
    begin
      ValidateNyxLCLImageSource(LDefinition.Image);
    end;
    Result := LDefinition;
  finally
    LStream.Free;
    FileClose(LHandle);
  end;
end;

procedure TResourcePicker.Cancel;
begin
  FReply := nil;
  Inc(FGeneration);
end;

procedure TResourcePicker.Pick(AKind: TNyxResourceKind; AReply: TNyxResourcePickReply);
var
  LDialog: TOpenDialog;
  LGeneration: Integer;
  LStatus: TNyxResourcePickStatus;
  LDefinition: INyxResourceDefinition;
  LError: TNyxText;
  LReply: TNyxResourcePickReply;
  LLease: INyxResourcePicker;
begin
  NyxResourceKindName(AKind);

  if FPicking then
  begin
    raise ENyxResource.Create('A native resource picker is already open');
  end;
  LLease := Self;
  Cancel;
  FReply := AReply;
  LGeneration := FGeneration;
  FPicking := True;
  LStatus := rpsCancelled;
  LDefinition := nil;
  LError := '';
  LDialog := TOpenDialog.Create(nil);
  try
    LDialog.Title := 'Import resource / ' + NyxResourceKindName(AKind);
    LDialog.Filter := 'All files|*';
    LDialog.Options := [ofFileMustExist, ofPathMustExist, ofEnableSizing];
    try

      if LDialog.Execute then
      begin
        LDefinition := ReadNyxResourceFile(TNyxText(LDialog.FileName), AKind);
        LStatus := rpsSelected;
      end;
    except
      on LException: Exception do
      begin
        LStatus := rpsFailed;
        LError := LException.Message;
      end;
    end;
  finally
    LDialog.Free;
    FPicking := False;
  end;
  LReply := nil;

  if LGeneration = FGeneration then
  begin
    LReply := FReply;
    FReply := nil;
  end;

  if Assigned(LReply) then
  begin
    LReply(LStatus, LDefinition, LError);
  end;
  LLease := nil;
end;

function NewNyxLCLResourcePicker: INyxResourcePicker;
begin
  Result := TResourcePicker.Create;
end;

end.
