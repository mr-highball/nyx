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
unit nyx.image.import.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Classes, SysUtils, Dialogs, nyx.text, nyx.images, nyx.image.import;

{ Local UTF-8 filename is borrowed for this bounded byte read, never persisted.
  File/header/decoder failures propagate; no accepted picture is modified. }
function ReadNyxImageFile(const AFileName: TNyxText): TNyxImageSource;
{ Decodes a detached candidate before a file or inline proposal is published.
  No widget/result slot is modified on failure. The temporary picture is owned
  here; immutable source bytes remain the caller's copied value. }
procedure ValidateNyxLCLImageSource(const ASource: TNyxImageSource);
function NewNyxLCLImagePicker: INyxImagePicker;

implementation

uses Graphics, nyx.images.lcl;

type
  TImagePicker = class(TInterfacedObject, INyxImagePicker)
  private
    FReply: TNyxImagePickReply;
    FGeneration: Integer;
    FPicking: Boolean;
  public
    procedure Pick(AReply: TNyxImagePickReply);
    procedure Cancel;
  end;

function ReadNyxImageFile(const AFileName: TNyxText): TNyxImageSource;
var
  LFile: TFileStream;
  LBytes: TNyxImageBytes;
  LSource: TNyxImageSource;
begin
  LFile := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try

    if (LFile.Size <= 0) or (LFile.Size > NyxImageMaximumBytes) then
    begin
      raise ENyxImage.Create('Choose a PNG or JPEG up to 1 MiB');
    end;
    SetLength(LBytes, Integer(LFile.Size));
    LFile.ReadBuffer(LBytes[0], Length(LBytes));
    LSource := NyxImportedImage(LBytes);
  finally
    LFile.Free;
  end;
  { FPC may alias a managed return slot with the caller's assignment. Publish
    Result only after decoding, so refusal cannot expose a partial candidate. }
  ValidateNyxLCLImageSource(LSource);
  Result := LSource;
end;

procedure ValidateNyxLCLImageSource(const ASource: TNyxImageSource);
var
  LPicture: TPicture;
begin
  LPicture := NewNyxLCLPicture(ASource);
  LPicture.Free;
end;

procedure TImagePicker.Cancel;
begin
  FReply := nil;
  Inc(FGeneration);
end;

procedure TImagePicker.Pick(AReply: TNyxImagePickReply);
var
  LDialog: TOpenDialog;
  LStatus: TNyxImagePickStatus;
  LSource: TNyxImageSource;
  LError: TNyxText;
  LReply: TNyxImagePickReply;
  LGeneration: Integer;
  LKeepAlive: INyxImagePicker;
begin

  if FPicking then
  begin
    raise ENyxImage.Create('A native image picker is already open');
  end;
  LKeepAlive := Self;
  Cancel;
  FReply := AReply;
  LGeneration := FGeneration;
  FPicking := True;
  LSource := NyxNoImage;
  LError := '';
  LStatus := ipsCancelled;
  LDialog := TOpenDialog.Create(nil);
  try
    LDialog.Title := 'Import image';
    LDialog.Filter := 'PNG or JPEG|*.png;*.jpg;*.jpeg';
    LDialog.Options := [ofFileMustExist, ofPathMustExist, ofEnableSizing];
    try

      if LDialog.Execute then
      begin
        LSource := ReadNyxImageFile(TNyxText(LDialog.FileName));
        LStatus := ipsSelected;
      end;
    except
      on LException: Exception do
      begin
        LStatus := ipsFailed;
        LError := TNyxText(LException.Message);
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
    LReply(LStatus, LSource, LError);
  end;
  LKeepAlive := nil;
end;

function NewNyxLCLImagePicker: INyxImagePicker;
begin
  Result := TImagePicker.Create;
end;

end.
