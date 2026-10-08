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
unit nyx.image.import.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses JS, Web, SysUtils, nyx.text, nyx.images, nyx.image.import;

{ User activation must call Pick directly. The adapter owns only its private
  file input/reader; the reusable form and accepted project stay independent. }
function NewNyxBrowserImagePicker: INyxImagePicker;

implementation

type
  TImagePicker = class(TInterfacedObject, INyxImagePicker)
  private
    FInput: TJSHTMLInputElement;
    FReader: TJSFileReader;
    FReply: TNyxImagePickReply;
    FValidation: TNyxImageValidationPolicy;
    function Chosen(AEvent: TJSEvent): Boolean;
    function Read(AEvent: TJSEvent): Boolean;
    function Cancelled(AEvent: TJSEvent): Boolean;
    procedure Complete(AStatus: TNyxImagePickStatus;
      const ASource: TNyxImageSource; const AError: TNyxText);
  public
    destructor Destroy; override;
    procedure Pick(AReply: TNyxImagePickReply); overload;
    procedure Pick(AReply: TNyxImagePickReply;
      const AValidation: TNyxImageValidationPolicy); overload;
    procedure Cancel;
  end;

procedure TImagePicker.Cancel;
begin
  FReply := nil;

  if FReader <> nil then
  begin
    FReader.onload := nil;
    FReader.onerror := nil;
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

destructor TImagePicker.Destroy;
begin
  Cancel;
  inherited Destroy;
end;

procedure TImagePicker.Pick(AReply: TNyxImagePickReply);
begin
  Pick(AReply, NyxImageValidation);
end;

procedure TImagePicker.Pick(AReply: TNyxImagePickReply;
  const AValidation: TNyxImageValidationPolicy);
begin
  Cancel;
  FReply := AReply;
  FValidation := AValidation;
  FInput := TJSHTMLInputElement(document.createElement('input'));
  FInput.setAttribute('type', 'file');
  FInput.setAttribute('accept', 'image/png,image/jpeg');
  FInput.style.setProperty('display', 'none');
  FInput.onchange := @Chosen;
  FInput.addEventListener('cancel', @Cancelled);
  document.body.appendChild(FInput);
  FInput.click;
end;

procedure TImagePicker.Complete(AStatus: TNyxImagePickStatus;
  const ASource: TNyxImageSource; const AError: TNyxText);
var
  LReply: TNyxImagePickReply;
  LKeepAlive: INyxImagePicker;
begin
  LKeepAlive := Self;
  LReply := FReply;
  Cancel;

  if Assigned(LReply) then
  begin
    LReply(AStatus, ASource, AError);
  end;
  LKeepAlive := nil;
end;

function TImagePicker.Cancelled(AEvent: TJSEvent): Boolean;
begin
  Result := True;
  Complete(ipsCancelled, NyxNoImage, '');
end;

function TImagePicker.Chosen(AEvent: TJSEvent): Boolean;
begin
  Result := True;

  if (FInput = nil) or (FInput.files.length <> 1) then
  begin
    Complete(ipsCancelled, NyxNoImage, '');
    Exit;
  end;

  if (FInput.files[0].size <= 0) or (FInput.files[0].size > NyxImageMaximumBytes) then
  begin
    Complete(ipsFailed, NyxNoImage, 'Choose a PNG or JPEG up to 1 MiB');
    Exit;
  end;
  FReader := TJSFileReader.new;
  FReader.onload := @Read;
  FReader.onerror := @Read;
  FReader.readAsDataURL(FInput.files[0]);
end;

function TImagePicker.Read(AEvent: TJSEvent): Boolean;
var
  LText: TNyxText;
  LSource: TNyxImageSource;
  LComma: Integer;
  LError: TNyxText;
begin
  Result := True;

  if FReader = nil then
  begin
    Exit;
  end;
  LError := '';
  LSource := NyxNoImage;
  try

    if FReader.error <> nil then
    begin
      raise ENyxImage.Create('Cannot read the selected image');
    end;
    LText := String(FReader.result);
    LComma := Pos(',', LText);

    if LComma = 0 then
    begin
      raise ENyxImage.Create('Image reader did not return an encoded raster');
    end;
    LSource := NyxImportedImageBase64(Copy(LText, LComma + 1, MaxInt), FValidation);
  except
    on LException: Exception do
    begin
      LError := LException.Message;
    end;
  end;

  if LError = '' then
  begin
    Complete(ipsSelected, LSource, '');
  end
  else
  begin
    Complete(ipsFailed, NyxNoImage, LError);
  end;
end;

function NewNyxBrowserImagePicker: INyxImagePicker;
begin
  Result := TImagePicker.Create;
end;

end.
