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

unit nyx.images.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Web, JS, nyx.text, nyx.model, nyx.images, nyx.image.lifecycle;

type
  TNyxBrowserImageSignal = procedure of object;
  { The request owns listeners and copied generation data, but only borrows its
    receiver and element. Retire must precede replacement/control disposal.
    Pending decode promises retain an independent lease and cannot call a
    retired receiver. No browser request object enters the portable payload. }
  INyxBrowserImageRequest = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001019000002}']
    procedure Retire;
  end;

function StartNyxBrowserImage(AImage: TJSHTMLImageElement;
  const ASource: TNyxImageSource; const ALifecycle: INyxImageLifecycle;
  ANotify: TNyxBrowserImageSignal): INyxBrowserImageRequest;

{ Apply closed image sizing to an existing ordinary HTML image. The element and
  node are borrowed synchronously; this adapter retains neither. Pixel fetching/
  decoding remains the browser's asynchronous image contract. }
procedure ApplyNyxBrowserImage(ANode: TNyxNode; AImage: TJSHTMLImageElement);

implementation

uses
  SysUtils;

type
  { The Promise closures retain this ordinary Pascal owner until either branch
    explicitly frees it. A captured local COM interface is released at function
    exit by pas2js, even if a future closure still refers to its wrapper. Keep
    the lease in an independently owned object instead; the request has no
    back-reference to this owner or Promise, so no reference cycle is formed. }
  TNyxBrowserImageLease = class
  public
    Request: INyxBrowserImageRequest;
  end;

  TNyxBrowserImageRequest = class(TInterfacedObject, INyxBrowserImageRequest)
  private
    FImage: TJSHTMLImageElement;
    FLifecycle: INyxImageLifecycle;
    FRequest: TNyxImageRequestID;
    FWire: TNyxText;
    FURL: String;
    FNotify: TNyxBrowserImageSignal;
    FLoaded: TJSRawEventHandler;
    FFailed: TJSRawEventHandler;
    FDecoding: Boolean;
    function Active: Boolean;
    procedure Loaded(AEvent: TJSEvent);
    procedure Failed(AEvent: TJSEvent);
    procedure Complete(AReady: Boolean);
    procedure Decode;
  public
    constructor Create(AImage: TJSHTMLImageElement;
      const ASource: TNyxImageSource; const ALifecycle: INyxImageLifecycle;
      ANotify: TNyxBrowserImageSignal);
    procedure Retire;
    destructor Destroy; override;
  end;

constructor TNyxBrowserImageRequest.Create(AImage: TJSHTMLImageElement;
  const ASource: TNyxImageSource; const ALifecycle: INyxImageLifecycle;
  ANotify: TNyxBrowserImageSignal);
var
  LAnchor: TJSHTMLAnchorElement;
begin
  inherited Create;
  FImage := AImage;
  FLifecycle := ALifecycle;
  FNotify := ANotify;
  FWire := ASource.ToWire;
  FRequest := ALifecycle.Start(ASource);

  if ASource.Kind = nisEmpty then
  begin
    FImage.removeAttribute('src');
    Exit;
  end;
  { Resolve relative locations by the same document base as img.src. The
    detached anchor performs no fetch and is not a second image control. }
  LAnchor := TJSHTMLAnchorElement(document.createElement('a'));
  LAnchor.href := FWire;
  FURL := LAnchor.href;
  FLoaded := @Loaded;
  FFailed := @Failed;
  FImage.addEventListener('load', FLoaded);
  FImage.addEventListener('error', FFailed);
  FImage.setAttribute('src', FWire);
end;

function TNyxBrowserImageRequest.Active: Boolean;
begin
  Result := (FImage <> nil) and (FLifecycle.Current.Request = FRequest) and
    (FLifecycle.Current.Phase = nipLoading) and
    (FImage.getAttribute('src') = FWire);
end;

procedure TNyxBrowserImageRequest.Loaded(AEvent: TJSEvent);
begin

  if Active and (FImage.currentSrc = FURL) then
  begin
    { A previous completely available request can remain while the new one is
      pending. Only a load for this URL starts decode, never src/complete alone. }
    Decode;
  end;
end;

procedure TNyxBrowserImageRequest.Failed(AEvent: TJSEvent);
begin

  if Active and ((FImage.currentSrc = '') or (FImage.currentSrc = FURL)) then
  begin
    Complete(False);
  end;
end;

procedure TNyxBrowserImageRequest.Decode;
var
  LLease: TNyxBrowserImageLease;

  function Decoded(AValue: JSValue): JSValue;
  begin
    Result := Undefined;

    try
      Complete(True);
    finally
      LLease.Free;
    end;
  end;

  function Rejected(AValue: JSValue): JSValue;
  begin
    Result := Undefined;

    try
      Complete(False);
    finally
      LLease.Free;
    end;
  end;

begin

  if FDecoding then
  begin
    Exit;
  end;
  FDecoding := True;
  LLease := TNyxBrowserImageLease.Create;
  LLease.Request := Self;
  { The promise owns the lease; this peer does not own the promise. Retire
    clears the weak receiver even when host decoding has yet to settle. }
  try
    FImage.decode._then(@Decoded, @Rejected);
  except
    LLease.Free;
    raise;
  end;
end;

procedure TNyxBrowserImageRequest.Complete(AReady: Boolean);
var
  LNotify: TNyxBrowserImageSignal;
  LLease: INyxBrowserImageRequest;
  LAccepted: Boolean;
begin
  LLease := Self;

  if not Active then
  begin
    Exit;
  end;

  if AReady then
  begin

    if FImage.currentSrc <> FURL then
    begin
      Exit;
    end;

    if (FImage.naturalWidth <= 0) or (FImage.naturalHeight <= 0) then
    begin
      AReady := False;
    end;
  end;

  if AReady then
  begin
    LAccepted := FLifecycle.Ready(FRequest, FImage.naturalWidth, FImage.naturalHeight);
  end
  else
  begin
    LAccepted := FLifecycle.Fail(FRequest, nifDecode, 'Browser could not decode the image request');
  end;
  LNotify := FNotify;
  Retire;

  if (LLease <> nil) and LAccepted and Assigned(LNotify) then
  begin
    { Do not read the borrowed binding/renderer after its callback returns. }
    LNotify;
  end;
end;

procedure TNyxBrowserImageRequest.Retire;
begin
  FNotify := nil;

  if FImage <> nil then
  begin

    if Assigned(FLoaded) then
    begin
      FImage.removeEventListener('load', FLoaded);
      FImage.removeEventListener('error', FFailed);
    end;
    FImage := nil;
  end;
  FLoaded := nil;
  FFailed := nil;
end;

destructor TNyxBrowserImageRequest.Destroy;
begin
  Retire;
  inherited Destroy;
end;

function StartNyxBrowserImage(AImage: TJSHTMLImageElement;
  const ASource: TNyxImageSource; const ALifecycle: INyxImageLifecycle;
  ANotify: TNyxBrowserImageSignal): INyxBrowserImageRequest;
begin
  Result := TNyxBrowserImageRequest.Create(AImage, ASource, ALifecycle, ANotify);
end;

procedure ApplyNyxBrowserImage(ANode: TNyxNode; AImage: TJSHTMLImageElement);
const
  CPositions: array[TNyxImageAnchor] of String = ('0%', '50%', '100%');
begin
  AImage.style.setProperty('object-fit',
    NyxImageFitName(ReadNyxImageFit(ANode.Prop('image-fit', 'contain'))));
  AImage.style.setProperty('object-position',
    CPositions[ReadNyxImageAnchor(ANode.Prop('image-position-x', 'center'))] + ' ' +
    CPositions[ReadNyxImageAnchor(ANode.Prop('image-position-y', 'center'))]);
  { Portable sizing describes encoded pixels. EXIF orientation/color management
    still need their own qualified contract; do not silently infer a shared one. }
  AImage.style.setProperty('image-orientation', 'none');
end;

end.
