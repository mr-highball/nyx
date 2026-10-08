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

unit nyx.images.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Classes, SysUtils, Types, Graphics, ExtCtrls, nyx.images;

type
  { Ordinary LCL image with portable object sizing. Picture ownership, clipping,
    transparency, callbacks and accessibility remain LCL's contracts. Only the
    destination rectangle changes; no Nyx document/node is retained. }
  TNyxLCLImage = class(TImage)
  private
    FFit: TNyxImageFit;
    FHorizontal: TNyxImageAnchor;
    FVertical: TNyxImageAnchor;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ConfigureImage(AFit: TNyxImageFit; AHorizontal,
      AVertical: TNyxImageAnchor);
    function DestRect: TRect; override;
  end;

{ Caller owns the independently decoded picture. Temporary source bytes/stream
  belong only to this call. Decoder failure cannot change an accepted picture.
  Locations retain legacy local-file behavior; missing files produce no picture. }
function NewNyxLCLPicture(const ASource: TNyxImageSource): TPicture;

implementation

constructor TNyxLCLImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFit := nifContain;
  FHorizontal := niaCenter;
  FVertical := niaCenter;
end;

procedure TNyxLCLImage.ConfigureImage(AFit: TNyxImageFit; AHorizontal,
  AVertical: TNyxImageAnchor);
begin

  if (FFit = AFit) and (FHorizontal = AHorizontal) and (FVertical = AVertical) then
  begin
    Exit;
  end;
  FFit := AFit;
  FHorizontal := AHorizontal;
  FVertical := AVertical;
  Invalidate;
end;

function TNyxLCLImage.DestRect: TRect;
var
  LRect: TNyxImageRectangle;

  function Coordinate(AValue: Double): Integer;
  begin

    if (AValue < Low(Integer)) or (AValue > High(Integer)) then
    begin
      raise ENyxImage.Create('Image destination exceeds this widgetset coordinate range');
    end;
    Result := Round(AValue);
  end;

begin
  LRect := NyxImageRectangle(Picture.Width, Picture.Height, ClientWidth,
    ClientHeight, FFit, FHorizontal, FVertical);
  Result := Rect(Coordinate(LRect.Left), Coordinate(LRect.Top),
    Coordinate(LRect.Left + LRect.Width), Coordinate(LRect.Top + LRect.Height));
end;

function NewNyxLCLPicture(const ASource: TNyxImageSource): TPicture;
var
  LBytes: TNyxImageBytes;
  LStream: TMemoryStream;
  LExtension: String;
begin
  Result := TPicture.Create;
  try
    case ASource.Kind of
      nisEmpty:
        begin
          { The independent empty candidate deliberately clears a picture. }
        end;
      nisLocation:
        begin

          if FileExists(ASource.ToWire) then
          begin
            Result.LoadFromFile(ASource.ToWire);
          end;
        end;
      nisEmbedded:
        begin
          LBytes := ASource.Bytes;
          LStream := TMemoryStream.Create;
          try
            LStream.WriteBuffer(LBytes[0], Length(LBytes));
            LStream.Position := 0;
            LExtension := '.png';

            if ASource.Format = nimJPEG then
            begin
              LExtension := '.jpg';
            end;
            Result.LoadFromStreamWithFileExt(LStream, LExtension);
          finally
            LStream.Free;
          end;
        end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

end.
