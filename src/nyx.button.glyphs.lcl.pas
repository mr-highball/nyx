{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
unit nyx.button.glyphs.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses Classes, Graphics, IntfGraphics, nyx.images, nyx.button.presentation;

type
  { Renderer-owned, bounded decoded atlas cache. It uses the ordinary native
    image decoder and retains immutable pixels only. Buttons receive independent
    tiny rasters, so candidate/adopted cache ownership never outlives a widget.
    A source can be shared by every toolbar command without decoding it again. }
  TNyxLCLButtonGlyphCache = class
  private
    FImages: TStringList;
    function Pixels(const ASource: TNyxImageSource): TLazIntfImage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Dimensions(const ASource: TNyxImageSource; out AWidth, AHeight: Integer);
    { Decode and crop before replacing the accepted glyph. The caller owns the
      result. No resource transport occurs here; unavailable sources return nil. }
    function Prepare(const ASource: TNyxImageSource;
      const APresentation: TNyxButtonPresentation; AForeground: TColor): TBitmap;
  end;

implementation

uses SysUtils, Math, FPImage, GraphType, nyx.images.lcl;

constructor TNyxLCLButtonGlyphCache.Create;
begin
  inherited Create;
  FImages := TStringList.Create;
  FImages.OwnsObjects := True;
  { URLs and packed Base64 are exact data: changing case changes the source. }
  FImages.CaseSensitive := True;
end;

destructor TNyxLCLButtonGlyphCache.Destroy;
begin
  FImages.Free;
  inherited Destroy;
end;

function TNyxLCLButtonGlyphCache.Pixels(const ASource: TNyxImageSource): TLazIntfImage;
var
  LIndex: Integer;
  LPicture: TPicture;
begin
  Result := nil;

  if ASource.Kind = nisEmpty then
  begin
    Exit;
  end;
  LIndex := FImages.IndexOf(ASource.ToWire);

  if LIndex >= 0 then
  begin
    Exit(TLazIntfImage(FImages.Objects[LIndex]));
  end;
  LPicture := NewNyxLCLPicture(ASource);
  try

    if (LPicture.Width < 1) or (LPicture.Height < 1) or
      not (LPicture.Graphic is TRasterImage) then
    begin
      Exit;
    end;
    Result := TRasterImage(LPicture.Graphic).CreateIntfImage;
    try

      if FImages.Count >= 4 then
      begin
        FImages.Delete(0);
      end;
      FImages.AddObject(ASource.ToWire, Result);
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    LPicture.Free;
  end;
end;

procedure TNyxLCLButtonGlyphCache.Dimensions(const ASource: TNyxImageSource;
  out AWidth, AHeight: Integer);
var
  LImage: TLazIntfImage;
begin
  AWidth := 0;
  AHeight := 0;
  LImage := Pixels(ASource);

  if LImage <> nil then
  begin
    AWidth := LImage.Width;
    AHeight := LImage.Height;
  end;
end;

function TNyxLCLButtonGlyphCache.Prepare(const ASource: TNyxImageSource;
  const APresentation: TNyxButtonPresentation; AForeground: TColor): TBitmap;
var
  LSource, LTile: TLazIntfImage;
  LBounds: TNyxGlyphBounds;
  LColor, LSample: TFPColor;
  LX, LY, LSX, LSY, LWidth, LHeight, LCount: Integer;
  LRed, LGreen, LBlue, LAlpha: Int64;
  LStartX, LStartY, LEndX, LEndY: Integer;
begin
  Result := nil;
  LSource := Pixels(ASource);

  if LSource = nil then
  begin
    Exit;
  end;
  LBounds := APresentation.GlyphCell.PixelBounds(LSource.Width, LSource.Height);
  LWidth := APresentation.GlyphBoxSize;
  LHeight := Max(1, Round(LWidth * LBounds.Height / LBounds.Width));

  if LHeight > APresentation.GlyphBoxSize then
  begin
    LHeight := APresentation.GlyphBoxSize;
    LWidth := Max(1, Round(LHeight * LBounds.Width / LBounds.Height));
  end;
  LTile := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha]);
  try
    LTile.SetSize(LWidth, LHeight);
    { Area samples preserve the generated antialiased alpha when a large atlas
      cell becomes a small logical glyph. RGB is alpha-weighted to avoid dark
      fringes around transparent pixels; theme ink replaces RGB, never alpha. }
    for LY := 0 to LHeight - 1 do
    begin
      LStartY := LBounds.Top + LY * LBounds.Height div LHeight;
      LEndY := LBounds.Top + Max((LY + 1) * LBounds.Height div LHeight, LY * LBounds.Height div LHeight + 1);
      for LX := 0 to LWidth - 1 do
      begin
        LStartX := LBounds.Left + LX * LBounds.Width div LWidth;
        LEndX := LBounds.Left + Max((LX + 1) * LBounds.Width div LWidth, LX * LBounds.Width div LWidth + 1);
        LRed := 0; LGreen := 0; LBlue := 0; LAlpha := 0; LCount := 0;
        for LSY := LStartY to LEndY - 1 do
        begin
          for LSX := LStartX to LEndX - 1 do
          begin
            LSample := LSource.Colors[LSX, LSY];
            Inc(LRed, Int64(LSample.Red) * LSample.Alpha);
            Inc(LGreen, Int64(LSample.Green) * LSample.Alpha);
            Inc(LBlue, Int64(LSample.Blue) * LSample.Alpha);
            Inc(LAlpha, LSample.Alpha);
            Inc(LCount);
          end;
        end;
        LColor := colTransparent;
        LColor.Alpha := LAlpha div LCount;

        if APresentation.Tone = ngtForeground then
        begin
          LColor.Red := Red(ColorToRGB(AForeground)) * 257;
          LColor.Green := Green(ColorToRGB(AForeground)) * 257;
          LColor.Blue := Blue(ColorToRGB(AForeground)) * 257;
        end
        else if LAlpha > 0 then
        begin
          LColor.Red := LRed div LAlpha;
          LColor.Green := LGreen div LAlpha;
          LColor.Blue := LBlue div LAlpha;
        end;
        LTile.Colors[LX, LY] := LColor;
      end;
    end;
    Result := TBitmap.Create;
    try
      Result.LoadFromIntfImage(LTile);
    except
      FreeAndNil(Result);
      raise;
    end;
  finally
    LTile.Free;
  end;
end;

end.
