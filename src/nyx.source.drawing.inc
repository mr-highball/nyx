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
{ Closed drawing constructors invoke portable typed authoring only. Plans/paths
  cross the reader as immutable data values; no provider implementation runs. }

function TConfigurationReader.DrawingConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LMethod: TNyxText;
  LBuilder: INyxDrawingBuilder;
  LPath: TNyxDrawingPath;
  LStyle: TNyxDrawingStyle;
  LSize: Double;
  LAnchor: TNyxDrawingTextAnchor;
  LBold: Boolean;
  LBuilt: Boolean;

  procedure Count(AMinimum, AMaximum: Integer);
  begin

    if (Length(LArgs) < AMinimum) or (Length(LArgs) > AMaximum) then
    begin
      Fail('Wrong drawing argument count');
    end;
  end;

  procedure Kind(AIndex: Integer; AKind: TValueKind);
  begin

    if (AIndex >= Length(LArgs)) or (LArgs[AIndex].Kind <> AKind) then
    begin
      Fail('Wrong typed drawing argument family');
    end;
  end;

begin
  Result := Default(TValue);
  LArgs := nil;
  LBuilt := False;

  if At('(') then
  begin
    LArgs := Arguments(6);
  end;

  if AName = 'newnyxdrawingplan' then
  begin
    Count(1, 1);
    Kind(0, vkCanvasBounds);
    LBuilder := NewNyxDrawingPlan(LArgs[0].CanvasBounds);
    Result.Kind := vkDrawingPlan;
  end
  else if AName = 'nyxdrawingpath' then
  begin
    Count(1, 1);
    Kind(0, vkCanvasPoint);
    Result.Kind := vkDrawingPath;
    Result.Data := NyxDrawingPath(LArgs[0].CanvasPoint).ToData;
  end
  else if AName = 'nyxdrawingrectangle' then
  begin
    Count(1, 1);
    Kind(0, vkCanvasBounds);
    Result.Kind := vkDrawingPath;
    Result.Data := NyxDrawingRectangle(LArgs[0].CanvasBounds).ToData;
  end
  else if AName = 'nyxdrawingstyle' then
  begin
    Count(0, 0);
    Result.Kind := vkDrawingStyle;
    Result.Data := NyxDrawingStyle.ToData;
  end
  else if (AName = 'nyxdrawingpaint') or (AName = 'nyxthemepaint') then
  begin
    Count(1, 1);
    Result.Kind := vkDrawingPaint;

    if AName = 'nyxthemepaint' then
    begin
      Kind(0, vkDrawingRole);
      Result.Data := NyxThemePaint(TNyxThemeColor(LArgs[0].Ordinal)).ToData;
    end
    else
    begin
      Kind(0, vkRGBColor);
      Result.Data := NyxDrawingPaint(LArgs[0].RGBColor).ToData;
    end;
  end
  else if AName = 'nyxdrawingprovider' then
  begin
    Count(1, 1);
    Kind(0, vkText);
    Result.Kind := vkDrawingProvider;
    Result.Text := NyxDrawingProvider(LArgs[0].Text).Name;
  end
  else
    Fail('Unsupported drawing constructor');

  while At('.') do
  begin
    Inc(FCursor);
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := nil;

    if At('(') then
    begin
      LArgs := Arguments(6);
    end;
    case Result.Kind of
      vkDrawingPlan:
        begin

          if LBuilt then
          begin
            Fail('An immutable drawing plan has no builder methods');
          end;

          if LMethod = 'path' then
          begin
            Count(2, 2);
            Kind(0, vkDrawingPath);
            Kind(1, vkDrawingStyle);
            LBuilder.Path(TNyxDrawingPath.FromData(LArgs[0].Data),
              TNyxDrawingStyle.FromData(LArgs[1].Data));
          end
          else if LMethod = 'text' then
          begin
            Count(3, 6);
            Kind(0, vkText);
            Kind(1, vkCanvasPoint);
            Kind(2, vkDrawingPaint);
            LSize := 14;
            LAnchor := ndtaStart;
            LBold := False;

            if Length(LArgs) > 3 then
            begin
              LSize := Numeric(LArgs[3]);
            end;

            if Length(LArgs) > 4 then
            begin
              Kind(4, vkDrawingAnchor);
              LAnchor := TNyxDrawingTextAnchor(LArgs[4].Ordinal);
            end;

            if Length(LArgs) > 5 then
            begin
              Kind(5, vkBoolean);
              LBold := LArgs[5].Ordinal <> 0;
            end;
            LBuilder.Text(LArgs[0].Text, LArgs[1].CanvasPoint,
              TNyxDrawingPaint.FromData(LArgs[2].Data), LSize, LAnchor, LBold);
          end
          else if LMethod = 'build' then
          begin
            Count(0, 0);
            Result.Data := LBuilder.Build.ToData;
            LBuilt := True;
          end
          else
            Fail('Unsupported drawing builder method');
        end;
      vkDrawingPath:
        begin
          LPath := TNyxDrawingPath.FromData(Result.Data);

          if LMethod = 'lineto' then
          begin
            Count(1, 1);
            Kind(0, vkCanvasPoint);
            LPath := LPath.LineTo(LArgs[0].CanvasPoint);
          end
          else if LMethod = 'curveto' then
          begin
            Count(3, 3);
            Kind(0, vkCanvasPoint);
            Kind(1, vkCanvasPoint);
            Kind(2, vkCanvasPoint);
            LPath := LPath.CurveTo(LArgs[0].CanvasPoint,
              LArgs[1].CanvasPoint, LArgs[2].CanvasPoint);
          end
          else if LMethod = 'close' then
          begin
            Count(0, 0);
            LPath := LPath.Close;
          end
          else
            Fail('Unsupported typed path method');
          Result.Data := LPath.ToData;
        end;
      vkDrawingStyle:
        begin
          Count(1, 2);
          Kind(0, vkDrawingPaint);
          LStyle := TNyxDrawingStyle.FromData(Result.Data);

          if LMethod = 'fill' then
          begin
            Count(1, 1);
            LStyle := LStyle.Fill(TNyxDrawingPaint.FromData(LArgs[0].Data));
          end
          else if LMethod = 'stroke' then
          begin
            LSize := 1;

            if Length(LArgs) = 2 then
            begin
              LSize := Numeric(LArgs[1]);
            end;
            LStyle := LStyle.Stroke(TNyxDrawingPaint.FromData(LArgs[0].Data), LSize);
          end
          else
            Fail('Unsupported typed drawing style method');
          Result.Data := LStyle.ToData;
        end;
    else
      Fail('Drawing value has no fluent methods');
    end;
  end;

  if (Result.Kind = vkDrawingPlan) and not LBuilt then
  begin
    Fail('Drawing authoring requires an immutable Build result');
  end;
end;
