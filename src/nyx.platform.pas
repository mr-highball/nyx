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
unit nyx.platform;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses nyx.types, nyx.text, nyx.model;

{ Project typed presentation overrides into an independently realized tree.
  Defaults and rules in the authored document remain untouched. Fixed platform
  rules are consumed so another target cannot subsequently change this tree.
  Viewport rules remain for resize-time projection on the concrete target. }
procedure ApplyNyxPlatform(ARoot: TNyxNode; APlatform: TNyxPlatform);

{ Admission checks every piecewise viewport interval on both concrete targets.
  Inconsistent effective size/split bounds refuse before mounting or publishing.
  Candidates contain copied properties only and retain no authored children. }
procedure ValidateNyxViewportBounds(ANode: TNyxNode);

implementation

uses SysUtils, nyx.responsive;

procedure ApplyNyxPlatform(ARoot: TNyxNode; APlatform: TNyxPlatform);

  procedure Visit(ANode: TNyxNode);
  var
    LCount: Integer;
    LIndex: Integer;
    LPlatform: TNyxPlatform;
    LAttribute: TNyxAttribute;
    LKey: TNyxText;
  begin
    LCount := ANode.Props.Count;
    for LIndex := 0 to LCount - 1 do
    begin
      LKey := ANode.Props.Names[LIndex];

      if TryNyxPlatformKey(LKey, LPlatform, LAttribute) and
        (LPlatform = APlatform) then
      begin
        ANode.SetProp(NyxAttributeName(LAttribute), ANode.Prop(LKey));
      end;
    end;
    for LIndex := ANode.Props.Count - 1 downto 0 do
    begin

      if TryNyxPlatformKey(ANode.Props.Names[LIndex], LPlatform, LAttribute) then
      begin
        ANode.Props.Delete(LIndex);
      end;
    end;
    for LIndex := 0 to ANode.Count - 1 do
    begin
      Visit(ANode.Children[LIndex]);
    end;
  end;

begin

  if (ARoot = nil) or not ARoot.IsRealized or (APlatform = npfAny) then
  begin
    raise ENyxModel.Create('Platform projection requires a realized tree and a concrete target');
  end;
  Visit(ARoot);
end;

procedure ValidateNyxViewportBounds(ANode: TNyxNode);
var
  LBoundaries: TNyxStrings;
  LViewport: TNyxViewportWidth;
  LPlatform: TNyxPlatform;
  LTarget: TNyxPlatform;
  LAttribute: TNyxAttribute;
  LIndex: Integer;
  LBoundary: Integer;
  LProbe: TNyxNode;
  LPosition: Integer;
  LMinimum: Integer;
  LMaximum: Integer;

  procedure AddBoundary(AValue: Integer);
  var
    LText: TNyxText;
  begin
    LText := IntToStr(AValue);

    if LBoundaries.IndexOf(LText) < 0 then
    begin
      LBoundaries.Add(LText);
    end;
  end;

begin
  LBoundaries := TNyxStrings.Create;
  try
    for LIndex := 0 to ANode.Props.Count - 1 do
    begin

      if TryNyxViewportKey(ANode.Props.Names[LIndex], LViewport, LPlatform, LAttribute) then
      begin
        AddBoundary(LViewport.Minimum);
        AddBoundary(LViewport.Maximum);
      end;
    end;

    if LBoundaries.Count = 0 then
    begin
      Exit;
    end;
    AddBoundary(0);
    { Every half-open interval starts at one of these authored bounds. Testing
      those exact starts qualifies all combinations, including overlaps and
      unbounded tails, without sampling guessed screen sizes. }
    for LTarget := npfBrowser to npfNativeLCL do
    begin
      LProbe := TNyxNode.CreateRealized(ANode.Kind, ANode.ID, ANode.ID, ANode.ID);
      try
        LProbe.Props.Assign(ANode.Props);
        ApplyNyxPlatform(LProbe, LTarget);
        for LBoundary := 0 to LBoundaries.Count - 1 do
        begin
          LProbe.ApplyViewport(StrToInt(LBoundaries[LBoundary]), LTarget);
          NyxNodeSizeConstraints(LProbe).Validate;

          if ANode.ProjectionKind = 'split-view' then
          begin
            LPosition := StrToIntDef(LProbe.Prop('split-position'), 65);
            LMinimum := StrToIntDef(LProbe.Prop('split-minimum'), 15);
            LMaximum := StrToIntDef(LProbe.Prop('split-maximum'), 85);

            if (LMinimum > LMaximum) or (LPosition < LMinimum) or (LPosition > LMaximum) then
            begin
              raise ENyxModel.Create('Responsive split position must fit its bounds on ' + ANode.ID);
            end;
          end;
        end;
      finally
        LProbe.Free;
      end;
    end;
  finally
    LBoundaries.Free;
  end;
end;

end.
