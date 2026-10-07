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

{ Included inside TNyxCodegen.Generate. Emit portable meaning through typed
  fluent declarations; focus, search buffers and runtime check changes stay out. }

  procedure EmitMenus;
  const
    CSides: array[TNyxPopoverSide] of TNyxText =
      ('npsBelow', 'npsAbove', 'npsRight', 'npsLeft');
    CAlignments: array[TNyxPopoverAlignment] of TNyxText =
      ('npaStart', 'npaCenter', 'npaEnd');
    CSizing: array[TNyxPopoverSizing] of TNyxText = ('npzContent', 'npzFixed');
    COpening: array[TNyxMenuOpening] of TNyxText = ('nmoFirst', 'nmoLast');
    CMatch: array[TNyxTypeAheadMatch] of TNyxText = ('ntmFolded', 'ntmExact');
  var
    LIndex: Integer;
    LItemIndex: Integer;
    LDefinition: INyxMenuDefinition;
    LItem: INyxMenuDeclarationItem;
    LOptions: TNyxMenuOptions;
    LPlacement: TNyxPopoverOptions;
    LCall: TNyxText;
    LDismissals: TNyxText;
    LRoot: TNyxText;

    function BooleanText(AValue: Boolean): TNyxText;
    begin

      if AValue then
      begin
        Result := 'True';
      end
      else
      begin
        Result := 'False';
      end;
    end;

  begin
    for LIndex := 0 to ADocument.Menus.Count - 1 do
    begin
      LDefinition := ADocument.Menus.Definition(ADocument.Menus.Reference(LIndex));
      LOptions := LDefinition.Options;
      LPlacement := LOptions.Placement;
      LRoot := 'NyxPageRoot';

      if LDefinition.Root.Kind = nrReusable then
      begin
        LRoot := 'NyxReusableRoot';
      end;
      LDismissals := '';

      if npdEscape in LPlacement.Dismissals then
      begin
        LDismissals := 'npdEscape';
      end;

      if npdOutsidePress in LPlacement.Dismissals then
      begin

        if LDismissals <> '' then
        begin
          LDismissals := LDismissals + ', ';
        end;
        LDismissals := LDismissals + 'npdOutsidePress';
      end;
      LLines.Add('');
      LLines.Add('    Result.Menus.Define(');
      LLines.Add('      NyxMenuRef(' +
        PascalString(ADocument.Menus.Reference(LIndex).Name) + '),');
      LLines.Add('      NewNyxMenuDefinition(');
      LLines.Add('        ' + LRoot + '(' + PascalString(LDefinition.Root.Name) + '),');
      LLines.Add('        NyxMenu(' + PascalString(LPlacement.Title) + ')');
      LLines.Add('          .Opening(' + COpening[LOptions.OpenAt] + ')');
      LLines.Add('          .Wrap(' + BooleanText(LOptions.Wraps) + ')');
      LLines.Add('          .TypeAhead(NyxTypeAhead');
      LLines.Add('            .Enabled(' + BooleanText(LOptions.Search.IsEnabled) + ')');
      LLines.Add('            .WindowMilliseconds(' + IntToStr(LOptions.Search.WindowMS) + ')');
      LLines.Add('            .Match(' + CMatch[LOptions.Search.MatchMode] + '))');
      LLines.Add('          .Presentation(NyxPopover(' + PascalString(LPlacement.Title) + ')');
      LLines.Add('            .Placement(' + CSides[LPlacement.Side] + ', ' +
        CAlignments[LPlacement.Alignment] + ')');
      LLines.Add('            .Sizing(' + CSizing[LPlacement.SizeMode] + ')');
      LLines.Add('            .Size(' + IntToStr(LPlacement.Width) + ', ' +
        IntToStr(LPlacement.Height) + ')');
      LLines.Add('            .Spacing(' + IntToStr(LPlacement.Gap) + ', ' +
        IntToStr(LPlacement.Margin) + ')');

      if LPlacement.InitialFocus.Name <> '' then
      begin
        LLines.Add('            .Focus(NyxPart(' +
          PascalString(LPlacement.InitialFocus.Name) + '))');
      end;
      LLines.Add('            .DismissOn([' + LDismissals + '])))');
      for LItemIndex := 0 to LDefinition.Count - 1 do
      begin
        LItem := LDefinition.Item(LItemIndex);
        LCall := 'NyxPart(' + PascalString(LItem.Part.Name) + ')';
        case LItem.Kind of
          nmiAction:
            LCall := 'Action(' + LCall + ', NyxMenuCommand(' +
              PascalString(LItem.Command.Name) + '), ' + BooleanText(LItem.IsEnabled) + ')';
          nmiCheck:
            LCall := 'Check(' + LCall + ', NyxMenuCommand(' +
              PascalString(LItem.Command.Name) + '), ' + BooleanText(LItem.IsChecked) +
              ', ' + BooleanText(LItem.IsEnabled) + ')';
          nmiRadio:
            LCall := 'Radio(' + LCall + ', NyxMenuCommand(' +
              PascalString(LItem.Command.Name) + '), NyxMenuGroup(' +
              PascalString(LItem.Group.Name) + '), ' + BooleanText(LItem.IsChecked) +
              ', ' + BooleanText(LItem.IsEnabled) + ')';
          nmiSeparator:
            LCall := 'Separator(' + LCall + ')';
          nmiSubmenu:
            LCall := 'Submenu(' + LCall + ', NyxMenuRef(' +
              PascalString(LItem.Submenu.Name) + '), ' + BooleanText(LItem.IsEnabled) + ')';
        end;
        LLines.Add('        .' + LCall);
      end;
      LLines[LLines.Count - 1] := LLines[LLines.Count - 1] + ');';
    end;
  end;
