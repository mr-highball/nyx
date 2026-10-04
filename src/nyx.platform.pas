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
  Defaults and rules in the authored document remain untouched. Rules are
  removed from the result so other targets cannot subsequently affect this
  projection, including in-place live binding refreshes. }
procedure ApplyNyxPlatform(ARoot: TNyxNode; APlatform: TNyxPlatform);

implementation

uses SysUtils;

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

      if Copy(ANode.Props.Names[LIndex], 1, 5) = '@nyx.' then
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

end.
