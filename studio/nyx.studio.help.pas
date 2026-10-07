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

unit nyx.studio.help;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.model, nyx.studio.session;

const
  NyxStudioComponentHelpID = 'action-component-help';

{ Return an independently owned public Nyx help recipe for the current selected
  projection. Nil means no selection/registered explanation. Caller frees the
  document after the managed presenter copies it. No design/source mutation. }
function BuildNyxStudioComponentHelp(ASession: TNyxStudioSession): TNyxDocument;

implementation

uses
  nyx.text, nyx.types, nyx.controls, nyx.component.help, nyx.composition, nyx.schema;

function BuildNyxStudioComponentHelp(ASession: TNyxStudioSession): TNyxDocument;
var
  LSource: TNyxNode;
  LIndex: Integer;
  LCapabilities: TNyxText;
  LPrimitive: TNyxPrimitiveInfo;
  LCard: INyxCard;
begin
  Result := nil;

  if (ASession = nil) or (ASession.Selected = nil) then
  begin
    Exit;
  end;
  LSource := NyxProjectionSource(ASession.Selected, ASession.Document);
  LIndex := ASession.Catalog.IndexOf(LSource.Kind);

  if LIndex < 0 then
  begin
    LIndex := ASession.Catalog.IndexOf(LSource.ProjectionKind);
  end;

  if LIndex < 0 then
  begin
    Exit;
  end;
  LCapabilities := '';

  if FindNyxPrimitive(LSource.ProjectionKind, LPrimitive) then
  begin
    LCapabilities := 'Browser: ' + NyxCapabilityText(LPrimitive.Browser) +
      ' / LCL: ' + NyxCapabilityText(LPrimitive.Native);
  end;
  LCard := NewNyxComponentHelp(ASession.Catalog[LIndex].Title,
    ASession.Catalog[LIndex].Discovery.Description, LCapabilities);
  Result := TNyxDocument.Create;
  try
    Result.AddPage(LCard);
  except
    Result.Free;
    raise;
  end;
end;

end.

