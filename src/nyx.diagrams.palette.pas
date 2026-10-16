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
unit nyx.diagrams.palette;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.model, nyx.controls, nyx.diagram.declarations;

type
  { Closed presentation intent. Only Open carries a saved definition identity;
    New starts an independent proposal and Resume restores a retained window. }
  TNyxDiagramLibraryAction = (ndlaNone, ndlaNew, ndlaOpen, ndlaResume);

{ Shared Nyx library face. Open buttons carry copied typed-name metadata at
  this explicit presentation boundary; no callbacks/document are retained.
  New asks the owner to open an independent unsaved proposal; Save is explicit. }
function NewNyxDiagramLibrary(const AID: TNyxText; ADocument: TNyxDocument): INyxCard;
{ Capture only marked library controls, refusing malformed metadata. New has an
  absent reference; opening never saves or mutates the accepted document. }
function CaptureNyxDiagramLibrary(ANode: TNyxNode; out AAction: TNyxDiagramLibraryAction;
  out AReference: TNyxDiagramRef): Boolean;

implementation

uses nyx.data, nyx.diagram;

const
  CLibraryAction = 'nyx.diagramLibrary.action';
  CLibraryName = 'nyx.diagramLibrary.name';

function NewNyxDiagramLibrary(const AID: TNyxText; ADocument: TNyxDocument): INyxCard;
var
  LIndex: Integer;
  LButton: INyxButton;
  LReference: TNyxDiagramRef;
begin

  if ADocument = nil then
  begin
    raise ENyxDiagram.Create('Diagram discovery requires its document');
  end;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(8).Done;
  Result.Add(NewNyxHeading(AID + '-title').WithText('Diagrams'));
  Result.Add(NewNyxLabel(AID + '-help').WithText('Open a saved diagram, or start a new workflow.'));
  LButton := NewNyxButton(AID + '-new').WithText('New diagram');
  LButton.Node.Extensions.SetValue(NyxExtension(CLibraryAction), NyxData(Ord(ndlaNew)));
  Result.Add(LButton);
  LButton := NewNyxButton(AID + '-resume').WithText('Resume diagram workspace');
  LButton.Node.Extensions.SetValue(NyxExtension(CLibraryAction), NyxData(Ord(ndlaResume)));
  Result.Add(LButton);
  for LIndex := 0 to ADocument.Diagrams.Count - 1 do
  begin
    LReference := ADocument.Diagrams.Reference(LIndex);
    LButton := NewNyxButton(AID + '-open-' + TNyxText(IntToStr(LIndex))).WithText(LReference.Name);
    LButton.Node.Extensions.SetValue(NyxExtension(CLibraryAction), NyxData(Ord(ndlaOpen)));
    LButton.Node.Extensions.SetValue(NyxExtension(CLibraryName), NyxData(LReference.Name));
    Result.Add(LButton);
  end;
end;

function CaptureNyxDiagramLibrary(ANode: TNyxNode; out AAction: TNyxDiagramLibraryAction;
  out AReference: TNyxDiagramRef): Boolean;
var
  LAction: Integer;
begin
  AAction := ndlaNone;
  AReference := Default(TNyxDiagramRef);
  Result := (ANode <> nil) and ANode.Extensions.Has(NyxExtension(CLibraryAction));

  if not Result then
  begin
    Exit;
  end;
  LAction := ANode.Extensions.Value(NyxExtension(CLibraryAction)).AsInteger;

  if (LAction < Ord(ndlaNew)) or (LAction > Ord(ndlaResume)) then
  begin
    raise ENyxDiagram.Create('Unknown diagram library action');
  end;
  AAction := TNyxDiagramLibraryAction(LAction);

  if AAction = ndlaOpen then
  begin
    AReference := NyxDiagramRef(ANode.Extensions.Value(NyxExtension(CLibraryName)).AsText);
  end;
end;

end.
