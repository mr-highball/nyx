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

unit nyx.projection.fixture;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model;

{ Complete handwritten construction, intentionally outside the strict fluent
  importer grammar. The caller owns the returned document and its descendants. }
function BuildNyxDocument: TNyxDocument;

implementation

uses SysUtils, nyx.text, nyx.types, nyx.controls, nyx.state, nyx.resources;

const
  CQualificationText: TNyxText = 'A rocket 🚀, a letter 𐐷, and é.';

type
  { A normal application helper: specialized managed controls remain substitutable
    and are admitted by the document only after their independent construction. }
  TNotebookCards = class
  public
    class function Caption: TNyxText; static;
    class function Definition: INyxCard; static;
  end;

function PageName(AIndex: Integer): TNyxText;
begin
  Result := 'Notebook ' + TNyxText(IntToStr(AIndex));
end;

class function TNotebookCards.Caption: TNyxText;
begin
  Result := CQualificationText;
end;

class function TNotebookCards.Definition: INyxCard;
var
  LCaptionHeading: INyxHeading;
begin
  Result := NewNyxCard('reusable-note', ncoDescriptor);
  Result.Configure.Padding(3 * 4).Surface(True);
  LCaptionHeading := NewNyxHeading('note-caption', ncoDescriptor);
  LCaptionHeading.Configure.Text(Caption).PartName(NyxPart('caption'));
  Result.Add(LCaptionHeading);
end;

function BuildNyxDocument: TNyxDocument;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LHeading: INyxHeading;
  LInstance: INyxComponent;
  LIndex: Integer;
  LSuffix: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Handwritten notebook';
    LDocument.State.SetValue(NyxTextState('greeting'), TNotebookCards.Caption);
    LDocument.State.SetValue(NyxIntegerState('notes'), 1 + 2);
    LDocument.State.SetValue(NyxNumberState('scale'), 1 / 8);
    LDocument.Resources.Define(NyxResourceRef('instructions'),
      NyxTextResource(TNotebookCards.Caption));
    LDocument.Resources.Define(NyxResourceRef('data'),
      NyxJSONResource('{"value":"' + TNotebookCards.Caption + '"}'));
    LDocument.AddComponent(TNotebookCards.Definition);
    for LIndex := 1 to 2 do
    begin
      LSuffix := TNyxText(IntToStr(LIndex));
      LPage := NewNyxPage('notebook-' + LSuffix, ncoDescriptor);
      LPage.Configure.Text(PageName(LIndex)).Padding(8 + LIndex);
      LHeading := NewNyxHeading('heading-' + LSuffix, ncoDescriptor);
      LHeading.Text := 'Notes for page ' + LSuffix;
      LPage.Add(LHeading);
      LInstance := NewNyxComponent('note-' + LSuffix, ncoDescriptor);
      LInstance.Configure.Component(NyxComponent('reusable-note'));
      LPage.Add(LInstance);
      LDocument.AddPage(LPage);
    end;
    LDocument.Validate;
    Result := LDocument;
    LDocument := nil;
  finally
    LDocument.Free;
  end;
end;

end.
