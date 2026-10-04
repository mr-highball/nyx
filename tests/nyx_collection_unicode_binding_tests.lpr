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
program nyx_collection_unicode_binding_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.collections,
  nyx.collections.view.types, nyx.model, nyx.codec, nyx.codegen,
  nyx.source, nyx.studio.builds
  {$ifdef PAS2JS}
  , Web
  {$endif};

var
  LDocument, LDecoded, LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSpec, LRead: TNyxCollectionViewSpec;
  LWire, LSource, LPacket: TNyxText;

begin
  LDocument := TNyxDocument.Create;
  LDecoded := nil;
  LCandidate := nil;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LDocument.Collections.Define(NyxCollection('tasks/🌙'),
      NyxCollectionSchema.Text(NyxTextField('caption/🌙'), ''), []);
    LSpec := NyxCollectionView(NyxCollection('tasks/🌙'))
      .Column(NyxTextField('caption/🌙'), 'Task 🌙', cmEditable);
    LRead := TNyxCollectionViewSpec.FromData(LSpec.ToData);

    if LRead.Key.Name <> LSpec.Key.Name then
    begin
      raise Exception.Create('Spec packet key differs');
    end;
    LDocument.AddPage(TNyxNode.Create(nkTable, 'tasks').Binds.Collection(LSpec).Done);
    LWire := TNyxCodec.Encode(LDocument);
    WriteLn('PASS direct spec and wire encode');
    LDecoded := TNyxCodec.Decode(LWire);
    WriteLn('PASS native Unicode design decode');
    LSource := TNyxCodegen.Generate(LDocument);
    LPacket := EncodeNyxBuildRequest(LDocument, LSource);
    DecodeNyxBuildRequest(LPacket, LCandidate, LSource);
    WriteLn('PASS Unicode companion envelope');
    FreeAndNil(LCandidate);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);

    if TNyxCodec.Encode(LCandidate) <> LWire then
    begin
      raise Exception.Create('Reconstructed Unicode binding differs');
    end;
    WriteLn('PASS native Unicode companion replay');
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS Unicode binding packet, design, envelope and companion';
    document.body.setAttribute('data-collection-unicode', 'passed');
    {$endif}
  finally
    LWorkspace.Free;
    LCandidate.Free;
    LDecoded.Free;
    LDocument.Free;
  end;
end.
