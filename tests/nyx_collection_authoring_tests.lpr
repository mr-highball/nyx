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
program nyx_collection_authoring_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  nyx.text,
  nyx.model,
  nyx.codegen,
  nyx.composition,
  nyx.test.collections.authoring
  {$ifdef PAS2JS}
  , Web
  {$else}
  , Interfaces, Forms, Classes
  {$endif};

{$ifndef PAS2JS}
procedure ExportFixture;
var
  LDocument: TNyxDocument;
  LIsolated: TNyxDocument;
  LSource: TNyxText;
  LStream: TFileStream;

  procedure Save(const AName, AText: TNyxText);
  begin
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + AName + '.pas', fmCreate);
    try
      LStream.WriteBuffer(AText[1], Length(AText));
    finally
      LStream.Free;
    end;
  end;
begin

  if ParamCount = 0 then
  begin
    Exit;
  end;
  LDocument := CreateNyxCollectionAuthoringFixture;
  LIsolated := nil;
  try
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.fixture.collection.authoring');
    Save('nyx.fixture.collection.authoring', LSource);
    LIsolated := CloneNyxViewDocument(LDocument, LDocument.Pages[1]);
    Save('nyx.fixture.collection.page', TNyxCodegen.Generate(LIsolated, 'nyx.fixture.collection.page'));
    FreeAndNil(LIsolated);
    LIsolated := CloneNyxViewDocument(LDocument, LDocument.Components[0]);
    Save('nyx.fixture.collection.component', TNyxCodegen.Generate(LIsolated, 'nyx.fixture.collection.component'));
  finally
    LIsolated.Free;
    LDocument.Free;
  end;
end;
{$endif}

procedure Run;
var
  LShared, LControls: Integer;
begin
  try
    LShared := RunNyxCollectionAuthoringTests;
    LControls := RunNyxCollectionAuthoringJourney;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(LShared) + ' shared / ' +
      IntToStr(LControls) + ' collection authoring controls';
    document.body.setAttribute('data-collection-authoring', 'passed');
    {$else}
    ExportFixture;
    WriteLn('PASS ', LShared, ' shared / ', LControls, ' collection authoring controls');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-collection-authoring', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      Halt(1);
      {$endif}
    end;
  end;
end;

begin
  {$ifndef PAS2JS}
  Application.Initialize;
  {$endif}
  Run;
end.
