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

program nyx_collection_generated_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  SysUtils,
  {$IFDEF PAS2JS}
  Web,
  {$ENDIF}
  nyx.text,
  nyx.model,
  nyx.codec,
  nyx.collections,
  nyx.application.state,
  nyx.test.collections.registry,
  nyx.fixture.collections;

var
  LExpected: TNyxDocument;
  LActual: TNyxDocument;
  LApplication: TNyxApplicationState;
  LSnapshot: INyxCollectionSnapshot;
  LStore: INyxCollection;
  LCount: Integer;

begin
  LExpected := nil;
  LActual := nil;
  LApplication := nil;
  try
    try
      LExpected := CreateNyxCollectionFixture;
      LActual := nyx.fixture.collections.BuildNyxDocument;

      if TNyxCodec.Encode(LActual) <> TNyxCodec.Encode(LExpected) then
      begin
        raise ENyxCollection.Create('Compiled collection builder must reproduce the complete expected design');
      end;
      LCount := 1;
      LSnapshot := LActual.Collections.Snapshot(NyxCollection('tasks/🌙'));
      LApplication := TNyxApplicationState.Create(LActual);
      LStore := LApplication.Collections.Collection(NyxCollection('tasks/🌙'));
      LStore.Update(NyxCollectionItem(NyxItem(NyxCollection('tasks/🌙'), 'design/🌙'))
        .WithValue(NyxBooleanField('complete'), True));

      if not LStore.Snapshot.ItemAt(0).GetValue(NyxBooleanField('complete')) or
        LSnapshot.ItemAt(0).GetValue(NyxBooleanField('complete')) then
      begin
        raise ENyxCollection.Create('Compiled defaults and runtime stores must be independent');
      end;
      Inc(LCount);
      FreeAndNil(LApplication);
      FreeAndNil(LActual);

      if (LStore.Snapshot.Count <> 2) or (LSnapshot.Revision <> 0) then
      begin
        raise ENyxCollection.Create('Retained compiled store/snapshot must outlive their owners');
      end;
      Inc(LCount);
      {$IFDEF PAS2JS}
      document.body.textContent := 'PASS ' + IntToStr(LCount) + ' compiled collection checks';
      document.body.setAttribute('data-collection-generated', 'passed');
      {$ELSE}
      WriteLn('PASS ', LCount, ' compiled collection checks');
      {$ENDIF}
    except
      on LException: Exception do
      begin
        {$IFDEF PAS2JS}
        document.body.textContent := 'FAIL ' + LException.Message;
        document.body.setAttribute('data-collection-generated', 'failed');
        {$ELSE}
        WriteLn(LException.ClassName + ': ', LException.Message);
        Halt(1);
        {$ENDIF}
      end;
    end;
  finally
    LApplication.Free;
    LActual.Free;
    LExpected.Free;
  end;
end.
