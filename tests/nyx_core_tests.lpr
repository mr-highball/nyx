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

program nyx_core_tests;

{$mode delphi}{$H+}
{$codepage utf8}

uses
  nyx.text,
  SysUtils,
  Classes,
  nyx.model,
  nyx.codegen,
  nyx.codec,
  nyx.sample,
  nyx.test.core,
  nyx.test.source,
  nyx.test.controls,
  nyx.test.callbacks,
  nyx.test.source.managed,
  nyx.test.source.structural,
  nyx.test.collections.registry,
  nyx.test.studio;

var
  LDocument: TNyxDocument;
  LEditedSource: TNyxText;
  LChecks: Integer;
procedure SaveUTF8(const APath, AText: TNyxText);
var
  LStream: TFileStream;
begin
  { File streams preserve generated UTF-8 bytes without passing through an ANSI
    TStringList.Text setter before the compiler reads the source again. }
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if AText <> '' then
    begin
      LStream.WriteBuffer(AText[1], Length(AText));
    end;
  finally
    LStream.Free;
  end;
end;

begin
  try
    LChecks := RunNyxCoreTests;
    WriteLn('PASS ', LChecks, ' core checks');
    LChecks := RunNyxStudioTests;
    WriteLn('PASS ', LChecks, ' composition/designer checks');

    if ParamCount > 0 then
    begin
      LDocument := CreateNyxPersistenceFixture;
      try
        SaveUTF8(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.generated.view.pas',
          TNyxCodegen.Generate(LDocument));
        SaveUTF8(IncludeTrailingPathDelimiter(ParamStr(1)) + 'sample.nyx',
          TNyxCodec.Encode(LDocument));
      finally
        LDocument.Free;
      end;
      LDocument := CreateNyxEditedFixture(LEditedSource);
      try
        SaveUTF8(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.edited.view.pas',
          LEditedSource);
      finally
        LDocument.Free;
      end;
      LDocument := CreateNyxLegacyControlFixture(LEditedSource);
      try
        SaveUTF8(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.legacy.controls.pas', LEditedSource);
      finally
        LDocument.Free;
      end;
      LDocument := CreateNyxCallbackFixture(LEditedSource);
      try
        SaveUTF8(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.callback.fixture.pas', LEditedSource);
      finally
        LDocument.Free;
      end;
      LDocument := CreateNyxManagedSourceFixture(LEditedSource);
      try
        SaveUTF8(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.managed.view.pas', LEditedSource);
      finally
        LDocument.Free;
      end;
      LDocument := CreateNyxStructuralSourceFixture(LEditedSource);
      try
        SaveUTF8(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.structural.view.pas', LEditedSource);
      finally
        LDocument.Free;
      end;
      LDocument := CreateNyxCollectionFixture;
      try
        SaveUTF8(IncludeTrailingPathDelimiter(ParamStr(1)) + 'nyx.fixture.collections.pas',
          TNyxCodegen.Generate(LDocument, 'nyx.fixture.collections'));
        SaveUTF8(IncludeTrailingPathDelimiter(ParamStr(1)) + 'collections.nyx',
          TNyxCodec.Encode(LDocument));
      finally
        LDocument.Free;
      end;
    end;
  except
    on LException: Exception do
    begin
      WriteLn(LException.ClassName + ': ' + LException.Message);
      DumpExceptionBackTrace(Output);
      Halt(1);
    end;
  end;
end.
