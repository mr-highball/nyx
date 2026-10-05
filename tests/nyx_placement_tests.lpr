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

program nyx_placement_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses
  SysUtils, {$ifdef PAS2JS}Web,{$else}Classes,{$endif}
  nyx.text, nyx.studio.projects, nyx.test.placement;
var
  LPair: TNyxProjectPair;
  LChecks: Integer;
  {$ifndef PAS2JS}
  LStream: TFileStream;
  LDirectory: TNyxText;
  procedure WriteBytes(const AName, AText: TNyxText);
  begin
    LStream := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if AText <> '' then
      begin
        LStream.WriteBuffer(AText[1], Length(AText));
      end;
    finally
      LStream.Free;
    end;
  end;
  {$endif}
begin
  try
    LChecks := RunNyxPlacementJourney(LPair);
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(LChecks) + ' semantic placement checks';
    document.body.setAttribute('data-nyx-placement', 'passed');
    {$else}

    if ParamCount = 1 then
    begin
      LDirectory := IncludeTrailingPathDelimiter(ParamStr(1));
      ForceDirectories(LDirectory);
      WriteBytes('nyx.generated.view.pas', LPair.Source);
      WriteBytes('design.nyx', LPair.Design);
      WriteBytes('project.nyxpair', EncodeNyxProject(LPair));
    end;
    WriteLn('PASS ', LChecks, ' semantic placement checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := E.Message;
      document.body.setAttribute('data-nyx-placement', 'failed');
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
