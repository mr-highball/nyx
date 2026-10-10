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

unit nyx.test.source.compilation.browser;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.studio.sourcecompilation;

{ Qualification provider for the actually compiled owned HTTP fixtures. Exact
  source and artifact/receipt identities are taken from the native compiler's
  staged outputs. This does not implement an HTTP compilation service or claim
  that an arbitrary future source string has been compiled. }
function NyxFixtureBrowserCompiler(const ASource, AFailureSource: TNyxText;
  const AManifest: TNyxDataValue): INyxSourceCompiler;

implementation

uses nyx.model, nyx.studio.builds, nyx.studio.sourceprojection,
  nyx.studio.sourcecompilation.browser;

type
  TStagedOperation = class(TInterfacedObject, INyxSourceCompilation)
  public
    procedure Cancel;
    function GetState: TNyxSourceCompilationState;
  end;
  TStagedBuilder = class(TInterfacedObject, INyxBrowserSourceBuilder)
  public
    Source: TNyxText;
    FailureSource: TNyxText;
    Manifest: TNyxDataValue;
    function Compile(const ASource: TNyxText;
      const APort: INyxBrowserSourceBuildPort): INyxSourceCompilation;
  end;

procedure TStagedOperation.Cancel;
begin
  { The staged compiler invocation has already physically finished. Its browser
    execution is independently owned/cancelled by the production adapter. }
end;

function TStagedOperation.GetState: TNyxSourceCompilationState;
begin
  Result := scsCompleted;
end;

function TStagedBuilder.Compile(const ASource: TNyxText;
  const APort: INyxBrowserSourceBuildPort): INyxSourceCompilation;
var
  LIndex: Integer;
  LEntry: TNyxDataValue;
  LBuild: INyxSourceProjectionBuild;
begin
  LIndex := 0;

  if ASource = FailureSource then
  begin
    LIndex := 1;
  end
  else if ASource <> Source then
  begin
    raise ENyxModel.Create('This fixture has no compiler receipt for the requested source');
  end;
  LEntry := Manifest.Field('workers').Item(LIndex);
  LBuild := NewNyxSourceProjectionBuild(NyxSourceProjectionRef(
    LEntry.Field('ticket').AsText), NyxSourceProjectionFailure(ASource,
    btBrowser, spsCompiled, ''), LEntry.Field('artifact').AsText, '');
  Result := TStagedOperation.Create;
  APort.Compiled(LBuild);
end;

function NyxFixtureBrowserCompiler(const ASource, AFailureSource: TNyxText;
  const AManifest: TNyxDataValue): INyxSourceCompiler;
var
  LOwner: TStagedBuilder;
  LBuilder: INyxBrowserSourceBuilder;
begin
  LOwner := TStagedBuilder.Create;
  LBuilder := LOwner;
  LOwner.Source := ASource;
  LOwner.FailureSource := AFailureSource;
  LOwner.Manifest := AManifest.Copy;
  Result := NewNyxBrowserSourceCompiler(LBuilder);
end;

end.
