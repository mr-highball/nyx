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

unit nyx.studio.projectopening.shared;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.source.preparation, nyx.studio.workspaces,
  nyx.studio.session, nyx.studio.sourcecompilation,
  nyx.studio.sourcecompilation.shared, nyx.studio.sourcepublications;

type
  { Optional host strategy for an explicit editor Open. Each idle compiler owns
    copied authority/workspace/revision and the sealed imported pair/resolution.
    Its operation executes those inputs before private whole-pair publication.
    The request carries no mutable session, node, renderer or execution proof. }
  INyxSharedProjectOpeningCompilerFactory = interface(INyxSharedSourceCompilerFactory)
    ['{6C080403-81B5-4E91-B222-10102610001C}']
    function CreateOpeningCompiler(const ACapability, AIssuer: TNyxText;
      const AWorkspace: TNyxWorkspaceRef; ARevision: Integer;
      const ARequest: TNyxStudioProjectRequest): INyxSharedSourceCompiler;
  end;

  { Revocable UI-only bridge capability. Availability is independent of installed
    application compilers. Start reserves the exact acknowledged live baseline;
    completion admits the independent imported pair and waits for observing
    acknowledgement. Detached hosts refuse without borrowing a retired editor. }
  INyxSharedProjectOpeningSourceHost = interface(INyxSharedSourceHost)
    ['{6C080403-81B5-4E91-B222-10102610001D}']
    function OpeningAvailable: Boolean;
    function StartOpening(const ARequest: TNyxStudioProjectRequest;
      const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
    function AdmitOpening(const ARequest: TNyxStudioProjectRequest;
      const APrepared: INyxPreparedSource;
      const AReceipt: TNyxSourcePublicationReceipt): TNyxSourceCompletion;
  end;

{ Older shared providers keep their existing Apply/visual behavior. Missing
  whole-project publication never silently becomes a local compile-only Open. }
function NyxSharedProjectOpeningAvailable(const AHost: INyxSharedSourceHost): Boolean;
function StartNyxSharedProjectOpening(const AHost: INyxSharedSourceHost;
  const ARequest: TNyxStudioProjectRequest;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
function AdmitNyxSharedProjectOpening(const AHost: INyxSharedSourceHost;
  const ARequest: TNyxStudioProjectRequest; const APrepared: INyxPreparedSource;
  const AReceipt: TNyxSourcePublicationReceipt): TNyxSourceCompletion;

implementation

uses SysUtils, nyx.model;

function NyxSharedProjectOpeningAvailable(const AHost: INyxSharedSourceHost): Boolean;
var
  LHost: INyxSharedProjectOpeningSourceHost;
begin
  Result := Supports(AHost, INyxSharedProjectOpeningSourceHost, LHost);

  if Result then
  begin
    Result := LHost.OpeningAvailable;
  end;
end;

function StartNyxSharedProjectOpening(const AHost: INyxSharedSourceHost;
  const ARequest: TNyxStudioProjectRequest;
  const APort: INyxSharedSourceCompilationPort): INyxSourceCompilation;
var
  LHost: INyxSharedProjectOpeningSourceHost;
begin

  if not Supports(AHost, INyxSharedProjectOpeningSourceHost, LHost) or
    not LHost.OpeningAvailable then
  begin
    raise ENyxModel.Create('This shared compiler host does not support project Open');
  end;
  Result := LHost.StartOpening(ARequest, APort);
end;

function AdmitNyxSharedProjectOpening(const AHost: INyxSharedSourceHost;
  const ARequest: TNyxStudioProjectRequest; const APrepared: INyxPreparedSource;
  const AReceipt: TNyxSourcePublicationReceipt): TNyxSourceCompletion;
var
  LHost: INyxSharedProjectOpeningSourceHost;
begin

  if not Supports(AHost, INyxSharedProjectOpeningSourceHost, LHost) then
  begin
    raise ENyxModel.Create('Shared project Open completion lost its specialized host');
  end;
  Result := LHost.AdmitOpening(ARequest, APrepared, AReceipt);
end;

end.
