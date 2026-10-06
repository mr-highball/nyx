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
unit nyx.studio.builds;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model;

type
  { Closed compiler choices. A reusable build requires an exact definition
    root; a view requires an exact page root. Application builds omit a root.
    Machine profile paths and arguments never belong to this portable contract. }
  TNyxBuildTarget = (btBrowser, btNativeLCL);
  TNyxBuildScope = (bsView, bsReusable, bsApplication);
  { A cancelling job still owns its execution slot. Terminal means the owned
    process AND worker have been joined, not merely asked to stop. }
  TNyxBuildJobState = (bjsQueued, bjsRunning, bjsCancelling, bjsSucceeded,
    bjsFailed, bjsCancelled);
  { Execution failures are distinct from source diagnostics. Cancellation is
    represented by lifecycle state; it is not a compiler rejection. }
  TNyxCompilerFailure = (bcfNone, bcfCompiler, bcfTimeBudget, bcfLogBudget);

function NyxCompilerFailureName(AValue: TNyxCompilerFailure): TNyxText;
function ParseNyxCompilerFailure(const AValue: TNyxText): TNyxCompilerFailure;
function NyxBuildJobStateName(AValue: TNyxBuildJobState): TNyxText;
function ParseNyxBuildJobState(const AValue: TNyxText): TNyxBuildJobState;
function NyxBuildJobTerminal(AValue: TNyxBuildJobState): Boolean;
function NyxBuildTargetName(AValue: TNyxBuildTarget): TNyxText;
function NyxBuildScopeName(AValue: TNyxBuildScope): TNyxText;
function ParseNyxBuildTarget(const AValue: TNyxText): TNyxBuildTarget;
function ParseNyxBuildScope(const AValue: TNyxText): TNyxBuildScope;

{ Versioned compiler request pairs an admitted portable design with its accepted
  companion. Compiler paths/options belong to the service profile, never this
  message. The caller borrows the design on Encode; Decode transfers a newly
  owned document only after the complete envelope is admitted. Failure returns
  nil/empty outputs. Source is exact UTF-8/Unicode text, including comments. }
function EncodeNyxBuildRequest(ADocument: TNyxDocument;
  const ASource: TNyxText): TNyxText;
procedure DecodeNyxBuildRequest(const AMessage: TNyxText;
  out ADocument: TNyxDocument; out ASource: TNyxText);

implementation

uses
  nyx.data,
  nyx.codec;

function NyxCompilerFailureName(AValue: TNyxCompilerFailure): TNyxText;
const
  CNames: array[TNyxCompilerFailure] of TNyxText = ('none', 'compiler',
    'time-budget', 'log-budget');
begin
  Result := CNames[AValue];
end;

function ParseNyxCompilerFailure(const AValue: TNyxText): TNyxCompilerFailure;
var
  LValue: TNyxCompilerFailure;
begin
  for LValue := Low(TNyxCompilerFailure) to High(TNyxCompilerFailure) do
  begin

    if AValue = NyxCompilerFailureName(LValue) then
    begin
      Exit(LValue);
    end;
  end;
  raise ENyxModel.Create('Unknown compiler execution failure');
end;

function NyxBuildJobStateName(AValue: TNyxBuildJobState): TNyxText;
const
  CNames: array[TNyxBuildJobState] of TNyxText = ('queued', 'running',
    'cancelling', 'succeeded', 'failed', 'cancelled');
begin
  Result := CNames[AValue];
end;

function ParseNyxBuildJobState(const AValue: TNyxText): TNyxBuildJobState;
var
  LValue: TNyxBuildJobState;
begin
  for LValue := Low(TNyxBuildJobState) to High(TNyxBuildJobState) do
  begin

    if AValue = NyxBuildJobStateName(LValue) then
    begin
      Exit(LValue);
    end;
  end;
  raise ENyxModel.Create('Unknown compiler job state');
end;

function NyxBuildJobTerminal(AValue: TNyxBuildJobState): Boolean;
begin
  Result := AValue in [bjsSucceeded, bjsFailed, bjsCancelled];
end;

function NyxBuildTargetName(AValue: TNyxBuildTarget): TNyxText;
const
  CNames: array[TNyxBuildTarget] of TNyxText = ('browser', 'lcl');
begin
  Result := CNames[AValue];
end;

function NyxBuildScopeName(AValue: TNyxBuildScope): TNyxText;
const
  CNames: array[TNyxBuildScope] of TNyxText = ('view', 'reusable', 'application');
begin
  Result := CNames[AValue];
end;

function ParseNyxBuildTarget(const AValue: TNyxText): TNyxBuildTarget;
var
  LValue: TNyxBuildTarget;
begin
  for LValue := Low(TNyxBuildTarget) to High(TNyxBuildTarget) do
  begin

    if AValue = NyxBuildTargetName(LValue) then
    begin
      Exit(LValue);
    end;
  end;
  raise ENyxModel.Create('Build target must be browser or lcl');
end;

function ParseNyxBuildScope(const AValue: TNyxText): TNyxBuildScope;
var
  LValue: TNyxBuildScope;
begin
  for LValue := Low(TNyxBuildScope) to High(TNyxBuildScope) do
  begin

    if AValue = NyxBuildScopeName(LValue) then
    begin
      Exit(LValue);
    end;
  end;
  raise ENyxModel.Create('Build scope must be view, reusable or application');
end;

function EncodeNyxBuildRequest(ADocument: TNyxDocument;
  const ASource: TNyxText): TNyxText;
begin

  if (ADocument = nil) or (ASource = '') then
  begin
    raise ENyxModel.Create('A companion build requires its design and accepted Pascal');
  end;
  Result := NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('design', TNyxDataValue.ParseJSON(TNyxCodec.Encode(ADocument))),
    NyxField('source', NyxData(ASource))
  ]).ToJSON;
end;

procedure DecodeNyxBuildRequest(const AMessage: TNyxText;
  out ADocument: TNyxDocument; out ASource: TNyxText);
var
  LMessage: TNyxDataValue;
  LSource: TNyxText;
begin
  ADocument := nil;
  ASource := '';
  LMessage := TNyxDataValue.ParseJSON(AMessage);

  if (LMessage.Kind <> ndObject) or (LMessage.Count <> 3) then
  begin
    raise ENyxModel.Create('A companion request contains version, design and source');
  end;

  if LMessage.Field('version').AsInteger <> 1 then
  begin
    raise ENyxModel.Create('Unsupported companion request version');
  end;
  LSource := LMessage.Field('source').AsText;

  if LSource = '' then
  begin
    raise ENyxModel.Create('A companion request must retain its accepted Pascal');
  end;
  ADocument := TNyxCodec.Decode(LMessage.Field('design').ToJSON);
  ASource := LSource;
end;

end.
