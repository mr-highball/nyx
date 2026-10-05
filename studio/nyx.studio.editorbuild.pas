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

unit nyx.studio.editorbuild;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.studio.builds;

type
  { Distinct open identities prevent accidental use of a document root as an
    output profile, operation receipt or compiler job. Empty records mean unset.
    Factories validate text; the service still rechecks identity and currentness. }
  TNyxBuildOutputRef = record
  private
    FID: TNyxText;
  public
    property ID: TNyxText read FID;
  end;

  TNyxBuildRootRef = record
  private
    FID: TNyxText;
  public
    property ID: TNyxText read FID;
  end;

  TNyxBuildOperationRef = record
  private
    FID: TNyxText;
  public
    property ID: TNyxText read FID;
  end;

  TNyxBuildJobRef = record
  private
    FID: TNyxText;
  public
    property ID: TNyxText read FID;
  end;

  { Managed fluent authoring, independent of DOM, LCL, compiler paths and source
    overrides. Arguments returns detached wire data; it does not submit a job.
    A request captures the service's exact accepted pair at its revision. }
  INyxCompilerRequest = interface
    ['{FE7EBC67-B962-49D6-95B6-6351939A900D}']
    function Target(AValue: TNyxBuildTarget): INyxCompilerRequest;
    function Scope(AValue: TNyxBuildScope): INyxCompilerRequest;
    function Root(const AValue: TNyxBuildRootRef): INyxCompilerRequest;
    function AtRevision(AValue: Integer): INyxCompilerRequest;
    function Output(const AValue: TNyxBuildOutputRef): INyxCompilerRequest;
    function Operation(const AValue: TNyxBuildOperationRef): INyxCompilerRequest;
    function Arguments: TNyxDataValue;
  end;

function NyxBuildOutput(const AID: TNyxText): TNyxBuildOutputRef;
function NyxBuildRoot(const AID: TNyxText): TNyxBuildRootRef;
function NyxBuildOperation(const AID: TNyxText): TNyxBuildOperationRef;
function NyxBuildJob(const AID: TNyxText): TNyxBuildJobRef;
function NewNyxCompilerRequest: INyxCompilerRequest;
{ Closed read/status packets at the explicit serialization boundary. Status is
  bounded at twenty diagnostics, starting at an exact zero-based offset. }
function NyxCompilerOutputs: TNyxDataValue;
function NyxCompilerStatus(const AJob: TNyxBuildJobRef; AOffset: Integer = 0): TNyxDataValue;

implementation

uses
  SysUtils, nyx.types, nyx.model, nyx.editing;

type
  TCompilerRequest = class(TInterfacedObject, INyxCompilerRequest)
  private
    FTarget: TNyxBuildTarget;
    FScope: TNyxBuildScope;
    FRoot: TNyxBuildRootRef;
    FRevision: Integer;
    FOutput: TNyxBuildOutputRef;
    FOperation: TNyxBuildOperationRef;
  public
    function Target(AValue: TNyxBuildTarget): INyxCompilerRequest;
    function Scope(AValue: TNyxBuildScope): INyxCompilerRequest;
    function Root(const AValue: TNyxBuildRootRef): INyxCompilerRequest;
    function AtRevision(AValue: Integer): INyxCompilerRequest;
    function Output(const AValue: TNyxBuildOutputRef): INyxCompilerRequest;
    function Operation(const AValue: TNyxBuildOperationRef): INyxCompilerRequest;
    function Arguments: TNyxDataValue;
  end;

procedure ValidateIdentity(const AID: TNyxText);
begin

  if (NyxTextScalarCount(AID) < 1) or (NyxTextScalarCount(AID) > 120) or
    (Pos(#0, AID) > 0) or (Pos(#10, AID) > 0) or (Pos(#13, AID) > 0) then
  begin
    raise Exception.Create('Compiler identity requires 1..120 Unicode scalars without control separators');
  end;
end;

function NyxBuildOutput(const AID: TNyxText): TNyxBuildOutputRef;
var
  LIndex: Integer;
begin

  if Length(AID) <> 32 then
  begin
    raise Exception.Create('Inspect compiler outputs for their exact profile identity');
  end;
  for LIndex := 1 to Length(AID) do
  begin

    if not (AID[LIndex] in ['0'..'9', 'a'..'f']) then
    begin
      raise Exception.Create('Compiler output identity must be a lowercase hexadecimal fingerprint');
    end;
  end;
  Result.FID := AID;
end;

function NyxBuildRoot(const AID: TNyxText): TNyxBuildRootRef;
var
  LIdentity: TNyxNode;
begin
  { Use the authored root domain's exact 128-scalar admission, rather than the
    shorter receipt domain. This temporary descriptor owns no descendants. }
  LIdentity := TNyxNode.Create(nkPage, AID);
  LIdentity.Free;
  Result.FID := AID;
end;

function NyxBuildOperation(const AID: TNyxText): TNyxBuildOperationRef;
begin
  ValidateIdentity(AID);
  Result.FID := AID;
end;

function NyxBuildJob(const AID: TNyxText): TNyxBuildJobRef;
begin
  ValidateIdentity(AID);
  Result.FID := AID;
end;

function NewNyxCompilerRequest: INyxCompilerRequest;
begin
  Result := TCompilerRequest.Create;
end;

function TCompilerRequest.Target(AValue: TNyxBuildTarget): INyxCompilerRequest;
begin
  FTarget := AValue;
  Result := Self;
end;

function TCompilerRequest.Scope(AValue: TNyxBuildScope): INyxCompilerRequest;
begin
  FScope := AValue;

  if AValue = bsApplication then
  begin
    FRoot := Default(TNyxBuildRootRef);
  end;
  Result := Self;
end;

function TCompilerRequest.Root(const AValue: TNyxBuildRootRef): INyxCompilerRequest;
begin

  if FScope = bsApplication then
  begin
    raise Exception.Create('Application compilation omits a view root');
  end;
  FRoot := AValue;
  Result := Self;
end;

function TCompilerRequest.AtRevision(AValue: Integer): INyxCompilerRequest;
begin

  if AValue < 1 then
  begin
    raise Exception.Create('Compiler request requires an exact positive revision');
  end;
  FRevision := AValue;
  Result := Self;
end;

function TCompilerRequest.Output(const AValue: TNyxBuildOutputRef): INyxCompilerRequest;
begin
  FOutput := AValue;
  Result := Self;
end;

function TCompilerRequest.Operation(const AValue: TNyxBuildOperationRef): INyxCompilerRequest;
begin
  FOperation := AValue;
  Result := Self;
end;

function TCompilerRequest.Arguments: TNyxDataValue;
var
  LFields: array of TNyxDataField;
begin

  if (FRevision < 1) or (FOutput.ID = '') or (FOperation.ID = '') or
    ((FScope <> bsApplication) and (FRoot.ID = '')) then
  begin
    raise Exception.Create('Compiler request requires revision, output, operation and its scoped root');
  end;
  SetLength(LFields, 6);
  LFields[0] := NyxField('mode', NyxData('request'));
  LFields[1] := NyxField('expectedRevision', NyxData(FRevision));
  LFields[2] := NyxField('outputID', NyxData(FOutput.ID));
  LFields[3] := NyxField('operationId', NyxData(FOperation.ID));
  LFields[4] := NyxField('target', NyxData(NyxBuildTargetName(FTarget)));
  LFields[5] := NyxField('scope', NyxData(NyxBuildScopeName(FScope)));

  if FScope <> bsApplication then
  begin
    SetLength(LFields, 7);
    LFields[6] := NyxField('view', NyxData(FRoot.ID));
  end;
  Result := NyxObject(LFields);
end;

function NyxCompilerOutputs: TNyxDataValue;
begin
  Result := NyxObject([NyxField('mode', NyxData('outputs'))]);
end;

function NyxCompilerStatus(const AJob: TNyxBuildJobRef; AOffset: Integer): TNyxDataValue;
begin

  if (AJob.ID = '') or (AOffset < 0) then
  begin
    raise Exception.Create('Compiler status requires its job and a nonnegative offset');
  end;
  Result := NyxObject([NyxField('mode', NyxData('status')),
    NyxField('job', NyxData(AJob.ID)), NyxField('offset', NyxData(AOffset)),
    NyxField('limit', NyxData(20))]);
end;

end.
