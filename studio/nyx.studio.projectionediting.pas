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


unit nyx.studio.projectionediting;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.studio.sourceprojection, nyx.source.preparation, nyx.schema;

{ Prepare an actually executed result inside the immutable dispatched creator
  environment. It borrows no session/history/renderer. The returned one-shot
  pair enters the existing CompleteSourceRequest baseline/schema guard.
  Failed/compiled-only results expose diagnostics and no owners. This explicit
  trusted execution bridge cannot be serialized as a literal-worker reply, file
  import or MCP project payload; those retain their ordinary admission rules. }
function PrepareNyxProjectedSource(const AProjection: INyxSourceProjection;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedSource;

implementation

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.codec, nyx.source;

type
  TProjectedSource = class(TInterfacedObject, INyxPreparedSource, INyxSchemaAction)
  private
    FProjection: INyxSourceProjection;
    FSource: TNyxText;
    FDesign: TNyxText;
    FDiagnostic: TNyxSourceDiagnostic;
    FSchemaRevision: Integer;
    FDocument: TNyxDocument;
    FWorkspace: TNyxSourceWorkspace;
  public
    destructor Destroy; override;
    procedure Execute;
    function GetSource: TNyxText;
    function GetDesign: TNyxText;
    function GetDiagnostic: TNyxSourceDiagnostic;
    function GetSchemaRevision: Integer;
    function ToData: TNyxDataValue;
    procedure Take(var ADocument: TNyxDocument; var AWorkspace: TNyxSourceWorkspace);
  end;

destructor TProjectedSource.Destroy;
begin
  FWorkspace.Free;
  FDocument.Free;
  inherited Destroy;
end;

procedure TProjectedSource.Execute;
begin
  FDocument := FProjection.CopyDocument;
  ValidateNyxDocumentProperties(FDocument);
  FDesign := TNyxCodec.Encode(FDocument);

  if FDesign <> FProjection.Design then
  begin
    raise ENyxModel.Create('Executed design changed during detached source preparation');
  end;
  FWorkspace := TNyxSourceWorkspace.Create;
  FWorkspace.AcceptExecuted(FDocument, FSource);
end;

function TProjectedSource.GetSource: TNyxText;
begin
  Result := FSource;
end;

function TProjectedSource.GetDesign: TNyxText;
begin
  Result := FDesign;
end;

function TProjectedSource.GetDiagnostic: TNyxSourceDiagnostic;
begin
  Result := FDiagnostic;
end;

function TProjectedSource.GetSchemaRevision: Integer;
begin
  Result := FSchemaRevision;
end;

function TProjectedSource.ToData: TNyxDataValue;
begin
  Result := Default(TNyxDataValue);
  { No Boolean metadata flag is proof of actual execution. Browser results must
    enter through the separately owned projection channel, with its expected
    source/reference/target, before this immutable local adapter is prepared. }
  raise ENyxModel.Create('Executed source preparation has no literal-worker transport');
end;

procedure TProjectedSource.Take(var ADocument: TNyxDocument;
  var AWorkspace: TNyxSourceWorkspace);
begin

  if (ADocument <> nil) or (AWorkspace <> nil) then
  begin
    raise ENyxModel.Create('Projected source transfer requires empty destinations');
  end;

  if FDiagnostic.Defined or (FDocument = nil) or (FWorkspace = nil) then
  begin
    raise ENyxModel.Create('Projected source failed or was already transferred');
  end;
  ADocument := FDocument;
  AWorkspace := FWorkspace;
  FDocument := nil;
  FWorkspace := nil;
end;

function PrepareNyxProjectedSource(const AProjection: INyxSourceProjection;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedSource;
var
  LOwner: TProjectedSource;
  LAction: INyxSchemaAction;
begin

  if (AProjection = nil) or (ASchemas = nil) then
  begin
    raise ENyxModel.Create('Projected source needs an execution result and captured creators');
  end;
  LOwner := TProjectedSource.Create;
  Result := LOwner;
  LOwner.FSource := AProjection.Source;
  LOwner.FSchemaRevision := ASchemas.Revision;

  if AProjection.State <> spsExecuted then
  begin
    LOwner.FDiagnostic.Defined := True;
    LOwner.FDiagnostic.Message := AProjection.Message;

    if LOwner.FDiagnostic.Message = '' then
    begin
      LOwner.FDiagnostic.Message := 'Source has no executed admitted design';
    end;
    Exit;
  end;
  LOwner.FProjection := AProjection;
  LAction := LOwner;
  try
    ASchemas.Execute(LAction);
  except
    on LException: Exception do
    begin
      LOwner.FDiagnostic.Defined := True;
      LOwner.FDiagnostic.Message := LException.Message;

      if LException is ENyxSource then
      begin
        LOwner.FDiagnostic.Message := ENyxSource(LException).DiagnosticText;
        LOwner.FDiagnostic.Line := ENyxSource(LException).Line;
        LOwner.FDiagnostic.Column := ENyxSource(LException).Column;
      end;
      FreeAndNil(LOwner.FWorkspace);
      FreeAndNil(LOwner.FDocument);
      LOwner.FDesign := '';
    end;
  end;
  { Retain admitted values/owners, not the producer receipt or compiler report. }
  LOwner.FProjection := nil;
end;

end.
