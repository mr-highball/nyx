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


unit nyx.source.preparation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.model, nyx.source, nyx.schema;

type
  { A processor owns both staged resources until Take transfers them together.
    Immutable text/diagnostics survive transfer. Failure exposes no resources.
    The result borrows neither the accepted editor nor its history/renderers.
    Take is a one-time ownership operation, serialized by the receiving host. }
  INyxPreparedSource = interface(IInterface)
    ['{8B6CF439-AE07-4C41-A6D8-79C501B85403}']
    function GetSource: TNyxText;
    function GetDesign: TNyxText;
    function GetDiagnostic: TNyxSourceDiagnostic;
    function GetSchemaRevision: Integer;
    function ToData: TNyxDataValue;
    procedure Take(var ADocument: TNyxDocument; var AWorkspace: TNyxSourceWorkspace);
    property Source: TNyxText read GetSource;
    property Design: TNyxText read GetDesign;
    property Diagnostic: TNyxSourceDiagnostic read GetDiagnostic;
    property SchemaRevision: Integer read GetSchemaRevision;
  end;

{ Complete strict source admission inside an explicit immutable creator context.
  The processor owns every reconstructed node/default/recipe and retains exact
  source. User admission failures become owned diagnostics; no accepted project
  is borrowed or published. Native workers may call this on independently owned
  input. A host must still guard document/source/schema currentness and transfer
  both resources as one undoable publication. This is not compiler execution. }
function PrepareNyxSource(const ASource: TNyxText;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedSource;

{ Receive a result from the bundled Pascal source worker on its private channel.
  The host supplies the exact dispatched source/environment, and still performs
  the session's fresh baseline guard. Validates the wire, owned design, creator
  rules and exact paired frame; does not repeat Pascal admission on the UI loop.
  This trusted processor handoff is NOT admission for files, HTTP project imports
  or MCP mutations. Those entry points continue to reconstruct their source. }
function ReceiveNyxPreparedSource(const AData: TNyxDataValue;
  const ASource: TNyxText; const ASchemas: INyxSchemaSnapshot): INyxPreparedSource;

implementation

uses
  SysUtils, nyx.codec;

type
  TNyxPreparedSource = class(TInterfacedObject, INyxPreparedSource, INyxSchemaAction)
  private
    FSource: TNyxText;
    FDesign: TNyxText;
    FSnapshot: TNyxText;
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

destructor TNyxPreparedSource.Destroy;
begin
  FWorkspace.Free;
  FDocument.Free;
  inherited Destroy;
end;

procedure TNyxPreparedSource.Execute;
begin
  FDocument := TNyxSourceWorkspace.PrepareDraft(FSource, FWorkspace);
  FDesign := FWorkspace.Capture.Design;
  FSnapshot := FWorkspace.Snapshot;
end;

function TNyxPreparedSource.GetSource: TNyxText;
begin
  Result := FSource;
end;

function TNyxPreparedSource.GetDesign: TNyxText;
begin
  Result := FDesign;
end;

function TNyxPreparedSource.GetDiagnostic: TNyxSourceDiagnostic;
begin
  Result := FDiagnostic;
end;

function TNyxPreparedSource.GetSchemaRevision: Integer;
begin
  Result := FSchemaRevision;
end;

function TNyxPreparedSource.ToData: TNyxDataValue;
begin
  Result := NyxObject([
    NyxField('version', NyxData(1)), NyxField('source', NyxData(FSource)),
    NyxField('design', NyxData(FDesign)), NyxField('workspace', NyxData(FSnapshot)),
    NyxField('schemaRevision', NyxData(FSchemaRevision)),
    NyxField('diagnostic', NyxObject([
      NyxField('defined', NyxData(FDiagnostic.Defined)),
      NyxField('message', NyxData(FDiagnostic.Message)),
      NyxField('line', NyxData(FDiagnostic.Line)),
      NyxField('column', NyxData(FDiagnostic.Column))]))]);
end;

procedure TNyxPreparedSource.Take(var ADocument: TNyxDocument;
  var AWorkspace: TNyxSourceWorkspace);
begin

  if (ADocument <> nil) or (AWorkspace <> nil) then
  begin
    raise ENyxModel.Create('Prepared source transfer requires empty destinations');
  end;

  if FDiagnostic.Defined or (FDocument = nil) or (FWorkspace = nil) then
  begin
    raise ENyxModel.Create('Prepared source has failed or its owners were already transferred');
  end;
  ADocument := FDocument;
  AWorkspace := FWorkspace;
  FDocument := nil;
  FWorkspace := nil;
end;

function PrepareNyxSource(const ASource: TNyxText;
  const ASchemas: INyxSchemaSnapshot): INyxPreparedSource;
var
  LOwner: TNyxPreparedSource;
  LAction: INyxSchemaAction;
begin

  if ASchemas = nil then
  begin
    raise ENyxModel.Create('Isolated source preparation requires a captured creator environment');
  end;
  LOwner := TNyxPreparedSource.Create;
  Result := LOwner;
  LOwner.FSource := ASource;
  LOwner.FSchemaRevision := ASchemas.Revision;
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
      LOwner.FSnapshot := '';
    end;
  end;
end;

function DecodePreparedSource(const AData: TNyxDataValue;
  const ASource: TNyxText; ASchemaRevision: Integer): INyxPreparedSource;
var
  LOwner: TNyxPreparedSource;
  LDiagnostic: TNyxDataValue;
  LWorkspace: TNyxDataValue;
  LParts: TNyxStrings;
  LFrame: TNyxText;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 6) or
    (AData.Field('version').AsInteger <> 1) or
    (AData.Field('source').AsText <> ASource) or
    (AData.Field('schemaRevision').AsInteger <> ASchemaRevision) then
  begin
    raise ENyxModel.Create('Source worker reply does not match the dispatched command');
  end;
  LOwner := TNyxPreparedSource.Create;
  Result := LOwner;
  LOwner.FSource := ASource;
  LOwner.FSchemaRevision := ASchemaRevision;
  LOwner.FDesign := AData.Field('design').AsText;
  LOwner.FSnapshot := AData.Field('workspace').AsText;
  LDiagnostic := AData.Field('diagnostic');

  if (LDiagnostic.Kind <> ndObject) or (LDiagnostic.Count <> 4) then
  begin
    raise ENyxModel.Create('Source worker diagnostic requires four exact fields');
  end;
  LOwner.FDiagnostic.Defined := LDiagnostic.Field('defined').AsBoolean;
  LOwner.FDiagnostic.Message := LDiagnostic.Field('message').AsText;
  LOwner.FDiagnostic.Line := LDiagnostic.Field('line').AsInteger;
  LOwner.FDiagnostic.Column := LDiagnostic.Field('column').AsInteger;

  if (LOwner.FDiagnostic.Line < 0) or (LOwner.FDiagnostic.Column < 0) or
    ((LOwner.FDiagnostic.Line = 0) <> (LOwner.FDiagnostic.Column = 0)) then
  begin
    raise ENyxModel.Create('Source worker diagnostic has invalid coordinates');
  end;

  if LOwner.FDiagnostic.Defined then
  begin

    if (LOwner.FDiagnostic.Message = '') or (LOwner.FDesign <> '') or
      (LOwner.FSnapshot <> '') then
    begin
      raise ENyxModel.Create('Failed source worker reply contains partial owners');
    end;
    Exit;
  end;

  if (LOwner.FDiagnostic.Message <> '') or (LOwner.FDiagnostic.Line <> 0) then
  begin
    raise ENyxModel.Create('Successful source worker reply contains an error');
  end;
  LWorkspace := TNyxDataValue.ParseJSON(LOwner.FSnapshot);
  LParts := TNyxStrings.Create;
  try
    LParts.Add(LWorkspace.Field('prefix').AsText);
    LParts.Add(LWorkspace.Field('body').AsText);
    LParts.Add(LWorkspace.Field('suffix').AsText);
    LFrame := LParts.Join;
  finally
    LParts.Free;
  end;

  if (LWorkspace.Kind <> ndObject) or (LWorkspace.Count <> 5) or
    (LWorkspace.Field('design').AsText <> LOwner.FDesign) or
    (LFrame <> ASource) then
  begin
    raise ENyxModel.Create('Source worker reply contains an inconsistent paired frame');
  end;
  LOwner.FDocument := TNyxCodec.Decode(LOwner.FDesign);
  ValidateNyxDocumentProperties(LOwner.FDocument);
  LOwner.FWorkspace := TNyxSourceWorkspace.Create;
  LOwner.FWorkspace.Restore(LOwner.FSnapshot);
end;

type
  TSourceReceiveAction = class(TInterfacedObject, INyxSchemaAction)
  public
    Data: TNyxDataValue;
    Source: TNyxText;
    Revision: Integer;
    Prepared: INyxPreparedSource;
    procedure Execute;
  end;

procedure TSourceReceiveAction.Execute;
begin
  Prepared := DecodePreparedSource(Data, Source, Revision);
end;

function ReceiveNyxPreparedSource(const AData: TNyxDataValue;
  const ASource: TNyxText; const ASchemas: INyxSchemaSnapshot): INyxPreparedSource;
var
  LOwner: TSourceReceiveAction;
  LAction: INyxSchemaAction;
begin

  if ASchemas = nil then
  begin
    raise ENyxModel.Create('Source reply requires its dispatched creator environment');
  end;
  LOwner := TSourceReceiveAction.Create;
  LAction := LOwner;
  LOwner.Data := AData;
  LOwner.Source := ASource;
  LOwner.Revision := ASchemas.Revision;
  ASchemas.Execute(LAction);
  Result := LOwner.Prepared;
end;

end.
