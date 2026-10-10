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

unit nyx.studio.sourceprojection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.model, nyx.source, nyx.studio.builds,
  nyx.studio.compiler;

const
  NyxProjectionMaximumSourceBytes = 1024 * 1024;
  NyxProjectionMaximumResultBytes = 4 * 1024 * 1024;
  NyxProjectionResultFile: TNyxText = 'projection-result.json';

type
  { Compilation is separate from execution and complete design admission.
    Compiled browser artifacts have no design until their owned worker executes. }
  TNyxSourceProjectionState = (spsCompiled, spsExecuted, spsUnavailable,
    spsCompilationFailed, spsExecutionFailed, spsInvalidDesign, spsCancelled);

  { Trusted host qualification choice. Checked native producers include range,
    overflow and heap diagnostics; this is not a client-supplied compiler option. }
  TNyxSourceProjectionChecks = (spcDefault, spcChecked);

  { Distinct opaque producer identity, copied by value. It contains no path,
    credential or editor pointer. The host binds it to its exact submitted source;
    a caller cannot use this value alone as authority to publish a project. }
  TNyxSourceProjectionRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;

  { Immutable detached result. Source is the exact submitted unit; Design is an
    admitted canonical codec snapshot only in Executed state. Report is independently
    managed compiler evidence, not proof of execution. No document/editor/renderer
    or machine profile is borrowed. CopyDocument returns a new caller-owned tree;
    every call is independent, and unavailable designs refuse before allocation. }
  INyxSourceProjection = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026000001}']
    function GetSource: TNyxText;
    function GetTarget: TNyxBuildTarget;
    function GetState: TNyxSourceProjectionState;
    function GetMessage: TNyxText;
    function GetDesign: TNyxText;
    function GetReport: INyxCompilerReport;
    function CopyDocument: TNyxDocument;
    property Source: TNyxText read GetSource;
    property Target: TNyxBuildTarget read GetTarget;
    property State: TNyxSourceProjectionState read GetState;
    property Message: TNyxText read GetMessage;
    property Design: TNyxText read GetDesign;
    property Report: INyxCompilerReport read GetReport;
  end;

  { Detached build receipt. The opaque reference identifies this invocation;
    Artifact is an owned relative worker URL only after successful browser
    compilation. That compiled result has no design until the worker executes.
    RuntimeLog records native child diagnostics separately from compiler Report.
    Retaining this receipt retains no executor, process, editor or directory. }
  INyxSourceProjectionBuild = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026000002}']
    function GetReference: TNyxSourceProjectionRef;
    function GetProjection: INyxSourceProjection;
    function GetArtifact: TNyxText;
    function GetRuntimeLog: TNyxText;
    property Reference: TNyxSourceProjectionRef read GetReference;
    property Projection: INyxSourceProjection read GetProjection;
    property Artifact: TNyxText read GetArtifact;
    property RuntimeLog: TNyxText read GetRuntimeLog;
  end;

{ ASCII identity admission and closed state encoding are persistence boundaries. }
function NyxSourceProjectionRef(const AName: TNyxText): TNyxSourceProjectionRef;
function NyxSourceProjectionStateName(AState: TNyxSourceProjectionState): TNyxText;
procedure ValidateNyxProjectionSource(const ASource: TNyxText);
procedure ValidateNyxProjectionTarget(ATarget: TNyxBuildTarget);

{ Host receipt boundary. Compiled browser results require their exact fixed
  relative worker path; every other state refuses an advertised artifact. }
function NewNyxSourceProjectionBuild(const AReference: TNyxSourceProjectionRef;
  const AProjection: INyxSourceProjection;
  const AArtifact, ARuntimeLog: TNyxText): INyxSourceProjectionBuild;

{ Host result constructor for nonexecuted states. Executed requires the producer
  receive path below, not a Boolean claim or caller-supplied design string. }
function NyxSourceProjectionFailure(const ASource: TNyxText;
  ATarget: TNyxBuildTarget; AState: TNyxSourceProjectionState;
  const AMessage: TNyxText; const AReport: INyxCompilerReport = nil): INyxSourceProjection;

{ Called by the compiler-produced wrapper after BuildNyxDocument returns.
  Document is borrowed only during complete validation/encoding. These packets
  belong to an explicitly owned execution channel; files/HTTP/MCP project input
  must not substitute them for ordinary source admission. }
function CaptureNyxSourceProjection(ADocument: TNyxDocument;
  const AReference: TNyxSourceProjectionRef; ATarget: TNyxBuildTarget): TNyxDataValue;
function NyxSourceProjectionFailurePacket(const AReference: TNyxSourceProjectionRef;
  ATarget: TNyxBuildTarget; AState: TNyxSourceProjectionState): TNyxDataValue;

{ Receive only the exact expected producer/target on its owned channel. Validate
  the complete bounded shape, decode an independent candidate, admit its properties
  and preserve its exact codec meaning before publishing an immutable result.
  Malformed/mismatched replies return InvalidDesign without a usable tree.
  This stages data; later editor admission still needs its fresh pair/context guard. }
function ReceiveNyxSourceProjection(const ASource: TNyxText;
  const AReference: TNyxSourceProjectionRef; ATarget: TNyxBuildTarget;
  const AMessage: TNyxText; const AReport: INyxCompilerReport = nil): INyxSourceProjection;

{ Generate only fixed Pascal wrapper behavior. The complete supplied unit is
  compiled separately; no strict builder interpreter runs here. Unit/ticket
  names are admitted before interpolation. Native owns a byte file; browser owns
  a worker message. Invoking source also invokes its initializers/helpers: this
  is explicit execution, not a filesystem/network sandbox or untrusted import. }
function GenerateNyxSourceProjectionProgram(const AUnit: TNyxPascalUnitRef;
  const AReference: TNyxSourceProjectionRef; ATarget: TNyxBuildTarget): TNyxText;

implementation

uses
  SysUtils, nyx.bytes, nyx.codec, nyx.codegen, nyx.schema;

type
  TSourceProjectionBuild = class(TInterfacedObject, INyxSourceProjectionBuild)
  private
    FReference: TNyxSourceProjectionRef;
    FProjection: INyxSourceProjection;
    FArtifact: TNyxText;
    FRuntimeLog: TNyxText;
  public
    function GetReference: TNyxSourceProjectionRef;
    function GetProjection: INyxSourceProjection;
    function GetArtifact: TNyxText;
    function GetRuntimeLog: TNyxText;
  end;

  TSourceProjection = class(TInterfacedObject, INyxSourceProjection)
  private
    FSource: TNyxText;
    FTarget: TNyxBuildTarget;
    FState: TNyxSourceProjectionState;
    FMessage: TNyxText;
    FDesign: TNyxText;
    FReport: INyxCompilerReport;
  public
    function GetSource: TNyxText;
    function GetTarget: TNyxBuildTarget;
    function GetState: TNyxSourceProjectionState;
    function GetMessage: TNyxText;
    function GetDesign: TNyxText;
    function GetReport: INyxCompilerReport;
    function CopyDocument: TNyxDocument;
  end;

function NyxSourceProjectionRef(const AName: TNyxText): TNyxSourceProjectionRef;
var
  LIndex: Integer;
begin

  if (Length(AName) < 1) or (Length(AName) > 96) then
  begin
    raise ENyxModel.Create('Projection identity requires 1..96 ASCII characters');
  end;
  for LIndex := 1 to Length(AName) do
  begin

    if not (AName[LIndex] in ['a'..'z', 'A'..'Z', '0'..'9', '-', '_']) then
    begin
      raise ENyxModel.Create('Projection identity is not an ASCII token');
    end;
  end;
  Result.FName := AName;
end;

function NyxSourceProjectionStateName(AState: TNyxSourceProjectionState): TNyxText;
const
  CNames: array[TNyxSourceProjectionState] of TNyxText = ('compiled', 'executed',
    'unavailable', 'compilation-failed', 'execution-failed', 'invalid-design', 'cancelled');
begin

  if (Ord(AState) < Ord(Low(TNyxSourceProjectionState))) or
    (Ord(AState) > Ord(High(TNyxSourceProjectionState))) then
  begin
    raise ENyxModel.Create('Unknown source projection state');
  end;
  Result := CNames[AState];
end;

procedure ValidateNyxProjectionSource(const ASource: TNyxText);
begin

  if (ASource = '') or (NyxUTF8ByteCount(ASource) > NyxProjectionMaximumSourceBytes) then
  begin
    raise ENyxModel.Create('Projection requires a complete source unit within 1 MiB');
  end;
end;

procedure ValidateNyxProjectionTarget(ATarget: TNyxBuildTarget);
begin

  if (Ord(ATarget) < Ord(Low(TNyxBuildTarget))) or
    (Ord(ATarget) > Ord(High(TNyxBuildTarget))) then
  begin
    raise ENyxModel.Create('Unknown source projection target');
  end;
end;

function TSourceProjectionBuild.GetReference: TNyxSourceProjectionRef;
begin
  Result := FReference;
end;

function TSourceProjectionBuild.GetProjection: INyxSourceProjection;
begin
  Result := FProjection;
end;

function TSourceProjectionBuild.GetArtifact: TNyxText;
begin
  Result := FArtifact;
end;

function TSourceProjectionBuild.GetRuntimeLog: TNyxText;
begin
  Result := FRuntimeLog;
end;

function NewNyxSourceProjectionBuild(const AReference: TNyxSourceProjectionRef;
  const AProjection: INyxSourceProjection;
  const AArtifact, ARuntimeLog: TNyxText): INyxSourceProjectionBuild;
var
  LBuild: TSourceProjectionBuild;
  LExpected: TNyxText;
begin
  NyxSourceProjectionRef(AReference.Name);

  if AProjection = nil then
  begin
    raise ENyxModel.Create('Projection receipt requires a result');
  end;
  LExpected := '';

  if (AProjection.State = spsCompiled) and (AProjection.Target = btBrowser) then
  begin
    LExpected := 'builds/' + AReference.Name + '/nyx_projection.js';
  end;

  if AArtifact <> LExpected then
  begin
    raise ENyxModel.Create('Projection receipt cannot advertise this artifact');
  end;
  NyxUTF8ByteCount(ARuntimeLog);
  LBuild := TSourceProjectionBuild.Create;
  Result := LBuild;
  LBuild.FReference := AReference;
  LBuild.FProjection := AProjection;
  LBuild.FArtifact := AArtifact;
  LBuild.FRuntimeLog := ARuntimeLog;
end;

function TSourceProjection.GetSource: TNyxText;
begin
  Result := FSource;
end;

function TSourceProjection.GetTarget: TNyxBuildTarget;
begin
  Result := FTarget;
end;

function TSourceProjection.GetState: TNyxSourceProjectionState;
begin
  Result := FState;
end;

function TSourceProjection.GetMessage: TNyxText;
begin
  Result := FMessage;
end;

function TSourceProjection.GetDesign: TNyxText;
begin
  Result := FDesign;
end;

function TSourceProjection.GetReport: INyxCompilerReport;
begin
  Result := FReport;
end;

function TSourceProjection.CopyDocument: TNyxDocument;
begin

  if (FState <> spsExecuted) or (FDesign = '') then
  begin
    raise ENyxModel.Create('Projection has no admitted design');
  end;
  Result := TNyxCodec.Decode(FDesign);
end;

function NyxSourceProjectionFailure(const ASource: TNyxText;
  ATarget: TNyxBuildTarget; AState: TNyxSourceProjectionState;
  const AMessage: TNyxText; const AReport: INyxCompilerReport): INyxSourceProjection;
var
  LResult: TSourceProjection;
begin
  ValidateNyxProjectionSource(ASource);
  ValidateNyxProjectionTarget(ATarget);
  NyxSourceProjectionStateName(AState);

  if AState = spsExecuted then
  begin
    raise ENyxModel.Create('Executed projection requires producer result admission');
  end;
  NyxUTF8ByteCount(AMessage);
  LResult := TSourceProjection.Create;
  Result := LResult;
  LResult.FSource := ASource;
  LResult.FTarget := ATarget;
  LResult.FState := AState;
  LResult.FMessage := AMessage;
  LResult.FReport := AReport;
end;

function ProjectionPacket(const AReference: TNyxSourceProjectionRef;
  ATarget: TNyxBuildTarget; AState: TNyxSourceProjectionState;
  const ADesign, AMessage: TNyxText): TNyxDataValue;
begin
  NyxSourceProjectionRef(AReference.Name);
  ValidateNyxProjectionTarget(ATarget);
  Result := NyxObject([
    NyxField('version', NyxData(1)),
    NyxField('ticket', NyxData(AReference.Name)),
    NyxField('target', NyxData(NyxBuildTargetName(ATarget))),
    NyxField('state', NyxData(NyxSourceProjectionStateName(AState))),
    NyxField('design', NyxData(ADesign)),
    NyxField('message', NyxData(AMessage))]);

  if NyxUTF8ByteCount(Result.ToJSON) > NyxProjectionMaximumResultBytes then
  begin
    raise ENyxModel.Create('Projection result exceeds its 4 MiB channel budget');
  end;
end;

function CaptureNyxSourceProjection(ADocument: TNyxDocument;
  const AReference: TNyxSourceProjectionRef; ATarget: TNyxBuildTarget): TNyxDataValue;
var
  LDesign: TNyxText;
begin

  if ADocument = nil then
  begin
    raise ENyxModel.Create('Source constructor returned no document');
  end;
  ValidateNyxDocumentProperties(ADocument);
  LDesign := TNyxCodec.Encode(ADocument);
  Result := ProjectionPacket(AReference, ATarget, spsExecuted, LDesign, '');
end;

function NyxSourceProjectionFailurePacket(const AReference: TNyxSourceProjectionRef;
  ATarget: TNyxBuildTarget; AState: TNyxSourceProjectionState): TNyxDataValue;
var
  LMessage: TNyxText;
begin
  case AState of
    spsExecutionFailed:
      begin
        LMessage := 'The source constructor failed during execution';
      end;
    spsInvalidDesign:
      begin
        LMessage := 'The source constructor returned an invalid design';
      end;
  else
    begin
      raise ENyxModel.Create('Producer failures are execution or design failures');
    end;
  end;
  Result := ProjectionPacket(AReference, ATarget, AState, '', LMessage);
end;

function ReceiveNyxSourceProjection(const ASource: TNyxText;
  const AReference: TNyxSourceProjectionRef; ATarget: TNyxBuildTarget;
  const AMessage: TNyxText; const AReport: INyxCompilerReport): INyxSourceProjection;
var
  LData: TNyxDataValue;
  LDocument: TNyxDocument;
  LResult: TSourceProjection;
  LDesign: TNyxText;
  LState: TNyxSourceProjectionState;
begin
  ValidateNyxProjectionSource(ASource);
  NyxSourceProjectionRef(AReference.Name);
  ValidateNyxProjectionTarget(ATarget);
  LDocument := nil;
  try
    try

      if NyxUTF8ByteCount(AMessage) > NyxProjectionMaximumResultBytes then
      begin
        raise ENyxModel.Create('Projection reply exceeds its byte budget');
      end;
      LData := TNyxDataValue.ParseJSON(AMessage);

      if (LData.Kind <> ndObject) or (LData.Count <> 6) or
        (LData.Field('version').ToJSON <> '1') or
        (LData.Field('ticket').Kind <> ndText) or
        (LData.Field('target').Kind <> ndText) or
        (LData.Field('state').Kind <> ndText) or
        (LData.Field('design').Kind <> ndText) or
        (LData.Field('message').Kind <> ndText) or
        (LData.Field('ticket').AsText <> AReference.Name) or
        (LData.Field('target').AsText <> NyxBuildTargetName(ATarget)) then
      begin
        raise ENyxModel.Create('Projection reply does not match its complete producer envelope');
      end;
      LState := spsExecuted;

      if LData.Field('state').AsText = NyxSourceProjectionStateName(spsExecutionFailed) then
      begin
        LState := spsExecutionFailed;
      end
      else if LData.Field('state').AsText = NyxSourceProjectionStateName(spsInvalidDesign) then
      begin
        LState := spsInvalidDesign;
      end
      else if LData.Field('state').AsText <> NyxSourceProjectionStateName(spsExecuted) then
      begin
        raise ENyxModel.Create('Projection producer has an unknown execution state');
      end;
      LDesign := LData.Field('design').AsText;

      if LState <> spsExecuted then
      begin

        if LDesign <> '' then
        begin
          raise ENyxModel.Create('Failed projection cannot carry an admitted design');
        end;
        Exit(NyxSourceProjectionFailure(ASource, ATarget, LState,
          LData.Field('message').AsText, AReport));
      end;

      if LData.Field('message').AsText <> '' then
      begin
        raise ENyxModel.Create('Executed projection cannot carry a failure message');
      end;
      LDocument := TNyxCodec.Decode(LDesign);
      ValidateNyxDocumentProperties(LDocument);

      if TNyxCodec.Encode(LDocument) <> LDesign then
      begin
        raise ENyxModel.Create('Projection design is not an exact canonical snapshot');
      end;
      LResult := TSourceProjection.Create;
      Result := LResult;
      LResult.FSource := ASource;
      LResult.FTarget := ATarget;
      LResult.FState := spsExecuted;
      LResult.FDesign := LDesign;
      LResult.FReport := AReport;
    except
      on LException: Exception do
      begin
        Result := NyxSourceProjectionFailure(ASource, ATarget, spsInvalidDesign,
          'Projection reply could not be admitted', AReport);
      end;
    end;
  finally
    LDocument.Free;
  end;
end;

function GenerateNyxSourceProjectionProgram(const AUnit: TNyxPascalUnitRef;
  const AReference: TNyxSourceProjectionRef; ATarget: TNyxBuildTarget): TNyxText;
var
  LLines: TNyxStrings;
  LTarget: TNyxText;
begin
  TNyxCodegen.AdmitUnitName(AUnit.Name);
  NyxSourceProjectionRef(AReference.Name);
  ValidateNyxProjectionTarget(ATarget);
  LTarget := 'btBrowser';

  if ATarget = btNativeLCL then
  begin
    LTarget := 'btNativeLCL';
  end;
  LLines := TNyxStrings.Create;
  try
    LLines.Add('program nyx_projection;');
    LLines.Add('{$mode delphi}{$H+}{$codepage utf8}');
    LLines.Add('uses');

    if ATarget = btBrowser then
    begin
      LLines.Add('  WebWorker, WebOrWorker,');
    end
    else
    begin
      LLines.Add('  Classes, nyx.bytes,');
    end;
    LLines.Add('  SysUtils, nyx.text, nyx.data, nyx.model, nyx.studio.builds,');
    LLines.Add('  nyx.studio.sourceprojection, ' + AUnit.Name + ';');
    LLines.Add('var');
    LLines.Add('  LDocument: TNyxDocument;');
    LLines.Add('  LPacket: TNyxDataValue;');

    if ATarget = btNativeLCL then
    begin
      LLines.Add('  LBytes: TNyxBytes;');
      LLines.Add('  LFile: TFileStream;');
    end;
    LLines.Add('begin');
    LLines.Add('  LDocument := nil;');
    LLines.Add('  try');
    LLines.Add('    try');
    LLines.Add('      LDocument := BuildNyxDocument;');
    LLines.Add('      try');
    LLines.Add('        LPacket := CaptureNyxSourceProjection(LDocument,');
    LLines.Add('          NyxSourceProjectionRef(''' + AReference.Name + '''), ' + LTarget + ');');
    LLines.Add('      except');
    LLines.Add('        on LException: Exception do');
    LLines.Add('        begin');
    LLines.Add('          LPacket := NyxSourceProjectionFailurePacket(');
    LLines.Add('            NyxSourceProjectionRef(''' + AReference.Name + '''),');
    LLines.Add('            ' + LTarget + ', spsInvalidDesign);');
    LLines.Add('        end;');
    LLines.Add('      end;');
    LLines.Add('    except');
    LLines.Add('      on LException: Exception do');
    LLines.Add('      begin');
    LLines.Add('        LPacket := NyxSourceProjectionFailurePacket(');
    LLines.Add('          NyxSourceProjectionRef(''' + AReference.Name + '''),');
    LLines.Add('          ' + LTarget + ', spsExecutionFailed);');
    LLines.Add('      end;');
    LLines.Add('    end;');
    LLines.Add('  finally');
    LLines.Add('    LDocument.Free;');
    LLines.Add('  end;');

    if ATarget = btBrowser then
    begin
      LLines.Add('  WebWorker.Self_.postMessage(LPacket.ToJSON);');
    end
    else
    begin
      LLines.Add('  LBytes := NyxEncodeUTF8(LPacket.ToJSON);');
      LLines.Add('  LFile := TFileStream.Create(''' + NyxProjectionResultFile + ''', fmCreate);');
      LLines.Add('  try');
      LLines.Add('');
      LLines.Add('    if Length(LBytes) > 0 then');
      LLines.Add('    begin');
      LLines.Add('      LFile.WriteBuffer(LBytes[0], Length(LBytes));');
      LLines.Add('    end;');
      LLines.Add('  finally');
      LLines.Add('    LFile.Free;');
      LLines.Add('  end;');
    end;
    LLines.Add('end.');
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

end.
