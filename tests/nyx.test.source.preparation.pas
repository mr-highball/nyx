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


unit nyx.test.source.preparation;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Public detached preparation and creator-environment evidence. Native threaded
  consumers additionally retain a snapshot while new metadata is published.
  These are qualification inputs, never initial Studio demo/review content. }
function RunNyxSourcePreparationTests: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.schema,
  nyx.codec, nyx.codegen, nyx.source, nyx.source.preparation,
  nyx.event.payload, nyx.controls,
  nyx.test.source.structural;

type
  TPublicationAction = class(TInterfacedObject, INyxSchemaAction)
  public
    Calls: Integer;
    Fail: Boolean;
    procedure Execute;
  end;

  TValidationAction = class(TInterfacedObject, INyxSchemaAction)
  public
    Document: TNyxDocument;
    Inner: INyxSchemaSnapshot;
    ForceFailure: Boolean;
    Calls: Integer;
    procedure Execute;
  end;

procedure TValidationAction.Execute;
var
  LOther: TValidationAction;
  LLease: INyxSchemaAction;
  LRejected: Boolean;
begin
  Inc(Calls);
  ValidateNyxDocumentProperties(Document);

  if Inner <> nil then
  begin
    LOther := TValidationAction.Create;
    LLease := LOther;
    LOther.Document := Document;
    LRejected := False;
    try
      Inner.Execute(LLease);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;

    if not LRejected then
    begin
      raise ENyxModel.Create('The newer creator constraint was omitted');
    end;
    ValidateNyxDocumentProperties(Document);
  end;

  if ForceFailure then
  begin
    raise ENyxModel.Create('Deliberate schema-scope failure');
  end;
end;

function ReplaceField(const AData: TNyxDataValue; const AName: TNyxText;
  const AValue: TNyxDataValue): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, AData.Count);
  for LIndex := 0 to AData.Count - 1 do
  begin

    if AData.Key(LIndex) = AName then
    begin
      LFields[LIndex] := NyxField(AName, AValue);
    end
    else
    begin
      LFields[LIndex] := NyxField(AData.Key(LIndex), AData.Field(AData.Key(LIndex)));
    end;
  end;
  Result := NyxObject(LFields);
end;

procedure TPublicationAction.Execute;
begin
  Inc(Calls);

  if Fail then
  begin
    raise ENyxModel.Create('Publication fixture failure');
  end;
end;

function RunNyxSourcePreparationTests: Integer;
var
  LBefore: INyxSchemaSnapshot;
  LCurrent: INyxSchemaSnapshot;
  LRoundTrip: INyxSchemaSnapshot;
  LPrepared: INyxPreparedSource;
  LReceived: INyxPreparedSource;
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LPage: INyxColumn;
  LCustom: INyxControl;
  LProperty: TNyxPropertyInfo;
  LEvent: TNyxEventSchema;
  LAction: TValidationAction;
  LActionLease: INyxSchemaAction;
  LPublication: TPublicationAction;
  LPublicationLease: INyxSchemaAction;
  LSource: TNyxText;
  LDesign: TNyxText;
  LWire: TNyxDataValue;
  LRow: TNyxDataValue;
  LBad: TNyxDataValue;
  LRejected: Boolean;
  LRevision: Integer;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Source preparation: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure RejectWire(const AData: TNyxDataValue);
  var
    LUnexpected: INyxSchemaSnapshot;
  begin
    LRejected := False;
    try
      LUnexpected := ReadNyxSchemaSnapshot(AData);
      Check(LUnexpected <> nil, 'a successful transport read returns an owner');
    except
      on Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (NyxSchemaRevision = LRevision),
      'invalid creator transport cannot publish or change the global registry');
  end;

begin
  Result := 0;
  LBefore := CaptureNyxSchemas;
  LDocument := TNyxDocument.Create;
  LCandidate := nil;
  LWorkspace := nil;
  try
    LPage := NewNyxColumn('home');
    LDocument.AddPage(LPage);
    LCustom := NewNyxControl(NyxCustomKind('source-preparation-box'), 'creator-box');
    LCustom.Configure.CustomProjection(NyxCustomKind(NyxKindName(nkColumn)))
      .Gap(1).Text(TNyxText('Fixture / 🌙 漢字')).Done;
    LPage.Add(LCustom);
    LCustom := nil;
    LPage := nil;
    LSource := AddNyxStructuralFixture(TNyxCodegen.Generate(LDocument));
    LDesign := TNyxCodec.Encode(LDocument);
    LProperty := Default(TNyxPropertyInfo);
    LProperty.Key := 'gap';
    LProperty.Title := TNyxText('Creator spacing / 🌙 漢字');
    LProperty.ValueType := npInteger;
    LProperty.Minimum := 3;
    LProperty.Maximum := 8;
    LProperty.DefaultValue := '3';
    LProperty.Advanced := True;
    LProperty.Support := NyxPropertySupport(npmPresentation, ncCustom, ncCustom,
      TNyxText('Exact creator help / 🌙 漢字'));
    LEvent := NyxNamedEventSchema(NyxEvent(TNyxText('creator/🌙')), 'Creator reply',
      'A creator-owned signal', ncCustom, ncCustom);
    LEvent.Contexts := [nctxKeyboard, nctxTextEdit];
    RegisterNyxSchema(NyxCustomKind('source-preparation-box'), [LProperty], [LEvent]);
    LCurrent := CaptureNyxSchemas;
    LRevision := NyxSchemaRevision;
    Check((LCurrent.Revision = LRevision) and (LBefore.Revision < LRevision),
      'publication advances the creator generation independently of documents');
    LWire := LCurrent.ToData;
    LRoundTrip := ReadNyxSchemaSnapshot(TNyxDataValue.ParseJSON(LWire.ToJSON));
    Check((LRoundTrip.ToData.ToJSON = LWire.ToJSON) and
      (LRoundTrip.Revision = LCurrent.Revision),
      'property/help/support, payload/context and Unicode metadata transport exactly');
    Check(LBefore.ToData.Field('schemas').Count + 1 = LCurrent.ToData.Field('schemas').Count,
      'a retained earlier environment is unaffected by later registry growth');

    LPrepared := PrepareNyxSource(LSource, LBefore);
    Check(not LPrepared.Diagnostic.Defined and (LPrepared.Source = LSource) and
      (LPrepared.SchemaRevision = LBefore.Revision),
      'complete independent preparation uses its exact captured creator environment');
    LReceived := ReceiveNyxPreparedSource(LPrepared.ToData, LSource, LBefore);
    Check(not LReceived.Diagnostic.Defined and (LReceived.Design = LPrepared.Design),
      'processor transport preserves Unicode and the dispatched older creator environment');
    LReceived := nil;
    LPrepared.Take(LCandidate, LWorkspace);
    try
      Check((LCandidate.Count = 2) and (LCandidate.ComponentCount = 1) and
        (LCandidate.Find('creator-box').Prop('text') = TNyxText('Fixture / 🌙 漢字')) and
        (LCandidate.Find('code-action').Part('button') <> nil),
        'fresh owned recipes, custom controls, state, roots and text reconstruct');
      Check((TNyxCodec.Encode(LDocument) = LDesign) and
        (LWorkspace.Capture.Design = LPrepared.Design) and (LPrepared.Source = LSource),
        'prepared owners and retained text are independent of the accepted baseline');
      LRejected := False;
      try
        LPrepared.Take(LCandidate, LWorkspace);
      except
        on ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LCandidate <> nil) and (LWorkspace <> nil),
        'nonempty transfer destinations are preserved');
    finally
      FreeAndNil(LWorkspace);
      FreeAndNil(LCandidate);
    end;
    LRejected := False;
    try
      LPrepared.Take(LCandidate, LWorkspace);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LCandidate = nil) and (LWorkspace = nil),
      'ownership transfers exactly once');
    LPrepared := PrepareNyxSource(LSource, LRoundTrip);
    Check(LPrepared.Diagnostic.Defined and (LPrepared.Diagnostic.Message <> '') and
      (LPrepared.Diagnostic.Line = 0) and (LPrepared.Design = ''),
      'the current creator bound is enforced without inventing a lexer location');
    LRejected := False;
    try
      LPrepared.Take(LCandidate, LWorkspace);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LCandidate = nil) and (LWorkspace = nil),
      'failed preparation exposes no partially admitted resources');

    LAction := TValidationAction.Create;
    LActionLease := LAction;
    LAction.Document := LDocument;
    LAction.Inner := LCurrent;
    LBefore.Execute(LActionLease);
    Check(LAction.Calls = 1, 'nested failure restores the enclosing older environment');
    LAction.ForceFailure := True;
    LRejected := False;
    try
      LBefore.Execute(LActionLease);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (CaptureNyxSchemas.ToData.ToJSON = LWire.ToJSON),
      'scope failure restores current global metadata');
    LPrepared := PrepareNyxSource(LSource + #10 + '''unfinished', LBefore);
    Check(LPrepared.Diagnostic.Defined and (LPrepared.Diagnostic.Line > 0) and
      (LPrepared.Diagnostic.Column > 0) and (LPrepared.Source = LSource + #10 + '''unfinished'),
      'invalid exact source retains its positioned detached diagnostic');

    RejectWire(ReplaceField(LWire, 'version', NyxData(2)));
    RejectWire(ReplaceField(LWire, 'revision', NyxData(-1)));
    LRow := LWire.Field('schemas').Item(LWire.Field('schemas').Count - 1);
    LBad := ReplaceField(LRow, 'properties', NyxArray([
      ReplaceField(LRow.Field('properties').Item(0), 'type', NyxData(999))]));
    RejectWire(ReplaceField(LWire, 'schemas', NyxArray([LBad])));
    RejectWire(ReplaceField(LWire, 'schemas', NyxArray([LRow, LRow])));
    LBad := ReplaceField(LRow, 'events', NyxArray([
      ReplaceField(LRow.Field('events').Item(0), 'producer', NyxData(False))]));
    RejectWire(ReplaceField(LWire, 'schemas', NyxArray([LBad])));
    LBad := ReplaceField(LRow, 'events', NyxArray([
      ReplaceField(LRow.Field('events').Item(0), 'routes', NyxArray([NyxNull]))]));
    RejectWire(ReplaceField(LWire, 'schemas', NyxArray([LBad])));
    Check((LCurrent.ToData.ToJSON = LWire.ToJSON) and
      (TNyxCodec.Encode(LDocument) = LDesign), 'failed wire/preparation retains all input owners');
    LPublication := TPublicationAction.Create;
    LPublicationLease := LPublication;
    Check(not CommitNyxSchemaRevision(LBefore.Revision, LPublicationLease) and
      (LPublication.Calls = 0), 'stale creator publication performs no candidate action');
    Check(CommitNyxSchemaRevision(LRevision, LPublicationLease) and
      (LPublication.Calls = 1), 'current creator generation executes one publication');
    LPublication.Fail := True;
    LRejected := False;
    try
      CommitNyxSchemaRevision(LRevision, LPublicationLease);
    except
      on ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    LPublication.Fail := False;
    Check(LRejected and CommitNyxSchemaRevision(LRevision, LPublicationLease) and
      (LPublication.Calls = 3), 'failed publication releases the guard for later work');
  finally
    LPublicationLease := nil;
    LActionLease := nil;
    LReceived := nil;
    LPrepared := nil;
    LRoundTrip := nil;
    LCurrent := nil;
    LBefore := nil;
    LWorkspace.Free;
    LCandidate.Free;
    LCustom := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

end.
