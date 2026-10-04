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

unit nyx.test.identity;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.model;

{ Shared authored boundary names; concatenate complete portable text values. }
function NyxIdentityDefinitionID: TNyxText;
function NyxIdentityInstanceID: TNyxText;
{ Add a source fixture for persistence, generated compilation and real target
  selection/events. Caller owns the document and every admitted root. }
procedure AddNyxIdentityFixture(ADocument: TNyxDocument);
function RunNyxIdentityTests: Integer;

implementation

uses
  SysUtils,
  nyx.codec,
  nyx.composition,
  nyx.studio.session,
  nyx.studio.view;

function RepeatText(const AText: TNyxText; ACount: Integer): TNyxText;
var
  LIndex: Integer;
begin
  Result := '';
  for LIndex := 1 to ACount do
  begin
    Result := Result + AText;
  end;
end;

function NyxIdentityDefinitionID: TNyxText;
begin
  Result := RepeatText('🌙', 128);
end;

function NyxIdentityInstanceID: TNyxText;
begin
  Result := RepeatText('界', 128);
end;

procedure AddNyxIdentityFixture(ADocument: TNyxDocument);
var
  LDefinition: TNyxNode;
  LOuter: TNyxNode;
  LPage: TNyxNode;
  LInstance: TNyxNode;
begin
  LDefinition := TNyxNode.Create('card', NyxIdentityDefinitionID);
  ADocument.AddComponent(LDefinition);
  LDefinition.Add(TNyxNode.Create('button', 'action/🌙~1')
    .SetProp('part', 'action').SetProp('text', 'Save / 🌙').SetProp('emit', 'save'));
  LOuter := TNyxNode.Create('card', 'outer/definition~🌙');
  ADocument.AddComponent(LOuter);
  LOuter.Add(TNyxNode.Create('component', 'inner/ref~🌙')
    .SetProp('part', 'nested').SetProp('component', LDefinition.ID));
  ADocument.AddComponent(TNyxNode.Create('button', 'instance')
    .SetProp('text', 'Runtime action').SetProp('emit', 'runtime'));
  LPage := TNyxNode.Create('page', 'identity/page~🌙');
  ADocument.AddPage(LPage);
  { The authored slash name equals the other reference's qualified key. Put it
    first to expose lookup implementations that return the first mixed match. }
  LPage.Add(TNyxNode.Create('button', 'literal/instance')
    .SetProp('text', 'Literal action').SetProp('emit', 'literal'));
  LPage.Add(TNyxNode.Create('component', 'literal').SetProp('component', 'instance'));
  LInstance := TNyxNode.Create('component', NyxIdentityInstanceID)
    .SetProp('component', LOuter.ID);
  LPage.Add(LInstance);
  LInstance.OverridePart('nested/action').Named('identity/caption~🌙')
    .SetProp('text', 'Save this instance / 🌙');
  LInstance.OverridePart('nested', 'append').Named('identity/content~🌙')
    .Add(TNyxNode.Create('button', 'own/action~🌙')
      .SetProp('text', 'Own action / 🌙').SetProp('emit', 'own'));
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('FAIL identity: ' + AMessage);
  end;
  Inc(ACount);
end;

procedure RejectRename(ANode: TNyxNode; const AID: TNyxText; var ACount: Integer);
var
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  LBefore := ANode.ID;
  LRejected := False;
  try
    ANode.Named(AID);
  except
    on LException: ENyxModel do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected and (ANode.ID = LBefore), 'failed rename preserves identity', ACount);
end;

function JSONIdentity(const AQuotedID: TNyxText): TNyxText;
begin
  Result := '{"version":1,"title":"","pages":[{"kind":"page","id":' +
    AQuotedID + ',"props":{},"children":[]}],"components":[]}';
end;

procedure RejectJSONIdentity(const AQuotedID: TNyxText; var ACount: Integer);
var
  LDocument: TNyxDocument;
  LRejected: Boolean;
begin
  LDocument := nil;
  LRejected := False;
  try
    try
      LDocument := TNyxCodec.Decode(JSONIdentity(AQuotedID));
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'escaped malformed Unicode identity is rejected', ACount);
  finally
    LDocument.Free;
  end;
end;

function RunNyxIdentityTests: Integer;
var
  LNode: TNyxNode;
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LRuntime: TNyxNode;
  LCopy: TNyxNode;
  LNested: TNyxNode;
  LSource: TNyxText;
  LInvalid: TNyxText;
  LSession: TNyxStudioSession;
  LShell: TNyxDocument;
  LBefore: TNyxText;
  LRejected: Boolean;
begin
  Result := 0;
  LNode := TNyxNode.Create('label', 'before');
  try
    LNode.Named(NyxIdentityDefinitionID);
    Check(LNode.ID = NyxIdentityDefinitionID, '128 supplementary scalars are admitted', Result);
    LNode.Named(NyxIdentityInstanceID);
    Check(LNode.ID = NyxIdentityInstanceID, '128 CJK scalars are admitted', Result);
    LNode.Named(RepeatText('x', 128));
    Check(LNode.SourceID = LNode.ID, '128 ASCII scalars retain source identity', Result);
    RejectRename(LNode, RepeatText('🌙', 129), Result);
    RejectRename(LNode, RepeatText('界', 129), Result);
    RejectRename(LNode, RepeatText('x', 129), Result);
    RejectRename(LNode, '', Result);
    RejectRename(LNode, '   ', Result);
    RejectRename(LNode, ' ', Result); { U+00A0, not ordinary ASCII space. }
    RejectRename(LNode, 'x' + #10, Result);
    RejectRename(LNode, 'x' + #127, Result);
    {$IFDEF PAS2JS}
    LInvalid := Chr($d800);
    {$ELSE}
    { An encoded surrogate is malformed UTF-8, corresponding to the browser's
      unpaired UTF-16 surrogate. Tag raw bytes without an ANSI conversion. }
    SetLength(LInvalid, 3);
    LInvalid[1] := AnsiChar($ed);
    LInvalid[2] := AnsiChar($a0);
    LInvalid[3] := AnsiChar($80);
    SetCodePage(RawByteString(LInvalid), CP_UTF8, False);
    {$ENDIF}
    RejectRename(LNode, LInvalid, Result);
    LNode.Named('🌙/a~b');
    Check(NyxQualifiedID('', LNode.ID) = TNyxText('🌙~1a~0b'),
      'escape preserves Unicode and source separators', Result);
    Check(NyxQualifiedID('', 'a/b') <> NyxQualifiedID('', 'a~1b'),
      'escaped literal and slash segments remain distinct', Result);
    Check(NyxQualifiedID('scope', 'a/b') <> NyxQualifiedID('scope/a', 'b'),
      'a literal separator cannot forge a nested path', Result);
    LNode.Named(' 🌙 ');
    Check(LNode.ID = TNyxText(' 🌙 '), 'meaningful leading/trailing spaces are preserved', Result);
    LNode.Named('é'); { e + U+0301, two scalar values, without normalization. }
    Check(LNode.ID <> TNyxText('é'), 'canonically equivalent spellings remain distinct IDs', Result);
  finally
    LNode.Free;
  end;
  RejectJSONIdentity('"\uD800"', Result);
  RejectJSONIdentity('"\uDC00"', Result);
  LDecoded := TNyxCodec.Decode(JSONIdentity('"\uD83C\uDF19"'));
  try
    Check(LDecoded.Pages[0].ID = TNyxText('🌙'),
      'a JSON escaped surrogate pair produces one valid supplementary scalar', Result);
  finally
    LDecoded.Free;
  end;
  LDocument := TNyxDocument.Create;
  try
    AddNyxIdentityFixture(LDocument);
    LDocument.Validate;
    LBefore := TNyxCodec.Encode(LDocument);
    LDecoded := CloneNyxViewDocument(LDocument, LDocument.Find(NyxIdentityInstanceID));
    try
      Check((LDecoded.Count = 1) and (LDecoded.ComponentCount = 2) and
        (LDecoded.Pages[0].ID = NyxIdentityInstanceID) and
        (LDecoded.Find('identity/caption~🌙') <> nil) and
        (LDecoded.FindComponent('instance') = nil),
        'isolated instance keeps IDs and only its transitive definitions', Result);
      LDecoded.Pages[0].SetProp('extension', 'independent');
      Check(TNyxCodec.Encode(LDocument) = LBefore,
        'isolated view mutation leaves the source design unchanged', Result);
    finally
      LDecoded.Free;
    end;
    LDecoded := CloneNyxViewDocument(LDocument, LDocument.FindComponent(NyxIdentityDefinitionID));
    try
      Check((LDecoded.ComponentCount = 0) and
        (LDecoded.Pages[0].ID = NyxIdentityDefinitionID),
        'definition-only preview preserves maximum-length ID without duplication', Result);
    finally
      LDecoded.Free;
    end;
    LDecoded := TNyxCodec.Decode(LBefore);
    try
      Check(TNyxCodec.Encode(LDecoded) = LBefore,
        'long supplementary and separator IDs round-trip exactly', Result);
      LRuntime := RealizeNyxView(LDecoded, LDecoded.Find(NyxIdentityInstanceID));
      try
        LNested := LRuntime.Part('nested');
        Check(LNested.SourceID = NyxIdentityDefinitionID,
          'nested template identity remains authored text', Result);
        Check(LNested.DesignID = NyxIdentityInstanceID,
          'inherited parts retain editable instance ownership', Result);
        Check(Length(LNested.ID) > 128, 'qualification is independent of authored size', Result);
        Check((LNested.Part('action').SourceID = TNyxText('action/🌙~1')) and
          (LNested.Part('action').Prop('text') = TNyxText('Save this instance / 🌙')),
          'nested overrides keep source identity and customize only this instance', Result);
        Check(LRuntime.Find(NyxQualifiedID(NyxQualifiedID('', NyxIdentityInstanceID),
          'own/action~🌙')).DesignID = TNyxText('own/action~🌙'),
          'inserted payload retains its own editable identity', Result);
        LCopy := LRuntime.Clone;
        try
          Check((LCopy.ID = LRuntime.ID) and LCopy.IsRealized and
            (LCopy.Part('nested').SourceID = LNested.SourceID),
            'cloned realized trees retain identity role and long qualification', Result);
        finally
          LCopy.Free;
        end;
        RejectRename(LRuntime, 'replacement', Result);
        LRejected := False;
        try
          LDocument.AddPage(LRuntime);
        except
          on LException: ENyxModel do
          begin
            LRejected := True;
          end;
        end;
        Check(LRejected and not LDocument.Contains(LRuntime),
          'design admission refuses a realized tree without consuming it', Result);
        Check(TNyxCodec.Encode(LDecoded) = LBefore,
          'realization and customization preserve the complete source design', Result);
      finally
        LRuntime.Free;
      end;
      LRuntime := RealizeNyxView(LDecoded, LDecoded.Pages[0]);
      try
        Check((LRuntime.Find('literal~1instance').DesignID = 'literal/instance') and
          (LRuntime.Find('literal/instance').DesignID = 'literal'),
          'owned slash identity cannot collide with reusable qualification', Result);
      finally
        LRuntime.Free;
      end;
    finally
      LDecoded.Free;
    end;
    LSession := TNyxStudioSession.Create;
    try
      LSession.Load(TNyxCodec.Encode(LDocument));
      LSession.Activate('identity/page~🌙');
      LSession.Select(NyxIdentityInstanceID);
      LShell := BuildNyxStudioView(LSession, DefaultNyxStudioViewState);
      try
        LShell.Validate;
        Check((LShell.Find('tree-node-4').Prop('select-id') = NyxIdentityInstanceID) and
          (LShell.Find('view-component-0').Prop('view-id') = NyxIdentityDefinitionID),
          'Studio chrome carries long authored identities as command data', Result);
      finally
        LShell.Free;
      end;
      LBefore := LSession.Save;
      LSource := LBefore;
      { Replace an admitted ID in otherwise valid JSON with a scalar-overflow
        identity. This checks atomic Studio import, not just a constructor call. }
      LSource := StringReplace(LSource, NyxIdentityInstanceID,
        RepeatText('界', 129), [rfReplaceAll]);
      LRejected := False;
      try
        LSession.Load(LSource);
      except
        on LException: ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LSession.Save = LBefore) and
        (LSession.SelectedID = NyxIdentityInstanceID) and
        (LSession.ActiveViewID = TNyxText('identity/page~🌙')),
        'invalid Unicode-ID import retains design, selection and view', Result);
    finally
      LSession.Free;
    end;
  finally
    LDocument.Free;
  end;
end;

end.
