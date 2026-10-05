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

unit nyx.studio.edits;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.model, nyx.catalog;

type
  { Closed semantic operation vocabulary. JSON names are admitted once at the
    transport boundary; internal behavior never dispatches arbitrary properties
    or method names. A whole immutable patch builds one detached candidate. }
  TNyxDesignOperation = (doCreate, doUpdate, doMove, doDelete, doTitle, doTokens,
    doDerive, doInstance, doOverride, doInherit);

  { Copied typed reusable intents. IDs, definition references and named paths
    have separate families; override behavior is a closed Pascal enum. Mutable
    caller arrays are copied into a patch and never retained by reference. }
  TNyxReusableChange = record
  private
    FOperation: TNyxDesignOperation;
    FControl, FOwner: TNyxControlRef;
    FDefinition: TNyxComponentRef;
    FPath: TNyxPartRef;
    FMode: TNyxOverrideMode;
    FIndex: Integer;
    FIdentities: array of TNyxIdentityAssignment;
  end;

  INyxDesignPatch = interface
    ['{6B582F67-B92C-4191-9798-7CB64402280E}']
    { Borrow both arguments. The caller owns the returned document, including on
      publication rejection; exceptions release all partially constructed nodes. }
    function Candidate(ADocument: TNyxDocument; ACatalog: TNyxCatalog): TNyxDocument;
  end;

{ Decode 1..64 operations. Primitive property values retain their JSON scalar
  type; unknown fields/operations fail. IDs and custom kind names are user data.
  Schema/property/document admission also runs on the complete detached result. }
function ReadNyxDesignPatch(const AOperations: TNyxDataValue): INyxDesignPatch;
{ Derive copies an exact subtree without altering it or replacing its uses.
  Every descendant needs an explicit destination; the definition owns the copy. }
function NyxDeriveComponent(const ASource: TNyxControlRef;
  const ADefinition: TNyxComponentRef;
  const AIdentities: array of TNyxIdentityAssignment): TNyxReusableChange;
{ Instantiate adds a reference, not a copy of the definition. -1 appends; other
  positions are exact. AParent remains the owner of the new authored reference. }
function NyxInstantiateComponent(const ADefinition: TNyxComponentRef;
  const AControl, AParent: TNyxControlRef; AIndex: Integer = -1): TNyxReusableChange;
{ Override names the exact instance-owned descriptor. A repeated path requires
  the same descriptor ID. Payload changes use ordinary create/move/delete in the
  same grouped transaction; incomplete replace/append rules refuse at admission. }
function NyxOverrideComponentPart(const AInstance, ADescriptor: TNyxControlRef;
  const APath: TNyxPartRef; AMode: TNyxOverrideMode): TNyxReusableChange;
{ Restore inheritance removes only the exact matching local descriptor/payload.
  Missing or changed owner/path/identity refuses; definitions remain untouched. }
function NyxInheritComponentPart(const AInstance, ADescriptor: TNyxControlRef;
  const APath: TNyxPartRef): TNyxReusableChange;
{ Copy 1..64 typed commands into the existing design transaction engine. Source,
  drafts, revision and paired Undo remain owned by the ordinary Studio session. }
function NyxReusablePatch(const AChanges: array of TNyxReusableChange): INyxDesignPatch;

implementation

uses
  nyx.schema, nyx.design.tokens, nyx.composition;

type
  TDesignOperation = record
    Operation: TNyxDesignOperation;
    ID: TNyxText;
    Kind: TNyxText;
    Parent: TNyxText;
    Index: Integer;
    Root: TNyxText;
    Properties: TNyxDataValue;
    Source: TNyxText;
    Component: TNyxComponentRef;
    Path: TNyxPartRef;
    Mode: TNyxOverrideMode;
    Identities: array of TNyxIdentityAssignment;
  end;

  TDesignPatch = class(TInterfacedObject, INyxDesignPatch)
  private
    FOperations: array of TDesignOperation;
  public
    function Candidate(ADocument: TNyxDocument; ACatalog: TNyxCatalog): TNyxDocument;
  end;

function NyxDeriveComponent(const ASource: TNyxControlRef;
  const ADefinition: TNyxComponentRef;
  const AIdentities: array of TNyxIdentityAssignment): TNyxReusableChange;
var
  LIndex: Integer;
begin
  Result := Default(TNyxReusableChange);
  Result.FOperation := doDerive;
  Result.FOwner := ASource;
  Result.FDefinition := ADefinition;
  SetLength(Result.FIdentities, Length(AIdentities));
  for LIndex := 0 to High(AIdentities) do
  begin
    Result.FIdentities[LIndex] := AIdentities[LIndex];
  end;
end;

function NyxInstantiateComponent(const ADefinition: TNyxComponentRef;
  const AControl, AParent: TNyxControlRef; AIndex: Integer): TNyxReusableChange;
begin

  if AIndex < -1 then
  begin
    raise ENyxModel.Create('Instantiation index must be -1 or an exact nonnegative position');
  end;
  Result := Default(TNyxReusableChange);
  Result.FOperation := doInstance;
  Result.FDefinition := ADefinition;
  Result.FControl := AControl;
  Result.FOwner := AParent;
  Result.FIndex := AIndex;
end;

function NyxOverrideComponentPart(const AInstance, ADescriptor: TNyxControlRef;
  const APath: TNyxPartRef; AMode: TNyxOverrideMode): TNyxReusableChange;
begin

  if not (AMode in [noProperties, noAppend, noPrepend, noReplace, noRemove]) then
  begin
    raise ENyxModel.Create('Override requires a closed operation');
  end;
  Result := Default(TNyxReusableChange);
  Result.FOperation := doOverride;
  Result.FOwner := AInstance;
  Result.FControl := ADescriptor;
  Result.FPath := APath;
  Result.FMode := AMode;
end;

function NyxInheritComponentPart(const AInstance, ADescriptor: TNyxControlRef;
  const APath: TNyxPartRef): TNyxReusableChange;
begin
  Result := NyxOverrideComponentPart(AInstance, ADescriptor, APath, noProperties);
  Result.FOperation := doInherit;
end;

function NyxReusablePatch(const AChanges: array of TNyxReusableChange): INyxDesignPatch;
var
  LOwner: TDesignPatch;
  LIndex, LMap: Integer;
  LOperation: TDesignOperation;
begin

  if (Length(AChanges) < 1) or (Length(AChanges) > 64) then
  begin
    raise ENyxModel.Create('A transaction requires 1..64 operations');
  end;
  LOwner := TDesignPatch.Create;
  Result := LOwner;
  SetLength(LOwner.FOperations, Length(AChanges));
  for LIndex := 0 to High(AChanges) do
  begin
    LOperation := Default(TDesignOperation);
    LOperation.Operation := AChanges[LIndex].FOperation;

    if not (LOperation.Operation in [doDerive, doInstance, doOverride, doInherit]) then
    begin
      raise ENyxModel.Create('Reusable transaction contains an unconstructed command');
    end;
    LOperation.ID := AChanges[LIndex].FControl.ID;
    LOperation.Parent := AChanges[LIndex].FOwner.ID;
    LOperation.Source := AChanges[LIndex].FOwner.ID;
    LOperation.Component := AChanges[LIndex].FDefinition;
    LOperation.Path := AChanges[LIndex].FPath;
    LOperation.Mode := AChanges[LIndex].FMode;
    LOperation.Index := AChanges[LIndex].FIndex;
    LOperation.Properties := NyxObject([]);
    SetLength(LOperation.Identities, Length(AChanges[LIndex].FIdentities));
    for LMap := 0 to High(LOperation.Identities) do
    begin
      LOperation.Identities[LMap] := AChanges[LIndex].FIdentities[LMap];
    end;
    LOwner.FOperations[LIndex] := LOperation;
  end;
end;

function HasField(const AObject: TNyxDataValue; const AKey: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to AObject.Count - 1 do
  begin

    if AObject.Key(LIndex) = AKey then
    begin
      Exit(True);
    end;
  end;
end;

procedure CheckFields(const AObject: TNyxDataValue; const AAllowed: TNyxText);
var
  LIndex: Integer;
begin

  if AObject.Kind <> ndObject then
  begin
    raise ENyxModel.Create('A semantic operation must be an object');
  end;
  for LIndex := 0 to AObject.Count - 1 do
  begin

    if Pos('|' + AObject.Key(LIndex) + '|', AAllowed) = 0 then
    begin
      raise ENyxModel.Create('Unknown operation field: ' + AObject.Key(LIndex));
    end;
  end;
end;

function ReadNyxDesignPatch(const AOperations: TNyxDataValue): INyxDesignPatch;
var
  LOwner: TDesignPatch;
  LIndex: Integer;
  LWire: TNyxDataValue;
  LName: TNyxText;
  LOperation: TDesignOperation;
  LMap: TNyxDataValue;
  LMapIndex: Integer;
  LMode: TNyxOverrideMode;
  LFound: Boolean;
begin

  if (AOperations.Kind <> ndArray) or (AOperations.Count < 1) or
    (AOperations.Count > 64) then
  begin
    raise ENyxModel.Create('A transaction requires 1..64 operations');
  end;
  LOwner := TDesignPatch.Create;
  Result := LOwner;
  SetLength(LOwner.FOperations, AOperations.Count);
  for LIndex := 0 to AOperations.Count - 1 do
  begin
    LWire := AOperations.Item(LIndex);
    LOperation := Default(TDesignOperation);
    LOperation.Index := -1;
    LOperation.Properties := NyxObject([]);
    LName := LWire.Field('op').AsText;

    if LName = 'create' then
    begin
      LOperation.Operation := doCreate;
      CheckFields(LWire, '|op|id|kind|parent|index|root|properties|');
      LOperation.Kind := LWire.Field('kind').AsText;

      if HasField(LWire, 'root') then
      begin
        { A root and a child placement describe different ownership contracts.
          Refuse ambiguous input rather than silently ignoring its placement. }

        if HasField(LWire, 'parent') or HasField(LWire, 'index') then
        begin
          raise ENyxModel.Create('Root creation cannot also specify parent/index');
        end;
        LOperation.Root := LWire.Field('root').AsText;

        if (LOperation.Root <> 'page') and (LOperation.Root <> 'component') then
        begin
          raise ENyxModel.Create('Root must be page or component');
        end;
      end
      else
      begin
        LOperation.Parent := LWire.Field('parent').AsText;
      end;
      LOperation.ID := LWire.Field('id').AsText;
    end
    else if LName = 'derive' then
    begin
      LOperation.Operation := doDerive;
      CheckFields(LWire, '|op|source|id|identities|');
      LOperation.Source := LWire.Field('source').AsText;
      LOperation.Component := NyxComponent(LWire.Field('id').AsText);
      LMap := LWire.Field('identities');

      if LMap.Kind <> ndObject then
      begin
        raise ENyxModel.Create('Derivation identities require an exact source-to-destination object');
      end;
      SetLength(LOperation.Identities, LMap.Count);
      for LMapIndex := 0 to LMap.Count - 1 do
      begin
        LOperation.Identities[LMapIndex] := NyxIdentity(NyxControl(LMap.Key(LMapIndex)),
          NyxControl(LMap.Field(LMap.Key(LMapIndex)).AsText));
      end;
    end
    else if LName = 'instance' then
    begin
      LOperation.Operation := doInstance;
      CheckFields(LWire, '|op|id|component|parent|index|');
      LOperation.ID := LWire.Field('id').AsText;
      LOperation.Component := NyxComponent(LWire.Field('component').AsText);
      LOperation.Parent := LWire.Field('parent').AsText;
    end
    else if (LName = 'override') or (LName = 'inherit') then
    begin
      LOperation.Operation := doInherit;
      LOperation.ID := LWire.Field('id').AsText;
      LOperation.Parent := LWire.Field('instance').AsText;
      LOperation.Path := NyxPart(LWire.Field('path').AsText);

      if LName = 'override' then
      begin
        CheckFields(LWire, '|op|id|instance|path|mode|');
        LOperation.Operation := doOverride;
        LFound := False;
        for LMode := Low(TNyxOverrideMode) to High(TNyxOverrideMode) do
        begin

          if NyxOverrideName(LMode) = LWire.Field('mode').AsText then
          begin
            LOperation.Mode := LMode;
            LFound := True;
            Break;
          end;
        end;

        if not LFound then
        begin
          raise ENyxModel.Create('Override mode must be properties, append, prepend, replace or remove');
        end;
      end
      else
      begin
        CheckFields(LWire, '|op|id|instance|path|');
      end;
    end
    else if LName = 'update' then
    begin
      LOperation.Operation := doUpdate;
      CheckFields(LWire, '|op|id|properties|');
      LOperation.ID := LWire.Field('id').AsText;
      LOperation.Properties := LWire.Field('properties');
    end
    else if LName = 'move' then
    begin
      LOperation.Operation := doMove;
      CheckFields(LWire, '|op|id|parent|index|');
      LOperation.ID := LWire.Field('id').AsText;
      LOperation.Parent := LWire.Field('parent').AsText;
    end
    else if LName = 'delete' then
    begin
      LOperation.Operation := doDelete;
      CheckFields(LWire, '|op|id|');
      LOperation.ID := LWire.Field('id').AsText;
    end
    else if LName = 'title' then
    begin
      LOperation.Operation := doTitle;
      CheckFields(LWire, '|op|value|');
      LOperation.ID := LWire.Field('value').AsText;
    end
    else if LName = 'tokens' then
    begin
      LOperation.Operation := doTokens;
      CheckFields(LWire, '|op|values|');
      LOperation.Properties := LWire.Field('values');
    end
    else
    begin
      raise ENyxModel.Create('Unknown semantic operation: ' + LName);
    end;

    if HasField(LWire, 'index') then
    begin
      LOperation.Index := LWire.Field('index').AsInteger;

      if LOperation.Index < 0 then
      begin
        raise ENyxModel.Create('Explicit insertion index cannot be negative');
      end;
    end;

    if (LOperation.Operation = doCreate) and HasField(LWire, 'properties') then
    begin
      LOperation.Properties := LWire.Field('properties');
    end;

    if LOperation.Properties.Kind <> ndObject then
    begin
      raise ENyxModel.Create('Properties/tokens require typed object values');
    end;
    LOwner.FOperations[LIndex] := LOperation;
  end;
end;

procedure ConfigureNode(ANode: TNyxNode; ADocument: TNyxDocument;
  const AProperties: TNyxDataValue);
var
  LInfos: TNyxPropertyInfos;
  LIndex: Integer;
  LInfoIndex: Integer;
  LKey: TNyxText;
  LValue: TNyxDataValue;
  LText: TNyxText;
begin
  { This node belongs exclusively to a detached candidate. Stage the complete
    scalar payload before resolving metadata: selectors such as input type,
    projection and reusable definition can change the value domain or available
    properties in the same operation. JSON member order has no semantic meaning.
    Keep the original typed values for admission below; staging a spelling never
    authorizes a string as a number/Boolean or publishes an unknown property. }
  for LIndex := 0 to AProperties.Count - 1 do
  begin
    LKey := AProperties.Key(LIndex);
    LValue := AProperties.Field(LKey);
    case LValue.Kind of
      ndNull:
        begin
          LText := '';
        end;
      ndText:
        begin
          LText := LValue.AsText;
        end;
      ndBoolean:
        begin

          if LValue.AsBoolean then
          begin
            LText := 'true';
          end
          else
          begin
            LText := 'false';
          end;
        end;
      ndNumber:
        begin
          LText := LValue.AsDecimal.Text;
        end;
      else
      begin
        raise ENyxModel.Create('Component properties require scalar JSON values');
      end;
    end;
    ANode.SetProp(LKey, LText);
  end;
  LInfos := NyxProperties(ANode, ADocument);
  for LIndex := 0 to AProperties.Count - 1 do
  begin
    LKey := AProperties.Key(LIndex);
    LInfoIndex := 0;
    while (LInfoIndex < Length(LInfos)) and (LInfos[LInfoIndex].Key <> LKey) do
    begin
      Inc(LInfoIndex);
    end;

    if LInfoIndex = Length(LInfos) then
    begin
      raise ENyxModel.Create('Property is not published for this component: ' + LKey);
    end;
    LValue := AProperties.Field(LKey);

    if LValue.Kind = ndNull then
    begin
      LText := '';
    end
    else
    begin
      case LInfos[LInfoIndex].ValueType of
        npBoolean:
          begin

            if LValue.AsBoolean then
            begin
              LText := 'true';
            end
            else
            begin
              LText := 'false';
            end;
          end;
        npInteger: LText := IntToStr(LValue.AsInteger);
        npNumber: LText := LValue.AsDecimal.Text;
        else
        begin
          LText := LValue.AsText;
        end;
      end;
    end;
    { SetProp is the explicit schema/serialization boundary. Agent strings cannot
      bypass their published scalar types; the entire document validates below. }
    ANode.SetProp(LKey, LText);
  end;
end;

function RequireNode(ADocument: TNyxDocument; const AID: TNyxText): TNyxNode;
begin
  Result := ADocument.Find(AID);

  if Result = nil then
  begin
    raise ENyxModel.Create('Component does not exist: ' + AID);
  end;
end;

function TDesignPatch.Candidate(ADocument: TNyxDocument;
  ACatalog: TNyxCatalog): TNyxDocument;
var
  LIndex: Integer;
  LChildIndex: Integer;
  LInsert: Integer;
  LOperation: TDesignOperation;
  LNode: TNyxNode;
  LParent: TNyxNode;
  LAncestor: TNyxNode;
  LRule: TNyxNode;
begin
  Result := ADocument.Clone;
  try
    for LIndex := 0 to High(FOperations) do
    begin
      LOperation := FOperations[LIndex];
      case LOperation.Operation of
        doDerive:
          begin
            LNode := CloneNyxReusableDefinition(Result,
              RequireNode(Result, LOperation.Source), LOperation.Component,
              LOperation.Identities);
            try
              Result.AddComponent(LNode);
              LNode := nil;
            finally
              LNode.Free;
            end;
          end;
        doInstance:
          begin

            if (LOperation.ID = '') or (Result.Find(LOperation.ID) <> nil) or
              (Result.FindComponent(LOperation.Component.Name) = nil) then
            begin
              raise ENyxModel.Create('Instantiation requires a new identity and an existing reusable definition');
            end;
            LParent := RequireNode(Result, LOperation.Parent);
            LNode := TNyxNode.Create(nkComponent, LOperation.ID);
            try
              LNode.Configure.Component(LOperation.Component).Done;
              LInsert := LOperation.Index;

              if LInsert < 0 then
              begin
                LInsert := LParent.Count;
              end;
              LParent.Insert(LInsert, LNode);
              LNode := nil;
            finally
              LNode.Free;
            end;
          end;
        doOverride, doInherit:
          begin
            LNode := RequireNode(Result, LOperation.Parent);

            if (LNode.ProjectionKind <> NyxKindName(nkComponent)) or
              (LOperation.ID = '') then
            begin
              raise ENyxModel.Create('Part editing requires a reusable instance and exact descriptor identity');
            end;
            LRule := nil;
            for LChildIndex := 0 to LNode.Count - 1 do
            begin

              if LNode.Children[LChildIndex].Prop('path') = LOperation.Path.Name then
              begin
                LRule := LNode.Children[LChildIndex];
                Break;
              end;
            end;

            if (LRule <> nil) and (LRule.ID <> LOperation.ID) then
            begin
              raise ENyxModel.Create('The exact part descriptor identity changed');
            end;

            if LOperation.Operation = doInherit then
            begin

              if LRule = nil then
              begin
                raise ENyxModel.Create('Restored inheritance requires an existing local part descriptor');
              end;
              LNode.Remove(LRule);
            end
            else
            begin

              if (LRule = nil) and (Result.Find(LOperation.ID) <> nil) then
              begin
                raise ENyxModel.Create('Part descriptor identity is already occupied');
              end;
              LNode.OverridePart(LOperation.Path, LOperation.Mode).Named(LOperation.ID);
            end;
          end;
        doCreate:
          begin

            if Result.Find(LOperation.ID) <> nil then
            begin
              raise ENyxModel.Create('Component ID is already in use: ' + LOperation.ID);
            end;
            LNode := ACatalog.NewNode(LOperation.Kind, LOperation.ID);
            try
              if LOperation.Root = 'page' then
              begin
                Result.AddPage(LNode);
              end
              else if LOperation.Root = 'component' then
              begin
                Result.AddComponent(LNode);
              end
              else
              begin
                LParent := RequireNode(Result, LOperation.Parent);
                LInsert := LOperation.Index;

                if LInsert < 0 then
                begin
                  LInsert := LParent.Count;
                end;
                LParent.Insert(LInsert, LNode);
              end;
              LNode := nil;
              { Ownership transfers before metadata lookup so declared ancestor
                field domains and reusable-part context participate in admission.
                Failure releases the entire candidate, never the accepted tree. }
              ConfigureNode(RequireNode(Result, LOperation.ID), Result,
                LOperation.Properties);
            finally
              LNode.Free;
            end;
          end;
        doUpdate: ConfigureNode(RequireNode(Result, LOperation.ID), Result,
          LOperation.Properties);
        doMove, doDelete:
          begin
            LNode := RequireNode(Result, LOperation.ID);

            if LNode.Parent = nil then
            begin
              raise ENyxModel.Create('Page/component roots cannot be moved or deleted by a control operation');
            end;

            if LOperation.Operation = doDelete then
            begin
              LNode.Parent.Remove(LNode);
            end
            else
            begin
              LParent := RequireNode(Result, LOperation.Parent);
              LAncestor := LParent;
              while LAncestor <> nil do
              begin

                if LAncestor = LNode then
                begin
                  raise ENyxModel.Create('Moving a component would create a cycle');
                end;
                LAncestor := LAncestor.Parent;
              end;
              LChildIndex := 0;
              while LNode.Parent.Children[LChildIndex] <> LNode do
              begin
                Inc(LChildIndex);
              end;
              LNode.Parent.Extract(LChildIndex);
              try
                LInsert := LOperation.Index;

                if LInsert < 0 then
                begin
                  LInsert := LParent.Count;
                end;
                LParent.Insert(LInsert, LNode);
                LNode := nil;
              finally
                LNode.Free;
              end;
            end;
          end;
        doTitle: Result.Title := LOperation.ID;
        doTokens: SetNyxDesignTokens(Result, LOperation.Properties);
      end;
    end;
    Result.Validate;
    ValidateNyxDocumentProperties(Result);
  except
    Result.Free;
    Result := nil;
    raise;
  end;
end;

end.
