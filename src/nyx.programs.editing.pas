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

unit nyx.programs.editing;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.data, nyx.model, nyx.programs.declarations;

type
  { Closed replacement/removal intent; related graph/reference changes belong
    to the same candidate command and complete document admission. }
  TNyxProgramChangeKind = (npcgDefine, npcgRemove);

  { Immutable copied authoring request. Definition bytes contain only admitted
    source/contract/limits, never executable runtime authority or a document.
    Apply touches a caller-owned candidate; accepted history publishes later. }
  TNyxProgramChange = record
  private
    FDefined: Boolean;
    FKind: TNyxProgramChangeKind;
    FReference: TNyxProgramRef;
    FDefinition: TNyxDataValue;
  public
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxProgramChange; static;
    procedure Apply(ADocument: TNyxDocument);
    property Defined: Boolean read FDefined;
    property Kind: TNyxProgramChangeKind read FKind;
    property Reference: TNyxProgramRef read FReference;
  end;

function NyxDefineProgram(const AReference: TNyxProgramRef;
  const ADefinition: INyxProgramDefinition): TNyxProgramChange;
function NyxRemoveProgram(const AReference: TNyxProgramRef): TNyxProgramChange;

implementation

uses nyx.flow.types;

function NyxRemoveProgram(const AReference: TNyxProgramRef): TNyxProgramChange;
begin
  Result := Default(TNyxProgramChange);
  Result.FDefined := True;
  Result.FKind := npcgRemove;
  Result.FReference := NyxProgramRef(AReference.Name);
end;

function NyxDefineProgram(const AReference: TNyxProgramRef;
  const ADefinition: INyxProgramDefinition): TNyxProgramChange;
var
  LInput: INyxProgramDefinition;
begin

  if ADefinition = nil then
  begin
    raise ENyxModel.Create('A program definition change requires admitted metadata');
  end;
  Result := NyxRemoveProgram(AReference);
  Result.FKind := npcgDefine;
  { A creator getter may release the caller's const-interface variable. The
    copied name and this independent lease survive until admission completes. }
  LInput := ADefinition;
  Result.FDefinition := NyxProgramDefinitionFromData(LInput.ToData).ToData;
end;

function TNyxProgramChange.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    raise ENyxModel.Create('An uninitialized program change refuses');
  end;

  if FKind = npcgDefine then
  begin
    Result := NyxObject([NyxField('op', NyxData('program-define')),
      NyxField('name', NyxData(FReference.Name)), NyxField('definition', FDefinition)]);
  end
  else
  begin
    Result := NyxObject([NyxField('op', NyxData('program-remove')),
      NyxField('name', NyxData(FReference.Name))]);
  end;
end;

class function TNyxProgramChange.FromData(const AData: TNyxDataValue): TNyxProgramChange;
var
  LName: TNyxText;
begin
  LName := AData.Field('op').AsText;

  if LName = 'program-define' then
  begin
    ValidateNyxFlowFields(AData, ['op', 'name', 'definition']);
    Result := NyxDefineProgram(NyxProgramRef(AData.Field('name').AsText),
      NyxProgramDefinitionFromData(AData.Field('definition')));
  end
  else if LName = 'program-remove' then
  begin
    ValidateNyxFlowFields(AData, ['op', 'name']);
    Result := NyxRemoveProgram(NyxProgramRef(AData.Field('name').AsText));
  end
  else
  begin
    raise ENyxModel.Create('Unknown typed program change');
  end;
end;

procedure TNyxProgramChange.Apply(ADocument: TNyxDocument);
begin

  if not FDefined or (ADocument = nil) then
  begin
    raise ENyxModel.Create('Program changes require an owned candidate document');
  end;

  if FKind = npcgDefine then
  begin
    ADocument.Programs.Define(FReference, NyxProgramDefinitionFromData(FDefinition));
  end
  else
  begin
    ADocument.Programs.Remove(FReference);
  end;
end;

end.
