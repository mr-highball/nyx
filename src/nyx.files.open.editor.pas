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

unit nyx.files.open.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.files.open;

type
  { Owned unfinished selection-intent fields, including invalid budgets. The
    draft copies text only; no picker, selected file or runtime host is retained.
    Restore does not admit these values as executable file access. }
  TNyxFileOpenEditorDraft = record
  private
    FData: TNyxDataValue;
  public
    procedure Clear;
    function Defined: Boolean;
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode;
      AIncludePristine: Boolean = True);
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxFileOpenEditorDraft; static;
  end;

{ Public Nyx form for a single bounded byte snapshot. Extension hints are
  suggestions; no path, browser handle, host permission or encoding is saved.
  Capture validates the whole independent value without changing the form. }
function NewNyxFileOpenEditor(const AID: TNyxText;
  const AOptions: TNyxFileOpenOptions): INyxCard;
procedure LoadNyxFileOpenEditor(const AID: TNyxText; ARoot: TNyxNode;
  const AOptions: TNyxFileOpenOptions);
function CaptureNyxFileOpenEditor(const AID: TNyxText;
  ARoot: TNyxNode): TNyxFileOpenOptions;

implementation

uses nyx.files, nyx.flow.types, nyx.bytes;

const
  CEditor = 'nyx.file-open-editor';
  CFields: array[0..2] of TNyxText = ('title', 'extensions', 'maximum');

function Form(const AID: TNyxText; ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if ARoot <> nil then
  begin
    Result := ARoot.Find(AID);
  end;

  if (Result = nil) or not Result.Extensions.Has(NyxExtension(CEditor)) then
  begin
    raise ENyxFile.Create('File-open editor is unavailable');
  end;
end;

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxFile.Create('File-open editor field is unavailable');
  end;
end;

function RawValues(AForm: TNyxNode): TNyxDataValue;
var
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  SetLength(LFields, Length(CFields));
  for LIndex := 0 to High(CFields) do
  begin
    LFields[LIndex] := NyxField(CFields[LIndex],
      NyxData(Field(AForm, CFields[LIndex]).Prop('value')));
  end;
  Result := NyxObject(LFields);
end;

procedure LoadNyxFileOpenEditor(const AID: TNyxText; ARoot: TNyxNode;
  const AOptions: TNyxFileOpenOptions);
var
  LForm: TNyxNode;
  LExtensions: TNyxText;
  LIndex: Integer;
begin
  AOptions.Validate;
  LForm := Form(AID, ARoot);
  Field(LForm, 'title').Configure.Value(AOptions.DialogTitle).Done;
  Field(LForm, 'maximum').Configure.Value(IntToStr(AOptions.ByteLimit)).Done;
  LExtensions := '';
  for LIndex := 0 to AOptions.ExtensionCount - 1 do
  begin

    if LIndex > 0 then
    begin
      LExtensions := LExtensions + ', ';
    end;
    LExtensions := LExtensions + AOptions.Extension(LIndex);
  end;
  Field(LForm, 'extensions').Configure.Value(LExtensions).Done;
end;

function CaptureNyxFileOpenEditor(const AID: TNyxText;
  ARoot: TNyxNode): TNyxFileOpenOptions;
var
  LForm: TNyxNode;
  LMaximum: Integer;
  LText: TNyxText;
  LStart: Integer;
  LIndex: Integer;
begin
  LForm := Form(AID, ARoot);
  Result := NyxFileOpenOptions.Title(Field(LForm, 'title').Prop('value'));

  if not TryStrToInt(Field(LForm, 'maximum').Prop('value'), LMaximum) then
  begin
    raise ENyxFile.Create('The file byte budget must be an integer');
  end;
  Result := Result.MaximumBytes(LMaximum);
  LText := Field(LForm, 'extensions').Prop('value');
  LStart := 1;
  for LIndex := 1 to Length(LText) + 1 do
  begin

    if (LIndex = Length(LText) + 1) or (LText[LIndex] = ',') then
    begin

      if Trim(Copy(LText, LStart, LIndex - LStart)) <> '' then
      begin
        Result := Result.Allow(Trim(Copy(LText, LStart, LIndex - LStart)));
      end;
      LStart := LIndex + 1;
    end;
  end;
end;

function NewNyxFileOpenEditor(const AID: TNyxText;
  const AOptions: TNyxFileOpenOptions): INyxCard;
begin
  AOptions.Validate;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), NyxNull);
  Result.Add(NewNyxInput(AID + '-title').WithText('Dialog title'));
  Result.Add(NewNyxInput(AID + '-extensions').WithText('Suggested extensions, separated by commas'));
  Result.Add(NewNyxInput(AID + '-maximum').WithText('Maximum bytes'));
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'Choose one file through an explicitly configured runtime host. ' +
    'Read returns complete bytes and metadata; decoding is a separate operation. ' +
    'Cancellation and unavailable access remain distinct outcomes.'));
  LoadNyxFileOpenEditor(AID, Result.Node, AOptions);
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), RawValues(Result.Node));
end;

procedure TNyxFileOpenEditorDraft.Clear;
begin
  FData := NyxNull;
end;

function TNyxFileOpenEditorDraft.Defined: Boolean;
begin
  Result := FData.Kind = ndObject;
end;

procedure TNyxFileOpenEditorDraft.Capture(const AID: TNyxText;
  ARoot: TNyxNode; AIncludePristine: Boolean);
var
  LForm: TNyxNode;
  LValues: TNyxDataValue;
begin
  Clear;

  if (ARoot = nil) or (ARoot.Find(AID) = nil) then
  begin
    Exit;
  end;
  LForm := Form(AID, ARoot);
  LValues := RawValues(LForm);

  if not AIncludePristine and
    (LValues.ToJSON = LForm.Extensions.Value(NyxExtension(CEditor)).ToJSON) then
  begin
    Exit;
  end;
  FData := NyxObject([NyxField('version', NyxData(1)), NyxField('values', LValues)]);
end;

function TNyxFileOpenEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LIndex: Integer;
begin
  Result := False;

  if not Defined or (ARoot = nil) or (ARoot.Find(AID) = nil) then
  begin
    Exit;
  end;
  LForm := Form(AID, ARoot);
  for LIndex := 0 to High(CFields) do
  begin
    Field(LForm, CFields[LIndex]).Configure.Value(
      FData.Field('values').Field(CFields[LIndex]).AsText).Done;
  end;
  Result := True;
end;

function TNyxFileOpenEditorDraft.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if Defined then
  begin
    Result := FData.Copy;
  end;
end;

class function TNyxFileOpenEditorDraft.FromData(
  const AData: TNyxDataValue): TNyxFileOpenEditorDraft;
var
  LIndex: Integer;
begin
  Result.Clear;

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  ValidateNyxFlowFields(AData, ['version', 'values']);

  if AData.Field('version').AsInteger <> 1 then
  begin
    raise ENyxFile.Create('Unknown file-open draft version');
  end;
  ValidateNyxFlowFields(AData.Field('values'), CFields);
  for LIndex := 0 to High(CFields) do
  begin

    if NyxUTF8ByteCount(AData.Field('values').Field(CFields[LIndex]).AsText) > 8192 then
    begin
      raise ENyxFile.Create('File-open draft field exceeds its text budget');
    end;
  end;
  Result.FData := AData.Copy;
end;

end.
