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

unit nyx.files.save.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.files.save;

type
  { Owned unfinished field values, including invalid budgets or names. Restoring
    a draft never grants file access or validates it as admitted save options.
    No form, document, renderer or borrowed callback is retained. }
  TNyxFileSaveEditorDraft = record
  private
    FData: TNyxDataValue;
  public
    procedure Clear;
    function Defined: Boolean;
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode;
      AIncludePristine: Boolean = True);
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxFileSaveEditorDraft; static;
  end;

{ Reusable public Nyx form for portable save intent. Closed mode/activation
  choices, a Boolean filter choice and numeric budget replace raw configuration
  JSON. Extensions are comma-separated leaf suffixes, not paths or permissions.
  Host/directory/handle authority is never accepted or retained by this form. }
function NewNyxFileSaveEditor(const AID: TNyxText;
  const AOptions: TNyxFileSaveOptions): INyxCard;
procedure LoadNyxFileSaveEditor(const AID: TNyxText; ARoot: TNyxNode;
  const AOptions: TNyxFileSaveOptions);
{ Validate all fields into one independent options value or raise ENyxFile.
  A refused capture preserves every unfinished input and any existing design. }
function CaptureNyxFileSaveEditor(const AID: TNyxText;
  ARoot: TNyxNode): TNyxFileSaveOptions;

implementation

uses nyx.files, nyx.flow.types, nyx.bytes;

const
  CEditor = 'nyx.file-save-editor';
  CFields: array[0..7] of TNyxText =
    ('name', 'title', 'mode', 'media', 'extensions', 'all-types', 'maximum', 'activation');
  CModes: array[TNyxFileSaveMode] of TNyxText = ('Prefer picker', 'Picker', 'Download');
  CActivation: array[TNyxDownloadActivation] of TNyxText = ('Require user action', 'Platform policy');

function Form(const AID: TNyxText; ARoot: TNyxNode): TNyxNode;
begin
  Result := nil;

  if ARoot <> nil then
  begin
    Result := ARoot.Find(AID);
  end;

  if (Result = nil) or not Result.Extensions.Has(NyxExtension(CEditor)) then
  begin
    raise ENyxFile.Create('File-save editor is unavailable');
  end;
end;

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxFile.Create('File-save editor field is unavailable');
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
    LFields[LIndex] := NyxField(CFields[LIndex], NyxData(Field(AForm, CFields[LIndex]).Prop('value')));
  end;
  Result := NyxObject(LFields);
end;

procedure LoadNyxFileSaveEditor(const AID: TNyxText; ARoot: TNyxNode;
  const AOptions: TNyxFileSaveOptions);
var
  LForm: TNyxNode;
  LExtensions: TNyxText;
  LIndex: Integer;
begin
  AOptions.Validate;
  LForm := Form(AID, ARoot);
  Field(LForm, 'name').Configure.Value(AOptions.SuggestedName).Done;
  Field(LForm, 'title').Configure.Value(AOptions.DialogTitle).Done;
  Field(LForm, 'mode').Configure.Value(CModes[AOptions.SaveMode]).Done;
  Field(LForm, 'media').Configure.Value(AOptions.ContentType.Name).Done;
  Field(LForm, 'all-types').Configure.Value(AOptions.IncludeAllTypes).Done;
  Field(LForm, 'maximum').Configure.Value(IntToStr(AOptions.ByteLimit)).Done;
  Field(LForm, 'activation').Configure.Value(CActivation[AOptions.ActivationPolicy]).Done;
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

function CaptureNyxFileSaveEditor(const AID: TNyxText;
  ARoot: TNyxNode): TNyxFileSaveOptions;
var
  LForm: TNyxNode;
  LMode: TNyxFileSaveMode;
  LActivation: TNyxDownloadActivation;
  LFound: Boolean;
  LMaximum: Integer;
  LText: TNyxText;
  LStart: Integer;
  LIndex: Integer;
begin
  LForm := Form(AID, ARoot);
  Result := NyxFileSaveOptions(Field(LForm, 'name').Prop('value'))
    .Title(Field(LForm, 'title').Prop('value'))
    .MediaType(NyxMediaType(Field(LForm, 'media').Prop('value')));
  LFound := False;
  for LMode := Low(TNyxFileSaveMode) to High(TNyxFileSaveMode) do
  begin

    if Field(LForm, 'mode').Prop('value') = CModes[LMode] then
    begin
      Result := Result.Mode(LMode);
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxFile.Create('Choose a save mode');
  end;
  LFound := False;
  for LActivation := Low(TNyxDownloadActivation) to High(TNyxDownloadActivation) do
  begin

    if Field(LForm, 'activation').Prop('value') = CActivation[LActivation] then
    begin
      Result := Result.DownloadActivation(LActivation);
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin
    raise ENyxFile.Create('Choose a download activation policy');
  end;

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
  LText := Field(LForm, 'all-types').Prop('value');

  if (LText <> 'true') and (LText <> 'false') then
  begin
    raise ENyxFile.Create('Choose whether to include all file types');
  end;
  Result := Result.AllTypes(LText = 'true');
end;

function NewNyxFileSaveEditor(const AID: TNyxText;
  const AOptions: TNyxFileSaveOptions): INyxCard;

  procedure Input(const AName, ACaption: TNyxText);
  begin
    Result.Add(NewNyxInput(AID + '-' + AName).WithText(ACaption));
  end;

begin
  AOptions.Validate;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), NyxNull);
  Input('name', 'Suggested filename');
  Input('title', 'Dialog title');
  Result.Add(NewNyxSelect(AID + '-mode').Configure.Text('Save mode')
    .Items(CModes[nfsmPreferPicker] + #10 + CModes[nfsmPicker] + #10 + CModes[nfsmDownload]).Done);
  Input('media', 'Media type (MIME)');
  Input('extensions', 'Extensions, separated by commas');
  Result.Add(NewNyxCheckbox(AID + '-all-types').WithText('Include all file types'));
  Input('maximum', 'Maximum bytes');
  Result.Add(NewNyxSelect(AID + '-activation').Configure.Text('Download activation')
    .Items(CActivation[nfdaRequired] + #10 + CActivation[nfdaPlatform]).Done);
  Result.Add(NewNyxLabel(AID + '-help').WithText(
    'A filename is a suggestion. A runtime host and user action control file access. ' +
    'Written means write and close completed; a download receipt records only the request.'));
  LoadNyxFileSaveEditor(AID, Result.Node, AOptions);
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), RawValues(Result.Node));
end;

procedure TNyxFileSaveEditorDraft.Clear;
begin
  FData := NyxNull;
end;

function TNyxFileSaveEditorDraft.Defined: Boolean;
begin
  Result := FData.Kind = ndObject;
end;

procedure TNyxFileSaveEditorDraft.Capture(const AID: TNyxText;
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

function TNyxFileSaveEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LIndex: Integer;
  LText: TNyxText;
begin
  Result := False;

  if not Defined or (ARoot = nil) or (ARoot.Find(AID) = nil) then
  begin
    Exit;
  end;
  LForm := Form(AID, ARoot);
  for LIndex := 0 to High(CFields) do
  begin
    LText := FData.Field('values').Field(CFields[LIndex]).AsText;

    if CFields[LIndex] = 'all-types' then
    begin
      Field(LForm, CFields[LIndex]).Configure.Value(LText = 'true').Done;
    end
    else
    begin
      Field(LForm, CFields[LIndex]).Configure.Value(LText).Done;
    end;
  end;
  Result := True;
end;

function TNyxFileSaveEditorDraft.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if Defined then
  begin
    Result := FData.Copy;
  end;
end;

class function TNyxFileSaveEditorDraft.FromData(
  const AData: TNyxDataValue): TNyxFileSaveEditorDraft;
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
    raise ENyxFile.Create('Unknown file-save draft version');
  end;
  ValidateNyxFlowFields(AData.Field('values'), CFields);
  for LIndex := 0 to High(CFields) do
  begin

    if NyxUTF8ByteCount(AData.Field('values').Field(CFields[LIndex]).AsText) > 8192 then
    begin
      raise ENyxFile.Create('File-save draft field exceeds its text budget');
    end;
  end;

  if (AData.Field('values').Field('all-types').AsText <> 'true') and
    (AData.Field('values').Field('all-types').AsText <> 'false') then
  begin
    raise ENyxFile.Create('File-save checkbox draft must be Boolean');
  end;
  Result.FData := AData.Copy;
end;

end.
