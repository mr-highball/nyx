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

unit nyx.http.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.model, nyx.controls, nyx.http.types;

type
  { Independent unfinished request fields and admitted header rows. Malformed
    numbers/body text remain drafts until CaptureNyxHTTPRequestEditor validates
    a complete request. No document, transport or renderer is retained. }
  TNyxHTTPRequestEditorDraft = record
  private
    FData: TNyxDataValue;
  public
    procedure Capture(const AID: TNyxText; ARoot: TNyxNode);
    function Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
    procedure Clear;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxHTTPRequestEditorDraft; static;
    function Defined: Boolean;
  end;

{ Public Nyx compound form with typed method choices, explicit extension method,
  UTF-8/Base64 body choice, numeric limits and individually editable header rows.
  It edits descriptions only: no request is sent and no host authority is held. }
function NewNyxHTTPRequestEditor(const AID: TNyxText;
  const ARequest: TNyxHTTPRequest): INyxCard;
procedure LoadNyxHTTPRequestEditor(const AID: TNyxText; ARoot: TNyxNode;
  const ARequest: TNyxHTTPRequest);
function CaptureNyxHTTPRequestEditor(const AID: TNyxText;
  ARoot: TNyxNode): TNyxHTTPRequest;
{ Local header/method/body actions return True for an ordinary renderer Sync.
  Invalid edits raise before changing admitted headers or any design document. }
function HandleNyxHTTPRequestEditor(ANode, ARoot: TNyxNode): Boolean;

implementation

uses nyx.bytes, nyx.resource.sources, nyx.flow.types;

const
  CEditor = 'nyx.http-editor';
  CBodyKind = 'nyx.http-body-kind';
  CFields: array[0..9] of TNyxText =
    ('url', 'method', 'custom-method', 'body-kind', 'body', 'deadline', 'maximum',
     'header', 'header-name', 'header-value');

function Form(const AID: TNyxText; ARoot: TNyxNode): TNyxNode;
begin

  if ARoot = nil then
  begin
    raise ENyxBytes.Create('HTTP request editor is unavailable');
  end;
  Result := ARoot.Find(AID);

  if (Result = nil) or not Result.Extensions.Has(NyxExtension(CEditor)) then
  begin
    raise ENyxBytes.Create('HTTP request editor is unavailable');
  end;
end;

function Field(AForm: TNyxNode; const AName: TNyxText): TNyxNode;
begin
  Result := AForm.Find(AForm.ID + '-' + AName);

  if Result = nil then
  begin
    raise ENyxBytes.Create('HTTP request field is unavailable');
  end;
end;

function Value(AForm: TNyxNode; const AName: TNyxText): TNyxText;
begin
  Result := Field(AForm, AName).Prop('value');
end;

procedure SetValue(AForm: TNyxNode; const AName, AValue: TNyxText);
begin
  Field(AForm, AName).Configure.Value(AValue).Done;
end;

function Headers(AForm: TNyxNode): TNyxHTTPHeaders;
begin
  Result := TNyxHTTPHeaders.FromData(AForm.Extensions.Value(NyxExtension(CEditor)));
end;

procedure PublishHeaders(AForm: TNyxNode; const AHeaders: TNyxHTTPHeaders);
var
  LItems: TNyxText;
  LFirst: TNyxText;
  LIndex: Integer;
begin
  LItems := '';
  LFirst := '';
  for LIndex := 0 to AHeaders.Count - 1 do
  begin

    if LIndex > 0 then
    begin
      LItems := LItems + #10;
    end;
    LItems := LItems + TNyxText(IntToStr(LIndex + 1)) + ' / ' + AHeaders.Name(LIndex);

    if LIndex = 0 then
    begin
      LFirst := LItems;
    end;
  end;
  AForm.Extensions.SetValue(NyxExtension(CEditor), AHeaders.ToData);
  Field(AForm, 'header').Configure.Items(LItems).Value(LFirst).Done;
end;

function SelectedHeader(AForm: TNyxNode): Integer;
var
  LText: TNyxText;
  LSlash: Integer;
begin
  LText := Value(AForm, 'header');
  LSlash := Pos(' / ', LText);

  if (LSlash < 2) or not TryStrToInt(Copy(LText, 1, LSlash - 1), Result) or
    (Result < 1) or (Result > Headers(AForm).Count) then
  begin
    raise ENyxBytes.Create('Choose an exact saved header row');
  end;
  Dec(Result);
end;

procedure ShowChoices(AForm: TNyxNode);
begin
  Field(AForm, 'custom-method').Configure.Visible(Value(AForm, 'method') = 'Custom').Done;
end;

function CaptureNyxHTTPRequestEditor(const AID: TNyxText;
  ARoot: TNyxNode): TNyxHTTPRequest;
var
  LForm: TNyxNode;
  LMethod: TNyxHTTPMethod;
  LFound: Boolean;
  LDeadline: Integer;
  LMaximum: Integer;
  LHeaders: TNyxHTTPHeaders;
  LIndex: Integer;
begin
  LForm := Form(AID, ARoot);
  Result := NyxHTTPRequest(NyxResourceURL(Value(LForm, 'url')));
  LFound := False;
  for LMethod := Low(TNyxHTTPMethod) to High(TNyxHTTPMethod) do
  begin

    if Value(LForm, 'method') = NyxHTTPMethod(LMethod).Name then
    begin
      Result := Result.Method(LMethod);
      LFound := True;
      Break;
    end;
  end;

  if not LFound then
  begin

    if Value(LForm, 'method') <> 'Custom' then
    begin
      raise ENyxBytes.Create('Choose a typed HTTP method');
    end;
    Result := Result.Method(NyxHTTPMethod(Value(LForm, 'custom-method')));
  end;

  if Value(LForm, 'body-kind') = 'UTF-8 text' then
  begin
    Result := Result.WithBody(NyxEncodeUTF8(Value(LForm, 'body')));
  end
  else if Value(LForm, 'body-kind') = 'Base64 bytes' then
  begin
    Result := Result.WithBody(NyxDecodeBase64(Value(LForm, 'body')));
  end
  else
  begin
    raise ENyxBytes.Create('Choose UTF-8 text or Base64 bytes');
  end;

  if not TryStrToInt(Value(LForm, 'deadline'), LDeadline) or
    not TryStrToInt(Value(LForm, 'maximum'), LMaximum) then
  begin
    raise ENyxBytes.Create('HTTP deadline and byte limit must be integers');
  end;
  Result := Result.WholeRequest(LDeadline).MaximumBytes(LMaximum);
  LHeaders := Headers(LForm);
  for LIndex := 0 to LHeaders.Count - 1 do
  begin
    Result := Result.AppendHeader(LHeaders.Name(LIndex), LHeaders.Value(LIndex));
  end;
end;

procedure LoadNyxHTTPRequestEditor(const AID: TNyxText; ARoot: TNyxNode;
  const ARequest: TNyxHTTPRequest);
var
  LForm: TNyxNode;
  LRequest: TNyxHTTPRequest;
  LMethod: TNyxHTTPMethod;
  LText: TNyxText;
  LTextBody: Boolean;
begin
  LForm := Form(AID, ARoot);
  LRequest := TNyxHTTPRequest.FromData(ARequest.ToData);
  SetValue(LForm, 'url', LRequest.URL.Address);
  SetValue(LForm, 'method', 'Custom');
  SetValue(LForm, 'custom-method', LRequest.RequestMethod.Name);
  for LMethod := Low(TNyxHTTPMethod) to High(TNyxHTTPMethod) do
  begin

    if LRequest.RequestMethod.Name = NyxHTTPMethod(LMethod).Name then
    begin
      SetValue(LForm, 'method', NyxHTTPMethod(LMethod).Name);
      Break;
    end;
  end;
  SetValue(LForm, 'deadline', IntToStr(LRequest.DeadlineMS));
  SetValue(LForm, 'maximum', IntToStr(LRequest.ByteLimit));
  LTextBody := False;
  try
    LText := NyxDecodeUTF8(LRequest.Body);
    { Native text widgets cannot display embedded NUL reliably. Preserve such
      bodies through the explicit byte presentation, without narrowing bytes. }
    LTextBody := Pos(#0, LText) = 0;
  except
    on E: ENyxBytes do
    begin
      LTextBody := False;
    end;
  end;

  if LTextBody then
  begin
    SetValue(LForm, 'body-kind', 'UTF-8 text');
    SetValue(LForm, 'body', LText);
  end
  else
  begin
    SetValue(LForm, 'body-kind', 'Base64 bytes');
    SetValue(LForm, 'body', NyxEncodeBase64(LRequest.Body));
  end;
  PublishHeaders(LForm, LRequest.Headers);
  LForm.Extensions.SetValue(NyxExtension(CBodyKind), NyxData(Value(LForm, 'body-kind')));
  SetValue(LForm, 'header-name', '');
  SetValue(LForm, 'header-value', '');
  ShowChoices(LForm);
end;

function NewNyxHTTPRequestEditor(const AID: TNyxText;
  const ARequest: TNyxHTTPRequest): INyxCard;

  procedure Input(const AName, ACaption: TNyxText);
  begin
    Result.Add(NewNyxInput(AID + '-' + AName).Configure.Text(ACaption).Done);
  end;

  procedure Choice(const AName, ACaption, AItems: TNyxText);
  begin
    Result.Add(NewNyxSelect(AID + '-' + AName).Configure.Text(ACaption).Items(AItems).Done);
  end;

  procedure Button(const AName, ACaption: TNyxText);
  begin
    Result.Add(NewNyxButton(AID + '-' + AName).WithText(ACaption));
  end;

begin
  ARequest.Validate;
  Result := NewNyxCard(AID);
  Result.Configure.Padding(12).Gap(10).Done;
  Result.Node.Extensions.SetValue(NyxExtension(CEditor), NyxHTTPHeaders.ToData);
  Input('url', 'HTTP or HTTPS URL');
  Choice('method', 'HTTP method', 'GET' + #10 + 'HEAD' + #10 + 'POST' + #10 +
    'PUT' + #10 + 'PATCH' + #10 + 'DELETE' + #10 + 'OPTIONS' + #10 + 'Custom');
  Input('custom-method', 'Extension method token');
  Choice('body-kind', 'Request body', 'UTF-8 text' + #10 + 'Base64 bytes');
  Result.Add(NewNyxMemo(AID + '-body').Configure.Text('Body content').Done);
  Input('deadline', 'Whole request deadline (milliseconds)');
  Input('maximum', 'Maximum response bytes');
  Result.Add(NewNyxLabel(AID + '-headers-help').WithText(
    'Edit request headers individually. Host-managed headers are reported before applying.'));
  Choice('header', 'Saved header', '');
  Input('header-name', 'Header name');
  Input('header-value', 'Header value');
  Button('header-load', 'Load header');
  Button('header-set', 'Add or replace header');
  Button('header-remove', 'Remove selected header');
  Result.Add(NewNyxLabel(AID + '-authority').WithText(
    'Designing a request sends nothing. The running application supplies its transport.'));
  LoadNyxHTTPRequestEditor(AID, Result.Node, ARequest);
end;

function HandleNyxHTTPRequestEditor(ANode, ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LHeaders: TNyxHTTPHeaders;
  LCandidate: TNyxHTTPHeaders;
  LSelected: Integer;
  LIndex: Integer;
  LPrevious: TNyxText;
  LNext: TNyxText;
  LBody: TNyxBytes;
  LText: TNyxText;
begin
  Result := False;

  if (ANode = nil) or (ARoot = nil) or (ARoot.Find(ANode.ID) <> ANode) then
  begin
    Exit;
  end;
  LForm := ANode;
  while (LForm <> nil) and not LForm.Extensions.Has(NyxExtension(CEditor)) do
  begin
    LForm := LForm.Parent;
  end;

  if LForm = nil then
  begin
    Exit;
  end;

  if ANode.ID = LForm.ID + '-method' then
  begin
    ShowChoices(LForm);
    Exit(True);
  end;

  if ANode.ID = LForm.ID + '-body-kind' then
  begin
    LPrevious := LForm.Extensions.Value(NyxExtension(CBodyKind)).AsText;
    LNext := Value(LForm, 'body-kind');
    try

      if LPrevious = 'UTF-8 text' then
      begin
        LBody := NyxEncodeUTF8(Value(LForm, 'body'));
      end
      else if LPrevious = 'Base64 bytes' then
      begin
        LBody := NyxDecodeBase64(Value(LForm, 'body'));
      end
      else
      begin
        raise ENyxBytes.Create('Current body presentation is unavailable');
      end;

      if LNext = 'Base64 bytes' then
      begin
        LText := NyxEncodeBase64(LBody);
      end
      else if LNext = 'UTF-8 text' then
      begin
        LText := NyxDecodeUTF8(LBody);

        if Pos(#0, LText) > 0 then
        begin
          raise ENyxBytes.Create('Keep Base64 presentation for embedded NUL bytes');
        end;
      end
      else
      begin
        raise ENyxBytes.Create('Choose UTF-8 text or Base64 bytes');
      end;
    except
      SetValue(LForm, 'body-kind', LPrevious);
      raise;
    end;
    SetValue(LForm, 'body', LText);
    LForm.Extensions.SetValue(NyxExtension(CBodyKind), NyxData(LNext));
    Exit(True);
  end;
  LHeaders := Headers(LForm);

  if ANode.ID = LForm.ID + '-header-load' then
  begin
    LSelected := SelectedHeader(LForm);
    SetValue(LForm, 'header-name', LHeaders.Name(LSelected));
    SetValue(LForm, 'header-value', LHeaders.Value(LSelected));
    Exit(True);
  end;

  if ANode.ID = LForm.ID + '-header-set' then
  begin
    ValidateNyxHTTPRequestHeader(Value(LForm, 'header-name'), Value(LForm, 'header-value'));
    LCandidate := LHeaders.SetValue(Value(LForm, 'header-name'), Value(LForm, 'header-value'));
    PublishHeaders(LForm, LCandidate);
    Exit(True);
  end;

  if ANode.ID = LForm.ID + '-header-remove' then
  begin
    LSelected := SelectedHeader(LForm);
    LCandidate := NyxHTTPHeaders;
    for LIndex := 0 to LHeaders.Count - 1 do
    begin

      if LIndex <> LSelected then
      begin
        LCandidate := LCandidate.Append(LHeaders.Name(LIndex), LHeaders.Value(LIndex));
      end;
    end;
    PublishHeaders(LForm, LCandidate);
    Exit(True);
  end;
end;

function TNyxHTTPRequestEditorDraft.Defined: Boolean;
begin
  Result := FData.Defined and (FData.Kind = ndObject);
end;

procedure TNyxHTTPRequestEditorDraft.Clear;
begin
  FData := NyxNull;
end;

function TNyxHTTPRequestEditorDraft.ToData: TNyxDataValue;
begin
  Result := NyxNull;

  if Defined then
  begin
    Result := FData.Copy;
  end;
end;

class function TNyxHTTPRequestEditorDraft.FromData(const AData: TNyxDataValue): TNyxHTTPRequestEditorDraft;
var
  LIndex: Integer;
begin
  Result := Default(TNyxHTTPRequestEditorDraft);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  ValidateNyxFlowFields(AData, ['version', 'values', 'headers']);

  if AData.Field('version').ToJSON <> '1' then
  begin
    raise ENyxBytes.Create('Unknown HTTP request draft version');
  end;

  if (AData.Field('values').Kind <> ndObject) or
    (AData.Field('values').Count <> Length(CFields)) then
  begin
    raise ENyxBytes.Create('HTTP request draft fields are incomplete');
  end;
  for LIndex := 0 to High(CFields) do
  begin
    AData.Field('values').Field(CFields[LIndex]).AsText;
  end;
  TNyxHTTPHeaders.FromData(AData.Field('headers'));
  Result.FData := AData.Copy;
end;

procedure TNyxHTTPRequestEditorDraft.Capture(const AID: TNyxText; ARoot: TNyxNode);
var
  LForm: TNyxNode;
  LFields: array of TNyxDataField;
  LIndex: Integer;
begin
  LForm := Form(AID, ARoot);
  SetLength(LFields, Length(CFields));
  for LIndex := 0 to High(CFields) do
  begin
    LFields[LIndex] := NyxField(CFields[LIndex], NyxData(Value(LForm, CFields[LIndex])));
  end;
  FData := NyxObject([NyxField('version', NyxData(1)),
    NyxField('values', NyxObject(LFields)), NyxField('headers', Headers(LForm).ToData)]);
end;

function TNyxHTTPRequestEditorDraft.Restore(const AID: TNyxText; ARoot: TNyxNode): Boolean;
var
  LForm: TNyxNode;
  LIndex: Integer;
begin
  Result := False;

  if not Defined then
  begin
    Exit;
  end;
  LForm := Form(AID, ARoot);
  PublishHeaders(LForm, TNyxHTTPHeaders.FromData(FData.Field('headers')));
  for LIndex := 0 to High(CFields) do
  begin
    SetValue(LForm, CFields[LIndex], FData.Field('values').Field(CFields[LIndex]).AsText);
  end;
  ShowChoices(LForm);
  LForm.Extensions.SetValue(NyxExtension(CBodyKind), NyxData(Value(LForm, 'body-kind')));
  Result := True;
end;

end.
