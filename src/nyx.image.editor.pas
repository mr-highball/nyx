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
unit nyx.image.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.types, nyx.images, nyx.model, nyx.controls;

type
  { Closed roles/actions of an ordinary reusable Nyx form. Import, Inline and Clear
    change a proposal; only Apply produces an accepted design command. }
  TNyxImageEditorField = (iefAlternative, iefFit, iefHorizontal, iefVertical,
    iefInlineFormat, iefInlineBase64);
  TNyxImageEditorAction = (ieaImport, ieaInline, ieaClear, ieaApply);

  { Copied exact-owner proposal. No document, renderer, mutable array or widget
    survives capture. ToData/FromData are the explicit processor wire boundary. }
  TNyxImageEditorChange = record
    Owner: TNyxText;
    Baseline: TNyxText;
    Source: TNyxImageSource;
    AlternativeText: TNyxText;
    Fit: TNyxImageFit;
    Horizontal: TNyxImageAnchor;
    Vertical: TNyxImageAnchor;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxImageEditorChange; static;
  end;

  { Unsubmitted input is retained verbatim, including invalid choices until
    Apply. Restore checks every field and the complete local/effective baseline
    before writing anything. Absent parked forms retain a draft; incompatible
    forms retire it. Caller-owned copies share only immutable strings. }
  TNyxImageEditorDraft = record
  private
    FEditor: TNyxText;
    FBaseline: TNyxText;
    FSource: TNyxImageSource;
    FValues: array[TNyxImageEditorField] of TNyxText;
    function GetDefined: Boolean;
  public
    procedure Capture(const AID: TNyxText; AShellRoot: TNyxNode);
    function Restore(AShellRoot: TNyxNode): Boolean;
    { Retains captured context/other fields for qualified imported/inline bytes. }
    procedure Propose(const ASource: TNyxImageSource);
    { Strict disposable preference boundary; null means no parked proposal. }
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxImageEditorDraft; static;
    procedure Clear;
    property Defined: Boolean read GetDefined;
  end;

{ Stable ordinary descendant identity for a closed field role. }
function NyxImageEditorFieldID(const AID: TNyxText;
  AField: TNyxImageEditorField): TNyxText;
{ Stable ordinary button identity; action admission still checks the live node. }
function NyxImageEditorActionID(const AID: TNyxText;
  AAction: TNyxImageEditorAction): TNyxText;
{ Includes the authored owner's exact property presence and effective projected
  values. An instance override cannot accidentally author its inherited node. }
function NyxImageEditorBaseline(AOwner, AProjection: TNyxNode): TNyxText;
{ Returns the copied exact baseline of a complete mounted image form. }
function NyxImageEditorContext(AEditor: TNyxNode): TNyxText;
{ Both arguments are borrowed during construction; the returned specialized
  card owns ordinary image/input/select/button descendants, with no back cycle. }
function NewNyxImageEditor(const AID: TNyxText;
  AOwner, AProjection: TNyxNode): INyxCard;
{ Recognizes only a current mounted button, never a matching detached identity. }
function NyxImageEditorAction(AButton, AShellRoot: TNyxNode;
  out AEditor: TNyxNode; out AAction: TNyxImageEditorAction): Boolean;
{ Typed import/clear writes only the mounted proposal and its preview. The caller
  must qualify its captured project/owner/baseline before asynchronous delivery. }
procedure ProposeNyxImageEditorSource(AEditor: TNyxNode;
  const ASource: TNyxImageSource);
{ Reads pasted canonical Base64 or a complete PNG/JPEG data URL. The selected
  closed format must match the payload. Refusal leaves the mounted preview and
  accepted pair untouched; the target may also qualify its actual pixel decoder. }
function ReadNyxImageEditorInline(AEditor: TNyxNode): TNyxImageSource;
{ False means another command. Invalid complete choices raise without changing
  any accepted model/history; the controller rechecks Baseline during admission. }
function CaptureNyxImageEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxImageEditorChange): Boolean;

implementation

uses nyx.layout.policy;

const
  CEditor = 'nyx.image-editor';
  COwner = 'nyx.image-editor.owner';
  CBaseline = 'nyx.image-editor.baseline';
  CFields: array[TNyxImageEditorField] of TNyxText =
    ('alternative', 'fit', 'horizontal', 'vertical', 'inline-format', 'inline-base64');
  CFormats: array[TNyxImageFormat] of TNyxText = ('PNG', 'JPEG');
  CAttributes: array[0..4] of TNyxAttribute =
    (atSource, atAlt, atImageFit, atImageHorizontal, atImageVertical);

function Complete(AEditor: TNyxNode): Boolean; forward;

function NyxImageEditorFieldID(const AID: TNyxText;
  AField: TNyxImageEditorField): TNyxText;
begin
  Result := AID + TNyxText('-') + CFields[AField];
end;

function NyxImageEditorContext(AEditor: TNyxNode): TNyxText;
begin

  if not Complete(AEditor) then
  begin
    raise ENyxModel.Create('Image context requires the complete current form');
  end;
  Result := AEditor.Prop(CBaseline);
end;

function NyxImageEditorActionID(const AID: TNyxText;
  AAction: TNyxImageEditorAction): TNyxText;
const
  CNames: array[TNyxImageEditorAction] of TNyxText = ('import', 'inline', 'clear', 'apply');
begin
  Result := AID + TNyxText('-') + CNames[AAction];
end;

function NyxImageEditorBaseline(AOwner, AProjection: TNyxNode): TNyxText;
var
  LLocal: array[0..4] of TNyxDataField;
  LEffective: array[0..4] of TNyxDataField;
  LIndex: Integer;
  LKey: TNyxText;
begin

  if (AOwner = nil) or (AProjection = nil) or
    (AProjection.ProjectionKind <> NyxKindName(nkImage)) then
  begin
    raise ENyxModel.Create('Image authoring requires its exact owner and image projection');
  end;
  for LIndex := 0 to High(CAttributes) do
  begin
    LKey := NyxAttributeName(CAttributes[LIndex]);
    LLocal[LIndex] := NyxField(LKey, NyxNull);

    if AOwner.Props.IndexOfName(LKey) >= 0 then
    begin
      LLocal[LIndex] := NyxField(LKey, NyxData(AOwner.Prop(LKey)));
    end;
    LEffective[LIndex] := NyxField(LKey, NyxData(AProjection.Prop(LKey)));
  end;
  Result := NyxObject([NyxField('owner', NyxData(AOwner.ID)),
    NyxField('kind', NyxData(AOwner.Kind)), NyxField('local', NyxObject(LLocal)),
    NyxField('effective', NyxObject(LEffective))]).ToJSON;
end;

function Complete(AEditor: TNyxNode): Boolean;
var
  LField: TNyxImageEditorField;
  LNode: TNyxNode;
  LKind: TNyxKind;
begin
  Result := False;

  if (AEditor = nil) or (AEditor.Kind <> NyxKindName(nkCard)) or
    (AEditor.Prop(CEditor) <> AEditor.ID) or (AEditor.Prop(COwner) = '') or
    (AEditor.Prop(CBaseline) = '') then
  begin
    Exit;
  end;
  LNode := AEditor.Find(AEditor.ID + TNyxText('-preview'));

  if (LNode = nil) or (LNode.Kind <> NyxKindName(nkImage)) or
    (AEditor.Find(AEditor.ID + TNyxText('-source-summary')) = nil) then
  begin
    Exit;
  end;
  for LField := Low(TNyxImageEditorField) to High(TNyxImageEditorField) do
  begin
    LNode := AEditor.Find(NyxImageEditorFieldID(AEditor.ID, LField));
    LKind := nkSelect;

    if LField = iefAlternative then
    begin
      LKind := nkInput;
    end;

    if LField = iefInlineBase64 then
    begin
      LKind := nkMemo;
    end;

    if (LNode = nil) or (LNode.Kind <> NyxKindName(LKind)) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

procedure ProposeNyxImageEditorSource(AEditor: TNyxNode;
  const ASource: TNyxImageSource);
var
  LSummary: TNyxText;
begin

  if not Complete(AEditor) then
  begin
    raise ENyxModel.Create('Image proposal requires the complete current form');
  end;
  LSummary := 'No image';
  case ASource.Kind of
    nisEmbedded:
      begin
        LSummary := 'Embedded image / ' + TNyxText(IntToStr(ASource.Width)) +
          TNyxText(' × ') + TNyxText(IntToStr(ASource.Height)) + TNyxText(' pixels');
      end;
    nisLocation:
      begin
        LSummary := 'External image reference';
      end;
    nisEmpty:
      begin
        { A deliberate empty proposal masks an inherited picture only on Apply. }
      end;
  end;
  AEditor.Find(AEditor.ID + TNyxText('-preview')).Configure.Source(ASource).Done;
  AEditor.Find(AEditor.ID + TNyxText('-source-summary')).Configure.Text(LSummary).Done;
end;

function ReadNyxImageEditorInline(AEditor: TNyxNode): TNyxImageSource;
var
  LFormat: TNyxImageFormat;
  LChoice: TNyxText;
  LPayload: TNyxText;
  LSource: TNyxImageSource;
begin

  if not Complete(AEditor) then
  begin
    raise ENyxModel.Create('Inline image requires the complete current form');
  end;
  LChoice := AEditor.Find(NyxImageEditorFieldID(AEditor.ID, iefInlineFormat)).Prop('value');

  if LChoice = CFormats[nimPNG] then
  begin
    LFormat := nimPNG;
  end
  else if LChoice = CFormats[nimJPEG] then
  begin
    LFormat := nimJPEG;
  end
  else
  begin
    raise ENyxImage.Create('Choose PNG or JPEG for the inline image');
  end;
  LPayload := AEditor.Find(NyxImageEditorFieldID(AEditor.ID, iefInlineBase64)).Prop('value');

  if Copy(LPayload, 1, 5) = 'data:' then
  begin
    LSource := TNyxImageSource.FromWire(LPayload);
  end
  else
  begin
    LSource := NyxEmbeddedImage(LFormat, LPayload);
  end;

  if (LSource.Kind <> nisEmbedded) or (LSource.Format <> LFormat) then
  begin
    raise ENyxImage.Create('Inline image must match the selected PNG or JPEG format');
  end;
  Result := LSource;
end;

function NewNyxImageEditor(const AID: TNyxText;
  AOwner, AProjection: TNyxNode): INyxCard;
const
  CLabels: array[TNyxImageEditorField] of TNyxText =
    ('Alternative text', 'Image fit', 'Horizontal anchor', 'Vertical anchor',
      'Inline image format', 'Inline Base64');
  CCaptions: array[TNyxImageEditorAction] of TNyxText =
    ('Import PNG or JPEG', 'Preview Base64', 'Clear image', 'Apply image');
var
  LField: TNyxImageEditorField;
  LAction: TNyxImageEditorAction;
  LInput: INyxControl;
  LButton: INyxButton;
  LItems: TNyxText;
  LFit: TNyxImageFit;
  LAnchor: TNyxImageAnchor;
  LBaseline: TNyxText;
begin
  LBaseline := NyxImageEditorBaseline(AOwner, AProjection);
  Result := NewNyxCard(AID);
  Result.Configure.Layout(TNyxLayoutPolicy.Column).Gap(10).Done;
  Result.Node.SetProp(CEditor, AID).SetProp(COwner, AOwner.ID)
    .SetProp(CBaseline, LBaseline);
  Result.Add(NewNyxHeading(AID + TNyxText('-title')).WithText('Image'));
  Result.Add(NewNyxLabel(AID + TNyxText('-source-summary')));
  Result.Add(NewNyxImage(AID + TNyxText('-preview')).Configure
    .Height(120).ImageFit(nifContain).AlternativeText('Image proposal preview').Done);
  for LField := Low(TNyxImageEditorField) to High(TNyxImageEditorField) do
  begin

    if LField = iefAlternative then
    begin
      LInput := NewNyxInput(NyxImageEditorFieldID(AID, LField));
      LInput.Configure.Value(AProjection.Prop(NyxAttributeName(atAlt)))
        .Hint('Describe the image purpose. Decorative images may use empty text.').Done;
    end
    else if LField = iefInlineBase64 then
    begin
      LInput := NewNyxMemo(NyxImageEditorFieldID(AID, LField));
      LInput.Configure.Height(96)
        .Hint('Paste canonical Base64 or a complete PNG/JPEG data URL, up to 1 MiB. Preview before Apply.').Done;
    end
    else
    begin
      LItems := '';

      if LField = iefInlineFormat then
      begin
        LItems := CFormats[nimPNG] + TNyxText(#10) + CFormats[nimJPEG];
      end
      else if LField = iefFit then
      begin
        for LFit := Low(TNyxImageFit) to High(TNyxImageFit) do
        begin

          if LItems <> '' then
          begin
            LItems := LItems + TNyxText(#10);
          end;
          LItems := LItems + NyxImageFitName(LFit);
        end;
      end
      else
      begin
        for LAnchor := Low(TNyxImageAnchor) to High(TNyxImageAnchor) do
        begin

          if LItems <> '' then
          begin
            LItems := LItems + TNyxText(#10);
          end;
          LItems := LItems + NyxImageAnchorName(LAnchor);
        end;
      end;
      LInput := NewNyxSelect(NyxImageEditorFieldID(AID, LField));
      LInput.Configure.Items(LItems).Done;
      case LField of
        iefFit:
          begin
            LInput.Configure.Value(NyxImageFitName(ReadNyxImageFit(
              AProjection.Prop(NyxAttributeName(atImageFit))))).Done;
          end;
        iefHorizontal:
          begin
            LInput.Configure.Value(NyxImageAnchorName(ReadNyxImageAnchor(
              AProjection.Prop(NyxAttributeName(atImageHorizontal))))).Done;
          end;
        iefVertical:
          begin
            LInput.Configure.Value(NyxImageAnchorName(ReadNyxImageAnchor(
              AProjection.Prop(NyxAttributeName(atImageVertical))))).Done;
          end;
        iefAlternative:
          begin
            { Its text input was constructed by the branch above. }
          end;
        iefInlineFormat:
          begin
            LInput.Configure.Value(CFormats[nimPNG]).Done;
          end;
        iefInlineBase64:
          begin
            { Its multiline input was constructed by the branch above. }
          end;
      end;
    end;
    LInput.Configure.Text(CLabels[LField]).AccessibleName(CLabels[LField]).Done;
    Result.Add(LInput);
  end;
  for LAction := Low(TNyxImageEditorAction) to High(TNyxImageEditorAction) do
  begin
    LButton := NewNyxButton(NyxImageEditorActionID(AID, LAction)).WithText(CCaptions[LAction]);
    LButton.Node.SetProp(CEditor, AID);
    Result.Add(LButton);
  end;
  ProposeNyxImageEditorSource(Result.Node,
    TNyxImageSource.FromWire(AProjection.Prop(NyxAttributeName(atSource))));
end;

function NyxImageEditorAction(AButton, AShellRoot: TNyxNode;
  out AEditor: TNyxNode; out AAction: TNyxImageEditorAction): Boolean;
var
  LAction: TNyxImageEditorAction;
begin
  Result := False;
  AEditor := nil;
  AAction := ieaImport;

  if (AButton = nil) or (AShellRoot = nil) or (AButton.Prop(CEditor) = '') then
  begin
    Exit;
  end;
  AEditor := AShellRoot.Find(AButton.Prop(CEditor));

  if not Complete(AEditor) then
  begin
    raise ENyxModel.Create('Image action requires its complete mounted form');
  end;
  for LAction := Low(TNyxImageEditorAction) to High(TNyxImageEditorAction) do
  begin

    if (AButton.ID = NyxImageEditorActionID(AEditor.ID, LAction)) and
      (AEditor.Find(AButton.ID) = AButton) and (AButton.Kind = NyxKindName(nkButton)) then
    begin
      AAction := LAction;
      Exit(True);
    end;
  end;
end;

function CaptureNyxImageEditor(AButton, AShellRoot: TNyxNode;
  out AChange: TNyxImageEditorChange): Boolean;
var
  LEditor: TNyxNode;
  LAction: TNyxImageEditorAction;
begin
  AChange := Default(TNyxImageEditorChange);
  Result := NyxImageEditorAction(AButton, AShellRoot, LEditor, LAction) and
    (LAction = ieaApply);

  if not Result then
  begin
    Exit;
  end;
  AChange.Owner := LEditor.Prop(COwner);
  AChange.Baseline := LEditor.Prop(CBaseline);
  AChange.Source := TNyxImageSource.FromWire(
    LEditor.Find(LEditor.ID + TNyxText('-preview')).Prop(NyxAttributeName(atSource)));
  AChange.AlternativeText := LEditor.Find(
    NyxImageEditorFieldID(LEditor.ID, iefAlternative)).Prop('value');
  AChange.Fit := ReadNyxImageFit(LEditor.Find(
    NyxImageEditorFieldID(LEditor.ID, iefFit)).Prop('value'));
  AChange.Horizontal := ReadNyxImageAnchor(LEditor.Find(
    NyxImageEditorFieldID(LEditor.ID, iefHorizontal)).Prop('value'));
  AChange.Vertical := ReadNyxImageAnchor(LEditor.Find(
    NyxImageEditorFieldID(LEditor.ID, iefVertical)).Prop('value'));
end;

function TNyxImageEditorChange.ToData: TNyxDataValue;
begin
  Result := NyxObject([NyxField('owner', NyxData(Owner)),
    NyxField('baseline', NyxData(Baseline)), NyxField('source', NyxData(Source.ToWire)),
    NyxField('alternative', NyxData(AlternativeText)), NyxField('fit', NyxData(NyxImageFitName(Fit))),
    NyxField('horizontal', NyxData(NyxImageAnchorName(Horizontal))),
    NyxField('vertical', NyxData(NyxImageAnchorName(Vertical)))]);
end;

class function TNyxImageEditorChange.FromData(const AData: TNyxDataValue): TNyxImageEditorChange;
begin

  if (AData.Kind <> ndObject) or (AData.Count <> 7) then
  begin
    raise ENyxModel.Create('Image intent requires its exact seven-field value');
  end;
  Result := Default(TNyxImageEditorChange);
  Result.Owner := AData.Field('owner').AsText;
  Result.Baseline := AData.Field('baseline').AsText;
  Result.Source := TNyxImageSource.FromWire(AData.Field('source').AsText);
  Result.AlternativeText := AData.Field('alternative').AsText;
  Result.Fit := ReadNyxImageFit(AData.Field('fit').AsText);
  Result.Horizontal := ReadNyxImageAnchor(AData.Field('horizontal').AsText);
  Result.Vertical := ReadNyxImageAnchor(AData.Field('vertical').AsText);

  if (Result.Owner = '') or (Result.Baseline = '') or
    (Result.ToData.ToJSON <> AData.ToJSON) then
  begin
    raise ENyxModel.Create('Image intent requires its exact owner and baseline');
  end;
end;

function TNyxImageEditorDraft.GetDefined: Boolean;
begin
  Result := FEditor <> '';
end;

function TNyxImageEditorDraft.ToData: TNyxDataValue;
var
  LFields: array[TNyxImageEditorField] of TNyxDataField;
  LField: TNyxImageEditorField;
begin
  Result := NyxNull;

  if not Defined then
  begin
    Exit;
  end;
  for LField := Low(TNyxImageEditorField) to High(TNyxImageEditorField) do
  begin
    LFields[LField] := NyxField(CFields[LField], NyxData(FValues[LField]));
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('editor', NyxData(FEditor)),
    NyxField('baseline', NyxData(FBaseline)), NyxField('source', NyxData(FSource.ToWire)),
    NyxField('values', NyxObject(LFields))]);
end;

class function TNyxImageEditorDraft.FromData(const AData: TNyxDataValue): TNyxImageEditorDraft;
var
  LField: TNyxImageEditorField;
  LValues: TNyxDataValue;
begin
  Result := Default(TNyxImageEditorDraft);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;
  LValues := AData.Field('values');

  if (AData.Kind <> ndObject) or (AData.Count <> 5) or
    (AData.Field('version').AsInteger <> 1) or
    (LValues.Kind <> ndObject) or
    (LValues.Count <> Ord(High(TNyxImageEditorField)) + 1) then
  begin
    raise ENyxModel.Create('Unsupported image proposal preference');
  end;
  Result.FEditor := AData.Field('editor').AsText;
  Result.FBaseline := AData.Field('baseline').AsText;
  Result.FSource := TNyxImageSource.FromWire(AData.Field('source').AsText);
  for LField := Low(TNyxImageEditorField) to High(TNyxImageEditorField) do
  begin
    Result.FValues[LField] := LValues.Field(CFields[LField]).AsText;
  end;

  if (Result.FEditor = '') or (Result.FBaseline = '') then
  begin
    raise ENyxModel.Create('Image proposal preference requires its exact form context');
  end;
end;

procedure TNyxImageEditorDraft.Clear;
begin
  Self := Default(TNyxImageEditorDraft);
end;

procedure TNyxImageEditorDraft.Propose(const ASource: TNyxImageSource);
begin

  if not Defined then
  begin
    raise ENyxModel.Create('Image import requires a captured form proposal');
  end;
  FSource := ASource;
end;

procedure TNyxImageEditorDraft.Capture(const AID: TNyxText; AShellRoot: TNyxNode);
var
  LEditor: TNyxNode;
  LField: TNyxImageEditorField;
  LCandidate: TNyxImageEditorDraft;
begin
  LEditor := nil;

  if AShellRoot <> nil then
  begin
    LEditor := AShellRoot.Find(AID);
  end;

  if LEditor = nil then
  begin
    Exit;
  end;

  if not Complete(LEditor) then
  begin
    Clear;
    Exit;
  end;
  LCandidate := Default(TNyxImageEditorDraft);
  LCandidate.FEditor := AID;
  LCandidate.FBaseline := LEditor.Prop(CBaseline);
  LCandidate.FSource := TNyxImageSource.FromWire(
    LEditor.Find(AID + TNyxText('-preview')).Prop(NyxAttributeName(atSource)));
  for LField := Low(TNyxImageEditorField) to High(TNyxImageEditorField) do
  begin
    LCandidate.FValues[LField] := LEditor.Find(NyxImageEditorFieldID(AID, LField)).Prop('value');
  end;
  Self := LCandidate;
end;

function TNyxImageEditorDraft.Restore(AShellRoot: TNyxNode): Boolean;
var
  LEditor: TNyxNode;
  LField: TNyxImageEditorField;
begin
  Result := False;

  if not Defined or (AShellRoot = nil) then
  begin
    Exit;
  end;
  LEditor := AShellRoot.Find(FEditor);

  if LEditor = nil then
  begin
    Exit;
  end;

  if not Complete(LEditor) or (LEditor.Prop(CBaseline) <> FBaseline) then
  begin
    Clear;
    Exit;
  end;
  ProposeNyxImageEditorSource(LEditor, FSource);
  for LField := Low(TNyxImageEditorField) to High(TNyxImageEditorField) do
  begin
    LEditor.Find(NyxImageEditorFieldID(FEditor, LField)).SetProp('value', FValues[LField]);
  end;
  Result := True;
end;

end.
