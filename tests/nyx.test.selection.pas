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

unit nyx.test.selection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model;

{ Explicit current-source public-Pascal enrichment of the exact MCP-authored
  selection seed. It never edits an installed review or rewrites that export. }
procedure EnrichNyxSelectionFixture(ADocument: TNyxDocument);
{ Shared strict wire/source/ownership assertions, with no target controls.
  The argument stays caller-owned and unchanged by every candidate attempt. }
function RunNyxSelectionContracts(ADocument: TNyxDocument): Integer;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.state, nyx.controls,
  nyx.selection, nyx.codec, nyx.codegen, nyx.schema, nyx.source,
  nyx.studio.projects, nyx.studio.session;

procedure EnrichNyxSelectionFixture(ADocument: TNyxDocument);
var
  LStepper: INyxStepper;
  LReference: INyxComponent;
begin
  SetNyxSelectionOptions(RetainNyxControl(ADocument.Find('journey-stepper')),
    TNyxSelectionOptions.Defaults.Selected(nvDefault).Unselected(nvPrimary));
  SetNyxSelectionOptions(RetainNyxControl(ADocument.Find('project-rating')),
    TNyxSelectionOptions.Defaults.Styling(nssAuthored));
  SetNyxSelectionOptions(RetainNyxControl(ADocument.Find('project-sections')),
    TNyxSelectionOptions.Defaults.Selected(nvPrimary).Unselected(nvSecondary));
  LStepper := NewNyxStepper('brand-steps');
  LStepper.WithText('Independently styled steps');
  LStepper.Step1Button.Configure.Text('Plan').Variant(nvPrimary).Done;
  LStepper.Step2Button.Configure.Text('Create').Variant(nvSecondary).Done;
  LStepper.Step3Button.Configure.Text('Review').Variant(nvPrimary).Done;
  LStepper.Step4Button.Configure.Text('Publish').Variant(nvDefault).Done;
  SetNyxSelectionOptions(LStepper, TNyxSelectionOptions.Defaults.Styling(nssAuthored));
  ADocument.AddComponent(LStepper);
  LStepper := nil;
  ADocument.State.SetValue(NyxIntegerState('first.selection'), 1)
    .SetValue(NyxIntegerState('second.selection'), 3)
    .SetValue(NyxBooleanState('first.readonly'), False);
  LReference := NewNyxComponent('first');
  LReference.Configure.Component(NyxComponent('brand-steps')).Done;
  LReference.Binds.Value(NyxIntegerState('first.selection'))
    .ReadOnly(NyxBooleanState('first.readonly')).Done;
  ADocument.Pages[0].Add(LReference);
  LReference := NewNyxComponent('second');
  LReference.Configure.Component(NyxComponent('brand-steps')).Done;
  LReference.Binds.Value(NyxIntegerState('second.selection')).Done;
  { A local typed override changes presentation, independently of definition
    and the first instance. State defaults remain owned by the document. }
  SetNyxSelectionOptions(LReference,
    TNyxSelectionOptions.Defaults.Selected(nvPrimary).Unselected(nvDefault));
  ADocument.Pages[0].Add(LReference);
  LReference := nil;
end;

function RunNyxSelectionContracts(ADocument: TNyxDocument): Integer;
const
  CBadWire: array[0..5] of TNyxText = (
    '{"version":2,"styling":"variants","selected":"primary","unselected":"default"}',
    '{"version":1,"styling":"unknown","selected":"primary","unselected":"default"}',
    '{"version":1,"styling":"variants","selected":"unknown","unselected":"default"}',
    '{"version":1,"styling":"variants","selected":"primary","unselected":true}',
    '{"version":1,"styling":"variants","selected":"primary"}',
    '{"version":1,"styling":"variants","selected":"primary","unselected":"default","extra":true}');
var
  LChecks: Integer;
  LIndex: Integer;
  LRefused: Boolean;
  LOriginal: TNyxSelectionOptions;
  LChanged: TNyxSelectionOptions;
  LVariant: TNyxVariant;
  LBefore: TNyxText;
  LSource: TNyxText;
  LWorkspace: TNyxSourceWorkspace;
  LCandidate: TNyxDocument;
  LInvalid: TNyxDocument;
  LSession: TNyxStudioSession;
  LEdited: TNyxText;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create(AReason);
    end;
    Inc(LChecks);
  end;

begin
  LChecks := 0;
  LOriginal := TNyxSelectionOptions.Defaults;
  LChanged := LOriginal.Styling(nssAuthored).Selected(nvSecondary).Unselected(nvGhost);
  Check((LOriginal.StylingMode = nssVariants) and
    (LOriginal.SelectedVariant = nvPrimary) and (LOriginal.UnselectedVariant = nvDefault),
    'fluent copies preserve legacy defaults');
  Check(TNyxSelectionOptions.FromData(LChanged.ToData).ToData.ToJSON = LChanged.ToData.ToJSON,
    'typed authored mode round-trips independently');
  for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
  begin
    LChanged := LOriginal.Selected(LVariant).Unselected(LVariant);
    Check(TNyxSelectionOptions.FromData(LChanged.ToData).SelectedVariant = LVariant,
      'every closed variant survives exact wire admission');
  end;
  for LIndex := Low(CBadWire) to High(CBadWire) do
  begin
    LRefused := False;
    try
      TNyxSelectionOptions.FromData(TNyxDataValue.ParseJSON(CBadWire[LIndex]));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'malformed selection policy refuses before publication');
  end;
  LRefused := False;
  try
    SetNyxSelectionOptions(nil, LOriginal);
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'nil authoring control refuses');
  LBefore := TNyxCodec.Encode(ADocument);
  LSource := TNyxCodegen.Generate(ADocument, 'nyx.generated.selection');
  Check((Pos('INyxStepper', LSource) > 0) and
    (Pos('SetNyxSelectionOptions', LSource) > 0) and
    (Pos('.Styling(nssAuthored)', LSource) > 0) and
    (Pos('.Selected(nvDefault)', LSource) > 0),
    'crafted generated interfaces and enums describe the policy');
  LWorkspace := TNyxSourceWorkspace.Create;
  LCandidate := nil;
  LInvalid := nil;
  LSession := nil;
  try
    LCandidate := LWorkspace.Candidate(ADocument, LSource);
    Check(TNyxCodec.Encode(LCandidate) = LBefore,
      'ordinary source reconstructs exact definitions and local overrides');
    FreeAndNil(LCandidate);
    LRefused := False;
    try
      LCandidate := LWorkspace.Candidate(ADocument,
        StringReplace(LSource, '.Styling(nssAuthored)', '.Styling(''authored'')', []));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused and (TNyxCodec.Encode(ADocument) = LBefore),
      'untyped source choice refuses atomically');
    LRefused := False;
    try
      LCandidate := LWorkspace.Candidate(ADocument,
        StringReplace(LSource, '.Selected(nvDefault)', '.Selected(nssAuthored)', []));
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LCandidate);
    Check(LRefused and (TNyxCodec.Encode(ADocument) = LBefore),
      'wrong Pascal enum family refuses atomically');
    LInvalid := ADocument.Clone;
    SetNyxSelectionOptions(RetainNyxControl(LInvalid.Pages[0]), LOriginal);
    LRefused := False;
    try
      ValidateNyxDocumentProperties(LInvalid);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'non-value layout host refuses selection policy');
    LSession := TNyxStudioSession.Create(NyxProjectPair(LBefore, LSource));
    LEdited := StringReplace(LSource, '.Selected(nvDefault)', '.Selected(nvSecondary)', []);
    LSession.SetSourceDraft(LEdited);
    LSession.ApplySourceDraft;
    Check((NyxSelectionOptions(LSession.Document.Find('journey-stepper')).SelectedVariant = nvSecondary) and
      (LSession.Source = LEdited), 'ordinary Studio source admission installs typed policy and exact Pascal');
    LSession.Undo;
    Check((LSession.Save = LBefore) and (LSession.Source = LSource),
      'one paired Undo restores original design and Pascal');
    LSession.Redo;
    Check((NyxSelectionOptions(LSession.Document.Find('journey-stepper')).SelectedVariant = nvSecondary) and
      (LSession.Source = LEdited), 'one paired Redo restores exact changed policy and source');
  finally
    LSession.Free;
    LInvalid.Free;
    LCandidate.Free;
    LWorkspace.Free;
  end;
  Check(TNyxCodec.Encode(ADocument) = LBefore, 'all qualification leaves authored pair unchanged');
  Result := LChecks;
end;

end.
