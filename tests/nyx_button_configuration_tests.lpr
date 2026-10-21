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
program nyx_button_configuration_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data, nyx.bytes,
  nyx.images, nyx.resources, nyx.button.presentation, nyx.button.configuration,
  nyx.responsive, nyx.presentations, nyx.containers, nyx.composition, nyx.contract,
  nyx.codec, nyx.codegen, nyx.source, nyx.schema, nyx.event.payload, nyx.platform
  {$ifdef PAS2JS}, Web{$endif};

const
  CPNG: TNyxText =
    'iVBORw0KGgoAAAANSUhEUgAAAGQAAAAyEAIAAAB1xzWqAAAACXBIWXMAAAAAAAAAAACdYiYy' +
    'AAABMElEQVR4nO3OsQ0AIAzAsP7/dOEEtsgSGTxnduf2fbE/gMwHIPsDyHwAsj+AzAcg+wPI' +
    'fACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsD' +
    'yHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7' +
    'A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg' +
    '+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwH' +
    'IPsDyHwAsj+AzAcg+wPIfACyP4DMByD7A8h8ALI/gMwHIPsDxwO6T+sr8laFkAAAAABJRU5E' +
    'rkJggg==';

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Button configuration: ' + AReason);
  end;
  Inc(GChecks);
end;

function GlyphFace: TNyxButtonPresentation;
begin
  Result := NyxButtonPresentation.Glyph(NyxEmbeddedImage(nimPNG, CPNG))
    .Cell(NyxGlyphCell(4, 4, 2, 1)).GlyphGap(0).Content(nbcGlyphOnly)
    .Density(nbdCompact).FontWeight(nbwSemibold).GlyphTone(ngtForeground);
end;

{ Each source check reconstructs an independent owner. Exact codec equality
  deliberately includes ordered property insertion and metadata JSON bytes. }
procedure RoundTrip(ADocument: TNyxDocument; const AReason: TNyxText);
var
  LSource: TNyxText;
  LWorkspace: TNyxSourceWorkspace;
  LRead: TNyxDocument;
  LDecoded: TNyxDocument;
begin
  LWorkspace := nil;
  LRead := nil;
  LDecoded := nil;
  try
    ValidateNyxDocumentProperties(ADocument);
    LSource := TNyxCodegen.Generate(ADocument, 'nyx.fixture.button.presentation');
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = TNyxCodec.Encode(ADocument),
      AReason + ': source reconstructs exact ordered document');
    LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(ADocument));
    Check(TNyxCodec.Encode(LDecoded) = TNyxCodec.Encode(ADocument),
      AReason + ': codec retains exact metadata and scopes');
    Check(LWorkspace.Render(LRead) = LSource,
      AReason + ': unchanged workspace retains admitted source');
  finally
    LDecoded.Free;
    LRead.Free;
    LWorkspace.Free;
  end;
end;

function Refused(ADocument: TNyxDocument): Boolean;
begin
  Result := False;
  try
    ValidateNyxDocumentProperties(ADocument);
  except
    on Exception do
    begin
      Result := True;
    end;
  end;
end;

function BasicDocument(out AButton: INyxButton): TNyxDocument;
var
  LPage: INyxPage;
begin
  Result := TNyxDocument.Create;
  LPage := NewNyxPage('home');
  Result.AddPage(LPage);
  AButton := NewNyxButton('launch');
  LPage.Add(AButton);
end;

procedure ScopeAndOrderCases;
var
  LDocument: TNyxDocument;
  LButton: INyxButton;
  LFace: TNyxButtonPresentation;
  LFacade: INyxButtonConfiguration;
  LConfiguration: INyxConfiguration;
  LSource: TNyxText;
begin
  LDocument := BasicDocument(LButton);
  try
    LFace := GlyphFace;
    { Author the optional property before caption/size so a deferred second
      emission pass cannot accidentally pass the strict ordered-pair oracle. }
    NyxButtonConfigure(LButton.Configure).Presentation(LFace);
    LButton.Configure.Text('Launch').Width(96).Done;
    LConfiguration := LButton.Configure.ForPlatform(npfBrowser);
    LFacade := NyxButtonConfigure(LConfiguration);
    LFacade.Presentation(LFace.Alignment(nbaStart));
    LButton.Configure.Height(36).Done;
    LButton.Configure.ForPlatform(npfBrowser).Text('Browser launch').Done;
    NyxButtonConfigure(LButton.Configure.WhenViewport(TNyxViewportWidth.Below(600)))
      .Presentation(LFace.Density(nbdTouch));
    LButton.Configure.Text('Launch command').Done;
    LDocument.Presentations.Define(NyxPresentation('Dense commands'),
      TNyxPresentationCondition.Manual);
    NyxButtonConfigure(LButton.Configure.WhenPresentation(NyxPresentation('Dense commands')))
      .Presentation(LFace.GlyphGap(2));
    LButton.Configure.WhenPresentation(NyxPresentation('Dense commands'))
      .AccessibleName('Launch the project').Done;
    RoundTrip(LDocument, 'interspersed base/platform/viewport/manual presentation');
    Check(LFacade.Done = LConfiguration,
      'optional managed facet returns the exact caller configuration');
    LSource := TNyxCodegen.Generate(LDocument);
    Check(Pos('NyxButtonConfigure(', LSource) > 0,
      'canonical values use the optional typed public facade');
    Check(Pos('Clear(atButtonPresentation)', LSource) = 0,
      'ordered emission creates no empty placeholder property');
    LButton.Configure.ForPlatform(npfBrowser).Clear(atButtonPresentation).Done;
    RoundTrip(LDocument, 'explicit scoped empty presentation mask');
    LButton.Configure.ForPlatform(npfBrowser).Reset(atButtonPresentation).Done;
    RoundTrip(LDocument, 'scoped inheritance after reset');
    { Raw Metadata may retain legal noncanonical JSON. Typed source would
      canonicalize it, so the ordinary exact-text Metadata fallback must win.
      Document whitespace leaves quoted image/resource payloads byte-exact. }
    LButton.Configure.Metadata(atButtonPresentation,
      ' ' + LFace.ToData.ToJSON + ' ').Done;
    RoundTrip(LDocument, 'noncanonical legal metadata spelling');
  finally
    LFacade := nil;
    LConfiguration := nil;
    LButton := nil;
    LDocument.Free;
  end;
end;

procedure ResourceCases;
var
  LDocument: TNyxDocument;
  LButton: INyxButton;
  LImage: TNyxResourceImageRef;
  LSource: TNyxText;
begin
  LDocument := BasicDocument(LButton);
  try
    LButton.Configure.Text('Resource command').Done;
    LDocument.Resources.Define(NyxResourceRef('Studio atlas'),
      NyxResourceFromBytes(nrkImage, NyxDecodeBase64(CPNG)));
    LImage := NyxResourceImage(NyxResourceRef('Studio atlas'))
      .Localize(NyxDefaultLocale, NyxDefaultLocale);
    NyxButtonConfigure(LButton.Configure).Presentation(
      NyxButtonPresentation.Glyph(LImage).Content(nbcGlyphCaption));
    RoundTrip(LDocument, 'localized image reference with default locale selectors');
    LSource := TNyxCodegen.Generate(LDocument);
    Check(Pos('.Localize(NyxDefaultLocale, NyxDefaultLocale)', LSource) > 0,
      'undefined locale selectors emit the typed default rather than a blank locale');
    Check(Pos('nyx.resources', LSource) > 0,
      'resource glyph source includes its public dependency');
    NyxButtonConfigure(LButton.Configure).Presentation(
      NyxButtonPresentation.Glyph(NyxResourceImage(NyxResourceRef('Missing atlas'))));
    Check(Refused(LDocument), 'missing resource refuses at effective document admission');
  finally
    LButton := nil;
    LDocument.Free;
  end;
end;

procedure ReconciliationCases;
var
  LDocument: TNyxDocument;
  LButton: INyxButton;
  LWorkspace: TNyxSourceWorkspace;
  LRead: TNyxDocument;
  LChanged: TNyxDocument;
  LVerified: TNyxDocument;
  LVerification: TNyxSourceWorkspace;
  LSource: TNyxText;
  LNext: TNyxText;
  LBefore: TNyxText;
begin
  LDocument := BasicDocument(LButton);
  LWorkspace := nil;
  LRead := nil;
  LChanged := nil;
  LVerified := nil;
  LVerification := nil;
  try
    NyxButtonConfigure(LButton.Configure).Presentation(GlyphFace);
    LButton.Configure.Text('Launch').Width(90).Done;
    LSource := TNyxCodegen.Generate(LDocument);
    LSource := StringReplace(LSource, '    NyxButtonConfigure(',
      '    // Retain this command note.' + #10 + '    NyxButtonConfigure(', []);
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    LBefore := TNyxCodec.Encode(LRead);
    LChanged := LRead.Clone;
    LChanged.Find('launch').Configure.ButtonPresentation(
      GlyphFace.GlyphGap(7).GlyphTone(ngtOriginal)).Done;
    LNext := LWorkspace.Render(LChanged);
    Check(Pos('// Retain this command note.', LNext) > 0,
      'wrapper reconciliation retains surrounding authored notes');
    Check(Pos('.GlyphGap(7)', LNext) > 0,
      'changed immutable presentation is not collapsed to an empty source atom');
    LVerified := TNyxSourceWorkspace.PrepareDraft(LNext, LVerification);
    Check(TNyxCodec.Encode(LVerified) = TNyxCodec.Encode(LChanged),
      'presentation edit preserves exact ordered paired meaning');
    Check(TNyxCodec.Encode(LRead) = LBefore,
      'source candidate does not mutate the independently accepted document');
    FreeAndNil(LVerified);
    FreeAndNil(LVerification);
    FreeAndNil(LChanged);
    LChanged := LRead.Clone;
    LChanged.Find('launch').Configure.Reset(atButtonPresentation).Done;
    LNext := LWorkspace.Render(LChanged);
    LVerified := TNyxSourceWorkspace.PrepareDraft(LNext, LVerification);
    Check(TNyxCodec.Encode(LVerified) = TNyxCodec.Encode(LChanged),
      'reset removes the whole wrapper without a dangling prefix');
    FreeAndNil(LVerified);
    FreeAndNil(LVerification);
    FreeAndNil(LChanged);
    LChanged := LRead.Clone;
    LChanged.Find('home').Remove(LChanged.Find('launch'));
    LNext := LWorkspace.Render(LChanged);
    LVerified := TNyxSourceWorkspace.PrepareDraft(LNext, LVerification);
    Check(TNyxCodec.Encode(LVerified) = TNyxCodec.Encode(LChanged),
      'removing the owner removes the complete optional facade statement');
    Check(Pos('NyxButtonConfigure(', LNext) = 0,
      'no removed-owner wrapper survives lexical pruning');
  finally
    LVerification.Free;
    LVerified.Free;
    LChanged.Free;
    LWorkspace.Free;
    LRead.Free;
    LButton := nil;
    LDocument.Free;
  end;
end;

procedure EffectiveAdmissionCases;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LDefinition: INyxColumn;
  LFace: INyxButton;
  LReference: INyxComponent;
  LRule: TNyxNode;
  LBefore: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  try
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LDefinition := NewNyxColumn('command-definition');
    LDefinition.Configure.Compound(True).Done;
    LFace := NewNyxButton('definition-face');
    LFace.Configure.PartName(NyxPart('command')).Text('Inherited command').Done;
    LDefinition.Add(LFace);
    LDocument.AddComponent(LDefinition);
    LReference := NewNyxComponent('first-command');
    LReference.Reference := NyxComponent('command-definition');
    LPage.Add(LReference);
    LRule := LReference.Node.OverridePart(NyxPart('command')).Named('command-rule');
    LRule.Configure.ButtonPresentation(GlyphFace).Done;
    LBefore := TNyxCodec.NodeSnapshot(LDefinition.Node).ToJSON;
    Check(not Refused(LDocument),
      'raw descriptor glyph-only admission uses the inherited effective caption');
    RoundTrip(LDocument, 'named part presentation inherits its caption');
    LRule.Configure.ForPlatform(npfBrowser).Text('').AccessibleName('Accessible command').Done;
    Check(not Refused(LDocument), 'effective scoped accessible name admits a hidden caption');
    LRule.Configure.ForPlatform(npfBrowser).AccessibleName('').Done;
    Check(Refused(LDocument), 'effective platform caption and accessible-name clears refuse');
    LRule.Configure.ForPlatform(npfBrowser).AccessibleName('Accessible command').Done;
    LRule.Configure.ProjectAs(nkLabel).Done;
    Check(Refused(LDocument), 'resolved nonbutton part cannot inherit button presentation');
    LRule.Configure.Reset(atProjection).Done;
    LRule.Configure.WhenViewport(TNyxViewportWidth.Below(600))
      .Metadata(atButtonPresentation, '{"version":99}').Done;
    Check(Refused(LDocument), 'malformed scoped metadata cannot hide behind descriptor skipping');
    Check(TNyxCodec.NodeSnapshot(LDefinition.Node).ToJSON = LBefore,
      'refused effective overrides preserve the reusable definition');
  finally
    LFace := nil;
    LReference := nil;
    LDefinition := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

procedure ScopedContinuationCases;
var
  LDocument: TNyxDocument;
  LButton: INyxButton;
  LWorkspace: TNyxSourceWorkspace;
  LVerification: TNyxSourceWorkspace;
  LRead: TNyxDocument;
  LExpected: TNyxDocument;
  LVerified: TNyxDocument;
  LSource: TNyxText;
  LNext: TNyxText;
begin
  LDocument := BasicDocument(LButton);
  LWorkspace := nil;
  LVerification := nil;
  LRead := nil;
  LExpected := nil;
  LVerified := nil;
  try
    LButton.Configure.Text('Launch').Done;
    NyxButtonConfigure(LButton.Configure.ForPlatform(npfBrowser)).Presentation(GlyphFace);
    LSource := TNyxCodegen.Generate(LDocument);
    LNext := StringReplace(LSource, '.GlyphTone(ngtForeground));',
      '.GlyphTone(ngtForeground)).Done.Text(''Continued browser'').Done;', []);
    Check(LNext <> LSource, 'continuation addresses the actual emitted optional facade');
    LExpected := LDocument.Clone;
    LExpected.Find('launch').Configure.ForPlatform(npfBrowser).Text('Continued browser').Done;
    LRead := TNyxSourceWorkspace.PrepareDraft(LNext, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = TNyxCodec.Encode(LExpected),
      'Done continuation preserves exact initial platform scope and property insertion order');
    LExpected.Find('launch').Configure.ForPlatform(npfBrowser)
      .ButtonPresentation(GlyphFace.GlyphGap(9)).Done;
    LNext := LWorkspace.Render(LExpected);
    Check(Pos('.Done.Text(''Continued browser'').Done;', LNext) > 0,
      'presentation reconciliation retains the authored ordinary-facade continuation');
    LVerified := TNyxSourceWorkspace.PrepareDraft(LNext, LVerification);
    Check(TNyxCodec.Encode(LVerified) = TNyxCodec.Encode(LExpected),
      'continued facade reconstructs the complete changed paired document');
    LExpected.Find('launch').Configure.ForPlatform(npfBrowser)
      .ButtonPresentation(NyxButtonPresentation.Density(nbdTouch)).Done;
    LNext := TNyxCodegen.Generate(LExpected);
    Check(Pos('NyxButtonPresentation.Density(nbdTouch)', LNext) > 0,
      'crafted source omits implied presentation defaults');
    Check(Pos('.ForPlatform(npfAny)', LNext) = 0,
      'ordinary fresh configuration omits the implied platform selector');
    RoundTrip(LExpected, 'nondefault-only crafted presentation');
  finally
    LVerified.Free;
    LExpected.Free;
    LRead.Free;
    LVerification.Free;
    LWorkspace.Free;
    LButton := nil;
    LDocument.Free;
  end;
end;

procedure ResponsiveNameCases;
var
  LDocument: TNyxDocument;
  LButton: INyxButton;
  LPage: TNyxNode;
begin
  LDocument := BasicDocument(LButton);
  try
    LButton.Configure.Text('Launch').Done;
    NyxButtonConfigure(LButton.Configure).Presentation(GlyphFace);
    LDocument.Presentations.Define(NyxPresentation('Manual command'),
      TNyxPresentationCondition.Manual);
    LDocument.Presentations.Define(NyxPresentation('Narrow pane'),
      TNyxPresentationCondition.Within(NyxContainer('commands'),
        TNyxViewportCondition.Any.WidthBelow(500).Orientation(nvoPortrait)));
    LPage := LDocument.Find('home');
    LPage.Configure.QueryContainer(NyxContainer('commands')).Containment(nccSize).Done;
    LButton.Configure.WhenPresentation(NyxPresentation('Narrow pane'))
      .Text('').AccessibleName('Pane command').Done;
    LButton.Configure.WhenPresentation(NyxPresentation('Manual command'))
      .AccessibleName('').Done;
    Check(Refused(LDocument),
      'combined manual selection and independent portrait container can erase the full name');
    LButton.Configure.WhenPresentation(NyxPresentation('Manual command'))
      .AccessibleName('Manual command').Done;
    Check(not Refused(LDocument),
      'manual accessible name covers the combined container presentation');
    RoundTrip(LDocument, 'combined manual/container/orientation admission');
    LButton.Configure.WhenViewport(TNyxViewportWidth.Below(600))
      .Text('').AccessibleName('').Done;
    Check(Refused(LDocument),
      'automatic compact scope with no manual selection refuses unnamed glyph-only face');
  finally
    LButton := nil;
    LDocument.Free;
  end;
end;

procedure GrammarAndEventCases;
var
  LDocument: TNyxDocument;
  LButton: INyxButton;
  LWorkspace: TNyxSourceWorkspace;
  LRead: TNyxDocument;
  LSource: TNyxText;
  LInvalid: TNyxText;
  LRefused: Boolean;
  LEvents: TNyxEventSchemas;
  LIndex: Integer;
  LCount: Integer;
begin
  LDocument := BasicDocument(LButton);
  LWorkspace := nil;
  LRead := nil;
  try
    LButton.Configure.Text('Launch').Done;
    NyxButtonConfigure(LButton.Configure).Presentation(GlyphFace);
    LSource := TNyxCodegen.Generate(LDocument);
    LInvalid := StringReplace(LSource, '.GlyphGap(0)', '.GlyphGap(nbcCaption)', []);
    Check(LInvalid <> LSource, 'grammar refusal addresses an actual emitted typed argument');
    LRefused := False;
    try
      LRead := TNyxSourceWorkspace.PrepareDraft(LInvalid, LWorkspace);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRead = nil) and (LWorkspace = nil),
      'wrong enum family refuses atomically without a candidate');
    LEvents := NyxEventsMetadata(LButton.Node, LDocument);
    LCount := 0;
    for LIndex := 0 to High(LEvents) do
    begin

      if LEvents[LIndex].Trigger in
        [ntImageLoading, ntImageReady, ntImageError, ntImageCleared] then
      begin
        Inc(LCount);
        Check(nctxImage in LEvents[LIndex].Contexts,
          'button glyph lifecycle advertises its normal typed image context');
        Check(not LEvents[LIndex].DeclaredProducer,
          'image lifecycle discovery grants no custom producer authority');
      end;
    end;
    Check(LCount = 4, 'button glyph exposes all four image lifecycle hooks');
  finally
    LWorkspace.Free;
    LRead.Free;
    LButton := nil;
    LDocument.Free;
  end;
end;

{ Renderer Build receives a concrete target projection. Re-evaluating that
  mutated base as another platform would invent invalid opposite-target values. }
procedure RealizedAdmissionCases;
var
  LDocument: TNyxDocument;
  LButton: INyxButton;
  LContext: TNyxNode;
  LProjection: TNyxNode;
  LBefore: TNyxText;
  LRefused: Boolean;
begin
  LDocument := BasicDocument(LButton);
  LContext := nil;
  try
    LButton.Configure.Text('Launch').Done;
    NyxButtonConfigure(LButton.Configure).Presentation(GlyphFace);
    NyxButtonConfigure(LButton.Configure.ForPlatform(npfBrowser))
      .Presentation(GlyphFace.Content(nbcCaption));
    LButton.Configure.ForPlatform(npfBrowser).Text('').Done;
    NyxButtonConfigure(LButton.Configure.ForPlatform(npfNativeLCL)
      .WhenViewport(TNyxViewportWidth.Below(600))).Presentation(GlyphFace);
    Check(not Refused(LDocument), 'independent platform caption contracts admit together');
    LBefore := TNyxCodec.Encode(LDocument);
    LContext := RealizeNyxContext(LDocument, LButton.Node, LProjection);
    ApplyNyxPlatform(LContext, npfBrowser);
    LContext.ApplyViewport(390, 900, npfBrowser);
    ValidateNyxProperties(LProjection);
    Check(LProjection.Prop(NyxAttributeName(atText)) = '',
      'direct admission checks the actual caption-mode browser projection');
    LProjection.Configure.ButtonPresentation(NyxButtonPresentation
      .Glyph(NyxResourceImage(NyxResourceRef('Missing runtime atlas')))).Done;
    LRefused := False;
    try
      ValidateNyxProperties(LProjection);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'direct realized admission refuses a missing owned-context resource');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'direct runtime checks preserve the complete authored source document');
  finally
    LContext.Free;
    LButton := nil;
    LDocument.Free;
  end;
end;

begin
  try
    ScopeAndOrderCases;
    ResourceCases;
    ReconciliationCases;
    EffectiveAdmissionCases;
    ScopedContinuationCases;
    ResponsiveNameCases;
    GrammarAndEventCases;
    RealizedAdmissionCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-button-configuration', 'passed');
    document.body.setAttribute('data-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' button configuration checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-button-configuration', 'failed');
      document.body.setAttribute('data-error', E.Message);
      raise;
      {$else}
      WriteLn(StdErr, E.Message);
      Halt(1);
      {$endif}
    end;
  end;
end.
