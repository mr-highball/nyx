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
program nyx_scroll_presentation_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Math, nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls,
  nyx.containers, nyx.contract, nyx.platform, nyx.responsive,
  nyx.scroll.presentation, nyx.scroll.configuration, nyx.schema,
  nyx.scroll.editor, nyx.codegen, nyx.codec, nyx.source,
  nyx.studio.session, nyx.studio.view, nyx.studio.commands, nyx.studio.edits,
  nyx.studio.transactions
  {$ifdef PAS2JS}, Web, nyx.scroll.browser{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Scroll presentation: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure Refuse(const AWire: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    TNyxScrollPresentation.FromData(TNyxDataValue.ParseJSON(AWire));
  except
    on Exception do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'invalid wire refuses: ' + AWire);
end;

procedure Values;
var
  LBase: TNyxScrollPresentation;
  LFloor: TNyxScrollPresentation;
begin
  LBase := Default(TNyxScrollPresentation);
  Check(LBase.ToData.ToJSON = NyxScrollPresentation.ToData.ToJSON,
    'zero-initialized value has valid factory semantics');
  LFloor := LBase.MinimumThickness(24);
  Check(LBase.SizingMode = nssFollowZoom, 'fluent changes do not mutate prior value');
  Check(Abs(LBase.EffectiveScale(0.5, 16) - 0.5) < 0.00001,
    'follow zoom retains the independent content factor');
  Check(Abs(LFloor.EffectiveScale(0.5, 16) - 1.5) < 0.00001,
    'physical floor compensates for reduced content');
  Check(Abs(LFloor.EffectiveScale(2, 16) - 2) < 0.00001,
    'floor does not shrink larger chrome');
  Check(LFloor.Sizing(nssKeepSize).EffectiveScale(0.5, 16) = 1,
    'keep size is independent of content zoom');
  Check(LFloor.Sizing(nssFollowZoom).PhysicalMinimum = 24,
    'switching mode retains authored floor');
  Check(TNyxScrollPresentation.FromData(LFloor.ToData).ToData.ToJSON =
    LFloor.ToData.ToJSON, 'exact data round trip');
  Refuse('{"version":1,"sizing":"keep-size","minimumThickness":0}');
  Refuse('{"version":1,"sizing":"keep-size","minimumThickness":257}');
  Refuse('{"version":1,"sizing":"keep-size","minimumThickness":4294967310}');
  Refuse('{"version":1,"sizing":"keep-size","minimumThickness":"24"}');
  Refuse('{"version":1,"sizing":"keep-size","minimumThickness":2.5}');
  Refuse('{"version":2,"sizing":"keep-size","minimumThickness":24}');
  Refuse('{"version":1,"sizing":"other","minimumThickness":24}');
  Refuse('{"version":1,"sizing":"keep-size","minimumThickness":24,"extra":true}');
end;

{ Independent reconstruction verifies ordered model bytes and retained source,
  rather than merely finding expected strings in generated Pascal. }
procedure RoundTrip(ADocument: TNyxDocument);
var
  LSource: TNyxText;
  LRead: TNyxDocument;
  LDecoded: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
begin
  LRead := nil;
  LDecoded := nil;
  LWorkspace := nil;
  try
    ValidateNyxDocumentProperties(ADocument);
    LSource := TNyxCodegen.Generate(ADocument);
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = TNyxCodec.Encode(ADocument),
      'crafted source reconstructs the exact ordered document');
    LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(ADocument));
    Check(TNyxCodec.Encode(LDecoded) = TNyxCodec.Encode(ADocument),
      'save/open preserves all presentation scopes');
    Check(LWorkspace.Render(LRead) = LSource, 'unchanged authored source is preserved');
  finally
    LRead.Free;
    LDecoded.Free;
    LWorkspace.Free;
  end;
end;

procedure Configuration;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LMemo: INyxMemo;
  LConfiguration: INyxConfiguration;
  LSource: TNyxText;
  LWire: TNyxText;
  LWorkspace: TNyxSourceWorkspace;
  LVerifiedWorkspace: TNyxSourceWorkspace;
  LRead: TNyxDocument;
  LChanged: TNyxDocument;
  LVerified: TNyxDocument;
begin
  LDocument := TNyxDocument.Create;
  LWorkspace := nil;
  LVerifiedWorkspace := nil;
  LRead := nil;
  LChanged := nil;
  LVerified := nil;
  try
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LMemo := NewNyxMemo('notes');
    LPage.Add(LMemo);
    LSource := TNyxCodegen.Generate(LDocument);
    Check(Pos('nyx.scroll.presentation', LSource) = 0,
      'older/default designs do not acquire policy imports or metadata');
    LConfiguration := LMemo.Configure;
    Check(NyxScrollConfigure(LConfiguration).Presentation(
      NyxScrollPresentation.MinimumThickness(24)).Done = LConfiguration,
      'managed facet returns the exact original scope');
    LMemo.Configure.Text('Notes').Height(120).Done;
    NyxScrollConfigure(LMemo.Configure.ForPlatform(npfBrowser)).Presentation(
      NyxScrollPresentation.Sizing(nssKeepSize));
    NyxScrollConfigure(LMemo.Configure.WhenViewport(TNyxViewportWidth.Below(600)))
      .Presentation(NyxScrollPresentation.MinimumThickness(28));
    RoundTrip(LDocument);
    LSource := TNyxCodegen.Generate(LDocument);
    Check(Pos('NyxScrollConfigure(LNotesMemo.Configure)', LSource) > 0,
      'source authors a specialized control through the public facet');
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    LChanged := LRead.Clone;
    LChanged.Find('notes').Configure.ScrollPresentation(
      NyxScrollPresentation.MinimumThickness(32));
    LSource := LWorkspace.Render(LChanged);
    LVerified := TNyxSourceWorkspace.PrepareDraft(LSource, LVerifiedWorkspace);
    Check(TNyxCodec.Encode(LVerified) = TNyxCodec.Encode(LChanged),
      'reconciliation changes the typed atom without losing sibling scopes');
    LWire := '{ "version":1, "sizing":"keep-size", "minimumThickness":24 }';
    LDocument.Find('notes').SetProp('scroll-presentation', LWire);
    Check(LDocument.Find('notes').ScrollPresentation.PhysicalMinimum = 24,
      'exact-wire model cache admits a fresh value');
    RoundTrip(LDocument);
    LDocument.Find('notes').SetProp('scroll-presentation', '{"invalid":true}');
    try
      LDocument.Find('notes').ScrollPresentation.Validate;
      Check(False, 'changed invalid wire must not reuse an old admitted cache');
    except
      on ENyxScrollPresentation do
      begin
        Check(True, 'invalid direct property change cannot hide behind cached metadata');
      end;
    end;
    NyxScrollConfigure(LMemo.Configure).UseDefault;
    Check(LDocument.Find('notes').Prop('scroll-presentation') = '',
      'UseDefault withdraws explicit presentation');
    RoundTrip(LDocument);
  finally
    LMemo := nil;
    LPage := nil;
    LConfiguration := nil;
    LVerified.Free;
    LChanged.Free;
    LRead.Free;
    LWorkspace.Free;
    LVerifiedWorkspace.Free;
    LDocument.Free;
  end;
end;

{ Exercise the ordinary composed Inspector and its shared queued-command
  capture. This is model/controller evidence, separate from actual pointer UI. }
procedure StudioAuthoring;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LReloaded: TNyxStudioSession;
  LShell: TNyxDocument;
  LPage: INyxPage;
  LState: TNyxStudioViewState;
  LField: TNyxNode;
  LEdit: TNyxStudioDesignEdit;
  LPatch: INyxDesignPatch;
  LTransaction: INyxProjectTransaction;
  LBefore: TNyxText;
  LAfter: TNyxText;
  LSourceBefore: TNyxText;
  LRefused: Boolean;
begin
  LDocument := TNyxDocument.Create;
  LSession := TNyxStudioSession.Create;
  LReloaded := TNyxStudioSession.Create;
  LShell := nil;
  try
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LPage.Add(NewNyxMemo('notes').WithText('Your notes'));
    LSession.Load(TNyxCodec.Encode(LDocument));
    LSession.Select('notes');
    LBefore := LSession.Save;
    LSourceBefore := LSession.Source;
    LState := DefaultNyxStudioViewState;
    LShell := BuildNyxStudioView(LSession, LState);
    LField := LShell.Find('inspector-scroll-sizing');
    Check((LField <> nil) and (LField.Prop('value') = 'Platform default'),
      'ordinary Studio exposes the unset policy without creating an override');
    LField.SetProp('value', 'Keep normal size');
    Check(CaptureNyxStudioAuthoring(LSession, LField, ntChange, LShell.Pages[0],
      Default(TNyxStudioPendingDesign), LEdit) = sacEdit,
      'ordinary field captures one typed authoring command');
    Check(LEdit.Action = sdaTransaction, 'policy edits use the shared undoable transaction');
    Check(LEdit.TransactionDesignBaseline = TNyxCodec.Encode(LSession.Document),
      'queued proposal retains the exact paired design baseline');
    LTransaction := LEdit.Transaction.Materialize;
    Check(LTransaction.TryDesignPatch(LPatch), 'semantic operation shares the normal design patch');
    LSession.ApplyPatch(LPatch);
    Check(LSession.Selected.ScrollPresentation.SizingMode = nssKeepSize,
      'accepted command changes the selected control policy');
    Check(Pos('NyxScrollConfigure(', LSession.Source) > 0,
      'same authoring command publishes crafted typed source');
    LAfter := LSession.Save;
    LRefused := False;
    try
      CaptureNyxStudioAuthoring(LSession, LField, ntChange, LShell.Pages[0],
        Default(TNyxStudioPendingDesign), LEdit);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSession.Save = LAfter),
      'a stale mounted field refuses without replacing the accepted pair');
    LSession.Undo;
    Check((LSession.Save = LBefore) and (LSession.Source = LSourceBefore),
      'one Undo restores the exact design and source together');
    LSession.Redo;
    Check(LSession.Save = LAfter, 'Redo restores the accepted policy');
    LReloaded.Load(LAfter);
    Check(LReloaded.Save = LAfter, 'ordinary Save/Open retains the policy');
    FreeAndNil(LShell);
    LShell := BuildNyxStudioView(LSession, LState);
    LField := LShell.Find('inspector-scroll-minimum');
    LField.SetProp('value', '28');
    Check(CaptureNyxStudioAuthoring(LSession, LField, ntChange, LShell.Pages[0],
      Default(TNyxStudioPendingDesign), LEdit) = sacEdit,
      'numeric minimum is captured as an integer-valued policy');
    LTransaction := LEdit.Transaction.Materialize;
    Check(LTransaction.TryDesignPatch(LPatch), 'minimum retains the grouped patch path');
    LSession.ApplyPatch(LPatch);
    Check((LSession.Selected.ScrollPresentation.SizingMode = nssMinimumThickness) and
      (LSession.Selected.ScrollPresentation.PhysicalMinimum = 28),
      'editing the minimum selects its sizing mode without a second draft');
  finally
    LPatch := nil;
    LTransaction := nil;
    LShell.Free;
    LSession.Free;
    LReloaded.Free;
    LPage := nil;
    LDocument.Free;
  end;
end;

{$ifdef PAS2JS}
procedure BrowserMetrics;
var
  LRefused: Boolean;
begin
  Check(Abs(NyxBrowserScrollChromeExtent(NyxScrollPresentation.Sizing(nssKeepSize), 0.5) - 34)
    < 0.00001, 'CSS compensation preserves physical thickness under ancestor transform');
  LRefused := False;
  try
    NyxBrowserScrollChromeExtent(NyxScrollPresentation.Sizing(nssKeepSize), 1e-300);
  except
    on ENyxScrollPresentation do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'unbounded CSS compensation refuses before a style mutation');
end;
{$endif}

begin
  Values;
  Configuration;
  StudioAuthoring;
  {$ifdef PAS2JS}
  BrowserMetrics;
  document.body.setAttribute('data-checks', IntToStr(GChecks));
  document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' scroll presentation checks';
  {$else}
  WriteLn('PASS ', GChecks, ' scroll presentation checks');
  {$endif}
end.
