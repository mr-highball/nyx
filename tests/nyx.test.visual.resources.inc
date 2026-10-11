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

{ Ordinary queued transactions execute the entire handwritten builder before
  publication. This fixture owns its session and runtime home, never the live
  Studio. Resource definitions/labels/cache/locale and bindings share one paired
  history step; immutable definitions have no lifetime link back to the session. }
procedure ResourceJourney(AExecutor: TNyxBuildExecutor; const ACompiler: INyxSourceCompiler;
  const AOriginal: INyxSourceProjection; const AOutput: TNyxText);
const
  CText: TNyxText = 'A crafted resource 🚀 𐐷 é.';
  CJSON: TNyxText = '{"literal.path":[{"title":"A crafted resource 🚀 𐐷 é.",' +
    '"exact":9007199254740993}]}';
var
  LSession: TNyxStudioSession;
  LCommands: TNyxSourceCommands;
  LDraft: TNyxText;
  LSource: TNyxText;
  LBuild: INyxSourceProjectionBuild;
  LBefore: TNyxDocument;
  LAfter: TNyxDocument;
  LDefinition: INyxResourceDefinition;
  LSpec: TNyxBindingSpec;
  LRejected: Boolean;
  LEdit: TNyxStudioDesignEdit;

  procedure Submit(const ATransaction: INyxProjectTransaction; const AReason: TNyxText);
  var
    LEdit: TNyxStudioDesignEdit;
    LPrevious: TNyxProjectPair;
    LAccepted: TNyxProjectPair;
  begin
    LPrevious := NyxProjectPair(LSession.Save, LSession.Source);
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTransaction;
    LEdit.Selection := LSession.SelectedID;
    LEdit.View := LSession.ActiveViewID;
    LEdit.Transaction := CaptureNyxProjectTransaction(ATransaction);
    LCommands.Edit(LEdit);
    Drain(LCommands);
    Check(LCommands.State = nssApplied, AReason + ': ' + LCommands.Message);
    LAccepted := NyxProjectPair(LSession.Save, LSession.Source);
    Check(not LSession.SourceDraftPending and (LSession.DraftSource = LSession.Source) and
      (LSession.SourceDraftBase = ''),
      AReason + ' preserves the resolved draft state');
    Check(Pos('NyxTextResource(TNotebookCards.Caption)', LSession.Source) > 0,
      AReason + ' preserves the unchanged helper-defined resource');
    LSession.Undo;
    Check((LSession.Save = LPrevious.Design) and (LSession.Source = LPrevious.Source),
      AReason + ' has one exact paired Undo');
    LSession.Redo;
    Check((LSession.Save = LAccepted.Design) and (LSession.Source = LAccepted.Source) and
      not LSession.SourceDraftPending, AReason + ' has one exact paired Redo');
  end;

  procedure Resources(const AChanges: array of TNyxResourceChange; const AReason: TNyxText);
  begin
    Submit(NyxProjectTransaction([NyxResourceStep(NyxResourcePatch(AChanges))]), AReason);
  end;

begin
  LSession := TNyxStudioSession.Create;
  LCommands := nil;
  LBefore := nil;
  LAfter := nil;
  try
    LSession.AdoptProjectedProject(NyxProjectPair(AOriginal.Design, AOriginal.Source), AOriginal);
    LSession.Activate('notebook-1');
    LSession.Select('heading-1');
    LDraft := AOriginal.Source + TNyxText(#10 + '{ Resource idea still unfinished 🚀 }');
    LSession.SetSourceDraft(LDraft);
    LCommands := TNyxSourceCommands.Create(LSession, nil);
    LCommands.UseCompiler(ACompiler);
    { Grouped semantic commands retain their explicit draft-resolution guard.
      Unlike a scalar inspector intent, this operation must not silently resolve
      or overwrite unfinished Pascal. Qualify refusal before resolving it here. }
    LEdit := Default(TNyxStudioDesignEdit);
    LEdit.Action := sdaTransaction;
    LEdit.Selection := LSession.SelectedID;
    LEdit.View := LSession.ActiveViewID;
    LEdit.Transaction := CaptureNyxProjectTransaction(NyxProjectTransaction([
      NyxResourceStep(NyxResourcePatch([NyxDefineResource(NyxResourceRef('draft-guard'),
        NyxDefaultLocale, NyxTextResource('Refused while drafting'))]))]));
    LCommands.Edit(LEdit);
    Drain(LCommands);
    Check((LCommands.State = nssRejected) and
      (Pos('Resolve the pending Pascal draft', LCommands.Message) > 0) and
      (LSession.Save = AOriginal.Design) and
      (LSession.Source = AOriginal.Source) and (LSession.DraftSource = LDraft) and
      (LSession.SourceDraftBase = AOriginal.Source),
      'pending Pascal refuses a resource group while preserving the complete draft/base/pair');
    LSession.DiscardSourceDraft;
    Submit(NyxProjectTransaction([
      NyxDesignStep(NyxPlacementPatch([NyxPlaceNewControl(nkImage, NyxControl('resource-image'),
        NyxControl('notebook-1'), nplInside)])),
      NyxResourceStep(NyxResourcePatch([
        NyxDefineResource(NyxResourceRef('copy'), NyxDefaultLocale,
          NyxTextResource('Default copy').Tagged(NyxResourceLabel('Copy'))),
        NyxDefineResource(NyxResourceRef('copy'), NyxLocale('en-US'),
          NyxTextResource(CText).Describe('English copy', 'Purpose and help 🚀')),
        NyxDefineResource(NyxResourceRef('structured'), NyxDefaultLocale, NyxJSONResource(CJSON)),
        NyxDefineResource(NyxResourceRef('packed'), NyxDefaultLocale,
          NyxBinaryResource(NyxDecodeBase64('AAECA/8='))),
        NyxDefineResource(NyxResourceRef('picture'), NyxDefaultLocale,
          NyxImageResource(NyxEmbeddedImage(nimPNG, NyxMappingPNG))),
        NyxDefineResource(NyxResourceRef('hosted'), NyxDefaultLocale,
          NyxHostedResource(nrkText, NyxResourceURL('https://example.test/copy.txt'))
            .Tagged(NyxResourceLabel('Remote'))
            .Cache(NyxResourceCache.Persistent.FreshFor(300).StaleFor(60)
              .MaximumBytes(4096).ServerPolicy(rcspOverride))
            .Fallback(NyxTextResource('Offline copy').Tagged(NyxResourceLabel('Offline')))
            .Describe('Hosted copy', 'A declared fallback, no qualification fetch')),
        NyxBindResource(NyxControl('heading-1'), bpText,
          NyxResourceValue(NyxResourceRef('structured')).Field('literal.path').Item(0)
            .Field('title').AsText),
        NyxBindResource(NyxControl('heading-2'), bpText,
          NyxResourceValue(NyxResourceRef('copy')).Localize(NyxLocale('en-US'), NyxDefaultLocale)),
        NyxBindResourceImage(NyxControl('resource-image'), NyxResourceImage(NyxResourceRef('picture')))
      ]))]), 'construct typed resources, image and bindings as one group');
    Check(LSession.Document.Resources.Count = 8, 'original and all new exact variants coexist');
    Check(LSession.Document.Resources.Definition(NyxResourceRef('structured'), NyxDefaultLocale)
      .Data.Field('literal.path').Item(0).Field('exact').AsDecimal.Text = '9007199254740993',
      'JSON resource retains its exact integer token beyond browser safe integers');
    Check((Pos('.Cache(NyxResourceCache', LSession.Source) > 0) and
      (Pos('rcspOverride', LSession.Source) > 0) and (Pos('INyxImage;', LSession.Source) > 0),
      'source uses the existing fluent cache and specialized image interface');
    LDefinition := LSession.Document.Resources.Definition(NyxResourceRef('hosted'), NyxDefaultLocale);
    Resources([
      NyxSetResourceLabels(NyxResourceRef('copy'), NyxLocale('en-US'),
        NyxResourceLabels.Add(NyxResourceLabel('English')).Add(NyxResourceLabel('Copy'))),
      NyxDefineResource(NyxResourceRef('hosted'), NyxDefaultLocale,
        LDefinition.Cache(NyxResourceCache.Memory.FreshFor(0).StaleFor(20).MaximumBytes(8192)))
    ], 'update one locale labels and hosted policy');
    Check(LSession.Document.Resources.Definition(NyxResourceRef('copy'), NyxLocale('en-US'))
      .Text = CText, 'label-only update preserves exact content');
    Resources([
      NyxClearResourceBinding(NyxControl('heading-1'), bpText),
      NyxRemoveResource(NyxResourceRef('structured'), NyxDefaultLocale),
      NyxDefineResource(NyxResourceRef('packed'), NyxDefaultLocale, NyxTextResource(CText))
    ], 'repair consumers with deletion and independent resource retyping');
    Check(LSession.Document.Find('heading-1').FindAuthoredBinding(bpText, LSpec) and LSpec.Cleared,
      'Clear retains its explicit inheritance mask');
    Resources([
      NyxInheritResourceBinding(NyxControl('heading-1'), bpText),
      NyxDefineResource(NyxResourceRef('structured'), NyxDefaultLocale, NyxJSONResource(CJSON)),
      NyxBindResource(NyxControl('heading-1'), bpText,
        NyxResourceValue(NyxResourceRef('structured')).Field('literal.path').Item(0).Field('title'))
    ], 'recreate an exact removed identity in a later continuation');
    Check(LSession.Document.Resources.Reference(7).Name = 'structured',
      'recreated variant appends without moving retained membership');
    SaveText(AOutput + 'web/resource-base-source.pas', LSession.Source);
    SaveText(AOutput + 'web/resource-base-design.json', LSession.Save);
    Resources([
      NyxSetResourceLabels(NyxResourceRef('copy'), NyxLocale('en-US'),
        NyxResourceLabels.Add(NyxResourceLabel('Reviewed'))),
      NyxDefineResource(NyxResourceRef('copy|name: 🚀'), NyxDefaultLocale,
        NyxTextResource(CText + #0 + 'Exact bytes').Describe('Packed text', 'Includes NUL'))
    ], 'retain open Unicode identities and exact packed text');
    LSource := LSession.Source;
    LBuild := AExecutor.ProjectSource(LSource, NyxPascalUnit('nyx.projection.fixture'),
      btBrowser, spcChecked);
    Check(LBuild.Projection.State = spsCompiled, 'complete mixed resource unit compiles for pas2js');
    SaveText(AOutput + 'web/resource-source.pas', LSource);
    SaveText(AOutput + 'web/resource-design.json', LSession.Save);
    SaveText(AOutput + 'web/resource-worker.json', NyxObject([
      NyxField('reference', NyxData(LBuild.Reference.Name)),
      NyxField('artifact', NyxData(LBuild.Artifact))]).ToJSON);
    { Registry order is meaningful and Define intentionally retains positions.
      An unexpressed reorder refuses instead of clearing computed definitions. }
    LBefore := LSession.Document.Clone;
    LAfter := LBefore.Clone;
    LDefinition := LAfter.Resources.Definition(NyxResourceRef('instructions'), NyxDefaultLocale);
    LAfter.Resources.Remove(NyxResourceRef('instructions'), NyxDefaultLocale);
    LAfter.Resources.Define(NyxResourceRef('instructions'), LDefinition);
    LRejected := False;
    try
      CustomizeNyxExecutedSource(LSource, LBefore, LAfter);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Source = LSource),
      'unexpressed registry reordering refuses without changing the accepted pair');
  finally
    LAfter.Free;
    LBefore.Free;
    LCommands.Free;
    LSession.Free;
  end;
end;
