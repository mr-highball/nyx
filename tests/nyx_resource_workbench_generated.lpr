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


program nyx_resource_workbench_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.model, nyx.codegen, nyx.source, nyx.schema, nyx.codec,
  nyx.types, nyx.resources, nyx.binding.types, nyx.studio.session,
  nyx.studio.projects, nyx.controls, nyx.resources.editor,
  nyx.generated.view, nyx.test.resource.workbench
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LChecks: Integer;
  LWorkspace: TNyxSourceWorkspace;
  LReconstructed: TNyxDocument;
  LSession: TNyxStudioSession;
  LForm: INyxCard;
  LExisting: INyxCard;
  LEditor: TNyxNode;
  LAction: TNyxResourceEditorAction;
  LSelection: TNyxResourceEditorSelection;
  LChange: TNyxResourceEditorChange;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;

begin
  try
    LDocument := BuildNyxDocument;
    try
      LChecks := CheckNyxResourceWorkbench(LDocument);
      Inc(LChecks, CheckNyxResourceSelection(LDocument));
      { Reconstruction checks source admission independently of the host's
        compiler. It must retain all common file kinds and their consumers. }
      LWorkspace := nil;
      LReconstructed := nil;
      try
        LDocument.Validate;
        ValidateNyxDocumentProperties(LDocument);
        LReconstructed := TNyxSourceWorkspace.PrepareDraft(
          TNyxCodegen.Generate(LDocument), LWorkspace);
        Inc(LChecks, CheckNyxResourceWorkbench(LReconstructed));
        { Reproduce the ordinary incremental text addition with an existing
          JSON row relationship. A fully populated builder alone misses this
          publication boundary. Every session owner remains detached. }
        LReconstructed.Find('workshop-notes').RemoveBinding(bpText);
        LReconstructed.Resources.Remove(NyxResourceRef('notes'), NyxDefaultLocale);
        LReconstructed.Resources.Remove(NyxResourceRef('packed'), NyxDefaultLocale);
        LSession := TNyxStudioSession.Create(NyxProjectPair(TNyxCodec.Encode(LReconstructed),
          TNyxCodegen.Generate(LReconstructed)));
        try
          LExisting := NewNyxResourceEditor('workbench-existing-form', LSession.Document.Resources,
            NyxResourceSelection(NyxResourceRef('copy'), NyxDefaultLocale),
            LSession.Document.Find('workshop-notes'), LSession.Document.Find('workshop-notes'));
          try

            if not NyxResourceEditorAction(LExisting.Node.Find(
              NyxResourceEditorActionID(LExisting.ID, reaNew)), LExisting.Node,
              LEditor, LAction, LSelection) or (LAction <> reaNew) or
              LSelection.Reference.Defined then
            begin
              raise Exception.Create('New resource retained the previously opened variant');
            end;
            Inc(LChecks);
          finally
            LEditor := nil;
            LExisting := nil;
          end;
          LForm := NewNyxResourceEditor('workbench-resource-form', LSession.Document.Resources,
            LSelection, LSession.Document.Find('workshop-notes'),
            LSession.Document.Find('workshop-notes'));
          try
            LForm.Node.Find(NyxResourceEditorFieldID(LForm.ID, refName)).Configure.Value('notes').Done;
            ProposeNyxResourceEditor(LForm.Node,
              NyxTextResource(WorkbenchNotes).Describe(WorkbenchNotesTitle, WorkbenchNotesHelp));
            LForm.Node.Find(NyxResourceEditorFieldID(LForm.ID, refBind)).Configure.Value(True).Done;
            LForm.Node.Find(NyxResourceEditorFieldID(LForm.ID, refTarget))
              .Configure.Value(NyxBindingPropertyTitle(bpText)).Done;
            LForm.Node.Find(NyxResourceEditorFieldID(LForm.ID, refPath))
              .Configure.Value('File text / text').Done;

            if not CaptureNyxResourceEditor(LForm.Node.Find(
              NyxResourceEditorActionID(LForm.ID, reaApply)), LForm.Node, LChange) then
            begin
              raise Exception.Create('Common text form did not capture its typed proposal');
            end;
            LSession.Select('workshop-notes');
            LEdit := Default(TNyxStudioDesignEdit);
            LEdit.Action := sdaResource;
            LEdit.Selection := 'workshop-notes';
            LEdit.View := 'resource-home';
            LEdit.Resource := LChange;
            LSchemas := CaptureNyxSchemas;
            LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
            LPrepared := PrepareNyxStudioDesign(ReadNyxStudioDesignRequest(LRequest.ToData), LSchemas);

            if LPrepared.Diagnostic.Defined then
            begin
              raise Exception.Create(LPrepared.Diagnostic.Message);
            end;

            if LSession.CompleteDesignRequest(LRequest, LPrepared) <> nscApplied then
            begin
              raise Exception.Create('Queued text form did not publish its pair');
            end;
            LPrepared := nil;
            LSchemas := nil;
          finally
            LForm := nil;
          end;

          { The default copy and its hosted English variant both remain after
            the packed resource is removed and notes are recreated. Adding a
            different file must not discard an existing localized sibling. }

          if (LSession.Document.Resources.Count <> 3) or
            not LSession.Document.Resources.Contains(NyxResourceRef('copy'), NyxLocale('en-GB')) then
          begin
            raise Exception.Create('Incremental text publication lost catalog membership');
          end;
          Inc(LChecks);
        finally
          LSession.Free;
        end;
      finally
        LReconstructed.Free;
        LWorkspace.Free;
      end;
      WriteLn('PASS / exact emitted resource workbench / ', LChecks, ' checks');
      {$ifdef PAS2JS}
      document.body.setAttribute('data-test-result', 'passed');
      {$endif}
    finally
      LDocument.Free;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL / ', LException.Message);
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      document.body.setAttribute('data-test-result', 'failed');
      {$else}
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
