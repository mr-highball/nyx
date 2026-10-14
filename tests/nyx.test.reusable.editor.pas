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

unit nyx.test.reusable.editor;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Shared public form/plan, ownership and paired source-worker checks. }
function VerifyNyxReusableEditor: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.state, nyx.composition, nyx.reusable.editor,
  nyx.studio.projects, nyx.studio.session, nyx.studio.inspector, nyx.schema;

function VerifyNyxReusableEditor: Integer;
var
  LDocument: TNyxDocument;
  LRoot: INyxPage;
  LCard: INyxColumn;
  LRow: INyxRow;
  LSession: TNyxStudioSession;
  LForm: INyxCard;
  LForm2: INyxCard;
  LPlan: TNyxReusableProposal;
  LMap: TNyxReusableIdentities;
  LCopy: TNyxNode;
  LDraft: TNyxReusableEditorDraft;
  LPair: TNyxProjectPair;
  LBefore: TNyxText;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LRefused: Boolean;
  LText: TNyxText;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Reusable form: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LSession := nil;
  LCopy := nil;
  try
    LRoot := NewNyxPage('home');
    LDocument.AddPage(LRoot);
    LCard := NewNyxColumn('reply');
    LCard.Configure.Compound(True).Done;
    LRoot.Add(LCard);
    LCard.Add(NewNyxLabel('title').Configure.PartName(NyxPart('title'))
      .Text('Write something thoughtful').Done);
    LCard.Add(NewNyxMemo('body').Configure.PartName(NyxPart('body')).Done);
    LRow := NewNyxRow('actions');
    LRow.Configure.PartName(NyxPart('actions')).Done;
    LCard.Add(LRow);
    LRow.Add(NewNyxButton('send').Configure.PartName(NyxPart('send')).Text('Send').Done);
    LRoot.Add(NewNyxLabel('note-title'));
    LDocument.State.SetValue(NyxTextState('reply'), TNyxText('Exact 🌙 notes'));
    LBefore := TNyxCodec.Encode(LDocument);
    LPlan := NyxReusableProposal(LDocument, NyxControl('reply'), NyxComponent('note'));
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'proposal never changes the source document');
    LMap := LPlan.Identities;
    Check((Length(LMap) = 4) and (LMap[0].Destination.ID = 'note-title-2'),
      'complete purpose-based mapping avoids an existing page identity');
    Check((LMap[1].Destination.ID = 'note-body') and
      (LMap[3].Destination.ID = 'note-send'), 'nested names retain control purpose');
    LMap[0].Destination := NyxControl('caller-owned-change');
    LMap := LPlan.Identities;
    Check(LMap[0].Destination.ID = 'note-title-2', 'returned mapping does not alias the proposal');
    LCopy := CloneNyxReusableDefinition(LDocument, LCard.Node, LPlan.Definition, LMap);
    Check(LCopy.Part(NyxPart('actions/send')).Prop('text') = 'Send',
      'copy retains nested named-part customization');
    ReleaseNyxNode(LCopy);
    LCopy := nil;
    LRefused := False;
    try
      NyxReusableProposal(LDocument, NyxControl('reply'), NyxComponent('home'));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (TNyxCodec.Encode(LDocument) = LBefore),
      'occupied definition refuses without replacing its source');
    LText := TNyxText('A component 🌙');
    LPlan := NyxReusableProposal(LDocument, NyxControl('reply'), NyxComponent(LText));
    Check(LPlan.Definition.Name = LText, 'open component names preserve supplementary Unicode');
    LText := '';
    while Length(LText) < 128 do
    begin
      LText := LText + 'x';
    end;
    LPlan := NyxReusableProposal(LDocument, NyxControl('reply'), NyxComponent(LText));
    LMap := LPlan.Identities;
    Check((Length(LPlan.Definition.Name) = 128) and (Length(LMap[0].Destination.ID) <= 128),
      'maximum-length names retain bounded descendant identity stems');
    LPair := NyxProjectPair(LBefore, TNyxCodegen.Generate(LDocument));
    LSession := TNyxStudioSession.Create(LPair);
    LSession.Select('reply');
    LForm := NewNyxReusableEditor(NyxStudioReusableEditorID,
      LSession.Document, NyxControl('reply'));
    LForm.Part(NyxPart('name')).Configure.Value('note').Done;
    Check(PreviewNyxReusableInspector(LSession, LForm.Part(NyxPart('preview')).Node, LForm.Node),
      'public preview reads the guarded current form');
    Check((Pos('note-title-2', LForm.Part(NyxPart('summary')).Node.Prop('text')) > 0) and
      (EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair)),
      'bounded name preview retains the exact accepted pair');
    LDraft.Capture(NyxStudioReusableEditorID, LForm.Node);
    LForm2 := NewNyxReusableEditor(NyxStudioReusableEditorID,
      LSession.Document, NyxControl('reply'));
    Check(LDraft.Restore(LForm2.Node) and
      (LForm2.Part(NyxPart('name')).Node.Prop('value') = 'note'),
      'incomplete name survives independent form reconstruction');
    LRefused := False;
    try
      PreviewNyxReusableInspector(LSession, LForm2.Part(NyxPart('preview')).Node, LForm.Node);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'identically named action from a foreign form refuses');
    LBefore := EncodeNyxProject(LSession.ProjectSnapshot);
    Check(CaptureNyxReusableInspector(LSession, LForm.Part(NyxPart('create')).Node,
      LForm.Node, LEdit), 'Create captures one typed source transaction');
    LSchemas := CaptureNyxSchemas;
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'independent paired processor admits the name plan');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'complete current request publishes one definition');
    Check((LSession.Document.FindComponent('note') <> nil) and
      (LSession.Document.Find('reply').Parent.ID = 'home'),
      'creation retains the original while adding its independent reusable copy');
    LPair := LSession.ProjectSnapshot;
    LSession.Undo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = LBefore,
      'one paired Undo restores exact design and Pascal');
    LSession.Redo;
    Check(EncodeNyxProject(LSession.ProjectSnapshot) = EncodeNyxProject(LPair),
      'one paired Redo restores the complete named copy');
    LRefused := False;
    try
      CaptureNyxReusableInspector(LSession, LForm.Part(NyxPart('create')).Node,
        LForm.Node, LEdit);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'stale occupied identity context refuses another creation');
    LForm2 := NewNyxReusableEditor(NyxStudioReusableEditorID,
      LSession.Document, NyxControl('reply'));
    Check(not LDraft.Restore(LForm2.Node), 'changed source/registry retires parked input');
  finally
    LPrepared := nil;
    LSchemas := nil;
    LForm := nil;
    LForm2 := nil;
    LSession.Free;
    LRoot := nil;
    LCard := nil;
    LRow := nil;
    ReleaseNyxNode(LCopy);
    LDocument.Free;
  end;
end;

end.
