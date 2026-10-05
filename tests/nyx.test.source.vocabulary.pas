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

unit nyx.test.source.vocabulary;

{$mode delphi}{$H+}
{$codepage utf8}

interface

{ Exercise closed authoring choices through real generated candidates and the
  public Studio session. Reserved/wrong-family symbols must keep the accepted
  pair and retained draft/history; no internal vocabulary table is inspected. }
function RunNyxSourceVocabularyTests: Integer;

implementation

uses
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.codegen,
  nyx.codec,
  nyx.source,
  nyx.studio.session,
  nyx.test.source.managed;

function RunNyxSourceVocabularyTests: Integer;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LCandidate: TNyxDocument;
  LSource: TNyxText;
  LBefore: TNyxText;
  LBeforeDesign: TNyxText;
  LDraft: TNyxText;
  LLayout: TNyxLayoutMode;
  LWrap: TNyxFlowWrap;
  LAlign: TNyxCrossAlignment;
  LJustify: TNyxJustification;
  LSizing: TNyxSizing;
  LTouch: TNyxTouchBehavior;
  LVariant: TNyxVariant;
  LAction: TNyxAction;
  LInput: TNyxInputType;
  LAttribute: TNyxAttribute;
  LIndex: Integer;
  LRejected: Boolean;
const
  CReserved: array[0..9] of TNyxText = ('NKPANEL', 'NLGRID', 'BPTEXT',
    'NESEQUENTIAL', 'NSOSIDEBYSIDE', 'CMEDITABLE', 'ATWIDTHSIZING',
    'NJSPACEEVENLY', 'NCODESCRIPTOR', 'CSAPPLICATION');

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Source vocabulary: ' + AReason);
    end;
    Inc(Result);
  end;

  procedure RoundTrip;
  begin
    LWorkspace.Reset;
    LSource := TNyxCodegen.Generate(LDocument);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    try
      Check(TNyxCodec.Encode(LCandidate) = TNyxCodec.Encode(LDocument),
        'current typed choices reconstruct exact document meaning');
    finally
      LCandidate.Free;
    end;
  end;

  procedure Reject(const ABefore, AAfter: TNyxText);
  begin
    LDraft := EditNyxManagedFixture(LBefore, ABefore, AAfter);
    Check(LDraft <> LBefore, 'invalid fixture changes its intended source region');
    LSession.SetSourceDraft(LDraft);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on ENyxSource do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Source = LBefore) and
      (LSession.Save = LBeforeDesign) and (LSession.DraftSource = LDraft) and
      LSession.CanUndo and not LSession.CanRedo,
      'invalid symbol retains exact accepted pair, buffer and history');
    LSession.DiscardSourceDraft;
  end;

begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LWorkspace := TNyxSourceWorkspace.Create;
  LSession := TNyxStudioSession.Create;
  try
    LRoot := TNyxNode.Create(nkColumn, 'home');
    LDocument.AddPage(LRoot);
    LRoot.Configure.Text(TNyxText('Literal NLGRID / 🌙 / 漢字')).Done;
    for LLayout := Low(TNyxLayoutMode) to High(TNyxLayoutMode) do
    begin
      LRoot.Configure.Layout(LLayout).Done;
      RoundTrip;
    end;
    for LWrap := Low(TNyxFlowWrap) to High(TNyxFlowWrap) do
    begin
      LRoot.Configure.Wrap(LWrap).Done;
      RoundTrip;
    end;
    for LAlign := Low(TNyxCrossAlignment) to High(TNyxCrossAlignment) do
    begin
      LRoot.Configure.Align(LAlign).Done;
      RoundTrip;
    end;
    for LJustify := Low(TNyxJustification) to High(TNyxJustification) do
    begin
      LRoot.Configure.Justify(LJustify).Done;
      RoundTrip;
    end;
    for LSizing := Low(TNyxSizing) to High(TNyxSizing) do
    begin
      LRoot.Configure.WidthSizing(LSizing).HeightSizing(LSizing).Done;
      RoundTrip;
    end;
    for LTouch := Low(TNyxTouchBehavior) to High(TNyxTouchBehavior) do
    begin
      LRoot.Configure.TouchBehavior(LTouch).Done;
      RoundTrip;
    end;
    for LVariant := Low(TNyxVariant) to High(TNyxVariant) do
    begin
      LRoot.Configure.Variant(LVariant).Done;
      RoundTrip;
    end;
    for LAction := Low(TNyxAction) to High(TNyxAction) do
    begin
      LRoot.Configure.Action(LAction).Done;
      RoundTrip;
    end;
    for LInput := Low(TNyxInputType) to High(TNyxInputType) do
    begin
      LRoot.Configure.InputType(LInput).Done;
      RoundTrip;
    end;
    for LAttribute := Low(TNyxAttribute) to High(TNyxAttribute) do
    begin
      LRoot.Configure.Clear(LAttribute).Done;
      RoundTrip;
    end;

    LRoot.Configure.Layout(nlColumn).Variant(nvPrimary).Action(naToggle)
      .Text(TNyxText('Literal NLGRID / 🌙 / 漢字')).Done;
    LSource := TNyxCodegen.Generate(LDocument);
    LSource := EditNyxManagedFixture(LSource, '.Layout(nlColumn)', '.Layout(NLCOLUMN)');
    LSource := EditNyxManagedFixture(LSource, '.Variant(nvPrimary)', '.Variant(NVPRIMARY)');
    LSession.Load(TNyxCodec.Encode(LDocument));
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    Check((LSession.Source = LSource) and
      (LSession.Save = TNyxCodec.Encode(LDocument)),
      'mixed-case enum source preserves exact typed meaning and literal text');
    LBefore := LSession.Source;
    LBeforeDesign := LSession.Save;
    for LIndex := Low(CReserved) to High(CReserved) do
    begin
      Reject('LHomeColumn', CReserved[LIndex]);
    end;
    Reject('.Layout(NLCOLUMN)', '.Layout(neUIQueue)');
    Reject('.Variant(NVPRIMARY)', '.Variant(nlGrid)');
    Reject('.Layout(NLCOLUMN)', '.Layout(nlColumnLike)');
    LSession.Select('home');
    LSession.SetProperty('text', 'Visual edit keeps authored enums');
    Check((Pos('.Layout(NLCOLUMN)', LSession.Source) > 0) and
      (Pos('.Variant(NVPRIMARY)', LSession.Source) > 0),
      'ordinary visual reconciliation preserves authored enum spelling');
    LSession.Undo;
    Check((LSession.Source = LBefore) and (LSession.Save = LBeforeDesign),
      'paired Undo restores exact typed source and design after visual edit');
    LSession.Redo;
    Check(LSession.Document.Pages[0].Prop('text') = 'Visual edit keeps authored enums',
      'paired Redo restores the visual operation');
  finally
    LSession.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

end.
