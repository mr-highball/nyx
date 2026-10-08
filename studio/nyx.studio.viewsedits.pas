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

unit nyx.studio.viewsedits;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text, nyx.studio.projects;

const
  { Counts Unicode scalars on both targets, independently of UTF-8/UTF-16 storage.
    Inspection remains paged; this bounds one explicit authoring proposal. }
  NyxMaximumViewsCharacters = 262144;

type
  { Immutable source intent, owning exact expected/replacement builder text.
    The delimited body includes its whitespace and comments, but neither marker.
    Candidate uses ordinary source Apply on an independent session and returns
    owned paired text. The publishing controller adds exactly one Undo entry.
    No application helpers, imports, markers or private compiler settings can be
    supplied through this contract. Pending drafts and unsupported Pascal refuse
    without changing the supplied pair. Admission never executes application code. }
  INyxViewsPatch = interface(IInterface)
    ['{43956E70-832E-46D7-89F5-E2A79CD6E937}']
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
  end;

{ Capture a complete exact builder replacement. Inspect bounded windows at one
  revision and concatenate them for AExpected. Scalar budgets and encoding are
  checked at capture; exact source agreement and grammar are checked by Candidate. }
function NyxViewsPatch(const AExpected, AReplacement: TNyxText): INyxViewsPatch;

implementation

uses
  nyx.model, nyx.source, nyx.studio.session;

type
  TNyxViewsPatch = class(TInterfacedObject, INyxViewsPatch)
  private
    FExpected: TNyxText;
    FReplacement: TNyxText;
  public
    constructor Create(const AExpected, AReplacement: TNyxText);
    function Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
  end;

procedure ValidateBuilderText(const AText: TNyxText);
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AText) do
  begin

    if not NyxNextScalar(AText, LIndex, LScalar) then
    begin
      raise ENyxModel.Create('Views text must contain valid Unicode scalars');
    end;
    Inc(LCount);

    if LCount > NyxMaximumViewsCharacters then
    begin
      raise ENyxModel.Create('Views text exceeds 262144 Unicode scalars');
    end;
  end;
end;

constructor TNyxViewsPatch.Create(const AExpected, AReplacement: TNyxText);
begin
  inherited Create;
  ValidateBuilderText(AExpected);
  ValidateBuilderText(AReplacement);
  FExpected := AExpected;
  FReplacement := AReplacement;
end;

function TNyxViewsPatch.Candidate(const APair: TNyxProjectPair): TNyxProjectPair;
var
  LPrefix: TNyxText;
  LBuilder: TNyxText;
  LSuffix: TNyxText;
  LSource: TNyxText;
  LCheckedPrefix: TNyxText;
  LCheckedBuilder: TNyxText;
  LCheckedSuffix: TNyxText;
  LSession: TNyxStudioSession;
begin

  if APair.Pending then
  begin
    raise ENyxModel.Create('Resolve the pending Pascal draft before views edits');
  end;
  SplitNyxSourceFrame(APair.Source, LPrefix, LBuilder, LSuffix);

  if LBuilder <> FExpected then
  begin
    raise ENyxModel.Create('Views expected text differs from the accepted builder');
  end;

  if LBuilder = FReplacement then
  begin
    raise ENyxModel.Create('Views proposal has no source changes');
  end;
  LSource := LPrefix + FReplacement + LSuffix;
  { The existing lexer distinguishes actual markers from quoted lookalikes and
    rejects duplicate/reordered markers and directives. Repartition before Apply
    so a replacement cannot escape into an application helper or import section. }
  SplitNyxSourceFrame(LSource, LCheckedPrefix, LCheckedBuilder, LCheckedSuffix);

  if (LCheckedPrefix <> LPrefix) or (LCheckedBuilder <> FReplacement) or
    (LCheckedSuffix <> LSuffix) then
  begin
    raise ENyxModel.Create('Views edits must retain the exact source boundaries');
  end;
  LSession := TNyxStudioSession.Create(APair);
  try
    LSession.SetSourceDraft(LSource);
    LSession.ApplySourceDraft;
    Result := LSession.ProjectSnapshot;

    if (Result.Source <> LSource) or Result.Pending then
    begin
      raise ENyxModel.Create('Views admission must preserve exact authored source');
    end;
  finally
    LSession.Free;
  end;
end;

function NyxViewsPatch(const AExpected, AReplacement: TNyxText): INyxViewsPatch;
begin
  Result := TNyxViewsPatch.Create(AExpected, AReplacement);
end;

end.
