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
program nyx_pascal_outline_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.source,
  nyx.pascal.project, nyx.pascal.drafts, nyx.pascal.outline,
  nyx.studio.view, nyx.studio.presentation
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
end;

procedure Run;
const
  CQuery: TNyxText = 'An unfinished query 🚀 𐐷';
  CSignature: TNyxText = 'Rocket 🚀 Straße 𐐀';
  COtherSignature: TNyxText = 'Rocket 𐐷';
  CFoldedSignature: TNyxText = 'rocket 🚀 STRASSE 𐐨';
var
  LProject: INyxPascalProject;
  LIndex: INyxPascalRoutineIndex;
  LSource: TNyxText;
  LOrdinal: Integer;
  LCard: INyxCard;
  LRow: Integer;
  LPart: TNyxPascalOutlinePart;
  LAction: TNyxPascalOutlineAction;
  LRefused: Boolean;
  LPreferences: TNyxStudioPresentation;
  LRestored: TNyxStudioPresentation;
begin
  { Lexical UI fixture only: no compiler, constructor or application execution
    is inferred from this independently owned fourteen-routine translation unit. }
  LSource := 'unit outline.samples;' + #10 + 'interface' + #10 + 'implementation' + #10;
  for LOrdinal := 0 to 13 do
  begin
    LSource := LSource + 'function Example' + IntToStr(LOrdinal) +
      '(AValue: Integer; const ACaption: String = ''' + CSignature + '''): Integer;' + #10 + 'begin' + #10 +
      '  Result := AValue;' + #10 + 'end;' + #10;
  end;
  LSource := LSource + 'end.' + #10;
  LProject := NewNyxPascalProject(NyxPascalUnit('outline.samples'), LSource);
  LIndex := CaptureNyxPascalRoutineIndex(LProject, TNyxPascalSourceRef.Main);
  Check(LIndex.Count = 14, 'outline fixture contains fourteen independent routines');
  LCard := NewNyxPascalOutline('outline', LProject, LIndex,
    TNyxPascalSourceRef.Main, '', '', 0);
  Check((LCard.Node.Find('outline-row-0') <> nil) and
    (LCard.Node.Find('outline-row-11') <> nil) and
    (LCard.Node.Find('outline-row-12') = nil), 'first page mounts at most twelve routine rows');
  Check((LCard.Node.Find('outline-previous').Prop('enabled') = 'false') and
    (LCard.Node.Find('outline-next').Prop('enabled') = 'true'),
    'first page has only forward navigation');
  Check(LCard.Node.Find('outline-go-0-1').Prop('enabled') = 'false',
    'an absent declaration counterpart is visibly disabled');
  LAction := NyxPascalOutlineAction('outline', LCard.Node.Find('outline-go-0-2'),
    ntClick, LRow, LPart);
  Check((LAction = poaNavigate) and (LRow = 0) and (LPart = popBody),
    'body action resolves a typed counterpart and revision-local ordinal');
  Check(NyxPascalOutlineLocation(LIndex.Item(LRow), LPart).IsDefined,
    'body intent retains a real physical site');
  Check(NyxPascalOutlineAction('outline', LCard.Node.Find('outline-go-0-2'),
    ntChange, LRow, LPart) = poaNone, 'navigation never runs on a field-change trigger');
  LCard := NewNyxPascalOutline('outline', LProject, LIndex,
    TNyxPascalSourceRef.Main, '', '', MaxInt);
  Check((LCard.Node.Find('outline-row-12') <> nil) and
    (LCard.Node.Find('outline-row-13') <> nil) and
    (LCard.Node.Find('outline-row-11') = nil), 'oversized page clamps to the final bounded slice');
  Check((LCard.Node.Find('outline-previous').Prop('enabled') = 'true') and
    (LCard.Node.Find('outline-next').Prop('enabled') = 'false'),
    'last page has only backward navigation');
  LCard := NewNyxPascalOutline('outline', LProject, LIndex,
    TNyxPascalSourceRef.Main, '', '', -8);
  Check(LCard.Node.Find('outline-row-0') <> nil, 'negative page clamps to the first slice');
  LCard := NewNyxPascalOutline('outline', LProject, LIndex,
    TNyxPascalSourceRef.Main, 'example13', 'example13', 0);
  Check((LCard.Node.Find('outline-summary').Prop('text') = '1 routine / accepted source') and
    (LCard.Node.Find('outline-row-13') <> nil) and
    (LCard.Node.Find('outline-row-1') = nil), 'name filtering preserves original row identities');
  LCard := NewNyxPascalOutline('outline', LProject, LIndex,
    TNyxPascalSourceRef.Main, CQuery, 'INTEGER', 0);
  Check((LCard.Node.Find('outline-summary').Prop('text') = '14 routines / accepted source') and
    (LCard.Node.Find('outline-query').Prop('value') = CQuery),
    'applied signature search is independent of exact unfinished Unicode input');
  LCard := NewNyxPascalOutline('outline', LProject, LIndex,
    TNyxPascalSourceRef.Main, CSignature, CSignature, 0);
  Check(LCard.Node.Find('outline-summary').Prop('text') = '14 routines / accepted source',
    'signature search retains exact supplementary Unicode operands');
  LCard := NewNyxPascalOutline('outline', LProject, LIndex,
    TNyxPascalSourceRef.Main, COtherSignature, COtherSignature, 0);
  Check(LCard.Node.Find('outline-summary').Prop('text') = '0 routines / accepted source',
    'distinct supplementary signature text cannot collapse to an ANSI question mark');
  LCard := NewNyxPascalOutline('outline', LProject, LIndex,
    TNyxPascalSourceRef.Main, CFoldedSignature, CFoldedSignature, 0);
  Check(LCard.Node.Find('outline-summary').Prop('text') = '14 routines / accepted source',
    'outline reuses full Unicode folding including expansion and supplementary case pairs');
  LCard := NewNyxPascalOutline('outline', LProject, LIndex,
    TNyxPascalSourceRef.Main, 'NoMatch', 'NoMatch', 0);
  Check((LCard.Node.Find('outline-summary').Prop('text') = '0 routines / accepted source') and
    (LCard.Node.Find('outline-next').Prop('enabled') = 'false') and
    (LCard.Node.Find('outline-row-0') = nil), 'empty search leaves a usable bounded card');
  LCard := NewNyxPascalOutline('outline', LProject, nil,
    TNyxPascalSourceRef.Main, '', '', 0, 'A readable lexical limit');
  Check(LCard.Node.Find('outline-message').Prop('text') = 'A readable lexical limit',
    'failed discovery remains readable without invented routine rows');
  LRefused := False;
  try
    LCard := NewNyxPascalOutline('outline', LProject, LIndex,
      TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('shared.inc')), '', '', 0);
  except
    on ENyxPascalProject do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'include scope cannot masquerade as an owning unit');
  LRefused := False;
  try
    LCard := NewNyxPascalOutline('outline', LProject, LIndex,
      TNyxPascalSourceRef.UnitFile(NyxPascalUnit('other.owner')), '', '', 0);
  except
    on ENyxPascalProject do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, 'an index from another owner refuses before building a misleading form');
  LCard := NewNyxPascalOutline('outline', LProject, LIndex,
    TNyxPascalSourceRef.Main, '', 'Example13', 0);
  LIndex := nil;
  LProject := nil;
  Check(LCard.Node.Find('outline-name-13').Prop('text') = 'Example13',
    'copied card content survives complete input/index retirement');
  LPreferences := DefaultNyxStudioPresentation;
  LPreferences.SourceTab := nstOutline;
  LRestored := DecodeNyxStudioPresentation(EncodeNyxStudioPresentation(LPreferences));
  Check(LRestored.SourceTab = nstOutline, 'ordinary presentation round-trips the outline tab');
  Check((Ord(nstSource) = 0) and (Ord(nstMessages) = 1) and (Ord(nstFiles) = 2),
    'historical source preference ordinals remain unchanged');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-outline', 'passed');
    document.body.setAttribute('data-outline-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' Pascal outline contracts');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-outline', 'failed');
      document.body.textContent := LException.Message;
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
