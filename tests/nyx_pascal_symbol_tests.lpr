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

program nyx_pascal_symbol_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.source, nyx.pascal.project, nyx.pascal.sources,
  nyx.pascal.drafts, nyx.pascal.outline, nyx.controls, nyx.model
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
end;

procedure Run;
const
  CSource: TNyxText = 'unit symbol.fixture;' + #10 +
    '{$mode delphi}{$H+}{$codepage utf8}' + #10 + 'interface' + #10 +
    '// Physical Unicode 🚀 𐐷' + #10 +
    'type' + #10 +
    '  TChoice = (First, Second = (1 shl 3), Third);' + #10 +
    '  TAlias = type String;' + #10 +
    '  TWidget = class' + #10 +
    '  private' + #10 +
    '    FTitle, FHint: String;' + #10 +
    '  public' + #10 +
    '    type TPart = record Value: Integer; end;' + #10 +
    '    const DefaultSize = 12;' + #10 +
    '    var Enabled: Boolean;' + #10 +
    '    procedure Refresh; virtual;' + #10 +
    '    property Title: String read FTitle write FTitle;' + #10 +
    '    property Items[AIndex: Integer]: String read FTitle; default;' + #10 +
    '  end;' + #10 +
    '  TForward = class;' + #10 +
    '  TClassRef = class of TWidget;' + #10 +
    '  TCallback = procedure(AValue: String) of object;' + #10 +
    '  TPair<T> = record Left, Right: T; end;' + #10 +
    '  TFactory<T: class, constructor> = class public Value: T; end;' + #10 +
    '  TWidgetHelper = class helper for TWidget' + #10 +
    '  public function CaptionLength: Integer; end;' + #10 +
    '  TVariant = record' + #10 +
    '    case Tag: Boolean of' + #10 +
    '      True: (Count: Integer);' + #10 +
    '      False: (Text: String);' + #10 +
    '  end;' + #10 +
    '  {$I declarations.inc}' + #10 +
    'const Greeting = ''A friendly name 🚀'';' + #10 +
    'resourcestring Welcome = ''Welcome'';' + #10 +
    'var Current, Previous: TWidget;' + #10 +
    'function MakeWidget: TWidget;' + #10 +
    'implementation' + #10 +
    'procedure TWidget.Refresh;' + #10 +
    'var LocalOnly: Integer;' + #10 +
    'begin LocalOnly := 2; end;' + #10 +
    'function MakeWidget: TWidget;' + #10 +
    'begin Result := nil; end;' + #10 +
    'end.' + #10;
  CInclude: TNyxText = '// Shared declarations 🚀 𐐷' + #10 +
    'TIncluded = record' + #10 +
    '  Caption: String;' + #10 + 'end;' + #10;
  CCurrentPrefix: TNyxText = '{ Current declarations 🚀 𐐷 }' + #10;
  CQuery: TNyxText = 'Unfinished search 🚀';
var
  LProject: INyxPascalProject;
  LSources: INyxPascalSourceSnapshot;
  LIndex: INyxPascalSymbolIndex;
  LOther: INyxPascalSymbolIndex;
  LStatus: INyxPascalRoutineReadStatus;
  LSite: TNyxPascalSymbolSite;
  LMember: TNyxPascalSourceRef;
  LOrdinal: Integer;
  LRejected: Boolean;
  LText: TNyxText;
  LAt: Integer;
  LExpectedCount: Integer;
  LCard: INyxCard;

  function Find(const AName: TNyxText): TNyxPascalSymbolSite;
  var
    LAt: Integer;
  begin
    for LAt := 0 to LIndex.SymbolCount - 1 do
    begin

      if LIndex.SymbolAt(LAt).QualifiedName = AName then
      begin
        Exit(LIndex.SymbolAt(LAt));
      end;
    end;
    raise Exception.Create('Missing declaration: ' + AName);
  end;

  procedure Site(const AName: TNyxText; AKind: TNyxPascalSymbolKind;
    AVisibility: TNyxPascalSymbolVisibility);
  var
    LSite: TNyxPascalSymbolSite;
  begin
    LSite := Find(AName);
    Check((LSite.Kind = AKind) and (LSite.Visibility = AVisibility) and
      LSite.Location.IsDefined, 'Declared kind/visibility/site: ' + AName);
  end;

begin
  LProject := NewNyxPascalProject(NyxPascalUnit('symbol.fixture'), CSource)
    .WithInclude(NyxPascalInclude('declarations.inc'), CInclude);
  LSources := CaptureNyxPascalSources(LProject);
  LIndex := CaptureNyxPascalSymbolIndex(LSources, TNyxPascalSourceRef.Main);
  LExpectedCount := LIndex.SymbolCount;
  Site('TChoice', pskType, psvInterface);
  Site('TChoice.First', pskEnumMember, psvInterface);
  Site('TChoice.Second', pskEnumMember, psvInterface);
  Site('TChoice.Third', pskEnumMember, psvInterface);
  Site('TAlias', pskType, psvInterface);
  Site('TWidget.FTitle', pskField, psvPrivate);
  Site('TWidget.FHint', pskField, psvPrivate);
  Site('TWidget.TPart.Value', pskField, psvPublic);
  Site('TWidget.DefaultSize', pskConstant, psvPublic);
  Site('TWidget.Enabled', pskField, psvPublic);
  Site('TWidget.Title', pskProperty, psvPublic);
  Site('TWidget.Items', pskProperty, psvPublic);
  Site('TForward', pskType, psvInterface);
  Site('TClassRef', pskType, psvInterface);
  Site('TCallback', pskType, psvInterface);
  Site('TPair.Left', pskField, psvPublic);
  Site('TPair.Right', pskField, psvPublic);
  Site('TFactory.Value', pskField, psvPublic);
  Site('TWidgetHelper.CaptionLength', pskRoutine, psvPublic);
  Site('TVariant.Tag', pskField, psvPublic);
  Site('TVariant.Count', pskField, psvPublic);
  Site('TVariant.Text', pskField, psvPublic);
  Site('Greeting', pskConstant, psvInterface);
  Site('Welcome', pskResourceString, psvInterface);
  Site('Current', pskVariable, psvInterface);
  Site('Previous', pskVariable, psvInterface);
  LSite := Find('TIncluded.Caption');
  LMember := TNyxPascalSourceRef.IncludeFile(NyxPascalInclude('declarations.inc'));
  Check(LSite.Location.FileReference.SameFile(LMember) and
    (LSite.Location.Line = 3) and (LSite.Location.Column = 3),
    'Expanded declarations retain physical include Unicode positions');
  Check(Supports(LIndex, INyxPascalRoutineReadStatus, LStatus) and
    (LStatus.ProblemText = ''), 'Complete declarations have no recognized lexical fault');
  LCard := NewNyxPascalSymbolOutline('symbols', LProject, LIndex,
    TNyxPascalSourceRef.Main, CQuery, 'twidget.title', 200, pssAccepted);
  Check(LCard.Node.Find('symbols-summary').Prop('text') = '1 declaration / accepted source',
    'Public declaration card clamps pages and shares full-fold name/scope search');
  Check((LCard.Node.Find('symbols-query').Prop('value') = CQuery) and
    (LCard.Node.Find('symbols-content').Prop('value') = 'Declarations'),
    'Declared-structure presentation keeps exact unfinished query and closed content choice');
  LCard := NewNyxPascalSymbolOutline('symbols', LProject, LIndex,
    TNyxPascalSourceRef.Main, '', 'property', 0, pssEditing);
  Check(LCard.Node.Find('symbols-summary').Prop('text') = '2 declarations / editing buffers',
    'Property intent is searchable without dumping declaration text');
  Check((NyxPascalOutlineContent('Declarations') = pocDeclarations) and
    (NyxPascalOutlineContent('Routines') = pocRoutines),
    'Content captions decode at the presentation boundary into typed choices');
  LCard := nil;
  for LOrdinal := 0 to LIndex.SymbolCount - 1 do
  begin
    Check(LIndex.SymbolAt(LOrdinal).Name <> 'LocalOnly',
      'Implementation locals never masquerade as unit fields or constants');
  end;
  LOther := LIndex;
  LIndex := nil;
  LProject := nil;
  LSources := nil;
  Check(LOther.SymbolCount = LExpectedCount, 'Detached declaration lifetime is independent of all providers');
  LRejected := False;
  try
    LOther.SymbolAt(LOther.SymbolCount);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Invalid declaration ordinals refuse');

  LProject := NewNyxPascalProject(NyxPascalUnit('symbol.fixture'), CSource)
    .WithInclude(NyxPascalInclude('declarations.inc'), CInclude);
  LSources := CaptureNyxPascalSources(LProject).WithBuffer(LMember,
    CCurrentPrefix + CInclude +
    'TIncomplete = record Missing: String');
  LIndex := CaptureNyxPascalSymbolIndex(LSources, TNyxPascalSourceRef.Main);
  LSite := Find('TIncluded.Caption');
  Check((LSite.Location.Line = 4) and LSite.Location.FileReference.SameFile(LMember),
    'Current helper text shifts only its own physical declaration position');
  Check(Supports(LIndex, INyxPascalRoutineReadStatus, LStatus) and
    (LStatus.ProblemText <> '') and LStatus.ProblemLocation.IsDefined and
    LStatus.ProblemLocation.FileReference.SameFile(LMember),
    'Incomplete editing declaration retains complete prefix and exact physical fault');

  LText := 'unit small; interface const Before = 1;' + #10 +
    '{$ifdef UNKNOWN}type Conditional = Integer;{$endif}' + #10 +
    'implementation end.';
  LProject := NewNyxPascalProject(NyxPascalUnit('small'), LText);
  LIndex := CaptureNyxPascalSymbolIndex(CaptureNyxPascalSources(LProject),
    TNyxPascalSourceRef.Main);
  Check((LIndex.SymbolCount = 1) and (Find('Before').Kind = pskConstant),
    'Conditional scope never fabricates a chosen branch or later declaration');
  Check(Supports(LIndex, INyxPascalRoutineReadStatus, LStatus) and
    (LStatus.ProblemText <> ''), 'Conditional inspection explicitly reports its unresolved boundary');

  LText := 'unit bounded; interface const ';

  for LAt := 0 to 4096 do
  begin
    LText := LText + 'Name' + IntToStr(LAt) + '=1;' + #10;
  end;
  LText := LText + 'implementation end.';
  LProject := NewNyxPascalProject(NyxPascalUnit('bounded'), LText);
  LRejected := False;
  try
    LIndex := CaptureNyxPascalSymbolIndex(CaptureNyxPascalSources(LProject),
      TNyxPascalSourceRef.Main);
  except
    on Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'Hard declaration quotas never return a partial success');
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' declaration catalog checks';
    {$else}
    WriteLn('PASS ', GChecks, ' declaration catalog checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, E.Message);
      Halt(1);
      {$endif}
    end;
  end;
end.
