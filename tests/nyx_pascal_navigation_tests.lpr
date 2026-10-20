{ Nyx source-navigation qualification. MIT license, see LICENSE.
  Pure source values exercise retained trails without compiler, filesystem,
  project admission, authored execution or a platform UI. }
program nyx_pascal_navigation_tests;
{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.editing, nyx.text.projection, nyx.pascal.names,
  nyx.pascal.drafts, nyx.pascal.navigation, nyx.scheduler
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AMessage: TNyxText);
begin
  Inc(GChecks);

  if not AValue then raise Exception.Create(AMessage);
end;

procedure Run;
const
  CContext: TNyxText = 'begin' + #13#10 + '  Greeting := ''Hello 🚀 𐐷'';' + #13#10 +
    '  Status := Greeting;' + #13#10 + 'end;';
var
  LMain, LUnit: TNyxPascalSourceRef;
  LA, LB, LC, LDepart, LPoint, LBad: TNyxPascalNavigationPoint;
  LTrail, LOld, LNext, LBounded: INyxPascalNavigationTrail;
  LStatus: TNyxPascalNavigationStatus;
  LCancel: INyxCancellationScope;
  LText, LOther, LRepeated: TNyxText;
  LCaret, LLine, LColumn, LAt: Integer;
  LRejected: Boolean;
begin
  LMain := TNyxPascalSourceRef.Main;
  LUnit := TNyxPascalSourceRef.UnitFile(NyxPascalUnit('Helpers.Greetings'));
  LA := NyxPascalNavigationPoint(LMain, CContext, NyxTextScalarCount('begin' + #13#10 + '  Greeting'));
  LB := NyxPascalNavigationPoint(LUnit, 'unit Helpers.Greetings;' + #10 + 'interface', 5);
  LC := NyxPascalNavigationPoint(LMain, CContext, NyxTextScalarCount(CContext));
  LTrail := NewNyxPascalNavigationTrail;
  Check((LTrail.Count = 0) and (LTrail.Position = -1) and not LTrail.Current.Defined and
    not LTrail.CanTravel(pndBack) and not LTrail.CanTravel(pndForward), 'An empty trail retains no invented location');
  LOld := LTrail;
  LTrail := LTrail.Visit(LA, LB);
  Check((LOld.Count = 0) and (LTrail.Count = 2) and (LTrail.Position = 1) and
    LTrail.Current.SameLocation(LB) and LTrail.CanTravel(pndBack), 'Visit publishes a detached two-location trail');
  LDepart := NyxPascalNavigationPoint(LUnit, LB.Text, 12);
  LNext := LTrail.Travel(pndBack, LDepart, LA.Text, LStatus);
  Check((LStatus = pnsExact) and LNext.Current.SameLocation(LA) and
    (LTrail.Position = 1) and LNext.CanTravel(pndForward), 'Back preserves its previous immutable reader');
  LTrail := LNext.Travel(pndForward, LA, LB.Text, LStatus);
  Check((LStatus = pnsExact) and LTrail.Current.SameLocation(LDepart),
    'Forward returns to the actual departure caret, not the original landing caret');
  LNext := LTrail.Travel(pndBack, LDepart, LA.Text, LStatus);
  LTrail := LNext.Visit(LA, LC);
  Check((LTrail.Count = 2) and not LTrail.CanTravel(pndForward) and
    LTrail.Current.SameLocation(LC) and LNext.CanTravel(pndForward),
    'A new jump branches without mutating the old forward history');
  LOld := LTrail;
  LTrail := LTrail.Visit(LC, LC);
  Check(LTrail = LOld, 'An identical source/caret jump adds no duplicate landmark');
  LNext := LTrail.Travel(pndForward, LC, CContext, LStatus);
  Check((LNext = nil) and (LStatus = pnsUnavailable) and (LTrail = LOld),
    'Unavailable traversal returns no candidate and preserves the trail');
  LA.Position(LLine, LColumn);
  Check((LLine = 2) and (LColumn = 11), 'Logical positions count CRLF once and columns as scalars');
  LPoint := NyxPascalNavigationPoint(LMain, '🚀𐐷' + #13 + 'x' + #10 + 'y', 5);
  LPoint.Position(LLine, LColumn);
  Check((LLine = 3) and (LColumn = 1), 'Supplementary scalars and mixed lone newlines retain exact positions');

  for LAt := 0 to 2 do
  begin
    case LAt of
      0: LText := NyxPresentText(CContext, nteLF);
      1: LText := NyxPresentText(CContext, nteCR);
    else LText := '// new preface' + #10 + NyxPresentText(CContext, nteLF);
    end;
    LStatus := LA.Resolve(LText, LCaret);
    Check((LStatus = pnsRelocated) and (LCaret >= 0), 'Authored newline projection/prefix relocation succeeds ' + IntToStr(LAt));
    LPoint := NyxPascalNavigationPoint(LMain, LText, LCaret);
    LPoint.Position(LLine, LColumn);
    Check((LColumn = 11) and (LLine = 2 + Ord(LAt = 2)), 'Relocated logical source caret remains exact ' + IntToStr(LAt));
  end;
  LText := 'before ' + StringOfChar('a', 40) + CContext + StringOfChar('b', 40) + ' after';
  LPoint := NyxPascalNavigationPoint(LMain, LText,
    NyxTextScalarCount('before ' + StringOfChar('a', 40) + CContext) - 6);
  LOther := 'new unrelated prefix ' + StringOfChar('a', 40) + CContext +
    StringOfChar('b', 40) + ' changed unrelated suffix';
  LStatus := LPoint.Resolve(LOther, LCaret);
  Check((LStatus = pnsRelocated) and (LCaret = NyxTextScalarCount(
    'new unrelated prefix ' + StringOfChar('a', 40) + CContext) - 6),
    'A unique surrounding context survives multiple disjoint outside edits');
  LPoint := NyxPascalNavigationPoint(LMain, StringOfChar('a', 80) + ' target ' + StringOfChar('b', 80), 84);
  LRepeated := 'changed ' + LPoint.Text + ' / ' + LPoint.Text + ' tail';
  LStatus := LPoint.Resolve(LRepeated, LCaret);
  Check((LStatus = pnsAmbiguous) and (LCaret = -1), 'Repeated exact context refuses rather than selecting its first occurrence');
  LStatus := LPoint.Resolve('The old target was removed.', LCaret);
  Check((LStatus = pnsChanged) and (LCaret = -1), 'Removed/replaced context publishes no approximate caret');
  LStatus := LA.Resolve('begin' + #13#10 + '  Welcome := ''Goodbye'';' + #13#10 + 'end;', LCaret);
  Check((LStatus = pnsChanged) and (LCaret = -1), 'A changed target inside the retained context is not a semantic rename guess');
  LCancel := NewNyxCancellationScope;
  LCancel.Cancel;
  LStatus := LA.Resolve(CContext, LCaret, LCancel);
  Check((LStatus = pnsCancelled) and (LCaret = -1), 'Cancellation refuses even an unchanged target');
  LNext := LOld.Travel(pndBack, LC, CContext, LStatus, LCancel);
  Check((LNext = nil) and (LStatus = pnsCancelled) and (LOld.Position = 1),
    'Cancelled traversal never moves the prior cursor');
  LRejected := False;
  try
    NyxPascalNavigationPoint(LMain, 'x' + #13#10 + 'y', 2);
  except
    on ENyxPascalNavigation do LRejected := True;
  end;
  Check(LRejected, 'A logical caret inside CRLF refuses instead of silently choosing an edge');

  LBounded := NewNyxPascalNavigationTrail(TNyxPascalNavigationLimits.Defaults.UpToEntries(2));
  LBounded := LBounded.Visit(LA, LB);
  LBounded := LBounded.Visit(LB, LC);
  Check((LBounded.Count = 2) and LBounded.Destination(pndBack).SameLocation(LB) and
    LBounded.Current.SameLocation(LC), 'Entry eviction preserves the immediate return pair');
  LBounded := NewNyxPascalNavigationTrail(TNyxPascalNavigationLimits.Defaults
    .UpToBytes(LA.RetainedBytes + LB.RetainedBytes));
  LBounded := LBounded.Visit(LA, LB);
  Check(LBounded.RetainedBytes = LA.RetainedBytes + LB.RetainedBytes,
    'Accounted bytes include each copied text, selector and fixed location allowance');
  LRejected := False;
  LOld := LBounded;
  LNext := nil;
  try
    LNext := LBounded.Visit(LB, NyxPascalNavigationPoint(LMain, StringOfChar('z', 1024), 0));
  except
    on ENyxPascalNavigation do LRejected := True;
  end;
  Check(LRejected and (LNext = nil) and (LBounded = LOld) and (LOld.Position = 1),
    'An unretainable immediate pair refuses atomically instead of dropping the return location');
  LBounded := NewNyxPascalNavigationTrail(TNyxPascalNavigationLimits.Defaults.UpToEntries(0));
  LBounded := LBounded.Visit(LA, LB);
  Check((LBounded.Count = 0) and (LBounded.RetainedBytes = 0), 'Explicit zero-entry policy disables retention');
  LRejected := False;
  try
    NyxPascalNavigationPoint(LMain, StringOfChar('x', 4 * 1024 * 1024 + 1), 0);
  except
    on ENyxPascalNavigation do LRejected := True;
  end;
  Check(LRejected, 'Source retention never enlarges the existing four-MiB inspected buffer allowance');
  LText := StringOfChar('x', 4 * 1024 * 1024 + 1);
  {$ifdef PAS2JS}
  LText := LText + Char($D800);
  {$else}
  LText := LText + AnsiChar($FF);
  {$endif}
  for LAt := 0 to 1 do
  begin
    LRejected := False;
    try

      if LAt = 0 then NyxPascalNavigationPoint(LMain, LText, 0)
      else LA.Resolve(LText, LCaret);
    except
      on E: ENyxPascalNavigation do
        LRejected := Pos('four-MiB allowance', E.Message) > 0;
    end;
    Check(LRejected, 'Storage-size preflight refuses before an oversized malformed suffix is decoded ' + IntToStr(LAt));
  end;
  for LAt := 0 to 4 do
  begin
    LBad := Default(TNyxPascalNavigationPoint);
    LRejected := False;
    try
      case LAt of
        0: LBad.Validate;
        1: NyxPascalNavigationPoint(LMain, 'x', 2);
        2: TNyxPascalNavigationLimits.Defaults.UpToEntries(1);
        3: TNyxPascalNavigationLimits.Defaults.UpToEntries(1025);
        4: TNyxPascalNavigationLimits.Defaults.UpToBytes(-1);
      end;
    except
      on ENyxPascalNavigation do LRejected := True;
    end;
    Check(LRejected, 'Malformed point/policy refuses ' + IntToStr(LAt));
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' source navigation checks';
    {$else}
    WriteLn('PASS ', GChecks, ' source navigation checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + E.Message;
      {$else}
      WriteLn(StdErr, 'FAIL ', E.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
