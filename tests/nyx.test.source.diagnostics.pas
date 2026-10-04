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

unit nyx.test.source.diagnostics;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxSourceDiagnosticTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.source,
  nyx.studio.session,
  nyx.studio.source;

function RunNyxSourceDiagnosticTests: Integer;
var
  LSession: TNyxStudioSession;
  LBefore, LDesign, LDraft, LText: TNyxText;
  LDiagnostic: TNyxSourceDiagnostic;
  LPanel, LAction: TNyxNode;
  LPosition: Integer;
  LRejected: Boolean;
  LError: ENyxSource;

  procedure Check(ACondition: Boolean; const AMessage: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Source diagnostics: ' + AMessage);
    end;
    Inc(Result);
  end;
begin
  Result := 0;
  LText := '🌙漢字 x' + #13#10 + 'second';
  LPosition := NyxTextPosition(LText, 1, 5);
  Check(Copy(LText, LPosition, 1) = 'x', 'scalar column maps to the exact storage site');
  LError := ENyxSource.CreateAt('Unknown 🌙 field', LText, LPosition);
  try
    Check((LError.Line = 1) and (LError.Column = 5) and
      (LError.DiagnosticText = TNyxText('Pascal 1:5: Unknown 🌙 field')),
      'owned diagnostic keeps exact Unicode text independent of native exceptions');
  finally
    LError.Free;
  end;
  Check(NyxTextPosition(LText, 1, 100) = Pos(#13, LText),
    'long columns clamp before CRLF');
  Check(Copy(LText, NyxTextPosition(LText, 2, 1), 6) = 'second',
    'second line skips the whole CRLF');
  Check(NyxTextPosition(LText, 5, 1) = Length(LText) + 1, 'missing line clamps at EOF');
  LRejected := False;
  try
    NyxTextPosition(LText, 0, 1);
  except
    on LException: EArgumentException do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'invalid coordinates reject');

  LSession := TNyxStudioSession.Create;
  LPanel := nil;
  LAction := nil;
  try
    Check(not LSession.SourceDiagnostic.Defined, 'fresh session has no error');
    LBefore := LSession.Source;
    LDesign := LSession.Save;
    { A lexer failure after supplementary Unicode has a precise complete-file
      coordinate without relying on any generated local/property spelling. }
    LDraft := '// 🌙漢字' + #10 + LBefore + #10 + '{ unfinished';
    LSession.SetSourceDraft(LDraft);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: ENyxSource do
      begin
        LRejected := True;
        LDiagnostic := LSession.SourceDiagnostic;
        Check(LDiagnostic.Defined and (LDiagnostic.Line = LException.Line) and
          (LDiagnostic.Column = LException.Column), 'owned error retains lexer coordinates');
      end;
    end;
    Check(LRejected and (LSession.Source = LBefore) and (LSession.Save = LDesign) and
      (LSession.DraftSource = LDraft), 'error does not replace pair or draft');
    LPanel := BuildNyxSourceDiagnostic(LSession);
    Check((LPanel <> nil) and (LPanel.Find(NyxStudioDiagnosticGoID) <> nil),
      'public Nyx diagnostic includes explicit navigation');
    LAction := LPanel.Find(NyxStudioDiagnosticGoID).Clone;
    Check(RouteNyxSourceDiagnostic(LSession, LAction, ntClick, LDiagnostic) and
      (Copy(LDraft, NyxTextPosition(LDraft, LDiagnostic.Line, LDiagnostic.Column), 1) = '{'),
      'route resolves the current exact draft site');
    LSession.SetSourceDraft(LDraft);
    Check(LSession.SourceDiagnostic.Defined, 'unchanged editor notification retains its error');
    LSession.SetSourceDraft(LBefore);
    Check(not LSession.SourceDiagnostic.Defined, 'editing clears the previous diagnostic');
    LRejected := False;
    try
      RouteNyxSourceDiagnostic(LSession, LAction, ntClick, LDiagnostic);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'stale shell action cannot navigate a changed draft');
    LSession.ApplySourceDraft;
    Check(not LSession.SourceDiagnostic.Defined and (LSession.Source = LBefore),
      'successful/no-op apply retains clean accepted source');
  finally
    LAction.Free;
    LPanel.Free;
    LSession.Free;
  end;
end;

end.
