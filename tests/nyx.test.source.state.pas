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

unit nyx.test.source.state;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxSourceStateTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.state,
  nyx.codec,
  nyx.binding.types,
  nyx.source,
  nyx.studio.session,
  nyx.test.binding;

function ReplaceFirst(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  { Typed concatenation retains the native UTF-8 source codepage; no ANSI
    collection or chained RTL result stands between the fixture and reader. }
  LPosition := Pos(ABefore, ASource);

  if LPosition = 0 then
  begin
    raise Exception.Create('Source-state fixture cannot find its authored edit');
  end;
  Result := Copy(ASource, 1, LPosition - 1) + AAfter +
    Copy(ASource, LPosition + Length(ABefore), MaxInt);
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL source state: ' + AMessage);
  end;
  Inc(ACount);
end;

function RunNyxSourceStateTests: Integer;
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LSource: TNyxText;
  LDraft: TNyxText;
  LBaseline: TNyxText;
  LAccepted: TNyxText;
  LSpec: TNyxBindingSpec;
  LRejected: Boolean;

  procedure Reject(const ABefore, AAfter, AReason: TNyxText);
  begin
    LDraft := ReplaceFirst(LSource, ABefore, AAfter);
    LCandidate := nil;
    LRejected := False;
    try
      try
        LCandidate := LWorkspace.Candidate(LDocument, LDraft);
      except
        on LException: Exception do
        begin
          LRejected := True;
          Check(LException.Message <> '', 'diagnostic: ' + AReason, Result);
        end;
      end;
    finally
      LCandidate.Free;
      LCandidate := nil;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LBaseline) and
      (LWorkspace.Render(LDocument) = LSource), 'atomic refusal: ' + AReason, Result);
  end;

begin
  Result := 0;
  LDocument := CreateNyxBindingFixture;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LBaseline := TNyxCodec.Encode(LDocument);
    LSource := LWorkspace.Render(LDocument);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    try
      Check(TNyxCodec.Encode(LCandidate) = LBaseline,
        'complete binding/default fixture reads exactly', Result);
    finally
      LCandidate.Free;
    end;
    LDraft := ReplaceFirst(LSource, 'NyxTextState(''🌙/reply'')',
      'NyxTextState(''discussion / 🌙'')');
    LDraft := ReplaceFirst(LDraft, '.SetValue(LReplyTextState, ''Café / 🌙 / 漢字'')',
      '.SetValue(LReplyTextState, TNyxText(''Typed reply / 🌙'') + TNyxText(#10) + ''second line'')');
    LDraft := ReplaceFirst(LDraft, '.SetValue(LRatioNumberState, 0.1)',
      '.SetValue(LRatioNumberState, 1)');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(not LCandidate.State.Has('🌙/reply') and
        (LCandidate.State.GetValue(NyxTextState('discussion / 🌙')) =
        TNyxText('Typed reply / 🌙') + NyxScalarText(10) + 'second line'),
        'typed reference rename/default retains exact multiline Unicode', Result);
      Check(LCandidate.Find('reply-memo').Bindings[0].StateName = TNyxText('discussion / 🌙'),
        'named reference migrates memo binding', Result);
      Check(LCandidate.Find('review-caption').Bindings[0].StateName = TNyxText('discussion / 🌙'),
        'named reference migrates unmounted-page binding', Result);
      Check((LCandidate.State.Value('ratio').Kind = nskNumber) and
        (LCandidate.State.GetValue(NyxNumberState('ratio')) = 1.0),
        'Pascal integer widening retains Number family', Result);
    finally
      LCandidate.Free;
    end;
    LDraft := ReplaceFirst(LSource, '.SetValue(LWidthIntegerState, 300)',
      '.SetValue(LWidthIntegerState, 360)' + #10 +
      '      .SetValue(NyxTextState(''status''), ''Ready / 🌙'')');
    LDraft := ReplaceFirst(LDraft, 'LReplyMemo.Binds' + #10,
      'LReplyMemo.Binds' + #10 + '      .Hint(NyxTextState(''status''))' + #10);
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(LCandidate.State.GetValue(NyxTextState('status')) = TNyxText('Ready / 🌙'),
        'inline typed constructor admits a new default', Result);
      Check(LCandidate.Find('reply-memo').FindBinding(bpHint, LSpec) and
        (LSpec.ValueKind = nskText) and (LSpec.StateName = 'status'),
        'new fluent binding shares that default', Result);
      Check(LCandidate.State.GetValue(NyxIntegerState('width')) = 360,
        'existing integer default updates', Result);
    finally
      LCandidate.Free;
    end;
    LDraft := ReplaceFirst(LSource, 'LReplyMemo.Binds' + #10,
      'LReplyMemo.Binds' + #10 + '      .Text(NyxIntegerState(''quantity''))' + #10);
    LDraft := ReplaceFirst(LDraft, '.Value(LReplyTextState)',
      '.Value(LReplyTextState, bdFromState)');
    LDraft := ReplaceFirst(LDraft, '.ReadOnly(LReadonlyBooleanState)',
      '.Clear(bpReadOnly)');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(LCandidate.Find('reply-memo').FindBinding(bpText, LSpec) and
        (LSpec.ValueKind = nskInteger), 'caption intentionally projects an Integer', Result);
      Check(LCandidate.Find('reply-memo').FindBinding(bpValue, LSpec) and
        (LSpec.Direction = bdFromState), 'Value accepts typed direction', Result);
      Check(not LCandidate.Find('reply-memo').FindBinding(bpReadOnly, LSpec) and
        (LCandidate.Find('reply-memo').BindingCount = 4),
        'Clear retains explicit local unbinding', Result);
    finally
      LCandidate.Free;
    end;
    LDraft := ReplaceFirst(LSource, '.ReadOnly(LReadonlyBooleanState)',
      '.Inherit(bpReadOnly)');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(not LCandidate.Find('reply-memo').FindBinding(bpReadOnly, LSpec) and
        (LCandidate.Find('reply-memo').BindingCount = 2),
        'Inherit removes only the local descriptor', Result);
    finally
      LCandidate.Free;
    end;
    LDraft := ReplaceFirst(LSource, '.SetValue(LWidthIntegerState, 300)',
      '.SetValue(LWidthIntegerState, 300)' + #10 +
      '      .SetValue(NyxTextState(''temporary''), ''value'')' + #10 +
      '      .Remove(NyxTextState(''temporary''))');
    LCandidate := LWorkspace.Candidate(LDocument, LDraft);
    try
      Check(not LCandidate.State.Has('temporary'), 'typed Remove follows prior source operations', Result);
    finally
      LCandidate.Free;
    end;
    Reject('.SetValue(LQuantityIntegerState, 2)', '.SetValue(LQuantityIntegerState, ''2'')',
      'integer text');
    Reject('.SetValue(LQuantityIntegerState, 2)', '.SetValue(LQuantityIntegerState, 1.5)',
      'fractional integer');
    Reject('.SetValue(LEnabledBooleanState, True)', '.SetValue(LEnabledBooleanState, ''true'')',
      'Boolean text');
    Reject('NyxTextState(''🌙/reply'')', 'NyxBooleanState(''🌙/reply'')', 'declared reference type');
    Reject('.SetValue(LRatioNumberState, 0.1)', '.SetValue(LRatioNumberState, 1e999)',
      'nonfinite default');
    Reject('.SetValue(LRatioNumberState, 0.1)', '.SetValue(LRatioNumberState, 1e-999)',
      'nonzero underflow');
    Reject('.SetValue(LWidthIntegerState, 300)', '.SetValue(LWidthIntegerState, 120000)',
      'bound layout admission');
    Reject('.Enabled(LEnabledBooleanState)', '.Enabled(LReplyTextState)', 'binding target family');
    Reject('.Value(LReplyTextState)', '.Value(''🌙/reply'')', 'reference text refused');
    Reject('.Value(LReplyTextState)', '.Value(LReplyTextState, ''from-state'')', 'direction text');
    Reject('.Value(LReplyTextState)', '.Value(NyxTextState(''missing''))', 'missing default');
    Reject('.Value(LReplyTextState)', '.Value(LUnknownTextState)', 'undeclared reference');
    Reject('.Value(LReplyTextState)', '.Clear(atValue)', 'attribute is not binding property');
    Reject('.SetValue(LQuantityIntegerState, 2)', '.Remove(LQuantityIntegerState)',
      'removal of used default');
    Reject('LReplyTextState := NyxTextState(''🌙/reply'');', '',
      'use before initialization');
    Reject('LReplyMemo := NewNyxMemo(', 'LReplyMemo.Binds.Value(LReplyTextState).Done;' + #10 +
      '    LReplyMemo := NewNyxMemo(', 'binding before construction/admission');
    Reject('NewNyxPage(''editor'')', 'NewNyxPage(' +
      'LReplyMemo.Binds.Value(LReplyTextState).Done; ''editor'')', 'statement hidden in constructor');
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;

  LSession := TNyxStudioSession.Create;
  try
    LSession.Load(LBaseline);
    LSource := LSession.Source;
    LDraft := ReplaceFirst(LSource, '.SetValue(LQuantityIntegerState, 2)',
      '.SetValue(LQuantityIntegerState, 4)');
    LSession.SetSourceDraft(LDraft);
    LSession.ApplySourceDraft;
    LAccepted := LSession.Save;
    Check(LSession.Document.State.GetValue(NyxIntegerState('quantity')) = 4,
      'shared session applies a numeric source default', Result);
    LSession.Undo;
    Check((LSession.Save = LBaseline) and (LSession.Source = LSource),
      'paired history restores defaults and their source', Result);
    LSession.Redo;
    Check((LSession.Save = LAccepted) and (LSession.Source = LDraft),
      'paired redo restores defaults/source', Result);
    LDraft := ReplaceFirst(LSession.Source, '.SetValue(LQuantityIntegerState, 4)',
      '.SetValue(LQuantityIntegerState, 5000)');
    LSession.SetSourceDraft(LDraft);
    LRejected := False;
    try
      LSession.ApplySourceDraft;
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LAccepted) and (LSession.DraftSource = LDraft),
      'declared compound range rejects draft without publication', Result);
  finally
    LSession.Free;
  end;
end;

end.
