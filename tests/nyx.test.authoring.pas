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

unit nyx.test.authoring;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxAuthoringTests: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.state,
  nyx.binding.types,
  nyx.binding,
  nyx.model,
  nyx.codec,
  nyx.schema,
  nyx.studio.authoring,
  nyx.studio.commands,
  nyx.studio.session,
  nyx.studio.view,
  nyx.test.binding;

procedure Check(ACondition: Boolean; const AReason: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxState.Create('Authoring: ' + AReason);
  end;
  Inc(ACount);
end;

function RunNyxAuthoringTests: Integer;
const
  CBadInputs: array[0..5] of TNyxText = ('True', '1.5', '2147483648', '0.1oops', '1e-999', '42');
  CBadKinds: array[0..5] of TNyxStudioStateInput = (
    ssiBoolean, ssiInteger, ssiInteger, ssiNumber, ssiNumber, ssiEscapedText);
var
  LSession: TNyxStudioSession;
  LFixture: TNyxDocument;
  LShell: TNyxDocument;
  LAccepted: TNyxDocument;
  LProjection: TNyxNode;
  LState: TNyxStudioViewState;
  LNode: TNyxNode;
  LSpec: TNyxBindingSpec;
  LValue: TNyxStateValue;
  LBefore: TNyxText;
  LSource: TNyxText;
  LIndex: Integer;
  LRejected: Boolean;
  LNewName: TNyxText;
  LExpectedNumber: Double;

  procedure Rebuild;
  begin
    FreeAndNil(LShell);
    LShell := BuildNyxStudioView(LSession, LState);
  end;

begin
  Result := 0;
  LValue := TNyxStateValue.FromText('🌙 / 漢字' + #0 + 'exact');
  Check((NyxStudioStateInputFor(LValue) = ssiEscapedText) and
    (ParseNyxStudioStateInput(ssiEscapedText, NyxStudioStateEditorText(LValue)).TextValue = LValue.TextValue),
    'escaped text editor preserves supplementary Unicode and NUL', Result);
  Check(ParseNyxStudioStateInput(ssiInteger, '-2147483648').IntegerValue = Low(Integer),
    'integer editor admits the signed lower boundary', Result);
  LExpectedNumber := 1.2345678901234567;
  Check(ParseNyxStudioStateInput(ssiNumber, '1.2345678901234567').NumberValue = LExpectedNumber,
    'number editor retains Double precision', Result);
  for LIndex := 0 to High(CBadInputs) do
  begin
    LRejected := False;
    try
      ParseNyxStudioStateInput(CBadKinds[LIndex], CBadInputs[LIndex]);
    except
      on LException: Exception do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'typed default input refuses coercion: ' + CBadInputs[LIndex], Result);
  end;
  LSession := TNyxStudioSession.Create;
  LFixture := CreateNyxBindingFixture;
  LShell := nil;
  try
    LNode := TNyxNode.Create(nkMemo, 'reply-template');
    LFixture.AddComponent(LNode);
    LNode.Binds.Value(NyxTextState('🌙/reply')).Done;
    LFixture.Pages[0].Add(TNyxNode.Create(nkComponent, 'reuse-one').Configure
      .Component(NyxComponent('reply-template')).Done);
    LFixture.Pages[1].Add(TNyxNode.Create(nkComponent, 'reuse-two').Configure
      .Component(NyxComponent('reply-template')).Done);
    LSession.Load(TNyxCodec.Encode(LFixture));
    LSession.Select('reply-memo');
    LBefore := LSession.Save;
    LAccepted := LSession.Document;
    LSession.SetBinding(TNyxBindingSpec.Bound(bpValue, '🌙/reply', nskText, bdTwoWay));
    Check((LSession.Save = LBefore) and (LSession.Document = LAccepted),
      'no-op binding retains the accepted document and history', Result);
    LRejected := False;
    try
      LSession.SetBinding(TNyxBindingSpec.Bound(bpValue, 'checked', nskBoolean, bdTwoWay));
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Document = LAccepted) and (LSession.Save = LBefore),
      'wrong-kind binding preserves accepted identity and source', Result);
    LSession.SetBinding(TNyxBindingSpec.Clear(bpValue));
    LSession.Undo;
    LBefore := LSession.Save;
    LAccepted := LSession.Document;
    LRejected := False;
    try
      LSession.RemoveState('🌙/reply');
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Document = LAccepted) and (LSession.Save = LBefore),
      'removing a referenced default preserves baseline and redo', Result);
    LSession.Redo;
    Check(not LSession.Selected.FindBinding(bpValue, LSpec),
      'rejected removal retains the successful binding redo command', Result);
    LSession.Undo;
    LNewName := 'reply / 🌙 / 漢字';
    LSession.RenameState('🌙/reply', LNewName);
    Check((LSession.Document.State.Key(0) = LNewName) and
      (LSession.Document.State.Count = LFixture.State.Count) and
      (LSession.Document.State.Value(LNewName).TextValue = LFixture.State.Value('🌙/reply').TextValue),
      'state rename retains order, kind and exact default', Result);
    Check((LSession.Document.Find('reply-memo').Bindings[0].StateName = LNewName) and
      (LSession.Document.Find('reply-template').Bindings[0].StateName = LNewName) and
      (LSession.Document.Find('search').Part('query').Bindings[0].StateName = LNewName),
      'rename migrates page, definition and compound part references atomically', Result);
    LSource := LSession.Source;
    Check((Pos('NyxTextState(''🌙/reply'')', LSource) = 0) and
      (Pos('NyxTextState(''' + LNewName + ''')', LSource) > 0),
      'renamed references immediately reach crafted generated source', Result);
    LBefore := LSession.Save;
    LAccepted := LSession.Document;
    LRejected := False;
    try
      LSession.RenameState(LNewName, 'enabled');
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Document = LAccepted) and (LSession.Save = LBefore),
      'duplicate rename cannot overwrite a differently typed key', Result);
    LSession.RenameState(LNewName, LNewName);
    Check(LSession.Document = LAccepted, 'no-op rename preserves accepted handles', Result);
    LSession.Undo;
    Check(LSession.Document.State.Has('🌙/reply'), 'one undo restores the complete rename', Result);
    LSession.Redo;
    Check(LSession.Save = LBefore, 'rename redo reconstructs exact source meaning', Result);
    LSession.Select('reuse-one');
    LSession.SetBinding(TNyxBindingSpec.Clear(bpValue));
    LProjection := LSession.SelectedProjection;
    try
      Check(not LProjection.FindBinding(bpValue, LSpec),
        'instance clear removes inherited value binding', Result);
    finally
      LProjection.Free;
    end;
    Check(LSession.Document.Find('reply-template').FindBinding(bpValue, LSpec) and
      (LSpec.StateName = LNewName), 'instance unbinding preserves its template', Result);
    LSession.InheritBinding(bpValue);
    LProjection := LSession.SelectedProjection;
    try
      Check(LProjection.FindBinding(bpValue, LSpec) and (LSpec.StateName = LNewName),
        'inherit restores the independent template contract', Result);
    finally
      LProjection.Free;
    end;
    LState := DefaultNyxStudioViewState;
    LState.StateVisible := True;
    LState.BindingsVisible := True;
    LState.BindingTarget := bpValue;
    Rebuild;
    Check((LShell.Find('studio-state') <> nil) and (LShell.Find('studio-bindings') <> nil) and
      (LShell.Find('inspector-value').Prop('enabled') = 'false'),
      'Nyx shell exposes state/bindings and effective inherited value is not a fallback editor', Result);
    Check((LShell.Find('binding-state-0') <> nil) and (LShell.Find('binding-state-1') = nil),
      'memo value choices include text and exclude Boolean state', Result);
    LNode := LShell.Find('binding-clear');
    LSession.Select('reply-mirror');
    LBefore := LSession.Save;
    LRejected := False;
    try
      RouteNyxStudioAuthoring(LSession, LNode, ntClick, LShell.Pages[0]);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBefore),
      'stale inspector actions cannot bind a different current selection', Result);
    LSession.Select('reuse-one');
    Check(RouteNyxStudioAuthoring(LSession, LNode, ntClick, LShell.Pages[0]),
      'portable route applies Nyx unbinding action', Result);
    Rebuild;
    Check(RouteNyxStudioAuthoring(LSession, LShell.Find('binding-inherit'), ntClick, LShell.Pages[0]),
      'portable route applies inherited binding recovery', Result);
    Rebuild;
    LShell.Find(NyxStudioBindingFlowID).Configure.Value(NyxStudioBindingDirectionTitle(bdFromState)).Done;
    Check(RouteNyxStudioAuthoring(LSession, LShell.Find(NyxStudioBindingFlowID), ntChange, LShell.Pages[0]),
      'flow editing uses the shared typed route', Result);
    LProjection := LSession.SelectedProjection;
    try
      LProjection.Configure.Value('Rejected canvas draft').Done;
      LBefore := LSession.Save;
      LRejected := False;
      try
        LSession.SetCanvasValue(LProjection);
      except
        on LException: ENyxState do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LSession.Save = LBefore),
        'from-state canvas edits cannot write an invisible authored fallback', Result);
    finally
      LProjection.Free;
    end;
    LSession.SetBinding(TNyxBindingSpec.Bound(bpValue, LNewName, nskText, bdTwoWay));
    LProjection := LSession.SelectedProjection;
    try
      LProjection.Configure.Value('Canvas default / 🌙').Done;
      LSession.SetCanvasValue(LProjection);
      Check(LSession.Document.State.Value(LNewName).TextValue = TNyxText('Canvas default / 🌙'),
        'two-way canvas edits become one authored-default command', Result);
    finally
      LProjection.Free;
    end;
    LSession.Undo;
    LSession.CreateState('opaque', TNyxStateValue.FromText('🌙' + #0 + 'exact'));
    Rebuild;
    LIndex := LSession.Document.State.Count - 1;
    LNode := LShell.Find('state-default-' + IntToStr(LIndex));
    Check((LNode.Prop(NyxStudioStateInputKey) = NyxStudioStateInputName(ssiEscapedText)) and
      (Pos(#0, LNode.Prop('value')) = 0),
      'opaque NUL defaults use a lossless ordinary Nyx text editor', Result);
    LNode.Configure.Value('"Changed 🌙\u0000exact"').Done;
    Check(RouteNyxStudioAuthoring(LSession, LNode, ntChange, LShell.Pages[0]) and
      (LSession.Document.State.Value('opaque').TextValue = TNyxText('Changed 🌙') + #0 + 'exact'),
      'escaped editor route retains exact data without lossy control text', Result);
    LSession.Select('remember-checkbox');
    Rebuild;
    Check((LShell.Find('binding-state-0') = nil) and (LShell.Find('binding-state-1') <> nil),
      'Boolean value choices exclude text and retain Boolean references', Result);
    LState.NewStateName := 'count / 🌙';
    LState.NewStateInput := ssiInteger;
    LState.NewStateValue := '42';
    Rebuild;
    Check(RouteNyxStudioAuthoring(LSession, LShell.Find(NyxStudioAddStateID), ntClick, LShell.Pages[0]) and
      (LSession.Document.State.Value('count / 🌙').IntegerValue = 42),
      'portable Add route creates an exact typed default from Nyx fields', Result);
    LBefore := LSession.Save;
    LAccepted := LSession.Document;
    LRejected := False;
    try
      RouteNyxStudioAuthoring(LSession, LShell.Find(NyxStudioAddStateID), ntClick, LShell.Pages[0]);
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Document = LAccepted) and (LSession.Save = LBefore),
      'duplicate creation preserves the accepted document and history', Result);
    LSession.CreateState('replyText', TNyxStateValue.FromText(''));
    LSession.CreateState('canReplyBoolean', TNyxStateValue.FromBoolean(True));
    LSession.CreateState('totalIntegerState', TNyxStateValue.FromInteger(0));
    LSource := LSession.Source;
    Check((Pos('LReplyTextState: TNyxTextStateRef;', LSource) > 0) and
      (Pos('LCanReplyBooleanState: TNyxBooleanStateRef;', LSource) > 0) and
      (Pos('LTotalIntegerState: TNyxIntegerStateRef;', LSource) > 0) and
      (Pos('TextTextState', LSource) = 0) and (Pos('StateIntegerState', LSource) = 0),
      'type-bearing state names remain purposeful without generated repetition', Result);
  finally
    LShell.Free;
    LFixture.Free;
    LSession.Free;
  end;
end;

end.
