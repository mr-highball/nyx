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
unit nyx.test.split;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses nyx.model;

function CreateNyxSplitFixture: TNyxDocument;
function RunNyxSplitTests: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.controls, nyx.schema, nyx.codec,
  nyx.codegen, nyx.source, nyx.composition, nyx.platform, nyx.split,
  nyx.behavior, nyx.data, nyx.state, nyx.studio.session;

function CreateNyxSplitFixture: TNyxDocument;
var
  LPage: INyxPage;
  LSplit: INyxSplitView;
  LFirst: INyxColumn;
  LSecond: INyxMemo;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Split workspace / 🌙';
    LPage := NewNyxPage('workspace');
    Result.AddPage(LPage);
    LSplit := NewNyxSplitView('work-split');
    LPage.Add(LSplit);
    LSplit.Configure.SplitOrientation(nsoStacked).SplitPosition(65)
      .SplitMinimum(10).SplitMaximum(90).Height(480).Done;
    LSplit.Configure.ForPlatform(npfNativeLCL).SplitOrientation(nsoSideBySide)
      .SplitResizable(False).Done;
    LSplit.Configure.ForPlatform(npfBrowser).SplitResizable(True).Done;
    LFirst := NewNyxColumn('first-pane');
    LSplit.Add(LFirst);
    LFirst.Configure.Padding(12).Done;
    LFirst.Add(NewNyxHeading('preview-heading').WithText('A place to create'));
    LSecond := NewNyxMemo('second-pane');
    LSplit.Add(LSecond);
    LSecond.Text := 'Draft';
    LSecond.Value := 'Keep this draft / 🌙漢字';
    ValidateNyxDocumentProperties(Result);
  except
    Result.Free;
    raise;
  end;
end;

function RunNyxSplitTests: Integer;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LCandidate: TNyxDocument;
  LRuntime: TNyxNode;
  LSplit: INyxSplitView;
  LBase: INyxConfiguration;
  LScoped: INyxConfiguration;
  LWorkspace: TNyxSourceWorkspace;
  LState: TNyxSplitState;
  LGeometry: TNyxSplitGeometry;
  LSource: TNyxText;
  LWire: TNyxText;
  LSnapshot: TNyxEventInfo;
  LRejected: Boolean;
  LIndex: Integer;
  LSession: TNyxStudioSession;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Split/platform: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LDocument := CreateNyxSplitFixture;
  LWorkspace := TNyxSourceWorkspace.Create;
  try
    LSplit := RetainNyxControl(LDocument.Find('work-split')) as INyxSplitView;
    Check(LSplit.Orientation = nsoStacked, 'specialized orientation');
    Check((LSplit.Position = 65) and LSplit.Resizable, 'typed defaults');
    LBase := LSplit.Configure;
    LScoped := LBase.ForPlatform(npfBrowser);
    LScoped.Gap(7);
    LBase.Gap(3);
    Check((LSplit.Node.Prop('gap') = '3') and
      (LSplit.Node.Prop(NyxPlatformKey(npfBrowser, atGap)) = '7'),
      'retained scoped and default facades remain independent');
    LWire := TNyxCodec.Encode(LDocument);
    LCopy := TNyxCodec.Decode(LWire);
    try
      Check(TNyxCodec.Encode(LCopy) = LWire, 'wire retains both platform rules');
    finally
      LCopy.Free;
    end;
    LCopy := LDocument.Clone;
    try
      LCopy.Find('work-split').Configure.ForPlatform(npfBrowser).Gap(9);
      Check(LSplit.Node.Prop(NyxPlatformKey(npfBrowser, atGap)) = '7',
        'clone owns independent scoped values');
    finally
      LCopy.Free;
    end;
    LSource := LWorkspace.Render(LDocument);
    Check(Pos('INyxSplitView', LSource) > 0, 'generated specialized interface');
    Check(Pos('NewNyxSplitView', LSource) > 0, 'generated specialized factory');
    Check(Pos('.ForPlatform(npfNativeLCL)', LSource) > 0, 'crafted native directive');
    Check(Pos('.SplitOrientation(nsoSideBySide)', LSource) > 0, 'crafted typed orientation');
    Check(Pos('@nyx.native-lcl:', LSource) = 0, 'wire strings stay out of generated authoring');
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    try
      Check(TNyxCodec.Encode(LCandidate) = LWire, 'source reconstructs every rule');
    finally
      LCandidate.Free;
    end;
    LSource := StringReplace(LSource, '.SplitPosition(65)', '.SplitPosition(60)', []);
    LCandidate := LWorkspace.Candidate(LDocument, LSource);
    try
      Check(LCandidate.Find('work-split').Prop('split-position') = '60',
        'hand-edited typed position admits');
      Check(LCandidate.Find('work-split').Prop(NyxPlatformKey(npfNativeLCL,
        atSplitResizable)) = 'false', 'hand edit retains other platform');
    finally
      LCandidate.Free;
    end;
    LRuntime := RealizeNyxView(LDocument, LDocument.Pages[0]);
    try
      ApplyNyxPlatform(LRuntime, npfBrowser);
      Check(LRuntime.Find('work-split').Prop('split-resizable') = 'true',
        'browser projection chooses browser behavior');
      Check(LRuntime.Find('work-split').Prop('split-orientation', 'stacked') = 'stacked',
        'browser retains portable orientation');
      Check(LRuntime.Find('work-split').Prop('gap') = '7', 'browser presentation override');
      Check(LRuntime.Find('work-split').Props.IndexOfName(
        NyxPlatformKey(npfNativeLCL, atSplitResizable)) < 0,
        'other target rules leave realized view');
    finally
      LRuntime.Free;
    end;
    LRuntime := RealizeNyxView(LDocument, LDocument.Pages[0]);
    try
      ApplyNyxPlatform(LRuntime, npfNativeLCL);
      Check(LRuntime.Find('work-split').Prop('split-resizable') = 'false',
        'native projection chooses native behavior');
      Check(LRuntime.Find('work-split').Prop('split-orientation') = 'side-by-side',
        'native orientation override');
      Check(LRuntime.Find('work-split').Prop('gap') = '3', 'native uses ordinary gap');
    finally
      LRuntime.Free;
    end;
    Check(TNyxCodec.Encode(LDocument) = LWire, 'projections never change authored design');
    LSession := TNyxStudioSession.Create;
    try
      LSession.Load(LWire);
      LSession.Select('work-split');
      LSession.SetProperty(NyxPlatformKey(npfBrowser, atSplitResizable), 'false');
      Check(LSession.Selected.Prop(NyxPlatformKey(npfBrowser, atSplitResizable)) = 'false',
        'ordinary visual command publishes a scoped property');
      Check(Pos('.ForPlatform(npfBrowser)', LSession.Source) > 0,
        'visual reconciliation retains crafted directives');
      LSession.Undo;
      Check(LSession.Selected.Prop(NyxPlatformKey(npfBrowser, atSplitResizable)) = 'true',
        'undo restores scoped rule');
      LSession.Redo;
      Check(LSession.Selected.Prop(NyxPlatformKey(npfBrowser, atSplitResizable)) = 'false',
        'redo restores scoped rule and source together');
    finally
      LSession.Free;
    end;
    LRejected := False;
    try
      LScoped.SplitPosition(101);
    except
      on ENyxModel do LRejected := True;
    end;
    Check(LRejected and (LSplit.Node.Props.IndexOfName(
      NyxPlatformKey(npfBrowser, atSplitPosition)) < 0), 'typed range rejects before mutation');
    LRejected := False;
    try
      LScoped.Value('target-specific scalar');
    except
      on ENyxModel do LRejected := True;
    end;
    Check(LRejected and (TNyxCodec.Encode(LDocument) = LWire), 'state meaning cannot diverge');
    LCopy := LDocument.Clone;
    try
      LCopy.Find('work-split').SetProp('@nyx.browser:split-resizable', 'perhaps');
      LRejected := False;
      try
        ValidateNyxDocumentProperties(LCopy);
      except
        on ENyxModel do LRejected := True;
      end;
      Check(LRejected, 'wire Boolean strongly admitted');
      LCopy.Find('work-split').SetProp('@nyx.browser:split-resizable', 'true');
      LCopy.Find('work-split').SetProp('@nyx.browser:component', 'hidden-root');
      LRejected := False;
      try
        ValidateNyxDocumentProperties(LCopy);
      except
        on ENyxModel do LRejected := True;
      end;
      Check(LRejected, 'reserved unsupported directive refused');
    finally
      LCopy.Free;
    end;
    LState := TNyxSplitState.Create(LSplit.Node);
    try
      for LIndex := 0 to 101 do
      begin
        LGeometry := LState.Geometry(LIndex, 44);
        Check((LGeometry.FirstExtent >= 0) and (LGeometry.SecondExtent >= 0) and
          (LGeometry.FirstExtent + LGeometry.DividerExtent +
          LGeometry.SecondExtent = LIndex), 'geometry consumes tiny and normal extents exactly');
      end;
      LState.BeginDrag(100, 200);
      Check(not LState.Drag(100) and (LState.Position = 65), 'touch starts without jump');
      Check(LState.Drag(60) and (LState.Position = 45), 'physical delta uses available extent');
      LState.EndDrag(True);
      Check(LState.Position = 65, 'cancel restores initial proportion');
      LState.BeginDrag(100, 200);
      LState.Drag(-10000);
      Check(LState.Position = 10, 'drag clamps minimum');
      LState.EndDrag(False);
      Check(LState.Key(nkEndKey) and (LState.Position = 90), 'keyboard maximum');
      Check(LState.Key(nkUpKey, True) and (LState.Position = 80), 'shift keyboard adjustment');
      Check(not LState.Key(nkLeftKey), 'wrong orientation key stays available');
      LGeometry := LState.Geometry(644, 44);
      Check((LGeometry.FirstExtent = 480) and (LGeometry.SecondExtent = 120),
        'resize retains proportion');
      LSnapshot := NyxSplitChange(LSplit.Node, 80).Info.Copy;
      Check((LSnapshot.ValueKind = nskInteger) and
        (LSnapshot.Value.AsInteger = 80) and LSnapshot.HasValue, 'owned integer resize event');
    finally
      LState.Free;
    end;
    LScoped := nil;
    LBase := nil;
    LSplit := nil;
  finally
    LWorkspace.Free;
    LDocument.Free;
  end;
  Check((LSnapshot.Value.AsInteger = 80) and (LSnapshot.OriginID = 'work-split'),
    'event snapshot survives descriptor disposal');
end;

end.
