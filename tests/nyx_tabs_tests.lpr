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

program nyx_tabs_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.data, nyx.types, nyx.model, nyx.controls,
  nyx.codec, nyx.codegen, nyx.tabs, nyx.schema, nyx.composition
  {$ifdef NYX_COMPILED_TABS}, nyx.generated.view{$endif}
  {$ifdef PAS2JS}, Web{$endif};

var
  LDocument: TNyxDocument;
  LTabs: INyxTabs;
  LPage: INyxTab;
  LState: TNyxTabState;
  LOptions: TNyxTabsOptions;
  LChange: TNyxTabTransition;
  LBefore: TNyxText;
  LRefused: Boolean;
  LChecks: Integer;
  LIndex: Integer;
  LGenerated: TNyxText;
  LDecoded: TNyxDocument;
  LRuntime: TNyxNode;
  LSibling: TNyxTabState;
  {$ifndef PAS2JS}
  LStream: TFileStream;
  {$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create('Tabs: ' + AReason);
  end;
  Inc(LChecks);
end;

procedure CheckState(AFocus, ASelection: Integer; const AReason: TNyxText);
begin
  Check((LState.FocusIndex = AFocus) and (LState.SelectedIndex = ASelection), AReason);
end;

begin
  LDocument := nil;
  LState := nil;
  LDecoded := nil;
  LRuntime := nil;
  LSibling := nil;
  try
    LDocument := TNyxDocument.Create;
    LTabs := NewNyxTabs('details-tabs');
    LDocument.AddPage(LTabs);
    for LIndex := 0 to 3 do
    begin
      LPage := NewNyxTab(TNyxText('page-') + TNyxText(IntToStr(LIndex)));
      LPage.Configure.Text(TNyxText('Page ') + TNyxText(IntToStr(LIndex))).Done;

      if LIndex = 1 then
      begin
        LPage.Configure.Enabled(False).Done;
      end;

      if LIndex = 2 then
      begin
        LPage.Configure.Visible(False).Done;
      end;
      LTabs.Add(LPage);
    end;
    LPage := nil;
    LBefore := TNyxCodec.Encode(LDocument);
    LState := TNyxTabState.Create(LTabs.Node);
    CheckState(0, 0, 'default selects the first available page');
    Check(LState.Count = 4, 'disabled/hidden pages retain independent entry identities');
    Check((not LState.Entry(1).Available) and (not LState.Entry(2).Available),
      'authored availability is copied');
    Check(LState.Entry(1).Visible and not LState.Entry(1).Enabled and
      not LState.Entry(2).Visible and LState.Entry(2).Enabled,
      'hidden and disabled restrictions remain distinct');
    LChange := LState.Move(ntmNext);
    CheckState(3, 3, 'automatic movement skips disabled and hidden pages');
    Check(LChange.FocusChanged and LChange.SelectionChanged, 'automatic transition explains both changes');
    LState.Move(ntmNext);
    CheckState(0, 0, 'next wraps to the first available page');
    LState.Move(ntmPrevious);
    CheckState(3, 3, 'previous wraps to the last available page');
    LState.Move(ntmFirst);
    CheckState(0, 0, 'first admits an available edge');
    LState.Move(ntmLast);
    CheckState(3, 3, 'last admits an available edge');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'interaction never mutates authored policy or child properties');
    FreeAndNil(LState);
    LOptions := TNyxTabsOptions.Defaults.Activation(ntaManual)
      .Orientation(ntoVertical).Initial(NyxControl('page-3')).Wrap(False);
    Check(TNyxTabsOptions.FromData(LOptions.ToData).ToData.ToJSON = LOptions.ToData.ToJSON,
      'typed choices and initial reference round trip exactly');
    SetNyxTabsOptions(LTabs, LOptions);
    LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
    Check(NyxTabsOptions(LDecoded.Pages[0]).ToData.ToJSON = LOptions.ToData.ToJSON,
      'persistence retains exact typed tab policy');
    FreeAndNil(LDecoded);
    LGenerated := TNyxCodegen.Generate(LDocument);
    Check(Pos('nyx.tabs,', LGenerated) > 0, 'configured source imports the public tab contract');
    Check(Pos('.Activation(ntaManual)', LGenerated) > 0,
      'generated behavior uses the closed activation enum');
    Check(Pos('.Initial(NyxControl(''page-3''))', LGenerated) > 0,
      'generated selection uses a distinct typed control reference');
    Check(Pos('.Wrap(False)', LGenerated) > 0, 'generated wrapping uses a real Boolean');
    {$ifndef PAS2JS}

    if ParamCount = 1 then
    begin
      { Exact generated bytes are compiled separately on both targets. This
        explicit file boundary is qualification output, never a design edit. }
      LStream := TFileStream.Create(ParamStr(1), fmCreate);
      try
        LStream.WriteBuffer(LGenerated[1], Length(LGenerated));
      finally
        LStream.Free;
      end;
    end;
    {$endif}
    {$ifdef NYX_COMPILED_TABS}
    LDecoded := BuildNyxDocument;
    Check(TNyxCodec.Encode(LDecoded) = TNyxCodec.Encode(LDocument),
      'executed emitted Pascal reconstructs the exact configured design');
    Check(Supports(LDecoded.Pages[0].ComponentReference, INyxTabs),
      'executed builder retains the specialized reference-counted tabs interface');
    FreeAndNil(LDecoded);
    {$endif}
    LState := TNyxTabState.Create(LTabs.Node);
    CheckState(3, 3, 'typed initial selection is independent of page position');
    LChange := LState.Move(ntmNext);
    CheckState(3, 3, 'nonwrapping boundary preserves both indices');
    Check(not LChange.FocusChanged and not LChange.SelectionChanged, 'boundary is an unchanged transition');
    LChange := LState.Move(ntmPrevious);
    CheckState(0, 3, 'manual movement changes focus independently of selection');
    Check(LChange.FocusChanged and not LChange.SelectionChanged, 'manual transition reports only focus');
    LState.ActivateFocused;
    CheckState(0, 0, 'explicit activation chooses the focused page');
    LState.Activate(NyxControl('page-3'));
    CheckState(3, 3, 'pointer/programmatic activation uses typed page identity');
    LRefused := False;
    try
      LState.Activate(NyxControl('page-1'));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'disabled activation refuses');
    CheckState(3, 3, 'refusal preserves focus and selection');
    LRefused := False;
    try
      LState.Focus(NyxControl('missing'));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'missing identity refuses');
    CheckState(3, 3, 'missing focus intent remains atomic');
    LBefore := LTabs.Node.Extensions.ToJSON;
    LRefused := False;
    try
      TNyxTabsOptions.FromData(NyxObject([NyxField('version', NyxData(1))]));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LTabs.Node.Extensions.ToJSON = LBefore), 'invalid wire policy cannot replace authored options');
    LTabs.Node.Children[1].SetProp('enabled', 'maybe');
    LRefused := False;
    try
      LDecoded := TNyxCodec.Decode(TNyxCodec.Encode(LDocument));
      ValidateNyxDocumentProperties(LDecoded);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    FreeAndNil(LDecoded);
    Check(LRefused, 'malformed availability refuses candidate admission');
    LTabs.Node.Children[1].Configure.Enabled(False).Done;
    SetNyxTabsOptions(LTabs, LOptions.Initial(NyxControl('page-1')));
    LRefused := False;
    try
      ValidateNyxDocumentProperties(LDocument);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'unavailable initial selection refuses before rendering');
    SetNyxTabsOptions(LTabs, LOptions);
    LTabs.Node.Children[0].Configure.Text(TNyxText('Exact 🌙') + #0 + TNyxText('é')).Done;
    FreeAndNil(LState);
    LState := TNyxTabState.Create(LTabs.Node);
    Check(LState.Entry(0).Caption = TNyxText('Exact 🌙') + #0 + TNyxText('é'),
      'captions preserve supplementary Unicode and NUL');
    LTabs := nil;
    FreeAndNil(LDocument);
    LState.Activate(NyxControl('page-0'));
    CheckState(0, 0, 'source retirement leaves an independently owned interaction state');
    FreeAndNil(LState);
    LTabs := NewNyxTabs('empty-tabs');
    LState := TNyxTabState.Create(LTabs.Node);
    CheckState(-1, -1, 'empty tabs have explicit absent indices');
    LState.Move(ntmNext);
    LState.ActivateFocused;
    CheckState(-1, -1, 'empty navigation is a harmless unchanged transition');
    FreeAndNil(LState);
    LTabs := nil;
    LDocument := TNyxDocument.Create;
    LTabs := NewNyxTabs('reusable-tabs');
    LTabs.Add(NewNyxTab('first-page').Configure.Text('First').Done);
    LTabs.Add(NewNyxTab('second-page').Configure.Text('Second').Done);
    SetNyxTabsOptions(LTabs, TNyxTabsOptions.Defaults.Initial(NyxControl('second-page')));
    LDocument.AddComponent(LTabs);
    LDocument.AddPage(NewNyxPage('reuse-page'));
    LDocument.Pages[0].Add(NewNyxComponent('first-instance')
      .Configure.Component(NyxComponent('reusable-tabs')).Done);
    LDocument.Pages[0].Add(NewNyxComponent('second-instance')
      .Configure.Component(NyxComponent('reusable-tabs')).Done);
    LBefore := TNyxCodec.Encode(LDocument);
    LRuntime := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LState := TNyxTabState.Create(LRuntime.Children[0]);
    LSibling := TNyxTabState.Create(LRuntime.Children[1]);
    CheckState(1, 1, 'reusable initial intent resolves its local authored child');
    Check(LState.Entry(1).Reference.ID <> LSibling.Entry(1).Reference.ID,
      'independent instances expose distinct qualified child identities');
    LState.Activate(LState.Entry(0).Reference);
    Check(LSibling.SelectedIndex = 1, 'first instance selection cannot change its sibling');
    Check(TNyxCodec.Encode(LDocument) = LBefore, 'realization and selection retain exact reusable source');
    FreeAndNil(LRuntime);
    LState.Activate(LState.Entry(1).Reference);
    CheckState(1, 1, 'retiring the realized tree leaves an owned instance snapshot');
    FreeAndNil(LState);
    FreeAndNil(LSibling);
    LTabs := nil;
    FreeAndNil(LDocument);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-tabs', 'passed');
    document.body.setAttribute('data-nyx-tabs-checks', IntToStr(LChecks));
    {$endif}
    WriteLn('PASS ', LChecks, ' shared typed tab policy/interaction checks');
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-nyx-tabs', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      {$endif}
      ExitCode := 1;
    end;
  end;
  LState.Free;
  LPage := nil;
  LTabs := nil;
  LDocument.Free;
  LDecoded.Free;
  LRuntime.Free;
  LSibling.Free;
end.
