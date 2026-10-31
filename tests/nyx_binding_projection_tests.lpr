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
program nyx_binding_projection_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, {$ifdef PAS2JS}Web,{$endif} nyx.text, nyx.types, nyx.state,
  nyx.model, nyx.controls, nyx.binding.types, nyx.binding.projection, nyx.binding,
  nyx.expressions, nyx.composition, nyx.interaction.policy;

type
  { Borrowed model/coordinator receiver deliberately accepts, refuses, fails or
    replaces a notification. It outlives entered callbacks and owns only the
    successor it creates; the fixture owns Root, Store and Live. }
  TProjectionProbe = class
  public
    Root: TNyxNode;
    Store: TNyxState;
    Live: TNyxLiveBindings;
    Successor: TNyxLiveBindings;
    FullCalls: Integer;
    ChangedCalls: Integer;
    Accept: Boolean;
    Fail: Boolean;
    Replace: Boolean;
    Expected: TNyxText;
    Held: INyxBindingProjectionChanges;
    procedure FullSync;
    function Changed(const AChanges: INyxBindingProjectionChanges): Boolean;
  end;
  { Mutable caller-owned policy with pure UI-thread getters. Its flags change
    without changing node property storage, and it owns no model or target. }
  TProjectionInteractionPolicy = class(TInterfacedObject,
    INyxRuntimeEnabledPolicy, INyxRuntimeReadOnlyPolicy)
  public
    AllowedFlag: Boolean;
    ReadOnlyFlag: Boolean;
    function GetAllowed: Boolean;
    function GetReadOnly: Boolean;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxState.Create('Projection observation: ' + AReason);
  end;
  Inc(GChecks);
end;

procedure TProjectionProbe.FullSync;
begin
  Inc(FullCalls);
end;

function TProjectionInteractionPolicy.GetAllowed: Boolean;
begin
  Result := AllowedFlag;
end;

function TProjectionInteractionPolicy.GetReadOnly: Boolean;
begin
  Result := ReadOnlyFlag;
end;

function TProjectionProbe.Changed(const AChanges: INyxBindingProjectionChanges): Boolean;
begin
  Inc(ChangedCalls);
  Held := AChanges;
  Check((Store.GetValue(NyxTextState('message')) = Expected) and
    (Root.Find('caption').Prop('text') = Expected) and
    (Root.Find('computed').Prop('text') = 'Read: ' + Expected),
    'notification observes admitted state and every current computed output');

  if Fail then
  begin
    raise ENyxState.Create('Intentional post-commit projection failure');
  end;

  if Replace then
  begin
    Live.Retire;
    Successor := TNyxLiveBindings.Create(Root, Store);
    Successor.OnSync := FullSync;
    { Subscription installation waits until this ordinary state notification
      returns idle. Retirement itself revokes the old receiver immediately. }
  end;
  Result := Accept;
end;

procedure ObservationCases;
var
  LRoot: TNyxNode;
  LLabel: TNyxNode;
  LCopy: TNyxNode;
  LBefore: INyxBindingProjectionCapture;
  LChanges: INyxBindingProjectionChanges;
  LProperties: TNyxBindingProjectionProperties;
  LControl: TNyxControlRef;
  LRefused: Boolean;
begin
  LRoot := TNyxNode.Create(nkColumn, 'root');
  LCopy := nil;
  try
    LLabel := TNyxNode.Create(nkLabel, 'caption-🌙');
    LRoot.Add(LLabel);
    LLabel.Configure.Text('Before' + #0 + '🌙').Hint('Old').Done;
    LLabel.Binds.Text(NyxTextState('original')).Done;
    LBefore := CaptureNyxBindingProjection(LRoot);
    LChanges := LBefore.Changes(LRoot);
    Check((LChanges <> nil) and (LChanges.Count = 0), 'unchanged is a complete empty result');
    LLabel.Configure.Text('After' + #0 + '🌙=exact').AccessibleName('Name 🌙').Done;
    LLabel.Props.Delete(LLabel.Props.IndexOfName('hint'));
    LChanges := LBefore.Changes(LRoot);
    Check((LChanges <> nil) and (LChanges.Count = 1) and
      (LChanges.ControlAt(0).ID = TNyxText('caption-🌙')) and
      (LChanges.PropertiesAt(0) = [bpText, bpHint, bpAccessibleName]),
      'changed, added and removed properties form one typed Unicode identity');
    LProperties := LChanges.PropertiesAt(0);
    Exclude(LProperties, bpText);
    Check(bpText in LChanges.PropertiesAt(0), 'returned set cannot mutate retained observation');
    LControl := LChanges.ControlAt(0);
    LControl.ID := 'Caller mutation';
    Check((LControl.ID = 'Caller mutation') and
      (LChanges.ControlAt(0).ID = TNyxText('caption-🌙')), 'returned reference is independently copied');
    LRefused := False;
    try
      LChanges.ControlAt(1);
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'out-of-range result reads refuse');
    LCopy := LRoot.Clone;
    LCopy.Find('caption-🌙').Binds.Text(NyxTextState('different')).Done;
    Check(LBefore.Changes(LCopy) = nil, 'changed binding contract requests full synchronization');
    LCopy.Find('caption-🌙').Binds.Text(NyxTextState('original')).Done;
    LLabel.SetProp('creator-setting', 'extension');
    Check(LBefore.Changes(LRoot) = nil, 'unknown changed extension requests full synchronization');
    Check(LBefore.Changes(LCopy) <> nil, 'unknown later writes cannot contaminate copied observation');
    LCopy.Add(TNyxNode.Create(nkBadge, 'new'));
    Check(LBefore.Changes(LCopy) = nil, 'changed child shape refuses');
    FreeAndNil(LCopy);
    LCopy := TNyxNode.Create(nkColumn, 'root');
    LCopy.Add(TNyxNode.Create(nkLabel, 'different'));
    Check(LBefore.Changes(LCopy) = nil, 'same counts do not establish identity');
    Check(LBefore.Changes(nil) = nil, 'missing current root refuses');
    FreeAndNil(LRoot);
    LBefore := nil;
    Check((LChanges.ControlAt(0).ID = TNyxText('caption-🌙')) and
      (LChanges.PropertiesAt(0) = [bpText, bpHint, bpAccessibleName]),
      'retained result survives complete source/capture destruction');
  finally
    LChanges := nil;
    LBefore := nil;
    LCopy.Free;
    LRoot.Free;
  end;
end;

procedure RuntimePolicyCases;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LEntry: TNyxNode;
  LPolicy: TProjectionInteractionPolicy;
  LEnabled: INyxRuntimeEnabledPolicy;
  LReadOnly: INyxRuntimeReadOnlyPolicy;
  LBefore: INyxBindingProjectionCapture;
  LRestricted: INyxBindingProjectionCapture;
  LChanges: INyxBindingProjectionChanges;
begin
  LDocument := TNyxDocument.Create;
  LRoot := nil;
  try
    LDocument.AddPage(NewNyxPage('home'));
    LDocument.Pages[0].Add(NewNyxInput('entry'));
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LEntry := LRoot.Find('entry');
    LPolicy := TProjectionInteractionPolicy.Create;
    LPolicy.AllowedFlag := True;
    LEnabled := LPolicy;
    LReadOnly := LPolicy;
    LEntry.AttachEnabledPolicy(LEnabled);
    LEntry.AttachReadOnlyPolicy(LReadOnly);
    LBefore := CaptureNyxBindingProjection(LRoot);
    Check(LBefore.Changes(LRoot).Count = 0, 'unchanged effective runtime policies remain a complete observation');
    LPolicy.AllowedFlag := False;
    LPolicy.ReadOnlyFlag := True;
    LChanges := LBefore.Changes(LRoot);
    Check((LChanges.Count = 1) and (LChanges.ControlAt(0).ID = 'entry') and
      (LChanges.PropertiesAt(0) = [bpEnabled, bpReadOnly]),
      'policy-only interaction changes are classified without stored property changes');
    LRestricted := CaptureNyxBindingProjection(LRoot);
    LEntry.DetachEnabledPolicy(LEnabled);
    LEntry.DetachReadOnlyPolicy(LReadOnly);
    LEnabled := nil;
    LReadOnly := nil;
    LPolicy := nil;
    LChanges := LRestricted.Changes(LRoot);
    Check((LChanges.Count = 1) and (LChanges.PropertiesAt(0) = [bpEnabled, bpReadOnly]),
      'policy removal restores flags after every policy owner is released');
  finally
    LChanges := nil;
    LRestricted := nil;
    LBefore := nil;
    LEnabled := nil;
    LReadOnly := nil;
    LRoot.Free;
    LDocument.Free;
  end;
end;

procedure CoordinatorCases;
var
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LProbe: TProjectionProbe;
  LLive: TNyxLiveBindings;
  LLabel: TNyxNode;
  LRefused: Boolean;
  LRevision: Integer;
  LCalls: Integer;
begin
  LDocument := TNyxDocument.Create;
  LRoot := TNyxNode.Create(nkPage, 'root');
  LStore := nil;
  LProbe := TProjectionProbe.Create;
  LLive := nil;
  try
    LDocument.State.SetValue(NyxTextState('message'), 'Before').SetValue(NyxIntegerState('quantity'), 1);
    LStore := LDocument.State.Clone;
    LLabel := TNyxNode.Create(nkLabel, 'caption');
    LRoot.Add(LLabel);
    LLabel.Binds.Text(NyxTextState('message')).Done;
    LLabel := TNyxNode.Create(nkLabel, 'computed');
    LRoot.Add(LLabel);
    LLabel.Binds.Text(NyxTextExpression('Read: ').Append(
      NyxTextExpression(NyxTextState('message')))).Done;
    LLabel := TNyxNode.Create(nkInput, 'quantity');
    LRoot.Add(LLabel);
    LLabel.Configure.InputType(niNumber).Maximum(5).Done;
    LLabel.Binds.Value(NyxIntegerState('quantity')).Done;
    LDocument.AddPage(LRoot);
    LRoot := nil;
    LRoot := RealizeNyxView(LDocument, LDocument.Pages[0]);
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LProbe.Root := LRoot;
    LProbe.Store := LStore;
    LProbe.Live := LLive;
    LLive.OnSync := LProbe.FullSync;
    LLive.OnProjectionSync := LProbe.Changed;
    LLive.Activate;
    LProbe.FullCalls := 0;
    LProbe.Accept := True;
    LProbe.Expected := 'Admitted 🌙';
    LStore.SetValue(NyxTextState('message'), LProbe.Expected);
    Check((LProbe.ChangedCalls = 1) and (LProbe.FullCalls = 0) and
      (LProbe.Held.Count = 2), 'ordinary writes use accepted typed change capability');
    LRevision := LStore.Revision;
    LCalls := LProbe.ChangedCalls;
    LRefused := False;
    try
      LStore.Apply([NyxStateValue(NyxTextState('message'), 'Must not publish'),
        NyxStateValue(NyxIntegerState('quantity'), 6)]);
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LRevision = LStore.Revision) and
      (LProbe.ChangedCalls = LCalls) and
      (LRoot.Find('caption').Prop('text') = LProbe.Expected),
      'complete candidate domain refusal precedes every projection notification');
    LProbe.Accept := False;
    LProbe.Expected := 'Fallback';
    LStore.SetValue(NyxTextState('message'), LProbe.Expected);
    Check(LProbe.FullCalls = 1, 'target refusal invokes legacy complete synchronization');
    LProbe.Fail := True;
    LProbe.Expected := 'Committed despite callback error';
    LRefused := False;
    try
      LStore.SetValue(NyxTextState('message'), LProbe.Expected);
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LStore.GetValue(NyxTextState('message')) = LProbe.Expected) and
      (LRoot.Find('computed').Prop('text') = 'Read: ' + LProbe.Expected) and
      (LProbe.FullCalls = 1), 'notification failure retains commit without fallback/retry');
    LProbe.Fail := False;
    LProbe.Replace := True;
    LProbe.Expected := 'Retiring';
    LStore.SetValue(NyxTextState('message'), LProbe.Expected);
    Check(not LLive.RefreshReady and (LProbe.FullCalls = 1),
      'retired notification cannot fall back into a prepared successor');
    LProbe.Successor.Activate;
    LRefused := False;
    try
      LLive.OnProjectionSync := LProbe.Changed;
    except
      on ENyxState do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'retired coordinator refuses callback revival');
    LCalls := LProbe.ChangedCalls;
    LStore.SetValue(NyxTextState('message'), 'Successor only');
    Check((LCalls = LProbe.ChangedCalls) and (LProbe.FullCalls = 2),
      'later writes reach only the independent successor');
  finally
    LProbe.Successor.Free;
    LLive.Free;
    LProbe.Free;
    LStore.Free;
    LRoot.Free;
    LDocument.Free;
  end;
end;

begin
  try
    ObservationCases;
    RuntimePolicyCases;
    CoordinatorCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-projection-model', 'passed');
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' projection model checks';
    {$else}
    WriteLn('PASS ', GChecks, ' projection model checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-projection-model', 'failed');
      document.body.setAttribute('data-projection-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
