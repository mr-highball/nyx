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
unit nyx.test.interactions;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses nyx.model;

function CreateNyxInteractionFixture: TNyxDocument;
function RunNyxInteractionTests: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.controls, nyx.callbacks, nyx.schema, nyx.contract,
  nyx.codec, nyx.source, nyx.studio.session, nyx.state, nyx.binding,
  nyx.behavior, nyx.composition, nyx.interaction, nyx.catalog,
  nyx.collections, nyx.collections.view.types;

function CreateNyxInteractionFixture: TNyxDocument;
var
  LPage: INyxPage;
  LMemo: INyxMemo;
  LTasks: INyxList;
  LTrigger: TNyxTrigger;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxPage('interactions');
    Result.AddPage(LPage);
    LPage.Configure.Layout(nlColumn).Gap(12).Padding(12).Done;
    LMemo := NewNyxMemo('reply');
    LPage.Add(LMemo);
    LMemo.Configure.Text('Reply').Value(TNyxText('Original / 🌙')).Height(140).Done;
    { Each phase owns its declaration. Deliberately mix notification-only hooks
      and scalar payloads so a bridge cannot simply retarget the main snapshot.
      Input-specific keyboard/text data remains available on signal-only hooks. }
    LMemo.Node.Contract.Signal(ntBeforeKeyDown).Signal(ntAfterKeyDown)
      .Signal(ntBeforeKeyPress).Signal(ntAfterKeyPress)
      .Signal(ntBeforeKeyUp).Signal(ntAfterKeyUp)
      .Signal(ntBeforeTextInput).Signal(ntAfterTextInput)
      .On(ntKeyPress, NyxTargetValue, NyxTextDomain)
      .On(ntTextInput, NyxOriginValue, NyxTextDomain);
    LPage.Add(NewNyxCode('source-block').WithText('begin' + #10 + '  Draw;'));
    LPage.Add(NewNyxLink('reference').WithText('Reference'));
    { A collection event belongs to an actual bound collection control. Keep the
      memo's exact supported family assertion instead of declaring a producer
      that a text editor cannot supply. Canonical/source coverage still exercises
      every runtime trigger through the appropriate specialized control. }
    Result.Collections.Define(NyxCollection('tasks'),
      NyxCollectionSchema.Text(NyxTextField('caption'), ''), [
      NyxCollectionItem(NyxItem(NyxCollection('tasks'), 'review'))
        .WithValue(NyxTextField('caption'), 'Review the interaction')]);
    LTasks := NewNyxList('tasks');
    LTasks.Binds.Collection(NyxCollectionView(NyxCollection('tasks'))
      .Column(NyxTextField('caption'), 'Task')).Done;
    LPage.Add(LTasks);
    for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
    begin

      if NyxIsRuntimeTrigger(LTrigger) then
      begin

        if LTrigger = ntSelectionChange then
        begin
          NyxCallbacks(LTasks).OnSelectionChange.Add(NyxHandler('TInteractionProbe'),
            NyxCallbackID('tasks.selection'));
        end
        else
        begin
          NyxCallbacks(LMemo).On(LTrigger).Add(NyxHandler('TInteractionProbe'),
            NyxCallbackID('reply.' + NyxTriggerName(LTrigger)));
        end;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function RunCapabilitySchemaTests: Integer;
var
  LCatalog: TNyxCatalog;
  LNode: TNyxNode;
  LInfos: TNyxPropertyInfos;
  LAgain: TNyxPropertyInfos;
  LPublished: TNyxPropertyInfo;
  LSupport: TNyxPropertySupport;
  LIndex: Integer;
  LProperty: Integer;
  LFound: Boolean;
  LComplete: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Capability schema: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LCatalog := TNyxCatalog.Create;
  try
    for LIndex := 0 to LCatalog.Count - 1 do
    begin
      LNode := LCatalog.NewNode(LCatalog[LIndex].Kind, 'capability');
      try
        LInfos := NyxProperties(LNode);
        LComplete := Length(LInfos) > 0;
        for LProperty := 0 to High(LInfos) do
        begin
          LComplete := LComplete and LInfos[LProperty].Support.Defined and
            (LInfos[LProperty].Support.Description <> '');
        end;
        Check(LComplete, 'owned property support for ' + LNode.Kind);
      finally
        LNode.Free;
      end;
    end;
    LNode := LCatalog.NewNode(nkLink, 'link');
    try
      LSupport := NyxPropertySupport(LNode, atHref);
      Check((LSupport.Browser = ncAvailable) and (LSupport.Native = ncMissing),
        'native command face does not advertise unimplemented URL navigation');
      Check(NyxPropertySupport(LNode, atSplitPosition).Native = ncMissing,
        'split sizing is not advertised on an ordinary link');
    finally
      LNode.Free;
    end;
    LNode := LCatalog.NewNode(nkMemo, 'memo');
    try
      LSupport := NyxPropertySupport(LNode, atReadOnly);
      Check((LSupport.Meaning = npmInteraction) and
        (LSupport.Browser = ncAvailable) and (LSupport.Native = ncAvailable),
        'memo policy has explicit shared support');
      LSupport := NyxPropertySupport(LNode, atPlaceholder);
      Check((LSupport.Browser = ncAvailable) and (LSupport.Native = ncBasic),
        'multiline native placeholder is qualified instead of assumed');
      LNode.Configure.ForPlatform(npfBrowser).Placeholder('Browser hint').Done;
      LInfos := NyxProperties(LNode);
      LFound := False;
      for LProperty := 0 to High(LInfos) do
      begin

        if LInfos[LProperty].Key = NyxPlatformKey(npfBrowser, atPlaceholder) then
        begin
          LFound := (LInfos[LProperty].Support.Browser = ncAvailable) and
            (LInfos[LProperty].Support.Native = ncMissing) and
            (Pos('Browser-only', LInfos[LProperty].Support.Description) = 1);
        end;
      end;
      Check(LFound, 'platform support reflects the actual selected scope');
    finally
      LNode.Free;
    end;
    LPublished := Default(TNyxPropertyInfo);
    LPublished.Key := 'creator-tone';
    LPublished.Title := 'Tone';
    LPublished.ValueType := npText;
    LPublished.Support := NyxPropertySupport(npmPresentation, ncAvailable, ncCustom,
      TNyxText('Creator help / 🌙'));
    RegisterNyxSchema(NyxCustomKind('capability-schema-fixture'), [LPublished], []);
    LPublished.Support := NyxPropertySupport(npmCustom, ncMissing, ncMissing, 'Changed caller');
    LNode := TNyxNode.Create('capability-schema-fixture', 'custom');
    try
      LNode.Configure.ProjectAs(nkMemo).Done;
      LInfos := NyxProperties(LNode);
      LFound := False;
      for LProperty := 0 to High(LInfos) do
      begin

        if LInfos[LProperty].Key = 'creator-tone' then
        begin
          LFound := (LInfos[LProperty].Support.Browser = ncAvailable) and
            (LInfos[LProperty].Support.Native = ncCustom) and
            (LInfos[LProperty].Support.Description = TNyxText('Creator help / 🌙'));
          LInfos[LProperty].Support := LPublished.Support;
        end;
      end;
      Check(LFound, 'creator support retains an independent Unicode declaration');
      LAgain := NyxProperties(LNode);
      LFound := False;
      for LProperty := 0 to High(LAgain) do
      begin

        if LAgain[LProperty].Key = 'creator-tone' then
        begin
          LFound := LAgain[LProperty].Support.Browser = ncAvailable;
        end;
      end;
      Check(LFound, 'returned support snapshots cannot mutate the registry');
    finally
      LNode.Free;
    end;
  finally
    LCatalog.Free;
  end;
end;

function RunInteractionPolicyTests: Integer;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LScope: INyxColumn;
  LMemo: INyxMemo;
  LUnbound: INyxMemo;
  LClear: INyxButton;
  LRoot: TNyxNode;
  LStore: TNyxState;
  LLive: TNyxLiveBindings;
  LSnapshot: TNyxInteractionPolicy;
  LRejected: Boolean;
  LRevision: Integer;
  LWire: TNyxText;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Interaction policy: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LDocument := TNyxDocument.Create;
  LRoot := nil;
  LStore := nil;
  LLive := nil;
  try
    LDocument.State.SetValue(NyxTextState('reply'), 'Original / 🌙')
      .SetValue(NyxBooleanState('protected'), True);
    LPage := NewNyxPage('policy');
    LDocument.AddPage(LPage);
    LScope := NewNyxColumn('settings');
    LPage.Add(LScope);
    LScope.Configure.Compound(True).Done;
    LScope.Binds.ReadOnly(NyxBooleanState('protected')).Done;
    LMemo := NewNyxMemo('bound-reply');
    LScope.Add(LMemo);
    LMemo.Configure.PartName(NyxPart('reply')).ReadOnly(False).Done;
    LMemo.Binds.Value(NyxTextState('reply')).Done;
    LUnbound := NewNyxMemo('unbound-reply');
    LScope.Add(LUnbound);
    LUnbound.Configure.PartName(NyxPart('unbound')).Value(TNyxText('Keep / 🌙')).Done;
    LClear := NewNyxButton('clear');
    LScope.Add(LClear);
    LClear.Configure.Action(naClear).Target(NyxPart('unbound')).Done;
    LWire := TNyxCodec.Encode(LDocument);
    LRoot := RealizeNyxView(LDocument, LPage.Node);
    LStore := LDocument.State.Clone;
    LLive := TNyxLiveBindings.Create(LRoot, LStore);
    LLive.Activate;
    LSnapshot := NyxInteractionPolicy(LRoot.Find('bound-reply'));
    Check(LSnapshot.Enabled and LSnapshot.Visible and LSnapshot.ReadOnly and
      LSnapshot.CanIssueCommand and not LSnapshot.CanEditValue,
      'typed inherited policy keeps observational commands and refuses value edits');
    LRevision := LStore.Revision;
    LRejected := False;
    try
      LLive.Edit(LRoot.Find('bound-reply'), 'Forbidden');
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.Revision = LRevision) and
      (LRoot.Find('bound-reply').Prop('value') = TNyxText('Original / 🌙')),
      'read-only ancestor refuses bound edits atomically despite local false');
    LRejected := False;
    try
      LLive.ProposeText(LRoot.Find('bound-reply'), 'Forbidden proposal');
    except
      on LException: ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.Revision = LRevision),
      'protected text has no admissible proposal callback');
    LRejected := False;
    try
      LLive.Dispatch(LRoot.Find('clear'), ntClick);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LStore.Revision = LRevision) and
      (LRoot.Find('unbound-reply').Prop('value') = TNyxText('Keep / 🌙')),
      'compound clear cannot mutate an unbound protected target');
    LRejected := False;
    try
      DispatchNyxBehavior(LRoot.Find('clear'), ntClick);
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LRoot.Find('unbound-reply').Prop('value') = TNyxText('Keep / 🌙')),
      'standalone behavior obeys the same inherited target policy');
    LStore.SetValue(NyxTextState('reply'), 'External / 🌙');
    Check((LRoot.Find('bound-reply').Prop('value') = TNyxText('External / 🌙')) and
      NyxInteractionPolicy(LRoot.Find('bound-reply')).ReadOnly,
      'programmatic state updates still project into a read-only view');
    LStore.SetValue(NyxBooleanState('protected'), False);
    Check(LSnapshot.ReadOnly and NyxInteractionPolicy(LRoot.Find('bound-reply')).CanEditValue,
      'retained policy snapshots are independent of live state changes');
    LLive.Edit(LRoot.Find('bound-reply'), 'Accepted / 🌙');
    LLive.Dispatch(LRoot.Find('clear'), ntClick);
    Check((LStore.GetValue(NyxTextState('reply')) = TNyxText('Accepted / 🌙')) and
      (LRoot.Find('unbound-reply').Prop('value') = ''),
      'removing protection reactivates bound and unbound commands');
    LRoot.Find('settings').Configure.Enabled(False).Done;
    Check(not NyxInteractionPolicy(LRoot.Find('bound-reply')).CanIssueCommand,
      'disabled scope cannot be re-enabled by its descendant');
    LRoot.Find('settings').Configure.Enabled(True).Visible(False).Done;
    Check(not NyxInteractionPolicy(LRoot.Find('bound-reply')).CanEditValue,
      'hidden scope refuses value editing');
    Check(TNyxCodec.Encode(LDocument) = LWire, 'runtime policy never changes authored defaults');
  finally
    LLive.Free;
    LStore.Free;
    LRoot.Free;
    LDocument.Free;
  end;
end;

function RunNyxInteractionTests: Integer;
var
  LDocument: TNyxDocument;
  LCopy: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSession: TNyxStudioSession;
  LMetadata: TNyxEventSchemas;
  LTrigger: TNyxTrigger;
  LDecoded: TNyxTrigger;
  LIndex: Integer;
  LFound: Boolean;
  LSource: TNyxText;
  LWire: TNyxText;
  LBefore: TNyxText;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise ENyxModel.Create('Interaction contracts: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := RunInteractionPolicyTests + RunCapabilitySchemaTests;
  LDocument := CreateNyxInteractionFixture;
  LWorkspace := TNyxSourceWorkspace.Create;
  LSession := TNyxStudioSession.Create;
  try
    LMetadata := NyxEventsMetadata(LDocument.Find('reply'));
    Check(Length(LMetadata) = 43, 'memo exposes editing, capture and drag runtime families');
    for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
    begin

      if not NyxIsRuntimeTrigger(LTrigger) then
      begin
        Continue;
      end;
      Check(TryNyxTrigger(NyxTriggerName(LTrigger), LDecoded) and
        (LDecoded = LTrigger), 'canonical wire identity: ' + NyxTriggerTitle(LTrigger));
      LMetadata := NyxEventsMetadata(LDocument.Find('reply'));

      if LTrigger = ntSelectionChange then
      begin
        LMetadata := NyxEventsMetadata(LDocument.Find('tasks'));
      end;
      LFound := False;
      for LIndex := 0 to High(LMetadata) do
      begin

        if LMetadata[LIndex].Trigger = LTrigger then
        begin
          LFound := (LMetadata[LIndex].Title = NyxTriggerTitle(LTrigger)) and
            (LMetadata[LIndex].Description <> '');

          if LTrigger = ntScroll then
          begin
            LFound := LFound and (LMetadata[LIndex].Browser = ncAvailable) and
              (LMetadata[LIndex].Native = ncBasic);
          end
          else if LTrigger = ntScrollEnd then
          begin
            LFound := LFound and (LMetadata[LIndex].Browser = ncBasic) and
              (LMetadata[LIndex].Native = ncMissing);
          end
          else if LTrigger = ntBeforeEdit then
          begin
            LFound := LFound and (LMetadata[LIndex].Browser = ncAvailable) and
              (LMetadata[LIndex].Native = ncMissing);
          end
          else if LTrigger in [ntCompositionStart, ntCompositionUpdate,
            ntCompositionEnd, ntTextSelectionChange] then
          begin
            LFound := LFound and (LMetadata[LIndex].Browser in [ncAvailable, ncBasic]) and
              (LMetadata[LIndex].Native = ncBasic);
          end
          else if LTrigger in [ntPointerCancel, ntPointerCapture, ntPointerCaptureLost] then
          begin
            LFound := LFound and (LMetadata[LIndex].Browser = ncAvailable) and
              (LMetadata[LIndex].Native = ncBasic) and
              (nctxPointer in LMetadata[LIndex].Contexts);
          end
          else if LTrigger in [ntDragStart, ntDrag, ntDragEnter, ntDragOver,
            ntDragExit, ntDrop, ntDragEnd] then
          begin
            LFound := LFound and (LMetadata[LIndex].Browser = ncBasic) and
              (nctxDrag in LMetadata[LIndex].Contexts) and
              (nctxPointer in LMetadata[LIndex].Contexts);

            if LTrigger = ntDrag then
            begin
              LFound := LFound and (LMetadata[LIndex].Native = ncMissing);
            end
            else
            begin
              LFound := LFound and (LMetadata[LIndex].Native = ncBasic);
            end;
          end
          else
          begin
            LFound := LFound and (LMetadata[LIndex].Browser = ncAvailable) and
              (LMetadata[LIndex].Native = ncAvailable);
          end;
        end;
      end;
      Check(LFound, 'meaningful both-target schema: ' + NyxTriggerTitle(LTrigger));
    end;
    LWire := TNyxCodec.Encode(LDocument);
    LCopy := TNyxCodec.Decode(LWire);
    try
      Check(TNyxCodec.Encode(LCopy) = LWire, 'wire retains every registration');
    finally
      LCopy.Free;
    end;
    LSource := LWorkspace.Render(LDocument);
    for LTrigger := Low(TNyxTrigger) to High(TNyxTrigger) do
    begin

      if NyxIsRuntimeTrigger(LTrigger) then
      begin
        Check(Pos('.' + NyxTriggerTitle(LTrigger), LSource) > 0,
          'crafted fluent event source: ' + NyxTriggerTitle(LTrigger));
      end;
    end;
    LCopy := LWorkspace.Candidate(LDocument, LSource);
    try
      Check(TNyxCodec.Encode(LCopy) = LWire, 'all fluent event families reconstruct');
    finally
      LCopy.Free;
    end;
    LMetadata := NyxEventsMetadata(LDocument.Find('source-block'));
    Check(Length(LMetadata) = 34,
      'read-only code has focus/key/pointer/viewport/drag observations, no edit hook');
    LSession.Load(LWire);
    LSession.Select('source-block');
    LBefore := LSession.Save;
    LSession.AddCallback(ntBeforeKeyPress, LIndex);
    Check(Pos('.OnBeforeKeyPress', LSession.Source) > 0,
      'Studio creates a strongly typed key-press registration');
    Check(Pos('TODO', LSession.Source) > 0, 'Studio preserves the editable handler template');
    LSession.Undo;
    Check(LSession.Save = LBefore, 'undo restores the complete paired source/design');
    LSession.Redo;
    Check(Pos('.OnBeforeKeyPress', LSession.Source) > 0, 'redo restores callback source');
  finally
    LSession.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

end.
