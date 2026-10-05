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

unit nyx.test.studio;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.types,
  nyx.text,
  SysUtils,
  nyx.model,
  nyx.codec,
  nyx.catalog,
  nyx.composition,
  nyx.behavior,
  nyx.studio.session,
  nyx.studio.outputs,
  nyx.studio.view;

function RunNyxStudioTests: Integer;

implementation

uses
  nyx.test.schema,
  nyx.test.schema.admission,
  nyx.test.fluent,
  nyx.test.state,
  nyx.test.collections,
  nyx.test.collections.registry,
  nyx.test.collections.view,
  nyx.test.json,
  nyx.test.binding,
  nyx.test.authoring,
  nyx.test.data,
  nyx.test.events,
  nyx.test.contract,
  nyx.test.overrides,
  nyx.test.identity,
  nyx.test.theme,
  nyx.test.source,
  nyx.test.source.state,
  nyx.test.source.contract,
  nyx.test.source.managed,
  nyx.test.source.indexed,
  nyx.test.source.context,
  nyx.test.source.history,
  nyx.test.compiler,
  nyx.test.source.structural,
  nyx.test.source.diagnostics,
  nyx.test.callbacks,
  nyx.test.controls,
  nyx.test.palette;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise Exception.Create('FAIL: ' + AMessage);
  end;
  Inc(ACount);
end;

function RunNyxStudioTests: Integer;
var
  LCatalog: TNyxCatalog;
  LDocument: TNyxDocument;
  LRoot: TNyxNode;
  LOther: TNyxNode;
  LDerived: TNyxNode;
  LRealized: TNyxNode;
  LDispatch: TNyxDispatch;
  LSession: TNyxStudioSession;
  LBaseline: TNyxText;
  LSelected: TNyxText;
  LRejected: Boolean;
  LIndex: Integer;
  LOutputs: TNyxOutputConfiguration;
  LLoadedOutputs: TNyxOutputConfiguration;
  LState: TNyxStudioViewState;
  LShell: TNyxDocument;
  LPanel: TNyxStudioPanel;
begin
  Result := RunNyxSchemaTests + RunNyxSchemaAdmissionTests + RunNyxOverrideTests + RunNyxThemeTests +
    RunNyxIdentityTests + RunNyxFluentTests + RunNyxStateTests + RunNyxJSONTests +
    RunNyxBindingTests + RunNyxAuthoringTests + RunNyxDataTests + RunNyxEventTests +
    RunNyxContractTests + RunNyxCallbackAuthoringTests + RunNyxSourceTests + RunNyxSourceStateTests +
    RunNyxSourceContractTests + RunNyxControlTests + RunNyxPaletteTests + RunNyxManagedSourceTests +
    RunNyxStructuralSourceTests + RunNyxCollectionTests + RunNyxCollectionRegistryTests +
    RunNyxCollectionViewTests + RunNyxSourceDiagnosticTests + RunNyxIndexedSourceTests +
    RunNyxSourceContextTests + RunNyxSourceHistoryTests +
    RunNyxSourceAdmissionTests + RunNyxCompilerDiagnosticTests;
  LCatalog := TNyxCatalog.Create;
  LDocument := TNyxDocument.Create;
  try
    Check(LCatalog.Count >= 70, 'primitive and compound breadth', Result);
    LRoot := LCatalog.NewNode('labeled-button', 'labeled');
    LDocument.AddPage(LRoot);
    LOther := LCatalog.NewNode('labeled-button', 'other');
    LDocument.AddPage(LOther);
    LRoot.Part('label').SetProp('text', 'Customized label');
    Check(LOther.Part('label').Prop('text') = 'A useful action',
      'recipe instance isolation', Result);
    LRoot.Part('button').SetProp('text', 'Take flight');
    Check(LRoot.Part('button').Parent = LRoot, 'named parts retain ownership', Result);
    LDerived := LCatalog.NewNode('list-card', 'derived');
    try
      LDerived.Part('actions').Add(TNyxNode.Create('button', 'extra')
        .SetProp('part', 'floating').SetProp('text', '+'));
      LCatalog.RegisterRecipe('custom-list-card', 'My list', 'Custom', LDerived);
    finally
      LDerived.Free;
    end;
    LDerived := LCatalog.NewNode('custom-list-card', 'custom');
    LDocument.AddPage(LDerived);
    Check(LDerived.Part('actions/floating').Prop('text') = '+',
      'derived recipe adds arbitrary action', Result);
    LDocument.Validate;
    Check(True, 'qualified compound identities validate', Result);
    for LIndex := 0 to LCatalog.Count - 1 do
    begin

      if LCatalog[LIndex].Kind <> 'component' then
      begin
        LOther := LCatalog.NewNode(LCatalog[LIndex].Kind, 'fixture-' + IntToStr(LIndex));
        LDocument.AddPage(LOther);
      end;
    end;
    LDocument.Validate;
    Check(True, 'every catalog factory produces valid identities', Result);
    LRoot := LDocument.Find('fixture-' + IntToStr(LCatalog.IndexOf('number-stepper')));
    LRealized := RealizeNyxView(LDocument, LRoot);
    try
      LDispatch := DispatchNyxBehavior(LRealized.Part('increment'), ntClick);
      Check((LRealized.Part('value').Prop('value') = '2') and
        (LDispatch.Source = LRealized) and (LDispatch.EventName = 'increment'),
        'portable compound state and semantic event', Result);
      Check(LRoot.Part('value').Prop('value') = '1',
        'runtime mutation preserves recipe design', Result);
      LRealized.Part('value').SetProp('value', '999');
      DispatchNyxBehavior(LRealized.Part('increment'), ntClick);
      Check(LRealized.Part('value').Prop('value') = '999', 'stepper upper clamp', Result);
    finally
      LRealized.Free;
    end;
    LRoot := LDocument.Find('fixture-' + IntToStr(LCatalog.IndexOf('segmented-control')));
    LDispatch := DispatchNyxBehavior(LRoot.Part('month'), ntClick);
    Check((LRoot.Prop('value') = 'month') and
      (LRoot.Part('month').Prop('variant') = 'primary') and
      (LRoot.Part('day').Prop('variant') = ''), 'portable segmented selection', Result);
    LRoot := LDocument.Find('fixture-' + IntToStr(LCatalog.IndexOf('search-field')));
    LRoot.Part('query').SetProp('value', 'Find me');
    DispatchNyxBehavior(LRoot.Part('clear'), ntClick);
    Check(LRoot.Part('query').Prop('value') = '', 'portable compound clear action', Result);
  finally
    LDocument.Free;
    LCatalog.Free;
  end;
  LSession := TNyxStudioSession.Create;
  try
    LBaseline := LSession.Save;
    LSession.SetTitle('Personal workspace / 🌙 漢字');
    Check((LSession.Document.Title = TNyxText('Personal workspace / 🌙 漢字')) and
      (Pos('Personal workspace / 🌙 漢字', LSession.Source) > 0),
      'project identity is ordinary authored Unicode text', Result);
    LSession.Undo;
    Check(LSession.Save = LBaseline, 'project title participates in design history', Result);
    LOutputs := TNyxOutputConfiguration.Create;
    try
      LState := DefaultNyxStudioViewState;
      LState.OutputVisible := True;
      LState.Outputs := LOutputs;
      ValidateNyxOutputTarget('');
      LShell := BuildNyxStudioView(LSession, LState);
      try
        LShell.Validate;
        Check((LShell.Find('output-none') <> nil) and
          (LShell.Find('output-pas2js') = nil),
          'Studio admits designing before output selection or tools', Result);
      finally
        LShell.Free;
      end;
      LOutputs.SetField('pas2js', 'C:/tools/🌙 漢字/pas2js.exe');
      LOutputs.SetField('runtime', 'C:/tools/rtl.js');
      LLoadedOutputs := TNyxOutputConfiguration.Decode(LOutputs.Encode);
      try
        Check(LLoadedOutputs.Field('pas2js') = TNyxText('C:/tools/🌙 漢字/pas2js.exe'),
          'output profile preserves Unicode machine paths', Result);
        Check(LLoadedOutputs.Field('fpc') = '',
          'browser configuration does not require a native profile', Result);
      finally
        LLoadedOutputs.Free;
      end;
      LState.OutputTarget := 'lcl';
      ValidateNyxOutputTarget(LState.OutputTarget);
      LShell := BuildNyxStudioView(LSession, LState);
      try
        Check((LShell.Find('output-fpc') <> nil) and
          (LShell.Find('output-pas2js') = nil) and (LSession.Save = LBaseline),
          'late output switch keeps project and history independent', Result);
      finally
        LShell.Free;
      end;
      Check(Pos('C:/tools/', LSession.Source) = 0,
        'machine profiles stay out of exported project Pascal', Result);
      LState.Compact := True;
      for LPanel := Low(TNyxStudioPanel) to High(TNyxStudioPanel) do
      begin
        LState.Panel := LPanel;
        LShell := BuildNyxStudioView(LSession, LState);
        try
          LShell.Validate;
          Check((LShell.Find('studio-workspace').Count = 1) and
            (LShell.Find('studio-panelbar').Count = 3) and
            ((LShell.Find('studio-center') <> nil) = (LPanel = nspDesign)) and
            ((LShell.Find('studio-left') <> nil) = (LPanel = nspProject)) and
            ((LShell.Find('studio-right') <> nil) = (LPanel = nspInspector)) and
            (LSession.Save = LBaseline),
            'compact panels retain shared authoring controls without changing the design', Result);
        finally
          LShell.Free;
        end;
      end;
      LRejected := False;
      try
        LOutputs.SetField('arguments', '-anything');
      except
        on LException: ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected, 'profiles reject arbitrary compiler argument fields', Result);
    finally
      LOutputs.Free;
    end;
    LSession.AddKind('search-field');
    LSelected := LSession.SelectedID;
    Check(LSession.Selected.Kind = 'search-field', 'designer adds compound', Result);
    LSession.Undo;
    Check(LSession.Save = LBaseline, 'undo restores accepted design', Result);
    LSession.Redo;
    Check(LSession.Document.Find(LSelected) <> nil, 'redo restores compound identity', Result);
    LSession.Select(LSelected);
    LSession.DuplicateSelected;
    Check(LSession.SelectedID <> LSelected, 'duplicate allocates fresh identities', Result);
    LSession.Document.Validate;
    LSession.AddPage;
    Check(LSession.Document.Count = 2, 'designer creates second page', Result);
    LSession.AddComponentInstance('welcome-card');
    LSession.Document.Validate;
    Check(LSession.Selected.Prop('component') = 'welcome-card', 'designer instantiates reusable', Result);
    LBaseline := LSession.Save;
    LRejected := False;
    try
      LSession.SetProperty('component', 'missing-definition');
    except
      on LException: ENyxModel do
        LRejected := True;
    end;
    Check(LRejected and (LSession.Save = LBaseline), 'invalid command rolls back atomically', Result);
    LRejected := False;
    try
      LSession.Load('{"version":99}');
    except
      on LException: Exception do
        LRejected := True;
    end;
    Check(LRejected and (LSession.Save = LBaseline), 'invalid import preserves accepted design', Result);
    LSession.SetProperty('text', '🌙 漢字');
    LSession.SetProperty('text', 'Another caption');
    LSession.Undo;
    Check(LSession.Selected.Prop('text') = TNyxText('🌙 漢字'),
      'Unicode caption survives designer history', Result);
    LBaseline := LSession.Save;
    LRejected := False;
    try
      LSession.SetProperty('enabled', 'not-a-boolean');
    except
      on LException: ENyxModel do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected and (LSession.Save = LBaseline),
      'typed property rejection retains accepted designer state', Result);
    LSession.Redo;
    Check(LSession.Selected.Prop('text') = 'Another caption',
      'typed rejection preserves existing redo history', Result);
    LBaseline := LSession.Save;
    LDocument := TNyxCodec.Decode(LBaseline);
    try
      LDocument.Find(LSession.SelectedID).SetProp('width', '120000');
      LRejected := False;
      try
        LSession.Load(TNyxCodec.Encode(LDocument));
      except
        on LException: ENyxModel do
        begin
          LRejected := True;
        end;
      end;
      Check(LRejected and (LSession.Save = LBaseline),
        'typed invalid import never replaces the accepted project', Result);
    finally
      LDocument.Free;
    end;
  finally
    LSession.Free;
  end;
end;

end.
