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
program nyx_column_events_source;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, Classes, nyx.text, nyx.bytes, nyx.types, nyx.model, nyx.controls,
  nyx.state, nyx.data, nyx.codec, nyx.codegen, nyx.source, nyx.callbacks,
  nyx.schema, nyx.scheduler, nyx.collections, nyx.collections.columns,
  nyx.collections.view.types, nyx.collections.arrangement,
  nyx.studio.session, nyx.studio.inspector;

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create('Column authoring: ' + AReason);
  end;
  Inc(GChecks);
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LTable: INyxTable;
  LSpec: TNyxCollectionViewSpec;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Arrange your service monitor';
    Result.State.SetValue(NyxTextState('column-status'), 'Arrange a column to see its field.')
      .SetValue(NyxTextState('column-preferences'), '')
      .SetValue(NyxIntegerState('column-change-count'), 0);
    Result.Collections.Define(NyxCollection('services'),
      NyxCollectionSchema.Text(NyxTextField('service 🌙'), '')
        .Integer(NyxIntegerField('pid'), 0), [
      NyxCollectionItem(NyxItem(NyxCollection('services'), 'gateway'))
        .WithValue(NyxTextField('service 🌙'), 'Gateway')
        .WithValue(NyxIntegerField('pid'), 2401),
      NyxCollectionItem(NyxItem(NyxCollection('services'), 'worker'))
        .WithValue(NyxTextField('service 🌙'), 'Background worker')
        .WithValue(NyxIntegerField('pid'), 2402)]);
    LPage := NewNyxPage('monitor');
    LPage.Configure.Padding(16).Gap(12).Done;
    Result.AddPage(LPage);
    LPage.Add(NewNyxHeading('monitor-heading').Configure.Text('Your service monitor').Done);
    LPage.Add(NewNyxLabel('column-status-label').Binds.Text(NyxTextState('column-status')).Done);
    LSpec := NyxCollectionView(NyxCollection('services'))
      .Column(NyxTextField('service 🌙'), 'Service')
        .ColumnSizing(NyxColumnSizing.Fixed(180).Minimum(50).Maximum(600))
      .Column(NyxIntegerField('pid'), 'Process ID')
        .ColumnSizing(NyxColumnSizing.Fixed(100).Minimum(50).Maximum(400))
      .TableLayout(NyxTableLayout);
    LTable := NewNyxTable('service-table');
    LTable.Configure.Height(200).Done;
    LTable.Binds.Collection(LSpec);
    LPage.Add(LTable);
    LPage.Add(NewNyxLabel('peer-guide').Configure.Text('Independent peer arrangement').Done);
    LTable := NewNyxTable('peer-table');
    LTable.Configure.Height(160).Done;
    LTable.Binds.Collection(LSpec);
    LPage.Add(LTable);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

function ReplaceOnce(const ASource, ABefore, AAfter: TNyxText): TNyxText;
var
  LPosition: Integer;
begin
  LPosition := Pos(ABefore, ASource);
  Check(LPosition > 0, 'implementation replacement addresses the exact emitted TODO');
  Result := Copy(ASource, 1, LPosition - 1) + AAfter +
    Copy(ASource, LPosition + Length(ABefore), MaxInt);
end;

procedure Save(const AName: String; const AText: TNyxText);
var
  LFile: TFileStream;
  LBytes: TNyxBytes;
begin
  LBytes := NyxEncodeUTF8(AText);
  LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) + AName, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LFile.WriteBuffer(LBytes[0], Length(LBytes));
    end;
  finally
    LFile.Free;
  end;
end;

procedure Run;
var
  LBase: TNyxDocument;
  LLoaded: TNyxDocument;
  LSession: TNyxStudioSession;
  LEvents: TNyxEventSchemas;
  LIndex: Integer;
  LFound: Integer;
  LLine: Integer;
  LFirst: TNyxHandlerRef;
  LSecond: TNyxHandlerRef;
  LBeforeDesign: TNyxText;
  LBeforeSource: TNyxText;
  LAfterDesign: TNyxText;
  LAfterSource: TNyxText;
  LSource: TNyxText;
  LInspector: INyxColumn;
begin
  Check(ParamCount = 1, 'an explicit external output directory is required');
  LBase := Fixture;
  LSession := TNyxStudioSession.Create;
  try
    LEvents := NyxEventsMetadata(LBase.Find('service-table'), LBase);
    LFound := 0;
    for LIndex := 0 to High(LEvents) do
    begin

      if LEvents[LIndex].Trigger = ntColumnsChanged then
      begin
        Inc(LFound);
        Check((LEvents[LIndex].Browser = ncAvailable) and
          (LEvents[LIndex].Native = ncAvailable) and
          (nctxCollectionColumns in LEvents[LIndex].Contexts) and
          (Pos('CollectionColumns', LEvents[LIndex].Description) > 0),
          'bound table metadata describes the actual typed adapter payload');
      end;
    end;
    Check(LFound = 1, 'bound table advertises one built-in columns event');
    LEvents := NyxEventsMetadata(NewNyxTable('unbound').Node);
    LFound := 0;
    for LIndex := 0 to High(LEvents) do
    begin

      if LEvents[LIndex].Trigger = ntColumnsChanged then
      begin
        Inc(LFound);
      end;
    end;
    Check(LFound = 0, 'an unbound table does not promise the runtime event');
    Check(not NyxIsInputTrigger(ntColumnsChanged) and
      (NyxTriggerSymbol(ntColumnsChanged) = 'ntColumnsChanged'),
      'closed trigger is a committed notification with a stable Pascal symbol');
    LSession.Load(TNyxCodec.Encode(LBase));
    LSession.Select('service-table');
    LBeforeDesign := TNyxCodec.Encode(LSession.Document);
    LBeforeSource := LSession.Source;
    LFirst := LSession.AddCallback(ntColumnsChanged, LLine);
    Check((LLine > 0) and (Pos('// TODO: implement ' + LFirst.Name + '.', LSession.Source) > 0),
      'ordinary Studio Add supplies a navigable implementation TODO');
    LAfterDesign := TNyxCodec.Encode(LSession.Document);
    LAfterSource := LSession.Source;
    LSession.Undo;
    Check((TNyxCodec.Encode(LSession.Document) = LBeforeDesign) and
      (LSession.Source = LBeforeSource), 'one Undo restores the exact document/source pair');
    LSession.Redo;
    Check((TNyxCodec.Encode(LSession.Document) = LAfterDesign) and
      (LSession.Source = LAfterSource), 'Redo restores the complete event/source pair');
    LSecond := LSession.AddCallback(ntColumnsChanged, LLine);
    LSession.SetCallbackPolicy(ntColumnsChanged, neUIQueue);
    Check((Length(NyxAuthoredEvents(LSession.Selected)[0].Callbacks) = 2) and
      (NyxAuthoredEvents(LSession.Selected)[0].Policy = neUIQueue),
      'multiple registrations share the ordinary per-event scheduler policy');
    LInspector := NewNyxColumn('column-events-inspector');
    AddNyxEventsInspector(LInspector.Node, LSession, LSession.Selected,
      Default(TNyxCallbackRemoval));
    Check(Pos('OnColumnsChanged', TNyxCodec.NodeSnapshot(LInspector.Node).ToJSON) > 0,
      'ordinary Nyx-built Events inspector contains the callback card');
    LSource := LSession.Source;
    Check((Pos('.On(ntColumnsChanged)', LSource) > 0) and
      (Pos('.OnColumnsChanged', LSource) = 0),
      'crafted source uses the existing strongly typed On interface without a GUID change');
    { These ordinary handwritten bodies consume only detached typed payloads.
      They need no renderer, platform handle or test-only callback registration. }
    LSource := ReplaceOnce(LSource, '// TODO: implement ' + LFirst.Name + '.',
      '{ Keep the exact accepted field and count in application state. }' + #10 + #10 +
      '  if not AEvent.CollectionColumns.Defined then' + #10 +
      '  begin' + #10 + '    Exit;' + #10 + '  end;' + #10 +
      '  Context.State.SetValue(NyxTextState(''column-status''),' + #10 +
      '    AEvent.CollectionColumns.ChangeAt(0).Field.Name);' + #10 +
      '  Context.State.SetValue(NyxIntegerState(''column-change-count''),' + #10 +
      '    Context.State.GetValue(NyxIntegerState(''column-change-count'')) + 1);');
    LSource := ReplaceOnce(LSource, '// TODO: implement ' + LSecond.Name + '.',
      '{ Persist the detached preferences without retaining a live view. }' + #10 + #10 +
      '  if AEvent.CollectionColumns.Defined then' + #10 +
      '  begin' + #10 +
      '    Context.State.SetValue(NyxTextState(''column-preferences''),' + #10 +
      '      AEvent.CollectionColumns.After.ToData.ToJSON);' + #10 + '  end;');
    LSource := ReplaceOnce(LSource, 'unit nyx.generated.view;', 'unit nyx.generated.column.events;');
    LLoaded := TNyxCodec.Decode(TNyxCodec.Encode(LSession.Document));
    try
      Check(TNyxCodec.Encode(LLoaded) = TNyxCodec.Encode(LSession.Document),
        'strict saved metadata retains policy and both registration identities');
    finally
      LLoaded.Free;
    end;
    Save('nyx.generated.column.events.pas', LSource);
    Save('column-events.nyx', TNyxCodec.Encode(LSession.Document));
  finally
    LInspector := nil;
    LSession.Free;
    LBase.Free;
  end;
end;

begin
  try
    Run;
    WriteLn('PASS ', GChecks, ' Studio column authoring/source checks');
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', TNyxText(LException.Message));
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
