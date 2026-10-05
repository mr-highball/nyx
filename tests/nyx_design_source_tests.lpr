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

program nyx_design_source_tests;
{$mode delphi}{$H+}{$codepage utf8}
uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.model, nyx.controls, nyx.codec,
  nyx.codegen, nyx.schema, nyx.source, nyx.source.preparation,
  nyx.studio.projects, nyx.studio.session, nyx.test.source.canvas
  {$ifdef PAS2JS}, Web{$else}, Classes{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise ENyxModel.Create('Design preparation: ' + AReason);
  end;
  Inc(GChecks);
end;

function Fixture(ACrafted: Boolean = True; ARepeated: Boolean = False): TNyxProjectPair;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LSource: TNyxText;
  LParts: TNyxStrings;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'English design review';
    LPage := NewNyxPage('home');
    LPage.Configure.Layout(nlColumn).Gap(12).Done;
    LPage.Add(NewNyxLabel('greeting').WithText('Hello'));
    LPage.Add(NewNyxMemo('notes').WithText('Notes'));
    LDocument.AddPage(LPage);

    if ARepeated then
    begin
      LDocument.Find('greeting').Extensions.SetValue(NyxExtension('review'), NyxData('Last'));
    end;
    LSource := TNyxCodegen.Generate(LDocument);

    if not ACrafted then
    begin
      Exit(NyxProjectPair(TNyxCodec.Encode(LDocument), LSource));
    end;
    LSource := StringReplace(LSource, 'LGreetingLabel', 'LWelcomeCaption', [rfReplaceAll]);
    LSource := StringReplace(LSource, '''Hello''', '''Hel'' + ''lo''', [rfReplaceAll]);

    if ARepeated then
    begin
      { Two supported extension slots set different values. Their order matters:
        the second restores the admitted final value. Configure deliberately
        remains one block per control. Perform ASCII fixture replacements BEFORE
        adding its owned Unicode helper prefix. }
      LSource := StringReplace(LSource, '''Last''', '''First''', []);
      LSource := StringReplace(LSource, '    LNotesMemo :=',
        '    LWelcomeCaption.Extensions.SetValue(NyxExtension(''review''), NyxData(''Last''));' + #10 + #10 +
        '    LNotesMemo :=', []);
    end;
    { Plain generation has no explicit workspace markers yet. Prefix comments
      are a real handwritten extension boundary; assemble exact owned text
      rather than replacing a marker that does not exist or passing Unicode
      through the native ANSI StringReplace boundary. }
    LParts := TNyxStrings.Create;
    try
      LParts.Add('{ Application helper remains handwritten / 🌙 / 漢字 }');
      LParts.Add(LSource);
      LSource := LParts.Join(#10);
    finally
      LParts.Free;
    end;
    Result := NyxProjectPair(TNyxCodec.Encode(LDocument), LSource);
  finally
    LDocument.Free;
  end;
end;

function Intent(ASession: TNyxStudioSession; AAction: TNyxStudioDesignAction;
  const AName: TNyxText = ''; const AValue: TNyxText = ''): TNyxStudioDesignEdit;
begin
  Result := Default(TNyxStudioDesignEdit);
  Result.Action := AAction;
  Result.Selection := ASession.SelectedID;
  Result.View := ASession.ActiveViewID;
  Result.Name := AName;
  Result.Value := AValue;
end;

{$ifndef PAS2JS}
procedure ExportPair(const APair: TNyxProjectPair; const ASubdirectory: TNyxText = '');
var
  LDirectory: TNyxText;

  procedure WriteText(const AName, AText: TNyxText);
  var
    LStream: TFileStream;
  begin
    LStream := TFileStream.Create(IncludeTrailingPathDelimiter(LDirectory) + AName, fmCreate);
    try

      if Length(AText) > 0 then
      begin
        LStream.WriteBuffer(AText[1], Length(AText));
      end;
    finally
      LStream.Free;
    end;
  end;

begin

  if ParamCount = 1 then
  begin
    LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + ASubdirectory;
    ForceDirectories(LDirectory);
    WriteText('nyx.generated.view.pas', APair.Source);
    WriteText('expected.nyx', APair.Design);
  end;
end;
{$endif}

procedure RunCanvas;
var
  LPair: TNyxProjectPair;
begin
  Inc(GChecks, RunNyxCanvasSourceTests(LPair));
  {$ifndef PAS2JS}ExportPair(LPair, 'canvas');{$endif}
end;

procedure Run;
var
  LSession: TNyxStudioSession;
  LOther: TNyxStudioSession;
  LBefore: TNyxProjectPair;
  LAfter: TNyxProjectPair;
  LEdit: TNyxStudioDesignEdit;
  LRequest: TNyxStudioDesignRequest;
  LRoundTrip: TNyxStudioDesignRequest;
  LPrepared: INyxPreparedDesign;
  LReceived: INyxPreparedDesign;
  LSchemas: INyxSchemaSnapshot;
  LDocument: TNyxDocument;
  LAction: TNyxStudioDesignAction;
  LKind: TNyxText;
  LRefused: Boolean;
  LNoUndo: Boolean;
  LVariant: Integer;
  LTransferred: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LProperty: TNyxPropertyInfo;
begin
  LSession := TNyxStudioSession.Create(Fixture);
  LOther := nil;
  try
    LSession.Select('greeting');
    LBefore := LSession.ProjectSnapshot;
    Check(Pos('LWelcomeCaption', LBefore.Source) > 0, 'Fixture contains the crafted local name');
    Check(Pos(TNyxText('Application helper remains handwritten / 🌙 / 漢字'),
      LBefore.Source) > 0, 'Fixture contains exact handwritten Unicode in its extension boundary');
    LDocument := LSession.Document;
    LSchemas := CaptureNyxSchemas;
    LEdit := Intent(LSession, sdaProperty, NyxAttributeName(atText), 'A new caption / 🌙');
    LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
    LRoundTrip := ReadNyxStudioDesignRequest(TNyxDataValue.ParseJSON(LRequest.ToData.ToJSON));
    Check(LRequest.SameRequest(LRoundTrip), 'Private ticket wire retains exact baseline/intent Unicode');
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(not LPrepared.Diagnostic.Defined, 'Independent property preparation admits the candidate');
    Check((LSession.Document = LDocument) and (LSession.ProjectSnapshot.Source = LBefore.Source),
      'Processor leaves the accepted tree and handwritten source untouched');
    LReceived := ReceiveNyxPreparedDesign(TNyxDataValue.ParseJSON(LPrepared.ToData.ToJSON),
      LRequest, LSchemas);
    Check(LReceived.Source = LPrepared.Source, 'Private reply retains the complete exact companion');
    Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied,
      'Fresh paired publication admits the reconstructed worker result');
    Check(LSession.Document.Find('greeting').Prop('text') = TNyxText('A new caption / 🌙'),
      'Published document keeps exact supplementary text');
    Check((Pos('LWelcomeCaption', LSession.Source) > 0) and
      (Pos(TNyxText('Application helper remains handwritten / 🌙 / 漢字'), LSession.Source) > 0),
      'Reconciliation preserves crafted local names and handwritten Unicode comments');
    LAfter := LSession.ProjectSnapshot;
    {$ifndef PAS2JS}ExportPair(LAfter);{$endif}
    LSession.Undo;
    Check((LSession.Source = LBefore.Source) and (LSession.Save = LBefore.Design) and
      not LSession.CanUndo, 'One Undo restores both original files without processor intermediates');
    LSession.Redo;
    Check((LSession.Source = LAfter.Source) and (LSession.Save = LAfter.Design),
      'Redo restores the exact admitted pair');
    LPrepared := nil;
    LReceived := nil;

    LSession.Undo;
    LSession.Select('greeting');
    LNoUndo := LSession.CanUndo;
    LRequest := LSession.PrepareDesignRequest(
      Intent(LSession, sdaProperty, NyxAttributeName(atText), 'Hello'), LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscUnchanged,
      'No-op intent retains exact handwritten expression');
    Check((LSession.CanUndo = LNoUndo) and LSession.CanRedo,
      'No-op preparation does not consume paired history');
    LPrepared := nil;

    LRequest := LSession.PrepareDesignRequest(
      Intent(LSession, sdaProperty, NyxAttributeName(atText), 'Worker value'), LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    LSession.Document.Find('greeting').Configure.Text('Direct accepted mutation').Done;
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscStale,
      'Fresh baseline guard detects direct public mutation');
    Check(LSession.Document.Find('greeting').Prop('text') = 'Direct accepted mutation',
      'Stale processor cannot overwrite direct mutation');
    LPrepared := nil;

    LBefore := LSession.ProjectSnapshot;
    LOther := TNyxStudioSession.Create(LBefore);
    LRequest := LSession.PrepareDesignRequest(
      Intent(LSession, sdaTitle, '', 'Worker title'), LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(LOther.CompleteDesignRequest(LRequest, LPrepared) = nscStale,
      'Identical files in another session cannot accept this owner ticket');
    LRefused := False;
    try
      LRoundTrip := LOther.PrepareDesignRequest(
        Intent(LOther, sdaTitle, '', 'Other title'), LSchemas.Revision);
      LReceived := ReceiveNyxPreparedDesign(LPrepared.ToData, LRoundTrip, LSchemas);
    except
      on LException: ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'Private result with a different ticket refuses before publication');
    LPrepared := nil;
    FreeAndNil(LOther);

    LSession.SetSourceDraft(LSession.Source + #10 + '// My pending application notes');
    LBefore := LSession.ProjectSnapshot;
    LRequest := LSession.PrepareDesignRequest(
      Intent(LSession, sdaTitle, '', 'Accepted design title'), LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
      'Visual edit can admit its pair beside an existing source draft');
    LAfter := LSession.ProjectSnapshot;
    Check(LAfter.Pending and (LAfter.Draft = LBefore.Draft) and
      (LAfter.DraftBase = LBefore.DraftBase),
      'Visual reconciliation retains the exact pending draft and its original stale base');
    LPrepared := nil;
    LSession.DiscardSourceDraft;

    LRequest := LSession.PrepareDesignRequest(
      Intent(LSession, sdaProperty, NyxAttributeName(atGap), 'invalid number'), LSchemas.Revision);
    LBefore := LSession.ProjectSnapshot;
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    Check(LPrepared.Diagnostic.Defined and (LPrepared.Design = '') and
      (LPrepared.Source = ''), 'Failed admission owns diagnostics without partial files');
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscRejected,
      'Rejected property command preserves accepted owners');
    Check((LSession.Save = LBefore.Design) and (LSession.Source = LBefore.Source),
      'Rejected visual command changes neither member of the pair');
    LPrepared := nil;

    { A structural move must carry admitted authored configuration AFTER the
      new ownership call. Multiple original slots retain their execution order;
      comments, specialized names and unchanged expressions remain exact. }
    for LVariant := 0 to 1 do
    begin
      LSession.LoadProject(Fixture(True, LVariant = 1));
      LSession.Select('greeting');
      LBefore := LSession.ProjectSnapshot;
      LEdit := Intent(LSession, sdaMove);
      LEdit.Direction := nmdNext;
      LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
      LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
      Check(not LPrepared.Diagnostic.Defined,
        'Authored move admits its reordered slots / ' + LPrepared.Diagnostic.Message);
      LReceived := ReceiveNyxPreparedDesign(TNyxDataValue.ParseJSON(LPrepared.ToData.ToJSON),
        LRequest, LSchemas);
      Check(LSession.CompleteDesignRequest(LRequest, LReceived) = nscApplied,
        'Authored move publishes its independently reconstructed exact pair');
      Check((LSession.Document.Pages[0].Children[0].ID = 'notes') and
        (LSession.Document.Pages[0].Children[1].ID = 'greeting') and
        (LSession.Document.Find('greeting').Prop('text') = 'Hello'),
        'Moved ownership and ordered configuration preserve exact final meaning');

      if LVariant = 1 then
      begin
        Check(LSession.Document.Find('greeting').Extensions.Value(NyxExtension('review')).AsText = 'Last',
          'Relocated supported extension slots retain their authored execution order');
      end;
      Check((Pos('LWelcomeCaption', LSession.Source) > 0) and
        (Pos('''Hel'' + ''lo''', LSession.Source) > 0) and
        (Pos(TNyxText('Application helper remains handwritten / 🌙 / 漢字'),
          LSession.Source) > 0), 'Authored move preserves local, expression and Unicode helper');
      LAfter := LSession.ProjectSnapshot;
      {$ifndef PAS2JS}ExportPair(LAfter);{$endif}
      LSession.Undo;
      Check((LSession.Source = LBefore.Source) and (LSession.Save = LBefore.Design) and
        not LSession.CanUndo, 'One Undo restores the exact original authored move pair');
      LSession.Redo;
      Check((LSession.Source = LAfter.Source) and (LSession.Save = LAfter.Design),
        'Authored move Redo restores the exact admitted pair');
      LPrepared := nil;
      LReceived := nil;
      LEdit.Direction := nmdPrevious;
      LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
      LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
      Check(not LPrepared.Diagnostic.Defined and
        (LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied) and
        (LSession.Document.Pages[0].Children[0].ID = 'greeting') and
        (LSession.Document.Find('greeting').Prop('text') = 'Hello'),
        'Reverse authored move keeps its specialized configuration and final value');
      LPrepared := nil;
    end;

    { Every structural command uses its existing session implementation.
      Runtime selection results and paired history are qualified independently. }
    for LAction := sdaAddKind to sdaCustomizePart do
    begin

      if LAction in [sdaProperty, sdaTitle] then
      begin
        Continue;
      end;
      LSession.LoadProject(Fixture);
      LSession.Select('greeting');
      LEdit := Intent(LSession, LAction);

      if LAction = sdaAddKind then
      begin
        LEdit.Name := NyxKindName(nkButton);
      end
      else if LAction = sdaMove then
      begin
        LEdit.Direction := nmdNext;
      end
      else if LAction = sdaAddInstance then
      begin
        LSession.CreateComponent;
        LKind := LSession.ActiveViewID;
        LSession.Activate('home');
        LEdit := Intent(LSession, LAction, LKind);
      end
      else if LAction = sdaCustomizePart then
      begin
        LSession.CreateComponent;
        LKind := LSession.ActiveViewID;
        LSession.Activate('home');
        LSession.AddComponentInstance(LKind);
        LEdit := Intent(LSession, LAction, '.');
      end;
      LBefore := LSession.ProjectSnapshot;
      LRequest := LSession.PrepareDesignRequest(LEdit, LSchemas.Revision);
      LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
      Check(not LPrepared.Diagnostic.Defined, 'Structural action admits ' +
        IntToStr(Ord(LAction)) + ' / ' + LPrepared.Diagnostic.Message);
      Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied,
        'Structural action publishes one pair ' + IntToStr(Ord(LAction)));
      LAfter := LSession.ProjectSnapshot;
      LSession.Undo;
      Check((LSession.Source = LBefore.Source) and (LSession.Save = LBefore.Design),
        'Structural Undo restores exact pair ' + IntToStr(Ord(LAction)));
      LSession.Redo;
      Check((LSession.Source = LAfter.Source) and (LSession.Save = LAfter.Design),
        'Structural Redo restores exact pair ' + IntToStr(Ord(LAction)));
      LPrepared := nil;
    end;

    LSession.LoadProject(Fixture);
    LBefore := LSession.ProjectSnapshot;
    LRequest := LSession.PrepareDesignRequest(
      Intent(LSession, sdaTitle, '', 'Reloaded candidate'), LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    LSession.LoadProject(LBefore);
    Check(LSession.CompleteDesignRequest(LRequest, LPrepared) = nscStale,
      'Reloading identical files retires earlier generation tickets');
    LPrepared := nil;

    LRequest := LSession.PrepareDesignRequest(
      Intent(LSession, sdaTitle, '', 'Candidate before typing'), LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    LSession.SetSourceDraft(LSession.Source + TNyxText(#10 + '// Fresh application typing / 🌙'));
    LAfter := LSession.ProjectSnapshot;
    Check((LSession.CompleteDesignRequest(LRequest, LPrepared) = nscStale) and
      (LSession.DraftSource = LAfter.Draft),
      'Typing after dispatch refuses stale visual publication and retains exact new draft');
    LPrepared := nil;
    LSession.DiscardSourceDraft;

    LSession.Select('greeting');
    LRequest := LSession.PrepareDesignRequest(
      Intent(LSession, sdaTitle, '', 'Navigation review'), LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    LSession.Select('notes');
    Check((LSession.CompleteDesignRequest(LRequest, LPrepared) = nscApplied) and
      (LSession.SelectedID = 'notes'),
      'Independent navigation does not retarget the command or steal current selection');
    LPrepared := nil;

    LBefore := LSession.ProjectSnapshot;
    LRequest := LSession.PrepareDesignRequest(
      Intent(LSession, sdaTitle, '', 'Transfer review'), LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    LTransferred := nil;
    LWorkspace := nil;
    try
      LPrepared.Take(LTransferred, LWorkspace);
      Check((LTransferred <> nil) and (LWorkspace <> nil),
        'One transfer owns the complete independent document/workspace pair');
      LRefused := False;
      try
        LSession.CompleteDesignRequest(LRequest, LPrepared);
      except
        on LException: ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LSession.Source = LBefore.Source) and
        (LSession.Save = LBefore.Design),
        'Consumed result refuses reuse without partially publishing either owner');
    finally
      LWorkspace.Free;
      LTransferred.Free;
    end;
    LPrepared := nil;

    LRequest := LSession.PrepareDesignRequest(
      Intent(LSession, sdaTitle, '', 'Older creator environment'), LSchemas.Revision);
    LPrepared := PrepareNyxStudioDesign(LRequest, LSchemas);
    LProperty := Default(TNyxPropertyInfo);
    LProperty.Key := 'review-text';
    LProperty.Title := 'Creator review text';
    LProperty.ValueType := npText;
    RegisterNyxSchema(NyxCustomKind('design-preparation-creator-review'), [LProperty], []);
    Check((LSession.CompleteDesignRequest(LRequest, LPrepared) = nscStale) and
      (LSession.Source = LBefore.Source) and (LSession.Save = LBefore.Design),
      'Creator publication retires captured tickets while preserving the accepted pair');
    LPrepared := nil;
  finally
    LReceived := nil;
    LPrepared := nil;
    LOther.Free;
    LSession.Free;
  end;
end;

begin
  try
    Run;
    RunCanvas;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-design-source', 'passed');
    document.body.setAttribute('data-design-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' detached design/source checks');
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-design-source', 'failed');
      document.body.setAttribute('data-design-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
