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

{ Ordinary mounted Logic inputs qualify the specialized program form, draft
  restoration and complete paired publication. Setup owns an isolated local
  fixture; the separate authenticated consumer composes actual demos through MCP. }
procedure Step;
var
  LDocument: TNyxDocument;
begin
  {$ifndef PAS2JS}

  if GFailure.Error <> '' then
  begin
    raise Exception.Create(GFailure.Error);
  end;
  {$endif}

  if Clock - GStarted > 180000 then
  begin
    raise Exception.Create('Actual Studio program-route input deadline at stage ' + IntToStr(GStage) +
      ' / source busy=' + BoolToStr(GStudio.SourceBusy, True) +
      ' / presentation pending=' + BoolToStr(GStudio.PresentationPending, True));
  end;

  if GStudio.SourceBusy or GStudio.PresentationPending then
  begin
    Exit;
  end;
  case GStage of
    0:
    begin
      LDocument := ProgramLogicDocument;
      try
        LDocument.Programs.Define(NyxProgramRef('Formal greeting'), NewNyxProgramDefinition(
          'begin Caption := ''Good day, '' + Name end.', GreetingProgramContract));
        GStudio.SourceCommands.OpenProject(NyxProjectPair(TNyxCodec.Encode(LDocument),
          TNyxCodegen.Generate(LDocument)));
      finally
        LDocument.Free;
      end;
    end;
    1:
    begin
      GBefore := EncodeNyxProject(GStudio.ProjectSnapshot);

      if not OpenPrograms then
      begin
        Exit;
      end;
    end;
    2:
    begin
      Change('graph', 'Greeting flow');
      Click('studio-logic-editor-open');
    end;
    3:
    begin
      Change('step', 'greeting');
      Click('studio-logic-editor-load');
    end;
    4:
    begin
      Check(Node('studio-logic-editor-program-program').Prop('value') = 'Greeting',
        'Loading a saved program step populates its actual specialized select');
      Check(Node('studio-logic-editor-creator-config').Prop('visible') = 'false',
        'Built-in program authoring hides the raw creator configuration boundary');
      Change('program-program', 'Formal greeting');
      Check((Pos('Name', Node('studio-logic-editor-program-schema').Prop('text')) > 0) and
        (Pos('Caption', Node('studio-logic-editor-program-schema').Prop('text')) > 0),
        'Actual program choice displays its typed input and output ports');
      Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
        'Unapplied program choice changes neither accepted artifact');
      Click('action-logic-close');
    end;
    5:
    begin

      if not OpenPrograms then
      begin
        Exit;
      end;
    end;
    6:
    begin
      Check(Node('studio-logic-editor-program-program').Prop('value') = 'Formal greeting',
        'Closing and reopening retains the exact specialized program draft');
      Click('studio-logic-editor-set');
    end;
    7:
    begin
      Check(GStudio.SourceCommands.State = nssApplied,
        'Ordinary program-step Apply uses the paired source command: ' + GStudio.SourceCommands.Message);
      LDocument := TNyxCodec.Decode(GStudio.ProjectSnapshot.Design);
      try
        Check(LDocument.Flows.Definition(NyxFlowRef('Greeting flow')).Configuration(0)
          .Field('program').AsText = 'Formal greeting',
          'Applied specialized configuration belongs to the accepted saved graph');
      finally
        LDocument.Free;
      end;
      GAfter := EncodeNyxProject(GStudio.ProjectSnapshot);
      Check((GAfter <> GBefore) and
        (Pos('NewNyxProgramLogic(NyxProgramRef(''Formal greeting'')', GStudio.ProjectSnapshot.Source) > 0),
        'The same ordinary Apply produces crafted specialized Pascal');
      History(False);
    end;
    8:
    begin
      Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
        'One actual Undo restores the complete pre-configuration pair');
      History(True);
    end;
    9:
    begin
      Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GAfter,
        'One actual Redo restores the exact saved program route and Pascal');
      GDone := True;
    end;
  end;
  {$ifndef PAS2JS}
  WriteLn('Program Logic input stage ', GStage, ' / elapsed milliseconds ',
    Round(Clock - GStarted));
  Flush(Output);
  {$endif}
  Inc(GStage);
end;
