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
program nyx_choice_source_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, Classes, nyx.text, nyx.bytes, nyx.types, nyx.model, nyx.controls,
  nyx.choices, nyx.select.configuration, nyx.state, nyx.binding.types,
  nyx.codec, nyx.codegen, nyx.source
  {$ifdef PAS2JS}, Web{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

function Fixture: TNyxDocument;
var
  LPage: INyxPage;
  LSelect: INyxSelect;
begin
  Result := TNyxDocument.Create;
  try
    Result.Title := 'Friendly choices';
    Result.State.SetValue(NyxTextState('operation'), 'second/🌙');
    LSelect := NewNyxSelect('operation').WithText('Choose an operation');
    LSelect.Configure.Items('Legacy first' + #10 + 'Legacy second').Done;
    LSelect.Binds.Value(NyxTextState('operation'), bdTwoWay).Done;
    NyxSelectConfigure(LSelect).Choices(NewNyxChoices
      .WithChoice(NyxChoice(''), 'None')
      .WithChoice(NyxChoice('first'), 'Same caption 🌙')
      .WithChoice(NyxChoice('second/🌙'), 'Same caption 🌙'));
    LPage := NewNyxPage('home');
    LPage.Add(NewNyxHeading('title').WithText('Stable values, friendly captions'));
    LPage.Add(LSelect);
    Result.AddPage(LPage);
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

procedure RefuseSource(const ASource: TNyxText);
var
  LDocument: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LRefused: Boolean;
begin
  LDocument := nil;
  LWorkspace := nil;
  LRefused := False;
  try
    try
      LDocument := TNyxSourceWorkspace.PrepareDraft(ASource, LWorkspace);
    except
      on Exception do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'invalid choice source must refuse before adopting a document');
  finally
    LDocument.Free;
    LWorkspace.Free;
  end;
end;

{ Keep source edits in portable text. The native ANSI RTL StringReplace would
  convert unrelated Unicode source while replacing even an ASCII token. }
function ReplaceFirst(const ASource, AFrom, ATo: TNyxText): TNyxText;
var
  LAt: Integer;
begin
  LAt := Pos(AFrom, ASource);
  Check(LAt > 0, 'the requested source edit must find its exact original text');
  Result := Copy(ASource, 1, LAt - 1) + ATo +
    Copy(ASource, LAt + Length(AFrom), MaxInt);
end;

procedure Cases;
var
  LDocument: TNyxDocument;
  LRead: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LSource: TNyxText;
  LChanged: TNyxText;
  LBefore: TNyxText;
  LEdited: TNyxDocument;
  LVerified: TNyxDocument;
  LVerification: TNyxSourceWorkspace;
  {$ifndef PAS2JS}
  LBytes: TNyxBytes;
  LFile: TFileStream;
  {$endif}
begin
  LDocument := Fixture;
  LRead := nil;
  LWorkspace := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.fixture.choices');
    Check((Pos('LOperationSelect: INyxSelect;', LSource) > 0) and
      (Pos('SetNyxChoiceItems(LOperationSelect,', LSource) > 0),
      'generated Pascal uses a purpose-named specialized interface and typed attachment');
    Check((Pos('.WithChoice(NyxChoice(', LSource) > 0) and
      (Pos('nyx.choices, nyx.select.configuration', LSource) > 0),
      'source emits readable fluent choices and their public namespaces');
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(TNyxCodec.Encode(LRead) = LBefore,
      'generated choices reconstruct the exact complete document including bindings and legacy Items');
    Check(LWorkspace.Render(LRead) = LSource,
      'unchanged source retains its admitted byte-for-byte formatting');
    LEdited := LRead.Clone;
    LVerified := nil;
    LVerification := nil;
    try
      LEdited.Find('operation').SetChoiceItems(NewNyxChoices
        .WithChoice(NyxChoice('first'), 'A changed choice 🌙'));
      LChanged := LWorkspace.Render(LEdited);
      LVerified := TNyxSourceWorkspace.PrepareDraft(LChanged, LVerification);
      Check(TNyxCodec.Encode(LVerified) = TNyxCodec.Encode(LEdited),
        'model choice edits regenerate a complete exact typed source/document pair');
      Check(TNyxCodec.Encode(LRead) = LBefore,
        'preparing changed source does not mutate the accepted document');
      FreeAndNil(LVerified);
      FreeAndNil(LVerification);
      LEdited.Find('operation').RemoveChoiceItems;
      LChanged := LWorkspace.Render(LEdited);
      LVerified := TNyxSourceWorkspace.PrepareDraft(LChanged, LVerification);
      Check(not LVerified.HasChoiceItems and
        (TNyxCodec.Encode(LVerified) = TNyxCodec.Encode(LEdited)),
        'removing the declaration regenerates exact inherited/legacy meaning');
    finally
      LVerified.Free;
      LVerification.Free;
      LEdited.Free;
    end;
    {$ifndef PAS2JS}

    if ParamStr(1) <> '' then
    begin
      LBytes := NyxEncodeUTF8(LSource);
      LFile := TFileStream.Create(IncludeTrailingPathDelimiter(ParamStr(1)) +
        'nyx.fixture.choices.pas', fmCreate);
      try

        if Length(LBytes) > 0 then
        begin
          LFile.WriteBuffer(LBytes[0], Length(LBytes));
        end;
      finally
        LFile.Free;
      end;
    end;
    {$endif}
    FreeAndNil(LRead);
    FreeAndNil(LWorkspace);
    LChanged := ReplaceFirst(LSource, 'Same caption 🌙', 'Changed caption 🌙');
    LRead := TNyxSourceWorkspace.PrepareDraft(LChanged, LWorkspace);
    Check((LRead.Find('operation').ChoiceItems.Item(1).Caption =
      TNyxText('Changed caption 🌙')) and
      (LRead.Find('operation').ChoiceItems.Item(1).Key.Value = 'first'),
      'source caption editing preserves the stable typed value');
    FreeAndNil(LRead);
    FreeAndNil(LWorkspace);
    RefuseSource(ReplaceFirst(LSource, 'NyxChoice(''first'')', '''first'''));
    RefuseSource(ReplaceFirst(LSource, 'NyxChoice(''first'')', 'NyxChoice('''')'));
    RefuseSource(ReplaceFirst(LSource, 'SetNyxChoiceItems(LOperationSelect,',
      'SetNyxChoiceItems(LTitleHeading,'));
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'failed source candidates never mutate the independent authoring baseline');
    LDocument.Find('operation').SetChoiceItems(NewNyxChoices);
    LSource := TNyxCodegen.Generate(LDocument, 'nyx.fixture.empty.choices');
    LRead := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
    Check(LRead.Find('operation').HasChoiceItems and
      (LRead.Find('operation').ChoiceItems.Count = 0),
      'an empty fluent builder preserves its explicit inherited-set mask');
    Check(TNyxCodec.Encode(LRead) = TNyxCodec.Encode(LDocument),
      'empty choices retain the complete exact source/document pair');
  finally
    LRead.Free;
    LWorkspace.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Cases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-choice-source', 'passed');
    document.body.setAttribute('data-test-checks', IntToStr(GChecks));
    document.body.textContent := 'PASS ' + IntToStr(GChecks) + ' choice source checks';
    {$else}
    WriteLn('PASS ', GChecks, ' choice source checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-choice-source', 'failed');
      document.body.textContent := 'FAIL after ' + IntToStr(GChecks) + ': ' + E.Message;
      {$else}
      WriteLn('FAIL after ', GChecks, ': ', E.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
