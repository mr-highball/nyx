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
program nyx_focused_context_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.composition,
  nyx.codec, nyx.codegen, nyx.source, nyx.state, nyx.resources, nyx.root.types,
  nyx.content.mount, nyx.application.view, nyx.application.state,
  nyx.test.focused.context
  {$ifdef PAS2JS}, Web{$else}, Classes, nyx.bytes{$endif};

var
  GChecks: Integer;

procedure Check(AValue: Boolean; const AReason: String);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
{ Export exact UTF-8 pairs for independent compiler consumers, under an explicitly
  supplied owned directory. No project, configuration or host is addressed. }
procedure ExportDocument(const AName: String; ADocument: TNyxDocument);
var
  LDirectory: String;

  procedure Save(const AName: String; const AText: TNyxText);
  var
    LBytes: TNyxBytes;
    LStream: TFileStream;
  begin
    LBytes := NyxEncodeUTF8(AText);
    LStream := TFileStream.Create(LDirectory + AName, fmCreate);
    try

      if Length(LBytes) > 0 then
      begin
        LStream.WriteBuffer(LBytes[0], Length(LBytes));
      end;
    finally
      LStream.Free;
    end;
  end;
begin

  if ParamStr(1) = '' then
  begin
    Exit;
  end;
  LDirectory := IncludeTrailingPathDelimiter(ParamStr(1)) + AName + PathDelim;
  ForceDirectories(LDirectory);
  Save('nyx.generated.view.pas', TNyxCodegen.Generate(ADocument));
  Save('expected.nyx', TNyxCodec.Encode(ADocument));
end;
{$endif}

procedure Run;
const
  CViews: array[0..4] of TNyxText = ('home', 'Wrapper', 'focused-memo',
    'nested-canvas', FocusedUnicodeID);
var
  LDocument: TNyxDocument;
  LFocused: TNyxDocument;
  LDecoded: TNyxDocument;
  LSimple: TNyxDocument;
  LWorkspace: TNyxSourceWorkspace;
  LBlueprint: TNyxContentBlueprint;
  LRuntime: TNyxApplicationState;
  LView: TNyxApplicationView;
  LBefore: TNyxText;
  LSource: TNyxText;
  LIndex: Integer;
  LFailed: Boolean;
begin
  LDocument := FocusedContextDocument;
  LFocused := nil;
  LDecoded := nil;
  LSimple := nil;
  LBlueprint := nil;
  LRuntime := nil;
  LWorkspace := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    {$ifdef NYX_EXPECT_FOCUSED_GAP}
    LFailed := False;
    try
      LFocused := CloneNyxViewDocument(LDocument, LDocument.Find('Wrapper'));
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'baseline reproduces missing retained diagram/canvas dependency admission');
    Exit;
    {$endif}
    for LIndex := Low(CViews) to High(CViews) do
    begin
      LFocused := CloneNyxViewDocument(LDocument, LDocument.Find(CViews[LIndex]));
      Check((LFocused.Find(CViews[LIndex]) <> nil) and
        (LFocused.FindComponent('Wrapper') <> nil) and
        (LFocused.FindComponent('Leaf') <> nil) and
        (LFocused.FindComponent('Unrelated') = nil),
        'focus preserves selected identity and complete retained named context');
      Check((LFocused.Diagrams.ToData.ToJSON = LDocument.Diagrams.ToData.ToJSON) and
        (LFocused.Canvases.ToData.ToJSON = LDocument.Canvases.ToData.ToJSON) and
        (LFocused.Menus.ToData.ToJSON = LDocument.Menus.ToData.ToJSON),
        'retained diagram/canvas/menu definitions preserve their exact original roots');
      LView := TNyxApplicationView.Focused(NyxControl(CViews[LIndex]));
      Check(LView.Resolve(LFocused).ID = CViews[LIndex],
        'typed focus resolves requested page, reusable or descendant explicitly');
      LRuntime := LView.CreateRuntime(LFocused, npfAny, NyxDefaultLocale, NyxDefaultLocale);
      Check(LView.InitialCollections(LFocused, LRuntime) <> nil,
        'focused root/descendant bindings share an independent runtime context');
      LRuntime.State.SetValue(NyxTextState('reply'), 'Changed runtime reply');
      Check((LFocused.State.GetValue(NyxTextState('reply')) = 'Write a focused reply') and
        (LDocument.State.GetValue(NyxTextState('reply')) = 'Write a focused reply'),
        'focused application runtime never mutates either source default store');
      FreeAndNil(LRuntime);
      LBlueprint := TNyxContentBlueprint.Create(LDocument, LDocument.Find(CViews[LIndex]));
      Check(LBlueprint.Root.ID = CViews[LIndex],
        'content blueprint selects the requested node instead of the first context page');
      FreeAndNil(LBlueprint);
      LSource := TNyxCodegen.Generate(LFocused);
      Check(PrepareNyxCompanion(LFocused, LFocused, LSource, False) = LSource,
        'combined named declarations pass strict exact companion/import admission');
      LDecoded := TNyxSourceWorkspace.PrepareDraft(LSource, LWorkspace);
      Check(TNyxCodec.Encode(LDecoded) = TNyxCodec.Encode(LFocused),
        'focused context reconstructs its exact complete generated source meaning');
      {$ifndef PAS2JS}

      if LIndex = High(CViews) then
      begin
        ExportDocument('unicode', LFocused);
      end
      else
      begin
        ExportDocument(CViews[LIndex], LFocused);
      end;
      {$endif}
      FreeAndNil(LDecoded);
      FreeAndNil(LWorkspace);
      FreeAndNil(LFocused);
    end;
    LSimple := TNyxDocument.Create;
    LSimple.AddPage(NewNyxPage('plain-home'));
    LSimple.AddComponent(NewNyxLabel('plain-label').WithText('Simple reusable'));
    LFocused := CloneNyxViewDocument(LSimple, LSimple.Components[0]);
    Check((LFocused.Count = 1) and (LFocused.ComponentCount = 0) and
      (LFocused.Pages[0].ID = 'plain-label'), 'legacy simple reusable focus retains its exact shape');
    FreeAndNil(LFocused);
    LFailed := False;
    try
      LView := TNyxApplicationView.Focused(NyxControl('missing-control'));
      LView.Resolve(LDocument);
    except
      on ENyxModel do
      begin
        LFailed := True;
      end;
    end;
    Check(LFailed and (TNyxCodec.Encode(LDocument) = LBefore),
      'unknown focus refuses and every successful/refused operation preserves the original source');
  finally
    LWorkspace.Free;
    LRuntime.Free;
    LBlueprint.Free;
    LSimple.Free;
    LDecoded.Free;
    LFocused.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-focused-context', 'passed');
    document.body.setAttribute('data-focused-context-checks', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' focused source context checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-focused-context', 'failed');
      document.body.setAttribute('data-focused-context-error', E.Message);
      {$else}
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
