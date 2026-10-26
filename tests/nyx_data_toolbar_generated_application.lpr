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
program nyx_data_toolbar_generated_application;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web, nyx.application.browser,{$else}
  Classes, Interfaces, Forms, Controls, StdCtrls, nyx.application.lcl,{$endif}
  SysUtils, nyx.text, nyx.bytes, nyx.types, nyx.model, nyx.codec,
  nyx.coordinators, nyx.data.toolbar, nyx.collections, nyx.collections.view,
  nyx.generated.toolbar, nyx.generated.toolbar.disabled;

const
  { Native comparison stays in portable UTF-8 text without an implicit widening
    conversion caused by the supplementary character in an untyped literal. }
  CBaseline: TNyxText = 'Original author''s order 🌙';

type
  {$ifdef PAS2JS}
  { Ordinary browser application executes the exact emitted fixture builder. }
  TReviewApplication = TNyxBrowserApplication;
  {$else}
  { Ordinary native application executes the exact emitted fixture builder. }
  TReviewApplication = TNyxLCLApplication;
  { Preserve callback failure evidence without a blocking widgetset dialog. }
  TFailures = class
  public
    Message: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
  {$endif}

var
  GChecks: Integer;
  {$ifndef PAS2JS}
  GFailures: TFailures;
  {$endif}

procedure Check(ACondition: Boolean; const AMessage: TNyxText);
begin
  {$ifndef PAS2JS}

  if GFailures.Message <> '' then
  begin
    raise Exception.Create(GFailures.Message);
  end;
  {$endif}

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
procedure TFailures.Failed(ASender: TObject; AException: Exception);
begin

  if Message = '' then
  begin
    Message := AException.ClassName + ': ' + AException.Message;
  end;
end;

function LoadText(const AName: String): TNyxText;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(AName, fmOpenRead or fmShareDenyWrite);
  try
    Check(LStream.Size < 1048576, 'expected design remains a bounded source-fixture packet');
    SetLength(LBytes, LStream.Size);

    if Length(LBytes) > 0 then
    begin
      LStream.ReadBuffer(LBytes[0], Length(LBytes));
    end;
    Result := NyxDecodeUTF8(LBytes);
  finally
    LStream.Free;
  end;
end;
{$endif}

procedure Run;
var
  LDocument: TNyxDocument;
  LDisabledDocument: TNyxDocument;
  LApp: TReviewApplication;
  LBinding: INyxDataToolbarBinding;
  LRows: INyxCollectionView;
  LSourceBefore: TNyxText;
  LSearchID: TNyxText;
  LOwnerID: TNyxText;
  {$ifdef PAS2JS}
  LHost: TJSHTMLElement;
  {$else}
  LWidth: Integer;
  {$endif}
begin
  LApp := nil;
  {$ifdef PAS2JS}LHost := nil;{$endif}
  LDocument := nyx.generated.toolbar.BuildNyxDocument;
  LDisabledDocument := nil;
  try
    LDocument.Validate;
    Check(GetNyxDataToolbarIntent(LDocument.Find('tools')).Options.BaselineChoice.Name =
      CBaseline, 'compiled typed builder preserves exact supplementary Unicode');
    LSourceBefore := TNyxCodec.Encode(LDocument);
    {$ifndef PAS2JS}

    if ParamCount >= 1 then
    begin
      Check(LoadText(IncludeTrailingPathDelimiter(ParamStr(1)) + 'toolbar-design.json') =
        LSourceBefore, 'compiled generated builder reconstructs its own producer design bytes');
    end;
    {$endif}
    LDisabledDocument := nyx.generated.toolbar.disabled.BuildNyxDocument;
    Check((LDisabledDocument.Collections.Count = 0) and
      (GetNyxDataToolbarIntent(LDisabledDocument.Find('static-tools')).Mode = ntiDisabled),
      'compiled static-source promotion needs no collection source');
    LApp := TReviewApplication.Create;
    {$ifdef PAS2JS}
    LHost := TJSHTMLElement(Web.document.createElement('main'));
    Web.document.body.appendChild(LHost);
    LApp.Run(LDocument, LHost);
    {$else}
    LApp.Mount(LDocument);
    LWidth := 1280;

    if ParamCount >= 2 then
    begin
      LWidth := StrToInt(ParamStr(2));
    end;
    LApp.Window.ClientWidth := LWidth;
    LApp.Window.ClientHeight := 800;
    LApp.Window.Show;
    Application.ProcessMessages;
    {$endif}
    LOwnerID := NyxQualifiedID('left', 'tools');
    Check(Supports(LApp.View.CoordinatorBinding(LOwnerID), INyxDataToolbarBinding, LBinding)
      and LBinding.Connected, 'compiled source activates the ordinary specialized toolbar binding');
    LRows := LApp.View.CollectionView(NyxQualifiedID('left', 'people-table'));
    LSearchID := LApp.View.Root.Find(LOwnerID).Part(NyxPart('search')).ID;
    LApp.View.Reveal(LSearchID, niRuntime);
    {$ifdef PAS2JS}
    TJSHTMLInputElement(LApp.View.InputFor(LSearchID)).value := 'A';
    LApp.View.InputFor(LSearchID).dispatchEvent(TJSEvent.new('input'));
    {$else}
    TCustomEdit(LApp.View.InputFor(LSearchID)).Text := 'A';
    Application.ProcessMessages;
    {$endif}
    Check(LRows.Snapshot.Count = 1, 'actual generated Search uses its typed matching policy');
    Check(LApp.View.CollectionView(NyxQualifiedID('right', 'people-table')).Snapshot.Count = 2,
      'generated named override preserves independent sibling data');
    LBinding.Clear;
    Check(LRows.Snapshot.Count = 2, 'generated specialized Clear restores the admitted query');
    LBinding.Sort(NyxToolbarSort('Name descending'));
    Check(LRows.Snapshot.ItemAt(0).GetValue(NyxTextField('name')) = 'Grace',
      'compiled multi-field ordering remains a typed runtime query');
    Check(TNyxCodec.Encode(LDocument) = LSourceBefore,
      'generated application commands leave all authored fields and defaults intact');
    LApp.ShowPage('disabled-page');
    Check(not LBinding.Connected, 'generated application navigation retires prior command authority');
    LOwnerID := NyxQualifiedID('disabled-instance', 'toolbar-template');
    Check(Supports(LApp.View.CoordinatorBinding(LOwnerID), INyxDataToolbarBinding, LBinding)
      and not LBinding.Connected, 'compiled reusable reference carries its explicit Disabled mask');
  finally
    LBinding := nil;
    LRows := nil;
    LApp.Free;
    {$ifdef PAS2JS}

    if LHost <> nil then
    begin
      LHost.remove;
    end;
    {$endif}
    LDisabledDocument.Free;
    LDocument.Free;
  end;
end;

begin
  try
    {$ifndef PAS2JS}
    Application.Initialize;
    GFailures := TFailures.Create;
    try
      Application.OnException := GFailures.Failed;
      Run;
      Check(GFailures.Message = '', 'native generated app has no unhandled callback failure');
    finally
      Application.OnException := nil;
      GFailures.Free;
    end;
    WriteLn('PASS ', GChecks, ' generated DataToolbar application checks');
    {$else}
    Run;
    Web.document.body.setAttribute('data-toolbar-generated-checks', IntToStr(GChecks));
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      Web.document.body.setAttribute('data-toolbar-generated-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.ClassName, ': ', LException.Message);
      DumpExceptionBackTrace(StdErr);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
