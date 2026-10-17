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

unit nyx.studio.diagnostics;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.source,
  nyx.studio.compiler,
  nyx.studio.session;

const
  NyxStudioCompilerDiagnosticsID = 'studio-compiler-diagnostics';
  NyxStudioCompilerActionPrefix = 'action-compiler-diagnostic-';
  NyxStudioCompilerIndexKey = 'nyx.studio.compilerDiagnosticIndex';

{ Returns an owned ordinary Nyx composition or nil for an empty report. Reports
  and sessions are borrowed while composing; nodes retain typed index values,
  never interfaces/session pointers. Stale source/drafts visibly disable location
  actions. Complete source identity is checked again when an event is routed. }
function BuildNyxCompilerDiagnostics(ASession: TNyxStudioSession;
  const AReport: INyxCompilerReport): TNyxNode;
{ Route a current result's index to the source editor's scalar coordinates.
  Raises for stale, unmapped or invalid actions, leaving pair/draft/history intact.
  Native and browser controllers consume the same owned diagnostic value. }
function RouteNyxCompilerDiagnostic(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; const AReport: INyxCompilerReport;
  out ADiagnostic: TNyxSourceDiagnostic): Boolean;

implementation

uses
  SysUtils,
  nyx.data, nyx.pascal.drafts;

{ A compiler location is presentation, not source replacement authority. Resolve
  its typed member and compare the exact copied target text; unrelated drafts
  cannot redirect the site or unnecessarily disable a helper's action. }
function CurrentCompilerSite(ASession: TNyxStudioSession;
  const AReport: INyxCompilerReport; const AItem: TNyxCompilerDiagnostic): Boolean;
var
  LContext: INyxCompilerSourceReport;
  LIndex: Integer;
  LText: TNyxText;
begin
  Result := False;

  if (AReport = nil) or not AItem.Navigable or (AReport.Source <> ASession.Source) then
  begin
    Exit;
  end;
  LText := AReport.Source;

  if AItem.SourceFile.Role <> npsMain then
  begin

    if not Supports(AReport, INyxCompilerSourceReport, LContext) then
    begin
      Exit;
    end;
    LText := LContext.SourceText(AItem.SourceFile);
  end;
  for LIndex := 0 to ASession.SourceFileCount - 1 do
  begin

    if ASession.SourceFile(LIndex).SameFile(AItem.SourceFile) then
    begin
      Exit(ASession.DraftFileText(AItem.SourceFile) = LText);
    end;
  end;
end;

function BuildNyxCompilerDiagnostics(ASession: TNyxStudioSession;
  const AReport: INyxCompilerReport): TNyxNode;
var
  LIndex: Integer;
  LItem: TNyxCompilerDiagnostic;
  LMessage: TNyxText;
  LAccepted: TNyxText;
  LCurrent: Boolean;
  LAction: TNyxNode;
begin
  Result := nil;

  if (AReport = nil) or (AReport.Count = 0) then
  begin
    Exit;
  end;
  LAccepted := ASession.Source;
  LCurrent := (LAccepted = AReport.Source) and (ASession.DraftSource = LAccepted);
  Result := TNyxNode.Create(nkScroll, NyxStudioCompilerDiagnosticsID);
  try
    Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Surface(True).Height(180).Done;
    Result.Add(TNyxNode.Create(nkHeading, 'studio-compiler-title')
      .Configure.Text('Build diagnostics').Done);

    if not LCurrent then
    begin
      Result.Add(TNyxNode.Create(nkLabel, 'studio-compiler-stale')
        .Configure.Text('Locations are enabled only while their exact file text is unchanged.').Done);
    end;
    for LIndex := 0 to AReport.Count - 1 do
    begin
      LItem := AReport.Item(LIndex);
      LMessage := NyxCompilerSeverityName(LItem.Severity);

      if LItem.FileName <> '' then
      begin
        LMessage := LMessage + ' / ' + LItem.FileName + ':' + IntToStr(LItem.Line) +
          ':' + IntToStr(LItem.Column);
      end;
      LMessage := LMessage + ' / ' + LItem.Message;
      Result.Add(TNyxNode.Create(nkLabel, 'studio-compiler-message-' + IntToStr(LIndex))
        .Configure.Text(LMessage).Done);

      if LItem.Navigable then
      begin
        LAction := TNyxNode.Create(nkButton, NyxStudioCompilerActionPrefix + IntToStr(LIndex));
        Result.Add(LAction);
        LAction.Configure.Text('Go to ' + IntToStr(LItem.SourceLine) + ':' +
          IntToStr(LItem.SourceColumn)).Enabled(CurrentCompilerSite(ASession, AReport, LItem))
          .Hint('Open this location in the Pascal used for the build').Done;
        LAction.Extensions.SetValue(NyxExtension(NyxStudioCompilerIndexKey), NyxData(LIndex));
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function RouteNyxCompilerDiagnostic(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; const AReport: INyxCompilerReport;
  out ADiagnostic: TNyxSourceDiagnostic): Boolean;
var
  LIndex: Integer;
  LItem: TNyxCompilerDiagnostic;
begin
  ADiagnostic := Default(TNyxSourceDiagnostic);
  Result := (ATrigger = ntClick) and
    ANode.Extensions.Has(NyxExtension(NyxStudioCompilerIndexKey));

  if not Result then
  begin
    Exit;
  end;

  if AReport = nil then
  begin
    raise ENyxModel.Create('This compiler result is no longer available. Rebuild to navigate');
  end;
  LIndex := ANode.Extensions.Value(NyxExtension(NyxStudioCompilerIndexKey)).AsInteger;
  LItem := AReport.Item(LIndex);

  if not LItem.Navigable then
  begin
    raise ENyxModel.Create('This compiler location belongs to a different or generated source region');
  end;

  if not CurrentCompilerSite(ASession, AReport, LItem) then
  begin
    raise ENyxModel.Create('This source file changed after compilation. Restore its exact text or rebuild to navigate');
  end;
  ADiagnostic.Defined := True;
  ADiagnostic.Line := LItem.SourceLine;
  ADiagnostic.Column := LItem.SourceColumn;
  ASession.ActiveSourceFile := LItem.SourceFile;
  ADiagnostic.Message := NyxCompilerSeverityName(LItem.Severity) + ': ' + LItem.Message;
end;

end.
