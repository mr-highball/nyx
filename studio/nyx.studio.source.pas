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
unit nyx.studio.source;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.source,
  nyx.studio.session;

const
  { Shared chrome identity; selection is decoded to a typed file reference. }
  NyxStudioSourceFileID = 'studio-source-file';
  NyxStudioDiagnosticID = 'studio-source-diagnostic';
  NyxStudioDiagnosticGoID = 'action-source-diagnostic';
  NyxStudioDiagnosticSiteKey = 'nyx.studio.sourceDiagnosticSite';

{ Returns an owned public Nyx panel, or nil when the current draft has no error.
  The session is borrowed. Diagnostics are editor presentation, never design data;
  an unknown admission site is readable without inventing a navigation position. }
function BuildNyxSourceDiagnostic(ASession: TNyxStudioSession): TNyxNode;

{ Resolve the explicit navigation action against the current exact draft. Stale
  shell buttons reject instead of navigating a later buffer. The returned owned
  record borrows no exception, source node, document or target widget. }
function RouteNyxSourceDiagnostic(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; out ADiagnostic: TNyxSourceDiagnostic): Boolean;

implementation

uses
  SysUtils, nyx.data;

{ Copied presentation identity binds a rendered action to the current exact
  diagnostic. Buffer equality remains the session's guard; this value grants no
  source editing, history or execution authority. }
function DiagnosticSite(ASession: TNyxStudioSession;
  const ADiagnostic: TNyxSourceDiagnostic): TNyxDataValue;
begin
  Result := NyxObject([
    NyxField('role', NyxData(Ord(ASession.SourceDiagnosticFile.Role))),
    NyxField('file', NyxData(ASession.SourceDiagnosticFile.Name)),
    NyxField('line', NyxData(ADiagnostic.Line)),
    NyxField('column', NyxData(ADiagnostic.Column)),
    NyxField('message', NyxData(ADiagnostic.Message))]);
end;

function BuildNyxSourceDiagnostic(ASession: TNyxStudioSession): TNyxNode;
var
  LDiagnostic: TNyxSourceDiagnostic;
  LAction: TNyxNode;
begin
  Result := nil;
  LDiagnostic := ASession.SourceDiagnostic;

  if not LDiagnostic.Defined then
  begin
    Exit;
  end;
  Result := TNyxNode.Create(nkPanel, NyxStudioDiagnosticID);
  try
    Result.Configure.Layout(nlColumn).Gap(8).Padding(12).Surface(True).Done;
    Result.Add(TNyxNode.Create(nkLabel, 'studio-source-diagnostic-message')
      .Configure.Text(LDiagnostic.Message).Done);
    Result.Add(TNyxNode.Create(nkLabel, 'studio-source-diagnostic-file')
      .Configure.Text(ASession.SourceFileTitle(ASession.SourceDiagnosticFile)).Done);

    if (LDiagnostic.Line > 0) and (LDiagnostic.Column > 0) then
    begin
      LAction := TNyxNode.Create(nkButton, NyxStudioDiagnosticGoID);
      Result.Add(LAction);
      LAction.Configure.Text('Go to ' + IntToStr(LDiagnostic.Line) + ':' +
          IntToStr(LDiagnostic.Column)).Hint('Focus the error in its retained source file').Done;
      LAction.Extensions.SetValue(NyxExtension(NyxStudioDiagnosticSiteKey),
        DiagnosticSite(ASession, LDiagnostic));
    end;
  except
    Result.Free;
    raise;
  end;
end;

function RouteNyxSourceDiagnostic(ASession: TNyxStudioSession; ANode: TNyxNode;
  ATrigger: TNyxTrigger; out ADiagnostic: TNyxSourceDiagnostic): Boolean;
begin
  ADiagnostic := Default(TNyxSourceDiagnostic);
  Result := (ATrigger = ntClick) and (ANode.ID = NyxStudioDiagnosticGoID);

  if not Result then
  begin
    Exit;
  end;
  ADiagnostic := ASession.SourceDiagnostic;

  if not ADiagnostic.Defined or (ADiagnostic.Line < 1) or (ADiagnostic.Column < 1) then
  begin
    raise ENyxModel.Create('This source diagnostic no longer belongs to the current draft');
  end;

  if not ANode.Extensions.Has(NyxExtension(NyxStudioDiagnosticSiteKey)) or
    (ANode.Extensions.Value(NyxExtension(NyxStudioDiagnosticSiteKey)).ToJSON <>
      DiagnosticSite(ASession, ADiagnostic).ToJSON) then
  begin
    raise ENyxModel.Create('This rendered source location belongs to an earlier diagnostic');
  end;
  { The session retains the exact diagnostic buffer independently of selection;
    navigation switches presentation only and never replaces unfinished text. }
  ASession.ActiveSourceFile := ASession.SourceDiagnosticFile;
end;

end.
