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
program nyx_studio_operator_generated;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}JS, Web,{$else}Interfaces, Classes, Forms, Controls, StdCtrls,
  nyx.application.lcl, nyx.studio.projects, nyx.bytes,{$endif}
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.callbacks, nyx.codec,
  nyx.button.presentation, nyx.button.view, nyx.generated.view;

{$ifndef PAS2JS}
type
  { Dispatch the actual application's public button input; the application
    retains the borrowed native control until the end of this synchronous test. }
  TControlAccess = class(TControl);
  { Application callbacks cannot open a blocking widgetset error dialog. This
    independent receiver outlives the mounted app and retains copied text only. }
  TFailureObserver = class
  public
    Error: TNyxText;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
{$endif}

var
  GChecks: Integer;
  {$ifndef PAS2JS}GFailure: TFailureObserver;{$endif}

{$ifndef PAS2JS}
procedure TFailureObserver.Failed(ASender: TObject; AException: Exception);
begin
  Error := TNyxText(AException.Message);
end;
{$endif}

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

{$ifndef PAS2JS}
{ File boundaries carry bytes. The packet is this journey's own accepted export;
  nothing reads or replaces an observing editor's project or machine settings. }
function ReadPacket(const APath: TNyxText): TNyxProjectPair;
var
  LStream: TFileStream;
  LBytes: TNyxBytes;
begin
  LStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
  try
    Check((LStream.Size > 0) and (LStream.Size <= High(Integer)),
      'own operator packet fits the native byte buffer');
    SetLength(LBytes, Integer(LStream.Size));
    LStream.ReadBuffer(LBytes[0], Length(LBytes));
  finally
    LStream.Free;
  end;
  Result := DecodeNyxProject(NyxDecodeUTF8(LBytes));
end;
{$endif}

procedure Run;
var
  LDocument: TNyxDocument;
  LInfos: TNyxAuthoredEventInfos;
  LIndex: Integer;
  LClick: Boolean;
  {$ifndef PAS2JS}
  LPair: TNyxProjectPair;
  LApplication: TNyxLCLApplication;
  LButton: TControl;
  {$endif}
begin
  LDocument := BuildNyxDocument;
  {$ifndef PAS2JS}LApplication := nil;{$endif}
  try
    Check(LDocument.Title = 'A little workspace', 'compiled constructor retains the operator project');
    Check(LDocument.Find('project-button').Prop('text') = 'Launch the preview',
      'compiled application retains the Properties caption');
    Check(NyxButtonPresentationOf(LDocument.Find('project-button')).ControlDensity = nbdTouch,
      'compiled application retains the authored typed button presentation');
    LInfos := NyxAuthoredEvents(LDocument.Find('project-button'));
    LClick := False;
    for LIndex := 0 to High(LInfos) do
    begin

      if LInfos[LIndex].Trigger = ntClick then
      begin
        Check((Length(LInfos[LIndex].Callbacks) = 1) and
          (LInfos[LIndex].Callbacks[0].Handler.Name <> ''),
          'compiled constructor retains the single named Click callback');
        LClick := True;
      end;
    end;
    Check(LClick, 'the compiled document exposes its Click registration');
    {$ifndef PAS2JS}
    Check(ParamCount = 2, 'supply the own accepted packet and client width');
    LPair := ReadPacket(ParamStr(1));
    Check(not LPair.Pending and (TNyxCodec.Encode(LDocument) = LPair.Design),
      'compiled constructor exactly reproduces its own accepted design');
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := StrToInt(ParamStr(2));
    LApplication.Window.ClientHeight := 760;
    LApplication.Window.Show;
    Application.ProcessMessages;
    LButton := LApplication.View.ControlFor('project-button');
    Check((LButton <> nil) and LButton.IsVisible and LButton.Enabled,
      'own compiled application mounts the ordinary visible button');
    Check(TNyxText(TControlAccess(LButton).Caption) = 'Launch the preview',
      'the mounted application presents the exact authored caption');
    TControlAccess(LButton).Click;
    Application.ProcessMessages;
    Check(GFailure.Error = '', 'compiled application input reports no callback failure');
    Check(TNyxCodec.Encode(LDocument) = LPair.Design,
      'clicking the compiled callback-bearing button preserves the authored document');
    {$endif}
  finally
    {$ifndef PAS2JS}LApplication.Free;{$endif}
    LDocument.Free;
  end;
end;

begin
  {$ifndef PAS2JS}
  Application.Initialize;
  GFailure := TFailureObserver.Create;
  Application.OnException := GFailure.Failed;
  {$endif}
  try
    Run;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-operator-generated', IntToStr(GChecks));
    {$else}
    WriteLn('PASS ', GChecks, ' own compiled operator application checks at ', ParamStr(2));
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-event-error', LException.Message);
      {$else}
      WriteLn('FAIL ', LException.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
      {$endif}
    end;
  end;
  {$ifndef PAS2JS}
  Application.OnException := nil;
  GFailure.Free;
  {$endif}
end.
