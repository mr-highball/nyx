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

program nyx_file_runtime_studio_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Classes, SysUtils, nyx.text, nyx.model, nyx.studio.projects, nyx.studio.menu,
  nyx.studio.outputs
  {$ifdef PAS2JS}, JS, Web, nyx.studio.browser
  {$else}, Interfaces, Forms, Controls, StdCtrls, nyx.studio.lcl{$endif};

{$ifndef PAS2JS}
type
  { Exposes the inherited click only for owned actual controls. It owns no
    control; the renderer retains all widget lifetime and event dispatch. }
  TControlInput = class(TControl);
  { Captures widget callback failures so a failed fixture cannot open a blocking
    Lazarus exception dialog. The owner outlives every test widget callback. }
  TCallbackFailure = class
  public
    Error: String;
    procedure Failed(ASender: TObject; AException: Exception);
  end;
{$endif}

var
  GStudio: {$ifdef PAS2JS}TNyxStudio{$else}TNyxNativeStudio{$endif};
  GBefore: TNyxText;
  GStage: Integer;
  GChecks: Integer;
  GStarted: Double;
  GStepStarted: Double;
  GDone: Boolean;
  {$ifndef PAS2JS}
  GForm: TForm;
  GFailure: TCallbackFailure;
  {$endif}

{$ifndef PAS2JS}
procedure TCallbackFailure.Failed(ASender: TObject; AException: Exception);
begin
  Error := AException.ClassName + ': ' + AException.Message;
end;
{$endif}

procedure Check(ACondition: Boolean; const AMessage: String);
begin

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
  Inc(GChecks);
end;

procedure CheckProfile;
const
  CInvalid: array[0..8] of TNyxText = (
    '{"version":2,"fields":{},"fileSaving":"true"}',
    '{"version":2,"fields":{}}',
    '{"version":1,"fields":{},"fileSaving":true}',
    '{"version":3,"fields":{},"fileSaving":false,"fileOpening":"true"}',
    '{"version":3,"fields":{},"fileSaving":"false","fileOpening":true}',
    '{"version":3,"fields":{},"fileOpening":true}',
    '{"version":2,"fields":{},"fileSaving":false,"fileOpening":true}',
    '{"version":1,"fields":{},"fileOpening":true}',
    '{"version":3,"fields":{},"fileSaving":false,"unexpected":true}');
var
  LOptions: TNyxOutputConfiguration;
  LDecoded: TNyxOutputConfiguration;
  LLegacy: TNyxText;
  LSaveOnly: TNyxText;
  LRefused: Boolean;
  LIndex: Integer;
begin
  { The identical private profile boundary executes in native Pascal and pas2js;
    no compiler path is resolved and no host authority or file I/O is created. }
  LOptions := TNyxOutputConfiguration.Create;
  try
    LOptions.SetField('fpc', 'C:/compiler/moon-🌙/fpc.exe');
    LLegacy := LOptions.Encode;
    Check(not LOptions.FileSaving, 'legacy runtime profiles grant no file host');
    Check(not LOptions.FileOpening, 'legacy profiles grant no independent read host');
    LOptions.AllowFileSaving;
    LSaveOnly := LOptions.Encode;
    LDecoded := TNyxOutputConfiguration.Decode(LOptions.Encode);
    try
      Check(LDecoded.FileSaving, 'strict Boolean runtime opt-in round trips on this target');
      Check(LDecoded.Field('fpc') = LOptions.Field('fpc'), 'profile retains exact Unicode paths');
    finally
      LDecoded.Free;
    end;
    LOptions.AllowFileOpening;
    LDecoded := TNyxOutputConfiguration.Decode(LOptions.Encode);
    try
      Check(LDecoded.FileSaving and LDecoded.FileOpening,
        'both explicit permissions survive the typed version-3 boundary');
    finally
      LDecoded.Free;
    end;
    LOptions.AllowFileOpening(False);
    Check(LOptions.Encode = LSaveOnly, 'revoking opening restores exact legacy save-only bytes');
    LOptions.AllowFileSaving(False);
    Check(LOptions.Encode = LLegacy, 'revocation restores exact legacy profile bytes');
    LOptions.AllowFileOpening;
    LDecoded := TNyxOutputConfiguration.Decode(LOptions.Encode);
    try
      Check(LDecoded.FileOpening and not LDecoded.FileSaving,
        'opening does not silently grant save authority');
    finally
      LDecoded.Free;
    end;
    LOptions.AllowFileOpening(False);
    Check(LOptions.Encode = LLegacy, 'revoking read-only authority restores exact legacy bytes');
    for LIndex := 0 to High(CInvalid) do
    begin
      LRefused := False;
      LDecoded := nil;
      try
        LDecoded := TNyxOutputConfiguration.Decode(CInvalid[LIndex]);
      except
        on ENyxModel do
        begin
          LRefused := True;
        end;
      end;
      LDecoded.Free;
      Check(LRefused, 'malformed or legacy-foreign runtime authority refuses');
    end;
  finally
    LOptions.Free;
  end;
end;

function Clock: Double;
begin
  {$ifdef PAS2JS}Result := TJSDate.now;{$else}Result := GetTickCount64;{$endif}
end;

procedure Click(const AID: TNyxText);
{$ifdef PAS2JS}
var
  LElement: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LElement := TJSHTMLElement(document.querySelector('[data-node="' + AID + '"]'));

  if LElement = nil then
  begin
    raise Exception.Create('Missing runtime configuration input: ' + String(AID));
  end;
  LElement.click;
  {$else}
  TControlInput(GStudio.ShellView.ControlFor(AID)).Click;
  {$endif}
end;

procedure ActivateOutputs;
{$ifdef PAS2JS}
var
  LFace: TJSHTMLElement;
{$endif}
begin
  {$ifdef PAS2JS}
  LFace := TJSHTMLElement(document.querySelector('[data-node="action-outputs"]'));

  if (LFace = nil) or (LFace.getBoundingClientRect.height = 0) then
  begin
    { Compact hidden faces deliberately refuse input. Use the actual public
      managed menu an operator uses, through its closed Outputs action. }
    Click(NyxStudioActionMenuID);
    Click('studio-menu-project');
    Click('studio-menu-outputs');
    Exit;
  end;
  {$endif}
  Click(NyxStudioMenuActionTarget(smaOutputs));
end;

procedure Toggle(const AID: TNyxText = 'output-file-saving');
begin
  {$ifdef PAS2JS}
  TJSHTMLElement(document.querySelector('[data-node="' + AID + '"] input')).click;
  {$else}
  TCheckBox(GStudio.ShellView.InputFor(AID)).Checked :=
    not TCheckBox(GStudio.ShellView.InputFor(AID)).Checked;
  {$endif}
end;

function OutputNode(const AID: TNyxText): TNyxNode;
var
  LRoot: TNyxNode;
begin
  { Compact Studio mounts details as an independent owned section. Resolve the
    public section root each turn instead of assuming it belongs to chrome. }
  LRoot := GStudio.ShellView.RootFor(AID);

  if LRoot = nil then
  begin
    raise Exception.Create('Output section is not mounted: ' + String(AID));
  end;
  Result := LRoot.Find(AID);

  if Result = nil then
  begin
    raise Exception.Create('Output control is not mounted: ' + String(AID));
  end;
end;

procedure Step;
begin
  {$ifndef PAS2JS}

  if GFailure.Error <> '' then
  begin
    raise Exception.Create(GFailure.Error);
  end;
  {$endif}

  if (Clock - GStarted > 60000) or (Clock - GStepStarted > 15000) then
  begin
    raise Exception.Create('Runtime output configuration input deadline at stage ' +
      IntToStr(GStage) + ', checks ' + IntToStr(GChecks));
  end;
  if GStudio.PresentationPending then
  begin
    Exit;
  end;
  {$ifndef PAS2JS}
  WriteLn('Runtime configuration stage ', GStage);
  Flush(Output);
  {$endif}
  case GStage of
    0:
      begin
        ActivateOutputs;
      end;
    1:
      begin
        Check(OutputNode('output-file-saving').Prop('value') = 'false',
          'runtime save authority starts disabled before choosing a target');
        Check(OutputNode('output-file-opening').Prop('value') = 'false',
          'runtime opening authority also starts disabled without choosing a target');
        Toggle;
      end;
    2:
      begin
        Check(OutputNode('output-file-saving').Prop('value') = 'true',
          'actual target checkbox edits the typed runtime configuration');
        Toggle('output-file-opening');
        ActivateOutputs;
      end;
    3:
      begin
        ActivateOutputs;
      end;
    4:
      begin
        Check(OutputNode('output-file-saving').Prop('value') = 'true',
          'runtime policy survives hiding and rebuilding Outputs');
        Check(OutputNode('output-file-opening').Prop('value') = 'true',
          'independent opening policy survives ordinary Outputs reconstruction');
        Toggle;
      end;
    5:
      begin
        Check(OutputNode('output-file-saving').Prop('value') = 'false',
          'actual target input can explicitly revoke future build authority');
        Check(OutputNode('output-file-opening').Prop('value') = 'true',
          'revoking saving leaves the read-only policy enabled');
        Toggle('output-file-opening');
      end;
    6:
      begin
        Check(OutputNode('output-file-opening').Prop('value') = 'false',
          'actual read checkbox can independently revoke future opening authority');
        Check(EncodeNyxProject(GStudio.ProjectSnapshot) = GBefore,
          'configuration retains the exact accepted source/design/draft pair');
        Check(OutputNode('output-none') <> nil,
          'runtime configuration never requires selecting a target');
        GDone := True;
      end;
  end;
  Inc(GStage);
  GStepStarted := Clock;
end;

{$ifdef PAS2JS}
procedure Drive;
var
  LFace: TJSHTMLElement;
  LOptions: TJSObject;
begin
  try
    Step;

    if GDone then
    begin
      { Reveal the changed adjacent policies for the selective visual receipt.
        End alignment retains the saving checkbox above the opening checkbox;
        this changes scroll only, after all ordinary configuration assertions. }
      LFace := TJSHTMLElement(document.querySelector('[data-node="output-file-opening"]'));

      if LFace <> nil then
      begin
        LOptions := TJSObject.new;
        LOptions['block'] := 'end';
        LFace.scrollIntoView(LOptions);
      end;
      document.body.setAttribute('data-file-runtime-studio', 'passed');
      document.body.setAttribute('data-file-runtime-checks', IntToStr(GChecks));
      Exit;
    end;
    window.setTimeout(@Drive, 20);
  except
    on E: Exception do
    begin
      document.body.setAttribute('data-file-runtime-studio', 'failed');
      document.body.setAttribute('data-file-runtime-error', E.Message);
    end;
  end;
end;
{$endif}

begin
  CheckProfile;
  {$ifdef PAS2JS}
  document.body.setAttribute('data-file-runtime-profile-checks', IntToStr(GChecks));
  {$else}
  WriteLn('PASS ', GChecks, ' native runtime profile checks');

  if (ParamCount = 1) and (ParamStr(1) = '--profile') then
  begin
    Exit;
  end;
  {$endif}
  GChecks := 0;
  {$ifndef PAS2JS}

  if (ParamCount <> 1) or DirectoryExists(ParamStr(1)) then
  begin
    raise Exception.Create('Supply a fresh owned Studio project directory');
  end;
  Application.Initialize;
  GFailure := TCallbackFailure.Create;
  Application.OnException := GFailure.Failed;
  GForm := TForm.CreateNew(nil);
  GForm.SetBounds(20, 20, 1280, 900);
  GForm.Show;
  GStudio := TNyxNativeStudio.Create(GForm, ParamStr(1));
  GStudio.Run;
  {$else}
  GStudio := TNyxStudio.Create;
  GStudio.Run(False);
  {$endif}
  GBefore := EncodeNyxProject(GStudio.ProjectSnapshot);
  GStarted := Clock;
  GStepStarted := GStarted;
  {$ifdef PAS2JS}
  Drive;
  {$else}
  try
    while not GDone do
    begin
      CheckSynchronize;
      Application.ProcessMessages;
      Step;
      Sleep(20);
    end;
    WriteLn('PASS ', GChecks, ' actual native Studio runtime configuration checks');
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      ExitCode := 1;
    end;
  end;
  GStudio.Free;
  GForm.Free;
  Application.OnException := nil;
  GFailure.Free;
  {$endif}
end.
