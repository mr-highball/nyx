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

unit nyx.test.source.compilation;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.sourcecompilation;

type
  { Maintained asynchronous ordinary source-command journey. The supplied host
    must actually compile/execute the exact fixture source, not synthesize a
    successful result. Pump calls observe the live command; no fixed sleep is
    accepted as completion. The journey owns its independent editor/session. }
  INyxSourceCompilationJourney = interface(IInterface)
    ['{6C080403-81B5-4E91-B222-101026100006}']
    procedure Pump;
    function GetDone: Boolean;
    function GetChecks: Integer;
    property Done: Boolean read GetDone;
    property Checks: Integer read GetChecks;
  end;

function StartNyxSourceCompilationJourney(const ACompiler: INyxSourceCompiler;
  const ASource, AFailureSource, AExpected: TNyxText): INyxSourceCompilationJourney;

implementation

uses SysUtils, nyx.types, nyx.model, nyx.codec, nyx.studio.session,
  nyx.studio.sourcejobs, nyx.studio.commands;

type
  TJourney = class(TInterfacedObject, INyxSourceCompilationJourney)
  private
    FSession: TNyxStudioSession;
    FCommands: TNyxSourceCommands;
    FCompiler: INyxSourceCompiler;
    FSource: TNyxText;
    FFailureSource: TNyxText;
    FExpected: TNyxText;
    FBeforeSource: TNyxText;
    FBeforeDesign: TNyxText;
    FPhase: Integer;
    FChecks: Integer;
    FEditor: TNyxNode;
    FApply: TNyxNode;
    procedure Check(ACondition: Boolean; const AReason: TNyxText);
    procedure Apply(const ASource: TNyxText);
  public
    destructor Destroy; override;
    procedure Pump;
    function GetDone: Boolean;
    function GetChecks: Integer;
  end;

destructor TJourney.Destroy;
begin
  FCommands.Free;
  FSession.Free;
  FEditor.Free;
  FApply.Free;
  inherited Destroy;
end;

procedure TJourney.Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Ordinary compiled Apply: ' + AReason);
  end;
  Inc(FChecks);
end;

procedure TJourney.Apply(const ASource: TNyxText);
begin
  FEditor.Configure.Value(ASource);
  Check(RouteNyxStudioSource(FSession, FEditor, ntChange),
    'ordinary source field captures the exact draft');
  Check(FSession.DraftSource = ASource, 'draft is captured before compilation');
  Check(FCommands.Route(FApply, ntClick), 'ordinary Apply routes through the source controller');
end;

procedure TJourney.Pump;
var
  LRefused: Boolean;
  LDetachedSession: TNyxStudioSession;
  LDetachedCommands: TNyxSourceCommands;
begin

  if GetDone or FCommands.Busy then
  begin
    Exit;
  end;
  case FPhase of
    1:
      begin
        Check(FCommands.State = nssApplied, 'compiled constructor reaches ordinary command publication');
        Check((FSession.Source = FSource) and (FSession.Save = FExpected) and
          not FSession.SourceDraftPending, 'exact whole Pascal/design and consumed draft');
        Check((FSession.ActiveViewID = 'notebook-1') and
          (FSession.SelectedID = FSession.ActiveViewID),
          'replacement roots select a real view and scoped selection');
        Check(FSession.CanUndo and not FSession.CanRedo, 'one paired Apply command');
        FSession.Undo;
        Check((FSession.Source = FBeforeSource) and (FSession.Save = FBeforeDesign),
          'ordinary Undo restores the original complete pair');
        Check(not FSession.CanUndo and FSession.CanRedo, 'one ordinary Undo entry');
        FSession.Redo;
        Check((FSession.Source = FSource) and (FSession.Save = FExpected),
          'ordinary Redo restores complete helper construction');
        Apply(FFailureSource);
        LRefused := False;
        try
          FCommands.UseCompiler(nil);
        except
          on LException: Exception do
          begin
            LRefused := True;
          end;
        end;
        Check(LRefused, 'live compilation cannot change execution strategy');
        FPhase := 2;
      end;
    2:
      begin
        Check(FCommands.State = nssRejected, 'actual throwing constructor rejects in the editor');
        Check((FSession.Source = FSource) and (FSession.Save = FExpected) and
          (FSession.DraftSource = FFailureSource) and FSession.SourceDiagnostic.Defined,
          'execution failure retains exact accepted pair and unfinished source');
        Apply(FFailureSource);
        FSession.SetSourceDraft(FSource + #10);
        FPhase := 3;
      end;
    3:
      begin
        Check(FCommands.State = nssStale, 'newer draft supersedes an actual compiler completion');
        Check((FSession.Source = FSource) and (FSession.Save = FExpected) and
          (FSession.DraftSource = FSource + #10), 'stale compilation preserves the newer draft');
        Apply(FFailureSource);
        FCommands.Cancel;
        Check(FCommands.State = nssCancelled, 'ordinary cancellation is visible');
        Check(not FCommands.Busy and (FSession.Source = FSource) and
          (FSession.Save = FExpected) and (FSession.DraftSource = FFailureSource),
          'cancellation preserves the whole accepted pair and current draft');
        { Retirement exercises a real separately started compiler/worker. The
          native controller drains its operation; browser retirement terminates
          its worker. Both revoke delivery before freeing the accepted session. }
        LDetachedSession := TNyxStudioSession.Create;
        LDetachedCommands := nil;
        try
          LDetachedCommands := TNyxSourceCommands.Create(LDetachedSession, nil);
          LDetachedCommands.UseCompiler(FCompiler);
          LDetachedSession.SetSourceDraft(FSource);
          LDetachedCommands.Apply;
          Check(LDetachedCommands.Busy, 'independent editor has a live compiler command');
          LDetachedCommands.Detach;
          FreeAndNil(LDetachedCommands);
          Check(not LDetachedSession.CanUndo and LDetachedSession.SourceDraftPending,
            'retired producer cannot publish into its former editor');
        finally
          LDetachedCommands.Free;
          LDetachedSession.Free;
        end;
        FCommands.UseCompiler(nil);
        Apply(FFailureSource);
        FPhase := 4;
      end;
    4:
      begin
        Check(FCommands.State = nssRejected, 'literal authoring remains available without a compiler host');
        Check((FSession.Source = FSource) and (FSession.Save = FExpected),
          'literal refusal cannot forge execution or overwrite the accepted unit');
        FPhase := 5;
      end;
  end;
end;

function TJourney.GetDone: Boolean;
begin
  Result := FPhase = 5;
end;

function TJourney.GetChecks: Integer;
begin
  Result := FChecks;
end;

function StartNyxSourceCompilationJourney(const ACompiler: INyxSourceCompiler;
  const ASource, AFailureSource, AExpected: TNyxText): INyxSourceCompilationJourney;
var
  LOwner: TJourney;
begin
  LOwner := TJourney.Create;
  Result := LOwner;
  LOwner.FCompiler := ACompiler;
  LOwner.FSource := ASource;
  LOwner.FFailureSource := AFailureSource;
  LOwner.FExpected := AExpected;
  LOwner.FSession := TNyxStudioSession.Create;
  LOwner.FBeforeSource := LOwner.FSession.Source;
  LOwner.FBeforeDesign := LOwner.FSession.Save;
  LOwner.FCommands := TNyxSourceCommands.Create(LOwner.FSession, nil);
  LOwner.FCommands.UseCompiler(ACompiler);
  LOwner.FEditor := TNyxNode.Create(nkCodeEditor, 'studio-code');
  LOwner.FApply := TNyxNode.Create(nkButton, 'action-apply-source');
  LOwner.Apply(ASource);
  LOwner.Check(LOwner.FCommands.Busy, 'complete Pascal is dispatched as a live asynchronous command');
  LOwner.Check((LOwner.FSession.Source = LOwner.FBeforeSource) and
    (LOwner.FSession.Save = LOwner.FBeforeDesign), 'current pair stays exact during compilation');
  LOwner.FPhase := 1;
end;

end.
