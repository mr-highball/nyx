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
unit nyx.view.recovery;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.controls;

type
  { Display readiness is independent of document admission and compilation.
    Values own their diagnostic text, never a view, model, widget or callback.
    Zero/default is ready. A host retains its own session/revision authority. }
  TNyxViewRecoveryPhase = (nvrReady, nvrRetrying, nvrFailed);
  TNyxViewRecovery = record
  private
    FPhase: TNyxViewRecoveryPhase;
    FDiagnostic: TNyxText;
  public
    class function Ready: TNyxViewRecovery; static;
    class function Failed(const ADiagnostic: TNyxText): TNyxViewRecovery; static;
    class function Retrying(const ADiagnostic: TNyxText): TNyxViewRecovery; static;
    { A stale/refreshing display must not submit gestures against a newer model.
      This observation does not disable document Undo, editing or compilation. }
    function BlocksInput: Boolean;
    property Phase: TNyxViewRecoveryPhase read FPhase;
    property Diagnostic: TNyxText read FDiagnostic;
  end;

{ Managed compound with named title/diagnostic/retry parts. Returned ownership
  follows ordinary INyxPanel/descendant contracts. Its stable hidden controls let
  a host surface a refusal without attempting the same failing full composition.
  It requests a retry; the owning application decides how and what to refresh. }
function NewNyxViewRecovery(const AID: TNyxText;
  const AState: TNyxViewRecovery): INyxPanel;
{ Derives a control identity, not a command or revision authority. }
function NyxViewRecoveryRetryID(const AID: TNyxText): TNyxText;
{ Only the compound's retained retry part qualifies, including creator wrappers.
  The host must also validate
  its current owner/context and the event trigger before executing a retry. }
function NyxViewRecoveryAction(ANode: TNyxNode; const AID: TNyxText): Boolean;
{ Validate every fixed part before updating the current compound. Missing or
  retyped parts refuse before mutation; arbitrary extensions remain caller-owned.
  Target synchronization is explicit and can itself report a display refusal. }
procedure RestoreNyxViewRecovery(ARoot: TNyxNode; const AState: TNyxViewRecovery);

implementation

class function TNyxViewRecovery.Ready: TNyxViewRecovery;
begin
  Result := Default(TNyxViewRecovery);
end;

class function TNyxViewRecovery.Failed(const ADiagnostic: TNyxText): TNyxViewRecovery;
begin
  Result := Default(TNyxViewRecovery);
  Result.FPhase := nvrFailed;
  Result.FDiagnostic := ADiagnostic;
end;

class function TNyxViewRecovery.Retrying(const ADiagnostic: TNyxText): TNyxViewRecovery;
begin
  Result := Failed(ADiagnostic);
  Result.FPhase := nvrRetrying;
end;

function TNyxViewRecovery.BlocksInput: Boolean;
begin
  Result := FPhase <> nvrReady;
end;

function NyxViewRecoveryRetryID(const AID: TNyxText): TNyxText;
begin
  Result := AID + TNyxText('-retry');
end;

function NewNyxViewRecovery(const AID: TNyxText;
  const AState: TNyxViewRecovery): INyxPanel;
var
  LTitle: INyxLabel;
  LDiagnostic: INyxLabel;
  LRetry: INyxButton;
begin
  Result := NewNyxPanel(AID);
  Result.Configure.Layout(nlColumn).Padding(12).Gap(8).Surface(True).Compound(True).Done;
  LTitle := NewNyxLabel(AID + TNyxText('-title'));
  LTitle.Configure.PartName(NyxPart('title')).Done;
  Result.Add(LTitle);
  LDiagnostic := NewNyxLabel(AID + TNyxText('-diagnostic'));
  LDiagnostic.Configure.PartName(NyxPart('diagnostic')).Done;
  Result.Add(LDiagnostic);
  LRetry := NewNyxButton(NyxViewRecoveryRetryID(AID));
  LRetry.Configure.Text('Retry display').PartName(NyxPart('retry'))
    .Hint('Refresh the accepted design without creating a document history step').Done;
  Result.Add(LRetry);
  RestoreNyxViewRecovery(Result.Node, AState);
end;

function NyxViewRecoveryAction(ANode: TNyxNode; const AID: TNyxText): Boolean;
var
  LParent: TNyxNode;
begin
  Result := False;

  if (ANode = nil) or (ANode.ID <> NyxViewRecoveryRetryID(AID)) then
  begin
    Exit;
  end;
  LParent := ANode.Parent;
  while LParent <> nil do
  begin

    if LParent.ID = AID then
    begin
      Exit(LParent.Find(ANode.ID) = ANode);
    end;
    LParent := LParent.Parent;
  end;
end;

procedure RestoreNyxViewRecovery(ARoot: TNyxNode; const AState: TNyxViewRecovery);
var
  LTitle: TNyxNode;
  LDiagnostic: TNyxNode;
  LRetry: TNyxNode;
  LCaption: TNyxText;
begin

  if ARoot = nil then
  begin
    raise ENyxModel.Create('View recovery requires its owning compound');
  end;
  LTitle := ARoot.Find(ARoot.ID + TNyxText('-title'));
  LDiagnostic := ARoot.Find(ARoot.ID + TNyxText('-diagnostic'));
  LRetry := ARoot.Find(NyxViewRecoveryRetryID(ARoot.ID));

  if (LTitle = nil) or (LDiagnostic = nil) or (LRetry = nil) or
    (LTitle.Kind <> NyxKindName(nkLabel)) or
    (LDiagnostic.Kind <> NyxKindName(nkLabel)) or
    (LRetry.Kind <> NyxKindName(nkButton)) then
  begin
    raise ENyxModel.Create('View recovery parts no longer match their compound');
  end;
  LCaption := 'Display needs attention. Accepted content is retained.';

  if AState.Phase = nvrRetrying then
  begin
    LCaption := 'Refreshing the accepted display...';
  end;
  ARoot.Configure.Visible(AState.Phase <> nvrReady).Done;
  LTitle.Configure.Text(LCaption).Done;
  LDiagnostic.Configure.Text(AState.Diagnostic).Visible(AState.Diagnostic <> '').Done;
  LRetry.Configure.Enabled(AState.Phase = nvrFailed).Done;
end;

end.
