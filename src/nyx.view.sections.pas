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
unit nyx.view.sections;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.model, nyx.state, nyx.behavior, nyx.publication;

type
  { A section name is open application data, distinct from a control ID.
    It owns Unicode text only; default/empty references are undefined. }
  TNyxViewSectionRef = record
  private
    FName: TNyxText;
  public
    class function Named(const AName: TNyxText): TNyxViewSectionRef; static;
    function Defined: Boolean;
    property Name: TNyxText read FName;
  end;

  { Closed publication phases distinguish reversible work from terminal handles. }
  TNyxViewSectionChangeState = (vcsPrepared, vcsPreviewing, vcsPublished, vcsCanceled);
  { Prepare returns the managed handle declared after the target phase contract. }
  INyxViewSectionChange = interface;

  { A managed observation receiver borrows the currently published node for the
    synchronous call. Do not retain that node or form a cycle back to a section.
    Queued work must use copied identities/revocable ports as with normal views. }
  INyxViewSectionObserver = interface(IInterface)
    ['{7A4CA253-980A-4D7F-AD6A-40204880C07B}']
    procedure Changed(ANode: TNyxNode; const AEvent: TNyxEventInfo);
  end;

  { A section owns one independently mounted Nyx view. Prepare owns a fresh
    target candidate and retains no caller document/root. The borrowed host
    must outlive the section AND outstanding change handles. A nonnil runtime
    store is caller-owned and must outlive both candidates and mounted views;
    nil uses independent authored defaults for that replacement.
    Prepare/Close refuse during their own synchronous input/model dispatch;
    queue those commands to another UI turn. Unknown collection extensions
    require an explicit publication-readiness capability.
    Revisions describe view publications, not design document Undo or MCP
    revisions. Root borrows the current realized tree; it is nil before mounting. }
  INyxViewSection = interface(IInterface)
    ['{970175AB-3907-46D4-BA38-2EACF34DB928}']
    function GetReference: TNyxViewSectionRef;
    function GetRevision: Integer;
    function GetRoot: TNyxNode;
    function Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
      ADesignMode: Boolean = False; AState: TNyxState = nil): INyxViewSectionChange;
    { Retire the current view and advance its revision. Outstanding proposals
      become stale; cancel/release them before destroying the borrowed host. }
    procedure Close;
    property Reference: TNyxViewSectionRef read GetReference;
    property Revision: Integer read GetRevision;
    property Root: TNyxNode read GetRoot;
  end;

  { Target extension boundary used by the coordinator, never default authoring.
    Check is side-effect-free. Preview may raise after partial physical work;
    Rollback must restore the exact old placement even in that case.
    Publish performs only prepared ownership/revision assignments and MUST NOT
    raise, allocate, invoke callbacks or touch target controls. RetirePrevious
    disconnects former owners after EVERY group member publishes. Destruction
    cancels an unpublished candidate, never an accepted view. All methods run
    synchronously on the owning UI thread; target extensions must not pump it. }
  INyxViewSectionPublication = interface(IInterface)
    ['{8785E710-FF3F-47CB-A0B1-EFD2E7E04A65}']
    function GetSection: INyxViewSection;
    function GetExpectedRevision: Integer;
    function Check: Boolean;
    procedure Preview;
    procedure Rollback;
    procedure Publish;
    procedure RetirePrevious;
    property Section: INyxViewSection read GetSection;
    property ExpectedRevision: Integer read GetExpectedRevision;
  end;

  { Optional explicit cancellation cleanup. Fallible target disconnects run
    while the publication is still retained, before its nonthrowing destructor
    releases storage. Retirement is terminal/idempotent and never owns the
    accepted view. Dropping a handle still cleans up, but cannot report failure
    from a destructor. The original publication GUID/vtable stays unchanged. }
  INyxViewSectionCandidateRetirement = interface(IInterface)
    ['{8F7C1A45-696B-43E6-BA1C-5DB5285F60D8}']
    procedure RetireCandidate;
  end;

  { A handle leases its section/candidate until publication or Cancel/release.
    Cancel is idempotent after cancellation; during preview or after successful
    publication it raises. Published handles own only copied identity/revision.
    Stale refusal leaves a prepared handle available for explicit cancellation.
    Optional explicit retirement reports cleanup errors AFTER becoming canceled;
    implicit final release attempts cleanup through nonthrowing destructors. }
  INyxViewSectionChange = interface(IInterface)
    ['{A19260D3-5F55-4A41-9D36-02860B6A4D9F}']
    function GetReference: TNyxViewSectionRef;
    function GetExpectedRevision: Integer;
    function GetState: TNyxViewSectionChangeState;
    procedure Cancel;
    property Reference: TNyxViewSectionRef read GetReference;
    property ExpectedRevision: Integer read GetExpectedRevision;
    property State: TNyxViewSectionChangeState read GetState;
  end;

{ Wrap a target's owned publication port. Nil/undefined ports raise before
  admission. Ordinary callers obtain handles from Section.Prepare instead. }
function NewNyxViewSectionChange(const APublication: INyxViewSectionPublication):
  INyxViewSectionChange;

{ One synchronous publication of independently prepared sections. Empty is a
  no-op. Nil/foreign/nonprepared handles or duplicate section objects/names raise.
  A stale revision/schema/allocation, busy view or occupied/moved host returns
  False before any preview.
  Preview failure rolls back the failing member and earlier members in reverse
  order, cancels the whole group and propagates the exception. All prepared
  members publish before any old view retires; unmentioned sections are untouched.
  This is a view transaction, not a document/source/history mutation. }
function PublishNyxViewSections(const AChanges: array of INyxViewSectionChange): Boolean; overload;
{ Couple section placement with an already prepared effect, such as retained
  form context/event adoption. Effects validate before physical section previews,
  fence again after ALL previews, and install without callbacks beside section
  ownership. Notify follows every install. Uninstalled Retire must undo its own
  reversible preview; it runs on refusal/failure as well as success. A caller can
  use PrepareNyxGroup for multiple effects. Committed becomes True before ANY
  notification/retirement callout, so its failure must not restore accepted data.
  An owning local retains the original effect through callbacks that replace
  the caller's reference. Cleanup is always attempted; an already propagating
  first failure retains its exact identity even if effect retirement also fails. }
function PublishNyxViewSections(const AChanges: array of INyxViewSectionChange;
  const AEffects: INyxPreparedPublication; out ACommitted: Boolean): Boolean; overload;

implementation

{$ifdef PAS2JS}
uses JS;
{$endif}

type
  { Forward concrete handle is exposed only through the private access port. }
  TNyxViewSectionChange = class;
  { The coordinator accepts only canonical handles, not foreign phase facades. }
  INyxViewSectionChangeAccess = interface(IInterface)
    ['{6258781A-6966-450C-ADEC-F2B9CE419085}']
    function GetChange: TNyxViewSectionChange;
  end;

  { One copied identity/revision and its exact managed target retirement duty. }
  TNyxViewSectionChange = class(TInterfacedObject, INyxViewSectionChange,
    INyxViewSectionChangeAccess)
  private
    FReference: TNyxViewSectionRef;
    FExpectedRevision: Integer;
    FState: TNyxViewSectionChangeState;
    FPublication: INyxViewSectionPublication;
  public
    constructor Create(const APublication: INyxViewSectionPublication);
    function GetReference: TNyxViewSectionRef;
    function GetExpectedRevision: Integer;
    function GetState: TNyxViewSectionChangeState;
    function GetChange: TNyxViewSectionChange;
    procedure Cancel;
  end;

class function TNyxViewSectionRef.Named(const AName: TNyxText): TNyxViewSectionRef;
var
  LIndex: Integer;
  LScalar: Integer;
begin

  if AName = '' then
  begin
    raise ENyxModel.Create('A view section requires a nonempty Unicode name without NUL');
  end;
  LIndex := 1;
  while LIndex <= Length(AName) do
  begin

    if not NyxNextScalar(AName, LIndex, LScalar) or (LScalar = 0) then
    begin
      raise ENyxModel.Create('A view section requires valid Unicode without NUL');
    end;
  end;
  Result.FName := AName;
end;

function TNyxViewSectionRef.Defined: Boolean;
begin
  Result := FName <> '';
end;

constructor TNyxViewSectionChange.Create(const APublication: INyxViewSectionPublication);
var
  LSection: INyxViewSection;
begin
  inherited Create;

  if APublication = nil then
  begin
    raise ENyxModel.Create('A prepared view section requires a publication port');
  end;
  LSection := APublication.Section;

  if (LSection = nil) or not LSection.Reference.Defined or
    (APublication.ExpectedRevision < 0) then
  begin
    raise ENyxModel.Create('A prepared view section requires its exact section and revision');
  end;
  FReference := LSection.Reference;
  FExpectedRevision := APublication.ExpectedRevision;
  FState := vcsPrepared;
  FPublication := APublication;
end;

function TNyxViewSectionChange.GetReference: TNyxViewSectionRef;
begin
  Result := FReference;
end;

function TNyxViewSectionChange.GetExpectedRevision: Integer;
begin
  Result := FExpectedRevision;
end;

function TNyxViewSectionChange.GetState: TNyxViewSectionChangeState;
begin
  Result := FState;
end;

function TNyxViewSectionChange.GetChange: TNyxViewSectionChange;
begin
  Result := Self;
end;

procedure TNyxViewSectionChange.Cancel;
var
  LLease: INyxViewSectionChange;
  LPublication: INyxViewSectionPublication;
  LRetirement: INyxViewSectionCandidateRetirement;
begin

  if FState in [vcsPreviewing, vcsPublished] then
  begin
    raise ENyxModel.Create('An active/published view section change cannot be canceled');
  end;
  { Detach this exact duty before a target cleanup can release its caller or
    reenter cancellation. No destructor must propagate the cleanup exception. }
  LLease := Self;
  LPublication := FPublication;
  FState := vcsCanceled;
  FPublication := nil;
  try

    if Supports(LPublication, INyxViewSectionCandidateRetirement, LRetirement) then
    begin
      LRetirement.RetireCandidate;
    end;
  finally
    LRetirement := nil;
    LPublication := nil;
  end;
end;

function NewNyxViewSectionChange(const APublication: INyxViewSectionPublication):
  INyxViewSectionChange;
begin
  Result := TNyxViewSectionChange.Create(APublication);
end;

{ Diagnostic text is secondary to exception identity. Do not call arbitrary
  JavaScript toString hooks while retiring a failed group; unrecognized platform
  values get a fixed explanation and remain intact for the original rethrow. }
function ActiveSectionFailureText{$ifdef PAS2JS}(AValue: JSValue){$endif}: TNyxText;
begin
  {$ifdef PAS2JS}

  if isClassInstance(AValue) and (TObject(AValue) is Exception) then
  begin
    Result := Exception(AValue).Message;
  end
  else if isString(AValue) then
  begin
    Result := String(AValue);
  end
  else
  begin
    Result := 'JavaScript platform exception';
  end;
  {$else}
  Result := Exception(ExceptObject).Message;
  {$endif}
end;

function PublishNyxViewSections(const AChanges: array of INyxViewSectionChange;
  const AEffects: INyxPreparedPublication; out ACommitted: Boolean): Boolean;
var
  LAccess: array of INyxViewSectionChangeAccess;
  LIndex: Integer;
  LFormer: Integer;
  LPreview: Integer;
  LChange: TNyxViewSectionChange;
  LPreviewError: TNyxText;
  LRollbackError: TNyxText;
  LCleanupError: TNyxText;
  LRollbackFailed: Boolean;
  LCleanupFailed: Boolean;
  LRetirementFailed: Boolean;
  LEffects: INyxPreparedPublication;
  LEffectsFence: INyxPublicationReadiness;
  LPrimaryFailed: Boolean;
  {$ifdef PAS2JS}
  LRetirementFailure: JSValue;
  {$else}
  LRetirementFailure: Exception;
  {$endif}
begin
  Result := False;
  ACommitted := False;
  LEffects := AEffects;
  LPrimaryFailed := False;
  try
    try
      SetLength(LAccess, Length(AChanges));
      { Complete shape/duplicate checks before asking ANY target to preview. }
      for LIndex := 0 to High(AChanges) do
      begin

        if (AChanges[LIndex] = nil) or
          not Supports(AChanges[LIndex], INyxViewSectionChangeAccess, LAccess[LIndex]) then
        begin
          raise ENyxModel.Create('A section group requires Nyx prepared change handles');
        end;
        LChange := LAccess[LIndex].GetChange;

        if LChange.FState <> vcsPrepared then
        begin
          raise ENyxModel.Create('A section group requires unpublished prepared changes');
        end;
        for LFormer := 0 to LIndex - 1 do
        begin

          if (LAccess[LFormer].GetChange.FReference.Name = LChange.FReference.Name) or
            (LAccess[LFormer].GetChange.FPublication.Section = LChange.FPublication.Section) then
          begin
            raise ENyxModel.Create('A section group contains a duplicate section');
          end;
        end;
      end;

      if LEffects <> nil then
      begin

        if not Supports(LEffects, INyxPublicationReadiness, LEffectsFence) then
        begin
          raise ENyxModel.Create('Section effects require a final readiness fence');
        end;
        LEffects.Validate;
      end;
      for LIndex := 0 to High(LAccess) do
      begin

        if not LAccess[LIndex].GetChange.FPublication.Check then
        begin
          Exit;
        end;
      end;
      LPreview := -1;
      try
        for LIndex := 0 to High(LAccess) do
        begin
          LPreview := LIndex;
          LAccess[LIndex].GetChange.FState := vcsPreviewing;
          LAccess[LIndex].GetChange.FPublication.Preview;
        end;

        if LEffectsFence <> nil then
        begin
          { A later focus/parenting callout cannot invalidate an earlier retained
            context and then let its old event witness commit unnoticed. }
          LEffectsFence.CheckReady;
        end;
      except
        LPreviewError := ActiveSectionFailureText{$ifdef PAS2JS}(JSExceptValue){$endif};
        LRollbackError := '';
        LCleanupError := '';
        LRollbackFailed := False;
        LCleanupFailed := False;
        { Include the throwing preview: target parenting may already have changed.
          Try every earlier rollback even if an extension violates its obligation;
          cancel all candidates and report that additional target failure. }
        for LIndex := LPreview downto 0 do
        begin
          try
            LAccess[LIndex].GetChange.FPublication.Rollback;
          except

            if not LRollbackFailed then
            begin
              LRollbackError := ActiveSectionFailureText{$ifdef PAS2JS}(JSExceptValue){$endif};
              LRollbackFailed := True;
            end;
          end;
        end;
        for LIndex := 0 to High(LAccess) do
        begin
          LAccess[LIndex].GetChange.FState := vcsCanceled;
          try
            LAccess[LIndex].GetChange.Cancel;
          except

            if not LCleanupFailed then
            begin
              LCleanupError := ActiveSectionFailureText{$ifdef PAS2JS}(JSExceptValue){$endif};
              LCleanupFailed := True;
            end;
          end;
        end;

        if LRollbackFailed then
        begin
          raise ENyxModel.Create('Section preview failed: ' + LPreviewError +
            TNyxText('; rollback also failed: ') + LRollbackError);
        end;

        if LCleanupFailed then
        begin
          raise ENyxModel.Create('Section preview failed: ' + LPreviewError +
            TNyxText('; candidate retirement also failed: ') + LCleanupError);
        end;
        raise;
      end;
      for LIndex := 0 to High(LAccess) do
      begin
        LAccess[LIndex].GetChange.FPublication.Publish;
        LAccess[LIndex].GetChange.FState := vcsPublished;
      end;

      if LEffects <> nil then
      begin
        LEffects.Install;
      end;
      ACommitted := True;
      { Retire after the whole group is authoritative. Publication ports contain
        their former owners until this separate release boundary. A supported
        fallible disconnect must not skip any other member's exact retirement. }
      LRetirementFailed := False;
      {$ifdef PAS2JS}
      LRetirementFailure := Undefined;
      {$else}
      LRetirementFailure := nil;
      {$endif}

      if LEffects <> nil then
      begin
        try
          LEffects.Notify;
        except
          {$ifdef PAS2JS}
          LRetirementFailure := JSExceptValue;
          {$else}
          LRetirementFailure := Exception(AcquireExceptionObject);
          {$endif}
          LRetirementFailed := True;
        end;
      end;
      for LIndex := 0 to High(LAccess) do
      begin
        LChange := LAccess[LIndex].GetChange;
        try
          try
            LChange.FPublication.RetirePrevious;
          except

            if not LRetirementFailed then
            begin
              {$ifdef PAS2JS}
              LRetirementFailure := JSExceptValue;
              {$else}
              LRetirementFailure := Exception(AcquireExceptionObject);
              {$endif}
              LRetirementFailed := True;
            end;
          end;
        finally
          LChange.FPublication := nil;
        end;
      end;

      if LRetirementFailed then
      begin
        {$ifdef PAS2JS}
        asm
          throw LRetirementFailure;
        end;
        {$else}
        raise LRetirementFailure;
        {$endif}
      end;
      Result := True;
    except
      LPrimaryFailed := True;
      raise;
    end;
  finally
    { Explicit cleanup reports a failed effect rollback; dropping its last
      managed reference alone would hide that failure in a destructor. }

    if LEffects <> nil then
    begin
      try
        LEffects.Retire;
      except

        if not LPrimaryFailed then
        begin
          raise;
        end;
        { Do not replace an empty Pascal exception message or raw JavaScript
          throw value with a secondary cleanup exception. The surrounding
          unwind still owns and propagates the exact original failure. }
      end;
    end;
  end;
end;

function PublishNyxViewSections(const AChanges: array of INyxViewSectionChange): Boolean;
var
  LCommitted: Boolean;
begin
  Result := PublishNyxViewSections(AChanges, nil, LCommitted);
end;

end.
