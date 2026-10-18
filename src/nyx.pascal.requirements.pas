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
unit nyx.pascal.requirements;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses SysUtils, nyx.text, nyx.pascal.context, nyx.pascal.lexing,
  nyx.pascal.syntax.translated;

type
  { Closed compiler evidence request, independent of diagnostic text. A primitive
    size query establishes presence/absence and size in one exact compiler scope;
    it never asks the pristine host to bind an authored alias or record layout. }
  TNyxPascalCompilerRequirementKind = (pcrPrimitiveSize);
  { Immutable missing fact with separate context owner and observing source.
    ContextState=-1 selects the owner's independent bootstrap packet; other
    ordinals select an exact retained translation state. SourceOffset is virtual
    and may be EOF; OriginIndex=-1 honestly reports no mapped physical token.
    All leases point outward to frozen source/context, never an editor or query. }
  INyxPascalCompilerRequirement = interface(IInterface)
    ['{7BA1AE9A-2728-40F8-B575-245022BA2B82}']
    function Kind: TNyxPascalCompilerRequirementKind;
    function Reference: TNyxPascalSizeReference;
    function Context: INyxPascalCompilerContext;
    function OwnerSource: INyxPascalTranslatedSyntax;
    function ContextState: Integer;
    function Source: INyxPascalTranslatedSyntax;
    function SourceOffset: Integer;
    function OriginIndex: Integer;
  end;
  { Optional read-only response capability. Exact first-use order and at most
    64 complete requirements are captured; missing capability means no request,
    never implicit permission to infer requirements from an English message. }
  INyxPascalCompilerRequirements = interface(IInterface)
    ['{259C234F-EE06-42DD-9F00-022F43437A1C}']
    function RequirementCount: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
  end;

{ Freeze context and retain exact immutable source leases. The selected owner
  state must equal that context, and the requested primitive fact must be unknown.
  Invalid provenance/ranges/identity refuses before publishing a requirement. }
function NewNyxPascalPrimitiveRequirement(const AContext: INyxPascalCompilerContext;
  const AReference: TNyxPascalSizeReference; const AOwner, ASource: INyxPascalTranslatedSyntax;
  AContextState, ASourceOffset: Integer): INyxPascalCompilerRequirement;
{ Freeze an alternate requirement once; no producer/callback survives capture. }
function CaptureNyxPascalRequirement(const ASource: INyxPascalCompilerRequirement): INyxPascalCompilerRequirement;
{ An empty or one-requirement owned response. Nil explicitly means empty. }
function NewNyxPascalRequirements(const ARequirement: INyxPascalCompilerRequirement = nil): INyxPascalCompilerRequirements;
{ Freeze the optional capability of a response. Invalid count/getter/context
  refuses the whole collection; absent/nil capabilities yield an empty snapshot. }
function CaptureNyxPascalRequirements(const AResponse: IInterface): INyxPascalCompilerRequirements;
{ Exact retained pre-token state at an actual origin or complete EOF. -2 means
  no captured state at that virtual position; -1 stays reserved for bootstrap. }
function NyxPascalContextStateAt(const ASource: INyxPascalTranslatedSyntax;
  AOffset: Integer): Integer;

implementation

type
  { Owns only immutable snapshots, copied selector and checked scalar indices.
    Source leases have no reverse reference to this requirement or its result. }
  TCompilerRequirement = class(TInterfacedObject, INyxPascalCompilerRequirement)
  private
    FReference: TNyxPascalSizeReference;
    FContext: INyxPascalCompilerContext;
    FOwner: INyxPascalTranslatedSyntax;
    FSource: INyxPascalTranslatedSyntax;
    FState: Integer;
    FOffset: Integer;
    FOrigin: Integer;
  public
    constructor Create(const AContext: INyxPascalCompilerContext;
      const AReference: TNyxPascalSizeReference; const AOwner, ASource: INyxPascalTranslatedSyntax;
      AState, AOffset: Integer);
    function Kind: TNyxPascalCompilerRequirementKind;
    function Reference: TNyxPascalSizeReference;
    function Context: INyxPascalCompilerContext;
    function OwnerSource: INyxPascalTranslatedSyntax;
    function ContextState: Integer;
    function Source: INyxPascalTranslatedSyntax;
    function SourceOffset: Integer;
    function OriginIndex: Integer;
  end;
  { Owns a detached dynamic interface vector, never interface fields in records
    or static arrays. Only factory construction writes entries before publication. }
  TCompilerRequirements = class(TInterfacedObject, INyxPascalCompilerRequirements)
  public
    Values: array of INyxPascalCompilerRequirement;
    function RequirementCount: Integer;
    function RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
  end;

function NyxPascalContextStateAt(const ASource: INyxPascalTranslatedSyntax;
  AOffset: Integer): Integer;
var
  LOrigin: Integer;
begin
  Result := -2;

  if (ASource = nil) or (AOffset < 0) or (AOffset > ASource.Syntax.Lexical.Scalars) then
  begin
    Exit;
  end;
  LOrigin := ASource.OriginAt(AOffset);

  if LOrigin >= 0 then
  begin
    Result := ASource.Origin(LOrigin).State;
  end
  else if (AOffset = ASource.Syntax.Lexical.Scalars) and ASource.CompleteTranslation then
  begin
    Result := ASource.Source.Occurrence(0).ExitState;
  end;
end;

constructor TCompilerRequirement.Create(const AContext: INyxPascalCompilerContext;
  const AReference: TNyxPascalSizeReference; const AOwner, ASource: INyxPascalTranslatedSyntax;
  AState, AOffset: Integer);
var
  LExpected: INyxPascalCompilerContext;
begin
  inherited Create;
  FContext := CaptureNyxPascalCompilerContext(AContext);
  FReference := NyxPascalSizeReference(AReference.Name);

  if (AOwner = nil) or (ASource = nil) or (AState < -1) or
    (AState >= AOwner.Source.StateCount) or (AOffset < 0) or
    (AOffset > ASource.Syntax.Lexical.Scalars) or
    (FContext.Family <> ppcFPC) or (FContext.Version.FullNumber = 0) or
    (FContext.Mode = ppmUnknown) or
    (NyxPascalCapturedSize(FContext, FReference).State <> pssUnknown) then
  begin
    raise ENyxPascalContext.Create('A missing primitive request needs exact compatible unknown compiler facts');
  end;

  if AState = -1 then
  begin
    LExpected := NyxPascalBootstrapContext(AOwner.Source.InitialContext);

    if NyxPascalCompilerScope(FContext) <> pcsoSystemBootstrap then
    begin
      raise ENyxPascalContext.Create('A bootstrap requirement cannot acquire pristine-program provenance');
    end;
  end
  else
  begin
    LExpected := AOwner.Source.State(AState);
  end;

  if (LExpected = nil) or not FContext.SameFacts(LExpected) then
  begin
    raise ENyxPascalContext.Create('A missing primitive requirement differs from its owning source context');
  end;
  FOwner := AOwner;
  FSource := ASource;
  FState := AState;
  FOffset := AOffset;
  FOrigin := ASource.OriginAt(AOffset);
end;

function TCompilerRequirement.Kind: TNyxPascalCompilerRequirementKind;
begin
  Result := pcrPrimitiveSize;
end;

function TCompilerRequirement.Reference: TNyxPascalSizeReference;
begin
  Result := FReference;
end;

function TCompilerRequirement.Context: INyxPascalCompilerContext;
begin
  Result := FContext;
end;

function TCompilerRequirement.OwnerSource: INyxPascalTranslatedSyntax;
begin
  Result := FOwner;
end;

function TCompilerRequirement.ContextState: Integer;
begin
  Result := FState;
end;

function TCompilerRequirement.Source: INyxPascalTranslatedSyntax;
begin
  Result := FSource;
end;

function TCompilerRequirement.SourceOffset: Integer;
begin
  Result := FOffset;
end;

function TCompilerRequirement.OriginIndex: Integer;
begin
  Result := FOrigin;
end;

function NewNyxPascalPrimitiveRequirement(const AContext: INyxPascalCompilerContext;
  const AReference: TNyxPascalSizeReference; const AOwner, ASource: INyxPascalTranslatedSyntax;
  AContextState, ASourceOffset: Integer): INyxPascalCompilerRequirement;
begin
  Result := TCompilerRequirement.Create(AContext, AReference, AOwner, ASource,
    AContextState, ASourceOffset);
end;

function CaptureNyxPascalRequirement(const ASource: INyxPascalCompilerRequirement): INyxPascalCompilerRequirement;
begin

  if (ASource = nil) or (ASource.Kind <> pcrPrimitiveSize) then
  begin
    raise ENyxPascalContext.Create('Capture requires one supported complete compiler requirement');
  end;
  Result := NewNyxPascalPrimitiveRequirement(ASource.Context, ASource.Reference,
    ASource.OwnerSource, ASource.Source, ASource.ContextState, ASource.SourceOffset);
end;

function TCompilerRequirements.RequirementCount: Integer;
begin
  Result := Length(Values);
end;

function TCompilerRequirements.RequirementAt(AIndex: Integer): INyxPascalCompilerRequirement;
begin

  if (AIndex < 0) or (AIndex >= Length(Values)) then
  begin
    raise ENyxPascalContext.Create('A compiler requirement index is outside its captured response');
  end;
  Result := Values[AIndex];
end;

function NewNyxPascalRequirements(const ARequirement: INyxPascalCompilerRequirement): INyxPascalCompilerRequirements;
var
  LResult: TCompilerRequirements;
begin
  LResult := TCompilerRequirements.Create;
  Result := LResult;

  if ARequirement <> nil then
  begin
    SetLength(LResult.Values, 1);
    LResult.Values[0] := CaptureNyxPascalRequirement(ARequirement);
  end;
end;

function CaptureNyxPascalRequirements(const AResponse: IInterface): INyxPascalCompilerRequirements;
var
  LSource: INyxPascalCompilerRequirements;
  LResult: TCompilerRequirements;
  LCount: Integer;
  LAt: Integer;
  LOwner: INyxPascalCompilerRequirements;
begin
  LResult := TCompilerRequirements.Create;
  LOwner := LResult;

  if (AResponse <> nil) and Supports(AResponse, INyxPascalCompilerRequirements, LSource) then
  begin
    LCount := LSource.RequirementCount;

    if (LCount < 0) or (LCount > 64) then
    begin
      raise ENyxPascalContext.Create('Compiler requirement responses exceed their bounded collection');
    end;
    SetLength(LResult.Values, LCount);
    for LAt := 0 to LCount - 1 do
    begin
      LResult.Values[LAt] := CaptureNyxPascalRequirement(LSource.RequirementAt(LAt));
    end;
  end;
  Result := LOwner;
end;

end.
