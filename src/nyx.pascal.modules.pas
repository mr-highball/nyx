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
unit nyx.pascal.modules;
{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.pascal.context, nyx.pascal.files, nyx.pascal.translation,
  nyx.pascal.requirements, nyx.pascal.compilertrace;

type
  { Open Pascal namespace for analysis, distinct from a physical file and the
    compiler-project admission identity. ASCII case folding is explicit; no
    filename convention, unit alias or filesystem search is inferred. }
  TNyxPascalModuleRef = record
  private
    FName: TNyxText;
  public
    property Name: TNyxText read FName;
  end;
  { One immutable captured namespace/root association. A nil Context inherits
    the requesting profile's facts/options. InContext freezes an explicit unit
    mode/lexical context, allowing SDK units to differ from application units.
    Neither form grants source, filesystem, compiler or execution authority. }
  INyxPascalModuleSource = interface(IInterface)
    ['{2D65A080-0690-44CD-887C-35AD8B0C7001}']
    function Name: TNyxPascalModuleRef;
    function Root: TNyxPascalPhysicalFile;
    function Context: INyxPascalCompilerContext;
    function Translation: TNyxPascalTranslationOptions;
    function InContext(const AContext: INyxPascalCompilerContext;
      const ATranslation: TNyxPascalTranslationOptions): INyxPascalModuleSource;
  end;
  { Persistent explicit module index. Entries retain independent source
    descriptors, not parsed worlds/results or their consumers. Replacement
    preserves ordinal and old snapshots. Up to 4096 entries are bounded even
    when only a small subset is loaded by a completion query. }
  INyxPascalModuleCatalog = interface(IInterface)
    ['{2D65A080-0690-44CD-887C-35AD8B0C7002}']
    function Count: Integer;
    function Entry(AIndex: Integer): INyxPascalModuleSource;
    function Find(const AName: TNyxPascalModuleRef): Integer;
    function WithModule(const ASource: INyxPascalModuleSource): INyxPascalModuleCatalog;
  end;

{ Validate dotted ASCII identifiers without compiler-project/codegen dependencies. }
function NyxPascalModule(const AName: TNyxText): TNyxPascalModuleRef;
{ Explicit inherited-profile association. Root need not be present until catalog
  capture; creating metadata never reads it or assumes the declared namespace. }
function NewNyxPascalModuleSource(const AName: TNyxPascalModuleRef;
  const ARoot: TNyxPascalPhysicalFile): INyxPascalModuleSource;
{ Empty persistent index; no host directories or implicit SDK roots are scanned. }
function NewNyxPascalModuleCatalog: INyxPascalModuleCatalog;
{ Freeze caller-serialized alternate entries and require their actual captured
  files. Duplicate namespace entries and physical-root aliases refuse. Declared
  root identity is checked again when its translated source is actually loaded. }
function CaptureNyxPascalModules(const AModules: INyxPascalModuleCatalog;
  const AFiles: INyxPascalFileCatalog): INyxPascalModuleCatalog;
{ Apply one exact fact reply to its unchanged captured module owner. Files,
  namespace, prior context and the reply's single-fact delta must still agree.
  Bootstrap facts update that independent packet, never the unit's source mode.
  Failure publishes no descriptor and edits no source, catalog, project or host. }
function ResolveNyxPascalModuleRequirement(const AFiles: INyxPascalFileCatalog;
  const AModule: INyxPascalModuleSource; const ARequirement: INyxPascalCompilerRequirement;
  const AOwner, AReply: INyxFPCCompilerCapture): INyxPascalModuleSource;

implementation

uses SysUtils, nyx.text.index, nyx.pascal.lexing, nyx.pascal.binding;

type
  { Frozen root metadata with optional independently captured compiler facts.
    Its configuration changes return a new descriptor, retaining no caller. }
  TModuleSource = class(TInterfacedObject, INyxPascalModuleSource)
  public
    ModuleName: TNyxPascalModuleRef;
    FileRoot: TNyxPascalPhysicalFile;
    Facts: INyxPascalCompilerContext;
    Options: TNyxPascalTranslationOptions;
    function Name: TNyxPascalModuleRef;
    function Root: TNyxPascalPhysicalFile;
    function Context: INyxPascalCompilerContext;
    function Translation: TNyxPascalTranslationOptions;
    function InContext(const AContext: INyxPascalCompilerContext;
      const ATranslation: TNyxPascalTranslationOptions): INyxPascalModuleSource;
  end;
  { Detached ordered entry vector plus a read-only published name index. Only
    WithModule builds a new vector/index; no query mutates a shared catalog. }
  TModuleCatalog = class(TInterfacedObject, INyxPascalModuleCatalog)
  private
    FEntries: array of INyxPascalModuleSource;
    FNames: TNyxTextIndex;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    function Entry(AIndex: Integer): INyxPascalModuleSource;
    function Find(const AName: TNyxPascalModuleRef): Integer;
    function WithModule(const ASource: INyxPascalModuleSource): INyxPascalModuleCatalog;
  end;

function NyxPascalModule(const AName: TNyxText): TNyxPascalModuleRef;
begin
  Result.FName := NyxPascalBindingName(AName).Name;
end;

function TModuleSource.Name: TNyxPascalModuleRef;
begin
  Result := ModuleName;
end;

function TModuleSource.Root: TNyxPascalPhysicalFile;
begin
  Result := FileRoot;
end;

function TModuleSource.Context: INyxPascalCompilerContext;
begin
  Result := Facts;
end;

function TModuleSource.Translation: TNyxPascalTranslationOptions;
begin
  Result := Options;
end;

function TModuleSource.InContext(const AContext: INyxPascalCompilerContext;
  const ATranslation: TNyxPascalTranslationOptions): INyxPascalModuleSource;
var
  LSource: TModuleSource;
begin
  ATranslation.Validate;
  LSource := TModuleSource.Create;
  Result := LSource;
  LSource.ModuleName := ModuleName;
  LSource.FileRoot := FileRoot;
  LSource.Facts := CaptureNyxPascalCompilerContext(AContext);
  LSource.Options := ATranslation;
end;

function NewNyxPascalModuleSource(const AName: TNyxPascalModuleRef;
  const ARoot: TNyxPascalPhysicalFile): INyxPascalModuleSource;
var
  LSource: TModuleSource;
begin
  LSource := TModuleSource.Create;
  Result := LSource;
  LSource.ModuleName := NyxPascalModule(AName.Name);
  LSource.FileRoot := NyxPascalPhysicalFile(ARoot.Name);
  LSource.Options := TNyxPascalTranslationOptions.Defaults;
end;

constructor TModuleCatalog.Create;
begin
  inherited Create;
  FNames := TNyxTextIndex.Create;
end;

destructor TModuleCatalog.Destroy;
begin
  FNames.Free;
  inherited Destroy;
end;

function TModuleCatalog.Count: Integer;
begin
  Result := Length(FEntries);
end;

function TModuleCatalog.Entry(AIndex: Integer): INyxPascalModuleSource;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxPascalContext.Create('Module index is outside the captured catalog');
  end;
  Result := FEntries[AIndex];
end;

function TModuleCatalog.Find(const AName: TNyxPascalModuleRef): Integer;
begin
  Result := FNames.IndexOf(NyxPascalASCIIKey(NyxPascalModule(AName.Name).Name));
end;

function TModuleCatalog.WithModule(const ASource: INyxPascalModuleSource): INyxPascalModuleCatalog;
var
  LSource: INyxPascalModuleSource;
  LCopy: TModuleCatalog;
  LAt: Integer;
  LTarget: Integer;
  LCount: Integer;
begin

  if ASource = nil then
  begin
    raise ENyxPascalContext.Create('A module catalog needs explicit root metadata');
  end;
  LSource := NewNyxPascalModuleSource(ASource.Name, ASource.Root);

  if ASource.Context <> nil then
  begin
    LSource := LSource.InContext(ASource.Context, ASource.Translation);
  end;
  LTarget := Find(LSource.Name);
  LCount := Count;

  if LTarget < 0 then
  begin

    if LCount >= 4096 then
    begin
      raise ENyxPascalContext.Create('The captured module index exceeds 4096 entries');
    end;
    LTarget := LCount;
    Inc(LCount);
  end;
  LCopy := TModuleCatalog.Create;
  Result := LCopy;
  SetLength(LCopy.FEntries, LCount);
  for LAt := 0 to LCount - 1 do
  begin

    if LAt = LTarget then
    begin
      LCopy.FEntries[LAt] := LSource;
    end
    else
    begin
      LCopy.FEntries[LAt] := FEntries[LAt];
    end;
    LCopy.FNames.AddFirst(NyxPascalASCIIKey(LCopy.FEntries[LAt].Name.Name), LAt);
  end;
end;

function NewNyxPascalModuleCatalog: INyxPascalModuleCatalog;
begin
  Result := TModuleCatalog.Create;
end;

function CaptureNyxPascalModules(const AModules: INyxPascalModuleCatalog;
  const AFiles: INyxPascalFileCatalog): INyxPascalModuleCatalog;
var
  LAt: Integer;
  LOther: Integer;
  LSource: INyxPascalModuleSource;
begin

  if (AModules = nil) or (AFiles = nil) or (AModules.Count < 0) or (AModules.Count > 4096) then
  begin
    raise ENyxPascalContext.Create('Module capture needs bounded explicit entries and files');
  end;
  Result := NewNyxPascalModuleCatalog;
  for LAt := 0 to AModules.Count - 1 do
  begin
    LSource := AModules.Entry(LAt);

    if LSource = nil then
    begin
      raise ENyxPascalContext.Create('An alternate module entry is missing');
    end;

    if (Result.Find(LSource.Name) >= 0) or (AFiles.Find(LSource.Root) < 0) then
    begin
      raise ENyxPascalContext.Create('Module roots must be captured with unique namespace identities');
    end;
    for LOther := 0 to Result.Count - 1 do
    begin

      if AFiles.Find(Result.Entry(LOther).Root) = AFiles.Find(LSource.Root) then
      begin
        raise ENyxPascalContext.Create('Physical module aliases require additional compiler context');
      end;
    end;
    Result := Result.WithModule(LSource);
  end;
end;
function ResolveNyxPascalModuleRequirement(const AFiles: INyxPascalFileCatalog;
  const AModule: INyxPascalModuleSource; const ARequirement: INyxPascalCompilerRequirement;
  const AOwner, AReply: INyxFPCCompilerCapture): INyxPascalModuleSource;
var
  LFiles: INyxPascalFileCatalog;
  LModule: INyxPascalModuleSource;
  LRequirement: INyxPascalCompilerRequirement;
  LReply: INyxFPCCompilerCapture;
  LOwner: INyxFPCCompilerCapture;
  LFact: TNyxPascalSizeFact;
  LExpected: INyxPascalCompilerContext;
  LContext: INyxPascalCompilerContext;
  LRoot: TNyxPascalPhysicalFile;
begin
  LFiles := CaptureNyxPascalFiles(AFiles);
  LModule := CaptureNyxPascalModules(NewNyxPascalModuleCatalog.WithModule(AModule), LFiles).Entry(0);
  LRequirement := CaptureNyxPascalRequirement(ARequirement);
  LReply := FreezeNyxFPCCompilerCapture(AReply);
  LOwner := FreezeNyxFPCCompilerCapture(AOwner);

  if not LOwner.Context.SameFacts(LRequirement.Context) then
  begin
    raise ENyxPascalContext.Create('A fact reply needs its exact previous capture and default anchor');
  end;
  LOwner := MergeNyxFPCSizeCapture(LOwner, LReply, LRequirement.Reference);
  LRoot := LRequirement.OwnerSource.Source.Files.FileAt(
    LRequirement.OwnerSource.Source.Occurrence(0).FileIndex);

  if not LFiles.SameFiles(LRequirement.OwnerSource.Source.Files) or
    (LModule.Root.Name <> LRoot.Name) or
    (NyxPascalASCIIKey(LModule.Name.Name) <>
      NyxPascalASCIIKey(LRequirement.OwnerSource.Syntax.Scope(0).Name)) or
    (LModule.Context = nil) or
    not LModule.Context.SameFacts(LRequirement.OwnerSource.Source.InitialContext) then
  begin
    raise ENyxPascalContext.Create('A compiler fact reply has stale or foreign module source ownership');
  end;
  LFact := NyxPascalCapturedSize(LReply.Context, LRequirement.Reference);

  if (LFact.State = pssUnknown) or (LFact.ProbeMode <> LRequirement.Context.Mode) or
    (LFact.ProbeLongStrings <> LRequirement.Context.Option('H')) then
  begin
    raise ENyxPascalContext.Create('A compiler fact reply does not resolve the requested mode and reference');
  end;
  LExpected := LOwner.Context;

  if not LExpected.SameFacts(LReply.Context) then
  begin
    raise ENyxPascalContext.Create('A compiler fact reply changed more than its requested fact');
  end;
  LContext := LModule.Context;

  if NyxPascalCompilerScope(LRequirement.Context) = pcsoSystemBootstrap then
  begin
    LExpected := NyxPascalBootstrapContext(LContext);

    if (LExpected = nil) or not LExpected.SameFacts(LRequirement.Context) then
    begin
      raise ENyxPascalContext.Create('The module compiler-root packet changed before fact application');
    end;
    LContext := NyxPascalWithBootstrapContext(LContext, LReply.Context);
  end
  else
  begin

    if not LContext.SameFacts(LRequirement.Context) then
    begin
      raise ENyxPascalContext.Create('Source-local facts cannot be applied as pristine module defaults');
    end;
    LContext := LReply.Context;
  end;
  Result := LModule.InContext(LContext, LModule.Translation);
end;

end.
