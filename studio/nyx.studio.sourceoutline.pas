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
unit nyx.studio.sourceoutline;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.model, nyx.controls, nyx.events, nyx.behavior, nyx.source,
  nyx.studio.session, nyx.pascal.drafts;

const
  NyxStudioSourceOutlineID = 'studio-source-outline';
  NyxStudioSourceOutlineTabID = 'action-source-outline-tab';

type
  { Field input preserves its physical widget. Rebuild updates only source
    presentation; Navigate additionally gives the adapter one physical caret. }
  TNyxStudioSourceOutlineEffect = (soeNone, soeField, soeRebuild, soeNavigate);

  { Borrowed presenter bridge for copied view records: pas2js cannot place COM
    interfaces in records. The controller's managed interface owns this object;
    presentation records must never free it or outlive that controller. }
  TNyxStudioSourceOutline = class(TInterfacedObject)
  public
    function Build(ASession: TNyxStudioSession): INyxCard; virtual; abstract;
    function Route(ASession: TNyxStudioSession; ARoot, ANode: TNyxNode;
      const AEvent: TNyxEventInfo;
      out ALocation: TNyxPascalRoutineLocation): TNyxStudioSourceOutlineEffect; virtual; abstract;
  end;

  { Transient managed outline owner shared by both adapters. It owns copied
    inputs/index/search, borrowing a session only during calls. Switching loads
    resets discovery; no document/control/compiler/history or session is retained.
    Changed inputs retire navigation. Editing positions follow captured buffers;
    accepted positions refuse a changed target without erasing unfinished text.
    Calls are serialized by the owning editor UI thread. Search/owner/page are
    presentation, deliberately absent from project Undo. }
  INyxStudioSourceOutline = interface(IInterface)
    ['{68F89A18-71A1-4D73-84BC-265587A0343D}']
    function Presenter: TNyxStudioSourceOutline;
    function Build(ASession: TNyxStudioSession): INyxCard;
    function Route(ASession: TNyxStudioSession; ARoot, ANode: TNyxNode;
      const AEvent: TNyxEventInfo;
      out ALocation: TNyxPascalRoutineLocation): TNyxStudioSourceOutlineEffect;
  end;

  { Optional read-only compiler context. A selected main/unit uses itself;
    includes use the ordinary Outline's explicit owner selector. This retains
    no source or session and changes neither the existing outline vtable nor Undo. }
  INyxStudioSourceOutlineContext = interface(IInterface)
    ['{4847558D-A467-44CA-B8C8-59A435EF9D03}']
    function CompletionOwner(ASession: TNyxStudioSession): TNyxPascalSourceRef;
  end;

function NewNyxStudioSourceOutline: INyxStudioSourceOutline;

implementation

uses SysUtils, nyx.types, nyx.data, nyx.pascal.project,
  nyx.pascal.outline, nyx.pascal.sources;

type
  { Cached index belongs to fixed accepted membership and exact inspected
    buffers for one explicit owning unit. Opaque mount
    serials distinguish retired rows even when renderers reconcile equal IDs.
    Exceptions become scoped readable feedback, never draft/history changes. }
  TSourceOutline = class(TNyxStudioSourceOutline, INyxStudioSourceOutline,
    INyxStudioSourceOutlineContext)
  private
    FContext: TNyxStudioCommandContext;
    FProject: INyxPascalProject;
    FSources: INyxPascalSourceSnapshot;
    FKind: TNyxPascalSourceSnapshotKind;
    FMainBuffer: TNyxText;
    FHelperBuffers: TNyxPascalFileDrafts;
    FIndex: INyxPascalRoutineIndex;
    FSymbols: INyxPascalSymbolIndex;
    FContent: TNyxPascalOutlineContent;
    FAttempted: Boolean;
    FOwner: TNyxPascalSourceRef;
    FQuery: TNyxText;
    FFilter: TNyxText;
    FMessage: TNyxText;
    FPage: Integer;
    FMount: Integer;
  public
    function Presenter: TNyxStudioSourceOutline;
    function CompletionOwner(ASession: TNyxStudioSession): TNyxPascalSourceRef;
    function Build(ASession: TNyxStudioSession): INyxCard; override;
    function Route(ASession: TNyxStudioSession; ARoot, ANode: TNyxNode;
      const AEvent: TNyxEventInfo;
      out ALocation: TNyxPascalRoutineLocation): TNyxStudioSourceOutlineEffect; override;
  end;

const
  CMountKey = 'nyx.studio.outline.mount';

function TSourceOutline.Presenter: TNyxStudioSourceOutline;
begin
  Result := Self;
end;

function TSourceOutline.CompletionOwner(ASession: TNyxStudioSession): TNyxPascalSourceRef;
begin
  Result := ASession.ActiveSourceFile;

  if Result.Role = npsInclude then
  begin
    Result := TNyxPascalSourceRef.Main;

    if ASession.MatchesCommandContext(FContext) then
    begin
      Result := FOwner;
    end;
  end;
end;

function TSourceOutline.Build(ASession: TNyxStudioSession): INyxCard;
var
  LProject: INyxPascalProject;
  LOwnerExists: Boolean;
  LInput: Integer;
  LMain: TNyxText;
  LHelpers: TNyxPascalFileDrafts;
  LReadStatus: INyxPascalRoutineReadStatus;
  LReadIndex: IInterface;
  LMessage: TNyxText;
  LProblem: TNyxPascalRoutineLocation;
begin

  if ASession = nil then
  begin
    raise ENyxModel.Create('A source outline requires its current Studio session');
  end;

  if not ASession.MatchesCommandContext(FContext) then
  begin
    FContext := ASession.CommandContext;
    FProject := nil;
    FSources := nil;
    FIndex := nil;
    FSymbols := nil;
    FContent := pocRoutines;
    FAttempted := False;
    FOwner := TNyxPascalSourceRef.Main;

    if ASession.ActiveSourceFile.Role = npsUnit then
    begin
      FOwner := ASession.ActiveSourceFile;
    end;
    FQuery := '';
    FFilter := '';
    FPage := 0;
    FMessage := '';
    FKind := pssEditing;
  end;
  LProject := ASession.AcceptedPascalProject;
  LMain := ASession.DraftSource;
  LHelpers := ASession.FileDrafts;

  if (FProject <> LProject) or
    ((FKind = pssEditing) and ((FMainBuffer <> LMain) or
      not FHelperBuffers.SameValues(LHelpers))) then
  begin
    FProject := LProject;
    FSources := nil;
    FIndex := nil;
    FSymbols := nil;
    FAttempted := False;
  end;
  LOwnerExists := FOwner.Role = npsMain;
  for LInput := 0 to FProject.Count - 1 do
  begin

    if (FProject.Input(LInput).Kind = npiUnit) and
      (FProject.Input(LInput).Name = FOwner.Name) then
    begin
      LOwnerExists := True;
    end;
  end;

  if not LOwnerExists then
  begin
    FOwner := TNyxPascalSourceRef.Main;
    FIndex := nil;
    FSymbols := nil;
    FAttempted := False;
    FPage := 0;
  end;

  if not FAttempted then
  begin
    FAttempted := True;
    try
      FSources := CaptureNyxPascalSources(FProject);
      FMainBuffer := LMain;
      FHelperBuffers := LHelpers;

      if FKind = pssEditing then
      begin
        FSources := FSources.WithBuffer(TNyxPascalSourceRef.Main, FMainBuffer);
        for LInput := 0 to FHelperBuffers.Count - 1 do
        begin
          FSources := FSources.WithBuffer(FHelperBuffers.Item(LInput).FileReference,
            FHelperBuffers.Item(LInput).Text);
        end;
      end;

      if FContent = pocDeclarations then
      begin
        FSymbols := CaptureNyxPascalSymbolIndex(FSources, FOwner);
      end
      else
      begin
        FIndex := CaptureNyxPascalRoutineIndex(FSources, FOwner);
      end;
    except
      on LException: Exception do
      begin
        {$ifdef PAS2JS}
        FMessage := LException.Message;
        {$else}
        FMessage := UTF8Encode(UnicodeString(LException.Message));
        {$endif}
      end;
    end;
  end;
  Inc(FMount);
  LMessage := FMessage;

  LReadIndex := FIndex;

  if FContent = pocDeclarations then
  begin
    LReadIndex := FSymbols;
  end;

  if Supports(LReadIndex, INyxPascalRoutineReadStatus, LReadStatus) and
    (LReadStatus.ProblemText <> '') then
  begin
    LProblem := LReadStatus.ProblemLocation;

    if LMessage <> '' then
    begin
      LMessage := LMessage + #10;
    end;

    if LProblem.IsDefined then
    begin
      LMessage := LMessage + ASession.SourceFileTitle(LProblem.FileReference) +
        ' / ' + IntToStr(LProblem.Line) + ':' + IntToStr(LProblem.Column) + ' / ';
    end;
    LMessage := LMessage + LReadStatus.ProblemText;
  end;

  if FContent = pocDeclarations then
  begin
    Result := NewNyxPascalSymbolOutline(NyxStudioSourceOutlineID, FProject, FSymbols,
      FOwner, FQuery, FFilter, FPage, FKind, LMessage);
  end
  else
  begin
    Result := NewNyxPascalOutline(NyxStudioSourceOutlineID, FProject, FIndex,
      FOwner, FQuery, FFilter, FPage, FKind, LMessage);
  end;
  Result.Node.Extensions.SetValue(NyxExtension(CMountKey), NyxData(FMount));
end;

function TSourceOutline.Route(ASession: TNyxStudioSession; ARoot, ANode: TNyxNode;
  const AEvent: TNyxEventInfo;
  out ALocation: TNyxPascalRoutineLocation): TNyxStudioSourceOutlineEffect;
var
  LAction: TNyxPascalOutlineAction;
  LRow: Integer;
  LPart: TNyxPascalOutlinePart;
  LForm: TNyxNode;
  LInput: Integer;
  LFound: Boolean;
  LOwner: TNyxPascalSourceRef;
begin
  Result := soeNone;
  ALocation := Default(TNyxPascalRoutineLocation);

  if (ASession = nil) or (ARoot = nil) or (ANode = nil) or AEvent.DefaultPrevented then
  begin
    Exit;
  end;
  LAction := NyxPascalOutlineAction(NyxStudioSourceOutlineID, ANode, AEvent.Trigger, LRow, LPart);

  if LAction = poaNone then
  begin
    Exit;
  end;
  Result := soeRebuild;
  try
    LForm := ARoot.Find(NyxStudioSourceOutlineID);

    if (ARoot.Find(ANode.ID) <> ANode) or (LForm = nil) or
      not LForm.Extensions.Has(NyxExtension(CMountKey)) or
      (LForm.Extensions.Value(NyxExtension(CMountKey)).AsInteger <> FMount) or
      not ASession.MatchesCommandContext(FContext) or
      (ASession.AcceptedPascalProject <> FProject) then
    begin
      raise ENyxModel.Create('The source changed. Use the refreshed outline before navigating');
    end;

    if (FKind = pssEditing) and ((ASession.DraftSource <> FMainBuffer) or
      not FHelperBuffers.SameValues(ASession.FileDrafts)) then
    begin
      raise ENyxModel.Create('The editing buffers changed. Use the refreshed outline before navigating');
    end;
    FMessage := '';
    FQuery := LForm.Find(NyxStudioSourceOutlineID + '-query').Prop('value');
    case LAction of
      poaContent:
        begin
          FContent := NyxPascalOutlineContent(ANode.Prop('value'));
          FIndex := nil;
          FSymbols := nil;
          FAttempted := False;
          FPage := 0;
        end;
      poaSnapshot:
        begin
          FKind := NyxPascalOutlineSnapshotKind(ANode.Prop('value'));
          FSources := nil;
          FIndex := nil;
          FSymbols := nil;
          FAttempted := False;
          FPage := 0;
        end;
      poaField:
        begin
          Result := soeField;
        end;
      poaOwner:
        begin
          LFound := False;
          for LInput := 0 to ASession.SourceFileCount - 1 do
          begin
            LOwner := ASession.SourceFile(LInput);

            if (LOwner.Role <> npsInclude) and
              (ASession.SourceFileTitle(LOwner) = ANode.Prop('value')) then
            begin
              FOwner := LOwner;
              FIndex := nil;
              FSymbols := nil;
              FAttempted := False;
              FPage := 0;
              LFound := True;
              Break;
            end;
          end;

          if not LFound then
          begin
            raise ENyxModel.Create('Choose a captured owning unit for the outline');
          end;
        end;
      poaFind:
        begin
          FFilter := FQuery;
          FPage := 0;
        end;
      poaPrevious:
        begin

          if FPage > 0 then
          begin
            Dec(FPage);
          end;
        end;
      poaNext:
        begin

          if FPage < 4096 div NyxPascalOutlinePageSize then
          begin
            Inc(FPage);
          end;
        end;
      poaNavigate:
        begin

          if FContent = pocDeclarations then
          begin

            if (FSymbols = nil) or (LRow >= FSymbols.SymbolCount) then
            begin
              raise ENyxModel.Create('This declaration is no longer in the captured outline');
            end;
            ALocation := FSymbols.SymbolAt(LRow).Location;
          end
          else
          begin

            if (FIndex = nil) or (LRow >= FIndex.Count) then
            begin
              raise ENyxModel.Create('This routine is no longer in the captured outline');
            end;
            ALocation := NyxPascalOutlineLocation(FIndex.Item(LRow), LPart);
          end;

          if not ALocation.IsDefined then
          begin
            raise ENyxModel.Create('This routine has no unambiguous physical location');
          end;

          if (FSources = nil) or (ASession.DraftFileText(ALocation.FileReference) <>
            FSources.Text(ALocation.FileReference)) then
          begin
            raise ENyxModel.Create('This file has unfinished Pascal. Apply or restore it before navigating');
          end;
          ASession.ActiveSourceFile := ALocation.FileReference;
          Result := soeNavigate;
        end;
      poaNone:
        begin
          { The early ownership decoder already excluded unrelated controls. }
        end;
    end;
  except
    on LException: Exception do
    begin
      ALocation := Default(TNyxPascalRoutineLocation);
      {$ifdef PAS2JS}
      FMessage := LException.Message;
      {$else}
      FMessage := UTF8Encode(UnicodeString(LException.Message));
      {$endif}
      Result := soeRebuild;
    end;
  end;
end;

function NewNyxStudioSourceOutline: INyxStudioSourceOutline;
begin
  Result := TSourceOutline.Create;
end;

end.
