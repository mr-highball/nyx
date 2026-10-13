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

unit nyx.tabs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.types, nyx.model, nyx.controls, nyx.data, nyx.state, nyx.behavior;

type
  TNyxTabActivation = (ntaAutomatic, ntaManual);
  TNyxTabOrientation = (ntoHorizontal, ntoVertical);
  TNyxTabMovement = (ntmPrevious, ntmNext, ntmFirst, ntmLast);
  { Immutable authoring policy. Initial names a direct tab child, independently
    of its caption. Defaults select the first available tab, horizontal automatic
    activation and wrapping focus. Wire names exist only in ToData/FromData. }
  TNyxTabsOptions = record
  private
    FInitial: TNyxControlRef;
    FActivation: TNyxTabActivation;
    FOrientation: TNyxTabOrientation;
    FWrap: Boolean;
  public
    class function Defaults: TNyxTabsOptions; static;
    function Initial(const ATab: TNyxControlRef): TNyxTabsOptions;
    function Activation(AValue: TNyxTabActivation): TNyxTabsOptions;
    function Orientation(AValue: TNyxTabOrientation): TNyxTabsOptions;
    function Wrap(AValue: Boolean): TNyxTabsOptions;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AValue: TNyxDataValue): TNyxTabsOptions; static;
    property InitialTab: TNyxControlRef read FInitial;
    property ActivationMode: TNyxTabActivation read FActivation;
    property OrientationMode: TNyxTabOrientation read FOrientation;
    property WrapFocus: Boolean read FWrap;
  end;
  { Copied entry, without a model/control pointer. Hidden headers disappear;
    disabled headers remain descriptive but cannot receive selection. Changing
    these authored restrictions requires a newly admitted state. }
  TNyxTabEntry = record
    Reference: TNyxControlRef;
    { Authored identity within this host. Binding never stores a qualified
      runtime ID, so independent reusable instances share a portable value. }
    SourceReference: TNyxControlRef;
    Caption: TNyxText;
    Visible: Boolean;
    Enabled: Boolean;
    Available: Boolean;
  end;
  { Value-only transition. -1 denotes no available tab. Focus and selection are
    deliberately independent for manual activation. Unchanged moves have both
    flags False; adapters may still handle a boundary key without a UI rebuild. }
  TNyxTabTransition = record
    BeforeFocus: Integer;
    AfterFocus: Integer;
    BeforeSelection: Integer;
    AfterSelection: Integer;
    FocusChanged: Boolean;
    SelectionChanged: Boolean;
  end;
  { UI-owned shared interaction state. Constructor borrows the descriptor only
    during admission, copies direct tab identities/captions/restrictions and
    refuses invalid children, duplicate identities or unavailable initial intent.
    Caller owns this object; no source document/view or observer is retained.
    Entries and indices are read-only. Move/Focus/Activate return one complete
    transition and never touch model properties or invoke application callbacks. }
  TNyxTabState = class
  private
    FOptions: TNyxTabsOptions;
    FEntries: array of TNyxTabEntry;
    FFocus: Integer;
    FSelection: Integer;
    FDefaultSelection: Integer;
    function GetCount: Integer;
    function Transition(AFocus, ASelection: Integer): TNyxTabTransition;
    function RequireAvailable(const ATab: TNyxControlRef): Integer;
  public
    constructor Create(ANode: TNyxNode);
    { Value-only copy. An index outside this immutable entry snapshot raises. }
    function Entry(AIndex: Integer): TNyxTabEntry;
    { Exact identity lookup; -1 means absent, independently of availability. }
    function IndexOf(const ATab: TNyxControlRef): Integer;
    { Focus also selects in automatic mode. Missing/unavailable intent refuses
      before either index changes; manual mode preserves the selected panel. }
    function Focus(const ATab: TNyxControlRef): TNyxTabTransition;
    { Explicit pointer/programmatic activation changes both indices atomically. }
    function Activate(const ATab: TNyxControlRef): TNyxTabTransition;
    { Manual keyboard activation; an empty state is a harmless unchanged intent. }
    function ActivateFocused: TNyxTabTransition;
    { Skip unavailable entries, honoring wrapping and orientation-independent
      logical edges. The adapter maps physical keys; no callback is entered here. }
    function Move(AMovement: TNyxTabMovement): TNyxTabTransition;
    { Proposals preserve the accepted indices. A renderer admits the complete
      state candidate before Commit and before changing retained presentation. }
    function ProposeFocus(const ATab: TNyxControlRef): TNyxTabTransition;
    function ProposeActivation(const ATab: TNyxControlRef): TNyxTabTransition;
    function ProposeFocusedActivation: TNyxTabTransition;
    function ProposeMove(AMovement: TNyxTabMovement): TNyxTabTransition;
    { Accept only the original baseline or selection already synchronized by
      the admitting state coordinator. Unrelated/stale transitions refuse. }
    procedure Commit(const AChange: TNyxTabTransition);
    { Explicit scalar boundary: empty means the policy's initial selection;
      otherwise an exact local source identity must name an available child.
      Failure preserves both indices. Programmatic changes move the roving
      entry with selection, without requesting physical keyboard focus. }
    procedure SynchronizeValue(const AValue: TNyxText);
    { Refresh copied captions/availability and the accepted value together.
      Structure/policy changes require remounting. The detached snapshot must
      validate completely before this host's values change. }
    procedure SynchronizeDescriptor(ANode: TNyxNode);
    property Count: Integer read GetCount;
    property FocusIndex: Integer read FFocus;
    property SelectedIndex: Integer read FSelection;
    property Options: TNyxTabsOptions read FOptions;
  end;

const
  NyxTabsOptionsKey = 'nyx.tabs';
  NyxMaximumTabs = 4096;

{ Typed authoring helper on tabs or a reusable reference promised to project as
  tabs. It copies the policy; the complete document validates that promise.
  Child-dependent admission occurs in TNyxTabState, allowing fluent composition
  in any order. }
procedure SetNyxTabsOptions(const ATabs: INyxControl; const AOptions: TNyxTabsOptions);
{ Typed selection authoring. The reference is local to the tabs definition;
  no caption, runtime-qualified identity or closed behavioral string is used. }
procedure SetNyxTabSelection(const ATabs: INyxTabs; const ATab: TNyxControlRef);
{ Text-state boundary for an open application identity. Suitable for typed
  defaults/assignments; admission resolves availability in each bound host. }
function NyxTabValue(const ATab: TNyxControlRef): TNyxText;
{ Read a declared policy, or the independent default. Malformed known data raises
  rather than silently changing interaction. Nil/non-tabs descriptors refuse. }
function NyxTabsOptions(ANode: TNyxNode): TNyxTabsOptions;
{ Standalone value-owned notification helper; it installs no model property.
  Pass the local source reference. Mounted renderers instead use the binding
  coordinator to admit state first and emit its accepted dispatch afterward. }
function NyxTabChange(ANode: TNyxNode; const ASelected: TNyxControlRef): TNyxDispatch;

implementation

uses
  nyx.application.navigation;

function NyxTabValue(const ATab: TNyxControlRef): TNyxText;
begin
  Result := NyxControl(ATab.ID).ID;
end;

procedure SetNyxTabSelection(const ATabs: INyxTabs; const ATab: TNyxControlRef);
begin

  if ATabs = nil then
  begin
    raise ENyxModel.Create('Tab selection requires a tabs control');
  end;
  ATabs.Configure.Value(NyxTabValue(ATab)).Done;
end;

function NyxTabChange(ANode: TNyxNode; const ASelected: TNyxControlRef): TNyxDispatch;
begin
  Result := DispatchNyxBehavior(ANode, ntChange);
  Result.Info.Value := NyxData(ASelected.ID);
  Result.Info.ValueKind := nskText;
  Result.Info.HasValue := True;
  Result.Info.ValueID := ANode.ID;
  Result.Info.Changed := True;
end;

class function TNyxTabsOptions.Defaults: TNyxTabsOptions;
begin
  Result.FInitial := Default(TNyxControlRef);
  Result.FActivation := ntaAutomatic;
  Result.FOrientation := ntoHorizontal;
  Result.FWrap := True;
end;

function TNyxTabsOptions.Initial(const ATab: TNyxControlRef): TNyxTabsOptions;
begin
  Result := Self;
  Result.FInitial := NyxControl(ATab.ID);
end;

function TNyxTabsOptions.Activation(AValue: TNyxTabActivation): TNyxTabsOptions;
begin
  Result := Self;
  Result.FActivation := AValue;
  Result.Validate;
end;

function TNyxTabsOptions.Orientation(AValue: TNyxTabOrientation): TNyxTabsOptions;
begin
  Result := Self;
  Result.FOrientation := AValue;
  Result.Validate;
end;

function TNyxTabsOptions.Wrap(AValue: Boolean): TNyxTabsOptions;
begin
  Result := Self;

  if AValue then
  begin
    Result.FWrap := True;
  end
  else
  begin
    Result.FWrap := False;
  end;
end;

procedure TNyxTabsOptions.Validate;
begin

  if (Ord(FActivation) < Ord(Low(TNyxTabActivation))) or
    (Ord(FActivation) > Ord(High(TNyxTabActivation))) or
    (Ord(FOrientation) < Ord(Low(TNyxTabOrientation))) or
    (Ord(FOrientation) > Ord(High(TNyxTabOrientation))) then
  begin
    raise ENyxModel.Create('Unknown tab activation or orientation');
  end;

  if FInitial.ID <> '' then
  begin
    NyxControl(FInitial.ID);
  end;
end;

function TNyxTabsOptions.ToData: TNyxDataValue;
var
  LInitial: TNyxDataValue;
  LActivation: TNyxText;
  LOrientation: TNyxText;
begin
  Validate;
  LInitial := NyxNull;

  if FInitial.ID <> '' then
  begin
    LInitial := NyxData(FInitial.ID);
  end;
  LActivation := 'automatic';

  if FActivation = ntaManual then
  begin
    LActivation := 'manual';
  end;
  LOrientation := 'horizontal';

  if FOrientation = ntoVertical then
  begin
    LOrientation := 'vertical';
  end;
  Result := NyxObject([NyxField('version', NyxData(1)), NyxField('initial', LInitial),
    NyxField('activation', NyxData(LActivation)), NyxField('orientation', NyxData(LOrientation)),
    NyxField('wrap', NyxData(FWrap))]);
end;

class function TNyxTabsOptions.FromData(const AValue: TNyxDataValue): TNyxTabsOptions;
var
  LIndex: Integer;
  LName: TNyxText;
  LChoice: TNyxText;
begin
  Result := Defaults;

  if (AValue.Kind <> ndObject) or (AValue.Count <> 5) then
  begin
    raise ENyxModel.Create('Tab policy requires its exact version-one fields');
  end;
  for LIndex := 0 to AValue.Count - 1 do
  begin
    LName := AValue.Key(LIndex);

    if (LName <> 'version') and (LName <> 'initial') and (LName <> 'activation') and
      (LName <> 'orientation') and (LName <> 'wrap') then
    begin
      raise ENyxModel.Create('Unknown tab policy field');
    end;
  end;

  if AValue.Field('version').AsInteger <> 1 then
  begin
    raise ENyxModel.Create('Unsupported tab policy version');
  end;

  if AValue.Field('initial').Kind <> ndNull then
  begin
    Result.FInitial := NyxControl(AValue.Field('initial').AsText);
  end;
  LChoice := AValue.Field('activation').AsText;

  if LChoice = 'manual' then
  begin
    Result.FActivation := ntaManual;
  end
  else if LChoice <> 'automatic' then
  begin
    raise ENyxModel.Create('Unknown tab activation');
  end;
  LChoice := AValue.Field('orientation').AsText;

  if LChoice = 'vertical' then
  begin
    Result.FOrientation := ntoVertical;
  end
  else if LChoice <> 'horizontal' then
  begin
    raise ENyxModel.Create('Unknown tab orientation');
  end;
  Result.FWrap := AValue.Field('wrap').AsBoolean;
  Result.Validate;
end;

procedure SetNyxTabsOptions(const ATabs: INyxControl; const AOptions: TNyxTabsOptions);
var
  LAdmitted: TNyxTabsOptions;
begin

  if (ATabs = nil) or not ((ATabs.Node.ProjectionKind = 'tabs') or
    (ATabs.Node.ProjectionKind = 'component')) then
  begin
    raise ENyxModel.Create('Tab policy authoring requires tabs or a reusable reference');
  end;
  LAdmitted := TNyxTabsOptions.FromData(AOptions.ToData);
  ATabs.Node.Extensions.SetValue(NyxExtension(NyxTabsOptionsKey), LAdmitted.ToData);
end;

function NyxTabsOptions(ANode: TNyxNode): TNyxTabsOptions;
begin

  if (ANode = nil) or (ANode.ProjectionKind <> 'tabs') then
  begin
    raise ENyxModel.Create('Tab policy requires a tabs descriptor');
  end;
  Result := TNyxTabsOptions.Defaults;

  if ANode.Extensions.Has(NyxExtension(NyxTabsOptionsKey)) then
  begin
    Result := TNyxTabsOptions.FromData(ANode.Extensions.Value(NyxExtension(NyxTabsOptionsKey)));
  end;
end;

constructor TNyxTabState.Create(ANode: TNyxNode);
var
  LIndex: Integer;
  LPrevious: Integer;
  LInitial: Integer;

  function Restriction(AChild: TNyxNode; AAttribute: TNyxAttribute): Boolean;
  var
    LText: TNyxText;
  begin
    LText := AChild.Prop(NyxAttributeName(AAttribute), 'true');

    if (LText <> 'true') and (LText <> 'false') then
    begin
      raise ENyxModel.Create('Tab availability requires an exact Boolean');
    end;
    Result := LText = 'true';
  end;
begin
  inherited Create;
  FFocus := -1;
  FSelection := -1;
  FOptions := NyxTabsOptions(ANode);

  if ANode.Count > NyxMaximumTabs then
  begin
    raise ENyxModel.Create('Tab count exceeds the supported bound');
  end;
  SetLength(FEntries, ANode.Count);
  for LIndex := 0 to ANode.Count - 1 do
  begin

    if ANode.Children[LIndex].ProjectionKind <> 'tab' then
    begin
      raise ENyxModel.Create('Tabs require direct tab children');
    end;
    FEntries[LIndex].Reference := NyxControl(ANode.Children[LIndex].ID);
    FEntries[LIndex].SourceReference := NyxControl(ANode.Children[LIndex].SourceID);
    FEntries[LIndex].Caption := NyxPageCaption(ANode.Children[LIndex]);
    FEntries[LIndex].Enabled := Restriction(ANode.Children[LIndex], atEnabled);
    FEntries[LIndex].Visible := Restriction(ANode.Children[LIndex], atVisible);
    FEntries[LIndex].Available := FEntries[LIndex].Enabled and FEntries[LIndex].Visible;
    for LPrevious := 0 to LIndex - 1 do
    begin

      if FEntries[LPrevious].Reference.ID = FEntries[LIndex].Reference.ID then
      begin
        raise ENyxModel.Create('Tabs require distinct direct child identities');
      end;
    end;

    if (FSelection < 0) and FEntries[LIndex].Available then
    begin
      FSelection := LIndex;
    end;
  end;

  if FOptions.InitialTab.ID <> '' then
  begin
    LInitial := IndexOf(FOptions.InitialTab);

    if (LInitial < 0) and ANode.IsRealized then
    begin
      { A reusable recipe retains authored local references while composition
        qualifies runtime IDs. Resolve only this host's direct SourceID scope;
        never search another instance or use its editor-selecting DesignID. }
      for LIndex := 0 to ANode.Count - 1 do
      begin

        if ANode.Children[LIndex].SourceID = FOptions.InitialTab.ID then
        begin

          if LInitial >= 0 then
          begin
            raise ENyxModel.Create('Initial tab reference is ambiguous in this instance');
          end;
          LInitial := LIndex;
        end;
      end;
    end;

    if LInitial < 0 then
    begin
      raise ENyxModel.Create('The initial tab is missing from this instance');
    end;
    FSelection := RequireAvailable(FEntries[LInitial].Reference);
  end;
  FFocus := FSelection;
  FDefaultSelection := FSelection;

  if ANode.Props.IndexOfName(NyxAttributeName(atValue)) >= 0 then
  begin
    SynchronizeValue(ANode.Prop(NyxAttributeName(atValue)));
  end;
end;

function TNyxTabState.GetCount: Integer;
begin
  Result := Length(FEntries);
end;

function TNyxTabState.Entry(AIndex: Integer): TNyxTabEntry;
begin

  if (AIndex < 0) or (AIndex >= Count) then
  begin
    raise ENyxModel.Create('Tab entry index is outside its snapshot');
  end;
  Result := FEntries[AIndex];
end;

function TNyxTabState.IndexOf(const ATab: TNyxControlRef): Integer;
var
  LIndex: Integer;
begin
  Result := -1;
  for LIndex := 0 to High(FEntries) do
  begin

    if FEntries[LIndex].Reference.ID = ATab.ID then
    begin
      Exit(LIndex);
    end;
  end;
end;

function TNyxTabState.RequireAvailable(const ATab: TNyxControlRef): Integer;
begin
  Result := IndexOf(ATab);

  if (Result < 0) or not FEntries[Result].Available then
  begin
    raise ENyxModel.Create('The requested tab is missing or unavailable');
  end;
end;

function TNyxTabState.Transition(AFocus, ASelection: Integer): TNyxTabTransition;
begin
  Result.BeforeFocus := FFocus;
  Result.BeforeSelection := FSelection;
  Result.AfterFocus := AFocus;
  Result.AfterSelection := ASelection;
  Result.FocusChanged := FFocus <> AFocus;
  Result.SelectionChanged := FSelection <> ASelection;
  FFocus := AFocus;
  FSelection := ASelection;
end;

function TNyxTabState.Focus(const ATab: TNyxControlRef): TNyxTabTransition;
var
  LFocus: Integer;
  LSelection: Integer;
begin
  LFocus := RequireAvailable(ATab);
  LSelection := FSelection;

  if FOptions.ActivationMode = ntaAutomatic then
  begin
    LSelection := LFocus;
  end;
  Result := Transition(LFocus, LSelection);
end;

function TNyxTabState.Activate(const ATab: TNyxControlRef): TNyxTabTransition;
var
  LIndex: Integer;
begin
  LIndex := RequireAvailable(ATab);
  Result := Transition(LIndex, LIndex);
end;

function TNyxTabState.ActivateFocused: TNyxTabTransition;
begin
  Result := Transition(FFocus, FFocus);
end;

function TNyxTabState.Move(AMovement: TNyxTabMovement): TNyxTabTransition;
var
  LIndex: Integer;
  LStep: Integer;
  LVisited: Integer;
begin

  if (Ord(AMovement) < Ord(Low(TNyxTabMovement))) or
    (Ord(AMovement) > Ord(High(TNyxTabMovement))) then
  begin
    raise ENyxModel.Create('Unknown tab focus movement');
  end;
  Result := Transition(FFocus, FSelection);

  if Count = 0 then
  begin
    Exit;
  end;
  LStep := 1;
  LIndex := FFocus;
  case AMovement of
    ntmNext:
    begin
      LStep := 1;
    end;
    ntmFirst:
    begin
      LIndex := -1;
    end;
    ntmLast:
    begin
      LIndex := Count;
      LStep := -1;
    end;
    ntmPrevious:
    begin
      LStep := -1;
    end;
  end;
  for LVisited := 0 to Count - 1 do
  begin
    Inc(LIndex, LStep);

    if (LIndex < 0) or (LIndex >= Count) then
    begin

      if not FOptions.WrapFocus or (AMovement in [ntmFirst, ntmLast]) then
      begin
        Exit;
      end;

      if LIndex < 0 then
      begin
        LIndex := Count - 1;
      end
      else
      begin
        LIndex := 0;
      end;
    end;

    if FEntries[LIndex].Available then
    begin
      Exit(Focus(FEntries[LIndex].Reference));
    end;
  end;
end;

function TNyxTabState.ProposeFocus(const ATab: TNyxControlRef): TNyxTabTransition;
var
  LFocus: Integer;
  LSelection: Integer;
begin
  LFocus := FFocus;
  LSelection := FSelection;
  try
    Result := Focus(ATab);
  finally
    FFocus := LFocus;
    FSelection := LSelection;
  end;
end;

function TNyxTabState.ProposeActivation(const ATab: TNyxControlRef): TNyxTabTransition;
var
  LFocus: Integer;
  LSelection: Integer;
begin
  LFocus := FFocus;
  LSelection := FSelection;
  try
    Result := Activate(ATab);
  finally
    FFocus := LFocus;
    FSelection := LSelection;
  end;
end;

function TNyxTabState.ProposeFocusedActivation: TNyxTabTransition;
var
  LFocus: Integer;
  LSelection: Integer;
begin
  LFocus := FFocus;
  LSelection := FSelection;
  try
    Result := ActivateFocused;
  finally
    FFocus := LFocus;
    FSelection := LSelection;
  end;
end;

function TNyxTabState.ProposeMove(AMovement: TNyxTabMovement): TNyxTabTransition;
var
  LFocus: Integer;
  LSelection: Integer;
begin
  LFocus := FFocus;
  LSelection := FSelection;
  try
    Result := Move(AMovement);
  finally
    FFocus := LFocus;
    FSelection := LSelection;
  end;
end;

procedure TNyxTabState.Commit(const AChange: TNyxTabTransition);
begin

  if not (((FFocus = AChange.BeforeFocus) and
    (FSelection = AChange.BeforeSelection)) or
    ((FFocus = AChange.AfterSelection) and
    (FSelection = AChange.AfterSelection))) then
  begin
    raise ENyxModel.Create('The tab proposal no longer matches its accepted state');
  end;

  if ((AChange.AfterFocus < -1) or (AChange.AfterFocus >= Count)) or
    ((AChange.AfterSelection < -1) or (AChange.AfterSelection >= Count)) then
  begin
    raise ENyxModel.Create('A tab proposal has an invalid entry index');
  end;

  if ((AChange.AfterFocus >= 0) and not FEntries[AChange.AfterFocus].Available) or
    ((AChange.AfterSelection >= 0) and not FEntries[AChange.AfterSelection].Available) then
  begin
    raise ENyxModel.Create('A tab proposal targets an unavailable entry');
  end;
  FFocus := AChange.AfterFocus;
  FSelection := AChange.AfterSelection;
end;

procedure TNyxTabState.SynchronizeValue(const AValue: TNyxText);
var
  LIndex: Integer;
  LSelected: Integer;
begin
  LSelected := FDefaultSelection;

  if AValue <> '' then
  begin
    LSelected := -1;
    for LIndex := 0 to High(FEntries) do
    begin

      if FEntries[LIndex].SourceReference.ID = AValue then
      begin

        if LSelected >= 0 then
        begin
          raise ENyxModel.Create('The selected tab identity is ambiguous');
        end;
        LSelected := LIndex;
      end;
    end;

    if (LSelected < 0) or not FEntries[LSelected].Available then
    begin
      raise ENyxModel.Create('The selected tab is missing or unavailable');
    end;
  end;

  if FSelection <> LSelected then
  begin
    FFocus := LSelected;
    FSelection := LSelected;
  end;
end;

procedure TNyxTabState.SynchronizeDescriptor(ANode: TNyxNode);
var
  LCandidate: TNyxTabState;
  LIndex: Integer;
  LFocus: Integer;
begin
  LCandidate := TNyxTabState.Create(ANode);
  try

    if (LCandidate.Count <> Count) or
      (LCandidate.Options.ToData.ToJSON <> FOptions.ToData.ToJSON) then
    begin
      raise ENyxModel.Create('Changed tab structure or policy requires remounting');
    end;
    for LIndex := 0 to Count - 1 do
    begin

      if (LCandidate.FEntries[LIndex].Reference.ID <> FEntries[LIndex].Reference.ID) or
        (LCandidate.FEntries[LIndex].SourceReference.ID <> FEntries[LIndex].SourceReference.ID) then
      begin
        raise ENyxModel.Create('Changed tab identity requires remounting');
      end;
    end;
    { An unbound host retains its local selection on unrelated refreshes.
      Once edited, its admitted runtime Value is the ordinary source of truth. }

    if (ANode.Props.IndexOfName(NyxAttributeName(atValue)) < 0) and
      (FSelection >= 0) and LCandidate.FEntries[FSelection].Available then
    begin
      LCandidate.FSelection := FSelection;
    end;
    LFocus := LCandidate.FSelection;

    if (LCandidate.FSelection = FSelection) and (FFocus >= 0) and
      LCandidate.FEntries[FFocus].Available then
    begin
      LFocus := FFocus;
    end;
    FEntries := LCandidate.FEntries;
    FDefaultSelection := LCandidate.FDefaultSelection;
    FFocus := LFocus;
    FSelection := LCandidate.FSelection;
  finally
    LCandidate.Free;
  end;
end;

end.
