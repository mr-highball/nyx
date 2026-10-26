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
unit nyx.data.toolbar.query;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.data, nyx.controls, nyx.types,
  nyx.collections.query.controls, nyx.collections.query.session;

const
  NyxDataToolbarQueryContextPart = 'query-context';
  NyxDataToolbarQueryStatusPart = 'query-context/status';
  NyxDataToolbarQueryDetailsPart = 'query-context/details';

type
  { Presentation is independent of query authority. Hidden suppresses the group;
    Summary adds truthful counts/ownership; Details also supplies an invoker. }
  TNyxDataToolbarQueryDisplay = (ntqdHidden, ntqdSummary, ntqdDetails);
  { Immutable optional presentation data. Absence preserves old source/codec
    defaults. Explicit choices and captions contain no view, actor, executable
    token or platform resource. Copies remain valid after a document retires. }
  TNyxDataToolbarQueryOptions = record
  private
    FDefined: Boolean;
    FDisplay: TNyxDataToolbarQueryDisplay;
    FCaption: TNyxText;
    FButton: TNyxText;
  public
    function Display(AValue: TNyxDataToolbarQueryDisplay): TNyxDataToolbarQueryOptions;
    function Caption(const AValue: TNyxText): TNyxDataToolbarQueryOptions;
    function ButtonText(const AValue: TNyxText): TNyxDataToolbarQueryOptions;
    function Copy: TNyxDataToolbarQueryOptions;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxDataToolbarQueryOptions; static;
    property Defined: Boolean read FDefined;
    property DisplayMode: TNyxDataToolbarQueryDisplay read FDisplay;
    property StatusCaption: TNyxText read FCaption;
    property ButtonCaption: TNyxText read FButton;
  end;

{ Explicit opt-in defaults to a summary plus Details. A default record remains
  absent so historical toolbar source and version-one options keep their meaning. }
function NyxDataToolbarQueryOptions: TNyxDataToolbarQueryOptions;
{ Public compound content uses ordinary Nyx controls. IDs belong to this owned
  subtree; the caller checks document-wide uniqueness before adoption. Visibility
  is an authored default, still subject to the caller's ordinary presentation
  conditions. No query authority or runtime observation is stored in these faces. }
function NewNyxDataToolbarQueryContent(const AID: TNyxText;
  const AOptions: TNyxDataToolbarQueryOptions): INyxColumn;
{ Bounded copied observation only. Other includes connected peers and retained
  contributors, avoiding any false cached liveness claim between query changes.
  Open Details classifies retained ownership from its fresh session snapshot. }
function NyxDataToolbarQuerySummary(const AObservation: TNyxQueryWorkbenchObservation;
  const ALocalOwner: TNyxQueryOwnerID; const AOptions: TNyxDataToolbarQueryOptions): TNyxText;

implementation

uses SysUtils, nyx.collections;

const
  CDisplays: array[TNyxDataToolbarQueryDisplay] of TNyxText = ('hidden', 'summary', 'details');

procedure ValidateCaption(const AValue: TNyxText);
var
  LIndex: Integer;
  LScalar: Integer;
  LCount: Integer;
begin
  LIndex := 1;
  LCount := 0;
  while LIndex <= Length(AValue) do
  begin

    if not NyxNextScalar(AValue, LIndex, LScalar) or (LScalar = 0) then
    begin
      raise ENyxCollection.Create('Query captions require valid Unicode text');
    end;
    Inc(LCount);

    if LCount > 256 then
    begin
      raise ENyxCollection.Create('Query captions are limited to 256 Unicode scalars');
    end;
  end;
end;

function NyxDataToolbarQueryOptions: TNyxDataToolbarQueryOptions;
begin
  Result := Default(TNyxDataToolbarQueryOptions);
  Result.FDefined := True;
  Result.FDisplay := ntqdDetails;
  Result.FCaption := 'Results';
  Result.FButton := 'Filters and sorting';
end;

function TNyxDataToolbarQueryOptions.Copy: TNyxDataToolbarQueryOptions;
begin
  Validate;
  Result := Self;
end;

procedure TNyxDataToolbarQueryOptions.Validate;
begin

  if not FDefined then
  begin
    Exit;
  end;

  if (Ord(FDisplay) < Ord(Low(TNyxDataToolbarQueryDisplay))) or
    (Ord(FDisplay) > Ord(High(TNyxDataToolbarQueryDisplay))) then
  begin
    raise ENyxCollection.Create('Unknown toolbar query presentation');
  end;
  ValidateCaption(FCaption);
  ValidateCaption(FButton);
end;

function TNyxDataToolbarQueryOptions.Display(AValue: TNyxDataToolbarQueryDisplay): TNyxDataToolbarQueryOptions;
begin
  Result := Self;

  if not FDefined then
  begin
    Result := NyxDataToolbarQueryOptions;
  end;
  Result.FDisplay := AValue;
  Result.Validate;
end;

function TNyxDataToolbarQueryOptions.Caption(const AValue: TNyxText): TNyxDataToolbarQueryOptions;
begin
  Result := Copy;

  if not Result.Defined then
  begin
    Result := NyxDataToolbarQueryOptions;
  end;
  Result.FCaption := AValue;
  Result.Validate;
end;

function TNyxDataToolbarQueryOptions.ButtonText(const AValue: TNyxText): TNyxDataToolbarQueryOptions;
begin
  Result := Copy;

  if not Result.Defined then
  begin
    Result := NyxDataToolbarQueryOptions;
  end;
  Result.FButton := AValue;
  Result.Validate;
end;

function TNyxDataToolbarQueryOptions.ToData: TNyxDataValue;
begin
  Validate;
  Result := NyxNull;

  if FDefined then
  begin
    Result := NyxObject([NyxField('version', NyxData(1)),
      NyxField('display', NyxData(CDisplays[FDisplay])),
      NyxField('caption', NyxData(FCaption)), NyxField('button', NyxData(FButton))]);
  end;
end;

class function TNyxDataToolbarQueryOptions.FromData(const AData: TNyxDataValue): TNyxDataToolbarQueryOptions;
var
  LDisplay: TNyxDataToolbarQueryDisplay;
  LFound: Boolean;
begin
  Result := Default(TNyxDataToolbarQueryOptions);

  if AData.Kind = ndNull then
  begin
    Exit;
  end;

  if (AData.Kind <> ndObject) or (AData.Count <> 4) or
    (AData.Field('version').AsInteger <> 1) then
  begin
    raise ENyxCollection.Create('Toolbar query presentation requires its exact version-one object');
  end;
  LFound := False;
  for LDisplay := Low(TNyxDataToolbarQueryDisplay) to High(TNyxDataToolbarQueryDisplay) do
  begin

    if AData.Field('display').AsText = CDisplays[LDisplay] then
    begin
      Result := NyxDataToolbarQueryOptions.Display(LDisplay).
        Caption(AData.Field('caption').AsText).ButtonText(AData.Field('button').AsText);
      LFound := True;
    end;
  end;

  if not LFound then
  begin
    raise ENyxCollection.Create('Unknown toolbar query presentation');
  end;
end;

function NewNyxDataToolbarQueryContent(const AID: TNyxText;
  const AOptions: TNyxDataToolbarQueryOptions): INyxColumn;
var
  LStatus: INyxLabel;
  LDetails: INyxButton;
begin
  AOptions.Validate;
  Result := NewNyxColumn(AID);
  Result.Configure.PartName(NyxPart(NyxDataToolbarQueryContextPart)).Gap(6).Flex(1).
    Visible(AOptions.DisplayMode <> ntqdHidden).Done;
  LStatus := NewNyxLabel(AID + '-status');
  LStatus.Configure.PartName(NyxPart('status')).Text(AOptions.StatusCaption).
    AccessibleName('Current query results').Done;
  Result.Add(LStatus);
  LDetails := NewNyxButton(AID + '-details');
  LDetails.Configure.PartName(NyxPart('details')).Text(AOptions.ButtonCaption).
    AccessibleName('Show current filters and sorting').
    Visible(AOptions.DisplayMode = ntqdDetails).Done;
  Result.Add(LDetails);
end;

function NyxDataToolbarQuerySummary(const AObservation: TNyxQueryWorkbenchObservation;
  const ALocalOwner: TNyxQueryOwnerID; const AOptions: TNyxDataToolbarQueryOptions): TNyxText;
var
  LLocal: Integer;
  LIndex: Integer;
begin
  AOptions.Validate;
  LLocal := 0;
  for LIndex := 0 to AObservation.Count - 1 do
  begin

    if ALocalOwner.Defined and
      (AObservation.ContributionAt(LIndex).Owner.ID.Value = ALocalOwner.Value) then
    begin
      Inc(LLocal);
    end;
  end;
  Result := TNyxText(IntToStr(AObservation.MatchedRows)) + ' of ' +
    TNyxText(IntToStr(AObservation.SourceRows)) + ' rows. ' +
    TNyxText(IntToStr(LLocal)) + ' filters from this toolbar; ' +
    TNyxText(IntToStr(AObservation.Count - LLocal)) + ' from other or earlier controls. ';

  if AObservation.HasUserSort then
  begin

    if ALocalOwner.Defined and (AObservation.SortOwner.ID.Value = ALocalOwner.Value) then
    begin
      Result := Result + 'This toolbar owns sorting.';
    end
    else
    begin
      Result := Result + 'Another control owns sorting.';
    end;
  end
  else
  begin
    Result := Result + 'Original ordering.';
  end;

  if LLocal = 0 then
  begin
    Result := Result + ' Input entries are separate from active filters.';
  end;

  if AOptions.StatusCaption <> '' then
  begin
    Result := AOptions.StatusCaption + ': ' + Result;
  end;
end;

end.
