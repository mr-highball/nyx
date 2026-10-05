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

program nyx_property_benchmark;

{$mode delphi}{$H+}
{$codepage utf8}

{ Measure complete shared Studio shell composition, selection/property queries
  and full admission on unchanged 128/512/2048-control projects. No model cache
  or abbreviated validator is used. Timing excludes fixture setup and assertions;
  rendering, physical input and source editing have separate maintained harnesses.
  --snapshot emits every public metadata field in catalog/descriptor order for
  exact before/after comparison, including both platform scopes. }

uses
  SysUtils,
  {$ifdef PAS2JS}
  Web,
  {$endif}
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.model,
  nyx.controls,
  nyx.catalog,
  nyx.schema,
  nyx.codec,
  nyx.studio.session,
  nyx.studio.view;

function Clock: Double;
begin
  {$ifdef PAS2JS}
  Result := window.performance.now;
  {$else}
  Result := GetTickCount64;
  {$endif}
end;

function Number(AValue: Double): TNyxText;
var
  LFormat: TFormatSettings;
begin
  {$ifdef PAS2JS}
  LFormat := TFormatSettings.Invariant;
  {$else}
  LFormat := DefaultFormatSettings;
  {$endif}
  LFormat.DecimalSeparator := '.';
  Result := FloatToStrF(AValue, ffFixed, 12, 3, LFormat);
end;

procedure Require(ACondition: Boolean; const AMessage: TNyxText);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('Property benchmark: ' + AMessage);
  end;
end;

function MetadataSnapshot: TNyxText;
var
  LCatalog: TNyxCatalog;
  LNode: TNyxNode;
  LInfos: TNyxPropertyInfos;
  LParts: TNyxStrings;
  LIndex: Integer;
  LProperty: Integer;
  LInfo: TNyxPropertyInfo;
begin
  LCatalog := TNyxCatalog.Create;
  LParts := TNyxStrings.Create;
  try
    for LIndex := 0 to LCatalog.Count - 1 do
    begin
      LNode := LCatalog.NewNode(LCatalog[LIndex].Kind, 'metadata');
      try
        LNode.Configure.ForPlatform(npfBrowser).Width(320).Done;
        LNode.Configure.ForPlatform(npfNativeLCL).Height(240).Done;
        LInfos := NyxProperties(LNode);
        for LProperty := 0 to High(LInfos) do
        begin
          LInfo := LInfos[LProperty];
          LParts.Add(NyxObject([
            NyxField('kind', NyxData(LNode.Kind)),
            NyxField('ordinal', NyxData(LProperty)),
            NyxField('key', NyxData(LInfo.Key)),
            NyxField('title', NyxData(LInfo.Title)),
            NyxField('type', NyxData(Ord(LInfo.ValueType))),
            NyxField('default', NyxData(LInfo.DefaultValue)),
            NyxField('choices', NyxData(LInfo.Choices)),
            NyxField('minimum', NyxData(LInfo.Minimum)),
            NyxField('maximum', NyxData(LInfo.Maximum)),
            NyxField('advanced', NyxData(LInfo.Advanced)),
            NyxField('support', NyxData(LInfo.Support.Defined)),
            NyxField('meaning', NyxData(Ord(LInfo.Support.Meaning))),
            NyxField('browser', NyxData(Ord(LInfo.Support.Browser))),
            NyxField('native', NyxData(Ord(LInfo.Support.Native))),
            NyxField('description', NyxData(LInfo.Support.Description))
          ]).ToJSON + #10);
        end;
      finally
        LNode.Free;
      end;
    end;
    Result := LParts.Join;
  finally
    LParts.Free;
    LCatalog.Free;
  end;
end;

function Measure(AControls: Integer): TNyxText;
var
  LDocument: TNyxDocument;
  LSession: TNyxStudioSession;
  LPage: INyxPage;
  LShell: TNyxDocument;
  LState: TNyxStudioViewState;
  LInfos: TNyxPropertyInfos;
  LBefore: TNyxText;
  LIndex: Integer;
  LStart: Double;
  LAdmission: Double;
  LQueries: Double;
  LCompose: Double;
begin
  LDocument := TNyxDocument.Create;
  LSession := TNyxStudioSession.Create;
  try
    LDocument.Title := 'Property workspace';
    LPage := NewNyxPage('home');
    for LIndex := 0 to AControls - 1 do
    begin
      LPage.Add(NewNyxLabel('caption-' + IntToStr(LIndex))
        .WithText('Caption ' + IntToStr(LIndex)));
    end;
    LDocument.AddPage(LPage);
    LSession.Load(TNyxCodec.Encode(LDocument));
    LBefore := LSession.Source;
    LStart := Clock;
    ValidateNyxDocumentProperties(LSession.Document);
    LAdmission := Clock - LStart;
    LStart := Clock;
    for LIndex := 0 to 63 do
    begin
      LSession.Select('caption-' + IntToStr(AControls - 1 - LIndex));
      LInfos := NyxProperties(LSession.Selected, LSession.Document);
    end;
    LQueries := Clock - LStart;
    Require((Length(LInfos) >= Ord(High(TNyxAttribute)) + 1) and
      (LInfos[0].Key = 'text') and LInfos[0].Support.Defined,
      'ordinary selected inspector exposes complete typed help');
    LState := DefaultNyxStudioViewState;
    LState.CodeVisible := True;
    LState.AdvancedProperties := True;
    LState.Panel := nspInspector;
    LStart := Clock;
    LShell := BuildNyxStudioView(LSession, LState);
    LCompose := Clock - LStart;
    try
      Require(LShell.Find('inspector-text') <> nil, 'public shell composes its text field');
      Require(LShell.Find('inspector-text').Prop('value') = 'Caption ' +
        IntToStr(AControls - 64), 'selected value reaches ordinary Studio inspector');
      Require(LShell.Find('inspector-width') <> nil, 'expanded typed properties remain visible');
      Require(Pos('Browser:', LShell.Find('inspector-text').Prop('hint')) > 0,
        'full adapter support reaches the ordinary inspector');
    finally
      LShell.Free;
    end;
    Require((LSession.Source = LBefore) and (LSession.SourceDraftBase = ''),
      'query and shell composition preserve accepted source/draft');
    Result := IntToStr(AControls) + ',64,' + Number(LAdmission) + ',' +
      Number(LQueries) + ',' + Number(LCompose) + #10;
  finally
    LSession.Free;
    LDocument.Free;
  end;
end;

var
  LOutput: TNyxText;
  LSnapshot: Boolean;
begin
  try
    {$ifdef PAS2JS}
    LSnapshot := window.location.search = '?snapshot';
    {$else}
    LSnapshot := (ParamCount > 0) and (ParamStr(1) = '--snapshot');
    {$endif}

    if LSnapshot then
    begin
      LOutput := MetadataSnapshot;
    end
    else
    begin
      LOutput := 'controls,queries,admission_ms,selection_metadata_ms,studio_compose_ms' + #10 +
        Measure(128) + Measure(512) + Measure(2048);
    end;
    {$ifdef PAS2JS}
    document.body.textContent := LOutput;
    document.body.setAttribute('data-property-benchmark', 'passed');
    {$else}
    Write(LOutput);
    {$endif}
  except
    on LException: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.textContent := 'FAIL ' + LException.Message;
      document.body.setAttribute('data-property-benchmark', 'failed');
      {$else}
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
      {$endif}
    end;
  end;
end.
