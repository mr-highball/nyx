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

unit nyx.studio.outputs;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  SysUtils,
  fpjson,
  nyx.text,
  nyx.model;

type
  { Optional machine-local output profiles. These paths never enter a design
    document or its undo history. Empty fields are valid during authoring;
    filesystem/toolchain readiness is checked by the server for a chosen build.
    Profiles are data, not compiler argument lists or shell command strings.
    Independent runtime file opt-ins select launcher adapters; they encode no
    file handle, borrowed host, current user gesture or filesystem path grant. }
  TNyxOutputConfiguration = class
  private
    FFields: TNyxStrings;
    FFileSaving: Boolean;
    FFileOpening: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    { Known omitted fields return empty text. Unknown keys raise a diagnostic,
      keeping field spelling errors distinct from intentionally absent tools. }
    function Field(const AKey: TNyxText): TNyxText;
    { Admits empty values and Unicode paths without checking file existence.
      Rejects unknown keys, control separators and values beyond 4096 units. }
    procedure SetField(const AKey, AValue: TNyxText);
    { Machine-local opt-in for generated application launchers. It grants only
      the target's interactive byte-save host; saved designs remain inert and
      browser permission/activation rules still apply. False is the default.
      This runtime choice does not require an output or installed compiler. }
    function AllowFileSaving(AValue: Boolean = True): TNyxOutputConfiguration;
    property FileSaving: Boolean read FFileSaving;
    { Independent machine-local opt-in for interactive single-file reads in
      generated application launchers. Defaults False; enabling it grants no
      saving authority, path access or browser activation. No output/compiler
      is required to change the policy. Revocation restores legacy encoding. }
    function AllowFileOpening(AValue: Boolean = True): TNyxOutputConfiguration;
    property FileOpening: Boolean read FFileOpening;
    { Deterministic, versioned local JSON; emits every known field, including
      empty ones, so an explicit clear overrides an inherited startup hint. }
    function Encode: TNyxText;
    { Returns an owned configuration. Invalid input frees the partial candidate;
      callers replace their accepted configuration only after successful decode. }
    class function Decode(const ASource: TNyxText): TNyxOutputConfiguration; static;
  end;

  { Closed editor settings, separate from transport field names. The first six
    choices correspond to NyxOutputFields; runtime permissions are independent
    Booleans and never acquire a file capability from a path or target. }
  TNyxOutputSetting = (nosPas2JS, nosRuntime, nosFPC, nosLazarus, nosPlatform,
    nosWidgetset, nosFileSaving, nosFileOpening);

  { Observe fills clean fields; Reload deliberately adopts saved fields that
    were untouched during this request. Save acknowledges its exact submitted
    snapshot. None of these intents selects a target or changes design history. }
  TNyxOutputSyncIntent = (osiObserve, osiReload, osiSave);

  { UI-thread reconciliation for one machine profile and one in-flight request.
    Owns only dirty/touched sets and exact submitted text, never a configuration,
    transport, view or callback. Mark records intent even after an edit returns
    to its old value. Accept borrows fully decoded configurations and updates the
    existing current object; failure/cancellation leaves its values and dirty
    fields intact. Dispose the transport before retiring this state owner. }
  TNyxOutputEditState = class
  private
    FDirty: set of TNyxOutputSetting;
    FEditedDuring: set of TNyxOutputSetting;
    FPending: Boolean;
    FIntent: TNyxOutputSyncIntent;
    FSent: TNyxText;
    function GetDirty: Boolean;
  public
    { Mark after a successful local input edit. Field names are accepted only
      at the existing UI/persistence boundary; unknown names refuse. }
    procedure Mark(ASetting: TNyxOutputSetting);
    procedure MarkField(const AField: TNyxText);
    { Capture before dispatch. A second request or absent configuration refuses
      without losing the first snapshot. The caller owns dispatch and decoding. }
    procedure BeginRequest(AIntent: TNyxOutputSyncIntent;
      ACurrent: TNyxOutputConfiguration);
    { Save replies must equal the submitted snapshot before any acknowledgment.
      Reload clears only adopted fields; newer edits remain dirty for a later
      explicit Apply. Decode/transport failures must instead call AbortRequest. }
    procedure Accept(ACurrent, ALoaded: TNyxOutputConfiguration);
    { Retire request metadata only. Safe when already idle; preserves dirty
      intent and never changes or frees the caller's configuration. }
    procedure AbortRequest;
    { Dirty describes unacknowledged local intent. Intent/Sent describe only a
      Pending request; reads have empty Sent and completion releases its text. }
    property Dirty: Boolean read GetDirty;
    property Pending: Boolean read FPending;
    property Intent: TNyxOutputSyncIntent read FIntent;
    property Sent: TNyxText read FSent;
  end;

const
  NyxOutputFields: array[0..5] of TNyxText = ('pas2js', 'runtime', 'fpc',
    'lazarus', 'platform', 'widgetset');

{ Empty means no output chosen. Choosing a target is independent of having its
  tools installed; neither operation changes the authored document. }
procedure ValidateNyxOutputTarget(const ATarget: TNyxText);

implementation

uses
  {$IFDEF PAS2JS}
  JS,
  fpjsonjs;
  {$ELSE}
  jsonparser;
  {$ENDIF}

function TNyxOutputEditState.GetDirty: Boolean;
begin
  Result := FDirty <> [];
end;

procedure TNyxOutputEditState.Mark(ASetting: TNyxOutputSetting);
begin

  if (Ord(ASetting) < Ord(Low(TNyxOutputSetting))) or
    (Ord(ASetting) > Ord(High(TNyxOutputSetting))) then
  begin
    raise ENyxModel.Create('Unknown output setting');
  end;
  Include(FDirty, ASetting);

  if FPending then
  begin
    Include(FEditedDuring, ASetting);
  end;
end;

procedure TNyxOutputEditState.MarkField(const AField: TNyxText);
var
  LIndex: Integer;
begin
  for LIndex := 0 to High(NyxOutputFields) do
  begin

    if NyxOutputFields[LIndex] = AField then
    begin
      Mark(TNyxOutputSetting(LIndex));
      Exit;
    end;
  end;
  raise ENyxModel.Create('Unknown output configuration field');
end;

procedure TNyxOutputEditState.BeginRequest(AIntent: TNyxOutputSyncIntent;
  ACurrent: TNyxOutputConfiguration);
begin

  if FPending or (ACurrent = nil) then
  begin
    raise ENyxModel.Create('Output configuration request is already pending or absent');
  end;

  if (Ord(AIntent) < Ord(Low(TNyxOutputSyncIntent))) or
    (Ord(AIntent) > Ord(High(TNyxOutputSyncIntent))) then
  begin
    raise ENyxModel.Create('Unknown output synchronization intent');
  end;
  FSent := '';

  if AIntent = osiSave then
  begin
    FSent := ACurrent.Encode;
  end;
  FIntent := AIntent;
  FEditedDuring := [];
  FPending := True;
end;

procedure TNyxOutputEditState.Accept(ACurrent, ALoaded: TNyxOutputConfiguration);
var
  LSetting: TNyxOutputSetting;
  LAdopt: Boolean;
begin

  if not FPending or (ACurrent = nil) or (ALoaded = nil) then
  begin
    raise ENyxModel.Create('Output configuration reply has no pending request or decoded profile');
  end;

  if (FIntent = osiSave) and (ALoaded.Encode <> FSent) then
  begin
    raise ENyxModel.Create('Saved output reply differs from the submitted configuration; local edits retained');
  end;
  for LSetting := Low(TNyxOutputSetting) to High(TNyxOutputSetting) do
  begin
    LAdopt := ((FIntent = osiObserve) and not (LSetting in FDirty)) or
      ((FIntent = osiReload) and not (LSetting in FEditedDuring));

    if LAdopt then
    begin
      case LSetting of
        nosFileSaving:
          begin
            ACurrent.AllowFileSaving(ALoaded.FileSaving);
          end;
        nosFileOpening:
          begin
            ACurrent.AllowFileOpening(ALoaded.FileOpening);
          end;
        else
          begin
            ACurrent.SetField(NyxOutputFields[Ord(LSetting)],
              ALoaded.Field(NyxOutputFields[Ord(LSetting)]));
          end;
      end;
    end;

    if (FIntent in [osiSave, osiReload]) and not (LSetting in FEditedDuring) then
    begin
      Exclude(FDirty, LSetting);
    end;
  end;
  AbortRequest;
end;

procedure TNyxOutputEditState.AbortRequest;
begin
  FPending := False;
  FEditedDuring := [];
  FSent := '';
end;

function IsOutputField(const AKey: TNyxText): Boolean;
var
  LIndex: Integer;
begin
  Result := False;
  for LIndex := 0 to High(NyxOutputFields) do
  begin

    if NyxOutputFields[LIndex] = AKey then
    begin
      Exit(True);
    end;
  end;
end;

procedure ValidateNyxOutputTarget(const ATarget: TNyxText);
begin

  if (ATarget <> '') and (ATarget <> 'browser') and (ATarget <> 'lcl') then
  begin
    raise ENyxModel.Create('Choose Browser or Native LCL in Target / output');
  end;
end;

constructor TNyxOutputConfiguration.Create;
begin
  inherited Create;
  FFields := TNyxStrings.Create;
end;

destructor TNyxOutputConfiguration.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TNyxOutputConfiguration.Field(const AKey: TNyxText): TNyxText;
var
  LIndex: Integer;
begin

  if not IsOutputField(AKey) then
  begin
    raise ENyxModel.Create('Unknown output field: ' + AKey);
  end;
  Result := '';
  LIndex := FFields.IndexOfName(AKey);

  if LIndex >= 0 then
  begin
    Result := Copy(FFields[LIndex], Length(AKey) + 2, MaxInt);
  end;
end;

procedure TNyxOutputConfiguration.SetField(const AKey, AValue: TNyxText);
var
  LIndex: Integer;
begin

  if not IsOutputField(AKey) then
  begin
    raise ENyxModel.Create('Unknown output field: ' + AKey);
  end;

  if (Length(AValue) > 4096) or (Pos(#0, AValue) > 0) or
    (Pos(#10, AValue) > 0) or (Pos(#13, AValue) > 0) then
  begin
    raise ENyxModel.Create('Output fields must be single-line paths or identifiers');
  end;
  LIndex := FFields.IndexOfName(AKey);

  if LIndex >= 0 then
  begin
    FFields[LIndex] := AKey + '=' + AValue;
  end
  else
  begin
    FFields.Add(AKey + '=' + AValue);
  end;
end;

function TNyxOutputConfiguration.AllowFileSaving(AValue: Boolean): TNyxOutputConfiguration;
begin
  FFileSaving := AValue;
  Result := Self;
end;

function TNyxOutputConfiguration.AllowFileOpening(AValue: Boolean): TNyxOutputConfiguration;
begin
  FFileOpening := AValue;
  Result := Self;
end;

function TNyxOutputConfiguration.Encode: TNyxText;
var
  LRoot: TJSONObject;
  LFields: TJSONObject;
  LIndex: Integer;
begin
  LRoot := TJSONObject.Create;
  try

    if FFileOpening then
    begin
      LRoot.Add('version', 3);
      LRoot.Add('fileSaving', FFileSaving);
      LRoot.Add('fileOpening', True);
    end
    else if FFileSaving then
    begin
      LRoot.Add('version', 2);
      LRoot.Add('fileSaving', True);
    end
    else
    begin
      { The default policy preserves the original profile bytes exactly. }
      LRoot.Add('version', 1);
    end;
    LFields := TJSONObject.Create;
    LRoot.Add('fields', LFields);
    for LIndex := 0 to High(NyxOutputFields) do
    begin
      LFields.Add(NyxOutputFields[LIndex], Field(NyxOutputFields[LIndex]));
    end;
    Result := LRoot.AsJSON;
  finally
    LRoot.Free;
  end;
end;

class function TNyxOutputConfiguration.Decode(
  const ASource: TNyxText): TNyxOutputConfiguration;
var
  LData: TJSONData;
  LRoot: TJSONObject;
  LFields: TJSONObject;
  LValue: TJSONData;
  LIndex: Integer;
  LDepth: Integer;
  LQuoted: Boolean;
  LEscaped: Boolean;
  LVersion: Integer;
begin
  { Bound input before either JSON parser sees it. This flat protocol needs no
    deep nesting, and browser/native parsers share the same admission limits. }

  if Length(ASource) > 32768 then
  begin
    raise ENyxModel.Create('Output configuration exceeds 32 KiB');
  end;
  LDepth := 0;
  LQuoted := False;
  LEscaped := False;
  for LIndex := 1 to Length(ASource) do
  begin

    if LQuoted then
    begin

      if LEscaped then
      begin
        LEscaped := False;
      end
      else if ASource[LIndex] = '\' then
      begin
        LEscaped := True;
      end
      else if ASource[LIndex] = '"' then
      begin
        LQuoted := False;
      end;
    end
    else
    begin
      case ASource[LIndex] of
        '"':
          begin
            LQuoted := True;
          end;
        '{', '[':
          begin
            Inc(LDepth);
          end;
        '}', ']':
          begin
            Dec(LDepth);
          end;
      end;

      if (LDepth > 4) or (LDepth < 0) then
      begin
        raise ENyxModel.Create('Invalid output configuration nesting');
      end;
    end;
  end;
  {$IFDEF PAS2JS}
  LData := JSValueToJSONData(TJSJSON.parse(ASource));
  {$ELSE}
  LData := GetJSON(ASource);
  {$ENDIF}
  Result := nil;
  try

    if not (LData is TJSONObject) then
    begin
      raise ENyxModel.Create('Output configuration must be an object');
    end;
    LRoot := TJSONObject(LData);
    LValue := LRoot.Find('version');

    if (LValue = nil) or (LValue.JSONType <> jtNumber) or
      ((LValue.AsFloat <> 1) and (LValue.AsFloat <> 2) and (LValue.AsFloat <> 3)) then
    begin
      raise ENyxModel.Create('Unsupported output configuration version');
    end;
    LVersion := LValue.AsInteger;
    LValue := LRoot.Find('fields');

    if not (LValue is TJSONObject) or
      ((LVersion = 1) and (LRoot.Count <> 2)) or
      ((LVersion = 2) and (LRoot.Count <> 3)) or
      ((LVersion = 3) and (LRoot.Count <> 4)) then
    begin
      raise ENyxModel.Create('Output configuration requires version and fields');
    end;
    LFields := TJSONObject(LValue);
    Result := TNyxOutputConfiguration.Create;
    try

      if LVersion in [2, 3] then
      begin
        LValue := LRoot.Find('fileSaving');

        if (LValue = nil) or (LValue.JSONType <> jtBoolean) then
        begin
          raise ENyxModel.Create('File-save runtime authority must be a Boolean');
        end;
        Result.AllowFileSaving(LValue.AsBoolean);
      end;

      if LVersion = 3 then
      begin
        LValue := LRoot.Find('fileOpening');

        if (LValue = nil) or (LValue.JSONType <> jtBoolean) then
        begin
          raise ENyxModel.Create('File-open runtime authority must be a Boolean');
        end;
        Result.AllowFileOpening(LValue.AsBoolean);
      end;
      for LIndex := 0 to LFields.Count - 1 do
      begin

        if LFields.Items[LIndex].JSONType <> jtString then
        begin
          raise ENyxModel.Create('Output fields must be strings');
        end;
        Result.SetField(LFields.Names[LIndex], LFields.Items[LIndex].AsString);
      end;
    except
      Result.Free;
      Result := nil;
      raise;
    end;
  finally
    LData.Free;
  end;
end;

end.
