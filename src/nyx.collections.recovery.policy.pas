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
unit nyx.collections.recovery.policy;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.collections, nyx.data;

const
  NyxDefaultDraftRecoveryEntries = 256;
  NyxDefaultDraftRecoveryPayloadBytes = 16 * 1024 * 1024;
  NyxDefaultDraftRecoveryWarningEntries = 64;
  NyxDefaultDraftRecoveryWarningTextBytes = 8 * 1024 * 1024;

type
  { Authored defaults contain only finite admission limits and advisory warning
    thresholds. Absence differs from explicitly saving library defaults. No
    draft, journal, revision, hold, receiver or target authority is serialized.
    Entry/payload limits accept zero; warning zero disables that warning. }
  TNyxCollectionDraftRecoveryPolicy = record
  private
    FDefined: Boolean;
    FEntries: Integer;
    FPayload: Integer;
    FWarnEntries: Integer;
    FWarnText: Integer;
  public
    function Entries(AValue: Integer): TNyxCollectionDraftRecoveryPolicy;
    function PayloadBytes(AValue: Integer): TNyxCollectionDraftRecoveryPolicy;
    function WarnEntries(AValue: Integer): TNyxCollectionDraftRecoveryPolicy;
    function WarnTextBytes(AValue: Integer): TNyxCollectionDraftRecoveryPolicy;
    procedure Validate;
    function ToData: TNyxDataValue;
    class function FromData(const AData: TNyxDataValue): TNyxCollectionDraftRecoveryPolicy; static;
    property Defined: Boolean read FDefined;
    property MaximumEntries: Integer read FEntries;
    property MaximumPayloadBytes: Integer read FPayload;
    property WarningEntries: Integer read FWarnEntries;
    property WarningTextBytes: Integer read FWarnText;
  end;

function NyxDraftRecoveryPolicy: TNyxCollectionDraftRecoveryPolicy;

implementation

function NyxDraftRecoveryPolicy: TNyxCollectionDraftRecoveryPolicy;
begin
  Result := Default(TNyxCollectionDraftRecoveryPolicy);
  Result.FDefined := True;
  Result.FEntries := NyxDefaultDraftRecoveryEntries;
  Result.FPayload := NyxDefaultDraftRecoveryPayloadBytes;
  Result.FWarnEntries := NyxDefaultDraftRecoveryWarningEntries;
  Result.FWarnText := NyxDefaultDraftRecoveryWarningTextBytes;
end;

procedure TNyxCollectionDraftRecoveryPolicy.Validate;
begin

  if not FDefined or (FEntries < 0) or (FPayload < 0) or
    (FWarnEntries < 0) or (FWarnText < 0) then
  begin
    raise ENyxCollection.Create('Create a defined recovery policy with nonnegative limits and warnings');
  end;
end;

function TNyxCollectionDraftRecoveryPolicy.Entries(AValue: Integer): TNyxCollectionDraftRecoveryPolicy;
begin
  Validate;
  Result := Self;
  Result.FEntries := AValue;
  Result.Validate;
end;

function TNyxCollectionDraftRecoveryPolicy.PayloadBytes(AValue: Integer): TNyxCollectionDraftRecoveryPolicy;
begin
  Validate;
  Result := Self;
  Result.FPayload := AValue;
  Result.Validate;
end;

function TNyxCollectionDraftRecoveryPolicy.WarnEntries(AValue: Integer): TNyxCollectionDraftRecoveryPolicy;
begin
  Validate;
  Result := Self;
  Result.FWarnEntries := AValue;
  Result.Validate;
end;

function TNyxCollectionDraftRecoveryPolicy.WarnTextBytes(AValue: Integer): TNyxCollectionDraftRecoveryPolicy;
begin
  Validate;
  Result := Self;
  Result.FWarnText := AValue;
  Result.Validate;
end;

function TNyxCollectionDraftRecoveryPolicy.ToData: TNyxDataValue;
begin

  if not FDefined then
  begin
    Exit(NyxNull);
  end;
  Validate;
  Result := NyxObject([NyxField('version', NyxData(1)),
    NyxField('maximumEntries', NyxData(FEntries)),
    NyxField('maximumPayloadBytes', NyxData(FPayload)),
    NyxField('warningEntries', NyxData(FWarnEntries)),
    NyxField('warningTextBytes', NyxData(FWarnText))]);
end;

class function TNyxCollectionDraftRecoveryPolicy.FromData(const AData: TNyxDataValue):
  TNyxCollectionDraftRecoveryPolicy;
var
  LPolicy: TNyxCollectionDraftRecoveryPolicy;
  LIndex: Integer;
begin
  LPolicy := Default(TNyxCollectionDraftRecoveryPolicy);
  AData.Validate;

  if AData.Kind <> ndNull then
  begin

    if (AData.Kind <> ndObject) or (AData.Count <> 5) or
      (AData.Field('version').AsInteger <> 1) then
    begin
      raise ENyxCollection.Create('Unsupported recovery policy descriptor');
    end;
    for LIndex := 0 to AData.Count - 1 do
    begin

      if Pos('|' + AData.Key(LIndex) + '|',
        '|version|maximumEntries|maximumPayloadBytes|warningEntries|warningTextBytes|') = 0 then
      begin
        raise ENyxCollection.Create('Unknown recovery policy field');
      end;
    end;
    LPolicy := NyxDraftRecoveryPolicy
      .Entries(AData.Field('maximumEntries').AsInteger)
      .PayloadBytes(AData.Field('maximumPayloadBytes').AsInteger)
      .WarnEntries(AData.Field('warningEntries').AsInteger)
      .WarnTextBytes(AData.Field('warningTextBytes').AsInteger);
  end;
  Result := LPolicy;
end;

end.
