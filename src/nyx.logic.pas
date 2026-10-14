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
unit nyx.logic;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  SysUtils, nyx.text, nyx.data, nyx.json, nyx.bytes, nyx.resources,
  nyx.scheduler, nyx.flow.types;

type
  { Shared structural selector, retaining the resource layer's exact Field/Item
    contract without introducing dotted strings or another JSON path evaluator. }
  TNyxDataPath = TNyxResourcePath;
  TNyxJSONLayout = (njlCompact, njlIndented);
  TNyxLogicPort = (nlpText, nlpJSON, nlpValue, nlpLeft, nlpRight);

  { Nonvisual managed operation. Descriptor/configuration are immutable copied
    values. Invoke owns no caller frame or UI/model; it returns a complete output
    frame or raises. Implementations cooperate with cancellation inside long work.
    Pure affinity promises independently owned worker-safe implementation state.
    Host/UI operations must explicitly declare UI affinity; it is not inferred
    from a name/configuration string. No runtime/editor reference cycles. }
  INyxLogicOperation = interface(IInterface)
    ['{D1411014-6120-4C84-A708-000000000001}']
    function GetDescriptor: TNyxLogicDescriptor;
    function GetConfiguration: TNyxDataValue;
    function Invoke(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues;
    property Descriptor: TNyxLogicDescriptor read GetDescriptor;
    property Configuration: TNyxDataValue read GetConfiguration;
  end;

  { Direct callers use the same specialized immutable operation as graph runs. }
  INyxJSONParse = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000002}']
    function Parse(const AText: TNyxText): TNyxDataValue;
  end;
  INyxJSONFormat = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000003}']
    function Format(const AValue: TNyxDataValue): TNyxText;
    function GetLayout: TNyxJSONLayout;
    function GetIndent: Integer;
    property Layout: TNyxJSONLayout read GetLayout;
    property Indent: Integer read GetIndent;
  end;
  INyxJSONSelect = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000004}']
    function Select(const AValue: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
    property Path: TNyxDataPath read GetPath;
  end;
  INyxTextJoin = interface(INyxLogicOperation)
    ['{D1411014-6120-4C84-A708-000000000005}']
    function Join(const ALeft, ARight: TNyxText): TNyxText;
    function GetSeparator: TNyxText;
    property Separator: TNyxText read GetSeparator;
  end;

function NyxLogicPort(APort: TNyxLogicPort): TNyxFlowPortRef;
function NyxDataPath: TNyxDataPath;
function NewNyxJSONParse: INyxJSONParse;
function NewNyxJSONFormat(ALayout: TNyxJSONLayout = njlIndented;
  AIndent: Integer = 2): INyxJSONFormat;
function NewNyxJSONSelect(const APath: TNyxDataPath): INyxJSONSelect;
function NewNyxTextJoin(const ASeparator: TNyxText = ''): INyxTextJoin;
{ Closed built-in persistence factory. It reconstructs only the public typed
  constructors above; no class loading, code execution or host authority. Creator
  extensions supply their own explicit resolver to graph wire admission. }
function NyxLogicFromData(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
procedure ValidateNyxLogicFrame(const ADescriptor: TNyxLogicDescriptor;
  ADirection: TNyxFlowDirection; const AValues: TNyxFlowValues);

implementation

type
  TLogic = class(TInterfacedObject, INyxLogicOperation)
  protected
    FDescriptor: TNyxLogicDescriptor;
    FConfiguration: TNyxDataValue;
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; virtual; abstract;
  public
    function GetDescriptor: TNyxLogicDescriptor;
    function GetConfiguration: TNyxDataValue;
    function Invoke(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues;
  end;
  TJSONParse = class(TLogic, INyxJSONParse)
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create;
    function Parse(const AText: TNyxText): TNyxDataValue;
  end;
  TJSONFormat = class(TLogic, INyxJSONFormat)
  private
    FLayout: TNyxJSONLayout;
    FIndent: Integer;
    function FormatValue(const AValue: TNyxDataValue;
      const ACancellation: INyxCancellationScope): TNyxText;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(ALayout: TNyxJSONLayout; AIndent: Integer);
    function Format(const AValue: TNyxDataValue): TNyxText;
    function GetLayout: TNyxJSONLayout;
    function GetIndent: Integer;
  end;
  TJSONSelect = class(TLogic, INyxJSONSelect)
  private
    FPath: TNyxDataPath;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const APath: TNyxDataPath);
    function Select(const AValue: TNyxDataValue): TNyxDataValue;
    function GetPath: TNyxDataPath;
  end;
  TTextJoin = class(TLogic, INyxTextJoin)
  private
    FSeparator: TNyxText;
  protected
    function Execute(const AInputs: TNyxFlowValues;
      const ACancellation: INyxCancellationScope): TNyxFlowValues; override;
  public
    constructor Create(const ASeparator: TNyxText);
    function Join(const ALeft, ARight: TNyxText): TNyxText;
    function GetSeparator: TNyxText;
  end;

procedure CheckCancellation(const AScope: INyxCancellationScope);
begin

  if (AScope <> nil) and AScope.Cancelled then
  begin
    raise ENyxFlowCancelled.Create('Logic invocation cancelled');
  end;
end;

function NyxLogicPort(APort: TNyxLogicPort): TNyxFlowPortRef;
const
  CNames: array[TNyxLogicPort] of TNyxText = ('text', 'json', 'value', 'left', 'right');
begin

  if (Ord(APort) < Ord(Low(TNyxLogicPort))) or (Ord(APort) > Ord(High(TNyxLogicPort))) then
  begin
    raise ENyxFlow.Create('Unknown built-in logic port');
  end;
  Result := NyxFlowPort(CNames[APort]);
end;

function NyxDataPath: TNyxDataPath;
begin
  Result := Default(TNyxDataPath);
end;

procedure ValidateNyxLogicFrame(const ADescriptor: TNyxLogicDescriptor;
  ADirection: TNyxFlowDirection; const AValues: TNyxFlowValues);
var
  LIndex: Integer;
  LCount: Integer;
  LPort: TNyxFlowPort;
begin
  LCount := 0;
  for LIndex := 0 to ADescriptor.PortCount - 1 do
  begin
    LPort := ADescriptor.Port(LIndex);

    if LPort.Direction <> ADirection then
    begin
      Continue;
    end;
    Inc(LCount);

    if AValues.Value(LPort.Reference).ValueType <> LPort.ValueType then
    begin
      raise ENyxFlow.Create('Logic port value family mismatch: ' + LPort.Reference.Name);
    end;
  end;

  if LCount <> AValues.Count then
  begin
    raise ENyxFlow.Create('Logic frame must contain exactly its declared ports');
  end;
end;

function TLogic.GetDescriptor: TNyxLogicDescriptor;
begin
  Result := FDescriptor;
end;

function TLogic.GetConfiguration: TNyxDataValue;
begin
  Result := FConfiguration.Copy;
end;

function TLogic.Invoke(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  CheckCancellation(ACancellation);
  ValidateNyxLogicFrame(FDescriptor, nfdInput, AInputs);
  Result := Execute(AInputs, ACancellation);
  CheckCancellation(ACancellation);
  ValidateNyxLogicFrame(FDescriptor, nfdOutput, Result);
end;

constructor TJSONParse.Create;
begin
  inherited Create;
  FConfiguration := NyxObject([]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('json-parse'), 'Parse JSON',
    'Read strict JSON text without losing exact numbers or Unicode.', nlgJSON)
    .Input(NyxLogicPort(nlpText), nftText, 'Strict JSON source text')
    .Output(NyxLogicPort(nlpJSON), nftJSON, 'Owned parsed JSON value')
    .Labelled('parse').Labelled('data').Labelled('json');
end;

function TJSONParse.Parse(const AText: TNyxText): TNyxDataValue;
begin
  Result := TNyxDataValue.ParseJSON(AText);
end;

function TJSONParse.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpJSON),
    NyxFlowJSON(Parse(AInputs.Value(NyxLogicPort(nlpText)).AsText)));
end;

constructor TJSONFormat.Create(ALayout: TNyxJSONLayout; AIndent: Integer);
begin
  inherited Create;

  if (Ord(ALayout) < Ord(Low(TNyxJSONLayout))) or
    (Ord(ALayout) > Ord(High(TNyxJSONLayout))) or (AIndent < 1) or (AIndent > 8) then
  begin
    raise ENyxFlow.Create('JSON format requires typed layout and 1..8 indent spaces');
  end;
  FLayout := ALayout;
  FIndent := AIndent;
  FConfiguration := NyxObject([NyxField('layout', NyxData(Ord(ALayout))),
    NyxField('indent', NyxData(AIndent))]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('json-format'), 'Format JSON',
    'Prettify or compact JSON while preserving field order, exact numbers and text.', nlgJSON)
    .Input(NyxLogicPort(nlpJSON), nftJSON, 'Owned JSON value')
    .Output(NyxLogicPort(nlpText), nftText, 'Formatted JSON source text')
    .Labelled('pretty').Labelled('compact').Labelled('format');
end;

function TJSONFormat.GetLayout: TNyxJSONLayout;
begin
  Result := FLayout;
end;

function TJSONFormat.GetIndent: Integer;
begin
  Result := FIndent;
end;

function TJSONFormat.FormatValue(const AValue: TNyxDataValue;
  const ACancellation: INyxCancellationScope): TNyxText;
var
  LParts: TNyxStrings;
  LBytes: Integer;

  procedure Add(const AText: TNyxText);
  begin
    CheckCancellation(ACancellation);
    Inc(LBytes, NyxUTF8ByteCount(AText));

    if LBytes > NyxMaximumJSONBytes then
    begin
      raise ENyxFlow.Create('Formatted JSON exceeds its UTF-8 byte boundary');
    end;
    LParts.Add(AText);
  end;

  procedure NewLine(ADepth: Integer);
  begin

    if FLayout = njlIndented then
    begin
      Add(#10 + TNyxText(StringOfChar(' ', ADepth * FIndent)));
    end;
  end;

  procedure Emit(const AData: TNyxDataValue; ADepth: Integer);
  var
    LIndex: Integer;
  begin
    CheckCancellation(ACancellation);

    if not (AData.Kind in [ndObject, ndArray]) then
    begin
      Add(AData.ToJSON);
      Exit;
    end;

    if AData.Kind = ndObject then
    begin
      Add('{');
    end
    else
    begin
      Add('[');
    end;
    for LIndex := 0 to AData.Count - 1 do
    begin

      if LIndex > 0 then
      begin
        Add(',');
      end;
      NewLine(ADepth + 1);

      if AData.Kind = ndObject then
      begin
        Add(NyxData(AData.Key(LIndex)).ToJSON + ':');

        if FLayout = njlIndented then
        begin
          Add(' ');
        end;
        Emit(AData.Field(AData.Key(LIndex)), ADepth + 1);
      end
      else
      begin
        Emit(AData.Item(LIndex), ADepth + 1);
      end;
    end;

    if AData.Count > 0 then
    begin
      NewLine(ADepth);
    end;

    if AData.Kind = ndObject then
    begin
      Add('}');
    end
    else
    begin
      Add(']');
    end;
  end;
begin
  AValue.Validate;
  LParts := TNyxStrings.Create;
  try
    LBytes := 0;
    Emit(AValue, 0);
    Result := LParts.Join;
  finally
    LParts.Free;
  end;
end;

function TJSONFormat.Format(const AValue: TNyxDataValue): TNyxText;
begin
  Result := FormatValue(AValue, nil);
end;

function TJSONFormat.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpText),
    NyxFlowText(FormatValue(AInputs.Value(NyxLogicPort(nlpJSON)).AsJSON, ACancellation)));
end;

constructor TJSONSelect.Create(const APath: TNyxDataPath);
begin
  inherited Create;
  FPath := APath.Copy;
  FConfiguration := NyxObject([NyxField('path', FPath.ToData)]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('json-select'), 'Select JSON',
    'Select an exact field/item path without dotted strings or coercion.', nlgJSON)
    .Input(NyxLogicPort(nlpJSON), nftJSON, 'Source JSON value')
    .Output(NyxLogicPort(nlpValue), nftJSON, 'Selected owned JSON value')
    .Labelled('path').Labelled('field').Labelled('extract');
end;

function TJSONSelect.GetPath: TNyxDataPath;
begin
  Result := FPath.Copy;
end;

function TJSONSelect.Select(const AValue: TNyxDataValue): TNyxDataValue;
begin
  Result := FPath.Select(AValue);
end;

function TJSONSelect.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpValue),
    NyxFlowJSON(Select(AInputs.Value(NyxLogicPort(nlpJSON)).AsJSON)));
end;

constructor TTextJoin.Create(const ASeparator: TNyxText);
begin
  inherited Create;
  NyxData(ASeparator);
  FSeparator := ASeparator;
  FConfiguration := NyxObject([NyxField('separator', NyxData(ASeparator))]);
  FDescriptor := NyxLogicDescriptor(NyxLogicKind('text-join'), 'Join text',
    'Combine two exact text values with a caller-defined separator.', nlgText)
    .Input(NyxLogicPort(nlpLeft), nftText, 'Left text')
    .Input(NyxLogicPort(nlpRight), nftText, 'Right text')
    .Output(NyxLogicPort(nlpText), nftText, 'Combined text')
    .Labelled('concatenate').Labelled('combine').Labelled('text');
end;

function TTextJoin.GetSeparator: TNyxText;
begin
  Result := FSeparator;
end;

function TTextJoin.Join(const ALeft, ARight: TNyxText): TNyxText;
begin
  Result := ALeft + FSeparator + ARight;
  NyxData(Result);
end;

function TTextJoin.Execute(const AInputs: TNyxFlowValues;
  const ACancellation: INyxCancellationScope): TNyxFlowValues;
begin
  Result := NyxFlowValues.WithValue(NyxLogicPort(nlpText),
    NyxFlowText(Join(AInputs.Value(NyxLogicPort(nlpLeft)).AsText,
      AInputs.Value(NyxLogicPort(nlpRight)).AsText)));
end;

function NewNyxJSONParse: INyxJSONParse;
begin
  Result := TJSONParse.Create;
end;

function NewNyxJSONFormat(ALayout: TNyxJSONLayout; AIndent: Integer): INyxJSONFormat;
begin
  Result := TJSONFormat.Create(ALayout, AIndent);
end;

function NewNyxJSONSelect(const APath: TNyxDataPath): INyxJSONSelect;
begin
  Result := TJSONSelect.Create(APath);
end;

function NewNyxTextJoin(const ASeparator: TNyxText): INyxTextJoin;
begin
  Result := TTextJoin.Create(ASeparator);
end;

function NyxLogicFromData(const AKind: TNyxLogicKindRef;
  const AConfiguration: TNyxDataValue): INyxLogicOperation;
var
  LLayout: Integer;
begin

  if AKind.Name = 'json-parse' then
  begin
    ValidateNyxFlowFields(AConfiguration, []);
    Result := NewNyxJSONParse;
  end
  else if AKind.Name = 'json-format' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['layout', 'indent']);
    LLayout := AConfiguration.Field('layout').AsInteger;

    if (LLayout < Ord(Low(TNyxJSONLayout))) or (LLayout > Ord(High(TNyxJSONLayout))) then
    begin
      raise ENyxFlow.Create('Unknown JSON layout');
    end;
    Result := NewNyxJSONFormat(TNyxJSONLayout(LLayout), AConfiguration.Field('indent').AsInteger);
  end
  else if AKind.Name = 'json-select' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['path']);
    Result := NewNyxJSONSelect(TNyxDataPath.FromData(AConfiguration.Field('path')));
  end
  else if AKind.Name = 'text-join' then
  begin
    ValidateNyxFlowFields(AConfiguration, ['separator']);
    Result := NewNyxTextJoin(AConfiguration.Field('separator').AsText);
  end
  else
    raise ENyxFlow.Create('Logic kind requires an explicitly registered resolver: ' + AKind.Name);
end;

end.
