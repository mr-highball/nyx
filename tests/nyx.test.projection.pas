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

unit nyx.test.projection;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.studio.builds, nyx.studio.sourceprojection;

{ Literal independent expected meaning, with no application helper/class/loop.
  The qualification source itself is compiled in another process/worker. }
function ExpectedNyxProjectionDesign: TNyxText;

{ Exercise the exact complete wire, detached lifetimes and malformed channel
  replies on both targets. Returns completed assertions; failures raise. }
function RunNyxProjectionPacketChecks(const ASource: TNyxText;
  const AReference: TNyxSourceProjectionRef; ATarget: TNyxBuildTarget;
  const AWire, AExpected: TNyxText): Integer;

implementation

uses SysUtils, nyx.model, nyx.codec, nyx.types, nyx.controls, nyx.state,
  nyx.resources, nyx.data;

const
  CQualificationText: TNyxText = 'A rocket 🚀, a letter 𐐷, and é.';

function ExpectedNyxProjectionDesign: TNyxText;
var
  LDocument: TNyxDocument;
  LDefinition: INyxCard;
  LCaption: INyxHeading;
  LPage: INyxPage;
  LHeading: INyxHeading;
  LInstance: INyxComponent;
begin
  LDocument := TNyxDocument.Create;
  try
    LDocument.Title := 'Handwritten notebook';
    LDocument.State.SetValue(NyxTextState('greeting'), CQualificationText);
    LDocument.State.SetValue(NyxIntegerState('notes'), 3);
    LDocument.State.SetValue(NyxNumberState('scale'), 0.125);
    LDocument.Resources.Define(NyxResourceRef('instructions'),
      NyxTextResource(CQualificationText));
    LDocument.Resources.Define(NyxResourceRef('data'),
      NyxJSONResource('{"value":"' + CQualificationText + '"}'));
    LDefinition := NewNyxCard('reusable-note', ncoDescriptor);
    LDefinition.Configure.Padding(12).Surface(True);
    LCaption := NewNyxHeading('note-caption', ncoDescriptor);
    LCaption.Configure.Text(CQualificationText).PartName(NyxPart('caption'));
    LDefinition.Add(LCaption);
    LDocument.AddComponent(LDefinition);

    LPage := NewNyxPage('notebook-1', ncoDescriptor);
    LPage.Configure.Text('Notebook 1').Padding(9);
    LHeading := NewNyxHeading('heading-1', ncoDescriptor);
    LHeading.Text := 'Notes for page 1';
    LPage.Add(LHeading);
    LInstance := NewNyxComponent('note-1', ncoDescriptor);
    LInstance.Configure.Component(NyxComponent('reusable-note'));
    LPage.Add(LInstance);
    LDocument.AddPage(LPage);

    LPage := NewNyxPage('notebook-2', ncoDescriptor);
    LPage.Configure.Text('Notebook 2').Padding(10);
    LHeading := NewNyxHeading('heading-2', ncoDescriptor);
    LHeading.Text := 'Notes for page 2';
    LPage.Add(LHeading);
    LInstance := NewNyxComponent('note-2', ncoDescriptor);
    LInstance.Configure.Component(NyxComponent('reusable-note'));
    LPage.Add(LInstance);
    LDocument.AddPage(LPage);
    LDocument.Validate;
    Result := TNyxCodec.Encode(LDocument);
  finally
    LDocument.Free;
  end;
end;

function RunNyxProjectionPacketChecks(const ASource: TNyxText;
  const AReference: TNyxSourceProjectionRef; ATarget: TNyxBuildTarget;
  const AWire, AExpected: TNyxText): Integer;
var
  LProjection: INyxSourceProjection;
  LFirst: TNyxDocument;
  LSecond: TNyxDocument;
  LOtherTarget: TNyxBuildTarget;
  LRejected: Boolean;
  LLarge: TNyxText;
  LCount: Integer;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Source projection: ' + AReason);
    end;
    Inc(LCount);
  end;

  procedure Unusable(const AValue: INyxSourceProjection;
    AState: TNyxSourceProjectionState; const AReason: TNyxText);
  var
    LCopy: TNyxDocument;
    LRefused: Boolean;
  begin
    Check((AValue <> nil) and (AValue.State = AState) and
      (AValue.Design = '') and (AValue.Source = ASource), AReason);
    LCopy := nil;
    LRefused := False;
    try
      try
        LCopy := AValue.CopyDocument;
      except
        on LException: Exception do
        begin
          LRefused := True;
        end;
      end;
      Check(LRefused and (LCopy = nil), AReason + ' exposes no tree');
    finally
      LCopy.Free;
    end;
  end;

begin
  LCount := 0;
  LProjection := ReceiveNyxSourceProjection(ASource, AReference, ATarget, AWire);
  Check(LProjection.State = spsExecuted, 'actual constructor executed');
  Check((LProjection.Source = ASource) and (LProjection.Target = ATarget),
    'exact source and target retained');
  Check(LProjection.Design = AExpected, 'complete independent expected design');
  LFirst := nil;
  LSecond := nil;
  try
    LFirst := LProjection.CopyDocument;
    LSecond := LProjection.CopyDocument;
    LProjection := nil;
    Check((LFirst <> LSecond) and (TNyxCodec.Encode(LFirst) = AExpected) and
      (TNyxCodec.Encode(LSecond) = AExpected), 'copies survive result release');
    Check((LSecond.Count = 2) and (LSecond.ComponentCount = 1) and
      (LSecond.Resources.Count = 2), 'complete page/component/resource membership');
    Check(LSecond.State.GetValue(NyxTextState('greeting')) = CQualificationText,
      'supplementary and combining text');
    Check((LSecond.State.GetValue(NyxIntegerState('notes')) = 3) and
      (LSecond.State.GetValue(NyxNumberState('scale')) = 0.125),
      'evaluated exact numeric state');
    LFirst.Title := 'An independent copy';
    LFirst.Free;
    LFirst := nil;
    Check(TNyxCodec.Encode(LSecond) = AExpected, 'independent ownership after mutation/free');
  finally
    LFirst.Free;
    LSecond.Free;
  end;
  LOtherTarget := btBrowser;

  if ATarget = btBrowser then
  begin
    LOtherTarget := btNativeLCL;
  end;
  Unusable(ReceiveNyxSourceProjection(ASource, NyxSourceProjectionRef('wrong-ticket'),
    ATarget, AWire), spsInvalidDesign, 'wrong invocation refused');
  Unusable(ReceiveNyxSourceProjection(ASource, AReference, LOtherTarget, AWire),
    spsInvalidDesign, 'wrong target refused');
  Unusable(ReceiveNyxSourceProjection(ASource, AReference, ATarget, '{'),
    spsInvalidDesign, 'malformed JSON refused');
  LLarge := TNyxText(StringOfChar(' ', NyxProjectionMaximumResultBytes + 1));
  Unusable(ReceiveNyxSourceProjection(ASource, AReference, ATarget, LLarge),
    spsInvalidDesign, 'excessive bytes refused');
  LLarge := '';
  Unusable(ReceiveNyxSourceProjection(ASource, AReference, ATarget,
    NyxSourceProjectionFailurePacket(AReference, ATarget, spsExecutionFailed).ToJSON),
    spsExecutionFailed, 'producer failure retained');
  Unusable(ReceiveNyxSourceProjection(ASource, AReference, ATarget,
    NyxObject([NyxField('version', NyxData(1)),
      NyxField('ticket', NyxData(AReference.Name)),
      NyxField('target', NyxData(NyxBuildTargetName(ATarget))),
      NyxField('state', NyxData('executed')),
      NyxField('design', NyxData('{}')), NyxField('message', NyxData(''))]).ToJSON),
    spsInvalidDesign, 'invalid design refused');

  LRejected := False;
  try
    CaptureNyxSourceProjection(nil, AReference, ATarget);
  except
    on LException: Exception do
    begin
      LRejected := True;
    end;
  end;
  Check(LRejected, 'nil source document refused');
  Result := LCount;
end;

end.
