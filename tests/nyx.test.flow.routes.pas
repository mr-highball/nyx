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


unit nyx.test.flow.routes;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model, nyx.flow.routes;

function RoutedLogicDocument: TNyxDocument;
function ProfileRoute(AMode: TNyxFlowCallbackMode = nfcmExclusive): TNyxFlowRoute;

implementation

uses nyx.types, nyx.state, nyx.flow.types, nyx.flow.declarations,
  nyx.flow.route.model, nyx.logic, nyx.test.logic.fixture, nyx.test.logic.document;

function ProfileRoute(AMode: TNyxFlowCallbackMode): TNyxFlowRoute;
begin
  Result := NyxFlowRoute(NyxFlowRouteID('Format profile'), NyxFlowRef('Profile'), ntClick, AMode)
    .InputState(LogicEndpoint('parse', nlpText), NyxTextState('source'))
    .InputConstant(LogicEndpoint('summary', nlpLeft), NyxFlowText('Profile'))
    .OutputState(LogicEndpoint('pretty', nlpText), NyxTextState('pretty'))
    .OutputState(LogicEndpoint('compact', nlpText), NyxTextState('compact'))
    .OutputState(LogicEndpoint('summary', nlpText), NyxTextState('summary'));
end;

function RoutedLogicDocument: TNyxDocument;
begin
  Result := LogicDocument;
  try
    Result.Flows.Define(NyxFlowRef('Profile'), LogicFixture);
    SetNyxFlowRoutes(Result.Find('run-button'), NewNyxFlowRoutes.WithRoute(ProfileRoute));
    { Field changes use their actual accepted event value, independently from
      button routes that sample the current state. Both share one saved graph. }
    SetNyxFlowRoutes(Result.Find('source-memo'), NewNyxFlowRoutes.WithRoute(
      NyxFlowRoute(NyxFlowRouteID('Live profile'), NyxFlowRef('Profile'), ntChange, nfcmExclusive)
        .InputEventValue(LogicEndpoint('parse', nlpText), nftText)
        .InputConstant(LogicEndpoint('summary', nlpLeft), NyxFlowText('Live profile'))
        .OutputState(LogicEndpoint('pretty', nlpText), NyxTextState('pretty'))
        .OutputState(LogicEndpoint('compact', nlpText), NyxTextState('compact'))
        .OutputState(LogicEndpoint('summary', nlpText), NyxTextState('summary'))));
    Result.Validate;
  except
    Result.Free;
    raise;
  end;
end;

end.
