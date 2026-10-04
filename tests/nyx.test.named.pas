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
unit nyx.test.named;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text, nyx.types, nyx.model, nyx.data, nyx.schema;

const
  NamedFixtureKind: TNyxText = 'named-event-fixture';
  NamedOpened: TNyxText = 'ItemOpened';
  NamedClosed: TNyxText = 'ItemClosed';
  NamedDetails: TNyxText = 'ItemDetails / 🌙 漢字';

{ Creator declarations are executable Pascal, shared by the test application
  and its compiled companion. They do not manufacture a physical adapter. }
procedure RegisterNamedFixture;
function NamedDesign: TNyxDocument;
function NamedCompanion(out ASource: TNyxText): TNyxDocument;

implementation

uses
  SysUtils, nyx.codec, nyx.contract, nyx.event.payload, nyx.callbacks,
  nyx.source, nyx.studio.session;

var
  GRegistered: Boolean;

procedure RegisterNamedFixture;
begin

  if GRegistered then
  begin
    Exit;
  end;
  RegisterNyxSchema(NyxCustomKind(NamedFixtureKind), [], [
    NyxNamedEventSchema(NyxEvent(NamedOpened), 'OnItemOpened',
      'An item opened; its exact ordinal is between 0 and 10.',
      ncCustom, ncCustom, NyxScalarPayload(NyxIntegerDomain.Range(0, 10))),
    NyxNamedEventSchema(NyxEvent(NamedClosed), 'OnItemClosed',
      'An item closed; a signal without a payload.', ncCustom, ncCustom),
    NyxNamedEventSchema(NyxEvent(NamedDetails), 'OnItemDetails',
      'Owned structured item details.', ncCustom, ncCustom, NyxDataPayload(ndObject)),
    NyxNamedEventSchema(NyxEvent('BrowserOnly'), 'OnBrowserOnly',
      'An explicitly browser-only producer.', ncCustom, ncMissing)
  ]);
  GRegistered := True;
end;

function NamedDesign: TNyxDocument;
var
  LPage: TNyxNode;
begin
  RegisterNamedFixture;
  Result := TNyxDocument.Create;
  LPage := TNyxNode.Create(nkPage, 'home');
  Result.AddPage(LPage);
  LPage.Add(TNyxNode.Create(NyxCustomKind(NamedFixtureKind), 'item-button')
    .Configure.ProjectAs(nkButton).Text('Open item').Done);
  Result.AddPage(TNyxNode.Create(nkPage, 'other'));
end;

function NamedCompanion(out ASource: TNyxText): TNyxDocument;
var
  LSession: TNyxStudioSession;
  LBase: TNyxDocument;
  LHandler: TNyxHandlerRef;
  LLine: Integer;
begin
  LSession := TNyxStudioSession.Create;
  LBase := NamedDesign;
  try
    LSession.Load(TNyxCodec.Encode(LBase));
    LSession.Select('item-button');
    LHandler := LSession.AddCallback(NyxEvent(NamedOpened), LLine);
    LSession.AddCallback(NyxEvent(NamedClosed), LLine);
    LSession.AddCallback(NyxEvent(NamedDetails), LLine);
    ASource := StringReplace(LSession.Source, 'unit nyx.generated.view;',
      'unit nyx.named.fixture;', []);
    ASource := StringReplace(ASource, 'implementation' + #10,
      'function NamedCalls: Integer;' + #10 + #10 + 'implementation' + #10 +
      #10 + 'var GNamedCalls: Integer;' + #10, []);
    ASource := StringReplace(ASource, NyxViewsEnd + #10,
      NyxViewsEnd + #10 + #10 + 'function NamedCalls: Integer;' + #10 +
      'begin' + #10 + '  Result := GNamedCalls;' + #10 + 'end;' + #10, []);
    ASource := StringReplace(ASource, '// TODO: implement ' + LHandler.Name + '.',
      'Inc(GNamedCalls, AEvent.Value.AsInteger);', []);
    Result := LSession.Document.Clone;
  finally
    LBase.Free;
    LSession.Free;
  end;
end;

end.
