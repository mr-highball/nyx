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

unit nyx.test.collections.targets;

{$mode delphi}{$H+}
{$codepage utf8}

interface

{ Actual application hosts consume the shared collection registry contract on
  both targets. This proves mount/navigation/lifetime integration, not list/table/
  tree collection binding or operating-system keyboard/touch interaction. }
function RunNyxCollectionApplicationJourney: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.model,
  nyx.state,
  nyx.collections,
  nyx.collections.registry,
  nyx.test.collections.registry,
  {$IFDEF PAS2JS}
  Web,
  nyx.application.browser;
  {$ELSE}
  nyx.application.lcl;
  {$ENDIF}

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxCollection.Create('Application collection test failed: ' + AMessage);
  end;
  Inc(ACount);
end;

function RunNyxCollectionApplicationJourney: Integer;
var
  LDocument: TNyxDocument;
  LStore: INyxCollection;
  LUnMounted: INyxCollections;
  LDefaults: INyxCollectionSnapshot;
  LKey: TNyxCollectionRef;
  LItem: TNyxItemRef;
  LField: TNyxBooleanFieldRef;
  LRejected: Boolean;
  {$IFDEF PAS2JS}
  LOne: TNyxBrowserApplication;
  LTwo: TNyxBrowserApplication;
  LHostOne: TJSHTMLElement;
  LHostTwo: TJSHTMLElement;
  {$ELSE}
  LOne: TNyxLCLApplication;
  LTwo: TNyxLCLApplication;
  {$ENDIF}
begin
  Result := 0;
  LDocument := CreateNyxCollectionFixture;
  LOne := nil;
  LTwo := nil;
  {$IFDEF PAS2JS}
  LHostOne := nil;
  LHostTwo := nil;
  {$ENDIF}
  try
    {$IFDEF PAS2JS}
    LOne := TNyxBrowserApplication.Create;
    LTwo := TNyxBrowserApplication.Create;
    {$ELSE}
    LOne := TNyxLCLApplication.Create;
    LTwo := TNyxLCLApplication.Create;
    {$ENDIF}
    LRejected := False;
    try
      LUnMounted := LOne.Collections;
    except
      on ENyxState do
      begin
        LRejected := True;
      end;
    end;
    Check(LRejected, 'unmounted application refuses runtime lookup', Result);
    {$IFDEF PAS2JS}
    LHostOne := TJSHTMLElement(document.createElement('div'));
    LHostTwo := TJSHTMLElement(document.createElement('div'));
    document.body.appendChild(LHostOne);
    document.body.appendChild(LHostTwo);
    LOne.Run(LDocument, LHostOne);
    LTwo.Run(LDocument, LHostTwo);
    {$ELSE}
    LOne.Mount(LDocument);
    LTwo.Mount(LDocument);
    {$ENDIF}
    LKey := NyxCollection('tasks/🌙');
    LItem := NyxItem(LKey, 'design/🌙');
    LField := NyxBooleanField('complete');
    LStore := LOne.Collections.Collection(LKey);
    LDefaults := LDocument.Collections.Snapshot(LKey);
    Check((LOne.Collections.Count = 2) and (LTwo.Collections.Count = 2) and
      (LStore.Snapshot.Revision = 0), 'actual mounts seed independent registry state', Result);
    LStore.Update(NyxCollectionItem(LItem).WithValue(LField, True));
    Check(LStore.Snapshot.Item(LItem).GetValue(LField) and
      not LTwo.Collections.Collection(LKey).Snapshot.Item(LItem).GetValue(LField) and
      not LDefaults.Item(LItem).GetValue(LField),
      'mounted sibling applications and saved defaults remain isolated', Result);
    LOne.ShowPage('review-page');
    LOne.ShowPage('home');
    Check((LOne.Collections.Collection(LKey).Snapshot.Revision = 1) and
      LOne.Collections.Collection(LKey).Snapshot.Item(LItem).GetValue(LField),
      'real navigation retains edited collection identity and revision', Result);
  finally
    { Application renderers borrow the document; dispose them first. The
      retained store/defaults contain no application or target-control pointer. }
    LOne.Free;
    LTwo.Free;
    {$IFDEF PAS2JS}

    if LHostOne <> nil then
    begin
      LHostOne.parentNode.removeChild(LHostOne);
    end;

    if LHostTwo <> nil then
    begin
      LHostTwo.parentNode.removeChild(LHostTwo);
    end;
    {$ENDIF}
    LDocument.Free;
  end;
  Check(LStore.Snapshot.Item(LItem).GetValue(LField) and
    not LDefaults.Item(LItem).GetValue(LField),
    'managed store/default snapshots survive actual host and document disposal', Result);
end;

end.
