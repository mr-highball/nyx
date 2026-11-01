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
program nyx_choice_model_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  SysUtils, nyx.text, nyx.types, nyx.data, nyx.choices, nyx.model,
  nyx.controls, nyx.select.configuration, nyx.codec, nyx.composition
  {$ifdef PAS2JS}, Web{$endif};

type
  { Foreign immutable-data boundary. Non-data getters deliberately fail, proving
    the model adopts the descriptor once instead of trusting provider storage.
    Reenter borrows an explicitly live test receiver only during ToData. }
  TForeignChoices = class(TInterfacedObject, INyxChoiceItems)
  public
    Data: TNyxDataValue;
    Reads: Integer;
    Reenter: TNyxNode;
    destructor Destroy; override;
    function GetCount: Integer;
    function GetTextBytes: Integer;
    function Item(AIndex: Integer): TNyxChoiceItem;
    function IndexOf(const AKey: TNyxChoiceRef): Integer;
    function WithChoice(const AKey: TNyxChoiceRef;
      const ACaption: TNyxText): INyxChoiceItems;
    function ToData: TNyxDataValue;
  end;

var
  GChecks: Integer;
  GRetired: Integer;

procedure Check(AValue: Boolean; const AReason: TNyxText);
begin

  if not AValue then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

destructor TForeignChoices.Destroy;
begin
  Inc(GRetired);
  inherited Destroy;
end;

function TForeignChoices.GetCount: Integer;
begin
  Result := 0;
  raise Exception.Create('Foreign Count must not run');
end;

function TForeignChoices.GetTextBytes: Integer;
begin
  Result := 0;
  raise Exception.Create('Foreign TextBytes must not run');
end;

function TForeignChoices.Item(AIndex: Integer): TNyxChoiceItem;
begin
  Result := Default(TNyxChoiceItem);
  raise Exception.Create('Foreign Item must not run');
end;

function TForeignChoices.IndexOf(const AKey: TNyxChoiceRef): Integer;
begin
  Result := -1;
  raise Exception.Create('Foreign IndexOf must not run');
end;

function TForeignChoices.WithChoice(const AKey: TNyxChoiceRef;
  const ACaption: TNyxText): INyxChoiceItems;
begin
  Result := nil;
  raise Exception.Create('Foreign builder must not run');
end;

function TForeignChoices.ToData: TNyxDataValue;
begin
  Inc(Reads);

  if Reenter <> nil then
  begin
    Reenter.RemoveChoiceItems;
  end;
  Result := Data;
end;

procedure RefuseDesign(ADocument: TNyxDocument; const AReason: TNyxText);
var
  LRefused: Boolean;
begin
  LRefused := False;
  try
    TNyxCodec.Encode(ADocument);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused, AReason);
end;

procedure Cases;
const
  CKey: TNyxText = 'stable/🌙';
  CCaption: TNyxText = 'Friendly caption 🌙';
  { Every earlier document-owned field promoted by version seventeen needs its
    original opaque meaning protected, even when its new registry is empty. }
  CPromoted: array[0..7] of TNyxText =
    ('collections', 'presentations', 'menus', 'resources',
     'canvases', 'flows', 'programs', 'diagrams');
var
  LSelect: INyxSelect;
  LConfiguration: INyxSelectConfiguration;
  LBaseline: INyxChoiceItems;
  LProvider: INyxChoiceItems;
  LForeign: TForeignChoices;
  LDocument: TNyxDocument;
  LRestored: TNyxDocument;
  LClone: TNyxDocument;
  LPage: TNyxNode;
  LNode: TNyxNode;
  LView: TNyxNode;
  LBefore: TNyxText;
  LWire: TNyxText;
  LRefused: Boolean;
  LIndex: Integer;
begin
  LSelect := NewNyxSelect('operation');
  LSelect.Configure.Items('Legacy first' + #10 + 'Legacy second').Value('unknown');
  LConfiguration := NyxSelectConfigure(LSelect);
  LBaseline := NewNyxChoices.WithChoice(NyxChoice(''), 'None')
    .WithChoice(NyxChoice(CKey), CCaption)
    .WithChoice(NyxChoice('other'), CCaption);
  LConfiguration.Choices(LBaseline);
  Check(LSelect.Node.HasChoiceItems and (LSelect.Node.ChoiceItems.Count = 3),
    'managed typed configuration adopts choices on the specialized select');
  Check((LSelect.Node.Prop('value') = 'unknown') and
    (LSelect.Node.Prop('items') = 'Legacy first' + #10 + 'Legacy second'),
    'choice admission never invents a selected value or destroys legacy Items');

  LForeign := TForeignChoices.Create;
  LForeign.Data := LBaseline.ToData;
  LProvider := LForeign;
  LSelect.Node.SetChoiceItems(LProvider);
  Check((LForeign.Reads = 1) and (LSelect.Node.ChoiceItems <> LProvider),
    'one foreign ToData is normalized into an independent builtin implementation');
  LForeign.Data := NewNyxChoices.ToData;
  Check(LSelect.Node.ChoiceItems.Count = 3, 'foreign later changes cannot mutate the adopted snapshot');
  LProvider := nil;
  Check(GRetired = 1, 'the node does not retain a foreign choice provider');
  LBefore := LSelect.Node.ChoiceItems.ToData.ToJSON;
  LForeign := TForeignChoices.Create;
  LForeign.Data := TNyxDataValue.ParseJSON('{"version":1,"items":[{"value":"x","caption":3}]}');
  LProvider := LForeign;
  LRefused := False;
  try
    LSelect.Node.SetChoiceItems(LProvider);
  except
    on ENyxChoice do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSelect.Node.ChoiceItems.ToData.ToJSON = LBefore),
    'malformed foreign descriptors preserve the exact accepted choices');
  LForeign.Data := LBaseline.ToData;
  LForeign.Reenter := LSelect.Node;
  LRefused := False;
  try
    LSelect.Node.SetChoiceItems(LProvider);
  except
    on ENyxModel do
    begin
      LRefused := True;
    end;
  end;
  Check(LRefused and (LSelect.Node.ChoiceItems.ToData.ToJSON = LBefore),
    'foreign reentrant removal refuses before replacing or erasing accepted choices');
  LProvider := nil;
  LConfiguration.Choices(NewNyxChoices).InheritChoices;
  Check(not LSelect.Node.HasChoiceItems, 'inheritance is distinct from an empty set');
  LConfiguration.Choices(LBaseline);
  LSelect := nil;
  Check(LConfiguration.Done.Node.ChoiceItems.Item(1).Key.Value = CKey,
    'the additive facade keeps the specialized control alive after caller release');

  LDocument := TNyxDocument.Create;
  try
    LPage := TNyxNode.Create(nkPage, 'home');
    LDocument.AddPage(LPage);
    LPage.Add(LConfiguration.Done);
    LConfiguration := nil;
    LWire := TNyxCodec.Encode(LDocument);
    Check(TNyxDataValue.ParseJSON(LWire).Field('version').AsInteger = 17,
      'typed choices deliberately select the new design version');
    LRestored := TNyxCodec.Decode(LWire);
    try
      LNode := LRestored.Find('operation');
      Check((LNode.ChoiceItems.Item(1).Key.Value = CKey) and
        (LNode.ChoiceItems.Item(1).Caption = CCaption) and
        (LNode.ChoiceItems.Item(2).Caption = CCaption),
        'save/open preserves exact Unicode, duplicate captions and distinct values');
      Check(TNyxCodec.Encode(LRestored) = LWire, 'new design encoding is deterministic after reopening');
      LClone := LRestored.Clone;
      try
        LClone.Find('operation').SetChoiceItems(NewNyxChoices);
        Check(LClone.HasChoiceItems and
          (LClone.Find('operation').ChoiceItems.Count = 0) and (LNode.ChoiceItems.Count = 3),
          'empty masks in clones do not alter the original immutable set');
        LClone.Find('operation').RemoveChoiceItems;
        Check(not LClone.HasChoiceItems and
          (TNyxDataValue.ParseJSON(TNyxCodec.Encode(LClone)).Field('version').AsInteger = 1),
          'removing the final choice declaration restores the earlier canonical version');
      finally
        LClone.Free;
      end;
      LView := RealizeNyxView(LRestored, LRestored.Pages[0]);
      try
        Check((LView.Find('operation') <> nil) and
          (LView.Find('operation').ChoiceItems.ToData.ToJSON = LBefore),
          'runtime composition carries the typed set without retaining its design document');
      finally
        LView.Free;
      end;
    finally
      LRestored.Free;
    end;

    for LIndex := 0 to High(CPromoted) do
    begin
      LDocument.Extensions.SetValue(NyxExtension(CPromoted[LIndex]), NyxData(TNyxText('opaque')));
      RefuseDesign(LDocument, 'promoted legacy root data must refuse instead of changing meaning');
      LDocument.Extensions.Remove(NyxExtension(CPromoted[LIndex]));
    end;
    LNode := LDocument.Find('operation');
    LNode.RemoveChoiceItems;
    LPage.Extensions.SetValue(NyxExtension(NyxChoiceItemsWireField), NyxData(TNyxText('legacy 🌙')));
    LWire := TNyxCodec.Encode(LDocument);
    LRestored := TNyxCodec.Decode(LWire);
    try
      Check(not LRestored.HasChoiceItems and
        (LRestored.Pages[0].Extensions.Value(NyxExtension(NyxChoiceItemsWireField)).AsText =
          TNyxText('legacy 🌙')), 'old opaque choiceItems retain extension meaning and Unicode');
    finally
      LRestored.Free;
    end;
    LNode.SetChoiceItems(LBaseline);
    RefuseDesign(LDocument, 'new choices elsewhere must not promote an old opaque node field');
    LPage.Extensions.Remove(NyxExtension(NyxChoiceItemsWireField));
    LPage.SetChoiceItems(NewNyxChoices);
    RefuseDesign(LDocument, 'an empty set still requires a select-capable projection');
    LPage.RemoveChoiceItems;
    Check(TNyxCodec.Encode(LDocument) <> '', 'failed exports leave the document usable');
  finally
    LDocument.Free;
  end;
end;

procedure ReusableCases;
var
  LDocument: TNyxDocument;
  LPage: INyxPage;
  LDefinition: INyxSelect;
  LInstance: INyxComponent;
  LPanel: INyxPanel;
  LView: TNyxNode;
begin
  LDocument := TNyxDocument.Create;
  try
    LDefinition := NewNyxSelect('choice-template');
    NyxSelectConfigure(LDefinition).Choices(NewNyxChoices
      .WithChoice(NyxChoice('default'), 'Inherited choice'));
    LDocument.AddComponent(LDefinition);
    LPage := NewNyxPage('home');
    LDocument.AddPage(LPage);
    LInstance := NewNyxComponent('inherited');
    LInstance.Reference := NyxComponent('choice-template');
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('masked');
    LInstance.Reference := NyxComponent('choice-template');
    LInstance.Node.SetChoiceItems(NewNyxChoices);
    LPage.Add(LInstance);
    LInstance := NewNyxComponent('replaced');
    LInstance.Reference := NyxComponent('choice-template');
    LInstance.Node.SetChoiceItems(NewNyxChoices.WithChoice(NyxChoice('other'), 'Local choice'));
    LPage.Add(LInstance);
    LDocument.Validate;
    LView := RealizeNyxView(LDocument, LPage.Node);
    try
      Check(LView.Children[0].ChoiceItems.Item(0).Key.Value = 'default',
        'a reusable select inherits its definition choices');
      Check(LView.Children[1].HasChoiceItems and (LView.Children[1].ChoiceItems.Count = 0),
        'an explicit empty instance masks its inherited choices');
      Check((LView.Children[2].ChoiceItems.Item(0).Key.Value = 'other') and
        (LDefinition.Node.ChoiceItems.Item(0).Key.Value = 'default'),
        'an instance choice replacement leaves the independent definition exact');
    finally
      LView.Free;
    end;
    LPanel := NewNyxPanel('panel-template');
    LDocument.AddComponent(LPanel);
    LInstance := NewNyxComponent('incompatible');
    LInstance.Reference := NyxComponent('panel-template');
    LInstance.Node.SetChoiceItems(NewNyxChoices);
    LPage.Add(LInstance);
    RefuseDesign(LDocument, 'deferred instance admission must reject an effective non-select root');
  finally
    LInstance := nil;
    LDefinition := nil;
    LPanel := nil;
    LPage := nil;
    LDocument.Free;
  end;
end;

begin
  try
    Cases;
    ReusableCases;
    {$ifdef PAS2JS}
    document.body.setAttribute('data-choice-model', 'passed');
    document.body.setAttribute('data-choice-checks', IntToStr(GChecks));
    document.body.appendChild(document.createTextNode(
      'PASS ' + IntToStr(GChecks) + ' choice model checks'));
    {$else}
    WriteLn('PASS ', GChecks, ' choice model checks');
    {$endif}
  except
    on E: Exception do
    begin
      {$ifdef PAS2JS}
      document.body.setAttribute('data-choice-model', 'failed');
      document.body.appendChild(document.createTextNode(E.Message));
      {$else}
      WriteLn('FAIL after ', GChecks, ' checks: ', E.Message);
      Halt(1);
      {$endif}
    end;
  end;
end.
