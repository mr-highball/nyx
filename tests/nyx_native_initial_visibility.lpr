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
program nyx_native_initial_visibility;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Forms, Controls, StdCtrls, Classes, SysUtils,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.disclosure,
  nyx.collections, nyx.collections.view, nyx.collections.view.types,
  nyx.interaction, nyx.codec,
  nyx.render.lcl;

type
  { Native window creation is the observation point, before any subsequent
    renderer Sync can conceal an incorrect initial visibility. Expected policy
    is copied while the factory's node is borrowed; no model owner is retained. }
  TVisibilityProbe = class(TEdit)
  private
    FInitiallyHidden: Boolean;
  protected
    procedure CreateWnd; override;
  public
    constructor ForNode(ANode: TNyxNode; AOwner: TComponent);
  end;

var
  GRendering: Boolean;
  GWrongInitialVisibility: Integer;
  GNativeInitializations: Integer;
  GChecks: Integer;

constructor TVisibilityProbe.ForNode(ANode: TNyxNode; AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitiallyHidden := not NyxInteractionVisible(ANode);
  Text := ANode.Prop(NyxAttributeName(atValue));
end;

procedure TVisibilityProbe.CreateWnd;
begin

  if GRendering then
  begin
    Inc(GNativeInitializations);

    if FInitiallyHidden and Visible then
    begin
      Inc(GWrongInitialVisibility);
    end;
  end;
  inherited CreateWnd;
end;

function ProbeFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := TVisibilityProbe.ForNode(ANode, AOwner);
end;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin

  if not ACondition then
  begin
    raise Exception.Create(AReason);
  end;
  Inc(GChecks);
end;

procedure Run;
var
  LDocument: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LPage: INyxPage;
  LFold: INyxDisclosure;
  LHidden: INyxColumn;
  LTable: INyxTable;
  LKey: TNyxCollectionRef;
  LFoldedInput: TWinControl;
  LHiddenInput: TWinControl;
  LVisibleInput: TWinControl;
  LBefore: TNyxText;
begin
  LDocument := TNyxDocument.Create;
  LRenderer := TNyxLCLRenderer.Create;
  LHost := TForm.Create(nil);
  try
    LHost.SetBounds(0, 0, 640, 640);
    LKey := NyxCollection('initialization');
    LDocument.Collections.Define(LKey,
      NyxCollectionSchema.Text(NyxTextField('Name'), ''),
      [NyxCollectionItem(NyxItem(LKey, 'first'))
        .WithValue(NyxTextField('Name'), 'An accepted record')]);
    LPage := NewNyxPage('home');
    LFold := NewNyxDisclosure('details').WithText('Details');
    LFold.Configure.Value(False).Done;
    LFold.BodyColumn.Add(NewNyxInput('folded-input')
      .Configure.Value(TNyxText('Retained notes 🌙')).Done);
    LPage.Add(LFold);
    LFold := nil;
    LHidden := NewNyxColumn('hidden-parent');
    LHidden.Configure.Visible(False).Done;
    LHidden.Add(NewNyxInput('hidden-input').Configure.Value('Hidden value').Done);
    LPage.Add(LHidden);
    LHidden := nil;
    LPage.Add(NewNyxInput('visible-input').Configure.Value('Visible value').Done);
    LTable := NewNyxTable('records');
    LTable.Binds.Collection(NyxCollectionView(LKey).Column(NyxTextField('Name'), 'Name'));
    LPage.Add(LTable);
    LTable := nil;
    LDocument.AddPage(LPage);
    LPage := nil;
    LBefore := TNyxCodec.Encode(LDocument);
    LRenderer.RegisterFactory(NyxKindName(nkInput), ProbeFactory);
    GRendering := True;
    try
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    finally
      GRendering := False;
    end;
    Check(GNativeInitializations > 0,
      'the real collection mount initialized native sibling controls before the render returned');
    Check(GWrongInitialVisibility = 0,
      'native initialization never exposes an ancestor-hidden or closed-disclosure input');
    LHost.Show;
    Application.ProcessMessages;
    LFoldedInput := TWinControl(LRenderer.InputFor('folded-input'));
    LHiddenInput := TWinControl(LRenderer.InputFor('hidden-input'));
    LVisibleInput := TWinControl(LRenderer.InputFor('visible-input'));
    Check(not LFoldedInput.Visible and not LHiddenInput.Visible and LVisibleInput.Visible,
      'the published native controls retain their complete effective visibility');
    Check(LRenderer.CollectionView('records').Selection.Count = 0,
      'native initialization does not become an accepted user selection');
    LRenderer.Root.Find('details').Configure.Value(True).Done;
    LRenderer.Sync;
    Application.ProcessMessages;
    Check(LRenderer.InputFor('folded-input') = LFoldedInput,
      'opening the disclosure reuses the original input object');
    Check(LFoldedInput.Visible and LFoldedInput.CanFocus,
      'a formerly closed input becomes an ordinary focusable native control');
    Check(TNyxText(TEdit(LFoldedInput).Text) = TNyxText('Retained notes 🌙'),
      'deferred native realization retains exact supplementary Unicode');
    LFoldedInput.SetFocus;
    Check(LFoldedInput.Focused, 'the opened input accepts native focus');
    LRenderer.Root.Find('details').Configure.Value(False).Done;
    LRenderer.Sync;
    Check(not LFoldedInput.Visible and not LHiddenInput.Visible,
      'closing retains the input and respects the independent hidden ancestor');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'runtime visibility never rewrites the authored document');
    LRenderer.Unmount;
    Check(LRenderer.Root = nil, 'ordinary renderer retirement completes after native realization');
    WriteLn('PASS ', GChecks, ' native initial-visibility checks');
  finally
    LPage := nil;
    LFold := nil;
    LHidden := nil;
    LTable := nil;
    LRenderer.Free;
    LHost.Free;
    LDocument.Free;
  end;
end;

begin
  Application.Initialize;
  Application.ShowMainForm := False;
  try
    Run;
  except
    on E: Exception do
    begin
      WriteLn('FAIL ', E.Message);
      DumpExceptionBackTrace(Output);
      ExitCode := 1;
    end;
  end;
end.
