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

unit nyx.test.controls.targets;

{$mode delphi}{$H+}
{$codepage utf8}

interface

function RunNyxManagedTargetJourney: Integer;

implementation

uses
  SysUtils,
  nyx.text,
  nyx.types,
  nyx.model,
  nyx.controls,
  nyx.test.controls,
  nyx.test.collections.targets,
  nyx.test.collections.controls,
  {$IFDEF PAS2JS}
  Web,
  nyx.render.browser;
  {$ELSE}
  Classes,
  Controls,
  Forms,
  StdCtrls,
  nyx.render.lcl;
  {$ENDIF}

var
  GRejectedRoot: INyxControl;

{$IFDEF PAS2JS}
function RejectRetainedFactory(ANode: TNyxNode): TJSHTMLElement;
{$ELSE}
function RejectRetainedFactory(ANode: TNyxNode; AOwner: TComponent): TControl;
{$ENDIF}
begin
  Result := nil;
  GRejectedRoot := RetainNyxControl(ANode);
  raise ENyxModel.Create('Intentional retained factory failure');
end;

procedure Check(ACondition: Boolean; const AMessage: TNyxText; var ACount: Integer);
begin

  if not ACondition then
  begin
    raise ENyxModel.Create('FAIL managed target: ' + AMessage);
  end;
  Inc(ACount);
end;

function RunNyxManagedTargetJourney: Integer;
var
  LDocument: TNyxDocument;
  LBadge: INyxBadge;
  LMemo: INyxMemo;
  LPart: INyxMemo;
  LRuntimeRoot: INyxControl;
  LRuntimeMemo: INyxMemo;
  LRejected: Boolean;
  {$IFDEF PAS2JS}
  LRenderer: TNyxBrowserRenderer;
  LHost: TJSHTMLElement;
  LInput: TJSHTMLTextAreaElement;
  {$ELSE}
  LRenderer: TNyxLCLRenderer;
  LHost: TForm;
  LInput: TMemo;
  {$ENDIF}
begin
  Result := 0;
  LDocument := CreateNyxManagedFixture(LBadge, LMemo);
  {$IFDEF PAS2JS}
  LHost := TJSHTMLElement(document.createElement('div'));
  document.body.appendChild(LHost);
  LRenderer := TNyxBrowserRenderer.Create;
  {$ELSE}
  LHost := TForm.CreateNew(nil);
  LHost.SetBounds(0, 0, 900, 900);
  LRenderer := TNyxLCLRenderer.Create;
  {$ENDIF}
  try
    LPart := (RetainNyxControl(LDocument.Find('managed-discussion')) as
      INyxCommentThread).ReplyMemo;
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    {$IFDEF PAS2JS}
    LInput := TJSHTMLTextAreaElement(LRenderer.ElementFor('managed-reply').querySelector('textarea'));
    Check(LRenderer.ElementFor('managed-status').textContent = 'Ready / 🌙',
      'unrelated badge implementation projects through its public descriptor', Result);
    Check(LInput.value = LMemo.Value, 'specialized memo value reaches the browser textarea', Result);
    Check(TJSHTMLTextAreaElement(LRenderer.ElementFor(LPart.ID).querySelector('textarea')).value =
      LPart.Value, 'typed compound part reaches the browser textarea', Result);
    {$ELSE}
    LInput := TMemo(LRenderer.InputFor('managed-reply'));
    Check(TLabel(LRenderer.ControlFor('managed-status')).Caption = TNyxText('Ready / 🌙'),
      'unrelated badge implementation projects through its public descriptor', Result);
    Check(LInput.Text = LMemo.Value, 'specialized memo value reaches the native memo', Result);
    Check(TMemo(LRenderer.InputFor(LPart.ID)).Text = LPart.Value,
      'typed compound part reaches the native memo', Result);
    {$ENDIF}

    LMemo.Value := 'Updated through INyxMemo / 🌙';
    LBadge.Text := 'Updated badge';
    LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    LRuntimeRoot := RetainNyxControl(LRenderer.Root);
    LRuntimeMemo := RetainNyxControl(LRenderer.Root.Find('managed-reply')) as INyxMemo;
    {$IFDEF PAS2JS}
    LInput := TJSHTMLTextAreaElement(LRenderer.ElementFor('managed-reply').querySelector('textarea'));
    Check(LInput.value = 'Updated through INyxMemo / 🌙',
      'typed authored change reconstructs an editable browser field', Result);
    Check(LRenderer.ElementFor('managed-status').textContent = 'Updated badge',
      'alternative interface remains editable through remount', Result);
    {$ELSE}
    LInput := TMemo(LRenderer.InputFor('managed-reply'));
    Check(LInput.Text = TNyxText('Updated through INyxMemo / 🌙'),
      'typed authored change reconstructs an editable native field', Result);
    Check(TLabel(LRenderer.ControlFor('managed-status')).Caption = 'Updated badge',
      'alternative interface remains editable through remount', Result);
    {$ENDIF}
    { A failing user factory can retain its candidate model. Releasing the
      candidate must preserve that interface and the already mounted view. }
    LRenderer.RegisterFactory(NyxKindName(nkColumn), @RejectRetainedFactory);
    LRejected := False;
    try
      LRenderer.Render(LDocument, LDocument.Pages[0], LHost);
    except
      on LException: ENyxModel do
      begin
        LRejected := LException.Message = 'Intentional retained factory failure';
      end;
    end;
    Check(LRejected and (LRenderer.Root = LRuntimeRoot.Node),
      'retained failed candidate preserves the admitted target view and original diagnostic', Result);
    Check((GRejectedRoot <> nil) and GRejectedRoot.Node.IsRealized,
      'a failed factory can safely retain its rejected model', Result);

  finally
    { Renderer borrows the document; dispose its controls first. Component
      interfaces retain their model meaning independently of target controls. }
    LRenderer.Free;
    {$IFDEF PAS2JS}
    LHost.parentNode.removeChild(LHost);
    {$ELSE}
    LHost.Free;
    {$ENDIF}
    LDocument.Free;
    GRejectedRoot := nil;
  end;
  Check((LMemo.Value = TNyxText('Updated through INyxMemo / 🌙')) and
    (LBadge.Text = 'Updated badge'), 'retained interfaces remain valid after target/document disposal', Result);
  LMemo.Value := 'After disposal';
  Check(LMemo.Value = 'After disposal', 'surviving typed control still admits an authored edit', Result);
  Check((LRuntimeRoot.Node.IsRealized) and
    (LRuntimeMemo.Value = TNyxText('Updated through INyxMemo / 🌙')),
    'managed runtime interfaces survive renderer-owned root disposal', Result);
  Inc(Result, RunNyxCollectionApplicationJourney);
  Inc(Result, RunNyxCollectionControlJourney);
end;

end.
