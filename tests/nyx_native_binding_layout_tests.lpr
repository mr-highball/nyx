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
program nyx_native_binding_layout_tests;

{$mode delphi}{$H+}{$codepage utf8}

uses
  Interfaces, Classes, SysUtils, Types, Forms, Controls,
  nyx.text, nyx.types, nyx.model, nyx.controls, nyx.render.lcl, nyx.widgets.lcl;

type
  { Borrowed actual native faces and copied geometry. The vector is discarded
    before replacing their frame; it neither owns controls nor extends a mount. }
  TFaceSnapshot = record
    Control: TControl;
    Bounds: TRect;
  end;

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AMessage: String);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create(AMessage);
  end;
end;

function LeafID(AIndex: Integer): TNyxText;
begin
  Result := 'action-' + IntToStr(AIndex);
end;

{ Every replacement deliberately reuses design identities. Control associations
  must belong to the new realized objects, including after empty/smaller frames. }
function MakeDocument(ACount: Integer; const ACaption: TNyxText): TNyxDocument;
var
  LPage: INyxPage;
  LRow: INyxRow;
  LIndex: Integer;
begin
  Result := TNyxDocument.Create;
  try
    LPage := NewNyxPage('layout');
    Result.AddPage(LPage);
    for LIndex := 0 to ACount - 1 do
    begin

      if (LIndex mod 8) = 0 then
      begin
        LRow := NewNyxRow('row-' + IntToStr(LIndex div 8));
        LPage.Add(LRow);
      end;
      LRow.Add(NewNyxButton(LeafID(LIndex)).WithText(ACaption));
    end;
  except
    Result.Free;
    raise;
  end;
end;

{ A partial candidate has already created its page and row when this factory
  refuses. The accepted frame must remain usable after candidate destruction. }
function RefuseButton(ANode: TNyxNode; AOwner: TComponent): TControl;
begin
  Result := nil;
  raise ENyxModel.Create('Intentional binding candidate refusal');
end;

procedure Run(AHost: TForm);
const
  CLeaves = 512;
  CRefreshes = 24;
  CReplacementCounts: array[0..5] of Integer = (17, 17, 0, 65, 3, 513);
var
  LDocument: TNyxDocument;
  LCandidate: TNyxDocument;
  LRenderer: TNyxLCLRenderer;
  LFaces: array of TFaceSnapshot;
  LIndex: Integer;
  LPass: Integer;
  LCount: Integer;
  LStarted: QWord;
  LSyncMilliseconds: QWord;
  LResizeMilliseconds: QWord;
  LCaption: TNyxText;
  LFace: TControl;
  LRefused: Boolean;
begin
  LDocument := MakeDocument(CLeaves, 'Explore');
  LCandidate := nil;
  LRenderer := TNyxLCLRenderer.Create;
  try
    LRenderer.Render(LDocument, LDocument.Pages[0], AHost);
    SetLength(LFaces, CLeaves);
    for LIndex := 0 to CLeaves - 1 do
    begin
      LFaces[LIndex].Control := LRenderer.ControlFor(LeafID(LIndex));
      LFaces[LIndex].Bounds := LFaces[LIndex].Control.BoundsRect;
    end;
    LStarted := GetTickCount64;
    for LPass := 1 to CRefreshes do
    begin
      LRenderer.Sync;
    end;
    LSyncMilliseconds := GetTickCount64 - LStarted;
    LStarted := GetTickCount64;
    for LPass := 1 to CRefreshes do
    begin
      AHost.ClientWidth := 968;
      AHost.ClientWidth := 960;
    end;
    LResizeMilliseconds := GetTickCount64 - LStarted;
    for LIndex := 0 to CLeaves - 1 do
    begin
      LFace := LRenderer.ControlFor(LeafID(LIndex));
      Check((LFace = LFaces[LIndex].Control) and
        EqualRect(LFace.BoundsRect, LFaces[LIndex].Bounds),
        'Repeated Sync/host resize retains every exact face and allocation');
    end;
    WriteLn('PROFILE native binding layout leaves=', CLeaves, ' sync-calls=', CRefreshes,
      ' sync-ms=', LSyncMilliseconds, ' resize-pairs=', CRefreshes,
      ' resize-ms=', LResizeMilliseconds);
    LFaces := nil;
    for LPass := Low(CReplacementCounts) to High(CReplacementCounts) do
    begin
      LCount := CReplacementCounts[LPass];
      LCaption := 'Frame ' + IntToStr(LPass);
      LCandidate := MakeDocument(LCount, LCaption);
      LRenderer.Render(LCandidate, LCandidate.Pages[0], AHost);
      FreeAndNil(LDocument);
      LDocument := LCandidate;
      LCandidate := nil;
      LRenderer.Sync;
      for LIndex := 0 to LCount - 1 do
      begin
        LFace := LRenderer.ControlFor(LeafID(LIndex));
        Check((TNyxLCLButton(LFace).Caption = LCaption) and (LFace.Width > 0),
          'Replacement frame resolves its own same-ID controls and geometry');
      end;
    end;
    LFace := LRenderer.ControlFor(LeafID(0));
    LRenderer.RegisterFactory(NyxKindName(nkButton), RefuseButton);
    LRefused := False;
    try
      LRenderer.Render(LDocument, LDocument.Pages[0], AHost);
    except
      on LException: ENyxModel do
      begin
        LRefused := LException.Message = 'Intentional binding candidate refusal';
      end;
    end;
    LRenderer.Sync;
    Check(LRefused and (LRenderer.ControlFor(LeafID(0)) = LFace),
      'Partially built refused frame cannot corrupt accepted binding lookup');
    LRenderer.Unmount;
    Check(not LRenderer.TryRefresh(LDocument, LDocument.Pages[0], False),
      'Unmounted frame retains no eligible bindings');
    FreeAndNil(LRenderer);
    LRenderer := TNyxLCLRenderer.Create;
    LRenderer.Render(LDocument, LDocument.Pages[0], AHost);
    LRenderer.Sync;
    Check(TNyxLCLButton(LRenderer.ControlFor(LeafID(512))).Caption = LCaption,
      'A fresh mount reconstructs associations after complete retirement');
  finally
    LFaces := nil;
    LRenderer.Free;
    LCandidate.Free;
    LDocument.Free;
  end;
end;

var
  LHost: TForm;
begin
  LHost := nil;
  try
    try
      Application.Initialize;
      LHost := TForm.CreateNew(nil);
      LHost.ClientWidth := 960;
      LHost.ClientHeight := 720;
      LHost.Show;
      Run(LHost);
      WriteLn('PASS ', GChecks, ' native binding layout checks');
    finally
      LHost.Free;
    end;
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
