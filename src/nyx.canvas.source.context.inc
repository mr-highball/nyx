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

{ Both adapters include this preparation; only their normal Present method
  touches target faces. No independent source setter can publish half a frame. }

type
  { One private source owner in an existing prepared group. Candidate is owned
    until Install transfers it to the view. Former is owned only after that swap.
    The outer view lease outlives retirement; no source retains its view. }
  TCanvasSourcePublication = class(TInterfacedObject, INyxPreparedPublication,
    INyxPublicationReadiness)
  public
    Owner: TCanvasView;
    Before: TNyxDocument;
    Candidate: TNyxDocument;
    Former: TNyxDocument;
    Expected: Integer;
    Installed: Boolean;
    Retired: Boolean;
    destructor Destroy; override;
    procedure Validate;
    procedure CheckReady;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;

destructor TCanvasSourcePublication.Destroy;
begin
  Retire;
  inherited Destroy;
end;

procedure TCanvasSourcePublication.Validate;
begin
  CheckReady;
end;

procedure TCanvasSourcePublication.CheckReady;
begin

  if Retired or Installed or (Owner = nil) or not Owner.FConnected or
    not Owner.FUpdating or not Owner.FSourcePreparing or (Candidate = nil) or
    (Owner.FPreparingDocument <> Candidate) or (Owner.FDocument <> Before) or
    (Owner.FPresentationRevision <> Expected) then
  begin
    raise ENyxCanvasScene.Create('Canvas source preparation is stale or retired');
  end;
end;

procedure TCanvasSourcePublication.Install;
begin
  Former := Owner.FDocument;
  Owner.FDocument := Candidate;
  Candidate := nil;
  Installed := True;
end;

procedure TCanvasSourcePublication.Notify;
begin
  { Source already agrees with every model before target reconciliation. }
end;

procedure TCanvasSourcePublication.Retire;
begin

  if Retired then
  begin
    Exit;
  end;
  Retired := True;
  Owner := nil;
  Before := nil;
  FreeAndNil(Candidate);
  FreeAndNil(Former);
end;

function TCanvasView.ApplySourceLayout(ADocument: TNyxDocument;
  const APlan: INyxCanvasLayoutPlan; const ACamera: TNyxCanvasCamera;
  AExpectedPresentation: Integer; const ACompanion: INyxPreparedPublication;
  const ACancellation: INyxCancellationScope): Boolean;
var
  LLease: INyxCanvasWorkspaceView;
  LSource: TCanvasSourcePublication;
  LSourceLease: INyxPreparedPublication;
  LGroup: INyxPreparedPublication;
  LDocument: TNyxDocument;
  LIndex: Integer;
begin
  RequireConnected;
  Result := False;

  if FUpdating or FSourcePreparing or (ADocument = nil) or (APlan = nil) or
    (AExpectedPresentation <> FPresentationRevision) then
  begin
    raise ENyxCanvasScene.Create('Canvas source layout requires an idle exact current frame');
  end;

  if (ACancellation <> nil) and ACancellation.Cancelled then
  begin
    Exit;
  end;
  LLease := Self;
  LDocument := nil;
  FSourcePreparing := True;
  try
    LDocument := ADocument.Clone;
    LDocument.Validate;
    RequireConnected;

    if FResources <> nil then
    begin
      for LIndex := 0 to LDocument.Resources.Count - 1 do
      begin

        if FResources.Declaration(LDocument.Resources.Reference(LIndex),
          LDocument.Resources.Locale(LIndex)).ToData.ToJSON <>
          LDocument.Resources.Definition(LDocument.Resources.Reference(LIndex),
            LDocument.Resources.Locale(LIndex)).ToData.ToJSON then
        begin
          raise ENyxCanvasScene.Create(
            'Refresh the application resource owner before replacing its source declarations');
        end;
      end;
    end;

    if AExpectedPresentation <> FPresentationRevision then
    begin
      raise ENyxCanvasScene.Create('Canvas changed while its source was captured');
    end;
    LSource := TCanvasSourcePublication.Create;
    LSourceLease := LSource;
    LSource.Owner := Self;
    LSource.Before := FDocument;
    LSource.Candidate := LDocument;
    LDocument := nil;
    LSource.Expected := AExpectedPresentation;
    FPreparingDocument := LSource.Candidate;

    if ACompanion = nil then
    begin
      LGroup := PrepareNyxGroup([LSourceLease]);
    end
    else
    begin
      LGroup := PrepareNyxGroup([LSourceLease, ACompanion]);
    end;
    try
      try
        Result := Present(ACamera, True, APlan, AExpectedPresentation,
          ACancellation, LGroup);
      except
        on ENyxCanvasPublicationCancelled do
        begin
          Result := False;
        end;
      end;
    finally
      FPreparingDocument := nil;
      LGroup.Retire;
    end;
  finally
    FPreparingDocument := nil;
    FSourcePreparing := False;
    LGroup := nil;
    LSourceLease := nil;
    LDocument.Free;

    if FConnected and (FResources <> nil) then
    begin
      FResources.Wake;
    end;
  end;
end;
