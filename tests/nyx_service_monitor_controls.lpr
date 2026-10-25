{ Nyx. Copyright (c) 2020 mr-highball. MIT license; see LICENSE. }
program nyx_service_monitor_controls;

{$mode delphi}{$H+}{$codepage utf8}

uses Interfaces, Classes, SysUtils, Forms, Controls, Grids,
  nyx.text, nyx.model, nyx.codec, nyx.codegen, nyx.controls,
  nyx.collections.presentation,
  nyx.collections, nyx.collections.mount, nyx.application.lcl,
  nyx.service.monitor
  {$ifdef NYX_SERVICE_GENERATED}, nyx.generated.view{$endif};

var
  GChecks: Integer;

procedure Check(ACondition: Boolean; const AReason: TNyxText);
begin
  Inc(GChecks);

  if not ACondition then
  begin
    raise Exception.Create('Service monitor: ' + AReason);
  end;
end;

{ Compare independently mounted body-density choices on real native grids.
  A runtime density update already preserves headers; authored initial density
  must do the same. An optional title-font probe checks actual caption metrics.
  The probe owns its cloned document until its host retires. }
function HeaderHeightWithDensity(ADocument: TNyxDocument; AWidth: Integer;
  ADensity: TNyxTableRowDensity; AUseLegacyHeader: Boolean = False;
  ATitlePixels: Integer = 0): Integer;
var
  LDocument: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LTable: TNyxNode;
  LPolicy: TNyxTablePresentationPolicy;
  LGrid: TStringGrid;
begin
  LDocument := ADocument.Clone;
  LApplication := nil;
  try
    LTable := LDocument.Find(ServiceTableID);
    LPolicy := LTable.CollectionView.TablePresentationPolicy.Density(ADensity);

    if AUseLegacyHeader then
    begin
      LPolicy := LPolicy.UseDefaultHeader;
    end;
    LTable.SetCollectionView(LTable.CollectionView.TablePresentation(LPolicy));
    LApplication := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 760;
    LApplication.Window.Show;
    Application.ProcessMessages;
    LGrid := TStringGrid(LApplication.View.ControlFor(ServiceTableID));

    if ATitlePixels > 0 then
    begin
      LGrid.TitleFont.Height := -ATitlePixels;
      LApplication.View.CollectionMount(ServiceTableID).Refresh;
      Application.ProcessMessages;
    end;
    Result := LGrid.RowHeights[0];
  finally
    LApplication.Free;
    LDocument.Free;
  end;
end;

{ Native pinned data panes are ordinary grids parented by the main table. Count
  only visible, nonempty panes wholly inside that table's current client frame;
  a declared pin alone cannot establish that a compact user can reach it. }
function VisibleTablePanes(ATable: TControl): Integer;
var
  LHost: TWinControl;
  LPane: TControl;
  LIndex: Integer;
begin
  Result := 0;

  if not (ATable is TStringGrid) or not ATable.IsVisible then
  begin
    Exit;
  end;
  Result := 1;
  LHost := TWinControl(ATable);
  for LIndex := 0 to LHost.ControlCount - 1 do
  begin
    LPane := LHost.Controls[LIndex];

    if (LPane is TStringGrid) and LPane.IsVisible and
      (LPane.Width > 0) and (LPane.Height > 0) and (LPane.Left >= 0) and
      (LPane.Left + LPane.Width <= LHost.ClientWidth) then
    begin
      Inc(Result);
    end;
  end;
end;

{ Explicit UTF-8 bytes preserve the generated program's portable text. The
  fixture refuses existing outputs rather than replacing another run's source. }
procedure WriteSource(const APath: String; const AText: TNyxText);
var
  LBytes: UTF8String;
  LStream: TFileStream;
begin

  if FileExists(APath) then
  begin
    raise Exception.Create('The generated source output already exists');
  end;
  LBytes := UTF8Encode(AText);
  LStream := TFileStream.Create(APath, fmCreate);
  try

    if Length(LBytes) > 0 then
    begin
      LStream.WriteBuffer(LBytes[1], Length(LBytes));
    end;
  finally
    LStream.Free;
  end;
end;

procedure Journey(AWidth: Integer);
var
  LDocument: TNyxDocument;
  LDecoded: TNyxDocument;
  LApplication: TNyxLCLApplication;
  LPeer: TNyxLCLApplication;
  LMount: INyxCollectionMount;
  LGateway: TNyxItemRef;
  LBefore: TNyxText;
  LEdited: TNyxText;
begin
  {$ifdef NYX_SERVICE_GENERATED}
  LDocument := BuildNyxDocument;
  {$else}
  LDocument := BuildServiceMonitorDocument;
  {$endif}
  LApplication := nil;
  LPeer := nil;
  try
    LBefore := TNyxCodec.Encode(LDocument);
    LDecoded := TNyxCodec.Decode(LBefore);
    try
      Check(TNyxCodec.Encode(LDecoded) = LBefore,
        'the complete authored table and collection survive persistence');
    finally
      LDecoded.Free;
    end;

    if ParamCount > 1 then
    begin
      WriteSource(ParamStr(2), TNyxCodegen.Generate(LDocument));
    end;
    LApplication := TNyxLCLApplication.Create;
    LPeer := TNyxLCLApplication.Create;
    LApplication.Mount(LDocument);
    LPeer.Mount(LDocument);
    LApplication.Window.ClientWidth := AWidth;
    LApplication.Window.ClientHeight := 760;
    LApplication.Window.Show;
    Application.ProcessMessages;
    LMount := LApplication.View.CollectionMount(ServiceTableID);
    Check((LMount <> nil) and LMount.Connected,
      'the public sample mounts its actual native collection adapter');
    Check(LApplication.View.ControlFor(ServiceTableID).IsVisible,
      'the table is visible at the requested native width');
    Check(VisibleTablePanes(LApplication.View.ControlFor(ServiceTableID)) = 3,
      'both pinned panes remain visible beside the center at this width');
    Check(HeaderHeightWithDensity(LDocument, AWidth, ntrdCompact) =
      TStringGrid(LApplication.View.ControlFor(ServiceTableID)).RowHeights[0],
      'initial body density leaves the independent query/column header unchanged');
    Check(TStringGrid(LApplication.View.ControlFor(ServiceTableID)).RowHeights[0] <
      HeaderHeightWithDensity(LDocument, AWidth, ntrdComfortable, True),
      'the explicit single-line header releases space while preserving its toolbar');
    Check(HeaderHeightWithDensity(LDocument, AWidth, ntrdComfortable, False, 48) >
      TStringGrid(LApplication.View.ControlFor(ServiceTableID)).RowHeights[0],
      'explicit header reservation follows the actual title font independently of body text');
    Check(LApplication.Collections.Collection(NyxCollection(ServiceCollectionName)) <>
      LPeer.Collections.Collection(NyxCollection(ServiceCollectionName)),
      'two applications own independent runtime collections');
    Check(LMount.View.Snapshot.Count = 4, 'all four sample records are reachable');
    LGateway := NyxItem(NyxCollection(ServiceCollectionName), 'gateway');
    LEdited := 'Gateway / 🌙';
    Check(LMount.EditCell(LGateway, 0, LEdited),
      'the logical-start pinned service field accepts a mounted edit');
    Check(LApplication.Collections.Collection(NyxCollection(ServiceCollectionName))
      .Snapshot.Item(LGateway).GetValue(NyxTextField('name')) = LEdited,
      'the exact Unicode edit reaches its original row and field');
    Check(LMount.EditCell(LGateway, 4, 'Paused'),
      'the logical-end pinned status field accepts a mounted edit');
    Check(LApplication.Collections.Collection(NyxCollection(ServiceCollectionName))
      .Snapshot.Item(LGateway).GetValue(NyxTextField('status')) = 'Paused',
      'the end-pinned edit reaches its original status field');
    Check(not LMount.EditCell(LGateway, 1, '9999'),
      'read-only process identifiers refuse edits');
    Check(LApplication.Collections.Collection(NyxCollection(ServiceCollectionName))
      .Snapshot.Item(LGateway).GetValue(NyxIntegerField('pid')) = 2401,
      'refusal preserves the accepted process identifier');
    Check(LPeer.Collections.Collection(NyxCollection(ServiceCollectionName))
      .Snapshot.Item(LGateway).GetValue(NyxTextField('name')) = 'Request gateway',
      'one application cannot edit another application through authored defaults');
    Check(TNyxCodec.Encode(LDocument) = LBefore,
      'mounted edits never rewrite the authored document or column defaults');
    FreeAndNil(LApplication);
    Check(not LMount.Connected, 'application disposal disconnects the held native mount');
    Check(LPeer.Collections.Collection(NyxCollection(ServiceCollectionName))
      .Snapshot.Item(LGateway).GetValue(NyxTextField('status')) = 'Running',
      'retiring one application leaves the other runtime alive and unchanged');
  finally
    LMount := nil;
    LPeer.Free;
    LApplication.Free;
    LDocument.Free;
  end;
end;

begin
  try
    Application.Initialize;
    Journey(StrToInt(ParamStr(1)));
    WriteLn('PASS ', GChecks, ' service monitor mount checks at ', ParamStr(1));
  except
    on LException: Exception do
    begin
      WriteLn('FAIL ', LException.Message);
      ExitCode := 1;
    end;
  end;
end.
