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
unit nyx.event.emitter;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.types,
  nyx.data,
  nyx.scheduler;

type
  { A custom control retains this port, never a renderer/node reference.
    Ports are dormant during offscreen factory construction, active only after
    the whole view is admitted, and disconnected before any widget is freed.
    Retained ports fail explicitly after unmount/navigation/destruction.
    Emit is UI-thread work; scheduled workers submit owned work with PostUI. }
  INyxEventEmitter = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001009000001}']
    function Emit(const AName: TNyxEventRef): Boolean; overload;
    function Emit(const AName: TNyxEventRef;
      const APayload: TNyxDataValue): Boolean; overload;
    function GetConnected: Boolean;
    property Connected: Boolean read GetConnected;
  end;

  { Adapter-owned lifetime boundary. The callback is borrowed while connected;
    it is cleared before renderer/control disposal. Scope retains no renderer,
    node, widget or emitter, so creator-owned ports cannot form a model cycle.
    Activate is used once, after candidate ownership transfers to its final
    renderer. Failed candidates are never activated. }
  TNyxNamedEventSink = function(const AOriginID: TNyxText;
    const AName: TNyxEventRef; const APayload: TNyxDataValue;
    AHasPayload: Boolean): Boolean of object;
  INyxEventEmitterScope = interface(IInterface)
    ['{739BC309-7893-48E3-9600-001009000002}']
    function ForControl(const AOriginID: TNyxText): INyxEventEmitter;
    procedure Activate(ASink: TNyxNamedEventSink);
    procedure Disconnect;
    function GetConnected: Boolean;
    function Send(const AOriginID: TNyxText; const AName: TNyxEventRef;
      const APayload: TNyxDataValue; AHasPayload: Boolean): Boolean;
    property Connected: Boolean read GetConnected;
  end;

function NewNyxEventEmitterScope(const AScheduler: INyxScheduler): INyxEventEmitterScope;

implementation

type
  TNyxEventEmitterScope = class(TInterfacedObject, INyxEventEmitterScope)
  private
    FScheduler: INyxScheduler;
    FSink: TNyxNamedEventSink;
    FDisconnected: Boolean;
  public
    constructor Create(const AScheduler: INyxScheduler);
    function ForControl(const AOriginID: TNyxText): INyxEventEmitter;
    procedure Activate(ASink: TNyxNamedEventSink);
    procedure Disconnect;
    function GetConnected: Boolean;
    function Send(const AOriginID: TNyxText; const AName: TNyxEventRef;
      const APayload: TNyxDataValue; AHasPayload: Boolean): Boolean;
  end;

  TNyxEventEmitter = class(TInterfacedObject, INyxEventEmitter)
  private
    FScope: INyxEventEmitterScope;
    FOriginID: TNyxText;
  public
    constructor Create(const AScope: INyxEventEmitterScope; const AOriginID: TNyxText);
    function Emit(const AName: TNyxEventRef): Boolean; overload;
    function Emit(const AName: TNyxEventRef;
      const APayload: TNyxDataValue): Boolean; overload;
    function GetConnected: Boolean;
  end;

function NewNyxEventEmitterScope(const AScheduler: INyxScheduler): INyxEventEmitterScope;
begin
  Result := TNyxEventEmitterScope.Create(AScheduler);
end;

constructor TNyxEventEmitterScope.Create(const AScheduler: INyxScheduler);
begin
  inherited Create;

  if AScheduler = nil then
  begin
    raise ENyxSchedule.Create('A named producer requires its UI scheduler');
  end;
  FScheduler := AScheduler;
  FScheduler.RequireUI;
end;

function TNyxEventEmitterScope.ForControl(const AOriginID: TNyxText): INyxEventEmitter;
begin
  FScheduler.RequireUI;

  if FDisconnected or (AOriginID = '') then
  begin
    raise ENyxSchedule.Create('A producer port requires a live control identity');
  end;
  Result := TNyxEventEmitter.Create(Self as INyxEventEmitterScope, AOriginID);
end;

procedure TNyxEventEmitterScope.Activate(ASink: TNyxNamedEventSink);
begin
  FScheduler.Admit(neSequential);

  if FDisconnected or Assigned(FSink) or not Assigned(ASink) then
  begin
    raise ENyxSchedule.Create('A producer scope can be activated once');
  end;
  FSink := ASink;
end;

procedure TNyxEventEmitterScope.Disconnect;
begin
  FScheduler.RequireUI;
  FDisconnected := True;
  FSink := nil;
end;

function TNyxEventEmitterScope.GetConnected: Boolean;
begin
  FScheduler.RequireUI;
  Result := not FDisconnected and Assigned(FSink);
end;

function TNyxEventEmitterScope.Send(const AOriginID: TNyxText;
  const AName: TNyxEventRef; const APayload: TNyxDataValue;
  AHasPayload: Boolean): Boolean;
var
  LKeepAlive: INyxEventEmitterScope;
begin
  FScheduler.Admit(neSequential);

  if not GetConnected then
  begin
    raise ENyxSchedule.Create('Named producer is not connected to an admitted view');
  end;
  { Callback navigation may release the renderer's last scope reference. The
    synchronous call owns the scope until its borrowed sink has returned. }
  LKeepAlive := Self;
  Result := FSink(AOriginID, AName, APayload, AHasPayload);
end;

constructor TNyxEventEmitter.Create(const AScope: INyxEventEmitterScope;
  const AOriginID: TNyxText);
begin
  inherited Create;
  FScope := AScope;
  FOriginID := AOriginID;
end;

function TNyxEventEmitter.GetConnected: Boolean;
begin
  Result := FScope.Connected;
end;

function TNyxEventEmitter.Emit(const AName: TNyxEventRef): Boolean;
begin
  Result := FScope.Send(FOriginID, AName, NyxNull, False);
end;

function TNyxEventEmitter.Emit(const AName: TNyxEventRef;
  const APayload: TNyxDataValue): Boolean;
begin
  Result := FScope.Send(FOriginID, AName, APayload, True);
end;

end.
