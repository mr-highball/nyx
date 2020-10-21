program nyx_web_socket;

{$mode delphi}

uses
  JS, Classes, SysUtils, Web, browserapp, nyx.types, nyx.element.inputmulti,
  nyx.element.input, nyx.element.button, nyx.layout, webwidget;

type

  { TApp }
  (*
    this demo shows how to connect and send messages via web sockets
    setup slightly like the demo:
      * https://www.websocket.org/echo.html
  *)
  TApp = class(TBrowserApplication)
  strict private
    FUI : INyxUI;
    FLog : INyxElementInputMulti;
    FURL,
    FMessage: INyxElementInput;
    FSocket : TJSWebSocket;
    FConnect,
    FSend: INyxElementButton;

    procedure ConnectClick(const AElement : INyxElement; const AEvent : TElementEvent);
    procedure SendClick(const AElement : INyxElement; const AEvent : TElementEvent);
  strict protected
    procedure InitUI; virtual;
    procedure DoRun; override;
  public
    procedure Connect(const AURL : String);
    procedure RenderOnSize;
    destructor Destroy; override;
  end;

{ TApp }

procedure TApp.ConnectClick(const AElement: INyxElement;
  const AEvent: TElementEvent);
begin
  Connect(FURL.Text);
end;

procedure TApp.SendClick(const AElement: INyxElement;
  const AEvent: TElementEvent);
begin
  if not Assigned(FSocket) then
    COnnect(FURL.Text);

  FSocket.send(FMessage.Text);
end;

procedure TApp.InitUI;
var
  I: Integer;
  LLayout: INyxLayoutRelational;
  LLayoutProp: INyxLayoutProportional;
  LLogCntr, LUpperCntr: INyxContainer;
  LID: String;
begin
  FUI := NewNyxUI;
  LLayoutProp := NewNyxLayoutProportional;
  LLayout := NewNyxLayoutRelational;

  //create the connection controls
  FURL := NewNyxInput;
  FURL
    .UpdateTextPrompt('wss://yoururl...')
    .UpdateText('wss://echo.websocket.org');

  FConnect := NewNyxButton;
  FConnect
    .UpdateText('Connect')
    .Observe(evClick, @ConnectClick, LID);

  FMessage := NewNyxInput;
  FMessage
    .UpdateTextPrompt('your message...');

  FSend := NewNyxButton;
  FSend
    .UpdateText('Send')
    .Observe(evClick, @SendClick, LID);

  //create the log
  FLog := NewNyxInputMulti
    .Size
      .UpdateHeight(1)
      .UpdateWidth(1)
      .UpdateMode(smPercent)
    .Element as INyxElementInputMulti;

  //layout elements
  LLayoutProp.Add(
    FURL,
    NewNyxProportionalBounds
      .UpdateLeft(0.5)
      .UpdateTop(0.05)
      .UpdateHorzAlignment(haCenter) as INyxProportionalBounds
  );

  LLayout
    .Add(FMessage, FURL, NewNyxRelationalBounds.UpdateTop(10).UpdateHorzAlignment(haLeft).UpdateVertAlignment(vaBottom) as INyxRelationalBounds)
    .Add(FConnect, FURL, NewNyxRelationalBounds.UpdateHorzAlignment(haRight).UpdateVertAlignment(vaTop) as INyxRelationalBounds)
    .Add(FSend, FMessage, NewNyxRelationalBounds.UpdateHorzAlignment(haRight).UpdateVertAlignment(vaTop) as INyxRelationalBounds);

  LUpperCntr := NewNyxContainer;
  LUpperCntr
    .Size
      .UpdateHeight(0.15)
      .UpdateWidth(1)
      .UpdateMode(smPercent);

  LLogCntr := NewNyxContainer;
  LLogCntr
    .Size
      .UpdateHeight(0.5)
      .UpdateWidth(1)
      .UpdateMode(smPercent);

  //add everything to the ui and render
  FUI
    .AddLayout(LLayoutProp, I)
    .AddLayout(LLayout, I)
    .AddContainer(LUpperCntr, I)
    .ContainerByIndex(I)
      .Add(FURL)
      .Add(FConnect)
      .Add(FMessage)
      .Add(FSend)
    .UI
    .AddContainer(LLogCntr, I)
    .ContainerByIndex(I)
      .Add(FLog)
    .UI
      .Render()
end;

procedure TApp.DoRun;
begin
  inherited DoRun;
  InitUI;
end;

procedure TApp.Connect(const AURL: String);

  function LogOpen(Event: TEventListenerEvent): boolean;
  begin
    FLog.Lines.Add('open');
  end;

  function LogClose(Event: TEventListenerEvent): boolean;
  begin
    FLog.Lines.Add('close');
  end;

  function LogMessage(Event: TEventListenerEvent): boolean;
  begin
    FLog.Lines.Add('message: [' + String(TJSMessageEvent(Event).Data) + ']');
  end;

  function LogError(Event: TEventListenerEvent): boolean;
  begin
    FLog.Lines.Add('error');
  end;

begin
  if Assigned(FSocket) then
    FSocket.close;

  FSocket := TJSWebSocket.new(AURL);
  FSocket.onopen := @LogOpen;
  FSocket.onclose:= @LogClose;
  FSocket.onmessage:= @LogMessage;
  FSocket.onerror := @LogError;
end;

procedure TApp.RenderOnSize;
begin
  FUI.Render();
end;

destructor TApp.Destroy;
begin
  inherited Destroy;
  FUI := nil;
  FSocket.close;
end;

var
  App : TApp;
begin
  App := TApp.Create;

  //this will likely be unnecessary in the futre, but for now
  //we add a window listener to re-render the ui if the page resizes
  window.addEventListener(sEventResize, @App.RenderOnSize);
  window.addEventListener(sEventScroll, @App.RenderOnSize);

  App.Initialize;
  App.Run;
end.
