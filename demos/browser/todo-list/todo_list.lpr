program todo_list;

{$mode delphi}

uses
  browserapp,
  JS,
  Classes,
  SysUtils,
  Web,
  webwidget,
  nyx.types,
  nyx.layout,
  nyx.element.button,
  nyx.element.checkbox,
  nyx.element.input;

type

  { TTodoList }
  (*
    this app shows how with only a few basic elements, we can build a
    simple todo list
  *)
  TTodoList = class(TBrowserApplication)
    procedure doRun; override;
  strict private
    FUI : INyxUI;
    FCtrlContainer,
    FItemContainer : INyxContainer;
    FPropLayout : INyxLayoutProportional;
    FRelLayout : INyxLayoutRelational;
    FInputItem : INyxElementInput;
    FBtnAdd : INyxElementButton;


    procedure AddClick(const AElement : INyxElement; const AEvent : TElementEvent);
  public
    procedure RenderOnSize(Event : TEventListenerEvent);
    procedure BuildUI;
  end;

procedure TTodoList.doRun;

begin
  BuildUI;
end;

procedure TTodoList.RenderOnSize(Event: TEventListenerEvent);
begin
  FUI.Render();
end;

procedure TTodoList.AddClick(const AElement: INyxElement;
  const AEvent: TElementEvent);
begin
  window.alert('not impl...');
end;

procedure TTodoList.BuildUI;
var
  I, J: Integer;
  LID: String;

  procedure SizeElements;
  begin
    //size our ctrl container to half the width of the screen and most of the height
    FCtrlContainer
      .Size
        .UpdateHeight(0.90) //90%
        .UpdateWidth(0.5) //50%
        .UpdateMode(smPercent);

    //size our item container
    FItemContainer
      .Size
        .UpdateHeight(0.90) //90%
        .UpdateWidth(1) //100%
        .UpdateMode(smPercent);
  end;

  procedure PositionElements;
  begin
    //put the control container in the center top portion of the screen
    FPropLayout.Add(
      FCtrlContainer,
      INyxProportionalBounds(NewNyxProportionalBounds
        .UpdateLeft(0.50) //todo - not working...
        .UpdateTop(0.05)
        .UpdateHorzAlignment(haCenter))
    );

    //put the item container below the control container
    FPropLayout.Add(
      FItemContainer,
      NewNyxProportionalBounds
        .UpdateTop(0.10) //below the ctrl's
    );

    //center the input box
    FPropLayout.Add(
      FInputItem,
      INyxProportionalBounds(NewNyxProportionalBounds
        .UpdateLeft(0.5)
        .UpdateHorzAlignment(haCenter))
    );

    //put the add button to the right of the input
    FRelLayout.Add(
      FBtnAdd, //what we are positioning
      FInputItem, //anchor
      INyxRelationalBounds(NewNyxRelationalBounds
        .UpdateLeft(10)
        .UpdateTop(-20)
        .UpdateHorzAlignment(haRight)//right side of the input
        .UpdateVertAlignment(vaTop)) //top of the input
    );
  end;

begin
  (*
    below we will construct a user interface that allows for user input
    of "todo" items (checkboxes). once the user checks the box, the todo
    item will be removed. we'll use a proportional layout to control
    the position of elements on the screen
  *)

  FUI := NewNyxUI;

  //we could let the ui keep track of these instead of adding them as fields
  //but this should be a little clearer at the expense of a few lines of code
  FCtrlContainer := NewNyxContainer;
  FItemContainer := NewNyxContainer;
  FPropLayout := NewNyxLayoutProportional;
  FRelLayout := NewNyxLayoutRelational;
  FInputItem := NewNyxInput;
  FBtnAdd := NewNyxButton;

  //wraps up all the sizing code
  SizeElements;

  //wraps up all the positioning code
  PositionElements;

  //add all the elements to the UI
  FUI
    .AddContainer(FCtrlContainer, I) //record container index
    .AddLayout(FPropLayout, J)
    .AddLayout(FRelLayout, J)
    .ContainerByIndex(I)
      .Add(FItemContainer) //item container is "inside" of the ctrl container
      .Add(
        FInputItem
          .UpdateTextPrompt('todo entry...')
          .UpdateName('input_todo')
      )
      .Add(
        FBtnAdd
          .UpdateText('Add')
          .UpdateName('btn_add')
          .Observe(evClick, @AddClick, LID)
    )
    .UI
      .Render();
end;

var
  Application : TTodoList;

begin
  Application:=TTodoList.Create(nil);

  //this will likely be unnecessary in the futre, but for now
  //we add a window listener to re-render the ui if the page resizes
  window.addEventListener(sEventResize, @Application.RenderOnSize);

  Application.Initialize;
  Application.Run;
  //Application.Free;
end.
