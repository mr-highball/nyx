program input_labels;

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
  nyx.element.input,
  nyx.element.statictext;

type

  { TInputLabels }
  (*
    a sample application showing how to add labels to every
    input element. these will follow the input even if position changes
  *)
  TInputLabels = class(TBrowserApplication)
  strict private
    FUI : INyxUI;
  strict protected
    procedure BuildUI;
  public
    procedure doRun; override;

    (*
      calls UI.Render() when the window is resized
    *)
    procedure RenderOnSize(Event : TEventListenerEvent);
  end;

procedure TInputLabels.BuildUI;
var
  I: Integer;
  LLayout: INyxLayoutRelational;
  LPropLayout : INyxLayoutProportional;

  (*
    method to parent a label to an element, used in a "foreach" call
    on the UI's elements collection
  *)
  procedure AddLabelToElements(const AElement : INyxElement);
  var
    LText: INyxElementStaticText;
    LBounds: INyxRelationalBounds;
  begin
    //create and add the text to the UI
    LText := NewNyxStaticText;
    FUI.ContainerByIndex(I).Add(LText);

    //set the text to the element's name
    LText
      .UpdateText(AElement.Name)
      .UpdateFormat([sfBold]); //we'll bold for emphasis

    //setup a bounds that anchors this text element to the input element
    LBounds := NewNyxRelationalBounds;
    LBounds.UpdateVertAlignment(vaTop); //anchor to top

    //add the text to be managed by the relational layout
    LLayout.Add(LText, AElement, LBounds);
  end;

  (*
    this method will setup proportional positions for all of
    the input elements based on the index using the following formula:
      Position = Index * 0.10 (for the top)
  *)
  procedure PositionInputsByIndex(const AElement : INyxElement);
  begin
    LPropLayout.Add(
      AElement,
      NewNyxProportionalBounds
        .UpdateTop(FUI.ContainerByIndex(I).Elements.IndexOf(AElement) * 0.10)
        .UpdateLeft(0.5) //position input to the center of the container
    );
  end;

begin
  FUI := NewNyxUI;
  LLayout := NewNyxLayoutRelational;
  LPropLayout := NewNyxLayoutProportional;

  //add a few inputs and update the "name" property. we'll use this in the
  //AddLabelToElements method as the label text
  FUI
    .AddLayout(LLayout, I) //this layout controls the label positioning
    .AddLayout(LPropLayout) //this layout controls the input positioning
    .AddContainer(NewNyxContainer, I)
    .ContainerByIndex(I)
      .Add(NewNyxInput.UpdateName('First Name'))
      .Add(NewNyxInput.UpdateName('Last Name'))
      .Add(NewNyxInput.UpdateName('Email'))
      .UpdateSize( //make the container take up 100% of the screen
        NewNyxSize
          .UpdateHeight(1)
          .UpdateWidth(1)
          .UpdateMode(smPercent)
      )
    .Elements
        .ForEach(@AddLabelToElements)
        .FindAll(IsNyxInput)
          .ForEach(@PositionInputsByIndex);

  //now that we've added all the inputs and attached labels show it in the browser
  FUI.Render();
end;

procedure TInputLabels.doRun;

begin
  BuildUI;
end;

procedure TInputLabels.RenderOnSize(Event: TEventListenerEvent);
begin
  FUI.Render();
end;

var
  Application : TInputLabels;

begin
  Application:=TInputLabels.Create(nil);

  //this will likely be unnecessary in the futre, but for now
  //we add a window listener to re-render the ui if the page resizes
  window.addEventListener(sEventResize, @Application.RenderOnSize);
  window.addEventListener(sEventScroll, @Application.RenderOnSize);

  Application.Initialize;
  Application.Run;

  //comment out free since we utilize the application object in events
  //Application.Free;
end.
