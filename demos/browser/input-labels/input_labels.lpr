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
  nyx.element.browser,
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
    (*
      dynamically builds a list of input elements with
      labels for each string in the @AInputs parameter
    *)
    procedure BuildUI(const AInputs : TStrings);
  public
    procedure doRun; override;

    (*
      calls UI.Render() when the window is resized
    *)
    procedure RenderOnSize(Event : TEventListenerEvent);
  end;

procedure TInputLabels.BuildUI(const AInputs: TStrings);
var
  I: Integer;
  LLayout: INyxLayoutRelational;
  LPropLayout : INyxLayoutProportional;
  LContainer: INyxContainer;

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
    LBounds
      .UpdateTop(-40)
      .UpdateVertAlignment(vaTop); //anchor to top

    //add the text to be managed by the relational layout
    LLayout.Add(LText, AElement, LBounds);
  end;

  (*
    this method will setup proportional positions for all of
    the input elements based on the index using the following formula:
      Position = (Index + 1) * 0.10 (for the top)
  *)
  procedure PositionInputsByIndex(const AElement : INyxElement);
  var
    J: Integer;
  begin
    FUI.ContainerByIndex(I).Elements.IndexOf(AElement, J);
    LPropLayout.Add(
      AElement,
      INyxProportionalBounds(NewNyxProportionalBounds
        .UpdateTop(Succ(J) * 0.10)
        .UpdateLeft(0.5) //position input to the center of the container
        .UpdateHorzAlignment(haCenter))
    );
  end;

begin
  FUI := NewNyxUI;
  LLayout := NewNyxLayoutRelational;
  LPropLayout := NewNyxLayoutProportional;
  LContainer := NewNyxContainer;

  //add inputs based on the strings and update the "name" property.
  //we'll use this in the AddLabelToElements method as the label text
  for I := 0 to Pred(AInputs.Count) do
    LContainer.Add(NewNyxInput.UpdateName(AInputs[I]));

  //add labels and size the container
  FUI
    .AddLayout(LLayout, I) //this layout controls the label positioning
    .AddLayout(LPropLayout, I) //this layout controls the input positioning
    .AddContainer(LContainer, I)
    .ContainerByIndex(I)
      .UpdateSize( //make the container take up 100% of the screen
        NewNyxSize
          .UpdateHeight(1)
          .UpdateWidth(1)
          .UpdateMode(smPercent) //use percent mode
      )
    .Container
      .Elements
        .ForEach(@PositionInputsByIndex);


  //now that we've added all the inputs render to the screen so they have
  //positions, then we need to add the labels and render again (we do this
  //because of the dynamic nature of adding labels, normally only one render would be needed)
  FUI.Render();
  LContainer.Elements.ForEach(@AddLabelToElements);
  FUI.Render();
end;

procedure TInputLabels.doRun;
var
  LInputs : TStringList;
begin
  //here we create a stringlist to hold the input "names". these could be
  //pulled from a db, web request, or any other dynamic source, but to test
  //we'll just add them manually
  LInputs := TStringList.Create;
  LInputs.Add('First Name');
  LInputs.Add('Middle Name');
  LInputs.Add('Last Name');
  LInputs.Add('Did this question show correctly?');
  BuildUI(LInputs);
  LInputs.Free;
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
