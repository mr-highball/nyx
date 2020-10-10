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

    procedure GetAllTextContent(const AType : TPropertyUpdateType;
      const AInput : INyxElementInput; const AProperty : TInputProperty);
  strict protected
    (*
      dynamically builds a list of input elements with
      labels for each string in the @AInputs parameter
    *)
    procedure BuildUI(const AInputs : TStrings);

    (*
      this method will take TStrings (TStringList or other) and fill
      it up with "question" and "answer". The "question" will be stored
      in the "name" and the "answer" will be stored as a value
    *)
    procedure AllInputTextToStrings(const AContent : TStrings);

    (*
      this method will return a single string with the question following
      by an answer. an optional question separator string can be used
      but will default to separate with a hyphen
    *)
    function AllInputTextToString(const AQuestionSeparator  : String = '-') : String;

    (*
      returns all of the inputs content without including the question as
      a single string
    *)
    function AllInputContentOnlyToString : String;
  public
    procedure doRun; override;

    (*
      calls UI.Render() when the window is resized
    *)
    procedure RenderOnSize(Event : TEventListenerEvent);
  end;

procedure TInputLabels.GetAllTextContent(const AType: TPropertyUpdateType;
  const AInput: INyxElementInput; const AProperty: TInputProperty);
begin
  //when any input has it's text changed (after update) then just display
  //a message box to the user with all of the text content
  if AType = puAfterUpdate then
    window.alert(AllInputTextToString());
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
      .UpdateTop(-40) //give the label some room above the input
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

  (*
    adds the GetAllTextContent observer method to each input so that
    whenever the text changes, we'll popup a message box
  *)
  procedure AddTextChangeObserverToInputs(const AElement : INyxElement);
  var
    LID: String;
    LInput: INyxElementInput;
  begin
    if AElement is INyxElementInput then
    begin
      LInput := AElement as INyxElementInput;
      LInput.Observe(ipText, @GetAllTextContent, LID);
    end;
  end;

begin
  FUI := NewNyxUI;
  LLayout := NewNyxLayoutRelational;
  LPropLayout := NewNyxLayoutProportional;

  LContainer := NewNyxContainer;
  LContainer
    .UpdateSize( //make the container take up 100% of the screen
      NewNyxSize
        .UpdateHeight(1)
        .UpdateWidth(1)
        .UpdateMode(smPercent) //use percent mode
    );

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
      .Elements
        .ForEach(@PositionInputsByIndex)
        .ForEach(@AddTextChangeObserverToInputs);

  //now that we've added all the inputs render to the screen so they have
  //positions, then we need to add the labels and render again (we do this
  //because of the dynamic nature of adding labels, normally only one render would be needed)
  FUI.Render();
  LContainer.Elements.ForEach(@AddLabelToElements);
  FUI.Render();
end;

procedure TInputLabels.AllInputTextToStrings(const AContent: TStrings);

  //fills the content as "name" : :"value" pair
  procedure FillContent(const AElement : INyxElement);
  var
    LInput: INyxElementInput;
  begin
    //we want to skip all of the labels
    if AElement is INyxElementStaticText then
      Exit;

    LInput := AElement as INyxElementInput;

    //the element name is the "question" and the element text is the "answer"
    AContent.AddPair(AElement.Name, LInput.Text);
  end;

begin
  if not Assigned(AContent) then
    raise Exception.Create('AllInputTextToStrings::AContent must not be nil');

  //use a "foreach" to fill the content
  FUI.ContainerByIndex(0).Elements.ForEach(@FillContent);
end;

function TInputLabels.AllInputTextToString(const AQuestionSeparator: String): String;
var
  I: Integer;
  LContent: TStringList;
begin
  Result := '';

  LContent := TStringList.Create;
  try
    //use our other method to fill up the content string list
    AllInputTextToStrings(LContent);

    //now loop through the content we just collected and separate the question/answer
    for I := 0 to Pred(LContent.Count) do
      Result := Result + LContent.Names[I] + AQuestionSeparator + LContent.ValueFromIndex[I] + sLineBreak;
  finally
    LContent.Free;
  end;
end;

function TInputLabels.AllInputContentOnlyToString: String;
var
  I: Integer;
  LContent: TStringList;
begin
  Result := '';

  LContent := TStringList.Create;
  try
    //use our other method to fill up the content string list
    AllInputTextToStrings(LContent);

    //now loop through the content we just collected append all the values
    for I := 0 to Pred(LContent.Count) do
      Result := Result + LContent.ValueFromIndex[I] + sLineBreak;
  finally
    LContent.Free;
  end;
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
