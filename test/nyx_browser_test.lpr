{ nyx

  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
program nyx_browser_test;

{$mode delphi}

uses
  Classes, SysUtils, browserapp, web, math, js, nyx.types, nyx.element.button,
  nyx.layout, nyx.layout.relational.browser, nyx.size, nyx.element,
  nyx.container, nyx.container.browser, nyx.element.input,
  nyx.element.input.browser, nyx.element.checkbox, nyx.element.checkbox.browser,
  nyx.element.inputmulti, nyx.element.inputmulti.browser, nyx.element.statictext;

type

  { TBrowserTest }
  (*
    test application which demonstrates how to build a user interface for
    the browser using the nyx framework and the pas2js compiler
  *)
  TBrowserTest = class(TBrowserApplication)
  strict private
    UI : INyxUI;

    (*
      handler for telling the world hello
    *)
    procedure HelloWorldClick(const AElement : INyxElement;
      const AEvent : TElementEvent);

    procedure RunningButtonClick(const AElement : INyxElement;
      const AEvent : TElementEvent);

    procedure EnterExitBigButton(const AElement : INyxElement;
      const AEvent : TElementEvent);

    procedure HideBigButton(const AElement : INyxElement;
      const AEvent : TElementEvent);

    procedure CheckChanged(const AType : TPropertyUpdateType;
      const ACheckbox : INyxElementCheckbox; const AProperty : TCheckboxProperty);

    procedure LinesChanged(const AType : TPropertyUpdateType;
      const AInput : INyxElementInputMulti; const AProperty : TInputMultiProperty);
  strict protected
    procedure doRun; override;

    (*
      main method which constructs the user interface
    *)
    procedure BuildUI;
    procedure TestContainerIsBrowser;
    procedure TestAddClickHandlerToButton;
    procedure TestSimpleInput;
    procedure TestSimpleCheckbox;
    procedure TestSimpleInputMulti;
    procedure TestSimpleStaticText;
  public
    destructor Destroy; override;
  end;

procedure TBrowserTest.HelloWorldClick(const AElement: INyxElement;
  const AEvent: TElementEvent);
begin
  window.alert('you clicked me!');
end;

procedure TBrowserTest.RunningButtonClick(const AElement: INyxElement;
  const AEvent: TElementEvent);
var
  LLayout: INyxLayoutFixed;
  LBounds: INyxFixedBounds;
  LButton: INyxElementButton;

const
  BUTTON_TEXT : array[0 .. 4] of String = (
    'oh no! please stop',
    'yikes!',
    'I''m warning you, stop!',
    'That hurt!',
    'be gentle senpai <3'
  );
begin
  LButton := AElement as INyxElementButton;

  //using the UI we fetch the fixed layout (this assumes it's in the first index)
  LLayout := UI.LayoutByIndex(0) as INyxLayoutFixed;

  //we also assume our button was properly added to the layout (which it was)
  //in order to obtain the bounds for the button
  LBounds := LLayout.Bounds[LButton];

  //lastly we'll increment the left position to make the button look
  //like it's running away from the user clicking it
  LBounds.UpdateLeft(LBounds.Left + 50);

  //now for some fun, just update the text of the button
  LButton.UpdateText(BUTTON_TEXT[RandomRange(0, 4)]);

  //lastly we need to render the UI for changes on the layout to take place
  UI.Render;
end;

procedure TBrowserTest.EnterExitBigButton(const AElement: INyxElement;
  const AEvent: TElementEvent);
begin
  if AEvent = evMouseEnter then
    window.alert('entered big button')
  else
    window.alert('exited big button');
end;

procedure TBrowserTest.HideBigButton(const AElement: INyxElement;
  const AEvent: TElementEvent);
begin
  (AElement as INyxElementButton).Visible := False;
end;

procedure TBrowserTest.CheckChanged(const AType: TPropertyUpdateType;
  const ACheckbox: INyxElementCheckbox; const AProperty: TCheckboxProperty);
var
  LVal: Boolean;
begin
  if AType = puAfterUpdate then
  begin
    LVal := ACheckbox.Checked;
    window.alert('checkbox has changed to ' + BoolToStr(LVal, True));
  end;
end;

procedure TBrowserTest.LinesChanged(const AType: TPropertyUpdateType;
  const AInput: INyxElementInputMulti; const AProperty: TInputMultiProperty);
begin
  WriteLn(AType, AInput.Text)
end;

procedure TBrowserTest.doRun;
begin
  inherited doRun;
  //BuildUI;
  //TestContainerIsBrowser;
  //TestAddClickHandlerToButton;
  //TestSimpleInput;
  //TestSimpleCheckbox;
  //TestSimpleInputMulti;
  TestSimpleStaticText;
end;

procedure TBrowserTest.BuildUI;
var
  I , J, K, L,
  LHelloIndex,
  LDisabledIndex,
  LRunIndex,
  LCenterIndex,
  LFollowIndex,
  LBigIndex: Integer;
  LID: String;
  LElement , LAnchor: INyxElement;
  LLayout: INyxLayoutProportional;
  LBounds: INyxProportionalBounds;
  LRelLayout : INyxLayoutRelational;
  LRelBound : INyxRelationalBounds;

  (*
    this is a helper method which positions buttons in a vertical stack
    using the 4th param as a "multiplier"

    note:
      the only purpose of this method is to show that things like this
      can be done, not that it's the best way to do it
  *)
  procedure PositionButton(const AUI : INyxUI; const AArgs : array of const);
  var
    LFixed: INyxLayoutFixed;
  begin
    //we stored the index to the layout in the args, so get it
    LFixed := UI.LayoutByIndex(AArgs[2].VInteger) as INyxLayoutFixed;

    //add the element to be positioned
    LFixed.Add(
      UI.ContainerByIndex(AArgs[1].VInteger).Elements[AArgs[0].VInteger],
      NewNyxFixedBounds
        .UpdateLeft(AArgs[3].VInteger) //use the "left" value from AArgs
        .UpdateTop(AArgs[4].VInteger) //use the "top" value from AArgs
    );
  end;

begin
  //init the UI
  UI := NewNyxUI;

  //setup the ui with the demo ui components
  UI
    .AddContainer(NewNyxContainer, I) //add a container (holds elements)
    .AddLayout(NewNyxLayoutFixed, J) //add a fixed layout (positions elements)
    .AddLayout(NewNyxLayoutProportional, K) //adds proportional layout
    .AddLayout(NewNyxLayoutRelational, L) //adds relational layout
    .ContainerByIndex(I) //get the container we just added
      .Add( //add a button to it
        NewNyxButton
          .UpdateText('Hello World') //sets our text for the display
          .Observe(evClick, @HelloWorldClick, LID), //attaches a handler to the click event
        LHelloIndex //optional recording of the index the button was added to in the container
      )
      .Add( //adds a new button but in this case, we will disable it
        NewNyxButton
          .UpdateText('a disabled button')
          .UpdateEnabled(False),
        LDisabledIndex
      )
      .Add( //adds a "running" button demonstrating dynamic positioning
        NewNyxButton
          .UpdateText('click and I run!')
          .Observe(evClick, @RunningButtonClick, LID),
        LRunIndex
      )
      .Add( //adds a button that is center to the container using proportional layout
        NewNyxButton
          .UpdateText('centered'),
        LCenterIndex
      )
      .Add(
        NewNyxButton
          .UpdateText('following button'),
        LFollowIndex
      )
      .Add(
        NewNyxButton
          .UpdateText('big button')
          .Observe(evMouseEnter, @EnterExitBigButton, LID)
          .Observe(evMouseExit, @EnterExitBigButton, LID)
          .Observe(evClick, @HideBigButton, LID)
          .UpdateName('big button'),
        LBigIndex
      )
    .UI //scope to the UI property
      (*
        take action can be used to pass a variable number and type of arguments
        that can be used by the input "action". below we define a format
        where the arguments are:
          0 - element index
          1 - container index
          2 - layout index
          3 - left distance of element
          4 - top distance of element
      *)
      .TakeAction(@PositionButton, [LHelloIndex, I, J, 100, 100])
      .TakeAction(@PositionButton, [LDisabledIndex, I, J, 100, 175])
      .TakeAction(@PositionButton, [LRunIndex, I, J, 100, 250]);

  //make the container 100% of screen
  UI
    .ContainerByIndex(I)
      .Size
        .UpdateHeight(1.0)
        .UpdateWidth(1.0)
        .UpdateMode(smPercent)
      .Element
        .UpdateName('container');

  //we could've done this via take action, but showing another way to
  //get and cast the layout added, and add the centered button with a proportional layout
  LElement := UI.ContainerByIndex(I).Elements[LCenterIndex];
  LLayout := UI.LayoutByIndex(K) as INyxLayoutProportional;
  LBounds := NewNyxProportionalBounds;
  LBounds
    .UpdateTop(0.5)
    .UpdateLeft(0.5);

  LLayout.Add(
    LElement,
    LBounds
  );

  //now show how to use a relational layout & bounds to "follow" the running button
  LElement := UI.ContainerByIndex(I).Elements[LFollowIndex];
  LAnchor := UI.ContainerByIndex(I).Elements[LRunIndex];
  LRelLayout := UI.LayoutByIndex(L) as INyxLayoutRelational;
  LRelBound := NewNyxRelationalBounds;

  LRelBound
    .UpdateLeft(30) //30px distance
    .UpdateVertAlignment(vaCenter)
    .UpdateHorzAlignment(haRight); //relative to left of the "anchor" button

  LRelLayout.Add(
    LElement,
    LAnchor,
    LRelBound
  );

  //make a big button that is positioned at the far right of the screen
  LElement := UI.ContainerByIndex(I).Elements[LBigIndex];
  LLayout := UI.LayoutByIndex(K) as INyxLayoutProportional;

  LBounds := NewNyxProportionalBounds;
  LBounds
    .UpdateLeft(1)
    .UpdateHorzAlignment(haRight);

  LLayout.Add(
    LElement,
    LBounds
  );

  LElement.Size.UpdateHeight(1);
  LElement.Size.UpdateWidth(0.10);
  LElement.Size.UpdateMode(smPercent);

  //renders all containers and elements to the screen
  UI.Render();
end;

procedure TBrowserTest.TestContainerIsBrowser;
var
  LContainer: INyxContainer;
  LUI: INyxUI;
  I, J: Integer;
  LLayout: INyxLayoutProportional;
begin
  LUI := NewNyxUI;

  //100% sized container
  LContainer := NewNyxContainer;
  LContainer
    .Size
      .UpdateHeight(1)
      .UpdateWidth(1)
      .UpdateMode(smPercent)
    .Element
      .UpdateName('container');

  LLayout := NewNyxLayoutProportional;

  //add the container/layout/button to the ui
  LUI
    .AddContainer(LContainer, I)
    .AddLayout(LLayout, J)
    .ContainerByIndex(I)
      .Add(NewNyxButton);

  //right align the button to the container
  LLayout.Add(
    LUI.ContainerByIndex(I).Elements[0],
    INyxProportionalBounds(NewNyxProportionalBounds
      .UpdateLeft(1)
      .UpdateHorzAlignment(haRight))
  );

  LUI.Render();
end;

procedure TBrowserTest.TestAddClickHandlerToButton;
var
  LUI: INyxUI;
  I: Integer;
  LID: String;
begin
  LUI := NewNyxUI;
  LUI
    .AddContainer(NewNyxContainer, I)
    .ContainerByIndex(I)
      .Add(
        NewNyxButton
          .UpdateText('Hello World')
          .Observe(evClick, @HelloWorldClick, LID)
      )
    .UI
      .Render();
end;

procedure TBrowserTest.TestSimpleInput;
var
  LUI: INyxUI;
  I: Integer;
  LID: String;
begin
  LUI := NewNyxUI;
  LUI
    .AddContainer(NewNyxContainer, I)
    .ContainerByIndex(I)
      .Add(
        NewNyxInput
          .UpdateText('Hello World')
          .UpdateTextPrompt('type something...')
      )
    .UI
      .Render();
end;

procedure TBrowserTest.TestSimpleCheckbox;
var
  LUI: INyxUI;
  I: Integer;
  LID: String;
begin
  LUI := NewNyxUI;
  LUI
    .AddContainer(NewNyxContainer, I)
    .ContainerByIndex(I)
      .Add(
        NewNyxCheckbox
          .UpdateText('Hello World')
          .Observe(cpChecked, @CheckChanged, LID)
      )
    .UI
      .Render();
end;

procedure TBrowserTest.TestSimpleInputMulti;
var
  LUI: INyxUI;
  I: Integer;
  LID: String;
begin
  LUI := NewNyxUI;
  LUI
    .AddContainer(NewNyxContainer, I)
    .ContainerByIndex(I)
      .Add(
        NewNyxInputMulti
          .UpdateText('Hello World')
          .Observe(imLines, @LinesChanged, LID)
          .Size
            .UpdateWidth(1)
            .UpdateHeight(1)
            .UpdateMode(smPercent)
          .Element
      )
    .UI
      .Render();
end;

procedure TBrowserTest.TestSimpleStaticText;
var
  LUI: INyxUI;
  I: Integer;
  LID: String;
begin
  LUI := NewNyxUI;
  LUI
    .AddContainer(NewNyxContainer, I)
    .ContainerByIndex(I)
      .Add(
        NewNyxStaticText.UpdateText('Normal Text')
      )
      .Add(
        NewNyxStaticText.UpdateText('Bold Text').UpdateFormat([sfBold])
      )
      .Add(
        NewNyxStaticText.UpdateText('Strike Text').UpdateFormat([sfStrikeThrough])
      )
      .Add(
        NewNyxStaticText.UpdateText('Underline Text').UpdateFormat([sfUnderline])
      )
      .Add(
        NewNyxStaticText.UpdateText('Italic Text').UpdateFormat([sfItalic])
      )
      .Add(
        NewNyxStaticText.UpdateText('All Format Text').UpdateFormat([sfBold, sfStrikeThrough, sfUnderline, sfItalic])
      )
    .UI
      .Render();
end;

destructor TBrowserTest.Destroy;
begin
  UI := nil;
  inherited Destroy;
end;

var
  Application : TBrowserTest;
begin
  Application:=TBrowserTest.Create(nil);
  Application.Initialize;
  Application.Run;
  //Application.Free;
end.
