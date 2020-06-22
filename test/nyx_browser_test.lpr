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
  Classes,
  SysUtils,
  browserapp,
  web,
  math,
  js,
  nyx.types,
  nyx.element.button,
  nyx.layout;

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
    procedure HelloWorldClick(const AButton : INyxElementButton;
      const AEvent : TButtonObserveEvent);

    procedure RunningButtonClick(const AButton : INyxElementButton;
      const AEvent : TButtonObserveEvent);

  strict protected
    procedure doRun; override; //calls BuildUI

    (*
      main method which constructs the user interface
    *)
    procedure BuildUI;
  public
    destructor Destroy; override;
  end;

procedure TBrowserTest.HelloWorldClick(const AButton: INyxElementButton;
  const AEvent: TButtonObserveEvent);
begin
  window.alert('you clicked me!');
end;

procedure TBrowserTest.RunningButtonClick(const AButton: INyxElementButton;
  const AEvent: TButtonObserveEvent);
var
  LLayout: INyxLayoutFixed;
  LBounds: INyxFixedBounds;

const
  BUTTON_TEXT : array[0 .. 4] of String = (
    'oh no! please stop',
    'yikes!',
    'I''m warning you, stop!',
    'That hurt!',
    'be gentle senpai <3'
  );
begin
  //using the UI we fetch the fixed layout (this assumes it's in the first index)
  LLayout := UI.LayoutByIndex(0) as INyxLayoutFixed;

  //we also assume our button was properly added to the layout (which it was)
  //in order to obtain the bounds for the button
  LBounds := LLayout.Bounds[AButton];

  //lastly we'll increment the left position to make the button look
  //like it's running away from the user clicking it
  LBounds.UpdateLeft(LBounds.Left + 50);

  //now for some fun, just update the text of the button
  AButton.UpdateText(BUTTON_TEXT[RandomRange(0, 4)]);

  //lastly we need to render the UI for changes on the layout to take place
  UI.Render;
end;

procedure TBrowserTest.doRun;
begin
  inherited doRun;
  BuildUI;
end;

procedure TBrowserTest.BuildUI;
var
  I , J,
  LHelloIndex,
  LDisabledIndex,
  LRunIndex: Integer;
  LID: String;

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
    .ContainerByIndex(I) //get the container we just added
      .Add( //add a button to it
        NewNyxButton
          .UpdateText('Hello World') //sets our text for the display
          .Observe(boClick, @HelloWorldClick, LID), //attaches a handler to the click event
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
          .Observe(boClick, @RunningButtonClick, LID),
        LRunIndex
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
      .TakeAction(@PositionButton, [LRunIndex, I, J, 100, 250])
    .Render(); //renders all containers and elements to the screen
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
