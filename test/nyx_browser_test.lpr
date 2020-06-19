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
  js,
  nyx.types,
  nyx.element.button, nyx.utils.browser.css;

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

procedure TBrowserTest.doRun;
begin
  inherited doRun;
  BuildUI;
end;

procedure TBrowserTest.BuildUI;
var
  I : Integer;
  LID: String;
begin
  //init the UI
  UI := NewNyxUI;

  //setup the ui with the demo ui components
  UI
    .AddContainer(NewNyxContainer, I) //add a container (holds elements)
    .ContainerByIndex(I) //get the container we just added
      .Add( //add a button to it
        NewNyxButton
          .UpdateText('Hello World') //sets our text for the display
          .Observe(boClick, @HelloWorldClick, LID), //attaches a handler to the click event
        I //optional recording of the index the button was added to in the container
      )
      .Add( //adds a new button but in this case, we will disable it
        NewNyxButton
          .UpdateText('a disabled button')
          .UpdateEnabled(False)
      )
    .UI //scope to the UI property in order to call render
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
  Application.Free;
end.
