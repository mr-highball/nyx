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
program process_tree;

{$mode delphi}{$H+}{$codepage utf8}

uses
  {$ifdef PAS2JS}
  Web, nyx.application.browser,
  {$else}
  Interfaces, Forms, nyx.application.lcl,
  {$endif}
  nyx.model, nyx.process.tree;

var
  GDocument: TNyxDocument;
  GBinding: TNyxProcessTreeDemo;
  {$ifdef PAS2JS}
  GApplication: TNyxBrowserApplication;
  {$else}
  GApplication: TNyxLCLApplication;
  {$endif}

begin
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GDocument := BuildProcessTreeDocument;
  try
    {$ifdef PAS2JS}
    GApplication := TNyxBrowserApplication.Create;
    GApplication.Run(GDocument, TJSHTMLElement(document.body));
    {$else}
    GApplication := TNyxLCLApplication.Create;
    try
      GApplication.Mount(GDocument);
    {$endif}
    GBinding := TNyxProcessTreeDemo.Create(GApplication.Context, GApplication.View.Events,
      GApplication.View.CollectionMount(ProcessTreeID).View);
    {$ifdef PAS2JS}
    document.body.setAttribute('data-nyx-process-tree-ready', 'passed');
    { A browser page owns this example for its lifetime. Embedded hosts close
      the binding before replacing the application or its borrowed document. }
    {$else}
      GApplication.Window.Show;
      Application.Run;
    finally
      GBinding.Free;
      GApplication.Free;
    end;
    {$endif}
  except
    {$ifdef PAS2JS}
    GBinding.Free;
    GApplication.Free;
    {$endif}
    GDocument.Free;
    raise;
  end;
  {$ifndef PAS2JS}GDocument.Free;{$endif}
end.
