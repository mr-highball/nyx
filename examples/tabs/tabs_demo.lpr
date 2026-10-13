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

program tabs_demo;

{$mode delphi}{$H+}{$codepage utf8}

uses
  nyx.model, nyx.generated.view
  {$ifdef PAS2JS}, Web, nyx.application.browser
  {$else}, Interfaces, Forms, nyx.application.lcl{$endif};

var
  GDocument: TNyxDocument;
  {$ifdef PAS2JS}
  GApplication: TNyxBrowserApplication;
  {$else}
  GApplication: TNyxLCLApplication;
  {$endif}

begin
  { The supplied companion is exported through Studio's semantic API. Retained tabs
    are ordinary library controls; this entry contains no tab implementation. }
  {$ifndef PAS2JS}Application.Initialize;{$endif}
  GDocument := BuildNyxDocument;
  {$ifdef PAS2JS}
  GApplication := TNyxBrowserApplication.Create;
  try
    GApplication.Run(GDocument, TJSHTMLElement(document.body));
  except
    GApplication.Free;
    GDocument.Free;
    raise;
  end;
  document.body.setAttribute('data-nyx-tabs-demo-ready', 'passed');
  { Browser page lifetime owns these two objects until navigation unloads it.
    Embedded callers instead explicitly free the host before its document. }
  {$else}
  try
    GApplication := TNyxLCLApplication.Create;
    try
      GApplication.Run(GDocument);
    finally
      GApplication.Free;
    end;
  finally
    GDocument.Free;
  end;
  {$endif}
end.
