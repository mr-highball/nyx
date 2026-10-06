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
unit nyx.observation;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { An installed observer can be prepared beside an accepted view. Its native
    hooks remain connected, but publication changes only their admitted router
    revision. Obtain and check this port before retiring the old view.

    PublishRevision is a commit operation: implementations must only copy the
    nonnegative admitted revision into themselves and their owned hooks. It must
    not allocate, call target APIs, dispatch callbacks or raise. The renderer
    supplies its actual committed event-router revision; no next-epoch guess is
    necessary. This port owns no renderer/control/model and retains no cycle. }
  INyxObservationPublication = interface(IInterface)
    ['{A0E94329-276E-4E44-BE24-771112ADB017}']
    function GetReady: Boolean;
    procedure PublishRevision(ARevision: Integer);
    property Ready: Boolean read GetReady;
  end;

implementation

end.
