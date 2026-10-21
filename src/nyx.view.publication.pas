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

unit nyx.view.publication;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.model, nyx.events, nyx.publication.types;

type
  { Managed, target-independent source of one optional root publication. The
    renderer retains this source for implicit remounts of its accepted runtime
    view. A provider must revoke borrowed host receivers before their disposal.

    Prepare borrows the exact document and already-realized candidate root only
    during the call. It reserves callbacks/observations on the supplied successor
    event view; it must never attach them to the still-live router generation.
    The returned nonnil participant owns its detached admission data and
    reservations and must support INyxPublicationReadiness. The renderer captures
    that capability before Build or outward validation; a missing fence refuses.

    Renderer admission validates it with the other candidates, then checks every
    final readiness fence before retirement. Install follows the existing pure
    flag/ownership contract, including host ownership needed by accepted controls.
    Notify failure is committed; Retire cannot free an installed runtime still
    borrowed by the view. Design views may use a one-shot retirement participant;
    only runtime views retain the source for later remounts. }
  INyxViewPublicationSource = interface(IInterface)
    ['{A85543CE-1F01-4545-8A77-CE0FAB4D6613}']
    function Prepare(ADocument: TNyxDocument; ARoot: TNyxNode;
      const AView: INyxPreparedEventView): INyxPreparedPublication;
  end;

implementation

end.
