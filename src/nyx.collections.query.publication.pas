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
unit nyx.collections.query.publication;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.collections.query,
  nyx.publication.types;

type
  { Immutable query intent captured from one exact live view. Implementations
    keep their view identity and monotonic intent proof private: identical query
    values from another view do not grant authority. The capture owns copied
    query data and an inert identity, never source rows, a store, a document or
    target controls. Row changes before preparation do not stale this intent.
    A capture can still be read after its view ends, but cannot revive that view. }
  INyxCollectionQueryCapture = interface(IInterface)
    ['{60B39FCC-3EF6-40CB-9F8A-C6BEBDCF6B00}']
    { Return an independently usable value. Changing a fluent copy cannot alter
      this capture, another reader or the view's accepted runtime policy. }
    function GetQuery: TNyxCollectionQuery;
    property Query: TNyxCollectionQuery read GetQuery;
  end;

  { Optional prepared-query capability; the existing collection-view interface
    and its GUID remain unchanged. Obtain this facet from the actual view.
    Unsupported implementations refuse explicitly rather than emulate atomic
    publication with successive ConfigureQuery/state writes. All methods belong
    to the owning UI thread; an interface is not cross-thread authority.

    An effective accepted policy change invalidates older captures, including
    A-to-B-to-A changes. Legacy equal ConfigureQuery remains a complete no-op.
    This API does not define contribution sessions, header controls or a new
    query evaluator. It does not authorize mixed source-row/query publication. }
  INyxCollectionQueryPublication = interface(IInterface)
    ['{13945B60-B486-446D-AEA2-B0384EDD232C}']
    { Capture accepted query intent, not a source revision. Implementations must
      distinguish their own opaque proof from foreign or fabricated captures. }
    function CaptureQuery: INyxCollectionQueryCapture;
    { Reserve this exact view against the supplied intent. A prepared/atomic
      source is required. Normalize and stage the full result, page, selection
      and hierarchy using the then-current source; publish and notify nothing.
      The preparation retains its view/source and displaced references through
      retirement. Selection, paging, subscriptions, query mutation and source
      row preparation must not bypass the reservation. A second same-view stage
      refuses; distinct views may contribute to one existing prepared group.

      Validate admits the detached proposal. Existing readiness checks run after
      every group validator. Install only adopts allocated data. Notify runs
      after all models install; observer failure reports committed data and does
      not prevent independent notifications. Idempotent Retire releases the
      reservation without undoing installed state; destruction abandons safely.
      An equal query can guard a group but retains snapshot identity and emits
      no query notification. Invalid/foreign/stale captures change nothing. }
    function PrepareQuery(const ACapture: INyxCollectionQueryCapture;
      const AQuery: TNyxCollectionQuery): INyxPreparedPublication;
  end;

implementation

end.
