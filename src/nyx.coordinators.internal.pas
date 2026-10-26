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

unit nyx.coordinators.internal;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.controls, nyx.model, nyx.coordinators, nyx.data, nyx.collections.bindings,
  nyx.events, nyx.scheduler, nyx.binding.inputs;

type
  { Trusted weak adapter seam. The exact full realized tree is borrowed only
    for this call. Return nil until THAT tree is adopted and its bindings are
    active; an old mounted view must never satisfy a candidate's lookup.
    This getter is pure and exposes neither tree nor widgets to creators. }
  INyxCoordinatorInputHost = interface(IInterface)
    ['{B10521D5-51B3-4D5C-8105-870FF76F501A}']
    function InputsFor(ATree: TNyxNode): INyxInputCommands;
  end;

  { Library integration seam for an existing built-in engine that returns real
    specialized control facades. The common driver supplies the SAME retained
    canonical candidate used by its guarded projection, never a cloned or ID-only
    replacement. Implementations may retain that control, not its weak parent,
    document or platform host; active authority still ends with the common scope.

    This optional interface deliberately stays separate from the public creator
    factory/projection contracts and their GUIDs. It is trusted Pascal plumbing,
    not a security boundary. It adds neither a renderer branch nor a lifecycle:
    its result enters the ordinary prepared coordinator mount and final fences. }
  INyxCanonicalCoordinatorFactory = interface(IInterface)
    ['{F439951C-7AF1-4899-91A4-AB8DD51CF132}']
    function PrepareCanonical(const ACanonical: INyxControl;
      const AProjection: INyxCoordinatorProjection;
      const AIntent: TNyxDataValue; const AViews: INyxCollectionBindings;
      const AView: INyxPreparedEventView; const AScheduler: INyxScheduler;
      const ARefresh: INyxCoordinatorRefresh): INyxPreparedCoordinator;
  end;

implementation

end.
