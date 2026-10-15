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

unit nyx.publication.types;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { UI-thread-only, single-use prepared model publication. Construction may
    reserve an owner but invokes no external callbacks. Validate admits a
    detached candidate. Install adopts preallocated data without callbacks,
    validation, scheduling, allocation or exceptions. Notify runs only AFTER
    every group participant installs; notification failure reports committed data.
    Retire is idempotent and releases reservations without rolling back installed
    models. Destruction retires abandoned work. Owners revoke borrowed receivers
    before destruction; the coordinator retains no document or control.
    This independent unit lets scalar state participate without a unit cycle.
    nyx.publication retains its original public aliases and interface GUID. }
  INyxPreparedPublication = interface
    ['{9367619F-A6CB-46D1-833C-C42473701750}']
    procedure Validate;
    procedure Install;
    procedure Notify;
    procedure Retire;
  end;
  TNyxPreparedPublications = array of INyxPreparedPublication;
  { Optional final readiness fence, preserving the original phase interface.
    Coordinators capture this capability before invoking validators, then check
    every fence after ALL validators and before ANY install. CheckReady may only
    inspect already-owned/revocable facts and raise; it must never invoke user
    callbacks, mutate models, schedule or allocate a candidate. This catches an
    owner revoked by a later participant's validator without rerunning callbacks. }
  INyxPublicationReadiness = interface(IInterface)
    ['{DAE081A5-AC84-4EAA-A83F-953E251DB261}']
    procedure CheckReady;
  end;

implementation

end.
