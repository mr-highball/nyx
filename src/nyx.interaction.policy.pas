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
unit nyx.interaction.policy;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { Managed runtime constraint, separate from authored/live enabled values.
    Allowed can follow another portable runtime (for example collection pages).
    Reads are synchronous UI-thread operations, must not mutate or call back,
    and must not retain a node/document/renderer. All attached policies AND with
    the effective enabled property; they can restrict, never override a caller.

    Realized nodes retain policies. Runtime projection copies retain the same
    read-only capabilities so commands cannot bypass a live constraint. An
    ordinary descriptor clone excludes them. Owners detach before retirement;
    policies must remain safe if a retained projection outlives their owner. }
  INyxRuntimeEnabledPolicy = interface(IInterface)
    ['{06F3E34F-FF52-4DA0-96AE-553A3587A7D8}']
    function GetAllowed: Boolean;
    property Allowed: Boolean read GetAllowed;
  end;

  { Read-only constraints OR with authored/inherited read-only state. They
    cannot enable editing, retain a tree/view or rewrite stored properties.
    The same UI-thread, projection-copy and retirement contract applies. }
  INyxRuntimeReadOnlyPolicy = interface(IInterface)
    ['{037EEC12-046F-4F49-9D60-00EF0C5AB078}']
    function GetReadOnly: Boolean;
    property ReadOnly: Boolean read GetReadOnly;
  end;

implementation

end.
