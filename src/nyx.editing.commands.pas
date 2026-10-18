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

unit nyx.editing.commands;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.types, nyx.editing;

type
  { Platform history producers use the same closed intentions as keyboard/menu
    commands. These are runtime input operations, never document Undo commands. }
  TNyxTextHistoryCommand = (nhcUndo, nhcRedo);

  { Optional managed UI-thread authority for one mounted text input. Adapters
    retain this port only for their current mount. The receiver must recheck its
    source/mount/read-only identity and keep exhausted commands owned; returning
    to an old platform stack is unsafe after programmatic edits. CanExecute is
    side-effect free. Execute publishes text/selection through the owning input
    controller and may retire the mount. Detach borrowed owners before release;
    neither method may destroy an actively dispatching platform control inline. }
  INyxTextHistoryCommands = interface(IInterface)
    ['{CB490151-6190-463A-9536-27CD73D5A05E}']
    function CanExecute(ACommand: TNyxTextHistoryCommand): Boolean;
    procedure Execute(ACommand: TNyxTextHistoryCommand);
    { Capture the pre-edit caret/text before a platform keyboard/clipboard/range
      replacement can move it. Observe only; do not write or destroy the input. }
    procedure BeforeEdit(const AEditing: TNyxEditingSnapshot);
  end;

{ Recognize history shortcuts including repeats. Composition ownership and
  effective edit/mount policy belong to the caller; an empty timeline still
  consumes a recognized command instead of returning to the platform stack. }
function TryNyxTextHistoryKey(const AKey: TNyxKeyStroke;
  out ACommand: TNyxTextHistoryCommand): Boolean;

implementation

function TryNyxTextHistoryKey(const AKey: TNyxKeyStroke;
  out ACommand: TNyxTextHistoryCommand): Boolean;
begin
  ACommand := nhcUndo;
  Result := True;

  if AKey.Matches(nkZKey, [nmControl], True) or AKey.Matches(nkZKey, [nmMeta], True) then
  begin
    Exit;
  end;

  if AKey.Matches(nkZKey, [nmControl, nmShift], True) or
    AKey.Matches(nkZKey, [nmMeta, nmShift], True) or AKey.Matches(nkYKey, [nmControl], True) then
  begin
    ACommand := nhcRedo;
    Exit;
  end;
  Result := False;
end;

end.
