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
unit nyx.binding.inputs;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.types, nyx.text, nyx.state, nyx.publication.types;

const
  NyxMaximumInputValues = 64;

type
  { Expected input-admission conflicts are distinct from malformed captures,
    unknown roles and implementation errors. No value is installed on refusal. }
  TNyxInputRefusalKind = (nirkInteraction, nirkProjection, nirkConflictingState);
  { Structured expected refusal retains a copied control identity and exact
    portable diagnostic, never a node, state owner or widget. Existing callers
    catching ENyxState remain compatible. Diagnostic avoids native ANSI loss in
    Exception.Message when a creator supplies supplementary Unicode. }
  ENyxInputAssignmentRefused = class(ENyxState)
  private
    FKind: TNyxInputRefusalKind;
    FControl: TNyxControlRef;
    FDiagnostic: TNyxText;
  public
    constructor Create(AKind: TNyxInputRefusalKind; const AControl: TNyxControlRef;
      const ADiagnostic: TNyxText);
    property Kind: TNyxInputRefusalKind read FKind;
    property Control: TNyxControlRef read FControl;
    property Diagnostic: TNyxText read FDiagnostic;
  end;
  { One copied typed assignment to an exact runtime identity. This is data,
    not authority: only a fresh capture from the same live input owner admits
    it. Values preserve their scalar family; no property names or wire strings
    select behavior. There are no node, widget, store or callback references. }
  TNyxInputValue = record
  private
    FControl: TNyxControlRef;
    FValue: TNyxStateValue;
    function GetControl: TNyxControlRef;
    function GetValue: TNyxStateValue;
  public
    property Control: TNyxControlRef read GetControl;
    property Value: TNyxStateValue read GetValue;
  end;

  { Immutable bounded observation from one exact mounted input generation.
    Stored absence stays distinct from an explicit empty text value; ValueAt
    refuses absence. The snapshot remains readable after retirement but cannot
    authorize a successor. It retains no node, widget, document or state store. }
  INyxInputCapture = interface(IInterface)
    ['{1656EB1A-0D55-437E-A3D8-79231F15190B}']
    function GetCount: Integer;
    function ControlAt(AIndex: Integer): TNyxControlRef;
    function HasValueAt(AIndex: Integer): Boolean;
    function ValueAt(AIndex: Integer): TNyxStateValue;
    property Count: Integer read GetCount;
  end;

  { UI-thread commands on the existing live scalar/binding owner. Capture admits
    1..64 distinct scalar roles. Prepare accepts only captured roles, checks
    current interaction/domain and writable binding contracts, then reserves
    the view and existing state owner without publishing or invoking handlers.

    Publish the returned preparation alone or in PublishNyxGroup with a prepared
    collection query. All model participants install before any notification.
    Refusal/abandonment changes nothing; notification failure remains committed.
    No click/change callbacks are synthesized by a programmatic reset. Retained
    command ports revoke on retirement; they do not keep their host alive.
    Text uses the existing portable input projection: memo LF normalization,
    single-line constraints and refusal of lossy NUL widget text still apply.
    Captures stale after accepted scalar/state/resource commands, including an
    A-to-B-to-A sequence. The finite per-view serial refuses at High(Integer).

    Target adapters add weak physical draft/readiness proof to this same port
    and synchronize only explicitly reset roles. Active composition or capture
    refuses before installation; opaque unsupported input faces refuse too.
    A standalone model owner may omit physical hooks. This contract grants no
    borrowed widget or permission to manipulate an adapter's controls. }
  INyxInputCommands = interface(IInterface)
    ['{3F54CE67-72E2-4E90-AF67-82C72C19A339}']
    function GetConnected: Boolean;
    function Capture(const AControls: array of TNyxControlRef): INyxInputCapture;
    function Prepare(const ACapture: INyxInputCapture;
      const AValues: array of TNyxInputValue): INyxPreparedPublication;
    property Connected: Boolean read GetConnected;
  end;

function NyxInputValue(const AControl: TNyxControlRef;
  const AValue: TNyxStateValue): TNyxInputValue; overload;
function NyxInputValue(const AControl: TNyxControlRef;
  const AValue: TNyxText): TNyxInputValue; overload;
function NyxInputValue(const AControl: TNyxControlRef;
  AValue: Boolean): TNyxInputValue; overload;
function NyxInputValue(const AControl: TNyxControlRef;
  AValue: Integer): TNyxInputValue; overload;
function NyxInputValue(const AControl: TNyxControlRef;
  AValue: Double): TNyxInputValue; overload;

implementation

constructor ENyxInputAssignmentRefused.Create(AKind: TNyxInputRefusalKind;
  const AControl: TNyxControlRef; const ADiagnostic: TNyxText);
begin
  inherited Create(ADiagnostic);
  FKind := AKind;
  FControl.ID := AControl.ID;
  FDiagnostic := ADiagnostic;
end;

function TNyxInputValue.GetControl: TNyxControlRef;
begin
  Result.ID := FControl.ID;
end;

function TNyxInputValue.GetValue: TNyxStateValue;
begin
  Result := FValue.Copy;
end;

function NyxInputValue(const AControl: TNyxControlRef;
  const AValue: TNyxStateValue): TNyxInputValue;
begin
  AValue.Validate;
  Result.FControl.ID := AControl.ID;
  Result.FValue := AValue.Copy;
end;

function NyxInputValue(const AControl: TNyxControlRef;
  const AValue: TNyxText): TNyxInputValue;
begin
  Result := NyxInputValue(AControl, TNyxStateValue.FromText(AValue));
end;

function NyxInputValue(const AControl: TNyxControlRef;
  AValue: Boolean): TNyxInputValue;
begin
  Result := NyxInputValue(AControl, TNyxStateValue.FromBoolean(AValue));
end;

function NyxInputValue(const AControl: TNyxControlRef;
  AValue: Integer): TNyxInputValue;
begin
  Result := NyxInputValue(AControl, TNyxStateValue.FromInteger(AValue));
end;

function NyxInputValue(const AControl: TNyxControlRef;
  AValue: Double): TNyxInputValue;
begin
  Result := NyxInputValue(AControl, TNyxStateValue.FromNumber(AValue));
end;

end.
