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
unit nyx.interaction;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses nyx.model;

type
  { Immutable effective interaction policy for a borrowed node and its ancestors.
    A descendant cannot re-enable a disabled/hidden ancestor or opt out of an
    ancestor's read-only scope. Read-only keeps focus and notification callbacks;
    it refuses user value changes, including compound actions. Application state
    writes remain deliberate programmatic updates and are not blocked by this
    UI policy. This record retains no node, widget, store or interface. }
  TNyxInteractionPolicy = record
  private
    FEnabled: Boolean;
    FVisible: Boolean;
    FReadOnly: Boolean;
    function GetCanIssueCommand: Boolean;
    function GetCanEditValue: Boolean;
  public
    property Enabled: Boolean read FEnabled;
    property Visible: Boolean read FVisible;
    property ReadOnly: Boolean read FReadOnly;
    property CanIssueCommand: Boolean read GetCanIssueCommand;
    property CanEditValue: Boolean read GetCanEditValue;
  end;

{ Inspect a mounted/independently realized tree, after platform scopes and live
  bindings have been projected. Nil fails explicitly; the caller keeps the tree
  alive during this synchronous read. No authored defaults are changed. }
function NyxInteractionPolicy(ANode: TNyxNode): TNyxInteractionPolicy;

implementation

uses nyx.types;

function TNyxInteractionPolicy.GetCanIssueCommand: Boolean;
begin
  Result := FEnabled and FVisible;
end;

function TNyxInteractionPolicy.GetCanEditValue: Boolean;
begin
  Result := GetCanIssueCommand and not FReadOnly;
end;

function NyxInteractionPolicy(ANode: TNyxNode): TNyxInteractionPolicy;
var
  LAncestor: TNyxNode;
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('Interaction policy requires a live node');
  end;
  Result.FEnabled := True;
  Result.FVisible := True;
  Result.FReadOnly := False;
  LAncestor := ANode;
  while LAncestor <> nil do
  begin
    Result.FEnabled := Result.FEnabled and
      (LAncestor.Prop(NyxAttributeName(atEnabled), 'true') <> 'false');
    Result.FVisible := Result.FVisible and
      (LAncestor.Prop(NyxAttributeName(atVisible), 'true') <> 'false');
    Result.FReadOnly := Result.FReadOnly or
      (LAncestor.Prop(NyxAttributeName(atReadOnly)) = 'true');
    LAncestor := LAncestor.Parent;
  end;
end;

end.

