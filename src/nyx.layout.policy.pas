{ nyx

  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit nyx.layout.policy;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.types;

type
  { Independent immutable value builder. Each fluent call returns a changed
    copy; it never changes a previously stored policy or retains a control.
    Configure.Layout copies these four closed choices into its descriptor.
    Metrics and child sizing remain independently configurable. }
  TNyxLayoutPolicy = record
  private
    FMode: TNyxLayoutMode;
    FWrap: TNyxFlowWrap;
    FAlignment: TNyxCrossAlignment;
    FJustification: TNyxJustification;
  public
    { Factories initialize every field. Row/Column provide readable starting
      points; Flow also admits grid/absolute, whose flow choices remain dormant. }
    class function Flow(AMode: TNyxLayoutMode): TNyxLayoutPolicy; static;
    class function Row: TNyxLayoutPolicy; static;
    class function Column: TNyxLayoutPolicy; static;
    function Wrap(AValue: TNyxFlowWrap): TNyxLayoutPolicy;
    function Align(AValue: TNyxCrossAlignment): TNyxLayoutPolicy;
    function Justify(AValue: TNyxJustification): TNyxLayoutPolicy;
    property Mode: TNyxLayoutMode read FMode;
    property Wrapping: TNyxFlowWrap read FWrap;
    property Alignment: TNyxCrossAlignment read FAlignment;
    property Justification: TNyxJustification read FJustification;
  end;

implementation

class function TNyxLayoutPolicy.Flow(AMode: TNyxLayoutMode): TNyxLayoutPolicy;
begin
  Result.FMode := AMode;
  Result.FWrap := nfwAutomatic;
  Result.FAlignment := ncaAutomatic;
  Result.FJustification := njStart;
end;

class function TNyxLayoutPolicy.Row: TNyxLayoutPolicy;
begin
  Result := Flow(nlRow);
end;

class function TNyxLayoutPolicy.Column: TNyxLayoutPolicy;
begin
  Result := Flow(nlColumn);
end;

function TNyxLayoutPolicy.Wrap(AValue: TNyxFlowWrap): TNyxLayoutPolicy;
begin
  Result := Self;
  Result.FWrap := AValue;
end;

function TNyxLayoutPolicy.Align(AValue: TNyxCrossAlignment): TNyxLayoutPolicy;
begin
  Result := Self;
  Result.FAlignment := AValue;
end;

function TNyxLayoutPolicy.Justify(AValue: TNyxJustification): TNyxLayoutPolicy;
begin
  Result := Self;
  Result.FJustification := AValue;
end;

end.
