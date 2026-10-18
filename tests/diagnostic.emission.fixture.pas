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
unit diagnostic.emission.fixture;

{$codepage utf8}
{ Begin the native compiler in ObjFPC mode. This source mode transition must
  retain the explicit warning request, independently of Nyx's implementation. }
{$warnings off}
{$mode delphi}{$H+}

interface

{ Real tokens allow FPC's queued settings to activate before scanner messages.
  The fixture is compiled alone, never linked into a product or executed. Its
  three deliberately visible markers are an independent compiler oracle. }
const
  CModeBarrier = 1;
{$warning NYX_EMISSION_HIDDEN_MODE}
{$warnings on}{$hints on}{$notes on}
const
  CBeforeBarrier = 2;
{$push}
{$warnings off explanatory text}
{$hints-}
{$notes off}
const
  CSuppressedBarrier = 3;
{$warning NYX_EMISSION_HIDDEN_WARNING}
{$hint NYX_EMISSION_HIDDEN_HINT}
{$note NYX_EMISSION_HIDDEN_NOTE}
{$pop}
const
  CRestoredBarrier = 4;
{$warning NYX_EMISSION_VISIBLE_WARNING}
{$hint NYX_EMISSION_VISIBLE_HINT}
{$note NYX_EMISSION_VISIBLE_NOTE}

implementation

end.
