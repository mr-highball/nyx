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

{ Include in an application unit that uses inline callback syntax too: mode
  switches are per unit. These gates select the qualified compiler families,
  not an assertion that every development snapshot implements closures correctly.
  NYX_DISABLE_INLINE_CALLBACKS retains the ordinary/nested adapters for a caller
  qualifying an older or affected compiler. Native nestedprocvars predates 3.3.1. }
{$undef NYX_HAS_INLINE_CALLBACKS}
{$ifdef PAS2JS}
  {$if PAS2JS_FULLVERSION >= 30301}
    {$define NYX_HAS_INLINE_CALLBACKS}
  {$endif}
{$else}
  {$modeswitch nestedprocvars}
  {$if FPC_FULLVERSION >= 30301}
    {$define NYX_HAS_INLINE_CALLBACKS}
  {$endif}
{$endif}
{$ifdef NYX_DISABLE_INLINE_CALLBACKS}
  {$undef NYX_HAS_INLINE_CALLBACKS}
{$endif}
{$ifdef NYX_HAS_INLINE_CALLBACKS}
  {$modeswitch functionreferences}
  {$modeswitch anonymousfunctions}
{$endif}
