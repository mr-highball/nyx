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
type
  { Value-only compiler oracle. Operators own no application or callback state.
    The source marker 🌟 𐐷 qualifies included operator scalar coordinates. }
  TOperatorVector = record
    Value: LongInt;
    function Add(AValue: LongInt): LongInt;
    class operator +(const A, B: TOperatorVector): TOperatorVector;
    class operator +(const A: TOperatorVector): TOperatorVector;
    class operator Add(const A: TOperatorVector; B: LongInt): TOperatorVector;
    class operator =(const A, B: TOperatorVector): Boolean;
    class operator Implicit(A: LongInt): TOperatorVector;
    class operator Explicit(const A: TOperatorVector): LongInt;
  end;
