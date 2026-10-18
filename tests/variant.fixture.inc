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
{$codepage utf8}
type
  { Two views of compiler-owned plain ordinal storage. The resolver retains
    both alternatives; only the compiled companion observes their overlap. }
  TCoordinates = record
    case Integer of
      0: (X, Y: LongInt);
      1: (Values: array[0..1] of LongInt);
  end;
  { Nested named selectors remain real fields in one ordinary record scope.
    Every arm contains value-only scalars, so no managed lifetime is conditional. }
  TPacket = record
    Fixed: LongInt;
    case Kind: Boolean of
      False: (First, Second: LongInt);
      True: (Prefix: LongInt;
        case Mode: Byte of
          1: (Detail: LongInt);
          2: (Other: LongInt));
  end;
  { Retained authoring hint, deliberately unused by the compiled companion. }
  TLegacyCoordinates = TCoordinates deprecated 'Use TCoordinates';
