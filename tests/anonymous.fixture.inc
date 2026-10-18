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
  { Grouped members share one inline type, while nested/array/variant bodies own
    separate member scopes. Plain scalars keep runtime lifetime unconditional. }
  TAnonymousScene = record
    First, Second: record
      Caption: LongInt;
      Detail: record
        Level: LongInt
      end;
    end;
    Rows: array[0..1] of packed record
      Count: LongInt;
    end;
    case Mode: Byte of
      0: (Extra: record
        case Boolean of
          False: (Low: LongInt);
          True: (High: LongInt);
      end);
      1: (Other: LongInt);
  end;
  { Named containers keep their original anonymous element identity. }
  TAnonymousBag = array[0..1] of record
    Code: LongInt;
  end;
  { Inline bodies inherit the owning template's concrete environment. }
  TAnonymousBox<T> = record
    Data: record
      Value: T;
    end;
  end;
  { A declared specialization, not an analyzer-generated replacement type. }
  TAnonymousIntegerBox = TAnonymousBox<LongInt>;

const
  Settings: record
    Limit: LongInt;
  end = (Limit: 8);
