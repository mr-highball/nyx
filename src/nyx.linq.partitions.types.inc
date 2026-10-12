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

  { An independent factory keeps nested sequence types out of INyxSequence<T>'s
    recursive generic expansion. Each result is an owned detached ordinary
    sequence, including a short final chunk, and can outlive all outer cursors.
    Size must be positive; no empty chunks or partial failed chunks are emitted. }
  TNyxChunk<T> = class
  public
    class function From(const ASource: INyxSequence<T>; ASize: Integer):
      INyxSequence<INyxSequence<T>>; static;
  end;

  { Compiler-visible support; applications use the fluent contract/factory.
    Capacity grows geometrically with actual rows up to a checked Integer limit.
    Managed values retain ordinary ownership; borrowed class objects/inner mutable
    arrays retain the existing source contract. No DOM/LCL or live-store ownership. }
  TQueryBuffer<T> = class
  private
    FValues: TNyxSequenceArray<T>;
    FCount: Integer;
    FLimit: Integer;
  public
    constructor Create(ALimit: Integer);
    procedure Add(const AValue: T);
    function ToArray: TNyxSequenceArray<T>;
    property Count: Integer read FCount;
  end;
  TQueryRing<T> = class
  private
    FValues: TNyxSequenceArray<T>;
    FHead: Integer;
    FCount: Integer;
    FLimit: Integer;
    FEmpty: T;
    function Offset(AOffset: Integer): Integer;
    procedure Grow;
  public
    constructor Create(ALimit: Integer);
    function Push(const AValue: T; out AReplaced: T): Boolean;
    function Pop(out AValue: T): Boolean;
    property Count: Integer read FCount;
  end;
  TQueryRangeCursor = class(TEnumerator<Integer>)
  private
    FValue: Integer;
    FRemaining: Integer;
    FStep: Integer;
    FFirst: Boolean;
  protected
    function Advance(out AValue: Integer): Boolean; override;
  public
    constructor Create(AStart, ACount, AStep: Integer);
  end;
  TQueryRangeSequence = class(TNyxSequence<Integer>, INyxSequenceCount)
  private
    FStart: Integer;
    FCount: Integer;
    FStep: Integer;
  public
    constructor Create(AStart, ACount, AStep: Integer);
    function GetEnumerator: INyxEnumerator<Integer>; override;
    function NonEnumeratedCount(out AValue: TNyxQueryCount): Boolean;
  end;
  TQueryGenerateCursor<T> = class(TEnumerator<T>)
  private
    FValue: T;
    FEmpty: T;
    FRemaining: Integer;
    FFirst: Boolean;
    FNext: INyxSelector<T, T>;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const AValue: T; ACount: Integer;
      const ANext: INyxSelector<T, T>);
  end;
  TQueryGenerateSequence<T> = class(TNyxSequence<T>, INyxSequenceCount)
  private
    FValue: T;
    FCount: Integer;
    FNext: INyxSelector<T, T>;
  public
    constructor Create(const AValue: T; ACount: Integer;
      const ANext: INyxSelector<T, T>);
    function GetEnumerator: INyxEnumerator<T>; override;
    function NonEnumeratedCount(out AValue: TNyxQueryCount): Boolean;
  end;
  TQueryInsertOperation = (qioAppend, qioPrepend, qioDefault);
  TQueryInsertCursor<T> = class(TEnumerator<T>)
  private
    FSource: INyxSequence<T>;
    FCursor: INyxEnumerator<T>;
    FValue: T;
    FEmpty: T;
    FOperation: TQueryInsertOperation;
    FInserted: Boolean;
    FHadValue: Boolean;
    FFinished: Boolean;
    procedure Retire;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<T>; const AValue: T;
      AOperation: TQueryInsertOperation);
  end;
  TQueryInsertSequence<T> = class(TNyxSequence<T>)
  private
    FSource: INyxSequence<T>;
    FValue: T;
    FOperation: TQueryInsertOperation;
  public
    constructor Create(const ASource: INyxSequence<T>; const AValue: T;
      AOperation: TQueryInsertOperation);
    function GetEnumerator: INyxEnumerator<T>; override;
  end;
  TQueryTailOperation = (qtoSkip, qtoTake);
  TQueryTailCursor<T> = class(TEnumerator<T>)
  private
    FSource: INyxSequence<T>;
    FCursor: INyxEnumerator<T>;
    FRing: TQueryRing<T>;
    FLimit: Integer;
    FOperation: TQueryTailOperation;
    FPrepared: Boolean;
    FFinished: Boolean;
    procedure Retire;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<T>; ACount: Integer;
      AOperation: TQueryTailOperation);
    destructor Destroy; override;
  end;
  TQueryTailSequence<T> = class(TNyxSequence<T>)
  private
    FSource: INyxSequence<T>;
    FCount: Integer;
    FOperation: TQueryTailOperation;
  public
    constructor Create(const ASource: INyxSequence<T>; ACount: Integer;
      AOperation: TQueryTailOperation);
    function GetEnumerator: INyxEnumerator<T>; override;
  end;
  TQueryReverseCursor<T> = class(TEnumerator<T>)
  private
    FSource: INyxSequence<T>;
    FValues: TNyxSequenceArray<T>;
    FIndex: Integer;
    FStarted: Boolean;
  protected
    function Advance(out AValue: T): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<T>);
  end;
  TQueryReverseSequence<T> = class(TNyxSequence<T>)
  private
    FSource: INyxSequence<T>;
  public
    constructor Create(const ASource: INyxSequence<T>);
    function GetEnumerator: INyxEnumerator<T>; override;
  end;
  TQueryChunkCursor<T> = class(TEnumerator<INyxSequence<T>>)
  private
    FSource: INyxSequence<T>;
    FCursor: INyxEnumerator<T>;
    FSize: Integer;
    FFinished: Boolean;
  protected
    function Advance(out AValue: INyxSequence<T>): Boolean; override;
  public
    constructor Create(const ASource: INyxSequence<T>; ASize: Integer);
  end;
  TQueryChunkSequence<T> = class(TNyxSequence<INyxSequence<T>>)
  private
    FSource: INyxSequence<T>;
    FSize: Integer;
  public
    constructor Create(const ASource: INyxSequence<T>; ASize: Integer);
    function GetEnumerator: INyxEnumerator<INyxSequence<T>>; override;
  end;
