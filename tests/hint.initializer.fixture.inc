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
  { Contextual hint words remain ordinary field names in initializer groups. }
  THintData = record
    platform: LongInt;
    deprecated: LongInt;
  end;
  { A named callback keeps its calling convention separate from variable hints. }
  TInitializerCallback = function(X: LongInt): LongInt; cdecl;

var
  { Retain the hint between the declared type and the Boolean initializer. }
  LegacyFlag: Boolean platform = False;
  { The compiler reads this hint message before assigning the initial value. }
  LegacyCount: LongInt deprecated 'Use CurrentCount' = 40;
  { Record labels and semicolons inside the value never become declaration hints. }
  CurrentData: THintData platform = (platform: 40; deprecated: 2);
  { Ordinary following siblings retain independent ranges and values. }
  FollowingValue: LongInt = 3;
  { The variable's hint and nil value do not alter the callback signature. }
  Callback: TInitializerCallback platform = nil;
