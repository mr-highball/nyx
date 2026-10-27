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
unit nyx.content.projection.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

type
  { UI-only logical-metric section. Explicit Close is idempotent and reports
    projection failures. Final release also balances the section without throwing
    over an active exception. Implementations retain the original physical
    lifetime frame, never a replacement renderer. Retirement prevents projection
    on exit. Do not keep a section open across a queued/UI event-loop turn. }
  INyxLCLLogicalUpdate = interface(IInterface)
    ['{782989F5-3CC3-49F9-9756-692AB0DCE1CD}']
    procedure Close;
  end;

  { Optional native content boundary, independent of monitor DPI. A target
    opens logical geometry before updating its own metrics and closes afterward.
    Nil opens no independent section: the tree may be unprojected, already inside
    an update, changing faces or retired. Callers still honor their input/target
    lifetime. ChangingFaces suppresses intermediate native resize callbacks;
    neither this port nor a returned section grants application edit authority.
    The managed port must revoke its borrowed renderer at generation retirement. }
  INyxLCLContentProjection = interface(IInterface)
    ['{C0AD8115-A07B-4E90-A21A-CE610CD96681}']
    function BeginLogicalUpdate: INyxLCLLogicalUpdate;
    function GetChangingFaces: Boolean;
    function GetScale: Double;
    property ChangingFaces: Boolean read GetChangingFaces;
    property Scale: Double read GetScale;
  end;

implementation

end.
