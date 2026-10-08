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


unit nyx.resources.import;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.text, nyx.resources;

type
  TNyxResourcePickStatus = (rpsFailed, rpsSelected, rpsCancelled);
  { A reply owns an immutable definition. Kind is caller-selected; filename/MIME
    never silently change the type. No local path, stream or control is retained. }
  TNyxResourcePickReply = procedure(AStatus: TNyxResourcePickStatus;
    const ADefinition: INyxResourceDefinition; const AError: TNyxText) of object;
  { One UI-thread import, up to the portable per-file byte budget. Native reply
    may complete inline; browser reply is asynchronous and needs user activation.
    Capture project/form context before Pick. Cancel silently disconnects the
    borrowed receiver; call it before retirement. Reentrant replies may release
    the picker. Repeated browser Pick retires the previous operation. }
  INyxResourcePicker = interface
    ['{29D24BFA-E0D4-4B54-8230-954743606F5D}']
    procedure Pick(AKind: TNyxResourceKind; AReply: TNyxResourcePickReply);
    procedure Cancel;
  end;

implementation

end.
