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

unit nyx.studio.exchange;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text;

type
  { Reply and timer notifications run on the editor UI thread. Their receiver
    is borrowed: cancellation must detach it before its owner is released. }
  TNyxEditorReply = procedure(AStatus: Integer; const AText: TNyxText) of object;
  TNyxEditorTick = procedure of object;

  { Private editor transport, separate from public MCP agent authority. Owns one
    request and one scheduled tick. Post is asynchronous and never invokes its
    receiver before returning. Routes are the closed connect/exchange choice;
    tokens and JSON are exact owned text at this explicit HTTP boundary.
    CancelRequest prevents local delivery; it cannot revoke server admission.
    Destroy must retire all transport work before a borrowed receiver dies. }
  TNyxStudioEditorExchange = class abstract
  public
    procedure Post(AConnect: Boolean; const AToken, ABody: TNyxText;
      AReply: TNyxEditorReply); virtual; abstract;
    procedure CancelRequest; virtual; abstract;
    procedure Schedule(ADelayMS: Integer; ATick: TNyxEditorTick); virtual; abstract;
    procedure CancelTick; virtual; abstract;
  end;

implementation

end.
