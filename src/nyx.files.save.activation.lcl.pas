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

unit nyx.files.save.activation.lcl;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  Controls, nyx.files.save, nyx.files.save.activation;

{ Runtime convenience composition with ordinary Nyx confirmation/modal adapters.
  Each request owns independent content. Owner is borrowed and must outlive the
  host and pending requests.
  Default policy prompts only after ActivationRequired; callers may always prompt
  or supply their own presentation factory to NewNyxActivatedFileSaveHost. }
function NewNyxLCLActivatedFileSaveHost(AOwner: TWinControl;
  const AHost: INyxFileSaveHost;
  APolicy: TNyxSaveActivationPolicy = nsapWhenRequired): INyxFileSaveHost;

implementation

uses SysUtils, nyx.text, nyx.confirmation, nyx.confirmation.lcl;

type
  { Managed factory retaining no request, payload or presenter. Its UI-thread
    sequence distinguishes simultaneously open recipes made by this host.
    The native owner is borrowed under the constructor's lifetime contract. }
  TPrompts = class(TInterfacedObject, INyxSaveActivationPrompts)
  private
    FSequence: Integer;
    FOwner: TWinControl;
  public
    constructor Create(AOwner: TWinControl);
    function CreatePrompt(const AOptions: TNyxFileSaveOptions;
      AByteCount: Integer): INyxConfirmation;
    function PromptOptions(const AOptions: TNyxFileSaveOptions): TNyxConfirmationOptions;
  end;

function TPrompts.PromptOptions(const AOptions: TNyxFileSaveOptions): TNyxConfirmationOptions;
begin
  Result := NyxConfirmation('Ready to save').Sizing(ncsContent);
end;

constructor TPrompts.Create(AOwner: TWinControl);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TPrompts.CreatePrompt(const AOptions: TNyxFileSaveOptions;
  AByteCount: Integer): INyxConfirmation;
begin
  Inc(FSequence);
  Result := NewNyxLCLConfirmation(FOwner, NewNyxSaveActivationContent(
    'file-save-' + TNyxText(IntToStr(FSequence)), AOptions, AByteCount));
end;

function NewNyxLCLActivatedFileSaveHost(AOwner: TWinControl;
  const AHost: INyxFileSaveHost; APolicy: TNyxSaveActivationPolicy): INyxFileSaveHost;
var
  LPrompts: INyxSaveActivationPrompts;
begin
  LPrompts := TPrompts.Create(AOwner);
  Result := NewNyxActivatedFileSaveHost(AHost, LPrompts, APolicy);
end;

end.
