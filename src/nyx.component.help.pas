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

unit nyx.component.help;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses
  nyx.text, nyx.controls;

const
  NyxComponentHelpRootID = 'component-help';

{ Reusable specialized Nyx content for contextual creator help. The caller owns
  the returned managed card and may add ordinary controls or customize named
  title/description/capabilities/close parts before presentation. All supplied
  text is user/creator content, independent of behavior or compiler targets. }
function NewNyxComponentHelp(const ATitle, ADescription,
  ACapabilities: TNyxText): INyxCard;

implementation

uses
  nyx.types;

function NewNyxComponentHelp(const ATitle, ADescription,
  ACapabilities: TNyxText): INyxCard;
var
  LTitle: INyxHeading;
  LDescription: INyxLabel;
  LCapabilities: INyxLabel;
  LClose: INyxButton;
begin
  Result := NewNyxCard(NyxComponentHelpRootID);
  Result.Configure.Layout(nlColumn).Padding(20).Gap(12).Compound(True).Done;
  LTitle := NewNyxHeading('component-help-title');
  LTitle.Configure.Text(ATitle).PartName(NyxPart('title')).Done;
  Result.Add(LTitle);
  LDescription := NewNyxLabel('component-help-description');
  LDescription.Configure.Text(ADescription).PartName(NyxPart('description')).Done;
  Result.Add(LDescription);
  LCapabilities := NewNyxLabel('component-help-capabilities');
  LCapabilities.Configure.Text(ACapabilities).PartName(NyxPart('capabilities')).Done;
  Result.Add(LCapabilities);
  LClose := NewNyxButton('component-help-close');
  LClose.Configure.Text('Close').PartName(NyxPart('close'))
    .OnClick(NyxSemantic(nseDismiss)).Done;
  Result.Add(LClose);
end;

end.

