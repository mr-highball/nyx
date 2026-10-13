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

unit nyx.test.application.navigation;

{$mode delphi}{$H+}{$codepage utf8}

interface

{ Pure shared admission/ownership/routing checks. Dedicated Unicode/NUL inputs
  are qualification data, never installed as starter/demo content. }
function VerifyNyxPageNavigation: Integer;

implementation

uses
  SysUtils, nyx.text, nyx.types, nyx.behavior, nyx.model, nyx.controls, nyx.root.types,
  nyx.codec, nyx.design.tokens, nyx.selection, nyx.application.navigation;

function VerifyNyxPageNavigation: Integer;
var
  LSource: TNyxDocument;
  LPage: INyxPage;
  LNavigation: INyxPageNavigation;
  LSelection: INyxPageNavigationState;
  LProjection: TNyxNode;
  LRetainedRoot: INyxToolbar;
  LCaption: INyxCaptionControl;
  LTarget: TNyxControlRef;
  LEvent: TNyxEventInfo;
  LBefore: TNyxText;
  LText: TNyxText;
  LRefused: Boolean;

  procedure Check(ACondition: Boolean; const AReason: TNyxText);
  begin

    if not ACondition then
    begin
      raise Exception.Create('Page navigation: ' + AReason);
    end;
    Inc(Result);
  end;

begin
  Result := 0;
  LProjection := nil;
  LSource := TNyxDocument.Create;
  try
    LRefused := False;
    try
      LNavigation := NewNyxPageNavigation(LSource);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused, 'empty source refuses without candidate ownership');
    LPage := NewNyxPage('captioned');
    LText := TNyxText('  Plans 🌙') + #0 + TNyxText('é  ');
    LPage.Text := LText;
    LSource.AddPage(LPage);
    LPage := nil;
    LSource.AddPage(TNyxNode.Create(nkPage, 'legacy-page'));
    SetNyxThemeTokens(LSource, NyxThemeTokens.FontSize(18));
    LBefore := TNyxCodec.Encode(LSource);
    LNavigation := NewNyxPageNavigation(LSource);
    Check(TNyxCodec.Encode(LSource) = LBefore, 'construction does not alter authored pages/theme');
    Check(LNavigation.Document <> LSource, 'navigation owns an independent document');
    LSelection := NyxPageNavigationState(LNavigation);
    Check(LSelection.CurrentPage.ID = '', 'construction does not imply a displayed page');
    LSelection.SelectPage(NyxControl('captioned'));
    Check(LSelection.IsCurrent(NyxControl(NyxPageNavigationButtonID(0))) and
      not LSelection.IsCurrent(NyxControl(NyxPageNavigationButtonID(1))),
      'one exact current target maps to its captured button');
    Check((LNavigation.Root.Part(NyxPart('page-0')).Node.Prop('variant') =
      NyxVariantName(nvPrimary)) and
      (LNavigation.Root.Part(NyxPart('page-1')).Node.Prop('pressed') = ''),
      'current highlighting never invents toggle semantics');
    LRefused := False;
    try
      LSelection.SelectPage(NyxControl('foreign-page'));
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and (LSelection.CurrentPage.ID = 'captioned'),
      'unknown page refuses without altering the current target');
    LSelection.Configure(TNyxSelectionOptions.Defaults
      .Selected(nvSuccess).Unselected(nvSecondary));
    Check(LNavigation.Root.Part(NyxPart('page-0')).Node.Prop('variant') =
      NyxVariantName(nvSuccess), 'copied custom current variant is applied');
    LSelection.Configure(TNyxSelectionOptions.Defaults.Styling(nssAuthored));
    LSelection.SelectPage(NyxControl('legacy-page'));
    Check((LSelection.CurrentPage.ID = 'legacy-page') and
      (LNavigation.Root.Part(NyxPart('page-0')).Node.Prop('variant') =
      NyxVariantName(nvSuccess)), 'authored styling retains creator variants');
    Check(not LSelection.IsCurrent(NyxControl('foreign-button')),
      'unknown button never reports current');
    LProjection := LNavigation.Root.Node.Clone;
    LSelection.Configure(TNyxSelectionOptions.Defaults);
    LSelection.Project(LProjection);
    Check((LProjection.Children[0].Prop('variant') = NyxVariantName(nvDefault)) and
      (LProjection.Children[1].Prop('variant') = NyxVariantName(nvPrimary)),
      'typed current policy projects into an independent runtime tree');
    LProjection.Add(TNyxNode.Create(nkLabel, 'extra-navigation-caption'));
    LProjection.Children[2].Configure.Text('Your workspace').Variant(nvWarning).Done;
    LSelection.Project(LProjection);
    Check((LProjection.Children[2].Prop('text') = 'Your workspace') and
      (LProjection.Children[2].Prop('variant') = NyxVariantName(nvWarning)),
      'additional creator-owned toolbar parts retain their text and appearance');
    LProjection.Remove(LProjection.Children[1]);
    LProjection.Add(TNyxNode.Create(nkButton, 'foreign-button'));
    LProjection.Children[0].Configure.Variant(nvWarning).Done;
    LRefused := False;
    try
      LSelection.Project(LProjection);
    except
      on ENyxModel do
      begin
        LRefused := True;
      end;
    end;
    Check(LRefused and
      (LProjection.Children[0].Prop('variant') = NyxVariantName(nvWarning)),
      'one foreign button refuses the complete projection before any style changes');
    ReleaseNyxNode(LProjection);
    LProjection := nil;
    Check(NyxThemeDeclaration(LNavigation.Document).ToJSON = NyxThemeDeclaration(LSource).ToJSON,
      'declared theme is copied exactly');
    Check(NyxPageCaption(LSource.Pages[0]) = LText, 'typed caption retains whitespace/Unicode/NUL');
    Check(NyxPageCaption(LSource.Pages[1]) = 'legacy-page', 'empty legacy caption falls back to ID');
    LSource.Pages[1].Configure.Text('Legacy caption').Done;
    Check(NyxPageCaption(LSource.Pages[1]) = 'Legacy caption', 'raw legacy caption uses the same contract');
    Check(Supports(LNavigation.Root.Part(NyxPart('page-0')), INyxCaptionControl, LCaption),
      'named navigation parts expose specialized caption interfaces');
    Check(LCaption.Text = LText, 'navigation copies exact authored text');
    Check(Supports(LNavigation.Root.Part(NyxPart('page-1')), INyxCaptionControl, LCaption) and
      (LCaption.Text = 'legacy-page'), 'captions are captured rather than reread after source edits');
    LEvent := Default(TNyxEventInfo);
    LEvent.Trigger := ntClick;
    LEvent.SourceID := NyxPageNavigationButtonID(0);
    LEvent.OriginID := LEvent.SourceID;
    Check(LNavigation.Resolve(LEvent, LTarget) and (LTarget.ID = 'captioned'),
      'caption text is independent of typed routing identity');
    LSource.MoveRoot(NyxRoot(nrPage, 'legacy-page'), 0);
    LSource.Find('captioned').Configure.Text('Changed caption').Done;
    Check(LNavigation.Resolve(LEvent, LTarget) and (LTarget.ID = 'captioned'),
      'source reordering/caption edits do not change captured button targets');
    FreeAndNil(LSource);
    LSelection.SelectPage(NyxControl('captioned'));
    Check(LSelection.CurrentPage.ID = 'captioned',
      'current page capability remains independent of retired source ownership');
    Check(LNavigation.Resolve(LEvent, LTarget) and (LTarget.ID = 'captioned'),
      'source retirement does not leave a borrowed routing pointer');
    LEvent.DefaultPrevented := True;
    Check(not LNavigation.Resolve(LEvent, LTarget) and (LTarget.ID = ''), 'consumed click refuses');
    LEvent.DefaultPrevented := False;
    LEvent.Trigger := ntChange;
    Check(not LNavigation.Resolve(LEvent, LTarget), 'non-click event refuses');
    LEvent.Trigger := ntClick;
    LEvent.SourceID := 'outside-navigation';
    Check(not LNavigation.Resolve(LEvent, LTarget), 'foreign source refuses');
    LEvent.SourceID := NyxPageNavigationButtonID(1);
    Check(not LNavigation.Resolve(LEvent, LTarget), 'another button cannot substitute event source');
    LEvent.SourceID := NyxPageNavigationButtonID(0);
    LEvent.OriginID := 'unknown-origin';
    Check(not LNavigation.Resolve(LEvent, LTarget), 'missing origin refuses');
    LNavigation.Root.Part(NyxPart('page-0')).Add(NewNyxLabel('navigation-icon'));
    LEvent.OriginID := 'navigation-icon';
    Check(LNavigation.Resolve(LEvent, LTarget), 'named button descendants retain routing');
    LEvent.SourceID := 'navigation-icon';
    Check(LNavigation.Resolve(LEvent, LTarget), 'descendant component sources retain routing');
    LEvent.SourceID := 'app-navigation';
    Check(LNavigation.Resolve(LEvent, LTarget), 'compound ancestor source resolves exact child origin');
    LRetainedRoot := LNavigation.Root;
    LCaption := nil;
    LNavigation := nil;
    Check(LSelection.CurrentPage.ID = 'captioned',
      'retained current-page capability owns the independent recipe');
    LSelection := nil;
    Check(Supports(LRetainedRoot.Part(NyxPart('page-0')), INyxCaptionControl, LCaption) and
      (LCaption.Text = LText), 'retained public parts survive independent recipe retirement');
    LCaption := nil;
    LRetainedRoot := nil;
  finally
    ReleaseNyxNode(LProjection);
    LSelection := nil;
    LCaption := nil;
    LNavigation := nil;
    LRetainedRoot := nil;
    LPage := nil;
    LSource.Free;
  end;
end;

end.
