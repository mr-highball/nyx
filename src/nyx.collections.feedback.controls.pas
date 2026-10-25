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
unit nyx.collections.feedback.controls;

{$mode delphi}{$H+}{$codepage utf8}

interface

uses nyx.controls, nyx.collections.feedback;

type
  { Specialized compound card with open named parts. Authors can replace/rebind
    labels, progress and actions through the ordinary Nyx control contract.
    Export expands these portable parts; no adapter-specific widget is needed. }
  INyxCollectionFeedbackPanel = interface(INyxCard)
    ['{1153C727-9706-4FB4-A42E-D67FB68F2361}']
    function GetParts: TNyxCollectionFeedbackParts;
    property Parts: TNyxCollectionFeedbackParts read GetParts;
  end;

{ Define AParts' scalar state on the document first, then bind its mounted view
  separately with BindNyxCollectionFeedback. Actions have no hidden callbacks. }
function NewNyxCollectionFeedbackPanel(const AParts: TNyxCollectionFeedbackParts;
  const AText: TNyxCollectionFeedbackText): INyxCollectionFeedbackPanel;

implementation

uses nyx.types, nyx.responsive;

type
  { Ordinary managed card owns its children; this value-only parts descriptor
    introduces no runtime context, view, event router or callback ownership. }
  TFeedbackPanel = class(TNyxCard, INyxCollectionFeedbackPanel)
  private
    FParts: TNyxCollectionFeedbackParts;
  public
    constructor Create(const AParts: TNyxCollectionFeedbackParts;
      const AText: TNyxCollectionFeedbackText); reintroduce;
    function GetParts: TNyxCollectionFeedbackParts;
  end;

constructor TFeedbackPanel.Create(const AParts: TNyxCollectionFeedbackParts;
  const AText: TNyxCollectionFeedbackText);
var
  LCaption: INyxLabel;
  LDetail: INyxLabel;
  LProgress: INyxProgress;
  LCommands: INyxRow;
  LRetry: INyxButton;
  LReset: INyxButton;
  LResume: INyxButton;
begin
  AParts.Validate;
  inherited Create(AParts.RootID);
  FParts := AParts;
  GetConfigure.Layout(nlColumn).Padding(12).Gap(8).Done;
  GetBinds.AccessibleName(AParts.Caption).Done;
  LCaption := NewNyxLabel(AParts.Caption.Name);
  Add(LCaption);
  LCaption.Binds.Text(AParts.Caption).AccessibleName(AParts.Caption).Done;
  LDetail := NewNyxLabel(AParts.Detail.Name);
  Add(LDetail);
  LDetail.Binds.Text(AParts.Detail).AccessibleName(AParts.Detail).Done;
  LProgress := NewNyxProgress(AParts.ProgressID).WithIndeterminate(True);
  Add(LProgress);
  LProgress.Configure.AccessibleName(AText.Heading(ncfkLoading)).Done;
  LProgress.Binds.Visible(AParts.Busy).Done;
  LCommands := NewNyxRow(AParts.RootID + '-actions');
  Add(LCommands);
  LCommands.Configure.Gap(8).Done;
  LCommands.Configure.WhenViewport(TNyxViewportWidth.Below(560)).Layout(nlColumn).Done;
  LRetry := NewNyxButton(AParts.RetryID).WithText(AText.RetryCaption);
  LCommands.Add(LRetry);
  LRetry.Binds.Visible(AParts.RetryVisible).Done;
  LReset := NewNyxButton(AParts.ResetID).WithText(AText.ResetCaption);
  LCommands.Add(LReset);
  LReset.Binds.Visible(AParts.ResetVisible).Done;
  LResume := NewNyxButton(AParts.ResumeID).WithText(AText.ResumeCaption);
  LCommands.Add(LResume);
  LResume.Binds.Visible(AParts.ResumeVisible).Done;
end;

function TFeedbackPanel.GetParts: TNyxCollectionFeedbackParts;
begin
  Result := FParts;
end;

function NewNyxCollectionFeedbackPanel(const AParts: TNyxCollectionFeedbackParts;
  const AText: TNyxCollectionFeedbackText): INyxCollectionFeedbackPanel;
begin
  Result := TFeedbackPanel.Create(AParts, AText);
end;

end.
