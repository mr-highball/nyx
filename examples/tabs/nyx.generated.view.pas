{ Nyx application views. BuildNyxDocument returns an owned document. }
unit nyx.generated.view;

{$mode delphi}{$H+}
{$codepage utf8}

interface

uses
  nyx.text,
  nyx.dates,
  nyx.times,
  nyx.colors,
  nyx.images,
  nyx.image.lifecycle,
  nyx.design.tokens,
  nyx.types,
  nyx.responsive,
  nyx.presentations,
  nyx.content,
  nyx.containers,
  nyx.editing,
  nyx.gestures,
  nyx.contract,
  nyx.data,
  nyx.state,
  nyx.binding.types,
  nyx.behavior,
  nyx.scheduler,
  nyx.events,
  nyx.callbacks,
  nyx.model,
  nyx.controls;

function BuildNyxDocument: TNyxDocument;

implementation

// <nyx:views>
// Studio synchronizes this builder; keep application helpers outside it.
function BuildNyxDocument: TNyxDocument;
var
  LIdeasPage: INyxPage;
  LIdeasHeading: INyxHeading;
  LIdeasIntroLabel: INyxLabel;
  LIdeaSectionsTabs: INyxTabs;
  LPlanTab: INyxTab;
  LPlanHeading: INyxHeading;
  LPlanNotesMemo: INyxMemo;
  LDetailsTab: INyxTab;
  LDetailsHeading: INyxHeading;
  LIdeaTitleInput: INyxInput;
  LReviewTab: INyxTab;
  LReviewHeading: INyxHeading;
  LReviewCopyLabel: INyxLabel;
begin
  Result := TNyxDocument.Create;
  try
    // Admit each control to its owner before configuring it.
    // A failed configuration releases the complete owned document.
    Result.Title := 'Untitled Nyx application';

    LIdeasPage := NewNyxPage('ideas');
    Result.AddPage(LIdeasPage);
    LIdeasPage.Configure
      .Layout(nlColumn)
      .Gap(16)
      .Padding(24)
      .Text('Ideas')
      .Done;

    LIdeasHeading := NewNyxHeading('ideas-heading');
    LIdeasPage.Add(LIdeasHeading);
    LIdeasHeading.Configure
      .Text('A place for every idea')
      .Done;

    LIdeasIntroLabel := NewNyxLabel('ideas-intro');
    LIdeasPage.Add(LIdeasIntroLabel);
    LIdeasIntroLabel.Configure
      .Text('Switch sections without losing the notes you are writing.')
      .Done;

    LIdeaSectionsTabs := NewNyxTabs('idea-sections');
    LIdeasPage.Add(LIdeaSectionsTabs);
    LIdeaSectionsTabs.Configure
      .Layout(nlColumn)
      .Gap(12)
      .Padding(16)
      .Text('Idea sections')
      .Height(440)
      .Done;

    LPlanTab := NewNyxTab('plan');
    LIdeaSectionsTabs.Add(LPlanTab);
    LPlanTab.Configure
      .Layout(nlColumn)
      .Gap(12)
      .Padding(20)
      .Text('Plan')
      .Done;

    LPlanHeading := NewNyxHeading('plan-heading');
    LPlanTab.Add(LPlanHeading);
    LPlanHeading.Configure
      .Text('Start with a small sketch')
      .Done;

    LPlanNotesMemo := NewNyxMemo('plan-notes');
    LPlanTab.Add(LPlanNotesMemo);
    LPlanNotesMemo.Configure
      .Text('Planning notes')
      .Placeholder('What would you like to make?')
      .Height(200)
      .Done;

    LDetailsTab := NewNyxTab('details');
    LIdeaSectionsTabs.Add(LDetailsTab);
    LDetailsTab.Configure
      .Layout(nlColumn)
      .Gap(12)
      .Padding(20)
      .Text('Details')
      .Done;

    LDetailsHeading := NewNyxHeading('details-heading');
    LDetailsTab.Add(LDetailsHeading);
    LDetailsHeading.Configure
      .Text('Make the details yours')
      .Done;

    LIdeaTitleInput := NewNyxInput('idea-title');
    LDetailsTab.Add(LIdeaTitleInput);
    LIdeaTitleInput.Configure
      .Text('Project title')
      .Placeholder('A brilliant little project')
      .Done;

    LReviewTab := NewNyxTab('review');
    LIdeaSectionsTabs.Add(LReviewTab);
    LReviewTab.Configure
      .Layout(nlColumn)
      .Gap(12)
      .Padding(20)
      .Text('Review')
      .Done;

    LReviewHeading := NewNyxHeading('review-heading');
    LReviewTab.Add(LReviewHeading);
    LReviewHeading.Configure
      .Text('Keep making things')
      .Done;

    LReviewCopyLabel := NewNyxLabel('review-copy');
    LReviewTab.Add(LReviewCopyLabel);
    LReviewCopyLabel.Configure
      .Text('Your notes and caret stay with their page. Return to Plan whenever you need.')
      .Done;
  except
    Result.Free;
    raise;
  end;
end;
// </nyx:views>

end.
