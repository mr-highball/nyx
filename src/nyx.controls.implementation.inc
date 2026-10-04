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


{ Maintained by tools/nyx_control_contracts.lpr; edit the contract source, then regenerate. }
constructor TNyxPage.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkPage, AID, AConstruction);
end;

function TNyxPage.WithText(const AText: TNyxText): INyxPage;
begin
  Text := AText;
  Result := Self as INyxPage;
end;

function NewNyxPage(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxPage;
begin
  Result := TNyxPage.Create(AID, AConstruction);
end;

constructor TNyxColumn.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkColumn, AID, AConstruction);
end;

function TNyxColumn.WithText(const AText: TNyxText): INyxColumn;
begin
  Text := AText;
  Result := Self as INyxColumn;
end;

function NewNyxColumn(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxColumn;
begin
  Result := TNyxColumn.Create(AID, AConstruction);
end;

constructor TNyxRow.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkRow, AID, AConstruction);
end;

function TNyxRow.WithText(const AText: TNyxText): INyxRow;
begin
  Text := AText;
  Result := Self as INyxRow;
end;

function NewNyxRow(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxRow;
begin
  Result := TNyxRow.Create(AID, AConstruction);
end;

constructor TNyxGrid.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkGrid, AID, AConstruction);
end;

function TNyxGrid.WithText(const AText: TNyxText): INyxGrid;
begin
  Text := AText;
  Result := Self as INyxGrid;
end;

function NewNyxGrid(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxGrid;
begin
  Result := TNyxGrid.Create(AID, AConstruction);
end;

constructor TNyxPanel.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkPanel, AID, AConstruction);
end;

function TNyxPanel.WithText(const AText: TNyxText): INyxPanel;
begin
  Text := AText;
  Result := Self as INyxPanel;
end;

function NewNyxPanel(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxPanel;
begin
  Result := TNyxPanel.Create(AID, AConstruction);
end;

constructor TNyxCard.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkCard, AID, AConstruction);
end;

function TNyxCard.WithText(const AText: TNyxText): INyxCard;
begin
  Text := AText;
  Result := Self as INyxCard;
end;

function NewNyxCard(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxCard;
begin
  Result := TNyxCard.Create(AID, AConstruction);
end;

constructor TNyxGroup.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkGroup, AID, AConstruction);
end;

function TNyxGroup.WithText(const AText: TNyxText): INyxGroup;
begin
  Text := AText;
  Result := Self as INyxGroup;
end;

function NewNyxGroup(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxGroup;
begin
  Result := TNyxGroup.Create(AID, AConstruction);
end;

constructor TNyxToolbar.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkToolbar, AID, AConstruction);
end;

function TNyxToolbar.WithText(const AText: TNyxText): INyxToolbar;
begin
  Text := AText;
  Result := Self as INyxToolbar;
end;

function NewNyxToolbar(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxToolbar;
begin
  Result := TNyxToolbar.Create(AID, AConstruction);
end;

constructor TNyxScroll.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkScroll, AID, AConstruction);
end;

function TNyxScroll.WithText(const AText: TNyxText): INyxScroll;
begin
  Text := AText;
  Result := Self as INyxScroll;
end;

function NewNyxScroll(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxScroll;
begin
  Result := TNyxScroll.Create(AID, AConstruction);
end;

constructor TNyxTabs.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkTabs, AID, AConstruction);
end;

function TNyxTabs.WithText(const AText: TNyxText): INyxTabs;
begin
  Text := AText;
  Result := Self as INyxTabs;
end;

function NewNyxTabs(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxTabs;
begin
  Result := TNyxTabs.Create(AID, AConstruction);
end;

constructor TNyxTab.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkTab, AID, AConstruction);
end;

function TNyxTab.WithText(const AText: TNyxText): INyxTab;
begin
  Text := AText;
  Result := Self as INyxTab;
end;

function NewNyxTab(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxTab;
begin
  Result := TNyxTab.Create(AID, AConstruction);
end;

constructor TNyxHeading.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkHeading, AID, AConstruction);
end;

function TNyxHeading.WithText(const AText: TNyxText): INyxHeading;
begin
  Text := AText;
  Result := Self as INyxHeading;
end;

function NewNyxHeading(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxHeading;
begin
  Result := TNyxHeading.Create(AID, AConstruction);
end;

constructor TNyxLabel.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkLabel, AID, AConstruction);
end;

function TNyxLabel.WithText(const AText: TNyxText): INyxLabel;
begin
  Text := AText;
  Result := Self as INyxLabel;
end;

function NewNyxLabel(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxLabel;
begin
  Result := TNyxLabel.Create(AID, AConstruction);
end;

constructor TNyxButton.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkButton, AID, AConstruction);
end;

function TNyxButton.WithText(const AText: TNyxText): INyxButton;
begin
  Text := AText;
  Result := Self as INyxButton;
end;

function NewNyxButton(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxButton;
begin
  Result := TNyxButton.Create(AID, AConstruction);
end;

constructor TNyxLink.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkLink, AID, AConstruction);
end;

function TNyxLink.WithText(const AText: TNyxText): INyxLink;
begin
  Text := AText;
  Result := Self as INyxLink;
end;

function NewNyxLink(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxLink;
begin
  Result := TNyxLink.Create(AID, AConstruction);
end;

constructor TNyxInput.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkInput, AID, AConstruction);
end;

function TNyxInput.WithText(const AText: TNyxText): INyxInput;
begin
  Text := AText;
  Result := Self as INyxInput;
end;

function NewNyxInput(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxInput;
begin
  Result := TNyxInput.Create(AID, AConstruction);
end;

constructor TNyxMemo.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkMemo, AID, AConstruction);
end;

function TNyxMemo.WithText(const AText: TNyxText): INyxMemo;
begin
  Text := AText;
  Result := Self as INyxMemo;
end;

function NewNyxMemo(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxMemo;
begin
  Result := TNyxMemo.Create(AID, AConstruction);
end;

constructor TNyxCheckbox.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkCheckbox, AID, AConstruction);
end;

function TNyxCheckbox.WithText(const AText: TNyxText): INyxCheckbox;
begin
  Text := AText;
  Result := Self as INyxCheckbox;
end;

function NewNyxCheckbox(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxCheckbox;
begin
  Result := TNyxCheckbox.Create(AID, AConstruction);
end;

constructor TNyxSwitch.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSwitch, AID, AConstruction);
end;

function TNyxSwitch.WithText(const AText: TNyxText): INyxSwitch;
begin
  Text := AText;
  Result := Self as INyxSwitch;
end;

function NewNyxSwitch(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSwitch;
begin
  Result := TNyxSwitch.Create(AID, AConstruction);
end;

constructor TNyxRadio.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkRadio, AID, AConstruction);
end;

function TNyxRadio.WithText(const AText: TNyxText): INyxRadio;
begin
  Text := AText;
  Result := Self as INyxRadio;
end;

function NewNyxRadio(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxRadio;
begin
  Result := TNyxRadio.Create(AID, AConstruction);
end;

constructor TNyxSelect.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSelect, AID, AConstruction);
end;

function TNyxSelect.WithText(const AText: TNyxText): INyxSelect;
begin
  Text := AText;
  Result := Self as INyxSelect;
end;

function NewNyxSelect(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSelect;
begin
  Result := TNyxSelect.Create(AID, AConstruction);
end;

constructor TNyxSpin.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSpin, AID, AConstruction);
end;

function TNyxSpin.WithText(const AText: TNyxText): INyxSpin;
begin
  Text := AText;
  Result := Self as INyxSpin;
end;

function NewNyxSpin(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSpin;
begin
  Result := TNyxSpin.Create(AID, AConstruction);
end;

constructor TNyxSlider.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSlider, AID, AConstruction);
end;

function TNyxSlider.WithText(const AText: TNyxText): INyxSlider;
begin
  Text := AText;
  Result := Self as INyxSlider;
end;

function NewNyxSlider(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSlider;
begin
  Result := TNyxSlider.Create(AID, AConstruction);
end;

constructor TNyxDate.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkDate, AID, AConstruction);
end;

function TNyxDate.WithText(const AText: TNyxText): INyxDate;
begin
  Text := AText;
  Result := Self as INyxDate;
end;

function NewNyxDate(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxDate;
begin
  Result := TNyxDate.Create(AID, AConstruction);
end;

constructor TNyxTime.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkTime, AID, AConstruction);
end;

function TNyxTime.WithText(const AText: TNyxText): INyxTime;
begin
  Text := AText;
  Result := Self as INyxTime;
end;

function NewNyxTime(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxTime;
begin
  Result := TNyxTime.Create(AID, AConstruction);
end;

constructor TNyxColor.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkColor, AID, AConstruction);
end;

function TNyxColor.WithText(const AText: TNyxText): INyxColor;
begin
  Text := AText;
  Result := Self as INyxColor;
end;

function NewNyxColor(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxColor;
begin
  Result := TNyxColor.Create(AID, AConstruction);
end;

constructor TNyxList.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkList, AID, AConstruction);
end;

function TNyxList.WithText(const AText: TNyxText): INyxList;
begin
  Text := AText;
  Result := Self as INyxList;
end;

function NewNyxList(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxList;
begin
  Result := TNyxList.Create(AID, AConstruction);
end;

constructor TNyxTable.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkTable, AID, AConstruction);
end;

function TNyxTable.WithText(const AText: TNyxText): INyxTable;
begin
  Text := AText;
  Result := Self as INyxTable;
end;

function NewNyxTable(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxTable;
begin
  Result := TNyxTable.Create(AID, AConstruction);
end;

constructor TNyxTree.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkTree, AID, AConstruction);
end;

function TNyxTree.WithText(const AText: TNyxText): INyxTree;
begin
  Text := AText;
  Result := Self as INyxTree;
end;

function NewNyxTree(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxTree;
begin
  Result := TNyxTree.Create(AID, AConstruction);
end;

constructor TNyxImage.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkImage, AID, AConstruction);
end;

function TNyxImage.WithText(const AText: TNyxText): INyxImage;
begin
  Text := AText;
  Result := Self as INyxImage;
end;

function NewNyxImage(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxImage;
begin
  Result := TNyxImage.Create(AID, AConstruction);
end;

constructor TNyxAvatar.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkAvatar, AID, AConstruction);
end;

function TNyxAvatar.WithText(const AText: TNyxText): INyxAvatar;
begin
  Text := AText;
  Result := Self as INyxAvatar;
end;

function NewNyxAvatar(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxAvatar;
begin
  Result := TNyxAvatar.Create(AID, AConstruction);
end;

constructor TNyxProgress.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkProgress, AID, AConstruction);
end;

function TNyxProgress.WithText(const AText: TNyxText): INyxProgress;
begin
  Text := AText;
  Result := Self as INyxProgress;
end;

function NewNyxProgress(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxProgress;
begin
  Result := TNyxProgress.Create(AID, AConstruction);
end;

constructor TNyxBadge.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkBadge, AID, AConstruction);
end;

function TNyxBadge.WithText(const AText: TNyxText): INyxBadge;
begin
  Text := AText;
  Result := Self as INyxBadge;
end;

function NewNyxBadge(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxBadge;
begin
  Result := TNyxBadge.Create(AID, AConstruction);
end;

constructor TNyxAlert.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkAlert, AID, AConstruction);
end;

function TNyxAlert.WithText(const AText: TNyxText): INyxAlert;
begin
  Text := AText;
  Result := Self as INyxAlert;
end;

function NewNyxAlert(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxAlert;
begin
  Result := TNyxAlert.Create(AID, AConstruction);
end;

constructor TNyxSeparator.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSeparator, AID, AConstruction);
end;

function TNyxSeparator.WithText(const AText: TNyxText): INyxSeparator;
begin
  Text := AText;
  Result := Self as INyxSeparator;
end;

function NewNyxSeparator(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSeparator;
begin
  Result := TNyxSeparator.Create(AID, AConstruction);
end;

constructor TNyxSpacer.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSpacer, AID, AConstruction);
end;

function TNyxSpacer.WithText(const AText: TNyxText): INyxSpacer;
begin
  Text := AText;
  Result := Self as INyxSpacer;
end;

function NewNyxSpacer(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSpacer;
begin
  Result := TNyxSpacer.Create(AID, AConstruction);
end;

constructor TNyxCode.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkCode, AID, AConstruction);
end;

function TNyxCode.WithText(const AText: TNyxText): INyxCode;
begin
  Text := AText;
  Result := Self as INyxCode;
end;

function NewNyxCode(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxCode;
begin
  Result := TNyxCode.Create(AID, AConstruction);
end;

constructor TNyxCodeEditor.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkCodeEditor, AID, AConstruction);
end;

function TNyxCodeEditor.WithText(const AText: TNyxText): INyxCodeEditor;
begin
  Text := AText;
  Result := Self as INyxCodeEditor;
end;

function NewNyxCodeEditor(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxCodeEditor;
begin
  Result := TNyxCodeEditor.Create(AID, AConstruction);
end;

constructor TNyxDesignSurface.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkDesignSurface, AID, AConstruction);
end;

function TNyxDesignSurface.WithText(const AText: TNyxText): INyxDesignSurface;
begin
  Text := AText;
  Result := Self as INyxDesignSurface;
end;

function NewNyxDesignSurface(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxDesignSurface;
begin
  Result := TNyxDesignSurface.Create(AID, AConstruction);
end;

constructor TNyxComponent.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkComponent, AID, AConstruction);
end;

function TNyxComponent.WithText(const AText: TNyxText): INyxComponent;
begin
  Text := AText;
  Result := Self as INyxComponent;
end;

function NewNyxComponent(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxComponent;
begin
  Result := TNyxComponent.Create(AID, AConstruction);
end;

constructor TNyxSplitView.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSplitView, AID, AConstruction);
end;

function TNyxSplitView.WithText(const AText: TNyxText): INyxSplitView;
begin
  Text := AText;
  Result := Self as INyxSplitView;
end;

function NewNyxSplitView(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSplitView;
begin
  Result := TNyxSplitView.Create(AID, AConstruction);
end;

constructor TNyxLabeledButton.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkLabeledButton, AID, AConstruction);
end;

function TNyxLabeledButton.WithText(const AText: TNyxText): INyxLabeledButton;
begin
  Text := AText;
  Result := Self as INyxLabeledButton;
end;

function NewNyxLabeledButton(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxLabeledButton;
begin
  Result := TNyxLabeledButton.Create(AID, AConstruction);
end;

function TNyxLabeledButton.GetCaptionLabel: INyxLabel;
begin
  Result := Part(NyxPart('label')) as INyxLabel;
end;

function TNyxLabeledButton.GetButton: INyxButton;
begin
  Result := Part(NyxPart('button')) as INyxButton;
end;

constructor TNyxSplitButton.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSplitButton, AID, AConstruction);
end;

function TNyxSplitButton.WithText(const AText: TNyxText): INyxSplitButton;
begin
  Text := AText;
  Result := Self as INyxSplitButton;
end;

function NewNyxSplitButton(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSplitButton;
begin
  Result := TNyxSplitButton.Create(AID, AConstruction);
end;

function TNyxSplitButton.GetPrimaryButton: INyxButton;
begin
  Result := Part(NyxPart('primary')) as INyxButton;
end;

function TNyxSplitButton.GetMenuButton: INyxButton;
begin
  Result := Part(NyxPart('menu')) as INyxButton;
end;

constructor TNyxSearchField.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSearchField, AID, AConstruction);
end;

function TNyxSearchField.WithText(const AText: TNyxText): INyxSearchField;
begin
  Text := AText;
  Result := Self as INyxSearchField;
end;

function NewNyxSearchField(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSearchField;
begin
  Result := TNyxSearchField.Create(AID, AConstruction);
end;

function TNyxSearchField.GetQueryInput: INyxInput;
begin
  Result := Part(NyxPart('query')) as INyxInput;
end;

function TNyxSearchField.GetSearchButton: INyxButton;
begin
  Result := Part(NyxPart('search')) as INyxButton;
end;

function TNyxSearchField.GetClearButton: INyxButton;
begin
  Result := Part(NyxPart('clear')) as INyxButton;
end;

constructor TNyxFormField.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkFormField, AID, AConstruction);
end;

function TNyxFormField.WithText(const AText: TNyxText): INyxFormField;
begin
  Text := AText;
  Result := Self as INyxFormField;
end;

function NewNyxFormField(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxFormField;
begin
  Result := TNyxFormField.Create(AID, AConstruction);
end;

function TNyxFormField.GetInput: INyxInput;
begin
  Result := Part(NyxPart('input')) as INyxInput;
end;

function TNyxFormField.GetHelpLabel: INyxLabel;
begin
  Result := Part(NyxPart('help')) as INyxLabel;
end;

function TNyxFormField.GetErrorAlert: INyxAlert;
begin
  Result := Part(NyxPart('error')) as INyxAlert;
end;

constructor TNyxLoginForm.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkLoginForm, AID, AConstruction);
end;

function TNyxLoginForm.WithText(const AText: TNyxText): INyxLoginForm;
begin
  Text := AText;
  Result := Self as INyxLoginForm;
end;

function NewNyxLoginForm(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxLoginForm;
begin
  Result := TNyxLoginForm.Create(AID, AConstruction);
end;

function TNyxLoginForm.GetTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('title')) as INyxHeading;
end;

function TNyxLoginForm.GetEmailInput: INyxInput;
begin
  Result := Part(NyxPart('email')) as INyxInput;
end;

function TNyxLoginForm.GetPasswordInput: INyxInput;
begin
  Result := Part(NyxPart('password')) as INyxInput;
end;

function TNyxLoginForm.GetRememberCheckbox: INyxCheckbox;
begin
  Result := Part(NyxPart('remember')) as INyxCheckbox;
end;

function TNyxLoginForm.GetSubmitButton: INyxButton;
begin
  Result := Part(NyxPart('submit')) as INyxButton;
end;

constructor TNyxSettingsPanel.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSettingsPanel, AID, AConstruction);
end;

function TNyxSettingsPanel.WithText(const AText: TNyxText): INyxSettingsPanel;
begin
  Text := AText;
  Result := Self as INyxSettingsPanel;
end;

function NewNyxSettingsPanel(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSettingsPanel;
begin
  Result := TNyxSettingsPanel.Create(AID, AConstruction);
end;

function TNyxSettingsPanel.GetTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('title')) as INyxHeading;
end;

function TNyxSettingsPanel.GetNotificationsSwitch: INyxSwitch;
begin
  Result := Part(NyxPart('notifications')) as INyxSwitch;
end;

function TNyxSettingsPanel.GetSyncSwitch: INyxSwitch;
begin
  Result := Part(NyxPart('sync')) as INyxSwitch;
end;

function TNyxSettingsPanel.GetLanguageSelect: INyxSelect;
begin
  Result := Part(NyxPart('language')) as INyxSelect;
end;

function TNyxSettingsPanel.GetSaveButton: INyxButton;
begin
  Result := Part(NyxPart('save')) as INyxButton;
end;

constructor TNyxDateRange.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkDateRange, AID, AConstruction);
end;

function TNyxDateRange.WithText(const AText: TNyxText): INyxDateRange;
begin
  Text := AText;
  Result := Self as INyxDateRange;
end;

function NewNyxDateRange(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxDateRange;
begin
  Result := TNyxDateRange.Create(AID, AConstruction);
end;

function TNyxDateRange.GetStartDate: INyxDate;
begin
  Result := Part(NyxPart('start')) as INyxDate;
end;

function TNyxDateRange.GetSeparatorLabel: INyxLabel;
begin
  Result := Part(NyxPart('separator')) as INyxLabel;
end;

function TNyxDateRange.GetFinishDate: INyxDate;
begin
  Result := Part(NyxPart('finish')) as INyxDate;
end;

constructor TNyxNumberStepper.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkNumberStepper, AID, AConstruction);
end;

function TNyxNumberStepper.WithText(const AText: TNyxText): INyxNumberStepper;
begin
  Text := AText;
  Result := Self as INyxNumberStepper;
end;

function NewNyxNumberStepper(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxNumberStepper;
begin
  Result := TNyxNumberStepper.Create(AID, AConstruction);
end;

function TNyxNumberStepper.GetDecrementButton: INyxButton;
begin
  Result := Part(NyxPart('decrement')) as INyxButton;
end;

function TNyxNumberStepper.GetValueSpin: INyxSpin;
begin
  Result := Part(NyxPart('value')) as INyxSpin;
end;

function TNyxNumberStepper.GetIncrementButton: INyxButton;
begin
  Result := Part(NyxPart('increment')) as INyxButton;
end;

constructor TNyxSegmentedControl.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSegmentedControl, AID, AConstruction);
end;

function TNyxSegmentedControl.WithText(const AText: TNyxText): INyxSegmentedControl;
begin
  Text := AText;
  Result := Self as INyxSegmentedControl;
end;

function NewNyxSegmentedControl(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSegmentedControl;
begin
  Result := TNyxSegmentedControl.Create(AID, AConstruction);
end;

function TNyxSegmentedControl.GetDayButton: INyxButton;
begin
  Result := Part(NyxPart('day')) as INyxButton;
end;

function TNyxSegmentedControl.GetWeekButton: INyxButton;
begin
  Result := Part(NyxPart('week')) as INyxButton;
end;

function TNyxSegmentedControl.GetMonthButton: INyxButton;
begin
  Result := Part(NyxPart('month')) as INyxButton;
end;

constructor TNyxRating.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkRating, AID, AConstruction);
end;

function TNyxRating.WithText(const AText: TNyxText): INyxRating;
begin
  Text := AText;
  Result := Self as INyxRating;
end;

function NewNyxRating(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxRating;
begin
  Result := TNyxRating.Create(AID, AConstruction);
end;

function TNyxRating.GetStar1Button: INyxButton;
begin
  Result := Part(NyxPart('star-1')) as INyxButton;
end;

function TNyxRating.GetStar2Button: INyxButton;
begin
  Result := Part(NyxPart('star-2')) as INyxButton;
end;

function TNyxRating.GetStar3Button: INyxButton;
begin
  Result := Part(NyxPart('star-3')) as INyxButton;
end;

function TNyxRating.GetStar4Button: INyxButton;
begin
  Result := Part(NyxPart('star-4')) as INyxButton;
end;

function TNyxRating.GetStar5Button: INyxButton;
begin
  Result := Part(NyxPart('star-5')) as INyxButton;
end;

constructor TNyxDataToolbar.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkDataToolbar, AID, AConstruction);
end;

function TNyxDataToolbar.WithText(const AText: TNyxText): INyxDataToolbar;
begin
  Text := AText;
  Result := Self as INyxDataToolbar;
end;

function NewNyxDataToolbar(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxDataToolbar;
begin
  Result := TNyxDataToolbar.Create(AID, AConstruction);
end;

function TNyxDataToolbar.GetSearchInput: INyxInput;
begin
  Result := Part(NyxPart('search')) as INyxInput;
end;

function TNyxDataToolbar.GetSortSelect: INyxSelect;
begin
  Result := Part(NyxPart('sort')) as INyxSelect;
end;

function TNyxDataToolbar.GetCreateButton: INyxButton;
begin
  Result := Part(NyxPart('create')) as INyxButton;
end;

function TNyxDataToolbar.GetRefreshButton: INyxButton;
begin
  Result := Part(NyxPart('refresh')) as INyxButton;
end;

constructor TNyxFilterBar.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkFilterBar, AID, AConstruction);
end;

function TNyxFilterBar.WithText(const AText: TNyxText): INyxFilterBar;
begin
  Text := AText;
  Result := Self as INyxFilterBar;
end;

function NewNyxFilterBar(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxFilterBar;
begin
  Result := TNyxFilterBar.Create(AID, AConstruction);
end;

function TNyxFilterBar.GetStatusSelect: INyxSelect;
begin
  Result := Part(NyxPart('status')) as INyxSelect;
end;

function TNyxFilterBar.GetQueryInput: INyxInput;
begin
  Result := Part(NyxPart('query')) as INyxInput;
end;

function TNyxFilterBar.GetApplyButton: INyxButton;
begin
  Result := Part(NyxPart('apply')) as INyxButton;
end;

function TNyxFilterBar.GetResetButton: INyxButton;
begin
  Result := Part(NyxPart('reset')) as INyxButton;
end;

constructor TNyxPagination.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkPagination, AID, AConstruction);
end;

function TNyxPagination.WithText(const AText: TNyxText): INyxPagination;
begin
  Text := AText;
  Result := Self as INyxPagination;
end;

function NewNyxPagination(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxPagination;
begin
  Result := TNyxPagination.Create(AID, AConstruction);
end;

function TNyxPagination.GetPreviousButton: INyxButton;
begin
  Result := Part(NyxPart('previous')) as INyxButton;
end;

function TNyxPagination.GetPageSpin: INyxSpin;
begin
  Result := Part(NyxPart('page')) as INyxSpin;
end;

function TNyxPagination.GetTotalLabel: INyxLabel;
begin
  Result := Part(NyxPart('total')) as INyxLabel;
end;

function TNyxPagination.GetNextButton: INyxButton;
begin
  Result := Part(NyxPart('next')) as INyxButton;
end;

constructor TNyxBreadcrumbs.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkBreadcrumbs, AID, AConstruction);
end;

function TNyxBreadcrumbs.WithText(const AText: TNyxText): INyxBreadcrumbs;
begin
  Text := AText;
  Result := Self as INyxBreadcrumbs;
end;

function NewNyxBreadcrumbs(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxBreadcrumbs;
begin
  Result := TNyxBreadcrumbs.Create(AID, AConstruction);
end;

function TNyxBreadcrumbs.GetHomeButton: INyxButton;
begin
  Result := Part(NyxPart('home')) as INyxButton;
end;

function TNyxBreadcrumbs.GetSeparatorLabel: INyxLabel;
begin
  Result := Part(NyxPart('separator')) as INyxLabel;
end;

function TNyxBreadcrumbs.GetSectionButton: INyxButton;
begin
  Result := Part(NyxPart('section')) as INyxButton;
end;

function TNyxBreadcrumbs.GetCurrentLabel: INyxLabel;
begin
  Result := Part(NyxPart('current')) as INyxLabel;
end;

constructor TNyxSidebarNav.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSidebarNav, AID, AConstruction);
end;

function TNyxSidebarNav.WithText(const AText: TNyxText): INyxSidebarNav;
begin
  Text := AText;
  Result := Self as INyxSidebarNav;
end;

function NewNyxSidebarNav(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSidebarNav;
begin
  Result := TNyxSidebarNav.Create(AID, AConstruction);
end;

function TNyxSidebarNav.GetTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('title')) as INyxHeading;
end;

function TNyxSidebarNav.GetOverviewButton: INyxButton;
begin
  Result := Part(NyxPart('overview')) as INyxButton;
end;

function TNyxSidebarNav.GetProjectsButton: INyxButton;
begin
  Result := Part(NyxPart('projects')) as INyxButton;
end;

function TNyxSidebarNav.GetSettingsButton: INyxButton;
begin
  Result := Part(NyxPart('settings')) as INyxButton;
end;

constructor TNyxMasterDetail.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkMasterDetail, AID, AConstruction);
end;

function TNyxMasterDetail.WithText(const AText: TNyxText): INyxMasterDetail;
begin
  Text := AText;
  Result := Self as INyxMasterDetail;
end;

function NewNyxMasterDetail(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxMasterDetail;
begin
  Result := TNyxMasterDetail.Create(AID, AConstruction);
end;

function TNyxMasterDetail.GetMasterList: INyxList;
begin
  Result := Part(NyxPart('master')) as INyxList;
end;

function TNyxMasterDetail.GetDetailColumn: INyxColumn;
begin
  Result := Part(NyxPart('detail')) as INyxColumn;
end;

function TNyxMasterDetail.GetDetailTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('detail/title')) as INyxHeading;
end;

function TNyxMasterDetail.GetDetailDescriptionLabel: INyxLabel;
begin
  Result := Part(NyxPart('detail/description')) as INyxLabel;
end;

function TNyxMasterDetail.GetDetailEditButton: INyxButton;
begin
  Result := Part(NyxPart('detail/edit')) as INyxButton;
end;

constructor TNyxListCard.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkListCard, AID, AConstruction);
end;

function TNyxListCard.WithText(const AText: TNyxText): INyxListCard;
begin
  Text := AText;
  Result := Self as INyxListCard;
end;

function NewNyxListCard(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxListCard;
begin
  Result := TNyxListCard.Create(AID, AConstruction);
end;

function TNyxListCard.GetTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('title')) as INyxHeading;
end;

function TNyxListCard.GetList: INyxList;
begin
  Result := Part(NyxPart('list')) as INyxList;
end;

function TNyxListCard.GetActionsRow: INyxRow;
begin
  Result := Part(NyxPart('actions')) as INyxRow;
end;

function TNyxListCard.GetActionsPrimaryButton: INyxButton;
begin
  Result := Part(NyxPart('actions/primary')) as INyxButton;
end;

constructor TNyxDataCard.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkDataCard, AID, AConstruction);
end;

function TNyxDataCard.WithText(const AText: TNyxText): INyxDataCard;
begin
  Text := AText;
  Result := Self as INyxDataCard;
end;

function NewNyxDataCard(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxDataCard;
begin
  Result := TNyxDataCard.Create(AID, AConstruction);
end;

function TNyxDataCard.GetTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('title')) as INyxHeading;
end;

function TNyxDataCard.GetTable: INyxTable;
begin
  Result := Part(NyxPart('table')) as INyxTable;
end;

function TNyxDataCard.GetActionsRow: INyxRow;
begin
  Result := Part(NyxPart('actions')) as INyxRow;
end;

function TNyxDataCard.GetActionsExportButton: INyxButton;
begin
  Result := Part(NyxPart('actions/export')) as INyxButton;
end;

constructor TNyxKanbanBoard.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkKanbanBoard, AID, AConstruction);
end;

function TNyxKanbanBoard.WithText(const AText: TNyxText): INyxKanbanBoard;
begin
  Text := AText;
  Result := Self as INyxKanbanBoard;
end;

function NewNyxKanbanBoard(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxKanbanBoard;
begin
  Result := TNyxKanbanBoard.Create(AID, AConstruction);
end;

function TNyxKanbanBoard.GetLane0Column: INyxColumn;
begin
  Result := Part(NyxPart('lane-0')) as INyxColumn;
end;

function TNyxKanbanBoard.GetLane0TitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('lane-0/title')) as INyxHeading;
end;

function TNyxKanbanBoard.GetLane0ItemsList: INyxList;
begin
  Result := Part(NyxPart('lane-0/items')) as INyxList;
end;

function TNyxKanbanBoard.GetLane0AddButton: INyxButton;
begin
  Result := Part(NyxPart('lane-0/add')) as INyxButton;
end;

function TNyxKanbanBoard.GetLane1Column: INyxColumn;
begin
  Result := Part(NyxPart('lane-1')) as INyxColumn;
end;

function TNyxKanbanBoard.GetLane1TitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('lane-1/title')) as INyxHeading;
end;

function TNyxKanbanBoard.GetLane1ItemsList: INyxList;
begin
  Result := Part(NyxPart('lane-1/items')) as INyxList;
end;

function TNyxKanbanBoard.GetLane1AddButton: INyxButton;
begin
  Result := Part(NyxPart('lane-1/add')) as INyxButton;
end;

function TNyxKanbanBoard.GetLane2Column: INyxColumn;
begin
  Result := Part(NyxPart('lane-2')) as INyxColumn;
end;

function TNyxKanbanBoard.GetLane2TitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('lane-2/title')) as INyxHeading;
end;

function TNyxKanbanBoard.GetLane2ItemsList: INyxList;
begin
  Result := Part(NyxPart('lane-2/items')) as INyxList;
end;

function TNyxKanbanBoard.GetLane2AddButton: INyxButton;
begin
  Result := Part(NyxPart('lane-2/add')) as INyxButton;
end;

constructor TNyxTimeline.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkTimeline, AID, AConstruction);
end;

function TNyxTimeline.WithText(const AText: TNyxText): INyxTimeline;
begin
  Text := AText;
  Result := Self as INyxTimeline;
end;

function NewNyxTimeline(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxTimeline;
begin
  Result := TNyxTimeline.Create(AID, AConstruction);
end;

function TNyxTimeline.GetEntry1Row: INyxRow;
begin
  Result := Part(NyxPart('entry-1')) as INyxRow;
end;

function TNyxTimeline.GetEntry1TimeBadge: INyxBadge;
begin
  Result := Part(NyxPart('entry-1/time')) as INyxBadge;
end;

function TNyxTimeline.GetEntry1ContentColumn: INyxColumn;
begin
  Result := Part(NyxPart('entry-1/content')) as INyxColumn;
end;

function TNyxTimeline.GetEntry1ContentTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('entry-1/content/title')) as INyxHeading;
end;

function TNyxTimeline.GetEntry1ContentDescriptionLabel: INyxLabel;
begin
  Result := Part(NyxPart('entry-1/content/description')) as INyxLabel;
end;

function TNyxTimeline.GetEntry2Row: INyxRow;
begin
  Result := Part(NyxPart('entry-2')) as INyxRow;
end;

function TNyxTimeline.GetEntry2TimeBadge: INyxBadge;
begin
  Result := Part(NyxPart('entry-2/time')) as INyxBadge;
end;

function TNyxTimeline.GetEntry2ContentColumn: INyxColumn;
begin
  Result := Part(NyxPart('entry-2/content')) as INyxColumn;
end;

function TNyxTimeline.GetEntry2ContentTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('entry-2/content/title')) as INyxHeading;
end;

function TNyxTimeline.GetEntry2ContentDescriptionLabel: INyxLabel;
begin
  Result := Part(NyxPart('entry-2/content/description')) as INyxLabel;
end;

function TNyxTimeline.GetEntry3Row: INyxRow;
begin
  Result := Part(NyxPart('entry-3')) as INyxRow;
end;

function TNyxTimeline.GetEntry3TimeBadge: INyxBadge;
begin
  Result := Part(NyxPart('entry-3/time')) as INyxBadge;
end;

function TNyxTimeline.GetEntry3ContentColumn: INyxColumn;
begin
  Result := Part(NyxPart('entry-3/content')) as INyxColumn;
end;

function TNyxTimeline.GetEntry3ContentTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('entry-3/content/title')) as INyxHeading;
end;

function TNyxTimeline.GetEntry3ContentDescriptionLabel: INyxLabel;
begin
  Result := Part(NyxPart('entry-3/content/description')) as INyxLabel;
end;

constructor TNyxStatCard.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkStatCard, AID, AConstruction);
end;

function TNyxStatCard.WithText(const AText: TNyxText): INyxStatCard;
begin
  Text := AText;
  Result := Self as INyxStatCard;
end;

function NewNyxStatCard(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxStatCard;
begin
  Result := TNyxStatCard.Create(AID, AConstruction);
end;

function TNyxStatCard.GetCaptionLabel: INyxLabel;
begin
  Result := Part(NyxPart('label')) as INyxLabel;
end;

function TNyxStatCard.GetValueHeading: INyxHeading;
begin
  Result := Part(NyxPart('value')) as INyxHeading;
end;

function TNyxStatCard.GetTrendBadge: INyxBadge;
begin
  Result := Part(NyxPart('trend')) as INyxBadge;
end;

constructor TNyxMetricGrid.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkMetricGrid, AID, AConstruction);
end;

function TNyxMetricGrid.WithText(const AText: TNyxText): INyxMetricGrid;
begin
  Text := AText;
  Result := Self as INyxMetricGrid;
end;

function NewNyxMetricGrid(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxMetricGrid;
begin
  Result := TNyxMetricGrid.Create(AID, AConstruction);
end;

function TNyxMetricGrid.GetMetric1Column: INyxColumn;
begin
  Result := Part(NyxPart('metric-1')) as INyxColumn;
end;

function TNyxMetricGrid.GetMetric1Label: INyxLabel;
begin
  Result := Part(NyxPart('metric-1/label')) as INyxLabel;
end;

function TNyxMetricGrid.GetMetric1ValueHeading: INyxHeading;
begin
  Result := Part(NyxPart('metric-1/value')) as INyxHeading;
end;

function TNyxMetricGrid.GetMetric1Progress: INyxProgress;
begin
  Result := Part(NyxPart('metric-1/progress')) as INyxProgress;
end;

function TNyxMetricGrid.GetMetric2Column: INyxColumn;
begin
  Result := Part(NyxPart('metric-2')) as INyxColumn;
end;

function TNyxMetricGrid.GetMetric2Label: INyxLabel;
begin
  Result := Part(NyxPart('metric-2/label')) as INyxLabel;
end;

function TNyxMetricGrid.GetMetric2ValueHeading: INyxHeading;
begin
  Result := Part(NyxPart('metric-2/value')) as INyxHeading;
end;

function TNyxMetricGrid.GetMetric2Progress: INyxProgress;
begin
  Result := Part(NyxPart('metric-2/progress')) as INyxProgress;
end;

function TNyxMetricGrid.GetMetric3Column: INyxColumn;
begin
  Result := Part(NyxPart('metric-3')) as INyxColumn;
end;

function TNyxMetricGrid.GetMetric3Label: INyxLabel;
begin
  Result := Part(NyxPart('metric-3/label')) as INyxLabel;
end;

function TNyxMetricGrid.GetMetric3ValueHeading: INyxHeading;
begin
  Result := Part(NyxPart('metric-3/value')) as INyxHeading;
end;

function TNyxMetricGrid.GetMetric3Progress: INyxProgress;
begin
  Result := Part(NyxPart('metric-3/progress')) as INyxProgress;
end;

constructor TNyxEmptyState.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkEmptyState, AID, AConstruction);
end;

function TNyxEmptyState.WithText(const AText: TNyxText): INyxEmptyState;
begin
  Text := AText;
  Result := Self as INyxEmptyState;
end;

function NewNyxEmptyState(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxEmptyState;
begin
  Result := TNyxEmptyState.Create(AID, AConstruction);
end;

function TNyxEmptyState.GetIllustrationAvatar: INyxAvatar;
begin
  Result := Part(NyxPart('illustration')) as INyxAvatar;
end;

function TNyxEmptyState.GetTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('title')) as INyxHeading;
end;

function TNyxEmptyState.GetDescriptionLabel: INyxLabel;
begin
  Result := Part(NyxPart('description')) as INyxLabel;
end;

function TNyxEmptyState.GetCreateButton: INyxButton;
begin
  Result := Part(NyxPart('create')) as INyxButton;
end;

constructor TNyxConfirmationDialog.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkConfirmationDialog, AID, AConstruction);
end;

function TNyxConfirmationDialog.WithText(const AText: TNyxText): INyxConfirmationDialog;
begin
  Text := AText;
  Result := Self as INyxConfirmationDialog;
end;

function NewNyxConfirmationDialog(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxConfirmationDialog;
begin
  Result := TNyxConfirmationDialog.Create(AID, AConstruction);
end;

function TNyxConfirmationDialog.GetTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('title')) as INyxHeading;
end;

function TNyxConfirmationDialog.GetDescriptionLabel: INyxLabel;
begin
  Result := Part(NyxPart('description')) as INyxLabel;
end;

function TNyxConfirmationDialog.GetActionsRow: INyxRow;
begin
  Result := Part(NyxPart('actions')) as INyxRow;
end;

function TNyxConfirmationDialog.GetActionsCancelButton: INyxButton;
begin
  Result := Part(NyxPart('actions/cancel')) as INyxButton;
end;

function TNyxConfirmationDialog.GetActionsConfirmButton: INyxButton;
begin
  Result := Part(NyxPart('actions/confirm')) as INyxButton;
end;

constructor TNyxNotificationCard.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkNotificationCard, AID, AConstruction);
end;

function TNyxNotificationCard.WithText(const AText: TNyxText): INyxNotificationCard;
begin
  Text := AText;
  Result := Self as INyxNotificationCard;
end;

function NewNyxNotificationCard(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxNotificationCard;
begin
  Result := TNyxNotificationCard.Create(AID, AConstruction);
end;

function TNyxNotificationCard.GetStatusBadge: INyxBadge;
begin
  Result := Part(NyxPart('status')) as INyxBadge;
end;

function TNyxNotificationCard.GetContentColumn: INyxColumn;
begin
  Result := Part(NyxPart('content')) as INyxColumn;
end;

function TNyxNotificationCard.GetContentTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('content/title')) as INyxHeading;
end;

function TNyxNotificationCard.GetContentMessageLabel: INyxLabel;
begin
  Result := Part(NyxPart('content/message')) as INyxLabel;
end;

function TNyxNotificationCard.GetDismissButton: INyxButton;
begin
  Result := Part(NyxPart('dismiss')) as INyxButton;
end;

constructor TNyxToast.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkToast, AID, AConstruction);
end;

function TNyxToast.WithText(const AText: TNyxText): INyxToast;
begin
  Text := AText;
  Result := Self as INyxToast;
end;

function NewNyxToast(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxToast;
begin
  Result := TNyxToast.Create(AID, AConstruction);
end;

function TNyxToast.GetMessageLabel: INyxLabel;
begin
  Result := Part(NyxPart('message')) as INyxLabel;
end;

function TNyxToast.GetUndoButton: INyxButton;
begin
  Result := Part(NyxPart('undo')) as INyxButton;
end;

function TNyxToast.GetDismissButton: INyxButton;
begin
  Result := Part(NyxPart('dismiss')) as INyxButton;
end;

constructor TNyxWizardStep.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkWizardStep, AID, AConstruction);
end;

function TNyxWizardStep.WithText(const AText: TNyxText): INyxWizardStep;
begin
  Text := AText;
  Result := Self as INyxWizardStep;
end;

function NewNyxWizardStep(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxWizardStep;
begin
  Result := TNyxWizardStep.Create(AID, AConstruction);
end;

function TNyxWizardStep.GetStepBadge: INyxBadge;
begin
  Result := Part(NyxPart('step')) as INyxBadge;
end;

function TNyxWizardStep.GetTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('title')) as INyxHeading;
end;

function TNyxWizardStep.GetContentColumn: INyxColumn;
begin
  Result := Part(NyxPart('content')) as INyxColumn;
end;

function TNyxWizardStep.GetContentNameInput: INyxInput;
begin
  Result := Part(NyxPart('content/name')) as INyxInput;
end;

function TNyxWizardStep.GetNavigationRow: INyxRow;
begin
  Result := Part(NyxPart('navigation')) as INyxRow;
end;

function TNyxWizardStep.GetNavigationBackButton: INyxButton;
begin
  Result := Part(NyxPart('navigation/back')) as INyxButton;
end;

function TNyxWizardStep.GetNavigationNextButton: INyxButton;
begin
  Result := Part(NyxPart('navigation/next')) as INyxButton;
end;

constructor TNyxStepper.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkStepper, AID, AConstruction);
end;

function TNyxStepper.WithText(const AText: TNyxText): INyxStepper;
begin
  Text := AText;
  Result := Self as INyxStepper;
end;

function NewNyxStepper(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxStepper;
begin
  Result := TNyxStepper.Create(AID, AConstruction);
end;

function TNyxStepper.GetStep1Button: INyxButton;
begin
  Result := Part(NyxPart('step-1')) as INyxButton;
end;

function TNyxStepper.GetStep2Button: INyxButton;
begin
  Result := Part(NyxPart('step-2')) as INyxButton;
end;

function TNyxStepper.GetStep3Button: INyxButton;
begin
  Result := Part(NyxPart('step-3')) as INyxButton;
end;

function TNyxStepper.GetStep4Button: INyxButton;
begin
  Result := Part(NyxPart('step-4')) as INyxButton;
end;

constructor TNyxProfileCard.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkProfileCard, AID, AConstruction);
end;

function TNyxProfileCard.WithText(const AText: TNyxText): INyxProfileCard;
begin
  Text := AText;
  Result := Self as INyxProfileCard;
end;

function NewNyxProfileCard(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxProfileCard;
begin
  Result := TNyxProfileCard.Create(AID, AConstruction);
end;

function TNyxProfileCard.GetAvatar: INyxAvatar;
begin
  Result := Part(NyxPart('avatar')) as INyxAvatar;
end;

function TNyxProfileCard.GetNameHeading: INyxHeading;
begin
  Result := Part(NyxPart('name')) as INyxHeading;
end;

function TNyxProfileCard.GetBioLabel: INyxLabel;
begin
  Result := Part(NyxPart('bio')) as INyxLabel;
end;

function TNyxProfileCard.GetActionsRow: INyxRow;
begin
  Result := Part(NyxPart('actions')) as INyxRow;
end;

function TNyxProfileCard.GetActionsMessageButton: INyxButton;
begin
  Result := Part(NyxPart('actions/message')) as INyxButton;
end;

function TNyxProfileCard.GetActionsFollowButton: INyxButton;
begin
  Result := Part(NyxPart('actions/follow')) as INyxButton;
end;

constructor TNyxMediaCard.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkMediaCard, AID, AConstruction);
end;

function TNyxMediaCard.WithText(const AText: TNyxText): INyxMediaCard;
begin
  Text := AText;
  Result := Self as INyxMediaCard;
end;

function NewNyxMediaCard(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxMediaCard;
begin
  Result := TNyxMediaCard.Create(AID, AConstruction);
end;

function TNyxMediaCard.GetMediaImage: INyxImage;
begin
  Result := Part(NyxPart('media')) as INyxImage;
end;

function TNyxMediaCard.GetTitleHeading: INyxHeading;
begin
  Result := Part(NyxPart('title')) as INyxHeading;
end;

function TNyxMediaCard.GetDescriptionLabel: INyxLabel;
begin
  Result := Part(NyxPart('description')) as INyxLabel;
end;

function TNyxMediaCard.GetActionsRow: INyxRow;
begin
  Result := Part(NyxPart('actions')) as INyxRow;
end;

function TNyxMediaCard.GetActionsOpenButton: INyxButton;
begin
  Result := Part(NyxPart('actions/open')) as INyxButton;
end;

function TNyxMediaCard.GetActionsFavoriteButton: INyxButton;
begin
  Result := Part(NyxPart('actions/favorite')) as INyxButton;
end;

constructor TNyxAvatarGroup.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkAvatarGroup, AID, AConstruction);
end;

function TNyxAvatarGroup.WithText(const AText: TNyxText): INyxAvatarGroup;
begin
  Text := AText;
  Result := Self as INyxAvatarGroup;
end;

function NewNyxAvatarGroup(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxAvatarGroup;
begin
  Result := TNyxAvatarGroup.Create(AID, AConstruction);
end;

function TNyxAvatarGroup.GetMember1Avatar: INyxAvatar;
begin
  Result := Part(NyxPart('member-1')) as INyxAvatar;
end;

function TNyxAvatarGroup.GetMember2Avatar: INyxAvatar;
begin
  Result := Part(NyxPart('member-2')) as INyxAvatar;
end;

function TNyxAvatarGroup.GetMember3Avatar: INyxAvatar;
begin
  Result := Part(NyxPart('member-3')) as INyxAvatar;
end;

function TNyxAvatarGroup.GetMember4Avatar: INyxAvatar;
begin
  Result := Part(NyxPart('member-4')) as INyxAvatar;
end;

function TNyxAvatarGroup.GetOverflowLabel: INyxLabel;
begin
  Result := Part(NyxPart('overflow')) as INyxLabel;
end;

constructor TNyxCommentThread.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkCommentThread, AID, AConstruction);
end;

function TNyxCommentThread.WithText(const AText: TNyxText): INyxCommentThread;
begin
  Text := AText;
  Result := Self as INyxCommentThread;
end;

function NewNyxCommentThread(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxCommentThread;
begin
  Result := TNyxCommentThread.Create(AID, AConstruction);
end;

function TNyxCommentThread.GetComment1Row: INyxRow;
begin
  Result := Part(NyxPart('comment-1')) as INyxRow;
end;

function TNyxCommentThread.GetComment1AuthorAvatar: INyxAvatar;
begin
  Result := Part(NyxPart('comment-1/author')) as INyxAvatar;
end;

function TNyxCommentThread.GetComment1BodyColumn: INyxColumn;
begin
  Result := Part(NyxPart('comment-1/body')) as INyxColumn;
end;

function TNyxCommentThread.GetComment1BodyNameLabel: INyxLabel;
begin
  Result := Part(NyxPart('comment-1/body/name')) as INyxLabel;
end;

function TNyxCommentThread.GetComment1BodyMessageLabel: INyxLabel;
begin
  Result := Part(NyxPart('comment-1/body/message')) as INyxLabel;
end;

function TNyxCommentThread.GetComment2Row: INyxRow;
begin
  Result := Part(NyxPart('comment-2')) as INyxRow;
end;

function TNyxCommentThread.GetComment2AuthorAvatar: INyxAvatar;
begin
  Result := Part(NyxPart('comment-2/author')) as INyxAvatar;
end;

function TNyxCommentThread.GetComment2BodyColumn: INyxColumn;
begin
  Result := Part(NyxPart('comment-2/body')) as INyxColumn;
end;

function TNyxCommentThread.GetComment2BodyNameLabel: INyxLabel;
begin
  Result := Part(NyxPart('comment-2/body/name')) as INyxLabel;
end;

function TNyxCommentThread.GetComment2BodyMessageLabel: INyxLabel;
begin
  Result := Part(NyxPart('comment-2/body/message')) as INyxLabel;
end;

function TNyxCommentThread.GetReplyMemo: INyxMemo;
begin
  Result := Part(NyxPart('reply')) as INyxMemo;
end;

function TNyxCommentThread.GetSendButton: INyxButton;
begin
  Result := Part(NyxPart('send')) as INyxButton;
end;

constructor TNyxPropertyGrid.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkPropertyGrid, AID, AConstruction);
end;

function TNyxPropertyGrid.WithText(const AText: TNyxText): INyxPropertyGrid;
begin
  Text := AText;
  Result := Self as INyxPropertyGrid;
end;

function NewNyxPropertyGrid(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxPropertyGrid;
begin
  Result := TNyxPropertyGrid.Create(AID, AConstruction);
end;

function TNyxPropertyGrid.GetProperty1Row: INyxRow;
begin
  Result := Part(NyxPart('property-1')) as INyxRow;
end;

function TNyxPropertyGrid.GetProperty1NameLabel: INyxLabel;
begin
  Result := Part(NyxPart('property-1/name')) as INyxLabel;
end;

function TNyxPropertyGrid.GetProperty1ValueInput: INyxInput;
begin
  Result := Part(NyxPart('property-1/value')) as INyxInput;
end;

function TNyxPropertyGrid.GetProperty2Row: INyxRow;
begin
  Result := Part(NyxPart('property-2')) as INyxRow;
end;

function TNyxPropertyGrid.GetProperty2NameLabel: INyxLabel;
begin
  Result := Part(NyxPart('property-2/name')) as INyxLabel;
end;

function TNyxPropertyGrid.GetProperty2ValueInput: INyxInput;
begin
  Result := Part(NyxPart('property-2/value')) as INyxInput;
end;

function TNyxPropertyGrid.GetProperty3Row: INyxRow;
begin
  Result := Part(NyxPart('property-3')) as INyxRow;
end;

function TNyxPropertyGrid.GetProperty3NameLabel: INyxLabel;
begin
  Result := Part(NyxPart('property-3/name')) as INyxLabel;
end;

function TNyxPropertyGrid.GetProperty3ValueInput: INyxInput;
begin
  Result := Part(NyxPart('property-3/value')) as INyxInput;
end;

constructor TNyxCommandBar.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkCommandBar, AID, AConstruction);
end;

function TNyxCommandBar.WithText(const AText: TNyxText): INyxCommandBar;
begin
  Text := AText;
  Result := Self as INyxCommandBar;
end;

function NewNyxCommandBar(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxCommandBar;
begin
  Result := TNyxCommandBar.Create(AID, AConstruction);
end;

function TNyxCommandBar.GetNewButton: INyxButton;
begin
  Result := Part(NyxPart('new')) as INyxButton;
end;

function TNyxCommandBar.GetOpenButton: INyxButton;
begin
  Result := Part(NyxPart('open')) as INyxButton;
end;

function TNyxCommandBar.GetSaveButton: INyxButton;
begin
  Result := Part(NyxPart('save')) as INyxButton;
end;

function TNyxCommandBar.GetSpacer: INyxSpacer;
begin
  Result := Part(NyxPart('spacer')) as INyxSpacer;
end;

function TNyxCommandBar.GetCommandInput: INyxInput;
begin
  Result := Part(NyxPart('command')) as INyxInput;
end;

constructor TNyxFloatingActionPanel.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkFloatingActionPanel, AID, AConstruction);
end;

function TNyxFloatingActionPanel.WithText(const AText: TNyxText): INyxFloatingActionPanel;
begin
  Text := AText;
  Result := Self as INyxFloatingActionPanel;
end;

function NewNyxFloatingActionPanel(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxFloatingActionPanel;
begin
  Result := TNyxFloatingActionPanel.Create(AID, AConstruction);
end;

function TNyxFloatingActionPanel.GetContentColumn: INyxColumn;
begin
  Result := Part(NyxPart('content')) as INyxColumn;
end;

function TNyxFloatingActionPanel.GetContentList: INyxList;
begin
  Result := Part(NyxPart('content/list')) as INyxList;
end;

function TNyxFloatingActionPanel.GetActionsRow: INyxRow;
begin
  Result := Part(NyxPart('actions')) as INyxRow;
end;

function TNyxFloatingActionPanel.GetActionsFloatingButton: INyxButton;
begin
  Result := Part(NyxPart('actions/floating')) as INyxButton;
end;

constructor TNyxSlotOverride.Create(const AID: TNyxText;
  AConstruction: TNyxConstruction);
begin
  inherited Create(nkSlotOverride, AID, AConstruction);
end;

function TNyxSlotOverride.WithText(const AText: TNyxText): INyxSlotOverride;
begin
  Text := AText;
  Result := Self as INyxSlotOverride;
end;

function NewNyxSlotOverride(const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxSlotOverride;
begin
  Result := TNyxSlotOverride.Create(AID, AConstruction);
end;

function NewNyxBuiltinControl(AKind: TNyxKind; const AID: TNyxText;
  AConstruction: TNyxConstruction): INyxControl;
begin
  case AKind of
    nkPage:
    begin
      Result := NewNyxPage(AID, AConstruction);
    end;
    nkColumn:
    begin
      Result := NewNyxColumn(AID, AConstruction);
    end;
    nkRow:
    begin
      Result := NewNyxRow(AID, AConstruction);
    end;
    nkGrid:
    begin
      Result := NewNyxGrid(AID, AConstruction);
    end;
    nkPanel:
    begin
      Result := NewNyxPanel(AID, AConstruction);
    end;
    nkCard:
    begin
      Result := NewNyxCard(AID, AConstruction);
    end;
    nkGroup:
    begin
      Result := NewNyxGroup(AID, AConstruction);
    end;
    nkToolbar:
    begin
      Result := NewNyxToolbar(AID, AConstruction);
    end;
    nkScroll:
    begin
      Result := NewNyxScroll(AID, AConstruction);
    end;
    nkTabs:
    begin
      Result := NewNyxTabs(AID, AConstruction);
    end;
    nkTab:
    begin
      Result := NewNyxTab(AID, AConstruction);
    end;
    nkHeading:
    begin
      Result := NewNyxHeading(AID, AConstruction);
    end;
    nkLabel:
    begin
      Result := NewNyxLabel(AID, AConstruction);
    end;
    nkButton:
    begin
      Result := NewNyxButton(AID, AConstruction);
    end;
    nkLink:
    begin
      Result := NewNyxLink(AID, AConstruction);
    end;
    nkInput:
    begin
      Result := NewNyxInput(AID, AConstruction);
    end;
    nkMemo:
    begin
      Result := NewNyxMemo(AID, AConstruction);
    end;
    nkCheckbox:
    begin
      Result := NewNyxCheckbox(AID, AConstruction);
    end;
    nkSwitch:
    begin
      Result := NewNyxSwitch(AID, AConstruction);
    end;
    nkRadio:
    begin
      Result := NewNyxRadio(AID, AConstruction);
    end;
    nkSelect:
    begin
      Result := NewNyxSelect(AID, AConstruction);
    end;
    nkSpin:
    begin
      Result := NewNyxSpin(AID, AConstruction);
    end;
    nkSlider:
    begin
      Result := NewNyxSlider(AID, AConstruction);
    end;
    nkDate:
    begin
      Result := NewNyxDate(AID, AConstruction);
    end;
    nkTime:
    begin
      Result := NewNyxTime(AID, AConstruction);
    end;
    nkColor:
    begin
      Result := NewNyxColor(AID, AConstruction);
    end;
    nkList:
    begin
      Result := NewNyxList(AID, AConstruction);
    end;
    nkTable:
    begin
      Result := NewNyxTable(AID, AConstruction);
    end;
    nkTree:
    begin
      Result := NewNyxTree(AID, AConstruction);
    end;
    nkImage:
    begin
      Result := NewNyxImage(AID, AConstruction);
    end;
    nkAvatar:
    begin
      Result := NewNyxAvatar(AID, AConstruction);
    end;
    nkProgress:
    begin
      Result := NewNyxProgress(AID, AConstruction);
    end;
    nkBadge:
    begin
      Result := NewNyxBadge(AID, AConstruction);
    end;
    nkAlert:
    begin
      Result := NewNyxAlert(AID, AConstruction);
    end;
    nkSeparator:
    begin
      Result := NewNyxSeparator(AID, AConstruction);
    end;
    nkSpacer:
    begin
      Result := NewNyxSpacer(AID, AConstruction);
    end;
    nkCode:
    begin
      Result := NewNyxCode(AID, AConstruction);
    end;
    nkCodeEditor:
    begin
      Result := NewNyxCodeEditor(AID, AConstruction);
    end;
    nkDesignSurface:
    begin
      Result := NewNyxDesignSurface(AID, AConstruction);
    end;
    nkComponent:
    begin
      Result := NewNyxComponent(AID, AConstruction);
    end;
    nkSplitView:
    begin
      Result := NewNyxSplitView(AID, AConstruction);
    end;
    nkLabeledButton:
    begin
      Result := NewNyxLabeledButton(AID, AConstruction);
    end;
    nkSplitButton:
    begin
      Result := NewNyxSplitButton(AID, AConstruction);
    end;
    nkSearchField:
    begin
      Result := NewNyxSearchField(AID, AConstruction);
    end;
    nkFormField:
    begin
      Result := NewNyxFormField(AID, AConstruction);
    end;
    nkLoginForm:
    begin
      Result := NewNyxLoginForm(AID, AConstruction);
    end;
    nkSettingsPanel:
    begin
      Result := NewNyxSettingsPanel(AID, AConstruction);
    end;
    nkDateRange:
    begin
      Result := NewNyxDateRange(AID, AConstruction);
    end;
    nkNumberStepper:
    begin
      Result := NewNyxNumberStepper(AID, AConstruction);
    end;
    nkSegmentedControl:
    begin
      Result := NewNyxSegmentedControl(AID, AConstruction);
    end;
    nkRating:
    begin
      Result := NewNyxRating(AID, AConstruction);
    end;
    nkDataToolbar:
    begin
      Result := NewNyxDataToolbar(AID, AConstruction);
    end;
    nkFilterBar:
    begin
      Result := NewNyxFilterBar(AID, AConstruction);
    end;
    nkPagination:
    begin
      Result := NewNyxPagination(AID, AConstruction);
    end;
    nkBreadcrumbs:
    begin
      Result := NewNyxBreadcrumbs(AID, AConstruction);
    end;
    nkSidebarNav:
    begin
      Result := NewNyxSidebarNav(AID, AConstruction);
    end;
    nkMasterDetail:
    begin
      Result := NewNyxMasterDetail(AID, AConstruction);
    end;
    nkListCard:
    begin
      Result := NewNyxListCard(AID, AConstruction);
    end;
    nkDataCard:
    begin
      Result := NewNyxDataCard(AID, AConstruction);
    end;
    nkKanbanBoard:
    begin
      Result := NewNyxKanbanBoard(AID, AConstruction);
    end;
    nkTimeline:
    begin
      Result := NewNyxTimeline(AID, AConstruction);
    end;
    nkStatCard:
    begin
      Result := NewNyxStatCard(AID, AConstruction);
    end;
    nkMetricGrid:
    begin
      Result := NewNyxMetricGrid(AID, AConstruction);
    end;
    nkEmptyState:
    begin
      Result := NewNyxEmptyState(AID, AConstruction);
    end;
    nkConfirmationDialog:
    begin
      Result := NewNyxConfirmationDialog(AID, AConstruction);
    end;
    nkNotificationCard:
    begin
      Result := NewNyxNotificationCard(AID, AConstruction);
    end;
    nkToast:
    begin
      Result := NewNyxToast(AID, AConstruction);
    end;
    nkWizardStep:
    begin
      Result := NewNyxWizardStep(AID, AConstruction);
    end;
    nkStepper:
    begin
      Result := NewNyxStepper(AID, AConstruction);
    end;
    nkProfileCard:
    begin
      Result := NewNyxProfileCard(AID, AConstruction);
    end;
    nkMediaCard:
    begin
      Result := NewNyxMediaCard(AID, AConstruction);
    end;
    nkAvatarGroup:
    begin
      Result := NewNyxAvatarGroup(AID, AConstruction);
    end;
    nkCommentThread:
    begin
      Result := NewNyxCommentThread(AID, AConstruction);
    end;
    nkPropertyGrid:
    begin
      Result := NewNyxPropertyGrid(AID, AConstruction);
    end;
    nkCommandBar:
    begin
      Result := NewNyxCommandBar(AID, AConstruction);
    end;
    nkFloatingActionPanel:
    begin
      Result := NewNyxFloatingActionPanel(AID, AConstruction);
    end;
    nkSlotOverride:
    begin
      Result := NewNyxSlotOverride(AID, AConstruction);
    end;
  end;
end;

function RetainNyxControl(ANode: TNyxNode): INyxControl;
var
  LKind: TNyxKind;
  LReference: INyxNode;
begin

  if ANode = nil then
  begin
    raise ENyxModel.Create('A retained descriptor is required');
  end;

  LReference := ANode.ComponentReference;

  if (LReference <> nil) and Supports(LReference, INyxControl, Result) then
  begin
    Exit;
  end;

  if not TryNyxKind(ANode.Kind, LKind) then
  begin
    Exit(TNyxControl.CreateFromNode(ANode));
  end;
  case LKind of
    nkPage:
    begin
      Result := TNyxPage.CreateFromNode(ANode);
    end;
    nkColumn:
    begin
      Result := TNyxColumn.CreateFromNode(ANode);
    end;
    nkRow:
    begin
      Result := TNyxRow.CreateFromNode(ANode);
    end;
    nkGrid:
    begin
      Result := TNyxGrid.CreateFromNode(ANode);
    end;
    nkPanel:
    begin
      Result := TNyxPanel.CreateFromNode(ANode);
    end;
    nkCard:
    begin
      Result := TNyxCard.CreateFromNode(ANode);
    end;
    nkGroup:
    begin
      Result := TNyxGroup.CreateFromNode(ANode);
    end;
    nkToolbar:
    begin
      Result := TNyxToolbar.CreateFromNode(ANode);
    end;
    nkScroll:
    begin
      Result := TNyxScroll.CreateFromNode(ANode);
    end;
    nkTabs:
    begin
      Result := TNyxTabs.CreateFromNode(ANode);
    end;
    nkTab:
    begin
      Result := TNyxTab.CreateFromNode(ANode);
    end;
    nkHeading:
    begin
      Result := TNyxHeading.CreateFromNode(ANode);
    end;
    nkLabel:
    begin
      Result := TNyxLabel.CreateFromNode(ANode);
    end;
    nkButton:
    begin
      Result := TNyxButton.CreateFromNode(ANode);
    end;
    nkLink:
    begin
      Result := TNyxLink.CreateFromNode(ANode);
    end;
    nkInput:
    begin
      Result := TNyxInput.CreateFromNode(ANode);
    end;
    nkMemo:
    begin
      Result := TNyxMemo.CreateFromNode(ANode);
    end;
    nkCheckbox:
    begin
      Result := TNyxCheckbox.CreateFromNode(ANode);
    end;
    nkSwitch:
    begin
      Result := TNyxSwitch.CreateFromNode(ANode);
    end;
    nkRadio:
    begin
      Result := TNyxRadio.CreateFromNode(ANode);
    end;
    nkSelect:
    begin
      Result := TNyxSelect.CreateFromNode(ANode);
    end;
    nkSpin:
    begin
      Result := TNyxSpin.CreateFromNode(ANode);
    end;
    nkSlider:
    begin
      Result := TNyxSlider.CreateFromNode(ANode);
    end;
    nkDate:
    begin
      Result := TNyxDate.CreateFromNode(ANode);
    end;
    nkTime:
    begin
      Result := TNyxTime.CreateFromNode(ANode);
    end;
    nkColor:
    begin
      Result := TNyxColor.CreateFromNode(ANode);
    end;
    nkList:
    begin
      Result := TNyxList.CreateFromNode(ANode);
    end;
    nkTable:
    begin
      Result := TNyxTable.CreateFromNode(ANode);
    end;
    nkTree:
    begin
      Result := TNyxTree.CreateFromNode(ANode);
    end;
    nkImage:
    begin
      Result := TNyxImage.CreateFromNode(ANode);
    end;
    nkAvatar:
    begin
      Result := TNyxAvatar.CreateFromNode(ANode);
    end;
    nkProgress:
    begin
      Result := TNyxProgress.CreateFromNode(ANode);
    end;
    nkBadge:
    begin
      Result := TNyxBadge.CreateFromNode(ANode);
    end;
    nkAlert:
    begin
      Result := TNyxAlert.CreateFromNode(ANode);
    end;
    nkSeparator:
    begin
      Result := TNyxSeparator.CreateFromNode(ANode);
    end;
    nkSpacer:
    begin
      Result := TNyxSpacer.CreateFromNode(ANode);
    end;
    nkCode:
    begin
      Result := TNyxCode.CreateFromNode(ANode);
    end;
    nkCodeEditor:
    begin
      Result := TNyxCodeEditor.CreateFromNode(ANode);
    end;
    nkDesignSurface:
    begin
      Result := TNyxDesignSurface.CreateFromNode(ANode);
    end;
    nkComponent:
    begin
      Result := TNyxComponent.CreateFromNode(ANode);
    end;
    nkSplitView:
    begin
      Result := TNyxSplitView.CreateFromNode(ANode);
    end;
    nkLabeledButton:
    begin
      Result := TNyxLabeledButton.CreateFromNode(ANode);
    end;
    nkSplitButton:
    begin
      Result := TNyxSplitButton.CreateFromNode(ANode);
    end;
    nkSearchField:
    begin
      Result := TNyxSearchField.CreateFromNode(ANode);
    end;
    nkFormField:
    begin
      Result := TNyxFormField.CreateFromNode(ANode);
    end;
    nkLoginForm:
    begin
      Result := TNyxLoginForm.CreateFromNode(ANode);
    end;
    nkSettingsPanel:
    begin
      Result := TNyxSettingsPanel.CreateFromNode(ANode);
    end;
    nkDateRange:
    begin
      Result := TNyxDateRange.CreateFromNode(ANode);
    end;
    nkNumberStepper:
    begin
      Result := TNyxNumberStepper.CreateFromNode(ANode);
    end;
    nkSegmentedControl:
    begin
      Result := TNyxSegmentedControl.CreateFromNode(ANode);
    end;
    nkRating:
    begin
      Result := TNyxRating.CreateFromNode(ANode);
    end;
    nkDataToolbar:
    begin
      Result := TNyxDataToolbar.CreateFromNode(ANode);
    end;
    nkFilterBar:
    begin
      Result := TNyxFilterBar.CreateFromNode(ANode);
    end;
    nkPagination:
    begin
      Result := TNyxPagination.CreateFromNode(ANode);
    end;
    nkBreadcrumbs:
    begin
      Result := TNyxBreadcrumbs.CreateFromNode(ANode);
    end;
    nkSidebarNav:
    begin
      Result := TNyxSidebarNav.CreateFromNode(ANode);
    end;
    nkMasterDetail:
    begin
      Result := TNyxMasterDetail.CreateFromNode(ANode);
    end;
    nkListCard:
    begin
      Result := TNyxListCard.CreateFromNode(ANode);
    end;
    nkDataCard:
    begin
      Result := TNyxDataCard.CreateFromNode(ANode);
    end;
    nkKanbanBoard:
    begin
      Result := TNyxKanbanBoard.CreateFromNode(ANode);
    end;
    nkTimeline:
    begin
      Result := TNyxTimeline.CreateFromNode(ANode);
    end;
    nkStatCard:
    begin
      Result := TNyxStatCard.CreateFromNode(ANode);
    end;
    nkMetricGrid:
    begin
      Result := TNyxMetricGrid.CreateFromNode(ANode);
    end;
    nkEmptyState:
    begin
      Result := TNyxEmptyState.CreateFromNode(ANode);
    end;
    nkConfirmationDialog:
    begin
      Result := TNyxConfirmationDialog.CreateFromNode(ANode);
    end;
    nkNotificationCard:
    begin
      Result := TNyxNotificationCard.CreateFromNode(ANode);
    end;
    nkToast:
    begin
      Result := TNyxToast.CreateFromNode(ANode);
    end;
    nkWizardStep:
    begin
      Result := TNyxWizardStep.CreateFromNode(ANode);
    end;
    nkStepper:
    begin
      Result := TNyxStepper.CreateFromNode(ANode);
    end;
    nkProfileCard:
    begin
      Result := TNyxProfileCard.CreateFromNode(ANode);
    end;
    nkMediaCard:
    begin
      Result := TNyxMediaCard.CreateFromNode(ANode);
    end;
    nkAvatarGroup:
    begin
      Result := TNyxAvatarGroup.CreateFromNode(ANode);
    end;
    nkCommentThread:
    begin
      Result := TNyxCommentThread.CreateFromNode(ANode);
    end;
    nkPropertyGrid:
    begin
      Result := TNyxPropertyGrid.CreateFromNode(ANode);
    end;
    nkCommandBar:
    begin
      Result := TNyxCommandBar.CreateFromNode(ANode);
    end;
    nkFloatingActionPanel:
    begin
      Result := TNyxFloatingActionPanel.CreateFromNode(ANode);
    end;
    nkSlotOverride:
    begin
      Result := TNyxSlotOverride.CreateFromNode(ANode);
    end;
  end;
end;
