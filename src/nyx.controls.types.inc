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
  INyxPage = interface;
  INyxColumn = interface;
  INyxRow = interface;
  INyxGrid = interface;
  INyxPanel = interface;
  INyxCard = interface;
  INyxGroup = interface;
  INyxToolbar = interface;
  INyxScroll = interface;
  INyxTabs = interface;
  INyxTab = interface;
  INyxHeading = interface;
  INyxLabel = interface;
  INyxButton = interface;
  INyxLink = interface;
  INyxInput = interface;
  INyxMemo = interface;
  INyxCheckbox = interface;
  INyxSwitch = interface;
  INyxRadio = interface;
  INyxSelect = interface;
  INyxSpin = interface;
  INyxSlider = interface;
  INyxDate = interface;
  INyxTime = interface;
  INyxColor = interface;
  INyxList = interface;
  INyxTable = interface;
  INyxTree = interface;
  INyxImage = interface;
  INyxAvatar = interface;
  INyxProgress = interface;
  INyxBadge = interface;
  INyxAlert = interface;
  INyxSeparator = interface;
  INyxSpacer = interface;
  INyxCode = interface;
  INyxCodeEditor = interface;
  INyxDesignSurface = interface;
  INyxComponent = interface;
  INyxSplitView = interface;
  INyxLabeledButton = interface;
  INyxSplitButton = interface;
  INyxSearchField = interface;
  INyxFormField = interface;
  INyxLoginForm = interface;
  INyxSettingsPanel = interface;
  INyxDateRange = interface;
  INyxNumberStepper = interface;
  INyxSegmentedControl = interface;
  INyxRating = interface;
  INyxDataToolbar = interface;
  INyxFilterBar = interface;
  INyxPagination = interface;
  INyxBreadcrumbs = interface;
  INyxSidebarNav = interface;
  INyxMasterDetail = interface;
  INyxListCard = interface;
  INyxDataCard = interface;
  INyxKanbanBoard = interface;
  INyxTimeline = interface;
  INyxStatCard = interface;
  INyxMetricGrid = interface;
  INyxEmptyState = interface;
  INyxConfirmationDialog = interface;
  INyxNotificationCard = interface;
  INyxToast = interface;
  INyxWizardStep = interface;
  INyxStepper = interface;
  INyxProfileCard = interface;
  INyxMediaCard = interface;
  INyxAvatarGroup = interface;
  INyxCommentThread = interface;
  INyxPropertyGrid = interface;
  INyxCommandBar = interface;
  INyxFloatingActionPanel = interface;
  INyxSlotOverride = interface;

  { Specialized page contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxPage = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200815DFA76}']
    function WithText(const AText: TNyxText): INyxPage;
  end;

  { Default managed page implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxPage = class(TNyxLayoutControl, INyxPage)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxPage;
  end;

  { Specialized column contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxColumn = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200F5F8D227}']
    function WithText(const AText: TNyxText): INyxColumn;
  end;

  { Default managed column implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxColumn = class(TNyxLayoutControl, INyxColumn)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxColumn;
  end;

  { Specialized row contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxRow = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200440E1D7B}']
    function WithText(const AText: TNyxText): INyxRow;
  end;

  { Default managed row implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxRow = class(TNyxLayoutControl, INyxRow)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxRow;
  end;

  { Specialized grid contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxGrid = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200AF871A91}']
    function WithText(const AText: TNyxText): INyxGrid;
  end;

  { Default managed grid implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxGrid = class(TNyxLayoutControl, INyxGrid)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxGrid;
  end;

  { Specialized panel contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxPanel = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200CCFE4C45}']
    function WithText(const AText: TNyxText): INyxPanel;
  end;

  { Default managed panel implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxPanel = class(TNyxLayoutControl, INyxPanel)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxPanel;
  end;

  { Specialized card contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxCard = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02008827595F}']
    function WithText(const AText: TNyxText): INyxCard;
  end;

  { Default managed card implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxCard = class(TNyxLayoutControl, INyxCard)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxCard;
  end;

  { Specialized group contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxGroup = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02005FB91E8C}']
    function WithText(const AText: TNyxText): INyxGroup;
  end;

  { Default managed group implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxGroup = class(TNyxLayoutControl, INyxGroup)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxGroup;
  end;

  { Specialized toolbar contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxToolbar = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200FD12C898}']
    function WithText(const AText: TNyxText): INyxToolbar;
  end;

  { Default managed toolbar implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxToolbar = class(TNyxLayoutControl, INyxToolbar)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxToolbar;
  end;

  { Specialized scroll contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxScroll = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200DD4EC22C}']
    function WithText(const AText: TNyxText): INyxScroll;
  end;

  { Default managed scroll implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxScroll = class(TNyxLayoutControl, INyxScroll)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxScroll;
  end;

  { Specialized tabs contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxTabs = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02000C1DCD2D}']
    function WithText(const AText: TNyxText): INyxTabs;
  end;

  { Default managed tabs implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxTabs = class(TNyxLayoutControl, INyxTabs)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxTabs;
  end;

  { Specialized tab contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxTab = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-020098F72E4C}']
    function WithText(const AText: TNyxText): INyxTab;
  end;

  { Default managed tab implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxTab = class(TNyxLayoutControl, INyxTab)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxTab;
  end;

  { Specialized heading contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxHeading = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-02009F481715}']
    function WithText(const AText: TNyxText): INyxHeading;
  end;

  { Default managed heading implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxHeading = class(TNyxCaptionControl, INyxHeading)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxHeading;
  end;

  { Specialized label contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxLabel = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-0200F69717FD}']
    function WithText(const AText: TNyxText): INyxLabel;
  end;

  { Default managed label implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxLabel = class(TNyxCaptionControl, INyxLabel)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxLabel;
  end;

  { Specialized button contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxButton = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-020043B27471}']
    function WithText(const AText: TNyxText): INyxButton;
  end;

  { Default managed button implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxButton = class(TNyxCaptionControl, INyxButton)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxButton;
  end;

  { Specialized link contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxLink = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-02000DDB0669}']
    function WithText(const AText: TNyxText): INyxLink;
  end;

  { Default managed link implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxLink = class(TNyxCaptionControl, INyxLink)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxLink;
  end;

  { Specialized input contract; inherited properties retain the TextInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxInput = interface(INyxTextInput)
    ['{737A7921-4621-4C6F-8C01-0200F9D86F7B}']
    function WithText(const AText: TNyxText): INyxInput;
  end;

  { Default managed input implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxInput = class(TNyxTextInput, INyxInput)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxInput;
  end;

  { Specialized memo contract; inherited properties retain the TextInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxMemo = interface(INyxTextInput)
    ['{737A7921-4621-4C6F-8C01-02009FE72543}']
    function WithText(const AText: TNyxText): INyxMemo;
  end;

  { Default managed memo implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxMemo = class(TNyxTextInput, INyxMemo)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxMemo;
  end;

  { Specialized checkbox contract; inherited properties retain the BooleanInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxCheckbox = interface(INyxBooleanInput)
    ['{737A7921-4621-4C6F-8C01-020006389B18}']
    function WithText(const AText: TNyxText): INyxCheckbox;
  end;

  { Default managed checkbox implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxCheckbox = class(TNyxBooleanInput, INyxCheckbox)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxCheckbox;
  end;

  { Specialized switch contract; inherited properties retain the BooleanInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxSwitch = interface(INyxBooleanInput)
    ['{737A7921-4621-4C6F-8C01-020093E05F71}']
    function WithText(const AText: TNyxText): INyxSwitch;
  end;

  { Default managed switch implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSwitch = class(TNyxBooleanInput, INyxSwitch)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSwitch;
  end;

  { Specialized radio contract; inherited properties retain the BooleanInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxRadio = interface(INyxBooleanInput)
    ['{737A7921-4621-4C6F-8C01-0200AE401EB4}']
    function WithText(const AText: TNyxText): INyxRadio;
  end;

  { Default managed radio implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxRadio = class(TNyxBooleanInput, INyxRadio)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxRadio;
  end;

  { Specialized select contract; inherited properties retain the ChoiceControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxSelect = interface(INyxChoiceControl)
    ['{737A7921-4621-4C6F-8C01-020011C2662D}']
    function WithText(const AText: TNyxText): INyxSelect;
  end;

  { Default managed select implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSelect = class(TNyxChoiceControl, INyxSelect)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSelect;
  end;

  { Specialized spin contract; inherited properties retain the IntegerInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxSpin = interface(INyxIntegerInput)
    ['{737A7921-4621-4C6F-8C01-02000914D819}']
    function WithText(const AText: TNyxText): INyxSpin;
  end;

  { Default managed spin implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSpin = class(TNyxIntegerInput, INyxSpin)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSpin;
  end;

  { Specialized slider contract; inherited properties retain the IntegerInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxSlider = interface(INyxIntegerInput)
    ['{737A7921-4621-4C6F-8C01-02006EAC67FC}']
    function WithText(const AText: TNyxText): INyxSlider;
  end;

  { Default managed slider implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSlider = class(TNyxIntegerInput, INyxSlider)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSlider;
  end;

  { Specialized date contract; inherited properties retain the TextInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxDate = interface(INyxTextInput)
    ['{737A7921-4621-4C6F-8C01-0200D472DC59}']
    function WithText(const AText: TNyxText): INyxDate;
  end;

  { Default managed date implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxDate = class(TNyxTextInput, INyxDate)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxDate;
  end;

  { Specialized time contract; inherited properties retain the TextInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxTime = interface(INyxTextInput)
    ['{737A7921-4621-4C6F-8C01-02005D3C9BE4}']
    function WithText(const AText: TNyxText): INyxTime;
  end;

  { Default managed time implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxTime = class(TNyxTextInput, INyxTime)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxTime;
  end;

  { Specialized color contract; inherited properties retain the TextInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxColor = interface(INyxTextInput)
    ['{737A7921-4621-4C6F-8C01-02003D7E6258}']
    function WithText(const AText: TNyxText): INyxColor;
  end;

  { Default managed color implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxColor = class(TNyxTextInput, INyxColor)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxColor;
  end;

  { Specialized list contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxList = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-02000CFB5881}']
    function WithText(const AText: TNyxText): INyxList;
  end;

  { Default managed list implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxList = class(TNyxCaptionControl, INyxList)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxList;
  end;

  { Specialized table contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxTable = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-02004A9C9BDF}']
    function WithText(const AText: TNyxText): INyxTable;
  end;

  { Default managed table implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxTable = class(TNyxCaptionControl, INyxTable)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxTable;
  end;

  { Specialized tree contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxTree = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-02006D8B34D5}']
    function WithText(const AText: TNyxText): INyxTree;
  end;

  { Default managed tree implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxTree = class(TNyxCaptionControl, INyxTree)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxTree;
  end;

  { Specialized image contract; inherited properties retain the ImageControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxImage = interface(INyxImageControl)
    ['{737A7921-4621-4C6F-8C01-0200B35135FA}']
    function WithText(const AText: TNyxText): INyxImage;
  end;

  { Default managed image implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxImage = class(TNyxImageControl, INyxImage)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxImage;
  end;

  { Specialized avatar contract; inherited properties retain the ImageControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxAvatar = interface(INyxImageControl)
    ['{737A7921-4621-4C6F-8C01-0200223449C4}']
    function WithText(const AText: TNyxText): INyxAvatar;
  end;

  { Default managed avatar implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxAvatar = class(TNyxImageControl, INyxAvatar)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxAvatar;
  end;

  { Specialized progress contract; inherited properties retain the IntegerInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxProgress = interface(INyxIntegerInput)
    ['{737A7921-4621-4C6F-8C01-0200CB084E46}']
    function WithText(const AText: TNyxText): INyxProgress;
  end;

  { Default managed progress implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxProgress = class(TNyxIntegerInput, INyxProgress)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxProgress;
  end;

  { Specialized badge contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxBadge = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-020035648278}']
    function WithText(const AText: TNyxText): INyxBadge;
  end;

  { Default managed badge implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxBadge = class(TNyxCaptionControl, INyxBadge)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxBadge;
  end;

  { Specialized alert contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxAlert = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-0200B7C358F9}']
    function WithText(const AText: TNyxText): INyxAlert;
  end;

  { Default managed alert implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxAlert = class(TNyxCaptionControl, INyxAlert)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxAlert;
  end;

  { Specialized separator contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxSeparator = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-02006A88F0BC}']
    function WithText(const AText: TNyxText): INyxSeparator;
  end;

  { Default managed separator implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSeparator = class(TNyxCaptionControl, INyxSeparator)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSeparator;
  end;

  { Specialized spacer contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxSpacer = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-0200EA0E4AD5}']
    function WithText(const AText: TNyxText): INyxSpacer;
  end;

  { Default managed spacer implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSpacer = class(TNyxCaptionControl, INyxSpacer)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSpacer;
  end;

  { Specialized code contract; inherited properties retain the CaptionControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxCode = interface(INyxCaptionControl)
    ['{737A7921-4621-4C6F-8C01-0200F9316CF4}']
    function WithText(const AText: TNyxText): INyxCode;
  end;

  { Default managed code implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxCode = class(TNyxCaptionControl, INyxCode)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxCode;
  end;

  { Specialized code-editor contract; inherited properties retain the TextInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxCodeEditor = interface(INyxTextInput)
    ['{737A7921-4621-4C6F-8C01-020081316BBE}']
    function WithText(const AText: TNyxText): INyxCodeEditor;
  end;

  { Default managed code-editor implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxCodeEditor = class(TNyxTextInput, INyxCodeEditor)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxCodeEditor;
  end;

  { Specialized design-surface contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxDesignSurface = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200AEA3B905}']
    function WithText(const AText: TNyxText): INyxDesignSurface;
  end;

  { Default managed design-surface implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxDesignSurface = class(TNyxLayoutControl, INyxDesignSurface)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxDesignSurface;
  end;

  { Specialized component contract; inherited properties retain the ReferenceControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxComponent = interface(INyxReferenceControl)
    ['{737A7921-4621-4C6F-8C01-02005EFD6EE0}']
    function WithText(const AText: TNyxText): INyxComponent;
  end;

  { Default managed component implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxComponent = class(TNyxReferenceControl, INyxComponent)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxComponent;
  end;

  { Specialized split-view contract; inherited properties retain the SplitControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxSplitView = interface(INyxSplitControl)
    ['{737A7921-4621-4C6F-8C01-020095D107C3}']
    function WithText(const AText: TNyxText): INyxSplitView;
  end;

  { Default managed split-view implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSplitView = class(TNyxSplitControl, INyxSplitView)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSplitView;
  end;

  { Specialized labeled-button contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxLabeledButton = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-020091724AF3}']
    function WithText(const AText: TNyxText): INyxLabeledButton;
    { Retains the named label part; missing/retyped parts raise a contract error. }
    function GetCaptionLabel: INyxLabel;
    property CaptionLabel: INyxLabel read GetCaptionLabel;
    { Retains the named button part; missing/retyped parts raise a contract error. }
    function GetButton: INyxButton;
    property Button: INyxButton read GetButton;
  end;

  { Default managed labeled-button implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxLabeledButton = class(TNyxLayoutControl, INyxLabeledButton)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxLabeledButton;
    { Retains the named label part; missing/retyped parts raise a contract error. }
    function GetCaptionLabel: INyxLabel;
    property CaptionLabel: INyxLabel read GetCaptionLabel;
    { Retains the named button part; missing/retyped parts raise a contract error. }
    function GetButton: INyxButton;
    property Button: INyxButton read GetButton;
  end;

  { Specialized split-button contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxSplitButton = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02001E66CD12}']
    function WithText(const AText: TNyxText): INyxSplitButton;
    { Retains the named primary part; missing/retyped parts raise a contract error. }
    function GetPrimaryButton: INyxButton;
    property PrimaryButton: INyxButton read GetPrimaryButton;
    { Retains the named menu part; missing/retyped parts raise a contract error. }
    function GetMenuButton: INyxButton;
    property MenuButton: INyxButton read GetMenuButton;
  end;

  { Default managed split-button implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSplitButton = class(TNyxLayoutControl, INyxSplitButton)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSplitButton;
    { Retains the named primary part; missing/retyped parts raise a contract error. }
    function GetPrimaryButton: INyxButton;
    property PrimaryButton: INyxButton read GetPrimaryButton;
    { Retains the named menu part; missing/retyped parts raise a contract error. }
    function GetMenuButton: INyxButton;
    property MenuButton: INyxButton read GetMenuButton;
  end;

  { Specialized search-field contract; inherited properties retain the TextInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxSearchField = interface(INyxTextInput)
    ['{737A7921-4621-4C6F-8C01-0200FC8C0FF0}']
    function WithText(const AText: TNyxText): INyxSearchField;
    { Retains the named query part; missing/retyped parts raise a contract error. }
    function GetQueryInput: INyxInput;
    property QueryInput: INyxInput read GetQueryInput;
    { Retains the named search part; missing/retyped parts raise a contract error. }
    function GetSearchButton: INyxButton;
    property SearchButton: INyxButton read GetSearchButton;
    { Retains the named clear part; missing/retyped parts raise a contract error. }
    function GetClearButton: INyxButton;
    property ClearButton: INyxButton read GetClearButton;
  end;

  { Default managed search-field implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSearchField = class(TNyxTextInput, INyxSearchField)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSearchField;
    { Retains the named query part; missing/retyped parts raise a contract error. }
    function GetQueryInput: INyxInput;
    property QueryInput: INyxInput read GetQueryInput;
    { Retains the named search part; missing/retyped parts raise a contract error. }
    function GetSearchButton: INyxButton;
    property SearchButton: INyxButton read GetSearchButton;
    { Retains the named clear part; missing/retyped parts raise a contract error. }
    function GetClearButton: INyxButton;
    property ClearButton: INyxButton read GetClearButton;
  end;

  { Specialized form-field contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxFormField = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200315A4B4A}']
    function WithText(const AText: TNyxText): INyxFormField;
    { Retains the named input part; missing/retyped parts raise a contract error. }
    function GetInput: INyxInput;
    property Input: INyxInput read GetInput;
    { Retains the named help part; missing/retyped parts raise a contract error. }
    function GetHelpLabel: INyxLabel;
    property HelpLabel: INyxLabel read GetHelpLabel;
    { Retains the named error part; missing/retyped parts raise a contract error. }
    function GetErrorAlert: INyxAlert;
    property ErrorAlert: INyxAlert read GetErrorAlert;
  end;

  { Default managed form-field implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxFormField = class(TNyxLayoutControl, INyxFormField)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxFormField;
    { Retains the named input part; missing/retyped parts raise a contract error. }
    function GetInput: INyxInput;
    property Input: INyxInput read GetInput;
    { Retains the named help part; missing/retyped parts raise a contract error. }
    function GetHelpLabel: INyxLabel;
    property HelpLabel: INyxLabel read GetHelpLabel;
    { Retains the named error part; missing/retyped parts raise a contract error. }
    function GetErrorAlert: INyxAlert;
    property ErrorAlert: INyxAlert read GetErrorAlert;
  end;

  { Specialized login-form contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxLoginForm = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200D1BEB12F}']
    function WithText(const AText: TNyxText): INyxLoginForm;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named email part; missing/retyped parts raise a contract error. }
    function GetEmailInput: INyxInput;
    property EmailInput: INyxInput read GetEmailInput;
    { Retains the named password part; missing/retyped parts raise a contract error. }
    function GetPasswordInput: INyxInput;
    property PasswordInput: INyxInput read GetPasswordInput;
    { Retains the named remember part; missing/retyped parts raise a contract error. }
    function GetRememberCheckbox: INyxCheckbox;
    property RememberCheckbox: INyxCheckbox read GetRememberCheckbox;
    { Retains the named submit part; missing/retyped parts raise a contract error. }
    function GetSubmitButton: INyxButton;
    property SubmitButton: INyxButton read GetSubmitButton;
  end;

  { Default managed login-form implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxLoginForm = class(TNyxLayoutControl, INyxLoginForm)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxLoginForm;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named email part; missing/retyped parts raise a contract error. }
    function GetEmailInput: INyxInput;
    property EmailInput: INyxInput read GetEmailInput;
    { Retains the named password part; missing/retyped parts raise a contract error. }
    function GetPasswordInput: INyxInput;
    property PasswordInput: INyxInput read GetPasswordInput;
    { Retains the named remember part; missing/retyped parts raise a contract error. }
    function GetRememberCheckbox: INyxCheckbox;
    property RememberCheckbox: INyxCheckbox read GetRememberCheckbox;
    { Retains the named submit part; missing/retyped parts raise a contract error. }
    function GetSubmitButton: INyxButton;
    property SubmitButton: INyxButton read GetSubmitButton;
  end;

  { Specialized settings-panel contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxSettingsPanel = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200EF175687}']
    function WithText(const AText: TNyxText): INyxSettingsPanel;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named notifications part; missing/retyped parts raise a contract error. }
    function GetNotificationsSwitch: INyxSwitch;
    property NotificationsSwitch: INyxSwitch read GetNotificationsSwitch;
    { Retains the named sync part; missing/retyped parts raise a contract error. }
    function GetSyncSwitch: INyxSwitch;
    property SyncSwitch: INyxSwitch read GetSyncSwitch;
    { Retains the named language part; missing/retyped parts raise a contract error. }
    function GetLanguageSelect: INyxSelect;
    property LanguageSelect: INyxSelect read GetLanguageSelect;
    { Retains the named save part; missing/retyped parts raise a contract error. }
    function GetSaveButton: INyxButton;
    property SaveButton: INyxButton read GetSaveButton;
  end;

  { Default managed settings-panel implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSettingsPanel = class(TNyxLayoutControl, INyxSettingsPanel)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSettingsPanel;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named notifications part; missing/retyped parts raise a contract error. }
    function GetNotificationsSwitch: INyxSwitch;
    property NotificationsSwitch: INyxSwitch read GetNotificationsSwitch;
    { Retains the named sync part; missing/retyped parts raise a contract error. }
    function GetSyncSwitch: INyxSwitch;
    property SyncSwitch: INyxSwitch read GetSyncSwitch;
    { Retains the named language part; missing/retyped parts raise a contract error. }
    function GetLanguageSelect: INyxSelect;
    property LanguageSelect: INyxSelect read GetLanguageSelect;
    { Retains the named save part; missing/retyped parts raise a contract error. }
    function GetSaveButton: INyxButton;
    property SaveButton: INyxButton read GetSaveButton;
  end;

  { Specialized date-range contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxDateRange = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200416557C1}']
    function WithText(const AText: TNyxText): INyxDateRange;
    { Retains the named start part; missing/retyped parts raise a contract error. }
    function GetStartDate: INyxDate;
    property StartDate: INyxDate read GetStartDate;
    { Retains the named separator part; missing/retyped parts raise a contract error. }
    function GetSeparatorLabel: INyxLabel;
    property SeparatorLabel: INyxLabel read GetSeparatorLabel;
    { Retains the named finish part; missing/retyped parts raise a contract error. }
    function GetFinishDate: INyxDate;
    property FinishDate: INyxDate read GetFinishDate;
  end;

  { Default managed date-range implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxDateRange = class(TNyxLayoutControl, INyxDateRange)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxDateRange;
    { Retains the named start part; missing/retyped parts raise a contract error. }
    function GetStartDate: INyxDate;
    property StartDate: INyxDate read GetStartDate;
    { Retains the named separator part; missing/retyped parts raise a contract error. }
    function GetSeparatorLabel: INyxLabel;
    property SeparatorLabel: INyxLabel read GetSeparatorLabel;
    { Retains the named finish part; missing/retyped parts raise a contract error. }
    function GetFinishDate: INyxDate;
    property FinishDate: INyxDate read GetFinishDate;
  end;

  { Specialized number-stepper contract; inherited properties retain the IntegerInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxNumberStepper = interface(INyxIntegerInput)
    ['{737A7921-4621-4C6F-8C01-020029519724}']
    function WithText(const AText: TNyxText): INyxNumberStepper;
    { Retains the named decrement part; missing/retyped parts raise a contract error. }
    function GetDecrementButton: INyxButton;
    property DecrementButton: INyxButton read GetDecrementButton;
    { Retains the named value part; missing/retyped parts raise a contract error. }
    function GetValueSpin: INyxSpin;
    property ValueSpin: INyxSpin read GetValueSpin;
    { Retains the named increment part; missing/retyped parts raise a contract error. }
    function GetIncrementButton: INyxButton;
    property IncrementButton: INyxButton read GetIncrementButton;
  end;

  { Default managed number-stepper implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxNumberStepper = class(TNyxIntegerInput, INyxNumberStepper)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxNumberStepper;
    { Retains the named decrement part; missing/retyped parts raise a contract error. }
    function GetDecrementButton: INyxButton;
    property DecrementButton: INyxButton read GetDecrementButton;
    { Retains the named value part; missing/retyped parts raise a contract error. }
    function GetValueSpin: INyxSpin;
    property ValueSpin: INyxSpin read GetValueSpin;
    { Retains the named increment part; missing/retyped parts raise a contract error. }
    function GetIncrementButton: INyxButton;
    property IncrementButton: INyxButton read GetIncrementButton;
  end;

  { Specialized segmented-control contract; inherited properties retain the ChoiceControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxSegmentedControl = interface(INyxChoiceControl)
    ['{737A7921-4621-4C6F-8C01-0200DB8FC393}']
    function WithText(const AText: TNyxText): INyxSegmentedControl;
    { Retains the named day part; missing/retyped parts raise a contract error. }
    function GetDayButton: INyxButton;
    property DayButton: INyxButton read GetDayButton;
    { Retains the named week part; missing/retyped parts raise a contract error. }
    function GetWeekButton: INyxButton;
    property WeekButton: INyxButton read GetWeekButton;
    { Retains the named month part; missing/retyped parts raise a contract error. }
    function GetMonthButton: INyxButton;
    property MonthButton: INyxButton read GetMonthButton;
  end;

  { Default managed segmented-control implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSegmentedControl = class(TNyxChoiceControl, INyxSegmentedControl)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSegmentedControl;
    { Retains the named day part; missing/retyped parts raise a contract error. }
    function GetDayButton: INyxButton;
    property DayButton: INyxButton read GetDayButton;
    { Retains the named week part; missing/retyped parts raise a contract error. }
    function GetWeekButton: INyxButton;
    property WeekButton: INyxButton read GetWeekButton;
    { Retains the named month part; missing/retyped parts raise a contract error. }
    function GetMonthButton: INyxButton;
    property MonthButton: INyxButton read GetMonthButton;
  end;

  { Specialized rating contract; inherited properties retain the IntegerInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxRating = interface(INyxIntegerInput)
    ['{737A7921-4621-4C6F-8C01-0200F2891050}']
    function WithText(const AText: TNyxText): INyxRating;
    { Retains the named star-1 part; missing/retyped parts raise a contract error. }
    function GetStar1Button: INyxButton;
    property Star1Button: INyxButton read GetStar1Button;
    { Retains the named star-2 part; missing/retyped parts raise a contract error. }
    function GetStar2Button: INyxButton;
    property Star2Button: INyxButton read GetStar2Button;
    { Retains the named star-3 part; missing/retyped parts raise a contract error. }
    function GetStar3Button: INyxButton;
    property Star3Button: INyxButton read GetStar3Button;
    { Retains the named star-4 part; missing/retyped parts raise a contract error. }
    function GetStar4Button: INyxButton;
    property Star4Button: INyxButton read GetStar4Button;
    { Retains the named star-5 part; missing/retyped parts raise a contract error. }
    function GetStar5Button: INyxButton;
    property Star5Button: INyxButton read GetStar5Button;
  end;

  { Default managed rating implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxRating = class(TNyxIntegerInput, INyxRating)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxRating;
    { Retains the named star-1 part; missing/retyped parts raise a contract error. }
    function GetStar1Button: INyxButton;
    property Star1Button: INyxButton read GetStar1Button;
    { Retains the named star-2 part; missing/retyped parts raise a contract error. }
    function GetStar2Button: INyxButton;
    property Star2Button: INyxButton read GetStar2Button;
    { Retains the named star-3 part; missing/retyped parts raise a contract error. }
    function GetStar3Button: INyxButton;
    property Star3Button: INyxButton read GetStar3Button;
    { Retains the named star-4 part; missing/retyped parts raise a contract error. }
    function GetStar4Button: INyxButton;
    property Star4Button: INyxButton read GetStar4Button;
    { Retains the named star-5 part; missing/retyped parts raise a contract error. }
    function GetStar5Button: INyxButton;
    property Star5Button: INyxButton read GetStar5Button;
  end;

  { Specialized data-toolbar contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxDataToolbar = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200B63B69D3}']
    function WithText(const AText: TNyxText): INyxDataToolbar;
    { Retains the named search part; missing/retyped parts raise a contract error. }
    function GetSearchInput: INyxInput;
    property SearchInput: INyxInput read GetSearchInput;
    { Retains the named sort part; missing/retyped parts raise a contract error. }
    function GetSortSelect: INyxSelect;
    property SortSelect: INyxSelect read GetSortSelect;
    { Retains the named create part; missing/retyped parts raise a contract error. }
    function GetCreateButton: INyxButton;
    property CreateButton: INyxButton read GetCreateButton;
    { Retains the named refresh part; missing/retyped parts raise a contract error. }
    function GetRefreshButton: INyxButton;
    property RefreshButton: INyxButton read GetRefreshButton;
  end;

  { Default managed data-toolbar implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxDataToolbar = class(TNyxLayoutControl, INyxDataToolbar)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxDataToolbar;
    { Retains the named search part; missing/retyped parts raise a contract error. }
    function GetSearchInput: INyxInput;
    property SearchInput: INyxInput read GetSearchInput;
    { Retains the named sort part; missing/retyped parts raise a contract error. }
    function GetSortSelect: INyxSelect;
    property SortSelect: INyxSelect read GetSortSelect;
    { Retains the named create part; missing/retyped parts raise a contract error. }
    function GetCreateButton: INyxButton;
    property CreateButton: INyxButton read GetCreateButton;
    { Retains the named refresh part; missing/retyped parts raise a contract error. }
    function GetRefreshButton: INyxButton;
    property RefreshButton: INyxButton read GetRefreshButton;
  end;

  { Specialized filter-bar contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxFilterBar = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-020061795917}']
    function WithText(const AText: TNyxText): INyxFilterBar;
    { Retains the named status part; missing/retyped parts raise a contract error. }
    function GetStatusSelect: INyxSelect;
    property StatusSelect: INyxSelect read GetStatusSelect;
    { Retains the named query part; missing/retyped parts raise a contract error. }
    function GetQueryInput: INyxInput;
    property QueryInput: INyxInput read GetQueryInput;
    { Retains the named apply part; missing/retyped parts raise a contract error. }
    function GetApplyButton: INyxButton;
    property ApplyButton: INyxButton read GetApplyButton;
    { Retains the named reset part; missing/retyped parts raise a contract error. }
    function GetResetButton: INyxButton;
    property ResetButton: INyxButton read GetResetButton;
  end;

  { Default managed filter-bar implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxFilterBar = class(TNyxLayoutControl, INyxFilterBar)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxFilterBar;
    { Retains the named status part; missing/retyped parts raise a contract error. }
    function GetStatusSelect: INyxSelect;
    property StatusSelect: INyxSelect read GetStatusSelect;
    { Retains the named query part; missing/retyped parts raise a contract error. }
    function GetQueryInput: INyxInput;
    property QueryInput: INyxInput read GetQueryInput;
    { Retains the named apply part; missing/retyped parts raise a contract error. }
    function GetApplyButton: INyxButton;
    property ApplyButton: INyxButton read GetApplyButton;
    { Retains the named reset part; missing/retyped parts raise a contract error. }
    function GetResetButton: INyxButton;
    property ResetButton: INyxButton read GetResetButton;
  end;

  { Specialized pagination contract; inherited properties retain the IntegerInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxPagination = interface(INyxIntegerInput)
    ['{737A7921-4621-4C6F-8C01-020046C013C9}']
    function WithText(const AText: TNyxText): INyxPagination;
    { Retains the named previous part; missing/retyped parts raise a contract error. }
    function GetPreviousButton: INyxButton;
    property PreviousButton: INyxButton read GetPreviousButton;
    { Retains the named page part; missing/retyped parts raise a contract error. }
    function GetPageSpin: INyxSpin;
    property PageSpin: INyxSpin read GetPageSpin;
    { Retains the named total part; missing/retyped parts raise a contract error. }
    function GetTotalLabel: INyxLabel;
    property TotalLabel: INyxLabel read GetTotalLabel;
    { Retains the named next part; missing/retyped parts raise a contract error. }
    function GetNextButton: INyxButton;
    property NextButton: INyxButton read GetNextButton;
  end;

  { Default managed pagination implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxPagination = class(TNyxIntegerInput, INyxPagination)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxPagination;
    { Retains the named previous part; missing/retyped parts raise a contract error. }
    function GetPreviousButton: INyxButton;
    property PreviousButton: INyxButton read GetPreviousButton;
    { Retains the named page part; missing/retyped parts raise a contract error. }
    function GetPageSpin: INyxSpin;
    property PageSpin: INyxSpin read GetPageSpin;
    { Retains the named total part; missing/retyped parts raise a contract error. }
    function GetTotalLabel: INyxLabel;
    property TotalLabel: INyxLabel read GetTotalLabel;
    { Retains the named next part; missing/retyped parts raise a contract error. }
    function GetNextButton: INyxButton;
    property NextButton: INyxButton read GetNextButton;
  end;

  { Specialized breadcrumbs contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxBreadcrumbs = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02005F2F8F2D}']
    function WithText(const AText: TNyxText): INyxBreadcrumbs;
    { Retains the named home part; missing/retyped parts raise a contract error. }
    function GetHomeButton: INyxButton;
    property HomeButton: INyxButton read GetHomeButton;
    { Retains the named separator part; missing/retyped parts raise a contract error. }
    function GetSeparatorLabel: INyxLabel;
    property SeparatorLabel: INyxLabel read GetSeparatorLabel;
    { Retains the named section part; missing/retyped parts raise a contract error. }
    function GetSectionButton: INyxButton;
    property SectionButton: INyxButton read GetSectionButton;
    { Retains the named current part; missing/retyped parts raise a contract error. }
    function GetCurrentLabel: INyxLabel;
    property CurrentLabel: INyxLabel read GetCurrentLabel;
  end;

  { Default managed breadcrumbs implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxBreadcrumbs = class(TNyxLayoutControl, INyxBreadcrumbs)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxBreadcrumbs;
    { Retains the named home part; missing/retyped parts raise a contract error. }
    function GetHomeButton: INyxButton;
    property HomeButton: INyxButton read GetHomeButton;
    { Retains the named separator part; missing/retyped parts raise a contract error. }
    function GetSeparatorLabel: INyxLabel;
    property SeparatorLabel: INyxLabel read GetSeparatorLabel;
    { Retains the named section part; missing/retyped parts raise a contract error. }
    function GetSectionButton: INyxButton;
    property SectionButton: INyxButton read GetSectionButton;
    { Retains the named current part; missing/retyped parts raise a contract error. }
    function GetCurrentLabel: INyxLabel;
    property CurrentLabel: INyxLabel read GetCurrentLabel;
  end;

  { Specialized sidebar-nav contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxSidebarNav = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02003106140F}']
    function WithText(const AText: TNyxText): INyxSidebarNav;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named overview part; missing/retyped parts raise a contract error. }
    function GetOverviewButton: INyxButton;
    property OverviewButton: INyxButton read GetOverviewButton;
    { Retains the named projects part; missing/retyped parts raise a contract error. }
    function GetProjectsButton: INyxButton;
    property ProjectsButton: INyxButton read GetProjectsButton;
    { Retains the named settings part; missing/retyped parts raise a contract error. }
    function GetSettingsButton: INyxButton;
    property SettingsButton: INyxButton read GetSettingsButton;
  end;

  { Default managed sidebar-nav implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSidebarNav = class(TNyxLayoutControl, INyxSidebarNav)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSidebarNav;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named overview part; missing/retyped parts raise a contract error. }
    function GetOverviewButton: INyxButton;
    property OverviewButton: INyxButton read GetOverviewButton;
    { Retains the named projects part; missing/retyped parts raise a contract error. }
    function GetProjectsButton: INyxButton;
    property ProjectsButton: INyxButton read GetProjectsButton;
    { Retains the named settings part; missing/retyped parts raise a contract error. }
    function GetSettingsButton: INyxButton;
    property SettingsButton: INyxButton read GetSettingsButton;
  end;

  { Specialized master-detail contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxMasterDetail = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-020069F8180B}']
    function WithText(const AText: TNyxText): INyxMasterDetail;
    { Retains the named master part; missing/retyped parts raise a contract error. }
    function GetMasterList: INyxList;
    property MasterList: INyxList read GetMasterList;
    { Retains the named detail part; missing/retyped parts raise a contract error. }
    function GetDetailColumn: INyxColumn;
    property DetailColumn: INyxColumn read GetDetailColumn;
    { Retains the named detail/title part; missing/retyped parts raise a contract error. }
    function GetDetailTitleHeading: INyxHeading;
    property DetailTitleHeading: INyxHeading read GetDetailTitleHeading;
    { Retains the named detail/description part; missing/retyped parts raise a contract error. }
    function GetDetailDescriptionLabel: INyxLabel;
    property DetailDescriptionLabel: INyxLabel read GetDetailDescriptionLabel;
    { Retains the named detail/edit part; missing/retyped parts raise a contract error. }
    function GetDetailEditButton: INyxButton;
    property DetailEditButton: INyxButton read GetDetailEditButton;
  end;

  { Default managed master-detail implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxMasterDetail = class(TNyxLayoutControl, INyxMasterDetail)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxMasterDetail;
    { Retains the named master part; missing/retyped parts raise a contract error. }
    function GetMasterList: INyxList;
    property MasterList: INyxList read GetMasterList;
    { Retains the named detail part; missing/retyped parts raise a contract error. }
    function GetDetailColumn: INyxColumn;
    property DetailColumn: INyxColumn read GetDetailColumn;
    { Retains the named detail/title part; missing/retyped parts raise a contract error. }
    function GetDetailTitleHeading: INyxHeading;
    property DetailTitleHeading: INyxHeading read GetDetailTitleHeading;
    { Retains the named detail/description part; missing/retyped parts raise a contract error. }
    function GetDetailDescriptionLabel: INyxLabel;
    property DetailDescriptionLabel: INyxLabel read GetDetailDescriptionLabel;
    { Retains the named detail/edit part; missing/retyped parts raise a contract error. }
    function GetDetailEditButton: INyxButton;
    property DetailEditButton: INyxButton read GetDetailEditButton;
  end;

  { Specialized list-card contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxListCard = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200DEB1165A}']
    function WithText(const AText: TNyxText): INyxListCard;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named list part; missing/retyped parts raise a contract error. }
    function GetList: INyxList;
    property List: INyxList read GetList;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/primary part; missing/retyped parts raise a contract error. }
    function GetActionsPrimaryButton: INyxButton;
    property ActionsPrimaryButton: INyxButton read GetActionsPrimaryButton;
  end;

  { Default managed list-card implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxListCard = class(TNyxLayoutControl, INyxListCard)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxListCard;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named list part; missing/retyped parts raise a contract error. }
    function GetList: INyxList;
    property List: INyxList read GetList;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/primary part; missing/retyped parts raise a contract error. }
    function GetActionsPrimaryButton: INyxButton;
    property ActionsPrimaryButton: INyxButton read GetActionsPrimaryButton;
  end;

  { Specialized data-card contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxDataCard = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-020059F7D996}']
    function WithText(const AText: TNyxText): INyxDataCard;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named table part; missing/retyped parts raise a contract error. }
    function GetTable: INyxTable;
    property Table: INyxTable read GetTable;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/export part; missing/retyped parts raise a contract error. }
    function GetActionsExportButton: INyxButton;
    property ActionsExportButton: INyxButton read GetActionsExportButton;
  end;

  { Default managed data-card implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxDataCard = class(TNyxLayoutControl, INyxDataCard)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxDataCard;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named table part; missing/retyped parts raise a contract error. }
    function GetTable: INyxTable;
    property Table: INyxTable read GetTable;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/export part; missing/retyped parts raise a contract error. }
    function GetActionsExportButton: INyxButton;
    property ActionsExportButton: INyxButton read GetActionsExportButton;
  end;

  { Specialized kanban-board contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxKanbanBoard = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02000304E707}']
    function WithText(const AText: TNyxText): INyxKanbanBoard;
    { Retains the named lane-0 part; missing/retyped parts raise a contract error. }
    function GetLane0Column: INyxColumn;
    property Lane0Column: INyxColumn read GetLane0Column;
    { Retains the named lane-0/title part; missing/retyped parts raise a contract error. }
    function GetLane0TitleHeading: INyxHeading;
    property Lane0TitleHeading: INyxHeading read GetLane0TitleHeading;
    { Retains the named lane-0/items part; missing/retyped parts raise a contract error. }
    function GetLane0ItemsList: INyxList;
    property Lane0ItemsList: INyxList read GetLane0ItemsList;
    { Retains the named lane-0/add part; missing/retyped parts raise a contract error. }
    function GetLane0AddButton: INyxButton;
    property Lane0AddButton: INyxButton read GetLane0AddButton;
    { Retains the named lane-1 part; missing/retyped parts raise a contract error. }
    function GetLane1Column: INyxColumn;
    property Lane1Column: INyxColumn read GetLane1Column;
    { Retains the named lane-1/title part; missing/retyped parts raise a contract error. }
    function GetLane1TitleHeading: INyxHeading;
    property Lane1TitleHeading: INyxHeading read GetLane1TitleHeading;
    { Retains the named lane-1/items part; missing/retyped parts raise a contract error. }
    function GetLane1ItemsList: INyxList;
    property Lane1ItemsList: INyxList read GetLane1ItemsList;
    { Retains the named lane-1/add part; missing/retyped parts raise a contract error. }
    function GetLane1AddButton: INyxButton;
    property Lane1AddButton: INyxButton read GetLane1AddButton;
    { Retains the named lane-2 part; missing/retyped parts raise a contract error. }
    function GetLane2Column: INyxColumn;
    property Lane2Column: INyxColumn read GetLane2Column;
    { Retains the named lane-2/title part; missing/retyped parts raise a contract error. }
    function GetLane2TitleHeading: INyxHeading;
    property Lane2TitleHeading: INyxHeading read GetLane2TitleHeading;
    { Retains the named lane-2/items part; missing/retyped parts raise a contract error. }
    function GetLane2ItemsList: INyxList;
    property Lane2ItemsList: INyxList read GetLane2ItemsList;
    { Retains the named lane-2/add part; missing/retyped parts raise a contract error. }
    function GetLane2AddButton: INyxButton;
    property Lane2AddButton: INyxButton read GetLane2AddButton;
  end;

  { Default managed kanban-board implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxKanbanBoard = class(TNyxLayoutControl, INyxKanbanBoard)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxKanbanBoard;
    { Retains the named lane-0 part; missing/retyped parts raise a contract error. }
    function GetLane0Column: INyxColumn;
    property Lane0Column: INyxColumn read GetLane0Column;
    { Retains the named lane-0/title part; missing/retyped parts raise a contract error. }
    function GetLane0TitleHeading: INyxHeading;
    property Lane0TitleHeading: INyxHeading read GetLane0TitleHeading;
    { Retains the named lane-0/items part; missing/retyped parts raise a contract error. }
    function GetLane0ItemsList: INyxList;
    property Lane0ItemsList: INyxList read GetLane0ItemsList;
    { Retains the named lane-0/add part; missing/retyped parts raise a contract error. }
    function GetLane0AddButton: INyxButton;
    property Lane0AddButton: INyxButton read GetLane0AddButton;
    { Retains the named lane-1 part; missing/retyped parts raise a contract error. }
    function GetLane1Column: INyxColumn;
    property Lane1Column: INyxColumn read GetLane1Column;
    { Retains the named lane-1/title part; missing/retyped parts raise a contract error. }
    function GetLane1TitleHeading: INyxHeading;
    property Lane1TitleHeading: INyxHeading read GetLane1TitleHeading;
    { Retains the named lane-1/items part; missing/retyped parts raise a contract error. }
    function GetLane1ItemsList: INyxList;
    property Lane1ItemsList: INyxList read GetLane1ItemsList;
    { Retains the named lane-1/add part; missing/retyped parts raise a contract error. }
    function GetLane1AddButton: INyxButton;
    property Lane1AddButton: INyxButton read GetLane1AddButton;
    { Retains the named lane-2 part; missing/retyped parts raise a contract error. }
    function GetLane2Column: INyxColumn;
    property Lane2Column: INyxColumn read GetLane2Column;
    { Retains the named lane-2/title part; missing/retyped parts raise a contract error. }
    function GetLane2TitleHeading: INyxHeading;
    property Lane2TitleHeading: INyxHeading read GetLane2TitleHeading;
    { Retains the named lane-2/items part; missing/retyped parts raise a contract error. }
    function GetLane2ItemsList: INyxList;
    property Lane2ItemsList: INyxList read GetLane2ItemsList;
    { Retains the named lane-2/add part; missing/retyped parts raise a contract error. }
    function GetLane2AddButton: INyxButton;
    property Lane2AddButton: INyxButton read GetLane2AddButton;
  end;

  { Specialized timeline contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxTimeline = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02000B0B62D6}']
    function WithText(const AText: TNyxText): INyxTimeline;
    { Retains the named entry-1 part; missing/retyped parts raise a contract error. }
    function GetEntry1Row: INyxRow;
    property Entry1Row: INyxRow read GetEntry1Row;
    { Retains the named entry-1/time part; missing/retyped parts raise a contract error. }
    function GetEntry1TimeBadge: INyxBadge;
    property Entry1TimeBadge: INyxBadge read GetEntry1TimeBadge;
    { Retains the named entry-1/content part; missing/retyped parts raise a contract error. }
    function GetEntry1ContentColumn: INyxColumn;
    property Entry1ContentColumn: INyxColumn read GetEntry1ContentColumn;
    { Retains the named entry-1/content/title part; missing/retyped parts raise a contract error. }
    function GetEntry1ContentTitleHeading: INyxHeading;
    property Entry1ContentTitleHeading: INyxHeading read GetEntry1ContentTitleHeading;
    { Retains the named entry-1/content/description part; missing/retyped parts raise a contract error. }
    function GetEntry1ContentDescriptionLabel: INyxLabel;
    property Entry1ContentDescriptionLabel: INyxLabel read GetEntry1ContentDescriptionLabel;
    { Retains the named entry-2 part; missing/retyped parts raise a contract error. }
    function GetEntry2Row: INyxRow;
    property Entry2Row: INyxRow read GetEntry2Row;
    { Retains the named entry-2/time part; missing/retyped parts raise a contract error. }
    function GetEntry2TimeBadge: INyxBadge;
    property Entry2TimeBadge: INyxBadge read GetEntry2TimeBadge;
    { Retains the named entry-2/content part; missing/retyped parts raise a contract error. }
    function GetEntry2ContentColumn: INyxColumn;
    property Entry2ContentColumn: INyxColumn read GetEntry2ContentColumn;
    { Retains the named entry-2/content/title part; missing/retyped parts raise a contract error. }
    function GetEntry2ContentTitleHeading: INyxHeading;
    property Entry2ContentTitleHeading: INyxHeading read GetEntry2ContentTitleHeading;
    { Retains the named entry-2/content/description part; missing/retyped parts raise a contract error. }
    function GetEntry2ContentDescriptionLabel: INyxLabel;
    property Entry2ContentDescriptionLabel: INyxLabel read GetEntry2ContentDescriptionLabel;
    { Retains the named entry-3 part; missing/retyped parts raise a contract error. }
    function GetEntry3Row: INyxRow;
    property Entry3Row: INyxRow read GetEntry3Row;
    { Retains the named entry-3/time part; missing/retyped parts raise a contract error. }
    function GetEntry3TimeBadge: INyxBadge;
    property Entry3TimeBadge: INyxBadge read GetEntry3TimeBadge;
    { Retains the named entry-3/content part; missing/retyped parts raise a contract error. }
    function GetEntry3ContentColumn: INyxColumn;
    property Entry3ContentColumn: INyxColumn read GetEntry3ContentColumn;
    { Retains the named entry-3/content/title part; missing/retyped parts raise a contract error. }
    function GetEntry3ContentTitleHeading: INyxHeading;
    property Entry3ContentTitleHeading: INyxHeading read GetEntry3ContentTitleHeading;
    { Retains the named entry-3/content/description part; missing/retyped parts raise a contract error. }
    function GetEntry3ContentDescriptionLabel: INyxLabel;
    property Entry3ContentDescriptionLabel: INyxLabel read GetEntry3ContentDescriptionLabel;
  end;

  { Default managed timeline implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxTimeline = class(TNyxLayoutControl, INyxTimeline)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxTimeline;
    { Retains the named entry-1 part; missing/retyped parts raise a contract error. }
    function GetEntry1Row: INyxRow;
    property Entry1Row: INyxRow read GetEntry1Row;
    { Retains the named entry-1/time part; missing/retyped parts raise a contract error. }
    function GetEntry1TimeBadge: INyxBadge;
    property Entry1TimeBadge: INyxBadge read GetEntry1TimeBadge;
    { Retains the named entry-1/content part; missing/retyped parts raise a contract error. }
    function GetEntry1ContentColumn: INyxColumn;
    property Entry1ContentColumn: INyxColumn read GetEntry1ContentColumn;
    { Retains the named entry-1/content/title part; missing/retyped parts raise a contract error. }
    function GetEntry1ContentTitleHeading: INyxHeading;
    property Entry1ContentTitleHeading: INyxHeading read GetEntry1ContentTitleHeading;
    { Retains the named entry-1/content/description part; missing/retyped parts raise a contract error. }
    function GetEntry1ContentDescriptionLabel: INyxLabel;
    property Entry1ContentDescriptionLabel: INyxLabel read GetEntry1ContentDescriptionLabel;
    { Retains the named entry-2 part; missing/retyped parts raise a contract error. }
    function GetEntry2Row: INyxRow;
    property Entry2Row: INyxRow read GetEntry2Row;
    { Retains the named entry-2/time part; missing/retyped parts raise a contract error. }
    function GetEntry2TimeBadge: INyxBadge;
    property Entry2TimeBadge: INyxBadge read GetEntry2TimeBadge;
    { Retains the named entry-2/content part; missing/retyped parts raise a contract error. }
    function GetEntry2ContentColumn: INyxColumn;
    property Entry2ContentColumn: INyxColumn read GetEntry2ContentColumn;
    { Retains the named entry-2/content/title part; missing/retyped parts raise a contract error. }
    function GetEntry2ContentTitleHeading: INyxHeading;
    property Entry2ContentTitleHeading: INyxHeading read GetEntry2ContentTitleHeading;
    { Retains the named entry-2/content/description part; missing/retyped parts raise a contract error. }
    function GetEntry2ContentDescriptionLabel: INyxLabel;
    property Entry2ContentDescriptionLabel: INyxLabel read GetEntry2ContentDescriptionLabel;
    { Retains the named entry-3 part; missing/retyped parts raise a contract error. }
    function GetEntry3Row: INyxRow;
    property Entry3Row: INyxRow read GetEntry3Row;
    { Retains the named entry-3/time part; missing/retyped parts raise a contract error. }
    function GetEntry3TimeBadge: INyxBadge;
    property Entry3TimeBadge: INyxBadge read GetEntry3TimeBadge;
    { Retains the named entry-3/content part; missing/retyped parts raise a contract error. }
    function GetEntry3ContentColumn: INyxColumn;
    property Entry3ContentColumn: INyxColumn read GetEntry3ContentColumn;
    { Retains the named entry-3/content/title part; missing/retyped parts raise a contract error. }
    function GetEntry3ContentTitleHeading: INyxHeading;
    property Entry3ContentTitleHeading: INyxHeading read GetEntry3ContentTitleHeading;
    { Retains the named entry-3/content/description part; missing/retyped parts raise a contract error. }
    function GetEntry3ContentDescriptionLabel: INyxLabel;
    property Entry3ContentDescriptionLabel: INyxLabel read GetEntry3ContentDescriptionLabel;
  end;

  { Specialized stat-card contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxStatCard = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02000C1E68DC}']
    function WithText(const AText: TNyxText): INyxStatCard;
    { Retains the named label part; missing/retyped parts raise a contract error. }
    function GetCaptionLabel: INyxLabel;
    property CaptionLabel: INyxLabel read GetCaptionLabel;
    { Retains the named value part; missing/retyped parts raise a contract error. }
    function GetValueHeading: INyxHeading;
    property ValueHeading: INyxHeading read GetValueHeading;
    { Retains the named trend part; missing/retyped parts raise a contract error. }
    function GetTrendBadge: INyxBadge;
    property TrendBadge: INyxBadge read GetTrendBadge;
  end;

  { Default managed stat-card implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxStatCard = class(TNyxLayoutControl, INyxStatCard)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxStatCard;
    { Retains the named label part; missing/retyped parts raise a contract error. }
    function GetCaptionLabel: INyxLabel;
    property CaptionLabel: INyxLabel read GetCaptionLabel;
    { Retains the named value part; missing/retyped parts raise a contract error. }
    function GetValueHeading: INyxHeading;
    property ValueHeading: INyxHeading read GetValueHeading;
    { Retains the named trend part; missing/retyped parts raise a contract error. }
    function GetTrendBadge: INyxBadge;
    property TrendBadge: INyxBadge read GetTrendBadge;
  end;

  { Specialized metric-grid contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxMetricGrid = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02009329F0C4}']
    function WithText(const AText: TNyxText): INyxMetricGrid;
    { Retains the named metric-1 part; missing/retyped parts raise a contract error. }
    function GetMetric1Column: INyxColumn;
    property Metric1Column: INyxColumn read GetMetric1Column;
    { Retains the named metric-1/label part; missing/retyped parts raise a contract error. }
    function GetMetric1Label: INyxLabel;
    property Metric1Label: INyxLabel read GetMetric1Label;
    { Retains the named metric-1/value part; missing/retyped parts raise a contract error. }
    function GetMetric1ValueHeading: INyxHeading;
    property Metric1ValueHeading: INyxHeading read GetMetric1ValueHeading;
    { Retains the named metric-1/progress part; missing/retyped parts raise a contract error. }
    function GetMetric1Progress: INyxProgress;
    property Metric1Progress: INyxProgress read GetMetric1Progress;
    { Retains the named metric-2 part; missing/retyped parts raise a contract error. }
    function GetMetric2Column: INyxColumn;
    property Metric2Column: INyxColumn read GetMetric2Column;
    { Retains the named metric-2/label part; missing/retyped parts raise a contract error. }
    function GetMetric2Label: INyxLabel;
    property Metric2Label: INyxLabel read GetMetric2Label;
    { Retains the named metric-2/value part; missing/retyped parts raise a contract error. }
    function GetMetric2ValueHeading: INyxHeading;
    property Metric2ValueHeading: INyxHeading read GetMetric2ValueHeading;
    { Retains the named metric-2/progress part; missing/retyped parts raise a contract error. }
    function GetMetric2Progress: INyxProgress;
    property Metric2Progress: INyxProgress read GetMetric2Progress;
    { Retains the named metric-3 part; missing/retyped parts raise a contract error. }
    function GetMetric3Column: INyxColumn;
    property Metric3Column: INyxColumn read GetMetric3Column;
    { Retains the named metric-3/label part; missing/retyped parts raise a contract error. }
    function GetMetric3Label: INyxLabel;
    property Metric3Label: INyxLabel read GetMetric3Label;
    { Retains the named metric-3/value part; missing/retyped parts raise a contract error. }
    function GetMetric3ValueHeading: INyxHeading;
    property Metric3ValueHeading: INyxHeading read GetMetric3ValueHeading;
    { Retains the named metric-3/progress part; missing/retyped parts raise a contract error. }
    function GetMetric3Progress: INyxProgress;
    property Metric3Progress: INyxProgress read GetMetric3Progress;
  end;

  { Default managed metric-grid implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxMetricGrid = class(TNyxLayoutControl, INyxMetricGrid)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxMetricGrid;
    { Retains the named metric-1 part; missing/retyped parts raise a contract error. }
    function GetMetric1Column: INyxColumn;
    property Metric1Column: INyxColumn read GetMetric1Column;
    { Retains the named metric-1/label part; missing/retyped parts raise a contract error. }
    function GetMetric1Label: INyxLabel;
    property Metric1Label: INyxLabel read GetMetric1Label;
    { Retains the named metric-1/value part; missing/retyped parts raise a contract error. }
    function GetMetric1ValueHeading: INyxHeading;
    property Metric1ValueHeading: INyxHeading read GetMetric1ValueHeading;
    { Retains the named metric-1/progress part; missing/retyped parts raise a contract error. }
    function GetMetric1Progress: INyxProgress;
    property Metric1Progress: INyxProgress read GetMetric1Progress;
    { Retains the named metric-2 part; missing/retyped parts raise a contract error. }
    function GetMetric2Column: INyxColumn;
    property Metric2Column: INyxColumn read GetMetric2Column;
    { Retains the named metric-2/label part; missing/retyped parts raise a contract error. }
    function GetMetric2Label: INyxLabel;
    property Metric2Label: INyxLabel read GetMetric2Label;
    { Retains the named metric-2/value part; missing/retyped parts raise a contract error. }
    function GetMetric2ValueHeading: INyxHeading;
    property Metric2ValueHeading: INyxHeading read GetMetric2ValueHeading;
    { Retains the named metric-2/progress part; missing/retyped parts raise a contract error. }
    function GetMetric2Progress: INyxProgress;
    property Metric2Progress: INyxProgress read GetMetric2Progress;
    { Retains the named metric-3 part; missing/retyped parts raise a contract error. }
    function GetMetric3Column: INyxColumn;
    property Metric3Column: INyxColumn read GetMetric3Column;
    { Retains the named metric-3/label part; missing/retyped parts raise a contract error. }
    function GetMetric3Label: INyxLabel;
    property Metric3Label: INyxLabel read GetMetric3Label;
    { Retains the named metric-3/value part; missing/retyped parts raise a contract error. }
    function GetMetric3ValueHeading: INyxHeading;
    property Metric3ValueHeading: INyxHeading read GetMetric3ValueHeading;
    { Retains the named metric-3/progress part; missing/retyped parts raise a contract error. }
    function GetMetric3Progress: INyxProgress;
    property Metric3Progress: INyxProgress read GetMetric3Progress;
  end;

  { Specialized empty-state contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxEmptyState = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-020011B2A5FA}']
    function WithText(const AText: TNyxText): INyxEmptyState;
    { Retains the named illustration part; missing/retyped parts raise a contract error. }
    function GetIllustrationAvatar: INyxAvatar;
    property IllustrationAvatar: INyxAvatar read GetIllustrationAvatar;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named description part; missing/retyped parts raise a contract error. }
    function GetDescriptionLabel: INyxLabel;
    property DescriptionLabel: INyxLabel read GetDescriptionLabel;
    { Retains the named create part; missing/retyped parts raise a contract error. }
    function GetCreateButton: INyxButton;
    property CreateButton: INyxButton read GetCreateButton;
  end;

  { Default managed empty-state implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxEmptyState = class(TNyxLayoutControl, INyxEmptyState)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxEmptyState;
    { Retains the named illustration part; missing/retyped parts raise a contract error. }
    function GetIllustrationAvatar: INyxAvatar;
    property IllustrationAvatar: INyxAvatar read GetIllustrationAvatar;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named description part; missing/retyped parts raise a contract error. }
    function GetDescriptionLabel: INyxLabel;
    property DescriptionLabel: INyxLabel read GetDescriptionLabel;
    { Retains the named create part; missing/retyped parts raise a contract error. }
    function GetCreateButton: INyxButton;
    property CreateButton: INyxButton read GetCreateButton;
  end;

  { Specialized confirmation-dialog contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxConfirmationDialog = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02005CD18E77}']
    function WithText(const AText: TNyxText): INyxConfirmationDialog;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named description part; missing/retyped parts raise a contract error. }
    function GetDescriptionLabel: INyxLabel;
    property DescriptionLabel: INyxLabel read GetDescriptionLabel;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/cancel part; missing/retyped parts raise a contract error. }
    function GetActionsCancelButton: INyxButton;
    property ActionsCancelButton: INyxButton read GetActionsCancelButton;
    { Retains the named actions/confirm part; missing/retyped parts raise a contract error. }
    function GetActionsConfirmButton: INyxButton;
    property ActionsConfirmButton: INyxButton read GetActionsConfirmButton;
  end;

  { Default managed confirmation-dialog implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxConfirmationDialog = class(TNyxLayoutControl, INyxConfirmationDialog)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxConfirmationDialog;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named description part; missing/retyped parts raise a contract error. }
    function GetDescriptionLabel: INyxLabel;
    property DescriptionLabel: INyxLabel read GetDescriptionLabel;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/cancel part; missing/retyped parts raise a contract error. }
    function GetActionsCancelButton: INyxButton;
    property ActionsCancelButton: INyxButton read GetActionsCancelButton;
    { Retains the named actions/confirm part; missing/retyped parts raise a contract error. }
    function GetActionsConfirmButton: INyxButton;
    property ActionsConfirmButton: INyxButton read GetActionsConfirmButton;
  end;

  { Specialized notification-card contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxNotificationCard = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02002746D2D3}']
    function WithText(const AText: TNyxText): INyxNotificationCard;
    { Retains the named status part; missing/retyped parts raise a contract error. }
    function GetStatusBadge: INyxBadge;
    property StatusBadge: INyxBadge read GetStatusBadge;
    { Retains the named content part; missing/retyped parts raise a contract error. }
    function GetContentColumn: INyxColumn;
    property ContentColumn: INyxColumn read GetContentColumn;
    { Retains the named content/title part; missing/retyped parts raise a contract error. }
    function GetContentTitleHeading: INyxHeading;
    property ContentTitleHeading: INyxHeading read GetContentTitleHeading;
    { Retains the named content/message part; missing/retyped parts raise a contract error. }
    function GetContentMessageLabel: INyxLabel;
    property ContentMessageLabel: INyxLabel read GetContentMessageLabel;
    { Retains the named dismiss part; missing/retyped parts raise a contract error. }
    function GetDismissButton: INyxButton;
    property DismissButton: INyxButton read GetDismissButton;
  end;

  { Default managed notification-card implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxNotificationCard = class(TNyxLayoutControl, INyxNotificationCard)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxNotificationCard;
    { Retains the named status part; missing/retyped parts raise a contract error. }
    function GetStatusBadge: INyxBadge;
    property StatusBadge: INyxBadge read GetStatusBadge;
    { Retains the named content part; missing/retyped parts raise a contract error. }
    function GetContentColumn: INyxColumn;
    property ContentColumn: INyxColumn read GetContentColumn;
    { Retains the named content/title part; missing/retyped parts raise a contract error. }
    function GetContentTitleHeading: INyxHeading;
    property ContentTitleHeading: INyxHeading read GetContentTitleHeading;
    { Retains the named content/message part; missing/retyped parts raise a contract error. }
    function GetContentMessageLabel: INyxLabel;
    property ContentMessageLabel: INyxLabel read GetContentMessageLabel;
    { Retains the named dismiss part; missing/retyped parts raise a contract error. }
    function GetDismissButton: INyxButton;
    property DismissButton: INyxButton read GetDismissButton;
  end;

  { Specialized toast contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxToast = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02005A91E6D0}']
    function WithText(const AText: TNyxText): INyxToast;
    { Retains the named message part; missing/retyped parts raise a contract error. }
    function GetMessageLabel: INyxLabel;
    property MessageLabel: INyxLabel read GetMessageLabel;
    { Retains the named undo part; missing/retyped parts raise a contract error. }
    function GetUndoButton: INyxButton;
    property UndoButton: INyxButton read GetUndoButton;
    { Retains the named dismiss part; missing/retyped parts raise a contract error. }
    function GetDismissButton: INyxButton;
    property DismissButton: INyxButton read GetDismissButton;
  end;

  { Default managed toast implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxToast = class(TNyxLayoutControl, INyxToast)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxToast;
    { Retains the named message part; missing/retyped parts raise a contract error. }
    function GetMessageLabel: INyxLabel;
    property MessageLabel: INyxLabel read GetMessageLabel;
    { Retains the named undo part; missing/retyped parts raise a contract error. }
    function GetUndoButton: INyxButton;
    property UndoButton: INyxButton read GetUndoButton;
    { Retains the named dismiss part; missing/retyped parts raise a contract error. }
    function GetDismissButton: INyxButton;
    property DismissButton: INyxButton read GetDismissButton;
  end;

  { Specialized wizard-step contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxWizardStep = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200524D7DAF}']
    function WithText(const AText: TNyxText): INyxWizardStep;
    { Retains the named step part; missing/retyped parts raise a contract error. }
    function GetStepBadge: INyxBadge;
    property StepBadge: INyxBadge read GetStepBadge;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named content part; missing/retyped parts raise a contract error. }
    function GetContentColumn: INyxColumn;
    property ContentColumn: INyxColumn read GetContentColumn;
    { Retains the named content/name part; missing/retyped parts raise a contract error. }
    function GetContentNameInput: INyxInput;
    property ContentNameInput: INyxInput read GetContentNameInput;
    { Retains the named navigation part; missing/retyped parts raise a contract error. }
    function GetNavigationRow: INyxRow;
    property NavigationRow: INyxRow read GetNavigationRow;
    { Retains the named navigation/back part; missing/retyped parts raise a contract error. }
    function GetNavigationBackButton: INyxButton;
    property NavigationBackButton: INyxButton read GetNavigationBackButton;
    { Retains the named navigation/next part; missing/retyped parts raise a contract error. }
    function GetNavigationNextButton: INyxButton;
    property NavigationNextButton: INyxButton read GetNavigationNextButton;
  end;

  { Default managed wizard-step implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxWizardStep = class(TNyxLayoutControl, INyxWizardStep)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxWizardStep;
    { Retains the named step part; missing/retyped parts raise a contract error. }
    function GetStepBadge: INyxBadge;
    property StepBadge: INyxBadge read GetStepBadge;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named content part; missing/retyped parts raise a contract error. }
    function GetContentColumn: INyxColumn;
    property ContentColumn: INyxColumn read GetContentColumn;
    { Retains the named content/name part; missing/retyped parts raise a contract error. }
    function GetContentNameInput: INyxInput;
    property ContentNameInput: INyxInput read GetContentNameInput;
    { Retains the named navigation part; missing/retyped parts raise a contract error. }
    function GetNavigationRow: INyxRow;
    property NavigationRow: INyxRow read GetNavigationRow;
    { Retains the named navigation/back part; missing/retyped parts raise a contract error. }
    function GetNavigationBackButton: INyxButton;
    property NavigationBackButton: INyxButton read GetNavigationBackButton;
    { Retains the named navigation/next part; missing/retyped parts raise a contract error. }
    function GetNavigationNextButton: INyxButton;
    property NavigationNextButton: INyxButton read GetNavigationNextButton;
  end;

  { Specialized stepper contract; inherited properties retain the IntegerInput family.
    WithText returns this exact interface for further fluent composition. }
  INyxStepper = interface(INyxIntegerInput)
    ['{737A7921-4621-4C6F-8C01-0200E29AC5DE}']
    function WithText(const AText: TNyxText): INyxStepper;
    { Retains the named step-1 part; missing/retyped parts raise a contract error. }
    function GetStep1Button: INyxButton;
    property Step1Button: INyxButton read GetStep1Button;
    { Retains the named step-2 part; missing/retyped parts raise a contract error. }
    function GetStep2Button: INyxButton;
    property Step2Button: INyxButton read GetStep2Button;
    { Retains the named step-3 part; missing/retyped parts raise a contract error. }
    function GetStep3Button: INyxButton;
    property Step3Button: INyxButton read GetStep3Button;
    { Retains the named step-4 part; missing/retyped parts raise a contract error. }
    function GetStep4Button: INyxButton;
    property Step4Button: INyxButton read GetStep4Button;
  end;

  { Default managed stepper implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxStepper = class(TNyxIntegerInput, INyxStepper)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxStepper;
    { Retains the named step-1 part; missing/retyped parts raise a contract error. }
    function GetStep1Button: INyxButton;
    property Step1Button: INyxButton read GetStep1Button;
    { Retains the named step-2 part; missing/retyped parts raise a contract error. }
    function GetStep2Button: INyxButton;
    property Step2Button: INyxButton read GetStep2Button;
    { Retains the named step-3 part; missing/retyped parts raise a contract error. }
    function GetStep3Button: INyxButton;
    property Step3Button: INyxButton read GetStep3Button;
    { Retains the named step-4 part; missing/retyped parts raise a contract error. }
    function GetStep4Button: INyxButton;
    property Step4Button: INyxButton read GetStep4Button;
  end;

  { Specialized profile-card contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxProfileCard = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200D72AF91B}']
    function WithText(const AText: TNyxText): INyxProfileCard;
    { Retains the named avatar part; missing/retyped parts raise a contract error. }
    function GetAvatar: INyxAvatar;
    property Avatar: INyxAvatar read GetAvatar;
    { Retains the named name part; missing/retyped parts raise a contract error. }
    function GetNameHeading: INyxHeading;
    property NameHeading: INyxHeading read GetNameHeading;
    { Retains the named bio part; missing/retyped parts raise a contract error. }
    function GetBioLabel: INyxLabel;
    property BioLabel: INyxLabel read GetBioLabel;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/message part; missing/retyped parts raise a contract error. }
    function GetActionsMessageButton: INyxButton;
    property ActionsMessageButton: INyxButton read GetActionsMessageButton;
    { Retains the named actions/follow part; missing/retyped parts raise a contract error. }
    function GetActionsFollowButton: INyxButton;
    property ActionsFollowButton: INyxButton read GetActionsFollowButton;
  end;

  { Default managed profile-card implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxProfileCard = class(TNyxLayoutControl, INyxProfileCard)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxProfileCard;
    { Retains the named avatar part; missing/retyped parts raise a contract error. }
    function GetAvatar: INyxAvatar;
    property Avatar: INyxAvatar read GetAvatar;
    { Retains the named name part; missing/retyped parts raise a contract error. }
    function GetNameHeading: INyxHeading;
    property NameHeading: INyxHeading read GetNameHeading;
    { Retains the named bio part; missing/retyped parts raise a contract error. }
    function GetBioLabel: INyxLabel;
    property BioLabel: INyxLabel read GetBioLabel;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/message part; missing/retyped parts raise a contract error. }
    function GetActionsMessageButton: INyxButton;
    property ActionsMessageButton: INyxButton read GetActionsMessageButton;
    { Retains the named actions/follow part; missing/retyped parts raise a contract error. }
    function GetActionsFollowButton: INyxButton;
    property ActionsFollowButton: INyxButton read GetActionsFollowButton;
  end;

  { Specialized media-card contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxMediaCard = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-020074F07280}']
    function WithText(const AText: TNyxText): INyxMediaCard;
    { Retains the named media part; missing/retyped parts raise a contract error. }
    function GetMediaImage: INyxImage;
    property MediaImage: INyxImage read GetMediaImage;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named description part; missing/retyped parts raise a contract error. }
    function GetDescriptionLabel: INyxLabel;
    property DescriptionLabel: INyxLabel read GetDescriptionLabel;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/open part; missing/retyped parts raise a contract error. }
    function GetActionsOpenButton: INyxButton;
    property ActionsOpenButton: INyxButton read GetActionsOpenButton;
    { Retains the named actions/favorite part; missing/retyped parts raise a contract error. }
    function GetActionsFavoriteButton: INyxButton;
    property ActionsFavoriteButton: INyxButton read GetActionsFavoriteButton;
  end;

  { Default managed media-card implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxMediaCard = class(TNyxLayoutControl, INyxMediaCard)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxMediaCard;
    { Retains the named media part; missing/retyped parts raise a contract error. }
    function GetMediaImage: INyxImage;
    property MediaImage: INyxImage read GetMediaImage;
    { Retains the named title part; missing/retyped parts raise a contract error. }
    function GetTitleHeading: INyxHeading;
    property TitleHeading: INyxHeading read GetTitleHeading;
    { Retains the named description part; missing/retyped parts raise a contract error. }
    function GetDescriptionLabel: INyxLabel;
    property DescriptionLabel: INyxLabel read GetDescriptionLabel;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/open part; missing/retyped parts raise a contract error. }
    function GetActionsOpenButton: INyxButton;
    property ActionsOpenButton: INyxButton read GetActionsOpenButton;
    { Retains the named actions/favorite part; missing/retyped parts raise a contract error. }
    function GetActionsFavoriteButton: INyxButton;
    property ActionsFavoriteButton: INyxButton read GetActionsFavoriteButton;
  end;

  { Specialized avatar-group contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxAvatarGroup = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02000B024302}']
    function WithText(const AText: TNyxText): INyxAvatarGroup;
    { Retains the named member-1 part; missing/retyped parts raise a contract error. }
    function GetMember1Avatar: INyxAvatar;
    property Member1Avatar: INyxAvatar read GetMember1Avatar;
    { Retains the named member-2 part; missing/retyped parts raise a contract error. }
    function GetMember2Avatar: INyxAvatar;
    property Member2Avatar: INyxAvatar read GetMember2Avatar;
    { Retains the named member-3 part; missing/retyped parts raise a contract error. }
    function GetMember3Avatar: INyxAvatar;
    property Member3Avatar: INyxAvatar read GetMember3Avatar;
    { Retains the named member-4 part; missing/retyped parts raise a contract error. }
    function GetMember4Avatar: INyxAvatar;
    property Member4Avatar: INyxAvatar read GetMember4Avatar;
    { Retains the named overflow part; missing/retyped parts raise a contract error. }
    function GetOverflowLabel: INyxLabel;
    property OverflowLabel: INyxLabel read GetOverflowLabel;
  end;

  { Default managed avatar-group implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxAvatarGroup = class(TNyxLayoutControl, INyxAvatarGroup)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxAvatarGroup;
    { Retains the named member-1 part; missing/retyped parts raise a contract error. }
    function GetMember1Avatar: INyxAvatar;
    property Member1Avatar: INyxAvatar read GetMember1Avatar;
    { Retains the named member-2 part; missing/retyped parts raise a contract error. }
    function GetMember2Avatar: INyxAvatar;
    property Member2Avatar: INyxAvatar read GetMember2Avatar;
    { Retains the named member-3 part; missing/retyped parts raise a contract error. }
    function GetMember3Avatar: INyxAvatar;
    property Member3Avatar: INyxAvatar read GetMember3Avatar;
    { Retains the named member-4 part; missing/retyped parts raise a contract error. }
    function GetMember4Avatar: INyxAvatar;
    property Member4Avatar: INyxAvatar read GetMember4Avatar;
    { Retains the named overflow part; missing/retyped parts raise a contract error. }
    function GetOverflowLabel: INyxLabel;
    property OverflowLabel: INyxLabel read GetOverflowLabel;
  end;

  { Specialized comment-thread contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxCommentThread = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200CEA4015D}']
    function WithText(const AText: TNyxText): INyxCommentThread;
    { Retains the named comment-1 part; missing/retyped parts raise a contract error. }
    function GetComment1Row: INyxRow;
    property Comment1Row: INyxRow read GetComment1Row;
    { Retains the named comment-1/author part; missing/retyped parts raise a contract error. }
    function GetComment1AuthorAvatar: INyxAvatar;
    property Comment1AuthorAvatar: INyxAvatar read GetComment1AuthorAvatar;
    { Retains the named comment-1/body part; missing/retyped parts raise a contract error. }
    function GetComment1BodyColumn: INyxColumn;
    property Comment1BodyColumn: INyxColumn read GetComment1BodyColumn;
    { Retains the named comment-1/body/name part; missing/retyped parts raise a contract error. }
    function GetComment1BodyNameLabel: INyxLabel;
    property Comment1BodyNameLabel: INyxLabel read GetComment1BodyNameLabel;
    { Retains the named comment-1/body/message part; missing/retyped parts raise a contract error. }
    function GetComment1BodyMessageLabel: INyxLabel;
    property Comment1BodyMessageLabel: INyxLabel read GetComment1BodyMessageLabel;
    { Retains the named comment-2 part; missing/retyped parts raise a contract error. }
    function GetComment2Row: INyxRow;
    property Comment2Row: INyxRow read GetComment2Row;
    { Retains the named comment-2/author part; missing/retyped parts raise a contract error. }
    function GetComment2AuthorAvatar: INyxAvatar;
    property Comment2AuthorAvatar: INyxAvatar read GetComment2AuthorAvatar;
    { Retains the named comment-2/body part; missing/retyped parts raise a contract error. }
    function GetComment2BodyColumn: INyxColumn;
    property Comment2BodyColumn: INyxColumn read GetComment2BodyColumn;
    { Retains the named comment-2/body/name part; missing/retyped parts raise a contract error. }
    function GetComment2BodyNameLabel: INyxLabel;
    property Comment2BodyNameLabel: INyxLabel read GetComment2BodyNameLabel;
    { Retains the named comment-2/body/message part; missing/retyped parts raise a contract error. }
    function GetComment2BodyMessageLabel: INyxLabel;
    property Comment2BodyMessageLabel: INyxLabel read GetComment2BodyMessageLabel;
    { Retains the named reply part; missing/retyped parts raise a contract error. }
    function GetReplyMemo: INyxMemo;
    property ReplyMemo: INyxMemo read GetReplyMemo;
    { Retains the named send part; missing/retyped parts raise a contract error. }
    function GetSendButton: INyxButton;
    property SendButton: INyxButton read GetSendButton;
  end;

  { Default managed comment-thread implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxCommentThread = class(TNyxLayoutControl, INyxCommentThread)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxCommentThread;
    { Retains the named comment-1 part; missing/retyped parts raise a contract error. }
    function GetComment1Row: INyxRow;
    property Comment1Row: INyxRow read GetComment1Row;
    { Retains the named comment-1/author part; missing/retyped parts raise a contract error. }
    function GetComment1AuthorAvatar: INyxAvatar;
    property Comment1AuthorAvatar: INyxAvatar read GetComment1AuthorAvatar;
    { Retains the named comment-1/body part; missing/retyped parts raise a contract error. }
    function GetComment1BodyColumn: INyxColumn;
    property Comment1BodyColumn: INyxColumn read GetComment1BodyColumn;
    { Retains the named comment-1/body/name part; missing/retyped parts raise a contract error. }
    function GetComment1BodyNameLabel: INyxLabel;
    property Comment1BodyNameLabel: INyxLabel read GetComment1BodyNameLabel;
    { Retains the named comment-1/body/message part; missing/retyped parts raise a contract error. }
    function GetComment1BodyMessageLabel: INyxLabel;
    property Comment1BodyMessageLabel: INyxLabel read GetComment1BodyMessageLabel;
    { Retains the named comment-2 part; missing/retyped parts raise a contract error. }
    function GetComment2Row: INyxRow;
    property Comment2Row: INyxRow read GetComment2Row;
    { Retains the named comment-2/author part; missing/retyped parts raise a contract error. }
    function GetComment2AuthorAvatar: INyxAvatar;
    property Comment2AuthorAvatar: INyxAvatar read GetComment2AuthorAvatar;
    { Retains the named comment-2/body part; missing/retyped parts raise a contract error. }
    function GetComment2BodyColumn: INyxColumn;
    property Comment2BodyColumn: INyxColumn read GetComment2BodyColumn;
    { Retains the named comment-2/body/name part; missing/retyped parts raise a contract error. }
    function GetComment2BodyNameLabel: INyxLabel;
    property Comment2BodyNameLabel: INyxLabel read GetComment2BodyNameLabel;
    { Retains the named comment-2/body/message part; missing/retyped parts raise a contract error. }
    function GetComment2BodyMessageLabel: INyxLabel;
    property Comment2BodyMessageLabel: INyxLabel read GetComment2BodyMessageLabel;
    { Retains the named reply part; missing/retyped parts raise a contract error. }
    function GetReplyMemo: INyxMemo;
    property ReplyMemo: INyxMemo read GetReplyMemo;
    { Retains the named send part; missing/retyped parts raise a contract error. }
    function GetSendButton: INyxButton;
    property SendButton: INyxButton read GetSendButton;
  end;

  { Specialized property-grid contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxPropertyGrid = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200D0BF4613}']
    function WithText(const AText: TNyxText): INyxPropertyGrid;
    { Retains the named property-1 part; missing/retyped parts raise a contract error. }
    function GetProperty1Row: INyxRow;
    property Property1Row: INyxRow read GetProperty1Row;
    { Retains the named property-1/name part; missing/retyped parts raise a contract error. }
    function GetProperty1NameLabel: INyxLabel;
    property Property1NameLabel: INyxLabel read GetProperty1NameLabel;
    { Retains the named property-1/value part; missing/retyped parts raise a contract error. }
    function GetProperty1ValueInput: INyxInput;
    property Property1ValueInput: INyxInput read GetProperty1ValueInput;
    { Retains the named property-2 part; missing/retyped parts raise a contract error. }
    function GetProperty2Row: INyxRow;
    property Property2Row: INyxRow read GetProperty2Row;
    { Retains the named property-2/name part; missing/retyped parts raise a contract error. }
    function GetProperty2NameLabel: INyxLabel;
    property Property2NameLabel: INyxLabel read GetProperty2NameLabel;
    { Retains the named property-2/value part; missing/retyped parts raise a contract error. }
    function GetProperty2ValueInput: INyxInput;
    property Property2ValueInput: INyxInput read GetProperty2ValueInput;
    { Retains the named property-3 part; missing/retyped parts raise a contract error. }
    function GetProperty3Row: INyxRow;
    property Property3Row: INyxRow read GetProperty3Row;
    { Retains the named property-3/name part; missing/retyped parts raise a contract error. }
    function GetProperty3NameLabel: INyxLabel;
    property Property3NameLabel: INyxLabel read GetProperty3NameLabel;
    { Retains the named property-3/value part; missing/retyped parts raise a contract error. }
    function GetProperty3ValueInput: INyxInput;
    property Property3ValueInput: INyxInput read GetProperty3ValueInput;
  end;

  { Default managed property-grid implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxPropertyGrid = class(TNyxLayoutControl, INyxPropertyGrid)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxPropertyGrid;
    { Retains the named property-1 part; missing/retyped parts raise a contract error. }
    function GetProperty1Row: INyxRow;
    property Property1Row: INyxRow read GetProperty1Row;
    { Retains the named property-1/name part; missing/retyped parts raise a contract error. }
    function GetProperty1NameLabel: INyxLabel;
    property Property1NameLabel: INyxLabel read GetProperty1NameLabel;
    { Retains the named property-1/value part; missing/retyped parts raise a contract error. }
    function GetProperty1ValueInput: INyxInput;
    property Property1ValueInput: INyxInput read GetProperty1ValueInput;
    { Retains the named property-2 part; missing/retyped parts raise a contract error. }
    function GetProperty2Row: INyxRow;
    property Property2Row: INyxRow read GetProperty2Row;
    { Retains the named property-2/name part; missing/retyped parts raise a contract error. }
    function GetProperty2NameLabel: INyxLabel;
    property Property2NameLabel: INyxLabel read GetProperty2NameLabel;
    { Retains the named property-2/value part; missing/retyped parts raise a contract error. }
    function GetProperty2ValueInput: INyxInput;
    property Property2ValueInput: INyxInput read GetProperty2ValueInput;
    { Retains the named property-3 part; missing/retyped parts raise a contract error. }
    function GetProperty3Row: INyxRow;
    property Property3Row: INyxRow read GetProperty3Row;
    { Retains the named property-3/name part; missing/retyped parts raise a contract error. }
    function GetProperty3NameLabel: INyxLabel;
    property Property3NameLabel: INyxLabel read GetProperty3NameLabel;
    { Retains the named property-3/value part; missing/retyped parts raise a contract error. }
    function GetProperty3ValueInput: INyxInput;
    property Property3ValueInput: INyxInput read GetProperty3ValueInput;
  end;

  { Specialized command-bar contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxCommandBar = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-0200D1A63212}']
    function WithText(const AText: TNyxText): INyxCommandBar;
    { Retains the named new part; missing/retyped parts raise a contract error. }
    function GetNewButton: INyxButton;
    property NewButton: INyxButton read GetNewButton;
    { Retains the named open part; missing/retyped parts raise a contract error. }
    function GetOpenButton: INyxButton;
    property OpenButton: INyxButton read GetOpenButton;
    { Retains the named save part; missing/retyped parts raise a contract error. }
    function GetSaveButton: INyxButton;
    property SaveButton: INyxButton read GetSaveButton;
    { Retains the named spacer part; missing/retyped parts raise a contract error. }
    function GetSpacer: INyxSpacer;
    property Spacer: INyxSpacer read GetSpacer;
    { Retains the named command part; missing/retyped parts raise a contract error. }
    function GetCommandInput: INyxInput;
    property CommandInput: INyxInput read GetCommandInput;
  end;

  { Default managed command-bar implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxCommandBar = class(TNyxLayoutControl, INyxCommandBar)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxCommandBar;
    { Retains the named new part; missing/retyped parts raise a contract error. }
    function GetNewButton: INyxButton;
    property NewButton: INyxButton read GetNewButton;
    { Retains the named open part; missing/retyped parts raise a contract error. }
    function GetOpenButton: INyxButton;
    property OpenButton: INyxButton read GetOpenButton;
    { Retains the named save part; missing/retyped parts raise a contract error. }
    function GetSaveButton: INyxButton;
    property SaveButton: INyxButton read GetSaveButton;
    { Retains the named spacer part; missing/retyped parts raise a contract error. }
    function GetSpacer: INyxSpacer;
    property Spacer: INyxSpacer read GetSpacer;
    { Retains the named command part; missing/retyped parts raise a contract error. }
    function GetCommandInput: INyxInput;
    property CommandInput: INyxInput read GetCommandInput;
  end;

  { Specialized floating-action-panel contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxFloatingActionPanel = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-020056209A1F}']
    function WithText(const AText: TNyxText): INyxFloatingActionPanel;
    { Retains the named content part; missing/retyped parts raise a contract error. }
    function GetContentColumn: INyxColumn;
    property ContentColumn: INyxColumn read GetContentColumn;
    { Retains the named content/list part; missing/retyped parts raise a contract error. }
    function GetContentList: INyxList;
    property ContentList: INyxList read GetContentList;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/floating part; missing/retyped parts raise a contract error. }
    function GetActionsFloatingButton: INyxButton;
    property ActionsFloatingButton: INyxButton read GetActionsFloatingButton;
  end;

  { Default managed floating-action-panel implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxFloatingActionPanel = class(TNyxLayoutControl, INyxFloatingActionPanel)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxFloatingActionPanel;
    { Retains the named content part; missing/retyped parts raise a contract error. }
    function GetContentColumn: INyxColumn;
    property ContentColumn: INyxColumn read GetContentColumn;
    { Retains the named content/list part; missing/retyped parts raise a contract error. }
    function GetContentList: INyxList;
    property ContentList: INyxList read GetContentList;
    { Retains the named actions part; missing/retyped parts raise a contract error. }
    function GetActionsRow: INyxRow;
    property ActionsRow: INyxRow read GetActionsRow;
    { Retains the named actions/floating part; missing/retyped parts raise a contract error. }
    function GetActionsFloatingButton: INyxButton;
    property ActionsFloatingButton: INyxButton read GetActionsFloatingButton;
  end;

  { Specialized slot-override contract; inherited properties retain the LayoutControl family.
    WithText returns this exact interface for further fluent composition. }
  INyxSlotOverride = interface(INyxLayoutControl)
    ['{737A7921-4621-4C6F-8C01-02001A3ABABC}']
    function WithText(const AText: TNyxText): INyxSlotOverride;
  end;

  { Default managed slot-override implementation. Subclass for behavior or implement the interface
    independently; adapters require only the retained portable Node contract. }
  TNyxSlotOverride = class(TNyxLayoutControl, INyxSlotOverride)
  public
    constructor Create(const AID: TNyxText = '';
      AConstruction: TNyxConstruction = ncoDefault); reintroduce;
    function WithText(const AText: TNyxText): INyxSlotOverride;
  end;

