inherited fraCBNavigation: TfraCBNavigation
  Width = 309
  Height = 437
  ParentFont = True
  OnResize = FrameResize
  object tlbTree: TToolBar
    Left = 0
    Top = 0
    Width = 309
    Height = 22
    EdgeBorders = []
    Flat = True
    Images = dmInterface.ilButtons
    List = True
    TabOrder = 0
    Wrapable = False
    object pnlView: TPanel
      Left = 0
      Top = 0
      Width = 36
      Height = 22
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'View:'
      TabOrder = 1
    end
    object cmbView: TComboBoxEx
      Left = 36
      Top = 0
      Width = 175
      Height = 22
      ItemsEx = <>
      Style = csExDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ItemHeight = 16
      ParentFont = False
      TabOrder = 0
      OnChange = cmbViewChange
      Images = dmInterface.ilBrowserNodes
      DropDownCount = 12
    end
    object ToolButton1: TToolButton
      Left = 211
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 5
      Style = tbsSeparator
    end
    object tbtnBack: TToolButton
      Left = 219
      Top = 0
      Enabled = False
      ImageIndex = 15
      Style = tbsDropDown
    end
    object tbtnForward: TToolButton
      Left = 255
      Top = 0
      Enabled = False
      ImageIndex = 16
      Style = tbsDropDown
    end
  end
  object pnlFilter: TPanel
    Left = 0
    Top = 22
    Width = 309
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      309
      29)
    object Label1: TLabel
      Left = 5
      Top = 8
      Width = 23
      Height = 13
      Caption = 'Find:'
    end
    object DragFrame: TShape
      Left = 35
      Top = 3
      Width = 227
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      Pen.Color = clRed
      Visible = False
    end
    object cmbSearch: TConceptGroupComboBox
      Left = 36
      Top = 4
      Width = 225
      Height = 21
      Style = csDropDown
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      Visible = False
      OnEnter = eSearchEnter
      OnExit = eSearchExit
      OnKeyPress = eSearchKeyPress
      OnPopulate = cmbSearchPopulate
    end
    object btnSearchType: TButton
      Left = 285
      Top = 4
      Width = 22
      Height = 22
      Hint = 'Select search-type to apply to search parameter'
      Anchors = [akTop, akRight]
      Caption = 'u'
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Marlett'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = btnSearchTypeClick
    end
    object eSearch: TEdit
      Left = 36
      Top = 4
      Width = 225
      Height = 21
      Hint = 'Press '#39'Return'#39' to populate tree'
      Anchors = [akLeft, akTop, akRight]
      MaxLength = 100
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnEnter = eSearchEnter
      OnExit = eSearchExit
      OnKeyDown = eSearchKeyDown
      OnKeyPress = eSearchKeyPress
    end
    object btnGo: TButton
      Left = 263
      Top = 4
      Width = 22
      Height = 22
      Hint = 'Click to populate tree'
      Anchors = [akTop, akRight]
      Caption = '&Go'
      TabOrder = 2
      OnClick = btnGoClick
    end
  end
  object tvNav: TRapidTree
    Left = 0
    Top = 51
    Width = 309
    Height = 386
    SmoothExpandCollapse = False
    FitColumnToClientWidth = True
    NodeSelectionStyle = tnsTextOnly
    FitToHeight = False
    DoubleBuffered = False
    Align = alClient
    TransparentMode = False
    DefaultRowHeight = 16
    RowCount = 0
    FixedRows = 0
    Options = [goRangeSelect]
    ParentShowHint = False
    PopupMenu = pmTree
    ShowHint = True
    TabOrder = 2
    OnClick = tvNavClick
    OnDblClick = tvNavDblClick
    OnDrawCell = tvNavDrawCell
    OnMouseDown = tvNavMouseDown
    OnExpanding = tvNavExpanding
    OnChange = tvNavChange
    HideSelection = False
    Showlines = True
    ShowRoot = True
    ShowButtons = True
    ShowImages = True
    ShowLogic = False
    Images = dmInterface.ilBrowserNodes
    SortType = stNone
    WordWrap = False
    AutoMove = False
    ToolTips = False
    AutoExpand = False
    TooltipColor = clInfoBk
    ToolTipPause = 1000
    StatesDrawed = True
    Data = {0400000000000000}
  end
  object pmSearch: TPopupMenu
    Left = 108
    Top = 240
  end
  object pmTree: TPopupMenu
    OnPopup = pmTreePopup
    Left = 40
    Top = 240
    object pmTreeBack: TMenuItem
      Caption = '&Back'
      Hint = 'Back'
      ImageIndex = 0
    end
    object pmTreeForward: TMenuItem
      Caption = '&Forward'
      Hint = 'Forward'
      ImageIndex = 1
    end
    object pmSep1: TMenuItem
      Caption = '-'
    end
    object pmExpandList: TMenuItem
      Caption = 'E&xpand List'
      Enabled = False
      Hint = 'Expand for a filtered search'
    end
    object pmTreeNewWindow: TMenuItem
      Caption = 'Open in &New window'
      Hint = 'Open item in new window'
      ImageIndex = 2
    end
    object pmTreeRefresh: TMenuItem
      Caption = '&Refresh'
      Hint = 'Refresh'
      ImageIndex = 3
    end
    object pmTreeConvertSep: TMenuItem
      Caption = '-'
      Visible = False
    end
    object pmNavigate: TMenuItem
      Caption = 'Navigate'
      Visible = False
      OnClick = pmNavigateClick
    end
    object pmTreeDuplicateValuation: TMenuItem
      Caption = 'Duplicate Valuation'
      Visible = False
      OnClick = pmTreeDuplicateValuationClick
    end
  end
end
