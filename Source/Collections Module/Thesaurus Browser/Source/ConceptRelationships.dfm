inherited fraConceptRelationships: TfraConceptRelationships
  Width = 585
  Height = 544
  OnResize = FrameResize
  inherited bvlBorder: TBevel
    Width = 577
    Height = 536
  end
  object lblRelationships: TLabel
    Left = 8
    Top = 8
    Width = 81
    Height = 13
    Caption = 'Relationships for '
  end
  object lblSelectedNode: TLabel
    Left = 92
    Top = 8
    Width = 3
    Height = 13
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object tvRelationships: TRapidTree
    Left = 9
    Top = 22
    Width = 565
    Height = 491
    Hint = 'Drag item to add to diagram'
    SmoothExpandCollapse = False
    FitToHeight = False
    DoubleBuffered = False
    LineColor = clScrollBar
    Anchors = [akLeft, akTop, akRight, akBottom]
    TransparentMode = False
    DefaultColWidth = 398
    DragMode = dmAutomatic
    RowCount = 0
    FixedRows = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goHorzLine]
    ParentFont = False
    TabOrder = 0
    OnDblClick = tvRelationshipsDblClick
    OnDrawCell = tvRelationshipsDrawCell
    HideSelection = False
    Showlines = False
    ShowRoot = True
    ShowButtons = True
    ShowImages = False
    ShowLogic = False
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
  object chkIncludeSynonyms: TCheckBox
    Left = 8
    Top = 518
    Width = 185
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Include synonyms of related items'
    TabOrder = 1
    OnClick = chkIncludeSynonymsClick
  end
end
