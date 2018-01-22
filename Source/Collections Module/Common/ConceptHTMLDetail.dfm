inherited fraConceptHTMLDetail: TfraConceptHTMLDetail
  Width = 353
  Height = 308
  inherited bvlBorder: TBevel
    Width = 345
    Height = 300
  end
  object HTMLViewer: THTMLViewer
    Left = 4
    Top = 4
    Width = 344
    Height = 300
    OnHotSpotClick = HTMLViewerHotSpotClick
    TabOrder = 0
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefBackground = clWindow
    BorderStyle = htFocused
    HistoryMaxCount = 0
    DefFontName = 'Arial'
    DefPreFontName = 'Courier New'
    DefFontSize = 10
    NoSelect = False
    ScrollBars = ssVertical
    CharSet = DEFAULT_CHARSET
    PrintMarginLeft = 2.000000000000000000
    PrintMarginRight = 2.000000000000000000
    PrintMarginTop = 2.000000000000000000
    PrintMarginBottom = 2.000000000000000000
    PrintScale = 1.000000000000000000
  end
end
