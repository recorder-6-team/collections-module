inherited fraSearchTerms: TfraSearchTerms
  Width = 548
  Height = 335
  inherited bvlBorder: TBevel
    Width = 540
    Height = 311
    Anchors = [akLeft, akTop, akRight]
  end
  object lblAutomaticTerms: TLabel
    Left = 24
    Top = 16
    Width = 119
    Height = 13
    Caption = 'Automatic Search Terms:'
  end
  object lblAdditionalTerms: TLabel
    Left = 24
    Top = 144
    Width = 118
    Height = 13
    Caption = 'Additional Search Terms:'
  end
  object mmAutomaticTerms: TMemo
    Left = 24
    Top = 40
    Width = 504
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    TabOrder = 0
  end
  object sgAdditionalTerms: TDSSStringGrid
    Left = 24
    Top = 168
    Width = 500
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    ColCount = 1
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor]
    TabOrder = 1
    ReadOnly = False
    ColWidths = (
      349)
  end
  object btnDel: TImageListButton
    Left = 495
    Top = 272
    Width = 27
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 2
    OnClick = btnDelClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object btnAdd: TImageListButton
    Left = 465
    Top = 272
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    TabOrder = 3
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
end
