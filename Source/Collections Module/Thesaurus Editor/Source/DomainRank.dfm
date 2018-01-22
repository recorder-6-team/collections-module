inherited fraDomainRank: TfraDomainRank
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 150
    Height = 13
    Caption = 'Ranks available for this domain:'
  end
  object sgRankEditor: TDSSStringGrid
    Left = 12
    Top = 35
    Width = 340
    Height = 205
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 4
    FixedCols = 0
    RowCount = 2
    TabOrder = 0
    OnDrawCell = sgRankEditorDrawCell
    OnKeyDown = sgRankEditorKeyDown
    ReadOnly = True
    ColWidths = (
      55
      149
      58
      49)
  end
  object btnEditColor: TColorButton
    Left = 278
    Top = 58
    Width = 51
    Height = 20
    ActiveColor = clBlue
    TabOrder = 1
    TabStop = True
  end
  object btnApplySort: TImageListButton
    Left = 12
    Top = 240
    Width = 85
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Apply Sort'
    TabOrder = 2
    OnClick = btnApplySortClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 10
  end
  object btnAdd: TImageListButton
    Left = 308
    Top = 240
    Width = 22
    Height = 22
    Anchors = [akRight, akBottom]
    TabOrder = 3
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnDelete: TImageListButton
    Left = 331
    Top = 240
    Width = 22
    Height = 22
    Anchors = [akRight, akBottom]
    TabOrder = 4
    OnClick = btnDeleteClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
end
