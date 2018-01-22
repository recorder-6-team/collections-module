inherited fraSpecimens: TfraSpecimens
  object shpGrid: TShape
    Tag = 2
    Left = 13
    Top = 15
    Width = 339
    Height = 227
    Anchors = [akLeft, akTop, akRight, akBottom]
    Pen.Color = clRed
  end
  object btnAdd: TImageListButton
    Left = 306
    Top = 242
    Width = 23
    Height = 23
    Hint = 'Add'
    Anchors = [akRight, akBottom]
    TabOrder = 0
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnDel: TImageListButton
    Left = 329
    Top = 242
    Width = 23
    Height = 23
    Hint = 'Delete'
    Anchors = [akRight, akBottom]
    TabOrder = 1
    OnClick = btnDelClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object sgSpecimens: TDSSStringGrid
    Left = 14
    Top = 16
    Width = 337
    Height = 225
    ColCount = 1
    FixedCols = 0
    RowCount = 2
    ScrollBars = ssVertical
    TabOrder = 2
    OnDblClick = sgSpecimensDblClick
    OnDrawCell = sgSpecimensDrawCell
    ReadOnly = True
    ColWidths = (
      332)
  end
end
