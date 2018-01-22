inherited fraDescriptors: TfraDescriptors
  object shpList: TShape
    Left = 12
    Top = 12
    Width = 340
    Height = 230
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
  object sgDescriptors: TDSSStringGrid
    Left = 13
    Top = 13
    Width = 338
    Height = 228
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 3
    FixedCols = 0
    RowCount = 2
    TabOrder = 2
    OnKeyDown = sgDescriptorsKeyDown
    ReadOnly = True
    ColWidths = (
      167
      79
      72)
  end
  object cmbParameter: TConceptGroupComboBox
    Left = 181
    Top = 35
    Width = 82
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    Visible = False
    OnExit = cmbParameterExit
    OnKeyDown = cmbParameterKeyDown
    OnPopulate = cmbParameterPopulate
  end
end
