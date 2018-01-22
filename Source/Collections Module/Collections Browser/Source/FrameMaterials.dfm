inherited fraMaterials: TfraMaterials
  object shpList: TShape
    Left = 12
    Top = 12
    Width = 340
    Height = 227
    Anchors = [akLeft, akTop, akRight, akBottom]
    Pen.Color = clRed
  end
  object sgMaterials: TDSSStringGrid
    Left = 13
    Top = 13
    Width = 338
    Height = 225
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 3
    FixedCols = 0
    RowCount = 2
    TabOrder = 0
    OnGetEditText = sgMaterialsGetEditText
    OnKeyDown = sgMaterialsKeyDown
    ReadOnly = True
    ColWidths = (
      196
      56
      63)
  end
  object btnAdd: TImageListButton
    Left = 306
    Top = 240
    Width = 23
    Height = 23
    Hint = 'Add'
    Anchors = [akRight, akBottom]
    TabOrder = 1
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnRemove: TImageListButton
    Left = 329
    Top = 240
    Width = 23
    Height = 23
    Hint = 'Delete'
    Anchors = [akRight, akBottom]
    TabOrder = 2
    OnClick = btnRemoveClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object eMaterial: TLinkedEdit
    Tag = 1
    Left = 14
    Top = 34
    Width = 198
    Height = 21
    TabOrder = 3
    Visible = False
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    OnFindData = eMaterialFindData
    OnGetData = eMaterialGetData
    ShowDragDropBorder = False
  end
  object cmbUnit: TConceptGroupComboBox
    Left = 267
    Top = 35
    Width = 67
    Height = 21
    ItemHeight = 13
    TabOrder = 4
    Visible = False
    HasNoSelectionItem = True
  end
end
