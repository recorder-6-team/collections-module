inherited fraRelatedOccurrences: TfraRelatedOccurrences
  inherited bvlBorder: TBevel
    Visible = False
  end
  object Shape5: TShape
    Tag = 2
    Left = 4
    Top = 5
    Width = 332
    Height = 101
    Pen.Color = clRed
  end
  object btnAdd: TImageListButton
    Left = 338
    Top = 4
    Width = 23
    Height = 23
    Hint = 'Add new related occurrence'
    TabOrder = 2
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object gbRelOccDetails: TGroupBox
    Left = 4
    Top = 112
    Width = 357
    Height = 161
    Caption = 'Details'
    TabOrder = 1
    object lblRelatedOcc: TLabel
      Left = 12
      Top = 20
      Width = 99
      Height = 13
      Caption = 'Related Occurrence:'
    end
    object lblType: TLabel
      Left = 12
      Top = 48
      Width = 88
      Height = 13
      Caption = 'Relationship Type:'
    end
    object Label4: TLabel
      Left = 12
      Top = 70
      Width = 52
      Height = 13
      Caption = 'Comments:'
    end
    object btnAccept: TImageListButton
      Left = 302
      Top = 130
      Width = 23
      Height = 23
      Hint = 'Accept changes'
      TabOrder = 3
      OnClick = btnAcceptClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 17
    end
    object eRelatedOcc: TLinkedEdit
      Tag = 1
      Left = 116
      Top = 16
      Width = 233
      Height = 23
      TabOrder = 0
      BorderStyle = bsSingle
      OnFindData = eRelatedOccFindData
      ShowButton = False
    end
    object mmComments: TMemo
      Left = 12
      Top = 84
      Width = 337
      Height = 45
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object btnDiscard: TImageListButton
      Left = 325
      Top = 130
      Width = 23
      Height = 23
      Hint = 'Discard changes'
      TabOrder = 4
      OnClick = btnDiscardClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 18
    end
    object cmbRelationshipTypes: TLuxIDComboBox
      Left = 116
      Top = 44
      Width = 185
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      OnPopulate = cmbRelationshipTypesPopulate
    end
  end
  object btnEdit: TImageListButton
    Left = 338
    Top = 27
    Width = 23
    Height = 23
    Hint = 'Edit selected related occurrence'
    TabOrder = 3
    OnClick = btnEditClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 1
  end
  object btnDelete: TImageListButton
    Left = 338
    Top = 50
    Width = 23
    Height = 23
    Hint = 'Delete selected related occurrence'
    TabOrder = 4
    OnClick = btnDeleteClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object sgRelatedOccs: TDSSStringGrid
    Left = 5
    Top = 6
    Width = 330
    Height = 99
    ColCount = 2
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
    TabOrder = 0
    OnClick = sgRelatedOccsClick
    OnKeyDown = sgRelatedOccsKeyDown
    ReadOnly = True
    ColWidths = (
      226
      99)
  end
end
