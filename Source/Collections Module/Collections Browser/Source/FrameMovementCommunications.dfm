inherited fraMovementCommunications: TfraMovementCommunications
  inherited bvlBorder: TBevel
    Visible = False
  end
  object Shape3: TShape
    Tag = 2
    Left = 3
    Top = 3
    Width = 332
    Height = 106
    Pen.Color = clRed
  end
  object btnAdd: TImageListButton
    Tag = 1
    Left = 338
    Top = 4
    Width = 22
    Height = 22
    Hint = 'Add new communication'
    TabOrder = 1
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnEdit: TImageListButton
    Tag = 1
    Left = 338
    Top = 26
    Width = 22
    Height = 22
    Hint = 'Edit selected communication'
    TabOrder = 2
    OnClick = btnEditClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 1
  end
  object btnRemove: TImageListButton
    Tag = 1
    Left = 338
    Top = 48
    Width = 22
    Height = 22
    Hint = 'Delete selected communication'
    TabOrder = 3
    OnClick = btnRemoveClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object gbDetails: TGroupBox
    Left = 4
    Top = 112
    Width = 357
    Height = 161
    Caption = 'Details'
    TabOrder = 4
    object Label27: TLabel
      Left = 8
      Top = 20
      Width = 26
      Height = 13
      Caption = 'From:'
    end
    object Label29: TLabel
      Left = 8
      Top = 73
      Width = 27
      Height = 13
      Caption = 'Type:'
    end
    object lblCommDate: TLabel
      Left = 200
      Top = 72
      Width = 26
      Height = 13
      Caption = 'Date:'
    end
    object Label30: TLabel
      Left = 8
      Top = 93
      Width = 40
      Height = 13
      Caption = 'Content:'
    end
    object Label31: TLabel
      Left = 12
      Top = 135
      Width = 72
      Height = 13
      Caption = 'File Reference:'
    end
    object Label1: TLabel
      Left = 8
      Top = 47
      Width = 16
      Height = 13
      Caption = 'To:'
    end
    object eReference: TEdit
      Left = 88
      Top = 132
      Width = 97
      Height = 21
      TabOrder = 5
    end
    object btnAccept: TImageListButton
      Left = 306
      Top = 133
      Width = 22
      Height = 22
      Hint = 'Accept changes'
      TabOrder = 6
      OnClick = btnAcceptClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 17
    end
    object btnDiscard: TImageListButton
      Left = 328
      Top = 133
      Width = 22
      Height = 22
      Hint = 'Discard changes'
      TabOrder = 7
      OnClick = btnDiscardClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 18
    end
    object mmContent: TMemo
      Left = 48
      Top = 93
      Width = 301
      Height = 33
      TabOrder = 4
    end
    object eNameTo: TUserEdit
      Tag = 1
      Left = 48
      Top = 42
      Width = 301
      Height = 23
      TabOrder = 1
      BorderStyle = bsSingle
      ImageIndex = 12
      ImageList = dmInterface.ilButtons
    end
    object eNameFrom: TUserEdit
      Tag = 1
      Left = 48
      Top = 14
      Width = 301
      Height = 23
      TabOrder = 0
      BorderStyle = bsSingle
      ImageIndex = 12
      ImageList = dmInterface.ilButtons
    end
    object eDate: TVagueDateEdit
      Left = 229
      Top = 69
      Width = 120
      Height = 21
      Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnExit = eDateExit
    end
    object cmbType: TConceptGroupComboBox
      Left = 48
      Top = 69
      Width = 108
      Height = 21
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object sgCommunications: TDSSStringGrid
    Left = 4
    Top = 4
    Width = 330
    Height = 104
    ColCount = 3
    FixedCols = 0
    RowCount = 2
    TabOrder = 0
    OnClick = sgCommunicationsClick
    OnKeyDown = sgCommunicationsKeyDown
    ReadOnly = True
    ColWidths = (
      98
      116
      78)
  end
end
