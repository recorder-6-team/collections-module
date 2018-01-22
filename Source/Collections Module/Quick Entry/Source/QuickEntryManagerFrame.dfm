inherited fraQuickEntryManager: TfraQuickEntryManager
  Width = 799
  Height = 443
  Font.Name = 'Arial'
  object Splitter1: TSplitter
    Left = 261
    Top = 0
    Height = 443
    Beveled = True
  end
  object pnlDetails: TPanel
    Left = 264
    Top = 0
    Width = 535
    Height = 443
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      535
      443)
    object Label2: TLabel
      Left = 8
      Top = 12
      Width = 30
      Height = 14
      Caption = 'Name:'
    end
    object Label3: TLabel
      Left = 8
      Top = 100
      Width = 120
      Height = 14
      Caption = 'Fields to include on form:'
    end
    object Label4: TLabel
      Left = 8
      Top = 40
      Width = 52
      Height = 14
      Caption = 'Data Type:'
    end
    object Label5: TLabel
      Left = 8
      Top = 68
      Width = 66
      Height = 14
      Caption = 'Subject Area:'
    end
    object eName: TEdit
      Left = 76
      Top = 8
      Width = 257
      Height = 22
      MaxLength = 100
      TabOrder = 0
    end
    object sgFields: TStringGrid
      Left = 4
      Top = 116
      Width = 510
      Height = 288
      Anchors = [akLeft, akTop, akRight, akBottom]
      DefaultColWidth = 128
      DefaultRowHeight = 18
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowMoving]
      TabOrder = 3
      OnRowMoved = sgFieldsRowMoved
      OnSelectCell = sgFieldsSelectCell
      ColWidths = (
        73
        112
        122
        89
        112)
      RowHeights = (
        18
        18)
    end
    object btnSave: TImageListButton
      Left = 438
      Top = 7
      Width = 79
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Save'
      TabOrder = 9
      OnClick = btnSaveClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 6
    end
    object btnCancel: TImageListButton
      Left = 438
      Top = 35
      Width = 79
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Cancel'
      TabOrder = 10
      OnClick = btnCancelClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 4
    end
    object cmbDataType: TComboBox
      Left = 76
      Top = 36
      Width = 177
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 1
      OnChange = cmbDataTypeChange
    end
    object cmbSubjectArea: TLuxIDComboBox
      Left = 76
      Top = 64
      Width = 177
      Height = 22
      ItemHeight = 14
      TabOrder = 2
    end
    object btnAddMeasurement: TImageListButton
      Left = 8
      Top = 411
      Width = 125
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Add Measurement'
      TabOrder = 4
      OnClick = btnAddMeasurementClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 0
      Spacing = -1
    end
    object btnMoveUp: TImageListButton
      Left = 344
      Top = 411
      Width = 79
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Move &Up'
      TabOrder = 7
      OnClick = btnMoveUpClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 21
    end
    object btnMoveDown: TImageListButton
      Left = 430
      Top = 411
      Width = 88
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Move &Down'
      TabOrder = 8
      OnClick = btnMoveDownClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 24
    end
    object btnAddNumber: TBitBtn
      Left = 140
      Top = 411
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Add &Number'
      TabOrder = 5
      OnClick = btnAddNumberClick
    end
    object btnAddMetadata: TBitBtn
      Left = 224
      Top = 411
      Width = 89
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Add &Metadata'
      TabOrder = 6
      OnClick = btnAddMetadataClick
    end
  end
  object pnlSelector: TPanel
    Left = 0
    Top = 0
    Width = 261
    Height = 443
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinWidth = 260
    TabOrder = 1
    DesignSize = (
      261
      443)
    object Label1: TLabel
      Left = 8
      Top = 7
      Width = 110
      Height = 14
      Caption = 'Quick Entry Templates:'
    end
    object lbTemplates: TListBox
      Left = 8
      Top = 24
      Width = 245
      Height = 380
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 14
      TabOrder = 0
      OnClick = lbTemplatesClick
    end
    object btnAdd: TImageListButton
      Left = 8
      Top = 411
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Add'
      TabOrder = 1
      OnClick = btnAddClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 0
      Spacing = -1
    end
    object btnEdit: TImageListButton
      Left = 92
      Top = 411
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Edit'
      TabOrder = 2
      OnClick = btnEditClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 1
      Spacing = -1
    end
    object btnDelete: TImageListButton
      Left = 176
      Top = 411
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Delete'
      TabOrder = 3
      OnClick = btnDeleteClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 2
      Spacing = -1
    end
  end
end
