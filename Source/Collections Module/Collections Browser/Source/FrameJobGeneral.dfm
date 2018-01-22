inherited fraJobGeneral: TfraJobGeneral
  object Label2: TLabel
    Left = 12
    Top = 15
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label3: TLabel
    Left = 12
    Top = 39
    Width = 51
    Height = 13
    Caption = 'Start Date:'
  end
  object lblStaff: TLabel
    Left = 12
    Top = 88
    Width = 25
    Height = 13
    Caption = 'Staff:'
  end
  object Label6: TLabel
    Left = 12
    Top = 223
    Width = 35
    Height = 13
    Caption = 'Details:'
  end
  object Label8: TLabel
    Left = 261
    Top = 15
    Width = 30
    Height = 13
    Caption = 'Job #:'
  end
  object Label7: TLabel
    Left = 200
    Top = 39
    Width = 48
    Height = 13
    Caption = 'End Date:'
  end
  object Shape1: TShape
    Tag = 2
    Left = 89
    Top = 83
    Width = 241
    Height = 67
    Pen.Color = clRed
  end
  object Label9: TLabel
    Left = 13
    Top = 155
    Width = 33
    Height = 13
    Caption = 'Status:'
  end
  object Label1: TLabel
    Left = 12
    Top = 57
    Width = 49
    Height = 26
    Caption = 'Estimated Duration:'
    WordWrap = True
  end
  object Label10: TLabel
    Left = 200
    Top = 63
    Width = 76
    Height = 13
    Caption = 'Actual Duration:'
  end
  object lblActualDuration: TLabel
    Left = 292
    Top = 63
    Width = 3
    Height = 13
    Color = clBtnFace
    Constraints.MaxWidth = 61
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label15: TLabel
    Left = 12
    Top = 170
    Width = 44
    Height = 26
    Caption = 'Known Domains:'
    WordWrap = True
  end
  object lblDomains: TLabel
    Left = 89
    Top = 178
    Width = 3
    Height = 13
    Color = clBtnFace
    Constraints.MaxWidth = 264
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label5: TLabel
    Left = 12
    Top = 199
    Width = 24
    Height = 13
    Caption = 'Cost:'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 192
    Top = 200
    Width = 45
    Height = 13
    Caption = 'Currency:'
  end
  object lblJobNo: TLabel
    Left = 296
    Top = 15
    Width = 3
    Height = 13
    Color = clBtnFace
    Constraints.MaxWidth = 57
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object mmDetails: TMemo
    Left = 89
    Top = 221
    Width = 265
    Height = 44
    ScrollBars = ssVertical
    TabOrder = 12
  end
  object eName: TEdit
    Left = 89
    Top = 12
    Width = 164
    Height = 21
    TabOrder = 0
  end
  object eDuration: TEdit
    Left = 89
    Top = 60
    Width = 31
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object udDuration: TUpDown
    Left = 120
    Top = 60
    Width = 11
    Height = 21
    Min = -32768
    Max = 32767
    TabOrder = 4
    OnClick = udDurationClick
  end
  object btnAdd: TImageListButton
    Left = 331
    Top = 83
    Width = 22
    Height = 22
    Hint = 'Add'
    TabOrder = 7
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnRemove: TImageListButton
    Left = 331
    Top = 105
    Width = 22
    Height = 22
    Hint = 'Delete'
    TabOrder = 8
    OnClick = btnRemoveClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object eAmount: TNumberEdit
    Left = 89
    Top = 196
    Width = 84
    Height = 21
    TabOrder = 10
    Maximum = 2000000000
  end
  object cmbDuration: TConceptGroupComboBox
    Left = 134
    Top = 60
    Width = 58
    Height = 21
    ItemHeight = 13
    TabOrder = 5
  end
  object cmbCurrency: TConceptGroupComboBox
    Left = 239
    Top = 196
    Width = 115
    Height = 21
    ItemHeight = 13
    TabOrder = 11
  end
  object eDateStart: TVagueDateEdit
    Left = 89
    Top = 36
    Width = 101
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object eDateEnd: TVagueDateEdit
    Left = 252
    Top = 36
    Width = 101
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object cmbStatus: TLuxIDComboBox
    Left = 89
    Top = 151
    Width = 101
    Height = 21
    ItemHeight = 13
    TabOrder = 9
    OnPopulate = cmbStatusPopulate
  end
  object sgConservators: TDSSStringGrid
    Left = 90
    Top = 84
    Width = 239
    Height = 65
    ColCount = 1
    DefaultColWidth = 210
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    TabOrder = 6
    OnKeyDown = sgConservatorsKeyDown
    OnSelectCell = sgConservatorsSelectCell
    ReadOnly = True
    ColWidths = (
      221)
  end
  object eStaff: TUserEdit
    Tag = 1
    Left = 90
    Top = 85
    Width = 224
    Height = 21
    OnExit = eStaffExit
    TabOrder = 13
    Visible = False
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    OnChange = eStaffChange
    OnFindData = eStaffFindData
    OnGetData = eStaffGetData
    OnKeyDown = eStaffKeyDown
    ShowDragDropBorder = False
  end
end
