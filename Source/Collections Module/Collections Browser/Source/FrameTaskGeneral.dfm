inherited fraTaskGeneral: TfraTaskGeneral
  object Label3: TLabel
    Left = 12
    Top = 15
    Width = 45
    Height = 13
    Caption = 'Date Set:'
  end
  object Label8: TLabel
    Left = 196
    Top = 16
    Width = 33
    Height = 13
    Caption = 'Status:'
  end
  object Label1: TLabel
    Left = 11
    Top = 44
    Width = 27
    Height = 13
    Caption = 'Type:'
  end
  object Label5: TLabel
    Left = 195
    Top = 44
    Width = 34
    Height = 13
    Caption = 'Priority:'
  end
  object lblIdentifiedBy: TLabel
    Left = 12
    Top = 99
    Width = 61
    Height = 13
    Caption = 'Identified By:'
  end
  object Label9: TLabel
    Left = 12
    Top = 127
    Width = 33
    Height = 13
    Caption = 'Action:'
  end
  object Label6: TLabel
    Left = 12
    Top = 200
    Width = 52
    Height = 13
    Caption = 'Comments:'
  end
  object Label2: TLabel
    Left = 12
    Top = 72
    Width = 43
    Height = 13
    Caption = 'Duration:'
  end
  object mmAction: TMemo
    Left = 82
    Top = 124
    Width = 271
    Height = 67
    ScrollBars = ssVertical
    TabOrder = 8
  end
  object mmComments: TMemo
    Left = 82
    Top = 197
    Width = 271
    Height = 66
    ScrollBars = ssVertical
    TabOrder = 9
  end
  object eDuration: TEdit
    Left = 82
    Top = 68
    Width = 29
    Height = 21
    TabOrder = 4
    Text = '0'
  end
  object udDuration: TUpDown
    Left = 111
    Top = 68
    Width = 11
    Height = 21
    Min = -32768
    Max = 32767
    TabOrder = 5
    OnClick = udDurationClick
  end
  object eIdentifiedBy: TUserEdit
    Tag = 1
    Left = 80
    Top = 96
    Width = 273
    Height = 23
    TabOrder = 7
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
  object eDateSet: TVagueDateEdit
    Left = 82
    Top = 12
    Width = 104
    Height = 21
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object cmbType: TConceptGroupComboBox
    Left = 82
    Top = 40
    Width = 105
    Height = 21
    ItemHeight = 13
    TabOrder = 2
  end
  object cmbDuration: TConceptGroupComboBox
    Left = 130
    Top = 68
    Width = 57
    Height = 21
    ItemHeight = 13
    TabOrder = 6
  end
  object cmbStatus: TLuxIDComboBox
    Left = 248
    Top = 12
    Width = 105
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnPopulate = cmbStatusPopulate
  end
  object cmbPriority: TLuxIDComboBox
    Left = 248
    Top = 40
    Width = 105
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    OnPopulate = cmbPriorityPopulate
  end
end
