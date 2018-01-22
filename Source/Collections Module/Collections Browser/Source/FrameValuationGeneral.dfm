inherited fraValuationGeneral: TfraValuationGeneral
  object Label2: TLabel
    Left = 12
    Top = 20
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object Label3: TLabel
    Left = 12
    Top = 53
    Width = 27
    Height = 13
    Caption = 'Type:'
  end
  object Label4: TLabel
    Left = 12
    Top = 84
    Width = 51
    Height = 13
    Caption = 'Valued By:'
  end
  object Label5: TLabel
    Left = 12
    Top = 116
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object Label6: TLabel
    Left = 186
    Top = 117
    Width = 45
    Height = 13
    Caption = 'Currency:'
  end
  object Label7: TLabel
    Left = 12
    Top = 181
    Width = 56
    Height = 13
    Caption = 'Description:'
  end
  object Label1: TLabel
    Left = 12
    Top = 142
    Width = 52
    Height = 26
    Caption = 'Valid From Date:'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 188
    Top = 142
    Width = 42
    Height = 26
    Caption = 'Valid To Date:'
    WordWrap = True
  end
  object Label9: TLabel
    Left = 192
    Top = 14
    Width = 53
    Height = 26
    Caption = 'Reference Number:'
    WordWrap = True
  end
  object eDate: TVagueDateEdit
    Left = 72
    Top = 17
    Width = 109
    Height = 21
    ImeName = 'eDate'
    TabOrder = 0
  end
  object cmbType: TConceptGroupComboBox
    Left = 72
    Top = 49
    Width = 110
    Height = 21
    ItemHeight = 13
    TabOrder = 2
  end
  object cmbCurrency: TConceptGroupComboBox
    Left = 236
    Top = 113
    Width = 113
    Height = 21
    ItemHeight = 13
    TabOrder = 5
  end
  object mmDescription: TMemo
    Left = 72
    Top = 178
    Width = 277
    Height = 83
    ScrollBars = ssVertical
    TabOrder = 8
  end
  object eDateFrom: TVagueDateEdit
    Left = 72
    Top = 145
    Width = 109
    Height = 21
    ImeName = 'eDate'
    TabOrder = 6
  end
  object eDateTo: TVagueDateEdit
    Left = 236
    Top = 145
    Width = 113
    Height = 21
    ImeName = 'eDate'
    TabOrder = 7
  end
  object eRefNumber: TEdit
    Left = 252
    Top = 17
    Width = 97
    Height = 21
    ImeName = 'eDate'
    TabOrder = 1
  end
  object eName: TUserEdit
    Tag = 1
    Left = 72
    Top = 80
    Width = 277
    Height = 23
    TabOrder = 3
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
  object eValue: TNumberEdit
    Left = 72
    Top = 113
    Width = 109
    Height = 21
    TabOrder = 4
    Maximum = 2147483647
  end
end
