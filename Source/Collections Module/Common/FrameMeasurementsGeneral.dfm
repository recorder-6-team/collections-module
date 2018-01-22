inherited fraMeasurementsGeneral: TfraMeasurementsGeneral
  object lblAppliesTo: TLabel
    Left = 36
    Top = 23
    Width = 53
    Height = 13
    Caption = 'Applies To:'
  end
  object lblMethod: TLabel
    Left = 36
    Top = 55
    Width = 39
    Height = 13
    Caption = 'Method:'
  end
  object lblDuration: TLabel
    Left = 36
    Top = 87
    Width = 43
    Height = 13
    Caption = 'Duration:'
  end
  object lblAccuracy: TLabel
    Left = 36
    Top = 119
    Width = 48
    Height = 13
    Caption = 'Accuracy:'
  end
  object lblParameter: TLabel
    Left = 36
    Top = 151
    Width = 51
    Height = 13
    Caption = 'Parameter:'
  end
  object lblUnit: TLabel
    Left = 36
    Top = 183
    Width = 22
    Height = 13
    Caption = 'Unit:'
  end
  object lblValue: TLabel
    Left = 36
    Top = 215
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object eAppliesTo: TEdit
    Left = 108
    Top = 20
    Width = 188
    Height = 21
    TabOrder = 0
  end
  object eDuration: TEdit
    Left = 108
    Top = 84
    Width = 188
    Height = 21
    TabOrder = 2
  end
  object eAccuracy: TEdit
    Left = 108
    Top = 116
    Width = 188
    Height = 21
    TabOrder = 3
  end
  object btnMore: TButton
    Left = 248
    Top = 244
    Width = 105
    Height = 23
    Caption = '<< &Less Details'
    TabOrder = 7
    OnClick = btnMoreClick
  end
  object cmbMethod: TConceptGroupComboBox
    Left = 108
    Top = 52
    Width = 188
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object cmbUnit: TConceptGroupComboBox
    Left = 108
    Top = 180
    Width = 97
    Height = 21
    ItemHeight = 13
    TabOrder = 5
  end
  object cmbParameter: TConceptGroupComboBox
    Left = 108
    Top = 148
    Width = 190
    Height = 21
    ItemHeight = 13
    MaxLength = 150
    TabOrder = 4
    OnPopulate = cmbParameterPopulate
  end
  object eValue: TEdit
    Left = 108
    Top = 211
    Width = 190
    Height = 21
    MaxLength = 50
    TabOrder = 6
    OnExit = eValueExit
  end
end
