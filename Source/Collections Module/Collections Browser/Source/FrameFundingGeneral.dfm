inherited fraFundingGeneral: TfraFundingGeneral
  object Label10: TLabel
    Left = 12
    Top = 112
    Width = 35
    Height = 13
    Caption = 'Details:'
  end
  object Label5: TLabel
    Left = 12
    Top = 20
    Width = 54
    Height = 13
    Caption = 'Funded By:'
  end
  object Label3: TLabel
    Left = 12
    Top = 51
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object Label7: TLabel
    Left = 12
    Top = 83
    Width = 39
    Height = 13
    Caption = 'Amount:'
  end
  object Label9: TLabel
    Left = 185
    Top = 84
    Width = 45
    Height = 13
    Caption = 'Currency:'
  end
  object mmDetails: TMemo
    Left = 13
    Top = 128
    Width = 338
    Height = 137
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object eName: TUserEdit
    Tag = 1
    Left = 72
    Top = 17
    Width = 278
    Height = 23
    TabOrder = 0
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
  object cmbCurrency: TConceptGroupComboBox
    Left = 236
    Top = 80
    Width = 115
    Height = 21
    ItemHeight = 13
    TabOrder = 3
  end
  object eDate: TVagueDateEdit
    Left = 72
    Top = 48
    Width = 101
    Height = 21
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object eAmount: TNumberEdit
    Left = 72
    Top = 80
    Width = 101
    Height = 21
    TabOrder = 2
    Maximum = 2147483647
  end
end
