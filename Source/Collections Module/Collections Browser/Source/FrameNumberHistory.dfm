inherited fraNumberHistory: TfraNumberHistory
  inherited lblNumber: TLabel
    Top = 84
  end
  inherited Label24: TLabel
    Top = 116
  end
  object Label1: TLabel [4]
    Left = 12
    Top = 52
    Width = 73
    Height = 13
    Caption = 'Number Status:'
  end
  object Label2: TLabel [5]
    Left = 12
    Top = 228
    Width = 43
    Height = 13
    Caption = 'Barcode:'
  end
  inherited mmNotes: TMemo
    Top = 112
    TabOrder = 3
  end
  object cmbType: TConceptGroupComboBox
    Left = 88
    Top = 17
    Width = 125
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnChange = cmbTypeChange
  end
  object cmbStatus: TLuxIDComboBox
    Left = 88
    Top = 49
    Width = 125
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnPopulate = cmbStatusPopulate
  end
  object ScrollBox1: TScrollBox
    Left = 88
    Top = 224
    Width = 265
    Height = 65
    Color = clWhite
    ParentColor = False
    TabOrder = 4
    object imgBarcode: TImage
      Left = 0
      Top = 0
      Width = 261
      Height = 45
      Transparent = True
    end
  end
  object eNumber: TEdit
    Left = 88
    Top = 80
    Width = 124
    Height = 21
    MaxLength = 30
    TabOrder = 2
    OnChange = eNumberChange
  end
  object Barcode: TBarcode
    Height = 70
    Top = 32
    Left = 264
    Modul = 2
    Ratio = 2.000000000000000000
    Typ = bcCode_2_5_industrial
    ShowTextFont.Charset = DEFAULT_CHARSET
    ShowTextFont.Color = clWindowText
    ShowTextFont.Height = -11
    ShowTextFont.Name = 'MS Sans Serif'
    ShowTextFont.Style = []
  end
end
