inherited fraBarcode: TfraBarcode
  object Label3: TLabel
    Left = 20
    Top = 8
    Width = 170
    Height = 18
    Caption = 'Barcode Type Selection'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label1: TLabel
    Left = 24
    Top = 120
    Width = 27
    Height = 14
    Caption = 'Type:'
  end
  object imgBarcode: TImage
    Left = 24
    Top = 176
    Width = 293
    Height = 69
  end
  object lblPreview: TLabel
    Left = 24
    Top = 160
    Width = 43
    Height = 14
    Caption = 'Preview:'
  end
  object Label4: TLabel
    Left = 20
    Top = 52
    Width = 270
    Height = 56
    Caption = 
      'Please select the barcode type to use when generating specimen l' +
      'abels.  Please note that some barcode types allow alphanumerical' +
      ' data values while others allow numerical data only.'
    WordWrap = True
  end
  object lblNumbers: TLabel
    Left = 240
    Top = 120
    Width = 66
    Height = 14
    Caption = 'Numbers only'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object cmbBarcodeType: TComboBox
    Left = 64
    Top = 116
    Width = 169
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 0
    OnChange = cmbBarcodeTypeChange
  end
  object Barcode: TBarcode
    Height = 67
    Top = 164
    Left = 276
    Modul = 2
    Ratio = 2.000000000000000000
    Typ = bcCode39Extended
    ShowTextFont.Charset = DEFAULT_CHARSET
    ShowTextFont.Color = clWindowText
    ShowTextFont.Height = -11
    ShowTextFont.Name = 'MS Sans Serif'
    ShowTextFont.Style = []
  end
end
