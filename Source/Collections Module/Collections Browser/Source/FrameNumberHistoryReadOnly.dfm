inherited fraNumberHistoryReadOnly: TfraNumberHistoryReadOnly
  inherited lblNumber: TLabel
    Top = 48
  end
  inherited Label24: TLabel
    Top = 76
  end
  object lblNumberValue: TLabel [4]
    Left = 91
    Top = 48
    Width = 64
    Height = 13
    Caption = 'NumberValue'
    Constraints.MaxWidth = 270
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblNumberType: TLabel [5]
    Left = 91
    Top = 20
    Width = 61
    Height = 13
    Caption = 'NumberType'
    Constraints.MaxWidth = 270
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  inherited mmNotes: TMemo
    Left = 91
    Top = 76
    Height = 193
    BorderStyle = bsNone
    Color = clBtnFace
    Font.Color = clBlue
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssNone
  end
end
