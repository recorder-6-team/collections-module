inherited fraConceptGroupVersionGeneral: TfraConceptGroupVersionGeneral
  Width = 324
  Height = 268
  Font.Name = 'Arial'
  ParentFont = False
  inherited bvlBorder: TBevel
    Left = 5
    Top = 3
    Width = 316
    Height = 260
  end
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 41
    Height = 14
    Caption = 'Version:'
  end
  object lblDate: TLabel
    Left = 12
    Top = 40
    Width = 76
    Height = 28
    Caption = 'Date version applicable from:'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 12
    Top = 72
    Width = 63
    Height = 28
    Caption = 'Date version applicable to:'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 12
    Top = 112
    Width = 70
    Height = 14
    Caption = 'Date acquired:'
  end
  object Label4: TLabel
    Left = 12
    Top = 144
    Width = 23
    Height = 14
    Caption = 'URL:'
  end
  object eUrl: TEdit
    Left = 96
    Top = 140
    Width = 190
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = eUrlChange
  end
  object btnUrlGo: TButton
    Left = 287
    Top = 140
    Width = 25
    Height = 21
    Cursor = crHandPoint
    Anchors = [akTop, akRight]
    Caption = 'Go'
    TabOrder = 1
    OnClick = btnUrlGoClick
  end
  object eVersionNumber: TEdit
    Left = 97
    Top = 12
    Width = 217
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 100
    TabOrder = 2
  end
  object eDateFrom: TVagueDateEdit
    Left = 96
    Top = 44
    Width = 145
    Height = 21
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object eDateTo: TVagueDateEdit
    Left = 96
    Top = 76
    Width = 145
    Height = 21
    Hint = '"VAGUEDATEEDIT2" is not a valid month or season name.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object eDateAcquired: TVagueDateEdit
    Left = 96
    Top = 108
    Width = 145
    Height = 21
    Hint = '"VAGUEDATEEDIT3" is not a valid month or season name.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
end
