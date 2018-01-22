inherited fraDesignationGeneral: TfraDesignationGeneral
  Height = 278
  inherited bvlBorder: TBevel
    Height = 270
  end
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 33
    Height = 13
    Caption = 'Status:'
  end
  object Label2: TLabel
    Left = 16
    Top = 44
    Width = 52
    Height = 13
    Caption = 'Date From:'
  end
  object Label3: TLabel
    Left = 203
    Top = 44
    Width = 42
    Height = 13
    Caption = 'Date To:'
  end
  object eDateFrom: TVagueDateEdit
    Left = 80
    Top = 40
    Width = 97
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
  object eDateTo: TVagueDateEdit
    Left = 255
    Top = 40
    Width = 97
    Height = 21
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object cmbStatus: TConceptGroupComboBox
    Left = 80
    Top = 12
    Width = 273
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
  end
end
