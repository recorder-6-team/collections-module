inherited fraPeopleGeneral: TfraPeopleGeneral
  object Label8: TLabel
    Left = 12
    Top = 15
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 12
    Top = 51
    Width = 61
    Height = 26
    Caption = 'Relationship Type:'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 208
    Top = 58
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object Label3: TLabel
    Left = 12
    Top = 91
    Width = 52
    Height = 13
    Caption = 'Comments:'
  end
  object mmComments: TMemo
    Left = 76
    Top = 91
    Width = 273
    Height = 145
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object eName: TUserEdit
    Tag = 1
    Left = 76
    Top = 10
    Width = 272
    Height = 23
    TabOrder = 0
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
  object cmbType: TConceptGroupComboBox
    Left = 76
    Top = 55
    Width = 117
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object eDate: TVagueDateEdit
    Left = 241
    Top = 55
    Width = 107
    Height = 21
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
end
