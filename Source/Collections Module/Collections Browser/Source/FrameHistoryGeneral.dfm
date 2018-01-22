inherited fraHistoryGeneral: TfraHistoryGeneral
  object Label8: TLabel
    Left = 12
    Top = 73
    Width = 52
    Height = 26
    Caption = 'Associated Person:'
    WordWrap = True
  end
  object Label13: TLabel
    Left = 12
    Top = 47
    Width = 26
    Height = 13
    Caption = 'From:'
  end
  object Label14: TLabel
    Left = 212
    Top = 47
    Width = 16
    Height = 13
    Caption = 'To:'
  end
  object Label2: TLabel
    Left = 12
    Top = 15
    Width = 23
    Height = 13
    Caption = 'Title:'
  end
  object Label15: TLabel
    Left = 11
    Top = 111
    Width = 52
    Height = 13
    Caption = 'Comments:'
  end
  object eFrom: TVagueDateEdit
    Left = 70
    Top = 44
    Width = 109
    Height = 21
    TabOrder = 1
  end
  object eTo: TVagueDateEdit
    Left = 244
    Top = 44
    Width = 109
    Height = 21
    TabOrder = 2
  end
  object eTitle: TEdit
    Left = 70
    Top = 12
    Width = 283
    Height = 21
    TabOrder = 0
  end
  object mmComments: TMemo
    Left = 70
    Top = 108
    Width = 283
    Height = 155
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object eSourcePerson: TUserEdit
    Tag = 1
    Left = 70
    Top = 76
    Width = 283
    Height = 23
    TabOrder = 3
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
end
