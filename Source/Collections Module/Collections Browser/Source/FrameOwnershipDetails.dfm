inherited fraOwnershipDetails: TfraOwnershipDetails
  object lblContact: TLabel
    Left = 12
    Top = 15
    Width = 40
    Height = 13
    Caption = 'Contact:'
  end
  object Label3: TLabel
    Left = 12
    Top = 43
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object Label6: TLabel
    Left = 12
    Top = 71
    Width = 31
    Height = 13
    Caption = 'Notes:'
  end
  object chkCompleted: TCheckBox
    Left = 246
    Top = 42
    Width = 75
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Completed:'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object mmNotes: TMemo
    Left = 72
    Top = 68
    Width = 273
    Height = 217
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object eContact: TUserEdit
    Tag = 1
    Left = 72
    Top = 10
    Width = 273
    Height = 23
    TabOrder = 0
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
  object eDate: TVagueDateEdit
    Left = 72
    Top = 40
    Width = 113
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
end
