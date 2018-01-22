inherited BaseMaterialMovementFrame: TBaseMaterialMovementFrame
  object Label3: TLabel
    Left = 12
    Top = 43
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object Label6: TLabel
    Left = 12
    Top = 69
    Width = 31
    Height = 13
    Caption = 'Notes:'
  end
  object lblContact: TLabel
    Left = 12
    Top = 15
    Width = 40
    Height = 13
    Caption = 'Contact:'
  end
  object mmNotes: TMemo
    Left = 72
    Top = 68
    Width = 273
    Height = 197
    ScrollBars = ssVertical
    TabOrder = 2
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
    TabOrder = 1
  end
  object eDate: TVagueDateEdit
    Left = 72
    Top = 40
    Width = 105
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
  object eContact: TUserEdit
    Tag = 1
    Left = 72
    Top = 11
    Width = 271
    Height = 23
    TabOrder = 3
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
  object eHiddenEdit: TLinkedEdit
    Tag = 1
    Left = 16
    Top = 240
    Width = 152
    Height = 23
    TabOrder = 4
    Visible = False
    BorderStyle = bsSingle
  end
end
