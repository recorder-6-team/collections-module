inherited fraConditionGeneral: TfraConditionGeneral
  object Label1: TLabel
    Left = 12
    Top = 20
    Width = 61
    Height = 13
    Caption = 'Check Type:'
  end
  object Label3: TLabel
    Left = 12
    Top = 55
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object lblCheckedBy: TLabel
    Left = 12
    Top = 93
    Width = 61
    Height = 13
    Caption = 'Checked By:'
  end
  object Label5: TLabel
    Left = 12
    Top = 132
    Width = 47
    Height = 13
    Caption = 'Condition:'
  end
  object Label6: TLabel
    Left = 12
    Top = 204
    Width = 35
    Height = 13
    Caption = 'Details:'
  end
  object Label2: TLabel
    Left = 12
    Top = 163
    Width = 44
    Height = 26
    Caption = 'Known Domains:'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 196
    Top = 13
    Width = 40
    Height = 26
    Caption = 'Ref. Number:'
    WordWrap = True
  end
  object chkAppliesToAllSpecimens: TCheckBox
    Left = 204
    Top = 132
    Width = 145
    Height = 17
    Caption = 'Applies to all Specimens'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object mmDetails: TMemo
    Left = 80
    Top = 204
    Width = 269
    Height = 57
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object eRefNumber: TEdit
    Left = 244
    Top = 16
    Width = 101
    Height = 21
    TabOrder = 1
  end
  object eCheckedBy: TUserEdit
    Tag = 1
    Left = 80
    Top = 88
    Width = 265
    Height = 23
    TabOrder = 3
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
  object cmbType: TConceptGroupComboBox
    Left = 80
    Top = 16
    Width = 101
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object cmbCondition: TConceptGroupComboBox
    Left = 80
    Top = 128
    Width = 101
    Height = 21
    ItemHeight = 13
    TabOrder = 4
  end
  object eDate: TVagueDateEdit
    Left = 80
    Top = 52
    Width = 121
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
  object mmDomains: TMemo
    Left = 79
    Top = 163
    Width = 268
    Height = 26
    BevelInner = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 7
  end
end
