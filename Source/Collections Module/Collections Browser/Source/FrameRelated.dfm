inherited fraRelated: TfraRelated
  Height = 305
  inherited bvlBorder: TBevel
    Height = 305
  end
  object lblRelatedUnit: TLabel
    Left = 12
    Top = 22
    Width = 81
    Height = 13
    Caption = 'Related <UNIT>:'
  end
  object Label20: TLabel
    Left = 12
    Top = 56
    Width = 88
    Height = 13
    Caption = 'Relationship Type:'
  end
  object Label21: TLabel
    Left = 12
    Top = 88
    Width = 87
    Height = 13
    Caption = 'Relationship Date:'
  end
  object Label23: TLabel
    Left = 12
    Top = 160
    Width = 52
    Height = 13
    Caption = 'Comments:'
  end
  object lblAuthor: TLabel
    Left = 12
    Top = 123
    Width = 34
    Height = 13
    Caption = 'Author:'
  end
  object mmComments: TMemo
    Left = 12
    Top = 176
    Width = 349
    Height = 117
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object btnTypeInferred: TImageListButton
    Left = 340
    Top = 52
    Width = 21
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object eName: TLinkedEdit
    Tag = 1
    Left = 108
    Top = 16
    Width = 230
    Height = 23
    TabOrder = 0
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
    OnChange = eNameChange
    ShowButton = False
  end
  object eAuthor: TUserEdit
    Tag = 1
    Left = 108
    Top = 117
    Width = 253
    Height = 23
    TabOrder = 4
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
  end
  object eDate: TVagueDateEdit
    Left = 108
    Top = 84
    Width = 156
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
  object cmbType: TLuxIDComboBox
    Left = 108
    Top = 52
    Width = 233
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnPopulate = cmbTypePopulate
  end
end
