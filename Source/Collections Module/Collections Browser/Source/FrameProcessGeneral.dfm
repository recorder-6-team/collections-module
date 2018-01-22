inherited fraProcessGeneral: TfraProcessGeneral
  object lblPerson: TLabel
    Left = 12
    Top = 219
    Width = 36
    Height = 13
    Caption = 'Person:'
  end
  object Label6: TLabel
    Left = 12
    Top = 42
    Width = 56
    Height = 13
    Caption = 'Description:'
  end
  object Label10: TLabel
    Left = 12
    Top = 15
    Width = 41
    Height = 13
    Caption = 'Process:'
  end
  object Label1: TLabel
    Left = 12
    Top = 248
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object mmDescription: TMemo
    Left = 80
    Top = 39
    Width = 247
    Height = 170
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnProcessInferred: TImageListButton
    Left = 328
    Top = 12
    Width = 21
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnProcessDescriptionInferred: TImageListButton
    Left = 328
    Top = 188
    Width = 21
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnProcessPersonInferred: TImageListButton
    Left = 328
    Top = 216
    Width = 21
    Height = 23
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object eDate: TVagueDateEdit
    Left = 80
    Top = 244
    Width = 121
    Height = 21
    TabOrder = 6
  end
  object cmbProcess: TConceptGroupComboBox
    Left = 80
    Top = 12
    Width = 247
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnKeyDown = cmbProcessKeyDown
    OnPopulate = cmbProcessPopulate
  end
  object btnDateInferred: TImageListButton
    Left = 201
    Top = 244
    Width = 21
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object ePerson: TUserEdit
    Tag = 1
    Left = 80
    Top = 216
    Width = 247
    Height = 23
    TabOrder = 4
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
end
