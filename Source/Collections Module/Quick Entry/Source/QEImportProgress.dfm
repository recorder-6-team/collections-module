inherited fraQEImportProgress: TfraQEImportProgress
  object lblProgress: TLabel
    Left = 40
    Top = 92
    Width = 96
    Height = 13
    Caption = 'Importing Data...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object pbProgress: TProgressBar
    Left = 40
    Top = 112
    Width = 241
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Smooth = True
    TabOrder = 0
  end
end
