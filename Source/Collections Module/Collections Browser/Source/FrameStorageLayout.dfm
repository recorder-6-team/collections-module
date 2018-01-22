inherited fraStorageLayout: TfraStorageLayout
  object btnNavigateUp: TImageListButton
    Left = 192
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Parent'
    TabOrder = 0
    OnClick = btnNavigateUpClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 21
  end
  object btnEdit: TImageListButton
    Left = 276
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Edit'
    TabOrder = 1
    OnClick = btnEditClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 1
  end
end
