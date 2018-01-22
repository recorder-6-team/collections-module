inherited BaseMDIChildEditor: TBaseMDIChildEditor
  Left = 376
  Top = 263
  Caption = 'BaseMDIChildEditor'
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object BrowserDetailsSplitter: TSplitter
    Left = 289
    Top = 0
    Width = 2
    Height = 256
    Beveled = True
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 256
    Width = 618
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnEdit: TImageListButton
      Left = 88
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Edit'
      TabOrder = 1
      OnClick = btnEditClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 1
    end
    object btnDelete: TImageListButton
      Left = 172
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 2
    end
    object btnAdd: TImageListButton
      Left = 4
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 0
      OnClick = btnAddClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 0
    end
  end
  object pnlBrowser: TPanel
    Left = 0
    Top = 0
    Width = 289
    Height = 256
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlDetails: TPanel
    Left = 291
    Top = 0
    Width = 327
    Height = 256
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object pnlSaveButtons: TPanel
      Left = 0
      Top = 221
      Width = 327
      Height = 35
      Align = alBottom
      BevelOuter = bvNone
      Constraints.MinWidth = 290
      TabOrder = 0
      object pnlSaveBtnsAlign: TPanel
        Left = 152
        Top = 0
        Width = 175
        Height = 35
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object btnSave: TImageListButton
          Left = 8
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Save'
          TabOrder = 0
          OnClick = btnSaveClick
          ImageList = dmInterface.ilButtons
          ImageIndex = 3
        end
        object btnCancel: TImageListButton
          Left = 92
          Top = 6
          Width = 75
          Height = 25
          Caption = 'Cancel'
          TabOrder = 1
          OnClick = btnCancelClick
          ImageList = dmInterface.ilButtons
          ImageIndex = 4
        end
      end
    end
  end
end
