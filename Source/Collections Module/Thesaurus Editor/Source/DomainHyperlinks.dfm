inherited fraDomainHyperlinks: TfraDomainHyperlinks
  Width = 345
  Height = 279
  inherited bvlBorder: TBevel
    Top = 7
    Width = 337
    Height = 266
  end
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 52
    Height = 13
    Caption = 'Hyperlinks:'
  end
  object lbHyperlinks: TListBox
    Left = 12
    Top = 28
    Width = 321
    Height = 59
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbHyperlinksClick
    OnKeyDown = lbHyperlinksKeyDown
  end
  object gbDetails: TGroupBox
    Left = 12
    Top = 113
    Width = 321
    Height = 150
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Details:'
    TabOrder = 1
    DesignSize = (
      321
      150)
    object Label2: TLabel
      Left = 8
      Top = 76
      Width = 47
      Height = 13
      Caption = 'Hyperlink:'
    end
    object Label3: TLabel
      Left = 8
      Top = 96
      Width = 49
      Height = 26
      Caption = 'Word Separator:'
      WordWrap = True
    end
    object Label4: TLabel
      Left = 8
      Top = 20
      Width = 31
      Height = 13
      HelpType = htKeyword
      HelpKeyword = 'y'
      Caption = 'Name:'
    end
    object Label5: TLabel
      Left = 8
      Top = 48
      Width = 51
      Height = 13
      Caption = 'Image File:'
    end
    object eHyperlink: TEdit
      Left = 72
      Top = 72
      Width = 237
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
    end
    object eWordSeparator: TEdit
      Left = 72
      Top = 100
      Width = 49
      Height = 21
      TabOrder = 4
    end
    object eItemName: TEdit
      Left = 72
      Top = 16
      Width = 237
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object chkUseConceptKey: TCheckBox
      Left = 72
      Top = 128
      Width = 165
      Height = 17
      Caption = 'Search using Concept Key'
      TabOrder = 5
      OnClick = chkUseConceptKeyClick
    end
    object btnSave: TImageListButton
      Left = 264
      Top = 120
      Width = 22
      Height = 23
      Anchors = [akTop, akRight, akBottom]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = btnSaveClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 6
    end
    object btnCancel: TImageListButton
      Left = 286
      Top = 120
      Width = 22
      Height = 23
      Anchors = [akTop, akRight, akBottom]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = btnCancelClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 4
    end
    object eImageFilePath: TEdit
      Left = 72
      Top = 44
      Width = 217
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object btnGetImageFilePath: TButton
      Left = 288
      Top = 44
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnGetImageFilePathClick
    end
  end
  object btnAdd: TImageListButton
    Left = 268
    Top = 89
    Width = 22
    Height = 22
    Anchors = [akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnEdit: TImageListButton
    Left = 290
    Top = 89
    Width = 22
    Height = 22
    Anchors = [akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = btnEditClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 1
  end
  object btnDelete: TImageListButton
    Left = 312
    Top = 89
    Width = 22
    Height = 22
    Anchors = [akRight, akBottom]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = btnDeleteClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object dlgOpenPic: TOpenPictureDialog
    Filter = 'All (*.bmp; *.jpg; *.gif)|*.bmp; *.jpg; *.gif'
    Left = 252
    Top = 152
  end
end
