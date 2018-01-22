inherited dlgExportToDict: TdlgExportToDict
  Left = 558
  Top = 149
  Caption = 'Export Concept Group to Taxon Dictionary'
  ClientHeight = 405
  ClientWidth = 369
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited btnOk: TImageListButton
    Top = 372
    TabOrder = 3
  end
  inherited btnCancel: TImageListButton
    Top = 372
    TabOrder = 4
  end
  inherited gbChecklist: TGroupBox
    Top = 284
    Height = 77
    Caption = 'Destination'
    object chkAllListVersions: TCheckBox
      Left = 104
      Top = 52
      Width = 237
      Height = 17
      Caption = 'Export into all list versions'
      TabOrder = 1
    end
  end
  inherited gbConceptGroup: TGroupBox
    Top = 8
    Caption = 'Source'
    TabOrder = 0
  end
  object gbPreferredLists: TGroupBox [4]
    Left = 8
    Top = 104
    Width = 353
    Height = 169
    Caption = 'Preferred Lists'
    TabOrder = 1
    object btnMoveUp: TImageListButton
      Left = 316
      Top = 88
      Width = 27
      Height = 29
      Hint = 'Move list up'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btnMoveUpClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 21
      Layout = blGlyphBottom
    end
    object btnMoveDown: TImageListButton
      Left = 316
      Top = 124
      Width = 27
      Height = 29
      Hint = 'Move list down'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = btnMoveDownClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 24
    end
    object btnAdd: TImageListButton
      Left = 316
      Top = 16
      Width = 27
      Height = 29
      Hint = 'Add a new preferred list'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnAddClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 0
    end
    object btnDelete: TImageListButton
      Left = 316
      Top = 52
      Width = 27
      Height = 29
      Hint = 'Delete preferred list'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnDeleteClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 2
    end
    object lbPreferredLists: TIDListBox
      Left = 16
      Top = 16
      Width = 289
      Height = 137
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbPreferredListsClick
    end
  end
  inherited ProgressTimer: TTimer
    Top = 352
  end
end
