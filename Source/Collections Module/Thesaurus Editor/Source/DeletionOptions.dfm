object dlgDeletionOptions: TdlgDeletionOptions
  Left = 452
  Top = 231
  BorderStyle = bsDialog
  Caption = 'Thesaurus Deletion Options'
  ClientHeight = 143
  ClientWidth = 363
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TImageListButton
    Left = 204
    Top = 113
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOkClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 284
    Top = 113
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object Panel1: TPanel
    Left = 4
    Top = 6
    Width = 355
    Height = 101
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object chkSynchronise: TCheckBox
      Left = 11
      Top = 5
      Width = 337
      Height = 35
      Caption = 
        'When deleting items from the Thesaurus, synchronise the deletion' +
        's by deleting records from the Taxon dictionary.'
      TabOrder = 0
      WordWrap = True
    end
    object chkLog: TCheckBox
      Left = 11
      Top = 43
      Width = 257
      Height = 17
      Caption = 'Log deletions as a script to the following file:'
      TabOrder = 1
      OnClick = chkLogClick
    end
    object eLog: TEdit
      Left = 28
      Top = 69
      Width = 292
      Height = 21
      TabOrder = 2
    end
    object btnLog: TButton
      Left = 320
      Top = 69
      Width = 23
      Height = 22
      Caption = '...'
      TabOrder = 3
      OnClick = btnLogClick
    end
  end
end
