object dlgQEImport: TdlgQEImport
  Left = 161
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Import Spreadsheet into Quick Entry'
  ClientHeight = 458
  ClientWidth = 521
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    521
    458)
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox: TScrollBox
    Left = 0
    Top = 0
    Width = 521
    Height = 415
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    TabOrder = 0
  end
  object btnCancel: TImageListButton
    Left = 438
    Top = 426
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ImageIndex = 4
  end
  object btnNext: TImageListButton
    Left = 350
    Top = 426
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Next'
    Enabled = False
    TabOrder = 2
    OnClick = btnNextClick
    ImageIndex = 6
  end
end
