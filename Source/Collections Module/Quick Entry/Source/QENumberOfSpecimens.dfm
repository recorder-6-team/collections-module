object dlgQENumberOfSpecimens: TdlgQENumberOfSpecimens
  Left = 398
  Top = 344
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Number Of Specimens'
  ClientHeight = 128
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblNumberOfspecimens: TLabel
    Left = 24
    Top = 20
    Width = 203
    Height = 26
    Caption = 'Please enter the number of specimens you would like to create:'
    WordWrap = True
  end
  object txtNumberOfSpecimens: TNumberEdit
    Left = 72
    Top = 52
    Width = 97
    Height = 21
    MaxLength = 9
    TabOrder = 0
    Maximum = 9999
    DecimalPlaces = 0
  end
  object btnOk: TImageListButton
    Left = 52
    Top = 84
    Width = 65
    Height = 21
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 128
    Top = 84
    Width = 65
    Height = 21
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
end
