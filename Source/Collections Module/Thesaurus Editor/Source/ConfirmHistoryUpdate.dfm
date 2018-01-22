object dlgConfirmHistoryUpdate: TdlgConfirmHistoryUpdate
  Left = 136
  Top = 321
  BorderStyle = bsDialog
  Caption = 'Confirm Update'
  ClientHeight = 109
  ClientWidth = 187
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblMessage: TLabel
    Left = 16
    Top = 16
    Width = 153
    Height = 41
    AutoSize = False
    Caption = 'lblMessage'
    WordWrap = True
  end
  object btnYes: TButton
    Left = 16
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Yes'
    ModalResult = 6
    TabOrder = 0
    OnClick = btnYesClick
  end
  object btnNo: TButton
    Left = 96
    Top = 72
    Width = 75
    Height = 25
    Caption = 'No'
    ModalResult = 7
    TabOrder = 1
  end
end
