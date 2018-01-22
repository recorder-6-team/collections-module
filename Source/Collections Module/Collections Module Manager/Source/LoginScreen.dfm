object frmLoginScreen: TfrmLoginScreen
  Left = 482
  Top = 228
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Thesaurus Editor Login'
  ClientHeight = 127
  ClientWidth = 251
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 237
    Height = 76
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 13
    Top = 24
    Width = 52
    Height = 14
    Caption = 'Username:'
  end
  object Label2: TLabel
    Left = 13
    Top = 52
    Width = 53
    Height = 14
    Caption = 'Password:'
  end
  object btnCancel: TImageListButton
    Left = 169
    Top = 91
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object btnOK: TImageListButton
    Left = 84
    Top = 91
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object cmbUsers: TLuxIDComboBox
    Left = 80
    Top = 21
    Width = 156
    Height = 22
    ItemHeight = 14
    TabOrder = 0
    OnPopulate = cmbUsersPopulate
  end
  object ePassword: TMaskEdit
    Left = 80
    Top = 48
    Width = 156
    Height = 22
    PasswordChar = '*'
    TabOrder = 1
  end
end
