object frmQuickEntryNumber: TfrmQuickEntryNumber
  Left = 548
  Top = 421
  BorderStyle = bsDialog
  Caption = 'Quick Entry Number'
  ClientHeight = 157
  ClientWidth = 261
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 245
    Height = 109
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 20
    Top = 20
    Width = 192
    Height = 26
    Caption = 
      'Please select the number type to add as a field on the Quick Ent' +
      'ry card:'
    WordWrap = True
  end
  object chkPreferred: TCheckBox
    Left = 20
    Top = 84
    Width = 189
    Height = 17
    Caption = 'Numbers created are preferred'
    TabOrder = 1
  end
  object cmbNumberType: TConceptGroupComboBox
    Left = 20
    Top = 52
    Width = 221
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object btnOK: TImageListButton
    Left = 94
    Top = 124
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOKClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 178
    Top = 124
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
end
