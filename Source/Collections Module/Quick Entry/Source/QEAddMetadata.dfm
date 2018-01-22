object frmQuickEntryMetadata: TfrmQuickEntryMetadata
  Left = 548
  Top = 421
  BorderStyle = bsDialog
  Caption = 'Quick Entry Metadata Field'
  ClientHeight = 185
  ClientWidth = 254
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 237
    Height = 137
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 20
    Top = 20
    Width = 159
    Height = 26
    Caption = 'Please select the concept to add metadata about:'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 20
    Top = 92
    Width = 153
    Height = 13
    Caption = 'Please select the metadata item:'
    WordWrap = True
  end
  object cmbTableName: TComboBox
    Left = 20
    Top = 52
    Width = 213
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cmbTableNameChange
  end
  object cmbAttribute: TLuxIDComboBox
    Left = 20
    Top = 108
    Width = 213
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object btnOK: TImageListButton
    Left = 84
    Top = 152
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
    Left = 168
    Top = 152
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
