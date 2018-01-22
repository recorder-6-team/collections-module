object dlgSelectTaxonList: TdlgSelectTaxonList
  Left = 318
  Top = 268
  BorderStyle = bsDialog
  Caption = 'Select Taxon List'
  ClientHeight = 89
  ClientWidth = 442
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
  object btnCancel: TButton
    Left = 232
    Top = 56
    Width = 73
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOk: TButton
    Left = 144
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object cmbTaxonList: TIDComboBox
    Left = 16
    Top = 16
    Width = 409
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cmbTaxonListChange
    OnPopulate = cmbTaxonListPopulate
  end
end
