object frmQuickEntryMeasurements: TfrmQuickEntryMeasurements
  Left = 433
  Top = 252
  BorderStyle = bsDialog
  Caption = 'Quick Entry Measurement Field'
  ClientHeight = 271
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lblInstruction: TLabel
    Left = 4
    Top = 8
    Width = 191
    Height = 13
    Caption = 'Please enter details of the measurement:'
  end
  object pnlTaxonData: TPanel
    Left = 4
    Top = 28
    Width = 297
    Height = 157
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 19
      Width = 67
      Height = 13
      Caption = 'Measurement:'
    end
    object Label2: TLabel
      Left = 16
      Top = 115
      Width = 48
      Height = 13
      Caption = 'Accuracy:'
    end
    object Label3: TLabel
      Left = 16
      Top = 51
      Width = 41
      Height = 13
      Caption = 'Qualifier:'
    end
    object Label4: TLabel
      Left = 16
      Top = 83
      Width = 22
      Height = 13
      Caption = 'Unit:'
    end
    object eTxAccuracy: TEdit
      Left = 88
      Top = 112
      Width = 190
      Height = 21
      MaxLength = 10
      TabOrder = 3
    end
    object cmbTxType: TIDComboBox
      Left = 88
      Top = 16
      Width = 190
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbTxTypeChange
      OnPopulate = cmbTxTypePopulate
    end
    object cmbTxQualifier: TIDComboBox
      Left = 88
      Top = 48
      Width = 190
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnPopulate = cmbTxQualifierPopulate
    end
    object cmbTxUnit: TIDComboBox
      Left = 88
      Top = 80
      Width = 109
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnPopulate = cmbTxUnitPopulate
    end
  end
  object btnOK: TImageListButton
    Left = 140
    Top = 240
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 3
    OnClick = btnOKClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 226
    Top = 240
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object rgMeasurementRelatesTo: TRadioGroup
    Left = 4
    Top = 192
    Width = 297
    Height = 41
    Caption = ' Measurement relates to: '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Specimen'
      'Occurrence')
    TabOrder = 2
    OnClick = rgMeasurementRelatesToClick
  end
  object pnlCollectionData: TPanel
    Left = 4
    Top = 28
    Width = 297
    Height = 157
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object lblAppliesTo: TLabel
      Left = 16
      Top = 11
      Width = 53
      Height = 13
      Caption = 'Applies To:'
    end
    object lblAccuracy: TLabel
      Left = 16
      Top = 83
      Width = 48
      Height = 13
      Caption = 'Accuracy:'
    end
    object lblParameter: TLabel
      Left = 16
      Top = 107
      Width = 51
      Height = 13
      Caption = 'Parameter:'
    end
    object lblUnit: TLabel
      Left = 16
      Top = 131
      Width = 22
      Height = 13
      Caption = 'Unit:'
    end
    object Label5: TLabel
      Left = 16
      Top = 35
      Width = 39
      Height = 13
      Caption = 'Method:'
    end
    object lblDuration: TLabel
      Left = 16
      Top = 59
      Width = 43
      Height = 13
      Caption = 'Duration:'
    end
    object eAppliesTo: TEdit
      Left = 88
      Top = 8
      Width = 190
      Height = 21
      MaxLength = 50
      TabOrder = 0
    end
    object eAccuracy: TEdit
      Left = 88
      Top = 80
      Width = 190
      Height = 21
      MaxLength = 50
      TabOrder = 1
    end
    object cmbParameter: TConceptGroupComboBox
      Left = 88
      Top = 104
      Width = 190
      Height = 21
      ItemHeight = 13
      MaxLength = 150
      TabOrder = 2
      OnPopulate = cmbParameterPopulate
    end
    object cmbUnit: TConceptGroupComboBox
      Left = 88
      Top = 128
      Width = 109
      Height = 21
      ItemHeight = 13
      TabOrder = 3
      OnPopulate = cmbUnitPopulate
    end
    object cmbMethod: TConceptGroupComboBox
      Left = 88
      Top = 32
      Width = 190
      Height = 21
      ItemHeight = 13
      MaxLength = 150
      TabOrder = 4
      OnPopulate = cmbMethodPopulate
    end
    object eDuration: TEdit
      Left = 88
      Top = 56
      Width = 190
      Height = 21
      TabOrder = 5
    end
  end
end
