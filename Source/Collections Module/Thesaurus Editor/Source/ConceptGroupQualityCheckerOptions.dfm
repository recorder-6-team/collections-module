object dlgConceptGroupQualityCheckerOptions: TdlgConceptGroupQualityCheckerOptions
  Left = 511
  Top = 403
  BorderStyle = bsDialog
  Caption = 'Concept Group Quality Checker Options'
  ClientHeight = 349
  ClientWidth = 298
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
  object gbDomainConceptGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 287
    Height = 77
    Caption = ' Domain and Concept Group to scan: '
    TabOrder = 0
    inline fraDCGSelector: TfraDomainConceptGroupsSelector
      Left = 9
      Top = 21
      Width = 270
      Height = 48
      TabOrder = 0
      DesignSize = (
        270
        48)
      inherited cmbDomains: TComboBox
        Width = 182
        OnKeyDown = fraDCGSelectorcmbDomainsKeyDown
        OnSelect = fraDCGSelectorcmbDomainsSelect
      end
      inherited btnHistory: TImageListButton
        Left = 248
        Visible = False
      end
      inherited cmbConceptGroups: TLuxIDComboBox
        Width = 182
      end
    end
  end
  object rgScanOptions: TRadioGroup
    Left = 8
    Top = 92
    Width = 287
    Height = 53
    Caption = ' Scan for: '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Potential Synonyms'
      'Duplicate Terms')
    TabOrder = 1
    OnClick = rgScanOptionsClick
  end
  object btnOk: TImageListButton
    Left = 136
    Top = 314
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = btnOkClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 220
    Top = 314
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object gbResults: TGroupBox
    Left = 8
    Top = 248
    Width = 285
    Height = 57
    Caption = ' Results: '
    TabOrder = 2
    object Label1: TLabel
      Left = 12
      Top = 24
      Width = 177
      Height = 13
      Caption = 'Maximum number of records returned:'
    end
    object eMaxRecords: TNumberEdit
      Left = 192
      Top = 20
      Width = 73
      Height = 21
      TabOrder = 0
      Text = '1000'
      Maximum = 1000000000
      DecimalPlaces = 0
    end
  end
  object gbDCGWithin: TGroupBox
    Left = 8
    Top = 160
    Width = 287
    Height = 77
    Caption = 'Domain and Concept Group to search within'
    TabOrder = 5
    inline fraDCGWithin: TfraDomainConceptGroupsSelector
      Left = 8
      Top = 24
      Width = 270
      Height = 48
      TabOrder = 0
      inherited cmbDomains: TComboBox
        Width = 182
        OnKeyDown = fraDCGWithincmbDomainsKeyDown
        OnSelect = fraDCGWithincmbDomainsSelect
      end
      inherited btnHistory: TImageListButton
        Left = 248
      end
      inherited cmbConceptGroups: TLuxIDComboBox
        Width = 182
      end
    end
  end
end
