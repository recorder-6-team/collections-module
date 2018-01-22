object dlgChecklistTransferForm: TdlgChecklistTransferForm
  Left = 308
  Top = 272
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'dlgChecklistTransferForm'
  ClientHeight = 210
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TImageListButton
    Left = 200
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Ok'
    Enabled = False
    TabOrder = 0
    OnClick = btnOkClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 286
    Top = 176
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object gbChecklist: TGroupBox
    Left = 8
    Top = 8
    Width = 353
    Height = 57
    Caption = 'Checklist'
    TabOrder = 2
    object Label1: TLabel
      Left = 12
      Top = 24
      Width = 46
      Height = 13
      Caption = 'Checklist:'
    end
    object cmbChecklist: TIDComboBox
      Left = 104
      Top = 24
      Width = 240
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnClick = cmbChecklistClick
      OnPopulate = cmbChecklistPopulate
    end
  end
  object gbConceptGroup: TGroupBox
    Left = 8
    Top = 76
    Width = 353
    Height = 89
    Caption = 'Concept Group'
    TabOrder = 3
    inline fraDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector
      Left = 16
      Top = 24
      Width = 329
      Height = 61
      TabOrder = 0
      inherited lblConceptGroup: TLabel
        Top = 36
      end
      inherited cmbDomains: TComboBox
        Width = 240
        OnSelect = fraDomainConceptGroupsSelectorcmbDomainsSelect
      end
      inherited btnHistory: TImageListButton
        Left = 320
        Top = 31
        Visible = False
      end
      inherited cmbConceptGroups: TLuxIDComboBox
        Top = 32
        Width = 240
        OnClick = fraDomainConceptGroupsSelectorcmbConceptGroupsClick
      end
    end
  end
  object ProgressTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = ProgressTimerTimer
    Left = 48
    Top = 176
  end
end
