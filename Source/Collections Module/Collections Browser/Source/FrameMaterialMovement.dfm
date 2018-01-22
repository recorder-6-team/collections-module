inherited fraMaterialMovementDetails: TfraMaterialMovementDetails
  inherited Label6: TLabel
    Top = 153
  end
  object lblAcquisitionMethod: TLabel [4]
    Left = 12
    Top = 121
    Width = 51
    Height = 26
    Caption = 'Acquisition Method:'
    WordWrap = True
  end
  object lblDepartment: TLabel [5]
    Left = 12
    Top = 65
    Width = 56
    Height = 26
    AutoSize = False
    Caption = 'Sent to:'
    Layout = tlCenter
    WordWrap = True
  end
  object Label7: TLabel [6]
    Left = 12
    Top = 99
    Width = 39
    Height = 13
    Caption = 'Amount:'
  end
  object Label9: TLabel [7]
    Left = 167
    Top = 100
    Width = 45
    Height = 13
    Caption = 'Currency:'
  end
  inherited mmNotes: TMemo
    Top = 152
    Height = 113
    TabOrder = 7
  end
  inherited chkCompleted: TCheckBox
    Checked = False
    State = cbUnchecked
  end
  object eDepartment: TLinkedEdit
    Tag = 1
    Left = 72
    Top = 67
    Width = 271
    Height = 23
    TabOrder = 3
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
  object cmbCurrency: TConceptGroupComboBox
    Left = 221
    Top = 96
    Width = 124
    Height = 21
    ItemHeight = 13
    TabOrder = 5
  end
  object cmbAcquisitionMethod: TConceptGroupComboBox
    Left = 72
    Top = 124
    Width = 273
    Height = 21
    ItemHeight = 13
    TabOrder = 6
  end
  object eAmount: TEdit
    Left = 72
    Top = 96
    Width = 89
    Height = 21
    TabOrder = 4
  end
end
