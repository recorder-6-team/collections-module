inherited fraOccurrenceGeneral: TfraOccurrenceGeneral
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 65
    Height = 13
    Caption = 'Record Type:'
  end
  object Label8: TLabel
    Left = 16
    Top = 76
    Width = 52
    Height = 13
    Caption = 'Comments:'
  end
  object Label28: TLabel
    Left = 16
    Top = 20
    Width = 72
    Height = 13
    Caption = 'Surveyor'#39's Ref:'
  end
  object lblVerificationStatus: TLabel
    Left = 288
    Top = 245
    Width = 63
    Height = 13
    Alignment = taRightJustify
    Caption = 'Not validated'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label1: TLabel
    Left = 16
    Top = 152
    Width = 31
    Height = 13
    Caption = 'Count:'
  end
  object mmCount: TMemo
    Left = 16
    Top = 168
    Width = 333
    Height = 65
    TabStop = False
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object eSurveyorRef: TEdit
    Left = 92
    Top = 16
    Width = 257
    Height = 21
    TabOrder = 1
  end
  object mmComments: TMemo
    Left = 92
    Top = 72
    Width = 257
    Height = 81
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object chkConfidential: TCheckBox
    Left = 16
    Top = 244
    Width = 97
    Height = 17
    Caption = 'Confidential'
    TabOrder = 3
  end
  object chkChecked: TCheckBox
    Left = 140
    Top = 244
    Width = 97
    Height = 17
    Caption = 'Checked'
    TabOrder = 4
  end
  object cmbRecordType: TConceptGroupComboBox
    Left = 92
    Top = 44
    Width = 257
    Height = 21
    ItemHeight = 13
    TabOrder = 5
    OnPopulate = cmbRecordTypePopulate
  end
end
