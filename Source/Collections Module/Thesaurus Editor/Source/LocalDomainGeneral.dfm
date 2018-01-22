inherited fraLocalDomainGeneral: TfraLocalDomainGeneral
  Width = 432
  Font.Name = 'Arial'
  ParentFont = False
  inherited bvlBorder: TBevel
    Width = 424
  end
  object lblLanguage: TLabel
    Left = 12
    Top = 55
    Width = 51
    Height = 28
    Caption = 'Primary Language:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label1: TLabel
    Left = 12
    Top = 17
    Width = 67
    Height = 28
    Caption = 'Local Domain Name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 12
    Top = 93
    Width = 73
    Height = 28
    Caption = 'Label for a concept group:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object eLocalDomainName: TEdit
    Left = 120
    Top = 20
    Width = 296
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object eConceptGroupLabel: TEdit
    Left = 120
    Top = 96
    Width = 296
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object cmbLanguage: TLuxIDComboBox
    Left = 120
    Top = 58
    Width = 297
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 14
    TabOrder = 1
    OnPopulate = cmbLanguagePopulate
  end
  inline fraPublishedTermRuleSelector: TfraPublishedTermRuleSelector
    Left = 8
    Top = 123
    Width = 414
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    inherited lblPublishedTermRule: TLabel
      Left = 4
      Width = 100
      Height = 14
    end
    inherited cmbPublishedTermRule: TLuxIDComboBox
      Width = 297
      Height = 22
      ItemHeight = 14
    end
  end
end
