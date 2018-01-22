inherited fraDomainGeneral: TfraDomainGeneral
  Width = 454
  Height = 260
  Font.Name = 'Arial'
  ParentFont = False
  inherited bvlBorder: TBevel
    Width = 446
    Height = 252
  end
  object Label1: TLabel
    Left = 12
    Top = 20
    Width = 68
    Height = 14
    Caption = 'Domain Name:'
  end
  object Label3: TLabel
    Left = 12
    Top = 138
    Width = 58
    Height = 14
    Caption = 'Security Bit:'
  end
  object Label4: TLabel
    Left = 11
    Top = 48
    Width = 79
    Height = 42
    Caption = 'Default Relation Type For Use In Hierarchy:'
    WordWrap = True
  end
  object cbOccurrencesAllowed: TCheckBox
    Left = 10
    Top = 169
    Width = 99
    Height = 25
    Alignment = taLeftJustify
    Caption = 'Occurrences Allowed:'
    TabOrder = 3
    WordWrap = True
    OnClick = cbOccurrencesAllowedClick
  end
  object eItemName: TEdit
    Left = 96
    Top = 16
    Width = 342
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object eSecurityBit: TNumberEdit
    Left = 96
    Top = 134
    Width = 33
    Height = 22
    MaxLength = 2
    TabOrder = 2
    OnKeyDown = eSecurityBitKeyDown
    Maximum = 32
  end
  object cmbRelationType: TLuxIDComboBox
    Left = 96
    Top = 59
    Width = 343
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 14
    TabOrder = 1
    OnPopulate = cmbRelationTypePopulate
  end
  object btnFetchSecurityBit: TButton
    Left = 135
    Top = 134
    Width = 213
    Height = 23
    Caption = 'Fetch Next Available Security Bit'
    TabOrder = 4
    OnClick = btnFetchSecurityBitClick
  end
  inline fraPublishedTermRuleSelector: TfraPublishedTermRuleSelector
    Left = 8
    Top = 96
    Width = 437
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    inherited lblPublishedTermRule: TLabel
      Left = 2
      Top = 0
      Width = 73
      Height = 28
      WordWrap = True
    end
    inherited cmbPublishedTermRule: TLuxIDComboBox
      Left = 89
      Top = 2
      Width = 342
      Height = 22
      ItemHeight = 14
    end
  end
end
