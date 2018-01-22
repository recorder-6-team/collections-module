inherited fraConceptGroupGeneral: TfraConceptGroupGeneral
  Width = 382
  Height = 288
  inherited bvlBorder: TBevel
    Left = 0
    Width = 378
    Height = 280
  end
  object Label4: TLabel
    Left = 12
    Top = 84
    Width = 25
    Height = 13
    Caption = 'URL:'
  end
  object Label1: TLabel
    Left = 12
    Top = 21
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 12
    Top = 52
    Width = 44
    Height = 13
    Caption = 'Authority:'
  end
  object Label3: TLabel
    Left = 12
    Top = 110
    Width = 62
    Height = 26
    Caption = 'Parent/Child relationship:'
    WordWrap = True
  end
  object lblLastChecked: TLabel
    Left = 11
    Top = 181
    Width = 69
    Height = 13
    Caption = 'Last Checked:'
  end
  object lblLastCheckedBy: TLabel
    Left = 97
    Top = 181
    Width = 131
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    WordWrap = True
  end
  object eUrl: TEdit
    Left = 120
    Top = 80
    Width = 219
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = eUrlChange
  end
  object btnUrlGo: TButton
    Left = 341
    Top = 80
    Width = 25
    Height = 21
    Cursor = crHandPoint
    Anchors = [akTop, akRight]
    Caption = 'Go'
    TabOrder = 3
    OnClick = btnUrlGoClick
  end
  object eName: TEdit
    Left = 120
    Top = 16
    Width = 246
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object eAuthority: TEdit
    Left = 120
    Top = 48
    Width = 246
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object cmbParentChildRelationship: TLuxIDComboBox
    Left = 120
    Top = 112
    Width = 248
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 4
    OnPopulate = cmbParentChildRelationshipPopulate
  end
  object btnUpdate: TButton
    Left = 249
    Top = 176
    Width = 55
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Update'
    TabOrder = 5
    OnClick = btnUpdateClick
  end
  object btnHistory: TButton
    Left = 311
    Top = 176
    Width = 55
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'History'
    TabOrder = 6
    OnClick = btnHistoryClick
  end
  inline fraPublishedTermRuleSelector: TfraPublishedTermRuleSelector
    Left = 8
    Top = 136
    Width = 365
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    inherited lblPublishedTermRule: TLabel
      Left = 3
    end
    inherited cmbPublishedTermRule: TLuxIDComboBox
      Width = 248
    end
  end
end
