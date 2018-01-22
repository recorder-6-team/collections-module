inherited fraMaintainRelationships: TfraMaintainRelationships
  HorzScrollBar.Range = 116
  VertScrollBar.Range = 230
  AutoScroll = False
  object Label3: TLabel [1]
    Left = 8
    Top = 52
    Width = 68
    Height = 13
    Caption = 'Forward Term:'
  end
  object Label4: TLabel [2]
    Left = 8
    Top = 84
    Width = 70
    Height = 13
    Caption = 'Reverse Term:'
  end
  object Label5: TLabel [3]
    Left = 8
    Top = 116
    Width = 108
    Height = 13
    Caption = 'Semantic Relationship:'
  end
  object Label6: TLabel [4]
    Left = 8
    Top = 148
    Width = 61
    Height = 13
    Caption = 'Available for:'
  end
  object eForwardTerm: TEdit
    Left = 120
    Top = 48
    Width = 233
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 100
    TabOrder = 1
  end
  object eReverseTerm: TEdit
    Left = 120
    Top = 80
    Width = 233
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 100
    TabOrder = 2
  end
  object chklbAvailability: TCheckListBox
    Left = 120
    Top = 144
    Width = 232
    Height = 142
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 75
    ItemHeight = 13
    Items.Strings = (
      'Concept Relations'
      'Meaning Relations'
      'Term Version Relations'
      'Occurrence Relations'
      'Collection Unit Relations')
    TabOrder = 4
  end
  object cmbSemanticRelationship: TLuxIDComboBox
    Left = 120
    Top = 112
    Width = 234
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 3
    OnChange = cmbSemanticRelationshipChange
    OnPopulate = cmbSemanticRelationshipPopulate
  end
end
