inherited fraDiagramRelationshipPropertiesInherited: TfraDiagramRelationshipPropertiesInherited
  inherited Label11: TLabel
    Visible = False
  end
  inherited Label15: TLabel
    Visible = False
  end
  inherited Label14: TLabel
    Visible = False
  end
  object Label1: TLabel [12]
    Left = 12
    Top = 64
    Width = 22
    Height = 14
    Caption = 'Item:'
  end
  object chkOverrideLine: TCheckBox
    Left = 12
    Top = 92
    Width = 121
    Height = 17
    Caption = 'Override line settings'
    TabOrder = 9
    OnClick = chkOverrideLineClick
  end
  object chkOverrideLabel: TCheckBox
    Left = 12
    Top = 148
    Width = 127
    Height = 17
    Caption = 'Override label settings'
    TabOrder = 10
    OnClick = chkOverrideLabelClick
  end
  object chkOverrideFont: TCheckBox
    Left = 12
    Top = 240
    Width = 121
    Height = 17
    Caption = 'Override font settings'
    TabOrder = 11
    OnClick = chkOverrideFontClick
  end
  object cmbItem: TComboBox
    Left = 100
    Top = 60
    Width = 229
    Height = 22
    Style = csOwnerDrawFixed
    ItemHeight = 16
    TabOrder = 12
    OnDrawItem = cmbItemDrawItem
  end
end
