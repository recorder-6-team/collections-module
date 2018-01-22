inherited fraDiagramConceptPropertiesInherited: TfraDiagramConceptPropertiesInherited
  inherited Label11: TLabel
    Visible = False
  end
  inherited Label15: TLabel
    Visible = False
  end
  object lblItem: TLabel [11]
    Left = 12
    Top = 64
    Width = 22
    Height = 14
    Caption = 'Item:'
  end
  inherited udLineWeight: TUpDown
    Left = 223
  end
  object chkOverrideShape: TCheckBox
    Left = 12
    Top = 92
    Width = 137
    Height = 17
    Caption = 'Override shape settings'
    TabOrder = 7
    OnClick = chkOverrideShapeClick
  end
  object chkOverrideFont: TCheckBox
    Left = 12
    Top = 188
    Width = 121
    Height = 17
    Caption = 'Override font settings'
    TabOrder = 8
    OnClick = chkOverrideFontClick
  end
  object cmbItem: TComboBox
    Left = 100
    Top = 60
    Width = 229
    Height = 22
    Style = csOwnerDrawFixed
    ItemHeight = 16
    TabOrder = 9
    OnDrawItem = cmbItemDrawItem
  end
end
