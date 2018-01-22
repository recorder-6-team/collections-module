inherited fraTermVersionGeneral: TfraTermVersionGeneral
  Width = 364
  Font.Name = 'Arial'
  ParentFont = False
  inherited bvlBorder: TBevel
    Width = 356
    Visible = False
  end
  object lblNumber: TLabel
    Left = 12
    Top = 76
    Width = 50
    Height = 14
    Caption = 'Attributes:'
    FocusControl = eLabel
    WordWrap = True
  end
  object Label2: TLabel
    Left = 12
    Top = 104
    Width = 47
    Height = 28
    Caption = 'Authority && Date:'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 64
    Height = 42
    Caption = 'Select existing term version'
    WordWrap = True
  end
  object Bevel1: TBevel
    Left = 8
    Top = 59
    Width = 345
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsTopLine
  end
  object eLabel: TEdit
    Left = 84
    Top = 72
    Width = 264
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object eAuthorityAndDate: TEdit
    Left = 84
    Top = 108
    Width = 264
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = eAuthorityAndDateChange
  end
  object cmbExistingTermVersions: TLuxIDComboBox
    Left = 84
    Top = 20
    Width = 265
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 14
    TabOrder = 0
    OnChange = cmbExistingTermVersionsChange
    OnPopulate = cmbExistingTermVersionsPopulate
  end
end
