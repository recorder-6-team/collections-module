object fraDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector
  Left = 0
  Top = 0
  Width = 335
  Height = 45
  TabOrder = 0
  DesignSize = (
    335
    45)
  object lblDomain: TLabel
    Left = 0
    Top = 4
    Width = 39
    Height = 13
    Caption = 'Domain:'
  end
  object lblConceptGroup: TLabel
    Left = 0
    Top = 28
    Width = 81
    Height = 13
    AutoSize = False
    Caption = 'Concept Group:'
  end
  object cmbDomains: TComboBox
    Left = 88
    Top = 0
    Width = 247
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 0
    OnDrawItem = cmbDomainsDrawItem
    OnKeyDown = cmbDomainsKeyDown
    OnSelect = cmbDomainsSelect
  end
  object btnHistory: TImageListButton
    Left = 313
    Top = 23
    Width = 21
    Height = 22
    Anchors = [akTop, akRight]
    TabOrder = 2
    OnClick = btnHistoryClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 14
  end
  object cmbConceptGroups: TLuxIDComboBox
    Left = 88
    Top = 24
    Width = 225
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnPopulate = cmbConceptGroupsPopulate
  end
end
