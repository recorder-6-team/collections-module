object fraPublishedTermRuleSelector: TfraPublishedTermRuleSelector
  Left = 0
  Top = 0
  Width = 347
  Height = 33
  TabOrder = 0
  DesignSize = (
    347
    33)
  object lblPublishedTermRule: TLabel
    Left = 8
    Top = 13
    Width = 101
    Height = 13
    Caption = 'Published Term Rule:'
  end
  object cmbPublishedTermRule: TLuxIDComboBox
    Left = 112
    Top = 8
    Width = 230
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnPopulate = cmbPublishedTermRulePopulate
  end
end
