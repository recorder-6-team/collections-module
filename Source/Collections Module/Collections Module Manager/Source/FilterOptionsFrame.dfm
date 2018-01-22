object fraFilterOptions: TfraFilterOptions
  Left = 0
  Top = 0
  Width = 482
  Height = 233
  TabOrder = 0
  object Label14: TLabel
    Left = 148
    Top = 16
    Width = 124
    Height = 13
    Caption = 'Select domains to filter on:'
  end
  object chklbDomains: TCheckListBox
    Left = 148
    Top = 32
    Width = 197
    Height = 189
    OnClickCheck = chklbDomainsClickCheck
    ItemHeight = 13
    TabOrder = 0
  end
end
