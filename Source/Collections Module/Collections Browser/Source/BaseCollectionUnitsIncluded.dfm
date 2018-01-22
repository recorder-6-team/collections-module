inherited BaseIncludedCollectionUnits: TBaseIncludedCollectionUnits
  Height = 305
  DesignSize = (
    365
    305)
  inherited bvlBorder: TBevel
    Height = 297
    Visible = False
  end
  object splitCollSpec: TSplitter
    Left = 0
    Top = 92
    Width = 365
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
  end
  object splitSpecStore: TSplitter
    Left = 0
    Top = 187
    Width = 365
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 365
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Collections'
    TabOrder = 0
  end
  object Panel3: TPanel
    Left = 0
    Top = 95
    Width = 365
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Specimens'
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 190
    Width = 365
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Storage'
    TabOrder = 2
  end
  object chklbCollections: TCheckListBox
    Left = 0
    Top = 27
    Width = 365
    Height = 65
    OnClickCheck = ClickCheckChange
    Align = alTop
    BevelInner = bvNone
    BorderStyle = bsNone
    IntegralHeight = True
    ItemHeight = 13
    Style = lbOwnerDrawFixed
    TabOrder = 3
    OnDrawItem = ListDrawItem
  end
  object chklbSpecimens: TCheckListBox
    Left = 0
    Top = 122
    Width = 365
    Height = 65
    OnClickCheck = ClickCheckChange
    Align = alTop
    BevelInner = bvNone
    BorderStyle = bsNone
    IntegralHeight = True
    ItemHeight = 13
    Style = lbOwnerDrawFixed
    TabOrder = 4
    OnDrawItem = ListDrawItem
  end
  object chklbStorage: TCheckListBox
    Left = 0
    Top = 217
    Width = 365
    Height = 78
    OnClickCheck = ClickCheckChange
    Align = alClient
    BevelInner = bvNone
    BorderStyle = bsNone
    IntegralHeight = True
    ItemHeight = 13
    Style = lbOwnerDrawFixed
    TabOrder = 5
    OnDrawItem = ListDrawItem
  end
end
