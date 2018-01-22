inherited fraMetadata: TfraMetadata
  object Panel2: TPanel
    Left = 9
    Top = 9
    Width = 347
    Height = 259
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'Panel2'
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 136
      Top = 0
      Height = 259
      Beveled = True
    end
    object pnlList: TPanel
      Left = 0
      Top = 0
      Width = 133
      Height = 259
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 71
        Height = 13
        Caption = 'Metadata Item:'
      end
      object lbMetadataItem: TListBox
        Left = 0
        Top = 16
        Width = 133
        Height = 243
        Style = lbOwnerDrawFixed
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 13
        ParentFont = False
        Sorted = True
        TabOrder = 0
        OnClick = lbMetadataItemClick
        OnDrawItem = lbMetadataItemDrawItem
      end
    end
    object pnlData: TPanel
      Left = 142
      Top = 0
      Width = 205
      Height = 259
      Align = alClient
      BevelOuter = bvNone
      Caption = 'pnlData'
      TabOrder = 1
      object Splitter2: TSplitter
        Left = 0
        Top = 61
        Width = 205
        Height = 4
        Cursor = crVSplit
        Align = alTop
        Beveled = True
      end
      object mmData: TMemo
        Left = 0
        Top = 65
        Width = 205
        Height = 194
        Align = alClient
        Constraints.MinHeight = 30
        TabOrder = 0
        OnExit = mmDataExit
      end
      object mmLabel: TMemo
        Left = 0
        Top = 16
        Width = 205
        Height = 45
        Align = alTop
        Color = clBtnFace
        Constraints.MinHeight = 30
        ReadOnly = True
        TabOrder = 1
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 205
        Height = 16
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Caption = 'Description && Value:'
        TabOrder = 2
      end
    end
    object Panel5: TPanel
      Left = 139
      Top = 0
      Width = 3
      Height = 259
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
    end
    object Panel6: TPanel
      Left = 133
      Top = 0
      Width = 3
      Height = 259
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
    end
  end
end
