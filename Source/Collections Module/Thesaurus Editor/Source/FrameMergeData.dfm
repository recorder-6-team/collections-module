inherited fraMergeData: TfraMergeData
  Width = 416
  OnResize = FrameResize
  object pnlInstruct: TPanel
    Left = 0
    Top = 0
    Width = 416
    Height = 121
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblInstruct: TLabel
      Left = 5
      Top = 5
      Width = 397
      Height = 39
      AutoSize = False
      Caption = 
        'First, drag or copy the item you wish to reallocate into the sou' +
        'rce item box on the left. After the merge is complete, this item' +
        ' is deleted and all data referring to the item is reallocated to' +
        ' the item you drag or copy to the second box.'
      WordWrap = True
    end
    object Label1: TLabel
      Left = 8
      Top = 104
      Width = 60
      Height = 13
      Caption = 'Source Item:'
    end
    object lblDestItem: TLabel
      Left = 184
      Top = 104
      Width = 79
      Height = 13
      Caption = 'Destination Item:'
    end
    object lblInstruct2: TLabel
      Left = 5
      Top = 49
      Width = 397
      Height = 39
      AutoSize = False
      Caption = 
        'Once selected, check that the data items you have chosen are cor' +
        'rect by double clicking on items to see the data they contain.  ' +
        'Double click foreign key data to view the record which the key l' +
        'inks to.  If correct, click Merge to perform the operation.'
      WordWrap = True
    end
  end
  object pnlSource: TPanel
    Tag = 2
    Left = 0
    Top = 121
    Width = 179
    Height = 145
    Align = alLeft
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 1
    Caption = 'pnlSource'
    TabOrder = 1
    object tvSourceItem: TTreeView
      Left = 1
      Top = 1
      Width = 177
      Height = 143
      Align = alLeft
      Images = ilTreeImages
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnClick = ItemClick
      OnCustomDrawItem = CustomDrawItem
    end
  end
  object pnlDest: TPanel
    Tag = 2
    Left = 179
    Top = 121
    Width = 237
    Height = 145
    Align = alClient
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 1
    Caption = 'Panel2'
    TabOrder = 2
    object tvTargetItem: TTreeView
      Left = 1
      Top = 1
      Width = 235
      Height = 143
      Align = alClient
      Images = ilTreeImages
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnClick = ItemClick
      OnCustomDrawItem = CustomDrawItem
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 266
    Width = 416
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object btnMerge: TImageListButton
      Left = 140
      Top = 5
      Width = 75
      Height = 25
      Caption = '&Merge'
      Default = True
      Enabled = False
      TabOrder = 0
      OnClick = btnMergeClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 6
    end
    object btnClear: TImageListButton
      Left = 224
      Top = 5
      Width = 75
      Height = 25
      Caption = '&Clear'
      TabOrder = 1
      OnClick = btnClearClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 4
    end
  end
  object ilTreeImages: TImageList
    Left = 48
    Top = 244
    Bitmap = {
      494C010103000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000FFFFFF00FFFF
      FF0084000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF0084000000FFFF
      FF00FFFFFF00000000000000FF0000000000C6C6C600FFFFFF00FFFFFF00C6C6
      C600FFFFFF00FFFFFF00C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600FFFFFF00FFFFFF00C6C6C60000000000C6C6C600FFFFFF00FFFF
      FF00C6C6C600FFFFFF00FFFFFF00C6C6C600FFFFFF00FFFFFF00C6C6C600FFFF
      FF00FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      000084000000000000000000FF0000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000FFFFFF00FFFF
      FF0084000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF0084000000FFFF
      FF00FFFFFF00000000000000FF0000000000C6C6C600FFFFFF00FFFFFF00C6C6
      C600FFFFFF00FFFFFF00C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600FFFFFF00FFFFFF00C6C6C60000000000C6C6C600FFFFFF00FFFF
      FF00C6C6C600FFFFFF00FFFFFF00C6C6C600FFFFFF00FFFFFF00C6C6C600FFFF
      FF00FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      000084000000000000000000FF0000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000FFFFFF00FFFF
      FF0084000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF0084000000FFFF
      FF00FFFFFF00000000000000FF0000000000C6C6C600FFFFFF00FFFFFF00C6C6
      C600FFFFFF00FFFFFF00C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600FFFFFF00FFFFFF00C6C6C60000000000C6C6C600FFFFFF00FFFF
      FF00C6C6C600FFFFFF000000FF000000FF000000FF000000FF000000FF000000
      FF00FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      000084000000000000000000FF0000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C6000000FF00840000008400000084000000840000000000
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000FFFFFF00FFFF
      FF0084000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF0084000000FFFF
      FF00FFFFFF00000000000000FF0000000000C6C6C600FFFFFF00FFFFFF00C6C6
      C600FFFFFF00FFFFFF00C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600FFFFFF00FFFFFF00C6C6C60000000000C6C6C600FFFFFF00FFFF
      FF00C6C6C600FFFFFF000000FF0084000000FFFFFF00FFFFFF00840000000000
      FF00FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      000084000000000000000000FF0000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C6000000FF00840000008400000084000000840000000000
      FF00C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000FFFFFF00FFFF
      FF0084000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF0084000000FFFF
      FF00FFFFFF00000000000000FF0000000000C6C6C600FFFFFF00FFFFFF00C6C6
      C600FFFFFF00FFFFFF00C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600FFFFFF00FFFFFF00C6C6C60000000000C6C6C600FFFFFF00FFFF
      FF00C6C6C600FFFFFF000000FF000000FF000000FF000000FF000000FF000000
      FF00FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      000084000000000000000000FF0000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000FFFFFF00FFFF
      FF0084000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF0084000000FFFF
      FF00FFFFFF00000000000000FF0000000000C6C6C600FFFFFF00FFFFFF00C6C6
      C600FFFFFF00FFFFFF00C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600FFFFFF00FFFFFF00C6C6C60000000000C6C6C600FFFFFF00FFFF
      FF00C6C6C600FFFFFF00FFFFFF00C6C6C600FFFFFF00FFFFFF00C6C6C600FFFF
      FF00FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000FFFFFF00FFFF
      FF0084000000FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF0084000000FFFF
      FF00FFFFFF00000000000000FF0000000000C6C6C600FFFFFF00FFFFFF00C6C6
      C600FFFFFF00FFFFFF00C6C6C60000000000840000000000FF00840000000000
      0000C6C6C600FFFFFF00FFFFFF00C6C6C60000000000C6C6C600FFFFFF00FFFF
      FF00C6C6C600FFFFFF00FFFFFF00C6C6C600FFFFFF00FFFFFF00C6C6C600FFFF
      FF00FFFFFF00C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600000000000000000000000000000000000000
      0000C6C6C600C6C6C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF000001FFFFFFFF00000001011080030000
      0001011080030000000101108003000000010110800300000001011080030000
      0001011080030000000101108003000000010110800300000001011080030000
      0001011080030000000101108003000000010110800300000001011080030000
      00010110800300000001FFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
end