inherited fraThesaurusNavigator: TfraThesaurusNavigator
  Width = 318
  Height = 498
  Constraints.MinWidth = 220
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Arial'
  OnEnter = FrameEnter
  OnResize = FrameResize
  inherited pnlTree: TPanel
    Top = 73
    Width = 318
    Height = 425
    TabOrder = 1
    inherited tvHierarchy: TRapidTree
      Width = 310
      Height = 417
      RowCount = 1
      PopupMenu = pmNode
      OnDrawCell = tvHierarchyDrawCell
      OnSelectCell = tvHierarchySelectCell
      OnExpanding = tvHierarchyExpanding
      OnChange = tvHierarchyChange
      Data = {
        9C00000001000000060854466C794E6F64658A000000060943656C6C73546578
        741300000006020D0A060648696464656E2000000000060A496D616765496E64
        65783400000000000000060D53656C6563746564496E6465784B000000000000
        00060A5374617465496E6465785F000000000000000604546578747300000006
        0854466C794E6F64650609556E6971756554616786000000FFFFFFFF00000000}
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 318
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    PopupMenu = pmNode
    TabOrder = 0
    DesignSize = (
      318
      73)
    object Label3: TLabel
      Left = 8
      Top = 56
      Width = 23
      Height = 14
      Caption = 'Find:'
    end
    object eSearch: TEdit
      Left = 96
      Top = 51
      Width = 169
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Enabled = False
      TabOrder = 1
      OnKeyPress = eSearchKeyPress
    end
    object btnGo: TButton
      Left = 267
      Top = 51
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Go'
      Enabled = False
      TabOrder = 2
      OnClick = btnGoClick
    end
    object btnSearchMode: TBitBtn
      Left = 291
      Top = 51
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Enabled = False
      TabOrder = 3
      OnClick = btnSearchModeClick
      Glyph.Data = {
        72000000424D72000000000000003E00000028000000090000000D0000000100
        010000000000340000000000000000000000020000000200000000000000FFFF
        FF00FF800000FF800000FF800000FF800000FF800000F7800000E3800000C180
        000080800000FF800000FF800000FF800000FF800000}
    end
    inline fraDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector
      Left = 8
      Top = 4
      Width = 305
      Height = 46
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      inherited lblDomain: TLabel
        Width = 38
        Height = 14
      end
      inherited lblConceptGroup: TLabel
        Width = 76
        Height = 14
      end
      inherited cmbDomains: TComboBox
        Width = 217
      end
      inherited btnHistory: TImageListButton
        Left = 283
      end
      inherited cmbConceptGroups: TLuxIDComboBox
        Width = 195
        Height = 22
        ItemHeight = 14
        OnChange = fraDomainConceptGroupsSelectorcmbConceptGroupsChange
        OnKeyDown = fraDomainConceptGroupsSelectorcmbConceptGroupsKeyDown
      end
    end
  end
  object pmSearchMode: TPopupMenu
    Left = 104
    Top = 112
    object pmSearchModeFind: TMenuItem
      Caption = 'Find'
      OnClick = pmSearchModeFindClick
    end
    object pmSearchModePopulateTopLevel: TMenuItem
      Caption = 'Populate Top Level'
      OnClick = pmSearchModePopulateTopLevelClick
    end
  end
  object pmNode: TPopupMenu
    Images = dmInterface.ilMenuItems
    Left = 32
    Top = 112
    object pmNodeCopy: TMenuItem
      Caption = '&Copy'
      ImageIndex = 2
      ShortCut = 16451
      OnClick = pmNodeCopyClick
    end
    object pmNodeRefreshNodeChildren: TMenuItem
      Caption = 'Refresh Node Children'
      ShortCut = 116
      OnClick = pmNodeRefreshNodeChildrenClick
    end
    object pmNodeShowAncestorHierarchy: TMenuItem
      Caption = 'Show &Ancestor Hierarchy'
      OnClick = pmNodeShowAncestorHierarchyClick
    end
  end
end
