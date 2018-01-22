inherited frmConceptGroupQualityChecker: TfrmConceptGroupQualityChecker
  Left = 228
  Top = 220
  Width = 950
  Height = 448
  Caption = 'Concept Group Quality Checker'
  Font.Name = 'Arial'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter: TSplitter
    Left = 0
    Top = 268
    Width = 942
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    Beveled = True
    MinSize = 70
    ResizeStyle = rsUpdate
    OnMoved = SplitterMoved
  end
  object sgScanResults: TFilteredStringGrid
    Left = 0
    Top = 0
    Width = 942
    Height = 268
    Align = alClient
    ColCount = 8
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor]
    TabOrder = 0
    OnClick = sgScanResultsClick
    OnDblClick = sgScanResultsDblClick
    OnDrawCell = sgScanResultsDrawCell
    OnKeyDown = sgScanResultsKeyDown
    ReadOnly = False
    ColWidths = (
      120
      64
      64
      64
      64
      64
      64
      64)
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 271
    Width = 942
    Height = 150
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlBetween: TPanel
      Left = 0
      Top = 0
      Width = 942
      Height = 24
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object chkResolveDuplicates: TCheckBox
        Left = 5
        Top = 4
        Width = 229
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Resolve duplicate terms to a single record'
        TabOrder = 0
      end
    end
    object sgSelectedItem: TDSSStringGrid
      Left = 0
      Top = 24
      Width = 942
      Height = 91
      Align = alClient
      ColCount = 8
      Ctl3D = False
      RowCount = 3
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
      ParentCtl3D = False
      TabOrder = 1
      OnDrawCell = sgSelectedItemDrawCell
      ReadOnly = True
      ColWidths = (
        120
        120
        100
        85
        120
        100
        64
        64)
    end
    object pnlButtons: TPanel
      Left = 0
      Top = 115
      Width = 942
      Height = 35
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      DesignSize = (
        942
        35)
      object btnProceed: TImageListButton
        Left = 607
        Top = 4
        Width = 93
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Proceed'
        TabOrder = 1
        OnClick = btnProceedClick
        ImageList = dmInterface.ilBrowserNodes
        ImageIndex = 26
      end
      object btnSelection: TImageListButton
        Left = 204
        Top = 4
        Width = 93
        Height = 25
        Caption = 'Unselect All'
        TabOrder = 0
        OnClick = btnSelectionClick
        ImageList = dmInterface.ilButtons
        ImageIndex = 22
      end
    end
  end
  object cmbResolution: TComboBox
    Left = 720
    Top = 24
    Width = 81
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 2
    Visible = False
    OnClick = cmbResolutionClick
    OnExit = cmbResolutionExit
  end
  object btnResolutionFilter: TPopupButton
    Left = 559
    Top = 4
    Width = 15
    Height = 15
    Hint = 'Filter on the resolution selected'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Visible = False
    ImageList = dmInterface.ilButtons
    ImageIndex = 9
  end
  object pmResolutionFilter: TPopupMenu
    Left = 616
    Top = 80
    object mnuToBeProcessed: TMenuItem
      Caption = 'To Be Processed'
      OnClick = FilterMenuClick
    end
    object mnuMakeSynonym: TMenuItem
      Caption = 'Make Synonym'
      OnClick = FilterMenuClick
    end
    object mnuMakeHomonym: TMenuItem
      Caption = 'Not a Synonym'
      OnClick = FilterMenuClick
    end
  end
end
