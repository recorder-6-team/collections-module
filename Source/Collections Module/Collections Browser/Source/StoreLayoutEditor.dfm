object frmStorageLayout: TfrmStorageLayout
  Left = 327
  Top = 197
  Width = 626
  Height = 560
  AxBorderStyle = afbNone
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnKeyDown = ActiveFormKeyDown
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Height = 526
  end
  object pnlStoreList: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 526
    Align = alLeft
    Constraints.MinWidth = 100
    TabOrder = 0
    object Label1: TLabel
      Left = 4
      Top = 8
      Width = 82
      Height = 14
      Caption = 'Available Stores:'
    end
    object lbAvailableStores: TIDListBox
      Left = 1
      Top = 17
      Width = 183
      Height = 508
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      DragMode = dmAutomatic
      ItemHeight = 14
      Sorted = True
      Style = lbOwnerDrawFixed
      TabOrder = 0
      OnDblClick = lbAvailableStoresDblClick
      OnDrawItem = lbAvailableStoresDrawItem
      OnKeyDown = ActiveFormKeyDown
    end
  end
  object pnlDiagram: TPanel
    Left = 188
    Top = 0
    Width = 430
    Height = 526
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButton: TPanel
      Left = 0
      Top = 485
      Width = 430
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        430
        41)
      object btnApply: TImageListButton
        Left = 350
        Top = 12
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Apply'
        TabOrder = 0
        OnClick = btnApplyClick
        ImageList = dmInterface.ilButtons
        ImageIndex = 17
      end
    end
  end
end
