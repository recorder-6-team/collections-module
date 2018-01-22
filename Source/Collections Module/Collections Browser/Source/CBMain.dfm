object frmCBMain: TfrmCBMain
  Left = 592
  Top = 565
  Width = 696
  Height = 480
  AxBorderStyle = afbNone
  Caption = 'frmCBMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = ActiveFormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter: TSplitter
    Left = 298
    Top = 0
    Width = 6
    Height = 420
    Align = alRight
    ResizeStyle = rsUpdate
    OnCanResize = SplitterCanResize
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 420
    Width = 688
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnAdd: TImageListButton
      Left = 4
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Add'
      Enabled = False
      TabOrder = 0
      OnClick = btnAddClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 0
      Spacing = -1
    end
    object btnEdit: TImageListButton
      Left = 88
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Edit'
      Enabled = False
      TabOrder = 1
      OnClick = btnEditClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 1
      Spacing = -1
    end
    object btnDelete: TImageListButton
      Left = 172
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Delete'
      Enabled = False
      TabOrder = 2
      OnClick = btnDeleteClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 2
      Spacing = -1
    end
  end
  object pnlDetails: TPanel
    Left = 304
    Top = 0
    Width = 384
    Height = 420
    Align = alRight
    BevelOuter = bvLowered
    BorderWidth = 4
    Constraints.MinHeight = 367
    Constraints.MinWidth = 384
    TabOrder = 1
  end
  inline fraCBNavigation: TfraCBNavigation
    Left = 0
    Top = 0
    Width = 298
    Height = 420
    Align = alClient
    TabOrder = 2
    inherited tlbTree: TToolBar
      Width = 298
      inherited cmbView: TComboBoxEx
        OnChange = fraCBNavigationcmbViewChange
      end
    end
    inherited pnlFilter: TPanel
      Width = 298
      DesignSize = (
        298
        29)
      inherited Label1: TLabel
        Height = 14
      end
      inherited cmbSearch: TConceptGroupComboBox
        Height = 22
        ItemHeight = 14
      end
      inherited btnSearchType: TButton
        Left = 275
      end
      inherited eSearch: TEdit
        Width = 214
        Height = 22
      end
      inherited btnGo: TButton
        Left = 252
      end
    end
    inherited tvNav: TRapidTree
      Width = 298
      Height = 369
      OnChanging = fraCBNavigationtvNavChanging
      OnChange = fraCBNavigationtvNavChange
      Data = {0400000000000000}
    end
    inherited pmTree: TPopupMenu
      inherited pmExpandList: TMenuItem
        OnClick = fraCBNavigationpmExpandListClick
      end
      inherited pmTreeNewWindow: TMenuItem
        OnClick = fraCBNavigationpmTreeNewWindowClick
      end
      inherited pmTreeRefresh: TMenuItem
        OnClick = fraCBNavigationpmTreeRefreshClick
      end
    end
  end
  object pmBack: TPopupMenu
    Left = 332
    Top = 124
  end
  object pmForward: TPopupMenu
    Left = 392
    Top = 124
  end
  object pmAdd: TPopupMenu
    Left = 12
    Top = 380
  end
end
