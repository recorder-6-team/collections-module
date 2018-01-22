object frmUCEMain: TfrmUCEMain
  Left = 263
  Top = 107
  Width = 595
  Height = 317
  AxBorderStyle = afbNone
  Caption = 'frmUCEMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnEdit: TImageListButton
    Left = 8
    Top = 260
    Width = 75
    Height = 25
    Caption = '&Edit'
    TabOrder = 1
    OnClick = btnEditClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 1
    Spacing = -1
  end
  object lvUsers: TListView
    Left = 8
    Top = 8
    Width = 154
    Height = 248
    Columns = <
      item
        Caption = 'User names'
        Width = 150
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    SortType = stText
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = lvUsersChange
  end
  object pnlDetails: TPanel
    Left = 166
    Top = 8
    Width = 419
    Height = 277
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    DesignSize = (
      419
      277)
    object Label4: TLabel
      Left = 8
      Top = 32
      Width = 75
      Height = 13
      Caption = 'System Access:'
    end
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 54
      Height = 13
      Caption = '&User name:'
    end
    object Label2: TLabel
      Left = 8
      Top = 84
      Width = 77
      Height = 13
      Caption = 'Domain Access:'
    end
    object lblUserName: TLabel
      Left = 68
      Top = 8
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object btnSave: TImageListButton
      Left = 251
      Top = 245
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Save'
      Enabled = False
      TabOrder = 0
      OnClick = btnSaveClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 6
      Spacing = -1
    end
    object btnCancel: TImageListButton
      Left = 335
      Top = 245
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = 'Cancel'
      Enabled = False
      TabOrder = 1
      OnClick = btnCancelClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 4
      Spacing = -1
    end
    object sgPermissions: TStringGrid
      Left = 8
      Top = 100
      Width = 401
      Height = 141
      Anchors = [akLeft, akTop, akBottom]
      DefaultRowHeight = 18
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing]
      TabOrder = 2
      OnClick = sgPermissionsClick
      OnDrawCell = sgPermissionsDrawCell
      ColWidths = (
        107
        64
        77
        64
        64)
      RowHeights = (
        18
        18)
    end
    object chkQuickEntry: TCheckBox
      Left = 92
      Top = 32
      Width = 97
      Height = 17
      Caption = '&Quick Entry'
      Enabled = False
      TabOrder = 3
    end
    object chkProcessQuickEntry: TCheckBox
      Left = 232
      Top = 32
      Width = 177
      Height = 17
      Caption = '&Processing of Quick Entry Cards'
      Enabled = False
      TabOrder = 4
    end
    object chkFinance: TCheckBox
      Left = 232
      Top = 52
      Width = 173
      Height = 17
      Caption = '&Finance (Valuations && Funding)'
      Enabled = False
      TabOrder = 5
    end
    object chkMovements: TCheckBox
      Left = 92
      Top = 52
      Width = 133
      Height = 17
      Caption = '&Enter/Edit Movements'
      Enabled = False
      TabOrder = 6
    end
  end
end
