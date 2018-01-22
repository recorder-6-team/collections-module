object dlgReportConfiguration: TdlgReportConfiguration
  Left = 261
  Top = 107
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Report Configuration'
  ClientHeight = 394
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    435
    394)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 4
    Top = 4
    Width = 425
    Height = 349
    Anchors = [akLeft, akTop, akRight, akBottom]
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 12
    Top = 127
    Width = 220
    Height = 13
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Select sections to include and their sort orders:'
  end
  object Label2: TLabel
    Left = 12
    Top = 12
    Width = 216
    Height = 13
    Caption = 'Enter a summary of the report and its purpose:'
  end
  object Label3: TLabel
    Left = 16
    Top = 99
    Width = 54
    Height = 13
    Caption = 'Output File:'
  end
  object btnOk: TImageListButton
    Left = 258
    Top = 361
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Enabled = False
    ModalResult = 1
    TabOrder = 3
    OnClick = btnOkClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 350
    Top = 361
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object cmbSortOrder: TComboBox
    Left = 24
    Top = 365
    Width = 144
    Height = 21
    BevelInner = bvNone
    BevelKind = bkFlat
    BevelOuter = bvNone
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    Ctl3D = False
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 5
    Visible = False
    OnChange = cmbSortOrderChange
  end
  object mmSummary: TMemo
    Left = 12
    Top = 28
    Width = 407
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object eOutputPath: TEdit
    Left = 80
    Top = 96
    Width = 318
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = eOutputPathChange
  end
  object btnGetOutputPath: TButton
    Left = 398
    Top = 96
    Width = 21
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = btnGetOutputPathClick
  end
end
