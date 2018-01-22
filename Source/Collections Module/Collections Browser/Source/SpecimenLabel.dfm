object dlgSpecimenLabel: TdlgSpecimenLabel
  Left = 459
  Top = 119
  Width = 543
  Height = 371
  BorderIcons = [biSystemMenu]
  Caption = 'Specimen Label Output'
  Color = clBtnFace
  Constraints.MinHeight = 322
  Constraints.MinWidth = 543
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormCreate
  DesignSize = (
    535
    344)
  PixelsPerInch = 96
  TextHeight = 14
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 521
    Height = 280
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object lblTreeView: TLabel
    Left = 16
    Top = 64
    Width = 68
    Height = 14
    Caption = 'Select Report:'
  end
  object lblReportPath: TLabel
    Left = 16
    Top = 16
    Width = 82
    Height = 14
    Caption = 'Report Directory:'
  end
  object tvSelectedReport: TTreeView
    Left = 16
    Top = 80
    Width = 249
    Height = 176
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnClick = tvSelectedReportClick
    OnDblClick = tvSelectedReportDblClick
  end
  object chkCreateLabel: TCheckBox
    Left = 16
    Top = 265
    Width = 161
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Generate Label Records'
    TabOrder = 1
  end
  object txtReportPath: TEdit
    Left = 16
    Top = 32
    Width = 505
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
  end
  object mmReportDescription: TRichEdit
    Left = 264
    Top = 80
    Width = 257
    Height = 176
    Anchors = [akTop, akRight, akBottom]
    Color = cl3DLight
    Lines.Strings = (
      'mmReportDescription')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object btnCancel: TImageListButton
    Left = 456
    Top = 297
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object btnOk: TImageListButton
    Left = 368
    Top = 297
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    Enabled = False
    ModalResult = 1
    TabOrder = 5
    OnClick = btnOkClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
end
