object frmQEMultiValuePopup: TfrmQEMultiValuePopup
  Left = 286
  Top = 227
  BorderStyle = bsDialog
  Caption = 'frmQEMultiValuePopup'
  ClientHeight = 198
  ClientWidth = 355
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 336
    Height = 149
    Shape = bsFrame
  end
  object sgValues: TDSSStringGrid
    Left = 16
    Top = 16
    Width = 294
    Height = 129
    HelpType = htKeyword
    ColCount = 1
    DefaultRowHeight = 17
    FixedCols = 0
    RowCount = 2
    TabOrder = 0
    OnKeyDown = sgValuesKeyDown
    ReadOnly = True
    ColWidths = (
      287)
  end
  object eName: TLinkedEdit
    Tag = 1
    Left = 17
    Top = 35
    Width = 289
    Height = 19
    Ctl3D = False
    ParentCtl3D = False
    OnExit = eNameExit
    TabOrder = 1
    Visible = False
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    OnFindData = eNameFindData
    OnGetData = eNameGetData
    OnKeyDown = eNameKeyDown
    ShowDragDropBorder = False
  end
  object btnAdd: TImageListButton
    Left = 312
    Top = 16
    Width = 22
    Height = 22
    TabOrder = 2
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnDelete: TImageListButton
    Left = 312
    Top = 37
    Width = 22
    Height = 22
    TabOrder = 3
    OnClick = btnDeleteClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object btnCancel: TImageListButton
    Left = 270
    Top = 164
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object btnOK: TImageListButton
    Left = 186
    Top = 164
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 4
    OnClick = btnOKClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
end
