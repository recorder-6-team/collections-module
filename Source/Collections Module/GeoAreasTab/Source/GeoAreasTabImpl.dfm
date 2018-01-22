object GeoAreasTabImpl: TGeoAreasTabImpl
  Left = 582
  Top = 190
  Width = 375
  Height = 292
  Anchors = [akLeft, akTop, akRight, akBottom]
  AxBorderStyle = afbNone
  Caption = 'GeoAreasTabImpl'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblPath: TLabel
    Left = 8
    Top = 220
    Width = 3
    Height = 13
    WordWrap = True
  end
  object sgAreas: TDSSStringGrid
    Left = 8
    Top = 8
    Width = 327
    Height = 207
    HelpType = htKeyword
    ColCount = 1
    FixedCols = 0
    RowCount = 2
    TabOrder = 0
    OnKeyDown = sgAreasKeyDown
    OnSelectCell = sgAreasSelectCell
    ReadOnly = True
    ColWidths = (
      308)
  end
  object btnAdd: TImageListButton
    Left = 336
    Top = 8
    Width = 22
    Height = 22
    Hint = 'Add'
    TabOrder = 2
    OnClick = btnAddClick
    ImageList = dmAddinInterface.ilButtons
    ImageIndex = 0
  end
  object btnRemove: TImageListButton
    Left = 336
    Top = 30
    Width = 22
    Height = 22
    Hint = 'Delete'
    TabOrder = 3
    OnClick = btnRemoveClick
    ImageList = dmAddinInterface.ilButtons
    ImageIndex = 2
  end
  object eName: TLinkedEdit
    Tag = 1
    Left = 9
    Top = 29
    Width = 310
    Height = 21
    OnExit = eNameExit
    TabOrder = 1
    Visible = False
    BorderStyle = bsSingle
    ImageIndex = 6
    ImageList = dmAddinInterface.ilButtons
    MaxLength = 0
    OnChange = eNameChange
    OnFindData = eNameFindData
    OnGetData = eNameGetData
    OnKeyDown = eNameKeyDown
    ShowDragDropBorder = False
  end
end
