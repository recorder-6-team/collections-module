inherited fraFieldDataGeoAreas: TfraFieldDataGeoAreas
  Height = 280
  inherited bvlBorder: TBevel
    Height = 272
    Visible = False
  end
  object lblPath: TLabel
    Left = 8
    Top = 228
    Width = 3
    Height = 13
    Anchors = [akLeft, akBottom]
    WordWrap = True
  end
  object sgAreas: TDSSStringGrid
    Left = 4
    Top = 8
    Width = 333
    Height = 217
    HelpType = htKeyword
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 1
    FixedCols = 0
    RowCount = 2
    TabOrder = 0
    OnKeyDown = sgAreasKeyDown
    OnMouseDown = sgAreasMouseDown
    OnMouseMove = sgAreasMouseMove
    OnSelectCell = sgAreasSelectCell
    ReadOnly = True
    ColWidths = (
      304)
  end
  object btnAdd: TImageListButton
    Left = 338
    Top = 5
    Width = 22
    Height = 22
    Hint = 'Add'
    TabOrder = 2
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnRemove: TImageListButton
    Left = 338
    Top = 27
    Width = 22
    Height = 22
    Hint = 'Delete'
    TabOrder = 3
    OnClick = btnRemoveClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object eName: TLinkedEdit
    Tag = 1
    Left = 6
    Top = 25
    Width = 303
    Height = 21
    OnExit = eNameExit
    TabOrder = 1
    Visible = False
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
    OnChange = eNameChange
    OnFindData = eNameFindData
    OnGetData = eNameGetData
    OnKeyDown = eNameKeyDown
    ShowDragDropBorder = False
  end
end
