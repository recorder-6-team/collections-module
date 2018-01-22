inherited fraFieldDataGeneral: TfraFieldDataGeneral
  object Label8: TLabel
    Left = 16
    Top = 16
    Width = 67
    Height = 13
    Caption = 'Survey Name:'
  end
  object lblDate: TLabel
    Left = 16
    Top = 156
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object Label4: TLabel
    Left = 16
    Top = 180
    Width = 49
    Height = 26
    Caption = 'Field Collectors:'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 101
    Width = 55
    Height = 13
    Caption = 'Spatial Ref:'
  end
  object Label3: TLabel
    Left = 16
    Top = 125
    Width = 49
    Height = 26
    Caption = 'Gathering Method:'
    WordWrap = True
  end
  object shpList: TShape
    Left = 84
    Top = 175
    Width = 241
    Height = 70
    Anchors = [akLeft, akTop, akBottom]
    Pen.Color = clRed
  end
  object Label5: TLabel
    Left = 16
    Top = 71
    Width = 75
    Height = 13
    Caption = 'Location Name:'
  end
  object Label1: TLabel
    Left = 16
    Top = 44
    Width = 75
    Height = 13
    Caption = 'Event Location:'
    WordWrap = True
  end
  object chkGatheringEvent: TCheckBox
    Left = 83
    Top = 246
    Width = 236
    Height = 23
    Alignment = taLeftJustify
    Anchors = [akLeft, akBottom]
    Caption = 'Details are for the Gathering Event:'
    TabOrder = 16
    WordWrap = True
  end
  object sgCollectors: TDSSStringGrid
    Left = 85
    Top = 176
    Width = 239
    Height = 68
    Anchors = [akLeft, akTop, akBottom]
    ColCount = 1
    FixedCols = 0
    RowCount = 2
    TabOrder = 11
    OnKeyDown = sgCollectorsKeyDown
    ReadOnly = True
    ColWidths = (
      229)
  end
  object btnAdd: TImageListButton
    Left = 328
    Top = 177
    Width = 21
    Height = 21
    Hint = 'Add'
    TabOrder = 13
    OnClick = btnAddClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnRemove: TImageListButton
    Left = 328
    Top = 198
    Width = 21
    Height = 21
    Hint = 'Delete'
    TabOrder = 14
    OnClick = btnRemoveClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object eSpatialRef: TSpatialRef
    Left = 84
    Top = 91
    Width = 241
    Height = 33
    TabOrder = 5
    DropDownMenu = pmMapWindow
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    OnGetFromMap = eSpatialRefGetFromMap
  end
  object btnSurveyInferred: TImageListButton
    Left = 328
    Top = 12
    Width = 21
    Height = 23
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnLocationInferred: TImageListButton
    Left = 328
    Top = 39
    Width = 21
    Height = 23
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnSpatialRefInferred: TImageListButton
    Left = 328
    Top = 97
    Width = 21
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnDateInferred: TImageListButton
    Left = 233
    Top = 153
    Width = 21
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 10
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnSampleTypeInferred: TImageListButton
    Left = 328
    Top = 127
    Width = 21
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnFieldCollectorsInferred: TImageListButton
    Left = 328
    Top = 224
    Width = 21
    Height = 21
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 15
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object eLocation: TLinkedEdit
    Tag = 1
    Left = 100
    Top = 39
    Width = 225
    Height = 23
    OnExit = eLocationExit
    TabOrder = 2
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
    OnChange = eLocationChange
    OnFindData = eLocationFindData
  end
  object cmbGatheringMethod: TLuxIDComboBox
    Left = 84
    Top = 128
    Width = 237
    Height = 21
    ItemHeight = 13
    TabOrder = 7
    OnPopulate = cmbGatheringMethodPopulate
  end
  object eDate: TVagueDateEdit
    Left = 84
    Top = 153
    Width = 148
    Height = 21
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 9
  end
  object eName: TUserEdit
    Tag = 1
    Left = 86
    Top = 197
    Width = 231
    Height = 21
    OnExit = eNameExit
    TabOrder = 12
    Visible = False
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
    OnFindData = eNameFindData
    OnGetData = eNameGetData
    OnKeyDown = eNameKeyDown
    ShowDragDropBorder = False
  end
  object eVagueLocation: TEdit
    Left = 100
    Top = 66
    Width = 225
    Height = 21
    TabOrder = 4
  end
  object eSurvey: TLinkedEdit
    Tag = 1
    Left = 100
    Top = 12
    Width = 225
    Height = 23
    TabOrder = 0
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
  end
  object pmMapWindow: TPopupMenu
    Images = dmInterface.ilButtons
    Left = 32
    Top = 224
  end
end
