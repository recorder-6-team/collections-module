inherited fraSources: TfraSources
  Width = 367
  Height = 280
  OnResize = FrameResize
  inherited bvlBorder: TBevel
    Width = 359
    Height = 272
  end
  object Label8: TLabel
    Left = 13
    Top = 8
    Width = 95
    Height = 13
    Caption = 'Internal Documents:'
  end
  object shpGrid: TShape
    Left = 13
    Top = 24
    Width = 341
    Height = 106
    Anchors = [akLeft, akTop, akRight]
    Pen.Color = clRed
  end
  object lblExternalRefs: TLabel
    Left = 13
    Top = 141
    Width = 94
    Height = 13
    Caption = 'External references:'
  end
  object shpList: TShape
    Left = 13
    Top = 158
    Width = 341
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Pen.Color = clRed
  end
  object btnViewExternalRef: TButton
    Left = 262
    Top = 248
    Width = 23
    Height = 23
    Anchors = [akRight, akBottom]
    Caption = '...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = btnViewExternalRefClick
  end
  object btnGetInternalRef: TImageListButton
    Left = 285
    Top = 131
    Width = 23
    Height = 23
    Anchors = [akRight]
    TabOrder = 2
    OnClick = btnGetInternalRefClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 12
  end
  object btnAddInternalRef: TImageListButton
    Left = 308
    Top = 131
    Width = 23
    Height = 23
    Hint = 'Add'
    Anchors = [akRight]
    TabOrder = 3
    OnClick = btnAddInternalRefClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnRemoveInternalRef: TImageListButton
    Left = 331
    Top = 131
    Width = 23
    Height = 23
    Hint = 'Delete'
    Anchors = [akRight]
    TabOrder = 4
    OnClick = btnRemoveInternalRefClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object btnRemoveExternalRef: TImageListButton
    Left = 331
    Top = 248
    Width = 23
    Height = 23
    Hint = 'Delete'
    Anchors = [akRight, akBottom]
    TabOrder = 9
    OnClick = btnRemoveExternalRefClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 2
  end
  object btnAddExternalRef: TImageListButton
    Left = 308
    Top = 248
    Width = 23
    Height = 23
    Hint = 'Add'
    Anchors = [akRight, akBottom]
    TabOrder = 8
    OnClick = btnAddExternalRefClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 0
  end
  object btnAddURL: TImageListButton
    Left = 285
    Top = 248
    Width = 23
    Height = 23
    Hint = 'Add'
    Anchors = [akRight, akBottom]
    TabOrder = 7
    OnClick = btnAddURLClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 13
  end
  object lbExternalRefs: TListBox
    Left = 14
    Top = 159
    Width = 339
    Height = 87
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 5
    OnClick = lbExternalRefsClick
    OnDblClick = btnViewExternalRefClick
    OnMouseMove = lbExternalRefsMouseMove
  end
  object sgInternalRefs: TDSSStringGrid
    Left = 14
    Top = 25
    Width = 339
    Height = 104
    Anchors = [akLeft, akTop, akRight]
    ColCount = 3
    FixedCols = 0
    RowCount = 2
    TabOrder = 0
    OnDblClick = sgInternalRefsDblClick
    ReadOnly = True
    ColWidths = (
      204
      45
      65)
  end
  object cmbOriginal: TLuxIDComboBox
    Left = 217
    Top = 46
    Width = 49
    Height = 21
    Anchors = [akLeft]
    ItemHeight = 13
    TabOrder = 1
    Visible = False
    OnPopulate = cmbOriginalPopulate
  end
  object dlgOpenFile: TOpenDialog
    Left = 224
    Top = 192
  end
end
