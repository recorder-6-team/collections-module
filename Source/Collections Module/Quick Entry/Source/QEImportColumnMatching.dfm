inherited fraQEImportColumnMatching: TfraQEImportColumnMatching
  Width = 501
  Height = 320
  DesignSize = (
    501
    320)
  object Label2: TLabel
    Left = 16
    Top = 20
    Width = 128
    Height = 13
    Caption = 'Columns to Fields mapping:'
  end
  object sgMap: TDSSStringGrid
    Left = 16
    Top = 40
    Width = 469
    Height = 260
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    DefaultRowHeight = 21
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
    TabOrder = 2
    OnCellSelectedCustom = sgMapCellSelectedCustom
    ReadOnly = False
    ColWidths = (
      225
      215)
  end
  object chkColumnTitles: TCheckBox
    Left = 333
    Top = 20
    Width = 161
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'Use first row for column titles'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = chkColumnTitlesClick
  end
  object cmbFields: TComboBox
    Left = 244
    Top = 64
    Width = 216
    Height = 21
    BevelKind = bkFlat
    BevelOuter = bvNone
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    Visible = False
    OnKeyDown = cmbFieldsKeyDown
    OnSelect = cmbFieldsSelect
  end
end
