inherited fraQEImportFileSelect: TfraQEImportFileSelect
  Width = 482
  Height = 503
  VertScrollBar.Visible = False
  object pnlDelimiters: TPanel
    Left = 0
    Top = 61
    Width = 482
    Height = 172
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 0
    Visible = False
    DesignSize = (
      482
      172)
    object lblQualifier: TLabel
      Left = 299
      Top = 111
      Width = 65
      Height = 13
      Caption = 'Text &Qualifier:'
    end
    object lblRecSeparator: TLabel
      Left = 299
      Top = 87
      Width = 87
      Height = 13
      Caption = '&Record Separator:'
    end
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 105
      Height = 13
      Caption = 'Delimiter selection'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Shape1: TShape
      Left = 124
      Top = 19
      Width = 353
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object rgSeparator: TRadioGroup
      Left = 44
      Top = 76
      Width = 235
      Height = 85
      Caption = ' Delimiters '
      Columns = 2
      Ctl3D = False
      ItemIndex = 0
      Items.Strings = (
        'Tab'
        'Semicolon (;)'
        'Comma (,)'
        'Space'
        'Other Symbol:')
      ParentCtl3D = False
      TabOrder = 2
      OnClick = rgSeparatorClick
    end
    object eSymbol: TEdit
      Left = 183
      Top = 133
      Width = 33
      Height = 21
      Ctl3D = True
      MaxLength = 1
      ParentCtl3D = False
      TabOrder = 3
    end
    object cmbTextQualifier: TComboBox
      Left = 392
      Top = 106
      Width = 70
      Height = 21
      Ctl3D = False
      ItemHeight = 13
      ItemIndex = 1
      ParentCtl3D = False
      TabOrder = 5
      Text = '"'
      Items.Strings = (
        ' '
        '"'
        #39)
    end
    object cmbRecSeparator: TComboBox
      Left = 392
      Top = 82
      Width = 70
      Height = 21
      Style = csDropDownList
      Ctl3D = False
      ItemHeight = 13
      ItemIndex = 0
      ParentCtl3D = False
      TabOrder = 4
      Text = 'CRLF'
      Items.Strings = (
        'CRLF'
        'CR'
        'LF')
    end
    object rbDelimited: TRadioButton
      Left = 26
      Top = 54
      Width = 380
      Height = 17
      Caption = 
        '&Delimited - Characters such as comma or tab separate each field' +
        '.'
      Checked = True
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 1
      TabStop = True
      OnClick = rbDelimitedClick
    end
    object rbFixedWidth: TRadioButton
      Left = 26
      Top = 32
      Width = 380
      Height = 17
      Caption = 
        'Fixed &Width - Fields are aligned in columns with spaces between' +
        ' each field.'
      TabOrder = 0
      OnClick = rbFixedWidthClick
    end
    object btnMore: TBitBtn
      Left = 377
      Top = 139
      Width = 85
      Height = 22
      Caption = '&More'
      TabOrder = 6
      OnClick = btnMoreClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000
        0000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000
        0000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000
        0000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphRight
      Spacing = 10
    end
    object btnLess: TBitBtn
      Left = 377
      Top = 139
      Width = 85
      Height = 22
      Caption = 'L&ess'
      TabOrder = 7
      Visible = False
      OnClick = btnLessClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000000000000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF000000000000000000000000000000000000000000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
        0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
        0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphRight
      Spacing = 10
    end
  end
  object pnlFormatting: TPanel
    Left = 0
    Top = 233
    Width = 482
    Height = 144
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 1
    Visible = False
    DesignSize = (
      482
      144)
    object lblAdvanced: TLabel
      Left = 12
      Top = 12
      Width = 107
      Height = 13
      Caption = 'Formatting Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object shpAdvanced: TShape
      Left = 124
      Top = 19
      Width = 353
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object gbImportedRows: TGroupBox
      Left = 28
      Top = 32
      Width = 221
      Height = 97
      Caption = ' Imported Rows '
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
      object Label6: TLabel
        Left = 36
        Top = 68
        Width = 20
        Height = 13
        Caption = 'from'
      end
      object Label7: TLabel
        Left = 124
        Top = 68
        Width = 9
        Height = 13
        Caption = 'to'
      end
      object rbImportAllRows: TRadioButton
        Left = 16
        Top = 20
        Width = 113
        Height = 17
        Caption = 'Import all rows'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = rbImportAllRowsClick
      end
      object rbImportSelectedRows: TRadioButton
        Left = 16
        Top = 44
        Width = 173
        Height = 17
        Caption = 'Import the following rows only'
        TabOrder = 1
        OnClick = rbImportSelectedRowsClick
      end
      object eRowsFrom: TNumberEdit
        Left = 60
        Top = 64
        Width = 57
        Height = 21
        Ctl3D = True
        Enabled = False
        ParentCtl3D = False
        TabOrder = 2
        Maximum = 9999
      end
      object eRowsTo: TNumberEdit
        Left = 144
        Top = 64
        Width = 57
        Height = 21
        Ctl3D = True
        Enabled = False
        ParentCtl3D = False
        TabOrder = 3
        Maximum = 9999
      end
    end
    object gbDateTime: TGroupBox
      Left = 260
      Top = 32
      Width = 205
      Height = 97
      Caption = ' Dates and Times '
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 1
      object lblDateOrder: TLabel
        Left = 16
        Top = 28
        Width = 61
        Height = 13
        Caption = 'Date &Format:'
      end
      object lblDateDelimiter: TLabel
        Left = 16
        Top = 56
        Width = 69
        Height = 13
        Caption = 'Date De&limiter:'
        FocusControl = eDateDelimiter
      end
      object eDateDelimiter: TEdit
        Left = 92
        Top = 52
        Width = 21
        Height = 21
        Ctl3D = True
        MaxLength = 1
        ParentCtl3D = False
        TabOrder = 1
        Text = '/'
      end
      object cmbDateFormat: TComboBox
        Left = 92
        Top = 24
        Width = 97
        Height = 21
        Ctl3D = True
        ItemHeight = 13
        ParentCtl3D = False
        TabOrder = 0
        Text = 'dd/mm/yyyy'
        Items.Strings = (
          'dd/mm/yyyy'
          'dd/mmmm/yyyy'
          'dd/mm/yy'
          'dd/mmm/yy'
          'mm/dd/yyyy'
          'mmm/dd/yyyy'
          'mm/dd/yy'
          'mmm/dd/yy'
          'yyyy/mm/dd')
      end
    end
  end
  object pnlTableSelect: TPanel
    Left = 0
    Top = 377
    Width = 482
    Height = 332
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = True
    ParentColor = True
    TabOrder = 2
    Visible = False
    DesignSize = (
      482
      332)
    object Label4: TLabel
      Left = 12
      Top = 12
      Width = 64
      Height = 13
      Caption = 'Data Table'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Shape3: TShape
      Left = 84
      Top = 19
      Width = 393
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object Label5: TLabel
      Left = 44
      Top = 36
      Width = 322
      Height = 13
      Caption = 
        'Please select the table that will be used for import from the li' +
        'st below:'
    end
    object lbTables: TListBox
      Left = 44
      Top = 56
      Width = 418
      Height = 268
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbTablesClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 482
    Height = 61
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      482
      61)
    object Shape2: TShape
      Left = 92
      Top = 19
      Width = 383
      Height = 1
      Anchors = [akLeft, akTop, akRight]
    end
    object Label2: TLabel
      Left = 12
      Top = 12
      Width = 75
      Height = 13
      Caption = 'File to Import'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object eFilePath: TEdit
      Left = 44
      Top = 32
      Width = 397
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
    object btnGetFilePath: TButton
      Left = 441
      Top = 32
      Width = 21
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btnGetFilePathClick
    end
  end
  object FileOpenDialog: TOpenDialog
    Filter = 
      'CSV file (*.csv)|*.csv|Excel file (*.xls)|*.xls|Text file (*.txt' +
      ')|*.txt|DBase file (*.dbf)|*.dbf|Lotus 1-2-3 file (*.wk1)|*.wk1|' +
      'Paradox file (*.db)|*.db|QuattroPro file (*.wq1)|*.wq1|ADO Conne' +
      'ction'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    OnTypeChange = FileOpenDialogTypeChange
    Left = 8
    Top = 32
  end
end
