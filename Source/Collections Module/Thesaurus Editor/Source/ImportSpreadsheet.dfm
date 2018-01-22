object dlgImportSpreadsheet: TdlgImportSpreadsheet
  Left = 395
  Top = 129
  ActiveControl = btnOpenFile
  BorderStyle = bsDialog
  Caption = 'Import Spreadsheet'
  ClientHeight = 528
  ClientWidth = 491
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 6
    Top = 8
    Width = 479
    Height = 483
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 16
    Top = 20
    Width = 50
    Height = 13
    Caption = 'File Name:'
  end
  object Label2: TLabel
    Left = 16
    Top = 52
    Width = 128
    Height = 13
    Caption = 'Columns to Fields mapping:'
  end
  object Label3: TLabel
    Left = 16
    Top = 292
    Width = 79
    Height = 13
    Caption = 'Import Data Into:'
  end
  object eFileName: TEdit
    Left = 76
    Top = 16
    Width = 373
    Height = 21
    TabOrder = 0
    OnChange = eFileNameChange
  end
  object btnImport: TImageListButton
    Left = 322
    Top = 496
    Width = 75
    Height = 25
    Caption = '&Import'
    Enabled = False
    TabOrder = 12
    OnClick = btnImportClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object btnCancel: TImageListButton
    Left = 410
    Top = 496
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 13
    ImageList = dmInterface.ilButtons
    ImageIndex = 4
  end
  object btnOpenFile: TButton
    Left = 448
    Top = 16
    Width = 21
    Height = 21
    Hint = 'Open file for import'
    Caption = '...'
    TabOrder = 1
    OnClick = btnOpenFileClick
  end
  object sgMap: TDSSStringGrid
    Left = 16
    Top = 72
    Width = 457
    Height = 210
    ColCount = 2
    RowCount = 2
    TabOrder = 3
    OnCellSelectedCustom = sgMapCellSelectedCustom
    ReadOnly = True
    ColWidths = (
      225
      226)
  end
  object pnlCover: TPanel
    Left = 18
    Top = 94
    Width = 453
    Height = 187
    BevelOuter = bvNone
    TabOrder = 4
  end
  object cmbFields: TComboBox
    Left = 240
    Top = 108
    Width = 201
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 5
    Visible = False
    OnKeyDown = cmbFieldsKeyDown
    OnSelect = cmbFieldsSelect
    Items.Strings = (
      'Author'
      'Child Of'
      'Citation Date'
      'Designation #1 End Date'
      'Designation #1 Start Date'
      'Designation #1 Status'
      'Designation #2 End Date'
      'Designation #2 Start Date'
      'Designation #2 Status'
      'Fact #1 Description'
      'Fact #1 Title'
      'Fact #1 Type'
      'Fact #2 Description'
      'Fact #2 Title'
      'Fact #2 Type'
      'Fact #3 Description'
      'Fact #3 Title'
      'Fact #3 Type'
      'Fact #4 Description'
      'Fact #4 Title'
      'Fact #4 Type'
      'Fact #5 Description'
      'Fact #5 Title'
      'Fact #5 Type'
      'Language'
      'Language Key'
      'List Code'
      'Name Type'
      'Rank'
      'Sort Code'
      'Synonym Of'
      'Term Name')
  end
  inline fraImportDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector
    Left = 16
    Top = 312
    Width = 457
    Height = 45
    TabOrder = 6
    inherited cmbDomains: TComboBox
      Width = 369
      OnSelect = fraDomainConceptGroupsSelectorcmbDomainsSelect
    end
    inherited btnHistory: TImageListButton
      Left = 435
      Visible = False
    end
    inherited cmbConceptGroups: TLuxIDComboBox
      Width = 369
      OnClick = fraDomainConceptGroupsSelectorcmbConceptGroupsClick
    end
  end
  object chkColumnTitles: TCheckBox
    Left = 312
    Top = 52
    Width = 161
    Height = 17
    Caption = 'Use first row for column titles'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = chkColumnTitlesClick
  end
  object chkConceptIntroducedVersion: TCheckBox
    Left = 104
    Top = 364
    Width = 197
    Height = 17
    Caption = 'Concepts are introduced in version:'
    TabOrder = 7
    OnClick = chkConceptIntroducedVersionClick
  end
  object chkConceptExpiredVersion: TCheckBox
    Left = 104
    Top = 388
    Width = 189
    Height = 17
    Caption = 'Concepts expire after version:'
    TabOrder = 9
    OnClick = chkConceptExpiredVersionClick
  end
  object cmbConceptIntroducedVersion: TLuxIDComboBox
    Left = 308
    Top = 360
    Width = 165
    Height = 21
    Enabled = False
    ItemHeight = 13
    TabOrder = 8
    OnChange = cmbConceptVersionChange
    OnPopulate = cmbVersionPopulate
  end
  object cmbConceptExpiredVersion: TLuxIDComboBox
    Left = 308
    Top = 384
    Width = 165
    Height = 21
    Enabled = False
    ItemHeight = 13
    TabOrder = 10
    OnChange = cmbConceptVersionChange
    OnPopulate = cmbVersionPopulate
  end
  inline fraSynonymDomainConceptGroupsSelector: TfraDomainConceptGroupsSelector
    Left = 16
    Top = 436
    Width = 457
    Height = 45
    Enabled = False
    TabOrder = 11
    inherited cmbDomains: TComboBox
      Width = 369
    end
    inherited btnHistory: TImageListButton
      Left = 435
      Visible = False
    end
    inherited cmbConceptGroups: TLuxIDComboBox
      Width = 369
      OnClick = fraSynonymDomainConceptGroupsSelectorcmbConceptGroupsClick
    end
  end
  object chkUsePrimarySynonymGroup: TCheckBox
    Left = 16
    Top = 416
    Width = 345
    Height = 17
    Caption = 
      'Use the following concept group as the primary source of synonym' +
      's:'
    TabOrder = 14
    OnClick = chkUsePrimarySynonymGroupClick
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'xls'
    Filter = 
      'Excel Spreadsheet (*.xls)|*.xls|Comma Separated Value Files (*.c' +
      'sv)|*.csv'
    Left = 168
    Top = 40
  end
  object excelFile: TSMImportFromXLS
    AbortOnProblem = True
    AnimatedStatus = False
    DataFormats.DateOrder = doDMY
    DataFormats.DateSeparator = '/'
    DataFormats.TimeSeparator = ':'
    DataFormats.FourDigitYear = True
    DataFormats.LeadingZerosInDate = True
    DataFormats.ThousandSeparator = ','
    DataFormats.DecimalSeparator = '.'
    DataFormats.CurrencyString = #163
    DataFormats.BooleanTrue = 'True'
    DataFormats.BooleanFalse = 'False'
    DataFormats.UseRegionalSettings = False
    DataSet = dsImport
    Options = [soExtendedStatistic, soSkipEmptyRow, soUseAnimatedControl]
    TitleStatus = 'Importing...'
    Statistic.TotalCount = 0
    Statistic.Result = irUnknown
    UseDisplayNames = False
    OnAfterRecordEvent = ImportObjectAfterRecord
    SourceFileName = 'SMImport.XLS'
    Left = 200
    Top = 40
  end
  object csvFile: TSMImportFromText
    AbortOnProblem = True
    AnimatedStatus = False
    DataFormats.DateOrder = doDMY
    DataFormats.DateSeparator = '/'
    DataFormats.TimeSeparator = ':'
    DataFormats.FourDigitYear = True
    DataFormats.LeadingZerosInDate = True
    DataFormats.ThousandSeparator = ','
    DataFormats.DecimalSeparator = '.'
    DataFormats.CurrencyString = #163
    DataFormats.BooleanTrue = 'True'
    DataFormats.BooleanFalse = 'False'
    DataFormats.UseRegionalSettings = False
    DataSet = dsImport
    Options = [soExtendedStatistic, soSkipEmptyRow, soUseAnimatedControl]
    TitleStatus = 'Importing...'
    Statistic.TotalCount = 0
    Statistic.Result = irUnknown
    UseDisplayNames = False
    OnAfterRecordEvent = ImportObjectAfterRecord
    SourceFileName = 'SMImport.TXT'
    Fixed = False
    FieldDelimiter = fdComma
    FieldDelimiterCustom = ','
    RecordSeparatorCustom = #13#10
    Left = 232
    Top = 40
  end
  object dsImport: TADOTable
    Left = 264
    Top = 40
  end
end
