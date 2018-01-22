object fraGeneralOptions: TfraGeneralOptions
  Left = 0
  Top = 0
  Width = 482
  Height = 233
  TabOrder = 0
  object Label1: TLabel
    Left = 9
    Top = 19
    Width = 153
    Height = 13
    Caption = 'Standard Report Template Path:'
  end
  object Label2: TLabel
    Left = 9
    Top = 47
    Width = 107
    Height = 13
    Caption = 'Specimen Image Path:'
  end
  object chkGroupByDomain: TCheckBox
    Left = 7
    Top = 110
    Width = 209
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Group determinations by domain:'
    TabOrder = 6
  end
  object eStandardReportTemplatePath: TEdit
    Left = 203
    Top = 16
    Width = 246
    Height = 21
    TabOrder = 0
  end
  object eSpecimentImagePath: TEdit
    Left = 203
    Top = 44
    Width = 246
    Height = 21
    TabOrder = 2
  end
  object btnGetStandardReportTemplatePath: TButton
    Left = 448
    Top = 16
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = btnGetStandardReportTemplatePathClick
  end
  object btnGetSpecimentImagePath: TButton
    Left = 448
    Top = 44
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = btnGetSpecimentImagePathClick
  end
  object chkPreferredSynonyms: TCheckBox
    Left = 7
    Top = 70
    Width = 209
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use only preferred synonyms:'
    TabOrder = 4
  end
  object chkUseOriginalNames: TCheckBox
    Left = 7
    Top = 90
    Width = 209
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Use originally entered specimen names:'
    TabOrder = 5
  end
  object chkShowGroupInQuickEntry: TCheckBox
    Left = 7
    Top = 130
    Width = 209
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Show determination lists in Quick Entry:'
    TabOrder = 7
  end
  object chkShowRecorderSpecimensTab: TCheckBox
    Left = 7
    Top = 150
    Width = 209
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Show Recorder specimens tab:'
    TabOrder = 8
  end
  object fbGetPath: TFolderBrowser
    BrowseFlags = []
    NewDialogStyle = False
    Title = 'Select folder'
    Left = 420
    Top = 72
  end
end
