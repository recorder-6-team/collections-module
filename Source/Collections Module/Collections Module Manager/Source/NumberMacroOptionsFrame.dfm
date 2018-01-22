object fraNumberMacroOptions: TfraNumberMacroOptions
  Left = 0
  Top = 0
  Width = 482
  Height = 233
  AutoScroll = False
  TabOrder = 0
  object Label15: TLabel
    Left = 12
    Top = 36
    Width = 128
    Height = 13
    Caption = 'Number Generation Macro '
  end
  object Label16: TLabel
    Left = 12
    Top = 84
    Width = 46
    Height = 13
    Caption = 'Example: '
  end
  object lblExample: TLabel
    Left = 60
    Top = 84
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label17: TLabel
    Left = 4
    Top = 8
    Width = 173
    Height = 13
    Caption = 'Select number type to edit macro for:'
  end
  object Label1: TLabel
    Left = 12
    Top = 212
    Width = 311
    Height = 13
    Caption = 
      ' When restarting the ID sequence, start with the following numbe' +
      'r:'
  end
  object btnID: TButton
    Left = 232
    Top = 32
    Width = 45
    Height = 21
    Caption = 'ID'
    TabOrder = 1
    OnClick = btnIDClick
  end
  object btnYear: TButton
    Left = 276
    Top = 32
    Width = 45
    Height = 21
    Caption = 'Year'
    TabOrder = 2
    OnClick = btnYearClick
  end
  object btnMonth: TButton
    Left = 320
    Top = 32
    Width = 45
    Height = 21
    Caption = 'Month'
    TabOrder = 3
    OnClick = btnMonthClick
  end
  object btnDay: TButton
    Left = 364
    Top = 32
    Width = 45
    Height = 21
    Caption = 'Day'
    TabOrder = 4
    OnClick = btnDayClick
  end
  object btnDept: TButton
    Left = 408
    Top = 32
    Width = 45
    Height = 21
    Hint = 'Current logged in user'#39's initials'
    Caption = 'Dept'
    TabOrder = 5
    OnClick = btnDeptClick
  end
  object gbIdGeneration: TGroupBox
    Left = 12
    Top = 104
    Width = 441
    Height = 101
    Caption = 'ID Generation:'
    TabOrder = 7
    object rbGlobalID: TRadioButton
      Left = 12
      Top = 16
      Width = 361
      Height = 17
      Caption = 'Generate a globally unique ID for this type of number'
      TabOrder = 0
      OnClick = rbGlobalIDClick
    end
    object rbMacroID: TRadioButton
      Left = 12
      Top = 36
      Width = 221
      Height = 29
      Caption = 
        'Generate an ID that is unique for the results of the following m' +
        'acro:'
      TabOrder = 1
      WordWrap = True
      OnClick = rbMacroIDClick
    end
    object btnIDYear: TButton
      Left = 256
      Top = 48
      Width = 45
      Height = 21
      Caption = 'Year'
      TabOrder = 2
      OnClick = btnIDYearClick
    end
    object btnIDMonth: TButton
      Left = 300
      Top = 48
      Width = 45
      Height = 21
      Caption = 'Month'
      TabOrder = 3
      OnClick = btnIDMonthClick
    end
    object btnIDDay: TButton
      Left = 344
      Top = 48
      Width = 45
      Height = 21
      Caption = 'Day'
      TabOrder = 4
      OnClick = btnIDDayClick
    end
    object btnIDDept: TButton
      Left = 388
      Top = 48
      Width = 45
      Height = 21
      Hint = 'Current logged in user'#39's initials'
      Caption = 'Dept'
      TabOrder = 5
      OnClick = btnIDDeptClick
    end
    object mmIDMacro: TMemo
      Left = 12
      Top = 68
      Width = 422
      Height = 25
      MaxLength = 100
      ScrollBars = ssVertical
      TabOrder = 6
      OnChange = mmIDMacroChange
      OnExit = mmIDMacroExit
    end
  end
  object cmbNumberType: TComboBox
    Left = 188
    Top = 4
    Width = 253
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cmbNumberTypeChange
  end
  object mmMacro: TMemo
    Left = 12
    Top = 52
    Width = 442
    Height = 25
    MaxLength = 200
    ScrollBars = ssVertical
    TabOrder = 6
    OnChange = mmMacroChange
  end
  object eNumberSequenceStart: TEdit
    Left = 336
    Top = 208
    Width = 117
    Height = 21
    TabOrder = 8
    OnChange = eNumberSequenceStartChange
  end
end
