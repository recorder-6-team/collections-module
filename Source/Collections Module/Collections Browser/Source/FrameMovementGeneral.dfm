inherited fraMovementGeneral: TfraMovementGeneral
  Height = 382
  ParentShowHint = False
  ShowHint = True
  inherited bvlBorder: TBevel
    Height = 374
  end
  object lblStaff: TLabel
    Left = 12
    Top = 9
    Width = 61
    Height = 26
    Caption = 'Staff Responsible:'
    WordWrap = True
  end
  object lblNumber: TLabel
    Left = 12
    Top = 190
    Width = 64
    Height = 13
    Caption = '???. Number:'
  end
  object Label6: TLabel
    Left = 12
    Top = 275
    Width = 31
    Height = 13
    Caption = 'Notes:'
  end
  object lblCompletionDate: TLabel
    Left = 12
    Top = 120
    Width = 61
    Height = 26
    Caption = 'Expected Return Date:'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 12
    Top = 218
    Width = 33
    Height = 13
    Hint = 
      'The status is updated when associated movement lines are marked ' +
      'as complete'
    Caption = 'Status:'
  end
  object lblStatus: TLabel
    Left = 78
    Top = 216
    Width = 3
    Height = 13
    Hint = 
      'The status is updated when associated movement lines are marked ' +
      'as complete'
    Constraints.MaxWidth = 109
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblLoanDate: TLabel
    Left = 12
    Top = 100
    Width = 53
    Height = 13
    Caption = 'Loan Date:'
  end
  object lblActualReturnDate: TLabel
    Left = 12
    Top = 149
    Width = 61
    Height = 26
    Caption = 'Actual Return Date:'
    WordWrap = True
  end
  object eNumber: TEdit
    Left = 80
    Top = 186
    Width = 108
    Height = 21
    MaxLength = 30
    TabOrder = 9
  end
  object mmNotes: TMemo
    Left = 80
    Top = 272
    Width = 273
    Height = 89
    ScrollBars = ssVertical
    TabOrder = 10
  end
  object gbSummary: TGroupBox
    Left = 200
    Top = 180
    Width = 130
    Height = 79
    Caption = 'Material Summary'
    TabOrder = 11
    object lblCollections: TLabel
      Left = 71
      Top = 20
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 11
      Top = 20
      Width = 54
      Height = 13
      Caption = 'Collections:'
    end
    object Label8: TLabel
      Left = 11
      Top = 38
      Width = 55
      Height = 13
      Caption = 'Specimens:'
    end
    object lblSpecimens: TLabel
      Left = 71
      Top = 38
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label12: TLabel
      Left = 11
      Top = 56
      Width = 40
      Height = 13
      Caption = 'Storage:'
    end
    object lblStorage: TLabel
      Left = 71
      Top = 56
      Width = 3
      Height = 13
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
  end
  object pnlOtherParty: TPanel
    Left = 6
    Top = 36
    Width = 353
    Height = 54
    BevelOuter = bvNone
    TabOrder = 0
    object lblOtherParty: TLabel
      Left = 6
      Top = 1
      Width = 64
      Height = 26
      AutoSize = False
      Caption = 'Other Party:'
      Constraints.MaxWidth = 64
      Layout = tlCenter
      WordWrap = True
    end
    object lblContact: TLabel
      Left = 6
      Top = 35
      Width = 40
      Height = 13
      Caption = 'Contact:'
    end
  end
  object eName: TUserEdit
    Tag = 1
    Left = 80
    Top = 12
    Width = 273
    Height = 23
    TabOrder = 1
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    OnFindData = eNameFindData
    OnGetData = eNameGetData
  end
  object eOtherParty: TUserEdit
    Tag = 1
    Left = 80
    Top = 39
    Width = 273
    Height = 23
    TabOrder = 2
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    OnChange = eOtherPartyChange
    OnFindData = eOtherPartyFindData
    OnGetData = eOtherPartyGetData
  end
  object eContact: TUserEdit
    Tag = 1
    Left = 80
    Top = 66
    Width = 273
    Height = 23
    TabOrder = 3
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    OnFindData = eContactFindData
    OnGetData = eContactGetData
  end
  object eCompletionDate: TVagueDateEdit
    Left = 80
    Top = 126
    Width = 108
    Height = 21
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object chkLoanComplete: TCheckBox
    Left = 186
    Top = 96
    Width = 143
    Height = 25
    Alignment = taLeftJustify
    Caption = 'Items have been loaned:'
    TabOrder = 5
  end
  object chkReturnComplete: TCheckBox
    Left = 186
    Top = 128
    Width = 143
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Items have been returned:'
    TabOrder = 7
    OnClick = chkReturnCompleteClick
  end
  object eActualReturnDate: TVagueDateEdit
    Left = 80
    Top = 156
    Width = 108
    Height = 21
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
  end
  object eLoanDate: TVagueDateEdit
    Left = 80
    Top = 96
    Width = 108
    Height = 21
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
end
