inherited fraSpecimenGeneral: TfraSpecimenGeneral
  Height = 406
  inherited bvlBorder: TBevel
    Height = 398
  end
  object Label3: TLabel
    Left = 219
    Top = 10
    Width = 35
    Height = 13
    Caption = 'Pref. #:'
  end
  object Label2: TLabel
    Left = 12
    Top = 10
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object lblPrefNumber: TLabel
    Left = 260
    Top = 10
    Width = 3
    Height = 13
    Constraints.MaxWidth = 93
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 11
    Top = 87
    Width = 44
    Height = 26
    Caption = 'Current Location:'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 12
    Top = 29
    Width = 50
    Height = 26
    Caption = 'Specimen Type:'
    WordWrap = True
  end
  object Label12: TLabel
    Left = 219
    Top = 37
    Width = 35
    Height = 13
    Caption = 'Acc. #:'
  end
  object lblAccNumber: TLabel
    Left = 260
    Top = 37
    Width = 3
    Height = 13
    Constraints.MaxWidth = 93
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 215
    Top = 92
    Width = 53
    Height = 13
    Caption = 'Placement:'
  end
  object Label5: TLabel
    Left = 215
    Top = 64
    Width = 53
    Height = 13
    Caption = 'Placement:'
  end
  object Label7: TLabel
    Left = 12
    Top = 161
    Width = 49
    Height = 13
    Caption = 'Collection:'
  end
  object Label8: TLabel
    Left = 12
    Top = 120
    Width = 49
    Height = 26
    Caption = 'Field Gatherers:'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 12
    Top = 178
    Width = 44
    Height = 26
    Caption = 'Known Domains:'
    WordWrap = True
  end
  object lblDomains: TLabel
    Left = 84
    Top = 186
    Width = 3
    Height = 13
    Cursor = crHandPoint
    Color = clBtnFace
    Constraints.MaxWidth = 265
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnMouseUp = lblDomainsMouseUp
  end
  object Label9: TLabel
    Left = 241
    Top = 161
    Width = 26
    Height = 13
    Caption = 'Dept:'
  end
  object lblDept: TLabel
    Left = 272
    Top = 161
    Width = 3
    Height = 13
    Color = clBtnFace
    Constraints.MaxWidth = 81
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label10: TLabel
    Left = 12
    Top = 210
    Width = 33
    Height = 13
    Caption = 'Status:'
  end
  object lblStatus: TLabel
    Left = 140
    Top = 206
    Width = 49
    Height = 13
    Cursor = crHandPoint
    Caption = 'movement'
    Color = clBtnFace
    Constraints.MaxWidth = 165
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnClick = lblStatusClick
  end
  object Label13: TLabel
    Left = 12
    Top = 258
    Width = 56
    Height = 26
    Caption = 'Gathering Site && Date:'
    WordWrap = True
  end
  object Label14: TLabel
    Left = 11
    Top = 59
    Width = 44
    Height = 26
    Caption = 'Usual Location:'
    WordWrap = True
  end
  object lblGatheringSite: TLabel
    Left = 84
    Top = 256
    Width = 3
    Height = 13
    Cursor = crHandPoint
    Color = clBtnFace
    Constraints.MaxWidth = 165
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnMouseUp = lblGatheringSiteMouseUp
  end
  object lblGatheringDate: TLabel
    Left = 84
    Top = 273
    Width = 3
    Height = 13
    Cursor = crHandPoint
    Color = clBtnFace
    Constraints.MaxWidth = 165
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnMouseUp = lblGatheringDateMouseUp
  end
  object lblName: TTermLabel
    Left = 85
    Top = 10
    Width = 1
    Height = 13
    Constraints.MaxWidth = 123
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblConditionCheck: TLabel
    Left = 84
    Top = 206
    Width = 43
    Height = 13
    Cursor = crHandPoint
    Caption = 'condition'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = lblConditionCheckClick
  end
  object lblLastJob: TLabel
    Left = 196
    Top = 206
    Width = 33
    Height = 13
    Cursor = crHandPoint
    Caption = 'last job'
    Color = clBtnFace
    Constraints.MaxWidth = 165
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Visible = False
    OnClick = lblLastJobClick
  end
  object lblMovementSpacer: TLabel
    Left = 128
    Top = 206
    Width = 4
    Height = 16
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblJobSpacer: TLabel
    Left = 192
    Top = 206
    Width = 4
    Height = 16
    Caption = '-'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object lblThumbnail: TLabel
    Left = 12
    Top = 296
    Width = 53
    Height = 13
    Caption = 'Multimedia:'
    WordWrap = True
  end
  object eUsualLocCode: TEdit
    Left = 274
    Top = 60
    Width = 79
    Height = 21
    TabOrder = 2
    OnChange = eUsualLocCodeChange
    OnEnter = eUsualLocCodeEnter
  end
  object mmPeople: TMemo
    Left = 82
    Top = 117
    Width = 271
    Height = 34
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object eCurrentLocCode: TEdit
    Left = 274
    Top = 88
    Width = 79
    Height = 21
    TabOrder = 4
    OnKeyDown = eCurrentLocCodeKeyDown
  end
  object chkConfidential: TCheckBox
    Left = 256
    Top = 206
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Confidential:'
    TabOrder = 8
  end
  object chkDangerous: TCheckBox
    Left = 256
    Top = 228
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Dangerous:'
    TabOrder = 9
  end
  object eCurrentLocation: TLinkedEdit
    Tag = 1
    Left = 82
    Top = 87
    Width = 127
    Height = 23
    TabOrder = 3
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
    OnKeyDown = eCurrentLocationKeyDown
    ShowButton = False
  end
  object eUsualLocation: TLinkedEdit
    Tag = 1
    Left = 82
    Top = 60
    Width = 127
    Height = 23
    OnEnter = eUsualLocationEnter
    TabOrder = 1
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
    OnChange = eUsualLocationChange
    ShowButton = False
  end
  object cmbType: TLuxIDComboBox
    Left = 82
    Top = 33
    Width = 108
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnPopulate = cmbTypePopulate
  end
  object eCollection: TLinkedEdit
    Tag = 1
    Left = 82
    Top = 156
    Width = 152
    Height = 23
    TabOrder = 6
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
    ShowButton = False
  end
  object chkChecked: TCheckBox
    Left = 256
    Top = 184
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Checked'
    TabOrder = 7
  end
  object chkInternalUse: TCheckBox
    Left = 256
    Top = 250
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Internal Use:'
    TabOrder = 10
  end
  object chkPublishToWeb: TCheckBox
    Left = 256
    Top = 272
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Web Use:'
    TabOrder = 11
  end
  object pnlThumbnail: TPanel
    Left = 82
    Top = 296
    Width = 271
    Height = 97
    Anchors = [akLeft, akTop, akBottom]
    BevelOuter = bvLowered
    TabOrder = 12
    object imgThumbnail: TImage
      Left = 1
      Top = 1
      Width = 269
      Height = 95
      Align = alClient
      Center = True
      Proportional = True
    end
  end
end
