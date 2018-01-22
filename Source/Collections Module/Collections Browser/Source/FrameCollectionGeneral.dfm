inherited fraCollectionGeneral: TfraCollectionGeneral
  object lblName: TLabel
    Left = 12
    Top = 15
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object lblNumber: TLabel
    Left = 180
    Top = 40
    Width = 35
    Height = 13
    Caption = 'Acc. #:'
  end
  object Label5: TLabel
    Left = 12
    Top = 216
    Width = 33
    Height = 13
    Caption = 'Status:'
  end
  object Label3: TLabel
    Left = 12
    Top = 65
    Width = 69
    Height = 13
    Caption = 'Assembled By:'
  end
  object Label13: TLabel
    Left = 10
    Top = 40
    Width = 71
    Height = 13
    Caption = 'Current Owner:'
  end
  object Label4: TLabel
    Left = 12
    Top = 90
    Width = 30
    Height = 13
    Caption = 'Topic:'
  end
  object Label7: TLabel
    Left = 12
    Top = 166
    Width = 58
    Height = 13
    Caption = 'Department:'
    WordWrap = True
  end
  object lblStatus: TLabel
    Left = 136
    Top = 216
    Width = 49
    Height = 13
    Cursor = crHandPoint
    Caption = 'movement'
    Color = clBtnFace
    Constraints.MaxWidth = 265
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnClick = lblStatusClick
  end
  object lblOwner: TLabel
    Left = 86
    Top = 40
    Width = 3
    Height = 13
    Color = clBtnFace
    Constraints.MaxWidth = 90
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lblAccNumber: TLabel
    Left = 221
    Top = 40
    Width = 3
    Height = 13
    Color = clBtnFace
    Constraints.MaxWidth = 132
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object lblDept: TLabel
    Left = 88
    Top = 166
    Width = 3
    Height = 13
    Color = clBtnFace
    Constraints.MaxWidth = 265
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label6: TLabel
    Left = 12
    Top = 185
    Width = 44
    Height = 26
    Caption = 'Known Domains:'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 12
    Top = 241
    Width = 24
    Height = 13
    Caption = 'Risk:'
  end
  object Label1: TLabel
    Left = 12
    Top = 139
    Width = 44
    Height = 26
    Caption = 'Current Location:'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 215
    Top = 141
    Width = 53
    Height = 13
    Caption = 'Placement:'
  end
  object Label9: TLabel
    Left = 215
    Top = 115
    Width = 53
    Height = 13
    Caption = 'Placement:'
  end
  object Label14: TLabel
    Left = 12
    Top = 111
    Width = 44
    Height = 26
    Caption = 'Usual Location:'
    WordWrap = True
  end
  object lblConditionCheck: TLabel
    Left = 85
    Top = 216
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
  object lblMovementSpacer: TLabel
    Left = 130
    Top = 216
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
    Left = 190
    Top = 216
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
  object lblLastJob: TLabel
    Left = 200
    Top = 216
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
  object eName: TEdit
    Left = 86
    Top = 12
    Width = 265
    Height = 21
    TabOrder = 0
  end
  object eTopic: TEdit
    Left = 86
    Top = 87
    Width = 266
    Height = 21
    TabOrder = 2
  end
  object eUsualLocCode: TEdit
    Left = 273
    Top = 114
    Width = 79
    Height = 21
    TabOrder = 4
    OnChange = eUsualLocCodeChange
    OnEnter = eUsualLocCodeEnter
  end
  object eCurrentLocCode: TEdit
    Left = 273
    Top = 140
    Width = 79
    Height = 21
    TabOrder = 6
    OnKeyDown = eCurrentLocCodeKeyDown
  end
  object mmDomains: TMemo
    Left = 87
    Top = 185
    Width = 264
    Height = 26
    BevelInner = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentCtl3D = False
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object cmbRisk: TConceptGroupComboBox
    Left = 84
    Top = 240
    Width = 269
    Height = 21
    ItemHeight = 13
    TabOrder = 8
  end
  object eAssembledBy: TUserEdit
    Tag = 1
    Left = 85
    Top = 61
    Width = 266
    Height = 23
    TabOrder = 1
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
  end
  object eCurrentLocation: TLinkedEdit
    Tag = 1
    Left = 85
    Top = 139
    Width = 124
    Height = 23
    TabOrder = 5
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    OnKeyDown = eCurrentLocationKeyDown
    ShowButton = False
  end
  object eUsualLocation: TLinkedEdit
    Tag = 1
    Left = 85
    Top = 113
    Width = 124
    Height = 23
    OnEnter = eUsualLocationEnter
    TabOrder = 3
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    OnChange = eUsualLocationChange
    ShowButton = False
  end
end
