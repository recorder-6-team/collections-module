inherited fraStorageGeneral: TfraStorageGeneral
  object Label2: TLabel
    Left = 12
    Top = 13
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label5: TLabel
    Left = 12
    Top = 41
    Width = 40
    Height = 13
    Caption = 'Number:'
  end
  object Label4: TLabel
    Left = 160
    Top = 42
    Width = 27
    Height = 13
    Caption = 'Type:'
  end
  object Label3: TLabel
    Left = 12
    Top = 146
    Width = 46
    Height = 26
    Caption = 'Details Summary:'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 12
    Top = 209
    Width = 52
    Height = 13
    Caption = 'Comments:'
  end
  object lblNumber: TLabel
    Left = 68
    Top = 41
    Width = 3
    Height = 13
    Constraints.MaxWidth = 90
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label9: TLabel
    Left = 12
    Top = 124
    Width = 33
    Height = 13
    Caption = 'Status:'
  end
  object lblStatus: TLabel
    Left = 128
    Top = 124
    Width = 49
    Height = 13
    Cursor = crHandPoint
    Caption = 'movement'
    Color = clBtnFace
    Constraints.MaxWidth = 282
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnClick = lblStatusClick
  end
  object Label1: TLabel
    Left = 12
    Top = 87
    Width = 44
    Height = 26
    Caption = 'Current Location:'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 215
    Top = 97
    Width = 53
    Height = 13
    Caption = 'Placement:'
  end
  object Label14: TLabel
    Left = 12
    Top = 59
    Width = 44
    Height = 26
    Caption = 'Usual Location:'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 215
    Top = 68
    Width = 53
    Height = 13
    Caption = 'Placement:'
  end
  object lblConditionCheck: TLabel
    Left = 68
    Top = 124
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
    Left = 120
    Top = 124
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
    Left = 184
    Top = 124
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
    Left = 196
    Top = 124
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
    Left = 68
    Top = 10
    Width = 284
    Height = 21
    TabOrder = 0
  end
  object cmbType: TConceptGroupComboBox
    Left = 192
    Top = 38
    Width = 81
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object mmDetails: TMemo
    Left = 68
    Top = 146
    Width = 285
    Height = 58
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 7
  end
  object mmComments: TMemo
    Left = 68
    Top = 209
    Width = 285
    Height = 58
    ScrollBars = ssVertical
    TabOrder = 8
  end
  object chkSpecimen: TCheckBox
    Left = 274
    Top = 42
    Width = 77
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Specimen:'
    TabOrder = 2
    OnClick = chkSpecimenClick
  end
  object eUsualLocCode: TEdit
    Left = 273
    Top = 66
    Width = 79
    Height = 21
    TabOrder = 4
    OnChange = eUsualLocCodeChange
    OnEnter = eUsualLocCodeEnter
  end
  object eCurrentLocCode: TEdit
    Left = 273
    Top = 94
    Width = 79
    Height = 21
    TabOrder = 6
    OnKeyDown = eCurrentLocCodeKeyDown
  end
  object eCurrentLocation: TLinkedEdit
    Tag = 1
    Left = 68
    Top = 94
    Width = 141
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
    Left = 68
    Top = 65
    Width = 141
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
