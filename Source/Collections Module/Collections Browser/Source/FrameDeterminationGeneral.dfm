inherited fraDeterminationGeneral: TfraDeterminationGeneral
  Height = 266
  OnResize = FrameResize
  inherited bvlBorder: TBevel
    Height = 256
  end
  object Label12: TLabel
    Left = 212
    Top = 135
    Width = 26
    Height = 13
    Caption = 'Date:'
  end
  object Label13: TLabel
    Left = 180
    Top = 45
    Width = 50
    Height = 13
    Caption = 'Det. Type:'
  end
  object Label11: TLabel
    Left = 12
    Top = 68
    Width = 68
    Height = 26
    Caption = 'Nomenclatural Status:'
    WordWrap = True
  end
  object Label10: TLabel
    Left = 12
    Top = 134
    Width = 25
    Height = 13
    Caption = 'Role:'
  end
  object lblDeterminer: TLabel
    Left = 12
    Top = 104
    Width = 54
    Height = 13
    Caption = 'Determiner:'
  end
  object Label8: TLabel
    Left = 12
    Top = 15
    Width = 68
    Height = 13
    Caption = 'Determination:'
  end
  object Label2: TLabel
    Left = 180
    Top = 74
    Width = 57
    Height = 13
    Caption = 'Confidence:'
  end
  object Label3: TLabel
    Left = 12
    Top = 45
    Width = 39
    Height = 13
    Caption = 'Domain:'
  end
  object Label6: TLabel
    Left = 12
    Top = 222
    Width = 31
    Height = 13
    Caption = 'Notes:'
  end
  object lblDomain: TLabel
    Left = 90
    Top = 45
    Width = 3
    Height = 13
    Constraints.MaxWidth = 82
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 12
    Top = 158
    Width = 77
    Height = 26
    AutoSize = False
    Caption = 'Determination Made Against:'
    WordWrap = True
  end
  object Label7: TLabel
    Left = 12
    Top = 194
    Width = 39
    Height = 13
    Caption = 'Method:'
  end
  object mmNotes: TMemo
    Left = 88
    Top = 219
    Width = 261
    Height = 38
    ScrollBars = ssVertical
    TabOrder = 12
  end
  object chkPreferred: TCheckBox
    Left = 210
    Top = 152
    Width = 139
    Height = 23
    Alignment = taLeftJustify
    Caption = 'Preferred Determination:'
    TabOrder = 9
  end
  object mmMethod: TMemo
    Left = 88
    Top = 191
    Width = 261
    Height = 19
    ScrollBars = ssVertical
    TabOrder = 11
  end
  object eDetermination: TLinkedEdit
    Tag = 1
    Left = 87
    Top = 11
    Width = 261
    Height = 23
    TabOrder = 0
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
    OnFindData = eDeterminationFindData
    OnGetData = eDeterminationGetData
  end
  object cmbType: TLuxIDComboBox
    Left = 240
    Top = 41
    Width = 109
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnPopulate = cmbTypePopulate
  end
  object cmbStatus: TConceptGroupComboBox
    Left = 88
    Top = 71
    Width = 88
    Height = 21
    ItemHeight = 13
    TabOrder = 2
  end
  object cmbConfidence: TLuxIDComboBox
    Left = 240
    Top = 72
    Width = 109
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    OnPopulate = cmbConfidencePopulate
  end
  object eDeterminer: TUserEdit
    Tag = 1
    Left = 88
    Top = 101
    Width = 238
    Height = 23
    TabOrder = 4
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    MaxLength = 0
  end
  object cmbRole: TLuxIDComboBox
    Left = 88
    Top = 131
    Width = 109
    Height = 21
    ItemHeight = 13
    TabOrder = 6
    OnPopulate = cmbRolePopulate
  end
  object eDate: TVagueDateEdit
    Left = 246
    Top = 131
    Width = 103
    Height = 21
    Hint = '"VAGUEDATEEDIT1" is not a valid month or season name.'
    TabOrder = 7
  end
  object cmbDetMadeAgainst: TLuxIDComboBox
    Left = 88
    Top = 161
    Width = 109
    Height = 21
    ItemHeight = 13
    TabOrder = 8
    OnPopulate = cmbDetMadeAgainstPopulate
  end
  object btnDeterminerInferred: TImageListButton
    Left = 326
    Top = 101
    Width = 22
    Height = 23
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = InferenceClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 6
  end
  object chkIncludeInLabel: TCheckBox
    Left = 210
    Top = 172
    Width = 139
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Include in label: '
    TabOrder = 10
  end
end
