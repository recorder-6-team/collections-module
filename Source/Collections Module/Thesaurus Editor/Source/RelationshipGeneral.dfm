inherited fraRelationshipGeneral: TfraRelationshipGeneral
  Width = 364
  Height = 273
  Font.Name = 'Lucida Sans Unicode'
  ParentFont = False
  inherited bvlBorder: TBevel
    Left = 5
    Top = 7
    Width = 354
    Height = 263
    Anchors = [akLeft, akTop, akRight]
  end
  object Label1: TLabel
    Left = 12
    Top = 19
    Width = 54
    Height = 14
    Caption = 'Related To:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 12
    Top = 70
    Width = 58
    Height = 28
    Caption = 'Relationship Type:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label3: TLabel
    Left = 12
    Top = 105
    Width = 47
    Height = 14
    Caption = 'Comment:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 12
    Top = 49
    Width = 43
    Height = 14
    Caption = 'Apply to:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 12
    Top = 244
    Width = 51
    Height = 14
    Caption = 'Multiplicity:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object mmComment: TMemo
    Left = 88
    Top = 101
    Width = 263
    Height = 133
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object cmbApplyTo: TComboBox
    Left = 88
    Top = 45
    Width = 263
    Height = 22
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 1
    OnChange = cmbApplyToChange
  end
  object cmbRelationshipType: TLuxIDComboBox
    Left = 88
    Top = 73
    Width = 263
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 2
    OnPopulate = cmbRelationshipTypePopulate
  end
  object eMultiplicity: TEdit
    Left = 88
    Top = 240
    Width = 45
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object chkInherited: TCheckBox
    Left = 194
    Top = 239
    Width = 79
    Height = 23
    Alignment = taLeftJustify
    Caption = 'Inherited:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object eText: TLinkedEdit
    Tag = 1
    Left = 88
    Top = 15
    Width = 262
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    OnExit = eTextExit
    TabOrder = 0
    BorderStyle = bsSingle
    ImageIndex = 12
    ImageList = dmInterface.ilButtons
    ShowButton = False
  end
end
