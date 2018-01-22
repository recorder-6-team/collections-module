inherited fraConceptGeneral: TfraConceptGeneral
  Width = 570
  Height = 412
  Constraints.MinWidth = 234
  Font.Name = 'Arial'
  ParentFont = False
  OnResize = FrameResize
  inherited bvlBorder: TBevel
    Width = 563
    Height = 405
    Anchors = [akLeft, akTop, akRight]
  end
  object lblBasicTerm: TLabel
    Left = 16
    Top = 20
    Width = 57
    Height = 14
    Caption = 'Basic Term:'
  end
  object lblLanguage: TLabel
    Left = 16
    Top = 210
    Width = 51
    Height = 14
    Caption = 'Language:'
  end
  object Label2: TLabel
    Left = 16
    Top = 178
    Width = 43
    Height = 14
    Caption = 'Preview:'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 16
    Top = 244
    Width = 27
    Height = 14
    Caption = 'Rank:'
  end
  object Label4: TLabel
    Left = 16
    Top = 340
    Width = 48
    Height = 14
    Caption = 'List Code:'
  end
  object Label5: TLabel
    Left = 16
    Top = 372
    Width = 51
    Height = 14
    Caption = 'Sort Code:'
  end
  object Label6: TLabel
    Left = 16
    Top = 308
    Width = 57
    Height = 14
    Caption = 'Name Type:'
  end
  object lblPreview: TTermLabel
    Left = 120
    Top = 177
    Width = 1
    Height = 17
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object lblAuthorAndDate: TLabel
    Left = 16
    Top = 85
    Width = 71
    Height = 14
    Caption = 'Author && Date:'
  end
  object lblAttributes: TLabel
    Left = 17
    Top = 52
    Width = 50
    Height = 14
    Caption = 'Attributes:'
  end
  object lblPublishedTerm: TLabel
    Left = 15
    Top = 148
    Width = 76
    Height = 14
    Caption = 'Published Term:'
  end
  object btnItalic: TImageListButton
    Left = 277
    Top = 144
    Width = 23
    Height = 24
    Hint = 'Italicise selected text'
    Anchors = [akTop, akRight]
    TabOrder = 5
    OnClick = btnItalicClick
    ImageList = dmInterface.ilButtons
    ImageIndex = 8
  end
  object btnInsertBotanical: TBitBtn
    Left = 300
    Top = 144
    Width = 24
    Height = 24
    Hint = 'Insert botanical naming construct'
    Anchors = [akTop, akRight]
    TabOrder = 6
    OnClick = btnInsertBotanicalClick
    Glyph.Data = {
      72000000424D72000000000000003E00000028000000090000000D0000000100
      010000000000340000000000000000000000020000000200000000000000FFFF
      FF00FF800000FF800000FF800000FF800000FF800000F7800000E3800000C180
      000080800000FF800000FF800000FF800000FF800000}
  end
  object chkPreferred: TCheckBox
    Left = 14
    Top = 275
    Width = 119
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Preferred:'
    TabOrder = 9
  end
  object eListCode: TEdit
    Left = 120
    Top = 336
    Width = 434
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 11
  end
  object cmbNameType: TConceptGroupComboBox
    Left = 120
    Top = 304
    Width = 434
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 14
    TabOrder = 10
  end
  object cmbRank: TLuxIDComboBox
    Left = 120
    Top = 240
    Width = 434
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 14
    TabOrder = 8
    OnChange = cmbRankChange
    OnPopulate = cmbRankPopulate
  end
  object cmbLanguage: TLuxIDComboBox
    Left = 120
    Top = 208
    Width = 434
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 14
    TabOrder = 7
    OnChange = cmbLanguageChange
    OnPopulate = cmbLanguagePopulate
  end
  object eTerm: TLinkedEdit
    Tag = 1
    Left = 120
    Top = 16
    Width = 433
    Height = 22
    OnExit = eTermExit
    TabOrder = 0
    BorderStyle = bsSingle
    OnChange = eTermChange
    OnKeyDown = eTermKeyDown
    ShowButton = False
    ShowDragDropBorder = False
    OnEditMouseDown = eTermEditMouseDown
  end
  object eSortCode: TEdit
    Left = 120
    Top = 368
    Width = 433
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 12
  end
  object eAuthorAndDate: TEdit
    Left = 120
    Top = 80
    Width = 433
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = eAuthorAndDateChange
  end
  object eAttributes: TEdit
    Left = 120
    Top = 48
    Width = 433
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = eAttributesChange
  end
  object ePublishedTerm: TEdit
    Left = 120
    Top = 144
    Width = 153
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    OnChange = ePublishedTermChange
  end
  inline fraPublishedTermRuleSelector: TfraPublishedTermRuleSelector
    Left = 8
    Top = 104
    Width = 545
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    inherited lblPublishedTermRule: TLabel
      Width = 100
      Height = 14
    end
    inherited cmbPublishedTermRule: TLuxIDComboBox
      Width = 433
      Height = 22
      ItemHeight = 14
      OnChange = fraPublishedTermRuleSelectorcmbPublishedTermRuleChange
    end
  end
  object chkAutomaticPublishedTerm: TCheckBox
    Left = 326
    Top = 142
    Width = 226
    Height = 27
    Alignment = taLeftJustify
    Anchors = [akTop, akRight]
    Caption = 'Published term is calculated automatically:'
    Enabled = False
    TabOrder = 13
    WordWrap = True
    OnClick = chkAutomaticPublishedTermClick
  end
  object chkAcknoledgeUpdate: TCheckBox
    Left = 420
    Top = 168
    Width = 132
    Height = 27
    Hint = 
      'The Basic Term has been updated so the Published Term could be o' +
      'ut of date. Please tick this box to acknowledge or update the Pu' +
      'blished term.'
    Alignment = taLeftJustify
    Anchors = [akTop, akRight]
    Caption = 'Acknowledge Update:'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 14
    Visible = False
  end
  object pmBotanicalNames: TPopupMenu
    Left = 348
    Top = 364
  end
end
