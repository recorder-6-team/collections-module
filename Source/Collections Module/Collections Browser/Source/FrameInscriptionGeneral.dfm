inherited fraInscriptionGeneral: TfraInscriptionGeneral
  Height = 276
  inherited bvlBorder: TBevel
    Height = 225
    Visible = False
  end
  object grbDetail: TGroupBox
    Left = 4
    Top = 0
    Width = 357
    Height = 200
    Caption = ' Detail '
    TabOrder = 0
    object Label22: TLabel
      Left = 12
      Top = 14
      Width = 27
      Height = 13
      Caption = 'Type:'
    end
    object Label25: TLabel
      Left = 12
      Top = 38
      Width = 40
      Height = 13
      Caption = 'Position:'
    end
    object Label24: TLabel
      Left = 12
      Top = 61
      Width = 51
      Height = 13
      Caption = 'Inscription:'
    end
    object Label2: TLabel
      Left = 12
      Top = 160
      Width = 52
      Height = 13
      Caption = 'Comments:'
    end
    object Label3: TLabel
      Left = 12
      Top = 99
      Width = 55
      Height = 13
      Caption = 'Translation:'
    end
    object Label4: TLabel
      Left = 12
      Top = 137
      Width = 51
      Height = 13
      Caption = 'Language:'
    end
    object ePosition: TEdit
      Left = 72
      Top = 34
      Width = 275
      Height = 21
      TabOrder = 1
    end
    object mmInscription: TMemo
      Left = 72
      Top = 58
      Width = 275
      Height = 36
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object mmComments: TMemo
      Left = 72
      Top = 157
      Width = 275
      Height = 36
      ScrollBars = ssVertical
      TabOrder = 5
    end
    object mmTranslation: TMemo
      Left = 72
      Top = 96
      Width = 275
      Height = 36
      ScrollBars = ssVertical
      TabOrder = 3
    end
    object cmbType: TLuxIDComboBox
      Left = 72
      Top = 10
      Width = 125
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnPopulate = cmbTypePopulate
    end
    object cmbTranslationLanguage: TLuxIDComboBox
      Left = 72
      Top = 134
      Width = 125
      Height = 21
      ItemHeight = 13
      TabOrder = 4
      OnPopulate = cmbTranslationLanguagePopulate
    end
    object chkCurrent: TCheckBox
      Left = 288
      Top = 13
      Width = 58
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Current:'
      TabOrder = 6
    end
  end
  object grbAttributedTo: TGroupBox
    Left = 4
    Top = 201
    Width = 357
    Height = 72
    Caption = ' Attributed To '
    TabOrder = 1
    object Label8: TLabel
      Left = 12
      Top = 19
      Width = 34
      Height = 13
      Caption = 'Author:'
    end
    object Label1: TLabel
      Left = 11
      Top = 47
      Width = 57
      Height = 13
      Caption = 'Confidence:'
    end
    object btnAuthorInferred: TImageListButton
      Left = 323
      Top = 16
      Width = 22
      Height = 23
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = InferenceClick
      ImageList = dmInterface.ilButtons
      ImageIndex = 6
    end
    object cmbConfidence: TConceptGroupComboBox
      Left = 72
      Top = 43
      Width = 125
      Height = 21
      Enabled = False
      ItemHeight = 13
      TabOrder = 2
    end
    object eAuthor: TUserEdit
      Tag = 1
      Left = 72
      Top = 16
      Width = 250
      Height = 23
      TabOrder = 0
      BorderStyle = bsSingle
      ImageIndex = 12
      ImageList = dmInterface.ilButtons
      OnChange = eAuthorChange
      OnGetData = eAuthorGetData
    end
  end
end
